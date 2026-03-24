rm(list = setdiff(ls(), c("spid_master", "vintage", "spid_data", "version")))
gc()

library(duckdb)
library(dplyr)
library(openxlsx2)

# ==============================================================================
# 0. Connect to DuckDB and load spatial extension
# ==============================================================================

con <- dbConnect(duckdb())
dbExecute(con, "INSTALL spatial; LOAD spatial;")

# ==============================================================================
# 1. Helper: register an sf object as a DuckDB table via a temp GeoPackage
#
#    DuckDB spatial reads files via ST_Read(); the cleanest way to pass an
#    in-memory sf object is to write it to a temp file first.
# ==============================================================================

sf_to_duckdb <- function(con, sf_obj, table_name, geom_col = "geom") {
  tmp <- tempfile(fileext = ".gpkg")
  sf::st_write(sf_obj, tmp, layer = table_name, quiet = TRUE, append = FALSE)
  dbExecute(con, glue::glue(
    "CREATE OR REPLACE TABLE {table_name} AS
     SELECT * FROM ST_Read('{tmp}');"
  ))
  unlink(tmp)
  invisible(table_name)
}

# ==============================================================================
# 2. Load inputs from R (xlsx via openxlsx2, gpkg via sf) then push to DuckDB
# ==============================================================================

message("Loading input data...")

spid_bounds <- read_xlsx(spid_master, sheet = "SPID boundaries") |>
  filter(!is.na(geo_code))

spid_miss <- read_xlsx(spid_master, sheet = "SPID missing boundaries")

admin0     <- sf::st_read(paste0(spid_data, "final/",   version, "/",
                                 tolower(vintage), "_admin0.gpkg"),
                          quiet = TRUE)

spid_subnat <- sf::st_read(paste0(spid_data, "interim/", version, "/",
                                  tolower(vintage), "_subnat.gpkg"),
                           quiet = TRUE)

dropped <- read_xlsx(paste0(spid_data, "interim/", version,
                            "/subnat_dropped.xlsx"))

# Push tabular data (no geometry) directly via dbWriteTable
dbWriteTable(con, "spid_bounds", spid_bounds, overwrite = TRUE)
dbWriteTable(con, "spid_miss",   spid_miss,   overwrite = TRUE)
dbWriteTable(con, "dropped",     dropped,     overwrite = TRUE)

# Push spatial data via temp GeoPackage
message("Registering spatial tables in DuckDB...")
sf_to_duckdb(con, admin0,      "admin0")
sf_to_duckdb(con, spid_subnat, "spid_subnat")

# ==============================================================================
# 3. Build survey key table and priority (most-recent survey wins per geo_code)
# ==============================================================================

dbExecute(con, "
CREATE OR REPLACE TABLE spid_all AS
SELECT *,
       concat(code, ' ', year, ' ', survname, ' ', byvar) AS key
FROM (
    SELECT * FROM spid_bounds
    UNION ALL
    SELECT * FROM spid_miss
);
")

# Replicate R's logic: spid_list = rev(unique(spid_all$key))
# arrange(match(key, spid_list)) + distinct(geo_code) means:
#   for each geo_code keep the row whose key appears latest in spid_list,
#   i.e. earliest in the original key order → ORDER BY key ASC, then DISTINCT.
# We encode this as ROW_NUMBER() partitioned by geo_code, ordered by key ASC.
dbExecute(con, "
CREATE OR REPLACE TABLE allowed_codes AS
WITH ranked AS (
    SELECT geo_code, key,
           ROW_NUMBER() OVER (
               PARTITION BY geo_code
               ORDER BY key ASC          -- first occurrence in forward order
           ) AS rn
    FROM spid_all
)
SELECT geo_code, key
FROM ranked
WHERE rn = 1;
")

# ==============================================================================
# 4. Edge-matching pipeline (single SQL pass across all surveys)
#
#    Steps mirror the R process_survey() function exactly:
#      a. attach geometries, apply priority / dropped filter
#      b. reproject to EPSG:3395
#      c. build per-survey union + boundary outline
#      d. intersect polygon boundaries with outline → interior edges
#      e. segmentize (100 m) → dump to points → snap to grid (10 m) → dedup
#      f. ST_VoronoiDiagram per survey
#      g. dump Voronoi cells, assign geo_code via point-in-cell join
#      h. gap-fill: Voronoi fringe ∪ original polygon
#      i. clip to admin-0
#      j. extract polygons, deduplicate geo_codes
#      k. recover geo_codes lost to clipping
# ==============================================================================

message("Running edge-matching pipeline in DuckDB...")

dbExecute(con, "
CREATE OR REPLACE TABLE spid_em_raw AS

WITH

-- ------------------------------------------------------------------ --
-- 4a. Attach geometries; enforce priority and dropped-regions filter  --
-- ------------------------------------------------------------------ --
sample_geoms AS (
    SELECT  a.key,
            a.geo_code,
            ST_MakeValid(s.geom) AS geom
    FROM    spid_all      a
    JOIN    spid_subnat   s  USING (geo_code)
    JOIN    allowed_codes ac USING (geo_code)
    WHERE   ac.key = a.key                          -- most-recent survey wins
      AND   s.geom IS NOT NULL
      AND   a.geo_code NOT IN (SELECT geo_code FROM dropped)
),

-- ------------------------------------------------------------------ --
-- 4b. Reproject to EPSG:3395 (metric CRS, matches R's st_transform)  --
-- ------------------------------------------------------------------ --
sample_proj AS (
    SELECT key, geo_code,
           ST_Transform(geom, 'EPSG:4326', 'EPSG:3395') AS geom
    FROM   sample_geoms
),

-- ------------------------------------------------------------------ --
-- 4c. Per-survey union → outline as linestring boundary              --
-- ------------------------------------------------------------------ --
survey_union AS (
    SELECT key,
           ST_MakeValid(ST_Union_Agg(geom)) AS unioned
    FROM   sample_proj
    GROUP  BY key
),

survey_outline AS (
    SELECT key,
           unioned,
           ST_Boundary(unioned) AS outline
    FROM   survey_union
),

-- ------------------------------------------------------------------ --
-- 4d. Interior border lines: intersect each polygon boundary         --
--     with the survey outline (shared edges only)                    --
-- ------------------------------------------------------------------ --
boundary_lines AS (
    SELECT  sp.key,
            sp.geo_code,
            ST_MakeValid(
                ST_Intersection(ST_Boundary(sp.geom), so.outline)
            ) AS line_geom
    FROM    sample_proj sp
    JOIN    survey_outline so USING (key)
    WHERE   ST_Intersects(ST_Boundary(sp.geom), so.outline)
      AND   ST_Dimension(ST_Boundary(sp.geom)) >= 1   -- skip point-like geoms
),

-- ------------------------------------------------------------------ --
-- 4e-i. Segmentize lines to 100 m max segment length               --
-- ------------------------------------------------------------------ --
segmentized AS (
    SELECT key, geo_code,
           ST_Segmentize(line_geom, 100) AS seg_geom   -- 100 m in EPSG:3395
    FROM   boundary_lines
    WHERE  line_geom IS NOT NULL
),

-- ------------------------------------------------------------------ --
-- 4e-ii. Dump segmentized lines → individual point rows             --
--        DuckDB ST_Dump returns a struct {geom, path};               --
--        UNNEST expands the array returned by ST_Dump.               --
-- ------------------------------------------------------------------ --
dumped_structs AS (
    SELECT key, geo_code,
           UNNEST(ST_Dump(seg_geom)) AS pt_struct
    FROM   segmentized
),

point_rows AS (
    SELECT key, geo_code,
           (pt_struct).geom AS raw_pt
    FROM   dumped_structs
    WHERE  ST_GeometryType((pt_struct).geom) = 'POINT'
),

-- ------------------------------------------------------------------ --
-- 4e-iii. Snap to 10 m grid and deduplicate                         --
-- ------------------------------------------------------------------ --
point_snapped AS (
    SELECT key, geo_code,
           ST_SnapToGrid(raw_pt, 10) AS pt
    FROM   point_rows
),

point_dedup AS (
    SELECT DISTINCT key, geo_code, pt
    FROM   point_snapped
    WHERE  pt IS NOT NULL
),

-- Filter surveys with at least 4 distinct points (Voronoi minimum)
point_counts AS (
    SELECT key, COUNT(*) AS n_pts
    FROM   point_dedup
    GROUP  BY key
),

point_filtered AS (
    SELECT pd.*
    FROM   point_dedup pd
    JOIN   point_counts pc USING (key)
    WHERE  pc.n_pts >= 4
),

-- ------------------------------------------------------------------ --
-- 4f. Voronoi diagram per survey                                     --
--     ST_VoronoiDiagram takes a MultiPoint and returns a             --
--     GeometryCollection of polygons.                                --
-- ------------------------------------------------------------------ --
voronoi_raw AS (
    SELECT key,
           ST_VoronoiDiagram(ST_Collect(pt)) AS voronoi_collection
    FROM   point_filtered
    GROUP  BY key
),

-- ------------------------------------------------------------------ --
-- 4f-ii. Dump Voronoi collection → individual cell rows             --
-- ------------------------------------------------------------------ --
voronoi_dumped AS (
    SELECT key,
           UNNEST(ST_Dump(voronoi_collection)) AS cell_struct
    FROM   voronoi_raw
),

voronoi_cells AS (
    SELECT key,
           ST_CollectionExtract((cell_struct).geom, 3) AS cell_geom
    FROM   voronoi_dumped
    WHERE  ST_GeometryType((cell_struct).geom) IN ('POLYGON', 'MULTIPOLYGON')
       OR  ST_GeometryType((cell_struct).geom) = 'GEOMETRYCOLLECTION'
),

-- ------------------------------------------------------------------ --
-- 4g. Assign each Voronoi cell to a geo_code via point-in-cell join  --
--     Tiebreak deterministically by geo_code to match R behaviour.   --
-- ------------------------------------------------------------------ --
voronoi_assigned AS (
    SELECT  vc.key,
            pf.geo_code,
            vc.cell_geom
    FROM    voronoi_cells vc
    JOIN    point_filtered pf
      ON    vc.key = pf.key
      AND   ST_Intersects(pf.pt, vc.cell_geom)
    QUALIFY ROW_NUMBER() OVER (
                PARTITION BY vc.key, ST_AsWKB(vc.cell_geom)
                ORDER BY pf.geo_code        -- deterministic tiebreak
            ) = 1
),

-- Union Voronoi cells per (survey, geo_code)
voronoi_union AS (
    SELECT key, geo_code,
           ST_MakeValid(ST_Union_Agg(cell_geom)) AS vor_geom
    FROM   voronoi_assigned
    GROUP  BY key, geo_code
),

-- ------------------------------------------------------------------ --
-- 4h. Gap-fill: Voronoi fringe outside original union + original     --
--     Mirrors R: st_difference(voronoi, samp_union) |> bind_rows     --
--               (sample_proj) |> group_by |> st_union               --
-- ------------------------------------------------------------------ --
gap_fill AS (
    SELECT  vu.key,
            vu.geo_code,
            ST_MakeValid(
                ST_Union(
                    ST_Difference(vu.vor_geom, so.unioned),
                    sp.geom
                )
            ) AS geom
    FROM    voronoi_union vu
    JOIN    survey_outline so USING (key)
    JOIN    sample_proj    sp USING (key, geo_code)
),

-- ------------------------------------------------------------------ --
-- 4i. Prep admin-0: project to EPSG:3395, union per country code     --
-- ------------------------------------------------------------------ --
admin0_proj AS (
    SELECT  SUBSTR(geo_code, 1, 3) AS cty,
            ST_MakeValid(
                ST_Union_Agg(
                    ST_Transform(geom, 'EPSG:4326', 'EPSG:3395')
                )
            ) AS geom
    FROM    admin0
    GROUP   BY cty
),

-- Clip gap-filled polygons to admin-0
clipped AS (
    SELECT  gf.key,
            gf.geo_code,
            ST_MakeValid(
                ST_Intersection(gf.geom, a0.geom)
            ) AS geom
    FROM    gap_fill gf
    JOIN    admin0_proj a0
      ON    SUBSTR(gf.key, 1, 3) = a0.cty
    WHERE   ST_Intersects(gf.geom, a0.geom)
      AND   gf.geom IS NOT NULL
),

-- ------------------------------------------------------------------ --
-- 4j. Extract polygons only; deduplicate geo_codes (most-recent wins) --
-- ------------------------------------------------------------------ --
poly_extracted AS (
    SELECT key, geo_code,
           ST_CollectionExtract(geom, 3) AS geom
    FROM   clipped
    WHERE  geom IS NOT NULL
      AND  NOT ST_IsEmpty(geom)
),

-- Re-union after collection extract (a geo_code may have split pieces)
poly_unioned AS (
    SELECT key, geo_code,
           ST_MakeValid(ST_Union_Agg(geom)) AS geom
    FROM   poly_extracted
    GROUP  BY key, geo_code
),

deduped AS (
    SELECT geo_code, geom
    FROM   poly_unioned
    QUALIFY ROW_NUMBER() OVER (
                PARTITION BY geo_code
                ORDER BY key DESC       -- reverse-chrono: latest key wins
            ) = 1
),

-- ------------------------------------------------------------------ --
-- 4k. Recover geo_codes entirely lost to admin-0 clipping            --
--     (mirrors R's dropped2 / bind_rows step)                        --
-- ------------------------------------------------------------------ --
dropped_back AS (
    SELECT  s.geo_code,
            ST_Transform(s.geom, 'EPSG:4326', 'EPSG:3395') AS geom
    FROM    spid_subnat s
    WHERE   s.geo_code NOT IN (SELECT geo_code FROM deduped)
      AND   s.geom IS NOT NULL
),

combined AS (
    SELECT geo_code, geom FROM deduped
    UNION ALL
    SELECT geo_code, geom FROM dropped_back
)

-- ------------------------------------------------------------------ --
-- Final select: reproject to WGS84, attach attributes, filter, sort  --
-- ------------------------------------------------------------------ --
SELECT  s.code,
        s.geo_year,
        s.geo_source,
        s.geo_level,
        s.geo_idvar,
        s.geo_id,
        s.geo_nvar,
        s.geo_name,
        c.geo_code,
        ST_Transform(c.geom, 'EPSG:3395', 'EPSG:4326') AS geom
FROM    combined c
JOIN    spid_subnat s USING (geo_code)
WHERE   ST_GeometryType(c.geom) IN ('POLYGON', 'MULTIPOLYGON')
ORDER   BY c.geo_code;
")

# ==============================================================================
# 5. Write outputs directly from DuckDB (no round-trip through R memory)
# ==============================================================================

message("Writing output files...")

out_base <- paste0(spid_data, "final/", version, "/", tolower(vintage), "_subnat_em")

dbExecute(con, glue::glue(
  "COPY (SELECT * FROM spid_em_raw)
   TO '{out_base}.gpkg'
   (FORMAT GDAL, DRIVER 'GeoPackage');"
))

dbExecute(con, glue::glue(
  "COPY (SELECT * FROM spid_em_raw)
   TO '{out_base}.shp'
   (FORMAT GDAL, DRIVER 'ESRI Shapefile');"
))

message("Outputs written to:\n  ", out_base, ".gpkg\n  ", out_base, ".shp")

# ==============================================================================
# 6. Pull result back into R as sf object for checks
# ==============================================================================

message("Reading result back into R for validation...")

spid_em <- sf::st_read(paste0(out_base, ".gpkg"), quiet = TRUE)

# ==============================================================================
# 7. Checks (mirrors original R script)
# ==============================================================================

spid_all_r <- bind_rows(spid_bounds, spid_miss) |>
  mutate(key = paste(code, year, survname, byvar))

cat("\n--- Validation checks ---\n")
cat("Unique geo_codes in input:      ", length(unique(spid_all_r$geo_code)), "\n")
cat("Unique geo_codes in output:     ", nrow(spid_em), "\n")
cat("Any duplicate geo_codes:        ", any(duplicated(spid_em$geo_code)), "\n")  # FALSE
cat("Any invalid geometries:         ", any(!sf::st_is_valid(spid_em)), "\n")     # FALSE

surv_list <- mutate(spid_bounds, key = paste(code, year, survname))
cat("Unique surveys in input:        ", length(unique(surv_list$key)), "\n")
cat("geo_codes matched to spid_bounds:", nrow(spid_em[spid_em$geo_code %in% spid_bounds$geo_code, ]), "\n")
cat("Unique countries in input:      ", length(unique(spid_bounds$code)), "\n")
cat("Unique countries in output:     ", length(unique(spid_em$code)), "\n")

# ==============================================================================
# 8. Disconnect
# ==============================================================================

dbDisconnect(con)
message("\nDone.")