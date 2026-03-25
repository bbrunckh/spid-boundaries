rm(list = setdiff(ls(), c("spid_master","vintage","spid_data","version"))) 
gc()

library(sf)
library(lwgeom)
library(openxlsx2)
library(dplyr)
library(furrr)

#------------------------------------------------------------------------------#
# Path helpers (defined once, reused throughout)
#------------------------------------------------------------------------------#

path_final   <- paste0(spid_data, "final/",  version, "/", tolower(vintage))
path_interim <- paste0(spid_data, "interim/", version, "/", tolower(vintage))

#------------------------------------------------------------------------------#
# SPID boundary IDs
#------------------------------------------------------------------------------#

spid_bounds <- read_xlsx(spid_master, sheet = "SPID boundaries") |>
  filter(!is.na(geo_code))

spid_miss <- read_xlsx(spid_master, sheet = "SPID missing boundaries")

#------------------------------------------------------------------------------#
# Admin-0 and subnational boundaries
#------------------------------------------------------------------------------#

admin0      <- st_read(paste0(path_final,   "_admin0.gpkg"))
spid_subnat <- st_read(paste0(path_interim, "_subnat.gpkg"))
dropped     <- read_xlsx(paste0(spid_data, "interim/", version, "/subnat_dropped.xlsx"))

#------------------------------------------------------------------------------#
# Build spid_all with geometry pre-joined
# Joining once here means workers never need spid_subnat
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# Build spid_all WITHOUT geometry — keeps it small for parallel export.
# Geometry is looked up per-worker from spid_geoms (geo_code → geom only).
#------------------------------------------------------------------------------#

spid_all <- bind_rows(spid_bounds, spid_miss) |>
  mutate(key = paste(code, year, survname, byvar)) |>
  arrange(key)

# Compact geometry lookup: one row per unique geo_code
spid_geoms <- spid_subnat |>
  select(geo_code, geom) |>
  st_make_valid() |>                          # repair before any geometry operation
  filter(!duplicated(geo_code))               # deduplicate on key, not geometry

stopifnot("Duplicate geo_codes in spid_subnat" = !anyDuplicated(spid_geoms$geo_code))

spid_list <- rev(unique(spid_all$key))

sf_use_s2(FALSE)

# Pre-index admin-0 by 3-letter country code, pre-unioned and pre-projected
# so each worker receives a ready-to-use admin-0 geometry
admin0_list <- split(admin0, substr(admin0$geo_code, 1, 3)) |>
  lapply(function(x) st_union(x) |> st_make_valid() |> st_transform(3395) |> st_make_valid())

# Pre-compute which geo_codes each survey key is responsible for.
# Each geo_code is assigned to its first occurrence in spid_list order
# (reversed-chronological), so the most recent survey wins.
geo_code_first_key <- spid_all |>
  arrange(match(key, spid_list)) |>
  distinct(geo_code, .keep_all = TRUE) |>
  select(geo_code, key)

allowed_codes_by_key <- geo_code_first_key |>
  group_by(key) |>
  summarize(allowed = list(geo_code)) |>
  tibble::deframe()

#------------------------------------------------------------------------------#
# Fingerprinting: deduplicate keys by their geo_code set
# Keys with identical allowed geo_code sets produce identical edge-match
# results, so only one representative key per unique set needs to be processed
#------------------------------------------------------------------------------#

dropped_codes <- dropped$geo_code  # extract once; avoids passing full data frame to workers

# Build a fingerprint for each key: sorted, pipe-separated allowed geo_codes.
# Keys with no allowed codes (all claimed by a more recent survey) are excluded
# entirely — they would have returned NULL anyway, but this avoids dispatching
# the job at all.
key_signatures <- tibble::enframe(
  lapply(allowed_codes_by_key, function(codes) {
    # Also apply the dropped filter here so the fingerprint reflects what
    # will actually be processed, not just what is nominally allowed
    effective <- setdiff(codes, dropped_codes)
    if (length(effective) == 0L) return(NA_character_)
    paste(sort(effective), collapse = "|")
  }),
  name  = "key",
  value = "sig"
) |>
  filter(!is.na(sig), key %in% spid_list)

# Keep only the first (most recent) key per unique geo_code fingerprint.
# spid_list is already in reversed-chronological order so the first match
# is always the most recent survey for that configuration.
unique_keys <- key_signatures |>
  arrange(match(key, spid_list)) |>
  distinct(sig, .keep_all = TRUE) |>
  pull(key)

message(
  length(spid_list), " total keys -> ",
  length(unique_keys), " unique geo_code sets to process (",
  length(spid_list) - length(unique_keys), " skipped as duplicates or empty)"
)

#------------------------------------------------------------------------------#
# Per-survey processing function
# - Works entirely in EPSG:3395; reprojects to 4326 only at return
# - st_make_valid() called only after topology-risk operations
# - st_boundary() replaces st_cast to MULTILINESTRING
# - Matrix-based point deduplication (faster than sf row-filtering)
# - vapply replaces sapply for type safety
#------------------------------------------------------------------------------#

process_survey <- function(key, spid_all, spid_geoms, admin0_list, skip_codes) {

  # --- Build sample: join geometry here, inside the worker ---
  sample_proj <- spid_all |>
    filter(key == !!key) |>
    select(geo_code) |>
    filter(!geo_code %in% skip_codes) |>
    left_join(spid_geoms, by = "geo_code") |>
    st_as_sf() |>
    filter(!is.na(st_dimension(geom))) |>
    st_make_valid() |>
    st_transform(3395) |>
    st_make_valid()

  if (nrow(sample_proj) == 0) return(NULL)

  # --- Target admin-0 (already unioned, validated, projected) ---
  cty         <- substr(key, 1, 3)
  target_proj <- admin0_list[[cty]]
  if (is.null(target_proj)) return(NULL)

  # --- Union of sample for boundary extraction and gap-fill ---
  samp_union_proj <- st_union(sample_proj) |> st_make_valid()

  # --- Interior shared boundaries via st_boundary ---
  outline <- st_boundary(samp_union_proj)

  lines <- suppressWarnings(
    st_intersection(sample_proj, outline) |>
      st_collection_extract("LINESTRING") |>
      st_make_valid()
  )
  if (nrow(lines) == 0) return(NULL)

  # --- Points: segmentize, snap, deduplicate via coordinate matrix ---
  pts_raw <- suppressWarnings(
    lines |>
      st_segmentize(dfMaxLength = units::set_units(100, m)) |>
      st_cast("MULTIPOINT") |>
      st_cast("POINT")
  )

  # Round to 10 m grid (equivalent to st_snap_to_grid) then deduplicate on matrix
  pts_matrix <- round(st_coordinates(pts_raw), -1)
  pts_matrix <- pts_matrix[!duplicated(pts_matrix), , drop = FALSE]
  if (nrow(pts_matrix) < 4) return(NULL)

  points_proj <- st_as_sf(
    as.data.frame(pts_matrix[, c("X","Y")]),
    coords = c("X","Y"),
    crs    = 3395
  )

  # --- Voronoi in EPSG:3395 ---
  voron <- tryCatch(
    st_collection_extract(st_voronoi(st_combine(points_proj))) |>
      st_set_crs(3395) |>
      st_make_valid(),
    error = function(e) {
      message("  Voronoi failed for: ", key, "\n  ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(voron)) return(NULL)

  voron_sf  <- st_sf(geom = voron)
  voron_idx <- vapply(
    st_intersects(points_proj, voron_sf),
    function(x) if (length(x) == 0L) NA_integer_ else x[[1L]],
    integer(1L)
  )

  valid_pts   <- !is.na(voron_idx)
  if (!any(valid_pts)) return(NULL)
  points_proj <- points_proj[valid_pts, ]
  voron_idx   <- voron_idx[valid_pts]

  # Re-attach geo_code via nearest-feature join back to sample polygons
  points_proj$geo_code <- sample_proj$geo_code[
    st_nearest_feature(points_proj, sample_proj)
  ]

  # --- Assign Voronoi cells and build gap-filled polygons ---
  em_prediff <- tryCatch({
    points_proj |>
      mutate(geom = voron[voron_idx]) |>
      st_make_valid() |>
      group_by(geo_code) |> summarize(geom = st_union(geom)) |>
      st_make_valid() |>
      st_difference(samp_union_proj) |>
      bind_rows(sample_proj) |>
      group_by(geo_code) |> summarize(geom = st_union(geom)) |>
      st_make_valid()
  }, error = function(e) {
    message("  Gap-fill failed for: ", key, "\n  ", conditionMessage(e))
    NULL
  })
  if (is.null(em_prediff) || nrow(em_prediff) == 0) return(NULL)

  # --- Clip to admin-0 and extract polygons ---
  em_intersected <- tryCatch(
    st_intersection(em_prediff, target_proj) |> st_make_valid(),
    error = function(e) {
      message("  Intersection failed for: ", key, "\n  ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(em_intersected) || nrow(em_intersected) == 0) return(NULL)

  em_poly <- em_intersected |>
    st_collection_extract("POLYGON") |>
    st_make_valid() |>
    group_by(geo_code) |> summarize(geom = st_union(geom))

  # Recover geo_codes lost in collection extraction
  missing_codes <- setdiff(em_prediff$geo_code, em_poly$geo_code)
  if (length(missing_codes) > 0) {
    em_poly <- bind_rows(
      em_poly,
      filter(em_prediff, geo_code %in% missing_codes)
    )
  }

  em <- st_transform(em_poly, 4326)  # reproject to WGS84 only at return

  if (nrow(em) == 0 || !"geo_code" %in% names(em)) return(NULL)
  return(em)
}

#------------------------------------------------------------------------------#
# Run — parallel across unique geo_code sets only
#------------------------------------------------------------------------------#

# Write large globals to disk so workers read from file rather than having
# them serialized and sent over the socket connection (~3 GiB → ~0 GiB export)
tmp_spid_all   <- tempfile(fileext = ".rds")
tmp_spid_geoms <- tempfile(fileext = ".rds")
saveRDS(spid_all,   tmp_spid_all)
saveRDS(spid_geoms, tmp_spid_geoms)

#------------------------------------------------------------------------------#
# Parallel wrapper
#------------------------------------------------------------------------------#

process_survey_parallel <- function(key) {
  # Read inside worker — only loaded once per worker process via OS file cache
  spid_all   <- readRDS(tmp_spid_all)
  spid_geoms <- readRDS(tmp_spid_geoms)

  allowed <- allowed_codes_by_key[[key]]
  if (is.null(allowed)) return(NULL)

  skip_codes <- union(dropped_codes, setdiff(unique(spid_all$geo_code), allowed))

  tryCatch(
    process_survey(key, spid_all, spid_geoms, admin0_list, skip_codes),
    error = function(e) {
      message("Error processing key: ", key, "\n  ", conditionMessage(e))
      NULL
    }
  )
}

n_workers <- max(1L, parallel::detectCores() - 2L)
plan(multisession, workers = n_workers)
message("Running with ", n_workers, " parallel workers.")

em_list <- future_map(
  unique_keys,
  process_survey_parallel,
  .progress = TRUE,
  .options  = furrr_options(
    seed     = TRUE,
    globals  = c("tmp_spid_all", "tmp_spid_geoms", "admin0_list",
                 "allowed_codes_by_key", "dropped_codes", "process_survey"),
    packages = c("sf", "lwgeom", "dplyr", "units")
  )
)

plan(sequential)

# Clean up temp files
unlink(c(tmp_spid_all, tmp_spid_geoms))

# Combine, dropping NULLs
spid_em <- bind_rows(Filter(Negate(is.null), em_list))

#------------------------------------------------------------------------------#
# Clean up
#------------------------------------------------------------------------------#

# Guard against the case where all surveys failed
if (nrow(spid_em) == 0 || !"geo_code" %in% names(spid_em)) {
  stop("Edge matching produced no results. Review the error messages above.")
}

# Keep first occurrence of each unique geo_code (most recent survey wins)
spid_em <- distinct(spid_em, geo_code, .keep_all = TRUE) |>
  left_join(st_drop_geometry(spid_subnat), by = "geo_code")

# Regions cropped out entirely by admin-0 intersection
dropped2 <- filter(spid_subnat, !geo_code %in% spid_em$geo_code)
dropped2

# Add dropped regions back (some have samples) and standardize column order
spid_em <- bind_rows(spid_em, dropped2) |>
  select(code, geo_year, geo_source, geo_level, geo_idvar, geo_id,
         geo_nvar, geo_name, geo_code) |>
  filter(st_geometry_type(geom) %in% c("POLYGON", "MULTIPOLYGON")) |>
  arrange(geo_code)
spid_em

# Check duplicates
any(duplicated(spid_em$geo_code))   # FALSE = no duplicates

# Check validity
any(!st_is_valid(spid_em))          # FALSE = all valid

# Save outputs
st_write(spid_em, paste0(path_final, "_subnat_em.gpkg"), append = FALSE)
st_write(spid_em, paste0(path_final, "_subnat_em.shp"),  append = FALSE)

#------------------------------------------------------------------------------#
# Checks
#------------------------------------------------------------------------------#

length(unique(spid_all$geo_code))

surv_list <- mutate(spid_bounds, key = paste(code, year, survname))
length(unique(surv_list$key))

nrow(spid_em[spid_em$geo_code %in% spid_bounds$geo_code, ])
length(unique(spid_bounds$geo_code))

length(unique(spid_em$code))
length(unique(spid_bounds$code))

# AM24 vintage: 2156 subnat regions with data from 1113 surveys in 138 countries
# SM25 vintage: 2261 subnat regions with data from 1243 surveys in 143 countries
# AM25 vintage: 2270 subnat regions with data from 1288 surveys in 143 countries
# SM26 vintage: 2354 subnat regions with data from 1318 surveys in 144 countries
