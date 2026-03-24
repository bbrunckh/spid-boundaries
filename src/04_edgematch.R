rm(list = setdiff(ls(), c("spid_master","vintage","spid_data","version"))) 
gc()

library(sf)
library(lwgeom)
library(openxlsx2)
library(dplyr)
library(furrr)

#------------------------------------------------------------------------------#
# SPID boundary IDs
#------------------------------------------------------------------------------#

spid_bounds <- read_xlsx(spid_master, sheet = "SPID boundaries") |>
  filter(!is.na(geo_code))

spid_miss <- read_xlsx(spid_master, sheet = "SPID missing boundaries")

#------------------------------------------------------------------------------#
# admin-0 and subnational boundaries
#------------------------------------------------------------------------------#

admin0 <- st_read(paste0(spid_data,"final/",version,"/",
                         tolower(vintage),"_admin0.gpkg"))

spid_subnat <- st_read(paste0(spid_data,"interim/",version,"/",
                              tolower(vintage),"_subnat.gpkg"))

dropped <- read_xlsx(paste0(spid_data,"interim/",version,
                            "/subnat_dropped.xlsx"))

#------------------------------------------------------------------------------#
# Edge match subnational boundaries to admin-0 - Voronoi method
#------------------------------------------------------------------------------#

spid_all <- bind_rows(spid_bounds, spid_miss) |>
  mutate(key = paste(code, year, survname, byvar)) |>
  arrange(key)

spid_list <- rev(unique(spid_all$key))

sf_use_s2(FALSE)

# Pre-index admin-0 by 3-letter country code
admin0_list <- split(admin0, substr(admin0$geo_code, 1, 3))

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
# Per-survey processing function
#------------------------------------------------------------------------------#

process_survey <- function(key, spid_all, spid_subnat, admin0_list, skip_codes) {

  # --- Build sample ---
  sample <- filter(spid_all, key == !!key) |>
    left_join(select(spid_subnat, geo_code, geom), by = "geo_code") |>
    select(geo_code, geom) |>
    st_as_sf() |>
    filter(!is.na(st_dimension(geom))) |>  # drop NA geometry rows
    st_make_valid()

  sample <- filter(sample, !geo_code %in% skip_codes)
  if (nrow(sample) == 0) return(NULL)

  # --- Target admin-0 ---
  cty    <- substr(key, 1, 3)
  target <- admin0_list[[cty]]
  if (is.null(target)) return(NULL)
  target <- st_union(target) |> st_make_valid()

  # --- Lines from polygon boundaries ---
  samp_union <- st_union(sample) |> st_make_valid()
  outline    <- st_cast(samp_union, "MULTILINESTRING")

  lines <- suppressWarnings(
    st_intersection(sample, outline) |>
      st_collection_extract("LINESTRING") |>
      st_make_valid()
  )
  if (nrow(lines) == 0) return(NULL)

  # --- Points: segmentize and snap — stay in EPSG:3395 throughout ---
  points_proj <- suppressWarnings(
    lines |>
      st_transform(3395) |>
      st_segmentize(dfMaxLength = units::set_units(100, m)) |>
      st_cast("MULTIPOINT") |>
      st_cast("POINT") |>
      st_snap_to_grid(units::set_units(10, m)) |>
      st_make_valid()
  )

  pts_coords  <- st_coordinates(points_proj)
  points_proj <- points_proj[!duplicated(pts_coords), ]
  if (nrow(points_proj) < 4) return(NULL)

  # --- Voronoi in EPSG:3395 ---
  voron <- tryCatch(
    st_collection_extract(st_voronoi(st_combine(points_proj))) |>
      st_set_crs(st_crs(points_proj)) |>
      st_make_valid(),
    error = function(e) {
      message("  Voronoi failed for: ", key, "\n  ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(voron)) return(NULL)

  voron_sf  <- st_sf(geom = voron)
  voron_idx <- st_intersects(points_proj, voron_sf)
  voron_idx <- sapply(
    voron_idx,
    function(x) if (length(x) == 0L) NA_integer_ else x[[1L]]
  )

  valid_pts   <- !is.na(voron_idx)
  if (!any(valid_pts)) return(NULL)
  points_proj <- points_proj[valid_pts, ]
  voron_idx   <- voron_idx[valid_pts]

  # --- Project all layers ---
  sample_proj     <- st_transform(sample, 3395)     |> st_make_valid()
  samp_union_proj <- st_transform(samp_union, 3395) |> st_make_valid()
  target_proj     <- st_transform(target, 3395)     |> st_make_valid()

  # --- Assign Voronoi cells and build gap-filled polygons ---
  em_prediff <- tryCatch({
    mutate(points_proj, geom = voron[voron_idx]) |>
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

  em <- st_transform(em_poly, 4326)  |> # reproject to WGS84
    filter(st_geometry_type(geom) %in% c("POLYGON", "MULTIPOLYGON"))

  if (nrow(em) == 0 || !"geo_code" %in% names(em)) return(NULL)
  return(em)
}

#------------------------------------------------------------------------------#
# Parallel wrapper
#------------------------------------------------------------------------------#

process_survey_parallel <- function(key) {
  allowed <- allowed_codes_by_key[[key]]
  if (is.null(allowed)) return(NULL)

  skip_codes <- union(dropped$geo_code,
                      setdiff(spid_all$geo_code, allowed))

  tryCatch(
    process_survey(key, spid_all, spid_subnat, admin0_list, skip_codes),
    error = function(e) {
      message("Error processing key: ", key, "\n  ", conditionMessage(e))
      NULL
    }
  )
}

#------------------------------------------------------------------------------#
# Run — parallel across surveys
#------------------------------------------------------------------------------#

n_workers <- max(1L, parallel::detectCores() - 1L)
plan(multisession, workers = n_workers)
message("Running with ", n_workers, " parallel workers.")

em_list <- future_map(
  spid_list,
  process_survey_parallel,
  .progress = TRUE,
  .options  = furrr_options(seed = TRUE)
)

plan(sequential)

# Combine, dropping NULLs
spid_em <- bind_rows(Filter(Negate(is.null), em_list))

#------------------------------------------------------------------------------#
# Clean up
#------------------------------------------------------------------------------#

# FIX 4: guard against the case where all surveys failed
if (nrow(spid_em) == 0 || !"geo_code" %in% names(spid_em)) {
  stop("Edge matching produced no results. Review the error messages above.")
}

# Keep first occurrence of each unique geo_code (most recent survey wins)
spid_em <- distinct(spid_em, geo_code, .keep_all = TRUE) |>
  left_join(st_drop_geometry(spid_subnat), by = "geo_code")

# Regions cropped out entirely by admin-0 intersection
dropped2 <- filter(spid_subnat, !geo_code %in% spid_em$geo_code)
dropped2

# Add dropped regions back (some have samples)
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

# Save EM geopackage
st_write(spid_em,
         paste0(spid_data,"final/",version,"/",tolower(vintage),"_subnat_em.gpkg"),
         append = FALSE)

# Save EM shapefile
st_write(spid_em,
         paste0(spid_data,"final/",version,"/",tolower(vintage),"_subnat_em.shp"),
         append = FALSE)

#------------------------------------------------------------------------------#
# Checks
#------------------------------------------------------------------------------#

length(unique(spid_all$geo_code))

surv_list <- mutate(spid_bounds, key = paste(code, year, survname))
length(unique(surv_list$key))

nrow(spid_em[spid_em$geo_code %in% spid_bounds$geo_code,])
length(unique(spid_bounds$geo_code))

length(unique(spid_em$code))
length(unique(spid_bounds$code))

# AM24 vintage: 2156 subnat regions with data from 1113 surveys in 138 countries
# SM25 vintage: 2261 subnat regions with data from 1243 surveys in 143 countries
# AM25 vintage: 2270 subnat regions with data from 1288 surveys in 143 countries
# SM26 vintage: 2354 subnat regions with data from 1318 surveys in 144 countries
