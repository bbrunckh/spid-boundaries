# run_edge_matching.R
#
# Orchestration script: loads survey/boundary data, deduplicates keys by
# recency, runs edge_match() in parallel over unique geo_code sets, and
# writes outputs. Expects these variables to exist in the environment already:
#   spid_master  <chr> path to the master .xlsx
#   spid_data    <chr> root data directory
#   version      <chr> e.g. "2020-01-01"
#   vintage      <chr> e.g. "AM25"

rm(list = setdiff(ls(), c("spid_master", "spid_data", "version", "vintage")))
gc()

library(sf)
library(openxlsx2)
library(dplyr)
library(furrr)
library(units)

source("src/fct_edge_match.R")   # loads edge_match()

sf_use_s2(FALSE)

# ── Paths ────────────────────────────────────────────────────────────────────

path_final   <- paste0(spid_data, "final/",   version, "/", tolower(vintage))
path_interim <- paste0(spid_data, "interim/", version, "/", tolower(vintage))

# ── Load boundary metadata ───────────────────────────────────────────────────

spid_bounds <- read_xlsx(spid_master, sheet = "SPID boundaries") |>
  filter(!is.na(geo_code))

spid_miss <- read_xlsx(spid_master, sheet = "SPID missing boundaries")

spid_all <- bind_rows(spid_bounds, spid_miss) |>
  mutate(key = paste(code, year, survname, byvar)) |>
  arrange(key)

# ── Load spatial data ────────────────────────────────────────────────────────

admin0      <- st_read(paste0(path_final,   "_admin0.gpkg"))
spid_subnat <- st_read(paste0(path_interim, "_subnat.gpkg"))
dropped     <- read_xlsx(paste0(spid_data, "interim/", version, "/subnat_dropped.xlsx"))

dropped_codes <- dropped$geo_code

# ── Build compact geometry lookup (deduplicate AFTER make_valid) ─────────────

spid_geoms <- spid_subnat |>
  select(geo_code, geom) |>
  distinct(geo_code, .keep_all = TRUE) |>   # order-stable; deduplicate first
  st_make_valid()                            # then validate only kept rows

stopifnot("Duplicate geo_codes in spid_geoms" = !anyDuplicated(spid_geoms$geo_code))

# ── Assign each geo_code to its most recent survey key ──────────────────────
#
# spid_list is reversed-chronological so element [[1]] is the most recent key.
# arrange(match(...)) sorts by that order; distinct() keeps the first = most
# recent. This dependency is explicit here rather than implicit in rev().

spid_list <- rev(unique(spid_all$key))

geo_code_first_key <- spid_all |>
  arrange(match(key, spid_list)) |>     # most-recent key first
  distinct(geo_code, .keep_all = TRUE) |>
  select(geo_code, key)

allowed_codes_by_key <- geo_code_first_key |>
  group_by(key) |>
  summarize(allowed = list(geo_code), .groups = "drop") |>
  tibble::deframe()

# ── Pre-index admin-0 by 3-letter country code ───────────────────────────────

admin0_list <- split(admin0, substr(admin0$geo_code, 1, 3)) |>
  lapply(function(x) {
    st_union(x) |>
      st_make_valid() |>
      st_transform(3395) |>
      st_make_valid() |>
      st_as_sf()   # edge_match() expects an sf object for `target`
  })

# ── Fingerprint: skip keys whose geo_code set is identical to a newer key ────

key_signatures <- tibble::enframe(
  lapply(allowed_codes_by_key, function(codes) {
    effective <- setdiff(codes, dropped_codes)
    if (length(effective) == 0L) return(NA_character_)
    paste(sort(effective), collapse = "|")
  }),
  name  = "key",
  value = "sig"
) |>
  filter(!is.na(sig), key %in% spid_list)

unique_keys <- key_signatures |>
  arrange(match(key, spid_list)) |>
  distinct(sig, .keep_all = TRUE) |>
  pull(key)

message(
  length(spid_list), " total keys -> ",
  length(unique_keys), " unique geo_code sets (",
  length(spid_list) - length(unique_keys), " skipped)"
)

# ── Write shared data to disk; workers read from file cache ──────────────────

tmp_spid_all   <- tempfile(fileext = ".rds")
tmp_spid_geoms <- tempfile(fileext = ".rds")
saveRDS(spid_all,   tmp_spid_all)
saveRDS(spid_geoms, tmp_spid_geoms)

# Guarantee cleanup even if future_map() throws
on.exit(unlink(c(tmp_spid_all, tmp_spid_geoms)), add = TRUE)

# ── Per-key worker function ───────────────────────────────────────────────────

process_key <- function(key) {

  spid_all   <- readRDS(tmp_spid_all)
  spid_geoms <- readRDS(tmp_spid_geoms)

  allowed <- allowed_codes_by_key[[key]]
  if (is.null(allowed)) return(NULL)

  # Codes this key is responsible for (after removing dropped + claimed codes)
  active_codes <- setdiff(allowed, dropped_codes)
  if (length(active_codes) == 0L) return(NULL)

  # Build subregions sf for this key
  subregions <- spid_all |>
    filter(key == !!key, geo_code %in% active_codes) |>
    select(geo_code) |>
    left_join(spid_geoms, by = "geo_code") |>
    st_as_sf() |>
    filter(!is.na(st_dimension(geom))) |>
    st_make_valid()

  if (nrow(subregions) == 0) return(NULL)

  # Target: pre-built admin-0 for this country (already sf)
  cty    <- substr(key, 1, 3)
  target <- admin0_list[[cty]]
  if (is.null(target)) return(NULL)

  tryCatch(
    edge_match(
      target     = target,
      subregions = subregions,
      id_col     = "geo_code",
      seg_length = set_units(500, "m"),
      verbose    = FALSE
    ),
    error = function(e) {
      message("Error on key ", key, ": ", conditionMessage(e))
      NULL
    }
  )
}

# ── Parallel execution ────────────────────────────────────────────────────────

n_workers <- max(1L, parallel::detectCores() - 2L)
plan(multisession, workers = n_workers)
message("Running with ", n_workers, " workers...")

em_list <- future_map(
  unique_keys,
  process_key,
  .progress = TRUE,
  .options  = furrr_options(
    seed    = TRUE,
    globals = c(
      "tmp_spid_all", "tmp_spid_geoms", "admin0_list",
      "allowed_codes_by_key", "dropped_codes", "edge_match"
    ),
    packages = c("sf", "dplyr", "units")
  )
)

plan(sequential)

# ── Combine results ───────────────────────────────────────────────────────────

spid_em <- bind_rows(Filter(Negate(is.null), em_list))

if (nrow(spid_em) == 0 || !"geo_code" %in% names(spid_em)) {
  stop("Edge matching produced no results. Review error messages above.")
}

# Most recent survey wins for any geo_code appearing in multiple results
spid_em <- distinct(spid_em, geo_code, .keep_all = TRUE) |>
  left_join(st_drop_geometry(spid_subnat), by = "geo_code")

# Regions cropped out entirely by admin-0 intersection — add back with warning
dropped2 <- filter(spid_subnat, !geo_code %in% spid_em$geo_code)

if (nrow(dropped2) > 0) {
  message(nrow(dropped2), " region(s) not in edge-matched output; appending original geometry.")
  spid_em <- bind_rows(spid_em, dropped2)
}

spid_em <- spid_em |>
  select(code, geo_year, geo_source, geo_level, geo_idvar, geo_id,
         geo_nvar, geo_name, geo_code) |>
  filter(st_geometry_type(geom) %in% c("POLYGON", "MULTIPOLYGON")) |>
  arrange(geo_code)

# ── Final assertions ──────────────────────────────────────────────────────────

if (anyDuplicated(spid_em$geo_code)) {
  stop("Duplicate geo_codes in final output — investigate.")
}

invalid_geoms <- !st_is_valid(spid_em)
if (any(invalid_geoms)) {
  message("Repairing ", sum(invalid_geoms), " invalid geometries in final output...")
  st_geometry(spid_em)[invalid_geoms] <- st_make_valid(
    st_geometry(spid_em)[invalid_geoms]
  )
  stopifnot("Geometry repair failed" = all(st_is_valid(spid_em)))
}

# ── Write outputs ─────────────────────────────────────────────────────────────

st_write(spid_em, paste0(path_final, "_subnat_em.gpkg"), append = FALSE)
st_write(spid_em, paste0(path_final, "_subnat_em.shp"),  append = FALSE)

message("Wrote ", nrow(spid_em), " regions to ", path_final, "_subnat_em.gpkg/.shp")

# ── Summary counts ────────────────────────────────────────────────────────────

message(
  "\nSummary:",
  "\n  Unique geo_codes in input:    ", length(unique(spid_all$geo_code)),
  "\n  Unique survey keys processed: ", length(unique_keys),
  "\n  Regions in output:            ", nrow(spid_em),
  "\n  Countries:                    ", length(unique(spid_em$code))
)