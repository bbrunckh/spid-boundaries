# edge_match.R
#
# Exportable edge-matching function using Voronoi gap-fill.
#
# Usage (from another script):
#   source("edge_match.R")
#   result <- edge_match(target = admin0_sf, subregions = subnat_sf)
#
# Arguments:
#   target      <sf> Single (pre-unioned) polygon — the national boundary to
#               clip to. Must be in any CRS; reprojected internally to EPSG:3395.
#   subregions  <sf> Subnational polygons to edge-match. Must contain at least
#               one polygon column. Any CRS accepted.
#   id_col      <chr> Name of the unique identifier column in `subregions`.
#               Default: "geo_code".
#   seg_length  <units> Max segment length for boundary densification.
#               Default: units::set_units(500, "m"). Decrease for finer
#               boundaries; increase for speed on large regions.
#   snap_grid   <numeric> Rounding precision (metres) for point deduplication.
#               Default: 10.
#   verbose     <logical> Print progress messages. Default: TRUE.
#
# Returns:
#   An sf object with the same rows and columns as `subregions`, with
#   geometry replaced by edge-matched polygons clipped to `target`.
#   Regions that could not be matched (e.g. entirely outside `target`)
#   are returned with their original geometry and a warning.
#   A logical column `em_ok` is added: TRUE = successfully edge-matched.

library(sf)
library(dplyr)
library(units)

edge_match <- function(
    target,
    subregions,
    id_col     = "geo_code",
    seg_length = set_units(500, "m"),
    snap_grid  = 10,
    verbose    = TRUE
) {

  # ── 0. Input validation ────────────────────────────────────────────────────

  stopifnot(
    "`target` must be an sf object"      = inherits(target, "sf"),
    "`subregions` must be an sf object"  = inherits(subregions, "sf"),
    "`id_col` must exist in subregions"  = id_col %in% names(subregions),
    "No duplicate IDs allowed"           = !anyDuplicated(subregions[[id_col]])
  )

  msg <- function(...) if (verbose) message("[edge_match] ", ...)

  # ── 1. Reproject everything to EPSG:3395 (metre-based, global) ────────────

  msg("Reprojecting to EPSG:3395...")
  sf_use_s2(FALSE)

  target_proj <- target |>
    st_union() |>
    st_make_valid() |>
    st_transform(3395) |>
    st_make_valid()

  sub_proj <- subregions |>
    st_make_valid() |>
    st_transform(3395) |>
    st_make_valid()

  n <- nrow(sub_proj)
  msg(n, " subregions loaded.")

  # ── 2. Extract interior shared boundaries ─────────────────────────────────

  msg("Extracting shared boundaries...")

  samp_union <- st_union(sub_proj) |> st_make_valid()
  outline    <- st_boundary(samp_union)

  lines <- suppressWarnings(
    st_intersection(sub_proj, outline) |>
      st_collection_extract("LINESTRING") |>
      st_make_valid()
  )

  if (nrow(lines) == 0) {
    warning("[edge_match] No shared boundaries found. Returning original geometry.")
    return(mutate(subregions, em_ok = FALSE))
  }

  # ── 3. Densify boundaries → deduplicated point cloud ──────────────────────

  msg("Densifying and deduplicating boundary points...")

  pts_raw <- suppressWarnings(
    lines |>
      st_segmentize(dfMaxLength = seg_length) |>
      st_cast("MULTIPOINT") |>
      st_cast("POINT")
  )

  # Round to snap_grid metres and deduplicate via coordinate matrix
  pts_matrix <- round(st_coordinates(pts_raw), -log10(snap_grid))
  pts_matrix <- pts_matrix[!duplicated(pts_matrix), , drop = FALSE]

  if (nrow(pts_matrix) < 4) {
    warning("[edge_match] Too few unique boundary points. Returning original geometry.")
    return(mutate(subregions, em_ok = FALSE))
  }

  points_proj <- st_as_sf(
    as.data.frame(pts_matrix[, c("X", "Y")]),
    coords = c("X", "Y"),
    crs    = 3395
  )

  msg(nrow(points_proj), " unique boundary points.")

  # ── 4. Voronoi tessellation ────────────────────────────────────────────────

  msg("Building Voronoi tessellation...")

  voron <- tryCatch(
    st_collection_extract(st_voronoi(st_combine(points_proj))) |>
      st_set_crs(3395) |>
      st_make_valid(),
    error = function(e) {
      warning("[edge_match] Voronoi failed: ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(voron)) {
    return(mutate(subregions, em_ok = FALSE))
  }

  voron_sf  <- st_sf(geom = voron)
  voron_idx <- vapply(
    st_intersects(points_proj, voron_sf),
    function(x) if (length(x) == 0L) NA_integer_ else x[[1L]],
    integer(1L)
  )

  valid_pts   <- !is.na(voron_idx)
  points_proj <- points_proj[valid_pts, ]
  voron_idx   <- voron_idx[valid_pts]

  if (!any(valid_pts)) {
    warning("[edge_match] No points intersected Voronoi cells.")
    return(mutate(subregions, em_ok = FALSE))
  }

  # Assign each point's id via nearest feature (back to sub_proj polygons)
  points_proj[[id_col]] <- sub_proj[[id_col]][
    st_nearest_feature(points_proj, sub_proj)
  ]

  # ── 5. Gap-fill: union Voronoi cells per id, then absorb gaps ─────────────
  #
  # FIX: previously used mutate(geom = voron[voron_idx]) which left the active
  # geometry as the original points.  All downstream spatial operations were
  # therefore running on points, not on the Voronoi polygons.  We now build a
  # fresh sf object with the Voronoi polygons as the active geometry column
  # before any grouping or set operations.
  #
  # FIX: bind_rows requires both inputs to share the same geometry column name.
  # We normalise both to "geometry" before binding.

  msg("Filling gaps with Voronoi cells...")

  em_prediff <- tryCatch({

    # Build an sf whose active geometry is the Voronoi polygon for each point.
    voron_pts_sf <- st_sf(
      setNames(list(points_proj[[id_col]]), id_col),
      geometry = voron[voron_idx],   # <-- Voronoi polygons as active geometry
      crs = 3395
    )

    # Union Voronoi cells per region id.
    voron_per_id <- voron_pts_sf |>
      group_by(.data[[id_col]]) |>
      summarize(geometry = st_union(geometry), .groups = "drop") |>
      st_make_valid()

    # Subtract the existing subregion union → only the true gap portions remain.
    gap_fill <- st_difference(voron_per_id, samp_union) |>
      st_make_valid()

    # Normalise the geometry column name in sub_proj before binding.
    sub_geom_col <- attr(sub_proj, "sf_column")
    sub_for_bind <- sub_proj |>
      select(all_of(id_col)) |>
      rename(geometry = !!sub_geom_col)

    # Combine each region's original polygon with its gap-fill slivers, then
    # union per id to produce a single, seamless polygon per region.
    bind_rows(sub_for_bind, gap_fill) |>
      group_by(.data[[id_col]]) |>
      summarize(geometry = st_union(geometry), .groups = "drop") |>
      st_make_valid()

  }, error = function(e) {
    warning("[edge_match] Gap-fill failed: ", conditionMessage(e))
    NULL
  })

  if (is.null(em_prediff) || nrow(em_prediff) == 0) {
    return(mutate(subregions, em_ok = FALSE))
  }

  # ── 6. Clip to target boundary ────────────────────────────────────────────

  msg("Clipping to target boundary...")

  em_clipped <- tryCatch(
    st_intersection(em_prediff, target_proj) |> st_make_valid(),
    error = function(e) {
      warning("[edge_match] Clip to target failed: ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(em_clipped) || nrow(em_clipped) == 0) {
    return(mutate(subregions, em_ok = FALSE))
  }

  # ── 7. Extract polygons; recover any ids lost in collection extraction ─────

  em_poly <- em_clipped |>
    st_collection_extract("POLYGON") |>
    st_make_valid() |>
    group_by(.data[[id_col]]) |>
    summarize(geometry = st_union(geometry), .groups = "drop")

  # Recover ids dropped by collection extraction using pre-clip geometry.
  missing_ids <- setdiff(em_prediff[[id_col]], em_poly[[id_col]])

  if (length(missing_ids) > 0) {
    msg(length(missing_ids), " id(s) lost in collection extraction — recovering...")

    recovered <- em_prediff |>
      filter(.data[[id_col]] %in% missing_ids) |>
      st_intersection(target_proj) |>
      st_collection_extract("POLYGON") |>
      st_make_valid() |>
      group_by(.data[[id_col]]) |>
      summarize(geometry = st_union(geometry), .groups = "drop")

    em_poly <- bind_rows(em_poly, recovered)
  }

  # ── 8. Reproject to WGS84 and rejoin attribute columns ────────────────────

  msg("Rejoining attributes and reprojecting to EPSG:4326...")

  em_wgs84 <- st_transform(em_poly, 4326)

  matched_ids <- em_wgs84[[id_col]]

  attrs <- st_drop_geometry(subregions)

  result <- attrs |>
    left_join(em_wgs84, by = id_col) |>
    mutate(em_ok = .data[[id_col]] %in% matched_ids) |>
    st_as_sf()

  failed_ids <- subregions[[id_col]][!subregions[[id_col]] %in% matched_ids]

  if (length(failed_ids) > 0) {
    warning(
      "[edge_match] ", length(failed_ids),
      " region(s) could not be edge-matched and retain original geometry: ",
      paste(failed_ids, collapse = ", ")
    )
    original_geom   <- st_geometry(subregions)[subregions[[id_col]] %in% failed_ids]
    failed_rows_idx <- which(result[[id_col]] %in% failed_ids)
    st_geometry(result)[failed_rows_idx] <- st_transform(
      original_geom,
      st_crs(result)
    )
  }

  # Final validity check with repair
  invalid <- !st_is_valid(result)
  if (any(invalid)) {
    msg("Repairing ", sum(invalid), " invalid geometry(ies) in output...")
    st_geometry(result)[invalid] <- st_make_valid(st_geometry(result)[invalid])
  }

  msg("Done. ", sum(result$em_ok), "/", nrow(result), " regions edge-matched successfully.")

  result
}
