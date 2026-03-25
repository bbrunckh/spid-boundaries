rm(list = setdiff(ls(), c("spid_master","vintage","spid_data","version"))) 
gc()

library(sf)
library(leaflet)
library(dplyr)
library(units)
library(openxlsx2)


#------------------------------------------------------------------------------#
# 0. Load data 
#------------------------------------------------------------------------------#

spid_em <- st_read(paste0(spid_data,"final/",version,"/",
                          tolower(vintage),"_subnat_em.gpkg"))

admin0 <- st_read(paste0(spid_data,"final/",version,"/",
                         tolower(vintage),"_admin0.gpkg"))

spid_bounds <- read_xlsx(spid_master, sheet = "SPID boundaries") |>
  filter(!is.na(geo_code))

spid_miss <- read_xlsx(spid_master, sheet = "SPID missing boundaries")

spid_all <- bind_rows(spid_bounds, spid_miss) |>
  mutate(key = paste(code, year, survname, byvar)) |>
  arrange(key)

sf_use_s2(FALSE)

cat("\n====================================================\n")
cat(" SPID Edge-Match Output Diagnostics\n")
cat("====================================================\n\n")

#------------------------------------------------------------------------------#
# 1. Basic counts
#------------------------------------------------------------------------------#

cat("--- 1. Basic counts ---\n")

n_regions  <- nrow(spid_em)
n_countries <- length(unique(spid_em$code))
n_with_data <- nrow(spid_em[spid_em$geo_code %in% spid_bounds$geo_code, ])
n_missing   <- nrow(spid_em[spid_em$geo_code %in% spid_miss$geo_code, ])

cat("Total subnational regions:          ", n_regions,   "\n")
cat("Regions with survey data:           ", n_with_data, "\n")
cat("Regions from missing-boundary list: ", n_missing,   "\n")
cat("Countries represented:              ", n_countries, "\n\n")

#------------------------------------------------------------------------------#
# 2. Geometry validity
#------------------------------------------------------------------------------#

cat("--- 2. Geometry validity ---\n")

invalid <- !st_is_valid(spid_em)
cat("Invalid geometries:  ", sum(invalid), "\n")
if (any(invalid)) {
  cat("  geo_codes with invalid geometry:\n")
  print(spid_em$geo_code[invalid])
}

empty <- st_is_empty(spid_em)
cat("Empty geometries:    ", sum(empty), "\n")
if (any(empty)) {
  cat("  geo_codes with empty geometry:\n")
  print(spid_em$geo_code[empty])
}
cat("\n")

#------------------------------------------------------------------------------#
# 3. Duplicate geo_codes
#------------------------------------------------------------------------------#

cat("--- 3. Duplicate geo_codes ---\n")

dups <- spid_em$geo_code[duplicated(spid_em$geo_code)]
cat("Duplicate geo_codes: ", length(dups), "\n")
if (length(dups) > 0) print(dups)
cat("\n")

#------------------------------------------------------------------------------#
# 4. Area checks — flag suspiciously small or large polygons
#------------------------------------------------------------------------------#

cat("--- 4. Area checks ---\n")

spid_em <- spid_em |>
  mutate(area_km2 = as.numeric(set_units(st_area(geom), km^2)))

area_summary <- summary(spid_em$area_km2)
print(area_summary)

# Threshold: flag regions smaller than 1 km² (likely slivers from edge matching)
tiny <- filter(spid_em, area_km2 < 1)
cat("\nRegions smaller than 1 km² (possible slivers): ", nrow(tiny), "\n")
if (nrow(tiny) > 0) print(select(st_drop_geometry(tiny), geo_code, code, area_km2))

# Flag regions larger than 2,000,000 km² (sanity check — larger than Greenland)
huge <- filter(spid_em, area_km2 > 2e6)
cat("Regions larger than 2,000,000 km²:              ", nrow(huge), "\n")
if (nrow(huge) > 0) print(select(st_drop_geometry(huge), geo_code, code, area_km2))
cat("\n")

#------------------------------------------------------------------------------#
# 5. Coverage check — does the union of subnational regions fill admin-0?
#    Gaps larger than 1 km² are flagged per country.
#------------------------------------------------------------------------------#

cat("--- 5. Coverage gaps vs admin-0 ---\n")

country_codes <- unique(spid_em$code)
gap_report    <- data.frame()

for (cty in country_codes) {

  # suppress warnings from st_difference (e.g. due to topology issues) but continue processing

  
  subnat_cty <- filter(spid_em, code == cty)
  admin_cty  <- filter(admin0,
                        geo_code == paste0(cty, "_2025_WB0")) |> select(geom)
  
  if (nrow(admin_cty) == 0) next
  
  gap <- tryCatch(
    st_difference(admin_cty, st_union(subnat_cty)),
    error = function(e) NULL
  )
  
  if (is.null(gap) || nrow(gap) == 0 || st_is_empty(gap)) next
  
  gap_area <- as.numeric(set_units(st_area(gap), km^2))
  
  if (gap_area > 1) {
    gap_report <- bind_rows(
      gap_report,
      data.frame(code = cty, gap_area_km2 = round(gap_area, 2))
    )
  }
}

if (nrow(gap_report) == 0) {
  cat("No coverage gaps > 1 km² detected. Edge matching looks complete.\n")
} else {
  cat("Countries with coverage gaps > 1 km²:\n")
  print(arrange(gap_report, desc(gap_area_km2)))
}
cat("\n")

#------------------------------------------------------------------------------#
# 6. Overlap check — do any subnational polygons overlap each other within
#    the same country? Overlaps indicate a topology problem.
#    Only flag pairs that share at least one survey key (code/year/survname/byvar).
#------------------------------------------------------------------------------#

cat("--- 6. Intra-country overlaps ---\n")

# Lookup: geo_code -> unique survey keys it appears in
geo_keys <- spid_all |>
  select(geo_code, key) |>
  filter(!is.na(geo_code))

overlap_report <- data.frame()

for (cty in country_codes) {
  
  subnat_cty <- filter(spid_em, code == cty)
  if (nrow(subnat_cty) < 2) next
  
  # Self-intersection matrix; suppress diagonal and upper triangle
  suppressWarnings(
    inter_mat <- st_intersects(subnat_cty, subnat_cty, sparse = FALSE)
  )
  diag(inter_mat) <- FALSE
  inter_mat[upper.tri(inter_mat)] <- FALSE
  
  pairs <- which(inter_mat, arr.ind = TRUE)
  
  if (nrow(pairs) == 0) next
  
  # Only flag pairs with genuine area overlap (not just shared boundaries)
  for (p in 1:nrow(pairs)) {
    a <- pairs[p, 1]; b <- pairs[p, 2]
    
    gc_a <- subnat_cty$geo_code[a]
    gc_b <- subnat_cty$geo_code[b]
    
    # Skip if the two regions share no survey key
    keys_a <- geo_keys$key[geo_keys$geo_code == gc_a]
    keys_b <- geo_keys$key[geo_keys$geo_code == gc_b]
    if (length(intersect(keys_a, keys_b)) == 0) next
    
    overlap_geom <- tryCatch(
      suppressWarnings(st_intersection(subnat_cty$geom[a], subnat_cty$geom[b])),
      error = function(e) NULL
    )
    if (is.null(overlap_geom) || st_is_empty(overlap_geom)) next
    
    overlap_type <- st_geometry_type(overlap_geom)
    if (!overlap_type %in% c("POLYGON","MULTIPOLYGON")) next
    
    overlap_area <- as.numeric(set_units(st_area(overlap_geom), km^2))
    if (overlap_area > 0.01) {  # ignore sub-10m² numerical noise
      overlap_report <- bind_rows(overlap_report, data.frame(
        code        = cty,
        geo_code_a  = gc_a,
        geo_code_b  = gc_b,
        overlap_km2 = round(overlap_area, 4)
      ))
    }
  }
}

if (nrow(overlap_report) == 0) {
  cat("No polygon overlaps detected.\n")
} else {
  cat("Overlapping polygon pairs:\n")
  print(arrange(overlap_report, desc(overlap_km2)))
}
cat("\n")

#------------------------------------------------------------------------------#
# 7. Attribute completeness — check for NAs in key fields
#------------------------------------------------------------------------------#

cat("--- 7. Attribute completeness ---\n")

key_fields <- c("code","geo_year","geo_source","geo_level",
                "geo_id","geo_name","geo_code")

for (field in key_fields) {
  n_na <- sum(is.na(spid_em[[field]]))
  flag <- if (n_na > 0) " <<< CHECK" else ""
  cat(sprintf("  %-15s  NA count: %d%s\n", field, n_na, flag))
}
cat("\n")

#------------------------------------------------------------------------------#
# 8. geo_code format check — expected pattern: XXX_YYYY_...
#------------------------------------------------------------------------------#

cat("--- 8. geo_code format ---\n")

bad_format <- spid_em$geo_code[!grepl("^[A-Z]{3}_\\d{4}_", spid_em$geo_code)]
cat("geo_codes not matching expected pattern (XXX_YYYY_...): ",
    length(bad_format), "\n")
if (length(bad_format) > 0) print(bad_format)
cat("\n")

#------------------------------------------------------------------------------#
# 9. Country-level region count summary
#------------------------------------------------------------------------------#

cat("--- 9. Regions per country ---\n")

region_counts <- spid_em |>
  st_drop_geometry() |>
  group_by(code) |>
  summarize(
    n_regions   = n(),
    n_with_data = sum(geo_code %in% spid_bounds$geo_code),
    .groups = "drop"
  ) |>
  arrange(desc(n_regions))

cat("Top 10 countries by region count:\n")
print(head(region_counts, 10))

cat("\nCountries with only 1 subnational region (possible issue):\n")
print(filter(region_counts, n_regions == 1))
cat("\n")

#------------------------------------------------------------------------------#
# 10. CRS check
#------------------------------------------------------------------------------#

cat("--- 10. CRS ---\n")
cat("CRS of spid_em: ", st_crs(spid_em)$input, "\n")
cat("Expected:        EPSG:4326\n")
if (st_crs(spid_em)$epsg != 4326) {
  cat("  <<< WARNING: CRS is not WGS84. Reproject before use in leaflet.\n")
} else {
  cat("  CRS OK.\n")
}
cat("\n")

#------------------------------------------------------------------------------#
# 11. Summary pass/fail
#------------------------------------------------------------------------------#

cat("====================================================\n")
cat(" Summary\n")
cat("====================================================\n")
cat(sprintf("  %-45s %s\n", "Invalid geometries:",
            ifelse(sum(invalid)==0, "PASS", paste("FAIL —", sum(invalid), "found"))))
cat(sprintf("  %-45s %s\n", "Empty geometries:",
            ifelse(sum(empty)==0,   "PASS", paste("FAIL —", sum(empty),   "found"))))
cat(sprintf("  %-45s %s\n", "Duplicate geo_codes:",
            ifelse(length(dups)==0, "PASS", paste("FAIL —", length(dups), "found"))))
cat(sprintf("  %-45s %s\n", "Coverage gaps > 1 km²:",
            ifelse(nrow(gap_report)==0,    "PASS", paste("WARN —", nrow(gap_report),    "countries"))))
cat(sprintf("  %-45s %s\n", "Polygon overlaps:",
            ifelse(nrow(overlap_report)==0,"PASS", paste("WARN —", nrow(overlap_report),"pairs"))))
cat(sprintf("  %-45s %s\n", "Slivers < 1 km²:",
            ifelse(nrow(tiny)==0,          "PASS", paste("WARN —", nrow(tiny),          "regions"))))
cat(sprintf("  %-45s %s\n", "CRS is WGS84:",
            ifelse(st_crs(spid_em)$epsg==4326, "PASS", "FAIL")))
cat("====================================================\n")

#------------------------------------------------------------------------------#
# 12. Leaflet visualization — interactive map for manual inspection
#    of edge-matched boundaries. This is not a formal test but can help spot
#    issues that automated checks might miss.
#------------------------------------------------------------------------------#

# leaflet requires WGS84 (EPSG:4326) and does not accept sf geometry columns
# named anything other than "geometry", so we rename and reproject if needed.

spid_em_wgs <- spid_em |>
  st_transform(4326) |>
  rename(geometry = geom) 

admin0_wgs <- admin0 |>
  st_transform(4326) |>
  rename(geometry = geom) 

spid_em_wgs <- spid_em |>
  filter(code == "KGZ", geo_source == "GAUL") |>
  st_transform(4326) |>
  rename(geometry = geom) 

admin0_wgs <- admin0 |>
  filter(code == "KGZ") |>
  st_transform(4326) |>
  rename(geometry = geom) 

# Colour palette — one colour per geo_level (or per country if preferred)

# Option A: colour by geo_level
levels_pal <- colorFactor(
  palette = "Set2",
  domain  = spid_em_wgs$geo_level
)

# Option B (alternative): colour by country — swap into addPolygons below
# country_pal <- colorFactor(palette = "Paired", domain = spid_em_wgs$code)

# Popup label — shows key attributes on click

popup_content <- paste0(
  "<b>", spid_em_wgs$geo_name, "</b><br>",
  "geo_code: ", spid_em_wgs$geo_code, "<br>",
  "Country: ",  spid_em_wgs$code,     "<br>",
  "Level: ",    spid_em_wgs$geo_level,"<br>",
  "Source: ",   spid_em_wgs$geo_source,"<br>",
  "Year: ",     spid_em_wgs$geo_year
)

# Map

leaflet() |>

  # Base tiles
  addProviderTiles(providers$CartoDB.Positron, group = "Light") |>
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>

  # Admin-0 outlines — drawn first so they sit beneath subnational fills
  addPolygons(
    data        = admin0_wgs,
    fill        = FALSE,
    color       = "#333333",
    weight      = 1.5,
    opacity     = 0.8,
    group       = "Admin-0 outlines"
  ) |>

  # Subnational edge-matched polygons
  addPolygons(
    data        = spid_em_wgs,
    fillColor   = ~levels_pal(geo_level),
    fillOpacity = 0.5,
    color       = "#ffffff",
    weight      = 0.5,
    opacity     = 0.7,
    popup       = popup_content,
    highlight   = highlightOptions(
      weight      = 2,
      color       = "#e31a1c",
      fillOpacity = 0.8,
      bringToFront = TRUE
    ),
    group       = "Subnational (EM)"
  ) |>

  # Legend
  addLegend(
    position = "bottomright",
    pal      = levels_pal,
    values   = spid_em_wgs$geo_level,
    title    = "Geo level",
    opacity  = 0.8
  ) |>

  # Layer toggle
  addLayersControl(
    baseGroups    = c("Light", "Satellite"),
    overlayGroups = c("Admin-0 outlines", "Subnational (EM)"),
    options       = layersControlOptions(collapsed = FALSE)
  ) |>

  # Scale bar
  addScaleBar(position = "bottomleft")

