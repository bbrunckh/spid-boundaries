rm(list = setdiff(ls(), c("spid_master","vintage","spid_data","version"))) 
gc()

library(sf)
library(lwgeom)
library(smoothr)
library(openxlsx2)
library(dplyr)
library(furrr)

## 4b. Patch - this script is for patching the spid_master with new geo_codes, and removing old geo_codes.

#------------------------------------------------------------------------------#
# version to patch
#------------------------------------------------------------------------------#

vintage <- "SM26"
version <- paste0(vintage,"_2026-03-05")

spid_data <- "~/Library/CloudStorage/OneDrive-WBG/spid-boundaries/data/"
# spid_data <- "C:/Users/wb587256/OneDrive - WBG/spid-boundaries/data/"
spid_master <- "~/Library/CloudStorage/OneDrive-WBG/Minh\ Cong\ Nguyen\'s\ files\ -\ Poverty\ and\ Shared\ Prosperity\ SM2026/Subnational/02.input/SPID\ boundaries\ SM26.xlsx"
# spid_master <- "C:/Users/wb587256/OneDrive - WBG/Minh\ Cong\ Nguyen\'s\ files\ -\ Poverty\ and\ Shared\ Prosperity\ SM2026/Subnational/02.input/SPID\ boundaries\ SM26.xlsx"

#------------------------------------------------------------------------------#
# SPID boundary IDs
#------------------------------------------------------------------------------#

spid_bounds <- read_xlsx(spid_master, sheet = "SPID boundaries") |>
  filter(!is.na(geo_code))

spid_x <- read_xlsx(spid_master, sheet = "SPID modified boundaries") |>
  mutate(geo_level = as.character(geo_level))

spid_miss <- read_xlsx(spid_master, sheet = "SPID missing boundaries")

spid_all <- bind_rows(spid_bounds, spid_miss) |>
  mutate(key = paste(code, year, survname, byvar)) |>
  arrange(key)

#------------------------------------------------------------------------------#
# Admin-0 and SPID edge matched subnational boundaries
#------------------------------------------------------------------------------#

path_final   <- paste0(spid_data, "final/",  version, "/", tolower(vintage))
path_interim   <- paste0(spid_data, "interim/",  version, "/", tolower(vintage))

admin0  <- st_read(paste0(path_final,   "_admin0.gpkg"))
spid_subnat <- st_read(paste0(path_interim, "_subnat.gpkg")) 
st_geometry(spid_subnat) <- "geometry"
spid_em <- st_read(paste0(path_final, "_subnat_em_WRONG_GMB2000.gpkg"))

# identify new geo_codes to patch in spid_em
new_geo_codes <- spid_all |> 
  filter(!geo_code %in% spid_em$geo_code)

# identify geo_codes to remove from spid_em
remove_geo_codes <- spid_em |> 
  st_drop_geometry() |>
  filter(!geo_code %in% spid_all$geo_code)

#------------------------------------------------------------------------------#
# Get geometry for new geo_codes from boundary data sources
#------------------------------------------------------------------------------#

# GAUL 2024 level 2
gaul2 <- st_read(paste0(spid_data,"raw//GAUL2024/GAUL_2024_L2"), crs = 4326) |>
  mutate(
    geo_level = "2",
    geo_id =as.character(if_else(!is.na(gaul2_code),gaul2_code, gaul1_code)),
    geo_name =if_else(!is.na(gaul2_name),gaul2_name, gaul1_name),
    geo_year = 2024,
    countryname = gaul0_name,
    code = iso3_code
  ) 
#... other sources if needed...

# ...unmodified boundaries if needed...

# modified boundaries 
spid_tomod <- filter(spid_x, geo_code %in% new_geo_codes$geo_code, 
  geo_year == 2024, geo_source=="GAUL") |> 
  left_join(gaul2[c("countryname","geo_id")]) |>
  st_as_sf()

mod_list <- unique(spid_tomod$geo_code)

rm(spid_mod)
sf_use_s2(FALSE)

for (i in 1:length(mod_list)){
  
  source <- filter(spid_tomod,geo_code==mod_list[i]) |> arrange(mod_order)
  modified <- source[1,c("geo_code","geometry")]
  
  for (r in 2:nrow(source)){
    
    if (source[r,]$mod_type=="union"){
      modified <- st_union(modified,source[r,"geometry"]) |>
        fill_holes(units::set_units(1, km^2))}
  }

  #... other operations if needed...
  
  if (!exists("spid_mod")){spid_mod <- modified}
  else{spid_mod <- bind_rows(spid_mod,modified)}
}

# merge in codes
spid_mod <- filter(new_geo_codes, geo_level=="x") |>
  distinct(geo_code, .keep_all=TRUE) |>
  select(code, geo_year,geo_source,geo_level,geo_idvar,geo_id,geo_nvar,geo_name,geo_code, key) |>
  left_join(spid_mod) |> st_as_sf()

#------------------------------------------------------------------------------#
# edge match geo_code sets with new or removed geo_codes 
#------------------------------------------------------------------------------#

# identify sets to patch in spid_em with new geo_codes
to_em <- spid_all |> 
  filter(key %in% spid_mod$key) |> 
  left_join(bind_rows(spid_subnat, spid_mod) |> select(geo_code, geometry)) |>
  st_as_sf() 

# edge match subregions for each key, then bind rows together
source("src/fct_edge_match.R")   # loads edge_match()

spid_em_patched <- to_em |> 
  group_by(key) |> 
  group_split() |> 
  future_map_dfr(~ edge_match(
    target     = admin0[admin0$code == substr(.x$key[1], 1, 3), ] |> st_make_valid() |> mutate(geom = st_union(geom)),
    subregions = .,
    id_col     = "geo_code",
    seg_length = set_units(100, "m"),
    snap_grid  = 10,
    verbose    = TRUE
  )
)
st_geometry(spid_em_patched) <- "geom"
plot(spid_em_patched["geo_code"])

# replace sets that were patched in spid_em
spid_em_new <- spid_em |> 
  filter(!geo_code %in% c(to_em$geo_code, remove_geo_codes$geo_code)) |>
  bind_rows(spid_em_patched) |>
  select(code, geo_year, geo_source, geo_level, geo_idvar, geo_id, geo_nvar, geo_name, geo_code, geom) |>
  filter(st_geometry_type(geom) %in% c("POLYGON", "MULTIPOLYGON")) |>
  arrange(geo_code)

# manual fix for Bishkek KGZ_2015_GAUL1_147293 which overlaps other subnational regions in GAUL 2015
kgz_union <- spid_em_new |>
  filter(code == "KGZ", geo_year == 2015, geo_source == "GAUL", geo_level == "1", geo_code != "KGZ_2015_GAUL1_147293") |>
  st_union()

target_idx <- which(spid_em_new$geo_code == "KGZ_2015_GAUL1_147293")

spid_em_new$geom[target_idx] <- st_difference(
  spid_em_new$geom[target_idx],
  kgz_union
)

# manual fix for GIN_2015_GAUL1_40704 which overlaps GIN_2015_GAUL1_40706 in GAUL 2015
gin_union <- spid_em_new |>
  filter(code == "GIN", geo_year == 2015, geo_source == "GAUL", geo_level == "1", geo_code != "GIN_2015_GAUL1_40704") |>
  st_union()

target_idx <- which(spid_em_new$geo_code == "GIN_2015_GAUL1_40704")

spid_em_new$geom[target_idx] <- st_difference(
  spid_em_new$geom[target_idx],
  gin_union
)

#CHECKS

# save patched spid_em
st_write(spid_em_new, paste0(path_final, "_subnat_em.gpkg"), append = FALSE)
st_write(spid_em_new, paste0(path_final, "_subnat_em.shp"),  append = FALSE)

# compare new spid_em with old spid_em to confirm that new geo_codes were added, and old geo_codes were removed
if (!all(new_geo_codes$geo_code %in% spid_em_new$geo_code)) {
  warning("Not all new geo_codes were added to spid_em.")
}
if (any(remove_geo_codes$geo_code %in% spid_em_new$geo_code)) {
  warning("Not all old geo_codes were removed from spid_em.")
}

# compare number of rows in new spid_em with old spid_em to confirm that the number of rows increased by the number of new geo_codes, minus the number of removed geo_codes
expected_rows <- nrow(spid_em) + nrow(new_geo_codes) - nrow(remove_geo_codes)
if (nrow(spid_em_new) != expected_rows) {
  warning("The number of rows in the new spid_em does not match the expected number of rows based on the number of new and removed geo_codes.")
}
# print number of rows in old and new spid_em for reference
message("Number of rows in old spid_em: ", nrow(spid_em))
message("Number of rows in new spid_em: ", nrow(spid_em_new))

message("Patched spid_em with ", nrow(new_geo_codes), " new geo_codes and removed ", nrow(remove_geo_codes), " old geo_codes." )

length(unique(spid_all$geo_code))

surv_list <- mutate(spid_bounds, key = paste(code, year, survname))
length(unique(surv_list$key))

nrow(spid_em_new[spid_em_new$geo_code %in% spid_bounds$geo_code, ])
length(unique(spid_bounds$geo_code))

length(unique(spid_em_new$code))
length(unique(spid_bounds$code))

