rm(list = setdiff(ls(), c("spid_master","vintage","spid_data","version")))
gc() # free-up unused memory

# load packages
library(sf)
library(smoothr)
library(openxlsx2)
library(dplyr)

#------------------------------------------------------------------------------#
# World Bank Admin-0 Boundaries (2025)
#------------------------------------------------------------------------------#

# World Bank admin-0 - 2025 version
wb0 <- st_read(paste0(spid_data,"raw/WB_2025/World Bank Official Boundaries - Admin 0_all_layers.gpkg"))

# check geometry is valid in s2
sf_use_s2(TRUE)
wb0_invalid <- filter(wb0, !st_is_valid(geom))

  # NOR, USA, PHL, CAN, BHS not valid - repair
  wb0_repaired <- st_make_valid(wb0_invalid)
  st_is_valid(wb0_repaired)
  
  # add repaired geometries back in
  wb0r1 <- filter(wb0, st_is_valid(geom)) |> bind_rows(wb0_repaired)
  any(!st_is_valid(wb0r1))

# check geometry is valid without S2
sf_use_s2(FALSE)
wb0_invalid2 <- filter(wb0r1, !st_is_valid(geom))

  # CAN not valid - repair
  wb0_repaired2 <- st_make_valid(wb0_invalid2)
  st_is_valid(wb0_repaired2)
  
  # add repaired geometries back in
  wb0r2 <- filter(wb0r1, st_is_valid(geom)) |> bind_rows(wb0_repaired2)
  any(!st_is_valid(wb0r2))

sf_use_s2(TRUE)
any(!st_is_valid(wb0r2))

# merge buffer zone in Cyprus - special case
cyp <- filter(wb0r2,NAM_0 %in% c("Cyprus", "UN Buffer Zone")) |>
  summarise(geom = st_union(st_make_valid(geom))) |>
  fill_holes(units::set_units(1, km^2))

wb0b <- filter(wb0r2,!NAM_0 %in% c("UN Buffer Zone")) |>
  mutate(geom = if_else(NAM_0 %in% c("Cyprus"),cyp$geom,geom))

# remove Taiwan from China and add as separate feature for survey merge
gaul0 <- st_read(paste0(spid_data,"raw/GAUL2015/gaul0"))
twn <- gaul0[gaul0$ADM0_NAME=="Taiwan",] |>
  mutate(NAM_0 = "Taiwan",ISO_A3 = "TWN",WB_A3 = "TWN", GAUL_0 = 147296) |>
  rename(geom = geometry) |>
  select(NAM_0, ISO_A3, WB_A3, GAUL_0)

chn <- filter(wb0b,NAM_0=="China") |> #remove TWN from CHN 
  st_difference(st_buffer(twn,units::set_units(5, km))) |>
  select(-contains("."))

wb0c <- filter(wb0b,NAM_0!="China") |>
  bind_rows(chn, twn) 

# add ID, tidy
wb0d <- arrange(wb0c, WB_A3, NAM_0) |>
  mutate(geo_id = row_number())

  # extract population at this point if needed

# merge WB codes and names, drop uninhabited territories
codes <- read_xlsx(spid_master, sheet = "admin0 codes")

wb0e <- select(wb0d, geo_id) |> left_join(codes) |>
  filter(!notes %in% c("drop, no pop")) |>
  select(code, geo_name, geo_code)

# merge geometry for duplicate geo_codes
dups <- group_by(wb0e,geo_code) |> filter(n()>1)

dupsm <- group_by(dups,code, geo_name, geo_code) |> 
  summarise(geom = st_union(st_make_valid(geom))) |>
  fill_holes(units::set_units(1, km^2)) 

# combine and check geo_codes are unique
wb0_geo <- filter(wb0e, !geo_code %in% dups$geo_code) |> bind_rows(dupsm) |>
  arrange(geo_code)

any(duplicated(wb0_geo$geo_code)) # FALSE = unique geo_codes
any(!st_is_valid(wb0_geo$geom))  # FALSE = all valid

# save geopackage
st_write(wb0_geo,
         paste0(spid_data,"final/",version,"/",tolower(vintage),"_admin0.gpkg"),
         append=FALSE)

# save shapefile
st_write(wb0_geo,
         paste0(spid_data,"final/",version,"/",tolower(vintage),"_admin0.shp"),
         append=FALSE)

#------------------------------------------------------------------------------#
# Extracting population for intermediate admin-0 boundaries
#------------------------------------------------------------------------------#
# 
# library(terra)
# library(exactextractr)
# 
# pop <- rast("~/Library/CloudStorage/OneDrive-WBG/Hazard exposure/inputs/population/GHS_POP_E2020_GLOBE_R2023A_4326_30ss_V1_0.tif")
# totalpop <- exact_extract(pop, wb0d,fun = "sum", append_cols = c("geo_id"))
# 
# codes <- left_join(st_drop_geometry(wb0d),totalpop) |>
#   rename(pop_ghs = sum)
# 
# write_xlsx(codes, paste0(spid_data,"interim/",version,"/admin0_attributes.xlsx"))
