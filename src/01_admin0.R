rm(list = setdiff(ls(), c("spid_master","vintage","spid_data","version")))
gc() # free-up unused memory

# load packages
library(sf)
library(smoothr)
library(openxlsx)
library(dplyr)

#------------------------------------------------------------------------------#
# World Bank Admin-0 boundaries
#------------------------------------------------------------------------------#

# World Bank admin-0 - High Resolution Country Polygons and Disputed Areas
wb0 <- st_read(paste0(spid_data,"raw/WB_countries_Admin0_10m"))
wb_disputes <- st_read(paste0(spid_data,"raw/WB_disputed_areas_Admin0_10m"))

# GAD medium resolution disputed areas
gad_disputes <- st_read(paste0(spid_data,"raw/WB_GAD_Disputes"))

# combine disputed areas
disputes <- filter(wb_disputes,!st_is_empty(geometry)) %>% 
  bind_rows(gad_disputes) %>%
  mutate(WB_NAME = if_else(is.na(WB_NAME), NAM_0,WB_NAME),
         WB_NAME = if_else(is.na(WB_NAME), NAM_0_Alt,WB_NAME)) %>%
  select(ISO_A3, WB_A3, WB_NAME) 

disputes2 <- group_by(disputes, st_intersects(disputes)) %>%
  summarise(ISO_A3 = first(ISO_A3),
            WB_A3 = first(WB_A3),
            WB_NAME = first(WB_NAME),
            geometry = st_union(geometry)) %>% select(-1) %>%
  fill_holes(units::set_units(1, km^2))

# check geometry is valid in s2
sf_use_s2(TRUE)
wb0_invalid <- filter(wb0, !st_is_valid(geometry))
dis_invalid <- filter(disputes2, !st_is_valid(geometry))

# IDN, MYS, RUS, FJI not valid using s2 spherical geometry package - repair
# Note RUS and FJI cross 180°
sf_use_s2(FALSE)

wb0_repaired <- st_intersection(wb0_invalid,
                               st_as_sfc(st_bbox(c(xmin=-180,xmax=180,
                                                   ymax=90,ymin=-90),
                                                 crs = st_crs(4326))))
sf_use_s2(TRUE)
st_is_valid(wb0_repaired)

# add repaired geometries back in
wb0r <- filter(wb0, st_is_valid(geometry)) %>% bind_rows(wb0_repaired)
any(!st_is_valid(wb0r))

# check geometry is valid without S2
sf_use_s2(FALSE)
wb0_invalid2 <- filter(wb0r, !st_is_valid(geometry))
dis_invalid2 <- filter(disputes2, !st_is_valid(geometry))

  # check all valid again
  any(!st_is_valid(wb0r))
  any(!st_is_valid(disputes2))
  
  sf_use_s2(TRUE)
  any(!st_is_valid(wb0r))
  any(!st_is_valid(disputes2))

# combine admin-0 and disputed areas for complete global coverage

  # remove overlap with disputed areas in admin-0
  wb0r$overlap <- as.numeric(st_intersects(wb0r,st_union(disputes2)))

  wb0_clipped <- filter(wb0r, !is.na(overlap)) %>%
    mutate(geometry = st_difference(geometry,st_union(disputes2$geometry)))

  # add clipped geometry back in
  wb0a <- filter(wb0r, is.na(overlap)) %>% bind_rows(wb0_clipped, disputes2)

# merge buffer zone in Cyprus - special case
cyp <- filter(wb0a,WB_NAME %in% c("Cyprus", "U.N. Buffer Zone in Cyprus")) %>%
  summarise(geometry = st_union(st_make_valid(geometry))) %>%
  fill_holes(units::set_units(1, km^2))

wb0b <- filter(wb0a,!WB_NAME %in% c("U.N. Buffer Zone in Cyprus")) %>%
  mutate(geometry = if_else(WB_NAME %in% c("Cyprus"),cyp$geometry,geometry))

# remove Taiwan from China and add as separate feature for survey merge
gaul0 <- st_read(paste0(spid_data,"raw/GAUL2015/gaul0"))
twn <- gaul0[gaul0$ADM0_NAME=="Taiwan",] %>%
  mutate(FORMAL_EN = "Taiwan",ISO_A3 = "TWN",WB_A3 = "TWN",WB_NAME = "Taiwan",
         WB_RULES = "Fill color should be same as China") %>%
  select(FORMAL_EN, ISO_A3, WB_A3, WB_NAME,WB_RULES)

chn <- filter(wb0b,WB_A3 %in% c("CHN")) %>% #remove TWN from CHN 
  st_difference(st_buffer(twn,units::set_units(5, km))) 

wb0c <- filter(wb0b,!WB_A3 %in% c("CHN")) %>%
  bind_rows(chn, twn) 

# add ID, tidy
wb0d <- arrange(wb0c, WB_A3, WB_NAME) %>%
  mutate(geo_id = row_number()) %>%
  select(geo_id, ISO_A3, WB_A3, WB_NAME, TYPE) 

# merge WB codes and names, drop uninhabited territories
codes <- read.xlsx(spid_master, sheet = "admin0 codes")

wb0e <- select(wb0d, geo_id) %>% left_join(codes) %>%
  filter(!notes %in% c("drop, no pop")) %>%
  select(code, geo_name, geo_code)

# merge geometry for duplicate geo_codes
dups <- group_by(wb0e,geo_code) %>% filter(n()>1) 

dupsm <- group_by(dups,code, geo_name, geo_code) %>% 
  summarise(geometry = st_union(st_make_valid(geometry))) %>%
  fill_holes(units::set_units(1, km^2)) 

# combine and check geo_codes are unique
wb0_geo <- filter(wb0e, !geo_code %in% dups$geo_code) %>% bind_rows(dupsm) %>%
  arrange(geo_code)

any(duplicated(wb0_geo$geo_code)) # unique geo_codes
any(!st_is_valid(wb0_geo$geometry))  # all valid

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
# pop <- rast("/Users/bbrunckhorst/Library/CloudStorage/OneDrive-WBG/Hazard exposure/inputs/population/GHS_POP_E2020_GLOBE_R2023A_4326_3ss_V1_0.tif")
# totalpop <- exact_extract(pop, wb0d,fun = "sum", append_cols = c("geo_id"))
# 
# codes <- left_join(st_drop_geometry(wb0d),totalpop) %>%
#   rename(pop_ghs = sum)
# 
# write.xlsx(codes, paste0(spid_data,"interim/",version,"/admin0_attributes.xlsx"))
# 