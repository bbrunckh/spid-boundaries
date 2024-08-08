rm(list = setdiff(ls(), c("spid_master","vintage","spid_data","version")))
gc() # free-up unused memory

# load packages
library(arcpullr)
library(sf)
library(smoothr)
library(openxlsx)
library(dplyr)

#------------------------------------------------------------------------------#
# World Bank Admin-0 and Disputed Polygons - from WB cartograpgy
#------------------------------------------------------------------------------#

# World Bank admin-0 polygons
# wb0 <- st_read(paste0(spid_data,"raw/WB_GAD_ADM0_Polygons"))
server <- "https://geowb.worldbank.org/hosting/rest/services/Hosted/"
GAD_0 <- "WB_GAD_Medium_Resolution/FeatureServer/5"
wb0 <- get_spatial_layer(paste0(server,GAD_0))

# World Bank disputed polygons
# wb_disputes <- st_read(paste0(spid_data,"raw/WB_GAD_Disputes"))
GAD_disputes <- "WB_GAD_Medium_Resolution/FeatureServer/6"
wb_disputes <- get_spatial_layer(paste0(server,GAD_disputes))

# check geometry is valid in S2
sf_use_s2(TRUE)
wb0_invalid <- filter(wb0, !st_is_valid(geoms))
dis_invalid <- filter(wb_disputes, !st_is_valid(geoms))
  # "AUS" "FJI" "IRL" "RUS" "GBR" "NOR" "USA" not valid using s2

# repair invalid s2 geometry
  wb0_repaired <- st_transform(wb0_invalid,3395) %>%
    st_make_valid() %>% st_transform(4326) 
  
  # fix polygons that cross 180
  sf_use_s2(FALSE)
  wb0_repaired <- st_intersection(wb0_repaired,
                               st_as_sfc(st_bbox(c(xmin=-180,xmax=180,
                                                   ymax=90,ymin=-90),
                                                   crs = st_crs(4326)))) 
  sf_use_s2(TRUE)
  st_is_valid(wb0_repaired)
  
# add repaired geometries back in
wb0r <- filter(wb0, st_is_valid(geoms)) %>% bind_rows(wb0_repaired)
any(!st_is_valid(wb0r))

# check geometry is valid without S2
sf_use_s2(FALSE)
wb0_invalid2 <- filter(wb0r, !st_is_valid(geoms))
dis_invalid2 <- filter(wb_disputes, !st_is_valid(geoms))

wb0_repaired2 <- st_make_valid(wb0_invalid2)
st_is_valid(wb0_repaired2)

dis_repaired2 <- st_make_valid(dis_invalid2)
st_is_valid(dis_repaired2)

# add repaired geometries back in
wb0r2 <- filter(wb0r, st_is_valid(geoms)) %>% bind_rows(wb0_repaired2)
wb_dis <- filter(wb_disputes, st_is_valid(geoms)) %>% 
  bind_rows(dis_repaired2)
  
  # check valid
  any(!st_is_valid(wb0r2))
  any(!st_is_valid(wb_dis))
  
  sf_use_s2(TRUE)
  any(!st_is_valid(wb0r2))
  any(!st_is_valid(wb_dis))
  # all valid!!

# combine admin-0 and disputed areas for complete global coverage

  # remove overlap with disputed areas in admin-0
  wb0r2$overlap <- as.numeric(st_intersects(wb0r2,st_union(wb_dis)))==1

  wb0_clipped <- filter(wb0r2, overlap==TRUE) %>%
    mutate(geoms = st_difference(geoms,st_union(wb_dis$geoms))) %>%
    st_make_valid()

  # combine disputed and admin-0 clipped geometry
  wb0a <- filter(wb0r2, is.na(overlap)) %>% 
    bind_rows(wb0_clipped, wb_dis) 

# merge buffer zone in Cyprus - special case
cyp <- filter(wb0a, nam_0 %in% c("Cyprus", "UN Buffer Zone in Cyprus")) %>%
  summarise(geoms = st_union(st_make_valid(geoms))) %>%
  fill_holes(units::set_units(1, km^2))

wb0b <- filter(wb0a,!nam_0 %in% c("UN Buffer Zone in Cyprus")) %>%
  mutate(geoms = if_else(nam_0 %in% c("Cyprus"),cyp$geoms,geoms))

# check and fix validity
  sf_use_s2(FALSE)
  any(!st_is_valid(wb0b))
  wb0c <- mutate(wb0b, geoms = if_else(!st_is_valid(geoms),
                                       st_make_valid(geoms),
                                       geoms))
  any(!st_is_valid(wb0c))
  
  sf_use_s2(TRUE)
  any(!st_is_valid(wb0c))

# merge WB codes and names, drop uninhabited territories
codes <- read.xlsx(spid_master, sheet = "admin0 codes")

wb0d <- select(wb0c, globalid) %>% left_join(codes) %>%
  filter(!notes %in% c("drop, no pop")) %>%
  select(code, geo_name, geo_code)
  
# merge geometry for duplicate geo_codes
dups <- group_by(wb0d,geo_code) %>% filter(n()>1) 

dupsm <- group_by(dups,code, geo_name, geo_code) %>% 
  summarise(geoms = st_union(geoms)) %>%
  fill_holes(units::set_units(1, km^2)) 

# combine and check geo_codes are unique
wb0_geo <- filter(wb0d, !geo_code %in% dups$geo_code) %>% bind_rows(dupsm) %>%
  arrange(geo_code)

any(duplicated(wb0_geo$geo_code)) # unique geo_codes
any(!st_is_valid(wb0_geo$geoms))  # all valid

# save geopackage
st_write(wb0_geo,
         paste0(spid_data,"final/",version,"/",tolower(vintage),"_admin0.gpkg"),
         append=FALSE)

# save shapefile
st_write(wb0_geo,
         paste0(spid_data,"final/",version,"/",tolower(vintage),"_admin0.shp"),
         append=FALSE)
