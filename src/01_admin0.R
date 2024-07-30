rm(list = setdiff(ls(), c("spid_master","vintage","spid_data"))) # workspace
gc() # free-up unused memory

# load packages
library(sf)
library(smoothr)
library(openxlsx)
library(dplyr)

#------------------------------------------------------------------------------#
# World Bank Admin-0 boundaries
#------------------------------------------------------------------------------#

# World Bank admin-0 - Official Country Polygons and Disputed Areas
wb0 <- st_read(paste0(spid_data,"raw/WB_countries_Admin0_10m"))
wb_disputes <- st_read(paste0(spid_data,"raw/WB_disputed_areas_Admin0_10m"))

# check geometry is valid
wb0_invalid <- wb0[!st_is_valid(wb0$geometry),] 


# IDN, MYS, RUS, FJI not valid using s2 spherical geometry package - repair
# Note RUS and FJI cross 180°
sf_use_s2(FALSE)

wb0_invalid <- st_intersection(wb0_invalid,
                               st_as_sfc(st_bbox(c(xmin=-180,xmax=180,
                                                   ymax=90,ymin=-90),
                                                 crs = st_crs(4326))))
wb0_invalid$geometry <- st_make_valid(wb0_invalid) %>% 
  s2::s2_rebuild() %>% st_as_sfc()

sf_use_s2(TRUE)

# add repaired geometries back in
wb0 <- filter(wb0, st_is_valid(geometry)) %>%bind_rows(wb0_invalid)

wb0[!st_is_valid(wb0$geometry),"WB_A3"] # all valid
wb_disputes[!st_is_valid(wb_disputes$geometry),"WB_NAME"] # all valid

# combine admin-0 and disputed areas for complete global coverage
wb_disputes <- filter(wb_disputes,!st_is_empty(geometry)) # remove 1 empty geom

# remove overlap with disputed areas in admin-0
wb0$overlap <- as.numeric(st_intersects(wb0,st_union(wb_disputes)))==1

wb0_clipped <- filter(wb0, overlap==TRUE) %>%
  mutate(geometry = st_difference(geometry,st_union(wb_disputes$geometry)))

# add clipped geometry back in
wb0 <- filter(wb0, is.na(overlap)) %>% bind_rows(wb0_clipped, wb_disputes)

# merge buffer zone in Cyprus - special case
cyp <- filter(wb0,WB_NAME %in% c("Cyprus", "U.N. Buffer Zone in Cyprus")) %>%
  summarise(geometry = st_union(st_make_valid(geometry))) %>%
  fill_holes(units::set_units(1, km^2))

wb0 <- filter(wb0,!WB_NAME %in% c("U.N. Buffer Zone in Cyprus")) %>%
  mutate(geometry = if_else(WB_NAME %in% c("Cyprus"),cyp$geometry,geometry))

# remove Taiwan from China and add as separate feature for survey merge
gaul0 <- st_read(paste0(spid_data,"raw/GAUL2015/gaul0"))
twn <- gaul0[gaul0$ADM0_NAME=="Taiwan",] %>%
  mutate(FORMAL_EN = "Taiwan",ISO_A3 = "TWN",WB_A3 = "TWN",WB_NAME = "Taiwan",
         WB_RULES = "Fill color should be same as China") %>%
  select(FORMAL_EN, ISO_A3, WB_A3, WB_NAME,WB_RULES)

chn <- filter(wb0,WB_A3 %in% c("CHN")) %>% #remove TWN from CHN 
  st_difference(st_buffer(twn,units::set_units(5, km))) 

wb0 <- filter(wb0,!WB_A3 %in% c("CHN")) %>%
  bind_rows(chn, twn)

# WB codes, drop some uninhabited territories, fix duplicate WB codes
codes <- read.xlsx(spid_master, sheet = "admin0 codes")

wb0m <- left_join(wb0,codes[c("WB_NAME","code","name")])  %>% 
  filter(!is.na(code)) %>% rename(rules = WB_RULES) %>% 
  select(code,name,rules) %>% arrange(code, name)

dups <- group_by(wb0m,code) %>% filter(n()>1 & code!= "XXX") 

dupsm <- group_by(dups,code) %>% summarise(geometry = st_union(geometry)) %>%
  mutate(name = case_when(
    code=="BES" ~ "Bonaire, Sint Eustatius and Saba (Neth.)",
    code=="CHI" ~ "Channel Islands",
    code=="UMI" ~ "United States Minor Outlying Islands (US)",
    code=="USA" ~ "United States"),
    rules = case_when(
      code=="BES" ~ "Name in italic",
      code=="CHI" ~ "Name in italic",
      code=="UMI" ~ "Name in italic",
      code=="USA" ~ "None"))

# combine fixed boundaries and add unique geo_codes
wb0_geo <- filter(wb0m, !code %in% dups$code) %>% bind_rows(dupsm) %>%
  group_by(code) %>%
  mutate(geo_code = ifelse(n()>1 & row_number()>0, # create unique geo codes
                           paste0(code,"_2020_WB0_",row_number()),
                           paste0(code,"_2020_WB0"))) %>% 
  select(code,name,rules,geo_code) %>% arrange(code,name)

wb0_geo$geo_code

# check valid
length(unique(wb0_geo$geo_code))
wb0_geo[!st_is_valid(wb0_geo$geometry),"code"]

# save geopackage
st_write(wb0_geo,
         paste0(spid_data,"final/",vintage,"/",tolower(vintage),"_admin0.gpkg"),
         append=FALSE)

# save shapefile
st_write(wb0_geo,
         paste0(spid_data,"final/",vintage,"/",tolower(vintage),"_admin0.shp"),
         append=FALSE)
