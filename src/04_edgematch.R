rm(list = setdiff(ls(), c("spid_master","vintage","spid_data","version"))) 
gc() # free-up unused memory

# load packages
library(sf)
library(lwgeom)
library(openxlsx)
library(dplyr)

#------------------------------------------------------------------------------#
# SPID boundary key - AM24
#------------------------------------------------------------------------------#

# AM24 SPID boundary key
spid_bounds <- read.xlsx(spid_master, sheet = "SPID boundaries") %>%
  filter(!is.na(geo_code)) # drop samples not mapped

# AM24 SPID missing samples
spid_miss <- read.xlsx(spid_master, sheet = "SPID missing boundaries")

#------------------------------------------------------------------------------#
# admin-0 and AM24 subnational boundaries
#------------------------------------------------------------------------------#

admin0 <- st_read(paste0(spid_data,"final/",version,"/",
                         tolower(vintage),"_admin0.gpkg"))

spid_subnat <- st_read(paste0(spid_data,"final/",version,"/",
                              tolower(vintage),"_subnat.gpkg"))

#------------------------------------------------------------------------------#
# Edge match subnational boundaries to admin-0 - Voronoi method
#------------------------------------------------------------------------------#

# list of surveys
spid_all <- bind_rows(spid_bounds, spid_miss) %>%
  mutate(key = paste(code,year,survname,byvar)) %>% arrange(key)

spid_list <- rev(unique(spid_all$key))

sf_use_s2(FALSE)
spid_em <- data.frame()

# loop over surveys
for (i in 1:length(spid_list)){
  print(spid_list[i])
  
  sample <- filter(spid_all,key==spid_list[i]) %>% 
    left_join(spid_subnat[c("geo_code", "geom")]) %>% 
    select(geo_code, geom) %>% st_as_sf()
  
  if(any(!sample$geo_code %in% spid_em$geo_code)){
    
  # target admin0 polygon
  target <- filter(admin0, 
                   geo_code == paste0(substr(spid_list[i],1,3),"_2023_WB0")) %>%
                     select(geom)
    
  # make lines from sample polygons
  samp_union <- st_union(sample) 
  outline <- st_cast(samp_union, "MULTILINESTRING")
  lines <- st_intersection(sample, outline) %>% 
    st_collection_extract("LINESTRING")
  
  # make points from lines
  points <- st_segmentize(lines,dfMaxLength = units::set_units(1000,m)) %>%
    st_cast("MULTIPOINT") %>% st_cast("POINT") %>% 
    st_transform(3395) %>%
    st_snap_to_grid(units::set_units(100,m)) %>% # snap to 100m grid
    st_transform(4326) %>% 
    distinct()
  
  # make voronoi polygons from points
  voron <- st_collection_extract(st_voronoi(do.call(c,st_geometry(points)))) %>% 
    st_set_crs(st_crs(points))
  
  # combine voronoi and subnat polygons, intersect with admin-0, keep polygons
  em <- mutate(points,geom = voron[unlist(st_intersects(points,voron))]) %>%
    st_make_valid()%>% group_by(geo_code) %>% summarize(geom=st_union(geom)) %>%
    st_difference(samp_union) %>% rbind(sample) %>%
    group_by(geo_code) %>% summarize(geom=st_union(geom)) %>% 
    st_intersection(target) %>% rowwise() %>% 
    mutate(geom = st_combine(st_collection_extract(geom, "POLYGON")))
  
  # bind
  if (nrow(spid_em)==0){spid_em <- em}
  else{spid_em <- bind_rows(spid_em,em)}
  
  print(paste0(round(100*i/length(spid_list)),"%")) # progress
  }
}

#------------------------------------------------------------------------------#
# Clean up
#------------------------------------------------------------------------------#

# keep first occurrence of each unique geo_code (most recent), merge with codes
spid_em <- distinct(spid_em, geo_code, .keep_all = TRUE) %>% 
  left_join(st_drop_geometry(spid_subnat)) 

# dropped regions (cropped out by admin-0 intersection)
dropped <- spid_subnat[!spid_subnat$geo_code %in% spid_em$geo_code,] 
dropped 

# add dropped regions back in (some have samples)
spid_em <- bind_rows(spid_em, dropped) %>%
  select(code, geo_year,geo_source,geo_level,geo_idvar,geo_id,geo_nvar,
         geo_name,geo_code) %>% 
  arrange(geo_code)
spid_em

# check valid
any(!st_is_valid(spid_em)) # All valid if FALSE

#save EM geopackage
st_write(spid_em,
         paste0(spid_data,"final/",version,"/",tolower(vintage),"_subnat_em.gpkg"),
         append=FALSE)

# save EM shapefile
st_write(spid_em,
         paste0(spid_data,"final/",version,"/",tolower(vintage),"_subnat_em.shp"),
         append=FALSE)

#checks
length(unique(spid_all$geo_code))

surv_list <- mutate(spid_bounds, key = paste(code,year,survname))
length(unique(surv_list$key))

nrow(spid_em[spid_em$geo_code %in% spid_bounds$geo_code,]) 
length(unique(spid_bounds$geo_code)) 

length(unique(spid_em$code))
length(unique(spid_bounds$code)) 

# AM24 vintage: 2156 subnat regions with data from 1112 surveys in 138 countries

#visualize - check global coverage
  # library(leaflet)
  # 
  # map <- filter(admin0, !code %in% spid_em$code) %>%
  #   bind_rows(spid_em) %>% select(code,geo_name)
  # 
  # leaflet(map) %>%
  #   addProviderTiles("CartoDB.Positron") %>%
  #   addPolygons(color = "green",
  #               popup = paste("Country: ", map$code, "<br>",
  #                             "Region: ", map$geo_name, "<br>"))
