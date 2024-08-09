rm(list = setdiff(ls(), c("spid_master","vintage","spid_data","version")))
gc() # free-up unused memory

# load packages
library(sf)
library(lwgeom)
library(openxlsx)
library(dplyr)

#------------------------------------------------------------------------------#
# NPL LSS-IV 2022 - survey matched boundaries
#------------------------------------------------------------------------------#

# NPL shapefile with new provinces from NSO - add names
  # source: http://nationalgeoportal.gov.np/

NPL <- st_read(paste0(spid_data,"raw/NSO/NPL_2022"))
NPL$name <- c("Koshi","Madhesh","Bagmati","Gandaki","Lumbini","Karnali",
              "Sudurpashchim")

dir.create(paste0(spid_data,"interim/",version,"/NPL_2022"))
st_write(NPL[,c(2,4)],
         paste0(spid_data,"interim/",version,"/NPL_2022/NPL_2022.shp"),
         append=FALSE)

#------------------------------------------------------------------------------#
# CIV 2008 - survey matched boundaries
#------------------------------------------------------------------------------#

# CIV 2007 sub-prefecture shapefile has incorrect ADM1 labels, need to reassign
  # source: http://purl.stanford.edu/vy294jp3453

CIV <- st_read(paste0(spid_data,"raw/NSO/CIV_2007"))

CIV  <- mutate(CIV,ADM1 = case_when(
  ID %in% c(138,139,153,154,155,156,167,168) ~ "Agneby",
  ID %in% c(61,62,63,74,75,91,115) ~ "Bafing",
  ID %in% c(12,13,14,15,16,142,157,171,177,178,182) ~ "Bas-Sassandra",
  ID %in% c(20,21,22,23,24,25,26,27,28,29,30,56) ~ "Denguele",
  ID %in% c(9,17,18,87,88,89,90,112,113,114,119,123) ~ "Dix-Huit Montangnes",
  ID %in% c(135,136,141,149,150) ~ "Fromager",
  ID %in% c(86,116,117,118,131,132,133) ~ "Haut-Sassandra",
  ID %in% c(94,105,106,120,129,134) ~ "Lacs",
  ID %in% c(146,147,152,158,159,165,166,172,173,176,181) ~ "Lagunes",
  ID %in% c(107,110,111,121,122) ~ "Marahoue",
  ID %in% c(124,125,130,137) ~ "Moyen Comoe",
  ID %in% c(7,8,10,11,19) ~ "Moyen-Cavally",
  ID %in% c(80,92,102,103,104,126,127,128,140,143,144,145) ~ "N'Zi Comoe",
  ID %in% c(3,4,5,6,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,50,
            58,59,184) ~ "Savanes",
  ID %in% c(148,160,169,170,174,175) ~ "Sud-Bandama",
  ID %in% c(151,161,162,163,164,179,180) ~ "Sud-Comoe",
  ID %in% c(47,48,49,64,65,70,71,81,82,83,84,93,94,95,96,97,98,
            99,100,108) ~ "Vallee Du Bandama",
  ID %in% c(54,55,57,60,66,67,68,69,72,73,76,85,101,109) ~ "Worodougou",
  ID %in% c(1,2,51,52,53,77,78,79,183,185,186) ~ "Zanzan"))

CIV <- group_by(CIV, ADM0, ADM1) %>%
  summarise(geometry = st_union(geometry)) %>%
  mutate(code = "CIV", ADM1_ID = row_number()) %>%
  select(code, ADM0, ADM1_ID, ADM1)

dir.create(paste0(spid_data,"interim/",version,"/CIV_2007"))
st_write(CIV,
         paste0(spid_data,"interim/",version,"/CIV_2007/CIV_2007.shp"),
         append=FALSE)

#------------------------------------------------------------------------------#
# SUR 2022 - survey matched boundaries
#------------------------------------------------------------------------------#

# SUR SSLC domains are defined by electricity Connection Areas
  # source: Connection areas shapefile provided by survey firm

SUR_domains <- read.xlsx(paste0(spid_data,
                                "raw/NSO/SUR_2016/CA domains.xlsx"))
SUR_CA <- st_read(paste0(spid_data,"raw/NSO/SUR_2016"))
SUR_CA$CA <- gsub("Wijk ", "", SUR_CA$Wijknaam)

SUR_coast <- left_join(SUR_CA,SUR_domains) %>% 
  group_by(domain) %>%
  summarize(geometry = st_union(geometry))

SUR_coast

SUR0 <- st_read(paste0(spid_data,"final/",version,"/",
                       tolower(vintage),"_admin0.gpkg")) %>%
  filter(geo_code=="SUR_2020_WB0") %>% rename(geometry=geom) %>%
  st_transform(st_crs(SUR_coast))

SUR_int <- st_difference(SUR0,st_union(SUR_coast)) %>%
  st_cast("POLYGON") %>% st_as_sf() %>%
  filter(st_area(.) > units::set_units(100,"km^2")) # remove crumbs

SUR_int$domain <- "Interior"

sample <- bind_rows(SUR_coast, SUR_int["domain"]) %>% st_as_sf()

# Edge matching
# make lines from polygons
samp_union <- st_union(sample) 
outline <- st_cast(samp_union, "MULTILINESTRING")
lines <- st_intersection(sample, outline) %>%
  st_collection_extract("LINESTRING") %>% st_make_valid()

# make points from lines
points <- st_segmentize(lines,dfMaxLength = units::set_units(100,m)) %>%
  st_cast("MULTIPOINT") %>% st_make_valid() %>% st_cast("POINT") 

# make voronoi polygons from points
voron <- st_collection_extract(st_voronoi(do.call(c,st_geometry(points)))) %>% 
  st_set_crs(st_crs(points))

# combine voronoi and subnat polygons, intersect with admin-0, keep polygons
em <- mutate(points,geometry = voron[unlist(st_intersects(points,voron))]) %>%
  st_make_valid()%>% group_by(domain) %>% 
  summarize(geometry=st_union(geometry)) %>%
  st_difference(samp_union) %>% rbind(sample) %>%
  group_by(domain) %>% summarize(geometry=st_union(geometry)) %>% 
  st_intersection(SUR0) %>% rowwise() %>% 
  mutate(geometry = st_combine(st_collection_extract(geometry, "POLYGON")))

SUR <- st_transform(em, 4326) 
SUR$d_code <- c(1,3,2)
dir.create(paste0(spid_data,"interim/",version,"/SUR_2016"))
st_write(SUR[,c("domain","d_code")],
         paste0(spid_data,"interim/",version,"/SUR_2016/SUR_2016.shp"),
         append=FALSE)
