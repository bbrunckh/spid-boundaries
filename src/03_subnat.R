rm(list = setdiff(ls(), c("spid_master","vintage","spid_data","version"))) 
gc() # free-up unused memory

# load packages
library(openxlsx)
library(dplyr)
library(sf)
library(smoothr)

#------------------------------------------------------------------------------#
# SPID boundary master list
#------------------------------------------------------------------------------#

# SPID boundary key
spid_bounds <- read.xlsx(spid_master, sheet = "SPID boundaries") %>%
  filter(!is.na(geo_code)) # drop samples not mapped

# SPID modified boundary key
spid_x <- read.xlsx(spid_master, sheet = "SPID modified boundaries") %>%
  mutate(geo_level = as.character(geo_level))

# Country codes
admin0_codes <- read.xlsx(spid_master, sheet = "admin0 codes")

# CHECKS

# no duplicate boundary ids in same survey
group_by(spid_bounds,code,year,survname, byvar) %>% filter(duplicated(geo_id))
group_by(spid_bounds,code,year,survname, byvar)%>%filter(duplicated(geo_name))
group_by(spid_bounds,code,year,survname,byvar)%>% filter(duplicated(geo_code))

group_by(spid_x,geo_code) %>% filter(duplicated(geo_id))
group_by(spid_x,geo_code) %>% filter(duplicated(geo_name))

# survey has consistent geo_source and geo_year
group_by(spid_bounds,code,year,survname,byvar) %>% 
  filter(length(unique(geo_year))>1)
group_by(spid_bounds,code,year,survname,byvar) %>% 
  filter(length(unique(geo_source))>1)

group_by(spid_x,geo_code) %>% filter(length(unique(geo_year))>1)
group_by(spid_x,geo_code) %>% filter(length(unique(geo_source))>1)

# check all modified boundaries are defined
spid_bounds[spid_bounds$geo_level=="x" &
              !spid_bounds$geo_code %in% spid_x$geo_code,]

# check surveys with different geo levels 
levels <- group_by(spid_bounds,code,year,survname, byvar) %>%
  filter(length(unique(geo_level))>1)

#------------------------------------------------------------------------------#
# Subnational boundary data sources
#------------------------------------------------------------------------------#

# Subnational boundaries from GAUL 2015

gaul0 <- st_read(paste0(spid_data,"raw/GAUL2015/gaul0")) %>%
  mutate(geo_level = "0")
gaul1 <- st_read(paste0(spid_data,"raw/GAUL2015/gaul1")) %>%
  mutate(geo_level = "1")
gaul2 <- st_read(paste0(spid_data,"raw//GAUL2015/gaul2"), 
                 crs = st_crs(gaul1)) %>%
  mutate(geo_level = "2")

gaul2015 <- bind_rows(gaul0,gaul1,gaul2) %>%
  mutate(geo_id =as.character(if_else(!is.na(ADM2_CODE),ADM2_CODE,
                              if_else(!is.na(ADM1_CODE),ADM1_CODE,ADM0_CODE))),
         geo_name =if_else(!is.na(ADM2_CODE),ADM2_NAME,
                           if_else(!is.na(ADM1_CODE),ADM1_NAME,ADM0_NAME)),
         geo_year = 2015,
         countryname = ADM0_NAME) %>%
  left_join(admin0_codes[c("code","ADM0_CODE")])

any(is.na(gaul2015$geo_id))

# Subnational boundaries from NUTS (EU +)

nuts21 <- st_read(paste0(spid_data,"raw/NUTS/NUTS_RG_01M_2021_4326.shp")) %>%
  mutate(geo_year = 2021)
nuts13<- st_read(paste0(spid_data,"raw//NUTS/NUTS_RG_01M_2013_4326.shp")) %>%
  mutate(geo_year = 2013)
nuts06<- st_read(paste0(spid_data,"raw/NUTS/NUTS_RG_01M_2006_4326.shp")) %>%
  mutate(geo_year = 2006)

nuts <- bind_rows(nuts21,nuts13,nuts06) %>%
  left_join(admin0_codes[c("code","CNTR_CODE", "NUTS_CNTR_NAME")]) %>%
  mutate(geo_id =as.character(NUTS_ID),
         geo_name = NAME_LATN,
         geo_level = as.character(LEVL_CODE),
         countryname = NUTS_CNTR_NAME) %>% 
           distinct(geo_id, .keep_all = TRUE) 

any(is.na(nuts$geo_id))

# Subnational boundaries from GADM v4.1

gadm1 <- st_read(paste0(spid_data,"raw/GADM41/gadm_410-levels.gpkg"), 
                 layer="ADM_1") %>%
  mutate(geo_level = "1")
gadm2 <- st_read(paste0(spid_data,"raw/GADM41/gadm_410-levels.gpkg"), 
                 layer="ADM_2")  %>%
  mutate(geo_level = "2")

gadm41 <- bind_rows(gadm1,gadm2) %>%
  mutate(geo_id =as.character(if_else(!is.na(GID_2),GID_2,GID_1)),
         geo_name =if_else(!is.na(GID_2),NAME_2,NAME_1),
         geo_year = 2022,
         code = GID_0,
         countryname = COUNTRY)

st_geometry(gadm41) <- "geometry"
  
any(is.na(gadm41$geo_id))

# Subnational boundaries from UN COD boundary data

un_bwa2011 <- st_read(paste0(spid_data,"raw/UN/BWA_2011")) %>%
  mutate(code = "BWA", geo_name = ADM2_EN, geo_level = "2", geo_year = 2011)
un_mar2023 <- st_read(paste0(spid_data,"raw/UN/MAR_2023")) %>%
  mutate(code = "MAR", geo_name = ADM1_FR, geo_level = "1", geo_year = 2023)
un_rus2022 <- st_read(paste0(spid_data,"raw/UN/RUS_2022")) %>%
  mutate(code = "RUS", geo_name = ADM1_EN, geo_level = "1", geo_year = 2022)
un_syc2010 <- st_read(paste0(spid_data,"raw/UN/SYC_2010")) %>%
  mutate(code = "SYC", geo_name = ADM2_EN, geo_level = "2", geo_year = 2010)

un <- bind_rows(un_bwa2011,un_mar2023,un_rus2022,un_syc2010) %>%
  mutate(geo_id=as.character(if_else(!is.na(ADM2_PCODE),ADM2_PCODE,ADM1_PCODE)),
         countryname = ADM0_EN)
  
any(is.na(un$geo_id))

# Subnational boundaries from NSO/other custom boundary data

nso_civ2007 <- st_read(paste0(spid_data,"interim/",version,"/CIV_2007")) %>%
  mutate(geo_id = as.character(ADM1_ID), code = "CIV", geo_name = ADM1,
         geo_level = "1", geo_year = 2007, countryname = "Côte d'Ivoire")

nso_cri2022 <- st_read(paste0(spid_data,"raw/NSO/CRI_2022")) %>%
  mutate(geo_id = as.character(COD_UGER), code = "CRI", geo_name = NOMB_UGER,
         geo_level = "1", geo_year = 2022, countryname = "Costa Rica") %>% 
  st_transform(st_crs(4326))

nso_npl2022 <- st_read(paste0(spid_data,"interim/",version,"/NPL_2022")) %>%
  mutate(geo_id = as.character(FIRST_STAT), code = "NPL", geo_name = name, 
         geo_level = "1", geo_year = 2022, countryname = "Nepal")

nso_sur2016 <- st_read(paste0(spid_data,"interim/",version,"/SUR_2016")) %>%
  mutate(geo_id = as.character(d_code), code = "SUR", geo_name = domain,
         geo_level = "1", geo_year = 2016, countryname = "Suriname")

nso_wsm2011 <- st_read(paste0(spid_data,"raw/NSO/WSM_2011")) %>%
  mutate(geo_id = as.character(rid), code = "WSM", geo_name = r_name, 
         geo_level = "1", geo_year = 2011, countryname = "Samoa")

nso <- bind_rows(nso_civ2007,nso_cri2022,nso_npl2022,nso_sur2016,nso_wsm2011)

any(is.na(nso$geo_id))

#------------------------------------------------------------------------------#
# Boundaries that don't need modification
#------------------------------------------------------------------------------#
  
# GAUL 
spid_gaul <- filter(spid_bounds,geo_source=="GAUL" & geo_level!="x") %>% 
  select(c(1,7:14)) %>% distinct(geo_code, .keep_all=TRUE)  %>% 
  left_join(gaul2015[c("countryname","geo_id")])

# NUTS 
spid_nuts <- filter(spid_bounds,geo_source=="NUTS" & geo_level!="x") %>% 
  select(c(1,7:14)) %>% distinct(geo_code, .keep_all=TRUE)  %>% 
  left_join(nuts[c("countryname","geo_id")])

# GADM
spid_gadm <- filter(spid_bounds,geo_source=="GADM" & geo_level!="x") %>% 
  select(c(1,7:14)) %>% distinct(geo_code, .keep_all=TRUE)  %>% 
  left_join(gadm41[c("countryname","geo_id")])

# UN COD
spid_un <- filter(spid_bounds,geo_source=="UN" & geo_level!="x") %>% 
  select(c(1,7:14)) %>% distinct(geo_code, .keep_all=TRUE)  %>% 
  left_join(un[c("countryname","geo_id")])

# NSO/other
spid_nso <- filter(spid_bounds,geo_source=="NSO" & geo_level!="x") %>% 
  select(c(1,7:14)) %>% distinct(geo_code, .keep_all=TRUE)  %>% 
  left_join(nso[c("code","countryname","geo_id")])

# ALL boundaries not to modify
spid_nomod <- bind_rows(spid_gaul, spid_nuts, spid_gadm, spid_un, spid_nso) %>%
  st_as_sf()

#------------------------------------------------------------------------------#
# Modified boundaries
#------------------------------------------------------------------------------#

# GAUL 
spid_gaulx <- filter(spid_x,geo_source=="GAUL") %>% 
  left_join(gaul2015[c("countryname","geo_id")])

# NUTS 
spid_nutsx <- filter(spid_x,geo_source=="NUTS") %>% 
  left_join(nuts[c("countryname","geo_id")])

# GADM
spid_gadmx <- filter(spid_x,geo_source=="GADM") %>% 
  left_join(gadm41[c("countryname","geo_id")])

# UN COD
spid_unx <- filter(spid_x,geo_source=="UN") %>% 
  left_join(un[c("countryname","geo_id")])

# ALL boundaries to modify
spid_tomod <- bind_rows(spid_gaulx, spid_nutsx, spid_gadmx, spid_unx) %>%
  st_as_sf()

# create modified boundaries from union/difference geometry operations
mod_list <- unique(spid_tomod$geo_code)

rm(spid_mod)
sf_use_s2(FALSE)

for (i in 1:length(mod_list)){
  
  source <- filter(spid_tomod,geo_code==mod_list[i]) %>% arrange(mod_order)
  modified <- source[1,c("geo_code","geometry")]
  
  for (r in 2:nrow(source)){
    
    if (source[r,]$mod_type=="union"){
      modified <- st_union(modified,source[r,"geometry"]) %>%
        fill_holes(units::set_units(1, km^2))}
    
    if (source[r,]$mod_type=="difference"){
      modified <- st_difference(modified,source[r,"geometry"]) %>%
        drop_crumbs(units::set_units(1, km^2))}
    
    if (source[r,]$mod_type=="sym_difference"){
      modified <- st_sym_difference(modified,source[r,"geometry"]) %>%
        drop_crumbs(units::set_units(1, km^2))}
    
    if (source[r,]$mod_type=="intersection"){
      modified <- st_intersection(modified,source[r,"geometry"]) %>%
        drop_crumbs(units::set_units(1, km^2))}
  }
  
  if (!exists("spid_mod")){spid_mod <- modified}
  else{spid_mod <- bind_rows(spid_mod,modified)}
}

# merge in codes
spid_mod <- filter(spid_bounds, geo_level=="x") %>%
  distinct(geo_code, .keep_all=TRUE) %>%
  select(code, geo_year,geo_source,geo_level,geo_idvar,geo_id,geo_nvar,
         geo_name,geo_code) %>%
  left_join(spid_mod) %>% st_as_sf()

#------------------------------------------------------------------------------#
# SPID missing regions
#------------------------------------------------------------------------------#

# Loop over each survey-year-byvar

spid_bounds <- mutate(spid_bounds, key = paste(code,year,survname,byvar))
spid_list <- unique(spid_bounds$key)

rm(missed_regions)

for (i in 1:length(spid_list)){  
    print(spid_list[i])
    sample <- filter(spid_bounds,key==spid_list[i]) 
    # add modified boundaries
    if (any(spid_x$geo_code %in% sample$geo_code)){
      sample_mod <- as.data.frame(c(sample[1,c(2:5)],
                    spid_x[spid_x$geo_code %in% sample$geo_code,]))
      sample <- bind_rows(sample, sample_mod) %>% filter(geo_level != "x")
    }
    
    level <- names(which.max(table(sample$geo_level))) 
    idvar <- names(which.max(table(sample$geo_idvar)))
    nvar <- names(which.max(table(sample$geo_nvar)))
    level <- names(which.max(table(sample$geo_level)))
    
    if (sample$code[1] %in% c("IRN","PAN","ZMB")){
      level <- "1"
      idvar <- "ADM1_CODE"
      nvar <- "ADM1_NAME"}
    
    if (sample$geo_source[1]=="GAUL") {source <- gaul2015}
    if (sample$geo_source[1]=="NUTS") {source <- nuts}
    if (sample$geo_source[1]=="GADM") {source <- gadm41}
    if (sample$geo_source[1]=="UN") {source <- un}
    if (sample$geo_source[1]=="NSO") {source <- nso}
    
    missed <- filter(source, code == sample$code[1] & 
                       geo_year == sample$geo_year[1] & 
                       geo_level == level &
                       !geo_id %in% sample$geo_id) %>% 
      select(geo_year, geo_level, geo_id, geo_name, geometry)
    
    if (nrow(missed) > 0){
      missed <- as.data.frame(c(sample[1,c(1:5,8)],missed)) %>%
                              mutate(geo_idvar = idvar, geo_nvar = nvar,
                                     geo_code = paste0(code,"_",geo_year,"_",
                                        geo_source,geo_level,"_",geo_id)) %>%
        select(code,year,survname,surveyid,byvar,geo_year,geo_source,geo_level,
               geo_idvar,geo_id,geo_nvar,geo_name,geo_code, geometry)
    
    if (!exists("missed_regions")){missed_regions <- missed}
    else{missed_regions <- bind_rows(missed_regions,missed)}
  }
}

missed_regions2 <- st_as_sf(missed_regions) %>% 
  filter(code!= "GMB") %>%# covered by crosswalk 
  filter(!(code=="IRN" & geo_name %in% c("Tehran","Khorasan"))) %>% # covered
  filter(!(code=="PAN" & geo_name %in% c("Panamá"))) %>% # covered
  filter(code!="SLE") %>% # covered
  filter(!(code=="THA" & geo_name %in% c("Nong Khai"))) %>% # covered
  filter(!(code=="TZA" & geo_name %in% c("Geita","Simiyu"))) %>% # covered
  arrange(code, year, survname, byvar)

# save list to check
write.xlsx(st_drop_geometry(missed_regions2),
           paste0(spid_data,"interim/",version,"/subnat_missing.xlsx"))

unique(missed_regions2$code)
length(unique(missed_regions2$geo_code))

# filter to unique missed regions not in main spid list
spid_missed <- unique(missed_regions2[c("code", "geo_year","geo_source",
                      "geo_level","geo_idvar","geo_id",
                      "geo_nvar","geo_name","geo_code","geometry")]) %>% 
  filter(!geo_code %in% spid_bounds$geo_code) 

#------------------------------------------------------------------------------#
# Combine subnational boundaries (including missing) and clip to admin-0
#------------------------------------------------------------------------------#

# combine boundaries
spid_noEM <- bind_rows(spid_nomod, spid_mod, spid_missed) %>% 
  arrange(code)

# clip to admin-0 boundary
rm(spid_clip)
wb0 <- st_read(paste0(spid_data,"final/",version,"/",
                      tolower(vintage),"_admin0.gpkg"))
sf_use_s2(FALSE)

for (i in unique(spid_noEM$code)){
  print(i)
  target <- filter(wb0,geo_code == paste0(i,"_2023_WB0")) %>% select(geom)
  clipped <- filter(spid_noEM, code==i) %>% st_intersection(target)
  if (!exists("spid_clip")){spid_clip <- clipped}
  else{spid_clip <- bind_rows(spid_clip,clipped)}
}
sf_use_s2(TRUE)

#------------------------------------------------------------------------------#
# Save subnational boundary data (not edge-matched)
#------------------------------------------------------------------------------#

# subnational regions not mapped by admin-0
dropped <- spid_noEM[!spid_noEM$geo_code %in% spid_clip$geo_code,]
dropped # 40 small islands cropped out

# save list to check
write.xlsx(st_drop_geometry(dropped),
           paste0(spid_data,"interim/",version,"/subnat_dropped.xlsx"))

# add back cropped regions (some have survey samples) & keep polygons only
spid_subnat <- bind_rows(spid_clip, dropped) %>% rowwise() %>% 
  mutate(geometry = st_combine(st_collection_extract(geometry, "POLYGON")))

# save geopackage (masked to admin-0 but NO edge-matching)
st_write(spid_subnat,
         paste0(spid_data,"final/",version,"/",tolower(vintage),"_subnat.gpkg"),
         append=FALSE)

# save shapefile
st_write(spid_subnat,
         paste0(spid_data,"final/",version,"/",tolower(vintage),"_subnat.shp"),
         append=FALSE)
