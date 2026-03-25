#------------------------------------------------------------------------------#
#     Geo-boundaries for Subnational Poverty and Inequality Data (SPID)        #
#                             R master script                                  #
#------------------------------------------------------------------------------#

# set vintage
vintage <- "SM26"

# set directory to boundary data
# spid_data <- "~/Library/CloudStorage/OneDrive-WBG/spid-boundaries/data/"
spid_data <- "C:/Users/wb587256/OneDrive - WBG/spid-boundaries/data/"

# set path to SPID master list (xlsx)
# spid_master <- "~/Library/CloudStorage/OneDrive-WBG/Minh\ Cong\ Nguyen\'s\ files\ -\ Poverty\ and\ Shared\ Prosperity\ SM2026/Subnational/02.input/SPID\ boundaries\ SM26.xlsx"
spid_master <- "C:/Users/wb587256/OneDrive - WBG/Minh\ Cong\ Nguyen\'s\ files\ -\ Poverty\ and\ Shared\ Prosperity\ SM2026/Subnational/02.input/SPID\ boundaries\ SM26.xlsx"

#------------------------------------------------------------------------------#

# add date stamp to vintage
version <- paste0(vintage,"_",Sys.Date())

# create output data folders for version
dir.create(paste0(spid_data,"interim/",version))
dir.create(paste0(spid_data,"final/",version))

# install packages using renv
# renv::restore()

# # install packages (without using renv)
# renv::deactivate()
# install.packages(c("dplyr", "lwgeom", "sf", "smoothr", "openxlsx2"))

# run scripts to create boundary data
source("src/01_admin0.R")
source("src/02_subnat_prep.R")
source("src/03_subnat.R")
source("src/04_edgematch.R")

# validate boundary data
source("src/05_validate.R")
