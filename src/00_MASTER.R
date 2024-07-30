#------------------------------------------------------------------------------#
#     Geo-boundaries for Subnational Poverty and Inequality Data (SPID)        #
#                             R master script                                  #
#------------------------------------------------------------------------------#

# set directory to boundary data
spid_data <- paste0("/Users/bbrunckhorst/Library/CloudStorage/OneDrive-WBG/",
                    "spid-boundaries/data/")

# set path to SPID master list (xlsx)
spid_master <- paste0("/Users/bbrunckhorst/Library/CloudStorage/OneDrive-WBG/",
                      "Subnational/04.output/SPID boundaries.xlsx")
# set vintage
vintage <- "AM24"

#------------------------------------------------------------------------------#

# create data folders for SPID vintage
dir.create(paste0(spid_data,"interim/",vintage))
dir.create(paste0(spid_data,"final/",vintage))

# install packages using renv
renv::restore()

# run scripts
source("01_admin0.R")
source("02_subnat_prep.R")
source("03_subnat.R")
source("04_edgematch.R")
