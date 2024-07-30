# Boundaries for subnational household surveys

This repository includes R scripts to produce subnational boundary data files corresponding with the World Bank Subnational Poverty and Inequality Database (SPID). 

The boundary data can be used to map the [SPID](https://pipmaps.worldbank.org/en/data/datatopics/poverty-portal/poverty-interactivemap), the [Global Subnational Atlas of Poverty (GSAP)](https://pipmaps.worldbank.org/en/data/datatopics/poverty-portal/poverty-geospatial) and to estimate the population at high risk from climate-related hazards for the [WBG scorecard vision indicator](https://scorecard.worldbank.org/en/scorecard/our-vision#planet).

## Overview

The proce

## Data
Subnational boundary data sources include Global Administrative Unit Layers (GAUL) 2015, Nomenclature of Territorial Units for Statistics (NUTS), GADM 4.1, United Nations Common Operational Datasets, and National Statistical Offices (NSOs).

## Instructions

To run the code and produce final spatial data files:
1. Clone the repository
2. Obtain the raw spatial data files and place them in the specified folders
3. Prepare the SPID boundary master list excel file
3. Open src/00.MASTER.R
  - change line 7 to the /data directory (with raw spatial data)
  - change line 11 to the SPID boundary master list excel file path
  - change line 14 to the vintage (e.g., "AM24")
  - run the script[^1]
  
## Description of code files

1. Prepare admin-0 data using official world bank country polygons.
2. Prepare non-standard subnational boundary data-files
3. Combine subnational boundaries based on SPID master list
 - prepare source boundary data
 - get unmodified boundaries
 - construct modified boundaries
 - get missing subnational boundaries
 - clip to Official WB admin-0 polygons
4. Edge-match subnational boundaries to WB admin-0 polygons

[^1]: The R package _renv_ is used to install the same version of packages and dependencies. In case this fails, deactivate _renv_ `renv::deactivate()` and try to run the master R script without `renv::restore` after installing the following packages (and their dependencies) from CRAN: _sf_, _smoothr_, _lwgeom_, _dplyr_, _openxlsx_.

## References
