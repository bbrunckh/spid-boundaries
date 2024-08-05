# Boundaries for subnational household surveys

This repository includes R scripts to produce subnational boundary data files corresponding with representative subnational survey data.

The boundary data can be used to map the [Global Subnational Atlas of Poverty (GSAP)](https://pipmaps.worldbank.org/en/data/datatopics/poverty-portal/poverty-geospatial), the [Subnational Poverty and Inequality Database (SPID)](https://pipmaps.worldbank.org/en/data/datatopics/poverty-portal/poverty-interactivemap), and for estimating the population at high risk from climate-related hazards [WBG scorecard vision indicator](https://scorecard.worldbank.org/en/scorecard/our-vision#planet).

## Overview
The code completes the following tasks:
1. Prepares the admin-0 boundary data so that it corresponds with World Bank country codes and has unique geo_codes.
2. Prepares subnational boundary data files.
3. Collates and modifies raw boundary data cso that it matches subnational samples in the SPID master list.
4. Edge-matches subnational boundaries to admin-0 boundaries. Ensures that they form a valid planar partition, made of valid polygons and having no gaps or overlaps.

## Data

**World Bank Official Boundaries** are used to map admin-0 and disputed areas. Data files are available from the [Development Data Hub](https://datacatalog.worldbank.org/search/dataset/0038272/World-Bank-Official-Boundaries).

**Subnational boundary data** sources include Global Administrative Unit Layers (GAUL) 2015, Nomenclature of Territorial Units for Statistics (NUTS), GADM (v4.1), United Nations Common Operational Datasets, and National Statistical Offices (NSOs).

The **SPID master list** maps each subnational household survey sample to regions mapped by the boundary data sources. This excel file provides the code with specific instructions to match and modify raw boundary data so that it corresponds with the geographic identifiers in household surveys.

## Instructions

To run the code and produce master spatial data files:

1. Clone the repository
2. Obtain the raw spatial data files and place them in the specified folders
3. Prepare the SPID boundary master list excel file
3. Run `00.MASTER.R`[^1]
    - modify line 7 with the `/data` directory you are using (with raw spatial data)
    - modify line 11 with file path to the the SPID boundary master
    - modify line 14 with the vintage (e.g., "AM24")
  
[^1]: The R package _renv_ is used to install the same version of packages and dependencies. In case this fails, deactivate _renv_ `renv::deactivate()` and try to run the master R script without `renv::restore` after installing the following packages (and their dependencies) from CRAN: _sf_, _smoothr_, _lwgeom_, _dplyr_, _openxlsx_.

## Description of code files

* `00_MASTER.R` sets directories, installs packages and runs all scripts
* `01_admin0.R` prepares admin-0 boundaries
* `02_subnat_prep.R` prepares non-standard subnational boundary data
* `03_subnat.R` combines subnational boundary data based on SPID master list:
    - prepare source boundary data
    - get unmodified boundaries
    - construct modified boundaries
    - look-up missing subnational boundaries
    - clip subnational boundaries to admin-0 polygons
* `04_edgematch.R` edge-matches subnational boundaries to admin-0 polygons

## References

## Acknowlegements
