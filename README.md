# Boundaries for subnational household surveys

This repository includes R scripts to produce boundary data files corresponding with the World Bank Subnational Poverty and Inequality Database (SPID). 

The boundary data is used to map the [SPID](https://pipmaps.worldbank.org/en/data/datatopics/poverty-portal/poverty-interactivemap), the [Global Subnational Atlas of Poverty (GSAP)](https://pipmaps.worldbank.org/en/data/datatopics/poverty-portal/poverty-geospatial) and to estimate the population at high risk from climate-related hazards for the [WBG scorecard vision indicator](https://scorecard.worldbank.org/en/scorecard/our-vision#planet).

## Overview

## Data

## Description
1. Prepare admin-0 data using official world bank country polygons.
2. Prepare non-standard subnational boundary data-files
3. Combine subnational boundaries based on SPID master list
 - prepare source boundary data
 - get unmodified boundaries
 - construct modified boundaries
 - get missing subnational boundaries
 - clip to Official WB admin-0 polygons
4. Edge-match subnational boundaries to WB admin-0 polygons

## Instructions

To run the code:
1. Clone the repository.
2. Obtain the raw spatial data files and place them in the specified folders.
3. Prepare the SPID boundary master list excel file.
3. Open the 00.MASTER.R script.
 - change lines 

## References
