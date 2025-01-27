****Food environment in Scotland****

The aim of this analysis is to characterize the food environment across Scotland and how the food environment varies by neighbourhood deprivation. 


***Data Files***

1. FSA dataset github 2024.xlsx
   
This file contains food outlet data downloaded for all local authorities in Scotland from the Food Standards Agency (FSA) website on 1 February 2024: https://ratings.food.gov.uk/open-data


2. local_authority_stats.xlsx

This file contains the land area (from WEBSITE) and population (from https://www.scotlandscensus.gov.uk/2022-results/scotland-s-census-2022-rounded-population-estimates/) for each local authority.


3. SIMD_2020v2_postcode_lookup.xlsx

This file contains the Scottish Index of Multiple Deprivation (SIMD) rank for 6,976 small areas (called data zones), considering information across seven domains: income, employment, education, health, 
access to services, crime, and housing. All 6,976 data zones are grouped into 5 bands (quintiles), each containing 20% of the data zones. Quintile 1 contains the 20% most 
deprived data zones in Scotland. As the SIMD score increases the level of deprivation decreases. These Look-up tables which relate individual postcodes to data zones were downloaded from https://www.nrscotland.gov.uk/statistics-and-data/geography/nrs-postcode-extract. 


***Code Files***

1. Data cleaning and Analysis Github.R

This R script includes:

-Data cleaning for FSA dataset to categorize food establishments as out of home (including restaurants, pubs, cafés, and takeaways), retail (including supermarkets and other establishments that primarily sell non-food products with a 
limited range of food products such as pharmacies) or other (mobile caterers, charity organizations, and home caterers).

-Outputs summary tables and figures of food outlet classification, overall and by local authority.

-Analyses the association between food outlet density and SIMD, overall and by local authority.
