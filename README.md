****Food environment in Scotland****

The aim of this analysis was to characterise the food environment across Scotland and how the food environment varies by neighbourhood deprivation. 


***Data Files***

1. fsa_20240926.xlsx
   
This file contains food outlet data downloaded for all local authorities in Scotland from the Food Standards Agency (FSA) website on 1 February 2024: https://ratings.food.gov.uk/open-data


2. local_authority_stats.xlsx

This file contains the land area (from https://www.gov.scot/publications/scottish-government-urban-rural-classification-2020/documents/) and population (from https://www.scotlandscensus.gov.uk/2022-results/scotland-s-census-2022-rounded-population-estimates/) for each local authority.


3. SIMD.xlsx

This file contains the Scottish Index of Multiple Deprivation (SIMD) rank for 6,976 small areas (called data zones), considering information across seven domains: income, employment, education, health, access to services, crime, and housing. All 6,976 data zones are grouped into 5 bands (quintiles), each containing 20% of the data zones. Quintile 1 contains the 20% most deprived data zones in Scotland. As the SIMD score increases the level of deprivation decreases. These Look-up tables which relate individual postcodes to data zones were downloaded from https://www.nrscotland.gov.uk/statistics-and-data/geography/nrs-postcode-extract. 


4. Local authorities.xlsx

This file contains data for each local authority on different sheets for looking at associations with SIMD with ease.


6. fsa_20241410.xlsx

This file contains organised data from table 2 to create figures with ease.
   

***Code Files***

1. Food_Environment_Scotland.R

This R script includes:

-Data cleaning for the FSA dataset to categorise food establishments as OOH (including restaurants, pubs, cafés, and takeaways), retail (including supermarkets and other establishments that primarily sell non-food products with a limited range of food products such as pharmacies) or other (mobile caterers, charity organizations, and home caterers).

-Outputs summary tables and figures of food outlet classification, overall and by local authority.

-Conducts an analysis of the association between food outlet density and SIMD, overall and by local authority. 

2. Food_establishment_matching.ipynb

   
