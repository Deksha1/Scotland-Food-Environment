***Food environment in Scotland***
The aim of this analysis is to characterize the food environment across Scotland and how the food environment varies by neighbourhood deprivation. 
Most recent dataset of all registered food businesses in 2024 from the Food Standards Agency was used. 

**Data Management Files**
•	Data cleaning and Analysis Github.R
This R script includes the data cleaning done on the Food Standards Agency dataset to categorise food establishments as out of home 
(including restaurants, pubs, cafés, and takeaways), retail (including supermarkets and other establishments that primarily sell non-food products with a 
limited range of food products such as pharmacies) or other (mobile caterers, charity organizations, and home caterers). This also includes codes for analysing 
association of density of types of outlets with deprivation in all local authorities in Scotland.

Two data files are needed to run this R script:
•	FSA dataset github 2024- This excel sheet is the clean dataset of included food outlets in Scotland for this analysis. 
•	SIMD+2020v2+-+postcode+lookup+-+updated+2023%023.xlsx – This excel sheet was used to link each food outlet with Scottish Index of Multiple Deprivation (SIMD) quintile. 
SIMD is a relative measure of deprivation across 6,976 small areas (called data zones), considering information across seven domains: income, employment, education, health, 
access to services, crime, and housing. All 6,976 data zones are grouped into 5 bands (quintiles), each containing 20% of the data zones. Quintile 1 contains the 20% most 
deprived data zones in Scotland. As the SIMD score increases the level of deprivation decreases. These Look-up tables which relate individual postcodes to data zones are 
available at https://www.nrscotland.gov.uk/statistics-and-data/geography/nrs-postcode-extract. 

