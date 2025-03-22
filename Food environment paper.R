# Paper title: Characterizing the food environment in Scotland and its association with deprivation: a national study 
# Last updated: 17 February 2025
# Author: Deksha Kapoor 

install.packages("readxl")
install.packages("dplyr")
install.packages("officer")
install.packages("tidyverse")

#load required packages
library(dplyr)
library(tidyr)
library(tidyverse)
library(rio)
library(ggplot2)
library(lubridate)
library(stringr)
library(readxl)
library(flextable)
library(gtsummary)
library(writexl)
library(tibble)
library(readxl)
library(janitor)
library(openxlsx)
library(forcats)
library(officer)

# IMPORT DATA ####
df.foodenv <- read_excel("C:/Users/deksh/OneDrive - University of Edinburgh/AIM 3/FSS data/fsa_20240926.xlsx")
df.la <- read_excel("C:/Users/deksh/OneDrive - University of Edinburgh/AIM 3/FSS data/clean data/local_authority_stats.xlsx")
df.simd <-read_excel("C:/Users/deksh/OneDrive - University of Edinburgh/AIM 3/FSS data/SIMD.xlsx")

# File for figures
file_path<-"C:/Users/deksh/OneDrive - University of Edinburgh/AIM 3/FSS data/paper draft/Health and Place/Github files/fsa_20241410.xlsx"

#File for Local Authority S5 Table
file_path_la<-"C:/Users/deksh/Desktop/3rd Nov FSS analysis/Local authorities.xlsx"

# Shapefile for map
scotland_map <- st_read("C:/Users/deksh/OneDrive - University of Edinburgh/AIM 3/FSS data/clean data/Scotland map_shape file/pub_las.shp") 

#set all column names to lower case and replace spaces and special characters with underscores
df.foodenv <- clean_names(df.foodenv)
df.la <- clean_names(df.la)
df.simd <- clean_names(df.simd)

#drop variables not needed
df.foodenv <- df.foodenv %>%
  select(-x24, -x25, -x26)

#rename for merge
df.la <- df.la  %>% 
  rename(LocalAuthorityName = local_authority_name) 

#fill in NA new_classification as 'Other'
df.foodenv <- df.foodenv %>%
  mutate(
    new_classification = case_when(is.na(new_classification) ~ "Other",
                                   TRUE ~ new_classification)
  )



# DATA CLEANING ####
## STEP 1 deleting non food related establishments from the data set
fsa_analysis_clean <- df.foodenv %>% 
  filter(!grepl(pattern = 'School', 
                x = business_name,
                ignore.case = TRUE)) %>% 
  filter(!grepl(pattern = 'Childminder', 
                x = business_name,
                ignore.case = TRUE)) %>%            
  filter(!grepl(pattern = 'nursery', 
                x = business_name,
                ignore.case = TRUE)) %>% 
  filter(!grepl(pattern = 'hospital', 
                x = business_name,
                ignore.case = TRUE)) %>% 
  filter(!grepl(pattern = 'hostel', 
                x = business_name,
                ignore.case = TRUE)) %>% 
  filter(!grepl(pattern = 'daycare', 
                x = business_name,
                ignore.case = TRUE)) %>% 
  filter(!grepl(pattern = 'day care', 
                x = business_name,
                ignore.case = TRUE)) %>% 
  filter(!grepl(pattern = 'childcare', 
                x = business_name,
                ignore.case = TRUE)) %>% 
  filter(!grepl(pattern = 'child care', 
                x = business_name,
                ignore.case = TRUE)) %>% 
  filter(!grepl(pattern = 'carehome', 
                x = business_name,
                ignore.case = TRUE)) %>% 
  filter(!grepl(pattern = 'care home', 
                x = business_name,
                ignore.case = TRUE))

## STEP2 RECATEGORISING BUSINESSTYPE USING BUSINESSNAME
fsa_analysis_clean$business_type %>% 
  unique() 
# recatagorise all retail chains from 'Retailers - other' to 'Retailers - supermarkets/hypermarkets

fsa_analysis_clean %>% 
  mutate(business_type = case_when(toupper(business_name) == "SCOTMID" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) %>%  
  mutate(business_type = case_when(toupper(business_name) == "MORRISONS" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) %>% 
  mutate(business_type = case_when(toupper(business_name) == "LONDIS" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) %>%  
  mutate(business_type = case_when(toupper(business_name) == "FARMFOODS" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) %>%  
  mutate(business_type = case_when(toupper(business_name) == "SCOTFRESH" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) %>%  
  mutate(business_type = case_when(toupper(business_name) == "SAINSBURY" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) %>%  
  mutate(business_type = case_when(toupper(business_name) == "ICELAND" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) %>%  
  mutate(business_type = case_when(toupper(business_name) == "FOODWAREHOUSE" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) %>%  
  mutate(business_type = case_when(toupper(business_name) == "TESCO" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) %>%  
  mutate(business_type = case_when(toupper(business_name) == "AULDS" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) %>%  
  mutate(business_type = case_when(toupper(business_name) == "ASDA" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) %>%  
  mutate(business_type = case_when(toupper(business_name) == "M&S" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) %>%  
  mutate(business_type = case_when(toupper(business_name) == "CO-OPERATIVE" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) %>% 
  mutate(business_type = case_when(toupper(business_name) == "LIDL" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) %>%  
  mutate(business_type = case_when(toupper(business_name) == "ALDI" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) %>%  
  mutate(business_type = case_when(toupper(business_name) == "ESSO" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) %>%  
  mutate(business_type = case_when(toupper(business_name) == "MARKS AND SPENCER" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) %>%  
  mutate(business_type = case_when(toupper(business_name) == "M & S" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) %>% 
  mutate(business_type = case_when(toupper(business_name) == "COOPERATIVE" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) %>%  
  mutate(business_type = case_when(toupper(business_name) == "SPAR" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) %>%
  mutate(business_type = case_when(toupper(business_name) == "MORRISONS DAILY" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) %>%  
  mutate(business_type = case_when(toupper(business_name) == "MORRISON DAILY" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) %>% 
  mutate(business_type = case_when(toupper(business_name) == "WM MORRISONS SUPERMARKET LIMITED" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) %>% 
  mutate(business_type = case_when(toupper(business_name) == "CO-OP" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) %>% 
  mutate(business_type = case_when(toupper(business_name) == "SCOTTISH CO-OP" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) %>% 
  mutate(business_type = case_when(toupper(business_name) == "COOPERATIVE FOOD" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) %>% 
  mutate(business_type = case_when(toupper(business_name) == "CO-OP FOOD" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) %>% 
  mutate(business_type = case_when(toupper(business_name) == " THE COOPERATIVE FOOD" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) %>% 
  mutate(business_type = case_when(toupper(business_name) == "THE COOPERATIVE FOOD GROUP" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) %>% 
  mutate(business_type = case_when(toupper(business_name) == "THE CO-OPERATIVE" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) %>% 
  mutate(business_type = case_when(toupper(business_name) == "CO-OPERATIVE FOOD STORE" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) %>% 
  mutate(business_type = case_when(toupper(business_name) == "SCOTMID CO-OPERATIVE" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) %>%
  mutate(business_type = case_when(toupper(business_name) == "FARMFOODS LTD" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) %>%
  mutate(business_type = case_when(toupper(business_name) == "FARMFOODS FREEZER CENTRES" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) %>% 
  mutate(business_type = case_when(toupper(business_name) == "FARMFOOD" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) %>% 
  mutate(business_type = case_when(toupper(business_name) == "FARMFOODS (C)" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) %>% 
  mutate(business_type = case_when(toupper(business_name) == "FOOD WAREHOUSE" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) %>% 
  mutate(business_type = case_when(toupper(business_name) == "ICELAND FROZEN FOODS" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) %>% 
  mutate(business_type = case_when(toupper(business_name) == "TESCO STORES LTD" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) %>% 
  mutate(business_type = case_when(toupper(business_name) == "TESCO EXPRESS" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) %>% 
  mutate(business_type = case_when(toupper(business_name) == "TESCO (IN-STORE BAKERY)" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) %>% 
  mutate(business_type = case_when(toupper(business_name) == "ALDI STORES LTD" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) %>% 
  mutate(business_type = case_when(toupper(business_name) == "ALDI STORES LIMITED" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) %>% 
  mutate(business_type = case_when(toupper(business_name) == "ASDA EXPRESS" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) %>% 
  mutate(business_type = case_when(toupper(business_name) == "M & S SIMPLY FOODS" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) %>% 
  mutate(business_type = case_when(toupper(business_name) == "M & S SIMPLY FOOD" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) %>% 
  mutate(business_type = case_when(toupper(business_name) == "MARKS AND SPENCER SIMPLY FOOD" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type)) 

# STEP 3 recatagorise all chains to 'Takeaway/sandwich shop'
fsa_analysis_clean %>% 
  mutate(business_type = case_when(toupper(business_name) == "GREGGS" ~ "Takeaway/sandwich shop",
                                   TRUE ~ business_type)) %>%  
  mutate(business_type = case_when(toupper(business_name) == "GREGGS OF EDINBURGH" ~ "Takeaway/sandwich shop",
                                   TRUE ~ business_type)) %>%  
  mutate(business_type = case_when(toupper(business_name) == "GREGGS OF SCOTLAND" ~ "Takeaway/sandwich shop",
                                   TRUE ~ business_type)) %>%  
  mutate(business_type = case_when(toupper(business_name) == "GREGGS PLC" ~ "Takeaway/sandwich shop",
                                   TRUE ~ business_type)) %>%  
  mutate(business_type = case_when(toupper(business_name) == "BAYNES" ~ "Takeaway/sandwich shop",
                                   TRUE ~ business_type)) %>%  
  mutate(business_type = case_when(toupper(business_name) == "BAYNES FAMILY BAKERS" ~ "Takeaway/sandwich shop",
                                   TRUE ~ business_type)) %>%  
  mutate(business_type = case_when(toupper(business_name) == "BAYNES THE FAMILY BAKERS" ~ "Takeaway/sandwich shop",
                                   TRUE ~ business_type)) %>% 
  mutate(business_type = case_when(toupper(business_name) == "BAYNES THE BAKERS" ~ "Takeaway/sandwich shop",
                                   TRUE ~ business_type)) %>% 
  mutate(business_type = case_when(toupper(business_name) == "BAYNES BAKERS" ~ "Takeaway/sandwich shop",
                                   TRUE ~ business_type)) %>% 
  mutate(business_type = case_when(toupper(business_name) == "BAYNES FAMILY BAKERY" ~ "Takeaway/sandwich shop",
                                   TRUE ~ business_type)) %>% 
  
  # STEP 4 recatagorise all FOODBANKS to 'Other catering premises', chains to 'restaurants/cafe/canteen'
  fsa_analysis_clean %>% 
  mutate(business_type = case_when(toupper(business_name) == "FOODBANKS" ~ "Other catering premises",
                                   TRUE ~ business_type)) %>%  
  mutate(business_type = case_when(toupper(business_name) == "SUBWAY" ~ "Restaurant/Cafe/Canteen",
                                   TRUE ~ business_type)) %>%  
  mutate(business_type = case_when(toupper(business_name) == "STARBUCKS" ~ "Restaurant/Cafe/Canteen",
                                   TRUE ~ business_type)) %>%  
  mutate(business_type = case_when(toupper(business_name) == "COSTA COFFEE" ~ "Restaurant/Cafe/Canteen",
                                   TRUE ~ business_type)) %>%  
  mutate(business_type = case_when(toupper(business_name) == "COSTA" ~ "Restaurant/Cafe/Canteen",
                                   TRUE ~ business_type)) %>%  
  mutate(business_type = case_when(toupper(business_name) == "DOMINO'S PIZZA" ~ "Restaurant/Cafe/Canteen",
                                   TRUE ~ business_type)) %>%  
  mutate(business_type = case_when(toupper(business_name) == "KFC" ~ "Restaurant/Cafe/Canteen",
                                   TRUE ~ business_type)) %>%    
  
  #STEP 5 re categorization of strings in business name  
fsa_analysis_clean$business_type[grepl("hotel", fsa_analysis_clean$business_name)] <- "Restaurant/Cafe/Canteen"
fsa_analysis_clean$business_type[grepl("Hotel", fsa_analysis_clean$business_name)] <- "Restaurant/Cafe/Canteen" 
fsa_analysis_clean$business_type[grepl("Cafe", fsa_analysis_clean$business_name)] <- "Restaurant/Cafe/Canteen" 
fsa_analysis_clean$business_type[grepl("cafe", fsa_analysis_clean$business_name)] <- "Restaurant/Cafe/Canteen"
fsa_analysis_clean$business_type[grepl("coffee", fsa_analysis_clean$business_name)] <- "Restaurant/Cafe/Canteen"
fsa_analysis_clean$business_type[grepl("Coffee", fsa_analysis_clean$business_name)] <- "Restaurant/Cafe/Canteen"
fsa_analysis_clean$business_type[grepl("Tearoom", fsa_analysis_clean$business_name)] <- "Restaurant/Cafe/Canteen"
fsa_analysis_clean$business_type[grepl("Tea room", fsa_analysis_clean$business_name)] <- "Restaurant/Cafe/Canteen"
fsa_analysis_clean$business_type[grepl("Takeaway", fsa_analysis_clean$business_name)] <- "Takeaway/sandwich shop"



# DATA ANALYSIS ####
##Tables##
## Table 2: Classification of food outlets in Scotland (n=31,135).



table2 <- fsa_analysis_clean %>% 
  dplyr::select(local_authority_name, business_type, new_classification) %>% 
  tbl_summary(by = local_authority_name,
              missing = "no",
              digits = list(all_categorical() ~ c(0,0)),
              statistic = list(all_categorical() ~ "{p}% ({n})"),
              label = c(business_type ~ "Business type",
                        new_classification ~ "Food outlet classification")) %>% 
  add_overall() %>%
  bold_labels() %>%
  as_flex_table()
print(table2)


## Distribution of food outlets by local authority ####

### Create a local authority level dataset ####
#get counts of OOH and retail in each local authority
df.fsa <- fsa_analysis_clean %>% 
  group_by(local_authority_name, new_classification) %>%
  summarise(count = n())

#reshape to wide format
df.fsa <- df.fsa %>%
  pivot_wider(names_from = new_classification, values_from = count)

#Combining columns of both dataframes
df.fsa1<- cbind(df.fsa, df.la)

#Removing repeat columns from the combined dataset as a result of combining
df.fsanew<- df.fsa1 %>% select(-LocalAuthorityName)

#rename variables 
df.fsanew <- df.fsanew  %>% 
  rename(count_ooh = OOH,
         count_retail = Retail,
         count_other = Other)

#calculate percent of outlets and density of outlets in local authorities 
df.fsanew <- df.fsanew  %>% 
  mutate(count_total = count_ooh+count_retail+count_other,
         percent_ooh = (count_ooh / count_total)*100,
         percent_retail = (count_retail / count_total)*100,
         percent_other = (count_other / count_total)*100,
         density_sqkm_ooh = count_ooh / land_area_sq_km,
         density_sqkm_retail = count_retail / land_area_sq_km,
         density_pop_ooh = count_ooh / (population/1000),
         density_pop_retail = count_retail / (population/1000)
  )   %>% 
  #reorder variables
  relocate(population, land_area_sq_km, count_ooh, count_retail, count_other, count_total, percent_ooh, percent_retail, percent_other, .after = local_authority_name)

### Create Table 3: Density of out of home (OOH), retail and other food outlets (outlets per km2 ) by local authority in Scotland, 2024 (n=31,135)
## and Supplemental Table S2  #### 
table3a <- df.fsanew %>% 
  dplyr::select(local_authority_name, percent_ooh, percent_retail, density_pop_ooh, density_sqkm_ooh, density_pop_retail,density_sqkm_retail) %>% 
  tbl_summary(by = local_authority_name,
              missing = "no",
              digits = list(percent_ooh ~ 0,
                            percent_retail ~ 0,
                            density_pop_ooh ~ 2,
                            density_sqkm_ooh ~ 2,
                            density_pop_retail ~ 2,
                            density_sqkm_retail ~ 2),
              statistic = list(percent_ooh ~ "{mean}%",
                               percent_retail ~ "{mean}%",
                               density_pop_ooh ~ "{mean}",
                               density_sqkm_ooh ~ "{mean}",
                               density_pop_retail ~ "{mean}",
                               density_sqkm_retail ~ "{mean}"),
              label = c(percent_ooh ~ "Percentage of outlets that are out of home",
                        percent_retail ~ "Percentage of outlets that are retailers",
                        density_pop_ooh ~ "Out of home outlets per 1000 people",
                        density_sqkm_ooh ~ "Out of home outlets per sq km",
                        density_pop_retail ~ "Food retailers per 1000 people",
                        density_sqkm_retail ~ "Food retailers per sq km")) %>% 
  add_overall() %>%
  bold_labels() %>%
  as_flex_table()

#convert flextables to data frames 
df.table3a <- table3a$body$dataset %>%
  rename("Business type" = label,
         "Overall" = stat_0,
         "Aberdeen City" = stat_1,
         "Aberdeenshire" = stat_2,
         "Angus" = stat_3,
         "Argyll and Bute" = stat_4,
         "Clackmannanshire" = stat_5,
         "Comhairle nan Eilean Siar (Western Isles)" = stat_6,
         "Dumfries and Galloway" = stat_7,
         "Dundee City" = stat_8,
         "East Ayrshire" = stat_9,
         "East Dunbartonshire" = stat_10,
         "East Lothian" = stat_11,
         "East Renfrewshire" = stat_12,
         "Edinburgh (City of)" = stat_13,
         "Falkirk" = stat_14,
         "Fife" = stat_15,
         "Glasgow City" = stat_16,
         "Highland" = stat_17,
         "Inverclyde" = stat_18,
         "Midlothian" = stat_19,
         "Moray" = stat_20,
         "North Ayrshire" = stat_21,
         "North Lanarkshire" = stat_22,
         "Orkney Islands" = stat_23,
         "Perth and Kinross" = stat_24,
         "Renfrewshire" = stat_25,
         "Scottish Borders" = stat_26,
         "Shetland Islands" = stat_27,
         "South Ayrshire" = stat_28,
         "South Lanarkshire" = stat_29,
         "Stirling" = stat_30,
         "West Dunbartonshire" = stat_31,
         "West Lothian" = stat_32)

df.table2 <- table2$body$dataset %>%
  rename("Food outlet characteristic" = label,
         "Overall" = stat_0,
         "Aberdeen City" = stat_1,
         "Aberdeenshire" = stat_2,
         "Angus" = stat_3,
         "Argyll and Bute" = stat_4,
         "Clackmannanshire" = stat_5,
         "Comhairle nan Eilean Siar (Western Isles)" = stat_6,
         "Dumfries and Galloway" = stat_7,
         "Dundee City" = stat_8,
         "East Ayrshire" = stat_9,
         "East Dunbartonshire" = stat_10,
         "East Lothian" = stat_11,
         "East Renfrewshire" = stat_12,
         "Edinburgh (City of)" = stat_13,
         "Falkirk" = stat_14,
         "Fife" = stat_15,
         "Glasgow City" = stat_16,
         "Highland" = stat_17,
         "Inverclyde" = stat_18,
         "Midlothian" = stat_19,
         "Moray" = stat_20,
         "North Ayrshire" = stat_21,
         "North Lanarkshire" = stat_22,
         "Orkney Islands" = stat_23,
         "Perth and Kinross" = stat_24,
         "Renfrewshire" = stat_25,
         "Scottish Borders" = stat_26,
         "Shetland Islands" = stat_27,
         "South Ayrshire" = stat_28,
         "South Lanarkshire" = stat_29,
         "Stirling" = stat_30,
         "West Dunbartonshire" = stat_31,
         "West Lothian" = stat_32) 

#save to excel 
write_xlsx(
  list(
    "Sheet 1"= df.table3a,
    "Outlet chars" = df.table2),
  path = "C:/Users/deksh/OneDrive - University of Edinburgh/AIM 3/FSS data/Table_Outlets_ByLocalAuthority.xlsx")

## S4 Association of SIMD with classification of outlets Overall and classification by OOH, retail and others  ##
#create flextables
library(dplyr)
library(gtsummary)
table3 <- fsa_analysis_clean %>% 
  dplyr::select(simd2020_quintile, business_type, new_classification) %>% 
  tbl_summary(by = simd2020_quintile,
              missing = "no",
              digits = list(all_categorical() ~ c(0,0)),
              statistic = list(all_categorical() ~ "{p}% ({n})"),
              label = c(business_type ~ "Business type",
                        new_classification ~ "Food outlet classification")) %>% 
  add_overall() %>%
  bold_labels() %>%
  add_p() %>% 
  as_flex_table() 

save_as_docx(table3,
             path = paste("C:/Users/deksh/OneDrive - University of Edinburgh/AIM 3/FSS data/","Table_Outlets_BySIMD",format(Sys.time(),"%d%m%Y"),".docx"))  


## S5 Association of SIMD with classification of outlets by local authorities ##

# Results - Outlets by SIMD #### 

# Create contingency table for North Ayrshire
Northa <- read_excel(file_path_la, sheet = "Northa")
contingency_table_Northa <- table(Northa$simd2020_quintile, Northa$new_classification)
print(contingency_table_Northa)

# Perform the Chi-squared test
chi_square_test <- chisq.test(contingency_table_Northa)

# Display the results
print(chi_square_test)

# Repeat for all local authorities 
# North Lanarkshire
NorthL <- read_excel(file_path_la, sheet = "North Lanarkshire")
contingency_table_NorthL <- table(NorthL$simd2020_quintile, NorthL$new_classification)
print(contingency_table_NorthL)
chi_square_test <- chisq.test(contingency_table_NorthL)
print(chi_square_test)

# Orkney Islands
Orkney <- read_excel(file_path_la, sheet = "Orkney Islands")
contingency_table_Orkney <- table(Orkney$simd2020_quintile, Orkney$new_classification)
print(contingency_table_Orkney)

#Perth and Kinross
Perth <- read_excel(file_path_la, sheet = "Perth")
contingency_table_Perth <- table(Perth$simd2020_quintile, Perth$new_classification)
print(contingency_table_Perth)
chi_square_test <- chisq.test(contingency_table_Perth)
print(chi_square_test)

#Renfrewshire
Renfrewshire <- read_excel(file_path_la, sheet = "Renfrewshire")
contingency_table_Renfrewshire <- table(Renfrewshire$simd2020_quintile, Renfrewshire$new_classification)
print(contingency_table_Renfrewshire)
chi_square_test <- chisq.test(contingency_table_Renfrewshire)
print(chi_square_test)

#Scottish Borders
ScottishBorders <- read_excel(file_path_la, sheet = "Scottish Borders")
contingency_table_ScottishBorders <- table(ScottishBorders$simd2020_quintile, ScottishBorders$new_classification)
print(contingency_table_ScottishBorders)

#Shetland Islands
ShetlandIslands <- read_excel(file_path_la, sheet = "Shetland Islands")
contingency_table_ShetlandIslands <- table(ShetlandIslands$simd2020_quintile, ShetlandIslands$new_classification)
print(contingency_table_ShetlandIslands)

#South Ayrshire
SouthAyrshire <- read_excel(file_path_la, sheet = "South Ayrshire")
contingency_table_SouthAyrshire <- table(SouthAyrshire$simd2020_quintile, SouthAyrshire$new_classification)
print(contingency_table_SouthAyrshire)

#South Lanarkshire
SouthLanarkshire <- read_excel(file_path_la, sheet = "South Lanarkshire")
contingency_table_SouthLanarkshire <- table(SouthLanarkshire$simd2020_quintile, SouthLanarkshire$new_classification)
print(contingency_table_SouthLanarkshire)
chi_square_test <- chisq.test(contingency_table_SouthLanarkshire)
print(chi_square_test)

#Stirling
Stirling <- read_excel(file_path_la, sheet = "Stirling")
contingency_table_Stirling <- table(Stirling$simd2020_quintile, Stirling$new_classification)
print(contingency_table_Stirling)
chi_square_test <- chisq.test(contingency_table_Stirling)
print(chi_square_test)

#West Dunbartonshire
WestDunbartonshire <- read_excel(file_path_la, sheet = "West Dunbartonshire")
contingency_table_WestDunbartonshire <- table(WestDunbartonshire$simd2020_quintile, WestDunbartonshire$new_classification)
print(contingency_table_WestDunbartonshire)

#West Lothian
WestLothian <- read_excel(file_path_la, sheet = "West Lothian")
contingency_table_WestLothian <- table(WestLothian$simd2020_quintile, WestLothian$new_classification)
print(contingency_table_WestLothian)

#Dundee
Dundee <- read_excel(file_path_la, sheet = "Dundee")
contingency_table_Dundee <- table(Dundee$simd2020_quintile, Dundee$new_classification)
print(contingency_table_Dundee)

#Comhairle nan Eilean Siar (Western Isles)
WesternIsles <- read_excel(file_path_la, sheet = "western Isles")
contingency_table_WesternIsles <- table(WesternIsles$simd2020_quintile, WesternIsles$new_classification)
print(contingency_table_WesternIsles)

#Dumfries
Dumfries <- read_excel(file_path_la, sheet = "Dumfries")
contingency_table_Dumfries <- table(Dumfries$simd2020_quintile, Dumfries$new_classification)
print(contingency_table_Dumfries)

#Argyll and Bute
AB <- read_excel(file_path_la, sheet = "Argyll and Bute")
contingency_table_AB <- table(AB$simd2020_quintile, AB$new_classification)
print(contingency_table_AB)

#Clackmannanshire
Clackmannanshire <- read_excel(file_path_la, sheet = "Clackmannanshire")
contingency_table_Clackmannanshire <- table(Clackmannanshire$simd2020_quintile, Clackmannanshire$new_classification)
print(contingency_table_Clackmannanshire)

#Angus
Angus <- read_excel(file_path_la, sheet = "Angus")
contingency_table_Angus <- table(Angus$simd2020_quintile, Angus$new_classification)
print(contingency_table_Angus)

#Aberdeen City
Aberdeen <- read_excel(file_path_la, sheet = "Aberdeen City")
contingency_table_Aberdeen <- table(Aberdeen$simd2020_quintile, Aberdeen$new_classification)
print(contingency_table_Aberdeen)
chi_square_test <- chisq.test(contingency_table_Aberdeen)
print(chi_square_test)

#Aberdeenshire
Aberdeenshire <- read_excel(file_path_la, sheet = "Aberdeenshire")
contingency_table_Aberdeenshire <- table(Aberdeenshire$simd2020_quintile, Aberdeenshire$new_classification)
print(contingency_table_Aberdeenshire)
chi_square_test <- chisq.test(contingency_table_Aberdeenshire)
print(chi_square_test)

#East Ayrshire
EA <- read_excel(file_path_la, sheet = "East Ayrshire")
contingency_table_EA <- table(EA$simd2020_quintile, EA$new_classification)
print(contingency_table_EA)

#East Dunbartonshire
ED <- read_excel(file_path_la, sheet = "East Dunbartonshire")
contingency_table_ED <- table(ED$simd2020_quintile, ED$new_classification)
print(contingency_table_ED)

#East Lothian
EL <- read_excel(file_path_la, sheet = "East Lothian")
contingency_table_EL <- table(EL$simd2020_quintile, EL$new_classification)
print(contingency_table_EL)

#East Renfrewshire
ER <- read_excel(file_path_la, sheet = "East Renfrewshire")
contingency_table_ER <- table(ER$simd2020_quintile, ER$new_classification)
print(contingency_table_ER)

#Edinburgh (City of)
Edinburgh <- read_excel(file_path_la, sheet = "Edinburgh")
contingency_table_Edinburgh <- table(Edinburgh$simd2020_quintile, Edinburgh$new_classification)
print(contingency_table_Edinburgh)
chi_square_test <- chisq.test(contingency_table_Edinburgh)
print(chi_square_test)

#Falkirk
Falkirk <- read_excel(file_path_la, sheet = "Falkirk")
contingency_table_Falkirk <- table(Falkirk$simd2020_quintile, Falkirk$new_classification)
print(contingency_table_Falkirk)

#Fife
Fife<- read_excel(file_path_la, sheet = "Fife")
contingency_table_Fife <- table(Fife$simd2020_quintile, Fife$new_classification)
print(contingency_table_Fife)
chi_square_test <- chisq.test(contingency_table_Fife)
print(chi_square_test)

#Glasgow City
Glasgow <- read_excel(file_path_la, sheet = "Glasgow")
contingency_table_Glasgow <- table(Glasgow$simd2020_quintile, Glasgow$new_classification)
print(contingency_table_Glasgow)
chi_square_test <- chisq.test(contingency_table_Glasgow)
print(chi_square_test)

# Highlands
Highland <- read_excel(file_path_la, sheet = "Highland")
contingency_table_Highland <- table(Highland$simd2020_quintile, Highland$new_classification)
print(contingency_table_Highland)
chi_square_test <- chisq.test(contingency_table_Highland)
print(chi_square_test)

#Inverclyde
Inverclyde <- read_excel(file_path_la, sheet = "Inverclyde")
contingency_table_Inverclyde <- table(Inverclyde$simd2020_quintile, Inverclyde$new_classification)
print(contingency_table_Inverclyde)

#Midlothian
MidL <- read_excel(file_path_la, sheet = "Midlothian")
contingency_table_MidL <- table(MidL$simd2020_quintile, MidL$new_classification)
print(contingency_table_MidL)

#Moray
Moray <- read_excel(file_path_la, sheet = "Moray")
contingency_table_Moray <- table(Moray$simd2020_quintile, Moray$new_classification)
print(contingency_table_Moray)


### Figures###
##Create Figure 2. Distribution of food outlets by local authority in Scotland, 2024 (n=31,135) 
##using data from table 2##

## (a)	Frequency of food outlets ## 
graph_data <- read_excel(file_path, sheet = "Graphdata")

# List of packages to install and load
packages <- c("ggplot2", "plotly", "lattice", "cowplot", "gridExtra",
              "sf", "rgdal", "rgeos", "tmap", "leaflet", "maps", "mapdata", "maptools")

# Install packages that are not already installed
installed_packages <- installed.packages()[,"Package"]
packages_to_install <- packages[!packages %in% installed_packages]

if(length(packages_to_install) > 0) {
  install.packages(packages_to_install)
}
# Load all libraries
lapply(packages, library, character.only = TRUE)

# Check which packages failed to load
failed_to_load <- packages[!packages %in% (.packages())]

if(length(failed_to_load) > 0) {
  cat("The following packages failed to load:", failed_to_load, "\n")
} else {
  cat("All packages loaded successfully.\n")
}
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)

# Reshape the data from wide to long format
df_long <- graph_data  %>%
  pivot_longer(cols = -`Local Authority`, names_to = "Category", values_to = "Count")

custom_colors <- c("Restaurant/Cafe/Canteen" = "skyblue", "Takeaway/sandwich shop"="blue3", "Retailers - other"="darkgrey", "Mobile caterer" = "green4", "Pub/bar/nightclub"= "cyan", "Retailers - supermarkets/hypermarkets"="grey","Other catering premises" = "green2" )

# Create the stacked bar plot
s <- ggplot(df_long, aes(x = reorder(`Local Authority`, -Count), y = Count, fill = Category)) +
  geom_bar(stat = "identity", position = "stack") +  scale_fill_manual(values = custom_colors) +
  labs(title = "Number of Outlets per Local Authority",
       x = "Local Authority",
       y = "Number of Outlets",
       fill = "Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5, hjust = 1),
        legend.text = element_text(size = 8)
  )
# Save a larger version of the plot
ggsave("Count_bar_chart.png", s, width = 12, height = 49, dpi = 800)
# Display the plot
print(s)

## (b) Proportion of outlets  
graph2data <- read_excel(file_path, sheet = "PGraph")

# Reshape the data from wide to long format
df_long <- graph2data  %>%
  pivot_longer(cols = -`Local Authority`, names_to = "Category", values_to = "Percentage")
custom_colors <- c("Restaurant/Cafe/Canteen" = "skyblue", "Takeaway/sandwich shop"="blue3", "Retailers - other"="darkgrey", "Mobile caterer" = "green4", "Pub/bar/nightclub"= "cyan", "Retailers - supermarkets/hypermarkets"="grey","Other catering premises" = "green2" )
# Create the stacked bar plot
q <- ggplot(df_long, aes(x = `Local Authority`, y = Percentage, fill = Category)) +
  geom_bar(stat = "identity", position = "stack") +  scale_fill_manual(values = custom_colors) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of Outlets by Local Authority",
       x = "Local Authority",
       y = "Proportion of Outlets",
       fill = "Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5, hjust = 1),
        legend.text = element_text(size = 8))
# Save a larger version of the plot
ggsave("Percentage_bar_chart.png", q, width = 12, height = 49, dpi = 800)
# Display the plot
print(q)

## Figure 3. Proportion of food outlets classified as (a) Out of home (OOH, n=18,707) and (b) Retail (n=8,847), 
## by local authority in Scotland, 2024 using data from table 2 ##	

dfretail <- read_excel(file_path, sheet = "Retail")

# Ensure column names match expected format
colnames(dfretail) <- c("Local Authority", "Retail")

# Merge map data with the dataset
scotland_map <- scotland_map %>%
  left_join(dfretail, by = c("local_auth" = "Local Authority"))

# Compute centroids for labeling 
#scotland_map$centroid <- st_centroid(scotland_map$geometry)
#scotland_map$lon <- st_coordinates(scotland_map$centroid)[, 1]
#scotland_map$lat <- st_coordinates(scotland_map$centroid)[, 2]

# Create the choropleth map
ggplot(scotland_map) +
  geom_sf(aes(fill = Retail), color = "white") + 
  #  geom_text(data = scotland_map, aes(x = lon, y = lat, label = Retail), 
  #            size = 2, color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "black") +
  labs(title = "Proportion of Retail in Scotland",
       fill = "Retail Percentage") +
  theme_minimal() + 
  theme_void()  

#Repeat same process for OOH
#Read source file
dfooh <- read_excel(file_path, sheet = "OOH")
## Ensure column names match expected format
colnames(dfooh) <- c("Local Authority", "OOH")
# Join the map data with the dataset
scotland_map <- scotland_map %>%
  left_join(dfooh, by = c("local_auth" = "Local Authority"))
# Create the choropleth map
ggplot(scotland_map) +
  geom_sf(aes(fill = OOH), color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "black") +
  labs(title = "Proportion of OOH in Scotland",
       fill = "OOH Percentage") +
  theme_minimal() + 
  theme_void()  			

## Figure 4. Association of SIMD with classification of outlets  ##
## (a) Overall Scotland- all outlets ##

graph1 <- read_excel(file_path, sheet = "Graph1")

# Convert data to long format for ggplot2
install.packages("reshape2")
library(reshape2)
data_long <- melt(graph1, id.vars = "SIMDQuintile", variable.name = "Category", value.name = "Percentage")

# Ensure Quintile is treated as a factor (categorical)
data_long$SIMDQuintile <- as.factor(data_long$SIMDQuintile)

custom_colors <- c("Restaurant/Cafe/Canteen" = "skyblue", "Takeaway/sandwich shop"="blue3", "Retailers - other"="darkgrey", "Mobile caterer" = "green4", "Pub/bar/nightclub"= "cyan", "Retailers - supermarkets/hypermarkets"="grey","Other catering premises" = "green2" )

# Create the stacked bar chart
install.packages("ggplot2")
library("ggplot2")
a<-ggplot(data_long, aes(x = SIMDQuintile, y = Percentage, fill = Category)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) + scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  labs(title = "Stacked Bar Chart by Quintile", x = "SIMDQuintile", y = "Percentage") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(legend.title = element_blank(), legend.position = "bottom", legend.key.size = unit(0.5, "cm"))

# Save a larger version of the plot
ggsave("Percentage_bar_chart.png", a, width = 12, height = 49, dpi = 800)
# Display the plot
print(a)

## (b) Overall Scotland- subcatagorised ##

#Read the file
graph2 <- read_excel(file_path, sheet = "Graph2")

# Convert data to long format for ggplot2
data_long2 <- melt(graph2, id.vars = "SIMDQuintile", variable.name = "Category", value.name = "Percentage")

# Ensure Quintile is treated as a factor (categorical)
data_long2$SIMDQuintile <- as.factor(data_long2$SIMDQuintile)

custom_colors2 <- c("Retail" = "green4", "OOH"="blue4", "Others"="lightblue" )

# Create the stacked bar chart
b<-ggplot(data_long2, aes(x = SIMDQuintile, y = Percentage, fill = Category)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) + scale_fill_manual(values = custom_colors2) +
  theme_minimal() +
  labs(title = "Categories by Quintile", x = "SIMDQuintile", y = "Percentage") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(legend.title = element_blank(), legend.position = "bottom", legend.key.size = unit(0.5, "cm"))

# Save a larger version of the plot
ggsave("Percentage_bar_chart.png", b, width = 12, height = 49, dpi = 800)

# Display the plot
print(b)

## (c) City of Edinburgh1 example of a local authority with higher proportion of both OOH and retail 
## in least deprived neighborhoods compared to most deprived ##

#Repeat above process for Graph 3 for Edinburgh
#Read the file
graph3 <- read_excel(file_path, sheet = "Edinburgh")

# Convert data to long format for ggplot2
data_long3 <- melt(graph3, id.vars = "SIMDQuintile", variable.name = "Category", value.name = "Percentage")

# Ensure Quintile is treated as a factor (categorical)
data_long3$SIMDQuintile <- as.factor(data_long3$SIMDQuintile)

custom_colors3 <- c("Retail" = "green4", "OOH"="blue4", "Others"="lightblue" )

# Create the stacked bar chart
c<-ggplot(data_long3, aes(x = SIMDQuintile, y = Percentage, fill = Category)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) + scale_fill_manual(values = custom_colors2) +
  theme_minimal() +
  labs(title = "Categories by Quintile", x = "SIMDQuintile", y = "Percentage") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(legend.title = element_blank(), legend.position = "bottom", legend.key.size = unit(0.5, "cm"))

# Save a larger version of the plot
ggsave("Percentage_bar_chart.png", c, width = 12, height = 49, dpi = 800)

# Display the plot
print(c)

## (d) Glasgow city2 example of a local authority with higher proportion of both OOH and retail 
## in most deprived neighborhoods compared to least deprived ##

#Repeat above process for Graph 4 for Glasgow
#Read the file
graph4 <- read_excel(file_path, sheet = "Glasgow")

# Convert data to long format for ggplot2
data_long4 <- melt(graph4, id.vars = "SIMDQuintile", variable.name = "Category", value.name = "Percentage")

# Ensure Quintile is treated as a factor (categorical)
data_long4$SIMDQuintile <- as.factor(data_long4$SIMDQuintile)

custom_colors4 <- c("Retail" = "green4", "OOH"="blue4", "Others"="lightblue" )

# Create the stacked bar chart
d<-ggplot(data_long4, aes(x = SIMDQuintile, y = Percentage, fill = Category)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) + scale_fill_manual(values = custom_colors2) +
  theme_minimal() +
  labs(title = "Categories by Quintile", x = "SIMDQuintile", y = "Percentage") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(legend.title = element_blank(), legend.position = "bottom", legend.key.size = unit(0.5, "cm"))
# Save a larger version of the plot
ggsave("Percentage_bar_chart.png", d, width = 12, height = 49, dpi = 800)
# Display the plot
print(d)
