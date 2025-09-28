# Paper title: Characterizing the food environment in Scotland and its association with deprivation: a national study 
# Last updated: 11  August  2025
# Author: Deksha Kapoor
# Supervisor: Lindsay Jaacks

# Prepare R environment ####

## Clear R environment ####
rm(list = ls())

## Installing all the required packages ####
install.packages("readxl")
install.packages("dplyr")
install.packages("officer")
install.packages("tidyverse")
install.packages("sf")
install.packages("tmap")
install.packages("reshape2")
install.packages("ggplo2")
install.packages("tidyr")
install.packages("gtsummary")
install.packages("flextable")

## Loading all required packages ####
library(readxl)
library(dplyr)
library(tidyr)
library(tidyverse)
library(rio)
library(ggplot2)
library(lubridate)
library(stringr)
library(flextable)
#library(gtsummary)
library(writexl)
library(tibble)
library(janitor)
library(openxlsx)
library(forcats)
library(officer)
library(sf)
library(tmap)
library(reshape2)

##setwd("Set working directory path")

# IMPORT DATA ####

df.foodenv <- read_excel("fsa_20240926.xlsx") #FSA Data set
df.la <- read_excel("local_authority_stats.xlsx") #Local authority 
df.simd <-read_excel("SIMD.xlsx") # SIMD quintiles in each postcode

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

## STEP 1 deleting non food related establishments from the data set ####
fsa_analysis_clean <- df.foodenv %>%
  filter(!str_detect(business_name, regex("School|Childminder|nursery|hospital|hostel|daycare|day care|childcare|child care|carehome|care home",ignore_case = TRUE)))


## STEP 2 re categorize all retail chains from 'Retailers - other' to 'Retailers - supermarkets/hypermarkets ####
fsa_analysis_clean <- fsa_analysis_clean %>%
  mutate(business_type = case_when(toupper(business_name) %in% 
                                     c("SCOTMID", 
                                       "MORRISONS", 
                                       "MORRISONS DAILY",
                                       "MORRISON DAILY",
                                       "WM MORRISONS SUPERMARKET LIMITED",
                                       "LONDIS",
                                       "SCOTFRESH",
                                       "SAINSBURY",
                                       "ICELAND",
                                       "ICELAND FROZEN FOODS",
                                       "FOODWAREHOUSE",
                                       "TESCO",
                                       "TESCO STORES LTD",
                                       "TESCO EXPRESS",
                                       "TESCO (IN-STORE BAKERY)",
                                       "ASDA",
                                       "ASDA EXPRESS",
                                       "MARKS AND SPENCER",
                                       "M & S",
                                       "M&S",
                                       "M & S SIMPLY FOODS",
                                       "M & S SIMPLY FOOD",
                                       "MARKS AND SPENCER SIMPLY FOOD",
                                       "LIDL",
                                       "CO-OP",
                                       "SCOTTISH CO-OP",
                                       "CO-OP FOOD",
                                       "SCOTMID CO-OPERATIVE",
                                       "CO-OPERATIVE",
                                       "COOPERATIVE",
                                       "COOPERATIVE FOOD",
                                       "THE CO-OPERATIVE",
                                       "THE COOPERATIVE FOOD",
                                       "THE COOPERATIVE FOOD GROUP",
                                       "CO-OPERATIVE FOOD STORE",
                                       "ALDI",
                                       "ALDI STORES LTD",
                                       "ALDI STORES LIMITED",
                                       "AULDS",
                                       "SPAR",
                                       "ESSO",
                                       "FOOD WAREHOUSE",
                                       "FARMFOODS LTD",
                                       "FARMFOOD",
                                       "FARMFOODS (C)",
                                       "FARMFOODS FREEZER CENTRES",
                                       "FARMFOODS") ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type ))


fsa_analysis_clean <- fsa_analysis_clean %>%
  mutate(new_classification = case_when(toupper(business_name) %in% 
                                          c("SCOTMID", 
                                            "MORRISONS", 
                                            "MORRISONS DAILY",
                                            "MORRISON DAILY",
                                            "WM MORRISONS SUPERMARKET LIMITED",
                                            "LONDIS",
                                            "SCOTFRESH",
                                            "SAINSBURY",
                                            "ICELAND",
                                            "ICELAND FROZEN FOODS",
                                            "FOODWAREHOUSE",
                                            "TESCO",
                                            "TESCO STORES LTD",
                                            "TESCO EXPRESS",
                                            "TESCO (IN-STORE BAKERY)",
                                            "ASDA",
                                            "ASDA EXPRESS",
                                            "MARKS AND SPENCER",
                                            "M & S",
                                            "M&S",
                                            "M & S SIMPLY FOODS",
                                            "M & S SIMPLY FOOD",
                                            "MARKS AND SPENCER SIMPLY FOOD",
                                            "LIDL",
                                            "CO-OP",
                                            "SCOTTISH CO-OP",
                                            "CO-OP FOOD",
                                            "SCOTMID CO-OPERATIVE",
                                            "CO-OPERATIVE",
                                            "COOPERATIVE",
                                            "COOPERATIVE FOOD",
                                            "THE CO-OPERATIVE",
                                            "THE COOPERATIVE FOOD",
                                            "THE COOPERATIVE FOOD GROUP",
                                            "CO-OPERATIVE FOOD STORE",
                                            "ALDI",
                                            "ALDI STORES LTD",
                                            "ALDI STORES LIMITED",
                                            "AULDS",
                                            "SPAR",
                                            "ESSO",
                                            "FOOD WAREHOUSE",
                                            "FARMFOODS LTD",
                                            "FARMFOOD",
                                            "FARMFOODS (C)",
                                            "FARMFOODS FREEZER CENTRES",
                                            "FARMFOODS") ~ "Retail",
                                        TRUE ~ new_classification ))



## STEP 3 re categorize all chains to 'Takeaway/sandwich shop'####
fsa_analysis_clean <- fsa_analysis_clean %>%
  mutate(business_type = case_when(toupper(business_name) %in% 
                                     c("GREGGS",
                                       "GREGGS PLC",
                                       "GREGGS OF SCOTLAND",
                                       "GREGGS OF EDINBURGH",
                                       "BAYNES", 
                                       "BAYNES BAKERS",
                                       "BAYNES FAMILY BAKERS",
                                       "BAYNES THE FAMILY BAKERS",
                                       "BAYNES FAMILY BAKERY",
                                       "BAYNES THE BAKERS") ~ "Takeaway/sandwich shop", 
                                   TRUE ~ business_type ))

fsa_analysis_clean <- fsa_analysis_clean %>%
  mutate(new_classification = case_when(toupper(business_name) %in% 
                                          c("GREGGS",
                                            "GREGGS PLC",
                                            "GREGGS OF SCOTLAND",
                                            "GREGGS OF EDINBURGH",
                                            "BAYNES", 
                                            "BAYNES BAKERS",
                                            "BAYNES FAMILY BAKERS",
                                            "BAYNES THE FAMILY BAKERS",
                                            "BAYNES FAMILY BAKERY",
                                            "BAYNES THE BAKERS") ~ "OOH", 
                                        TRUE ~ new_classification ))


## STEP 4 re categorize all FOODBANKS to 'Other catering premises', chains to 'restaurants/cafe/canteen'####
fsa_analysis_clean <- fsa_analysis_clean %>%
  mutate(business_type = case_when(toupper(business_name) == "FOODBANKS" ~ "Other catering premises",
                                   toupper(business_name) %in%  
                                     c("SUBWAY",
                                       "STARBUCKS",
                                       "COSTA COFFEE",
                                       "COSTA",
                                       "DOMINO'S PIZZA",
                                       "KFC") ~ "Restaurant/Cafe/Canteen",
                                   TRUE ~ business_type)) 

fsa_analysis_clean <- fsa_analysis_clean %>%
  mutate(new_classification = case_when(toupper(business_name) == "FOODBANKS" ~ "Other catering premises",
                                        toupper(business_name) %in%  
                                          c("SUBWAY",
                                            "STARBUCKS",
                                            "COSTA COFFEE",
                                            "COSTA",
                                            "DOMINO'S PIZZA",
                                            "KFC") ~ "OOH",
                                        TRUE ~ new_classification)) 


## STEP 5 re categorize strings in business name  ####
fsa_analysis_clean <- fsa_analysis_clean %>%
  mutate(business_type = if_else(str_detect(business_name, regex("hotel |
                                                                 cafe |
                                                                 coffee |
                                                                 Tearoom |
                                                                 Tea room", ignore_case = TRUE)),
                                 "Restaurant/Cafe/Canteen", business_type))

fsa_analysis_clean <- fsa_analysis_clean %>%
  mutate(new_classification = if_else(str_detect(business_name, regex("hotel | 
                                                                      cafe |
                                                                      coffee |
                                                                      Tearoom |
                                                                      Tea room", ignore_case = TRUE)),
                                      "OOH", new_classification))


fsa_analysis_clean$business_type[grepl("Takeaway", fsa_analysis_clean$business_name)] <- "Takeaway/sandwich shop"

fsa_analysis_clean$new_classification[grepl("Takeaway", fsa_analysis_clean$business_name)] <- "OOH"

table(fsa_analysis_clean$business_type, fsa_analysis_clean$new_classification)


# DATA ANALYSIS ####

##Tables

## Table S1: Classification of food outlets in Scotland, 2024 (n=31,135) ####
tables1 <- fsa_analysis_clean %>% 
  select(local_authority_name, business_type, new_classification) %>% 
  tbl_summary(by = local_authority_name,
              missing = "no",
              digits = list(all_categorical() ~ c(0,0)),
              statistic = list(all_categorical() ~ "{n} ({p}%)"),
              label = c(business_type ~ "Business type",
                        new_classification ~ "Food outlet classification")) %>% 
  add_overall() %>%
  bold_labels() %>%
  as_flex_table()

print(tables1)


## Table 3: Density of OOH, retail and other food outlets by local authority ####
## Supplemental Table S3: Proportion of food outlets classified as Out of home (n=18,707) and Retail (n=8,847), by local authority in Scotland, 2024  #### 

# Create a local authority level dataset
#get counts of OOH and retail in each local authority
df.fsa <- fsa_analysis_clean %>% 
  group_by(local_authority_name, new_classification) %>%
  summarise(count = n())

#reshape to wide format
df.fsa <- df.fsa %>%
  pivot_wider(names_from = new_classification, values_from = count)

#combining columns of both dataframes
df.fsa1<- cbind(df.fsa, df.la)

#removing repeat columns from the combined dataset as a result of combining
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
         density_sqkm_other = count_other / land_area_sq_km,
         density_sqkm_total = count_total / land_area_sq_km
  )   %>% 
  #reorder variables
  relocate(population, land_area_sq_km, count_ooh, count_retail, count_other, count_total, percent_ooh, percent_retail, percent_other,density_sqkm_ooh, density_sqkm_retail, density_sqkm_other, density_sqkm_total, .after = local_authority_name)

table3a <- df.fsanew %>% 
  select(local_authority_name, percent_ooh, percent_retail, density_sqkm_ooh,density_sqkm_retail) %>% 
  tbl_summary(by = local_authority_name,
              missing = "no",
              digits = list(percent_ooh ~ 0,
                            percent_retail ~ 0,
                            density_sqkm_ooh ~ 2,
                            density_sqkm_retail ~ 2),
              statistic = list(percent_ooh ~ "{mean}%",
                               percent_retail ~ "{mean}%",
                               density_sqkm_ooh ~ "{mean}",
                               density_sqkm_retail ~ "{mean}"),
              label = c(percent_ooh ~ "Percentage of outlets that are out of home",
                        percent_retail ~ "Percentage of outlets that are retailers",
                        density_sqkm_ooh ~ "Out of home outlets per sq km",
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

df.tables1 <- tables1$body$dataset %>%
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

print(df.table3a)

#save to excel 
write_xlsx(
  list(
    "Sheet 1"= df.table3a,
    "Outlet chars" = df.tables1),
  "Table_Outlets_ByLocalAuthority.xlsx")


## Table S5: Food outlet classification, overall and by Scottish Index of Multiple Deprivation (SIMD) quintile in Scotland, 2024 ####
tables5 <- fsa_analysis_clean %>% 
  select(simd2020_quintile, business_type, new_classification) %>% 
  tbl_summary(by = simd2020_quintile,
              missing = "no",
              digits = list(all_categorical() ~ c(0,0)),
              statistic = list(all_categorical() ~ "{n} ({p}%)"),
              label = c(business_type ~ "Business type",
                        new_classification ~ "Food outlet classification")) %>% 
  add_overall() %>%
  bold_labels() %>%
  add_p() %>% 
  as_flex_table() 

save_as_docx(
  tables5,
  path = paste0("Table_Outlets_BySIMD_", format(Sys.time(), "%d%m%Y"), ".docx")
)

write_xlsx(df.fsanew,"OutletdensitybyLA.xlsx")


##Table S6: Food outlet classification by local authority and Scottish Index of Multiple Deprivation (SIMD) in Scotland, 2024 ####

file_path_la<-"Local authorities.xlsx"


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
Aberdeen <- read_excel(file_path_la, sheet = "Aberdeen city")
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


# Figures ####
# File for figures
file_path<-"fsa_20241410.xlsx"

##Figure S2. Distribution of food outlets by local authority (a)Counts (b)Proportion####

## (a)	Frequency of food outlets ## 
graph_data <- read_excel(file_path, sheet = "Graphdata")

# Reshape the data from wide to long format
df_long <- graph_data  %>%
  pivot_longer(cols = -`Local Authority`, names_to = "Category", values_to = "Count")

custom_colors <- c("Restaurant/Cafe/Canteen" = "skyblue", "Takeaway/sandwich shop"="blue3", "Retailers - other"="darkgrey", "Mobile caterer" = "green4", "Pub/bar/nightclub"= "cyan", "Retailers - supermarkets/hypermarkets"="grey","Other catering premises" = "green2" )

# Create the stacked bar plot
s <- ggplot(df_long, aes(x = reorder(`Local Authority`, -Count), y = Count, fill = Category)) +
  geom_bar(stat = "identity", position = "stack") +  scale_fill_manual(values = custom_colors) +
  scale_y_continuous(limits = c(0, 5000)) +
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


## Figure 2. Association of SIMD with classification of outlets  ####
## (a) Overall Scotland- all outlets ##

graph1 <- read_excel(file_path, sheet = "Graph1")

# Convert data to long format for ggplot2

data_long <- graph1 |>
  pivot_longer(
    cols = -SIMDQuintile,
    names_to = "Category",
    values_to = "Percentage"
  )

# Ensure Quintile is treated as a factor (categorical)
data_long$SIMDQuintile <- as.factor(data_long$SIMDQuintile)

data_long$Category <- factor(
  data_long$Category,
  levels = c(
    "Restaurant/Cafe/Canteen",
    "Takeaway/sandwich shop",
    "Pub/bar/nightclub",
    "Retailers - other",
    "Retailers - supermarkets/hypermarkets",
    "Other catering premises",
    "Mobile caterer"
  )
)

custom_colors <- c("Restaurant/Cafe/Canteen" = "blue4", "Takeaway/sandwich shop"="blue3", "Retailers - other"="green4", "Mobile caterer" = "lightblue", "Pub/bar/nightclub"= "blue", "Retailers - supermarkets/hypermarkets"="green3","Other catering premises" = "skyblue")

# Create the stacked bar chart

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
data_long2 <- graph2 |>
  pivot_longer(
    cols = -SIMDQuintile,
    names_to = "Category",
    values_to = "Percentage"
  )

# Reorder the factor levels for Category
data_long2$Category <- factor(
  data_long2$Category,
  levels = c("Out of Home", "Retail", "Others")
)

# Ensure Quintile is treated as a factor (categorical)
data_long2$SIMDQuintile <- as.factor(data_long2$SIMDQuintile)

custom_colors2 <- c("Retail" = "green4", "Out of Home"="blue4", "Others"="lightblue" )

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

## (c) City of Edinburgh1 example of a local authority same as overall trend ##

#Read the file
graph3 <- read_excel(file_path, sheet = "Edinburgh")

# Convert data to long format for ggplot2
data_long3 <- graph3 |>
  pivot_longer(
    cols = -SIMDQuintile,
    names_to = "Category",
    values_to = "Percentage"
  )

# Reorder the factor levels for Category
data_long3$Category <- factor(
  data_long3$Category,
  levels = c("Out of Home", "Retail", "Others")
)

# Ensure Quintile is treated as a factor (categorical)
data_long3$SIMDQuintile <- as.factor(data_long3$SIMDQuintile)

custom_colors3 <- c("Retail" = "green4", "Out of Home"="blue4", "Others"="lightblue" )

# Create the stacked bar chart
c<-ggplot(data_long3, aes(x = SIMDQuintile, y = Percentage, fill = Category)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) + scale_fill_manual(values = custom_colors3) +
  theme_minimal() +
  labs(title = "Categories by Quintile", x = "SIMDQuintile", y = "Percentage") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(legend.title = element_blank(), legend.position = "bottom", legend.key.size = unit(0.5, "cm"))

# Save a larger version of the plot
ggsave("Percentage_bar_chart.png", c, width = 12, height = 49, dpi = 800)

# Display the plot
print(c)

## (d) Aberdeenshire example of a local authority - Opposite of Edinburgh ##

#Read the file
graph4 <- read_excel(file_path, sheet = "Aberdeenshire")

# Convert data to long format for ggplot2
data_long4 <- graph4 |>
  pivot_longer(
    cols = -SIMDQuintile,
    names_to = "Category",
    values_to = "Percentage"
  )

# Reorder the factor levels for Category
data_long4$Category <- factor(
  data_long4$Category,
  levels = c("Out of Home", "Retail", "Others")
)

# Ensure Quintile is treated as a factor (categorical)
data_long4$SIMDQuintile <- as.factor(data_long4$SIMDQuintile)

custom_colors4 <- c("Retail" = "green4", "Out of Home"="blue4", "Others"="lightblue" )

# Create the stacked bar chart
d<-ggplot(data_long4, aes(x = SIMDQuintile, y = Percentage, fill = Category)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) + scale_fill_manual(values = custom_colors4) +
  theme_minimal() +
  labs(title = "Categories by Quintile", x = "SIMDQuintile", y = "Percentage") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(legend.title = element_blank(), legend.position = "bottom", legend.key.size = unit(0.5, "cm"))
# Save a larger version of the plot
ggsave("Percentage_bar_chart.png", d, width = 12, height = 49, dpi = 800)
# Display the plot
print(d)



# Additional analysis ####

## Association between SIMD and total outlet density per km2 ####

#import file with SIMD postcodes

df.singlerecord <- read.csv("SingleRecord.csv")

#select variables on interest 
df.singlerecord <- df.singlerecord %>%
  select(Postcode, DataZone2022Code, ScottishIndexOfMultipleDeprivation2020Rank)

# import file with data zones with areakm2
df.datazones <- read.csv("datazone_areas.csv")

#select variables of interest 
df.datazones <- df.datazones %>%
  select(DZCode, area_km2)

#renaming variables
df.singlerecord <- df.singlerecord %>%
  rename(DZCode = DataZone2022Code,
         simd2020_rank = ScottishIndexOfMultipleDeprivation2020Rank,
         post_code = Postcode)

#merge file for SIMD quintiles and area
df.simd.density <- left_join(df.datazones, df.singlerecord, by = c("DZCode"))

rm(df.datazones,df.singlerecord)

#merge with FSA data set
df.simd.density <- left_join(df.simd.density, fsa_analysis_clean, by = c("post_code"))

#select variables of interest 
df.simd.density <- df.simd.density %>%
  select(DZCode, new_classification, local_authority_name, simd2020_quintile, area_km2)

#drop post codes without any outlets
df.simd.density <- df.simd.density %>%
  filter(!is.na(new_classification))

#calculate total outlet density 
df.outletdensity <- df.simd.density %>%
  group_by(DZCode) %>%
  summarise(SIMD = first(simd2020_quintile),
            Area = first(area_km2), 
            Local_Authority = first(local_authority_name),
            Outlets = n()) %>%
  mutate(Outlet_density = Outlets / Area)

# Ensure SIMD is a factor and Outlet_density is an integer
df.outletdensity$SIMD <- as.factor(df.outletdensity$SIMD)
df.outletdensity$Outlet_density <- as.integer(df.outletdensity$Outlet_density)

##Calculating Outlet density by SIMD

# Step 1: Unique area per DZCode (so area not repeated by outlet)
area_df <- df.simd.density %>%
  distinct(DZCode, simd2020_quintile, area_km2)

# Step 2: Total area per SIMD
area_by_simd <- area_df %>%
  group_by(simd2020_quintile) %>%
  summarise(Total_Area = sum(area_km2, na.rm = TRUE), .groups = "drop")

# Step 3: Count outlets (rows) per SIMD
outlets_by_simd <- df.simd.density %>%
  group_by(simd2020_quintile) %>%
  summarise(Outlets = n(), .groups = "drop")

# Step 4: Join and calculate density
tables4a.v1 <- outlets_by_simd %>%
  left_join(area_by_simd, by = "simd2020_quintile") %>%
  mutate(Outlet_density = Outlets / Total_Area)

tables4a.v1


# Analysis using Poisson regression
poisson_model <- glm(
  Outlet_density ~ SIMD,
  data = df.outletdensity,
  family = poisson(link = "log")
)

summary(poisson_model)


#End of code####
