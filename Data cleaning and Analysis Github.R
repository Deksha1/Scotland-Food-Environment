# Paper title: Characterizing the food environment in Scotland and its asso-ciation with deprivation: a national study 
# Last updated: 27 January 2025
# Author: Deksha Kapoor

#load required packages
library(dplyr)
library(tidyr)
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

# Import and tidy data ####

#food outlet data from FSA
df.foodenv = read_excel("C:/Users/deksh/OneDrive - University of Edinburgh/AIM 3/FSS data/fsa_20240926.xlsx")

#local authority stats (land area and population)
df.la <- read_excel("C:/Users/deksh/OneDrive - University of Edinburgh/AIM 3/FSS data/local_authority_stats.xlsx")

#set all column names to lower case and replace spaces and special characters with underscores
df.foodenv <- clean_names(df.foodenv)
df.la <- clean_names(df.la)

#drop variables not needed
df.foodenv <- df.foodenv %>%
  select(-x24, -x25, -x26)

#fill in NA new_classification as 'Other'
df.foodenv <- df.foodenv %>%
  mutate(
    new_classification = case_when(is.na(new_classification) ~ "Other",
                                   TRUE ~ new_classification)
        )

## ggplot example could not put in ascending order
# creating stacked bar chart using df.foodenv
# x axis = LA, y axis = count, colour = businesstype
ggplot(data = df.foodenv,
       aes(x = sort(local_authority_name), fill = business_type)) +
  geom_bar()

##DATA CLEANING 
## STEP 1 deleting non food related establishments from the data set
fsa_analysis_clean <- df.foodenv %>% 
  filter(!grepl(pattern = 'School', #! does the opposite
                x = business_name,
                ignore.case = TRUE)) %>% #ignore case would delete School and school
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
# firstly, let's check which business types already exist
fsa_analysis_clean$business_type %>% # pick all the values from the BusinessType column
  unique() # then return only the UNIQUE values

# recatagorise all retail chains from 'Retailers - other' to 'Retailers - supermarkets/hypermarkets
##using case when,toupper command and TRUE 
##using case_when function to modify BusinessType based on BusinessName
##toupper converts all the business names as caps so that the command is case insensitive
## TRUE ~ business_type this line leaves BusinessType of all other rows as-is
spar <- fsa_analysis_clean %>% 
  mutate(business_type = case_when(toupper(business_name) == "SPAR" ~ "Retailers - supermarkets/hypermarkets",
                                   TRUE ~ business_type) 
  ) %>%  
  filter(toupper(business_name) == "SPAR") %>% # filtering to see if mutate case_when has worked
  select(business_name,business_type) # select only the columns of interest to look at

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
  #STEP 5 some more cleaning on recatagorising 
  fsa_analysis_clean %>% 
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

#STEP 6 re categorization of strings in business name  
fsa_analysis_clean$business_type %>% # pick all the values from the BusinessType column
  unique() 

fsa_analysis_clean$business_type[grepl("hotel", fsa_analysis_clean$business_name)] <- "Restaurant/Cafe/Canteen"
fsa_analysis_clean$business_type[grepl("Hotel", fsa_analysis_clean$business_name)] <- "Restaurant/Cafe/Canteen" 
fsa_analysis_clean$business_type[grepl("Cafe", fsa_analysis_clean$business_name)] <- "Restaurant/Cafe/Canteen" 
fsa_analysis_clean$business_type[grepl("cafe", fsa_analysis_clean$business_name)] <- "Restaurant/Cafe/Canteen"
fsa_analysis_clean$business_type[grepl("coffee", fsa_analysis_clean$business_name)] <- "Restaurant/Cafe/Canteen"
fsa_analysis_clean$business_type[grepl("Coffee", fsa_analysis_clean$business_name)] <- "Restaurant/Cafe/Canteen"
fsa_analysis_clean$business_type[grepl("Tearoom", fsa_analysis_clean$business_name)] <- "Restaurant/Cafe/Canteen"
fsa_analysis_clean$business_type[grepl("Tea room", fsa_analysis_clean$business_name)] <- "Restaurant/Cafe/Canteen"
fsa_analysis_clean$business_type[grepl("Takeaway", fsa_analysis_clean$business_name)] <- "Takeaway/sandwich shop"

##DATA MANAGEMENT 

# Create a local authority level dataset ####
#get counts of OOH and retail in each local authority
df.foodenv.la <- df.foodenv %>% 
  group_by(local_authority_name, new_classification) %>%
  summarise(count = n())

#reshape to wide format
df.foodenv.la <- df.foodenv.la %>%
  pivot_wider(names_from = new_classification, values_from = count)

#rename for merge
df.la <- df.la  %>% 
  rename(local_authority_name = local_authority) # new name first then old name that we are changing

#merge
df.foodenv.la <- left_join(df.foodenv.la, df.la, by="local_authority_name")

#rename variables
dfood.la <- dfood.la  %>% 
  rename(count_ooh = OOH,
         count_retail = Retail,
         count_other = Other)

#import merged file
dfood.la <- read_excel("C:/Users/deksh/OneDrive - University of Edinburgh/AIM 3/FSS data/foodenv.xlsx")
print(dfood.la)

#calculate percent of outlets and density of outlets in local authorities

dfood.la <- dfood.la  %>% 
  mutate(count_total = count_ooh+count_retail+count_other,
         percent_ooh = (count_ooh / count_total)*100,
         percent_retail = (count_retail / count_total)*100,
         percent_other = (count_other / count_total)*100,
         density_sqkm_ooh = count_ooh / land_area_sq_km,
         density_sqkm_retail = count_retail / land_area_sq_km,
         density_pop_ooh = count_ooh / (population/1000),
         density_pop_retail = count_retail / (population/1000)
  )   %>% 
  # reorder variables
  relocate(population, land_area_sq_km, count_ooh, count_retail, count_other, count_total, percent_ooh, percent_retail, percent_other, .after = local_authority_name)


# Results- Outlets by local authority ####
install.packages("flextable")
library(flextable)
## Tables ####
#create flextables
install.packages("dplyr")
library(dplyr)
library(gtsummary)
table1 <- df.foodenv %>% 
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


table2 <- dfood.la %>% 
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
df.table1 <- table1$body$dataset %>%
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
install.packages("writexl")
library("writexl")
write_xlsx(
  list(
    "Business type" = df.table1,
    "Outlet chars" = df.table2),
  path = "C:/Users/deksh/OneDrive - University of Edinburgh/AIM 3/FSS data/Table_Outlets_ByLocalAuthority.xlsx")

## Figures ####
install.packages("ggplot2")
library(ggplot2)
library(ggplot2)
library(dplyr)
figure1 <- ggplot(dfood.la, aes(x=reorder(local_authority_name,desc(density_sqkm_ooh)), y=density_sqkm_ooh)) +  
  geom_bar(position=position_dodge(), stat="identity", fill="#440154FF") +
  labs(x="",
       y="Density of out of home (outlets per sq km)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))

ggsave(figure1, filename = "C:/Users/deksh/OneDrive - University of Edinburgh/AIM 3/FSS data/OOH_density_local_authority.jpeg",width = 10, height = 6)

figure2 <- ggplot(dfood.la, aes(x=reorder(local_authority_name,desc(density_sqkm_retail)), y=density_sqkm_retail)) +  
  geom_bar(position=position_dodge(), stat="identity", fill="#440154FF") +
  labs(x="",
       y="Density of food retail (outlets per sq km)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))

ggsave(figure2, filename = "C:/Users/deksh/OneDrive - University of Edinburgh/AIM 3/FSS data/Retail_density_local_authority.jpeg",width = 10, height = 6)



# Results - Outlets by SIMD #### 

## Tables ####
#create flextables
library(dplyr)
library(gtsummary)
table3 <- df.foodenv %>% 
  dplyr::select(simd2020_quintile, business_type, new_classification) %>% 
  tbl_summary(by = simd2020_quintile,
              missing = "no",
              digits = list(all_categorical() ~ c(0,0)),
              statistic = list(all_categorical() ~ "{p}% ({n})"),
              label = c(business_type ~ "Business type",
                        new_classification ~ "Food outlet classification")) %>% 
  add_overall() %>%
  bold_labels() %>%
  add_p() %>% # this adds a chi-square test
  as_flex_table() 

install.packages("officer")

library(officer)
library(flextable)

save_as_docx(table3,
             path = paste0("C:/Users/deksh/OneDrive - University of Edinburgh/AIM 3/FSS data/","Table_Outlets_BySIMD_",format(Sys.time(),"%d%m%Y"),".docx"))  

#create flextable for SMID by local authority
library(dplyr)
library(gtsummary)
table3 <- df.foodenv %>% 
  dplyr::select(simd2020_quintile, business_type, local_authority_name) %>% 
  tbl_summary(by = simd2020_quintile,
              missing = "no",
              digits = list(all_categorical() ~ c(0,0)),
              statistic = list(all_categorical() ~ "{p}% ({n})"),
              label = c(business_type ~ "Business type",
                        local_authority_name ~ "By Local Authority")) %>% 
  add_overall() %>%
  bold_labels() %>%
  add_p() %>% # this adds a chi-square test
  as_flex_table() 

install.packages("officer")

library(officer)
library(flextable)

save_as_docx(table3,
             path = paste0("C:/Users/deksh/OneDrive - University of Edinburgh/AIM 3/FSS data/","Table_Outlets_BySIMD_localauthority",format(Sys.time(),"%d%m%Y"),".docx"))  


## Figures ####


#calculate percentages for plotting
library(dplyr)
df.fiure3 <- df.foodenv %>%
  group_by(simd2020_quintile, new_classification) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)
#make figure  
library(ggplot2)
figure3 <- ggplot(df.fiure3, aes(fill=new_classification, y=percentage, x=simd2020_quintile))+
  geom_bar(position = "stack", stat = "identity")+
  labs(
    x = "SIMD Quintile",
    y = "Percentage of outlets",
    fill = "Food outlet classification")+
  theme_minimal() +
  theme(legend.position = "right", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5)) +
  scale_fill_brewer(palette = "Paired")

ggsave(figure3, filename = "C:/Users/deksh/OneDrive - University of Edinburgh/AIM 3/FSS data/Outlet_type_SIMD.jpeg", width = 9, height = 5)
figure3
  
## Chi square test for each local authority by SIMD
# Import the data ####
#food environment
install.packages("readxl")
library("readxl")
Northa <- read_excel("C:/Users/deksh/Desktop/3rd Nov FSS analysis/Northa.xlsx")

# Create contingency table # This was repeated for all local authorities 
contingency_table_Northa <- table(Northa$simd2020_quintile, Northa$new_classification)
print(contingency_table_Northa)
# Perform the Chi-squared test
chi_square_test <- chisq.test(contingency_table_Northa)
# Display the results
print(chi_square_test)
# This was repeated for all local authorities by editing the same command
contingency_table_dundee <- table(dundee$simd2020_quintile, dundee$new_classification)
print(contingency_table_dundee)
# Perform the Chi-squared test
chi_square_test <- chisq.test(contingency_table_dundee)
# Display the results
print(chi_square_test)

## Testing differences between retail and OOH density table 3 in paper draft
# Read the data from the 'Density' sheet
install.packages("readxl")
library("readxl")
data <- read_excel("C:/Users/deksh/Desktop/3rd Nov FSS analysis/t test_3nov24.xlsx")
# Perform an unpaired t-test
t_test_result <- t.test(data$`OOH density`, data$`Retail Density`, var.equal = TRUE)
# Display the results
print(t_test_result)
