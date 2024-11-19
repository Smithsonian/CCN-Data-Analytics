#contact: cheneyr@si.edu

#Manually working through global seagrass area dataset from MacKenzie et al 2020
#https://iopscience.iop.org/article/10.1088/1748-9326/ab7d06/meta#erlab7d06s5

library(tidyverse)
library(readr)
library(readxl)
library(janitor)
library(countrycode)

#read in ccn country spellings 
ccn_countries <- read_csv("CCN_Official_Countries_and_Territories_Spelling.csv")

#seagrass global area dataset from mackenzie et al 2020
seagrass <- read_xlsx("ERL_15_7_074041_suppdata.xlsx", sheet = 1) %>% 
  row_to_names(row_number = 11)


#create source vector for countrynames 
sourcevar <- seagrass$`Alpha-3`

#convert names to iso3
seagrass_convert <- seagrass %>% 
  rename(country = Sovereign,
         territory = Country) %>% 
  mutate(country_iso = countrycode(sourcevar = sourcevar, origin = "iso3c", destination = "country.name")) %>% 
  select(country_iso, country, territory, everything())


#seagrass countries 
seagrass_countries <- seagrass_convert %>% 
  select(country_iso, country, territory)





#check lists against eachother 

mismatch_both <- anti_join(seagrass_convert, ccn_countries) %>% distinct()


not_in_ccn <- as.list(mismatch_both$territory) 




# fix spelling and format to match ccn list, cross reference with iso3 country codes
# default to ESRI spelling 

seagrass_spellcheck <- seagrass %>% 
  rename(country = Sovereign, territory = Country) %>% 
  mutate(country = case_when(country == "Bahamas, The" ~ "Bahamas",
                             country == "Congo (Democratic Republic)" ~ "Democratic Republic of the Congo",
                             grepl("Côte d'Ivoire", country) ~ "Côte d'Ivoire",
                             grepl("Micronesia", country) ~ "Micronesia",
                             grepl("Myanmar", country) ~ "Myanmar",
                             territory == "Paracel Islands" ~ "China",
                             country == "Russia" ~ "Russian Federation",
                             territory == "Palau" ~ "Palau",
                             country == "Western Samoa" ~ "Samoa",
                             grepl("Kitts", country) ~ "Saint Kitts and Nevis",
                             grepl("St. Lucia", country) ~ "Saint Lucia",
                             grepl("Grenadine", country) ~ "Saint Vincent and the Grenadines",
                             country == "Cape Verde" ~ "Cabo Verde",
                             grepl("Tanz", country) ~ "Tanzania",
                             TRUE ~ country),
         territory = case_when(country == "Brunei" ~ "Brunei",
                               country == "China" ~ "China",
                               grepl("Côte d'Ivoire", country) ~ "Côte d'Ivoire",
                               territory == "Congo (Democratic Republic)" ~ "Democratic Republic of the Congo",
                               territory == "Curaçao (Neth.)" ~ "Curacao",
                               grepl("Gambia", country) ~ "Gambia",
                               grepl("Falkland", territory) ~ "Falkland Islands",
                               grepl("Iran", country) ~ "Iran",
                               grepl("Christmas", territory) ~ "Christmas Island",
                               grepl("Ashmore", territory) ~ "Ashmore and Cartier Islands",
                               grepl("Norfolk", territory) ~ "Norfolk Island",
                               grepl("Cocos", territory) ~ "Cocos (Keeling) Islands",
                               country == "North Korea" ~ "North Korea",
                               country == "South Korea" ~ "South Korea",
                               grepl("Myanmar", country) ~ "Myanmar",
                               grepl("United States of America", territory) ~ "United States",
                               territory == "Sint Maarten (Neth.)" ~ "Sint Maarten",
                               grepl("Micronesia", territory) ~ "Micronesia",
                               grepl("Nova", territory) ~ "Juan de Nova Island",
                               grepl("Sao Tome", country) ~ "Sao Tome and Principe",
                               grepl("Syria", country) ~ "Syria",
                               territory == "Australia (incl Jervis Bay Territory)" ~ "Australia", #check this 
                               grepl("and Saba", territory) ~ "Bonaire, Sint Eustatius and Saba", #also called BES Islands 
                               territory == "Cape Verde" ~ "Cabo Verde",
                               grepl("Tanz", territory) ~ "Tanzania",
                             TRUE ~ territory))



#check for inconsistencies 
matchtest <- anti_join(seagrass_spellcheck, ccn_countries)


#add countries and territories to CCN list 

#create new list 
countries_add <- tibble(territory = c("Ashmore and Cartier Islands", "Christmas Island", "Cocos (Keeling) Islands",
                                      "Norfolk Island", "Bassas da India", "Cook Islands","Europa Island", 
                                      "Glorioso Islands", "Hong Kong", "Ile Saint-Paul",
                                      "Iles Kerguelen", "Juan de Nova Island", "Macau", "Netherlands Antilles",
                                      "Bonaire, Sint Eustatius and Saba", "Niue", "Paracel Islands", "Europa Island",
                                      "Pitcairn Islands", "Reunion")) %>% 
  mutate(country = case_when(territory == "Bassas da India"|grepl("Ile", territory)|
                               territory == "Juan de Nova Island"|territory == "Europa Islands"|
                               territory == "Reunion" ~ "France", 
                             territory == "Pitcairn Islands" ~ "United Kingdom",
                             territory == "Macau"|territory == "Hong Kong"|territory == "Paracel Islands" ~ "China",
                             territory == "Niue"|territory == "Cook Islands" ~ "New Zealand",
                             grepl("Neth", territory) ~ "Netherlands",
                             TRUE ~ "Australia"))



ccn_countries_add <- ccn_countries %>% rbind(countries_add)
  
  
#test again
matchtest2 <- anti_join(seagrass_spellcheck, ccn_countries)


#update more territories and fix formatting 
seagrass_area_update <- matchtest2 %>%  
  mutate(country = case_when(country == "West Bank" ~ "Palestinian Territory",
                             country == "Taiwan" ~ "China",
                             country == "Western Sahara" ~ "Disputed",
                             country == "Spratly Islands" ~ "Disputed", 
                             TRUE ~ country),
         territory = case_when(grepl("Pales", country) ~ "Palestinian Territory",
                               grepl("Pitcairn", territory) ~ "Pitcairn",
                               grepl("Reunion", territory) ~ "Réunion",
                               territory == "Japan (Ryukyu)" ~ "Japan",
                               territory == "Virgin Islands (British)" ~ "British Virgin Islands",
                               territory == "Virgin Islands (USA)" ~ "US Virgin Islands",
                               grepl("Viet", territory) ~ "Vietnam",
                               territory == "Taiwan (China)" ~ "China",
                               territory == "Tokelau"|territory == "Cook Islands"|territory == "Niue" ~ "New Zealand",
                               grepl("United Kingdom", territory) ~ "United Kingdom",
                               grepl("Venezuela", territory) ~ "Venezuela",
                               territory == "Ashmore and Cartier Islands"|territory == "Christmas Island"|
                                 territory == "Cocos (Keeling) Islands"|territory == "Norfolk Island" ~ "Australia",
                               TRUE ~ territory)) 
#remove NA country?

#test again 
matchtest3 <- anti_join(seagrass_area_update, ccn_countries)

#notes 
# Juan de Nova, Europa, Glorioso Islands, Ile Saint Paul, Iles Ker --> inlcuded in "French southern territories"
# Western Sahara spans 2 bioregions --> 2 and 3 



#### ..Curate Country-Level Stocks ####

#check formatting of marsh and mangrove datasets 
mangroveMarsh <- read_csv("country_mapping_corrections/Global_Mangrove_Marsh_Summaries.csv")


#write files to integrate data 
#format? csv?
write_csv(seagrass_global, "")







