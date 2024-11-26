#contact: cheneyr@si.edu

#Manually working through global seagrass area dataset from MacKenzie et al 2020
#https://iopscience.iop.org/article/10.1088/1748-9326/ab7d06/meta#erlab7d06s5

library(tidyverse)
library(readr)
library(readxl)
library(janitor)
library(countrycode)

#read in ccn country spellings 
ccn_countries <- as_tibble(st_read("data/CCN_maps/Countries_w_EEZ_fishnet_Hydrobasins06_dissolved_241023.shp")) %>% 
  select(country, territory) %>% 
  distinct_all() %>% 
  arrange(country, territory)

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
                             country == "West Bank" ~ "Palestinian Territory",
                             country == "Taiwan" ~ "China",
                             country == "Western Sahara" ~ "Disputed",
                             country == "Spratly Islands" ~ "Disputed",
                             TRUE ~ country),
         territory = case_when(country == "Brunei" ~ "Brunei",
                               country == "China" ~ "China",
                               grepl("Côte d'Ivoire", country) ~ "Côte d'Ivoire",
                               territory == "Congo (Democratic Republic)" ~ "Democratic Republic of the Congo",
                               territory == "Curaçao (Neth.)" ~ "Curacao",
                               grepl("Gambia", country) ~ "Gambia",
                               grepl("Falkland", territory) ~ "Falkland Islands",
                               grepl("Iran", country) ~ "Iran",
                               grepl("Christmas", territory) ~ "Australia",
                               grepl("Ashmore", territory) ~ "Australia",
                               grepl("Norfolk", territory) ~ "Australia",
                               grepl("Cocos", territory) ~ "Australia",
                               grepl("Jervis", territory) ~ "Australia",
                               country == "North Korea" ~ "North Korea",
                               country == "South Korea" ~ "South Korea",
                               grepl("Myanmar", country) ~ "Myanmar",
                               grepl("United States of America", territory) ~ "United States",
                               territory == "Sint Maarten (Neth.)" ~ "Sint Maarten",
                               grepl("Micronesia", territory) ~ "Micronesia",
                               # grepl("Nova", territory) ~ "Juan de Nova Island",
                               grepl("Sao Tome", country) ~ "Sao Tome and Principe",
                               grepl("Syria", country) ~ "Syria",
                               # territory == "Australia (incl Jervis Bay Territory)" ~ "Australia", #check this 
                               # grepl("and Saba", territory) ~ "Bonaire, Sint Eustatius and Saba", #also called BES Islands 
                               territory == "Cape Verde" ~ "Cabo Verde",
                               grepl("Tanz", territory) ~ "Tanzania",
                               territory == "Bassas da India"|grepl("Ile", territory)|
                               territory == "Juan de Nova Island"|territory == "Europa Islands"|
                               territory == "Reunion" ~ "France", 
                               territory == "Pitcairn Islands" ~ "United Kingdom",
                               territory == "Macau"|territory == "Hong Kong"|territory == "Paracel Islands" ~ "China",
                               territory == "Niue"|territory == "Cook Islands" ~ "New Zealand",
                               # grepl("Neth", territory) ~ "Netherlands",
                               grepl("Pales", country) ~ "Palestinian Territory",
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
                               #territory == "Ashmore and Cartier Islands"|territory == "Christmas Island"|
                              #   territory == "Cocos (Keeling) Islands"|territory == "Norfolk Island" ~ "Australia",
                               territory %in% c("Bassas da India", "Europa Island", "Glorioso Islands", 
                                                "Ile Saint-Paul", "Iles Kerguelen",
                                                "Juan de Nova Island", "Tromelin Island") ~ "French Southern Territories",
                               grepl("Nova Island", territory) ~ "French Southern Territories",
                               territory == "Spratly Islands" ~ "Overlapping claim South China Sea",
                               territory == "Saint-Martin & Saint Barthélemy"~"Saint Barthelemy;Saint Maarten",
                               grepl("and Saba", territory) ~ "Bonaire;Saint Eustatius;Saba", #also called BES Islands 
                               territory == "Saint Helena, Ascension and Tristan da Cunha" ~ "Saint Helena;Ascension;Tristan da Cunha",
                              TRUE ~ territory)) %>% 
  filter(territory != "Netherlands Antilles") %>% 
  arrange(country, territory)




#check for inconsistencies 
matchtest <- anti_join(seagrass_spellcheck, ccn_countries)
# View(matchtest)


#notes 
# Juan de Nova, Europa, Glorioso Islands, Ile Saint Paul, Iles Ker --> inlcuded in "French southern territories"
# Western Sahara spans 2 bioregions --> 2 and 3 

seagrass_vis <- seagrass_spellcheck %>% 
  mutate(MaxEnt_km2 = as.numeric(MaxEnt_km2),
         `Seagrass Area_km2\r\nModerate to High Confidence` = as.numeric(`Seagrass Area_km2\r\nModerate to High Confidence`),
         `Seagrass Area_km2\r\nLow Confidence` = as.numeric(`Seagrass Area_km2\r\nLow Confidence`))

ggplot(seagrass_vis, aes(x = MaxEnt_km2, y = `Seagrass Area_km2\r\nModerate to High Confidence`)) +
  geom_point() +
  geom_point(aes(y = `Seagrass Area_km2\r\nLow Confidence`), color = "red") +
  scale_x_log10() +
  scale_y_log10() 

#### ..Curate Country-Level Stocks ####

#write files to integrate data 
#format? csv?
global_seagrass_data <- seagrass_spellcheck %>% 
  select(country, territory, Bioregion, `Seagrass Area_km2\r\nModerate to High Confidence`,
         `Seagrass Area_km2\r\nLow Confidence`) %>% 
  mutate(High_Confidence = as.numeric(`Seagrass Area_km2\r\nModerate to High Confidence`),
         Low_Confidence = as.numeric(`Seagrass Area_km2\r\nLow Confidence`)) %>% 
  select(country, territory, Bioregion, High_Confidence, Low_Confidence) %>% 
  mutate(seagrass_area_km2 = ifelse(!is.na(High_Confidence), High_Confidence, Low_Confidence),
         confidence = ifelse(!is.na(High_Confidence), "High_Confidence", "Low_Confidence")) %>% 
  arrange(country, territory, Bioregion, confidence) %>% 
  filter(complete.cases(seagrass_area_km2)) %>% 
  group_by(country, territory) %>% 
  summarise(seagrass_area_m2 = sum(seagrass_area_km2)*1000000)

# View(global_seagrass_data)

write_csv(global_seagrass_data, "seagrass_mapping/output/Seagrass_contry_territory_summary.csv")

marsh_mangrove <- read_csv("country_mapping_corrections/Global_Mangrove_Marsh_Summaries_byTerritoryAndCountry.csv")

marsh_mangrove_seagrass <- marsh_mangrove %>% 
  ungroup() %>% 
  full_join(global_seagrass_data) %>% 
  mutate(total_area_m2 = rowSums(across(c(marsh_area_m2,mangrove_area_m2,seagrass_area_m2)), na.rm=T)) %>% 
  arrange(-total_area_m2)

marsh_mangrove_seagrass_tall <- marsh_mangrove_seagrass %>% 
  filter(country!="Disputed",
         !grepl(";", territory)) %>% 
  mutate(territory = factor(territory, levels = rev(unique(marsh_mangrove_seagrass$territory)))) %>% 
  gather(key = "ecosystem", value = "squareMeter", -c(country,territory)) %>% 
  mutate(ecosystem = str_remove_all(ecosystem, "_area_m2"),
         hectare = squareMeter*0.0001) %>% 
  select(-squareMeter) %>% 
  group_by(territory, ecosystem) %>% 
  summarise(hectare=first(hectare),
            country=first(country)) %>% 
  ungroup() %>% 
  arrange(territory)

write_csv(marsh_mangrove_seagrass_tall, "seagrass_mapping/output/Marsh_mangrove_seagrass_territory_summary_tall.csv")

ggplot(marsh_mangrove_seagrass_tall, aes(x = territory, y = hectare)) +
  geom_point(aes(color = ecosystem)) +
  geom_segment(aes(y = 0, yend = hectare, color = ecosystem)) +
  facet_grid(.~ecosystem, scale="free") +
  scale_y_log10(labels = scales::comma) +
  coord_flip() +
  theme(axis.text.y = element_text(size = 8),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab(NULL)

ggsave("Three ecosystem area plot.jpg", height = 18, width = 7)  












