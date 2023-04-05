## CCRCN Data Analytics

source("resources/refresh_data.R")

## Script queries library for mangrove cores for global blue carbon inventorying

library(tidyverse)

mangrove_cores <- cores %>% 
  mutate(habitat = ifelse(grepl("mangrove", core_notes), "mangrove", habitat)) %>% 
  filter(habitat == "mangrove")

write_csv(mangrove_cores, "query/data/ccn_mangrove_cores.csv")


# Costa Rica Cores

cr_cores <- cores %>% 
  filter(country == "Costa Rica")

library(leaflet)

leaflet(cr_cores) %>% 
  addTiles() %>% 
  addCircleMarkers(radius = 3)


