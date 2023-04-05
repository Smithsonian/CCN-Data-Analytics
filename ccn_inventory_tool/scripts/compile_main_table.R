# Coastal Carbon Inventorying

# script to compile main lookup data table  

library(tidyverse)
library(readxl)

## Curate ####

# standardize tables
# make sure to keep track of data sources

# Global Mangrove area
gmw_area <- read_xls("data/mangroveWorld_Area.xls")

# Aboveground biomass Data
agb_data <- read_csv("data/input_data/original/country_agb.csv") %>% 
  mutate(notes = ifelse(grepl("\\*", country), yes = "Maximum canopy height could not be accurately measured due to misclassification of mangrove extent.", 
                        no = NA),
         country = case_when(grepl("\\*", country) ~ gsub("\\*", "", country),
                             T ~ country))

# Belowground biomass?

# Soil Organic Carbon

