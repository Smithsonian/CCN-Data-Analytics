## Program script to run inventorying functions for a given version of the synthesis

library(tidyverse)
library(sf)

# source inventorying functions
source("blue_carbon_data_inventory/scripts/inventory_functions.R")

# Import Core Data ####

# read in core data
cores <- read_csv("data/CCN_cores.csv", guess_max = 11000) %>% 
  # isolate CONUS cores
  rename(state = admin_division) %>%
  filter(country == "United States") %>%
  filter(state != "Puerto Rico" & state != "Hawaii" & state != "Alaska") %>%
  # standardize habitat classes to align with mapping products used in habitat metric
  mutate(habitat = recode(
    habitat, 
    "mudflat" = "unvegetated",
    # grouping seagrass, kelp, and algal mats together (because the mapping products don't distinguish these)
    "seagrass" = "EAB",
    "algal mat" = "EAB"))

# determine the studies included in each version 
v1_studies <- read_csv("https://ndownloader.figshare.com/files/42289539", guess_max = 7000) %>% distinct(study_id) %>% pull(study_id)
v2_studies <- read_csv("https://ndownloader.figshare.com/files/43251309", guess_max = 11000) %>% distinct(study_id) %>% pull(study_id)
  
## Version 1 Scores ####

v1_cores <- cores %>% filter(study_id %in% v1_studies)

v1_quantity <- quantityMetric(v1_cores)
v1_quality <- qualityMetric(v1_cores)
v1_spatial <- spatialMetric(v1_cores)
v1_habitat <- habitatMetric(v1_cores)

# join all rankings
v1_metrics <- v1_quantity %>% full_join(v1_quality) %>% full_join(v1_spatial) %>% full_join(v1_habitat)

# inventory the data and compute metrics
scores_v1 <- compositeMetricScore(v1_metrics)

# habitat breakdown
# v1_habitat_detailed <- habitatProportions(v1_cores)

## Version 2 Scores ####

v2_cores <- cores %>% filter(study_id %in% v2_studies)

v2_quantity <- quantityMetric(v2_cores)
v2_quality <- qualityMetric(v2_cores)
v2_spatial <- spatialMetric(v2_cores)
v2_habitat <- habitatMetric(v2_cores)

# join all rankings
v2_metrics <- v2_quantity %>% full_join(v2_quality) %>% full_join(v2_spatial) %>% full_join(v2_habitat)

# inventory the data and compute metrics
scores_v2 <- compositeMetricScore(v2_metrics)

# habitat breakdown
# v2_habitat_detailed <- habitatProportions(v2_cores)

## Blue Carbon Data Inventory Report ####

# Import states (for the BCDI report)
states <- st_read("blue_carbon_data_inventory/data/shapefiles/us_states/states_coastline_boundaries/cb_2017_us_state_500k.shp",
                  stringsAsFactors = F)

url_date <- format(Sys.time(), "%Y%m%d %H%M")
formated_date <- format(Sys.time(), "%Y/%m/%d-%H:%M")

# generate data contributor report
rmarkdown::render(input = "blue_carbon_data_inventory/scripts/bcdi_report.Rmd",
                  # output_format = "html_document",
                  output_file = paste0("bcdi_report_", url_date),
                  output_dir = "blue_carbon_data_inventory/inventory_reports/")
