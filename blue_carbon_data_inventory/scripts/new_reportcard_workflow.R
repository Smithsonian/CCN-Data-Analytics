## Program script to run inventorying functions for a given version of the synthesis
## Even more automated

library(tidyverse)
library(sf)

# source inventorying functions
source("blue_carbon_data_inventory/scripts/bcdi_functions.R")

# read in lookup table for past syntheses
# synthesis_history <- read_csv("https://raw.githubusercontent.com/Smithsonian/CCN-Data-Library/develop/docs/synthesis_resources/synthesis_history.csv")
# 
# versions_to_run <- synthesis_history %>% 
#   filter(table == "cores") %>%
#   filter(version != "1.1.0") %>%  # this overestimated the core count (V1.1.1 was the correction)
#   mutate(date = as.Date(date, format = "%m/%d/%y")) %>% 
#   arrange(date) %>% tail(2) %>% 
#   add_row(date = Sys.Date(), version = current_version)

# Import Core Data ####

current_version <- "1.3.0"

guess_max <- nrow(read_csv("https://raw.githubusercontent.com/Smithsonian/CCN-Data-Library/develop/data/CCN_synthesis/CCN_cores.csv"))

# read in core data
current_cores <- read_csv("https://raw.githubusercontent.com/Smithsonian/CCN-Data-Library/develop/data/CCN_synthesis/CCN_cores.csv", 
                          guess_max = guess_max) %>% 
  # isolate cores from the contiguous united states
  rename(state = admin_division) %>%
  filter(country == "United States") %>%
  filter(state != "Puerto Rico" & state != "Hawaii" & state != "Alaska") %>%
  # standardize habitat classes to align with mapping products used in habitat metric
  mutate(habitat = recode(habitat, 
                          "mudflat" = "unvegetated",
                          # grouping seagrass, kelp, and algal mats together (because the mapping products don't distinguish these)
                          "seagrass" = "EAB",
                          "algal mat" = "EAB"))

# determine the studies included in each version 
# v1_studies <- read_csv("https://ndownloader.figshare.com/files/42289539", guess_max = 7000) %>% distinct(study_id) %>% pull(study_id)
# v2_studies <- read_csv("https://ndownloader.figshare.com/files/43694076", guess_max = 11000) %>% distinct(study_id) %>% pull(study_id)

synthesis_history <- read_csv("https://raw.githubusercontent.com/Smithsonian/CCN-Data-Library/develop/docs/synthesis_resources/core_synthesis_history.csv")

versions_to_run <- synthesis_history %>% 
  # drop some unneccessary versions (some where the core count reduced??)
  filter(!(version %in% c("0.2.0", "1.1.0"))) %>%  # this overestimated the core count (V1.1.1 corrected this)
  filter(!(lubridate::year(as.Date(date)) %in% c(2021))) %>% 
  # add current core version info
  bind_rows(current_cores %>% 
              select(study_id, core_id) %>% 
              mutate(version = current_version, date = as.Date("2024-07-02")))

# For each past version, read and store the study IDs

## Looping Function

all_scores <- data.frame()
all_rankings <- data.frame()

# for(v in 1:nrow(versions_to_run)){
#   
#   # subset synthesis history to identify studies and cores for a particular version
#   # temp_version <- versions_to_run %>% filter(version == v) 
#     # make some fixes to study IDs that should align things better
#     # mutate(study_id = gsub("[.]", "", study_id),
#            # study_id = case_when(grepl(" ", study_id) ~ gsub(" ", "_", study_id), T ~ study_id))
#   
#   if(versions_to_run$version[v] == current_version){
#     
#     temp_table <- current_cores
#   } else {
#     
#     temp_version <- read_csv(paste0("https://ndownloader.figshare.com/files/", versions_to_run$filepath_id[v]), guess_max = guess_max) %>% 
#       distinct(study_id, core_id)
#     
#     # subset present cores table to include only the cores from this particular version iteration
#     # I am sure there might be a catch here
#     temp_table <- inner_join(current_cores, temp_version)
#     # subset_df <- current_cores %>% filter(study_id %in% unique(temp_version$study_id) | core_id %in% unique(temp_version$core_id))
#   }

for(v in unique(versions_to_run$version)){
  
  # subset synthesis history to identify studies and cores for a particular version
  temp_version <- versions_to_run %>% filter(version == v) %>% 
    # make some fixes to study IDs that should align things better
    mutate(study_id = gsub("[.]", "", study_id),
           study_id = case_when(grepl(" ", study_id) ~ gsub(" ", "_", study_id), T ~ study_id))
  
  # subset present cores table to include only the cores from this particular version iteration
  # I am sure there might be a catch here
  # subset_df <- left_join(temp_version, current_cores)
  temp_table <- current_cores %>% filter(study_id %in% unique(temp_version$study_id) | core_id %in% unique(temp_version$core_id))
  
  # v1_cores <- cores %>% filter(study_id %in% v1_studies)

    # compute metrics for given version
  quantity <- quantityMetric(temp_table)
  quality <- qualityMetric(temp_table)
  spatial <- spatialMetric(temp_table)
  habitat <- habitatMetric(temp_table)
  
  # join all rankings
  version_metrics <- quantity %>% full_join(quality) %>% full_join(spatial) %>% full_join(habitat) %>% 
    mutate(version = unique(temp_version$version),
           date = unique(temp_version$date))
  # add to final table
  all_rankings <- bind_rows(all_rankings, version_metrics)
  
  # inventory the data and compute metrics
  version_scores <- compositeMetricScore(version_metrics) %>% mutate(version = unique(temp_version$version),
                                                                     date = unique(temp_version$date))
  # add to final table
  all_scores <- bind_rows(all_scores, version_scores)
}

## Composite scores for Pew
scores_for_pew <- all_scores %>% filter(version == "1.3.0")
#   filter(version %in% c("1.0.0", "1.1.1", "1.2.0", "1.3.0"))
write_csv(scores_for_pew, "blue_carbon_data_inventory/report/ccn_report_card_data.csv")

all_rankings_clean <- all_rankings %>%
  select(-c(contains("_weight"), contains("_rank"), contains("mean_"))) %>% 
  rename(spatial_metric = spatial_representativeness_metric) %>% 
  mutate(habitat_metric = ifelse(state == "PA" | state == "DC", NA, habitat_metric)) %>% 
  select(date, version, everything())


all_rankings_clean %>% 
  # mutate(region = case_when(is.na(region) ~ "unknown", T ~ region)) %>% 
  # drop_na(region) %>% 
  # group_by(version, date, state) %>% 
  # summarise(core_count = sum(n)) %>% 
  # ungroup() %>% 
  ggplot() +
  geom_line(aes(date, quantity_metric)) +
  geom_point(aes(date, quantity_metric)) + 
  theme_bw() +
  facet_wrap(~state)

## Blue Carbon Data Inventory Report ####

# Import states (for the BCDI report)
states <- st_read("blue_carbon_data_inventory/data/shapefiles/us_states/states_coastline_boundaries/cb_2017_us_state_500k.shp",
                  stringsAsFactors = F)

# url_date <- format(Sys.time(), "%Y%m%d %H%M")
# formated_date <- format(Sys.time(), "%Y/%m/%d-%H:%M")

# generate data contributor report
rmarkdown::render(input = "blue_carbon_data_inventory/scripts/bcdi_report.Rmd",
                  # output_format = "html_document",
                  output_file = "BCDI_report",
                  output_dir = "blue_carbon_data_inventory/report/")

## Bibliography ####

# create bibliography for data used to update emissions factors
bib <- read_csv("data/CCN_study_citations.csv")

# library(RefManageR)

# filter synthesis study citations by relevant studies
bcdi_bib <- bib %>% 
  filter(study_id %in% unique(v2_cores$study_id)) %>% 
  mutate(across(everything(), as.character)) %>% 
  arrange(study_id)

# write bib to report folder
write_csv(bcdi_bib, "blue_carbon_data_inventory/report/BCDI_bibliography.csv")

