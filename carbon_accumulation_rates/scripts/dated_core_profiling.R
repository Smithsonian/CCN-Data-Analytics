## CCN-Data-Analytics
## Author: Jaxine Wolfe <wolfejax@si.edu>
## Date: 02-07-2023

## Workflow for determining and applying algorithms to analyze soil core data and produce probabalistic accretion rate models

## Prepare Workspace ####

library(tidyverse)

# source synthesis
# source("resources/pull_synthesis.R")

# cores <- getSynthesisData("cores")
# depthseries <- getSynthesisData("depthseries")
# methods <- getSynthesisData("methods")
# bib <- getSynthesisData("citations")

depthseries <- read_csv("data/CCRCN_depthseries.csv")
depthseries <- read_csv("data/CCRCN_depthseries.csv", guess_max = nrow(depthseries))
cores <- read_csv("data/CCRCN_cores.csv")
cores <- read_csv("data/CCRCN_cores.csv",
                  guess_max = nrow(cores))

## Algorithm Assignment ####

# isolate cores with dating information
dated_cores <- cores %>% 
  drop_na(dates_qual_code) %>% 
  filter(habitat %in% c("marsh", "mangrove", "swamp", "seagrass")) %>% 
  distinct(study_id, site_id, core_id) %>% 
  arrange(study_id, site_id, core_id) %>% 
  mutate(i = 1:n()) %>% 
  mutate(cs137_present = NA, 
          pb210_present = NA,
         c14_present = NA,
         historical_horizons_present = NA)

# # write table of dated core IDs for manual assignment of profile type
# write_csv(cores %>% drop_na(dates_qual_code) %>% distinct(study_id, site_id, core_id) %>% 
#             arrange(study_id, site_id, core_id),
#           "carbon_accumulation_rates/ccn_dated_cores.csv")

# loop through cores and determine presence/absence of necessary variables
for(i in 1:nrow(dated_cores)){
  temp_ds <- depthseries %>% filter(core_id == dated_cores$core_id[i]) %>%
    select_if(function(x) {!all(is.na(x))})
    
  # Flag cs137 
  if("cs137_activity" %in% names(temp_ds)){
    dated_cores$cs137_present[i] <- "Cs137"
  }
  
  # Flag pb210 
  if("total_pb210_activity" %in% names(temp_ds)){
    dated_cores$pb210_present[i] <-  "Pb210"
  }
  if("excess_pb210_activity" %in% names(temp_ds)){
    dated_cores$pb210_present[i] <-  "Pb210"
  }
  
  # Flag radiocarbon dating or historical horizons 
  if(any(c("c14_age") %in% names(temp_ds))){
    dated_cores$c14_present[i] <-  "C14"
  }
  
  if(any(c("marker_date") %in% names(temp_ds))){
    dated_cores$historical_horizons_present[i] <-  "Historical Horizons"
  }
}

# write_csv(dated_cores, "carbon_accumulation_rates/ccn_dated_cores.csv")

table_summary <- dated_cores %>% 
  gather(key="variable", value = "present", -c(i,study_id,  site_id, core_id)) %>% 
  filter(complete.cases(present)) %>% 
  arrange(present) %>% 
  group_by(i, study_id, site_id, core_id) %>% 
  summarise(variables_present = paste(present, collapse = ", "))

# create table indicating which algorithms should be used for which cores
# dated_core_algos <- dated_cores %>% 
#   # distinct(cs137_present, pb210_present, c14_or_horizons_present) %>% 
#   mutate(approach = case_when(cs137_present == T & pb210_present == T & c14_or_horizons_present == F ~ "PLUM (with Cs137 uncertainty propagation)",
#                               cs137_present == T & pb210_present == F & c14_or_horizons_present == F ~ "Single average rate (whole core)",
#                               cs137_present == F & pb210_present == F & c14_or_horizons_present == T ~ "BACON",
#                               cs137_present == F & pb210_present == T & c14_or_horizons_present == F ~ "PLUM",
#                               cs137_present == T & pb210_present == F & c14_or_horizons_present == T ~ "BACON",
#                               T ~ NA_character_))
# there is one core that only has no total Pb210, only excess Pb210 and Ra226
# without total Pb210,is this core still valid for accumulation rate modeling?

# dated_core_algos %>% drop_na(approach) %>% 
#   count(approach) %>% 
#   mutate(approach = reorder(approach, n)) %>% 
#   ggplot(aes(approach, n)) +
#   geom_col() + coord_flip()

# do units need standardization for the models?
# activity_units <- depthseries %>% 
#   select(study_id, contains("unit")) %>% distinct() %>% 
#   pivot_longer(col = -study_id, names_to = "activity_unit", values_to = "unit") %>% 
#   drop_na(unit)
#   
# sort(unique(activity_units$unit))
write_csv(table_summary, "carbon_accumulation_rates/ccn_dated_cores.csv")

## Accumulation Rate Modeling ####

# https://cran.r-project.org/web/packages/rbacon/vignettes/FAQ.html
# https://cran.r-project.org/web/packages/rbacon/vignettes/intro.html
