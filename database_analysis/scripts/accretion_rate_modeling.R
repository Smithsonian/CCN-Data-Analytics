## CCN-Data-Analytics
## Author: Jaxine Wolfe <wolfejax@si.edu>
## Date: 02-07-2023

## Workflow for determining and applying algorithms to analyze soil core data and produce probabalistic accretion rate models

## Prepare Workspace ####

library(tidyverse)

# source synthesis
source("resources/pull_synthesis.R")

cores <- getSynthesisData("cores")
depthseries <- getSynthesisData("depthseries")

## Algorithm Assignment ####

# isolate cores with dating information and join climate zones
dated_cores <- cores %>% 
  drop_na(dates_qual_code) %>% 
  distinct(study_id, core_id) %>% 
  mutate(cs137_present = FALSE, 
         pb210_present = FALSE,
         c14_or_horizons_present = FALSE)

# loop through cores and determine presence/absence of necessary variables
for(i in 1:nrow(dated_cores)){
  temp_ds <- depthseries %>% filter(core_id == dated_cores$core_id[i]) %>%
    select_if(function(x) {!all(is.na(x))})
    
  # Flag cs137 
  if("cs137_activity" %in% names(temp_ds)){
    dated_cores$cs137_present[i] <- TRUE
  }
  
  # Flag pb210 
  if("total_pb210_activity" %in% names(temp_ds)){
    dated_cores$pb210_present[i] <-  TRUE
  }
  
  # Flag radiocarbon dating or historical horizons 
  if(any(c("c14_age", "marker_date") %in% names(temp_ds))){
    dated_cores$c14_or_horizons_present[i] <-  TRUE
  }
}

# create table indicating which algorithms should be used for which cores
dated_core_algos <- dated_cores %>% 
  # distinct(cs137_present, pb210_present, c14_or_horizons_present) %>% 
  mutate(approach = case_when(cs137_present == T & pb210_present == T & c14_or_horizons_present == F ~ "PLUM (with Cs137 uncertainty propagation)",
                              cs137_present == T & pb210_present == F & c14_or_horizons_present == F ~ "Single average rate (whole core)",
                              cs137_present == F & pb210_present == F & c14_or_horizons_present == T ~ "BACON",
                              cs137_present == F & pb210_present == T & c14_or_horizons_present == F ~ "PLUM",
                              cs137_present == T & pb210_present == F & c14_or_horizons_present == T ~ "BACON",
                              T ~ NA_character_))
# there is one core that only has no total Pb210, only excess Pb210 and Ra226
# without total Pb210,is this core still valid for accumulation rate modeling?

dated_core_algos %>% drop_na(approach) %>% 
  count(approach) %>% 
  mutate(approach = reorder(approach, n)) %>% 
  ggplot(aes(approach, n)) +
  geom_col() + coord_flip()

# do units need standardization for the models?
activity_units <- depthseries %>% 
  select(study_id, contains("unit")) %>% distinct() %>% 
  pivot_longer(col = -study_id, names_to = "activity_unit", values_to = "unit") %>% 
  drop_na(unit)
  
sort(unique(activity_units$unit))


## Accumulation Rate Modeling ####

# https://cran.r-project.org/web/packages/rbacon/vignettes/FAQ.html
# https://cran.r-project.org/web/packages/rbacon/vignettes/intro.html
