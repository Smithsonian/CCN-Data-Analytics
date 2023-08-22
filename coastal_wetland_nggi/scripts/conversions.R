
# workflow to handle accretion rates that need to be converted to carbon accumulation rates

# Pseudocode steps

# CASE: Rate is expressed as Accretion (mm/yr or cm/yr) ----
# Identify the studies in the reported values table that express rates in terms of depth accretion 
# Identify the depth interval window for which those rates are reported
# Determine which cores match to those in the reported values table (unless this is a site)
# Calculate the carbon stock for those cores


## Prepare Workspace ####

library(tidyverse)

source("resources/pull_synthesis.R") # function to pull synthesis tables
source("coastal_wetland_nggi/scripts/nggi_utils.R")

# pull cores and depthseries tables
cores <- getSynthesisData("cores") %>% mutate(habitat = recode(habitat, "scrub shrub" = "scrub/shrub"))
ds <- getSynthesisData("depthseries")

## Calculate carbon interval stock 

# Gapfill depthseries data and calculate carbon density

ds_stocks <- ds %>%
  left_join(cores %>% select(study_id, site_id, core_id, habitat)) %>%
  # transfer representative depths to actual depths (need to adjust this later)
  mutate(depth_min = ifelse(is.na(depth_min) & !is.na(representative_depth_min), 
                            representative_depth_min, depth_min),
         depth_max = ifelse(is.na(depth_max) & !is.na(representative_depth_max), 
                            representative_depth_max, depth_max)) %>% 
  carbonStock(.)

# nrow(ds_stocks %>% drop_na(carbon_density))/nrow(ds_stocks)
# 63% coverage  
# Nahlic and Fennessey marsh outlier sample_id NWCA11-3583-1-11-MD-019-016-1
# density
# ggplot(ds_stocks %>% filter(carbon_density < 0.8), aes(carbon_density, habitat)) + geom_boxplot()
# ggplot(ds_stocks, aes(fraction_organic_matter, fraction_carbon, col = habitat)) + geom_point()
# stocks

# 
standard_stocks <- standardizeDepths(ds_stocks) %>% drop_na(fraction_carbon)

# # calculate whole 1m core stocks and scale to Mg/Ha
# core_stocks <- ds_stocks %>% 
#   drop_na(stock_gCm2) %>% 
#   group_by(study_id, site_id, core_id, habitat) %>%
#   summarize(stock_gCm2 = mean(stock_gCm2)) %>% 
#   #   # convert gC m-2 to MgC ha-1
#   mutate(stock_MgHa = stock_gCm2 * (10^4/10^6))
# 
# ggplot(core_stocks, aes(stock_gCm2, habitat)) + geom_boxplot()


## Reported Values ####

# NEEDS WORK
# turn this into a function which identifies the cores to pull numbers for
# and apply the accretion rate to CAR formula:
# Accretion rate (cm/yr) * DBD (g/cc) * fraction_carbon * 10000 (cm2/m2)

# needs to work through the Pb210 cols and then the Cs137, or create a long form table first
# and work rowwise

# read in reported values
reported <- read_csv("coastal_wetland_nggi/data/original/NGGI_reported_values_20230818 - literature_values.csv",
                     na = c("---", "NA", "")) %>% 
  select(-c(contains("radiocarbon"), contains("SET"), contains("marker"), contains("stock"),
            notes, reported_rates, publication_table)) %>% 
  # resolve the pb210 ...still a little problematic
  mutate(pb210_rate = as.numeric(pb210_rate), # the ranges will be dropped, need to address
         pb210_rate = case_when(is.na(pb210_rate) & !is.na(pb210_rate_cic) ~ pb210_rate_cic,
                                # can't seem to average the Luk??
                                T ~ pb210_rate))
  # filter(!is.na(pb210_rate) | !is.na(pb210_rate_cic) | !is.na(cs137_rate)) %>% 
  # spot fixes to match core_ids to the CCN synthesis
  # mutate(core_id = case_when(study_id == "Boyd_et_al_2017" ~ gsub("_", "", core_id),
  #                            study_id == "Boyd_2012" ~ gsub("-", "", core_id),
  #                             T ~ core_id)) %>% 
  # select_if(function(x) {!all(is.na(x))})
# do this later
  # mutate(pb210_rate = ifelse(pb210_rate_unit == "millimeterPerYear" ~ pb210_rate/10))

# specify accretion 
accretion_units <- c("millimeterPerYear", "centimeterPerYear")

accretion_datasets <- reported %>% 
  filter(pb210_rate_unit %in% accretion_units | cs137_rate_unit == accretion_units)

accretion_pb210 <- reported %>% 
  drop_na(pb210_rate | pb210_rate_cic) %>% 
  select_if(function(x) {!all(is.na(x))})

accretion_studies <- accretion_datasets %>% distinct(study_id) %>% pull(study_id)
# there's more now
# "Boyd_and_Sommerfield_2016" 
# "Kemp_et_al_2020" - matches         
# "Lagomasino_et_al_2020"     
# "Luk_et_al_2020"           
# "Smith_et_al_2015" - EXAMPLE     
# "Thom_1992"                 
# "Weis_et_al_2001"           
# "Vaughn_et_al_2020"   

# study <- "Smith_et_al_2015"

# for (study in sed_accumulation_studies) {
# reported_study <- reported %>% 
#   filter(study_id == study) %>% 
#   select_if(function(x) {!all(is.na(x))})
# 
# ccn_study <- ds %>% 
#   filter(study_id == study) %>% 
#   filter(core_id %in% unique(reported_study$core_id))
# }

