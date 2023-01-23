## CCN-Data-Analytics
## Author: Jaxine Wolfe <wolfejax@si.edu>
## Date: 11-07-2022

## Workflow for standardizing and deriving C accumulation values for the 2022 NGGI

## Outline ####

# read in reported values table
# read in synthesis tables: methods, cores, depthseries, impacts

# merge reported values with synthesis tables
# Merge any necessary information about these cores/sites that we have in the 
# Library along with the climate zones provided
# Filter out restored/degraded sites
# Merge climate zones
# Which/How many habitats are left?

# make sure everything is expressed in Carbon
# if only sed accumulation is present, 
# derive fraction carbon and resulting C accumulation rate using Craft relationship (1991) - we can specify more complexity later

# if only vertical accretion is present, get the stocks down to interval

# Perform unit conversions for C accumulation => g C m-2 yr-1

# if CRS and CIC models are used, we might end up averaging them

# Ari dating paper for Pb-210 derived dates
# https://bg.copernicus.org/articles/15/6791/2018/bg-15-6791-2018.html

# Kauffman total ecosystem carbon stocks
# https://onlinelibrary.wiley.com/doi/10.1111/gcb.15248

## Prepare Workspace ####

library(tidyverse)
library(readxl)

# source synthesis
source("shared_resources/refresh_data.R")

# read in reported values
reported <- read_csv("NGGI_2022/NGGI_2022_reported_values - literature_values.csv")

# read in climate zone table
climate_zones <- read_xlsx("nggi_2022/inventorying_bib/Climate zones.xlsx") %>% 
  mutate(Color = tolower(Color),
         Color = recode(Color, "oragen" = "orange"),
         State = recode(State, "Delamare" = "Delaware")) %>% 
  rename(admin_division = State, climate_zone = `Climate zone`, climate_color = Color)

# Prepare Core Synthesis ####

# isolate US cores with dating information and join climate zones
dated_us_cores <- cores %>% filter(country == "United States") %>% 
  # filter(habitat != "upland") %>% 
  drop_na(dates_qual_code) %>% 
  left_join(climate_zones) %>% 
  mutate(habitat = recode(habitat, "scrub shrub" = "scrub/shrub"))
  # left_join(methods %>% select())
  # left_join(depthseries %>% distinct(study_id, site_id, core_id, method_id)) %>% 
  # left_join(methods %>% select(study_id, method_id, fraction_carbon_type)) %>% 
  # select(study_id, site_id, core_id, method_id, fraction_carbon_type, everything()) %>% 
    # count(study_id, core_id)
  # filter(study_id %in% unique(reported$study_id))

# studies with split core methods
# Okeefe-Suttles_et_al_2021_Cape
# Luk_et_al_2020
# Breithaupt_et_al_2014
# McTigue_et_al_2020

gapfill_ds <- depthseries %>% 
  # filter(study_id %in% unique(dated_us_cores$study_id))
  filter(core_id %in% unique(dated_us_cores$core_id)) %>% 
  left_join(dated_us_cores %>% distinct(core_id, habitat)) %>% # add habitat info from cores table
  left_join(methods %>% distinct(study_id, method_id, fraction_carbon_type)) %>% 
  # gapfill fraction carbon 
  # relationship to convert total carbon to organic?
  # start with Craft 1991 relationship, increase complexity from there
  mutate(measured_or_modeled = case_when(is.na(fraction_carbon) ~ "modeled",
                                         study_id %in% c("Buffington_et_al_2020", "Drexler_et_al_2009", "Keshta_et_al_2020", "Radabaugh_et_al_2018", "Rodriguez_et_al_2022") ~ "modeled",
                                         T ~ "measured"),
         soc = case_when(is.na(fraction_carbon) & !is.na(fraction_organic_matter) & habitat == "marsh" ~ 0.4 * (100*fraction_organic_matter) + 0.0025*(100*fraction_organic_matter)^2,
                         is.na(fraction_carbon) & !is.na(fraction_organic_matter) & habitat == "mangrove" ~ 0.415 * (100*fraction_organic_matter) + 2.89,
                         fraction_carbon_type == "total carbon" ~ NA_real_,
                         T ~ fraction_carbon * 100)) %>% 
  select_if(function(x) {!all(is.na(x))}) %>%
  select(fraction_organic_matter, contains("carbon"), soc, everything())
# prepare reported values table and merge with core table
# do this after?
# join_tables <- reported %>%
#   # select(-contains("stock")) %>% 
#   filter(management == "natural") %>% 
#   left_join(cores) %>% 
#   left_join(methods) %>% 
#   left_join(impacts) %>% 
#   select_if(function(x) {!all(is.na(x))}) 

# total vs organic carbon
gapfill_ds %>%  
  drop_na(fraction_carbon) %>% 
  ggplot(aes(fraction_organic_matter*100, soc, col = fraction_carbon_type)) +
  geom_point(alpha = 0.5, pch = 1) 

gapfill_ds %>%  
  # filter(study_id == "Drexler_et_al_2009") %>% 
  # filter(habitat == "swamp") %>% 
  drop_na(soc) %>% 
  ggplot(aes(fraction_organic_matter * 100, soc, col = habitat)) +
  geom_point(alpha = 0.5, pch = 1) +
  facet_wrap(~measured_or_modeled)

gapfill_ds %>% drop_na(soc) %>% 
  ggplot(aes(soc)) +
  geom_density() + geom_rug()

## Reported Values ####

# investigate table
accumulation_types <- distinct(reported, study_id, accumulation_type)

## Pb-210

# create a function will perform unit conversions for C accumulation?

# "gramsPerSquareCentimeterPerYear" => divide by 0.0001
# "gramsPerSquareMeterPerYear"=> do nothing   
# "kilogramsPerSquareMeterPerYear" => multiply by 1000 
# "millimeterPerYear" => 
# "centimeterPerYear" =>  

pb <- reported %>% 
  select(contains("id"), core_count, accumulation_type, habitat,
         contains("depth"), contains("pb210")) %>% 
  filter(!grepl("-", pb210_rate)) %>% # Drexler study expressed C accumulation as a range
  mutate(across(-c(study_id, site_id, core_id, accumulation_type, habitat, pb210_rate_unit), as.numeric)) %>% 
  # drop_na(pb210_rate_unit) %>% 
  # unit conversions
  mutate(pb210_standardized = case_when(pb210_rate_unit == "gramsPerSquareCentimeterPerYear" ~ pb210_rate/0.0001,
                                        pb210_rate_unit == "kilogramsPerSquareMeterPerYear" ~ pb210_rate*1000,
                                        pb210_rate_unit == "gramsPerSquareMeterPerYear" ~ pb210_rate,
                                        T ~ NA_real_))

pb %>% 
  drop_na(pb210_standardized) %>% 
  ggplot(aes(pb210_standardized, col = accumulation_type)) +
  geom_density() +
  facet_wrap(~accumulation_type, scales = "free", dir = "v")

# pb %>% 
#   drop_na(pb210_standardized) %>% 
#   ggplot(aes(depth_min_cm, pb210_standardized, col = accumulation_type)) +
#   geom_point()
