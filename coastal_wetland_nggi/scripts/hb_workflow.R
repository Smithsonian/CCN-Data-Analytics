## CCN-Data-Analytics
# Workflow for standardizing C accumulation values for the 2022 NGGI
# contact: Henry Betts, BettsH@si.edu

# meng Qs:
# gC/cc to gC/m2?
# SOC1 or C?
# depth_min missing
# salinity_regime or salinity_in_water?
# change depth_min_cm to depth_min
# use salinity for anything?
# Callaway_2019 in reported = Callaway_2012 in meng?


# reported Qs:
# sediment accretion & total carbon to OC? just use total carbon
# drexler pb210_rate ranges

# synth Qs:
# remove entries w/o C stock data? NO, KEEP EVERYTHING IN (i.e., mgmt class to see if there is a significant difference later on)
# only pull depthseries data for studies that didn't already include carbon stock?
# no piazza_et_al_2021 (reported value) in data library?

# to do: 
# merge depth & RV (resolve core_ids)
# calc stock...gap fill dbd (jim's model)...also look for accretion v accumulation rates?
# merge meng - intersect(meng_merge$study_id, depth_merge$study_id)
# reported_avg won't left join
# tests (see nggi_data_analysis.R & docs folder & meeting notes)


## Prepare workspace ####
library(tidyverse)
library(readxl)

# source synthesis
source("resources/pull_synthesis.R")
source("coastal_wetland_nggi/scripts/nggi_utils.R")

# read in reported values
reported_raw <- read_csv("coastal_wetland_nggi/data/original/NGGI_2022_reported_values - literature_values.csv")

# read in data from previous NGGI
mengdat <- read_csv("coastal_wetland_nggi/data/original/US-BC-Analysis-1-105.csv")

# read in synthesis data
cores <- getSynthesisData("cores")
depth <- getSynthesisData("depthseries")

## Prepare Meng's 2017 synthesis data for merge with reported values ####
meng <- mengdat %>% 
  rename(latitude = Latitude,
         longitude = Longitude,
         habitat = Ecosystem,
         ecosystem = CCAP_Class,
         climate_zone = Climate_Zone,
         carbon_stock = SOC1, 
         carbon_stock_unit = SOC1units,
         depth_max = SOC1depth, 
         salinity_class = Salinity_Regime, 
         pb210_rate = delSOC1Pb, 
         pb210_rate_unit = delSOC1units,
         cs137_rate = delSOC2Cs,
         cs137_rate_unit = delSOC2units) %>% 
  mutate(study_id = paste(First_Author, Year, sep = "_"),
         pb210_rate = as.numeric(pb210_rate),
         salinity_class = case_when(salinity_class == "0-0.3" ~ "fresh",
                                    salinity_class == "0-0.4" ~ "fresh",
                                    salinity_class == "<0.5" ~ "fresh",
                                    salinity_class == "0-15" ~ "mesohaline",
                                    salinity_class == ">18" ~ "polyhaline",
                                    salinity_class == "<18" ~ "mesohaline",
                                    salinity_class == "20-35" ~ "mixoeuhaline",
                                    salinity_class == "oligo" ~ "oligiohaline",
                                    salinity_class == "poly" ~ "polyhaline",
                                    salinity_class == "meso" ~ "mesohaline",
                                    T ~ salinity_class)) %>%
  
  # isolate values where there's lead or cesium dating 
  filter(!is.na(pb210_rate | cs137_rate)) %>% 
  select(study_id, latitude, longitude, habitat, ecosystem, climate_zone, carbon_stock, carbon_stock_unit, depth_max,
         salinity_class, pb210_rate, pb210_rate_unit, cs137_rate, cs137_rate_unit)
         

## Reiterate JW's reported value workflow ####
# prepare climate zone list
climate_zones <- read_xlsx("coastal_wetland_nggi/data/original/Climate zones.xlsx") %>% 
  mutate(State = recode(State, "Delamare" = "Delaware")) %>% 
  rename(admin_division = State, 
         climate_zone = `Climate zone`)

# isolate US cores from the library with dating information and join climate zones
dated_us_cores <- cores %>% 
  filter(country == "United States") %>% 
  drop_na(dates_qual_code) %>% # isolate dated cores
  left_join(climate_zones) %>% 
  mutate(habitat = recode(habitat, "scrub shrub" = "scrub/shrub"),
         vegetation_class = case_when(core_id == "MR3" ~ "marsh",
                                      T ~ vegetation_class))


## Standardize reported values ####

# look for disaggregated core data. average across depth intervals. (McTigue_et_al_2020: 2L-Pb)
# reported_raw %>%
#   drop_na(core_id) %>% add_count(site_id, core_id) %>% filter(n > 1) 

# pull the summary of the disaggregated core to add back in later
reported_avg <- reported_raw %>% 
  filter(core_id == "2L-Pb") %>% 
  group_by(study_id, site_id, core_id, habitat, management, accumulation_type, pb210_rate_unit) %>%
  summarise(pb210_rate = mean(as.numeric(pb210_rate)),
            pb210_rate_se = sd(as.numeric(pb210_rate)))

reported <- reported_raw %>%
  filter(!grepl("-", pb210_rate)) %>% # Drexler_et_al_2013 has pb210_rate as a range
  filter(!grepl("2L-Pb", core_id)) %>% # remove the disaggregated core 
  separate(carbon_stock, into = c("carbon_stock", "carbon_stock_se"), sep = " [+][/][-] | [+][/][-]") %>%
  #  left_join(reported_avg) %>% # replace with the summary data
  mutate(pb210_rate = as.numeric(pb210_rate),
         depth_max = as.numeric(depth_max_cm),
         depth_min = as.numeric(depth_min_cm),
         carbon_stock = as.numeric(carbon_stock),
         pb210_rate = as.numeric(pb210_rate),
         
         # merge pb210 rate types (there's no overlap)
         pb210_rate = ifelse(!is.na(pb210_rate_cic), pb210_rate_cic, pb210_rate), 
         pb210_rate_se = ifelse(!is.na(pb210_rate_cic_se), pb210_rate_cic_se, pb210_rate_se),
         
         # convert OM to OC depending on habitat type
         carbon_stock = case_when(habitat == "tidal_fresh_marsh" & accumulation_type == "organic matter" ~ 0.486 * carbon_stock - 0.0160 * carbon_stock^2, 
                                                   habitat == "marsh" & accumulation_type == "organic matter" ~ 0.486 * carbon_stock - 0.0160 * carbon_stock^2,
                                                   habitat == "mangrove | scrub/shrub | swamp" & accumulation_type == "organic matter" ~ 0.427 * carbon_stock + 0.0635 * carbon_stock^2,
                                                   T ~ carbon_stock),
         
         # convert pb210 and cs137 rates to cm/yr (accretion rate) or g/m2/yr (accumulation rate), and carbon stock to Mg/ha
         carbon_stock = case_when(carbon_stock_unit == "gramsPerSquareMeter" ~ carbon_stock * .01, 
                                  carbon_stock_unit == "kilogramsPerSquareMeter" ~ carbon_stock * 10,
                                  T ~ carbon_stock),
         pb210_rate = case_when(pb210_rate_unit == "millimeterPerYear" ~ pb210_rate * .1, 
                                pb210_rate_unit == "gramsPerSquareCentimeterPerYear" ~ pb210_rate * 0.0001,
                                pb210_rate_unit == "kilogramsPerSquareMeterPerYear" ~ pb210_rate * 1000,
                                T ~ pb210_rate),
         cs137_rate = case_when(cs137_rate_unit == "millimeterPerYear" ~ cs137_rate * .1,
                                cs137_rate_unit == "gramsPerSquareCentimeterPerYear" ~ cs137_rate * 0.0001,
                                cs137_rate_unit == "kilogramsPerSquareMeterPerYear" ~ cs137_rate * 1000,
                                T ~ cs137_rate)) %>% 
  filter(!is.na(pb210_rate | cs137_rate)) %>% 
  left_join(dated_us_cores) %>% 
  assignEcosystem() %>% 
  select(study_id, site_id, core_id, core_count, habitat, ecosystem, climate_zone, carbon_stock, carbon_stock_se, 
         depth_min, depth_max, pb210_rate, pb210_rate_se, cs137_rate, cs137_rate_se)


## Merge reported values with depthseries data from the library ####

# remove redundant study_ids
reported_merge <- reported %>% 
  
  # remove study_ids duplicated in the meng data:
  # reported_test <- reported %>% 
  #   mutate(study_id = gsub("_et_al_|_and_.*_", "_", study_id)) # match the meng study_id structure
  # intersect(meng$study_id, reported_test$study_id)
  # note, Callaway_2019 in reported values = Callaway_2012 in meng
  filter(study_id != "Craft_2007") %>% 
  filter(study_id != "Noe_et_al_2016") %>% 
  filter(study_id != "Callaway_et_al_2019") %>% 
  
  # study_id without library stock value:
  # setdiff(reported$study_id, depth$study_id)
  filter(study_id != "Piazza_et_al_2021") %>% 
  
  # rename core_ids to match library data
  mutate(core_id = case_when(grepl("Nanticoke", site_id) ~ paste("NRM_", core_id, sep = ""), # Allen_et_al_2022
                             grepl("Typha", core_id) ~ gsub("Typha", "Typha ", core_id), # Arias-Ortiz_et_al_2021 
                             grepl("CRMS|CRMS0", core_id) ~ gsub("CRMS|CRMS0", "", core_id), # Baustian_et_al_2021
                             study_id == "Boyd_2012" ~ gsub("-", "", core_id), # Boyd_2012
                             study_id == "Boyd_et_al_2017" ~ gsub("_", "", core_id), # Boyd_et_al_2017
                             # grepl("BRI_low_A", core_id) ~ "Browns_Island_A_Low", # Callaway_et_al_2019 (but, remove from reported)
                             # grepl("BRI_mid_A", core_id) ~ "Browns_Island_A_Mid",
                             # grepl("BRI_high_A", core_id) ~ "Browns_Island_A_High",
                             # grepl("BRI_low_B", core_id) ~ "Browns_Island_B_Low",
                             # grepl("BRI_mid_B", core_id) ~ "Browns_Island_B_Mid",
                             # grepl("BRI_high_B", core_id) ~ "Browns_Island_B_High",
                             # grepl("CC_high_A", core_id) ~ "China_Camp_A_High",
                             # grepl("CC_low_A", core_id) ~ "China_Camp_A_low",
                             grepl("Choptank_non_tidal", core_id) ~ "ChoptankNontidal", # Ensign_et_al_2020
                             grepl("Choptank_head_of_tide", core_id) ~ "",
                             grepl("Choptank_TFFW", core_id) ~ "",
                             grepl("Choptank_marsh", core_id) ~ "",
                             grepl("Pocomoke_non_tidal", core_id) ~ "PocomokeNontidal",
                             grepl("Pocomoke_head_of_tide", core_id) ~ "",
                             grepl("Pocomoke_TFFW", core_id) ~ "",
                             grepl("Pocomoke_marsh", core_id) ~ "",
                             grepl("2J", core_id) ~ "1_2J", # Poppe_and_Rybczyk_2018
                             grepl("3F", core_id) ~ "2_3F",
                             grepl("4F", core_id) ~ "3_4F",
                             site_id == "Poppe_and_Rybczyk_2018" & grepl("5B", core_id) ~ "4_5B",
                             grepl("5F", core_id) ~ "5_5F",
                             grepl("5J", core_id) ~ "6_5J",
                             grepl("Salt_marsh", core_id) ~ gsub("Salt_marsh", "Salt_Marsh", core_id))) # Vaughn_et_al_2020

# unmatchable study_ids: Carlin_et_al_2021, Boyd_and_Sommerfield_2016
# Arias-Ortiz_et_al_2021 core_id fix adds a space
# Arriola_and_Cable_2017 - depth site_ids: snipe_creek_high_marsh/low_marsh/hammock
#                        - reported site_ids: mouth, mc, upstream, forest
#                        look at paper
# Baustian_et_al_2021 - double check to see if the "or" statement works 
# Breithaupt_et_al_2014 is a sum of 6 cores?
# Craft_2007 is a sum of 6 cores for e/of 3 sites (but, remove from reported)
# Drexler_et_al_2019 is a sum of 6 cores 
# Ensign_et_al_2020 - depth cores: upper/middle/lower tidal
#                   - reported cores: head of tide/TFFW/marsh
#                   how do these compare?
# Luk_et_al_2020 is a sum of 3 cores
# Peck_et_al_2020 - reported has higher core count than depth? reported has 2 sites for e/depth site
# Poppe_and_Rybczyk_2019 sites are a sum of 2 cores
# Poppe_et_al_2019 sites are a sum of 2 cores

depth_merge <- depth %>% 
  rename(cs137_rate = cs137_activity,
       cs137_rate_se = cs137_activity_se,
       pb210_rate = excess_pb210_activity,
       pb210_rate_se = excess_pb210_activity_se)

reported_stocks <- full_join(reported_merge, depth_merge)






# jim's function:
# predict_dbd_from_loi <- function(fom, k1 = 0.098, k2 = 1.67) {
#   return(1/((fom/k1) + ((1-fom)/k2)))
# }
# 
# plot(seq(0,1, by=0.01), predict_dbd_from_loi(seq(0,1, by=0.01)))


# If: Carbon accumulation rate
# Then: Good as is
# 
# If: Organic matter accumulation rate
# Then: Use fraction organic matter to fraction c conversion function
# 
# If: Mass accumulation rate
# 
# What else is included? 
#   If: Fraction carbon
# Then: CAR = MAR x Fraction carbon
# 
# If: Fraction organic matter
# Then: fraction carbon = fraction c conversion function (fraction organic matter)
#   CAR = MAR x fraction 
# 
# If: Accretion rate
# 
# What else is included? 
#   If: Fraction carbon
# Then: CAR = accretion (cm year) x Dry bulk density (g cc) x fraction carbon (dimensionless)
# 
# If: Fraction organic matter
# Then: fraction carbon = fraction c conversion function (fraction organic matter)
#   CAR = accretion (cm year) x Dry bulk density (g cc) x fraction carbon (dimensionless)












