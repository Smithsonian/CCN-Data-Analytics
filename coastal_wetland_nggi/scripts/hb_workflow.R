## CCN-Data-Analytics
# Workflow for standardizing C accumulation values for the 2022 NGGI
# contact: Henry Betts, BettsH@si.edu

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

## Meng data ####
# Prepare Meng's 2017 synthesis data for merge with reported values
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
                                    T ~ salinity_class),
         depth_min = ifelse(!is.na(depth_max), 0, NA_character_)) 
 

## Prepare climate zone list ####
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
reported_avg <- reported_raw %>% 
  filter(core_id == "2L-Pb") %>% 
  group_by(study_id, site_id, core_id, habitat, management, accumulation_type, pb210_rate_unit) %>%
  summarise(pb210_rate = mean(as.numeric(pb210_rate)),
            pb210_rate_se = sd(as.numeric(pb210_rate)))

# Drexler_et_al_2013 has pb210_rate as a range - remove and replace with summarized data
reported_drex <- reported_raw %>% 
  filter(grepl("Drexler_et_al_2013", study_id)) %>% 
  separate(pb210_rate, into = c("pb210_rate", "pb210_rate_max"), sep = "-") %>% 
  separate(depth_max_cm, into = c("depth_min_cm", "depth_max_cm"), sep = "-") %>% 
  mutate(pb210_rate = (as.numeric(pb210_rate) + as.numeric(pb210_rate_max))/2)


reported <- reported_raw %>%
  
  # remove the disaggregated core and replace with summary data
  filter(!grepl("2L-Pb", core_id)) %>%  
  full_join(reported_avg %>% 
              mutate(pb210_rate = as.character(pb210_rate))) %>% 
  
  # Drexler_et_al_2013 has pb210_rate as a range - remove and replace with summarized data
  filter(!grepl("Drexler_et_al_2013", study_id)) %>% 
  full_join(reported_drex %>% 
              mutate(depth_min_cm = as.numeric(depth_min_cm), # align class types
                     pb210_rate = as.character(pb210_rate))) %>% 
  
  
  separate(carbon_stock, into = c("carbon_stock", "carbon_stock_se"), sep = " [+][/][-] | [+][/][-]") %>%
  mutate(pb210_rate = as.numeric(pb210_rate),
         depth_max_cm = as.numeric(depth_max_cm),
         depth_min_cm = as.numeric(depth_min_cm),
         carbon_stock = as.numeric(carbon_stock),
         pb210_rate = as.numeric(pb210_rate),
         
         # merge pb210 rate types (there's no overlap)
         pb210_rate = ifelse(!is.na(pb210_rate_cic), pb210_rate_cic, pb210_rate), 
         pb210_rate_se = ifelse(!is.na(pb210_rate_cic_se), pb210_rate_cic_se, pb210_rate_se),
         
         # convert OM to OC depending on habitat type
         carbon_stock = case_when(habitat == "marsh" & accumulation_type == "organic matter" ~ 0.486 * carbon_stock - 0.0160 * carbon_stock^2,
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
                                T ~ cs137_rate),
         
         # make accumulation/accretion flag for cs137 and pb210 to convert accretion rate to accumulation rate after data library depthseries merge below
         cs_accretion = ifelse(cs137_rate_unit == "millimeterPerYear", "accretion",
                                         ifelse(cs137_rate_unit == "centimeterPerYear", "accretion", NA_character_)),
         cs_accumulation = ifelse(cs137_rate_unit == "gramsPerSquareMeterPerYear", "accumulation",
                                  ifelse(cs137_rate_unit == "gramsPerSquareCentimeterPerYear", "accumulation",
                                         ifelse(cs137_rate_unit == "kilogramsPerSquareMeterPerYear", "accumulation", NA_character_))),
         pb_accretion = ifelse(pb210_rate_unit == "millimeterPerYear", "accretion",
                               ifelse(pb210_rate_unit == "centimeterPerYear", "accretion", NA_character_)),
         pb_accumulation = ifelse(pb210_rate_unit == "gramsPerSquareCentimeterPerYear", "accumulation",
                                  ifelse(pb210_rate_unit == "gramsPerSquareMeterPerYear", "accumulation",
                                         ifelse(pb210_rate_unit == "kilogramsPerSquareMeterPerYear", "accumulation", NA_character_))),
         
         # create column for accumulation rates
         pb_CAR = ifelse(grepl("accretion", pb_accretion) & carbon_stock > 0, pb210_rate * carbon_stock, 
                         ifelse(grepl("accumulation", pb_accumulation), pb210_rate,
                                NA_character_)),
         cs_CAR = ifelse(grepl("accretion", cs_accretion) & carbon_stock > 0, cs137_rate * carbon_stock, 
                         ifelse(grepl("accumulation", cs_accumulation), cs137_rate,
                                NA_character_))) %>% 
  
  left_join(dated_us_cores) %>% 
  assignEcosystem() %>% 
  
  select(core_id, pb_CAR, cs_CAR, everything())

## Merge reported values with depthseries data from the library ####

# remove redundant study_ids
reported_merge <- reported %>% 
  
  # remove study_ids duplicated in the meng data:
  # reported_test <- reported %>% 
  #   mutate(study_id = gsub("_et_al_|_and_.*_", "_", study_id)) # match the meng study_id structure
  # intersect(meng$study_id, reported_test$study_id)
  # note, Callaway_2019 in reported values = Callaway_2012 in meng
  filter(study_id != "Craft_2007") %>% # 3 obs
  filter(study_id != "Noe_et_al_2016") %>% # 4 obs
  filter(study_id != "Callaway_et_al_2019") %>% # 34 obs
  
  # rename core_ids to match library data
  mutate(source = "reported_value", # create a flag to remove unmatched cores after the depthseries data merge
         core_id = case_when(grepl("Nanticoke", site_id) ~ paste("NRM_", core_id, sep = ""), # Allen_et_al_2022
                             grepl("Typha", core_id) ~ gsub("Typha", "Typha ", core_id), # Arias-Ortiz_et_al_2021 
                             grepl("CRMS|CRMS0", core_id) ~ gsub("CRMS|CRMS0", "", core_id), # Baustian_et_al_2021
                             study_id == "Boyd_2012" ~ gsub("-", "", core_id), # Boyd_2012
                             study_id == "Boyd_et_al_2017" ~ gsub("_", "", core_id), # Boyd_et_al_2017
                             grepl("Choptank_non_tidal", core_id) ~ "ChoptankNontidal", # Ensign_et_al_2020
                             # grepl("Choptank_head_of_tide", core_id) ~ "", HOW TO ASSIGN THESE?
                             # grepl("Choptank_TFFW", core_id) ~ "",
                             # grepl("Choptank_marsh", core_id) ~ "",
                             grepl("Pocomoke_non_tidal", core_id) ~ "PocomokeNontidal",
                             # grepl("Pocomoke_head_of_tide", core_id) ~ "", HOW TO ASSIGN THESE?
                             # grepl("Pocomoke_TFFW", core_id) ~ "",
                             # grepl("Pocomoke_marsh", core_id) ~ "",
                             grepl("2J", core_id) ~ "1_2J", # Poppe_and_Rybczyk_2018
                             grepl("3F", core_id) ~ "2_3F",
                             grepl("4F", core_id) ~ "3_4F",
                             site_id == "Poppe_and_Rybczyk_2018" & grepl("5B", core_id) ~ "4_5B",
                             grepl("5F", core_id) ~ "5_5F",
                             grepl("5J", core_id) ~ "6_5J",
                             grepl("Salt_marsh", core_id) ~ gsub("Salt_marsh", "Salt_Marsh", core_id), # Vaughn_et_al_2020
                             T ~ core_id)) %>% 
  
  select(core_id, pb_CAR, cs_CAR, everything())

# unmatchable study_ids: Carlin_et_al_2021, Boyd_and_Sommerfield_2016
# Arias-Ortiz_et_al_2021 core_id fix adds a space
# Arriola_and_Cable_2017 - depth site_ids: snipe_creek_high_marsh/low_marsh/hammock
#                        - reported site_ids: mouth, mc, upstream, forest
#                        look at paper
# Breithaupt_et_al_2014 is a sum of 6 cores
# Drexler_et_al_2019 is a sum of 6 cores 
# Ensign_et_al_2020 - depth cores: upper/middle/lower tidal
#                   - reported cores: head of tide/TFFW/marsh
#                   how do these compare?
# Luk_et_al_2020 is a sum of 3 cores
# Peck_et_al_2020 - reported has higher core count than depth? reported has 2 sites for e/depth site
# Poppe_and_Rybczyk_2019 sites are a sum of 2 cores
# Poppe_et_al_2019 sites are a sum of 2 cores

reported_stocks <- full_join(reported_merge %>% 
                               drop_na(core_id), 
                             depth, by = "core_id") %>% 
  drop_na(source) %>% # remove library entries not found in reported values
  filter(!grepl("-", dry_bulk_density)) %>% # remove negative DBD values
  mutate(dry_bulk_density = ifelse(is.na(dry_bulk_density), 0, dry_bulk_density), # convert missing values to 0 for MAR to CAR conversion below
         fraction_carbon = ifelse(is.na(fraction_carbon), 0, fraction_carbon),
         fraction_organic_matter = ifelse(is.na(fraction_organic_matter), 0, fraction_organic_matter),
         
         # convert FOM to FOC depending on habitat type (same conversion as above in reported)
         c_stock = dry_bulk_density * fraction_organic_matter, 
         foc_calc = ifelse(grepl("marsh", habitat) & fraction_organic_matter > 0, 0.486 * c_stock - 0.0160 * c_stock^2,
                           ifelse(grepl("swamp", habitat) & fraction_organic_matter > 0, 0.427 * c_stock + 0.0635 * c_stock^2,
                                  0)), # if FOM isn't available, mark as zero to omit in the CAR conversion below
         
         # use the FOC calculated above to convert accretion rate to C accumulation rate
         pb_CAR = ifelse(grepl("accretion", pb_accretion) & fraction_carbon > 0, pb210_rate * dry_bulk_density * fraction_carbon, # if FOC is present in the library
                             ifelse(grepl("accretion", pb_accretion) & fraction_carbon == 0 & foc_calc > 0, pb210_rate * foc_calc, # if FOC isn't present in the library, use calculated c_stock from FOM 
                                 ifelse(grepl("accretion", pb_accretion) & fraction_carbon == 0 & foc_calc == 0, NA_character_, # remove accretion rates that have no corresponding FOM or FOC
                                        pb_CAR))), # the remainder are the already calculated/existing accumulation rates
         cs_CAR = ifelse(grepl("accretion", cs_accretion) & fraction_carbon > 0, cs137_rate * dry_bulk_density * fraction_carbon,
                               ifelse(grepl("accretion", cs_accretion) & fraction_carbon == 0 & foc_calc > 0, cs137_rate * foc_calc,
                                      ifelse(grepl("accretion", cs_accretion) & fraction_carbon == 0 & foc_calc == 0, NA_character_, 
                                             cs_CAR)))) 
  
  



# TO DO
# resolve reported value and depthseries core id matches (and NAs!)
# merge with meng 
# subset by rate depths
# tests (see nggi_data_analysis.R & docs folder & meeting notes)



# QUESTIONS
# omit pb/cs rates from depthseries data?
# in table(reported$habitat): OM to OC conversion for mudflat and seagrass?
# remove core if reported value pb210_rate (cm/yr) is > 0 but no corresponding FOM or FOC?
# Callaway_2019 in reported = Callaway_2012 in meng?




# callaway 2019 core_id fixes:
# grepl("BRI_low_A", core_id) ~ "Browns_Island_A_Low",
# grepl("BRI_mid_A", core_id) ~ "Browns_Island_A_Mid",
# grepl("BRI_high_A", core_id) ~ "Browns_Island_A_High",
# grepl("BRI_low_B", core_id) ~ "Browns_Island_B_Low",
# grepl("BRI_mid_B", core_id) ~ "Browns_Island_B_Mid",
# grepl("BRI_high_B", core_id) ~ "Browns_Island_B_High",
# grepl("CC_high_A", core_id) ~ "China_Camp_A_High",
# grepl("CC_low_A", core_id) ~ "China_Camp_A_low",


