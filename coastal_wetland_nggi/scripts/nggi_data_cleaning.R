## CCN-Data-Analytics
## Author: Jaxine Wolfe and Henry Betts

## Workflow for standardizing C accumulation values for the 2022 NGGI
## And merging the table with the previous NGGI table from 2017

## Outline ####

# read in reported values table

# merge reported values with synthesis tables
# Merge any necessary information about these cores/sites that we have in the 
# Library along with the climate zones provided
# Filter out restored/degraded sites
# Merge climate zones
# Which/How many habitats are left?

# make sure everything is expressed in Carbon
# keep in mind - depth integration!!

# if carbon Accumulation data is present in the data release only but we dropped it during curation
# Revisit data curation and see if there are carbon stock values we can revive

# if only sed accumulation is present, 
# derive fraction carbon and resulting C accumulation rate using Craft relationship (1991) - we can specify more complexity later

# if only vertical accretion is present, get the stocks down to interval
# Perform unit conversions for C accumulation => g C m-2 yr-1
# if CRS and CIC models are used, we might end up averaging them

## Paper Resources
# Ari dating paper for Pb-210 derived dates
# https://bg.copernicus.org/articles/15/6791/2018/bg-15-6791-2018.html

# Kauffman total ecosystem carbon stocks
# https://onlinelibrary.wiley.com/doi/10.1111/gcb.15248

# other dating modeling resource
# https://www.sciencedirect.com/science/article/abs/pii/S1871101420300558

########################################
######## Meng Lu et al Workflow ########

# 1. Read in the assembled dataset.
# 2. Convert the variables to units of Mg ha-1 for biomass, Mg C ha-1 for soil carbon stock and Mg C ha-1 yr-1 for sequestration rate.

########################################

## Prepare Workspace ####

library(tidyverse)
library(readxl)

# source synthesis
# source("resources/pull_synthesis.R")
source("coastal_wetland_nggi/scripts/nggi_utils.R")

# read in reported values
reported_raw <- read_csv("coastal_wetland_nggi/data/original/NGGI_reported_values_20230818 - literature_values JH edit.csv",
                         na = c("---", "NA", ""))

# read in data from previous NGGI
mengdat <- read_csv("coastal_wetland_nggi/data/original/US-BC-Analysis-1-105_w_geog.csv")

# read in synthesis data
cores <- read_csv("data/CCRCN_cores.csv")
cores <- read_csv("data/CCRCN_cores.csv",
                  guess_max = nrow(cores))

# bib <- getSynthesisData("citations")

# read in climate zones
climate_zones <- read_xlsx("coastal_wetland_nggi/data/original/Climate zones.xlsx") %>%
  mutate(State = recode(State, "Delamare" = "Delaware")) %>%
  rename(admin_division = State, climate_zone = `Climate zone`) %>%
  select(-Color)

# position info 
core_position_cores <-  cores  %>% 
  select(study_id, site_id, core_id, latitude, longitude, country, admin_division)

core_positions_sites  <-  cores  %>% 
  group_by(study_id, site_id) %>% 
  summarise(latitude = mean(latitude, na.rm=T),
            longitude = mean(longitude, na.rm=T),
            country = first(country),
            admin_division = first(admin_division))

core_positions_studies <- cores  %>% 
  group_by(study_id) %>% 
  summarise(latitude = mean(latitude, na.rm=T),
            longitude = mean(longitude, na.rm=T),
            country = first(country),
            admin_division = first(admin_division))

reported_lat_lon_core <- reported_raw %>% 
  filter(complete.cases(study_id, site_id, core_id)) %>%  
  left_join(core_position_cores)

reported_lat_lon_site <- reported_raw %>% 
  filter(!is.na(site_id)&is.na(core_id)) %>%  
  left_join(core_positions_sites)

reported_lat_lon_study <- reported_raw %>% 
  filter(!is.na(study_id)&is.na(site_id)&is.na(core_id)) %>%  
  left_join(core_positions_studies)

reported_lat_lon <- bind_rows(reported_lat_lon_core,
                              reported_lat_lon_site,
                              reported_lat_lon_study)

missing <- reported_lat_lon %>% 
  filter(is.na(latitude))
# View(missing)


# Spatial join meng data to countries and admin zones



## Standardize Reported Values ####

reported_aligned <- reported_lat_lon %>% 
  # rename core_ids to match library data
  mutate(source = "reported_value", # create a flag to remove unmatched cores after the depthseries data merge
         depth_min_cm = ifelse(!is.na(depth_max_cm) & is.na(depth_min_cm), 0, depth_min_cm))

# check mismatches
mismatch <- anti_join(reported_aligned %>% select(study_id, core_id), cores) 
(mismatch)

## Handle outliers in the reported values table

# 2L-Pb core needs to be averaged across its depth intervals
core_2L_Pb <- reported_aligned %>% 
  # drop_na(core_id) %>% add_count(site_id, core_id) %>% filter(n > 1) %>% 
  filter(core_id == "TB_2L") %>% 
  group_by(study_id, site_id, core_id, habitat, management, accumulation_type, pb210_rate_unit) %>%
  summarise(pb210_rate = mean(as.numeric(pb210_rate)),
            pb210_rate_se = sd(as.numeric(pb210_rate)))

# Drexler_et_al_2013 has pb210_rate as a range - remove and replace with summarized data
reported_drex <- reported_aligned %>% 
  filter(grepl("Drexler_et_al_2013", study_id)) %>% 
  separate(pb210_rate, into = c("pb210_rate_min", "pb210_rate_max"), sep = "-", convert = T) %>% 
  separate(depth_max_cm, into = c("depth_min_cm", "depth_max_cm"), sep = "-", convert = T) %>% 
  mutate(pb210_rate = (as.numeric(pb210_rate_min) + as.numeric(pb210_rate_max))/2) %>% 
  select(-pb210_rate_min, -pb210_rate_max) %>% 
  select_if(~!all(is.na(.)))

# Join climate zone data with core table
core_categories <- cores %>%
  filter(country == "United States") %>% 
  left_join(climate_zones) %>% 
  rename(habitat_ccn = habitat) %>% # so it won't be confused with the reported values table one
  select(study_id, core_id, habitat_ccn, admin_division, climate_zone)

# core_categories %>% filter(study_id %in% unique(mismatch$study_id)) %>% 
  # distinct(study_id, climate_zone) %>% arrange(climate_zone)

# Create the finalized reported 
reported_clean <- reported_aligned %>% 
  filter(study_id != "Drexler_et_al_2013") %>%
  filter(is.na(core_id) | core_id != "TB_2L") %>%
  # Peck 2020 has an SE value of "z" and Gerlach marker rate is a note
  mutate(across(c(carbon_stock, depth_min_cm, depth_max_cm, pb210_rate, pb210_rate_se, marker_rate), 
                as.numeric)) %>% 
  bind_rows(reported_drex) %>% 
  bind_rows(core_2L_Pb) %>% 
  left_join(core_categories) %>% 
  
  # patch in some climate zones for studies that only had site-level rates
  mutate(climate_zone = case_when(study_id %in% c("Poppe_et_al_2019", "Poppe_and_Rybczyk_2019", "Thom_1992",
                                                  "Peck_et_al_2020", "McTigue_et_al_2020", "Krauss_et_al_2018",
                                                  "Drexler_et_al_2019", "Drexler_et_al_2013", "Boyd_2012", "Boyd_and_Sommerfield_2016") ~ "Warm Temperate",
                                  study_id %in% c("Breithaupt_et_al_2014", "Abbott_et_al_2019", "Piazza_et_al_2021") ~ "Subtropical",
                                  study_id %in% c("Drexler_et_al_2009", "Watson_and_Byrne_2013") ~ "Mediterranean",
                                  study_id == "Luk_et_al_2020" ~ "Cold Temperate",
                                  T ~ climate_zone)) %>% 
  
  mutate(inventory_year = "nggi_2022") %>% 
  
  mutate(ecosystem = case_when(
    habitat == "marsh" ~ "Estuarine Emergent Wetland",
    grepl("scrub|shrub", habitat) ~ "Estuarine Scrub/Shrub Wetland",
    habitat == "swamp" ~ "Palustrine Forested Wetland",
    habitat == "mangrove" ~ "Estuarine Forested Wetland",
    habitat == "seagrass" ~ "Estuarine Aquatic Bed",
    T ~ NA_character_)
  ) %>% 
  
  mutate(management = case_when(study_id %in% c("Giblin_and_Forbrich_2018", "Weston_et_al_2020") ~ "natural",
                                T ~ management)) %>% 
  # separate(carbon_stock, into = c("carbon_stock", "carbon_stock_se"), sep = " ") %>%
  
  # when CRS and CIC Pb210 rates are available, replace with their average
  mutate(pb210_rate = ifelse(!is.na(pb210_rate_cic & pb210_rate), (pb210_rate_cic + pb210_rate)/2,
                             ifelse(!is.na(pb210_rate_cic), pb210_rate_cic, pb210_rate)),
         pb210_rate_se = ifelse(!is.na(pb210_rate_cic_se & pb210_rate_se), (pb210_rate_cic_se + pb210_rate_se)/2,
                                ifelse(!is.na(pb210_rate_cic_se), pb210_rate_cic_se, pb210_rate_se)),
         pb210_rate_unit = ifelse(is.na(pb210_rate), NA, pb210_rate_unit),
         cs137_rate_unit = ifelse(is.na(cs137_rate), NA, cs137_rate_unit)
         ) %>% 
  arrange(study_id, site_id, core_id)
  

## Convert accretion rates to CAR
reported_convert <- accretionToCAR(reported_clean)

# studies with sediment accretion
# 1 Allen_et_al_2022         
# 2 Boyd_2012                
# 3 Boyd_and_Sommerfield_2016 # removed
# 4 Boyd_et_al_2017          
# 5 Drexler_et_al_2009  # radiocarbon rate
# 6 Gerlach_et_al_2017  # not included 
# 7 Lagomasino_et_al_2020    
# 8 Luk_et_al_2020           
# 9 Smith_et_al_2015         
# 10 Thom_1992                
# 11 Weis_et_al_2001          
# 12 Vaughn_et_al_2020 

# Baustian is missing rate data?

# reported_final <- reported_convert %>% 
#   # convert accumulation rates to gC ha-1 yr-1 
#   # what to do about cm/yr or mm/yr sediment accumulation rate? Leave out for now
#   mutate(pb210_rate_gChayr = case_when(pb210_rate_unit == "gramsPerSquareCentimeterPerYear" ~ pb210_rate*100,
#                                # pb210_rate_unit == "kilogramsPerSquareMeterPerYear" ~ pb210_rate*10, # doesn't occur anymore
#                                pb210_rate_unit == "gramsPerSquareMeterPerYear" ~ pb210_rate/100,
#                                pb210_CAR_unit == "gramsPerSquareMeterPerYear" ~ pb210_CAR/100,
#                                T ~ NA),
#          cs137_rate_gChaya = case_when(cs137_rate_unit == "gramsPerSquareCentimeterPerYear" ~ cs137_rate*100,
#                                # cs137_rate_unit == "kilogramsPerSquareMeterPerYear" ~ cs137_rate*10,
#                                cs137_rate_unit == "gramsPerSquareMeterPerYear" ~ cs137_rate/100,
#                                cs137_CAR_unit == "gramsPerSquareMeterPerYear" ~ cs137_CAR/100,
#                                T ~ NA)
#   )


## Join Meng Data ####

# Prepare Meng's 2017 synthesis data for merge with reported values
meng <- mengdat %>% 
  rename(
    #latitude = Latitude,
     #    longitude = Longitude,
         habitat = Ecosystem,
         # ecosystem = CCAP_Class,
         management = Management,
         climate_zone = Climate_Zone,
         # carbon_stock = SOC1, 
         # carbon_stock_unit = SOC1units,
         depth_max_cm = SOC1depth, 
         salinity_class = Salinity_Regime, 
         pb210_CAR = delSOC1Pb, 
         pb210_CAR_unit = delSOC1units,
         cs137_CAR = delSOC2Cs,
         cs137_CAR_unit = delSOC2units,
         marker_rate = delSOC3Marker,
         marker_rate_unit = delSOC3units,
         SET_rate = delSOC4SET,
         SET_rate_unit = delSOC4units,
         radiocarbon_rate = delSOC5RadioC,
         radiocarbon_rate_unit = delSOC5units) %>% 
  mutate(inventory_year = "nggi_2017",
         study_id = paste(First_Author, Year, sep = "_"),
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
         
         # if Ecosystem='marsh' then Ecosystem='Estuarine_Emergent_Wetlands';
         # if Ecosystem='mangrove' and Stature='shrub' then Ecosystem='Estuarine_Emergent_Wetlands';
         # if Ecosystem='mangrove' and Stature ne 'shrub' then Ecosystem='Estuarine_Forested_Wetlands';
         # if Ecosystem='tidal_fresh_marsh' then Ecosystem='Palustrine_Emergent_Wetland';
         # if Ecosystem='tidal_fresh_forest' then Ecosystem='Palustrine_Forested_Wetland';
         ecosystem = case_when(habitat == "marsh" ~ "Estuarine Emergent Wetland",
                               habitat == "mangrove" & Stature == "shrub" ~ "Estuarine Emergent Wetland",
                               habitat == "mangrove" & Stature == "tree" ~ "Estuarine Forested Wetland",
                               habitat == "mangrove" & is.na(Stature) ~ "Estuarine Forested Wetland",
                               habitat == "tidal_fresh_marsh" ~ "Palustrine Emergent Wetland",
                               habitat == "tidal_fresh_forest" ~ "Palustrine Forested Wetland",
                               TRUE ~ NA), 
         climate_zone = recode(climate_zone,
                         "temperate_cold" = "Cold Temperate",
                         "temperate_warm" = "Warm Temperate"),
         pb210_CAR_unit = recode(pb210_CAR_unit, "gC_m2" = "gramsPerSquareMeterPerYear"),
         cs137_CAR_unit = recode(cs137_CAR_unit, "gC_m2" = "gramsPerSquareMeterPerYear"),
         carbon_stock  = case_when(SOC1units == "OCg_cc" ~ SOC1 * 10000, # need to check these conversions
                                   SOC1units == "gC_m2" ~ SOC1 / 100, 
                                   SOC1units == "MgC_ha" ~ SOC1,
                                   T ~ NA),
         carbon_stock_unit = ifelse(!is.na(carbon_stock), "megagramsPerHectare", NA),
         depth_min_cm = ifelse(!is.na(depth_max_cm), 0, NA)) %>% 
  # recode management
  mutate(management = recode(management, 
                             "N" = "natural",
                             "R" = "restored",
                             "D" = "disturbed",
                             "M" = "impounded")) %>% 
  select_if(~!all(is.na(.))) %>% 
  filter(! study_id %in% c("Callaway_2012", "Craft_2007", "Crooks_2014", "Noe_2016", "Johnson_2007"))


# bind all 
reported_all <- bind_rows(reported_convert, meng) %>% 
  mutate(climate_zone = str_to_title(climate_zone)) %>% 
  select(names(reported_convert), everything()) %>% 
  select(-c(pb210_rate, pb210_rate_se, pb210_rate_unit, pb210_rate_cic, pb210_rate_cic_se,
            cs137_rate, cs137_rate_se, cs137_rate_unit, dry_bulk_density, fraction_organic_matter,
            fraction_carbon, reported_rates, notes, publication_table, contains("stock"), SOC1, SOC1units,
            Litter, BD, contains("Species"), contains("AGB"), contains("BGB"))) %>% 
  filter_at(vars(pb210_CAR, pb210_CAR_unit, cs137_CAR, cs137_CAR_unit), any_vars(!is.na(.))) %>% 
  select_if(~!all(is.na(.))) %>% 
  arrange(study_id)

write_csv(reported_all, "coastal_wetland_nggi/data/derived/lit_review_processed_all.csv")

# issues: 
# Boyd 2012, core NCGB1
# Lagomasino 2020, HG3, PP1, PP3

# test <- reported_all %>%
#   filter(inventory_year == "2022") %>%
#   filter(management == "natural") %>%
#   filter(habitat != "mudflat") %>%
#   mutate(core_count = ifelse(is.na(core_count), 1, core_count))
# sum(test$core_count)
# 
# nrow(meng %>% 
#   filter_at(vars(pb210_CAR, pb210_CAR_unit, cs137_CAR, cs137_CAR_unit), any_vars(!is.na(.))) %>% 
#   filter(management == "natural"))

## NGGI Summary Calculations ####

nggi_smry <- reported_all %>% 
  filter(management == "natural") %>% 
  filter(habitat != "mudflat") %>% 
  # filter(is.na(core_count) | core_count == 1) %>% # until we know how to handle a mean from multiple cores
  # convert from gC m-2 yr-1 to gC ha-1 yr-1 and calculate the geometric mean
  mutate(NewdelSOC1Pb = ifelse(pb210_CAR_unit != "centimeterPerYear", log10(pb210_CAR/100), NA),
         NewdelSOC2Cs = ifelse(cs137_CAR_unit != "centimeterPerYear", log10(cs137_CAR/100), NA)) %>% 
  group_by(ecosystem, climate_zone) %>% 
  summarize(MNewdelSOC1Pb = mean(NewdelSOC1Pb, na.rm = T),
            SeNewdelSOC1Pb = se(NewdelSOC1Pb, na.rm = T),
            SdNewdelSOC1Pb = sd(NewdelSOC1Pb, na.rm = T),
            MNewdelSOC2Cs = mean(NewdelSOC2Cs, na.rm = T),
            SeNewdelSOC2Cs = se(NewdelSOC2Cs, na.rm = T),
            SdNewdelSOC2Cs = sd(NewdelSOC2Cs, na.rm = T),
            pb210_n = sum(!is.na(NewdelSOC1Pb)),
            cs137_n = sum(!is.na(NewdelSOC2Cs))) %>% 
  ungroup() %>% 
  mutate(MGeodelSOC1Pb = 10^MNewdelSOC1Pb,
         MGeodelSOC2Cs = 10^MNewdelSOC2Cs,
         # confidence intervals
         LowerCIGeodelSOC1Pb = 10^(MNewdelSOC1Pb - (SeNewdelSOC1Pb*1.96)),
         LowerCIGeodelSOC2Cs = 10^(MNewdelSOC2Cs - (SeNewdelSOC2Cs*1.96)),
         UpperCIGeodelSOC1Pb = 10^(MNewdelSOC1Pb + (SeNewdelSOC1Pb*1.96)),
         UpperCIGeodelSOC2Cs = 10^(MNewdelSOC2Cs + (SeNewdelSOC2Cs*1.96)))


# things to iron out
# propagation of error from values that are already averaged to the site-level
# standardize to depth

## Data Visualization ####

rmarkdown::render(input = "coastal_wetland_nggi/scripts/nggi_2022_datavis.Rmd",
                  output_dir = "coastal_wetland_nggi/docs/")


## Write final NGGI table ####

nggi_smry_clean <- nggi_smry %>% 
  select(ecosystem, climate_zone, contains("Geo"), contains("_n")) %>% 
  select(ecosystem, climate_zone, contains("pb"), contains("cs")) %>% 
  mutate(across(everything(), ~replace_na(.x, NA)))

write_csv(nggi_smry_clean, "coastal_wetland_nggi/data/final/NGGI_2022_CAR.csv")

## Bibliography ####

library(RefManageR)

studies_for_bib <- reported_all %>% 
  filter(management == "natural") %>% 
  filter(habitat != "mudflat") %>% 
  distinct(study_id) %>% pull(study_id)

weston_article <- as.data.frame(GetBibEntryWithDOI("10.1029/2022EF003037")) %>% 
  mutate(study_id = "Weston_et_al_2020",
         bibliography_id = "Weston_et_al_2023_article",
         publication_type = "associated source") %>% 
  remove_rownames()

gib_for_article <- as.data.frame(GetBibEntryWithDOI("10.1002/2017JG004336")) %>% 
  mutate(study_id = "Giblin_and_Forbrich_2018",
         bibliography_id = "Forbrich_et_al_2018_article",
         publication_type = "associated source") %>% 
  remove_rownames()

nggi_bibs <- bib %>% 
  filter(study_id %in% studies_for_bib) %>% 
  mutate(across(everything(), as.character)) %>% 
  bind_rows(weston_article, gib_for_article) %>% 
  arrange(study_id)
# filter(bibtype != "Misc")

write_csv(nggi_bibs, "coastal_wetland_nggi/data/final/NGGI_2022_CAR_bibliography.csv")

## Misc code ----

# turn these into the NGGI classifications
# Ecosystems: Estuarine Emergent Wetlands, Estuarine Forested Wetlands, Palustrine Emergent Wetland, Palustrine Forested Wetland, seagrass 


# studies with split core methods
# Okeefe-Suttles_et_al_2021_Cape
# Luk_et_al_2020
# Breithaupt_et_al_2014
# McTigue_et_al_2020

# No associated article: 
# Messerschmidt_and_Kirwan_2020 - CCN release, has interval sedimentation rate (mm yr-1). Need to derive %OC and calculate CAR
# O'keefe Suttles, other release - CAR needs to be extracted from data releases
# Weston_et_al_2020, CCN release - no accumulation rates, we have to calculate these or ask Nat
# Buffington_et_al_2020 - CCN release, 1 core annual accretion rate given as 4.8mm/yr, Need to derive %OC since %C is modeled total?
# Gonneea_et_al_2018 - other release, depth interval CAR in original 
# Breithaupt_et_al_2020 - CCN release, maybe a rate column? might have to age depth model it

# Need to be added: (Now added??)
# Rodriguez_et_al_2022: https://doi.org/10.1038/s43247-022-00501-x
# Vaughn_et_al_2020: https://doi.org/10.1029/2019GB006334
# Piazza_et_al_2020: https://pubs.usgs.gov/of/2011/1094/OF11-1094.pdf
# Giblin_and_Forbrich_2018: https://agupubs.onlinelibrary.wiley.com/doi/full/10.1002/2017JG004336
# Weston et al 2020: https://doi.org/10.1029/2022EF003037


