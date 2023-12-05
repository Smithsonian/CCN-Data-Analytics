## CCN-NGGI-Analysis
## Authors: Jaxine Wolfe, Henry Betts, Rose Cheney, James Holmquist

## Workflow for standardizing C accumulation values for the 2022 NGGI
## And merging the table with the previous NGGI table from 2017

## Prepare Workspace ####

library(tidyverse)
library(readxl)

# source synthesis
# source("resources/pull_synthesis.R")
source("coastal_wetland_nggi/scripts/nggi_utils.R")

# read in reported values
reported_raw <- read_csv("coastal_wetland_nggi/data/NGGI_reported_values_20230818 - literature_values JH edit.csv",
                         na = c("---", "NA", ""))

# read in data from previous NGGI
mengdat <- read_csv("coastal_wetland_nggi/data/US-BC-Analysis-1-105_w_geog.csv")
# Geography has been assigned to this data in a different script

# read in synthesis data
core_n <- nrow(read_csv("data/CCN_cores.csv"))
cores <- read_csv("data/CCN_cores.csv", guess_max = core_n)

ds_n <- nrow(read_csv("data/CCN_depthseries.csv"))
ds <- read_csv("data/CCN_depthseries.csv", guess_max = ds_n)

# read in climate zones
climate_zones <- read_xlsx("coastal_wetland_nggi/data/Climate zones.xlsx") %>%
  mutate(State = recode(State, "Delamare" = "Delaware")) %>%
  rename(admin_division = State, climate_zone = `Climate zone`) %>%
  select(-Color)

## Prepare the Synthesis Data ####

# Gapfill CCN depthseries data and calculate carbon stock
ds_stocks <- ds %>%
  left_join(cores %>% select(study_id, site_id, core_id, habitat, country)) %>%
  filter(country == "United States") %>% 
  # transfer representative depths to depth min and max column if they are NA
  mutate(depth_min = ifelse(is.na(depth_min) & !is.na(representative_depth_min), 
                            representative_depth_min, depth_min),
         depth_max = ifelse(is.na(depth_max) & !is.na(representative_depth_max), 
                            representative_depth_max, depth_max)) %>% 
  carbonStock(.)

# ggplot(ds_stocks %>% filter(carbon_density < 0.8), aes(carbon_density, habitat)) + geom_boxplot()

# Standardize calculations for specified interval(s)
standard_stocks <- standardizeDepths(ds_stocks)

## Associate Climate Zone #### 

# Leverage synthesis cores to merge geography with reported value data
# This will allow the climate zones to be associated

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
  filter(!is.na(site_id) & is.na(core_id)) %>%  
  left_join(core_positions_sites)

reported_lat_lon_study <- reported_raw %>% 
  filter(!is.na(study_id)&is.na(site_id)&is.na(core_id)) %>%  
  left_join(core_positions_studies)

# reconstruct reported value table with assigned geography
reported_lat_lon <- bind_rows(reported_lat_lon_core,
                              reported_lat_lon_site,
                              reported_lat_lon_study)

missing <- reported_lat_lon %>% filter(is.na(latitude))
# View(missing)

## Standardize Reported Values ####

reported_aligned <- reported_lat_lon %>% 
  # set depth min to 0
  mutate(depth_min_cm = ifelse(!is.na(depth_max_cm) & is.na(depth_min_cm), 0, depth_min_cm))

# check mismatches
# mismatch <- anti_join(reported_aligned %>% select(study_id, core_id), cores) 
# (mismatch)

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

# Create the finalized reported 
reported_clean <- reported_aligned %>% 
  # Drexler 2013 is going to replaced by reported_drex, and Gerlach is one marker rate from a disturbed site so we'll leave it out
  filter(!(study_id %in% c("Drexler_et_al_2013", "Gerlach_et_al_2017"))) %>%
  filter(is.na(core_id) | core_id != "TB_2L") %>%
  mutate(across(c(carbon_stock, depth_min_cm, depth_max_cm, pb210_rate, pb210_rate_se, marker_rate), as.numeric)) %>% 
  bind_rows(reported_drex) %>% 
  bind_rows(core_2L_Pb) %>% 
  left_join(climate_zones) %>% 
  
  mutate(inventory_year = "nggi_2022",
         # correction to Kemp rate type
         accumulation_type = ifelse(study_id == "Kemp_et_al_2020", "sediment accretion", accumulation_type)) %>% 
  
  # patch in some climate zones for studies that only had site-level rates
  mutate(climate_zone = case_when(study_id %in% c("Thom_1992", "McTigue_et_al_2020") ~ "Warm Temperate",
                                  study_id == "Drexler_et_al_2009" ~ "Mediterranean",
                                  T ~ climate_zone)) %>%
  
  # small habitat assignment fixes from the latest database update
  mutate(habitat = case_when(core_id == "ELM1812-MFA1" ~ "mudflat",
                             core_id %in% c("BACHI", "FW", "TT") ~ "swamp",
                             core_id %in% c("St_Augustine_Mangrove", "Waccasassa_Bay_Mangrove") ~ "scrub/shrub",
                             T ~ habitat)) %>% 
  
  mutate(ecosystem = case_when(
    habitat == "marsh" ~ "Estuarine Emergent Wetland",
    grepl("scrub|shrub", habitat) ~ "Estuarine Scrub/Shrub Wetland",
    habitat == "swamp" ~ "Palustrine Forested Wetland",
    habitat == "mangrove" ~ "Estuarine Forested Wetland",
    habitat == "seagrass" ~ "Estuarine Aquatic Bed",
    T ~ NA_character_)
  ) %>% 
  
  mutate(management = case_when(study_id %in% c("Giblin_and_Forbrich_2018", "Weston_et_al_2023") ~ "natural",
                                T ~ management)) %>% 
  
  # when CRS and CIC Pb210 rates are available, replace with their average
  mutate(pb210_rate = ifelse(!is.na(pb210_rate_cic & pb210_rate), (pb210_rate_cic + pb210_rate)/2,
                             ifelse(!is.na(pb210_rate_cic), pb210_rate_cic, pb210_rate)),
         pb210_rate_se = ifelse(!is.na(pb210_rate_cic_se & pb210_rate_se), (pb210_rate_cic_se + pb210_rate_se)/2,
                                ifelse(!is.na(pb210_rate_cic_se), pb210_rate_cic_se, pb210_rate_se)),
         pb210_rate_unit = ifelse(is.na(pb210_rate), NA, pb210_rate_unit),
         cs137_rate_unit = ifelse(is.na(cs137_rate), NA, cs137_rate_unit)
         ) %>% 
  
  # finally, join the stocks data derived from the CCN depthseries
  left_join(standard_stocks %>% select(-site_id)) %>% 
  arrange(study_id, site_id, core_id) 

## Convert accretion rates to CAR and standardize units
reported_convert <- standardizeCAR(reported_clean)

# # studies with sediment accretion (no CAR provided)
# 1 Allen_et_al_2022         
# 2 Boyd_2012                
# 3 Boyd_and_Sommerfield_2016
# 4 Boyd_et_al_2017          
# 5 Drexler_et_al_2009       
# 6 Kemp_et_al_2020          
# 7 Lagomasino_et_al_2020    
# 8 Luk_et_al_2020           
# 9 Smith_et_al_2015         
# 10 Thom_1992                
# 11 Vaughn_et_al_2020        
# 12 Weis_et_al_2001  

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
  # using the CCN data for these studies instead
  filter(!study_id %in% c("Callaway_2012", "Craft_2007", "Crooks_2014", "Noe_2016", "Johnson_2007"))


# bind previous and new reported values
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

write_csv(reported_all, "coastal_wetland_nggi/report/lit_review_processed_all.csv")

## NGGI Summary Calculations ####

# Calculate emissions factors for all vegetated tidal wetlands that fall under "natural" management
nggi_smry <- reported_all %>% 
  filter(management == "natural") %>% 
  filter(habitat != "mudflat") %>% 
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

## Write final NGGI table ####

# clean up the emissions factors table for output
nggi_smry_clean <- nggi_smry %>% 
  select(ecosystem, climate_zone, contains("Geo"), contains("_n")) %>% 
  select(ecosystem, climate_zone, contains("pb"), contains("cs")) %>% 
  arrange(ecosystem, climate_zone) %>% 
  mutate(across(everything(), ~replace_na(.x, NA)))

write_csv(nggi_smry_clean, "coastal_wetland_nggi/report/NGGI_2022_CAR.csv")

## Bibliography ####

# create bibliography for data used to update emissions factors
bib <- read_csv("data/CCN_study_citations.csv")

# library(RefManageR)

# isolate studies from synthesis bib
studies_for_bib <- reported_all %>% 
  filter(management == "natural") %>% 
  filter(habitat != "mudflat") %>% 
  distinct(study_id) %>% pull(study_id)

# filter synthesis study citations by relevant studies
nggi_bibs <- bib %>% 
  filter(study_id %in% studies_for_bib) %>% 
  mutate(across(everything(), as.character)) %>% 
  arrange(study_id)

# write bib to report folder
write_csv(nggi_bibs, "coastal_wetland_nggi/report/NGGI_2022_CAR_bibliography.csv")

## Data Visualization ####

rmarkdown::render(input = "coastal_wetland_nggi/scripts/NGGI_2022_report.Rmd",
                  output_dir = "coastal_wetland_nggi/report")
