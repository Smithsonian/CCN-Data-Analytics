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

# Case: Carbon Accumulation data is present in the data release only but we dropped it during curation
# Revisit data curation and see if there are carbon stock values we can revive

# make sure everything is expressed in Carbon
# keep in mind - depth integration!!

# if only sed accumulation is present, 
# derive fraction carbon and resulting C accumulation rate using Craft relationship (1991) - we can specify more complexity later

# if only vertical accretion is present, get the stocks down to interval

# Perform unit conversions for C accumulation => g C m-2 yr-1

# if CRS and CIC models are used, we might end up averaging them

# Ari dating paper for Pb-210 derived dates
# https://bg.copernicus.org/articles/15/6791/2018/bg-15-6791-2018.html

# Kauffman total ecosystem carbon stocks
# https://onlinelibrary.wiley.com/doi/10.1111/gcb.15248

# other dating modeling resource
# https://www.sciencedirect.com/science/article/abs/pii/S1871101420300558

########################################
######## Meng Lu et al Workflow ########

# 1.1 Read in the assembled dataset.
# 1.2 Convert the variables to units of Mg ha-1 for biomass, Mg C ha-1 for soil carbon stock and Mg C ha-1 yr-1 for sequestration rate.
# 1.3 Calculate log10 value for each variable
# 1.4 Calculate the mean values for log10 values
# 1.5 Calculate the geomean values for each variable (with confidence interval)

########################################

## Prepare Workspace ####

library(tidyverse)
library(readxl)
library(googlesheets4)

# source synthesis
source("resources/refresh_data.R")
source("coastal_wetland_nggi/nggi_utils.R")

# read in reported values
# reported <- read_csv("coastal_wetland_nggi/input_data/NGGI_2022_reported_values - literature_values.csv")
  # mutate(study_id = case_when(study_id %in% c("Krauss_et_al_2018_Flux", "Krauss_et_al_2018_Holocene") ~ "Krauss_et_al_2018", 
  #                             site_id == "US-EDN" ~ "Carlin_et_al_2021", 
  #                             T ~ study_id))

gs4_deauth()
reported <- read_sheet("https://docs.google.com/spreadsheets/d/1quotFXqVVJVOP4tTML7Lrby13JKCohlDSbU0-0Oi9S0/edit#gid=0", 
                      na = c("NA", ""), col_types = "c")

# read in climate zone table
climate_zones <- read_xlsx("coastal_wetland_nggi/input_data/Climate zones.xlsx") %>% 
  mutate(Color = tolower(Color),
         Color = recode(Color, "oragen" = "orange"),
         State = recode(State, "Delamare" = "Delaware")) %>% 
  rename(admin_division = State, climate_zone = `Climate zone`, climate_color = Color)


# Prepare Synthesis Data ####

# isolate US cores with dating information and join climate zones
dated_us_cores <- cores %>% 
  filter(country == "United States") %>% 
  # filter(habitat != "upland") %>% 
  drop_na(dates_qual_code) %>% 
  left_join(climate_zones) %>% 
  mutate(habitat = recode(habitat, "scrub shrub" = "scrub/shrub"),
         vegetation_class = case_when(core_id == "MR3" ~ "marsh",
                                      T ~ vegetation_class)) %>% 
  assignEcosystem() %>% 
  arrange(study_id) %>% 
  select(study_id, site_id, core_id, habitat, vegetation_class, salinity_class, ecosystem, climate_zone, everything())
  # left_join(methods %>% select())
  # left_join(depthseries %>% distinct(study_id, site_id, core_id, method_id)) %>% 
  # left_join(methods %>% select(study_id, method_id, fraction_carbon_type)) %>% 
  # select(study_id, site_id, core_id, method_id, fraction_carbon_type, everything()) %>% 
    # count(study_id, core_id)
  # filter(study_id %in% unique(reported$study_id))

View(dated_us_cores %>% 
  drop_na(ecosystem) %>% 
  distinct(study_id, ecosystem, climate_zone) %>% 
  add_count(study_id) %>% filter(n > 1),
  title = "check_assignments")

# zone_count <- dated_us_cores %>% 
#   count(ecosystem, climate_zone)
              # study_id, site_id, habitat, 
              # vegetation_class, salinity_class, 
              # ecosystem, climate_zone) %>% arrange(ecosystem, climate_zone), title = "")
# turn these into the NGGI classifications
# Ecosystems: Estuarine Emergent Wetlands, Estuarine Forested Wetlands, Palustrine Emergent Wetland, Palustrine Forested Wetland, seagrass 

# View(dated_us_cores %>% select(study_id, site_id, core_id, habitat, vegetation_class, salinity_class))

# studies with split core methods
# Okeefe-Suttles_et_al_2021_Cape
# Luk_et_al_2020
# Breithaupt_et_al_2014
# McTigue_et_al_2020

# find the studies that don't match between the synthesis and the reported table
nomatch <- distinct(dated_us_cores, study_id) %>% 
  anti_join(reported %>% distinct(study_id)) %>% pull(study_id)
# nomatch$study_id

# No associated article: 
  # Messerschmidt_and_Kirwan_2020 - CCN release, has interval sedimentation rate (mm yr-1). Need to derive %OC and calculate CAR
  # O'keefe Suttles, other release - CAR needs to be extracted from data releases
  # Weston_et_al_2020, CCN release - no accumulation rates, we have to calculate these or ask Nat
  # Buffington_et_al_2020 - CCN release, 1 core annual accretion rate given as 4.8mm/yr, Need to derive %OC since %C is modeled total?
  # Gonneea_et_al_2018 - other release, depth interval CAR in original 
  # Breithaupt_et_al_2020 - CCN release, maybe a rate column? might have to age depth model it

# Need to be added: 
  # Rodriguez_et_al_2022: https://doi.org/10.1038/s43247-022-00501-x
  # Vaughn_et_al_2020: https://doi.org/10.1029/2019GB006334
  # Piazza_et_al_2020: https://pubs.usgs.gov/of/2011/1094/OF11-1094.pdf
  # Giblin_and_Forbrich_2018: https://agupubs.onlinelibrary.wiley.com/doi/full/10.1002/2017JG004336
  # Weston et al 2020: https://doi.org/10.1029/2022EF003037

nggi_bibs <- bib %>% 
  filter(study_id %in% unique(dated_us_cores$study_id))
  # filter(bibtype != "Misc")

## Reported Values ####

# investigate table
# lookup <- distinct(reported, study_id, accumulation_type) %>% 
#   full_join(gapfill_ds %>% distinct(study_id, action_flag)) %>% 
#   arrange(study_id)

View(reported %>% distinct(study_id, accumulation_type) %>% 
       filter(accumulation_type %in% c("sediment accretion", "organic matter")), 
      title = "reported_accumulation_type")
# these studies will need fraction organic matter in order to 

# define zones of climate and ecosystem
zones <- dated_us_cores %>% 
  # drop_na(ecosystem) %>% 
  distinct(study_id, climate_zone) %>% 
  arrange(study_id)

## Pb-210 and Cs-137

# create a function will perform unit conversions for C accumulation?

# mass accumulation
# "gramsPerSquareCentimeterPerYear"
# "gramsPerSquareMeterPerYear" 
# "kilogramsPerSquareMeterPerYear"

# depth accumulation
# "millimeterPerYear" => ?
# "centimeterPerYear" =>  ?

# 2L-Pb needs to be averaged 
reported_smry <- reported %>% 
  # drop_na(core_id) %>% add_count(site_id, core_id) %>% filter(n > 1) %>% 
  filter(core_id == "2L-Pb") %>% 
  group_by(study_id, site_id, core_id, habitat, management, accumulation_type, pb210_rate_unit) %>%
  summarise(pb210_rate = mean(as.numeric(pb210_rate)),
            pb210_rate_se = sd(as.numeric(pb210_rate)))

reported_subset <- reported %>% 
  
  # filter(accumulation_type == "organic carbon") %>% # only work with C accumulation for now
  filter(grepl("carbon", accumulation_type)) %>%  # total and organic carbon
  filter(habitat != "mudflat") %>% 
  filter(!grepl("-", pb210_rate)) %>% # Drexler study expressedes C accumulation as a range
  # filter(management == "natural" | is.na(management)) %>% 

  # select necessary cols and convert relevant cols to numeric
  select(contains("id"), core_count, accumulation_type, habitat,
         contains("depth"), contains("pb210"), contains("cs137")) %>% 
  mutate(across(-c(study_id, site_id, core_id, accumulation_type, habitat, pb210_rate_unit, cs137_rate_unit), 
                as.numeric)) %>% 
  
  # add averaged values for disaggregated core
  filter(core_id != "2L-Pb") %>%
  bind_rows(reported_smry) %>% 
  
  # bring the pb210 cic determined rates 
  mutate(pb210_rate = ifelse(!is.na(pb210_rate_cic), pb210_rate_cic, pb210_rate),
         pb210_rate_se = ifelse(!is.na(pb210_rate_cic_se), pb210_rate_cic_se, pb210_rate_se)) %>% 
  select(-pb210_rate_cic, -pb210_rate_cic_se) %>% 
  
  # join climate zones and assign ecoystem types
  left_join(zones) %>% 
  assignEcosystemByHabitat() %>% 
  select(study_id, site_id, core_id, accumulation_type, habitat, ecosystem, climate_zone, everything())
  

standardized <- reported_subset %>% 
  # convert sequestration rates to gC ha-1 yr-1 
  # what to do about cm/yr or mm/yr sediment accumulation rate? Leave out for now
  mutate(pb210_standardized = case_when(pb210_rate_unit == "gramsPerSquareCentimeterPerYear" ~ pb210_rate*100,
                                        pb210_rate_unit == "kilogramsPerSquareMeterPerYear" ~ pb210_rate*10,
                                        pb210_rate_unit == "gramsPerSquareMeterPerYear" ~ pb210_rate/100,
                                        T ~ NA_real_),
         pb210_standardized_se = case_when(pb210_rate_unit == "gramsPerSquareCentimeterPerYear" ~ pb210_rate_se*100,
                                           pb210_rate_unit == "kilogramsPerSquareMeterPerYear" ~ pb210_rate_se*10,
                                           pb210_rate_unit == "gramsPerSquareMeterPerYear" ~ pb210_rate_se/100,
                                           T ~ NA_real_),
         cs137_standardized = case_when(cs137_rate_unit == "gramsPerSquareCentimeterPerYear" ~ cs137_rate*100,
                                        cs137_rate_unit == "kilogramsPerSquareMeterPerYear" ~ cs137_rate*10,
                                        cs137_rate_unit == "gramsPerSquareMeterPerYear" ~ cs137_rate/100,
                                        T ~ NA_real_),
         cs137_standardized_se = case_when(cs137_rate_unit == "gramsPerSquareCentimeterPerYear" ~ cs137_rate_se*100,
                                        cs137_rate_unit == "kilogramsPerSquareMeterPerYear" ~ cs137_rate_se*10,
                                        cs137_rate_unit == "gramsPerSquareMeterPerYear" ~ cs137_rate_se/100,
                                        T ~ NA_real_)) %>% 
  # calculate log10 of the activities in order to determine the geomean
  mutate(log10_pb210 = log10(pb210_standardized),
         log10_pb210_se = log10(pb210_standardized_se),
         log10_cs137 = log10(cs137_standardized),
         log10_cs137_se = log10(cs137_standardized_se))

# accumulation %>% 
#   drop_na(pb210_standardized) %>% 
#   ggplot(aes(pb210_standardized, col = accumulation_type)) +
#   geom_density() +
#   facet_wrap(~accumulation_type, scales = "free", dir = "v")
# 
# accumulation %>%
#   drop_na(pb210_standardized, cs137_standardized) %>%
#   ggplot(aes(cs137_standardized, pb210_standardized, col = accumulation_type)) +
#   geom_point()

# calculate the geometric mean for each accumulation rate: mean = (a1 x a2 x...x an)^(1/n)
geomean_pb210 <- standardized %>% 
  drop_na(log10_pb210) %>% 
  group_by(ecosystem, climate_zone) %>%
  summarise(am = mean(log10_pb210, na.rm = T),
            stdev = sd(log10_pb210, na.rm = T),
            n = n(),
            ci_upper = am + (1.96*stdev)/sqrt(n),
            ci_lower = am - (1.96*stdev)/sqrt(n)
            # gm_test = prod(pb210_standardized)^(1/n)
            ) %>% 
  mutate(gm = 10^am,
         gm_ci_upper = 10^ci_upper,
         gm_ci_lower = 10^ci_lower
  )

ggplot(geomean_pb210, aes(ecosystem, gm, col = climate_zone)) + 
  geom_point() +
  coord_flip()

geomean_cs137 <- standardized %>% 
  drop_na(log10_cs137) %>% 
  group_by(ecosystem, climate_zone) %>%
  summarise(am = mean(log10_cs137, na.rm = T),
            stdev = sd(log10_cs137, na.rm = T),
            n = n(),
            ci_upper = am + (1.96*stdev)/sqrt(n),
            ci_lower = am - (1.96*stdev)/sqrt(n)) %>% 
  mutate(gm = 10^am,
         gm_ci_upper = 10^ci_upper,
         gm_ci_lower = 10^ci_lower
  )

# things to iron out
# total vs. organic carbon sequestration
# propagation of error from values that are already averaged to the site-level
# standardize to depth?

## GapFilling Depthseries ####

gapfill_ds <- depthseries %>% 
  # filter(study_id %in% unique(dated_us_cores$study_id))
  filter(core_id %in% unique(dated_us_cores$core_id)) %>% 
  left_join(dated_us_cores %>% distinct(core_id, habitat)) %>% # add habitat info from cores table
  left_join(methods %>% distinct(study_id, method_id, fraction_carbon_type)) %>%   
  select_if(function(x) {!all(is.na(x))}) %>% 
  # flag rows by the presence/absence of things 
  mutate(action_flag = case_when(!is.na(fraction_carbon) & fraction_carbon_type == "organic carbon" ~ "no action",
                                 !is.na(fraction_carbon) & fraction_carbon_type == "total carbon" ~ "convert TC to OC",
                                 is.na(fraction_carbon) & !is.na(fraction_organic_matter) ~ "convert LOI to C",
                                 T ~ NA_character_)) %>% 
  # gapfill fraction carbon 
  # relationship to convert total carbon to organic?
  # start with Craft 1991 relationship, increase complexity from there
  mutate(soc = case_when(action_flag == "no action" ~ fraction_carbon * 100,
                         action_flag == "convert LOI to C" & habitat == "marsh" ~ 0.4 * (100*fraction_organic_matter) + 0.0025*(100*fraction_organic_matter)^2,
                         action_flag == "convert LOI to C" & habitat == "mangrove" ~ 0.415 * (100*fraction_organic_matter) + 2.89,
                         action_flag == "convert TC to OC" ~ NA_real_,
                         T ~ NA_real_)
         # measured_or_modeled = case_when(study_id %in% c("Buffington_et_al_2020", "Drexler_et_al_2009",
         #                                                     "Keshta_et_al_2020", "Radabaugh_et_al_2018",
         #                                                     "Rodriguez_et_al_2022") ~ "modeled",
         #                                 action_flag == "convert LOI to C" ~ "modeled",
         #                                     T ~ "measured"),
  ) %>%
  drop_na(action_flag) %>% 
  select(study_id, fraction_organic_matter, contains("carbon"), action_flag, soc, everything())

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
  ggplot(aes(fraction_organic_matter * 100, soc, col = action_flag)) +
  geom_point(alpha = 0.5, pch = 1) +
  facet_wrap(~action_flag)

gapfill_ds %>% drop_na(soc) %>% 
  ggplot(aes(soc)) +
  geom_density() + geom_rug()




