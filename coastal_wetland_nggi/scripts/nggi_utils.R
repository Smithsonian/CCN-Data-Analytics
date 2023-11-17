## Coastal Wetland NGGI ####

## Utility functions for updating NGGI with new data

# calculate the geometric mean
geometricMean <- function(x, na.rm = TRUE){ 
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

# Simple assignment of habitat to ecosystem classes
assignEcosystemByHabitat <- function(df){
  new_df <- df %>%
    mutate(ecosystem = case_when(
      habitat == "marsh" ~ "Estuarine Emergent Wetland",
      grepl("scrub|shrub", habitat) ~ "Estuarine Scrub/Shrub Wetland",
      habitat == "swamp" ~ "Palustrine Forested Wetland",
      habitat == "mangrove" ~ "Estuarine Forested Wetland",
      habitat == "seagrass" ~ "Estuarine Aquatic Bed",
      T ~ NA_character_)
    )
  return(new_df)
}

# more suited to missing data in the CCN core table, using habitat, veg class, and salinity class
assignEcosystem <- function(df){
  new_df <- df %>% 
    mutate(ecosystem = case_when(
      habitat == "marsh" ~ "Estuarine Emergent Wetland", 
      grepl("scrub|scrub", habitat) ~ "Estuarine Scrub/Shrub Wetland",
      habitat == "swamp" ~ "Palustrine Forested Wetland",
      habitat == "mangrove" ~ "Estuarine Forested Wetland",
      habitat == "seagrass" ~ "Estuarine Aquatic Bed",
      # if habitat is NA
      is.na(habitat) & vegetation_class == "forested" & salinity_class == "fresh" ~ "Palustrine Forested Wetland",
      is.na(habitat) & vegetation_class == "forested to emergent" & salinity_class == "oligohaline" ~ "Palustrine Forested Wetland", 
      T ~ NA_character_)
    )
  return(new_df)
}

## Assign Climate Zone ----

# assignClimateZone <- function(df){
#   source("resources/pull_synthesis.R")
#   cores <- getSynthesisData("cores") %>% select(study_id, site_id, core_id, country, admin_division)
#   
#   # read in climate zone table
#   climate_zones <- read_xlsx("coastal_wetland_nggi/data/original/Climate zones.xlsx") %>% 
#     mutate(State = recode(State, "Delamare" = "Delaware")) %>% 
#     rename(admin_division = State, climate_zone = `Climate zone`) %>% 
#     select(-Color)
# }

# assigned based on the reverse workflow to convert CCAP classes to CCN database structure
# ccap_class %in% c("Estuarine Emergent Wetland",
#                         "Palustrine Emergent Wetland"),
#       "marsh",
#       NA),
# scrub/shrub
# PSS and ESS
# habitat_simplified = ifelse(ccap_class %in% c("Estuarine Scrub/Shrub Wetland",
#                                               "Palustrine Scrub/Shrub Wetland"),
#                             "scrub/shrub",
#                             habitat_simplified),
# # swamp
# # PFO is always swamp
# habitat_simplified = ifelse(ccap_class == "Palustrine Forested Wetland",
#                             "swamp",
#                             habitat_simplified),
# # swamp and mangrove
# # If the state is not LA, MS, AL, or FL then EFO counts as swamp
# # If it is it counts as mangrove
# habitat_simplified = ifelse(ccap_class == "Estuarine Forested Wetland",
#                             ifelse(state %in% c("LA", "MS", "AL", "FL", "GA", "SC", "TX"),
#                                    "mangrove",
#                                    "swamp"),
#                             habitat_simplified),
# # seagrass, kelp and algal mats
# # EAB and PAB
# habitat_simplified = ifelse(ccap_class %in% c("Estuarine Aquatic Bed", "Palustrine Aquatic Bed"),
#                             "seagrass, kelp and algal mats",
#                             habitat_simplified)

## Utility functions for carbon stock calculations

# LOItoC <- function(df){
#   if("habitat" %in% names(df)){
#     gf_ds <- df %>%
#       # gapfill carbon values using relationship developed by Jim for the synthesis
#       mutate(fraction_carbon = case_when(
#         is.na(fraction_carbon) & habitat == "marsh" ~ 0.427 * fraction_organic_matter + 0.0635 * (fraction_organic_matter^2),
#         is.na(fraction_carbon) & habitat == "mangrove" ~ 0.486 * fraction_organic_matter - 0.016 * (fraction_organic_matter^2),
#         T ~ fraction_carbon))
#     
#     return(gf_ds)
#     
#   } else {
#     print("Data table must have a habitat column.")
#   }
# }


## Predict DBD from LOI ----

predict_dbd_from_loi <- function(fom, k1 = 0.098, k2 = 1.67) {
  return(1/((fom/k1) + ((1-fom)/k2)))
}
# plot(seq(0,1, by=0.01), predict_dbd_from_loi(seq(0,1, by=0.01)))

## Predict OM from C ----

# for mangrove
predict_om_from_c_mangrove <- function(fraction_carbon, a=0.485667, b=-0.015966) {
  return((-b + sqrt((b^2) - 4*a*(0-fraction_carbon))) / (2*a))
}

# for marsh
predict_om_from_c_marsh <- function(fraction_carbon, b=0.427391, a=0.063454) {
  return((-b + sqrt((b^2) - 4*a*(0-fraction_carbon))) / (2*a))
}

# Gapfill and Calculate C Stock ----

# need to integrate seagrass (Howard 2014)
# For seagrass soils with % lOI < 0.20 % OC = -0.21 + 0.40 (% lOI); 
# For seagrass soils with % lOI > 0.20 % OC = -0.33 + 0.43 (% lOI)

carbonStock <- function(df){
  
  if("habitat" %in% names(df)){
    
    gf_ds <- df %>%
      
      # Case: LOI but no C
      # gapfill carbon values using relationship developed by Jim for the synthesis
      mutate(fraction_carbon = case_when(
        is.na(fraction_carbon) & habitat == "marsh" ~ 0.427 * fraction_organic_matter + 0.0635 * (fraction_organic_matter^2),
        is.na(fraction_carbon) & habitat %in% c("mangrove", "swamp, scrub/shrub") ~ 0.486 * fraction_organic_matter - 0.016 * (fraction_organic_matter^2),
        # need to double check these two...turning fraction carbon negative 
        # is.na(fraction_carbon) & habitat == "seagrass" & fraction_organic_matter < 0.002 ~ -0.0021 + 0.40 * fraction_organic_matter,
        # is.na(fraction_carbon) & habitat == "seagrass" & fraction_organic_matter > 0.002 ~ -0.0033 + 0.43 * fraction_organic_matter,
        T ~ fraction_carbon)) %>% 
      
      # Case: no DBD but has C
      mutate(fraction_organic_matter = case_when(
        is.na(dry_bulk_density) & !is.na(fraction_carbon) & habitat == "marsh" ~ predict_om_from_c_marsh(fraction_carbon),
        is.na(dry_bulk_density) & !is.na(fraction_carbon) & habitat %in% c("mangrove", "swamp, scrub/shrub") ~ predict_om_from_c_mangrove(fraction_carbon),
        T ~ fraction_organic_matter)) %>% 
      
      # Case: calculate DBD using LOI (preexisting and gapfilled)
      mutate(dry_bulk_density = case_when(
        is.na(dry_bulk_density) & !is.na(fraction_organic_matter) ~ predict_dbd_from_loi(fraction_organic_matter),
        T ~ dry_bulk_density)) %>% 
      
      # calculate carbon density and interval stocks
      mutate(carbon_density = dry_bulk_density * fraction_carbon,
             # units: g/cm3 * cm * 10000cm2/m2 => gC m-2
             stock_gCm2 = carbon_density * (depth_max - depth_min) * 10000
             # stock_MgHa = stock_gCm2 * (10^4/10^6)
      )
    return(gf_ds)
    
  } else {
    print("Data table must have a habitat column.")
  }
}

## Standardize depth intervals ----
# what standard depth intervals will give the highest resolution C stock?
# do we standardize the depth intervals before or after calculating cstock for each interval?

# horizons <- data.frame(horizon_min = c(0,100),
#                        horizon_max = c(100,200))
# top meter
# horizons <- data.frame(horizon_min = 0, horizon_max = 100)
# 
standardizeDepths <- function(df){
  
  # determine max depths of each core
  # the target intervals are assumed to be the full length of the core
  # might need to adjust this assumption in the near future
  target_intervals <- df %>% 
    filter(complete.cases(depth_max)) %>%
    mutate(depth_max = as.numeric(depth_max)) %>% 
    group_by(study_id, core_id) %>% 
    summarise(horizon_max = max(depth_max, na.rm = T)) %>% 
    mutate(horizon_min = 0)
  
  # Note: this function was adapted from Atlas code (written by Michael and/or Jim)
  standard_ds <- df %>%
    # mutate(across(where(cols %in% c("depth_min", "depth_max", "dry_bulk_density", "fraction_organic_matter", "fraction_carbon"))), as.numeric)
    merge(target_intervals) %>%
    # Keeps intervals between min and max horizon
    # If an interval crosses a horizon, it remains
    dplyr::filter(pmax(depth_min, horizon_min) < pmin(depth_max, horizon_max)) %>%
    dplyr::arrange(study_id, site_id, core_id, depth_min, depth_max) %>%
    # Calculate weights for each interval
    dplyr::mutate(overlapping_depth = pmin((depth_max-depth_min),
                                           (horizon_max-depth_min),
                                           (depth_max-horizon_min), na.rm = T)) %>%
    dplyr::group_by(study_id, site_id, core_id, horizon_min, horizon_max) %>%
    dplyr::mutate(total_depth = sum(overlapping_depth, na.rm = T),
                  weight = overlapping_depth / total_depth) %>%
    # Aggregate by horizon intervals
    dplyr::summarise(dry_bulk_density = sum(dry_bulk_density * weight, na.rm = T),
                     fraction_organic_matter = sum(fraction_organic_matter * weight, na.rm = T),
                     fraction_carbon = sum(fraction_carbon * weight, na.rm = T),
                     stock_gCm2 = sum(stock_gCm2 * weight, na.rm = T)) %>% 
    ungroup()
  
  return(standard_ds)
}

# coreStock <- function(df){
#   corestock <- df %>%
#     drop_na(dry_bulk_density, fraction_carbon) %>%
#     # filter(core_id %in% unique(cr_cores$core_id)) %>%
#     # select(where(~!all(is.na(.)))) %>%
#     # select_if(function(x) {!all(is.na(x))}) %>%
#     mutate(carbon_density = dry_bulk_density * fraction_carbon,
#            # calculate c stock in each interval (gC m-2)
#            # g/cm3 * 10000cm2/m2
#            cstock_interval = carbon_density * (horizon_max-horizon_min) * 10000)  %>%
#     # cases with fraction carbon but no DBD, drop NA cstock for now
#     # drop_na(cstock_interval) %>%
#     arrange(core_id, horizon_min) %>%
#     group_by(study_id, site_id, core_id) %>%
#     summarize(stock_gCm2 = mean(cstock_interval))
#   # min_depth = min(horizon_min),
#   # max_depth = max(horizon_max),
#   # )
#   
#   return(corestock)
# }

# Convert Soil Accretion to CAR ####

# Provide this function with the reported values table and it will convert accretion rates to carbon accumulation
# Note: conversions still need to be applied after to standardize rates to gCm-2y-1

# Applies the accretion rate to CAR formula:
# Accretion rate (cm/yr) * DBD (g/cc) * fraction_carbon * 10000 (cm2/m2)
accretionToCAR <- function(df){
  
  synthesis_carbon <- read_csv("coastal_wetland_nggi/data/derived/soil_carbon_calculations.csv", col_types = cols())
  
  new_df <- df %>% 
    left_join(synthesis_carbon %>% select(-site_id)) %>% 
    # populate final Pb210-based CAR
    mutate(
      pb210_CAR = case_when(
        pb210_rate_unit == "centimeterPerYear" ~ pb210_rate * dry_bulk_density * fraction_carbon * 10000,
        pb210_rate_unit == "millimeterPerYear" ~ pb210_rate * dry_bulk_density * fraction_carbon * 1000,
        grepl("carbon", accumulation_type) & pb210_rate_unit == "gramsPerSquareCentimeterPerYear" ~ pb210_rate*10000,
        grepl("carbon", accumulation_type) & pb210_rate_unit == "gramsPerSquareMeterPerYear" ~ pb210_rate,
        T ~ pb210_rate),
      pb210_CAR_unit = case_when(
        accumulation_type == "sediment accretion" & !is.na(pb210_CAR) ~ "gramsPerSquareMeterPerYear",
        accumulation_type == "sediment accretion" & !is.na(pb210_CAR) ~ "gramsPerSquareMeterPerYear",
        T ~ pb210_rate_unit)
    ) %>% 
    # populate final Cs137-based CAR
    mutate(
      cs137_CAR = case_when(
        cs137_rate_unit == "centimeterPerYear" ~ cs137_rate * dry_bulk_density * fraction_carbon * 10000,
        cs137_rate_unit == "millimeterPerYear" ~ cs137_rate * dry_bulk_density * fraction_carbon * 1000,
        grepl("carbon", accumulation_type) & cs137_rate_unit == "gramsPerSquareCentimeterPerYear" ~ cs137_rate * 10000,
        grepl("carbon", accumulation_type) & cs137_rate_unit == "gramsPerSquareMeterPerYear" ~ cs137_rate,
        T ~ cs137_rate),
      cs137_CAR_unit = case_when(
        accumulation_type == "sediment accretion" & !is.na(cs137_CAR) ~ "gramsPerSquareMeterPerYear",
        accumulation_type == "sediment accretion" & !is.na(cs137_CAR) ~ "gramsPerSquareMeterPerYear",
        T ~ cs137_rate_unit)
    )
  
  return(new_df)
}

## Standard Error ####

se <- function(x, na.rm=TRUE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}

