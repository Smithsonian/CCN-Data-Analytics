## Coastal Wetland NGGI ####

## Utility functions for adapting CCN data structures for inventorying lingo

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

LOItoC <- function(df){
  if("habitat" %in% names(df)){
    gf_ds <- df %>%
      # gapfill carbon values using relationship developed by Jim for the synthesis
      mutate(fraction_carbon = case_when(
        is.na(fraction_carbon) & habitat == "marsh" ~ 0.427 * fraction_organic_matter + 0.0635 * (fraction_organic_matter^2),
        is.na(fraction_carbon) & habitat == "mangrove" ~ 0.486 * fraction_organic_matter - 0.016 * (fraction_organic_matter^2),
        T ~ fraction_carbon))
    
    return(gf_ds)
    
  } else {
    print("Data table must have a habitat column.")
  }
}



## Predict DBD from LOI

predict_dbd_from_loi <- function(fom, k1 = 0.098, k2 = 1.67) {
  return(1/((fom/k1) + ((1-fom)/k2)))
}
# plot(seq(0,1, by=0.01), predict_dbd_from_loi(seq(0,1, by=0.01)))

predict_om_from_c_mangrove <- function(fraction_carbon, a=0.485667, b=-0.015966) {
  return((-b + sqrt((b^2) - 4*a*(0-fraction_carbon))) / (2*a))
}

predict_om_from_c_marsh <- function(fraction_carbon, b=0.427391, a=0.063454) {
  return((-b + sqrt((b^2) - 4*a*(0-fraction_carbon))) / (2*a))
}

carbonStock <- function(df){
  
  if("habitat" %in% names(df)){
    
    gf_ds <- df %>%
      
      # Case: LOI but no C
      # gapfill carbon values using relationship developed by Jim for the synthesis
      mutate(fraction_carbon = case_when(
        is.na(fraction_carbon) & habitat == "marsh" ~ 0.427 * fraction_organic_matter + 0.0635 * (fraction_organic_matter^2),
        is.na(fraction_carbon) & habitat == "mangrove" ~ 0.486 * fraction_organic_matter - 0.016 * (fraction_organic_matter^2),
        T ~ fraction_carbon)) %>% 
      
      # Case: no DBD but has C
      mutate(fraction_organic_matter = case_when(
        is.na(dry_bulk_density) & !is.na(fraction_carbon) & habitat == "marsh" ~ predict_om_from_c_marsh(fraction_carbon),
        is.na(dry_bulk_density) & !is.na(fraction_carbon) & habitat == "mangrove" ~ predict_om_from_c_mangrove(fraction_carbon),
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

# Standardize depth intervals ####
# what standard depth intervals will give the highest resolution C stock?
# do we standardize the depth intervals before or after calculating cstock for each interval?

# horizons <- data.frame(horizon_min = c(0,100),
#                        horizon_max = c(100,200))
# top meter
# horizons <- data.frame(horizon_min = 0, horizon_max = 100)
# 
standardizeDepths <- function(df){
  
  # determine max depths of each core
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
    dplyr::mutate(total_depth = sum(overlapping_depth),
                  weight = overlapping_depth / total_depth) %>%
    # Aggregate by horizon intervals
    dplyr::summarise(dry_bulk_density = sum(dry_bulk_density * weight),
                     fraction_organic_matter = sum(fraction_organic_matter * weight),
                     fraction_carbon = sum(fraction_carbon * weight),
                     stock_gCm2 = sum(stock_gCm2 * weight))
  
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