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
      grepl("scrub|scrub", habitat) ~ "Estuarine Scrub/Shrub Wetland",
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


