## CCN Blue Carbon Data Inventory ####

## contact: Jaxine Wolfe, wolfejax@si.edu
## date: 11-10-2021

# Script calculates state-level habitat representativeness metric

library(tidyverse)
library(foreign)

# Calculate wetland area by state and by state habitat

# Parse CCAP classes into habitats 
ccap_estimated_to_mapped <- read_csv("data/CCAP/2010Classes/CCAP2010DataOutput.csv") %>%
  select(X1, perPixelScaler) %>%
  rename(abbrevs = X1)

ccap_support <- read_csv("data/CCAP/2010Classes/ccapPixelCounts.csv") %>%
  select(-pixels)

ccap_estimated_to_mapped <- ccap_estimated_to_mapped %>%
  left_join(ccap_support) %>%
  rename(ccap_class = classes) %>%
  select(ccap_class, perPixelScaler)

# Identify paths for all data files
state_path <- "data/CCAP_NWI_Tidal_Clips/"
# list data
state_file_list <- c(list.files(path = state_path,
                               # Most of the data files are in their own subdirectories
                               #   so need to be recursive
                               pattern = ".dbf", recursive = TRUE))

# Iterate through states
for (i in 1:length(state_file_list)) {
  # Read in data
  state_file_name <- state_file_list[i]

  state_file <- as_tibble(read.dbf(
    paste(state_path, state_file_name, sep="/"),
    as.is = T
  )) %>%
    # add state as a category
    mutate(state = str_remove_all(state_file_name, "CCAP_2206to2010_AllNwiTidal_And_Est_"),
           state = str_remove_all(state, ".tif.vat.dbf")) %>%
    rename(ccap_class = X2010_Clas) %>%
    select(state, ccap_class, Count) %>%
    group_by(state, ccap_class) %>%
    summarise(Count = sum(Count, na.rm=T)) %>%
    ungroup() %>%
    arrange(-Count)

  # if i == 1 then make this the output table
  if (i == 1) {
    output_states <- state_file
  } else {
    # if not then add it to the output table with row bind
    output_states <- output_states %>%
      bind_rows(state_file)
  }
}

# Run some post processing

# Join CCAP 2010 Pixel scalers
output_states_scaled <- output_states %>%
  left_join(ccap_estimated_to_mapped) %>%
  # Calculate scaled area
  mutate(scaledCount = Count*perPixelScaler)

output_states_scaled_simplified <- output_states_scaled %>%
  # Simplify 2010 classes to
  # marsh
  # EEM and PEM
  mutate(habitat_simplified = ifelse(ccap_class %in% c("Estuarine Emergent Wetland",
                                                       "Palustrine Emergent Wetland"),
                                     "marsh",
                                     NA),
         # scrub/shrub
         # PSS and ESS
         habitat_simplified = ifelse(ccap_class %in% c("Estuarine Scrub/Shrub Wetland",
                                                       "Palustrine Scrub/Shrub Wetland"),
                                     "scrub/shrub",
                                     habitat_simplified),
         # swamp
         # PFO is always swamp
         habitat_simplified = ifelse(ccap_class == "Palustrine Forested Wetland",
                                     "swamp",
                                     habitat_simplified),
         # swamp and mangrove
         # If the state is not LA, MS, AL, or FL then EFO counts as swamp
         # If it is it counts as mangrove
         habitat_simplified = ifelse(ccap_class == "Estuarine Forested Wetland",
                                     ifelse(state %in% c("LA", "MS", "AL", "FL", "GA", "SC", "TX"),
                                            "mangrove",
                                            "swamp"),
                                     habitat_simplified),
         # seagrass, kelp and algal mats
         # EAB and PAB
         habitat_simplified = ifelse(ccap_class %in% c("Estuarine Aquatic Bed", "Palustrine Aquatic Bed"),
                                     "seagrass, kelp and algal mats",
                                     habitat_simplified)
         # all others are NA
         ) %>%

  # filter out NA
  filter(complete.cases(habitat_simplified)) %>%
  # group by state and habitat type
  group_by(state, habitat_simplified) %>%
  # summarise pixel counts
  summarise(scaledCount = sum(scaledCount, na.rm=T)) %>%
  # group by state
  ungroup() %>%
  group_by(state) %>%
  # calculate proportional area
  mutate(state_scaledCount = sum(scaledCount, na.rm=T)) %>%
  ungroup() %>%
  mutate(mapped_proportion = scaledCount/state_scaledCount) %>%
  ungroup() %>%
  arrange(state, -mapped_proportion)

output_states_scaled_habitat_ha <- output_states_scaled_simplified %>%
  mutate(estimated_area_ha = scaledCount*900*0.0001) %>%
  select(state, habitat_simplified, estimated_area_ha)

output_states_scaled_ha <- output_states_scaled_habitat_ha %>%
  group_by(state) %>%
  summarise(estimated_area_ha = sum(estimated_area_ha, na.rm=T))

# Write to file
write_csv(output_states_scaled_habitat_ha, "data/report_data/state_habitat_wetland_area_ccap.csv")
write_csv(output_states_scaled_ha, "data/report_data/state_wetland_area_ccap.csv")

# read in data
# output_states_scaled_habitat_ha <- read_csv("data/derived/ccap_state_wetland_area_habitat.csv")
# output_states_scaled_ha <- read_csv("data/derived/ccap_state_wetland_area.csv")
cores <- read_csv("data/derived/CONUS_cores.csv", guess_max = 7000)

# Simplify habitat categories
cores_habitat_simplified <- cores %>% 
  # If any of: marsh, scrub/shrub, swamp, mangrove, then let the definition stand
  mutate(habitat_simplified = ifelse(habitat %in% c("marsh", "scrub/shrub", "mangrove", "swamp", "scrub shrub"),
                                     habitat,
                                     # If either, seagrass or algal mats, then redefine as seagrass, kelp and algal mats
                                     ifelse(habitat %in% c("seagrass", "algal mat", "EAB"),
                                            "seagrass, kelp and algal mats",
                                            # If other then NA
                                            NA
                                            )),
         state = state.abb[match(state,state.name)]) %>% 
  filter(complete.cases(habitat_simplified)) %>% 
  # Group by state and calculate habitat proportions by core counts
  group_by(state, habitat_simplified) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(state) %>% 
  mutate(state_count = sum(n)) %>% 
  ungroup() %>% 
  mutate(state_core_proportion = n / state_count) %>% 
  arrange(state, -state_core_proportion)

# Run a full join between the mapped proportions and core representativeness
full_mapped_and_core_proprtions <- full_join(output_states_scaled_simplified,
                                             cores_habitat_simplified) %>% 
  select(state, habitat_simplified, mapped_proportion, state_core_proportion)

# Turn NA values for core representativeness into 0's
full_mapped_and_core_proprtions[is.na(full_mapped_and_core_proprtions)] <- 0

write_csv(full_mapped_and_core_proprtions, "data/report_data/mapped_and_cored_habitat_proportions.csv")

state_level_euclidean_distance <- full_mapped_and_core_proprtions %>% 
  # mutate to get the difference for each state
  mutate(exp_minus_obs = mapped_proportion - state_core_proportion) %>% 
  # Group by state
  group_by(state) %>%
  # calculate the sqrt(sum of differences squared), aka euclidean distance
  summarise(euclidean_distance = 1 - sqrt(sum(exp_minus_obs^2))) %>% 
  # arrange by euclidean distance
  arrange(-euclidean_distance)

# Write Csv
write_csv(state_level_euclidean_distance, "data/report_data/habitat_representativeness_metric.csv")
  
# should be higher for states that are out of sync and lower for states more in sync.
ggplot(data = state_level_euclidean_distance, aes(x = reorder(state, euclidean_distance), y = euclidean_distance)) +
  geom_point(pch=16) +
  geom_segment(aes(xend=state, yend=0)) +
  xlab("Top to Bottom; More to Less Represtative") +
  ylab("Euclidean Distance") +
  ggtitle("Habitat Representativeness Metric") +
  coord_flip()

# ggsave("figures/Euclidean Distance Mapped vs Cored Habitats.jpg",
#        width = 3.5,
#        height= 3.5, dpi=300)
