## Spatial Stats

## Produces the underlying summary of land cover area
## Output is two tables
##  1 - wetland area per state and habitat 
##  2 - wetland area per state

library(tidyverse)
library(foreign)

# Calculate wetland area by state and by state habitat

# Parse CCAP classes into habitats 
ccap_estimated_to_mapped_raw <- read_csv("blue_carbon_data_inventory/data/CCAP/2010Classes/CCAP2010DataOutput.csv") %>%
  select(...1, perPixelScaler) %>%
  rename(abbrevs = ...1)

ccap_support <- read_csv("blue_carbon_data_inventory/data/CCAP/2010Classes/ccapPixelCounts.csv") %>%
  select(-pixels)

ccap_estimated_to_mapped <- ccap_estimated_to_mapped_raw %>%
  left_join(ccap_support) %>%
  rename(ccap_class = classes) %>%
  select(ccap_class, perPixelScaler)

# Identify paths for all data files
state_path <- "database_inventory/data/CCAP_NWI_Tidal_Clips/"
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
  arrange(state, -mapped_proportion) %>% 
  # tacking this column on to consolidate tables
  # output_states_scaled_habitat_ha <- output_states_scaled_simplified %>%
  mutate(estimated_area_ha = scaledCount*900*0.0001)
# select(state, habitat_simplified, estimated_area_ha)

# summarize total wetland area per state (across all blue carbon habitats)
output_states_scaled_ha <- output_states_scaled_simplified %>%
  group_by(state) %>%
  summarise(estimated_area_ha = sum(estimated_area_ha, na.rm = T)) %>% ungroup() %>% 
  mutate(percent_wetland = 100*(estimated_area_ha/sum(estimated_area_ha)))

# Write to file (all three might not be necessary eventually)
write_csv(output_states_scaled_simplified, "blue_carbon_data_inventory/data/wetland_area/state_habitat_wetland_area.csv")
# write_csv(output_states_scaled_habitat_ha, "data/report_data/ccap_state_habitat_wetland_area.csv")
write_csv(output_states_scaled_ha, "blue_carbon_data_inventory/data/wetland_area/state_wetland_area.csv")

## Plot State Wetland Area ####

output_states_scaled_ha %>%
  mutate(percent_wetland = 100*(estimated_area_ha/sum(estimated_area_ha))) %>% 
  # reorder factors to plot in decending order
  mutate(state = fct_reorder(state, percent_wetland)) %>%
  # plot percent total CONUS wetland per state
  ggplot(aes(state, percent_wetland, fill = percent_wetland)) +
  geom_col() +
  # set gradient color palette
  scale_fill_gradient(name = "Percent\nWetland", low = "#b2e2e2", high = "#006d2c") +
  # make sure the text doesnt get cropped
  ylim(0, max(32, na.rm = T) + 2) +
  coord_flip() +
  geom_text(aes(label = paste0(round(percent_wetland, 2), "%")), size = 3, hjust = -0.2) +
  ylab("Proportion of Total Tidal Wetland (%)") + xlab("State") +
  theme_classic()

ggsave("blue_carbon_data_inventory/figures/state_wetland_proportion.jpg", width = 6, height = 6)

