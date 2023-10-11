## Data Inventorying Functions

## Utility functions for data inventorying
## Generates metrics provided synthesis data

## Source synthesis data

sourceSynthesis <- function(){
  library_url <- "https://raw.githubusercontent.com/Smithsonian/CCRCN-Data-Library/main/data/CCRCN_synthesis/"
  
  ## Core Synthesis ####
  # read in core table from the CCRCN Data Library (with assigned geography)
  cores <- read_csv(paste0(library_url, "CCRCN_cores.csv"), col_types = cols(.default = "c"))
  
  # subset core data for CONUS cores
  coastal_cores <- cores %>% 
    rename(state = admin_division) %>%
    filter(country == "United States") %>%
    filter(state != "Puerto Rico" & state != "Hawaii") %>%
    mutate(habitat = recode(habitat, 
                            "mudflat" = "unvegetated",
                            # we'll be comparing sampled cores to mapped habitat in which seagress, kelp, and algal mats are grouped together
                            "seagrass" = "EAB",
                            "algal mat" = "EAB"))
  
  return(coastal_cores)
}


## Quantity ####

quantityMetric <- function(cores){
  
  wetland_area <- read_csv("database_inventory/data/derived/wetland_area/state_wetland_area.csv")
  
  core_quantity <- cores %>% 
    group_by(state) %>% 
    summarise(n = n()) %>% # calculate number of cores per state
    ungroup() %>% 
    mutate(state = state.abb[match(state,state.name)]) %>% 
    left_join(wetland_area) %>% 
    mutate(cores_per_1000ha = n / estimated_area_ha * 1000) %>% # calculate number of cores per 1000ha of wetland in each states
    arrange(-cores_per_1000ha) %>% 
    mutate(quantity_metric_rank = row.names(.), # rank states by metric
           # calculate weighted metric for each state
           quantity_metric_weight = cores_per_1000ha/sum(cores_per_1000ha))  %>% 
    select(-c(estimated_area_ha, percent_wetland, n)) %>% 
    rename(quantity_metric = cores_per_1000ha)
  
  return(core_quantity)
}

# quantity_v1 <- quantityMetric(cores)

## Quality ####

qualityMetric <- function(cores){
  
  wetland_area <- read_csv("database_inventory/data/derived/wetland_area/state_wetland_area.csv")
  
  core_quality <- cores %>% 
    # count the number of cores per state with good quality data (for stocks, dates, or elevation)
    filter(stocks_qual_code == "C1" | 
             dates_qual_code == "B1" | 
             elevation_qual_code %in% c("A1", "A2")) %>% 
    group_by(state) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    mutate(state = state.abb[match(state,state.name)]) %>% 
    left_join(wetland_area) %>% 
    mutate(cores_per_1000ha = n / estimated_area_ha * 1000) %>% 
    arrange(-cores_per_1000ha) %>% 
    mutate(quality_metric_rank = row.names(.),
           quality_metric_weight = cores_per_1000ha/sum(cores_per_1000ha))  %>% 
    select(-c(estimated_area_ha, percent_wetland, n)) %>% 
    rename(quality_metric = cores_per_1000ha)
  
  return(core_quality)
}

# quality_v1 <- qualityMetric(cores)

## Spatial ####

spatialMetric <- function(cores){
  
  projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  # set cores as simple feature
  cores_sf <- st_as_sf(cores,
                       coords = c("longitude", "latitude"),
                       crs = projcrs)
  
  # Common projection for cores
  # equal area projection
  cores_aea <- st_transform(cores_sf,
                            crs = "+proj=aea +ellps=WGS84 +lat_1=29.5 +lat_2=45.5 +lon_0=-96 +x_0=0 +y_0=0")
  
  # Import HUC8 watershed data
  huc8shp <- st_read("database_inventory/data/shapefiles/coastal_HUC8s/HUC8_AllTidalNwiAndNonTidalPlusFarmedBelowMHHWS_ObviousOutliersRemoved.shp",
                     stringsAsFactors = F)
  
  # Common projection for watersheds
  huc8shp_aea <- st_transform(huc8shp,
                              crs = "+proj=aea +ellps=WGS84 +lat_1=29.5 +lat_2=45.5 +lon_0=-96 +x_0=0 +y_0=0")
  
  # Import states
  states <- st_read("database_inventory/data/shapefiles/us_states/states_coastline_boundaries/cb_2017_us_state_500k.shp",
                    stringsAsFactors = F)
  
  # Common projection for states
  states_aea <- st_transform(states,
                             crs = "+proj=aea +ellps=WGS84 +lat_1=29.5 +lat_2=45.5 +lon_0=-96 +x_0=0 +y_0=0")
  
  # Calculate centroid of watersheds
  huc8shp_aea_centroid <- huc8shp_aea %>% 
    st_centroid() %>% 
    # Join with states
    st_join(states_aea, join = st_nearest_feature)
  
  # ggplot(data = huc8shp_aea_centroid) +
  #   geom_sf(aes(color = STUSPS))
  
  # Which states are involved in the analysis?
  states_to_analyse <- unique(huc8shp_aea_centroid$NAME)
  
  spatial_rep_by_state <- data.frame(state = states_to_analyse,
                                     mean_core_distance = rep(NA, length(states_to_analyse)),
                                     mean_huc8_distance = rep(NA, length(states_to_analyse)),
                                     spatial_representativeness_metric = rep(NA, length(states_to_analyse)))
  
  # Iterate through each state
  for (i in 1:length(states_to_analyse)) {
    
    # Filter cores by state
    cores_in_state <- cores_aea %>% 
      filter(state == states_to_analyse[i])
    
    # Filter HUC8's by state
    huc8s_in_state <- huc8shp_aea_centroid %>% 
      filter(NAME == states_to_analyse[i])
    
    # Distance matrix for cores
    core_distance_matrix <- st_distance(cores_in_state)
    
    # Mean distance for cores
    mean_core_distance <- mean(core_distance_matrix[upper.tri(core_distance_matrix)])
    
    # Distance matrix for HUC8's
    huc8_distance_matrix <- st_distance(huc8s_in_state)
    
    # Mean distance for HUC8's
    mean_huc8_distance <- mean(huc8_distance_matrix[upper.tri(huc8_distance_matrix)])
    
    # Mean Core Dist. / Mean Wetland Watershed Dist.
    spatial_representativeness_metric <- mean_core_distance / mean_huc8_distance
    
    # Add to table
    spatial_rep_by_state$mean_core_distance[i] <- mean_core_distance
    spatial_rep_by_state$mean_huc8_distance[i] <- mean_huc8_distance 
    spatial_rep_by_state$spatial_representativeness_metric[i] <- spatial_representativeness_metric
    
  }
  
  # Rank and Graph
  spatial_rep_by_state$spatial_representativeness_metric[is.na(spatial_rep_by_state$spatial_representativeness_metric)] <- 0
  
  spatial_metric_table <- spatial_rep_by_state %>% 
    arrange(-spatial_representativeness_metric) %>% 
    mutate(state = state.abb[match(state,state.name)]) %>% 
    # add state-level ranks and weights
    mutate(spatial_metric_rank = row.names(.),
           spatial_metric_weight = spatial_representativeness_metric/sum(spatial_representativeness_metric))
    # arrange(spatial_representativeness_metric)
  
  # cut off for fxn
  return(spatial_metric_table)
}

# spatial_v1 <- spatialMetric(cores)
# 
# # plot spatial ranks
# spatial_rep_by_state %>% 
#   mutate(state = fct_reorder(state, spatial_representativeness_metric)) %>% 
#   ggplot(aes(x = state, y = spatial_representativeness_metric)) +
#   geom_point() +
#   geom_segment(aes(xend=state, yend=0)) +
#   xlab(element_blank()) +
#   ylab("Spatial Representativeness Score") +
#   coord_flip()

# will be included in the quadrat figure
# ggsave("figures/spatial_rep_by_state.jpg", width=3.54, height=3.54)
# ggsave("figures/spatial_rep_by_state.pdf", width=3.54, height=3.54)

# write_csv(spatial_rep_by_state, "data/report_data/spatial_rep_by_state.csv")

## Habitat Mapped Area Estimation ####

# Script calculates state-level habitat representativeness metric

# read in data
# output_states_scaled_habitat_ha <- read_csv("data/derived/ccap_state_wetland_area_habitat.csv")
# output_states_scaled_ha <- read_csv("data/derived/ccap_state_wetland_area.csv")
# cores <- read_csv("data/derived/CONUS_cores.csv", guess_max = 7000)


# Habitat Metric ####

habitatProportions <- function(cores){
  
  output_states_scaled_simplified <- read_csv("database_inventory/data/derived/wetland_area/state_habitat_wetland_area.csv")
  
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
  full_mapped_and_core_proprtions <- full_join(output_states_scaled_simplified, cores_habitat_simplified) %>% 
    select(state, habitat_simplified, mapped_proportion, state_core_proportion)
  
  # Turn NA values for core representativeness into 0's
  full_mapped_and_core_proprtions[is.na(full_mapped_and_core_proprtions)] <- 0
  
  # write_csv(full_mapped_and_core_proprtions, "data/report_data/mapped_and_cored_habitat_proportions.csv")
  
  return(full_mapped_and_core_proprtions)
}
  
# full_mapped_and_core_proprtions <- habitatProportions(cores)

habitatMetric <- function(cores){
  # call function which calculates mapped vs sampled proportions
  full_mapped_and_core_proprtions <- habitatProportions(cores)
  
  state_level_euclidean_distance <- full_mapped_and_core_proprtions %>% 
    # mutate to get the difference for each state
    mutate(exp_minus_obs = mapped_proportion - state_core_proportion) %>% 
    # Group by state
    group_by(state) %>%
    # calculate the sqrt(sum of differences squared), aka euclidean distance
    summarise(euclidean_distance = 1 - sqrt(sum(exp_minus_obs^2))) %>% 
    # arrange by euclidean distance
    arrange(-euclidean_distance) %>% 
    
    # calculate ranks and weights for habitat metric (aka. euclidean dist)
    rename(habitat_metric = euclidean_distance) %>%
    # arrange(-habitat_metric) %>%
    mutate(habitat_metric_rank = row.names(.),
           habitat_metric_weight = habitat_metric / sum(habitat_metric))

  return(state_level_euclidean_distance)  
}

# habitat_v1 <- habitatMetric(cores)
# why is DC and PA showing up?

# Write Csv (will be different for different versions of the synthesis)
# write_csv(state_level_euclidean_distance, "data/report_data/habitat_representativeness_metric.csv")

# should be higher for states that are out of sync and lower for states more in sync.
# ggplot(data = habitat_v1, aes(x = reorder(state, habitat_metric), y = habitat_metric)) +
#   geom_point(pch=16) +
#   geom_segment(aes(xend=state, yend=0)) +
#   xlab("Top to Bottom; More to Less Represtative") +
#   ylab("Euclidean Distance") +
#   ggtitle("Habitat Representativeness Metric") +
#   coord_flip()

# ggsave("figures/Euclidean Distance Mapped vs Cored Habitats.jpg",
#        width = 3.5,
#        height= 3.5, dpi=300)

compositeMetricScore <- function(cores){
  
  # run all metric-calculating functions
  ranked_quantity <- quantityMetric(cores)
  ranked_quality <- qualityMetric(cores)
  ranked_spatial <- spatialMetric(cores)
  ranked_habitat <- habitatMetric(cores)
  
  # Calculate composite score from rankings ####
  join_ranks <- ranked_quantity %>% 
    full_join(ranked_quality) %>% 
    full_join(ranked_spatial) %>% 
    full_join(ranked_habitat) %>% 
    # set the maximum rank
    mutate_at(vars(quantity_metric_rank, quality_metric_rank, spatial_metric_rank, habitat_metric_rank), 
              ~ifelse(is.na(.), 23, .))
  
  # calculate composite score
  rankings <- join_ranks %>% 
    mutate_at(vars(-state), as.numeric) %>% 
    mutate(total_metric_rank = (quantity_metric_rank + quality_metric_rank + spatial_metric_rank + habitat_metric_rank) / 4,
           total_metric_weight = quantity_metric_weight + quality_metric_weight + spatial_metric_weight + habitat_metric_weight) %>% 
    arrange(-total_metric_rank)
  
  # Normalized average metric weight was not used for the final figure
  # mutate(total_metric_weight = total_metric_weight / sum(total_metric_weight)) %>%
  # arrange(-total_metric_weight)
  
  # rankings$total_metric_weight <- rankings$total_metric_weight / sum(rankings$total_metric_weight)
  
  # rankings <- rankings %>%
  #   arrange(-total_metric_weight)
  
  # all_data_weights <- rankings %>% 
  #   select(state, quantity_metric_weight, quality_metric_weight, spatial_metric_weight, habitat_metric_weight, total_metric_weight) %>% 
  #   mutate(state = factor(state, rev(rankings$state))) %>% 
  #   gather(value = "weight", key = "metric", -state) %>% 
  #   mutate(metric = str_remove_all(metric, "_metric_weight")) %>% 
  #   filter(weight > 0)
  
  all_data_ranks <- rankings %>% 
    select(state, quantity_metric_rank, quality_metric_rank, spatial_metric_rank, habitat_metric_rank, total_metric_rank) %>% 
    mutate(state = factor(state, rankings$state)) %>% 
    gather(value = "rank", key = "metric", -state) %>% 
    mutate(metric = str_remove_all(metric, "_metric_rank"),
           metric = str_to_sentence(metric),
           metric = recode(metric,"Spatial" = "Spatial coverage", "Habitat" = "Habitat coverage"),
           metric = factor(metric, c("Total", "Quantity", "Quality", "Spatial coverage", "Habitat coverage")),
           rank = round(rank,0))
  
  final_ranks <- all_data_ranks %>% 
    mutate(rank = ifelse(state %in% c("DC", "PA"), NA, rank)) %>% 
    group_by(metric) %>%
    mutate(min_rank = min(rank, na.rm=T)) %>% 
    ungroup() %>% 
    mutate(normalized_rank = round((rank-min_rank)/(23-min_rank)*6)) %>% 
    select(-min_rank, -rank)
  
  return(final_ranks)
}

