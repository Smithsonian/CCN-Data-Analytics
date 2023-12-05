# Cs peak and historical horizon car

library(tidyverse)

# Quality of life functions
{
  
  match_target_horizons <- function(depthseries,
                                    target_horizons) {
    
    # depthseries_paired_down <- depthseries %>%
    # dplyr::select(study_id, site_id, everything(), -representative_depth_min, -representative_depth_max, -sample_id) %>%
    # dplyr::select(study_id:fraction_carbon) # %>%
    # dplyr::filter(core_id %in% subset_cores()$core_id) %>%
    # dplyr::mutate_at(c("depth_min", "depth_max", "dry_bulk_density", "fraction_carbon", "fraction_organic_matter"),
    #                  as.numeric)
    
    depthseries_horizons <- merge(depthseries, target_horizons) %>%
      # Keeps intervals between min and max horizon
      # If an interval crosses a horizon, it remains
      gather(key = "variable", value = "value", -c(depth_min, depth_max, horizon_min, horizon_max)) %>% 
      filter(complete.cases(value)) %>% 
      dplyr::filter(pmax(depth_min, horizon_min) < pmin(depth_max, horizon_max)) %>%
      dplyr::arrange(depth_min, depth_max) %>%
      # Calculate weights for each interval
      dplyr::mutate(overlapping_depth = pmin((depth_max-depth_min),
                                             (horizon_max-depth_min),
                                             (depth_max-horizon_min), na.rm=T)) %>%
      dplyr::group_by(variable, horizon_min, horizon_max) %>%
      dplyr::mutate(total_depth = sum(overlapping_depth),
                    weight = overlapping_depth / total_depth) %>%
      # Aggregate by horizon intervals
      dplyr::summarise(value=sum(value*weight)) %>% 
      ungroup() %>% 
      spread(key = variable, value = value)
    
    return(depthseries_horizons)
    
  }
  
  habitat_specfic_om_to_oc <- function(om, habitat) {

    oc <- ifelse(habitat == "marsh", om^2 * 0.063454 + om * 0.427391,
                 ifelse(habitat == "seagrass", om * 0.43, # Fourqueen et al., 2012
                        ifelse(habitat %in% c("mangrove", "swamp", "scrub/shrub"),  om^2 * -0.015966 + om * 0.485667, 
                               NA
                        )))
    return(oc)
    
  }
  
}


cs_peaks <- read_csv("carbon_accumulation_rates/output/tabs/peak_137Cs_accretion_rates.csv") %>% 
  filter(sig_level >= 2)

historical_horizons <- read_csv("carbon_accumulation_rates/output/tabs/historical_horizon_accretion_rates.csv")

depths <- read_csv("data/CCN_depthseries.csv")
depths <- read_csv("data/CCN_depthseries.csv", guess_max = nrow(depths), na = c("NA", ".", "no data"))
depths$depth_max <- as.numeric(depths$depth_max)
depths$depth_min <- as.numeric(depths$depth_min)

# cores
cores <- read_csv("data/CCN_cores.csv") %>% 
  filter(!is.na(dates_qual_code)&!is.na(stocks_qual_code)) %>% 
  filter(habitat %in% c("marsh", "mangrove", "seagrass", "swamp")) %>% 
  arrange(study_id, site_id, core_id)

names(cs_peaks)

cs_subset <- cs_peaks %>%
  select(study_id, site_id, core_id, habitat, country, admin_division, year, min_possible_peak_depth, depth_med, max_possible_peak_depth, sig_level, high_accretion, low_accretion) %>% 
  group_by(study_id, site_id, core_id) %>% 
  mutate(horizon_id = paste0("cs137_", 1:n()),
         data_type = "cs137") %>% 
  rename(depth_min = min_possible_peak_depth,
         depth_max = max_possible_peak_depth
         )

historical_horizon_subset <- historical_horizons %>% 
  select(study_id, site_id, core_id, habitat, country, admin_division, year, depth_min, depth_max, high_accretion, low_accretion) %>% 
  group_by(study_id, site_id, core_id) %>% 
  mutate(horizon_id = paste0("hh_", 1:n()),
         depth_med = (depth_min+depth_max)/2,
         data_type = "horizon")

all_together_now <- bind_rows(cs_subset, historical_horizon_subset) %>% 
  rename(horizon_depth_med = depth_med) %>% 
  select(-c(depth_min, depth_max)) %>% 
  left_join(depths, relationship = "many-to-many") %>% 
  select(study_id, site_id, core_id, habitat, country, admin_division, year, data_type, depth_min, 
         depth_max, high_accretion, low_accretion, horizon_id,
         horizon_depth_med, dry_bulk_density, fraction_organic_matter, fraction_carbon) %>% 
  arrange(study_id, site_id, core_id, data_type, depth_min, depth_max, horizon_id)

# gap fill 
all_together_now_gapfilled <- all_together_now %>% 
  mutate(dry_bulk_density = ifelse(dry_bulk_density<=0, NA, dry_bulk_density)) %>% 
  mutate(dry_bulk_density_gap_filled = ifelse(is.na(dry_bulk_density),
                                              1/((fraction_organic_matter/0.093) + ((1-fraction_organic_matter)/1.590)),
                                              dry_bulk_density)) %>% 
  mutate(oc_gap_filled = ifelse(is.na(fraction_carbon),
                                habitat_specfic_om_to_oc(om = fraction_organic_matter,
                                                         habitat = habitat),
                                fraction_carbon))

avg_carbon_per_horizon <- all_together_now_gapfilled %>% 
  filter(complete.cases(dry_bulk_density_gap_filled, oc_gap_filled)) %>% 
  group_by(study_id, site_id, core_id, horizon_id, data_type, habitat, country, admin_division, high_accretion, low_accretion) %>% 
  filter(depth_max <= horizon_depth_med) %>% 
  mutate(c_density = oc_gap_filled*dry_bulk_density_gap_filled)  %>% 
  filter(complete.cases(c_density)) %>% 
  summarise(carbon_density = mean(c_density)) %>% 
  ungroup() %>% 
  mutate(low_CAR = carbon_density*low_accretion*10000, 
            high_CAR = carbon_density*high_accretion*10000,
            carbon_accumulation_mean = (low_CAR+high_CAR)/2,
            carbon_accumulation_upper_CI = qunif(0.975, low_CAR, high_CAR),
            carbon_accumulation_lower_CI = qunif(0.025, low_CAR, high_CAR)) %>% 
  filter(complete.cases(carbon_accumulation_upper_CI, carbon_accumulation_lower_CI))
        
write_csv(avg_carbon_per_horizon, "carbon_accumulation_rates/output/cs_and_horizon_car.csv")

