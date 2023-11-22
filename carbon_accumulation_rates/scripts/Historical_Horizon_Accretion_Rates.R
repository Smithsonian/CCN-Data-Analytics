# Artificial horizons

library(tidyverse)

# Load in core data
depths <- read_csv("data/CCRCN_depthseries.csv")
depths <- read_csv("data/CCRCN_depthseries.csv", guess_max = nrow(depths))
cores <- read_csv("data/CCRCN_cores.csv")
cores <- read_csv("data/CCRCN_cores.csv",
                  guess_max = nrow(cores))

depths$depth_max <- as.numeric(depths$depth_max)
depths$depth_min <- as.numeric(depths$depth_min)

depths_horizons <- depths %>% 
  filter(!is.na(marker_date)) %>% 
  left_join(cores) %>% 
  select(study_id:core_id, depth_min:depth_max, habitat, marker_type, marker_notes, year, marker_date, marker_date_se) %>% 
  arrange(study_id, core_id, depth_min)

View(depths_horizons)

accretion_rates <- depths_horizons %>% 
  # filter(!is.na(date)) %>% 
  # left_join(cores) %>% 
  # What's the maximum possible accretion rate? 
  # high_accretion = max_depth / (core_year - 1963)
  mutate(high_accretion = depth_max / (year - marker_date),
         same_depth_increments = as.numeric(depth_min == depth_max),
         depth_min = depth_min - same_depth_increments*0.5,
         depth_max = depth_max + same_depth_increments*0.5,
         # What's the minimum possible accretion rate?
         # low_accretion = min_depth / (core_year - 1963)
         low_accretion = depth_min / (year - marker_date),
         # What's the lowest possible age for depth_min
         # min_depth * high_accretion
         age_min = year - depth_min / high_accretion,
         # What's the highest possible age for depth_max
         # max_depth * low_accretion
         age_max = year - depth_max / low_accretion,
         depth_med = (depth_min+depth_max)/2,
         # Treat as a uniform disribution and use moment matching to approximate a normal dist
         # mean = (min+max)/2
         # sd = sqrt((max-min)^2/12)
         age_mean = (age_min+age_max)/2,
         age_sd = sqrt((age_max-age_min)^2/12)
  ) %>% 
  # filter(age_mean != Inf & age_mean != -Inf,
  #        age_sd != Inf & age_sd != -Inf) %>% 
  mutate(age_sd = ifelse(age_sd == 0, 1, age_sd)) %>% 
  filter(is.na(marker_notes) | marker_notes != "137CS (peak)") # This occurs in a different file

write_csv(accretion_rates, "carbon_accumulation_rates/output/tabs/historical_horizon_accretion_rates.csv")

