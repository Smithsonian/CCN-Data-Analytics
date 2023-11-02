# 14C dates

library(tidyverse)

# Load in core data
depths <- read_csv("data/CCRCN_depthseries.csv")
depths <- read_csv("data/CCRCN_depthseries.csv", guess_max = nrow(depths))
cores <- read_csv("data/CCRCN_cores.csv")
cores <- read_csv("data/CCRCN_cores.csv",
                  guess_max = nrow(cores))

depths$depth_max <- as.numeric(depths$depth_max)
depths$depth_min <- as.numeric(depths$depth_min)

carbonate_list <- c("Mya arenaria", "Macoma balthica", 
                    "shell", "Mollusc")

depths_c14 <- depths %>% 
  filter(!is.na(c14_age)) %>% 
  left_join(cores) %>% 
  select(study_id:core_id, depth_min:depth_max, c14_age, c14_age_se, c14_material) %>% 
  arrange(study_id, core_id, depth_min) %>% 
  mutate(depth_max = ifelse(is.na(depth_max), depth_min, depth_max),
         depth_med = (depth_min+depth_max)/2,
         carbonate_or_organic = ifelse(c14_material %in% carbonate_list, "carbonate", "organic")) %>% 
  filter(c14_age > 0) %>% 
  arrange(study_id, site_id, core_id, depth_min, depth_max)

write_csv(depths_c14, "carbon_accumulation_rates/output/tabs/c14_ages.csv")

