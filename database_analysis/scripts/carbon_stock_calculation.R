## Analytics

library(tidyverse)

# source synthesis
source("shared_resources/refresh_data.R")

coreimpacts <- left_join(cores, impacts)

dsimpacts <- left_join(depthseries, impacts) %>% 
  mutate(impact_class = ifelse(is.na(impact_class), "not specified", impact_class))

# density
dsimpacts %>% 
  drop_na(fraction_carbon) %>% 
  filter(impact_class == "farmed") %>% 
  filter(depth_max <= 100) %>% 
  ggplot(aes(fraction_carbon, col = impact_class)) +
  geom_density() +
  geom_rug() +
  facet_wrap(~impact_class)

# fraction carbon ~ depth with impact class
dsimpacts %>% 
  drop_na(fraction_carbon) %>% 
  filter(depth_max <= 100) %>% 
  filter(impact_class != "not specified") %>% 
  select_if(function(x) {!all(is.na(x))}) %>% 
  # group_by(site_id, impact_class) %>%
  # summarise(mean_carbon = mean(fraction_carbon),
  #           mean_carbon_se = stderr(fraction_carbon))
ggplot(aes(impact_class, fraction_carbon, col = impact_class)) +
  geom_boxplot()

# Carbon stocks

dsmethods <- left_join(depthseries, methods) 

ds_methods_stock <- dsmethods %>% 
  filter(fraction_carbon_type == "organic carbon") %>% 
  drop_na(fraction_carbon, dry_bulk_density, depth_max, depth_min) %>% 
  filter(depth_max <= 100) %>% 
  select(-c(contains("activity"), contains("age"), contains("marker"), contains("_unit"))) %>% 
  select_if(function(x) {!all(is.na(x))}) %>% 
  mutate(OC = fraction_carbon * 100 * dry_bulk_density * 10) %>% # convert to mg C/cm3
  mutate(carbon_stock = ((depth_max - depth_min) * OC) * 10000/1000)

core_stocks <- ds_methods_stock %>% 
  group_by(study_id, site_id, core_id) %>% 
  summarise(core_carbon_stock = sum(carbon_stock),
            carbon_stock_sd = sd(carbon_stock)) %>% 
  left_join(impacts) %>% 
  mutate(impact_class = ifelse(is.na(impact_class), "not specified", impact_class))

core_stocks %>% 
  ggplot(aes(impact_class, core_carbon_stock, col = impact_class)) +
  geom_boxplot()
