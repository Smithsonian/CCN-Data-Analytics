# Tier III data analysis
# JH 2025-02-13
library(tidyverse)

tier_III_raw <- read_csv("tier_III_efs/data/Global_Mangrove_Marsh_CarbonMeans.csv")

tier_III_means <- tier_III_raw %>%
  filter(country!="Disputed",
         !grepl(";", territory)) %>% 
  dplyr::select(FishId, HYBAS_ID, country, territory, source, mangrove_carbon_MgPerHa, marsh_carbon_MgPerHa) %>% 
  gather(key = "ecosystem", value = "MgPerHa", -c(FishId, HYBAS_ID, country, territory, source)) %>% 
  mutate(ecosystem = str_remove_all(ecosystem, "_carbon_MgPerHa"))
  
tier_III_ns <- tier_III_raw %>%
  filter(country!="Disputed",
         !grepl(";", territory)) %>% 
  dplyr::select(FishId, HYBAS_ID, country, territory, source, mangrove_n, marsh_n) %>% 
  gather(key = "ecosystem", value = "n", -c(FishId, HYBAS_ID, country, territory, source)) %>% 
  mutate(ecosystem = str_remove_all(ecosystem, "_n"))

tier_III_sd <- tier_III_raw %>%
  filter(country!="Disputed",
         !grepl(";", territory)) %>% 
  dplyr::select(FishId, HYBAS_ID, country, territory, source, mangrove_carbon_sd, marsh_carbon_sd) %>% 
  gather(key = "ecosystem", value = "sd", -c(FishId, HYBAS_ID, country, territory, source)) %>% 
  mutate(ecosystem = str_remove_all(ecosystem, "_carbon_sd"))

tier_III_analysed <- tier_III_means %>% 
  left_join(tier_III_ns) %>% 
  left_join(tier_III_sd) %>% 
  filter(n > 0) %>% 
  group_by(territory, ecosystem) %>% 
  mutate(total_n = sum(n),
    weight = n / total_n) %>% 
  summarise(MgPerHa = sum(MgPerHa * weight),
            MgPerHa_sd = sqrt(sum(sd^2 * weight^2)))
  
  
  ungroup() %>% 
  arrange(territory)