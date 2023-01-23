## Carbon Stock Calculations

library(tidyverse)

mang_cores <- read_csv("global/data/CCRCN_global_mangrove/CCRCN_depthseries.csv", guess_max = 8000)
mang_cores_stand <- read_csv("global/data/CCRCN_global_mangrove/CCRCN_standardized_depthseries.csv", guess_max = 8000) 

# original depthseries
ds <- mang_cores %>% 
  filter(depth_max <= 100) %>% 
  mutate(carbon_method = ifelse(is.na(fraction_carbon), "modeled", "measured"),
       # gapfill carbon values using relationship in Blue Carbon Manual
       # Palau (Kaufmann et al. 2011)
       pct_carbon_gf = case_when(!is.na(fraction_carbon) ~ fraction_carbon * 100,
                                 is.na(fraction_carbon) & !is.na(fraction_organic_matter) ~ 0.415 * (fraction_organic_matter * 100) + 2.89))
# alternative: Ouyang and Lee mangrove relationship is 0.21 * %LOI^1.12
# however this undershot the measured values

ds %>% drop_na(fraction_organic_matter, pct_carbon_gf) %>% 
  # filter(carbon_method == "measured") %>% 
  ggplot(aes(fraction_organic_matter * 100, pct_carbon_gf, col = carbon_method)) + 
  geom_point(pch = 1) 
# facet_wrap(~study_id)

ds %>% drop_na(dry_bulk_density, pct_carbon_gf) %>% 
  ggplot(aes(dry_bulk_density, pct_carbon_gf, col = carbon_method)) + 
  geom_point(pch = 1) 

# depth-integrated datas
stand_ds <- mang_cores_stand %>% 
  filter(horizon_max <= 200) %>% 
  mutate(carbon_method = ifelse(is.na(fraction_carbon), "modeled", "measured"),
         # gapfill carbon values using relationship in Blue Carbon Manual
         # Palau (Kaufmann et al. 2011)
         pct_carbon_gf = case_when(!is.na(fraction_carbon) ~ fraction_carbon * 100,
                                        is.na(fraction_carbon) & !is.na(fraction_organic_matter) ~ 0.415 * (fraction_organic_matter * 100) + 2.89),
         soc = (dry_bulk_density * pct_carbon_gf)/100) # check this calculation

# plot, relationship seems to stray on the low carbon end 
ggplot(stand_ds, aes(fraction_organic_matter * 100, pct_carbon_gf, col = carbon_method)) + 
  geom_point()

stand_ds %>% drop_na(dry_bulk_density, pct_carbon_gf) %>% 
  ggplot(aes(dry_bulk_density, pct_carbon_gf, col = carbon_method)) + 
  geom_point(pch = 1) 

# comparison to Rovai et al 
# SOC units in mg cm-3
# consideration, mangrove soils are deeper than the top meter