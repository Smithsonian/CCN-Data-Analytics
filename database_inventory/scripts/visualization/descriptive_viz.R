## CCN Blue Carbon Data Inventory ####

## contact: Jaxine Wolfe, wolfejax@si.edu
## Summary statistics and visualizations for CONUS cores

library(tidyverse)
library(lubridate)
# library(leaflet)
library(RColorBrewer)

impacts <- read_csv("database_inventory/data/derived/ccn_conus_impacts.csv")
stateinfo <- read_csv("database_inventory/data/derived/state_lookup.csv")
cores <- read_csv("database_inventory/data/derived/ccn_conus_cores.csv", guess_max = 4000) %>% 
  left_join(stateinfo)

## Look at the years that data was sampled in

# Obs per year
cores %>% 
  drop_na(year) %>% 
  count(year, state) %>% 
  ggplot(aes(year, n)) +
  geom_col() +
  xlab("Sampling Year") + ylab("Number of Sediment Cores") +
  theme_bw() + facet_wrap(~state, scales = "free")

## Wetland vegetation classes

# cores %>% 
#   drop_na(vegetation_class) %>% 
#   # distinguish wetland types
#   mutate(wetland_type = ifelse(salinity_class == "fresh", yes = "Riverine tidal", no = "Estuarine intertidal")) %>% 
#   select(site_id, vegetation_class, wetland_type) %>% 
#   distinct() %>% 
#   count(vegetation_class, wetland_type) %>% 
#   arrange(wetland_type, n) %>% 
#   mutate(n = (n/sum(n))*100) %>% # uncomment this for proportions
#   
#   # plot
#   ggplot(aes(wetland_type, n, fill = vegetation_class)) +
#   geom_col() + # position = position_dodge()
#   scale_fill_brewer(name = "Wetland Vegetation\nType", palette = "BrBG", direction = -1) +
#   scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
#   xlab("Wetland Type") + ylab("Proportion of Total Sites (%)") +
#   coord_flip() +
#   theme_bw() +
#   theme(axis.text.y = element_text(size = 10)) 

cores %>% 
  drop_na(vegetation_class) %>% 
  # arrange(vegetation_class) %>% 
  distinct(core_id, vegetation_class, state) %>% 
  count(vegetation_class, state) %>% 
  arrange(n) %>% 
  mutate(n = 100*(n/sum(n)),
         vegetation_class = fct_reorder(vegetation_class, n)) %>% # veg_order = row.names(.)
  # plot
  ggplot(aes(vegetation_class, n, fill = vegetation_class)) + 
  geom_col() +
  scale_fill_brewer(name = "Wetland Vegetation\nType", palette = "BrBG", direction = -1) +
  # scale_fill_manual(name = "Impact Classes", values = impact_color) +
  xlab("Vegetation Class") + ylab("Proportion of Total Cores in CONUS (%)") +
  coord_flip() +
  theme_classic() +
  theme(axis.text.y = element_text(size = 10)) + 
  facet_wrap(~state, scales = "free")

## Habitat ####

# overall
cores %>% 
  drop_na(habitat) %>% 
  distinct(core_id, habitat) %>% 
  count(habitat) %>% 
  arrange(n) %>% 
  mutate(n = 100*(n/sum(n)),
         habitat = fct_reorder(habitat, n)) %>% 
  # plot
  ggplot(aes(habitat, n, fill = habitat)) + 
  geom_col() +
  scale_fill_brewer(name = "Habitat Type", palette = "BrBG", direction = -1) +
  xlab("Habitat") + ylab("Proportion of Cores in CONUS (%)") +
  coord_flip() +
  theme_classic() +
  theme(axis.text.y = element_text(size = 10))

## By state
cores %>% 
  drop_na(habitat) %>% 
  distinct(core_id, habitat, state) %>% 
  count(habitat, state) %>% 
  arrange(n) %>% 
  mutate(n = 100*(n/sum(n)),
         habitat = fct_reorder(habitat, n)) %>% 
  # plot
  ggplot(aes(habitat, n, fill = habitat)) + 
  geom_col() +
  scale_fill_brewer(name = "Habitat Type", palette = "BrBG", direction = -1) +
  xlab("Habitat") + ylab("Proportion of Total Sites (%)") +
  coord_flip() +
  theme_classic() +
  theme(axis.text.y = element_text(size = 10)) + 
  facet_wrap(~state, scales = "free")

## Impacts

cores_impacts <- left_join(cores, impacts) 
# theres a bit of expansion, probably b/c multiple impacts to a sampling location

impact_color <- brewer.pal(9, "RdPu")
impact_color <- colorRampPalette(impact_color)(10)

cores_impacts %>% 
  drop_na(impact_class, vegetation_class) %>% 
  arrange(impact_class) %>% 
  distinct(core_id, impact_class) %>% 
  count(impact_class) %>% 
  arrange(n) %>% 
  mutate(n = 100*(n/sum(n)),
         impact_class = fct_reorder(impact_class, n),
         impact_order = row.names(.)) %>% 
  # plot
  ggplot(aes(impact_class, n, fill = impact_class)) + 
  geom_col() +
  scale_fill_manual(name = "Impact Classes", values = impact_color) +
  xlab("Impact Class") + ylab("Proportion of Total Sites (%)") +
  coord_flip() + theme_bw() +
  theme(axis.text.y = element_text(size = 10)) 

# Impacts along different coasts
cores_impacts %>% 
  drop_na(impact_class) %>% 
  # arrange(impact_class) %>% 
  distinct(core_id, impact_class, coast) %>% 
  count(impact_class, coast) %>% 
  arrange(n) %>% 
  mutate(n = 100*(n/sum(n)),
         impact_class = fct_reorder(impact_class, n),
         impact_order = row.names(.)) %>% 
  # plot
  ggplot(aes(impact_class, n, fill = impact_class)) + 
  geom_col() +
  scale_fill_manual(name = "Impact Classes", values = impact_color) +
  xlab("Impact Class") + ylab("Proportion of Total Cores (%)") +
  coord_flip() + theme_bw() +
  theme(axis.text.y = element_text(size = 10)) +
  facet_wrap(~coast, scales = "free")


