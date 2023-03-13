## CCN Data Library and Database Paper
## Jaxine Wolfe <wolfejax@si.edu>

## Script to conjure up summary statistics and figures for the data library

## Prepare Workspace ####

library(tidyverse)
# library(viridis)
library(RColorBrewer)

# source synthesis
source("scripts/pull_synthesis.R")

## Database Analysis ####

country_smry <- cores %>% 
  count(country, name = "core_count") %>% 
  arrange(core_count)

# cores per habitat type
cores_per_hab <- cores %>%
  # drop_na(habitat) %>% 
  count(habitat, name = "core_count") %>% 
  arrange(core_count)

# cores_per_hab %>% 
#   # NA doesnt get picked up, needs reassignment or to be left out
#   mutate(habitat = fct_reorder(habitat, core_count)) %>% 
#   ggplot(aes(x = habitat, y = core_count, fill = habitat)) +
#   geom_col() +
#   coord_flip() +
#   scale_fill_brewer(palette = "BrBG") +
#   theme_bw()


all_habitat <- cores %>% 
  mutate(habitat = recode(habitat, 
                          "scrub shrub" = "scrub/shrub",
                          "mudflat" = "unvegetated")) %>% 
  group_by(habitat) %>%
  tally() %>%
  ungroup() %>%
  mutate(percent = 100*(n/sum(n)))



# Investigate all cores ####

# other metrics
length(unique(cores$core_id)) # 6745 cores
length(unique(cores$country)) # 64 countries
min(unique(cores$year), na.rm = T) # oldest core: 1960

# depth
max(unique(cores$max_depth), na.rm = T)

cores %>% 
  filter(!is.na(max_depth)) %>%
  ggplot(aes(max_depth)) + 
  geom_density() + geom_rug() +
  ggtitle("Max Depth Distribution for Atlas Cores")
# ggsave("./figures/maxdepth_dist.jpg", width = 6, height = 6)

## ... Habitats ####

# plot proportion of habitats represented in library cores
all_habitat %>%
  mutate(habitat = fct_reorder(habitat, percent)) %>% 
  filter(!is.na(habitat)) %>% 
  ggplot(aes(habitat, percent, fill = percent)) + 
  geom_col(fill = "darkgreen") + 
  # scale_color_brewer(palette = "BuGn") +
  xlab("Habitat Type") + ylab("Proportion of Cores (%)") +
  geom_text(aes(label = paste0(round(percent, 1), "%")), size = 2.75, hjust = -0.2) +
  ylim(0, 50) +
  # theme_classic() +
  coord_flip() + 
  theme_classic(base_size = 17)  
  # theme(axis.text.x = element_text(angle = 45, hjust=1))
ggsave("./figures/cores_per_habitat.jpg", width = 6, height = 6)

## ... Data Quality ####

corequal <- cores %>%
  # filter(habitat == "mangrove") %>%
  select(core_id, stocks_qual_code, dates_qual_code, elevation_qual_code) %>% 
  pivot_longer(-core_id, names_to = "utility", values_to = "quality") %>% 
  drop_na(quality) %>% 
  group_by(core_id) %>% 
  summarize(data_type = paste0(quality, collapse = ", "),
            level = n()) %>% 
  mutate(level = case_when(grepl("A", data_type) ~ "Level 3",
                           grepl("B", data_type) ~ "Level 2",
                           grepl("C", data_type) ~ "Level 1")) %>% 

  # mutate(stocks_qual_code = ifelse(!is.na(stocks_qual_code), 1, 0),
  #        dates_qual_code = ifelse(!is.na(dates_qual_code), 1, 0),
  #        elevation_qual_code = ifelse(!is.na(elevation_qual_code), 1, 0)) %>% 
  # 
  # distinct() %>%
  mutate(data_tier = recode(level,
                            "Level 1" = "Carbon Stocks",
                            "Level 2" = "Carbon Stocks + Age Depth Model",
                            "Level 3" = "Carbon Stocks + Age Depth Model + Elevation"))
  # mutate(data_type = gsub(";", " +", data_type)) %>%
  # group_by(data_type, data_tier) %>%
  # tally()

# plot
corequal %>% 
  count(level, data_tier) %>% 
  mutate(percent = 100*(n/sum(n))) %>% 
  ggplot(aes(level, percent, fill = data_tier)) + 
  geom_col() +
  theme(legend.position = "bottom") +
  ylab("Proportion of Cores (%)") + xlab("Data Quality Tiers")
  # theme_classic()
# ggsave("./figures/data_quality.jpg")


# Plot Salinity

# number of sites per salinity class
cores %>% 
  drop_na(salinity_class) %>% 
  arrange(salinity_class) %>% 
  distinct(core_id, salinity_class) %>% 
  count(salinity_class) %>% 
  mutate(salinity_class = fct_reorder(salinity_class, n)) %>%
  ggplot(aes(salinity_class, n, fill = salinity_class)) +
  geom_col() +
  xlab("Salinity Class") + ylab("Number of Cores") +
  coord_flip() + theme_bw() +
  theme(legend.position = "none")
ggsave("./figures/core_salinity_classes.jpg")

# Plot Vegetation

cores %>% 
  drop_na(vegetation_class) %>% 
  arrange(vegetation_class) %>% 
  distinct(core_id, vegetation_class) %>% 
  count(vegetation_class) %>% 
  mutate(vegetation_class = fct_reorder(vegetation_class, n)) %>%
  ggplot(aes(vegetation_class, n, fill = vegetation_class)) +
  geom_col() +
  xlab("Wetland Vegetation Class") + ylab("Number of Cores") +
  coord_flip() + theme_bw() +
  theme(legend.position = "none")
ggsave("./figures/core_vegetation_types.jpg")

# Plot Impact Classes

# impacts_prep <- impacts %>% drop_na(impact_class) %>% 
#   mutate(impact_class = recode(impact_class, "natural" = "least disturbed"),
#          impact_class_site = ifelse(is.na(core_id), impact_class, NA_character_)) %>% 
#   rename(impact_class_core = impact_class)

impacts %>% 
  drop_na(impact_class) %>% 
  arrange(impact_class) %>% 
  distinct(site_id, impact_class) %>% 
  count(impact_class) %>% 
  mutate(impact_class = fct_reorder(impact_class, n)) %>%
  ggplot(aes(impact_class, n, fill = impact_class)) +
  geom_col() +
  xlab("Impact Class") + ylab("Number of Sites") +
  coord_flip() + theme_bw() +
  theme(legend.position = "none")
ggsave("./figures/site_impacts.jpg")

## Plot core locations

library(leaflet)

leaflet(cores) %>% 
  # addTiles() %>% 
  addProviderTiles("CartoDB.DarkMatter") %>%
  addCircles(lat = ~latitude, lng = ~longitude, 
                   radius = 2, opacity = 0.5, color = "white")
