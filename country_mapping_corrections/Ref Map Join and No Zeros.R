# Generate

library(sf)
library(tidyverse)

# Load shapefiles as special feature
countries_eez_watershed_fishnet <- st_read("data/CCN_maps/Countries_w_EEZ_fishnet_Hydrobasins06_dissolved_241023.shp")

# Making sure all polygons are unique
countries_eez_watershed_fishnet_check_1 <- countries_eez_watershed_fishnet %>% 
  as_tibble() %>% 
  group_by(HYBAS_ID, FishId, country, territory, source) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  arrange(-n)

head(countries_eez_watershed_fishnet_check_1)

# Load pixel counts 
mangrove_marsh_pixel_count <- read_csv("country_mapping_corrections/Global_Mangrove_Marsh_Summaries.csv")

# Join shapefile and area counts
mangrove_marsh_map <- countries_eez_watershed_fishnet %>% 
  left_join(mangrove_marsh_pixel_count) %>% 
  # Remove zero values
  filter(mangrove_area_m2 > 0 | marsh_area_m2 > 0) %>% 
  select(-`system:index`)

st_write(mangrove_marsh_map, "data/CCN_maps/Global_Mangrove_Marsh_Summaries_noZeros.shp", append = F)

# Check areas
some_checks <- mangrove_marsh_map %>% 
  as_tibble() %>% 
  group_by(territory) %>% 
  summarise(marsh_area_m2 = sum(marsh_area_m2),
            mangrove_area_m2 = sum(mangrove_area_m2))

head(some_checks %>% arrange(-marsh_area_m2))
tail(some_checks %>% arrange(-marsh_area_m2))

head(some_checks %>% arrange(-mangrove_area_m2))
tail(some_checks %>% arrange(-mangrove_area_m2))
# Makes sense to me
