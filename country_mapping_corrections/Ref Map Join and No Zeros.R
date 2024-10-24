# Generate

library(sf)
library(tidyverse)

# Load shapefiles as special feature
countries_eez_watershed_fishnet <- st_read("data/CCN_maps/Countries_w_EEZ_fishnet_Hydrobasins06_241022.shp")

# Pixel counts 
mangrove_marsh_pixel_count <- read_csv("country_mapping_corrections/Global_Mangrove_Marsh_Summaries.csv")

mangrove_marsh_map <- countries_eez_watershed_fishnet %>% 
  left_join(mangrove_marsh_pixel_count, relationship =
              "many-to-many") %>% 
  filter(mangrove_pixels > 0 | marsh_pixels > 0) %>% 
  select(-`system:index`)

st_write(mangrove_marsh_map, "data/CCN_maps/Global_Mangrove_Marsh_Summaries_noZeros.shp", append = F)


some_checks <- mangrove_marsh_map %>% 
  as_tibble() %>% 
  group_by(territory) %>% 
  summarise(mangrove_pixels = sum(mangrove_pixels),
            marsh_pixels = sum(marsh_pixels))

head(some_checks %>% arrange(-marsh_pixels))
tail(some_checks %>% arrange(-marsh_pixels))

head(some_checks %>% arrange(-mangrove_pixels))
tail(some_checks %>% arrange(-mangrove_pixels))
