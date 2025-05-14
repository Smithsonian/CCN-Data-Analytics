# Prepare Territory Shapefiles for App Input

library(plyr)
library(dplyr)
library(readr)
library(tidyr)
library(sf)
library(leaflet)

#read in shp files for data analytics based map
world_ccn <- st_read("data/CCN_maps/CCN_Countries_and_EEZ_map.shp") %>% 
  filter(source != "EEZ")

# sort(unique(world_ccn$territory))

terrs <- sort(unique(world_ccn$territory))
selected_geo <- terrs[sample(1:length(terrs), 1)]
selected_geo <- "Monaco"

geo_subset <- world_ccn %>% filter(territory == selected_geo)

geo_simplified <- world_ccn %>% 
  filter(territory == selected_geo) %>% 
  st_make_valid() %>% 
  # https://www.r-bloggers.com/2021/03/simplifying-geospatial-features-in-r-with-sf-and-rmapshaper/
  # simplify features with a dTolerance of 1000 = 1km
  st_simplify(., preserveTopology = T, dTolerance = 500) %>% 
  # drop empty geometries before plotting
  filter(!st_is_empty(.))

# map the difference
leaflet() %>% 
  # basemap options
  addTiles(group = "OSM (default)") %>%
  addProviderTiles(providers$CartoDB, group = "CartoDB") %>% 
  
  # add polygon layer
  addPolygons(data = geo_subset, weight = 2,
              group = "Border") %>% 
  addPolygons(data = geo_simplified, weight = 2,
              group = "Border", color = "red") 
  
## Create Simplified version of the territory shapefile

territories_simplified <- st_read("data/CCN_maps/CCN_Countries_and_EEZ_map.shp") %>% 
  filter(source != "EEZ") %>% 
  st_make_valid() %>% 
  st_simplify(., preserveTopology = T, dTolerance = 500) %>% 
  # drop empty geometries before plotting
  filter(!st_is_empty(.))

# this simplification drops two territories
which(!(unique(world_ccn$territory) %in% unique(territories_simplified$territory)))
unique(world_ccn$territory)[46] # Clipperton
unique(world_ccn$territory)[120] # Monaco (restored with dTolerance 500)

# write the 
st_write(territories_simplified, "inventory_app_input/territory_shapefiles/ccn_territory_map.shp", append = F)

