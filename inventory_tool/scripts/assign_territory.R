## Join Territories to CCN data

# create lookup for app: study, site, core, lat, lon, territory, country

library(plyr)
library(dplyr)
library(readr)
library(tidyr)
library(sf)
library(leaflet)

#read in shp files for data analytics based map
world_ccn <- st_read("data/CCN_maps/CCN_Countries_and_EEZ_map.shp") %>% 
  st_make_valid()

core_path <- "https://raw.githubusercontent.com/Smithsonian/CCN-Data-Library/main/data/CCN_synthesis/CCN_cores.csv"
# plot_path <- "https://raw.githubusercontent.com/Smithsonian/CCN-Data-Library/main/data/CCN_synthesis/CCN_plots.csv"
# deal with biomass data later...

# read in current version
guess_max <- nrow(read_csv(core_path))
current_cores <- read_csv(core_path, guess_max = guess_max) %>% 
  rename(current_country = country) # so we can spot differences after the join

## Prepare point and polygon data ####

# isolate core data and drop NAs
# rand_select <- sample(1:8600, size = 100) # for testing, random selection of coordinates from the database
pnts <- current_cores %>% 
  drop_na(longitude, latitude) %>%
  # this could be problematic if coords are updated (it wouldn't detect)
  # filter(!(core_id %in% unique(already_assigned$core_id))) %>%
  select(study_id, site_id, core_id, latitude, longitude) %>%
  mutate(across(c(latitude, longitude), as.numeric))


sp_points <- st_as_sf(pnts, coords = c('longitude',"latitude")) # make points spatial
st_crs(sp_points) <- 4326 # Give the points a coordinate reference system (CRS)

## Spatial Join

# Join country with both countries and EEZ's
point_poly_join <- st_join(sp_points, world_ccn)

# check out out nodata values
# View(filter(point_poly_join, !complete.cases(country)))

# make corrections 
assigned_geography <- point_poly_join %>% 
  st_drop_geometry() %>% 
  mutate(country = ifelse(site_id == "Inhaca_Island", "Mozambique", country),
         territory = ifelse(site_id == "Inhaca_Island", "Mozambique", territory)) %>% 
  select(-source)

# output lookup table for cores and territories
write_csv(assigned_geography, "inventory_app_input/core_territory_lookup.csv")


