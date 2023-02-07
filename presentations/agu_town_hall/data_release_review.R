
library(tidyverse)
# mapping stuff
library(leaflet)
# library(rnaturalearth)
# library(rnaturalearthdata)
# library(sf)
# library(rgdal)

# read in data
studies <- read_csv("agu_town_hall/data_ingestion_20201209.csv")
cores <- read_csv("/Users/jaxinewolfe/Documents/SERC/CCN/CCRCN-Data-Library/data/CCRCN_V2/core_geography.csv", guess_max = 7000)
keshta_cores <- read_csv("/Users/jaxinewolfe/Documents/SERC/CCN/CCRCN-Data-Release-Formatting/data_releases/keshta_et_al_2020/data/final/Keshta_et_al_2020_cores.csv")
stlaurent_cores <- read_csv("/Users/jaxinewolfe/Documents/SERC/CCN/CCRCN-Data-Release-Formatting/data_releases/StLaurent_et_al_2020/data/final/StLaurent_et_al_2020_cores.csv")

## Data Wrangling ####

# filter data ingestion for published studies
releases <- studies %>%
  filter(status == "published") %>%
  rename(importance = `Important (1 - Yes: Using for WG or high priority for buidling international nodes, 2 - Maybe: Investigating or Deciding, 3 - No)`,
         urgency = `Urgent (1 - Yes: There's a data release we can find, 2 - Maybe/In Progress, 3 - No: Data release doesn't exist yet)`,
         data_type = `Data Types`) %>%
  mutate(study_id = strsplit(study_id, "; ")) %>%
  unnest(study_id)

# compile all cores  (including Keshta and StLaurent)
all_cores <- bind_rows(keshta_cores, stlaurent_cores) %>%
  dplyr::rename(latitude = core_latitude, longitude = core_longitude,
                year = core_year) %>%
  dplyr::select(study_id, site_id, year, latitude, longitude, habitat) %>%
  # Keshta has some non-standardized habitat types
  mutate(habitat = recode(habitat, "mudflat" = "unvegetated")) %>% 
  mutate(habitat = ifelse(study_id == "StLaurent_et_al_2020", "marsh", habitat)) %>%
  bind_rows(cores)
  
# filter by data released this year 
cores_published <- all_cores %>% 
  filter(study_id %in% releases$study_id) %>%
  # correct some geography assignments
  mutate(country = case_when(study_id == "Belshe_et_al_2019" ~ "Spain",
                             study_id == "Thom_1992" | study_id == "Kauffman_et_al_2020" | study_id == "Vaughn_et_al_2020" | study_id == "Poppe_et_al_2019" ~ "United States",
                             study_id == "Keshta_et_al_2020_a" | study_id == "Keshta_et_al_2020_b" | study_id == "StLaurent_et_al_2020" ~ "United States",
                             study_id == "Sanborn_and_Coxson_2020" ~ "Canada",
                             TRUE ~ country),
         admin_division = case_when(study_id == "Belshe_et_al_2019" ~ "Balearic Islands",
                             study_id == "Sanborn_and_Coxson_2020" ~ "British Columbia",
                             study_id == "Vaughn_et_al_2020" ~ "Florida",
                             study_id == "Poppe_et_al_2019" ~ "Washington",
                             study_id == "Keshta_et_al_2020_a" | study_id == "Keshta_et_al_2020_b" ~ "Maryland",
                             study_id == "StLaurent_et_al_2020" ~ "Delaware",
                             TRUE ~ admin_division))

# join the release df to the core table
study_cores <- full_join(cores_published, releases) %>%
  # filter(last_updated == "2020") %>% # for recent/updated releases
  filter(study_id != "Whigham_et_al_2020")

# trim this table down
study_cores_trim <- study_cores %>%
  dplyr::select(study_id, site_id, year, last_updated, data_type, latitude, longitude, habitat, country, admin_division)
  

# Investigate all cores ####

# core quality 
stock_cores <- length(which(!is.na(all_cores$stocks_qual_code)))
dated_cores <- length(which(!is.na(all_cores$dates_qual_code)))
elevation_cores <- length(which(!is.na(all_cores$elevation_qual_code)))

# other metrics
country_n <- length(unique(all_cores$country)) # geography 
oldest_core <- min(unique(all_cores$year), na.rm = T)

# compare to habitats across all cores
# studies w a lot of unidentified habitats: Schile-Beers_and_Megonigal_2017, Osland_et_al_2016, Drexler_et_al_2019
core_habitat_all <- all_cores %>% 
  group_by(habitat) %>%
  tally() %>%
  ungroup() %>%
  mutate(percent = 100*(n/sum(n)))
# plot
ggplot(core_habitat_all, aes(habitat, percent)) + 
  xlab("Habitat Type") + ylab("Percent of Cores per Habitat") +
  geom_col(fill = "darkgreen") + 
  geom_text(aes(label = paste0(round(percent, 1), "%")), size = 2.75, vjust = -0.5) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
ggsave("agu_town_hall/figures/core_habitat_synthesis.jpg")

# Investigate new releases ####

# ... Cores sampled through time ----------
# number of cores from different years
core_year <- study_cores_trim %>% 
  group_by(year) %>%
  tally()
# plot
ggplot(core_year, aes(year, n)) + geom_col()

# ... Cores published per year ----------

# number of cores from different years
core_count_year <- study_cores_trim %>%
  group_by(last_updated) %>%
  tally() %>% mutate(year_published = as.character(last_updated))
# plot
ggplot(core_count_year, aes(year_published, n, fill = year_published)) +
  geom_col() +
  theme_classic()

# Habitat composition ----------

# habitat types of data releases
core_habitat <- study_cores_trim %>% 
  group_by(habitat) %>%
  tally() %>%
  ungroup() %>%
  mutate(percent = 100*(n/sum(n)))
# plot
ggplot(core_habitat, aes(habitat, percent)) + 
  geom_col(fill = "darkgreen") + 
  xlab("Habitat Type") + ylab("Percent of Cores per Habitat") +
  theme_classic()
ggsave("agu_town_hall/figures/core_habitat_published.jpg")


# Data Quality ----------
# different data types (carbon stock, age depth, elevation, etc) - leave out Whigham
core_datatypes <- study_cores_trim %>%
  dplyr::select(study_id, data_type) %>% distinct() %>%
  mutate(data_tier = recode(data_type,
                            "Carbon Stocks" = "Level 1", 
                            "Carbon Stocks; Age Depth Model" = "Level 2", 
                            "Carbon Stocks; Age Depth Model; Elevation" = "Level 3")) %>%
  mutate(data_type = gsub(";", " +", data_type)) %>%
  group_by(data_type, data_tier) %>%
  tally()

# plot
ggplot(core_datatypes, aes(data_tier, n, fill = data_type)) + 
  geom_col() +
  ylab("Number of Data Releases") + xlab("Data Quality Tiers") +
  theme_classic()
ggsave("agu_town_hall/figures/data_quality.jpg")


# New study locations ----------
locations <- study_cores_trim %>%
  dplyr::select(study_id, site_id, last_updated, latitude, longitude, country, admin_division) %>% 
  drop_na(latitude) %>% 
  distinct() %>%
  group_by(study_id, site_id, country, admin_division, last_updated) %>%
  summarise(latitude = mean(latitude), longitude = mean(longitude))

# interactive map

# define palette 
pal <- colorFactor(palette = "RdBu", locations$last_updated)

leaflet(locations) %>% 
  addProviderTiles(providers$CartoDB) %>%
  # addTiles() %>%
  addCircleMarkers(lng = ~as.numeric(longitude), lat = ~as.numeric(latitude), 
                   radius = 5, label = ~study_id, opacity = 1, color = ~pal(last_updated)) %>%
  addLegend(pal = pal, values = ~last_updated)

# it appears that we've added Canada, Netherlands, and Germany
# View(locations %>% ungroup() %>% select(country, last_updated) %>% distinct())

# static map
# 
# # projection options
# wgs84 <- CRS("+proj=longlat +datum=WGS84")
# miller <- "+proj=mill +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
# lambert <- "+proj=lcc +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
# robin <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
# albers <- "+proj=aea +ellps=WGS84 +lat_1=29.5 +lat_2=45.5 +lon_0=-96 +x_0=0 +y_0=0"
# # set projection
# prj <- 4326
# 
# # prepare world map
# world <- sf::st_as_sf(maps::map("world", fill = TRUE, plot = FALSE)) # maps opt
# sf_map <- ne_countries(scale = 'medium', type = 'map_units', returnclass = 'sf') # rnaturalearth opt
# # set the world projection
# sf_map_prj <- st_transform(sf_map, crs = prj)
# 
# # convert locations to spatial data frame
# # unnecessary step
# # my_sp <- sp::SpatialPointsDataFrame(coords = data.frame(locations$longitude, 
# #                                                         locations$latitude), 
# #                                     data = data.frame(locations),
# #                                     proj4string = wgs84)
# 
# # set as simple feature
# sf_coords <- st_as_sf(locations, coords = c("longitude", "latitude"), crs = 4326)
# 
# # set the projection
# sf_coord_prj <- st_transform(sf_coords, crs = prj)
# b <- st_bbox(sf_coord_prj) # bounding box for map
# 
# ggplot() + 
#   geom_sf(data = sf_map_prj, color="black", size=0.1, fill="grey") +
#   geom_sf(data = sf_coord_prj) +
#   # geom_density2d(data = locations, aes(longitude, latitude)) +
#   coord_sf(xlim = c(b["xmin"], b["xmax"]), ylim = c(b["ymin"], b["ymax"]), crs = prj) +
#   theme_minimal() +   # take off x and y axes text
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#         legend.title = element_blank())


