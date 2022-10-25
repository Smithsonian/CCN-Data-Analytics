## CCN Blue Carbon Data Inventory ####

## contact: Jaxine Wolfe, wolfejax@si.edu
## date: 11-10-2021

# Spatial Representativeness Index

library(tidyverse)
library(sf)
library(rnaturalearth)
# library(rgeos)
library(gridExtra)

# Import Core Data
cores <- read_csv("data/derived/CONUS_cores.csv", guess_max = 7000)
projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# set cores as simple feature
cores_sf <- st_as_sf(cores,
         coords = c("longitude", "latitude"),
         crs = projcrs)

# Common projection for cores
# equal area projection
cores_aea <- st_transform(cores_sf,
                          crs = "+proj=aea +ellps=WGS84 +lat_1=29.5 +lat_2=45.5 +lon_0=-96 +x_0=0 +y_0=0")
                                                
# Import HUC8 watershed data
huc8shp <- st_read("data/shapefiles/coastal_HUC8s/HUC8_AllTidalNwiAndNonTidalPlusFarmedBelowMHHWS_ObviousOutliersRemoved.shp",
                   stringsAsFactors = F)

# Common projection for watersheds
huc8shp_aea <- st_transform(huc8shp,
                            crs = "+proj=aea +ellps=WGS84 +lat_1=29.5 +lat_2=45.5 +lon_0=-96 +x_0=0 +y_0=0")

# Import states
states <- st_read("data/shapefiles/us_states/states_coastline_boundaries/cb_2017_us_state_500k.shp",
                  stringsAsFactors = F)

# Common projection for states
states_aea <- st_transform(states,
                           crs = "+proj=aea +ellps=WGS84 +lat_1=29.5 +lat_2=45.5 +lon_0=-96 +x_0=0 +y_0=0")

# Calculate centroid of watersheds
huc8shp_aea_centroid <- huc8shp_aea %>% 
  st_centroid() %>% 
  # Join with states
  st_join(states_aea, join = st_nearest_feature)

# ggplot(data = huc8shp_aea_centroid) +
#   geom_sf(aes(color = STUSPS))

# Which states are involved in the analysis?
states_to_analyse <- unique(huc8shp_aea_centroid$NAME)

spatial_rep_by_state <- data.frame(state = states_to_analyse,
                                   mean_core_distance = rep(NA, length(states_to_analyse)),
                                   mean_huc8_distance = rep(NA, length(states_to_analyse)),
                                   spatial_representativeness_metric = rep(NA, length(states_to_analyse)))

# Iterate through each state
for (i in 1:length(states_to_analyse)) {
  
  # Filter cores by state
  cores_in_state <- cores_aea %>% 
    filter(state == states_to_analyse[i])
  
  # Filter HUC8's by state
  huc8s_in_state <- huc8shp_aea_centroid %>% 
    filter(NAME == states_to_analyse[i])
  
  # Distance matrix for cores
  core_distance_matrix <- st_distance(cores_in_state)
  
  # Mean distance for cores
  mean_core_distance <- mean(core_distance_matrix[upper.tri(core_distance_matrix)])
  
  # Distance matrix for HUC8's
  huc8_distance_matrix <- st_distance(huc8s_in_state)
  
  # Mean distance for HUC8's
  mean_huc8_distance <- mean(huc8_distance_matrix[upper.tri(huc8_distance_matrix)])
  
  # Mean Core Dist. / Mean Wetland Watershed Dist.
  spatial_representativeness_metric <- mean_core_distance / mean_huc8_distance
  
  # Add to table
  spatial_rep_by_state$mean_core_distance[i] <- mean_core_distance
  spatial_rep_by_state$mean_huc8_distance[i] <- mean_huc8_distance 
  spatial_rep_by_state$spatial_representativeness_metric[i] <- spatial_representativeness_metric
  
}

# Rank and Graph
spatial_rep_by_state$spatial_representativeness_metric[is.na(spatial_rep_by_state$spatial_representativeness_metric)] <- 0

spatial_rep_by_state <- spatial_rep_by_state %>% 
  arrange(spatial_representativeness_metric)

spatial_rep_by_state$state = factor(spatial_rep_by_state$state,
                                    levels = spatial_rep_by_state$state)

ggplot(data = spatial_rep_by_state, aes(x = state, y = spatial_representativeness_metric)) +
  geom_point() +
  geom_segment(aes(xend=state, yend=0)) +
  xlab(element_blank()) +
  ylab("Spatial Representativeness Score") +
  coord_flip()

# will be included in the quadrat figure
# ggsave("figures/spatial_rep_by_state.jpg", width=3.54, height=3.54)
# ggsave("figures/spatial_rep_by_state.pdf", width=3.54, height=3.54)

write_csv(spatial_rep_by_state, "data/report_data/spatial_rep_by_state.csv")

# Example Figures
# Continental Boundaries
map.sf <- ne_countries(scale = 'medium', type = 'map_units',
                       returnclass = 'sf')

map.na.sf <- map.sf[map.sf$continent == 'North America',]

map.na.sf.aea <- st_transform(map.na.sf, 
                              crs = "+proj=aea +ellps=WGS84 +lat_1=29.5 +lat_2=45.5 +lon_0=-96 +x_0=0 +y_0=0")

## Plot LA vs CA Sampling ####

# CA

# Just HUC8 centroids and cores for that state
# Filter cores by state
cores_in_CA <- cores_aea %>% 
  filter(state == "California")

# Filter HUC8's by state
huc8s_in_CA <- huc8shp_aea_centroid %>% 
  filter(NAME == "California")

# Generate lines between each huc8 centroid and each core
# https://gis.stackexchange.com/questions/270725/r-sf-package-points-to-multiple-lines-with-st-cast

# lifted this handy function from stack exchange
# https://stackoverflow.com/questions/17171148/non-redundant-version-of-expand-grid
expand.grid.unique <- function(x, y, include.equals=FALSE)
{
  x <- unique(x)
  
  y <- unique(y)
  
  g <- function(i)
  {
    z <- setdiff(y, x[seq_len(i-include.equals)])
    
    if(length(z)) cbind(x[i], z, deparse.level=0)
  }
  
  do.call(rbind, lapply(seq_along(x), g))
}

# cores_in_CA_connections_pairs <- expand.grid.unique(1:nrow(cores_in_CA), 1:nrow(cores_in_CA))
# cores_in_CA_connections_vect <- c()
# for (i in 1:nrow(cores_in_CA_connections_pairs)) {
#  
#       pair <- st_combine(c(cores_in_CA$geometry[cores_in_CA_connections_pairs[i,1]], cores_in_CA$geometry[cores_in_CA_connections_pairs[i,2]]))
#       line <- st_cast(pair, "LINESTRING")
#       cores_in_CA_connections_vect <- c(cores_in_CA_connections_vect,
#                                         line)
# }
# 
# cores_in_CA_connections_multiline <- st_sfc(st_multilinestring(as.list(cores_in_CA_connections_vect)),
#                                             crs = "+proj=aea +ellps=WGS84 +lat_1=29.5 +lat_2=45.5 +lon_0=-96 +x_0=0 +y_0=0")

# generate bounding box

b <- st_bbox(huc8s_in_CA)

# ggplot

CA_map <- ggplot() + 
  # continental boundary 
  geom_sf(data=map.na.sf.aea, color="black", size=0.1, fill="grey") +
  # huc8's
  geom_sf(data = huc8shp_aea, color = "darkgrey", size = 0.1) +
  # red lines for core connections
  # black lines for HUC8 core connections
  geom_sf(data = huc8s_in_CA, pch=16, size = 1, color = "black") +
  # geom_sf(data = cores_in_CA_connections_multiline, size = 0.005, color = "red", alpha = 0.001) +
  geom_sf(data = cores_in_CA, pch=16, size = 1.5, color = "red", alpha = 0.25) + # set low alpha so clusters are more apparent
  # project aes w bounding box
  coord_sf(xlim = c(b["xmin"], b["xmax"]), ylim = c(b["ymin"], b["ymax"]),
           crs="+proj=aea +ellps=WGS84 +lat_1=29.5 +lat_2=45.5 +lon_0=-96 +x_0=0 +y_0=0") +
  # Probably want to take off x and y axes text
  theme_minimal() +
  ggtitle("California - Low Score, Points Clustered Around SF Bay") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.title = element_blank())


# LA
# Just HUC8 centroids and cores for that state
# Filter cores by state
cores_in_LA <- cores_aea %>% 
  filter(state == "Louisiana")

# Filter HUC8's by state
huc8s_in_LA <- huc8shp_aea_centroid %>% 
  filter(NAME == "Louisiana")

# cores_in_LA_connections_pairs <- expand.grid.unique(1:nrow(cores_in_LA), 1:nrow(cores_in_LA))
# cores_in_LA_connections_vect <- c()
# for (i in 1:nrow(cores_in_LA_connections_pairs)) {
#   
#   pair <- st_combine(c(cores_in_LA$geometry[cores_in_LA_connections_pairs[i,1]], cores_in_LA$geometry[cores_in_LA_connections_pairs[i,2]]))
#   line <- st_cast(pair, "LINESTRING")
#   cores_in_LA_connections_vect <- c(cores_in_LA_connections_vect,
#                                     line)
# }
# 
# cores_in_LA_connections_multiline <- st_sfc(st_multilinestring(as.list(cores_in_LA_connections_vect)),
#                                             crs = "+proj=aea +ellps=WGS84 +lat_1=29.5 +lat_2=45.5 +lon_0=-96 +x_0=0 +y_0=0")

# generate bounding box

b_LA <- st_bbox(cores_in_LA)

# ggplot

LA_map <- ggplot() + 
  # continental boundary 
  geom_sf(data=map.na.sf.aea, color="black", size=0.1, fill="grey") +
  # huc8's
  geom_sf(data = huc8shp_aea, color = "darkgrey", size = 0.1) +
  # red lines for core connections
  # black lines for HUC8 core connections
  geom_sf(data = huc8s_in_LA, pch=16, size = 1, color = "black") +
  # geom_sf(data = cores_in_LA_connections_multiline, size = 0.005, color = "red", alpha = 0.001) +
  geom_sf(data = cores_in_LA, pch=16, size = 1.5, color = "red", alpha = 0.25) + # set low alpha so clusters are more apparent
  # project aes w bounding box
  coord_sf(xlim = c(b_LA["xmin"], b_LA["xmax"]), ylim = c(b_LA["ymin"], b_LA["ymax"]),
           crs="+proj=aea +ellps=WGS84 +lat_1=29.5 +lat_2=45.5 +lon_0=-96 +x_0=0 +y_0=0") +
  # Probably want to take off x and y axes text
  theme_minimal() +
  ggtitle("Louisiana - High Score, Spread Out Accross State") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.title = element_blank())
(LA_map)

grid.arrange(CA_map, LA_map, ncol=2, widths = c(1.25,2.75))

mapsTogether <- arrangeGrob(CA_map, LA_map, ncol=2, widths = c(1.25,2.75))

ggsave("figures/spatial_rep_CA_v_LA.jpg", mapsTogether, width = 7.25, height = 5)
ggsave("figures/spatial_rep_CA_v_LA.pdf", mapsTogether, width = 7.25, height = 5)
