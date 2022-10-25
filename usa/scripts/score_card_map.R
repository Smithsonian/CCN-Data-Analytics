## Map the composite scores

library(tidyverse)
library(sf)
library(rnaturalearth)

scores <- read_csv("usa/scripts/heatmap-ggplot-example/ccn_report_card_data.csv") %>% 
  filter(metric == "Total") %>% drop_na(normalized_rank)


# Import states
states <- st_read("usa/data/shapefiles/us_states/states_coastline_boundaries/cb_2017_us_state_500k.shp",
                  stringsAsFactors = F)

# Common projection for states
states_aea <- st_transform(states, crs = "+proj=aea +ellps=WGS84 +lat_1=29.5 +lat_2=45.5 +lon_0=-96 +x_0=0 +y_0=0")

# filter for coastal states
coastal_states <- states_aea %>% 
  filter(STUSPS %in% unique(scores$state)) %>% 
  rename(state = STUSPS) %>% 
  full_join(scores)

# Continental Boundaries
map.sf <- ne_countries(scale = 'medium', type = 'map_units', returnclass = 'sf')

map.na.sf <- map.sf[map.sf$continent == 'North America',]

map.na.sf.aea <- st_transform(map.na.sf, crs = "+proj=aea +ellps=WGS84 +lat_1=29.5 +lat_2=45.5 +lon_0=-96 +x_0=0 +y_0=0")

## Map States with Composite Scores ####

# generate bounding box

b <- st_bbox(coastal_states)

# ggplot score map

ggplot() + 
  # continental boundary 
  geom_sf(data=map.na.sf.aea, color="black", size = 0.1) +
  # states
  geom_sf(data = coastal_states, mapping = aes(fill = as.character(normalized_rank)), 
          color = "darkgrey", size = 0.1) +
  # custom colors...this scales it to 5 categories instead of 7, mismatching the colors
  scale_fill_manual(values = c("#4375B5", "#91BEDB", "#E0F3F8", "#FFFDC0", "#FEE090")) +
  # scale_fill_brewer(name = "Composite Score", palette = "RdYlBu", direction = - 1,
                    # labels = c("Best", "", "", "Fair", "", "", "Poor", "No Data")) +
  # project aes w bounding box
  coord_sf(xlim = c(b["xmin"], b["xmax"]), ylim = c(b["ymin"], b["ymax"]),
           crs="+proj=aea +ellps=WGS84 +lat_1=29.5 +lat_2=45.5 +lon_0=-96 +x_0=0 +y_0=0") +
  # Probably want to take off x and y axes text
  theme_minimal() +
  ggtitle("Coastal State Composite Scores") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "none")

ggsave("usa/figures/blue_carbon_score_map.jpg", width = 7.25, height = 5)

