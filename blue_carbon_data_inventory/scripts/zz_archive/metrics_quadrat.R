library(tidyverse)
library(gridExtra)
library(grid)

# Get core data
# Simplify habitat categories
cores <- read_csv("data/derived/CONUS_cores.csv", guess_max = 7000)
wetland_area <- read_csv("data/derived/state_wetland_area_ccap.csv")

# Quantity Metric
quantity_metric <- cores %>% 
  group_by(state) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(state = state.abb[match(state,state.name)]) %>% 
  left_join(wetland_area) %>% 
  mutate(quantity_metric = n / estimated_area_ha * 1000) %>% 
  arrange(-quantity_metric) %>% 
  select(state, quantity_metric)

# Quality metric
quality_metric <- cores %>% 
  filter(stocks_qual_code == "C1" | 
           dates_qual_code == "B1" | 
           elevation_qual_code %in% c("A1", "A2")) %>% 
  group_by(state) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(state = state.abb[match(state,state.name)]) %>% 
  left_join(wetland_area) %>% 
  mutate(quality_metric = n / estimated_area_ha * 1000) %>% 
  arrange(-quality_metric) %>% 
  select(state, quality_metric)

quality_metric <- bind_rows(quality_metric, data.frame(state = 
                                                         wetland_area$state[! wetland_area$state %in% quality_metric$state],
                                                       stringsAsFactors = F))

# spatial_rep_metric
spatial_rep_metric <- read_csv("data/derived/spatial_rep_by_state.csv") %>% 
  arrange(-spatial_representativeness_metric) %>% 
  mutate(state = state.abb[match(state,state.name)])

spatial_rep_metric <- bind_rows(spatial_rep_metric, data.frame(state = 
                                                                 wetland_area$state[! wetland_area$state %in% spatial_rep_metric$state],
                                                               stringsAsFactors = F))

spatial_rep_metric$spatial_representativeness_metric[spatial_rep_metric$state == "PA"] <- NA

# habitat_rep_metric
habitat_rep_metric <- read_csv("data/derived/habitat_representativeness_metric.csv") %>% 
  arrange(-euclidean_distance)

habitat_rep_metric <- bind_rows(habitat_rep_metric, data.frame(state = 
                                                                 wetland_area$state[! wetland_area$state %in% habitat_rep_metric$state],
                                                               stringsAsFactors = F))

# Make sure PA and DC are all NA's
habitat_rep_metric$euclidean_distance[habitat_rep_metric$state == "PA"] <- NA
habitat_rep_metric$euclidean_distance[habitat_rep_metric$state == "DC"] <- NA


# States detailed in report?
# May as well join into a single data frame

all_states_quadrats <- quantity_metric %>% 
  full_join(quality_metric) %>% 
  full_join(spatial_rep_metric) %>% 
  full_join(habitat_rep_metric)

# states_in_report <- c("CA", "FL", "ME", "MD", "MA", "NJ", "NY", "OR", "VA", "WA")

# all_states_quadrats <- all_states_quadrats %>% 
#   mutate(detailed_report = ifelse(state %in% states_in_report, "yes", "no"))

# recode NA metric values as 0
all_states_quadrats <- all_states_quadrats %>% 
  mutate(quantity_metric2 = ifelse(is.na(quantity_metric), 0, quantity_metric),
         quality_metric2 = ifelse(is.na(quality_metric), 0, quality_metric),
         spatial_representativeness_metric2 =  ifelse(is.na(spatial_representativeness_metric), 0, spatial_representativeness_metric),
         euclidean_distance2 = ifelse(is.na(euclidean_distance), 0, euclidean_distance))

quantity_plot <- ggplot(data = all_states_quadrats, 
                        aes(x = reorder(state, quantity_metric2), y = quantity_metric)) +
  geom_point(pch=16) +
  geom_segment(aes(xend=state, yend=0)) +
  xlab(NULL) +
  ylab("Cores per 1000 ha") +
  ggtitle("A. Quantity") +
  coord_flip() +
  # labs(color = "Detailed in report?") +
  theme_minimal()

(quantity_plot) 

quality_plot <- ggplot(data = all_states_quadrats, aes(x = reorder(state, quality_metric2), y = quality_metric)) +
  geom_point(pch=16) +
  geom_segment(aes(xend=state, yend=0)) +
  xlab(NULL) +
  ylab("High Quality Cores per 1000 ha") +
  ggtitle("B. Quality") +
  coord_flip() +
  # labs(color = "Detailed in report?") +
  theme_minimal() +
  theme(legend.position = "none")

(quality_plot) 

spatial_plot <- ggplot(data = all_states_quadrats, aes(x = reorder(state, spatial_representativeness_metric2), y = spatial_representativeness_metric)) +
  geom_point(pch=16) +
  geom_segment(aes(xend=state, yend=0)) +
  xlab(NULL) +
  ylab("Mean Sampling Distance\n/ Idealized Sampling Distance") +
  ggtitle("C. Spatial Representativeness") +
  coord_flip() +
  # labs(color = "Detailed in report?") +
  theme_minimal() +
  theme(legend.position = "none")

(spatial_plot) 

habitat_plot <- ggplot(data = all_states_quadrats, 
                       aes(x = reorder(state, euclidean_distance2), y = euclidean_distance)) +
  geom_point(pch=16) +
  geom_segment(aes(xend=state, yend=0)) +
  xlab(NULL) +
  ylab("1 - Distance Between Sampled\nand Mapped Proportions") +
  ggtitle("D. Habitat Representativeness") +
  coord_flip() +
  # labs(color = "Detailed in report?") +
  theme_minimal() +
  theme(legend.position = "none")

(habitat_plot)

grid.arrange(nrow=2, ncol=2,
             quantity_plot, quality_plot, 
             spatial_plot, habitat_plot)

grobs <- arrangeGrob(nrow=2, ncol=2,
                     quantity_plot, quality_plot, 
                     spatial_plot, habitat_plot,
                     left = textGrob("Lower to Higher Ranking", rot = 90, vjust = 1))

ggsave("figures/state_quadrat_figure.jpg", dpi=300, width = 6.5, height = 6.5, grobs)
ggsave("figures/state_quadrat_figure.pdf", width = 6.5, height = 6.5, grobs)
