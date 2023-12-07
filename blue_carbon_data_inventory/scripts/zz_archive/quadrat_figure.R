## Generate Quadrat Figure

library(tidyverse)
library(sf)
# library(rnaturalearth)
# library(rgeos)
library(gridExtra)

# # source inventorying functions
# source("database_inventory/scripts/metrics/inventory_functions.R")
# 
# # Import Core Data
# cores_v1 <- read_csv("database_inventory/data/synthesis/v1/CONUS_cores.csv", guess_max = 7000)
# cores_v2 <- sourceSynthesis()
# 
# # quadratFigure <- function(cores){
# #   
# # }
# 
# # run functions
# quantity <- quantityMetric(cores_v2)
# quality <- qualityMetric(cores_v2)
# spatial <- spatialMetric(cores_v2)
# habitat <- habitatMetric(cores_v2)
# 
# 
# # Quantity Metric
# quantity_metric <- quantity %>% 
#   # rename(quantity_metric = cores_per_1000ha) %>% 
#   # arrange(-quantity_metric) %>% 
#   select(state, quantity_metric)
# 
# # Quality metric
# quality_metric <- quality %>% 
#   # rename(quality_metric = cores_per_1000ha) %>% 
#   # arrange(-quality_metric) %>% 
#   select(state, quality_metric)
#   # add missing coastal states
#   # add_row(state = wetland_area$state[! wetland_area$state %in% quality$state],
#           # quality_metric = NA)
# 
# # Spatial Representativeness
# spatial_rep_metric <- spatial %>% 
#   select(state, spatial_representativeness_metric) %>% 
#   # add_row(state = wetland_area$state[! wetland_area$state %in% spatial_rep$state],
#           # spatial_representativeness_metric = NA) %>% 
#   # convert 0 values to NA
#   mutate(spatial_representativeness_metric = na_if(spatial_representativeness_metric, 0)) # just PA
# 
# # Habitat Representativeness
# 
# habitat_rep_metric <- habitat %>% 
#   select(state, habitat_metric) %>% 
#   # add_row(state = wetland_area$state[! wetland_area$state %in% habitat_rep$state],
#           # habitat_metric = NA) %>% 
#   # convert 0 values to NA
#   mutate(habitat_metric = ifelse(state == "PA" | state == "DC", NA, habitat_metric))
# 
# # join into a single data frame
# all_states_quadrats <- quantity_metric %>% 
#   full_join(quality_metric) %>% 
#   full_join(spatial_rep_metric) %>% 
#   full_join(habitat_rep_metric)
# # write this table to folder?
# write_csv(all_states_quadrats, "blue_carbon_data_inventory/tables/state_metrics_v2.csv")
# # this could be the end of a quadrat function 
# 
# # recode NA metric values as 0
# all_states_quadrats <- all_states_quadrats %>% 
#   mutate(quantity_metric2 = ifelse(is.na(quantity_metric), 0, quantity_metric),
#          quality_metric2 = ifelse(is.na(quality_metric), 0, quality_metric),
#          spatial_representativeness_metric2 =  ifelse(is.na(spatial_representativeness_metric), 0, spatial_representativeness_metric),
#          euclidean_distance2 = ifelse(is.na(habitat_metric), 0, habitat_metric))

## Construct State-Level Quadrat Figure ####
quantity_plot <- ggplot(data = all_states_quadrats, 
                        aes(x = reorder(state, quantity_metric2), y = quantity_metric)) +
  geom_point(pch=16) +
  geom_segment(aes(xend=state, yend=0)) +
  xlab(NULL) +
  ylab("Cores per 1000 ha tidal wetland") +
  # ggtitle("A. Quantity") +
  coord_flip() +
  # labs(color = "Detailed in report?") +
  theme_minimal()

(quantity_plot) 
# ggsave("figures/state_quantity.jpg", width = 6.5, height = 6.5, quantity_plot)


quality_plot <- ggplot(data = all_states_quadrats, aes(x = reorder(state, quality_metric2), y = quality_metric)) +
  geom_point(pch=16) +
  geom_segment(aes(xend=state, yend=0)) +
  xlab(NULL) +
  ylab("High Quality Cores per 1000 ha \ntidal wetland") +
  # ggtitle("B. Quality") +
  coord_flip() +
  # labs(color = "Detailed in report?") +
  theme_minimal() +
  theme(legend.position = "none")

(quality_plot) 
# ggsave("figures/state_quality.jpg", width = 6.5, height = 6.5, quality_plot)


spatial_plot <- ggplot(data = all_states_quadrats, aes(x = reorder(state, spatial_representativeness_metric2), y = spatial_representativeness_metric)) +
  geom_point(pch=16) +
  geom_segment(aes(xend=state, yend=0)) +
  xlab(NULL) +
  ylab("Mean Sampling Distance\n/ Idealized Sampling Distance") +
  # ggtitle("C. Spatial Representativeness") +
  coord_flip() +
  # labs(color = "Detailed in report?") +
  theme_minimal() +
  theme(legend.position = "none")

(spatial_plot) 
# ggsave("figures/state_spatialrep.jpg", width = 6.5, height = 6.5, spatial_plot)


habitat_plot <- ggplot(data = all_states_quadrats, 
                       aes(x = reorder(state, euclidean_distance2), y = habitat_metric)) +
  geom_point(pch=16) +
  geom_segment(aes(xend=state, yend=0)) +
  xlab(NULL) +
  ylab("1 - Distance Between Sampled\nand Mapped Proportions") +
  # ggtitle("D. Habitat Representativeness") +
  coord_flip() +
  # labs(color = "Detailed in report?") +
  theme_minimal() +
  theme(legend.position = "none")

(habitat_plot)
# ggsave("figures/state_habitatrep.jpg", width = 6.5, height = 6.5, habitat_plot)

# assemble quadrat figure
grid.arrange(nrow=2, ncol=2,
             quantity_plot, quality_plot, 
             spatial_plot, habitat_plot)

grobs <- arrangeGrob(nrow=2, ncol=2,
                     quantity_plot, quality_plot, 
                     spatial_plot, habitat_plot,
                     left = textGrob("Lower to Higher Ranking", rot = 90, vjust = 1))

# ggsave("figures/state_quadrat_figure.jpg", dpi=300, width = 6.5, height = 6.5, grobs)
# ggsave("figures/state_quadrat_figure.pdf", width = 6.5, height = 6.5, grobs)

