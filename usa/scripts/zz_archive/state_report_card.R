library(tidyverse)
library(RColorBrewer)

# Get core data
# Simplify habitat categories
cores <- read_csv("data/derived/CONUS_cores.csv", guess_max = 7000)
wetland_area <- read_csv("data/derived/state_wetland_area_ccap.csv")

# Quantity Metric
quantity_metric <- cores %>% 
  group_by(state) %>% 
  summarise(n = n()) %>% # calculate number of cores per state
  ungroup() %>% 
  mutate(state = state.abb[match(state,state.name)]) %>% 
  left_join(wetland_area) %>% 
  mutate(cores_per_1000ha = n / estimated_area_ha * 1000) %>% # calculate number of cores per 1000ha of wetland in each states
  arrange(-cores_per_1000ha)

quantity_metric$quantity_metric_rank <- 1:nrow(quantity_metric)
# calculate weighted metric for each state
quantity_metric$quantity_metric_weight <- quantity_metric$cores_per_1000ha/sum(quantity_metric$cores_per_1000ha)

quantity_metric <- quantity_metric %>% 
  select(state, quantity_metric_rank, quantity_metric_weight)

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
  mutate(cores_per_1000ha = n / estimated_area_ha * 1000) %>% 
  arrange(-cores_per_1000ha)

quality_metric$quality_metric_rank <- 1:nrow(quality_metric)
quality_metric$quality_metric_weight <- quality_metric$cores_per_1000ha/sum(quality_metric$cores_per_1000ha)

quality_metric <- quality_metric %>% 
  select(state, quality_metric_rank, quality_metric_weight)

# Spatial Representativeness Metric
spatial_rep_metric <- read_csv("data/derived/spatial_rep_by_state.csv") %>% 
  arrange(-spatial_representativeness_metric) %>% 
  mutate(state = state.abb[match(state,state.name)])

spatial_rep_metric$spatial_metric_rank <- 1:nrow(spatial_rep_metric)
spatial_rep_metric$spatial_metric_weight <- spatial_rep_metric$spatial_representativeness_metric/sum(spatial_rep_metric$spatial_representativeness_metric)

spatial_rep_metric <- spatial_rep_metric %>% 
  select(state, spatial_metric_rank, spatial_metric_weight)

# Habitat Representativeness Metric
habitat_rep_metric <- read_csv("data/derived/habitat_representativeness_metric.csv") %>% 
  mutate(habitat_metric = euclidean_distance) %>% 
  arrange(-habitat_metric)

habitat_rep_metric$habitat_metric_rank <- 1:nrow(habitat_rep_metric)
habitat_rep_metric$habitat_metric_weight <- habitat_rep_metric$habitat_metric / sum(habitat_rep_metric$habitat_metric)

habitat_rep_metric <- habitat_rep_metric %>% 
  select(state, habitat_metric_rank, habitat_metric_weight)

all_data <- quantity_metric %>% 
  full_join(quality_metric) %>% 
  full_join(spatial_rep_metric) %>% 
  full_join(habitat_rep_metric)

all_data$quantity_metric_rank[is.na(all_data$quantity_metric_rank)] <- 23
all_data$quality_metric_rank[is.na(all_data$quality_metric_rank)] <- 23
all_data$spatial_metric_rank[is.na(all_data$spatial_metric_rank)] <- 23
all_data$habitat_metric_rank[is.na(all_data$habitat_metric_rank)] <- 23

# calculate composite score
all_data <- all_data %>% 
  mutate(total_metric_rank = (quantity_metric_rank + quality_metric_rank + spatial_metric_rank + habitat_metric_rank) / 4,
         total_metric_weight = quantity_metric_weight + quality_metric_weight + spatial_metric_weight + habitat_metric_weight)

all_data$total_metric_weight <- all_data$total_metric_weight / sum(all_data$total_metric_weight)

all_data <- all_data %>% 
  arrange(-total_metric_weight)

all_data_weights <- all_data %>% 
  select(state, quantity_metric_weight, quality_metric_weight, spatial_metric_weight, habitat_metric_weight, total_metric_weight) %>% 
  mutate(state = factor(state, rev(all_data$state))) %>% 
  gather(value = "weight", key = "metric", -state) %>% 
  mutate(metric = str_remove_all(metric, "_metric_weight")) %>% 
  filter(weight > 0)

all_data <- all_data %>% 
  arrange(-total_metric_rank)

all_data_ranks <- all_data %>% 
  select(state, quantity_metric_rank, quality_metric_rank, spatial_metric_rank, habitat_metric_rank, total_metric_rank) %>% 
  mutate(state = factor(state, all_data$state)) %>% 
  gather(value = "rank", key = "metric", -state) %>% 
  mutate(metric = str_remove_all(metric, "_metric_rank"),
         metric = str_to_sentence(metric),
         metric = recode(metric,"Spatial" = "Spatial coverage", "Habitat" = "Habitat coverage"),
         metric = factor(metric, c("Total", "Quantity", "Quality", "Spatial coverage", "Habitat coverage")),
         rank = round(rank,0))

all_data_ranks <- all_data_ranks %>% 
  mutate(rank = ifelse(state %in% c("DC", "PA"), NA, rank)) %>% 
  group_by(metric) %>%
  mutate(min_rank = min(rank, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(normalized_rank = round((rank-min_rank)/(23-min_rank)*6))

write_csv(all_data_ranks, "data/derived/report_card_metrics.csv")


# Create figure for publication ####

ggplot(all_data_ranks, aes(y = metric, x = state)) +
  geom_raster(aes(fill = as.character(normalized_rank))) +
  scale_fill_brewer(palette = "RdYlBu", direction = - 1,
                    labels = c("Best", "", "", "Fair", "", "", "Poor", "No Data")) +
  geom_hline(aes(yintercept = 1.5), size = 1) +
  ylab(NULL) +
  xlab(NULL) +
  coord_flip() +
  # theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust=1, size = 12),
        legend.title = element_blank()) +
  ggtitle("State-Level Blue Carbon Data Report Card")

# extract the fill colors used by ggplot
# g <- ggplot_build(plot)
# unique(g$data[[1]]["fill"])

ggsave("figures/Blue_Carbon_Report_Card.jpg", width=5, height=5, dpi=300)

# ---------

## Generate the plot
# pew_state_abbrv <- sort(c("OR", "ME", "WA", "MA", "CA", "NY", 
#                      "NJ", "MD", "VA", "FL"))
# 
# pew_color <- ifelse(all_data_ranks$state %in% pew_state_abbrv, "black", "grey")
# 
# ggplot(all_data_ranks, aes(y = metric, x = state)) +
#   geom_raster(aes(fill = as.character(normalized_rank))) +
#   scale_fill_brewer(palette = "RdYlBu", direction = - 1,
#                     labels = c("Best", "", "", "Fair", "", "", "Poor", "No Data")) +
#   geom_hline(aes(yintercept = 1.5), size = 1) +
#   ylab(NULL) +
#   xlab(NULL) +
#   coord_flip() +
#   theme(axis.text.x = element_text(angle = 45, hjust=1, size = 12),
#         axis.text.y = element_text(colour = pew_color),
#         legend.title = element_blank()) +
#   ggtitle("State-Level Blue Carbon Data Report Card")
# 
# ggsave("figures/CONUS_state_report_card.jpg", width=5, height=5, dpi=300)
