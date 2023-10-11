## Script to compare the metrics from different versions
# turn into a function eventually

library(tidyverse)

v1 <- read_csv("database_inventory/tables/state_metrics_v1.csv") %>% 
  rename(spatial_metric = spatial_representativeness_metric)

v2 <- read_csv("database_inventory/tables/state_metrics_v2.csv") %>% 
  # make this more elegant in the future
  rename(quantity_metric_v2 = quantity_metric,
         quality_metric_v2 = quality_metric,
         spatial_metric_v2 = spatial_representativeness_metric,
         habitat_metric_v2 = habitat_metric)

all_metrics <- full_join(v1, v2) %>% 
  mutate(across(everything(), replace_na, 0)) %>% 
  mutate(quant_diff = quantity_metric_v2 - quantity_metric,
         qual_diff = quality_metric_v2 - quality_metric,
         spatial_diff = spatial_metric_v2 - spatial_metric, 
         hab_diff = habitat_metric_v2 - habitat_metric)


# Try a plot

# plotMetricDiffs <- function(metric){
#   
# }

# 
all_metrics %>% 
  mutate(state = fct_reorder(state, quant_diff)) %>% 
  ggplot(aes(state, quant_diff)) +
  geom_point() +
  geom_segment(aes(xend = state, yend = 0)) + coord_flip()
# states with no change in quantity should not have change in the other metrics...

all_metrics %>% 
  mutate(state = fct_reorder(state, qual_diff)) %>% 
  ggplot(aes(state, qual_diff)) +
  geom_point() +
  geom_segment(aes(xend = state, yend = 0)) + coord_flip()

all_metrics %>% 
  mutate(state = fct_reorder(state, spatial_diff)) %>% 
  ggplot(aes(state, spatial_diff)) +
  geom_point() +
  geom_segment(aes(xend = state, yend = 0)) + coord_flip()
# negative means the sampling became more clustered
# positive means it became more well-dispersed

all_metrics %>% 
  mutate(state = fct_reorder(state, hab_diff)) %>% 
  ggplot(aes(state, hab_diff)) +
  geom_point() +
  geom_segment(aes(xend = state, yend = 0)) + coord_flip()
# negative means the sampling became less representative of the mapped estimates
# positive means it became better representative


