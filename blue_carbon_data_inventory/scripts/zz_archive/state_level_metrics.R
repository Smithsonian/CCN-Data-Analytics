## CCN Blue Carbon Data Inventory ####

## contact: Jaxine Wolfe, wolfejax@si.edu
## date: 11-10-2021

# Script creates national overview of state metrics and rankings

# Prepare workspace
library(tidyverse)
library(RColorBrewer)
library(gridExtra)
library(grid)

# Read in necessary data
cores <- read_csv("usa/data/derived/CONUS_cores.csv", guess_max = 7000)
wetland_area <- read_csv("usa/data/derived/ccap_state_wetland_area.csv")

## Ranking Metric Scores ####

# Quantity Metric
quantity <- cores %>% 
  group_by(state) %>% 
  summarise(n = n()) %>% # calculate number of cores per state
  ungroup() %>% 
  mutate(state = state.abb[match(state,state.name)]) %>% 
  left_join(wetland_area) %>% 
  mutate(cores_per_1000ha = n / estimated_area_ha * 1000) %>% # calculate number of cores per 1000ha of wetland in each states
  arrange(-cores_per_1000ha) %>% 
  mutate(quantity_metric_rank = row.names(.), # rank states by metric
         # calculate weighted metric for each state
         quantity_metric_weight = cores_per_1000ha/sum(cores_per_1000ha))

ranked_quantity <- quantity %>% 
  select(state, quantity_metric_rank, quantity_metric_weight)

# Quality metric
quality <- cores %>% 
  # count the number of cores per state with good quality data (for stocks, dates, or elevation)
  filter(stocks_qual_code == "C1" | 
           dates_qual_code == "B1" | 
           elevation_qual_code %in% c("A1", "A2")) %>% 
  group_by(state) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(state = state.abb[match(state,state.name)]) %>% 
  left_join(wetland_area) %>% 
  mutate(cores_per_1000ha = n / estimated_area_ha * 1000) %>% 
  arrange(-cores_per_1000ha) %>% 
  mutate(quality_metric_rank = row.names(.),
         quality_metric_weight = cores_per_1000ha/sum(cores_per_1000ha))

ranked_quality <- quality %>% 
  select(state, quality_metric_rank, quality_metric_weight)

# Spatial Representativeness Metric
spatial_rep <- spatial_rep_by_state %>% 
  # read_csv("data/derived/spatial_rep_by_state.csv") %>% 
  arrange(-spatial_representativeness_metric) %>% 
  mutate(state = state.abb[match(state,state.name)]) %>% 
  mutate(spatial_metric_rank = row.names(.),
         spatial_metric_weight = spatial_representativeness_metric/sum(spatial_representativeness_metric))

ranked_spatial <- spatial_rep %>% 
  select(state, spatial_metric_rank, spatial_metric_weight)

# Habitat Representativeness Metric
habitat_rep <- read_csv("data/derived/habitat_representativeness_metric.csv") %>% 
  rename(habitat_metric = euclidean_distance) %>%
  arrange(-habitat_metric) %>% 
  mutate(habitat_metric_rank = row.names(.),
         habitat_metric_weight = habitat_metric / sum(habitat_metric))

ranked_habitat <- habitat_rep %>% 
  select(state, habitat_metric_rank, habitat_metric_weight)

# Calculate composite score from rankings ####
join_ranks <- ranked_quantity %>% 
  full_join(ranked_quality) %>% 
  full_join(ranked_spatial) %>% 
  full_join(ranked_habitat) %>% 
  # set the maximum rank
  mutate_at(vars(quantity_metric_rank, quality_metric_rank, spatial_metric_rank, habitat_metric_rank), 
            ~ifelse(is.na(.), 23, .))

# calculate composite score
rankings <- join_ranks %>% 
  mutate_at(vars(-state), as.numeric) %>% 
  mutate(total_metric_rank = (quantity_metric_rank + quality_metric_rank + spatial_metric_rank + habitat_metric_rank) / 4,
         total_metric_weight = quantity_metric_weight + quality_metric_weight + spatial_metric_weight + habitat_metric_weight) %>% 
  arrange(-total_metric_rank)

# Normalized average metric weight was not used for the final figure
  # mutate(total_metric_weight = total_metric_weight / sum(total_metric_weight)) %>%
  # arrange(-total_metric_weight)

# rankings$total_metric_weight <- rankings$total_metric_weight / sum(rankings$total_metric_weight)

# rankings <- rankings %>%
#   arrange(-total_metric_weight)

# all_data_weights <- rankings %>% 
#   select(state, quantity_metric_weight, quality_metric_weight, spatial_metric_weight, habitat_metric_weight, total_metric_weight) %>% 
#   mutate(state = factor(state, rev(rankings$state))) %>% 
#   gather(value = "weight", key = "metric", -state) %>% 
#   mutate(metric = str_remove_all(metric, "_metric_weight")) %>% 
#   filter(weight > 0)

all_data_ranks <- rankings %>% 
  select(state, quantity_metric_rank, quality_metric_rank, spatial_metric_rank, habitat_metric_rank, total_metric_rank) %>% 
  mutate(state = factor(state, rankings$state)) %>% 
  gather(value = "rank", key = "metric", -state) %>% 
  mutate(metric = str_remove_all(metric, "_metric_rank"),
         metric = str_to_sentence(metric),
         metric = recode(metric,"Spatial" = "Spatial coverage", "Habitat" = "Habitat coverage"),
         metric = factor(metric, c("Total", "Quantity", "Quality", "Spatial coverage", "Habitat coverage")),
         rank = round(rank,0))

final_ranks <- all_data_ranks %>% 
  mutate(rank = ifelse(state %in% c("DC", "PA"), NA, rank)) %>% 
  group_by(metric) %>%
  mutate(min_rank = min(rank, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(normalized_rank = round((rank-min_rank)/(23-min_rank)*6)) %>% 
  select(-min_rank, -rank)

# write_csv(final_ranks, "data/derived/ccn_report_card_data.csv")

# Create Report Card Figure ####

ggplot(final_ranks, aes(y = metric, x = state)) +
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

ggsave("figures/blue_carbon_report_card.jpg", width=5, height=5, dpi=300)

# write_csv(final_ranks, "data/derived/report_card_metrics.csv")


## State Quadrat Preparation ####

# Quantity Metric
quantity_metric <- quantity %>% 
  rename(quantity_metric = cores_per_1000ha) %>% 
  arrange(-quantity_metric) %>% 
  select(state, quantity_metric)

# Quality metric
quality_metric <- quality %>% 
  rename(quality_metric = cores_per_1000ha) %>% 
  arrange(-quality_metric) %>% 
  select(state, quality_metric) %>% 
  # add missing coastal states
  add_row(state = wetland_area$state[! wetland_area$state %in% quality$state],
          quality_metric = NA)

# Spatial Representativeness
spatial_rep_metric <- spatial_rep %>% 
  select(state, spatial_representativeness_metric) %>% 
  add_row(state = wetland_area$state[! wetland_area$state %in% spatial_rep$state],
          spatial_representativeness_metric = NA) %>% 
  # convert 0 values to NA
  mutate(spatial_representativeness_metric = na_if(spatial_representativeness_metric, 0))
  
# Habitat Representativeness

habitat_rep_metric <- habitat_rep %>% 
  select(state, habitat_metric) %>% 
  add_row(state = wetland_area$state[! wetland_area$state %in% habitat_rep$state],
          habitat_metric = NA) %>% 
  # convert 0 values to NA
  mutate(habitat_metric = ifelse(state == "PA" | state == "DC", NA, habitat_metric))

# join into a single data frame
all_states_quadrats <- quantity_metric %>% 
  full_join(quality_metric) %>% 
  full_join(spatial_rep_metric) %>% 
  full_join(habitat_rep_metric)
# write this table to folder?

# recode NA metric values as 0
all_states_quadrats <- all_states_quadrats %>% 
  mutate(quantity_metric2 = ifelse(is.na(quantity_metric), 0, quantity_metric),
         quality_metric2 = ifelse(is.na(quality_metric), 0, quality_metric),
         spatial_representativeness_metric2 =  ifelse(is.na(spatial_representativeness_metric), 0, spatial_representativeness_metric),
         euclidean_distance2 = ifelse(is.na(habitat_metric), 0, habitat_metric))

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
ggsave("figures/state_quantity.jpg", width = 6.5, height = 6.5, quantity_plot)


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
ggsave("figures/state_quality.jpg", width = 6.5, height = 6.5, quality_plot)


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
ggsave("figures/state_spatialrep.jpg", width = 6.5, height = 6.5, spatial_plot)


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
ggsave("figures/state_habitatrep.jpg", width = 6.5, height = 6.5, habitat_plot)

# assemble quadrat figure
grid.arrange(nrow=2, ncol=2,
             quantity_plot, quality_plot, 
             spatial_plot, habitat_plot)

grobs <- arrangeGrob(nrow=2, ncol=2,
                     quantity_plot, quality_plot, 
                     spatial_plot, habitat_plot,
                     left = textGrob("Lower to Higher Ranking", rot = 90, vjust = 1))

ggsave("figures/state_quadrat_figure.jpg", dpi=300, width = 6.5, height = 6.5, grobs)
ggsave("figures/state_quadrat_figure.pdf", width = 6.5, height = 6.5, grobs)

