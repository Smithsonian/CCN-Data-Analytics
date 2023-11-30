## Program script to run inventorying functions for a given version of the synthesis

library(tidyverse)
library(sf)
# library(rnaturalearth)
# library(rgeos)
# library(gridExtra)

# source inventorying functions
source("blue_carbon_data_inventory/scripts/inventory_functions.R")

# Import Core Data
cores_v1 <- read_csv("blue_carbon_data_inventory/data/synthesis/v1/CONUS_cores.csv", guess_max = 7000)

cores_v2 <- read_csv("https://raw.githubusercontent.com/Smithsonian/CCRCN-Data-Library/develop/data/CCRCN_synthesis/CCRCN_cores.csv", 
                  col_types = cols(.default = "c")) %>% 
  rename(state = admin_division) %>%
  filter(country == "United States") %>%
  filter(state != "Puerto Rico" & state != "Hawaii" & state != "Alaska") %>%
  mutate(habitat = recode(habitat, 
                          "mudflat" = "unvegetated",
                          # we'll be comparing sampled cores to mapped habitat in which seagress, kelp, and algal mats are grouped together
                          "seagrass" = "EAB",
                          "algal mat" = "EAB"))
  ## SPOT FIXES
  # mutate(habitat = case_when(core_id == "ELM1812-MFA1" ~ "mudflat",
  #                            study_id %in% c("Boyd_et_al_2017", "Watson_and_Byrne_2013", "Thom_1992",
  #                                            "Drexler_et_al_2019", "Carlin_et_al_2021") ~ "marsh",
  #                            core_id == "W4" ~ "marsh",
  #                            core_id %in% c("W1", "W2", "W2") ~ "swamp",
  #                            study_id == "Kauffman_et_al_2020" & vegetation_class == "forested" ~ "swamp",
  #                            # "Krauss_et_al_2018" and "Ensign_et_al_2020" is a mix of marsh and swamp..too much work to assign these right now
  #                            T ~ habitat))


## Version 1 Scores ####

# inventory the data and compute metrics
scores_v1 <- compositeMetricScore(cores_v1)

## Create Report Card Figure

ggplot(scores_v1, aes(y = metric, x = state)) +
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

# ggsave("figures/blue_carbon_report_card.jpg", width=5, height=5, dpi=300)

## Alternative Score Card ####

state_rank <- scores_v1 %>% 
  filter(metric == "Total") %>% 
  mutate(normalized_rank = ifelse(is.na(normalized_rank), 5, normalized_rank)) %>% 
  arrange(-normalized_rank)

scores_v1 %>%
  mutate(metric = factor(metric, c("Total", "Quantity", "Quality", "Spatial coverage", "Habitat coverage"))) %>% 
  mutate(state = factor(state, state_rank$state)) %>%
  mutate(normalized_rank = as.character(normalized_rank)) %>% 
  ggplot(aes(metric, state, fill = normalized_rank)) +
  geom_tile(aes(width=0.85, height=0.85)) +
  coord_equal() +
  geom_hline(aes(yintercept = 6.5), size = 0.5, col = "grey") +
  geom_hline(aes(yintercept = 9.5), size = 0.5, col = "grey") +
  geom_hline(aes(yintercept = 17.5), size = 0.5, col = "grey") +
  geom_hline(aes(yintercept = 21.5), size = 0.5, col = "grey") +
  geom_vline(aes(xintercept = 1.5), size = 1, col = "grey") +
  ylab(NULL) + xlab(NULL) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1, 
                    labels = c("Best", "", "", "Fair", "", "", "Poor", "No Data")) +
  # facet_wrap(vars(metric)) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

# ggsave("figures/report_card_variation.jpg")

## Version 2 Scores ####

## Generate V2 Metric Scores

# run functions
quantity <- quantityMetric(cores_v2)
quality <- qualityMetric(cores_v2)
spatial <- spatialMetric(cores_v2)
habitat <- habitatMetric(cores_v2)


# Quantity Metric
quantity_metric <- quantity %>% 
  # rename(quantity_metric = cores_per_1000ha) %>% 
  # arrange(-quantity_metric) %>% 
  select(state, quantity_metric)

# Quality metric
quality_metric <- quality %>% 
  # rename(quality_metric = cores_per_1000ha) %>% 
  # arrange(-quality_metric) %>% 
  select(state, quality_metric)
# add missing coastal states
# add_row(state = wetland_area$state[! wetland_area$state %in% quality$state],
# quality_metric = NA)

# Spatial Representativeness
spatial_rep_metric <- spatial %>% 
  select(state, spatial_representativeness_metric) %>% 
  # add_row(state = wetland_area$state[! wetland_area$state %in% spatial_rep$state],
  # spatial_representativeness_metric = NA) %>% 
  # convert 0 values to NA
  mutate(spatial_representativeness_metric = na_if(spatial_representativeness_metric, 0)) # just PA

# Habitat Representativeness

habitat_rep_metric <- habitat %>% 
  select(state, habitat_metric) %>% 
  # add_row(state = wetland_area$state[! wetland_area$state %in% habitat_rep$state],
  # habitat_metric = NA) %>% 
  # convert 0 values to NA
  mutate(habitat_metric = ifelse(state == "PA" | state == "DC", NA, habitat_metric))

# join into a single data frame
all_states_quadrats <- quantity_metric %>% 
  full_join(quality_metric) %>% 
  full_join(spatial_rep_metric) %>% 
  full_join(habitat_rep_metric)
# write this table to folder?
write_csv(all_states_quadrats, "blue_carbon_data_inventory/tables/state_metrics_v2.csv")


# inventory the data and compute metrics
scores_v2 <- compositeMetricScore(cores_v2)

## Create Report Card Figure ####

ggplot(scores_v2, aes(y = metric, x = state)) +
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

ggsave("blue_carbon_data_inventory/figures/blue_carbon_report_card_V2.jpg", width=5, height=5, dpi=300)


## Render Blue Carbon Data Inventory report

url_date <- format(Sys.time(), "%Y%m%d %H%M")
formated_date <- format(Sys.time(), "%Y/%m/%d-%H:%M")

# generate data contributor report
rmarkdown::render(input = "blue_carbon_data_inventory/scripts/bcdi_report.Rmd",
                  # output_format = "html_document",
                  output_file = paste0("bcdi_report_", url_date),
                  output_dir = "blue_carbon_data_inventory/inventory_reports/")

## Investigate Specific studies 

new_cores <- cores_v2 %>% 
  filter(!(core_id %in% unique(cores_v1$core_id))) 
  # select(study_id, state, habitat, contains("qual_code")) %>% 
  # distinct()

