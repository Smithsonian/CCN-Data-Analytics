## Program script to run inventorying functions for a given version of the synthesis

library(tidyverse)
library(sf)
# library(rnaturalearth)
# library(rgeos)
# library(gridExtra)

# source inventorying functions
source("database_inventory/scripts/inventory_functions.R")

# Import Core Data
cores_v1 <- read_csv("database_inventory/data/synthesis/v1/CONUS_cores.csv", guess_max = 7000)
cores_v2 <- sourceSynthesis()

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

ggsave("database_inventory/figures/blue_carbon_report_card_V2.jpg", width=5, height=5, dpi=300)


## Render Blue Carbon Data Inventory report

url_date <- format(Sys.time(), "%Y%m%d %H%M")
formated_date <- format(Sys.time(), "%Y/%m/%d-%H:%M")

# generate data contributor report
rmarkdown::render(input = "./database_inventory/scripts/bcdi_report.Rmd",
                  # output_format = "html_document",
                  output_file = paste0("bcdi_report_", url_date),
                  output_dir = "./database_inventory/inventory_reports/")
