library(tidyverse)
library(RColorBrewer)

df <- read_csv("scripts/heatmap-ggplot-example/example_df.csv")

df %>%
  mutate(directory = recode(directory,
                            "L1_directory" = "L1",
                            "L2_directory" = "L2",
                            "L3_directory" = "L3")) %>%
  ggplot(aes(id, directory, fill = status)) +
  geom_tile(aes(width=0.85, height=0.85)) +
  coord_equal() +
  scale_x_continuous(breaks = c(1, 10), labels = c("0%", "100%")) +
  scale_fill_manual(values = c("#c7eafe", "#0e5388")) +
  facet_wrap(vars(project), ncol = 1) + 
  theme_minimal() +
  theme(axis.title = element_blank(),
        text = element_text(size = 18),
        legend.position = "bottom") 


## try it out with the report card data
scores <- read_csv("scripts/heatmap-ggplot-example/ccn_report_card_data.csv")

scores %>%
  mutate(metric = factor(metric, c("Total", "Quantity", "Quality", "Spatial coverage", "Habitat coverage"))) %>% 
  mutate(state = factor(state, rankings$state),
         normalized_rank = as.character(normalized_rank)) %>% 
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

ggsave("figures/report_card_variation.jpg")
