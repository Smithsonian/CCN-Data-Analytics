library(tidyverse)
library(RColorBrewer)

df <- read_csv("database_inventory/scripts/heatmap-ggplot-example/example_df.csv")

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

