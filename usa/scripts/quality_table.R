## Analysis for Paper
## Jaxine Wolfe

library(tidyverse)

# read in data
cores <- read_csv("usa/data/derived/CONUS_cores.csv", guess_max = 7000)

## Quality metric table

# assign value of 1 to quality code observations (if they are present)
# so they can be tallied for report summary tables
core_qualities <- cores %>% 
  mutate(stocks_count = ifelse(!is.na(stocks_qual_code), 1, NA),
         dates_count = ifelse(!is.na(dates_qual_code), 1, NA),
         elev_count = ifelse(!is.na(elevation_qual_code), 1, NA)) %>% 
  # create column to indicate cases where cores didn't meet any qualifications
  mutate(na_qual_code = ifelse(is.na(stocks_count) & is.na(dates_count) & is.na(elev_count),
                               1, NA)) %>% 
  group_by(state) %>% 
  # Get counts by utility class
  summarise(`carbon stock` = sum(stocks_count, na.rm=T),
            `dated cores` = sum(dates_count, na.rm=T),
            `elevation measurements` = sum(elev_count, na.rm=T),
            `other utility` = sum(na_qual_code, na.rm = T),
            Total = n()) %>% 
  # Arrange in descending order for total number of cores
  arrange(-Total)
# The total is a bit deceiving because cores can fall under more than one category

# plot
plot_quality <- core_qualities %>% 
  select(-`other utility`, -Total) %>% 
  mutate(state = state.abb[match(state,state.name)]) %>% 
  mutate(state = fct_reorder(state, `carbon stock`)) %>% 
  pivot_longer(-state, names_to = "utility", values_to = "n_cores")

# green: #2D8F45
# brown: #825A30
ggplot(plot_quality, aes(state, n_cores)) +
  geom_col(fill = "#2A409A") +
  coord_flip() +
  facet_wrap(~utility, scales = "free_x",  nrow = 1) +
  ylab("Number of Cores") + xlab("State") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none")

ggsave("figures/core_quality.jpg")

# Write to file
write_csv(core_qualities, "data/tables/state_core_quality.csv")
