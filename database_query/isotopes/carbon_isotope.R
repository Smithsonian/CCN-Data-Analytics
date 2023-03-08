## CCRCN Data Analytics

source("resources/refresh_data.R")

## Isotope Data ####

isodata <- depthseries %>% drop_na(delta_c13) %>% 
  left_join(cores %>% select(study_id, site_id, core_id, habitat, country)) %>% 
  mutate(habitat = recode(habitat, "scrub shrub" = "scrub/shrub")) %>% 
  select(study_id, site_id, core_id, depth_min, depth_max, delta_c13, habitat, country)

isodata %>% filter(depth_max < 30) %>% 
  ggplot(aes(delta_c13, col = habitat)) +
  geom_density() +
  theme_bw()


isodata %>% 
  # filter(depth_max <= 100) %>% 
  filter(habitat == "marsh") %>% 
  ggplot(aes(x = depth_min, y = delta_c13)) +
  geom_line() +
  # geom_segment(aes(x = depth_min, y = delta_c13_mean - delta_c13_sd,
  # xend = depth_min, yend = delta_c13_mean + delta_c13_sd), col = "gray") +
  geom_point(size = 2, pch=21, fill="white") +
  scale_x_reverse() +
  ylab("δ13C (‰)") +
  xlab("Depth (cm)") +
  coord_flip() +
  theme_bw() +
  facet_wrap(~core_id)

