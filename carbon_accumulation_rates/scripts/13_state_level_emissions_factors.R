# Calculate and visualize state level emissions factors

library(tidyverse)

impacts <- read_csv("data/CCN_impacts.csv") %>% 
  distinct_all()

ccn_cars <- read_csv("carbon_accumulation_rates/output/tabs/ccn_car_210Pb.csv") %>% 
  filter(!is.na(carbon_accumulation_mean),
         country == "United States",
         habitat %in% c("marsh", "mangrove", "swamp", "seagrass")) %>% 
  left_join(impacts) %>% 
  mutate(impact_class = ifelse(is.na(impact_class), "natural", impact_class),
         impact_class2 = recode(impact_class,
                                "ditched"="drained",
                                "tidally restored"="restored",
                                "revegetated" = "restored",
                                "managed impounded"="impounded",
                                "tidally restricted"="impounded",
                                "salt impacted"="natural",
                               "diked and drained"="drained")) 

state_meds_all <- ccn_cars %>% 
  group_by(admin_division, habitat, impact_class2) %>% 
  summarise(median_car = median(carbon_accumulation_mean, na.rm = T),
            mean_car = mean(carbon_accumulation_mean, na.rm = T),
            sd = sqrt(sum(carbon_accumulation_se^2, na.rm = T))/n(),
            n = n(),
            upper_CI = quantile(carbon_accumulation_mean, 0.975),
            lower_CI = quantile(carbon_accumulation_mean, 0.025), 
            median_accretion = median(accretion_yr_per_cm_mean)) %>% 
  arrange(admin_division, habitat, impact_class2) %>% 
  mutate(se = sd/sqrt(n),
         CV = sd/mean_car) %>% 
  ungroup()

write_csv(state_meds_all, "carbon_accumulation_rates/output/tabs/Pb210_based_CARs_state_habitat_impact_summary.csv")

state_meds_nat <- ccn_cars %>% 
  filter(impact_class2 == "natural") %>% 
  group_by(admin_division, habitat) %>% 
  summarise(median_car = median(carbon_accumulation_mean, na.rm = T),
            mean_car = mean(carbon_accumulation_mean, na.rm = T),
            sd = sd(carbon_accumulation_mean, na.rm = T),
            n = n(),
            upper_CI = quantile(carbon_accumulation_mean, 0.975),
            lower_CI = quantile(carbon_accumulation_mean, 0.025)) %>% 
  arrange(-median_car) %>% 
  ungroup() %>% 
  mutate('emissions factor' = 107.7494,
         IPCC_ef = "U.S. inventory\nemissions factor")

order_these <- state_meds_nat %>% 
  group_by(admin_division) %>% 
  summarise(median_car = max(median_car)) %>% 
  arrange(-median_car)

ccn_cars_natural <- ccn_cars %>% 
  filter(impact_class2 == "natural")

ccn_cars_natural$admin_division <- factor(ccn_cars_natural$admin_division,
                                  levels = order_these$admin_division)

state_meds_nat$admin_division <- factor(state_meds_nat$admin_division,
                                  levels = order_these$admin_division)


cbp2 <- (c("#E69F00", 
           "#56B4E9", 
           # "#009E73",
           # "#F0E442", 
           "#0072B2", 
           "#D55E00", 
           "#CC79A7"))

ggplot(ccn_cars_natural, aes(x = carbon_accumulation_mean)) +
  geom_density(aes(fill = habitat)) +
  facet_wrap(.~admin_division) +
  scale_x_log10() +
  geom_vline(data = state_meds_nat, aes(xintercept = median_car, color = habitat), lty = 2) +
  geom_vline(data = state_meds_nat, aes(lty = IPCC_ef,
                                        xintercept = `emissions factor`
                                        ),   color = "black")  +
  xlab(expression(paste(""^"210","Pb-based Carbon Accumulation Rate (gC m"^"-2", " yr"^"-1",")"))) +
  labs(color="Median", fill = "Distribution", lty = element_blank()) +
  scale_fill_manual(values = cbp2) +
  scale_color_manual(values = cbp2)

ggsave("carbon_accumulation_rates/output/figs/State Level Carbon Burial Distribution Pb.pdf", width = 7.25, height = 5)

### 137 Cs
ccn_cars2 <- read_csv("carbon_accumulation_rates/output/tabs/cs_and_horizon_car.csv") %>% 
  filter(!is.na(carbon_accumulation_mean),
         country == "United States",
         habitat %in% c("marsh", "mangrove", "swamp", "seagrass")) %>% 
  left_join(impacts) %>% 
  mutate(impact_class = ifelse(is.na(impact_class), "natural", impact_class),
         impact_class2 = recode(impact_class,
                                "ditched"="drained",
                                "tidally restored"="restored",
                                "revegetated" = "restored",
                                "managed impounded"="impounded",
                                "tidally restricted"="impounded",
                                "salt impacted"="natural",
                                "diked and drained"="drained",
                                "storm or wind" = "natural")) %>% 
  distinct_all()

state_meds_all2 <- ccn_cars2 %>% 
  group_by(admin_division, habitat, impact_class2) %>% 
  summarise(median_car = median(carbon_accumulation_mean, na.rm = T),
            mean_car = mean(carbon_accumulation_mean, na.rm = T),
            sd = sd(carbon_accumulation_mean, na.rm = T),
            n = n(),
            upper_CI = quantile(carbon_accumulation_mean, 0.975),
            lower_CI = quantile(carbon_accumulation_mean, 0.025)) %>% 
  arrange(admin_division, habitat, impact_class2) %>% 
  ungroup()

write_csv(state_meds_all2, "carbon_accumulation_rates/output/tabs/Cs137_and_Horizon_based_CARs_state_habitat_impact_summary.csv")

state_meds_nat2 <- ccn_cars2 %>% 
  filter(impact_class2 == "natural") %>% 
  group_by(admin_division, habitat) %>% 
  summarise(median_car = median(carbon_accumulation_mean, na.rm = T),
            mean_car = mean(carbon_accumulation_mean, na.rm = T),
            sd = sd(carbon_accumulation_mean, na.rm = T),
            n = n(),
            upper_CI = quantile(carbon_accumulation_mean, 0.975),
            lower_CI = quantile(carbon_accumulation_mean, 0.025)) %>% 
  arrange(-median_car) %>% 
  ungroup() %>% 
  mutate('emissions factor' = 107.7494,
         IPCC_ef = "U.S. inventory\nemissions factor")

order_these2 <- state_meds_nat2 %>% 
  group_by(admin_division) %>% 
  summarise(median_car = max(median_car)) %>% 
  arrange(-median_car)

ccn_cars_natural2 <- ccn_cars2 %>% 
  filter(impact_class2 == "natural")

ccn_cars_natural2$admin_division <- factor(ccn_cars_natural2$admin_division,
                                          levels = order_these2$admin_division)

state_meds_nat2$admin_division <- factor(state_meds_nat2$admin_division,
                                        levels = order_these2$admin_division)


ggplot(ccn_cars_natural2, aes(x = carbon_accumulation_mean)) +
  geom_density(aes(fill = habitat)) +
  facet_wrap(.~admin_division) +
  scale_x_log10() +
  geom_vline(data = state_meds_nat2, aes(xintercept = median_car, color = habitat), lty = 2) +
  geom_vline(data = state_meds_nat2, aes(lty = IPCC_ef,
                                        xintercept = `emissions factor`
  ),   color = "black")  +
  xlab(expression(paste("Horizon-based Carbon Accumulation Rate (gC m"^"-2", " yr"^"-1",")"))) +
  labs(color="Median", fill = "Distribution", lty = element_blank()) +
  scale_fill_manual(values = cbp2) +
  scale_color_manual(values = cbp2)

ggsave("carbon_accumulation_rates/output/figs/State Level Carbon Burial Distribution Horizon.pdf", width = 7.25, height = 5)
