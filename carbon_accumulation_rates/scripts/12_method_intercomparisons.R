# Join lit-review and automated 
library(tidyverse)

ccn_cars <- read_csv("carbon_accumulation_rates/output/tabs/ccn_car_210Pb.csv")

ccn_cars_cs <- read_csv("carbon_accumulation_rates/output/tabs/cs_and_horizon_car.csv")

lit_all <- read_csv("coastal_wetland_nggi/report/lit_review_processed_all.csv")

# Pb-210 
{
  ccn_cars2 <- ccn_cars %>%  select(study_id, site_id, core_id, habitat, admin_division, 
                                    carbon_accumulation_mean, carbon_accumulation_lower_CI, 
                                    carbon_accumulation_upper_CI)
  lit_all2 <- lit_all %>% 
    select(study_id, site_id, core_id, habitat, pb210_CAR)
  
  lit_all_cores <- lit_all2 %>% 
    filter(complete.cases(study_id, site_id, core_id))
  
  join_cores <- ccn_cars2 %>% 
    left_join(lit_all_cores) %>% 
    filter(complete.cases(pb210_CAR, carbon_accumulation_mean))
  
  lit_only_sites <- lit_all2 %>% 
    filter(!is.na(study_id)&!is.na(site_id)&is.na(core_id),
           complete.cases(pb210_CAR)) %>% 
    select(-core_id)
  
  join_sites <- ccn_cars2 %>% 
    filter(study_id %in% lit_only_sites$study_id,
           complete.cases(carbon_accumulation_mean)) %>% 
    left_join(lit_only_sites, relationship = "many-to-many") %>% 
    filter(complete.cases(pb210_CAR, carbon_accumulation_mean))
  
  all_comps <- bind_rows(join_sites, join_cores)
  
}

# Cs-137 and horizon 
{
  ccn_cars_cs_hh <- ccn_cars_cs %>%  
    left_join(cores) %>% 
    select(study_id, site_id, core_id, habitat, admin_division,
           carbon_accumulation_mean, carbon_accumulation_lower_CI, 
           carbon_accumulation_upper_CI)
  
  lit_cs <- lit_all %>% 
    select(study_id, site_id, core_id, admin_division, habitat, cs137_CAR)
  
  lit_cs_cores <- lit_cs %>% 
    filter(complete.cases(study_id, site_id, core_id))
  
  join_cores_cs <- ccn_cars_cs_hh %>% 
    left_join(lit_cs_cores) %>% 
    filter(complete.cases(cs137_CAR, carbon_accumulation_mean))
  
  lit_cs_sites <- lit_cs %>% 
    filter(!is.na(study_id)&!is.na(site_id)&is.na(core_id),
           complete.cases(cs137_CAR)) %>% 
    select(-core_id) 
    
  join_sites_cs <- ccn_cars2 %>% 
    filter(study_id %in% lit_cs_sites$study_id) %>% 
    left_join(lit_cs_sites) %>% 
    filter(complete.cases(cs137_CAR, carbon_accumulation_mean))
  
  core_and_site_comps_cs <- bind_rows(join_cores_cs, join_sites_cs)
  
}

ggplot(join_cores, aes(x = pb210_CAR, y = carbon_accumulation_mean, color = admin_division)) +
  geom_abline(intercept=0, slope=1, color = "white", lty=2) +
  geom_segment(aes(xend=pb210_CAR, y = carbon_accumulation_lower_CI, yend=carbon_accumulation_upper_CI)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Literature Review") +
  ylab("Plum Output") +
  theme_dark() +
  ggtitle(expression(paste(""^"210","Pb-based Carbon Accumulation Rate (gC m"^"-2", " yr"^"-1",")"))) +
  theme(legend.title = element_blank())

ggplot(core_and_site_comps_cs, aes(x = cs137_CAR, y = carbon_accumulation_mean, color = admin_division)) +
  geom_abline(intercept=0, slope=1, color = "white", lty=2) +
  geom_segment(aes(xend=cs137_CAR, y = carbon_accumulation_lower_CI, yend=carbon_accumulation_upper_CI)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Literature Review") +
  ylab("Plum Output") +
  theme_dark() +
  ggtitle(expression(paste(""^"210","Pb-based Carbon Accumulation Rate (gC m"^"-2", " yr"^"-1",")"))) +
  theme(legend.title = element_blank())

pb_comps <- join_cores %>% 
  rename(`literature values` = pb210_CAR,
         `algorithm outputs` = carbon_accumulation_mean) %>% 
  mutate(date_type = "{}^210~Pb")
cs_comps <- core_and_site_comps_cs %>% 
  rename(`literature values` = cs137_CAR,
         `algorithm outputs` = carbon_accumulation_mean) %>% 
  mutate(date_type = "{}^137~Cs")

graph_intercomparison <- pb_comps %>% 
  bind_rows(cs_comps)

ggplot(graph_intercomparison, aes(x = `literature values`, y = `algorithm outputs`, color = admin_division)) +
  geom_abline(intercept=0, slope=1, color = "white", lty=2) +
  geom_segment(aes(xend=`literature values`, y = carbon_accumulation_lower_CI, yend=carbon_accumulation_upper_CI)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(.~date_type, labeller = label_parsed) +
  theme_dark() +
  ggtitle(expression(paste("Carbon Accumulation Rate (gC m"^"-2", " yr"^"-1",")"))) +
  theme(legend.title = element_blank(), legend.position = "bottom") +
  guides(color=guide_legend(nrow=3))

ggsave("carbon_accumulation_rates/output/figs/Comparison of Lit-Review and Automated Output.pdf", width = 6.33, height = 4)



core_and_site_comps_cs <- core_and_site_comps_cs %>% 
  mutate(log_carbon_accumulation_mean = log(carbon_accumulation_mean),
         log_cs137_CAR = log(cs137_CAR))
lm2 <- lm(log_carbon_accumulation_mean~log_cs137_CAR, data = core_and_site_comps_cs)
summary(lm2)

mean(core_and_site_comps_cs$log_cs137_CAR-core_and_site_comps_cs$log_carbon_accumulation_mean)

core_and_site_comps_cs2 <- core_and_site_comps_cs %>% 
  mutate(recreated_yn = ifelse(cs137_CAR >= carbon_accumulation_lower_CI & cs137_CAR <= carbon_accumulation_upper_CI,
                               "yes",
                               "no"))

how_many_recreated_cs <- core_and_site_comps_cs2 %>% 
  group_by(recreated_yn) %>% 
  summarise(n = n())

how_many_recreated_cs$pct <- how_many_recreated_cs$n / sum(how_many_recreated_cs$n) * 100
(how_many_recreated_cs)

join_cores <- join_cores %>% 
  mutate(log_carbon_accumulation_mean = log(carbon_accumulation_mean),
         log_pb210_CAR = log(pb210_CAR))
lm1 <- lm(log_carbon_accumulation_mean~log_pb210_CAR, data = join_cores)
summary(lm1)

mean(join_cores$log_pb210_CAR-join_cores$log_carbon_accumulation_mean)

join_cores2 <- join_cores %>% 
  mutate(recreated_yn = ifelse(pb210_CAR >= carbon_accumulation_lower_CI & pb210_CAR <= carbon_accumulation_upper_CI,
                               "yes",
                               "no"))

how_many_recreated <- join_cores2 %>% 
  group_by(recreated_yn) %>% 
  summarise(n = n())

how_many_recreated$pct <- how_many_recreated$n / sum(how_many_recreated$n) * 100
(how_many_recreated)

