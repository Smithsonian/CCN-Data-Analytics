# Check impact classes

source("resources/refresh_data.R")

core_impacts <- impacts %>% drop_na(core_id) %>% 
  filter(impact_class != "NA") %>% 
  group_by(study_id, core_id) %>% 
  summarize(impact_class = paste(impact_class,collapse=', ')) %>% 
  select(study_id, core_id, impact_class) %>% ungroup()

site_impacts <- impacts %>% filter(is.na(core_id)) %>% 
  select(study_id, site_id, impact_class) %>% 
  rename(site_impact = impact_class) %>% 
  group_by(study_id, site_id) %>% 
  summarize(site_impact = paste(site_impact,collapse=', '))  %>% ungroup()

core_impact_smry <- cores %>% 
  left_join(core_impacts) %>% 
  left_join(site_impacts) %>% 
  mutate(impact_class = coalesce(impact_class, site_impact)) %>% 
  select(study_id, site_id, core_id, impact_class) %>% 
  count(impact_class) %>% arrange(desc(n))

