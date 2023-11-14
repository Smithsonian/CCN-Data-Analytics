## State-Specifics

# source inventorying functions
source("blue_carbon_data_inventory/scripts/inventory_functions.R")

# create table with proportions of sampled and mapped habitat per state
full_mapped_and_core_props_long <- habitatProportions(cores_v2) %>% 
  gather(key = "sampled_v_area", value = "proportion", -c(state, habitat_simplified)) %>% 
  mutate(sampled_v_area = str_replace_all(sampled_v_area, "state_core_proportion", "sampled"),
         sampled_v_area = str_replace_all(sampled_v_area, "mapped_proportion", "estimated area"),
         state = factor(state, rev(unique(full_mapped_and_core_proprtions$state))))
  # rename(state_abbrv = state) 
  # left_join(state_lookup)

# create a custom color scale that will be used in the state reports as well
habs <- sort(unique(full_mapped_and_core_props_long$habitat_simplified))
hab_colorbrew <- c("#FC8D62", "#8DA0CB","#A6D854", "#FFD92F", "#E78AC3") # set 2 (colorblind friendly)
names(hab_colorbrew) <- habs # associate habitats with the colors
hab_colors <- scale_fill_manual(name = "habitat", values = hab_colorbrew) # create custom fill
# alternatives:
# hab_colorbrew <- brewer.pal(length(unique(pew_core_habitat$habitat)), "Set2")
# hab_colorbrew <- c("#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5", "#8DD3C7", "#BEBADA", "#FFFFB3", "#FB8072") # set 3 

ggplot(full_mapped_and_core_props_long, aes(x=state, y=proportion, fill=habitat_simplified)) +
  geom_bar(stat = "identity") +
  hab_colors +
  facet_wrap(.~sampled_v_area) +
  xlab(NULL) +
  coord_flip() +
  theme_minimal() +
  # labs(caption = fig_citation, fill = element_blank(), y = "Proportion") +
  ggtitle("State-level Comparison of Sampled and Mapped Habitat") +
  theme(axis.text.y = element_text(size = 12))
# ggsave(paste0(fig_dir, "CONUS_sampled_v_area_habitat.jpg"))


### AHHHH #####

# compare sampled to mapped habitat representation
full_mapped_and_core_props_long %>% 
  filter(state == "WA") %>%
  mutate(habitat_simplified = fct_reorder(habitat_simplified, proportion)) %>%
  # plot
  ggplot(aes(x = habitat_simplified, y = proportion, fill = habitat_simplified)) +
  geom_col(position = "dodge", show.legend = F) +
  facet_grid(~sampled_v_area) +
  hab_colors +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  coord_flip() +
  theme_bw() +
  theme(axis.text.y = element_text(size = 10)) +
  ylab("Proportion") + xlab("Habitat")
  # labs(caption = fig_citation) +
  # ggtitle(str_c(temp_state, "State Estimated and Sampled Habitat", sep = " "))


## Tiers

# gather the quality tiers into long form
tier_gather <- cores_v2 %>% 
  # Should this figure have pew states or all CONUS cores?
  # filter(state %in% pew_states) %>%
  gather(key = "tier", value = "sub_tier", 
         -c(study_id:habitat, country, state, max_depth)) %>%
  drop_na(sub_tier) %>%
  mutate(tier = str_remove(tier, "_qual_code")) %>%
  # add aids for plotting
  mutate(utility = recode(tier, 
                          "stocks" = "stock assessment",
                          "dates" = "carbon accretion rates",
                          "elevation" = "carbon sequestration modeling"),
         tier_level = recode(sub_tier, 
                             "C2" = "Good",
                             "B2" = "Good",
                             "A3" = "Good",
                             "A2" = "Better",
                             "C1" = "Best",
                             "B1" = "Best",
                             "A1" = "Best"))

# create a custom pallete
library(RColorBrewer)
# create a custom color scale that will be used in the state reports as well
tier_colorbrew <- brewer.pal(length(unique(tier_gather$tier_level)), "YlGnBu")
names(tier_colorbrew) <- unique(tier_gather$tier_level)
tier_colors <- scale_fill_manual(name = "Completeness\nand Qualtiy", values = tier_colorbrew)

# prepare data for plotting
conus_tiers <- tier_gather %>%
  # add_count(tier) %>%
  group_by(utility, tier_level) %>% 
  summarise(n = n()) %>% 
  select(
    #country, tier, 
    utility, tier_level, n) %>%
  ungroup() %>% 
  # distinct() %>%
  mutate(tier_level = fct_relevel(tier_level, "Good", "Better", "Best"),
         utility = fct_relevel(utility, "stock assessment", "carbon accretion rates", "carbon sequestration modeling"))


# Create table with core counts per quality tier and state
conus_state_tiers <- tier_gather %>%
  add_count(state, tier, utility, tier_level, name = "core_count") %>%
  select(state, tier, utility, tier_level, core_count) %>%
  distinct() %>%
  group_by(state) %>%
  mutate(proportion = core_count/sum(core_count))

state_tier <- conus_state_tiers %>% 
  filter(state == "OR") %>%
  mutate(tier_level = fct_relevel(tier_level, "Good", "Better", "Best"),
         utility = fct_relevel(utility, "stock assessment", "carbon accretion rates", "carbon sequestration modeling"))
# some levels are "unknown" because they may not exist for every state

state_total_counts <- state_tier %>% 
  group_by(utility) %>%
  summarise(n2 = sum(core_count, na.rm=T))
# some levels are "unknown" because they may not exist for every state

# bar plot quality tiers
ggplot(state_tier, aes(utility, core_count)) +
  geom_col(aes(fill = tier_level)) + tier_colors +
  geom_text(data = state_total_counts,
            aes(y = n2, label = n2, vjust = -0.5)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  xlab("Data Utility") + ylab("Number of Cores") +
  theme_classic() +
  # labs(caption = fig_citation) +
  theme(text = element_text(size = 12)) 
  # theme(axis.text.x = element_text(angle = 33, hjust=1)) +
  # ggtitle(str_c(temp_state, "State Core Utility, Completeness and Quality", sep = " "))
