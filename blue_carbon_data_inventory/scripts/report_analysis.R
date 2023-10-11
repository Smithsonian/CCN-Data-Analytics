## CCN Blue Carbon Data Inventory ####

## contact: Jaxine Wolfe, wolfejax@si.edu
## date: 11-10-2021

# Script runs analysis and outputs figures for the publication

## 1. Prepare Workspace ####

library(tidyverse)
library(RColorBrewer)
# library(leaflet)
# library(sf)
# library(RefManageR)
# library(gridExtra)
# library(grid)
# library(stringr)
# library(ggridges)

# make sure core data is current
# source("scripts/update_report_data.R")

# run source scripts for analysis
# source("scripts/habitat_metric.R") # generates habitat proportions table
# source("scripts/spatial_metric.R") # generates spatial rep by state table and map of clustering comparison
# source("scripts/state_level_metrics.R") # generates report card and quadrat figure

# read in data
# state_lookup <- read_csv("resources/tables/state_lookup.csv")
# wetland_area <- read_csv("data/state_wetland_area.csv")
wetland_area <- read_csv("data/derived/ccap_state_wetland_area.csv")
# ccap_wetland_habitat <- read_csv("data/report_data/state_habitat_wetland_area_ccap.csv")
cores <- read_csv("data/derived/CONUS_cores.csv", guess_max = 7000) %>% 
  mutate(state = state.abb[match(state,state.name)])
# state metrics caluclated in Four Quadrat Master Figure
# state_metrics <- read_csv("data/report_data/state_metrics.csv")
restorable_emissions <- read_csv("data/Restorable-Emissions/state_level_summaries.csv")
habitat_ha <- read_csv("data/derived/ccap_state_wetland_area_habitat.csv")

# designation citation for figures
# fig_citation <- "Holmquist et al 2021 (unpublished)"
# and the directory they will be saved in for the report
fig_dir <- "figures/"
tab_dir <- "tables/"


## ... Subset Core Data ####
# store states for pew reports
# pew_states <- sort(c("Oregon", "Maine", "Washington", "Massachusetts", "California", "New York", 
#                      "New Jersey", "Maryland", "Virginia", "Florida"))

# subset core data for cores in pew states
# pew_cores <- cores %>% filter(state %in% pew_states)


## 2. CONUS Overview ####


## ... State Wetland Area ####

# Wetland Area
state_wetland <- wetland_area %>%
  # rename(state_abbrv = state) %>% full_join(state_lookup) %>%
  # mutate(state = state.abb[match(state, state.name)]) %>% 
  mutate(percent_wetland = 100*(estimated_area_ha/sum(estimated_area_ha)))
# write table for reference in the report
write_csv(state_wetland %>% select(state, percent_wetland), "data/report_data/state_pct_wetland.csv")

state_wetland %>%
  # reorder factors to plot in decending order
  mutate(state = fct_reorder(state, percent_wetland)) %>%
  # plot percent total CONUS wetland per state
  ggplot(aes(state, percent_wetland, fill = percent_wetland)) +
  geom_col() +
  # set gradient color palette
  scale_fill_gradient(name = "Percent\nWetland", low = "#b2e2e2", high = "#006d2c") +
  # make sure the text doesnt get cropped
  ylim(0, max(state_wetland$percent_wetland, na.rm = T) + 2) +
  coord_flip() +
  geom_text(aes(label = paste0(round(percent_wetland, 2), "%")), size = 3, hjust = -0.2) +
  ylab("Proportion of Total Tidal Wetland (%)") + xlab("State") +
  theme_classic()

ggsave("figures/CONUS_state_wetland_area.jpg", width = 6.5, height = 6.5)


## ... State Core Counts ####

# Number of unique cores per contiguous state
core_state_smry <- cores %>%
  group_by(country, state) %>%
  summarise(core_count = n()) %>%
  mutate(core_percent = 100*(core_count/sum(core_count))) %>%
  ungroup() %>%
  # reorder factors to plot in descending order
  mutate(state = fct_reorder(state, core_count))

ggplot(core_state_smry, aes(x = state, y = core_count)) +
  geom_col(position = "dodge") + 
  ylim(0, max(core_state_smry$core_count, na.rm = T) + 60) +
  coord_flip() +
  geom_text(aes(label = core_count), size = 3, hjust = -0.1) +
  theme_classic() +
  # theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  ylab("Number of Soil Cores") + xlab("States")
  # labs(caption = fig_citation, fill = "Assessed in Report")
# ggsave(paste0(fig_dir, "CONUS_core_counts.jpg")) # not used in report

# ... Representative Quanity ####

# normalize core count by wetland area
normalized_cores <- cores %>%
  group_by(state) %>% tally() %>%
  ungroup() %>%
  left_join(state_wetland) %>%
  mutate(norm_core_count = n/estimated_area_ha)

normalized_cores %>%
  # mutate(state_category = ifelse(state %in% pew_states, "Yes", "No")) %>%
  # reorder factors to plot in decending order
  mutate(state = fct_reorder(state, norm_core_count)) %>%
  # plot normalized core count per state
  ggplot(aes(state, norm_core_count)) +
  geom_col(position = "dodge") +
  coord_flip() +
  theme_classic() +
  # theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  ylab("Normalized Core Count") + xlab("States")
  # labs(caption = fig_citation, fill = "Assessed in Report")
# ggsave(paste0(fig_dir, "CONUS_core_counts_normalized.jpg")) # not used in report


## ... Spatial Representativeness ####

# # This chunk is redundant since we have the four panel figure now (see Four Quadrat Master Figure.R)
# spatial_rep_by_state <- read_csv("data/report_data/spatial_rep_by_state.csv")
# 
# spatial_rep_by_state$state = factor(spatial_rep_by_state$state,
#                                     levels = spatial_rep_by_state$state)
# 
# spatial_rep_by_state %>%
#   mutate(state_category = ifelse(state %in% pew_states, "Yes", "No")) %>%
#   ggplot(aes(x = state, y = spatial_representativeness_metric, col = state_category)) +
#   geom_point() +
#   geom_segment(aes(xend=state, yend=0)) +
#   xlab(element_blank()) +
#   ylab("Spatial Representativeness Score") +
#   coord_flip() +
#   theme_bw() +
#   labs(caption = fig_citation, col = "Assessed in Report")
# ggsave("figures/CONUS_spatial_rep.jpg")


### ... Habitat Types ####

full_mapped_and_core_proprtions <- read_csv("data/report_data/mapped_and_cored_habitat_proportions.csv")

# create table with proportions of sampled and mapped habitat per state
full_mapped_and_core_props_long <- full_mapped_and_core_proprtions %>% 
  gather(key = "sampled_v_area", value = "proportion", -c(state, habitat_simplified)) %>% 
  mutate(sampled_v_area = str_replace_all(sampled_v_area, "state_core_proportion", "sampled"),
         sampled_v_area = str_replace_all(sampled_v_area, "mapped_proportion", "estimated area"),
         state = factor(state, rev(unique(full_mapped_and_core_proprtions$state)))) %>%
  rename(state_abbrv = state) %>%
  left_join(state_lookup)

# create a custom color scale that will be used in the state reports as well
habs <- sort(unique(full_mapped_and_core_props_long$habitat_simplified))
hab_colorbrew <- c("#FC8D62", "#8DA0CB","#A6D854", "#FFD92F", "#E78AC3") # set 2 (colorblind friendly)
names(hab_colorbrew) <- habs # associate habitats with the colors
hab_colors <- scale_fill_manual(name = "habitat", values = hab_colorbrew) # create custom fill
# alternatives:
# hab_colorbrew <- brewer.pal(length(unique(pew_core_habitat$habitat)), "Set2")
# hab_colorbrew <- c("#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5", "#8DD3C7", "#BEBADA", "#FFFFB3", "#FB8072") # set 3 

ggplot(full_mapped_and_core_props_long, aes(x=state_abbrv, y=proportion, fill=habitat_simplified)) +
  geom_bar(stat = "identity") +
  hab_colors +
  facet_wrap(.~sampled_v_area) +
  xlab(NULL) +
  coord_flip() +
  theme_minimal() +
  labs(caption = fig_citation, fill = element_blank(), y = "Proportion") +
  ggtitle("State-level Comparison of Sampled and Mapped Habitat") +
  theme(axis.text.y = element_text(size = 12))
ggsave(paste0(fig_dir, "CONUS_sampled_v_area_habitat.jpg"))

### ... Habitat Representation ####

# # This chunk is redundant since we have the four panel figure now (see Four Quadrat Master Figure.R)
#
# # read in habitat rep table 
# state_level_euclidean_distance <- read_csv("data/report_data/habitat_representativeness_metric.csv")
# 
# # should be higher for states that are out of sync and lower for states more in sync.
# state_level_euclidean_distance %>%
#   rename(state_abbrv = state) %>%
#   left_join(state_lookup) %>%
#   mutate(state_category = ifelse(state %in% pew_states, "Yes", "No")) %>%
#   ggplot(mapping = aes(x = reorder(state, -euclidean_distance), y = euclidean_distance, col = state_category)) +
#   geom_point(pch=16) +
#   geom_segment(aes(xend=state, yend=0)) +
#   xlab("Top to Bottom; More to Less Represtative") +
#   ylab("Euclidean Distance") +
#   ggtitle("Habitat Representativeness Metric") +
#   coord_flip() +
#   labs(caption = fig_citation, col = "Assessed in Report") +
#   theme_bw()
# ggsave("figures/CONUS_habitat_rep.jpg")


## ... Soil Core Data Utility and Availability ####

# gather the quality tiers into long form
tier_gather <- cores %>% 
  # Should this figure have pew states or all CONUS cores?
  # filter(state %in% pew_states) %>%
  gather(key = "tier", value = "sub_tier", 
         -c(study_id:habitat_assignment_method, country, state, max_depth)) %>%
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

conus_text <- conus_tiers %>% 
  group_by(utility) %>% 
  summarise(n2 = sum(n, na.rm=T))

# bar plot quality tiers
ggplot(conus_tiers, aes(x = utility, y = n)) +
  geom_col(aes(fill = tier_level)) + tier_colors +
  geom_text(data = conus_text, aes(y = n2, label = n2, vjust = -0.5)) +
  xlab("Utility") + 
  ylab("Number of Cores") +
  theme_classic() +
  # coord_flip() +
  # labs(caption = fig_citation) +
  theme(axis.text.x = element_text(angle = 33, hjust = 1)) +
  ggtitle("Data Utility, Completedness and Quality\nAcross All Coastal States")

ggsave(paste0(fig_dir, "CONUS_core_quality.jpg"))


## 3. Generate State Report Figures ####

# Create table with core counts per quality tier and state
conus_state_tiers <- tier_gather %>%
  add_count(state, state_abbrv, tier, utility, tier_level, name = "core_count") %>%
  select(state, state_abbrv, tier, utility, tier_level, core_count) %>%
  distinct() %>%
  group_by(state) %>%
  mutate(proportion = core_count/sum(core_count))

# determine the number of cores per habitat in the database
hab_freq <- cores %>% add_count(habitat) %>% select(habitat, n) %>% distinct()

# prep state metrics for plotting
all_states_quadrats <- state_metrics %>% 
  mutate(quantity_metric2 = ifelse(is.na(quantity_metric), 0, quantity_metric),
         quality_metric2 = ifelse(is.na(quality_metric), 0, quality_metric),
         spatial_representativeness_metric2 =  ifelse(is.na(spatial_representativeness_metric), 0, spatial_representativeness_metric),
         euclidean_distance2 = ifelse(is.na(euclidean_distance), 0, euclidean_distance))

# Loop through states and generate analysis figures
for (temp_state in pew_states) {
  
  ### ... State Wetland Area ####
  state_wetland %>%
    # filter(state %in% pew_states) %>% # include all coastal states for now
    mutate(highlight = ifelse(state == temp_state, "yes", "no")) %>%
    # reorder factors to plot in descending order
    mutate(state_abbrv = fct_reorder(state_abbrv, percent_wetland)) %>%
    # plot percent total CONUS wetland per state
    ggplot(aes(state_abbrv, percent_wetland, fill = highlight)) +
    geom_col(position = "dodge") +
    ylim(0, max(state_wetland$percent_wetland, na.rm = T) + 3) +
    coord_flip() +
    scale_fill_manual(values = c( "yes" = "tomato", "no" = "gray" ), guide = FALSE) +
    geom_text(aes(label = paste0(round(percent_wetland, 1), "%")), size = 2.75, hjust = -0.2) +
    theme_classic() +
    ylab("Wetland Percentage (%)") + xlab("State") +
    labs(caption = fig_citation) +
    ggtitle("State-level Percentage of Total Wetland in CONUS") +
    theme(text = element_text(size = 10))
  ggsave(str_replace_all(paste0(fig_dir, temp_state, "_state_wetland.jpg"), " ", "_"), width = 6, height = 5)

  ### ... State Habitat Representation ####
  
  # compare sampled to mapped habitat representation
  full_mapped_and_core_props_long %>% filter(state == temp_state) %>%
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
    ylab("Proportion") + xlab("Habitat") +
    labs(caption = fig_citation) +
    ggtitle(str_c(temp_state, "State Estimated and Sampled Habitat", sep = " "))
  
  ggsave(str_replace_all(paste0(fig_dir, temp_state, "_state_habitat.jpg"), " ", "_"), width = 7, height = 4)
  
    # extract the fill colors used by ggplot
    # g <- ggplot_build(plot)
    # unique(g$data[[1]]["fill"])
  
  ### ... Data Utility and Availability ####
  
  state_tier <- conus_state_tiers %>% 
    filter(state == temp_state) %>%
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
    labs(caption = fig_citation) +
    theme(text = element_text(size = 12)) +
    # theme(axis.text.x = element_text(angle = 33, hjust=1)) +
    ggtitle(str_c(temp_state, "State Core Utility, Completeness and Quality", sep = " "))
  ggsave(str_replace_all(paste0(fig_dir, temp_state, "_state_core_quality.jpg"), " ", "_"), width = 6, height = 5)
  
  # Rebuild quad figure of all the scores with state highlighted 
  {
    temp_state_abb <- state.abb[match(temp_state,state.name)]
    
    all_states_quadrats2 <- all_states_quadrats %>% 
      mutate(this_state = ifelse(state == temp_state_abb, "yes", "no"))
    
    quantity_plot <- ggplot(data = all_states_quadrats2, aes(x = reorder(state, quantity_metric2), y = quantity_metric, color = this_state)) +
      geom_point(pch=16) +
      geom_segment(aes(xend=state, yend=0)) +
      xlab(NULL) +
      ylab("Cores per 1000 ha") +
      scale_color_manual(values = c( "yes" = "tomato", "no" = "gray" ), guide = FALSE) +
      ggtitle("A. Quantity") +
      coord_flip() +
      labs(color = "Detailed in report?") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 8))
    
    (quantity_plot) 
    
    quality_plot <- ggplot(data = all_states_quadrats2, aes(x = reorder(state, quality_metric2), y = quality_metric, color = this_state)) +
      geom_point(pch=16) +
      geom_segment(aes(xend=state, yend=0)) +
      xlab(NULL) +
      ylab("High Quality Cores per 1000 ha") +
      ggtitle("B. Quality") +
      coord_flip() +
      scale_color_manual(values = c( "yes" = "tomato", "no" = "gray" ), guide = FALSE) +
      # labs(color = "Detailed in report?") +
      theme_minimal() +
      theme(legend.position = "none") +
      theme(axis.text.y = element_text(size = 8))
    
    (quality_plot) 
    
    spatial_plot <- ggplot(data = all_states_quadrats2, 
                           aes(x = reorder(state, spatial_representativeness_metric2), 
                               y = spatial_representativeness_metric, 
                               color = this_state)) +
      geom_point(pch=16) +
      geom_segment(aes(xend=state, yend=0)) +
      xlab(NULL) +
      ylab("Mean Sampling Distance\n/ Idealized Sampling Distance") +
      ggtitle("C. Spatial Representativeness") +
      scale_color_manual(values = c("yes" = "tomato", "no" = "gray" ), guide = FALSE) +
      coord_flip() +
      # labs(color = "Detailed in report?") +
      theme_minimal() +
      theme(legend.position = "none") +
      theme(axis.text.y = element_text(size = 8))
    
    (spatial_plot) 
    
    habitat_plot <- ggplot(data = all_states_quadrats2, aes(x = reorder(state, euclidean_distance2), y = euclidean_distance, color = this_state)) +
      geom_point(pch=16) +
      geom_segment(aes(xend=state, yend=0)) +
      xlab(NULL) +
      ylab("1 - Distance Between Sampled\nand Mapped Proportions") +
      scale_color_manual(values = c("yes" = "tomato", "no" = "gray" ), guide = FALSE) +
      ggtitle("D. Habitat Representativeness") +
      coord_flip() +
      # labs(color = "Detailed in report?") +
      theme_minimal() +
      theme(legend.position = "none") +
      theme(axis.text.y = element_text(size = 8))
    
    (habitat_plot)
    
    grid.arrange(nrow=2, ncol=2, top=temp_state,
                 quantity_plot, quality_plot, 
                 spatial_plot, habitat_plot)
    
    grobs <- arrangeGrob(nrow=2, ncol=2, top=temp_state,
                         quantity_plot, quality_plot, 
                         spatial_plot, habitat_plot,
                         left = textGrob("Lower to Higher Ranking", rot = 90, vjust = 1))
    
    ggsave(str_replace_all(paste0(fig_dir, temp_state, "_four_quadrants.jpg"), " ", "_"), dpi=300, width = 6.5, height = 6.5, grobs)
  }  
  

  # Let's do some tables
  {
    # One for areas
    # All habitats
    
    # Get estaruarine and palustrine tidal stocks 
    # Add up total stocks
    habitat_ha_state <- filter(habitat_ha, state == temp_state_abb) %>%
      mutate(`Million Tonnes C` = ifelse(habitat_simplified != "seagrass, kelp and algal mats",
                                 estimated_area_ha * 
                                 # 1 Hectare = 10000 Sq. meter
                                 10000 * 
                                 # 27.0 kg C m−3
                                 27.0 * 
                                 # 1 Metric ton = 1000 Kilogram
                                 1/1000 *
                                 # 1/1,000,000 million
                                 1/1000000,
                                 NA)) %>% 
      rename(Class = habitat_simplified,
             `Area (Ha)` = estimated_area_ha) %>% 
      select(-state)
      
      total_natural <- habitat_ha_state %>% 
        group_by() %>% 
        summarise(`Million Tonnes C` = sum(`Million Tonnes C`, na.rm = T),
                  `Area (Ha)` = sum(`Area (Ha)`, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(Class = "total natural")
      
      state_area_table <- habitat_ha_state %>% 
        bind_rows(total_natural)
    
    # restorable_emissions
    # habitat_ha 
    # Plus estimated impounded and drained
    # restorable_emissions_state <- filter(restorable_emissions, state == temp_state_abb) %>% 
    #   rename(Class = restoration_type2, 
    #          `Area (Ha)`=scaled_area_ha) %>% 
    #   select(Class, `Area (Ha)`)
    # 
    # state_area_table <- state_area_table %>% 
    #   bind_rows(restorable_emissions_state)
    # 
    # Write to file
    write_csv(state_area_table, 
              paste0(tab_dir, temp_state, "_state_area_summary.csv"))
  }

  # for (temp_state in pew_states) {
  # Filter by state
  # Clean up classifications
  cores_state <- filter(cores, state == temp_state)
  
  citations_state <- citations2 %>% 
    filter(study_id %in% cores_state$study_id) %>% 
    mutate(key = paste("@", key, sep="")) %>% 
    group_by(study_id) %>% 
    summarise(citation = paste(key, collapse = "; "))
  
  # assign value of 1 to quality code observations (if they are present)
  # so they can be tallied for report summary tables
  cores_state$stocks_qual_code[!is.na(cores_state$stocks_qual_code)] <- 1
  cores_state$dates_qual_code[!is.na(cores_state$dates_qual_code)] <- 1
  cores_state$elevation_qual_code[!is.na(cores_state$elevation_qual_code)] <- 1
  
  cores_state$stocks_qual_code <- as.numeric(cores_state$stocks_qual_code)
  cores_state$dates_qual_code <- as.numeric(cores_state$dates_qual_code)
  cores_state$elevation_qual_code <- as.numeric( cores_state$elevation_qual_code)
  
  # summarize number of cores in each quality tier and add bib ID reference
  cores_state_study_summaries <- cores_state %>% 
    # create column to indicate cases where cores didn't meet any qualifications
    mutate(na_qual_code = ifelse(is.na(stocks_qual_code) & is.na(dates_qual_code) & is.na(elevation_qual_code),
                                 1, NA)) %>% 
    group_by(study_id) %>% 
    # Get counts by utility class
    summarise(`stocks` = sum(stocks_qual_code, na.rm=T),
              `dated cores` = sum(dates_qual_code, na.rm=T),
              `elevation measurements` = sum(elevation_qual_code, na.rm=T),
              `other utility` = sum(na_qual_code, na.rm = T),
              Total = n()) %>% 
    # Arrange in descending order for total number of cores
    arrange(-Total) %>% 
    # Clean up names
    ungroup() %>% 
    left_join(citations_state, by = "study_id") %>% 
    mutate(study_id = str_replace_all(study_id, "_", " "),
           study_id = str_replace_all(study_id, "et al", "et al."),
           citation = paste("[", citation, "]", sep=""))
  
  # Write to file
  write_csv(cores_state_study_summaries, 
            paste0(tab_dir, temp_state, "_state_study_summary.csv"))
  # }
  
}


