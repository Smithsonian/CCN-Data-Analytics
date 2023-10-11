## The Forbidden Synthesis ####

# Synthesizes embargoed data and published data which hasn't been included in the CCRCN synthesis

library(tidyverse)
# library(sf)
library(leaflet)

# Synthesize Core Data ####

dirs <- list.files("data/forthcoming/", recursive = T, full.names = T)

forbidden_synthesis <- data.frame()

for(table in dirs){
  core_table <- read_csv(table)
  forbidden_synthesis <- bind_rows(forbidden_synthesis, core_table)
}

# which(is.na(cores$core_id)) # check for NA core ids
# length(unique(cores$core_id)) == nrow(cores) # check all core ids are unique
# which(is.na(cores$core_latitude)) # check for NA lat/lon

# store states for pew reports
# pew_states <- sort(c("Oregon", "Maine", "Washington", "Massachusetts", "California", "New York", 
#                      "New Jersey", "Maryland", "Virginia", "Florida"))

forbidden_cores <- forbidden_synthesis %>%
  filter(state != "International" & state != "Alaska") %>%
  mutate(longitude = core_longitude,
         latitude = core_latitude)

# plot to be sure of geography assignments
# define palette
pal <- colorFactor(palette = "Set3",
                   reverse = T,
                   forbidden_cores$state)

# visualize where forthcoming cores are in CONUS
leaflet(forbidden_cores) %>%
  addProviderTiles(providers$CartoDB) %>%
  # addTiles() %>%
  addCircleMarkers(lng = ~as.numeric(longitude), lat = ~as.numeric(latitude),
                   radius = 5, color = ~pal(state), label = ~study_id) %>%
  addLegend(pal = pal, values = ~state)

# write to forbidden_synthesis folder in the project report
# write_csv(forbidden_cores, "data/report_data/forthcoming_cores.csv")

# Quantify Unavailable Data ####
# this part might be better off in the report

tallying <- forbidden_cores %>%
  add_count(state, name = "core_count") %>%
  select(state, core_count) %>% distinct() %>%
  # reorder factors to plot in decending order
  mutate(state = fct_reorder(state, core_count))

# designation citation for figures
fig_citation <- "Holmquist et al 2021 (unpublished)"

# plot the number of cores that are not accounted for in the report
ggplot(tallying, aes(state, core_count)) +
  geom_col(position = "dodge") +
  coord_flip() +
  # scale_fill_manual(values = c( "yes" = "tomato", "no" = "gray" ), guide = FALSE) +
  # geom_text(aes(label = paste0(round(percent_wetland, 1), "%")), size = 2.75, hjust = -0.2) +
  theme_bw() +
  ylab("Number of Cores") + xlab("States") +
  labs(caption = fig_citation, fill = "Assessed in Report")
ggsave("figures/CONUS_forthcoming_cores.jpg")

# Contacts for Forthcoming Cores ####
contacts <- forbidden_cores %>%
  select(study_id, contact_info, state) %>%
  add_count(study_id, state, name = "core_count") %>%
  arrange(state) %>% distinct()

# table won't be included in the report, but we'll use it as a resource
write_csv(contacts, "resources/tables/forthcoming_core_contacts.csv")

