# New map fig

# Merge countries and eez shapefile

# Packages
library(sf)
library(tidyverse)
library(gridExtra)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)
library(ggrepel)

# Load shapefiles as special feature
countries <- st_read("data/world_countries/country.shp")
eez <- st_read("data/World_EEZ_v11_20191118/eez_v11.shp")

# make sure vocab is constant
countries2 <- countries %>%
  select(COUNTRYAFF, COUNTRY) %>%
  mutate(source = "countries",
         # Original recoding
         country = recode(COUNTRYAFF, "Congo DRC" = "Democratic Republic of the Congo",
                          "Brunei Darussalam" = "Brunei"),
         territory =recode(COUNTRY, "Congo DRC" = "Democratic Republic of the Congo",
                           "Brunei Darussalam" = "Brunei") ) %>% 
  # New recoding
  select(-c(COUNTRY, COUNTRYAFF))

eez2 <- eez %>% 
  filter(! POL_TYPE %in% c("Overlapping claim")) %>% 
  select(SOVEREIGN1, TERRITORY1) %>% 
  # Original recoding
  mutate(country = recode(SOVEREIGN1, "Comores" = "Comoros",
                          "Republic of Mauritius" = "Mauritius",
                          "East Timor" = "Timor-Leste",
                          "Republic of the Congo" = "Congo",
                          "Cape Verde" = "Cabo Verde",
                          "Ivory Coast" = "Côte d'Ivoire",
                          "Palestine" = "Palestinian Territory",
                          "Taiwan" = "China",
                          "Federal Republic of Somalia"="Somalia",
                          "Russia" = "Russian Federation"),
         territory = recode(TERRITORY1, "Comores" = "Comoros",
                            "Republic of Mauritius" = "Mauritius",
                            "East Timor" = "Timor-Leste",
                            "Republic of the Congo" = "Congo",
                            "Cape Verde" = "Cabo Verde",
                            "Ivory Coast" = "Côte d'Ivoire",
                            "Palestine" = "Palestinian Territory",
                            "Taiwan" = "China",
                            "Federal Republic of Somalia"="Somalia",
                            "Russia" = "Russian Federation")) %>% 
  # New recoding
  mutate(source = "EEZ") %>% 
  select(-c(SOVEREIGN1, TERRITORY1))

# Which countries are not in others
left_out_eez <- as_tibble(eez2) %>% 
  filter(! country %in% c(countries2$country)) %>% 
  select(-geometry) %>% 
  distinct_all()

write_csv(left_out_eez, "EEZs_not_in_countries.csv")

left_out_countries <- as_tibble(countries2) %>% 
  filter(! country %in% eez2$country) %>% 
  select(-geometry) %>% 
  distinct_all()

write_csv(left_out_countries, "Countries_w_no_EEZs.csv")

# These EEZ 'soveriegns' are not in the countries map

# Do nothing
# Western Sahara, Not claimed by Morocco or Mauritania (I think)

# make sure vocab is constant

# Change in EEZ
# Comores in EEZ, Spelled Comoros in countries, Comoros is correct 
# Republic of Mauritius in EEZ, just Mauritius in countries
# East Timor, Timor-Leste in countries
# Republic of the Congo, Just Congo in Countries
# Cape Verde, Cabo verde in countries
# Ivory Coast, 	Côte d'Ivoire in countries
# Palestine, Palestinian Territory in countries
# Taiwan in EEZ, China in countries, should probably change to China because of U.S. govt. policy
# Federal Republic of Somalia in EEZ, just Somalia in countries
# Russia in EEZ, Russian Federation in countries 

# Merge files together
eez_and_countries <- eez2 %>% 
  bind_rows(countries2)


eez_and_countries2 <- eez_and_countries %>% 
  # Fixing disagreements between what EEZ map calls a territory 
  # vs what ESRI calls a country
  # First, fix minor spelling differences. Defer to ESRI countries spelling
  mutate(territory = recode(territory, "Heard Island and McDonald Islands" = "Heard and McDonald Islands",
                            "Faeroe"="Faroe Islands",
                            "Clipperton Island"="Clipperton",
                            "Collectivity of Saint Martin"="Saint Martin",
                            "Saint-Barthélemy"="Saint Barthelemy",
                            "Saint-Pierre and Miquelon"="Saint Pierre and Miquelon",
                            "Curaçao"="Curacao",
                            "Sint-Eustatius"="Saint Eustatius",
                            "Sint-Maarten"="Sint Maarten",
                            "Canary Islands"="Canarias",
                            "United States Virgin Islands"="US Virgin Islands")
         ) %>% 
  # Then lump U.S. minor islands, French Minor Islands together, British Indian Ocean Territory Together
  mutate(territory = ifelse(territory %in% c("Amsterdam and Saint Paul Islands",
                                            "Crozet Islands", "Bassas da India",
                                            "Europa Island", "Juan de Nova Island",
                                            "Kerguélen"), 
                            "French Southern Territories", territory),
         territory = ifelse(territory %in% c("Howland and Baker islands", "Jarvis Island",
                                             "Johnston Atoll", "Palmyra Atoll",
                                             "Wake Island"), 
                            "United States Minor Outlying Islands", 
                            territory),
         # Then call U.S. non-contiguous states, US
         territory = ifelse(territory %in% c("Alaska", "Hawaii"), 
                            country, 
                            territory),
         # Then lump any remaining territory without a government or permanant population in w/ its country
         territory = ifelse(country %in% c("Australia", "Brazil", "Chile", "Colombia", "Ecuador", 
                                           "India", "Kiribati", "Mauritius", "New Zealand", "Norway", 
                                           "South Africa", "Timor-Leste"), 
                            country, 
                            territory)
         )

# Let's do a version where only the countries w EEZ's are included
eez_and_countries2 <- eez_and_countries2 %>% 
  filter(country %in% c(eez2$country,
                        sort(unique(eez$TERRITORY1)),
                        sort(unique(eez$TERRITORY2)),
                        sort(unique(eez$TERRITORY3)),
                        sort(unique(eez$SOVEREIGN2)),
                        sort(unique(eez$SOVEREIGN))
                        ))

# What's left should be territories that are so small their EEZ is mapped, but country is not
# ... and countries who are surrounded by disputed waters
look_for_strays <- as_tibble(eez_and_countries2) %>% 
  select(-geometry) %>% 
  distinct_all() %>% 
  arrange(country, territory) %>% 
  group_by(country, territory) %>% 
  mutate(n = n()) %>% 
  filter(n == 1)
View(look_for_strays)

# Combine 

eez_conflicts <- eez %>% 
  filter(POL_TYPE %in% c("Overlapping claim")) %>% 
  mutate(country = "Disputed",
         territory = GEONAME,
         source = "EEZ") %>% 
  select(country, territory, source)

eez_and_countries2 <- eez_and_countries2 %>% 
  bind_rows(eez_conflicts)

st_write(eez_and_countries2, "data/CCN_maps/CCN_Countries_and_EEZ_map.shp", append = F)
st_write(eez_and_countries2 %>% filter(source == "countries"), "data/CCN_maps/CCN_Countries_map.shp", append = F)
st_write(eez_and_countries2 %>% filter(source == "EEZ"), "data/CCN_maps/CCN_EEZ_map.shp", append = F)

 
# st_write(anti_meridian_sf, "AntiMeridian.shp")

#   
# b <- st_bbox(countries_display)
# 
# map_figure <- ggplot() + 
#   geom_sf(data = countries_display, aes(fill = over_or_under_rep), size=0.25, color="black") +
#   # geom_sf(data = anti_meridian_sf, size=0.25, color="red", lty = 2) +
#   # scale_color_manual(values = cbp1) +
#   theme(legend.position = c(0.45,0.65),
#          legend.spacing.y = unit(0.05, "cm"),
#          legend.margin = margin(0,0,0,0, unit="cm"),
#          legend.title = element_blank()) +
#   ylab(NULL) +
#   xlab(NULL) +
#  # coord_sf(xlim = c(b["xmin"], b["xmax"]), ylim = c(b["ymin"], b["ymax"]),
#  #              crs=crs_wintri) +
#   theme_minimal() +
#   theme(legend.title = element_blank())
# 
# # (map_figure)

