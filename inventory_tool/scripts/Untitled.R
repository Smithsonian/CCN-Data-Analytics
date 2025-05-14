# Calculate Core Level Carbon Stocks, Un-Weighted Country and Habitat Specific Stocks
# Spatially interpolated and weighted habitat and species specific stocks

# Import packages
library(tidyverse)
library(gstat)
library(sf)
library(spdep)

# Import data
cores <- read_csv("data/CCN_cores.csv")
depths <- read_csv("data/CCN_depthseries.csv")
depths <- read_csv("data/CCN_depthseries.csv", guess_max = nrow(depths))

# Import spatial data
mangrove_marsh_map <- st_read("data/CCN_maps/Global_Mangrove_Marsh_Summaries_noZeros.shp")

mangrove_marsh_map_centroid <- st_read("data/CCN_maps/Global_Mangrove_Marsh_Summaries_noZeros_centroid_oceanJoin.shp")

mangrove_marsh_map_centroid_tab <- mangrove_marsh_map_centroid %>% 
  as_tibble() %>% 
  select(-c(geometry, mngr__2, mrsh__2))

# Import country and territory level summaries
country_territory_areas <- read_csv("seagrass_mapping/output/Marsh_mangrove_seagrass_territory_summary_tall.csv")

# Internal Functions 
{
  standardizeDepths <- function(df, target_intervals){
    # Note: this function was adapted from Atlas code (written by Michael and/or Jim)
    standard_ds <- df %>%
      # mutate(across(where(cols %in% c("depth_min", "depth_max", "dry_bulk_density", "fraction_organic_matter", "fraction_carbon"))), as.numeric)
      merge(target_intervals) %>%
      # Keeps intervals between min and max horizon
      # If an interval crosses a horizon, it remains
      dplyr::filter(pmax(depth_min, horizon_min) < pmin(depth_max, horizon_max)) %>%
      dplyr::arrange(study_id, site_id, core_id, depth_min, depth_max) %>%
      # Calculate weights for each interval
      dplyr::mutate(overlapping_depth = pmin((depth_max-depth_min),
                                             (horizon_max-depth_min),
                                             (depth_max-horizon_min), na.rm=T)) %>%
      dplyr::group_by(study_id, site_id, core_id, horizon_min, horizon_max) %>%
      dplyr::mutate(total_depth = sum(overlapping_depth),
                    weight = overlapping_depth / total_depth) %>%
      # Aggregate by horizon intervals
      dplyr::summarise(dry_bulk_density = sum(dry_bulk_density * weight),
                       fraction_organic_matter = sum(fraction_organic_matter * weight),
                       fraction_carbon = sum(fraction_carbon * weight),
                       carbon_density = sum(carbon_density * weight)) %>%
      ungroup()
    
    return(standard_ds)
  }
  
  se <- function(x, na.rm=TRUE) {
    if (na.rm) x <- na.omit(x)
    sd(x)/sqrt(length(x))
  }
}

# Calculate OC from OM
om_to_oc_conversions <- read_csv("OM and C Relationship/tabs/Parameter_Summary_Table.csv") %>% 
  select(habitat, Parameter, mean) %>% 
  spread(value = mean, key = Parameter) %>% 
  filter(complete.cases(habitat)) %>% 
  select(-c(tau_random, tau_study))

# Filter cores that are too shallow
cores_deep <- cores %>% 
  filter(max_depth>=75)

habitats <- cores_deep %>% 
  select(study_id, site_id, core_id, habitat)

depths_deep <- depths %>% 
  semi_join(cores_deep) %>% 
  select(study_id, site_id, core_id, depth_min, depth_max, dry_bulk_density, fraction_organic_matter, fraction_carbon) %>% 
  left_join(habitats) %>% 
  filter(habitat %in% c("marsh", "mangrove", "seagrass")) %>% 
  left_join(om_to_oc_conversions) %>% 
  mutate(fraction_carbon = ifelse(is.na(fraction_carbon), 
                                  fraction_organic_matter*beta1 + fraction_organic_matter^2*beta2, 
                                  fraction_carbon),
         carbon_density = fraction_carbon*dry_bulk_density) %>% 
  filter(complete.cases(carbon_density),
         depth_max <= 100)

# Regularize depth increments

target_intervals <- data.frame(horizon_min = c(0,10,25,50), horizon_max = c(10,25,50,100))

synthesis_stocks <- standardizeDepths(depths_deep, target_intervals) %>% 
  mutate(stock_gCm2 = carbon_density * (horizon_max - horizon_min) * 10000,
         # convert gC m-2 to MgC ha-1
         stock_MgHa = stock_gCm2 * (10^4/10^6)) %>% 
  group_by(study_id, site_id, core_id) %>% 
  # Get core level averages
  summarise(stock_MgHa = sum(stock_MgHa)) %>% 
  filter(complete.cases(stock_MgHa))

# Create sf version
lat_lon_habitat <- cores_deep %>% 
  select(study_id, site_id, core_id, latitude, longitude, habitat)

synthesis_stocks_spatial <- synthesis_stocks %>% 
  left_join(lat_lon_habitat) %>% 
  st_as_sf(coords = c('longitude',"latitude")) # make points spatial

# Handle projections
st_crs(synthesis_stocks_spatial) <- 4326 # Give the points a coordinate reference system (CRS)

synthesis_stocks_spatial_proj <- st_transform(synthesis_stocks_spatial, 
                                              "+proj=wintri +lon_0=0 +lat_1=50.467 +x_0=0 +y_0=0 +R=6371000 +units=m +no_defs +type=crs")

mangrove_marsh_map_proj <- st_transform(mangrove_marsh_map,
                                        "+proj=wintri +lon_0=0 +lat_1=50.467 +x_0=0 +y_0=0 +R=6371000 +units=m +no_defs +type=crs")

# Join country with both countries and EEZ's
sp_points_country <- st_join(synthesis_stocks_spatial_proj, 
                             mangrove_marsh_map_proj,
                             join = st_nearest_feature)

sp_points_table <- sp_points_country %>% 
  as_tibble() %>% 
  select(-geometry)

country_averages <- sp_points_table %>% 
  rename(territory = terrtry) %>% 
  group_by(habitat, country, territory) %>% 
  summarise(stock_MgHa_mean = mean(stock_MgHa),
            n = n(),
            stock_MgHa_se = sd(stock_MgHa)/sqrt(n)) %>% 
  arrange(habitat, -stock_MgHa_mean) %>% 
  ungroup() %>% 
  mutate(stock_MgHa_upper_CI = qnorm(0.975, stock_MgHa_mean, stock_MgHa_se),
         stock_MgHa_lower_CI= qnorm(0.025, stock_MgHa_mean, stock_MgHa_se))

IPCC_tier_I <- as_tibble(data.frame(habitat = c("mangrove", "marsh", "seagrass"),
                                      TierI_mean = c(386, 255, 108),
                                      TierI_LowerCI = c(351,254,84),
                                      TierI_MgHa_UpperCI = c(424,297,139)))

country_averages_sig_diff <- country_averages %>% 
  left_join(IPCC_tier_I) %>% 
  mutate(overlaps_TierI = ifelse(pmax(stock_MgHa_lower_CI, TierI_LowerCI) < pmin(stock_MgHa_upper_CI, TierI_MgHa_UpperCI), 
                                 "overlaps Tier I",
                                 "sig. different from Tier I"))
country_averages_sig_diff[country_averages_sig_diff<0] <- 0

View(country_averages_sig_diff)

ggplot(country_averages_sig_diff, aes(x = territory, y = stock_MgHa_mean)) +
  geom_point(aes(color = habitat), position = position_dodge(width = 0.90)) +
  geom_crossbar(aes(ymin = stock_MgHa_lower_CI, ymax = stock_MgHa_upper_CI, color = habitat,
                    fill = overlaps_TierI), position = position_dodge(width = 0.90)) +
  # facet_grid(.~ecosystem, scale="free") +
  scale_y_continuous(labels = scales::comma) +
  coord_flip() +
  theme(axis.text.y = element_text(size = 8),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("white", "black"), na.translate = F)

# Start Kriging
sp_points_table_w_ocean <- sp_points_table %>% 
  left_join(mangrove_marsh_map_centroid_tab)


