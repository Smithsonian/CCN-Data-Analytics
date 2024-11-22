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

vlm <- read.table("data/VLM/VLM_Global_Imaged.txt")[,1:3]
names(vlm) <- c("longitude", "latitude", "vertical_rate")

ggplot(vlm, aes(x=longitude, y=latitude)) +
  geom_tile(aes(fill = vertical_rate)) +
  scale_fill_gradientn(colours = rainbow(10))

plot(mangrove_marsh_map_centroid)

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

st_crs(mangrove_marsh_map_centroid) <- 4326
mangrove_marsh_map_centroid_proj <- st_transform(mangrove_marsh_map_centroid,
                                                 "+proj=wintri +lon_0=0 +lat_1=50.467 +x_0=0 +y_0=0 +R=6371000 +units=m +no_defs +type=crs")


vlm_sf <- vlm %>%
  st_as_sf(coords = c('longitude',"latitude")) # make points spatial

# Handle projections
st_crs(vlm_sf) <- 4326 # Give the points a coordinate reference system (CRS)

vlm_proj <- st_transform(vlm_sf,
                         "+proj=wintri +lon_0=0 +lat_1=50.467 +x_0=0 +y_0=0 +R=6371000 +units=m +no_defs +type=crs")

  
  
# Join country with both countries and EEZ's
sp_points_country <- st_join(synthesis_stocks_spatial_proj, 
                             mangrove_marsh_map_proj,
                             join = st_nearest_feature)

sp_points_table <- sp_points_country %>% 
  as_tibble() %>% 
  select(-geometry)

country_averages <- sp_points_table %>% 
  group_by(country, terrtry, habitat, HYBAS_I, FishId) %>% 
  summarise(stock_MgHa_mean = mean(stock_MgHa),
            n = n(),
            stock_MgHa_se = sd(stock_MgHa)/sqrt(n)
            ) %>% 
  ungroup() %>% 
  rename(territory = terrtry) %>% 
  group_by(habitat, country, territory) %>% 
  summarise(stock_MgHa_mean = mean(stock_MgHa_mean),
            n = sum(n),
            n_sites = n(),
            stock_MgHa_se = sqrt(sum(stock_MgHa_se^2))) %>% 
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

# View(country_averages_sig_diff)

country_hab_TierI_diff <- country_averages_sig_diff %>% 
  mutate(tierI_diff = stock_MgHa_mean - TierI_mean) %>% 
  group_by(territory) %>% 
  summarise(mean_diff = mean(tierI_diff, na.rm = T)) %>% 
  arrange(mean_diff)

country_averages_sig_diff <- country_averages_sig_diff %>% 
  mutate(stock_MgHa_lower_CI = ifelse(n < 5, NA, stock_MgHa_lower_CI),
         stock_MgHa_upper_CI = ifelse(n < 5, NA, stock_MgHa_upper_CI)) %>% 
  mutate(territory = factor(territory, country_hab_TierI_diff$territory)) %>% 
  mutate(text_position = pmin(stock_MgHa_mean, stock_MgHa_lower_CI, na.rm=T))

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

library(ggrepel)

ggplot(country_averages_sig_diff, aes(x = territory, y = stock_MgHa_mean)) +
  geom_point(aes(color = habitat)) +
  geom_crossbar(aes(ymin = stock_MgHa_lower_CI, ymax = stock_MgHa_upper_CI, color = habitat,
                    fill = overlaps_TierI)) +
  geom_hline(data = IPCC_tier_I, aes(yintercept = TierI_mean)) +
  geom_hline(data = IPCC_tier_I, aes(yintercept = TierI_MgHa_UpperCI), lty = 2) + 
  geom_hline(data = IPCC_tier_I, aes(yintercept = TierI_LowerCI), lty = 2) + 
  geom_text(aes(label = n_sites, y = text_position), size = 2.5, nudge_y = -15) +
  # geom_ribbon(data = IPCC_tier_I, aes(ymin = TierI_LowerCI, ymax = TierI_MgHa_UpperCI)) +
  facet_grid(.~habitat, scale="free") +
  scale_y_continuous(labels = scales::comma) +
  coord_flip() +
  theme(axis.text.y = element_text(size = 8),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("white", "black"), na.translate = F)

# 

marsh_mangrove_seagrass_tall <- read_csv("seagrass_mapping/output/Marsh_mangrove_seagrass_territory_summary_tall.csv")

all_stocks <- marsh_mangrove_seagrass_tall %>% 
  filter(ecosystem != "total") %>% 
  rename(habitat=ecosystem) %>% 
  left_join(country_averages_sig_diff) %>% 
  select(-c(TierI_mean, TierI_LowerCI, TierI_MgHa_UpperCI)) %>% 
  left_join(IPCC_tier_I) %>% 
  mutate(compiled_EF = ifelse(is.na(stock_MgHa_mean),
                              TierI_mean, stock_MgHa_mean),
         compiled_UpperCI = ifelse(is.na(stock_MgHa_mean),
                                   TierI_MgHa_UpperCI, stock_MgHa_upper_CI),
         compiled_LowerCI = ifelse(is.na(stock_MgHa_mean),
                                   TierI_LowerCI, stock_MgHa_lower_CI),
         TierIorII = ifelse(is.na(stock_MgHa_mean),
                              "Tier I", "Tier II"),
         Total_Stocks = compiled_EF * hectare,
         Total_Stockers_UpperCI = compiled_UpperCI * hectare,
         Total_Stockers_LowerCI = compiled_LowerCI * hectare)

territory_totals <- all_stocks %>% 
  group_by(territory) %>% 
  summarise(Total_Stocks = sum(Total_Stocks, na.rm = T),
            Total_SE = sqrt((sum((Total_Stockers_UpperCI-Total_Stockers_LowerCI), na.rm=T)/3.92)),
            Total_Stockers_UpperCI = Total_Stocks-(Total_SE*3.92/2),
            Total_Stockers_LowerCI = Total_Stocks+(Total_SE*3.92/2)) %>% 
  ungroup() %>% 
  arrange(-Total_Stocks) %>% 
  mutate(habitat = "total")

all_stocks_vis <- all_stocks %>% 
  bind_rows(territory_totals) %>% 
  mutate(territory = factor(territory, levels = rev(territory_totals$territory)))

ggplot(all_stocks_vis, aes(x = territory, y = Total_Stocks)) +
  geom_point(aes(color = habitat)) +
  geom_crossbar(aes(ymin = Total_Stockers_LowerCI, ymax = Total_Stockers_UpperCI, color = habitat)) +
  # geom_hline(data = IPCC_tier_I, aes(yintercept = TierI_mean)) +
  # geom_hline(data = IPCC_tier_I, aes(yintercept = TierI_MgHa_UpperCI), lty = 2) + 
  # geom_hline(data = IPCC_tier_I, aes(yintercept = TierI_LowerCI), lty = 2) + 
  # geom_text(aes(label = n_sites, y = text_position), size = 2.5, nudge_y = -15) +
  # geom_ribbon(data = IPCC_tier_I, aes(ymin = TierI_LowerCI, ymax = TierI_MgHa_UpperCI)) +
  facet_grid(.~habitat, scale="free") +
  scale_y_continuous(labels = scales::comma) +
  coord_flip() +
  theme(axis.text.y = element_text(size = 8),
        axis.text.x = element_text(angle = 45, hjust = 1)) 


# Create a master figure


# 
# 
# # Start Kriging
# sp_points_table_w_ocean <- sp_points_country %>% 
#   left_join(mangrove_marsh_map_centroid_tab)
# 
# # Reclassify oceans
# # South Pacific Ocean, North Pacific Ocean, South China and Easter Archipelagic Seas - Pacific
# # North Atlantic Ocean, South Atlantic Ocean - Atlantic
# # Indian Ocean
# # Mediterranean Region
# 
# mangrove_marsh_map_centroid_df <- mangrove_marsh_map_centroid_proj %>% 
#   st_join(vlm_proj, join = st_nearest_feature) %>% 
#   st_transform(4326) %>% 
#   mutate(lon = st_coordinates(.)[,1],
#          lat = st_coordinates(.)[,2]) %>%
#   mutate(names = recode(names, "South Pacific Ocean" = "Pacific", "North Pacific Ocean"="Pacific", "South China and Easter Archipelagic Seas" = "Pacific",
#                         "North Atlantic Ocean"="Atlantic", "South Atlantic Ocean" = "Atlantic")) %>% 
#   as.data.frame() %>% 
#   select(-geometry)
# 
# ccn_points_table_w_ocean_df_points <- sp_points_table_w_ocean %>% 
#   st_join(vlm_proj, join = st_nearest_feature) %>% 
#   as.data.frame() %>% 
#   select(-geometry)
# 
# ggplot(ccn_points_table_w_ocean_df_points, aes(x = vertical_rate, y = stock_MgHa)) +
#   geom_point(aes(color = names)) +
#   facet_wrap(.~habitat)
# 
# ccn_points_table_w_ocean_df <- ccn_points_table_w_ocean_df_points %>% 
#   group_by(HYBAS_I, FishId, country, terrtry, source, habitat) %>% 
#   summarise(stock_MgHa = mean(stock_MgHa),
#             n = n()) %>% 
#   filter(n >= 5) %>% 
#   ungroup() %>% 
#   left_join(mangrove_marsh_map_centroid_df)
# 
# ggplot(ccn_points_table_w_ocean_df, aes(x = vertical_rate, y = stock_MgHa)) +
#   geom_point(aes(color = names)) +
#   facet_wrap(.~habitat) +
#   geom_smooth(method = "lm")
# 
# lm1 <- lm(stock_MgHa~vertical_rate+habitat*vertical_rate,
#           data = ccn_points_table_w_ocean_df)
# summary(lm1)
# # Create table with oceans and habitats
# habitats_and_ocean <- ccn_points_table_w_ocean_df %>% 
#   as_tibble() %>% 
#   group_by(names, habitat) %>% 
#   summarise(n = n()) %>% 
#   filter(n >= 10)
# 
# # sample variogram
# f.2 <- as.formula(stock_MgHa ~ 1|habitat)
# 
# ccn_points_table_w_ocean_df2 <- ccn_points_table_w_ocean_df
# sp::coordinates(ccn_points_table_w_ocean_df2) = ~lon+lat
# ccn_points_table_w_ocean_df2@proj4string <- sp::CRS("+proj=longlat +datum=WGS84")
# 
# mangrove_marsh_map_centroid_df2 <- mangrove_marsh_map_centroid_df 
# sp::coordinates(mangrove_marsh_map_centroid_df2) = ~lon+lat
# mangrove_marsh_map_centroid_df2@proj4string <- sp::CRS("+proj=longlat +datum=WGS84")
# 
# var.smpl.2 <- gstat::variogram(f.2, ccn_points_table_w_ocean_df2, cutoff = 20000, width = 100)
# 
# # fit variogram model via vgm() to tidalAmp
# dat.fit.2  <- gstat::fit.variogram(var.smpl.2, gstat::vgm(, "Sph", 500), fit.kappa = TRUE)
# 
# plot(var.smpl.2, dat.fit.2)
# 
# for (i in 1:nrow(habitats_and_ocean)) {
#   
#   
#   # Filter input and target data
#   ccn_points_filtered <- ccn_points_table_w_ocean_df %>% 
#     filter(names == habitats_and_ocean$names[i],
#            habitat == habitats_and_ocean$habitat[i])
#   
#   sp::coordinates(ccn_points_filtered) = ~lon+lat
#   ccn_points_filtered@proj4string <- sp::CRS("+proj=longlat +datum=WGS84")
#   
#   mangrove_marsh_map_centroid_filtered <- mangrove_marsh_map_centroid_df %>% 
#     filter(names == habitats_and_ocean$names[i])
#   
#   sp::coordinates(mangrove_marsh_map_centroid_filtered) = ~lon+lat
#   mangrove_marsh_map_centroid_filtered@proj4string <- sp::CRS("+proj=longlat +datum=WGS84")
#   
#   var.smpl.2 <- gstat::variogram(f.2, ccn_points_filtered, width = 100, cutoff = 10000)
#    
#   # fit variogram model via vgm() to tidalAmp
#   dat.fit.2  <- gstat::fit.variogram(var.smpl.2, gstat::vgm(, "Mat", 500), fit.kappa = TRUE)
#   
#   plot(var.smpl.2, dat.fit.2, main = paste(habitats_and_ocean$names[i], habitats_and_ocean$habitat[i]))
#   
#   # extrapolate 
#   stock_krig <- gstat::krige(formula = f.2,
#                                  locations = ccn_points_filtered,
#                                  newdata = mangrove_marsh_map_centroid_filtered,
#                                  model = dat.fit.2)
# 
#   # join kriged tidalAmp values to outlets_df
#   mangrove_marsh_map_centroid_filtered$stock_pred <- stock_krig$var1.pred
#   mangrove_marsh_map_centroid_filtered$stock_pred_se <- sqrt(stock_krig$var1.var)
#   
#   
# }
# 
# 
# 
#   
# 
# 




