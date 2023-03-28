## State-level Carbon Stock

library(tidyverse)

# read in synthesis data
root_dir <- "https://raw.githubusercontent.com/Smithsonian/CCRCN-Data-Library/main/data/CCRCN_synthesis/"
cores <- read_csv(paste0(root_dir, "CCRCN_cores.csv"), guess_max = 7000)
depthseries <- read_csv(paste0(root_dir, "CCRCN_depthseries.csv"), guess_max = 60000)

# read in ccap data


## Analysis ####

# define a depth to standardize estimates to
# top meter
horizons <- data.frame(horizon_min = 0, horizon_max = 100)

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
                     fraction_carbon = sum(fraction_carbon * weight))
  
  return(standard_ds)
}

carbonStock <- function(df){
  corestock <- df %>% 
    drop_na(dry_bulk_density, fraction_carbon) %>% 
    # filter(core_id %in% unique(cr_cores$core_id)) %>% 
    # select(where(~!all(is.na(.)))) %>% 
    # select_if(function(x) {!all(is.na(x))}) %>%
    mutate(carbon_density = dry_bulk_density * fraction_carbon,
           # calculate c stock in each interval (gC m-2)
           # g/cm2 * 10000cm2/m2
           cstock_interval = carbon_density * (horizon_max-horizon_min) * 10000)  %>% 
    # cases with fraction carbon but no DBD, drop NA cstock for now
    # drop_na(cstock_interval) %>% 
    arrange(core_id, horizon_min) %>% 
    group_by(study_id, site_id, core_id) %>% 
    summarize(core_stock = sum(cstock_interval))
  # min_depth = min(horizon_min),
  # max_depth = max(horizon_max),
  # )
  
  return(corestock)
}

# try it on the synthesis
synthesis_standardized <- standardizeDepths(depthseries, target_intervals = horizons)
synthesis_stocks <- carbonStock(synthesis_standardized) %>%
  # convert gC m-2 to MgC ha-1
  mutate(core_stock_MgHa = core_stock * (10^4/10^6))

core_stocks <- left_join(cores, synthesis_stocks)

## ... US States ####

us_stocks <- core_stocks %>% drop_na(core_stock) %>% 
  # rename(state = admin_division) %>%
  filter(country == "United States") %>%
  filter(state != "Puerto Rico" & state != "Hawaii") %>%
  mutate(state_abb = state.abb[match(state,state.name)]) %>% 
  mutate(habitat_simplified = recode(habitat, 
                          "mudflat" = "unvegetated",
                          # we'll be comparing sampled cores to mapped habitat in which seagress, kelp, and algal mats are grouped together
                          "seagrass" = "seagrass, kelp and algal mats",
                          "algal mat" = "seagrass, kelp and algal mats")) %>% 
  group_by(state_abb, habitat_simplified) %>% 
  summarize(n = n(),
            stock_mean = mean(core_stock_MgHa, na.rm = T),
            stock_se = sd(core_stock_MgHa, na.rm = T),
            co2_equiv = stock_mean * 3.67,
            co2_equiv_se = stock_se * 3.67)

us_stocks %>% ungroup() %>% 
  filter(habitat_simplified == "marsh") %>%
  mutate(state = paste0(state, ", n = ", n),
         state = fct_reorder(state, stock_mean)) %>% 
  ggplot(aes(stock_mean, state,
             xmin = stock_mean - stock_se, xmax = stock_mean + stock_se)) +
  geom_point() +
  geom_errorbar() +
  xlab("Carbon Stocks (MgC ha-1)") +
  # ggtitle("Marsh 1m Core C Stocks by State") +
  theme_bw()

# Scale estimates by wetland hectares


hab_area <- read_csv("database_inventory/data/derived/ccap_state_habitat_wetland.csv") %>% 
  rename(state_abb = state)
  
# state_wetland_area <- readxl::read_xls("data/mangroveWorld_Area.xls") %>% 
#   mutate(habitat = "mangrove",
#          habitat_area_ha = SUM_ST_AREA_SH/10000) %>% 
#   rename(country = COUNTRY) %>% # same column used to assign geography to cores
#   select(-c(Rowid, FID, FREQUENCY, SUM_ST_AREA_SH)) %>% 
#   drop_na(country) # 13.9km^2 were not assigned geography

state_wetland_stocks <- us_stocks %>% ungroup() %>% 
  # mutate(country = ifelse(country == "Laos", "Vietnam", country)) %>% 
  left_join(hab_area) %>% 
  mutate(habitat_stocks_TgC = (stock_mean * estimated_area_ha)/10^6,
         habitat_stocks_TgC_se = (stock_se * estimated_area_ha)/10^6)

state_wetland_stocks %>% 
  drop_na(habitat_stocks_TgC) %>% 
  filter(habitat_simplified == "marsh") %>%
  mutate(state_abb = paste0(state_abb, ", n = ", n),
         state_abb = fct_reorder(state_abb, habitat_stocks_TgC)) %>% 
  ggplot(aes(habitat_stocks_TgC, state_abb,
             xmin = habitat_stocks_TgC - habitat_stocks_TgC_se, 
             xmax = habitat_stocks_TgC + habitat_stocks_TgC_se)) +
  geom_point() +
  geom_errorbar() +
  ggtitle("Marsh 1m Soil C Stocks by State") +
  theme_bw()




