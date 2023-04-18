## script reads in current version of CCN Data Library synthesis

library(tidyverse)

## read in data synthesis
root_dir <- "https://raw.githubusercontent.com/Smithsonian/CCRCN-Data-Library/main/data/CCRCN_synthesis/"

# species <- read_csv(paste0(root_dir, "CCRCN_species.csv"), guess_max = 7000)
# methods <- read_csv(paste0(root_dir, "CCRCN_methods.csv"), guess_max = 7000)
# impacts <- read_csv(paste0(root_dir, "CCRCN_impacts.csv"), guess_max = 7000)
cores <- read_csv(paste0(root_dir, "CCRCN_cores.csv"), guess_max = 7000)
depthseries <- read_csv(paste0(root_dir, "CCRCN_depthseries.csv"), guess_max = 60000)
# bib <- read_csv(paste0(root_dir, "CCRCN_study_citations.csv"), guess_max = 600)

# Soil carbon density (g cm-3) = dry bulk density (g cm-3) * (% C/100)
# Amount carbon in core section (g cm-2) = Soil carbon density (g cm-3) * sample thickness interval (cm)

# calculate carbon stock for each interval in the synthesis (test run, no standardization yet)
raw_ds_stock <- depthseries %>%
  drop_na(dry_bulk_density, fraction_carbon) %>% 
  mutate(carbon_density = dry_bulk_density * fraction_carbon,
         # calculate c stock in each interval (gC m-2)
         # g/cm2 * 10000cm2/m2
         cstock_interval = carbon_density * (depth_max-depth_min) * 10000)  %>% 
  arrange(core_id, depth_min) %>% select(cstock_interval, everything())

# density plot
raw_ds_stock %>% 
  # filter(cstock_interval > 50000) %>% 
  ggplot(aes(abs(cstock_interval))) + # two Trettin depths are reversed
  geom_density() +
  geom_rug()

# plot a profile or two
raw_ds_stock %>% 
  filter(study_id == "Okeefe-Suttles_et_al_2021_FL") %>% 
  ggplot(aes(depth_min, cstock_interval, col = core_id)) +
  geom_line() +
  geom_point() +
  scale_x_reverse() + coord_flip() +
  ylab("Carbon Stock (gC m-2)") +
  theme_bw()
# facet_wrap(~core_id)

# Standardize depth intervals ####
# what standard depth intervals will give the highest resolution C stock?
# do we standardize the depth intervals before or after calculating cstock for each interval?

# horizons <- data.frame(horizon_min = c(0,100),
#                        horizon_max = c(100,200))
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

## Core Stock Plots ####

## ...Country ####
country_stocks <- core_stocks %>% drop_na(core_stock) %>% 
  mutate(country = ifelse(country == "Laos", "Vietnam", country)) %>% 
  mutate(habitat = recode(habitat, "scrub shrub" = "scrub/shrub")) %>% 
  group_by(country, habitat) %>% 
  summarize(n = n(),
            stock_mean = mean(core_stock_MgHa, na.rm = T),
            stock_se = sd(core_stock_MgHa, na.rm = T))

country_stocks %>% ungroup() %>% 
  filter(habitat == "mangrove") %>%
  mutate(country = paste0(country, ", n = ", n),
         country = fct_reorder(country, stock_mean)) %>% 
  ggplot(aes(stock_mean, country,
             xmin = stock_mean - stock_se, xmax = stock_mean + stock_se)) +
  geom_point() +
  geom_errorbar() +
  xlab("Carbon Stocks (MgC ha-1)") +
  ggtitle("Mangrove 1m Core C Stocks by Country") +
  theme_bw()
ggsave("figures/country_mangrove_1m_corestocks.jpg")


## Scale Estimates by wetland size

# read in habitat area 
mangrove_world_area <- readxl::read_xls("ccn_inventory_tool/data/mangroveWorld_Area.xls") %>% 
  mutate(habitat = "mangrove",
         habitat_area_ha = SUM_ST_AREA_SH/10000) %>% 
  rename(country = COUNTRY) %>% # same column used to assign geography to cores
  select(-c(Rowid, FID, FREQUENCY, SUM_ST_AREA_SH)) %>% 
  drop_na(country) # 13.9km^2 were not assigned geography
  
tier1_stocks <- mangrove_world_area %>% 
  # calculate global values for countries that we don't have data for
  filter(!(country %in% unique(country_stocks$country))) %>% 
  mutate(tier = "global",
         stock_mean = 386,
         stock_min = 55,
         stock_max = 1376) 

mangrove_stocks <- country_stocks %>% ungroup() %>% 
  filter(habitat == "mangrove") %>% 
  mutate(tier = "local",
         stock_min = stock_se, stock_max = stock_se) %>% 
  select(-stock_se) %>% 
  left_join(mangrove_world_area) %>% 
  bind_rows(tier1_stocks) %>% 
  mutate(data_type = "soil",
         habitat_stocks_TgC = (stock_mean * habitat_area_ha)/10^6,
         habitat_stocks_TgC_min = (stock_min * habitat_area_ha)/10^6,
         habitat_stocks_TgC_max = (stock_max * habitat_area_ha)/10^6) %>% 
  # calculate co2 equivalent
  mutate(co2_equiv = habitat_stocks_TgC * 3.67,
         co2_equiv_min = habitat_stocks_TgC_min * 3.67,
         co2_equiv_max = habitat_stocks_TgC_max * 3.67) %>% 
  select(country, habitat, data_type, tier, everything())
# filter the dataset by data type (and sub data type?) ex. soils => mineral v organic?
write_csv(mangrove_stocks, "ccn_inventory_tool/app_input/mangrove_stocks.csv")

mangrove_stocks %>% 
  mutate(country = paste0(country, ", n = ", n),
         country = fct_reorder(country, habitat_stocks_TgC)) %>% 
  ggplot(aes(habitat_stocks_TgC, country, col = tier,
             xmin = habitat_stocks_TgC - habitat_stocks_TgC_min, 
             xmax = habitat_stocks_TgC + habitat_stocks_TgC_max)) +
  geom_point() +
  geom_errorbar() +
  ggtitle("Mangrove 1m Soil C Stocks by Country") +
  theme_bw()
ggsave("figures/country_mangrove_soilC_1m.jpg")

## Mapping Experimentation ----

library(plotly)
# library(cartography)

# list of target countries
targets <- c("Bangladesh",
             "Brazil",
             "Cambodia",
             "Cameroon",
             "Colombia",
             "Costa Rica",
             "Dominican Republic",
             "East Timor",
             "Ecuador",
             "Federated States of Micronesia",
             "Fiji",
             "Ghana",
             "India",
             "Indonesia",
             "Kiribati",
             "Malaysia",
             "Maldives",
             "Marshall Islands",
             "Mexico",
             "Nauru",
             "Palau",
             "Papua New Guinea",
             "The Philippines",
             "Samoa",
             "Senegal",
             "Solomon Islands",
             "South Africa",
             "Sri Lanka",
             "Thailand",
             "Tonga",
             "Tuvalu",
             "Uganda",
             "Vanuatu",
             "Vietnam")


