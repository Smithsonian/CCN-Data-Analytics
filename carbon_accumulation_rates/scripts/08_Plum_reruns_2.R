
# Redo's 2
# CAR workflow

# Libraries
library(tidyverse)
# devtools::install_github("https://github.com/Smithsonian/rplum")
library(rplum)
library(rbacon)

# Overwrite stuff?
overwrite_stuff <- T

# Quality of life functions
{
  
  match_target_horizons <- function(depthseries,
                                    target_horizons) {
    
    # depthseries_paired_down <- depthseries %>%
    # dplyr::select(study_id, site_id, everything(), -representative_depth_min, -representative_depth_max, -sample_id) %>%
    # dplyr::select(study_id:fraction_carbon) # %>%
    # dplyr::filter(core_id %in% subset_cores()$core_id) %>%
    # dplyr::mutate_at(c("depth_min", "depth_max", "dry_bulk_density", "fraction_carbon", "fraction_organic_matter"),
    #                  as.numeric)
    
    depthseries_horizons <- merge(depthseries, target_horizons) %>%
      # Keeps intervals between min and max horizon
      # If an interval crosses a horizon, it remains
      gather(key = "variable", value = "value", -c(depth_min, depth_max, horizon_min, horizon_max)) %>% 
      filter(complete.cases(value)) %>% 
      dplyr::filter(pmax(depth_min, horizon_min) < pmin(depth_max, horizon_max)) %>%
      dplyr::arrange(depth_min, depth_max) %>%
      # Calculate weights for each interval
      dplyr::mutate(overlapping_depth = pmin((depth_max-depth_min),
                                             (horizon_max-depth_min),
                                             (depth_max-horizon_min), na.rm=T)) %>%
      dplyr::group_by(variable, horizon_min, horizon_max) %>%
      dplyr::mutate(total_depth = sum(overlapping_depth),
                    weight = overlapping_depth / total_depth) %>%
      # Aggregate by horizon intervals
      dplyr::summarise(value=sum(value*weight)) %>% 
      ungroup() %>% 
      spread(key = variable, value = value)
    
    return(depthseries_horizons)
    
  }
  
  habitat_specfic_om_to_oc <- function(om, habitat) {
    
    oc <- ifelse(habitat == "marsh", om^2 * 0.063454 + om * 0.427391,
                 ifelse(habitat == "seagrass", om * 0.43, # Fourqueen et al., 2012
                        ifelse(habitat %in% c("mangrove", "swamp", "scrub/shrub"),  om^2 * -0.015966 + om * 0.485667, 
                               NA
                        )))
    return(oc)
    
  }
  
}

# cores
cores <- read_csv("data/CCN_cores.csv") %>% 
  filter(!is.na(dates_qual_code)&!is.na(stocks_qual_code)) %>% 
  filter(habitat %in% c("marsh", "mangrove", "seagrass", "swamp")) %>% 
  arrange(study_id, site_id, core_id)

depths <- read_csv("data/CCN_depthseries.csv")
depths <- read_csv("data/CCN_depthseries.csv", guess_max = nrow(depths), na = c("NA", ".", "no data"))
depths$depth_max <- as.numeric(depths$depth_max)
depths$depth_min <- as.numeric(depths$depth_min)

library(leaflet)

# plot cores with no associated admin division
cores %>% 
  leaflet() %>%
  addProviderTiles(providers$CartoDB) %>%
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   radius = 2, label = ~study_id)

# 210Pb
pb_profiles <- read_csv("carbon_accumulation_rates/output/tabs/pb210_profiles.csv") %>% 
  distinct_all() %>% 
  group_by(study_id, site_id, core_id, depth_min, depth_max, radioisotope) %>% 
  summarise(radioactivity = mean(radioactivity,na.rm=T), 
            error = mean(error,na.rm=T)) %>% 
  ungroup()

pb_means <- pb_profiles %>% 
  select(-error) %>% 
  pivot_wider(names_from = radioisotope, values_from = radioactivity)

pb_error <- pb_profiles %>% 
  select(-radioactivity) %>% 
  pivot_wider(names_from = radioisotope, values_from = error) %>% 
  rename(total_pb210_activity_se = total_pb210_activity,
         ra226_activity_se = ra226_activity,
         excess_pb210_activity_se = excess_pb210_activity)

bd_data <- depths %>% 
  select(study_id:depth_max, dry_bulk_density, fraction_organic_matter) %>% 
  distinct_all() %>% 
  group_by(study_id, site_id, core_id, depth_min, depth_max) %>% 
  summarise(dry_bulk_density = mean(dry_bulk_density, na.rm = T),
            fraction_organic_matter = mean(fraction_organic_matter, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(dry_bulk_density = ifelse(dry_bulk_density<=0, NA, dry_bulk_density)) %>% 
  mutate(dry_bulk_density_gap_filled = ifelse(is.na(dry_bulk_density),
                                              1/((fraction_organic_matter/0.093) + ((1-fraction_organic_matter)/1.590)),
                                              dry_bulk_density)) %>% 
  group_by(study_id, site_id, core_id, depth_min, depth_max) %>% 
  filter(complete.cases(dry_bulk_density_gap_filled)) %>% 
  summarise(dry_bulk_density_gap_filled = mean(dry_bulk_density_gap_filled, na.rm = T)) %>% 
  ungroup()

pb_profiles2 <- pb_means %>% 
  left_join(pb_error) %>% 
  left_join(bd_data) %>% 
  filter(complete.cases(total_pb210_activity))
pb_profiles2[pb_profiles2 == NaN] <- NA

# Iterate through pb_profiles2 
unique_cores <- pb_profiles2 %>% 
  select(study_id, site_id, core_id) %>% 
  distinct_all()

for (i in 1:nrow(unique_cores)) {
  
  this_study_id <- unique_cores$study_id[i]
  this_site_id <- unique_cores$site_id[i]
  this_core_id <- unique_cores$core_id[i]
  
  pb_subset <- pb_profiles2 %>% 
    filter(study_id == this_study_id,
           site_id == this_site_id,
           core_id == this_core_id)
  
  # If there are any missing values,
  # try to 
  if (any(is.na(pb_subset$dry_bulk_density_gap_filled))) {
    
    bd_subset <- bd_data %>% 
      filter(study_id == this_study_id,
             site_id == this_site_id,
             core_id == this_core_id) %>% 
      select(-c(study_id, site_id, core_id))
    
    
    depth_intervals <- match_target_horizons(bd_subset, 
                                             data.frame(horizon_min = pb_subset$depth_min, 
                                                        horizon_max = pb_subset$depth_max)) %>% 
      rename(depth_min = horizon_min,
             depth_max = horizon_max) %>% 
      ungroup()
    
    pb_subset_bd_joined <- pb_subset %>% 
      select(-dry_bulk_density_gap_filled) %>% 
      left_join(depth_intervals)
    
    if (! "dry_bulk_density_gap_filled" %in% names(pb_subset_bd_joined)) {
      pb_subset_bd_joined <- mutate(pb_subset_bd_joined, dry_bulk_density_gap_filled = NA)
    }
    
    # If there are any more blank bulk density data, approximate them
    if (any(is.na(pb_subset_bd_joined$dry_bulk_density_gap_filled)) & 
        !all(is.na(pb_subset_bd_joined$dry_bulk_density_gap_filled))) {
      
      pb_subset_bd_joined$dry_bulk_density_gap_filled <- approx(x=pb_subset_bd_joined$depth_max, y=pb_subset_bd_joined$dry_bulk_density_gap_filled, xout = pb_subset_bd_joined$depth_max, rule = 2)$y
      
      
    }
    
    pb_profiles2$dry_bulk_density_gap_filled[pb_profiles2$study_id == this_study_id &
                                               pb_profiles2$site_id == this_site_id & 
                                               pb_profiles2$core_id == this_core_id] <- pb_subset_bd_joined$dry_bulk_density_gap_filled
    
  }
  
  
  
}

# Attributes needed for Plum table
# labID,depth(cm),density(g/cm^3),210Pb(Bq/kg),sd(210Pb),thickness(cm)
pb210_dates <- pb_profiles2 %>% 
  group_by(study_id, site_id, core_id) %>% 
  mutate(labID = 1:n()) %>% 
  ungroup() %>% 
  rename(`depth(cm)` = depth_max,
         `density(g/cm^3)`=dry_bulk_density_gap_filled,
         `210Pb(Bq/kg)` = total_pb210_activity,
         `sd(210Pb)` = total_pb210_activity_se,
         `226Ra(Bq/kg)` = ra226_activity,
         `sd(226Ra)` = ra226_activity_se) %>% 
  mutate(interval = `depth(cm)` - depth_min) %>% 
  
  # relations between the names of columns and their positions in the .csv file
  # idColumn       = 1
  # depthColumn    = 2
  # rhoColumn      = 3 # density
  # plumdataColumn = 4 # means of measurements
  # stdColumn      = 5 # their errors
  # deltaColumn    = 6 # sample thickess
  # radonColumn    = 7 # if present
  # sdRadonColumn  = 8 # if present
  select(study_id, site_id, core_id, labID, `depth(cm)`, `density(g/cm^3)`, `210Pb(Bq/kg)`, 
         `sd(210Pb)`, interval, `226Ra(Bq/kg)`, `sd(226Ra)`) %>% 
  filter(complete.cases(`210Pb(Bq/kg)`),
         `210Pb(Bq/kg)`>0) %>% 
  mutate(`sd(226Ra)` = ifelse(`226Ra(Bq/kg)`<=0, NA, `sd(226Ra)`),
         `226Ra(Bq/kg)` = ifelse(`226Ra(Bq/kg)`<=0, NA, `226Ra(Bq/kg)`))

# 137Cs
cs_peaks <- read_csv("carbon_accumulation_rates/output/tabs/peak_137Cs_accretion_rates.csv") %>% 
  filter(sig_level >= 2)

# labID
# age
# error
# depth
# cc = 0
cs_peaks <- cs_peaks %>% 
  group_by(study_id, site_id, core_id) %>% 
  mutate(labID = paste("Cs_", 1:n(), sep=""),
         age = 1950 - age_mean,
         error = age_sd,
         depth = depth_med,
         cc = 0) %>% 
  ungroup() %>% 
  select(study_id:core_id, labID:cc)

# 14C
c14_ages <- read_csv("carbon_accumulation_rates/output/tabs/c14_ages.csv")

c14_ages <- c14_ages %>% 
  filter(c14_age_se>0,
         carbonate_or_organic == "organic") %>% 
  group_by(study_id, site_id, core_id) %>% 
  mutate(labID = paste("C14_", 1:n(), sep=""),
         age = c14_age,
         error = c14_age_se,
         depth = depth_med,
         cc = ifelse(carbonate_or_organic == "carbonate", 2, 1),
         n=n()) %>% 
  ungroup() %>%
  filter(n>1) %>% 
  select(study_id:core_id, labID:cc)

# Historical horizon
historical_horizons <- read_csv("carbon_accumulation_rates/output/tabs/historical_horizon_accretion_rates.csv")

historical_horizons <- historical_horizons %>% 
  group_by(study_id, site_id, core_id) %>% 
  mutate(labID = paste("historical_horizon_", 1:n(), sep=""),
         age = 1950 - age_mean,
         error = age_sd,
         depth = depth_med,
         cc = 0) %>% 
  ungroup() %>% 
  select(study_id:core_id, labID:cc)

pb210_dates_max <- pb210_dates %>% 
  group_by(study_id, site_id, core_id) %>% 
  summarise(max_pb_depth = max(`depth(cm)`)) %>% 
  ungroup()

# All ancillary dates 
ancillary_dates <- bind_rows(cs_peaks, historical_horizons,
                             c14_ages) %>% 
  arrange(study_id, site_id, core_id, depth) %>% 
  ungroup() %>% 
  left_join(pb210_dates_max) %>% 
  filter(is.na(max_pb_depth)|depth<=max_pb_depth) %>% 
  select(-max_pb_depth)

# Filter out anything in ancillary dates that's below a 210Pb profiles

pb210_dates_max <- pb210_dates %>% 
  group_by(study_id, site_id, core_id) %>% 
  summarise(max_210Pb_depth = max(`depth(cm)`)) %>% 
  ungroup()

ancillary_dates2 <- ancillary_dates %>% 
  left_join(pb210_dates_max) %>% 
  filter(is.na(max_210Pb_depth)|depth<=max_210Pb_depth)

# All unique cores
for (i in 1:nrow(cores)) {
  
  this_study_id <- cores$study_id[i]
  this_site_id <- cores$site_id[i]
  this_core_id <- cores$core_id[i]
  
  print(paste0("Running ", this_study_id, ", ", this_site_id, ", ", this_core_id, "..."))
  
  pb_subset <- pb210_dates %>% filter(study_id == this_study_id,
                                      site_id == this_site_id,
                                      core_id == this_core_id) %>% 
    select(-study_id, -site_id, -core_id)
  
  ancillary_subset <- ancillary_dates %>% filter(study_id == this_study_id,
                                                 site_id == this_site_id,
                                                 core_id == this_core_id)
  
  c14_subset <- c14_ages %>% filter(study_id == this_study_id,
                                    site_id == this_site_id,
                                    core_id == this_core_id)
  
  # If 210-Pb
  if (nrow(pb_subset)>=1) {
    
    # Assume we're not using extra dates unless we add them
    use_extra_dates_file <- F
    
    # Create file name
    temp_core_name <- paste(this_study_id, this_site_id, this_core_id, sep="_")
    temp_core_name <- str_replace_all(temp_core_name, " ", "_")
    temp_core_name <- str_replace_all(temp_core_name, "-", "_")
    
    # If the thing produced an output in the first place
    og_file <- paste("Plum/Cores/", temp_core_name, "/", temp_core_name, "_tidy_plum.csv", sep="")
    
    # Check to see if the file is already done
    # If the thing produced an output in the first place
    og_file <- paste("Plum/Cores/", temp_core_name, "/", temp_core_name, "_tidy_plum.csv", sep="")
    # . then go onto check
    if (file.exists(og_file)) {
      # If posterior memory is high and peaked rerun
      tidyBaconOg <- read_csv(og_file)
      
      if (min(tidyBaconOg$memory)>0.98) {
        print("... re-running")
        
        # Should go here. 
        # Query surface age
        this_core <- cores %>%  filter(study_id == this_study_id,
                                       site_id == this_site_id,
                                       core_id == this_core_id)
        
        surface_date <- this_core$year[1]
        
        # Are cs peaks, horizons, or 14C present?
        if (nrow(ancillary_subset)>=1) {
          
          # Specify we are using extra tables
          use_extra_dates_file <- T
          ancillary_write_to_file <- select(ancillary_subset, labID:cc)
          
          # Write file
          write_csv(ancillary_write_to_file, paste("Plum/Cores/", temp_core_name, "/ancillary_dates.csv", sep=""))
        }
        
        # What assumption are we going to use for background
        n_pb210 <- sum(!is.na(pb_subset$`210Pb(Bq/kg)`))
        n_ra226 <- sum(!is.na(pb_subset$`226Ra(Bq/kg)`))
        
        # Decide on the radon case
        # If there is no radon
        if (n_ra226 == 0) {
          radon_case <- 0
        } else if (n_pb210 == n_ra226) {
          # If number of Ra samples is equal to number of total 210 samples
          radon_case <- 1
        } else if (n_ra226 < n_pb210) {
          # If number of Ra samples is less than the number of total 210 samples
          radon_case <- 1
        } 
        
        pb_subset <- pb_subset[,colSums(is.na(pb_subset))<nrow(pb_subset)]  
        
        # Write CSV
        write_csv(pb_subset, paste("Plum/Cores/", temp_core_name, "/", temp_core_name, ".csv", sep=""))
        
        temp_thickness <- min(pb_subset$interval)
        
        try({
          # Run Plum
          # Can't do radon case = 2 because there are missing values
          # Check if there is a 137Cs date
          if (use_extra_dates_file) { 
            
            print(paste0("... Running PLUM with ancillary dates. Radon case = ", radon_case))
            
            
            rplum::Plum(core=temp_core_name, thick = temp_thickness, coredir="Plum/Cores/",
                        date.sample=surface_date, radon.case=radon_case, suggest=F, ask=F, remember=F,
                        otherdates = "ancillary_dates", verbose=F,
                        mem.mean = 0.5, acc.mean = 5)
            
          } else {
            
            print(paste0("... Running PLUM with no ancillary dates. Radon case = ", radon_case))
            
            rplum::Plum(core=temp_core_name, thick = temp_thickness, coredir="Plum/Cores/",
                        date.sample=surface_date, radon.case=radon_case, suggest=F, ask=F, remember=F, verbose=F,
                        mem.mean = 0.5, acc.mean = 5)
          }
          gc()
          dev.off()
          
          ## convert MCMC output to friendlier data.frame
          friendlyBacon <- as_tibble(info$output) 
          
          n_depth_ints <- ncol(friendlyBacon)-2
          
          friendlyBaconNames <- c("logOdds",
                                  paste("accRate[", (1:n_depth_ints)*temp_thickness,"]", sep=""),
                                  "memory")
          names(friendlyBacon) <- friendlyBaconNames
          
          tidyBacon <- friendlyBacon %>% 
            mutate(iteration = 1:n()) %>% 
            gather(key = "depth_code", value = "accretion", -logOdds, -memory, -iteration) %>% 
            select(iteration, logOdds, memory, depth_code, accretion) %>% 
            arrange(iteration)
          
          write_csv(tidyBacon, paste("Plum/Cores/", temp_core_name, "/", temp_core_name, "_tidy_plum.csv", sep=""))
          
        })
      } else (print("... all good."))
    } else {
      print("... re-running")
      
      # Should go here. 
      # Query surface age
      this_core <- cores %>%  filter(study_id == this_study_id,
                                     site_id == this_site_id,
                                     core_id == this_core_id)
      
      surface_date <- this_core$year[1]
      
      # Are cs peaks, horizons, or 14C present?
      if (nrow(ancillary_subset)>=1) {
        
        # Specify we are using extra tables
        use_extra_dates_file <- T
        ancillary_write_to_file <- select(ancillary_subset, labID:cc)
        
        # Write file
        write_csv(ancillary_write_to_file, paste("Plum/Cores/", temp_core_name, "/ancillary_dates.csv", sep=""))
      }
      
      # What assumption are we going to use for background
      n_pb210 <- sum(!is.na(pb_subset$`210Pb(Bq/kg)`))
      n_ra226 <- sum(!is.na(pb_subset$`226Ra(Bq/kg)`))
      
      # Decide on the radon case
      # If there is no radon
      if (n_ra226 == 0) {
        radon_case <- 0
      } else if (n_pb210 == n_ra226) {
        # If number of Ra samples is equal to number of total 210 samples
        radon_case <- 1
      } else if (n_ra226 < n_pb210) {
        # If number of Ra samples is less than the number of total 210 samples
        radon_case <- 1
      } 
      
      pb_subset <- pb_subset[,colSums(is.na(pb_subset))<nrow(pb_subset)]  
      
      # Write CSV
      write_csv(pb_subset, paste("Plum/Cores/", temp_core_name, "/", temp_core_name, ".csv", sep=""))
      
      temp_thickness <- min(pb_subset$interval)
      
      try({
        # Run Plum
        # Can't do radon case = 2 because there are missing values
        # Check if there is a 137Cs date
        if (use_extra_dates_file) { 
          
          print(paste0("... Running PLUM with ancillary dates. Radon case = ", radon_case))
          
          
          rplum::Plum(core=temp_core_name, thick = temp_thickness, coredir="Plum/Cores/",
                      date.sample=surface_date, radon.case=radon_case, suggest=F, ask=F, remember=F,
                      otherdates = "ancillary_dates", verbose=F,
                      mem.mean = 0.5, acc.mean = 5)
          
        } else {
          
          print(paste0("... Running PLUM with no ancillary dates. Radon case = ", radon_case))
          
          rplum::Plum(core=temp_core_name, thick = temp_thickness, coredir="Plum/Cores/",
                      date.sample=surface_date, radon.case=radon_case, suggest=F, ask=F, remember=F, verbose=F,
                      mem.mean = 0.5, acc.mean = 5)
        }
        gc()
        dev.off()
        
        ## convert MCMC output to friendlier data.frame
        friendlyBacon <- as_tibble(info$output) 
        
        n_depth_ints <- ncol(friendlyBacon)-2
        
        friendlyBaconNames <- c("logOdds",
                                paste("accRate[", (1:n_depth_ints)*temp_thickness,"]", sep=""),
                                "memory")
        names(friendlyBacon) <- friendlyBaconNames
        
        tidyBacon <- friendlyBacon %>% 
          mutate(iteration = 1:n()) %>% 
          gather(key = "depth_code", value = "accretion", -logOdds, -memory, -iteration) %>% 
          select(iteration, logOdds, memory, depth_code, accretion) %>% 
          arrange(iteration)
        
        write_csv(tidyBacon, paste("Plum/Cores/", temp_core_name, "/", temp_core_name, "_tidy_plum.csv", sep=""))
        
      })
      
    }
  }
}