# Multi-Horizon CARS

# Library tidyverse
library(tidyverse)

# cores
cores <- read_csv("data/CCRCN_cores.csv") %>% 
  filter(!is.na(dates_qual_code)&!is.na(stocks_qual_code)) %>% 
  filter(habitat %in% c("marsh", "mangrove", "seagrass", "swamp")) %>% 
  arrange(study_id, site_id, core_id)

depths <- read_csv("data/CCRCN_depthseries.csv")
depths <- read_csv("data/CCRCN_depthseries.csv", guess_max = nrow(depths), na = c("NA", ".", "no data"))
depths$depth_max <- as.numeric(depths$depth_max)
depths$depth_min <- as.numeric(depths$depth_min)

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

cores_output <- cores %>% 
  mutate(accretion_yr_per_cm_mean = NA,
         accretion_yr_per_cm_se = NA,
         accretion_yr_per_cm_upper_CI = NA,
         accretion_yr_per_cm_lower_CI = NA,
         organic_matter_accumulation_mean = NA,
         organic_matter_accumulation_se =  NA,
         organic_matter_accumulation_upper_CI = NA,
         organic_matter_accumulation_lower_CI =  NA,
         carbon_accumulation_mean = NA,
         carbon_accumulation_se =  NA,
         carbon_accumulation_upper_CI = NA,
         carbon_accumulation_lower_CI = NA,
         max_age = NA,
         max_age_se = NA,
         max_age_upper_CI = NA,
         max_age_lower_CI = NA)





# All unique cores
for (i in 1:nrow(cores_output)) {
  
  this_study_id <- cores_output$study_id[i]
  this_site_id <- cores_output$site_id[i]
  this_core_id <- cores_output$core_id[i]
  
  print(paste0("Running ", this_study_id, ", ", this_site_id, ", ", this_core_id, "..."))
  
  # Create file name
  temp_core_name <- paste(this_study_id, this_site_id, this_core_id, sep="_")
  temp_core_name <- str_replace_all(temp_core_name, " ", "_")
  temp_core_name <- str_replace_all(temp_core_name, "-", "_")
  
  if (file.exists(paste("Plum/Cores/", temp_core_name, "/", temp_core_name, "_tidy_plum.csv", sep=""))) {
     
    tidyPlum<- read_csv(paste("Plum/Cores/", temp_core_name, "/", temp_core_name, "_tidy_plum.csv", sep="")) %>% 
      arrange(iteration) %>% 
      mutate(depth_max = as.numeric(str_remove_all(depth_code, "accRate|\\[|\\]")))
    
    if (min(tidyPlum$memory) < 0.99) {
      interval = tidyPlum$depth_max[1]
      
      tidyPlum <- tidyPlum %>% 
        mutate(depth_min = depth_max - interval)
      
      unique_mins <- unique(tidyPlum$depth_min)
      unique_maxs <- unique(tidyPlum$depth_max)
      
      # Match to see if om and bd are same resolution as oc
      depth_intervals <- depths %>% 
        filter(study_id == this_study_id, site_id == this_site_id, core_id == this_core_id) %>% 
        select(depth_min, depth_max, dry_bulk_density, fraction_organic_matter, fraction_carbon) %>% 
        filter(depth_max <= max(tidyPlum$depth_max))
      
      # query core info
      core_info <- cores %>% filter(study_id == this_study_id, site_id == this_site_id, core_id == this_core_id)
      
      if (! (all(depth_intervals$depth_min == unique_mins) & all(depth_intervals$depth_max == unique_maxs))) {
        
        depth_intervals <- match_target_horizons(depth_intervals, 
                                                 data.frame(horizon_min = unique_mins, horizon_max = unique_maxs)) %>% 
          rename(depth_min = horizon_min,
                 depth_max = horizon_max) %>% 
          ungroup()
        
        if (! "fraction_organic_matter" %in% names(depth_intervals)) {depth_intervals <- mutate(depth_intervals, fraction_organic_matter = NA)}
        if (! "dry_bulk_density" %in% names(depth_intervals)) {depth_intervals <- mutate(depth_intervals, dry_bulk_density = NA)}
        if (! "fraction_carbon" %in% names(depth_intervals)) {depth_intervals <- mutate(depth_intervals, fraction_carbon = NA)}
        
      }
      
      # If BD is missing gap fill
      if (any(is.na(depth_intervals$dry_bulk_density))) {
        
        depth_intervals <- depth_intervals %>% 
          mutate(dry_bulk_density = ifelse(is.na(dry_bulk_density),
                                           1/((fraction_organic_matter/0.093) + ((1-fraction_organic_matter)/1.590)),
                                           dry_bulk_density))
        
      }
      
      
      # If OC is missing, gap fill
      if (any(is.na(depth_intervals$fraction_carbon))) {
        
        # marsh, mangrove or seagrass
        this_habitat <- core_info$habitat[1]  
        
        new_oc <-  habitat_specfic_om_to_oc(om = depth_intervals$fraction_organic_matter,
                                            habitat = rep(this_habitat, length(depth_intervals$fraction_organic_matter)))
        
        depth_intervals <- depth_intervals %>% 
          mutate(fraction_carbon = ifelse(is.na(fraction_carbon), new_oc,
                                          fraction_carbon))
      }
      
      # Join regularized and gap filled version
      joined_plum <- tidyPlum %>% 
        left_join(depth_intervals)
      
      this_year <- core_info$year[1]
      
      # Calculate ages relative to collection
      joined_plum <- joined_plum %>% 
        mutate(interval = interval,
               n_years = accretion * interval) %>% 
        group_by(iteration) %>% 
        mutate(age = cumsum(n_years)) %>%
        ungroup() %>% 
        mutate(cal_age_ybp = (1950-this_year) + age) %>% 
        filter(age <= 120) %>% 
        mutate(organic_matter_accumulation = (dry_bulk_density*fraction_organic_matter)/accretion * 10000, 
               carbon_accumulation = (dry_bulk_density*fraction_carbon)/accretion * 10000)
      
      core_averages <- joined_plum %>% 
        group_by(iteration) %>% 
        summarise(accretion_yr_per_cm = mean(accretion, na.rm = T),
                  organic_matter_accumulation = mean(organic_matter_accumulation, na.rm=T),
                  carbon_accumulation = mean(carbon_accumulation, na.rm = T),
                  age_ybp = min(cal_age_ybp)) %>% 
        ungroup()
      
      simulation_averages <- core_averages %>% 
        group_by() %>% 
        summarise(accretion_yr_per_cm_mean = mean(accretion_yr_per_cm, na.rm = T),
                  accretion_yr_per_cm_se = sd(accretion_yr_per_cm, na.rm = T),
                  accretion_yr_per_cm_upper_CI = quantile(accretion_yr_per_cm, 0.975),
                  accretion_yr_per_cm_lower_CI = quantile(accretion_yr_per_cm, 0.025),
                  organic_matter_accumulation_mean = mean(organic_matter_accumulation, na.rm = T),
                  organic_matter_accumulation_se =  sd(organic_matter_accumulation, na.rm = T),
                  organic_matter_accumulation_upper_CI = quantile(organic_matter_accumulation, 0.975, na.rm = T),
                  organic_matter_accumulation_lower_CI =  quantile(organic_matter_accumulation, 0.025, na.rm = T),
                  carbon_accumulation_mean = mean(carbon_accumulation, na.rm = T),
                  carbon_accumulation_se =  sd(carbon_accumulation, na.rm = T),
                  carbon_accumulation_upper_CI = quantile(carbon_accumulation, 0.975, na.rm = T),
                  carbon_accumulation_lower_CI =  quantile(carbon_accumulation, 0.025, na.rm = T),
                  max_age = mean(age_ybp, na.rm=T),
                  max_age_se = sd(age_ybp, na.rm=T),
                  max_age_upper_CI = quantile(age_ybp, 0.975, na.rm = T),
                  max_age_lower_CI =  quantile(age_ybp, 0.025, na.rm = T))
      
      # Add to core output file
      cores_output$accretion_yr_per_cm_mean[i] <- simulation_averages$accretion_yr_per_cm_mean[1]
      cores_output$accretion_yr_per_cm_se[i] <- simulation_averages$accretion_yr_per_cm_se[1]
      cores_output$accretion_yr_per_cm_upper_CI[i] <- simulation_averages$accretion_yr_per_cm_upper_CI[1]
      cores_output$carbon_accumulation_lower_CI[i] <- simulation_averages$accretion_yr_per_cm_lower_CI[1]
      
      cores_output$organic_matter_accumulation_mean[i] <- simulation_averages$organic_matter_accumulation_mean[1]
      cores_output$organic_matter_accumulation_se[i] <- simulation_averages$organic_matter_accumulation_se[1]
      cores_output$organic_matter_accumulation_upper_CI[i] <- simulation_averages$organic_matter_accumulation_upper_CI[1]
      cores_output$organic_matter_accumulation_lower_CI[i] <- simulation_averages$organic_matter_accumulation_lower_CI[1]
      
      cores_output$carbon_accumulation_mean[i] <- simulation_averages$carbon_accumulation_mean[1]
      cores_output$carbon_accumulation_se[i] <- simulation_averages$carbon_accumulation_se[1]
      cores_output$carbon_accumulation_upper_CI[i] <- simulation_averages$carbon_accumulation_upper_CI[1]
      cores_output$carbon_accumulation_lower_CI[i] <- simulation_averages$carbon_accumulation_lower_CI[1]
      
      cores_output$max_age[i] <- simulation_averages$max_age[1]
      cores_output$max_age_se[i] <- simulation_averages$max_age_se[1]
      cores_output$max_age_upper_CI[i] <- simulation_averages$max_age_upper_CI[1]
      cores_output$max_age_lower_CI[i] <- simulation_averages$max_age_lower_CI[1] 
    }
  }
}




write_csv(cores_output, "carbon_accumulation_rates/ccn_car_210Pb.csv")

View(cores_output %>% filter(admin_division == "Maryland") %>% 
       select(study_id:core_id, accretion_yr_per_cm_mean:carbon_accumulation_lower_CI))

# simulation_averages <- core_averages %>% 
#   group_by() %>% 
#   summarise(accretion_yr_per_cm_mean = mean(accretion_yr_per_cm, na.rm = T),
#             accretion_yr_per_cm_se = sd(accretion_yr_per_cm, na.rm = T),
#             accretion_yr_per_cm_upper_CI = quantile(accretion_yr_per_cm, 0.975),
#             accretion_yr_per_cm_lower_CI = quantile(accretion_yr_per_cm, 0.025),
#             organic_matter_accumulation_mean = mean(organic_matter_accumulation, na.rm = T),
#             organic_matter_accumulation_se =  sd(organic_matter_accumulation, na.rm = T),
#             organic_matter_accumulation_upper_CI = quantile(organic_matter_accumulation, 0.975, na.rm = T),
#             organic_matter_accumulation_lower_CI =  quantile(organic_matter_accumulation, 0.025, na.rm = T),
#             carbon_accumulation_mean = mean(carbon_accumulation, na.rm = T),
#             carbon_accumulation_se =  sd(carbon_accumulation, na.rm = T),
#             carbon_accumulation_upper_CI = quantile(carbon_accumulation, 0.975, na.rm = T),
#             carbon_accumulation_lower_CI =  quantile(carbon_accumulation, 0.025, na.rm = T)
#   ) %>% 
#   mutate(site_id = core_to_analyse$site_id[1], core_id = core_to_analyse$core_id[1],
#          dating_depth_max= max(tidyBacon$depth_max)) %>% 
#   select(site_id:dating_depth_max, accretion_yr_per_cm_mean:carbon_accumulation_lower_CI)