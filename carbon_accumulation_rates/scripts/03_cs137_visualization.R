# Cs 137 analysis

library(ggrepel)
library(tidyverse)

# Load in core data
depths <- read_csv("data/CCN_depthseries.csv")
depths <- read_csv("data/CCN_depthseries.csv", guess_max = nrow(depths), na = c("NA", ".", "no data"))
cores <- read_csv("data/CCN_cores.csv")
cores <- read_csv("data/CCN_cores.csv",
                  guess_max = nrow(cores))
depths$depth_max <- as.numeric(depths$depth_max)
depths$depth_min <- as.numeric(depths$depth_min)

# Nolte_2020 NFB NFB_ungrazed_landward 0 to 64, should be 62 to 64
# Nolte_2020 HH HH_ungrazed_landward 0 to 50, should be 48 to 50
depths[grepl("NFB_ungrazed_landward", depths$core_id) & depths$depth_max == 64, "depth_min"] <- 62
depths[grepl("HH_ungrazed_landward", depths$core_id) & depths$depth_max == 50, "depth_min"] <- 48

depths <- depths %>% 
  filter(complete.cases(depth_min, depth_max))

any(!is.na(depths$pb214_activity)&is.na(depths$pb214_unit))
any(!is.na(depths$total_pb210_activity)&is.na(depths$pb210_unit))

depths$study_id[which(!is.na(depths$excess_pb210_activity)&is.na(depths$pb210_unit))]

any(!is.na(depths$ra226_activity)&is.na(depths$ra226_activity))
any(!is.na(depths$bi214_activity)&is.na(depths$bi214_unit))


# Subset only cores that have one of the following
# Total 210Pb, Excess 210Pb
cs_depths <- depths %>% 
  filter(!is.na(cs137_activity)) %>% 
  select(study_id:depth_max, cs137_activity:cs137_activity_se) %>% 
  distinct_all() %>% 
  group_by(study_id, site_id, core_id, method_id, depth_min, depth_max) %>% 
  summarise(cs137_activity = mean(cs137_activity, na.rm=T),
            cs137_activity_se = mean(cs137_activity_se, na.rm = T)) %>% 
  ungroup()

unique(cs_depths$study_id)

hist(log(cs_depths$cs137_activity_se/cs_depths$cs137_activity))

high_uncertainty <- exp(quantile(log(cs_depths$cs137_activity_se/cs_depths$cs137_activity), 
                                 0.975, na.rm=T))

cs_depths_gap_filled <- cs_depths %>% 
  mutate(cs137_activity_se = ifelse(!is.na(cs137_activity_se), 
                                    cs137_activity_se,
                                    cs137_activity * high_uncertainty
                                    ))

# What countries are represented?
cores_filtered <- cores %>% 
  filter(study_id %in% cs_depths_gap_filled$study_id)

# There is only one study from which we expect double peaks (Europe)
unique(cores_filtered$study_id[cores_filtered$country %in% c("Germany", "Netherlands")])

# Locate peaks
peaks_located <- cs_depths_gap_filled %>% 
  arrange(study_id, site_id, core_id, depth_min, depth_max) %>% 
  group_by(study_id, site_id, core_id) %>% 
  mutate(is_peak = ifelse(cs137_activity > lag(cs137_activity) & cs137_activity > lead(cs137_activity), 1, 0),
         max_core_depth = max(depth_max)) %>% 
  arrange(study_id, site_id, core_id, -is_peak, -cs137_activity) %>% 
  mutate(rank = 1:n()) %>% 
  ungroup() %>% 
  arrange(study_id, site_id, core_id, -is_peak, depth_min, depth_max)

# Peak depth ranking
peaks_dated_pre <-  peaks_located %>% 
  filter(rank == 1 | rank == 2 & study_id == "Nolte_2020",
         depth_max != max_core_depth) %>% 
  group_by(study_id, site_id, core_id) %>% 
  mutate(depth_rank = 1:n()) %>% 
  ungroup() %>% 
  mutate(date = ifelse(study_id == "Nolte_2020" & depth_rank == 1, 1986, 
                       ifelse(study_id == "Nolte_2020" & depth_rank == 2, 1963,
                              ifelse(depth_rank == 1, 1963, NA))))

# Assign ages to peaks
peaks_dated<- peaks_located %>% 
  arrange(study_id, site_id, core_id, depth_max, depth_min) %>% 
  left_join(peaks_dated_pre)


# Is it significant for site?
depths_z <- peaks_dated %>% 
  filter(cs137_activity >0) %>% 
  group_by(study_id, site_id) %>% 
  mutate(mean_cs = mean(cs137_activity),
         sd_cs = sd(cs137_activity)) %>% 
  ungroup() %>% 
  mutate(z_score = (cs137_activity - mean_cs) / sd_cs) %>% 
  mutate(p_value_overall = ifelse(!is.na(date), 1-pnorm(z_score), NA)) %>% 
  # create and index
  group_by(study_id, site_id, core_id) %>% 
  mutate(index = 1:n(),
         n_dates = n()) %>% 
  mutate(upper_p = NA,
         lower_p = NA,
         min_possible_peak_depth = NA,
         max_possible_peak_depth = NA) %>% 
  ungroup() %>% 
  filter(n_dates > 5)



# Integral of area shared by two curves
min.f1f2 <- function(x, mu1, mu2, sd1, sd2) {
  f1 <- dnorm(x, mean=mu1, sd=sd1)
  f2 <- dnorm(x, mean=mu2, sd=sd2)
  pmin(f1, f2)
}

depths_z$is_peak[is.na(depths_z$is_peak)] <- 0

# Does the peak stand out from it's neighboring values
 for (i in 1:nrow(depths_z)) {
   # Upper and lower p-values
   if (!is.na(depths_z$date[i])) {
     if (depths_z$index[i] == 1) {
       depths_z$upper_p[i] <- 0
       depths_z$min_possible_peak_depth[i] <-0
       
     } else {
       
       try({
         depths_z$upper_p[i] <- integrate(min.f1f2, -Inf, Inf, mu1 = depths_z$cs137_activity[i], mu2 = depths_z$cs137_activity[i-1],
                                          sd1 = depths_z$cs137_activity_se[i], sd2 = depths_z$cs137_activity_se[i-1])$value[1]
         depths_z$min_possible_peak_depth[i] <- depths_z$depth_max[i-1]
       })
     }
     
     try({
       depths_z$lower_p[i] <- integrate(min.f1f2, -Inf, Inf, mu1 = depths_z$cs137_activity[i], mu2 = depths_z$cs137_activity[i+1],
                                        sd1 = depths_z$cs137_activity_se[i], sd2 = depths_z$cs137_activity_se[i+1])$value[1]
       
       depths_z$max_possible_peak_depth[i] <- depths_z$depth_min[i+1]
     })
   }
 }

depths_sig <- depths_z %>% 
  mutate(sig_level = as.character(as.numeric(p_value_overall <= 0.05) + 
                                    as.numeric(upper_p <= 0.05) + 
                                    as.numeric(lower_p <= 0.05)))

cs137_vis <- depths_sig %>% 
  select(study_id, site_id, core_id, depth_min, depth_max, cs137_activity, cs137_activity_se, sig_level, date) %>% 
  rename(radioactivity=cs137_activity,
         error = cs137_activity_se)

cs137_vis$sig_level[is.na(cs137_vis$sig_level)] <- 0

study_site_combos <- cs137_vis %>% 
  select(study_id, site_id) %>% 
  distinct_all()

color_fill_df <- data.frame(sig_level = as.character(0:3),
                            color =  c("#000000", "#E69F00", "#56B4E9", "#009E73"))
cs137_vis <- cs137_vis %>% 
  left_join(color_fill_df) %>% 
  ungroup() %>% 
  mutate(error_lower = radioactivity-error,
         error_upper = radioactivity+error,
         error_lower = ifelse(error_lower<0, 0, error_lower))

for (i in 1:nrow(study_site_combos)) {
  cs_vis_subset <- cs137_vis %>% 
    filter(site_id == study_site_combos$site_id[i] &
             study_id==study_site_combos$study_id[i])
  
  output_file <- ggplot(data = cs_vis_subset, aes(x=(depth_max+depth_min)/2, y=radioactivity, color=color)) +
    geom_line(color = "black") +
    geom_point() +
    geom_segment(aes(xend=(depth_max+depth_min)/2, y=error_lower, yend=error_upper)) +
    geom_segment(aes(x=depth_max, xend=depth_min, yend=radioactivity)) +
    xlab("Depth (cm)") +
    facet_wrap(site_id~core_id) +
    scale_x_reverse() +
    # ylim(c(max(0, min(cs_vis_subset$radioactivity - cs_vis_subset$error)), max(cs_vis_subset$radioactivity + cs_vis_subset$error))) +
    coord_flip() +
    theme(legend.position = "bottom") +
    scale_color_identity("sig. level", labels = as.character(0:3), breaks = c("#000000", "#E69F00", "#56B4E9", "#009E73"), guide = "legend") +
    guides(color = guide_legend(override.aes = list(shape = 16, size = 3))) +
    geom_label_repel(aes(label = as.character(date), color = color), min.segment.length = 0, show.legend = FALSE)
    
  
  (output_file)
  
  ncores <- length(unique(cs_vis_subset$core_id))
  if (ncores == 1) {
    width = 4.25
    height = 5
  } else if (ncores == 2) {
    width = 5.61
    height = 5
  } else if (ncores <=4) {
    width = 8.5
    height = 5
  } else if (ncores <=8) {
    width = 8.5
    height = 8.5
  } else {
    width = 8.5
    height = 11
  }
  
  output_name <- paste0("carbon_accumulation_rates/output/figs/cs137/", study_site_combos$study_id[i], "_", study_site_combos$site_id[i], "_137Cs.jpg")
  ggsave(output_name, output_file, width = width, height = height, units = "in")
}

accretion_rates <- depths_sig %>% 
  filter(!is.na(date)) %>% 
  left_join(cores) %>% 
  # What's the maximum possible accretion rate? 
  # high_accretion = max_depth / (core_year - 1963)
  mutate(high_accretion = max_possible_peak_depth / (year - date),
         # What's the minimum possible accretion rate?
         # low_accretion = min_depth / (core_year - 1963)
         low_accretion = min_possible_peak_depth / (year - date),
         # What's the lowest possible age for depth_min
         # min_depth * high_accretion
         age_min = year - min_possible_peak_depth / high_accretion,
         # What's the highest possible age for depth_max
         # max_depth * low_accretion
         age_max = year - max_possible_peak_depth / low_accretion,
         depth_med = (min_possible_peak_depth+max_possible_peak_depth)/2,
         # Treat as a uniform disribution and use moment matching to approximate a normal dist
         # mean = (min+max)/2
         # sd = sqrt((max-min)^2/12)
         age_mean = (age_min+age_max)/2,
         age_sd = sqrt((age_max-age_min)^2/12)
  ) %>% 
  filter(age_mean != Inf & age_mean != -Inf,
         age_sd != Inf & age_sd != -Inf)

write_csv(accretion_rates, "carbon_accumulation_rates/output/tabs/peak_137Cs_accretion_rates.csv")

accretion_rates_summed <- accretion_rates %>% 
  group_by(sig_level) %>% 
  summarise(n = n(),
            sd_med = median(age_sd, na.rm=T),
            sd_mean = mean(age_sd, na.rm=T))
(accretion_rates_summed)

ggplot(accretion_rates, aes(x = age_sd)) +
  geom_density(aes(color = sig_level)) +
  scale_x_log10()

accretion_rates_sig_regional <- accretion_rates %>% 
  filter(sig_level >= 2) %>% 
  mutate(mid_accretion = (high_accretion+low_accretion)/2)

accretion_rates_sig_regional_states <- accretion_rates_sig_regional %>% 
  group_by(admin_division) %>% 
  summarise(median_accretion = median(mid_accretion)) %>% 
  arrange(-median_accretion)

accretion_rates_sig_regional$admin_division <- factor(accretion_rates_sig_regional$admin_division,
                                                      levels = accretion_rates_sig_regional_states$admin_division)

ggplot(accretion_rates_sig_regional, aes(x = mid_accretion)) +
  geom_density() +
  scale_x_log10() +
  facet_wrap(.~admin_division)




 
