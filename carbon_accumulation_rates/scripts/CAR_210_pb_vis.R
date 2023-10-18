# Pb-210
# Written by J. Holmquist 2023-10-17
library(tidyverse)

# Load in core data
depths <- read_csv("data/CCRCN_depthseries.csv")
depths <- read_csv("data/CCRCN_depthseries.csv", guess_max = nrow(depths))


# Subset only cores that have one of the following
# Total 210Pb, Excess 210Pb
pb_depths <- depths %>% 
  filter(!is.na(excess_pb210_activity) | ! is.na(total_pb210_activity)) %>% 
  select(study_id:depth_max, excess_pb210_activity:bi214_unit)

# Fix a couple of study specific units
# All units in weston 2022 should be microcuries per grams
# Callaway should be ln(picocuries per gram)
pb_depths[grepl("Weston", pb_depths$study_id), grepl("unit", names(pb_depths))] <- "microcuriesPerGram"
pb_depths[grepl("Callaway", pb_depths$study_id), grepl("unit", names(pb_depths))] <- "lnPicocuriesPerGram"
pb_depths[grepl("Giblin", pb_depths$study_id), grepl("pb214", names(pb_depths))] <- NA

# Nolte_2020 NFB NFB_ungrazed_landward 0 to 64, should be 62 to 64
# Nolte_2020 HH HH_ungrazed_landward 0 to 50, should be 48 to 50
pb_depths[grepl("NFB_ungrazed_landward", pb_depths$core_id) & pb_depths$depth_max == 64, "depth_min"] <- 62
pb_depths[grepl("HH_ungrazed_landward", pb_depths$core_id) & pb_depths$depth_max == 50, "depth_min"] <- 48




convertToBqPerKg <- function(value, unit) {
  
  output_value <- ifelse(! is.na(unit),
                         ifelse(unit %in% c("microcuriesPerGram"),
                         # 1 microcurie = 37000 Bq, 1000 g = 1 kg
                         value * 1000 * 37000,
                         ifelse(unit %in% c("disintegrationsPerMinutePerGram",
                                            "disintegrations_per_minute_per_gram",
                                            "distintegrationsPerMinutePerGram",
                                            "disintegrations per minute per gram"),
                                # * 1 dpm / 0.0166666667 Bq * 1000 g / 1kg
                                value * 0.0166666667 * 1000,
                                ifelse(unit %in% c("becquerelPerGram"),
                                       value * 1000,
                                       ifelse(unit %in% c("lnPicocuriesPerGram"),
                                              # 0.037 Bq per pq * 1000 g / kg
                                              exp(value) * 0.037 * 1000,
                                              value))
                                )
                         ), NA)
  
}

# Normalize units to Bq per kg
unique(pb_depths$pb210_unit)
unique(pb_depths$ra226_unit)
unique(pb_depths$pb214_unit)
unique(pb_depths$bi214_unit)

pb_depths_normalized <- pb_depths %>% 
  mutate(excess_pb210_activity = convertToBqPerKg(excess_pb210_activity, pb210_unit),
         excess_pb210_activity_se = convertToBqPerKg(excess_pb210_activity_se, pb210_unit),
         total_pb210_activity = convertToBqPerKg(total_pb210_activity, pb210_unit),
         total_pb210_activity_se = convertToBqPerKg(total_pb210_activity_se, pb210_unit),
         ra226_activity = convertToBqPerKg(ra226_activity, ra226_unit),
         ra226_activity_se = convertToBqPerKg(ra226_activity_se, ra226_unit),
         pb214_activity = convertToBqPerKg(pb214_activity, pb214_unit),
         pb214_activity_se = convertToBqPerKg(pb214_activity_se, pb214_unit),
         bi214_activity = convertToBqPerKg(bi214_activity, bi214_unit),
         bi214_activity_se = convertToBqPerKg(bi214_activity_se, bi214_unit))

# Consistent Classification of Ra-226
pb_depths_ra226_consistent <- pb_depths_normalized %>% 
  mutate(ra226_activity = ifelse(!is.na(ra226_activity), ra226_activity, 
                                 ifelse(!is.na(pb214_activity)&!is.na(bi214_activity), 
                                        (pb214_activity+bi214_activity)/2, 
                                        pmax(pb214_activity,bi214_activity, na.rm=T))),
         ra226_activity_se = ifelse(!is.na(ra226_activity_se), ra226_activity_se, 
                                  ifelse(!is.na(pb214_activity_se)&!is.na(bi214_activity_se), 
                                         sqrt(pb214_activity_se^2+bi214_activity_se^2)/2, 
                                         pmax(pb214_activity_se,bi214_activity_se, na.rm=T))))

type_of_ra226_method <- pb_depths_ra226_consistent %>% 
  select(study_id, site_id, core_id, ra226_activity) %>%
  filter(!is.na(ra226_activity)) %>% 
  group_by(study_id, site_id, core_id) %>%
  summarise(n_ra226 = n()) %>% 
  ungroup()
  


# Consistent Calculation of Excess Pb-210
pb_depths_xspb210_consistent <- pb_depths_ra226_consistent %>% 
  left_join(type_of_ra226_method) %>% 
  mutate(excess_pb210_activity = ifelse(!is.na(excess_pb210_activity)|(n_ra226<=4), excess_pb210_activity,
                                        total_pb210_activity - ra226_activity),
         excess_pb210_activity_se = ifelse(!is.na(excess_pb210_activity)|(n_ra226<=4), excess_pb210_activity_se,
                                        sqrt(total_pb210_activity_se^2 + ra226_activity_se^2)))



# Fill in missing total_se values with high number
high_uncertainty <- exp(quantile(log(pb_depths_xspb210_consistent$total_pb210_activity_se/pb_depths_xspb210_consistent$total_pb210_activity), 
                                 0.975, na.rm=T))


hist(log(pb_depths_xspb210_consistent$ra226_activity_se/pb_depths_xspb210_consistent$ra226_activity))
high_uncertainty_ra <- exp(quantile(log(pb_depths_xspb210_consistent$ra226_activity_se/pb_depths_xspb210_consistent$ra226_activity), 
                                 0.975, na.rm=T))


# Fill in missing ra 226 with mean value
median_background <- median(pb_depths_xspb210_consistent$ra226_activity, na.rm=T)

pb_depths_gap_filled <- pb_depths_xspb210_consistent %>% 
  group_by(study_id, site_id, core_id) %>% 
  mutate(max_total_pb210_activity = max(total_pb210_activity, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(total_pb210_activity = ifelse(max_total_pb210_activity != -Inf,
                                       total_pb210_activity,
                                       excess_pb210_activity+median_background),
         total_pb210_activity_se = ifelse(!is.na(total_pb210_activity_se),
                                          total_pb210_activity_se,
                                          excess_pb210_activity_se
                                          ),
      total_pb210_activity_se = ifelse(!is.na(total_pb210_activity_se), 
                                          total_pb210_activity_se, 
                                          total_pb210_activity*high_uncertainty),
      ra226_activity_se = ifelse(!is.na(ra226_activity_se), ra226_activity_se,
                                 ra226_activity*high_uncertainty_ra)
         )


# Graph profiles by study / site - combo 
pb_depths_ready_to_go <- pb_depths_gap_filled %>% 
  arrange(study_id, site_id, core_id, depth_min, depth_max)

pb210_vis1 <- pb_depths_ready_to_go %>% 
  select(study_id, site_id, core_id, depth_min, depth_max, total_pb210_activity, ra226_activity, excess_pb210_activity) %>% 
  gather(key="radioisotope", value="radioactivity", -study_id, -core_id, -depth_min, -depth_max, -site_id)

pb210_vis2 <- pb_depths_ready_to_go %>% 
  select(study_id, site_id, core_id, depth_min, depth_max, total_pb210_activity_se, ra226_activity_se, excess_pb210_activity_se) %>% 
  gather(key="radioisotope", value="error", -study_id, -core_id, -depth_min, -depth_max, -site_id) %>% 
  mutate(radioisotope = str_remove(radioisotope, "_se"))

pb210_vis <- pb210_vis1 %>% 
  left_join(pb210_vis2)

study_site_combos <- pb210_vis %>% 
  select(study_id, site_id) %>% 
  distinct_all()

for (i in 1:nrow(study_site_combos)) {
  pb_vis_subset <- pb210_vis %>% 
    filter(site_id == study_site_combos$site_id[i] &
             study_id==study_site_combos$study_id[i])
  
  output_file <- ggplot(data = pb_vis_subset, aes(x=(depth_max+depth_min)/2, y=radioactivity, color=radioisotope)) +
    geom_line() +
    geom_point() +
    geom_segment(aes(xend=(depth_max+depth_min)/2, y=radioactivity-error, yend=radioactivity+error)) +
    geom_segment(aes(x=depth_max, xend=depth_min, yend=radioactivity)) +
    xlab("Depth (cm)") +
    facet_wrap(site_id~core_id) +
    scale_x_reverse() +
    scale_y_log10() +
    coord_flip() +
    theme(legend.position = "bottom")
  
  (output_file)
  
  ncores <- length(unique(pb_vis_subset$core_id))
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
  
  output_name <- paste0("carbon_accumulation_rates/output/figs/", study_site_combos$study_id[i], "_", study_site_combos$site_id[i], "_210Pb.jpg")
  ggsave(output_name, output_file, width = width, height = height, units = "in")
}

