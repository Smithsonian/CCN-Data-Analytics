## Update CCRCN Data ####

library(tidyverse)
# library(RefManageR)

## Download CCRCN library data ####
library_url <- "https://raw.githubusercontent.com/Smithsonian/CCRCN-Data-Library/main/data/CCRCN_synthesis/derivative/"

## Core Synthesis ####
# read in core table from the CCRCN Data Library (with assigned geography)
cores <- read_csv(paste0(library_url, "CCRCN_cores.csv"), col_types = cols(.default = "c"))

# subset core data for CONUS cores
coastal_cores <- cores %>% 
  rename(state = admin_division) %>%
  filter(country == "United States") %>%
  filter(state != "Puerto Rico" & state != "Hawaii") %>%
  mutate(habitat = recode(habitat, 
                          "mudflat" = "unvegetated",
                          # we'll be comparing sampled cores to mapped habitat in which seagress, kelp, and algal mats are grouped together
                          "seagrass" = "EAB",
                          "algal mat" = "EAB"))

# write files
write_csv(coastal_cores, "data/derived/ccn_conus_cores.csv")

## Impacts ####
impacts <- read_csv(paste0(library_url, "CCRCN_impacts.csv"), col_types = cols(.default = "c")) 

conus_impacts <- impacts %>%
  filter(site_id %in% unique(coastal_cores$site_id))

write_csv(conus_impacts, "data/derived/ccn_conus_impacts.csv")

## Synthesis Bibliography ####

# these studies have DOIs for their keys, which will make citing difficult in the report
# replace_key <- c("Vaughn_et_al_2020", "Messerschmidt_and_Kirwan_2020", "Buffington_et_al_2020",
#              "Lagomasino_et_al_2020", "Breithaupt_et_al_2020", "Peck_et_al_2020")

# read in study citations from CCRCN Data Library
citations <- read_csv(paste0(library_url, "CCRCN_study_citations.csv"))
  # filter down to the studies used in this report
  # filter(study_id %in% unique(pew_cores$study_id))
  # if I leave in all the articles, theres an invalid Invalid UTF-8 error in the report
  # filter(bibtype == "Misc") %>% # filter for just the data releases for now
  # recode some of these keys that are DOIs
  # mutate(key = ifelse(bibliography_id %in% replace_key, bibliography_id, key))

conus_citations <- citations %>% 
  filter(study_id %in% unique(coastal_cores$study_id))
  

# write study citations
write_csv(conus_citations, "data/derived/ccn_conus_citations.csv")
