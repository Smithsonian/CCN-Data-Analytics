## CCN-Data-Analytics
## Jaxine Wolfe <wolfejax@si.edu>

# Library Bib Query: Find instances where we have a seagrass dataset that is not in the provided list
# Note any papers with seagrass stock or carbon accumulation data that don’t appear in the Atlas

library(tidyverse)
library(readxl)
library(fuzzyjoin)

source("shared_resources/refresh_data.R")

seagrass_cores <- cores %>% filter(habitat == "seagrass")

seagrass_bib <- bib %>%
  filter(study_id %in% unique(seagrass_cores$study_id)) %>% 
  filter(bibtype != "Misc")
  # mutate(reference = str_c(author, year, title, sep = " "))

ccn_bib <- seagrass_bib %>% 
  filter(bibliography_id != "Fourqurean_et_al_2012_article") %>% 
  select(study_id, title) %>% arrange(study_id) %>% 
  distinct()

# wrangle citation info from Hilary
orig_studyinfo <- read_xlsx("database_query/data/hilary_pubs_to_check.xlsx", 
                        sheet = "Study information", skip = 1) %>% 
  select("Article ID", "Primary reference", "Secondary reference") %>% 
  slice(-1) %>% 
  pivot_longer(cols = -"Article ID", names_to = "reference_type", values_to = "reference") %>% 
  drop_na(reference) %>% 
  distinct() 

studyinfo_yr <- as.data.frame(str_extract_all(orig_studyinfo$reference, "\\d{4}", simplify = T))[,1]

clean_studyinfo <- orig_studyinfo %>% 
  # pull the first author 
  mutate(first_author = word(reference, 1, 2),
         first_author = gsub(",", "", first_author),
         year = studyinfo_yr,
         study_id = paste0(first_author, "_et_al_", year)) %>% 
  arrange(study_id) %>% select(-first_author, -year) %>% 
  filter(!grepl("unpublished", reference, ignore.case = T)) %>%
  # excel drag in DOIs causing expansion
  separate(reference, into = c("reference", "doi"), sep = "doi:|DOI:") %>% 
  distinct(study_id, reference) %>% rename(studyinfo_ref = reference)
## Not going to use this sheet for now, I think it's mostly shared references

## refcheck sheet
orig_refcheck <- read_xlsx("database_query/data/hilary_pubs_to_check.xlsx", sheet = "Ref Check") %>% 
  rename(reference = "List of publications from which data is to be extracted") %>% 
  select(reference) %>% drop_na(reference)
  
refyear <- as.data.frame(str_extract_all(orig_refcheck$reference, "\\d{4}", simplify = T))[,1]

clean_refcheck <- orig_refcheck %>% 
  # pull the first author 
  mutate(first_author = word(reference, 1, 2),
         first_author = gsub(",", "", first_author),
         year = refyear,
         study_id = paste0(first_author, "_et_al_", year)) %>% 
  arrange(study_id) %>% select(-first_author, -year) 
  
# join all provided references
final_refs <- full_join(clean_refcheck, clean_studyinfo) %>%
  mutate(reference = ifelse(is.na(reference), studyinfo_ref, reference)) %>% 
  select(-studyinfo_ref) %>% arrange(study_id)

# fuzzy match based on reference
bib_match <- stringdist_join(ccn_bib, clean_refcheck,
                             by = "study_id",
                             mode = "left",
                             ignore_case = T, 
                             method = "osa", 
                             max_dist = 20,
                             distance_col = "dist") %>% 
  select(dist, contains("study_id"), everything()) %>% 
  group_by(study_id.x) %>% 
  filter(dist == min(dist)) %>% 
  rename(ccn_study_id = study_id.x, ref_study_id = study_id.y,
         ccn_title = title, provided_reference = reference) %>% 
  arrange(ccn_study_id, dist)

# conduct a second fuzzy match for the IDs
bib_match2 <- bib_match %>% 
  rowwise() %>%
  mutate(title_match = agrepl(ccn_title, provided_reference, ignore.case = T)) %>% 
  select(title_match, everything())

non_matching <- bib_match2 %>% 
  filter(title_match == FALSE) 

# studies in the Data Library that they don't have
missing_citations <- seagrass_bib %>% 
  filter(bibliography_id != "Fourqurean_et_al_2012_article") %>% 
  filter(study_id %in% unique(non_matching$ccn_study_id)) %>% 
  arrange(study_id)

## Studies the Data Library doesn't have
to_investigate <- clean_refcheck %>% 
  filter(!(study_id %in% unique(bib_match2$ref_study_id))) %>% 
  select(study_id, reference) %>% 
  mutate(mentions_carbon = ifelse(grepl("carbon|Carbon|stock|soil|sequestration|storage", reference), 
                                    TRUE, FALSE),
         mentions_biomass = ifelse(grepl("biomass", reference), TRUE, FALSE)) %>% 
  arrange(desc(mentions_carbon), desc(mentions_biomass))

write_csv(to_investigate, "database_query/output/seagrass_pubs_to_screen.csv")
