## CCN Blue Carbon Data Inventory ####

## contact: Jaxine Wolfe, wolfejax@si.edu
## date: 11-10-2021

# Script curates data release citations

## ...Prepare Citations ####

citations <- read_csv("data/report_data/CONUS_study_citations.csv")

# prepare study citations for reference in the report
# keep articles that have are the only citation for a study
synths <- c("Holmquist_et_al_2018", "Sanderman_2018", "Fourqurean_et_al_2012")

citations2 <- citations %>%
  filter(study_id %in% unique(cores$study_id)) %>%
  # filter(!(bibliography_id %in% synths)) %>%
  # https://stackoverflow.com/questions/17291287/how-to-identify-delete-non-utf-8-characters-in-r
  # mutate_at(vars(names(citations)), function(x){iconv(x, "latin1", "UTF-8",sub='')}) %>% # fix encoding issue
  group_by(study_id, bibliography_id) %>% 
  mutate(n = 1:n()) %>%
  ungroup() %>% 
  # create unique key for reference
  mutate(key = paste(bibliography_id, n, sep="_")) %>% 
  filter(key != "Fourqurean_et_al_2012_2")

# create citation formulas that can be referenced in the report 
study_refs <- citations2 %>% 
  mutate(citation = paste0("[@", key, "]")) %>%
  select(-n)

# convert to .bib file
bib_file <- study_refs %>%
  select(-bibliography_id, -study_id, -publication_type, -citation) %>%
  distinct() %>%
  arrange(key) %>% 
  column_to_rownames("key")

# write study citations for cores in pew-selected states
write_csv(study_refs, "data/report_data/pew_core_citations.csv")
# Write .bib file
WriteBib(as.BibEntry(bib_file), "CCRCN_bibliography.bib")

## Check out bibs to search keys
# book_bib <- ReadBib("book.bib")
# ccn_bib <- as.data.frame(ReadBib("CCRCN_bibliography.bib"))
