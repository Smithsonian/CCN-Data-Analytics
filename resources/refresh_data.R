## CCN-Data-Analytics
## Jaxine Wolfe <wolfejax@si.edu>

## script reads in current version of CCN Data Library synthesis

library(tidyverse)

## read in data synthesis
root_dir <- "https://raw.githubusercontent.com/Smithsonian/CCRCN-Data-Library/main/data/CCRCN_synthesis/"

species <- read_csv(paste0(root_dir, "CCRCN_species.csv"), guess_max = 7000)
methods <- read_csv(paste0(root_dir, "CCRCN_methods.csv"), guess_max = 7000)
impacts <- read_csv(paste0(root_dir, "CCRCN_impacts.csv"), guess_max = 7000)
cores <- read_csv(paste0(root_dir, "CCRCN_cores.csv"), guess_max = 7000)
depthseries <- read_csv(paste0(root_dir, "CCRCN_depthseries.csv"), guess_max = 60000)
bib <- read_csv(paste0(root_dir, "CCRCN_study_citations.csv"), guess_max = 600)
