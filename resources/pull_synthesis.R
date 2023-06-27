## CCN-Data-Analytics
## Jaxine Wolfe <wolfejax@si.edu>

## script reads in current version of CCN Data Library synthesis

library(tidyverse)

getSynthesisData <- function(table){
  ## read in data synthesis
  root_dir <- "https://raw.githubusercontent.com/Smithsonian/CCRCN-Data-Library/main/data/CCRCN_synthesis/CCRCN_"
  
  switch (table,
          "methods" = {synth_table <- read_csv(paste0(root_dir, "methods.csv"), guess_max = 7000)},
          "cores" = {synth_table <- read_csv(paste0(root_dir, "cores.csv"), guess_max = 7000)},
          "depthseries" = {synth_table <- read_csv(paste0(root_dir, "depthseries.csv"), guess_max = 60000)},
          "impacts" = {synth_table <- read_csv(paste0(root_dir, "impacts.csv"), guess_max = 7000)},
          "species" = {synth_table <- read_csv(paste0(root_dir, "species.csv"), guess_max = 7000)},
          "citations" = {synth_table <- read_csv(paste0(root_dir, "study_citations.csv"), guess_max = 600)}
  )
  
  return(synth_table)
}
