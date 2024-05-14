####################################################################################################
## Project: qc-study
## Script purpose: Summarise data
## Date: 9th May 2024
## Author: Tim Cadman
## Email: tica@sund.ku.dk
####################################################################################################  
library(dplyr)
library(stringr)
library(dsHelper)
####################################################################################################
# Get information about all possible data  
####################################################################################################
path_to_dictionaries <- "/Users/tcadman/Library/Mobile Documents/com~apple~CloudDocs/work/repos/ds-dictionaries/dictionaries"
all_dics <- .list_all_dic_files(path_to_dictionaries)
available_dicts <- .list_cohort_dics_available(all_dics$long_name, conns)


####################################################################################################
# Summarise table availability  
####################################################################################################
dict.ref <- available_dicts %>%
  bind_rows(.id = "cohort") %>%
  dplyr::select(cohort, available)

all_stats <- dict.ref %>%
  pmap(function(cohort, available){
    dh.getStats(
    df = available, 
    conns = conns[cohort])
  }) %>%
  set_names(dict.ref$available)




