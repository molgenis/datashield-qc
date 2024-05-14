####################################################################################################
## Project: qc-study
## Script purpose: Assign tables
## Date:  9th May 2024
## Author: Tim Cadman
## Email: tica@sund.ku.dk
####################################################################################################
library(purrr)
library(dsBaseClient)

qc.dic %>%
  pmap(function(name, path, cohort){
    datashield.assign.table(
      symbol = name, 
      table = path, 
      conns = conns[cohort])
  })
