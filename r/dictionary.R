####################################################################################################
## Project: qc-study
## Script purpose: Define tables
## Date: 9th May 2024
## Author: Tim Cadman
## Email: tica@sund.ku.dk
####################################################################################################
library(tibble)
library(dplyr)

qc1.dic <- tribble(
  ~name, ~path,
  "core_2_6_non_rep", "lifecycle/core/nonrep",
  "core_2_6_monthly_rep", "lifecycle/core/monthlyrep", 
  "core_2_6_yearly_rep", "lifecycle/core/yearlyrep",   
  "core_2_6_trimester_rep", "lifecycle/core/trimesterrep",
  "outcome_1_7_non_rep", "lifecycle/outcome/nonrep", 
  "outcome_1_7_yearly_rep", "lifecycle/outcome/yearlyrep") %>%
  mutate(cohort = "cohort_1")

qc2.dic <- tribble(
  ~name, ~path,
  "core_2_6_monthly_rep", "lifecycle/core/monthlyrep", 
  "core_2_6_yearly_rep", "lifecycle/core/yearlyrep") %>%
  mutate(cohort = "cohort_2")

qc.dic <- bind_rows(qc1.dic, qc2.dic)
