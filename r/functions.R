####################################################################################################
## Project: qc-study
## Script purpose: Define functions
## Date: 9th May 2024
## Author: Tim Cadman
## Email: tica@sund.ku.dk
####################################################################################################

library(dplyr)
library(stringr)

.list_dictionary_types <- function(path_to_dictionaries){
  list.dirs(path_to_dictionaries, recursive = FALSE)
}

.list_latest_version <- function(dict_types){
  all_versions <- list.dirs(dict_types, recursive = FALSE)
  latest_version <- sort(all_versions, decreasing = TRUE)[1]
  return(latest_version)
}

.list_available_dics <- function(latest_version){
  available <- list.files(latest_version)
}

.extract_name <- function(latest_version){
  neat_name <- str_extract(latest_version, "(?<=ds-dictionaries/dictionaries/)(.*?)(?=/[^/]*$)")
  return(neat_name)
}

.format_dictionary_names <- function(files, neat_names){
  out <- files %>%
    set_names(neat_names) %>%
    map(as_tibble) %>%
    map(bind_rows) %>%
    bind_rows(.id = "type") %>%
    dplyr::rename(name = value) %>%
    mutate(name = str_remove(name, "\\.xlsx$")) %>%
    mutate(long_name = paste0(type, "_", name))
  return(out)
}

.list_all_dic_files <- function(path_to_dictionaries){
  types <- .list_dictionary_types(path_to_dictionaries)
  latest <- types %>% map_chr(.list_latest_version)
  files <- latest %>% map(.list_available_dics)
  neat_names <- latest %>% map(.extract_name)
  out <- .format_dictionary_names(files, neat_names)
  return(out)
}

.list_cohort_dics_available <- function(dictionaries, conns){
  objects <- ds.ls(datasources = conns)
  objects_clean <- objects %>% map(~.x$objects.found)
  objects_are_dicts <- objects_clean %>%
    map(~.x[.x %in% dictionaries])
  
  out <- objects_are_dicts %>%
    map(tibble) %>%
    map(~dplyr::rename(., long_name = 1)) %>%
    map(~mutate(., available = long_name))
  
  return(out)
}

.summarise_available_dics_per_cohort <- function(master_list, cohort_list){
  out <- left_join(master_list, cohort_list, by = "long_name")
  return(out)
}

.summarise_available_dics <- function(path_to_dictionaries){
  all_dics <- .list_all_dic_files(path_to_dictionaries)  
  available_dicts <- .list_cohort_dics_available(all_dics$long_name, conns)  
  out <- available_dicts %>% 
    map(
      ~.summarise_available_dics_per_cohort(
        master_list = all_dics,
        cohort_list = .x
      ))
  return(out)
}

.get_demographics <- function(){
  
  demo_non <- dh.getStats(
    df = "core_2_6_non_rep",
    vars = c("agebirth_m_y", "ethn3_m", "sex")) 
        
  dh.makeStrata(
    df = "core_2_6_yearly_rep", 
    id_var = "child_id", 
    age_var = "age_years", 
    var_to_subset = "edu_m_", 
    bands = c(0, 1), 
    mult_action = "earliest", 
    band_action = "ge_l", 
    new_obj = "mat_ed")
  
  mat_ed <- dh.getStats(
    vars = "edu_m_.0_1", 
    df = "mat_ed")
  
  repeated_dfs <- c("chemicals_ath_1_2_yearly_rep", "core_2_6_yearly_rep", "outcome_1_7_yearly_rep", 
                    "outcome_ath_1_3_yearly_rep", "urban_ath_1_3_yearly_rep")
  
  child_ages <- repeated_dfs %>% 
    map(
      ~tryCatch(
        dh.getStats(
        vars = "age_years", 
        df = .x), 
    error = function(cond){
      print(cond)
      }
    )
  )
  
  data_exists <- child_ages %>% map_lgl(function(x){names(x)[[1]] == "categorical"})
  child_ages_exists <- child_ages[data_exists]

  
  
  
  ## Need to use long names
  
  
age of children, education
  
}



