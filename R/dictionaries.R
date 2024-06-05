#' Identify Cohort Dictionaries
#'
#' This function identifies cohort dictionaries by processing a directory of dictionary files
#' and then matches the extracted cohort dictionaries with the data tables. It cleans and organizes
#' the dictionary information and returns a final table.
#'
#' @param conns The connections object to the database where the data tables are stored.
#' @importFrom dplyr bind_rows group_by arrange slice group_split left_join
#' @importFrom purrr map map_lgl pmap
#' @importFrom stringr str_remove_all str_split str_detect
#' @importFrom DSI datashield.tables
#' @return A final table containing cleaned and organized cohort dictionary information.
#' @export
identify_cohort_dics <- function(conns) {
  dictionaries <- list_all_dic_files()
  cohort_dics <- .get_cohort_dics(conns)
  matched_dics <- .match_cohort_dictionaries_with_ref(dictionaries, cohort_dics)
  formatted <- .make_final_dic_table(matched_dics)
  return(formatted)
}

#' List All Dictionary Files
#'
#' This function lists all dictionary files available in the specified path by combining all the helper functions.
#'
#' @return A data frame with all available dictionary files and their formatted names.
#' @export
list_all_dic_files <- function() {
  response <- .get_dics_from_api()
  neat_response <- .format_response(response)
  shorter_paths <- .shorten_paths(neat_response)
  paths_as_tibble <- .split_paths_to_tibble(shorter_paths)
  out <- .make_final_path_table(paths_as_tibble)
  return(out)
}

#' Retrieve Dictionary Paths from GitHub API
#'
#' This function makes a request to the GitHub API to get the list of dictionary files from the lifecycle-project's repository.
#'
#' @importFrom httr2 request req_perform resp_body_json
#' @return A list containing the JSON response from the GitHub API.
#' @noRd
.get_dics_from_api <- function(){
  req <- request("https://api.github.com/repos/lifecycle-project/ds-dictionaries/git/trees/master?recursive=true")
  resp <- req_perform(req)
  content <- resp_body_json(resp)
  return(content)
}

#' Extract Paths from API Response
#'
#' This function extracts file paths from the JSON response obtained from the GitHub API.
#'
#' @param response A list containing the JSON response from the GitHub API.
#' @return A character vector of file paths.
#' @noRd
.format_response <- function(response){
  paths <- response$tree %>% map_chr(~.$path)
  return(paths)
}

#' Filter and Shorten Paths
#'
#' This function filters the list of paths to include only `.xlsx` files and removes the `dictionaries/` prefix.
#'
#' @param neat_response A character vector of file paths.
#' @importFrom stringr str_subset str_remove
#' @return A character vector of shortened file paths.
#' @noRd
.shorten_paths <- function(neat_response){
  shorter <- neat_response %>%
    str_subset(".xlsx") %>%
    str_remove("dictionaries/")
  return(shorter)
}

#' Split Paths into Tibble
#'
#' This function splits the shortened paths into a tibble with separate columns for type, version, and file name.
#'
#' @param shorter_paths A character vector of shortened file paths.
#' @return A tibble with columns `type`, `version`, and `file`.
#' @noRd
.split_paths_to_tibble <- function(shorter_paths){
  paths_tibble <- shorter_paths %>%
    str_split("/") %>%
    map(t) %>%
    map(as_tibble) %>%
    bind_rows() %>%
    set_names(c("type", "version", "file"))
  return(paths_tibble)
}

#' Create Final Path Table
#'
#' This function processes the tibble to create a final table with unique dictionary names, removing misnamed dictionaries.
#'
#' @param paths_as_tibble A tibble with columns `type`, `version`, and `file`.
#' @importFrom dplyr mutate distinct
#' @return A tibble with columns `type` and `name`.
#' @noRd
.make_final_path_table <- function(paths_as_tibble){
  name <- type <- ref <- long_name <- NULL
  out <- paths_as_tibble %>%
    dplyr::filter(file != "1_1_peripheral_blood.xlsx") %>% ## Misnamed dictionary
    .strip_excess_from_name() %>%
    .make_long_name() %>%
    dplyr::select(type, name, long_name) %>%
    distinct() %>%
    .add_id() %>%
    dplyr::select(type, name, long_name, ref) %>%
    return(out)
}

#' Strip Excess Characters from File Name
#'
#' This function removes specified patterns from the `file` column of a tibble to create a cleaned `name` column.
#'
#' @param paths_as_tibble A tibble with a column named `file` containing file paths.
#'
#' @return A tibble with a new column `name` where excess characters have been stripped.
#' @noRd
.strip_excess_from_name <- function(paths_as_tibble){
  name <- NULL
  stripped <- paths_as_tibble %>%
    mutate(
      name = str_remove(file, ".xlsx"),
      name = str_remove(name, "\\b[_\\d]+"),
      name = str_remove_all(name, "_rep"))
  return(stripped)
}

#' Create Long Name by Concatenating Type and Name
#'
#' This function concatenates the `type` and `name` columns of a tibble to create a new `long_name` column.
#'
#' @param stripped A tibble with columns `type` and `name`.
#'
#' @return A tibble with a new column `long_name`.
#' @noRd
.make_long_name <- function(stripped){
  type <- name <- NULL
  out <- stripped %>%
    mutate(long_name = paste0(type, "_", name))
  return(out)
}

#' Add ID Column to Tibble
#'
#' This function adds a `ref` column to a tibble, containing unique IDs for each row.
#'
#' @param paths_as_tibble A tibble.
#' @importFrom dplyr row_number
#' @return A tibble with a new column `ref` containing unique IDs.
#' @noRd
.add_id <- function(paths_as_tibble){
  out <- paths_as_tibble %>%
    mutate(ref = paste0("id_", row_number()))
  return(out)
}

#' Get Cohort Dictionaries
#'
#' @param conns Database connections object.
#' @return A tibble containing cohort dictionaries.
#' @noRd
.get_cohort_dics <- function(conns) {
  coh_tables <- datashield.tables(conns) %>%
    map(as_tibble) %>%
    bind_rows(.id = "cohort")
  return(coh_tables)
}

#' Match Cohort Dictionaries with Reference
#'
#' This function matches cohort dictionaries with reference dictionaries, joins IDs, and adds labels.
#'
#' @param reference_dics A tibble containing reference dictionaries.
#' @param cohort_dics A tibble containing cohort dictionaries.
#'
#' @return A tibble with cohort dictionaries matched and labeled.
#' @noRd
.match_cohort_dictionaries_with_ref <- function(reference_dics, cohort_dics){
  all_matches <- .get_all_dictionary_matches(reference_dics, cohort_dics)
  all_matches_clean <- .clean_ids(all_matches)
  ref_with_matches <- .join_ids_with_cohort_table(cohort_dics, all_matches_clean)
  ref_with_labels <- .join_labels_with_cohort_table(ref_with_matches, reference_dics)
  return(ref_with_labels)
}

#' Get All Dictionary Matches
#'
#' This function gets all matches between dictionaries and cohort dictionaries.
#'
#' @param dictionaries A tibble containing reference dictionaries.
#' @param cohort_dics A tibble containing cohort dictionaries.
#'
#' @return A list of all matches.
#' @noRd
.get_all_dictionary_matches <- function(dictionaries, cohort_dics){
  all_matches <- cohort_dics$value %>%
    map(~.get_single_dictionary_match(dictionaries, .x))
}

#' Get Single Dictionary Match
#'
#' This function gets a single match between a reference dictionary and a cohort dictionary.
#'
#' @param dictionaries A tibble containing reference dictionaries.
#' @param single_cohort_dic A single cohort dictionary to match.
#'
#' @return A character vector of matched IDs.
#' @noRd
.get_single_dictionary_match <- function(dictionaries, single_cohort_dic){
  match <- .check_cohort_dic_with_all_possible(dictionaries$long_name, single_cohort_dic)
  id <- .get_id_from_match(dictionaries$ref, match)
  return(id)
}

#' Get ID from Match
#'
#' This function retrieves the ID corresponding to matched reference dictionary entries.
#'
#' @param ids A vector of IDs from reference dictionaries.
#' @param matches A logical vector indicating which entries in the reference dictionaries matched.
#'
#' @return A vector of matched IDs.
#' @export
.get_id_from_match <- function(ids, matches){
  match_id <- ids[matches]
  return(match_id)
}


#' Check Cohort Dictionary with All Possible Matches
#'
#' This function checks a cohort dictionary against all possible reference dictionary matches.
#'
#' @param long_names A character vector of long names from reference dictionaries.
#' @param single_cohort_dic A single cohort dictionary to check.
#'
#' @return A logical vector indicating if a match is found.
#' @noRd
.check_cohort_dic_with_all_possible <- function(long_names, single_cohort_dic){
  all_possible_matches <- long_names %>%
    map_lgl(~.check_single_match(.x, single_cohort_dic))
}

#' Check Single Match Between Reference and Cohort Dictionary
#'
#' This function checks if a single reference dictionary long name matches a single cohort dictionary.
#'
#' @param single_ref_long_name A single long name from a reference dictionary.
#' @param single_cohort_dic A single cohort dictionary to check.
#'
#' @return A logical value indicating if all components match.
#' @noRd
.check_single_match <- function(single_ref_long_name, single_cohort_dic){
  components <- .extract_reference_components(single_ref_long_name)
  any_match <- .check_if_any_components_match(single_cohort_dic, components)
  all_match <- .check_if_all_components_match(any_match)
  return(all_match)
}

#' Extract Reference Components
#'
#' This function extracts the components of a reference dictionary long name.
#'
#' @param long_name A long name from a reference dictionary.
#'
#' @return A character matrix of components.
#' @noRd
.extract_reference_components <- function(long_name){
  components <- str_split(long_name, "_", simplify = TRUE)
  return(components)
}

#' Check if Any Components Match
#'
#' This function checks if any components of a reference dictionary long name match a cohort dictionary.
#'
#' @param single_cohort_dic A single cohort dictionary.
#' @param components A character matrix of components from a reference dictionary long name.
#'
#' @return A list of logical values indicating if each component matches.
#' @noRd
.check_if_any_components_match <- function(single_cohort_dic, components){
  any_match <- components %>%
    map(~.check_if_component_matches(single_cohort_dic, .x))
  return(any_match)
}

#' Check if Component Matches
#'
#' This function checks if a single component of a reference dictionary long name matches a cohort dictionary.
#'
#' @param single_cohort_dic A single cohort dictionary.
#' @param single_ref_component A single component from a reference dictionary long name.
#'
#' @return A logical value indicating if the component matches.
#' @noRd
.check_if_component_matches <- function(single_cohort_dic, single_ref_component){
  match_present <- str_detect(single_cohort_dic, single_ref_component)
  return(match_present)
}

#' Check if All Components Match
#'
#' This function checks if all components of a reference dictionary long name match a cohort dictionary.
#'
#' @param any_match A list of logical values indicating if each component matches.
#'
#' @return A logical value indicating if all components match.
#' @noRd
.check_if_all_components_match <- function(any_match){
  all_match <- all(unlist(any_match))
}

#' Join Labels with Cohort Table
#'
#' This function joins labels from reference dictionaries with cohort dictionaries based on `ref` column.
#'
#' @param coh_dics A tibble containing cohort dictionaries.
#' @param dictionaries A tibble containing reference dictionaries with labels.
#'
#' @return A tibble with labels joined to cohort dictionaries.
#' @noRd
.join_labels_with_cohort_table <- function(coh_dics, dictionaries) {
  with_labels <- left_join(coh_dics, dictionaries, by = "ref")
  return(with_labels)
}

#' Join IDs with Cohort Table
#'
#' This function joins IDs with cohort dictionaries.
#'
#' @param cohort_dics A tibble containing cohort dictionaries.
#' @param clean_ids A vector of cleaned IDs to join.
#'
#' @return A tibble with IDs joined to cohort dictionaries.
#' @noRd
.join_ids_with_cohort_table <- function(cohort_dics, clean_ids) {
  with_ids <- cohort_dics %>%
    mutate(ref = clean_ids)
  return(with_ids)
}

#' Clean IDs
#'
#' This function cleans IDs by handling specific cases and replacing them with appropriate values.
#'
#' @param ids A list of IDs to be cleaned.
#'
#' @return A cleaned vector of IDs.
#' @noRd
.clean_ids <- function(ids) {
  ids_clean <- ids %>%
    map(function(x) {
      if (identical(x, character(0))) {
        x <- NA
      }
      if (identical(x, c("id_13", "id_17"))) {
        x <- "id_17"
      }
      if (identical(x, c("id_14", "id_18"))) {
        x <- "id_17"
      }
      if (identical(x, c("id_15", "id_19"))) {
        x <- "id_19"
      }
      if (identical(x, c("id_16", "id_20"))) {
        x <- "id_20"
      }

      return(x)
    })
  return(unlist(ids_clean))
}

#' Join IDs with Cohort Table
#'
#' @param cohort_dics The cohort dictionary data frame.
#' @param clean_ids Cleaned IDs.
#' @return A data frame with IDs joined with the cohort table.
#' @noRd
.join_ids_with_cohort_table <- function(cohort_dics, clean_ids) {
  with_ids <- cohort_dics %>%
    mutate(ref = clean_ids)
  return(with_ids)
}

#' Join Labels with Cohort Table
#'
#' @param coh_dics Cohort dictionary data frame.
#' @param dictionaries Data frame containing dictionary information.
#' @return A data frame with labels joined with the cohort table.
#' @noRd
.join_labels_with_cohort_table <- function(coh_dics, dictionaries) {
  with_labels <- left_join(coh_dics, dictionaries, by = "ref")
  return(with_labels)
}

#' Make Final Dictionary Table
#' @param cohort_table_with_ids Data frame containing cohort table with IDs.
#' @return A final dictionary table.
#' @noRd
.make_final_dic_table <- function(cohort_table_with_labels) {
  ref <- desc <- value <- cohort <- type <- name <- NULL
  neat_table <- cohort_table_with_labels %>%
    group_by(cohort, ref) %>%
    arrange(desc(value)) %>%
    group_split() %>%
    map(~ slice(., 1)) %>%
    bind_rows() %>%
    mutate(long_name = paste0(type, "_", name))
  return(neat_table)
}
