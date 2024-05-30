#' Identify Cohort Dictionaries
#'
#' This function identifies cohort dictionaries by processing a directory of dictionary files
#' and then matches the extracted cohort dictionaries with the data tables. It cleans and organizes
#' the dictionary information and returns a final table.
#'
#' @param path_to_dic The path to the directory containing the dictionary files.
#' @param conns The connections object to the database where the data tables are stored.
#' @importFrom dplyr bind_rows group_by arrange slice group_split left_join
#' @importFrom purrr map map_lgl pmap
#' @importFrom stringr str_remove_all str_split str_detect
#' @importFrom DSI datashield.tables
#' @return A final table containing cleaned and organized cohort dictionary information.
#' @export
identify_cohort_dics <- function(conns) {
  dictionaries <- list_all_dic_files()
  # dictionaries <- .list_all_dic_files_old(path_to_dic)
  dictionaries$ref <- paste0("id_", 1:nrow(dictionaries))
  elements <- .get_essential_dic_elements(dictionaries$name)
  # elements_split <- .split_essential_dic_elements(elements)
  elements_split_with_type <- .join_split_dic_elements_with_type(elements, dictionaries$type)
  cohort_dics <- .get_cohort_dics(conns)
  ids <- .get_ids_for_everything(cohort_dics$value, elements_split_with_type, dictionaries$ref)
  clean_ids <- unlist(.clean_ids(ids))
  cohort_table_with_ids <- .join_ids_with_cohort_table(cohort_dics, clean_ids)
  cohort_table_with_labels <- .join_labels_with_cohort_table(cohort_table_with_ids, dictionaries)
  final_table <- .make_final_dic_table(cohort_table_with_labels)
  return(final_table)
}



#' List All Dictionary Files
#'
#' This function lists all dictionary files available in the specified path by combining all the helper functions.
#'
#' @param path_to_dictionaries The path to the directory containing dictionaries.
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

.get_dics_from_api <- function(){
  req <- request("https://api.github.com/repos/lifecycle-project/ds-dictionaries/git/trees/master?recursive=true")
  resp <- req_perform(req)
  content <- resp_body_json(resp)
  return(content)
}

.format_response <- function(response){
  paths <- content$tree %>% map_chr(~.$path)
  return(paths)
}

.shorten_paths <- function(neat_response){
  shorter <- neat_response %>%
    str_subset(".xlsx") %>%
    str_remove("dictionaries/")
  return(shorter)
}

.split_paths_to_tibble <- function(shorter_paths){
  paths_tibble <- shorter_paths %>%
    str_split("/") %>%
    map(t) %>%
    map(as_tibble) %>%
    bind_rows() %>%
    set_names(c("type", "version", "file"))
  return(paths_tibble)
}

.make_final_path_table <- function(paths_as_tibble){
  out <- paths_as_tibble %>%
  mutate(
    name = str_remove(file, ".xlsx"),
    name = str_remove(name, "\\b[_\\d]+")) %>%
  dplyr::select(type, name) %>%
  distinct() %>%
    dplyr::filter(name != "peripheral_blood") ## Misnamed dictionary
  return(out)
}
















 .list_all_dic_files_old <- function(path_to_dictionaries){

  types <- .list_dictionary_types(path_to_dictionaries)
  latest <- types %>% map_chr(.list_latest_version)
  files <- latest %>% map(.list_available_dics)
  neat_names <- latest %>% map(.extract_name)
  out <- .format_dictionary_names(files, neat_names)
  return(out)
}


#' List Dictionary Types
#'
#' This function lists all dictionary types available in the specified path.
#'
#' @param path_to_dictionaries The path to the directory containing dictionaries.
#'
#' @return A character vector of paths to dictionary types.
#' @noRd
.list_dictionary_types <- function(path_to_dictionaries) {
  list.dirs(path_to_dictionaries, recursive = FALSE)
}

#' List Latest Version
#'
#' This function lists the latest version of a dictionary type.
#'
#' @param dict_types A character vector of paths to dictionary types.
#'
#' @return A character string of the path to the latest version of the dictionary.
#' @noRd
.list_latest_version <- function(dict_types) {
  all_versions <- list.dirs(dict_types, recursive = FALSE)
  latest_version <- sort(all_versions, decreasing = TRUE)[1]
  return(latest_version)
}

#' List Available Dictionaries
#'
#' This function lists all available dictionary files in the latest version directory.
#'
#' @param latest_version The path to the latest version of the dictionary.
#'
#' @return A character vector of available dictionary files.
#' @noRd
.list_available_dics <- function(latest_version) {
  list.files(latest_version)
}

#' Extract Name from Path
#'
#' This function extracts a neat name from the latest version path.
#'
#' @param latest_version The path to the latest version of the dictionary.
#'
#' @return A character string of the extracted neat name.
#' @importFrom stringr str_extract
#' @noRd
.extract_name <- function(latest_version) {
  neat_name <- str_extract(latest_version, "(?<=ds-dictionaries/dictionaries/)(.*?)(?=/[^/]*$)")
  return(neat_name)
}

#' Format Dictionary Names
#'
#' This function formats dictionary names by creating a tidy data frame from the file names.
#'
#' @param files A list of character vectors containing dictionary file names.
#' @param neat_names A list of neat names corresponding to the dictionary files.
#' @return A data frame with formatted dictionary names.
#' @importFrom rlang set_names
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate
#' @importFrom stringr str_remove
#' @noRd
.format_dictionary_names <- function(files, neat_names) {
  value <- name <- type <- long_name <- cohort <- NULL
  out <- files %>%
    set_names(neat_names) %>%
    map(as_tibble) %>%
    map(bind_rows) %>%
    bind_rows(.id = "type") %>%
    dplyr::rename(name = value) %>%
    mutate(
      name = str_remove(name, "\\.xlsx$"),
      long_name = str_remove(name, "\\b[_\\d]+")
    ) %>%
    mutate(long_name = paste0(type, "_", long_name))
  return(out)
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
  ref <- desc <- value <- cohort <- NULL
  neat_table <- cohort_table_with_labels %>%
    group_by(cohort, ref) %>%
    arrange(desc(value)) %>%
    group_split() %>%
    map(~ slice(., 1)) %>%
    bind_rows() %>%
    mutate(long_name = paste0(type, "_", name))
  return(neat_table)
}

#' Get Essential Dictionary Elements
#'
#' @param dictionaries Vector containing dictionary names.
#' @return A vector of essential dictionary elements.
#' @noRd
.get_essential_dic_elements <- function(dictionaries) {
  stems <- str_remove_all(dictionaries, "^[\\d_]+")
  stems <- str_remove_all(stems, "_rep$")
  return(stems)
}

#' Split Essential Dictionary Elements
#'
#' @param essential_dic_elements Vector containing essential dictionary elements.
#' @return A list of split essential dictionary elements.
#' @noRd
.split_essential_dic_elements <- function(essential_dic_elements) {
  stems_split <- essential_dic_elements %>% map(~ str_split(.x, "_"))
  return(stems_split)
}

#' Join Split Dictionary Elements with Type
#'
#' @param split_dic_elements List containing split dictionary elements.
#' @param types Vector containing dictionary types.
#' @return A list of split dictionary elements with types.
#' @noRd
.join_split_dic_elements_with_type <- function(split_dic_elements, types) {
  all_dic_parts <- list(split_dic_elements, types) %>%
    pmap(function(.x, .y) {
      out <- c(.x[[1]], .y)
      return(out)
    })
}

#' Get IDs for Everything
#'
#' @param dic_vec Vector containing dictionary elements.
#' @param all_dic_parts List containing all dictionary parts.
#' @param id_refs Vector containing ID references.
#' @return A vector of IDs for all elements.
#' @noRd
.get_ids_for_everything <- function(dic_vec, all_dic_parts, id_refs) {
  dic_vec %>%
    map(~ .summarise_which_dic_matched(.x, all_dic_parts, id_refs))
}

#' Summarise Which Dictionary Matched

#' @param single_coh_dic Single cohort dictionary element.
#' @param all_dic_parts List containing all dictionary parts.
#' @param id_refs Vector containing ID references.
#' @return Names of the matched dictionary.
#' @noRd
.summarise_which_dic_matched <- function(single_coh_dic, all_dic_parts, id_refs) {
  what_is_matched <- .find_if_any_dic_present(single_coh_dic, all_dic_parts)
  names_of_what_matched <- id_refs[what_is_matched]
  return(names_of_what_matched)
}

#' Find if Any Dictionary Present
#'
#' @param single_coh_dic Single cohort dictionary element.
#' @param all_dic_parts List containing all dictionary parts.
#' @return Logical vector indicating whether any dictionary is present.
#' @noRd
.find_if_any_dic_present <- function(single_coh_dic, all_dic_parts) {
  any_present <- all_dic_parts %>%
    map_lgl(~ .find_if_dic_present(single_coh_dic, .x))
  return(any_present)
}

#' Find if Dictionary Present
#'
#' @param single_coh_dic Single cohort dictionary element.
#' @param single_dic_part Single dictionary part.
#' @return Logical value indicating whether the dictionary is present.
#' @noRd
.find_if_dic_present <- function(single_coh_dic, single_dic_part) {
  present <- single_dic_part %>%
    map(~ str_detect(single_coh_dic, .x))
  is_it_present <- all(unlist(present))
  return(is_it_present)
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

#' Clean IDs
#'
#' @param ids Vector containing IDs.
#' @return Cleaned IDs.
#' @noRd
.clean_ids <- function(ids) {
  ids_clean <- ids %>%
    map(function(x) {
      if (identical(x, character(0))) {
        x <- NA
      }
      if (identical(x, c("id_12", "id_16"))) {
        x <- "id_16"
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
      return(x)
    })
  return(ids_clean)
}
