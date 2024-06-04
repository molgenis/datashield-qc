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
#' @export
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
#' @export
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
#' @export
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
#' @export
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
#' @export
.make_final_path_table <- function(paths_as_tibble){
  name <- type <- NULL
  out <- paths_as_tibble %>%
    mutate(
      name = str_remove(file, ".xlsx"),
      name = str_remove(name, "\\b[_\\d]+")) %>%
    dplyr::select(type, name) %>%
    distinct() %>%
    dplyr::filter(name != "peripheral_blood") ## Misnamed dictionary
  return(out)
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
