#' Format Dictionary Information
#'
#' This function formats dictionary information by combining cohort-specific dictionaries with a master dictionary,
#' indicating the availability of each variable for each cohort, and restructuring the data for better readability.
#'
#' @param coh_dics A data frame containing cohort-specific dictionary information.
#' @param all_dics A data frame containing the master dictionary with all possible variables.
#' @return A data frame with formatted dictionary information, showing the availability of variables for each cohort.
#' @importFrom purrr map_chr imap
#' @importFrom tidyr pivot_wider
#' @export
format_dictionary_info <- function(coh_dics, all_dics) {
  cohort <- . <- value <- long_name <- dictionary <- available <- NULL
  formatted <- coh_dics %>%
    group_by(cohort) %>%
    group_split() %>%
    set_names(map_chr(., ~ .x$cohort[1])) %>%
    map(~ left_join(all_dics, .x, by = c("type", "name", "long_name"))) %>%
    imap(~ .x %>% mutate(cohort = .y)) %>%
    bind_rows() %>%
    mutate(available = ifelse(is.na(value), "no", "yes")) %>%
    mutate(dictionary = str_remove_all(long_name, "\\d+_\\d+_")) %>%
    dplyr::select(cohort, dictionary, available) %>%
    pivot_wider(
      names_from = cohort,
      values_from = available
    )

  return(formatted)
}
