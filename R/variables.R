#' Summarize Variables
#'
#' This function summarizes the variables available in the dictionaries by counting the total variables and
#' getting the maximum sample size for each cohort.
#'
#' @param qc_stats output from qc stats
#' @importFrom dplyr case_when
#' @return A data frame summarizing the total number of variables and the maximum sample size for each cohort.
#' @export
summarise_variables <- function(qc_stats) {
  n_vars <- .count_vars(qc_stats)
  max_obs <- .get_max_obs(qc_stats)
  combined <- left_join(n_vars, max_obs, by = "cohort")
  transposed <- .flip_rows_columns(combined)
  out <- .rename_vars(transposed)
  return(out)
}

#' Count Variables per Cohort
#'
#' This function counts the number of distinct variables for each cohort in the provided QC statistics data.
#'
#' @param qc_stats A list containing QC statistics data. It is expected that `qc_stats` has a component named `variables`
#'   which is a list of data frames. Each data frame should contain at least the columns `cohort` and `variable`.
#'
#' @return A tibble with two columns: `cohort` and `total_vars`. Each row represents a cohort and the number of distinct
#'   variables associated with that cohort.
#'
#' @importFrom dplyr %>% n
#' @importFrom purrr pmap
#' @noRd
.count_vars <- function(qc_stats) {
  cohort <- NULL
  n_vars <- qc_stats$variables %>%
    purrr::pmap(dplyr::bind_rows) %>%
    purrr::map(~dplyr::select(., cohort, variable)) %>%
    dplyr::bind_rows() %>%
    dplyr::filter(cohort != "combined") %>%
    dplyr::distinct() %>%
    dplyr::group_by(cohort) %>%
    dplyr::summarise(total_vars = n())

  return(n_vars)
}

#' Get Maximum Sample Size
#'
#' This function retrieves the maximum sample size for each cohort from a non-repeated measures data frame.
#'
#' @param non_rep_df The non-repeated measures data frame.
#' @param conns A datashield connections object
#' @return A data frame with the maximum sample size for each cohort.
#' @importFrom dsBaseClient ds.dim
#' @importFrom dplyr filter
#' @noRd
.get_max_obs <- function(qc_stats) {
  cohort <- valid_n <- max_obs <- NULL
  max_sample <- qc_stats$variables$core_non_rep$continuous %>%
    group_by(cohort) %>%
    arrange(valid_n) %>%
    slice(1) %>%
    dplyr::rename("max_obs" = "cohort_n") %>%
    filter(cohort != "combined") %>%
    dplyr::select(cohort, max_obs)

  return(max_sample)
}

#' Flip Rows and Columns in a Summary Table
#'
#' This function takes a summary table with specific columns and flips the rows and columns.
#' Specifically, it transforms the summary table from wide to long format and then back to wide format,
#' effectively swapping the rows and columns for certain measures.
#'
#' @param var_summary A data frame containing the summary of variables.
#'   The data frame should include at least the columns `total_vars`, `max_obs`, and `cohort`.
#' @return A data frame with flipped rows and columns. The output data frame will have the measures
#'   (`total_vars` and `max_obs`) as rows and the cohorts as columns.
#' @importFrom tidyr pivot_longer pivot_wider
#' @noRd
.flip_rows_columns <- function(var_summary) {
  total_vars <- max_obs <- cohort <- value <- NULL
  flipped <- var_summary %>%
    pivot_longer(
      cols = c(total_vars, max_obs),
      names_to = "measure",
      values_to = "value"
    ) %>%
    pivot_wider(
      names_from = cohort,
      values_from = value
    )
  return(flipped)
}

#' Rename Variables in Transposed Data
#'
#' This function renames certain values in the `measure` column of the transposed data.
#'
#' @param transposed A data frame containing a column named `measure`. The values in this column will be modified based on specific conditions.
#' @return A data frame with the `measure` column values renamed:
#'   "total_vars" is renamed to "Number of variables" and "max_obs" is renamed to "Maximum number of participants".
#' @importFrom dplyr case_when
#' @noRd
.rename_vars <- function(transposed) {
  out <- transposed %>%
    dplyr::mutate(measure = dplyr::case_when(
      measure == "total_vars" ~ "Number of variables",
      measure == "max_obs" ~ "Maximum number of participants"
    ))

  return(out)
}



