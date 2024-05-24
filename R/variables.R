#' Summarize Variables
#'
#' This function summarizes the variables available in the dictionaries by counting the total variables and
#' getting the maximum sample size for each cohort.
#'
#' @param available_dics A data frame containing available dictionary information.
#' @param core_non_rep Character specifying name of core non repeated data set
#' @param conns A datashield connections object
#' @importFrom dplyr case_when
#' @return A data frame summarizing the total number of variables and the maximum sample size for each cohort.
#' @export
summarise_variables <- function(available_dics, core_non_rep = "core_non_rep", conns) {
  cols <- .get_all_columns(available_dics, conns)
  cols_formatted <- .format_columns(cols)
  lengths <- .get_max_sample(core_non_rep, conns)
  counts <- .count_variable(cols_formatted)
  var_summary <- left_join(counts, lengths, by = "cohort")
  summary_flipped <- .flip_rows_columns(var_summary)
  out <- summary_flipped %>%
    mutate(measure = case_when(
      measure == "total_vars" ~ "Number of variables",
      measure == "max_obs" ~ "Maximum number of participants"
    ))
  return(out)
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


#' Get Maximum Sample Size
#'
#' This function retrieves the maximum sample size for each cohort from a non-repeated measures data frame.
#'
#' @param non_rep_df The non-repeated measures data frame.
#' @param conns A datashield connections object
#' @return A data frame with the maximum sample size for each cohort.
#' @importFrom dsBaseClient ds.dim
#' @noRd
.get_max_sample <- function(non_rep_df, conns) {
  cohort <- NULL
  lengths <- ds.dim(non_rep_df)

  lengths_formatted <- lengths %>%
    set_names(c(names(conns), "combined")) %>%
    map(~ .[1]) %>%
    map(as_tibble) %>%
    bind_rows(.id = "cohort") %>%
    dplyr::rename("max_obs" = "value") %>%
    dplyr::filter(cohort != "combined")

  return(lengths_formatted)
}

#' Count Variables
#'
#' This function counts the total number of variables available in the formatted columns for each cohort.
#'
#' @param cols_formatted A data frame containing formatted columns.
#'
#' @return A data frame with the total number of variables for each cohort.
#' @importFrom dplyr group_by reframe n
#' @noRd
.count_variable <- function(cols_formatted) {
  cohort <- NULL
  counts <- cols_formatted %>%
    group_by(cohort) %>%
    reframe(
      total_vars = n()
    )
  return(counts)
}

#' Get All Columns
#'
#' This function retrieves all column names from the available dictionaries for each cohort.
#'
#' @param available_dics A data frame containing available dictionary information.
#' @param conns A datashield connections object
#' @return A named list of column names for each dictionary.
#' @importFrom dsBaseClient ds.colnames
#' @importFrom rlang set_names
#' @noRd
.get_all_columns <- function(available_dics, conns) {
  available_dics %>%
    pmap(function(cohort, long_name, ...) {
      ds.colnames(long_name, datasources = conns[cohort])
    }) %>%
    set_names(available_dics$long_name)
}

#' Format Columns
#'
#' This function formats the retrieved column names into a tidy data frame.
#'
#' @param columns A named list of column names for each dictionary.
#'
#' @return A data frame with formatted column names.
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_longer
#' @noRd
.format_columns <- function(columns) {
  formatted <- columns %>%
    map(as_tibble) %>%
    map(
      ~ pivot_longer(
        data = .,
        everything(),
        names_to = "cohort",
        values_to = "variable"
      )
    ) %>%
    bind_rows(.id = "df")
  return(formatted)
}
