#' Get Demographics Data
#'
#' This function retrieves and processes demographic data from various data sources.
#'
#' @param available_dics A data frame containing available dictionary information.
#' @param core_df The core data frame to be used.
#' @param yearly_df The yearly data frame containing repeated measures.
#' @param edu_var The educational variable to be used.
#' @param conns A datashield connections object
#'
#' @return A list containing combined demographic statistics.
#' @importFrom dsHelper dh.getStats dh.makeStrata
#' @importFrom dplyr %>%
#' @importFrom purrr pmap
#' @export
get_demographics <- function(available_dics, core_df, yearly_df, edu_var, conns) {

  suppressMessages(
    .make_first_year_from_repeats(available_dics, yearly_df, edu_var = edu_var, conns = conns)
    edu_stats <- .get_stats_where_available(available_dics, yearly_df, "df_year_1", edu_var, conns)
    child_age_stats <- .get_child_age_range(available_dics, conns)
    out <- list(
      categorical = edu_stats$categorical,
      continous = child_age_stats)
  )
  return(out)
}

#' Get Oldest and Youngest Ages
#'
#' This function retrieves the oldest and youngest ages from the combined age statistics.
#'
#' @param age_stats_combined A data frame containing combined age statistics.
#' @param conns A datashield connections object
#' @return A data frame with the oldest and youngest ages formatted.
#' @importFrom dplyr if_else mutate n
#' @noRd
.get_oldest_youngest_ages <- function(repeated_stats) {
  cohort <- NULL
  formatted <- repeated_stats %>%
    map(~.x$continuous) %>%
    bind_rows() %>%
    dplyr::filter(cohort != "combined") %>%
    group_by(cohort) %>%
    arrange(mean) %>%
    slice(c(1, n())) %>%
    mutate(variable = if_else(mean == min(mean), "min_age", "max_age"))
  return(formatted)
}

#' Get Child Age Range
#'
#' This function retrieves the age range of children from the repeated measures data.
#'
#' @param available_dics A data frame containing available dictionary information.
#' @param conns A datashield connections object
#' @return A list of age statistics for children.
#' @noRd
.get_child_age_range <- function(available_dics, conns) {
  long_name <- NULL
  repeated_dfs <- .get_repeated_df_names(available_dics)
  repeated_stats <- .get_stats_repeated(available_dics, repeated_dfs, conns)
  child_ages <- .get_oldest_youngest_ages(repeated_stats)
  return(child_ages)
}

.get_stats_repeated <- function(avaiable_dics, repeated_dfs, conns){
  child_ages <- available_dics %>%
    dplyr::filter(long_name %in% repeated_dfs) %>%
    pmap(function(cohort, long_name, ...) {
      dh.getStats(
        vars = "age_years",
        df = long_name,
        conns = conns[cohort]
      )
    })
}

.get_repeated_df_names <- function(available_dics){
  repeated_dfs <- available_dics$long_name %>%
    str_subset("weekly|monthly|yearly|trimester")
  return(repeated_dfs)
}
#' Get Statistics Where Available
#'
#' This function retrieves statistics where available from the provided data sources.
#'
#' @param available_dics A data frame containing available dictionary information.
#' @param filter_var The variable used to filter the dictionary.
#' @param df The data frame from which to extract statistics.
#' @param vars A vector of variables to extract statistics for.
#' @param conns A datashield connections object
#'
#' @return A list of statistics for the specified variables.
#' @importFrom dplyr pull
#' @noRd
.get_stats_where_available <- function(available_dics, filter_var, df, vars, conns) {
  long_name <- cohort <- NULL
  available_dics %>%
    dplyr::filter(long_name == filter_var) %>%
    pull(cohort) %>%
    map(
      ~ dh.getStats(
        df = df,
        vars = vars,
        conns = conns[.x]
      )
    ) %>%
    pmap(bind_rows)
}

#' Make First Year from Repeats
#'
#' This function processes repeated measures data to extract the earliest measure for each child.
#'
#' @param available_dics A data frame containing available dictionary information.
#' @param year_rep_df The yearly data frame containing repeated measures.
#' @param edu_var The educational variable to be used.
#' @param conns A datashield connections object
#' @importFrom dplyr pull
#' @importFrom dsBaseClient ds.dataFrameSubset
#'
#' @return None. The function modifies the data in place.
#' @noRd
.make_first_year_from_repeats <- function(available_dics, year_rep_df, edu_var,
                                          conns) {
  long_name <- cohort <- NULL
  available_dics %>%
    dplyr::filter(long_name == year_rep_df) %>%
    pull(cohort) %>%
    map(
      ~ dsBaseClient::ds.dataFrameSubset(
        df.name = year_rep_df,
        V1.name = paste0(year_rep_df, "$age_years"),
        V2.name = "1",
        Boolean.operator = "<",
        newobj = "df_year_1",
        datasources = conns[.x]
      )
    )
}
