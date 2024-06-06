#' Get Statistics for All Tables
#'
#' This function retrieves statistics for a list of cohorts from a specified connection.
#'
#' @param cohort_dics_available A data frame containing cohort dictionaries, including at least the columns 'cohort' and 'long_name'.
#' @param conns A list of connections, with names corresponding to the cohorts.
#' @return A named list of statistics for each cohort.
#' @importFrom purrr pmap set_names
#' @importFrom cli cli_alert_info
#' @importFrom dplyr %>%
#' @importFrom dsHelper dh.getStats
#' @export
.get_stats_all_tables <- function(cohort_dics_available, conns){
  stats <- cohort_dics_available %>%
    pmap(function(cohort, long_name, ...) {
      cli_alert_info(long_name)
      suppressMessages(
        suppressWarnings(
          dh.getStats(
            df = long_name,
            conns = conns[cohort])
        )
      )
    }) %>%
    set_names(cohort_dics_available$long_name)
  return(stats)
}
