#' Perform Quality Control for DataShield Connections
#'
#' This function performs a series of quality control checks on DataShield connections. It checks server accessibility, logs into the servers, retrieves package and dictionary information, assigns variables, fetches variable statistics, and obtains key demographics.
#'
#' @param login_data A tibble containing server information.
#' @importFrom cli cli_h1 cli_alert_success
#' @importFrom DSI datashield.pkg_status
#' @importFrom rlang set_names
#' @return A list containing:
#' \item{accessible}{Boolean indicating if servers are reachable.}
#' \item{login_errors}{Summary of login errors, if any.}
#' \item{packages}{Information about the DataShield packages available.}
#' \item{coh_dics}{Dictionary information available for each cohort.}
#' \item{all_dics}{All possible dictionary information.}
#' \item{variables}{Statistics for all variables.}
#' \item{demographics}{Specific demographic information.}
#' @export
run_qc <- function(login_data) {
  options(datashield.progress = FALSE)

  cli_h1("Performing DataSHIELD QC")

  cli_h1("Checking if servers are reachable")
  accessible <- .check_server_up(login_data)
  cli_alert_success("Done")

  cli_h1("Logging in")
  login_return <- .login_qc(login_data)
  cli_alert_success("Done")

  cli_h1("Fetching package information")
  packages <- datashield.pkg_status(login_return$conns)
  cli_alert_success("Done")

  cli_h1("Fetching table information")
  all_possible_dictionaries <- list_all_dic_files()
  cohort_dics_available <- identify_cohort_dics(login_return$conns)
  cli_alert_success("Done")

  cli_h1("Assigning tables")
  assign_where_available(cohort_dics_available, login_return$conns)
  cli_alert_success("Done")

  cli_h1("Fetching variable information")
  options(cli.spinner = "line")
  all_vars <- .get_stats_all_tables(cohort_dics_available, login_return$conns)
  cli_alert_success("Done")

  cli_h1("Fetching additional demographic information")
  demographics <- get_demographics(cohort_dics_available, "core_non", "core_yearly", "edu_m_", login_return$conns)

  out <- list(
    accessible = accessible,
    login_errors = login_return$login_summary,
    packages = packages,
    coh_dics = cohort_dics_available,
    all_dics = all_possible_dictionaries,
    variables = all_vars,
    demographics = demographics
  )

  cli_alert_success("QC Complete")

  return(out)
}
