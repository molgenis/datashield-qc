#' Perform Quality Control for DataShield Connections
#'
#' This function performs a series of quality control checks on DataShield connections. It fetches package information,
#' retrieves dictionary information, assigns variables, fetches variable statistics, and obtains key demographics.
#'
#' @param login_data Tibble of server info.
#' @importFrom cli cli_h1 cli_alert_info
#' @importFrom DSI datashield.pkg_status
#' @importFrom rlang set_names
#' @return A list containing:
#' \item{packages}{Information about the DataShield packages available.}
#' \item{variables}{Statistics for the all variables.}
#' \item{demographics}{Specific demographic information.}
#' @export
run_qc <- function(login_data){
  options(datashield.progress = F)

  cli_h1("Performing DataSHIELD QC")

  cli_h1("Checking if servers are reachable")
  accessible <- .check_server_up(login_data)

  cli_h1("Logging in")
  login_return <- .login_qc(login_data)

  cli_h1("Fetching package information")
  packages <- datashield.pkg_status(login_return$conns)

  cli_h1("Fetching table information")
  all_possible_dictionaries <- list_all_dic_files()
  cohort_dics_available <- identify_cohort_dics(login_return$conns)

  cli_h1("Assigning variables")
  assign_where_available(cohort_dics_available, login_return$conns)

  cli_h1("Fetching variable information")
  all_vars <- .get_stats_all_tables(cohort_dics_available, login_return$conns)

  out <- list(
    packages = packages,
    coh_dics = cohort_dics_available,
    all_dics = all_possible_dictionaries,
    variables = all_vars)

  return(out)

}
