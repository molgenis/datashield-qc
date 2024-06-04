#' Perform Quality Control for DataShield Connections
#'
#' This function performs a series of quality control checks on DataShield connections. It fetches package information,
#' retrieves dictionary information, assigns variables, fetches variable statistics, and obtains key demographics.
#'
#' @param conns A list of DataShield connections.
#' @importFrom cli cli_h1 cli_alert_info
#' @importFrom DSI datashield.pkg_status
#' @return A list containing:
#' \item{packages}{Information about the DataShield packages available.}
#' \item{variables}{Statistics for the all variables.}
#' \item{demographics}{Specific demographic information.}
#' @export
perform_qc <- function(conns){
  options(datashield.progress = F)
  ## Find out how to suppress the bar with percentages

  ## Add a nice message at the top

  cli_h1("Fetching package information")
  packages <- datashield.pkg_status(conns)

  cli_h1("Getting dictionary information")
  all_possible_dictionaries <- list_all_dic_files()
  cohort_dics_available <- identify_cohort_dics(conns)

  cli_h1("Assigning variables")
  assign_where_available(cohort_dics_available, conns)

  cli_h1("Fetching variable information")
  all_vars <- cohort_dics_available %>%
    pmap(function(cohort, long_name, ...) {
      cli_alert_info(paste0("\nTable ", long_name))
      invisible(
        dh.getStats(
          df = long_name,
          conns = conns[cohort])
      )
    })

  cli_h1("Fetching additional demographics")
  ### This should be done in the formatting stage, don't get core demographics twice
  demographics <- .get_demographics(
    available_dics = cohort_dics_available,
    core_df = "core_non_rep",
    yearly_df = "core_yearly_rep",
    core_vars = c("agebirth_m_y", "ethn3_m", "sex"),
    edu_var = "edu_m_",
    repeated_dfs <- c("chemicals_ath_yearly_rep", "core_yearly_rep", "outcome_yearly_rep",
                      "outcome_ath_yearly_rep", "urban_ath_yearly_rep"),
    conns = conns)

  out <- list(
    packages = packages,
    variables = all_vars,
    demographics = demographics)

  return(out)

}
