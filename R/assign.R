#' Assign Data Tables to Connections Where Available
#'
#' This function takes a data frame of available data dictionaries and assigns tables to connections.
#' It uses the DataSHIELD function `datashield.assign.table` to assign each table to its respective cohort connection.
#'
#' @param available_dics A data frame containing the available data dictionaries. 
#'   The data frame should include columns `long_name`, `value`, and `cohort`.
#' @param conns A list of DataSHIELD connections indexed by cohort.
#' @return None. This function is called for its side effects of assigning tables to connections.
#' @importFrom purrr pmap
#' @importFrom DSI datashield.assign.table
#' @export
#' @export
assign_where_available <- function(available_dics, conns){
  available_dics %>%
    pmap(function(long_name, value, cohort, ...){
      datashield.assign.table(
        symbol = long_name,
        table = value,
        conns = conns[cohort])
    })
}
