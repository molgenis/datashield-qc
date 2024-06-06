#' Check if Server is Up
#'
#' This function checks if the servers specified in the login data are up and running.
#' It sends a request to each server URL and formats the error messages if any.
#'
#' @param login_data A data frame containing the login data with a column `url`.
#' @return A tibble with columns `success` indicating if the server is up and `message` containing the error message if any.
#' @noRd
.check_server_up <- function(login_data){
  response <- login_data$url %>% map(.get_handle_error)
  errors <- response %>% map(.extract_get_error)
  formatted <- errors %>%
    map(.format_get_error) %>%
    set_names(login_data$cohort) %>%
    bind_rows(.id = "cohort")
  return(formatted)
}

#' Get Handle Error
#'
#' This function sends a request to the specified URL and safely captures any errors.
#'
#' @param url A character string specifying the URL to which the request is sent.
#' @importFrom purrr safely
#' @return A list containing the response or the error captured.
#' @noRd
.get_handle_error <- function(url){
  req_perform_safely <- safely(req_perform)
  req <- request(url) %>%
    req_perform_safely()
  return(req)
}

#' Extract GET Error
#'
#' This function extracts the error message from the response if there is any.
#'
#' @param response A list containing the response from the server request.
#' @return A character string containing the error message.
#' @noRd
.extract_get_error <- function(response){
  error <- response$error$message
  return(error)
}

#' Format GET Errors
#'
#' This function formats the error messages into a tibble indicating if the request was successful.
#'
#' @param error A character string containing the error message.
#' @importFrom tibble tibble
#' @return A tibble with columns `success` and `message`.
#' @noRd
.format_get_error <- function(error){
  if(is.null(error)){
    out <- tibble(success = "Yes", message = NA)
  } else {
    out <- tibble(message = error) %>%
      mutate(success = ifelse(is.null(message), "Yes", "No"))
  }
  return(out)
}
