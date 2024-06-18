#' Login Quality Control Function
#'
#' This function handles the login process for both Armadillo and Opal servers.
#' It splits credentials by server, retrieves tokens for Armadillo servers,
#' builds login objects, handles the login process, and constructs a summary of the login process.
#'
#' @param credentials A data frame containing the login credentials.
#'   It should include columns like `cohort`, `url`, `server`, `username`, `password`, `value`.
#' @importFrom cli cli_abort
#' @return A list containing the successful connections and a summary of the login process.
#' @noRd
.login_qc <- function(credentials){
  credentials_split <- .split_credentials_by_server(credentials)

  if(nrow(credentials_split$armadillo) > 0){
    credentials_split$armadillo <- .get_and_add_tokens(credentials_split)
  }

  login_data <- .build_login_objects(credentials_split)
  login_result <- .handle_login(login_data)
  login_success <- .get_login_success(login_result)
  login_failure <- .get_login_failure(login_result)
  login_summary <- .build_login_summary(credentials, login_failure)

  if(is.null(login_success)){
    cli_abort("Login failed for all cohorts, aborting QC")
  }

  return(
    list(
      conns = login_success,
      login_summary = login_summary)
  )
}

#' Split Credentials by Server
#'
#' This function splits the credentials into Armadillo and Opal based on the server type.
#'
#' @param credentials A data frame containing the login credentials.
#' @importFrom rlang set_names
#' @return A list of credentials split by server.
#' @noRd
.split_credentials_by_server <- function(credentials){
  server <- NULL
  split <- list(
    opal = credentials %>% dplyr::filter(server == "opal"),
    armadillo = credentials %>% dplyr::filter(server == "armadillo"))
  return(split)
}

#' .get_and_add_tokens
#'
#' This function retrieves tokens for Armadillo servers and merges them with the credentials.
#'
#' @param credentials A data frame containing the Armadillo login credentials.
#' @return A data frame of Armadillo credentials with tokens.
#' @noRd
.get_and_add_tokens <- function(credentials_split){
  token_result <- .get_tokens_result(credentials_split$armadillo)
  token_values <- .get_token_values(token_result)
  armadillo_credentials <- .join_tokens_with_credentials(credentials_split, token_values)
  return(armadillo_credentials)
}

#' Get Tokens
#'
#' This function retrieves tokens for Armadillo servers using `armadillo.get_token`.
#'
#' @param credentials A data frame containing the Armadillo login credentials.
#' @importFrom MolgenisArmadillo armadillo.get_token
#' @importFrom rlang set_names
#' @return A list of token retrieval results.
#' @noRd
.get_tokens_result <- function(credentials){
  server <- NULL
  .get_token_safely <- safely(armadillo.get_token)
  credentials %>%
    dplyr::filter(server == "armadillo") %>%
    pull(url) %>%
    map(~.get_token_safely(.x)) %>%
    set_names(credentials$cohort)
}

#' Get Token Values
#'
#' This function extracts token values from the token retrieval results.
#'
#' @param token_result A list containing the token retrieval results.
#' @return A data frame of token values.
#' @noRd
.get_token_values <- function(token_result){
  value <- token_result %>%
    map("result") %>%
    map(as_tibble) %>%
    bind_rows(.id = "cohort")
  return(value)
}

#' Join Tokens with Credentials
#'
#' This function joins the token values with the Armadillo credentials.
#'
#' @param credentials_split A list containing data frames of credentials split by server type. The list should have a named element `armadillo` which contains the Armadillo credentials.
#' @param token_values A data frame containing the token values to be joined with the Armadillo credentials. It should include a column `cohort` to join on.
#'
#' @return A data frame of Armadillo credentials with the token values joined.
#' @noRd
.join_tokens_with_credentials <- function(credentials_split, token_values){
  armadillo_credentials <- left_join(credentials_split$armadillo, token_values, by = "cohort")
  return(armadillo_credentials)
}

#' Build Login Objects
#'
#' This function builds login objects using `DSLoginBuilder` for both Armadillo and Opal credentials.
#'
#' @param armadillo_credentials A data frame of Armadillo login credentials.
#' @param opal_credentials A data frame of Opal login credentials.
#' @importFrom DSI newDSLoginBuilder
#' @return A list of login objects split by server.
#' @noRd
.build_login_objects <- function(credentials_split){
  builder <- newDSLoginBuilder()
  if(nrow(credentials_split$armadillo > 0)){
  .make_login_armadillo(builder, credentials_split$armadillo)
  }

  if(nrow(credentials_split$opal > 0)){
  .make_login_opal(builder, credentials_split$opal)
  }

  login_split <- .split_login_object(builder)
  return(login_split)
}

#' Make Login Armadillo
#'
#' This function adds Armadillo login information to the `DSLoginBuilder`.
#'
#' @param builder An object of class `DSLoginBuilder`.
#' @param armadillo_credentials A data frame of Armadillo login credentials.
#' @return None.
#' @noRd
.make_login_armadillo <- function(builder, armadillo_credentials){
  armadillo_credentials %>%
    pmap(function(cohort, url, server, value, ...){
      builder$append(
        url = url,
        server = cohort,
        token = value,
        driver = "ArmadilloDriver",
        profile = "xenon")
    })
}

#' Make Login Opal
#'
#' This function adds Opal login information to the `DSLoginBuilder`.
#'
#' @param builder An object of class `DSLoginBuilder`.
#' @param opal_credentials A data frame of Opal login credentials.
#' @return None.
#' @noRd
.make_login_opal <- function(builder, opal_credentials){
  opal_credentials %>%
    pmap(function(cohort, url, server, username, password){
      builder$append(
        url = url,
        server = cohort,
        user = username,
        password = password,
        driver = "OpalDriver",
        profile = "xenon")
    })
}

#' Split Login Object by Server
#'
#' This function builds the login object using the provided builder and then splits it by the `server` attribute.
#'
#' @param builder An object used to build the login data.
#' @importFrom dplyr group_by group_split
#' @return A list of tibbles, each containing login data for a specific server.
#' @noRd
.split_login_object <- function(builder){
  server <- NULL
  split <- builder$build() %>%
    group_by(server) %>%
    group_split()
  return(split)
}


#' Handle Login
#'
#' This function handles the login process safely using `datashield.login`.
#'
#' @param login_data A list of login data objects.
#' @importFrom DSI datashield.login
#' @importFrom rlang set_names
#' @return A list containing the login results.
#' @noRd
.handle_login <- function(login_data){
  login_safely <- safely(datashield.login)
  suppressMessages(
    suppressWarnings(
      login_result <- login_data %>%
        map(~login_safely(.x, assign = F)) %>%
        set_names(login_data %>% map("server"))
    )
  )
  return(login_result)
}

#' Get Login Success
#'
#' This function extracts successful login results.
#'
#' @param login_result A list containing the login results.
#' @importFrom purrr compact
#' @return A list of successful login results.
#' @noRd
.get_login_success <- function(login_result){
  success <- login_result %>%
    map("result") %>%
    compact() %>%
    map(~.x[[1]])
  return(success)
}

#' Get Login Failure
#'
#' This function extracts and formats login failure results.
#'
#' @param login_result A list containing the login results.
#' @return A data frame of formatted login failures.
#' @noRd
.get_login_failure <- function(login_result){
  login_errors <- .get_login_errors(login_result)
  formatted <- .format_login_errors(login_errors)
  return(formatted)
}

#' Get Login Errors
#'
#' This function extracts errors from the token retrieval results.
#'
#' @param tokens_safely A list of safely executed token retrieval results.
#' @importFrom purrr compact
#' @return A list of login errors.
#' @noRd
.get_login_errors <- function(tokens_safely){
  errors <- tokens_safely %>%
    map("error") %>%
    compact()
  return(errors)
}

#' Format Login Errors
#'
#' This function formats login errors into a tidy format for reporting.
#'
#' @param login_errors A list containing the login errors.
#' @importFrom tibble as_tibble
#' @return A data frame of formatted login errors.
#' @noRd
.format_login_errors <- function(login_errors){
  formatted <- login_errors %>%
    map(errorCondition) %>%
    map("message") %>%
    map(as_tibble) %>%
    bind_rows(.id = "cohort")
  return(formatted)
}

#' Build Login Summary
#'
#' This function constructs a summary of the login process indicating success or failure for each cohort.
#'
#' @param credentials A data frame containing the login credentials.
#' @param login_failure A data frame containing the login failures.
#' @return A data frame summarizing the login process.
#' @noRd
.build_login_summary <- function(credentials, login_failure){
  value <- NULL

  if(nrow(login_failure) > 0){
    login_summary <- left_join(credentials, login_failure, by = "cohort") %>%
      mutate(success = ifelse(is.na(value), TRUE, FALSE))
  } else {
    login_summary <- credentials %>% mutate(success = TRUE)

  }
  return(login_summary)
}
