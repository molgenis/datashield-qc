#' Format Dictionary Information
#'
#' This function formats dictionary information by combining cohort-specific dictionaries with a master dictionary,
#' indicating the availability of each variable for each cohort, and restructuring the data for better readability.
#'
#' @param coh_dics A data frame containing cohort-specific dictionary information.
#' @param all_dics A data frame containing the master dictionary with all possible variables.
#' @return A data frame with formatted dictionary information, showing the availability of variables for each cohort.
#' @importFrom purrr map_chr imap
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr mutate group_by group_split left_join bind_rows select
#' @importFrom stringr str_remove_all
#' @noRd
format_dictionary_info <- function(coh_dics, all_dics) {
  cohort <- . <- value <- long_name <- dictionary <- available <- NULL
  formatted <- coh_dics %>%
    group_by(cohort) %>%
    group_split() %>%
    set_names(map_chr(., ~ .x$cohort[1])) %>%
    map(~ left_join(all_dics, .x, by = c("type", "name", "long_name"))) %>%
    imap(~ .x %>% mutate(cohort = .y)) %>%
    bind_rows() %>%
    mutate(available = ifelse(is.na(value), "no", "yes")) %>%
    mutate(dictionary = str_remove_all(long_name, "\\d+_\\d+_")) %>%
    select(cohort, dictionary, available) %>%
    pivot_wider(
      names_from = cohort,
      values_from = available
    )

  return(formatted)
}

#' Summarize Variables
#'
#' This function summarizes the variables available in the dictionaries by counting the total variables and
#' getting the maximum sample size for each cohort.
#'
#' @param variables A list of data frames containing variable information.
#' @return A data frame summarizing the total number of variables and the maximum sample size for each cohort.
#' @importFrom dplyr left_join across
#' @export
summarise_total_vars_and_obs <- function(variables) {
  total_vars <- NULL
  collapsed <- .collapse_vars(variables)
  n_vars <- .count_vars(collapsed)
  max_obs <- .get_max_obs(variables)
  combined <- left_join(n_vars, max_obs, by = "cohort") %>%
    mutate(across(c(max_obs, total_vars), ~as.character(.x)))
  transposed <- .flip_rows_columns(combined, c("total_vars", "max_obs"))
  out <- .rename_vars(transposed)
  return(out)
}

#' Summarize Observations per Variable
#'
#' This function summarizes the observations per variable for each cohort.
#'
#' @param variables A list of data frames containing variable information.
#' @return A data frame summarizing the observations per variable for each cohort.
#' @importFrom dplyr select filter distinct mutate bind_rows
#' @importFrom purrr map map_depth
#' @importFrom tidyr pivot_wider
#' @export
summarise_obs_per_variable <- function(variables) {
  category <- cohort <- valid_n <- NULL
  obs <- variables %>%
    map_depth(2, ~ dplyr::select(., variable, cohort, valid_n, any_of("category"))) %>%
    map(bind_rows) %>%
    bind_rows(.id = "dictionary") %>%
    dplyr::filter(!is.na(category)) %>%
    dplyr::select(-category) %>%
    dplyr::filter(cohort != "combined") %>%
    distinct() %>%
    mutate(valid_n = as.character(valid_n)) %>%
    pivot_wider(
      names_from = cohort,
      values_from = valid_n
    )
  return(obs)
}

#' Summarize Values and Class
#'
#' This function summarizes the values and class of the variables in the dictionaries.
#'
#' @param variables A list of data frames containing variable information.
#' @return A data frame summarizing the values and class of the variables.
#' @export
summarise_values_and_class <- function(variables) {
  values_class <- .loop_table_one(variables) %>%
    .format_values_class()
  return(values_class)
}

#' Summarize Demographics
#'
#' This function summarizes demographic information based on the provided variables and demographics data.
#'
#' @param variables A list of data frames containing variable information.
#' @param demographics A data frame containing demographic information.
#' @return A data frame summarizing demographic information.
#' @export
summarise_demographics <- function(variables, demographics) {
  var_labels <- .make_var_ref()
  cat_labels <- .make_cat_ref()
  non_rep_vars <- .filter_non_rep(variables)
  vars_joined <- .join_demographics(non_rep_vars, demographics)
  out <- .make_demographic_table(vars_joined, var_labels, cat_labels)
  return(out)
}

#' Join Demographics
#'
#' This function joins non-repeated variables with demographics data.
#'
#' @param non_rep_vars A list of non-repeated variable data frames.
#' @param demographics A data frame containing demographic information.
#' @return A list of joined data frames.
#' @importFrom purrr pmap
#' @noRd
.join_demographics <- function(non_rep_vars, demographics) {
  vars_joined <- list(non_rep_vars, demographics) %>%
    pmap(bind_rows)
  return(vars_joined)
}

#' Filter Non-Repeated Variables
#'
#' This function filters non-repeated variables from the provided list of variables.
#'
#' @param variables A list of data frames containing variable information.
#' @return A list of filtered non-repeated variable data frames.
#' @importFrom dplyr filter
#' @noRd
.filter_non_rep <- function(variables) {
  non_rep_vars <- variables$core_non %>%
    map(~ dplyr::filter(., variable %in% c("agebirth_m_y", "ethn3_m", "sex")))
  return(non_rep_vars)
}

#' Make Demographic Table
#'
#' This function creates a demographic table from the joined variable and demographic data.
#'
#' @param vars_joined A list of joined variable and demographic data frames.
#' @param var_labels A data frame containing variable labels.
#' @param cat_labels A data frame containing category labels.
#' @return A data frame summarizing demographic information.
#' @importFrom dsHelper dh.createTableOne
#' @noRd
.make_demographic_table <- function(vars_joined, var_labels, cat_labels) {
  category <- NULL
  out <- dh.createTableOne(
    stats = vars_joined,
    type = "cohort",
    vars = c("agebirth_m_y", "ethn3_m", "sex", "min_age", "max_age", "edu_m_"),
    inc_missing = FALSE,
    perc_denom = "total",
    var_labs = var_labels,
    cat_labs = cat_labels,
    cont_format = "mean_sd"
  ) %>%
    dplyr::rename(Category = category)
  return(out)
}

#' Make Variable Reference
#'
#' This function creates a reference table for variable labels.
#'
#' @return A tibble containing variable labels.
#' @importFrom tibble tibble
#' @noRd
.make_var_ref <- function() {
  label_ref <- tibble(
    variable = c("agebirth_m_y", "ethn3_m", "sex", "min_age", "max_age", "edu_m_"),
    var_label = c("Maternal age", "Maternal ethnicity", "Child sex",
                  "Youngest child age", "Oldest child age", "Maternal education")
  )
  return(label_ref)
}

#' Make Category Reference
#'
#' This function creates a reference table for category labels.
#'
#' @return A tibble containing category labels.
#' @importFrom tibble tibble
#' @noRd
.make_cat_ref <- function() {
  cat_ref <- tibble(
    variable = c(
      rep("ethn3_m", 3), rep("sex", 2), rep("edu_m_", 3)),
    category = c("1", "2", "3", "1", "2", "1", "2", "3"),
    cat_label = c("White Western", "Mixed", "Other", "Male", "Female", "High", "Medium", "Low")
  )
  return(cat_ref)
}

#' Collapse Variables
#'
#' This function collapses the variables into a single data frame.
#'
#' @param variables A list of data frames containing variable information.
#' @return A single data frame with collapsed variables.
#' @importFrom purrr pmap
#' @noRd
.collapse_vars <- function(variables) {
  collapsed <- variables %>%
    pmap(bind_rows)
  return(collapsed)
}

#' Count Variables per Cohort
#'
#' This function counts the number of distinct variables for each cohort in the provided data.
#'
#' @param collapsed A data frame containing collapsed variable information.
#' @return A tibble with the number of distinct variables per cohort.
#' @importFrom dplyr select distinct group_by summarise
#' @importFrom purrr map
#' @noRd
.count_vars <- function(collapsed) {
  cohort <- NULL
  n_vars <- collapsed %>%
    purrr::map(~ dplyr::select(., cohort, variable)) %>%
    dplyr::bind_rows() %>%
    dplyr::filter(cohort != "combined") %>%
    dplyr::distinct() %>%
    dplyr::group_by(cohort) %>%
    dplyr::summarise(total_vars = n())
  return(n_vars)
}

#' Get Maximum Sample Size
#'
#' This function retrieves the maximum sample size for each cohort from the provided data.
#'
#' @param variables A list of data frames containing variable information.
#' @return A data frame with the maximum sample size for each cohort.
#' @importFrom dplyr select filter group_by arrange slice rename
#' @importFrom purrr map
#' @noRd
.get_max_obs <- function(variables) {
  cohort <- valid_n <- max_obs <- desc <- NULL
  max_sample <- variables$core_non %>%
    map(~ dplyr::select(., cohort, valid_n)) %>%
    bind_rows() %>%
    dplyr::filter(cohort != "combined") %>%
    group_by(cohort) %>%
    arrange(desc(valid_n)) %>%
    slice(1) %>%
    rename("max_obs" = "valid_n") %>%
    dplyr::select(cohort, max_obs)
  return(max_sample)
}

#' Flip Rows and Columns in a Summary Table
#'
#' This function flips the rows and columns in a summary table, effectively swapping the rows and columns for certain measures.
#'
#' @param var_summary A data frame containing the summary of variables.
#' @param non_cohort_vars A vector of non-cohort variable names.
#' @return A data frame with flipped rows and columns.
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom tidyselect all_of
#' @noRd
.flip_rows_columns <- function(var_summary, non_cohort_vars) {
  total_vars <- max_obs <- cohort <- value <- NULL
  flipped <- var_summary %>%
    pivot_longer(
      cols = all_of(non_cohort_vars),
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
#' @return A data frame with the `measure` column values renamed.
#' @importFrom dplyr mutate case_when
#' @noRd
.rename_vars <- function(transposed) {
  out <- transposed %>%
    dplyr::mutate(measure = dplyr::case_when(
      measure == "total_vars" ~ "Number of variables",
      measure == "max_obs" ~ "Maximum number of participants"
    ))
  return(out)
}

#' Format Values and Class
#'
#' This function formats the values and class information for the variables.
#'
#' @param looped_vars A list of data frames containing variable information.
#' @return A data frame with formatted values and class information.
#' @importFrom dplyr select mutate bind_rows
#' @importFrom tidyselect everything
#' @noRd
.format_values_class <- function(looped_vars) {
  category <- dictionary <- variable <- type <- NULL
  formatted <- looped_vars %>%
    bind_rows(.id = "dictionary") %>%
    mutate(type = ifelse(category == "Mean \u00B1 SD", "Continuous", "Categorical")) %>%
    dplyr::select(dictionary, variable, Type = type, Category = category, everything())
  return(formatted)
}

#' Loop through Table One
#'
#' This function creates a summary table for each variable in the provided data.
#'
#' @param variables A list of data frames containing variable information.
#' @return A list of summary tables.
#' @importFrom dsHelper dh.createTableOne
#' @noRd
.loop_table_one <- function(variables) {
  variables %>%
    map(
      ~ dh.createTableOne(
        stats = .x,
        vars = c(.x$categorical$variable, .x$continuous$variable) %>% unique(),
        type = "cohort",
        cont_format = "mean_sd",
        inc_missing = FALSE,
        perc_denom = "valid"
      )
    )
}

#' Make Collapse Vector
#'
#' This function creates a vector for collapsing data by dictionary.
#'
#' @param data A data frame containing dictionary information.
#' @return A named vector for collapsing data.
#' @noRd
.make_collapse_vector <- function(data) {
  groups <- rle(data$dictionary)$lengths
  names(groups) <- unique(data$dictionary)
  return(groups)
}

#' Create Server Table
#'
#' This function processes the `accessible` data frame to create a table that
#' shows the success and error messages for different cohorts.
#'
#' @param accessible A data frame with columns `success`, `message`, and `cohort`.
#' @return A data frame with columns `type` and the cohorts, where `type` indicates
#'         whether it is a success or error message.
#' @importFrom dplyr mutate
#' @importFrom tidyr pivot_longer pivot_wider
#' @export
.make_server_table <- function(accessible){
  success <- cohort <- value <- type <- NULL
  server <- accessible %>%
    pivot_longer(
      cols = c(success, message),
      names_to = "type",
      values_to = "value") %>%
    pivot_wider(
      names_from = cohort,
      values_from = value) %>%
    mutate(type = ifelse(type == "success", "Successfully logged in", "Error message"))
  return(server)
}

#' Create Package Table
#'
#' This function converts the `version_status` data frame into a tibble with
#' the row names as a new column named `Package`.
#'
#' @param version_status A data frame where the row names represent package names.
#' @return A tibble with a new column `Package` and the original columns of the input data frame.
#' @importFrom tibble as_tibble
#' @export
.make_package_table <- function(version_status){
  package <- version_status %>%
    as_tibble(rownames = "Package")
  return(package)
}

#' Create Table Headers
#'
#' This function generates a vector of table headers based on the column names
#' of the input table.
#'
#' @param table A data frame or tibble.
#' @return A character vector with the headers for the table, where the first
#'         element is an empty string.
#' @export
.make_table_headers <- function(table){
  headers <- c("", colnames(table)[-1])
  return(headers)
}

