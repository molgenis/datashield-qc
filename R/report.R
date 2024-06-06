#' Summarize Variables
#'
#' This function summarizes the variables available in the dictionaries by counting the total variables and
#' getting the maximum sample size for each cohort.
#'
#' @param qc_stats output from qc stats
#' @importFrom dplyr case_when
#' @return A data frame summarizing the total number of variables and the maximum sample size for each cohort.
#' @export
sumarise_total_vars_and_obs <- function(variables) {
  collapsed <- .collapse_vars(variables)
  n_vars <- .count_vars(collapsed)
  max_obs <- .get_max_obs(variables)
  combined <- left_join(n_vars, max_obs, by = "cohort")
  transposed <- .flip_rows_columns(combined, c("total_vars", "max_obs"))
  out <- .rename_vars(transposed)
  return(out)
}

summarise_obs_per_variable <- function(variables){
  obs <- variables %>%
    map_depth(2, ~dplyr::select(., variable, cohort, valid_n, any_of("category"))) %>%
    map(bind_rows) %>%
    bind_rows(.id = "dictionary") %>%
    dplyr::filter(!is.na(category)) %>%
    dplyr::select(-category) %>%
    dplyr::filter(cohort != "combined") %>%
    distinct() %>%
    mutate(valid_n = as.character(valid_n)) %>%
    pivot_wider(
      names_from = cohort,
      values_from = valid_n)
  return(obs)
}

summarise_values_and_class <- function(variables){
  values_class <- .loop_table_one(variables) %>%
    .format_values_class()
  return(values_class)
}

summarise_demographics <- function(variables, demographics){
  var_labels <- .make_var_ref()
  cat_labels <- .make_cat_ref()
  non_rep_vars <- .filter_non_rep(variables)
  vars_joined <- .join_demographics(non_rep_vars, demographics)
  out <- .make_demographic_table(vars_joined, var_labels, cat_labels)
  return(out)
}

.join_demographics <- function(non_rep_vars, demographics){
  vars_joined <- list(non_rep_vars, demographics) %>%
    pmap(bind_rows)
  return(vars_joined)
}


.filter_non_rep <- function(variables){
  non_rep_vars <- variables$core_non %>%
    map(~dplyr::filter(., variable %in% c("agebirth_m_y", "ethn3_m", "sex")))
  return(non_rep_vars)
}

.make_demographic_table <- function(vars_joined, var_labels, cat_labels){
  out <- dh.createTableOne(
    stats = vars_joined,
    type = "cohort",
    vars = c("agebirth_m_y", "ethn3_m", "sex", "min_age", "max_age", "edu_m_"),
    inc_missing = F,
    perc_denom = "total",
    var_labs = var_labels,
    cat_labs = cat_labels,
    cont_format = "mean_sd")
  return(out)
}

.make_var_ref <- function(){
  label_ref <- tibble(
    variable = c("agebirth_m_y", "ethn3_m", "sex", "min_age", "max_age", "edu_m_"),
    var_label = c("Maternal age", "Maternal ethnicity", "Child sex",
                  "Youngest child age", "Oldest child age", "Maternal education")
  )
  return(label_ref)
}

.make_cat_ref <- function(){
  cat_ref <- tibble(
    variable = c(
      rep("ethn3_m", 3), rep("sex", 2), rep("edu_m_", 3)),
    category = c("1", "2", "3", "1", "2", "1", "2", "3"),
    cat_label = c("White Western", "Mixed", "Other", "Male", "Female", "High", "Medium", "Low"))
  return(cat_ref)
}

.collapse_vars <- function(variables){
  collapsed <- variables %>%
    pmap(bind_rows)
  return(collapsed)
}

#' Count Variables per Cohort
#'
#' This function counts the number of distinct variables for each cohort in the provided QC statistics data.
#'
#' @param qc_stats A list containing QC statistics data. It is expected that `qc_stats` has a component named `variables`
#'   which is a list of data frames. Each data frame should contain at least the columns `cohort` and `variable`.
#'
#' @return A tibble with two columns: `cohort` and `total_vars`. Each row represents a cohort and the number of distinct
#'   variables associated with that cohort.
#'
#' @importFrom dplyr %>% n
#' @importFrom purrr pmap
#' @noRd
.count_vars <- function(collapsed) {
  cohort <- NULL
  n_vars <- collapsed %>%
    purrr::map(~dplyr::select(., cohort, variable)) %>%
    dplyr::bind_rows() %>%
    dplyr::filter(cohort != "combined") %>%
    dplyr::distinct() %>%
    dplyr::group_by(cohort) %>%
    dplyr::summarise(total_vars = n())

  return(n_vars)
}

#' Get Maximum Sample Size
#'
#' This function retrieves the maximum sample size for each cohort from a non-repeated measures data frame.
#'
#' @param non_rep_df The non-repeated measures data frame.
#' @param conns A datashield connections object
#' @return A data frame with the maximum sample size for each cohort.
#' @importFrom dsBaseClient ds.dim
#' @importFrom dplyr filter
#' @noRd
.get_max_obs <- function(variables) {
  cohort <- valid_n <- max_obs <- NULL
  max_sample <- variables$core_non %>%
    map(~dplyr::select(., cohort, valid_n)) %>%
    bind_rows() %>%
    dplyr::filter(cohort != "combined") %>%
    group_by(cohort) %>%
    arrange(desc(valid_n)) %>%
    slice(1) %>%
    dplyr::rename("max_obs" = "valid_n") %>%
    dplyr::select(cohort, max_obs)
  return(max_sample)
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
#' @return A data frame with the `measure` column values renamed:
#'   "total_vars" is renamed to "Number of variables" and "max_obs" is renamed to "Maximum number of participants".
#' @importFrom dplyr case_when
#' @noRd
.rename_vars <- function(transposed) {
  out <- transposed %>%
    dplyr::mutate(measure = dplyr::case_when(
      measure == "total_vars" ~ "Number of variables",
      measure == "max_obs" ~ "Maximum number of participants"
    ))

  return(out)
}

.format_values_class <- function(looped_vars){
  formatted <- looped_vars %>%
    bind_rows(.id = "dictionary") %>%
    mutate(type = ifelse(category == "Mean \u00B1 SD", "Continuous", "Categorical")) %>%
  dplyr::select(dictionary, variable, type, category, everything())
  return(formatted)
}

.loop_table_one <- function(variables){
  variables %>%
    map(
      ~dh.createTableOne(
        stats = .x,
        vars = c(.x$categorical$variable, .x$continuous$variable) %>% unique(),
        type = "cohort",
        cont_format = "mean_sd",
        inc_missing = F,
        perc_denom = "valid")
    )
}

.make_collapse_vector <- function(data){
  groups <- rle(data$dictionary)$lengths
  names(groups) <- unique(data$dictionary)
  return(groups)
}

