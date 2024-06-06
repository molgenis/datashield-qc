#' Generate a Quality Control Report
#'
#' This function generates a quality control (QC) report from the provided QC statistics. It renders an RMarkdown document using the `qc-summary.Rmd` template and saves the report to the specified output directory.
#'
#' @param qc_stats A list containing QC statistics.
#' @param sections Character, specifying which sections to include in the report. Options are "technical", "high-level" and "detailed"
#' @param output_dir A character string specifying the directory where the report will be saved. Defaults to the current working directory.
#' @importFrom rmarkdown render
#' @return The path to the generated report.
#' @export
make_qc_report <- function(qc_stats, sections, output_dir = NULL) {

  if (is.null(output_dir)) {
    output_dir <- getwd()
  }

  technical <- ifelse("technical" %in% sections, TRUE, FALSE)
  high <- ifelse("high-level" %in% sections, TRUE, FALSE)
  detailed <- ifelse("detailed" %in% sections, TRUE, FALSE)

  rmarkdown::render(
    "inst/templates/qc-summary.Rmd",
    params = list(
      qc_stats = qc_stats,
      technical = technical,
      high = high,
      detailed = detailed),
    output_dir = output_dir)
}
