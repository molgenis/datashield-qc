make_qc_report <- function(qc_stats, output_dir = NULL){

  if(is.null(output_dir)){
    output_dir <- getwd()
  }

  rmarkdown::render("R/qc-summary.Rmd", params = list(qc_stats = test), output_dir = output_dir)
}
