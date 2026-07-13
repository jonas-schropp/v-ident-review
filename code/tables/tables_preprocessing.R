#' Export preprocessing extraction summary
#'
#' @param preproc_path string(1) Path to `preproc.rds`.
#' @param output_dir string(1) Directory where `summary_preprocessing.csv` is written.
#'
#' @returns A tibble invisibly. The same table is written to
#'   `summary_preprocessing.csv` in `output_dir`.
#'
#' @details
#' This function intentionally exports only one machine-readable CSV with the
#' extracted preprocessing fields needed for audit and downstream manuscript
#' summaries: study id, experiment, modality, normalization, segmentation, and
#' noise removal.
get_preprocessing_info <- function(
    preproc_path = here::here("data", "preproc.rds"),
    output_dir = here::here("data")
) {
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("readr", quietly = TRUE)
  requireNamespace("stringr", quietly = TRUE)

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  normalize_missing <- function(x) {
    x <- stringr::str_squish(as.character(x))
    x <- dplyr::na_if(x, "")
    x <- dplyr::na_if(x, "NA")
    dplyr::coalesce(x, "not reported")
  }

  summary_preprocessing <- readr::read_rds(preproc_path) |>
    dplyr::transmute(
      `Study ID` = as.character(.data$id),
      Experiment = .data$experience,
      Modality = .data$modality,
      Normalization = normalize_missing(.data$normalization),
      Segmentation = normalize_missing(.data$segmentation),
      `Noise removal` = normalize_missing(.data$`noise reduction`)
    ) |>
    dplyr::arrange(.data$`Study ID`, .data$Experiment, .data$Modality)

  readr::write_csv(summary_preprocessing, file.path(output_dir, "summary_preprocessing.csv"))

  invisible(summary_preprocessing)
}
