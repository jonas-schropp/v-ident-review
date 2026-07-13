#' Export authentication algorithm summary table
#'
#' @param auth_path string(1) Path to `auth.rds`.
#' @param output_dir string(1) Directory where CSV table is written.
#'
#' @returns A tibble invisibly. The same table is written to
#'   `authentication_algorithms.csv` in `output_dir`.
#'
#' @details
#' The authentication data are summarized by the coded algorithm fields created
#' by `code_algorithm()` in `code/data-cleaning/code-auth.R`. Counts are
#' reported at both the study level (`studies`, distinct study IDs) and the
#' experiment/extraction-row level (`experiments`). Study IDs are retained for
#' auditability. Modality is collapsed to ECG, PPG, or Other to align with the
#' review tables that focus on broad vital-sign modality groups.
#'
#' Algorithm rows are ordered and visually grouped by broad family: deep
#' learning, tree-based, kernel-based, linear, distance-based, and
#' probability-based.
#'
#' @examples
#' \dontrun{
#' tables_authentication_algorithms()
#' }
tables_authentication_algorithms <- function(
    auth_path = here::here("data", "auth.rds"),
    output_dir = here::here("results", "tables")
) {
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("readr", quietly = TRUE)
  requireNamespace("stringr", quietly = TRUE)
  requireNamespace("tibble", quietly = TRUE)

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  auth <- readr::read_rds(auth_path) |>
    dplyr::mutate(id = as.character(.data$id))

  if (!all(c("algorithm", "algorithm_class") %in% names(auth))) {
    if (!exists("code_algorithm", mode = "function")) {
      source(here::here("code", "data-cleaning", "code-auth.R"))
    }
    auth <- code_algorithm(auth)
  }

  class_levels <- c("dl", "dl_temp", "tree", "kernel", "lm", "dist", "prob")
  class_labels <- c(
    dl = "Deep learning",
    dl_temp = "Temporal deep learning",
    tree = "Tree-based methods",
    kernel = "Kernel-based methods",
    lm = "Linear models",
    dist = "Distance-based methods",
    prob = "Probability-based methods"
  )

  algorithm_summary <- auth |>
    dplyr::filter(!is.na(.data$algorithm), !is.na(.data$algorithm_class)) |>
    dplyr::mutate(
      modality_group = dplyr::case_when(
        stringr::str_detect(.data$modality, stringr::regex("ECG", ignore_case = TRUE)) ~ "ECG",
        stringr::str_detect(.data$modality, stringr::regex("PPG", ignore_case = TRUE)) ~ "PPG",
        TRUE ~ "Other"
      ),
      algorithm_class = factor(
        .data$algorithm_class,
        levels = class_levels,
        labels = unname(class_labels[class_levels])
      )
    ) |>
    dplyr::group_by(.data$algorithm_class, .data$algorithm, .data$modality_group) |>
    dplyr::summarise(
      studies = dplyr::n_distinct(.data$id),
      experiments = dplyr::n(),
      study_ids = paste(sort(unique(.data$id)), collapse = "; "),
      .groups = "drop"
    ) |>
    dplyr::arrange(
      .data$algorithm_class,
      factor(.data$modality_group, levels = c("ECG", "PPG", "Other")),
      .data$algorithm
    ) |>
    dplyr::mutate(algorithm_class = as.character(.data$algorithm_class)) |>
    dplyr::rename(
      `Algorithm class` = "algorithm_class",
      Algorithm = "algorithm",
      Modality = "modality_group",
      Studies = "studies",
      Experiments = "experiments",
      `Study IDs` = "study_ids"
    )

  attr(algorithm_summary, "flextable_group_cols") <- "Algorithm class"
  attr(algorithm_summary, "flextable_header_labels") <- c(
    `Algorithm class` = "Algorithm class",
    Algorithm = "Algorithm",
    Modality = "Modality",
    Studies = "Studies",
    Experiments = "Experiments",
    `Study IDs` = "Study IDs"
  )

  algorithm_summary2 <- auth |>
    dplyr::filter(!is.na(.data$algorithm), !is.na(.data$algorithm_class)) |>
    dplyr::mutate(
      algorithm_class = factor(
        .data$algorithm_class,
        levels = class_levels,
        labels = unname(class_labels[class_levels])
      )
    ) |>
    dplyr::group_by(.data$algorithm_class, .data$algorithm) |>
    dplyr::summarise(
      Studies = dplyr::n_distinct(.data$id),
      Experiments = dplyr::n(),
      `Study IDs` = paste(sort(unique(.data$id)), collapse = "; "),
      .groups = "drop"
    ) |>
    dplyr::rename(
      `Algorithm class` = "algorithm_class",
      Algorithm = "algorithm"
    ) |>
    dplyr::arrange(
      factor(.data$`Algorithm class`, levels = unname(class_labels[class_levels])),
      .data$Algorithm
    )

  attr(algorithm_summary2, "flextable_group_cols") <- "Algorithm class"

  write_table_csv_docx(algorithm_summary, file.path(output_dir, "authentication_algorithms.csv"))
  write_table_csv_docx(algorithm_summary2, file.path(output_dir, "authentication_algorithms2.csv"))

  invisible(list(
    authentication_algorithms = algorithm_summary,
    authentication_algorithms2 = algorithm_summary2
  ))
}
