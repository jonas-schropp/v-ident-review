#' Export modality-by-year summary tables
#'
#' @param general_path string(1) Path to `general.rds`.
#' @param signal_path string(1) Path to `signal.rds`.
#' @param output_dir string(1) Directory where CSV tables are written.
#'
#' @returns A named list of tibbles invisibly. The same tables are written to
#'   `output_dir` as CSV files.
#'
#' @details
#' The current data-cleaning pipeline stores one `.rds` file per extraction
#' sheet, so this function reads `general.rds` and `signal.rds` rather than the
#' older workbook sheets. The exported tables use the modality dummy variables
#' (`ECG`, `PPG`, `EDA`, `SCG`, `VHF`, and `microphone`) rather than the coarse
#' `modality` label because one experiment can use several sensors at once.
#' `modality_by_year_studies.csv` and `modality_by_year_experiments.csv`
#' each have one modality row, one column per publication year, and a final
#' `study_ids` audit column.
#'
#' A second compact overview table summarizes dummy-modality use across all
#' years. It also includes `study_ids`; for all-year modality totals this
#' remains a useful reference and is not too crowded for the current review
#' dataset.
tables_modality_over_time <- function(
    general_path = here::here("data", "general.rds"),
    signal_path = here::here("data", "signal.rds"),
    output_dir = here::here("results", "tables")
) {
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("readr", quietly = TRUE)
  requireNamespace("stringr", quietly = TRUE)
  requireNamespace("tidyr", quietly = TRUE)

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  general <- readr::read_rds(general_path) |>
    dplyr::mutate(id = as.character(.data$id)) |>
    dplyr::select("id", "year") |>
    dplyr::distinct()

  signal <- readr::read_rds(signal_path) |>
    dplyr::mutate(id = as.character(.data$id))

  dummy_modalities <- c("ECG", "PPG", "EDA", "SCG", "VHF", "microphone")
  modality_levels <- c(dummy_modalities, "multimodal")

  is_dummy_present <- function(x) {
    x_chr <- tolower(as.character(x))
    x_chr %in% c("1", "yes", "true", "y")
  }

  pdat <- signal |>
    dplyr::left_join(general, by = "id") |>
    dplyr::mutate(
      ECG = is_dummy_present(.data$ECG) | stringr::str_detect(.data$modality, stringr::regex("ECG", ignore_case = TRUE)),
      PPG = is_dummy_present(.data$PPG) | stringr::str_detect(.data$modality, stringr::regex("PPG", ignore_case = TRUE)),
      EDA = is_dummy_present(.data$EDA) | stringr::str_detect(.data$modality, stringr::regex("EDA|GSR", ignore_case = TRUE)),
      SCG = is_dummy_present(.data$SCG) | stringr::str_detect(.data$modality, stringr::regex("SCG", ignore_case = TRUE)),
      VHF = is_dummy_present(.data$VHF) | stringr::str_detect(.data$modality, stringr::regex("VHF", ignore_case = TRUE)),
      microphone = is_dummy_present(.data$microphone) | stringr::str_detect(.data$modality, stringr::regex("microphone|voice", ignore_case = TRUE))
    ) |>
    dplyr::mutate(multimodal = rowSums(dplyr::pick(dplyr::all_of(dummy_modalities)), na.rm = TRUE) > 1)

  modality_long <- pdat |>
    dplyr::select("id", "year", "experience", dplyr::all_of(modality_levels)) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(modality_levels),
      names_to = "modality",
      values_to = "present"
    ) |>
    dplyr::filter(.data$present, !is.na(.data$year))

  by_year_counts <- modality_long |>
    dplyr::group_by(.data$modality, .data$year) |>
    dplyr::summarise(
      experiments = dplyr::n(),
      studies = dplyr::n_distinct(.data$id),
      .groups = "drop"
    )

  make_by_year_table <- function(count_col) {
    by_year_counts |>
      dplyr::select("modality", "year", count = dplyr::all_of(count_col)) |>
      tidyr::complete(
        modality = modality_levels,
        year = sort(unique(general$year[!is.na(general$year)])),
        fill = list(count = 0)
      ) |>
      tidyr::pivot_wider(
        names_from = "year",
        values_from = "count",
        values_fill = 0
      )
  }

  modality_study_ids <- modality_long |>
    dplyr::group_by(.data$modality) |>
    dplyr::summarise(
      study_ids = paste(sort(unique(.data$id)), collapse = "; "),
      .groups = "drop"
    )

  by_year_studies <- make_by_year_table("studies") |>
    dplyr::left_join(modality_study_ids, by = "modality") |>
    dplyr::mutate(
      study_ids = dplyr::coalesce(.data$study_ids, ""),
      modality = factor(.data$modality, levels = modality_levels)
    ) |>
    dplyr::arrange(.data$modality) |>
    dplyr::mutate(modality = as.character(.data$modality))

  by_year_experiments <- make_by_year_table("experiments") |>
    dplyr::left_join(modality_study_ids, by = "modality") |>
    dplyr::mutate(
      study_ids = dplyr::coalesce(.data$study_ids, ""),
      modality = factor(.data$modality, levels = modality_levels)
    ) |>
    dplyr::arrange(.data$modality) |>
    dplyr::mutate(modality = as.character(.data$modality))

  overview <- modality_long |>
    dplyr::group_by(.data$modality) |>
    dplyr::summarise(
      experiments = dplyr::n(),
      studies = dplyr::n_distinct(.data$id),
      study_ids = paste(sort(unique(.data$id)), collapse = "; "),
      .groups = "drop"
    ) |>
    tidyr::complete(
      modality = modality_levels,
      fill = list(experiments = 0, studies = 0, study_ids = "")
    ) |>
    dplyr::arrange(factor(.data$modality, levels = modality_levels))

  write_table_csv_docx(by_year_studies, file.path(output_dir, "modality_by_year_studies.csv"))
  write_table_csv_docx(by_year_experiments, file.path(output_dir, "modality_by_year_experiments.csv"))
  write_table_csv_docx(overview, file.path(output_dir, "modality_overview.csv"))

  invisible(list(
    by_year_studies = by_year_studies,
    by_year_experiments = by_year_experiments,
    overview = overview
  ))
}
