#' Export study characteristics table
#'
#' @param general_path string(1) Path to `general.rds`.
#' @param extraction_path string(1) Path to an experiment-level extraction `.rds`
#'   file, such as `signal.rds`, or an already joined extraction data frame.
#' @param output_path string(1) CSV path for the exported study characteristics
#'   table.
#'
#' @returns A tibble invisibly. The same table is written to `output_path` as a
#'   CSV file.
#'
#' @details
#' The table keeps one row per study and combines bibliographic fields from
#' `general.rds` with counts from an experiment-level extraction object. The
#' extraction row count is computed per study ID, and the optional modality list
#' is included when the extraction object contains a `modality` column.
tables_study_characteristics <- function(
    general_path = here::here("data", "general.rds"),
    extraction_path = here::here("data", "signal.rds"),
    output_path = here::here("results", "tables", "study_characteristics.csv")
) {
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("readr", quietly = TRUE)

  output_dir <- dirname(output_path)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  general <- readr::read_rds(general_path) |>
    dplyr::mutate(id = as.character(.data$id))

  extraction <- if (is.character(extraction_path)) {
    readr::read_rds(extraction_path)
  } else {
    extraction_path
  }

  required_general_cols <- c("id", "authors", "year")
  missing_general_cols <- setdiff(required_general_cols, names(general))
  if (length(missing_general_cols) > 0) {
    stop(
      "general data is missing required columns: ",
      paste(missing_general_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (!"id" %in% names(extraction)) {
    stop("extraction data is missing required column: id", call. = FALSE)
  }

  extraction <- extraction |>
    dplyr::mutate(id = as.character(.data$id))

  collapse_values <- function(x) {
    x <- unique(stats::na.omit(as.character(x)))
    x <- x[x != ""]

    if (length(x) == 0) {
      NA_character_
    } else {
      paste(sort(x), collapse = "; ")
    }
  }

  study_info <- general |>
    dplyr::group_by(.data$id) |>
    dplyr::summarise(
      authors = collapse_values(.data$authors),
      year = collapse_values(.data$year),
      .groups = "drop"
    )

  extraction_counts <- extraction |>
    dplyr::group_by(.data$id) |>
    dplyr::summarise(
      experiments = dplyr::n(),
      .groups = "drop"
    )

  if ("modality" %in% names(extraction)) {
    modality_counts <- extraction |>
      dplyr::mutate(modality = as.character(.data$modality)) |>
      dplyr::filter(!is.na(.data$modality), .data$modality != "") |>
      dplyr::group_by(.data$id) |>
      dplyr::summarise(
        modalities = collapse_values(.data$modality),
        .groups = "drop"
      )

    study_characteristics <- study_info |>
      dplyr::left_join(extraction_counts, by = "id") |>
      dplyr::left_join(modality_counts, by = "id") |>
      dplyr::mutate(
        experiments = dplyr::coalesce(.data$experiments, 0L),
        modalities = dplyr::coalesce(.data$modalities, "not reported"),
        year_sort = suppressWarnings(as.numeric(.data$year))
      ) |>
      dplyr::arrange(.data$year_sort, suppressWarnings(as.numeric(.data$id)), .data$id) |>
      dplyr::select(-"year_sort")
  } else {
    study_characteristics <- study_info |>
      dplyr::left_join(extraction_counts, by = "id") |>
      dplyr::mutate(
        experiments = dplyr::coalesce(.data$experiments, 0L),
        year_sort = suppressWarnings(as.numeric(.data$year))
      ) |>
      dplyr::arrange(.data$year_sort, suppressWarnings(as.numeric(.data$id)), .data$id) |>
      dplyr::select(-"year_sort")
  }

  write_table_csv_docx(study_characteristics, output_path)

  invisible(study_characteristics)
}
