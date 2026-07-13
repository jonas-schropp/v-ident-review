#' Export validation strategy and metric-reporting summary tables
#'
#' @param eval_path string(1) Path to `eval.rds`.
#' @param output_dir string(1) Directory where CSV tables are written.
#'
#' @returns A named list of tibbles invisibly. The same tables are written to
#'   `output_dir` as CSV files.
#'
#' @details
#' The evaluation extraction data contain one row per experiment. The exported
#' tables therefore include both experiment counts (`experiments`) and distinct
#' publication counts (`studies`), matching the manuscript's use of both units.
#' `validation_strategies.csv` summarizes coded internal-validation strategy
#' categories with normalized missing external-validation fields, while
#' `validation_strategies_supplement.csv` preserves the raw extracted internal
#' and external validation descriptions. `reported_metrics.csv` provides a
#' compact one-row-per-metric summary of reported and unreported study and
#' experiment counts, including percentages.
tables_validation_metrics <- function(
    eval_path = here::here("data", "eval.rds"),
    output_dir = here::here("results", "tables")
) {
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("readr", quietly = TRUE)
  requireNamespace("stringr", quietly = TRUE)
  requireNamespace("tibble", quietly = TRUE)
  requireNamespace("tidyr", quietly = TRUE)

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  eval <- readr::read_rds(eval_path) |>
    dplyr::mutate(
      id = as.character(.data$id),
      dplyr::across(dplyr::everything(), as.character)
    )

  is_reported <- function(x) {
    x <- stringr::str_squish(as.character(x))
    !is.na(x) &
      x != "" &
      !stringr::str_to_lower(x) %in% c("na", "n/a", "not reported", "not described", "?")
  }

  is_ambiguous_validation <- function(x) {
    x_clean <- stringr::str_to_lower(stringr::str_squish(as.character(x)))
    stringr::str_detect(
      x_clean,
      stringr::regex(
        paste(
          "unclear",
          "not clear",
          "not specified",
          "not described",
          "\\bmaybe\\b",
          sep = "|"
        ),
        ignore_case = TRUE
      )
    )
  }

  derive_unseen_intruders <- function(external) {
    external_clean <- stringr::str_to_lower(stringr::str_squish(as.character(external)))
    dplyr::case_when(
      !is_reported(external_clean) ~ "No",
      stringr::str_detect(
        external_clean,
        stringr::regex("\\bno\\b|no unseen|no intruders|not described", ignore_case = TRUE)
      ) ~ "No",
      stringr::str_detect(
        external_clean,
        stringr::regex("intruder|imposter|impostor|non-user|non-patient", ignore_case = TRUE)
      ) ~ "Yes",
      TRUE ~ "Unknown"
    )
  }

  derive_validation_category <- function(internal, external, unseen_intruders) {
    internal_clean <- stringr::str_to_lower(stringr::str_squish(as.character(internal)))
    external_clean <- stringr::str_to_lower(stringr::str_squish(as.character(external)))
    combined <- paste(internal_clean, external_clean)

    dplyr::case_when(
      !is_reported(internal_clean) & !is_reported(external_clean) ~ "Unclear or not reported",
      is_ambiguous_validation(internal_clean) | is_ambiguous_validation(external_clean) ~ "Unclear or not reported",
      stringr::str_detect(
        combined,
        stringr::regex("external dataset|independent dataset|another dataset|separate dataset", ignore_case = TRUE)
      ) ~ "External dataset validation",
      unseen_intruders == "Yes" |
        stringr::str_detect(
          internal_clean,
          stringr::regex("held[- ]?out|leave[- ]?one|lo[- ]?intruder", ignore_case = TRUE)
        ) ~ "Held-out users or unseen intruders",
      stringr::str_detect(
        internal_clean,
        stringr::regex("\\bday\\b|days|session|task", ignore_case = TRUE)
      ) ~ "Day-based or session-based split",
      stringr::str_detect(
        internal_clean,
        stringr::regex("time[- ]?based|across time|\\bt1\\b|\\bt2\\b|monte carlo", ignore_case = TRUE)
      ) ~ "Time-based split",
      stringr::str_detect(
        internal_clean,
        stringr::regex("cross[- ]?validation|\\bcv\\b|fold|loo", ignore_case = TRUE)
      ) ~ "Cross-validation",
      stringr::str_detect(
        internal_clean,
        stringr::regex("within|random split|[0-9]+/[0-9]+|train/test", ignore_case = TRUE)
      ) ~ "Within-subject / within-session split",
      TRUE ~ "Unclear or not reported"
    )
  }

  validation_strategies_supplement <- eval |>
    dplyr::transmute(
      id = .data$id,
      internal = .data$internal,
      external = .data$external
    ) |>
    dplyr::group_by(.data$internal, .data$external) |>
    dplyr::summarise(
      studies = dplyr::n_distinct(.data$id),
      experiments = dplyr::n(),
      study_ids = paste(sort(unique(.data$id)), collapse = "; "),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$internal, .data$external)

  validation_category_levels <- c(
    "Within-subject / within-session split",
    "Cross-validation",
    "Time-based split",
    "Day-based or session-based split",
    "Held-out users or unseen intruders",
    "External dataset validation",
    "Unclear or not reported"
  )

  validation_strategies <- eval |>
    dplyr::transmute(
      id = .data$id,
      internal = dplyr::if_else(is_reported(.data$internal), .data$internal, "not reported"),
      external = dplyr::if_else(is_reported(.data$external), .data$external, "not reported")
    ) |>
    dplyr::mutate(
      unseen_intruders = derive_unseen_intruders(.data$external),
      validation_category = derive_validation_category(
        .data$internal,
        .data$external,
        .data$unseen_intruders
      ),
      validation_category = factor(.data$validation_category, levels = validation_category_levels)
    ) |>
    dplyr::group_by(.data$validation_category) |>
    dplyr::summarise(
      studies = dplyr::n_distinct(.data$id),
      experiments = dplyr::n(),
      study_ids = paste(sort(unique(.data$id)), collapse = "; "),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$validation_category) |>
    dplyr::mutate(validation_category = as.character(.data$validation_category))

  metric_columns <- tibble::tribble(
    ~metric, ~column,
    "EER", "EER",
    "accuracy", "accuracy",
    "F1", "F1",
    "AUC", "AUC",
    "memory performance", "memory performance",
    "authentication time", "authentication time",
    "enrollment time", "Enroll time",
    "further metrics", "further metrics"
  )

  metric_report_flags <- eval |>
    dplyr::mutate(experiment_row = dplyr::row_number()) |>
    dplyr::select("experiment_row", "id", dplyr::all_of(metric_columns$column)) |>
    tidyr::pivot_longer(
      cols = -c("experiment_row", "id"),
      names_to = "column",
      values_to = "value"
    ) |>
    dplyr::left_join(metric_columns, by = "column") |>
    dplyr::mutate(reported = is_reported(.data$value))

  format_metric_summary <- function(data) {
    data |>
      dplyr::mutate(
        `Not reported studies` = .data$total_studies - .data$`Reported studies`,
        `Not reported experiments` = .data$total_experiments - .data$`Reported experiments`,
        `Reported studies (%)` = round(100 * .data$`Reported studies` / .data$total_studies, 1),
        `Reported experiments (%)` = round(100 * .data$`Reported experiments` / .data$total_experiments, 1)
      ) |>
      dplyr::select(
        "Metric",
        "Reported studies",
        "Not reported studies",
        "Reported studies (%)",
        "Reported experiments",
        "Not reported experiments",
        "Reported experiments (%)"
      )
  }

  reported_metrics_base <- metric_report_flags |>
    dplyr::group_by(.data$metric, .data$id) |>
    dplyr::summarise(
      study_reported = any(.data$reported),
      reported_experiments = sum(.data$reported),
      total_experiments = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::group_by(.data$metric) |>
    dplyr::summarise(
      total_studies = dplyr::n(),
      `Reported studies` = sum(.data$study_reported),
      total_experiments = sum(.data$total_experiments),
      `Reported experiments` = sum(.data$reported_experiments),
      .groups = "drop"
    ) |>
    dplyr::mutate(Metric = factor(.data$metric, levels = metric_columns$metric)) |>
    dplyr::arrange(.data$Metric) |>
    dplyr::mutate(Metric = as.character(.data$Metric)) |>
    format_metric_summary()

  total_studies <- dplyr::n_distinct(eval$id)
  total_experiments <- nrow(eval)

  calibration_row <- tibble::tibble(
    Metric = "calibration",
    total_studies = total_studies,
    `Reported studies` = 0L,
    total_experiments = total_experiments,
    `Reported experiments` = 0L
  ) |>
    format_metric_summary()

  performance_metric_columns <- c("EER", "accuracy", "F1", "AUC")

  multiple_metric_experiments <- metric_report_flags |>
    dplyr::filter(.data$metric %in% performance_metric_columns) |>
    dplyr::group_by(.data$experiment_row, .data$id) |>
    dplyr::summarise(experiment_reported = sum(.data$reported) > 1, .groups = "drop")

  multiple_metrics_row <- multiple_metric_experiments |>
    dplyr::group_by(.data$id) |>
    dplyr::summarise(study_reported = any(.data$experiment_reported), .groups = "drop") |>
    dplyr::summarise(
      Metric = "multiple metrics",
      total_studies = dplyr::n(),
      `Reported studies` = sum(.data$study_reported),
      total_experiments = nrow(multiple_metric_experiments),
      `Reported experiments` = sum(multiple_metric_experiments$experiment_reported),
      .groups = "drop"
    ) |>
    format_metric_summary()

  reported_metrics <- dplyr::bind_rows(
    reported_metrics_base,
    calibration_row,
    multiple_metrics_row
  )

  write_table_csv_docx(validation_strategies, file.path(output_dir, "validation_strategies.csv"))
  write_table_csv_docx(
    validation_strategies_supplement,
    file.path(output_dir, "validation_strategies_supplement.csv")
  )
  write_table_csv_docx(reported_metrics, file.path(output_dir, "reported_metrics.csv"))

  invisible(list(
    validation_strategies = validation_strategies,
    validation_strategies_supplement = validation_strategies_supplement,
    reported_metrics = reported_metrics
  ))
}
