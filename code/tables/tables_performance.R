#' Export authentication performance publication summaries and audit detail
#'
#' @param eval_path string(1) Path to `eval.rds`.
#' @param output_dir string(1) Directory where the CSV and DOCX tables are written.
#'
#' @returns A tibble invisibly. Publication tables are written to
#'   `authentication_performance_by_study.csv` and
#'   `authentication_performance_by_experiment.csv` in `output_dir`.
#'
#' @details
#' The evaluation extraction stores EER and accuracy as free-text percentage
#' strings. This function parses the leading numeric percentage from each field
#' (for example, `4.8%` becomes `4.8` and `92.3 (6)` becomes `92.3`) and treats
#' blank, `NA`, and other non-numeric values as missing.
#'
#' The by-study publication table first computes within-study summaries for each
#' metric and modality indicator, then reports the median [Q1-Q3] of the within-study
#' medians and the mean (SD) of the within-study means. The by-experiment table
#' summarizes all contributing experiments directly, without first collapsing by
#' study. Both tables include modality indicators for ECG, PPG, EDA, SCG, VHF,
#' microphone, plus a multimodal row for records containing more than one of
#' those modalities.
#'
#' Machine-readable audit detail, including study-level rows and `raw_values`, is
#' written separately to `authentication_performance_audit_detail.csv`.
#'
#' @examples
#' \dontrun{
#' tables_authentication_performance_by_study()
#' }
tables_authentication_performance_by_study <- function(
    eval_path = here::here("data", "eval.rds"),
    output_dir = here::here("results", "tables"),
    group_modality = TRUE
) {
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("readr", quietly = TRUE)
  requireNamespace("stringr", quietly = TRUE)
  requireNamespace("tidyr", quietly = TRUE)
  requireNamespace("tibble", quietly = TRUE)

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  parse_percent <- function(x) {
    x <- as.character(x)
    x <- dplyr::na_if(stringr::str_squish(x), "")
    x <- dplyr::na_if(x, "NA")
    readr::parse_number(x, na = c("", "NA", "N/A", "not reported", "NR", "na"))
  }

  metric_summary <- function(x) {
    tibble::tibble(
      median = if (all(is.na(x))) NA_real_ else stats::median(x, na.rm = TRUE),
      Q1 = if (all(is.na(x))) NA_real_ else unname(stats::quantile(x, 0.25, na.rm = TRUE)),
      Q3 = if (all(is.na(x))) NA_real_ else unname(stats::quantile(x, 0.75, na.rm = TRUE)),
      mean = if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE),
      SD = if (sum(!is.na(x)) <= 1) NA_real_ else stats::sd(x, na.rm = TRUE),
      min = if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE),
      max = if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
    )
  }

  format_one_decimal <- function(x) {
    dplyr::if_else(is.na(x), NA_character_, format(round(x, 1), nsmall = 1, trim = TRUE))
  }

  format_performance_columns <- function(data) {
    data |>
      dplyr::mutate(dplyr::across(c("median", "Q1", "Q3", "mean", "SD", "min", "max"), ~ round(.x, 1))) |>
      dplyr::mutate(
        `median [Q1-Q3]` = dplyr::if_else(
          is.na(.data$median),
          NA_character_,
          paste0(
            format_one_decimal(.data$median),
            " [",
            format_one_decimal(.data$Q1),
            "-",
            format_one_decimal(.data$Q3),
            "]"
          )
        ),
        `mean (SD)` = dplyr::if_else(
          is.na(.data$mean),
          NA_character_,
          paste0(format_one_decimal(.data$mean), " (", format_one_decimal(.data$SD), ")")
        )
      ) |>
      dplyr::select(-dplyr::all_of(c("median", "Q1", "Q3", "mean", "SD"))) |>
      dplyr::relocate("median [Q1-Q3]", "mean (SD)", .after = "Metric (%)")
  }

  modality_levels <- c("ECG", "PPG", "EDA", "SCG", "VHF", "Microphone", "Multimodal")

  eval <- readr::read_rds(eval_path) |>
    dplyr::mutate(
      id = as.character(.data$id),
      modality_text = stringr::str_squish(as.character(.data$modality)),
      ECG = stringr::str_detect(.data$modality_text, stringr::regex("\\bECG\\b|electrocardi", ignore_case = TRUE)),
      PPG = stringr::str_detect(.data$modality_text, stringr::regex("\\bPPG\\b|photopleth", ignore_case = TRUE)),
      EDA = stringr::str_detect(.data$modality_text, stringr::regex("\\bEDA\\b|electrodermal|galvanic skin|\\bGSR\\b", ignore_case = TRUE)),
      SCG = stringr::str_detect(.data$modality_text, stringr::regex("\\bSCG\\b|seismocardi", ignore_case = TRUE)),
      VHF = stringr::str_detect(.data$modality_text, stringr::regex("\\bVHF\\b", ignore_case = TRUE)),
      Microphone = stringr::str_detect(.data$modality_text, stringr::regex("microphone|\\bmic\\b|audio|voice|speech", ignore_case = TRUE))
    ) |>
    dplyr::mutate(
      Multimodal = rowSums(dplyr::pick(dplyr::all_of(modality_levels[-7])), na.rm = TRUE) > 1
    )

  performance_long <- eval |>
    dplyr::transmute(
      id = .data$id,
      experience = .data$experience,
      dplyr::across(dplyr::all_of(modality_levels)),
      EER_raw = as.character(.data$EER),
      accuracy_raw = as.character(.data$accuracy),
      EER = parse_percent(.data$EER),
      accuracy = parse_percent(.data$accuracy)
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(modality_levels),
      names_to = "modality",
      values_to = "modality_present"
    ) |>
    dplyr::filter(.data$modality_present) |>
    tidyr::pivot_longer(
      cols = c("EER", "accuracy"),
      names_to = "metric",
      values_to = "percent"
    ) |>
    dplyr::mutate(
      raw_value = dplyr::if_else(.data$metric == "EER", .data$EER_raw, .data$accuracy_raw),
      raw_value = dplyr::na_if(stringr::str_squish(.data$raw_value), ""),
      raw_value = dplyr::na_if(.data$raw_value, "NA"),
      metric = dplyr::recode(.data$metric, accuracy = "Accuracy"),
      metric = factor(.data$metric, levels = c("EER", "Accuracy")),
      modality = factor(.data$modality, levels = modality_levels)
    )

  study_level <- performance_long |>
    dplyr::group_by(.data$id, .data$modality, .data$metric) |>
    dplyr::summarise(
      study_median = if (all(is.na(.data$percent))) NA_real_ else stats::median(.data$percent, na.rm = TRUE),
      study_mean = if (all(is.na(.data$percent))) NA_real_ else mean(.data$percent, na.rm = TRUE),
      contributing_experiments = sum(!is.na(.data$percent)),
      missing_experiments = sum(is.na(.data$percent)),
      total_experiments = dplyr::n(),
      raw_values = paste(sort(unique(stats::na.omit(.data$raw_value))), collapse = " | "),
      .groups = "drop"
    )

  performance_by_study <- study_level |>
    dplyr::filter(.data$contributing_experiments > 0) |>
    dplyr::group_by(.data$modality, .data$metric) |>
    dplyr::summarise(
      median = stats::median(.data$study_median, na.rm = TRUE),
      Q1 = unname(stats::quantile(.data$study_median, 0.25, na.rm = TRUE)),
      Q3 = unname(stats::quantile(.data$study_median, 0.75, na.rm = TRUE)),
      mean = mean(.data$study_mean, na.rm = TRUE),
      SD = if (dplyr::n() <= 1) NA_real_ else stats::sd(.data$study_mean, na.rm = TRUE),
      min = min(.data$study_median, na.rm = TRUE),
      max = max(.data$study_median, na.rm = TRUE),
      `N (studies)` = dplyr::n_distinct(.data$id),
      `N (experiments)` = sum(.data$contributing_experiments),
      `Missing experiments` = sum(.data$missing_experiments),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      modality = factor(.data$modality, levels = modality_levels),
      metric = factor(.data$metric, levels = c("EER", "Accuracy"))
    ) |>
    dplyr::arrange(.data$modality, .data$metric) |>
    dplyr::rename(Modality = "modality", `Metric (%)` = "metric") |>
    format_performance_columns()

  performance_by_experiment <- performance_long |>
    dplyr::filter(!is.na(.data$percent)) |>
    dplyr::group_by(.data$modality, .data$metric) |>
    dplyr::summarise(
      metric_summary(.data$percent),
      `N (studies)` = dplyr::n_distinct(.data$id),
      `N (experiments)` = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      modality = factor(.data$modality, levels = modality_levels),
      metric = factor(.data$metric, levels = c("EER", "Accuracy"))
    ) |>
    dplyr::arrange(.data$modality, .data$metric) |>
    dplyr::rename(Modality = "modality", `Metric (%)` = "metric") |>
    format_performance_columns()

  audit_detail <- study_level |>
    dplyr::arrange(.data$id, .data$modality, .data$metric) |>
    dplyr::rename(
      study_id = "id",
      Modality = "modality",
      `Metric (%)` = "metric",
      median = "study_median",
      mean = "study_mean",
      `Contributing experiments` = "contributing_experiments",
      `Missing experiments` = "missing_experiments",
      `Total experiments` = "total_experiments"
    )

  write_table_csv_docx(performance_by_study, file.path(output_dir, "authentication_performance_by_study.csv"))
  write_table_csv_docx(performance_by_experiment, file.path(output_dir, "authentication_performance_by_experiment.csv"))
  write_table_csv_docx(audit_detail, file.path(output_dir, "authentication_performance_audit_detail.csv"))

  invisible(performance_by_study)
}
