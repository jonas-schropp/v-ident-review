#' Export modality-by-method cross-tab summary tables
#'
#' @param signal_path string(1) Path to `signal.rds` with modality coding.
#' @param preproc_path string(1) Path to `preproc.rds` with preprocessing coding.
#' @param features_path string(1) Path to `features.rds` with feature coding.
#' @param auth_path string(1) Path to `auth.rds` with authentication-model coding.
#' @param eval_path string(1) Optional path to `eval.rds` with evaluation metrics.
#' @param output_dir string(1) Directory where CSV tables are written.
#'
#' @returns A named list of tibbles invisibly. The same tables are written to
#'   `output_dir` as CSV files.
#'
#' @details
#' This script joins the coded extraction tables at the study/experience/modality
#' level and exports compact cross-tabs for the main modality groups used in
#' `tables_modality_over_time()`: ECG, PPG, and Other. Counts are reported at the
#' study level and at the experiment/extraction-row level, and each cell includes
#' `study_ids` to make the tabulations auditable. When `eval_path` is available,
#' an additional modality-by-available-performance-metric table is exported.
tables_modality_methods <- function(
    signal_path = here::here("data", "signal.rds"),
    preproc_path = here::here("data", "preproc.rds"),
    features_path = here::here("data", "features.rds"),
    auth_path = here::here("data", "auth.rds"),
    eval_path = here::here("data", "eval.rds"),
    output_dir = here::here("results", "tables")
) {
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("readr", quietly = TRUE)
  requireNamespace("stringr", quietly = TRUE)
  requireNamespace("tidyr", quietly = TRUE)

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  join_keys <- c("id", "experience", "modality")
  modality_levels <- c("ECG", "PPG", "Other")

  normalize_keys <- function(data) {
    data |>
      dplyr::mutate(
        id = as.character(.data$id),
        experience = as.character(.data$experience),
        modality = as.character(.data$modality)
      )
  }

  coalesce_missing <- function(x) {
    x <- as.character(x)
    x[is.na(x) | x == "" | x == "NA"] <- "not reported"
    x
  }

  label_algorithm_class <- function(x) {
    dplyr::recode(
      as.character(x),
      dl = "Deep learning (e.g. FF-NN, CNN)",
      dl_temp = "Temporal deep learning (e.g. LSTM, RNN)",
      dist = "Distance-based (e.g. kNN, template matching)",
      kernel = "Kernel-based (SVM)",
      lm = "Linear model (e.g. LR, LDA)",
      prob = "Probabilistic (NB)",
      tree = "Tree-based (e.g. RF, DT)",
      .default = as.character(x)
    )
  }

  add_algorithm_class <- function(auth) {
    if ("algorithm_class" %in% names(auth)) {
      return(auth)
    }

    auth |>
      dplyr::mutate(
        algorithm = dplyr::case_when(
          .data$`Authentication algorithm` %in% c("FF-NN", "MLP", "CNN", "RNN", "LSTM", "Autoencoder", "GBM", "RF", "DT", "SVM", "LR", "LDA", "kNN", "NB") ~ .data$`Authentication algorithm`,
          .data$`Authentication algorithm` == "distance based" ~ "unspecified distance metric",
          .data$`Authentication algorithm` %in% c("TCM + LSTM + template matching", "CNN + LSTM", "CRNN") ~ "TCM/CNN+LSTM/CRNN",
          .data$`Authentication algorithm` %in% c("NA", "") & stringr::str_detect(.data$`Algorithm Details`, stringr::regex("template matching", ignore_case = TRUE)) ~ "unspecified distance metric",
          .data$`Authentication algorithm` %in% c("NA", "") ~ NA_character_,
          TRUE ~ .data$`Authentication algorithm`
        ),
        algorithm_class = dplyr::case_when(
          .data$algorithm %in% c("FF-NN", "MLP", "CNN") ~ "dl",
          .data$algorithm %in% c("RNN", "LSTM", "TCM/CNN+LSTM/CRNN", "Autoencoder") ~ "dl_temp",
          .data$algorithm %in% c("GBM", "RF", "DT") ~ "tree",
          .data$algorithm == "SVM" ~ "kernel",
          .data$algorithm %in% c("LR", "LDA") ~ "lm",
          .data$algorithm %in% c("kNN", "unspecified distance metric") ~ "dist",
          .data$algorithm == "NB" ~ "prob",
          TRUE ~ NA_character_
        )
      )
  }

  summarise_crosstab <- function(data, by_col) {
    data |>
      dplyr::mutate(coded_value = coalesce_missing({{ by_col }})) |>
      dplyr::distinct(.data$id, .data$experience, .data$modality, .data$modality_group, .data$coded_value) |>
      dplyr::group_by(.data$modality_group, .data$coded_value) |>
      dplyr::summarise(
        study_count = dplyr::n_distinct(.data$id),
        experiment_count = dplyr::n(),
        study_ids = paste(sort(unique(.data$id)), collapse = "; "),
        .groups = "drop"
      ) |>
      dplyr::arrange(factor(.data$modality_group, levels = modality_levels), .data$coded_value)
  }

  signal <- readr::read_rds(signal_path) |>
    normalize_keys() |>
    dplyr::mutate(
      ECG = .data$ECG == "yes" | .data$ECG == 1 | stringr::str_detect(.data$modality, stringr::regex("ECG", ignore_case = TRUE)),
      PPG = .data$PPG == "yes" | .data$PPG == 1 | stringr::str_detect(.data$modality, stringr::regex("PPG", ignore_case = TRUE)),
      Other = .data$EDA == "yes" | .data$EDA == 1 | .data$GSR == "yes" | .data$GSR == 1 |
        .data$SCG == "yes" | .data$SCG == 1 | .data$VHF == "yes" | .data$VHF == 1 |
        .data$microphone == "yes" | .data$microphone == 1 | .data$BVP == "yes" | .data$BVP == 1 |
        .data$HR == "yes" | .data$HR == 1 | .data$breathing == "yes" | .data$breathing == 1 |
        stringr::str_detect(.data$modality, stringr::regex("EDA|GSR|SCG|VHF|microphone|voice|BVP|HR|breath", ignore_case = TRUE))
    )

  modality_long <- signal |>
    dplyr::select(dplyr::all_of(join_keys), "ECG", "PPG", "Other") |>
    tidyr::pivot_longer(
      cols = c("ECG", "PPG", "Other"),
      names_to = "modality_group",
      values_to = "present"
    ) |>
    dplyr::filter(.data$present) |>
    dplyr::select(-"present") |>
    dplyr::distinct()

  features <- readr::read_rds(features_path) |>
    normalize_keys() |>
    dplyr::select(dplyr::all_of(join_keys), "feature_type") |>
    dplyr::distinct()

  auth <- readr::read_rds(auth_path) |>
    normalize_keys() |>
    add_algorithm_class() |>
    dplyr::select(dplyr::all_of(join_keys), "algorithm_class") |>
    dplyr::distinct()

  preproc <- readr::read_rds(preproc_path) |>
    normalize_keys() |>
    dplyr::transmute(
      id = .data$id,
      experience = .data$experience,
      modality = .data$modality,
      Filtering = coalesce_missing(.data$filteringyn),
      Segmentation = coalesce_missing(.data$segmentation_type),
      Normalization = coalesce_missing(.data$normalizationyn)
    ) |>
    tidyr::pivot_longer(
      cols = c("Filtering", "Segmentation", "Normalization"),
      names_to = "Preprocessing domain",
      values_to = "Category"
    ) |>
    dplyr::mutate(Category = stringr::str_to_title(.data$Category)) |>
    dplyr::distinct()

  eval_metrics <- NULL
  if (!is.null(eval_path) && file.exists(eval_path)) {
    eval_metrics <- readr::read_rds(eval_path) |>
      normalize_keys() |>
      dplyr::rowwise() |>
      dplyr::mutate(
        available_performance_metrics = paste(
          c("EER", "accuracy", "F1", "AUC")[
            !is.na(dplyr::c_across(dplyr::all_of(c("EER", "accuracy", "F1", "AUC")))) &
              dplyr::c_across(dplyr::all_of(c("EER", "accuracy", "F1", "AUC"))) != "NA"
          ],
          collapse = "; "
        )
      ) |>
      dplyr::ungroup() |>
      dplyr::select(dplyr::all_of(join_keys), "available_performance_metrics") |>
      dplyr::mutate(available_performance_metrics = dplyr::na_if(.data$available_performance_metrics, "")) |>
      dplyr::distinct()
  }

  method_data <- modality_long |>
    dplyr::left_join(auth, by = join_keys) |>
    dplyr::left_join(features, by = join_keys)

  if (!is.null(eval_metrics)) {
    method_data <- method_data |>
      dplyr::left_join(eval_metrics, by = join_keys)
  }

  modality_by_algorithm_class <- summarise_crosstab(method_data, .data$algorithm_class) |>
    dplyr::mutate(coded_value = label_algorithm_class(.data$coded_value)) |>
    dplyr::rename(algorithm_class = .data$coded_value)

  modality_by_feature_type <- summarise_crosstab(method_data, .data$feature_type) |>
    dplyr::rename(feature_type = .data$coded_value)

  modality_by_preprocessing_category <- modality_long |>
    dplyr::left_join(preproc, by = join_keys) |>
    dplyr::mutate(Category = stringr::str_to_title(coalesce_missing(.data$Category))) |>
    dplyr::distinct(.data$id, .data$experience, .data$modality, .data$modality_group, .data$`Preprocessing domain`, .data$Category) |>
    dplyr::group_by(.data$modality_group, .data$`Preprocessing domain`, .data$Category) |>
    dplyr::summarise(
      Studies = dplyr::n_distinct(.data$id),
      Experiments = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::arrange(factor(.data$modality_group, levels = modality_levels), .data$`Preprocessing domain`, .data$Category) |>
    dplyr::rename(Modality = .data$modality_group)

  modality_by_performance_metric <- NULL
  if ("available_performance_metrics" %in% names(method_data)) {
    performance_metric_data <- method_data |>
      dplyr::mutate(available_performance_metrics = coalesce_missing(.data$available_performance_metrics)) |>
      tidyr::separate_longer_delim(.data$available_performance_metrics, delim = "; ")

    performance_metric_totals <- performance_metric_data |>
      dplyr::distinct(.data$id, .data$experience, .data$modality, .data$modality_group) |>
      dplyr::group_by(.data$modality_group) |>
      dplyr::summarise(
        modality_study_count = dplyr::n_distinct(.data$id),
        modality_experiment_count = dplyr::n(),
        .groups = "drop"
      )

    modality_by_performance_metric <- performance_metric_data |>
      summarise_crosstab(.data$available_performance_metrics) |>
      dplyr::left_join(performance_metric_totals, by = "modality_group") |>
      dplyr::mutate(
        study_percent = round(100 * .data$study_count / .data$modality_study_count, 1),
        experiment_percent = round(100 * .data$experiment_count / .data$modality_experiment_count, 1)
      ) |>
      dplyr::select(-"modality_study_count", -"modality_experiment_count") |>
      dplyr::mutate(
        coded_value = factor(
          .data$coded_value,
          levels = c(sort(setdiff(unique(.data$coded_value), "not reported")), "not reported")
        )
      ) |>
      dplyr::arrange(factor(.data$modality_group, levels = modality_levels), .data$coded_value) |>
      dplyr::mutate(coded_value = as.character(.data$coded_value)) |>
      dplyr::rename(`Performance metric` = .data$coded_value)
  }

  write_table_csv_docx(modality_by_algorithm_class, file.path(output_dir, "modality_by_algorithm_class.csv"))
  write_table_csv_docx(modality_by_feature_type, file.path(output_dir, "modality_by_feature_type.csv"))
  write_table_csv_docx(modality_by_preprocessing_category, file.path(output_dir, "modality_by_preprocessing_category.csv"))
  if (!is.null(modality_by_performance_metric)) {
    write_table_csv_docx(modality_by_performance_metric, file.path(output_dir, "modality_by_performance_metric.csv"))
  }

  output <- list(
    modality_by_algorithm_class = modality_by_algorithm_class,
    modality_by_feature_type = modality_by_feature_type,
    modality_by_preprocessing_category = modality_by_preprocessing_category
  )
  if (!is.null(modality_by_performance_metric)) {
    output$modality_by_performance_metric <- modality_by_performance_metric
  }

  invisible(output)
}
