#' Export feature extraction summary tables
#'
#' @param features_path string(1) Path to `features.rds`.
#' @param output_dir string(1) Directory where CSV tables are written.
#' @param n_representative_features integer(1) Maximum number of unique feature
#'   descriptions to retain per summary row.
#'
#' @returns A named list of tibbles invisibly. The same tables are written to
#'   `output_dir` as CSV files.
#'
#' @details
#' The exported tables summarize feature extraction patterns at both the
#' experiment/extraction-row level and the study level. Modalities are grouped
#' using the same ECG / PPG / Other logic as `tables_modality_over_time()`: ECG
#' and PPG are identified directly, and less common vital-sign modalities (for
#' example EDA, GSR, SCG, VHF, microphone/voice, BVP, HR, and breathing) are
#' grouped as `Other`. Multi-modal experiments can therefore contribute to more
#' than one modality group, matching the modality-over-time summaries.
#'
#' `feature_type_overview.csv` focuses on coded feature attributes
#' (`feature_type`, `feature_dim`, and `dim_reduction`).
#' `feature_selection_overview.csv` is a simplified publication table that
#' aggregates feature selection into broad categories and reports study and
#' experiment counts only. `feature_selection_overview_supplement.csv` retains
#' publication-ready feature-selection categories, dimensionality-reduction
#' details, and `study_ids` for audit traceability without exposing the raw
#' extraction-text columns.
tables_features <- function(
    features_path = here::here("data", "features.rds"),
    output_dir = here::here("results", "tables"),
    n_representative_features = 3
) {
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("readr", quietly = TRUE)
  requireNamespace("stringr", quietly = TRUE)
  requireNamespace("tidyr", quietly = TRUE)

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  collapse_values <- function(x, max_values = Inf) {
    values <- sort(unique(stats::na.omit(as.character(x))))
    values <- values[values != ""]

    if (length(values) == 0) {
      return("not reported")
    }

    if (is.finite(max_values)) {
      values <- utils::head(values, max_values)
    }

    paste(values, collapse = " | ")
  }

  features <- readr::read_rds(features_path) |>
    dplyr::mutate(
      id = as.character(.data$id),
      feature_type = dplyr::na_if(as.character(.data$feature_type), "NA"),
      feature_dim = dplyr::na_if(as.character(.data$feature_dim), "NA"),
      dim_reduction = dplyr::na_if(as.character(.data$dim_reduction), "NA"),
      `Feature selection` = dplyr::na_if(as.character(.data$`Feature selection`), "NA"),
      Features = dplyr::na_if(as.character(.data$Features), "NA"),
      feature_type = dplyr::coalesce(.data$feature_type, "not coded"),
      feature_dim = dplyr::coalesce(.data$feature_dim, "not coded"),
      dim_reduction = dplyr::coalesce(.data$dim_reduction, "not coded"),
      `Feature selection` = dplyr::coalesce(.data$`Feature selection`, "not reported"),
      Features = dplyr::coalesce(.data$Features, "not reported"),
      ECG = stringr::str_detect(.data$modality, stringr::regex("ECG", ignore_case = TRUE)),
      PPG = stringr::str_detect(.data$modality, stringr::regex("PPG", ignore_case = TRUE)),
      Other = stringr::str_detect(
        .data$modality,
        stringr::regex("EDA|GSR|SCG|VHF|microphone|voice|BVP|HR|breath", ignore_case = TRUE)
      ) | (!.data$ECG & !.data$PPG),
      feature_selection_category = dplyr::case_when(
        stringr::str_detect(
          .data$`Feature selection`,
          stringr::regex("^not reported$|^not mentioned$", ignore_case = TRUE)
        ) ~ "none / not reported",
        stringr::str_detect(
          .data$`Feature selection`,
          stringr::regex("^not described$", ignore_case = TRUE)
        ) ~ "none / not reported",
        stringr::str_detect(
          .data$`Feature selection`,
          stringr::regex("^none$", ignore_case = TRUE)
        ) ~ "none / not reported",
        stringr::str_detect(
          .data$`Feature selection`,
          stringr::regex("implicit|automatic|ANN|LSTM|training|model", ignore_case = TRUE)
        ) ~ "Implicit/model-based",
        stringr::str_detect(
          .data$`Feature selection`,
          stringr::regex(
            "correlation|PCA|singular value decomposition|selected|examined|scores|prior experiment|candidate",
            ignore_case = TRUE
          )
        ) ~ "Explicit selection",
        TRUE ~ "none / not reported"
      )
    )

  features_long <- features |>
    dplyr::select(
      "id",
      "experience",
      "modality",
      "Features",
      "Feature selection",
      "feature_selection_category",
      "feature_type",
      "feature_dim",
      "dim_reduction",
      "ECG",
      "PPG",
      "Other"
    ) |>
    tidyr::pivot_longer(
      cols = c("ECG", "PPG", "Other"),
      names_to = "modality_group",
      values_to = "present"
    ) |>
    dplyr::filter(.data$present) |>
    dplyr::select(-"present")

  summarise_features <- function(data, ...) {
    data |>
      dplyr::group_by(...) |>
      dplyr::summarise(
        studies = dplyr::n_distinct(.data$id),
        experiments = dplyr::n(),
        study_ids = collapse_values(.data$id),
        Features = collapse_values(.data$Features, n_representative_features),
        .groups = "drop"
      ) |>
      dplyr::arrange(
        factor(.data$modality_group, levels = c("ECG", "PPG", "Other")),
        .data$feature_type,
        .data$feature_dim,
        .data$dim_reduction
      )
  }

  feature_type_overview <- features_long |>
    summarise_features(
      .data$modality_group,
      .data$feature_type,
      .data$feature_dim,
      .data$dim_reduction
    ) |>
    dplyr::select(
      "modality_group",
      "feature_type",
      "feature_dim",
      "dim_reduction",
      "study_ids",
      "studies",
      "experiments"
    ) |>
    dplyr::rename(
      Modality = "modality_group",
      `Feature type` = "feature_type",
      `Feature dimension` = "feature_dim",
      `Dimensionality reduction` = "dim_reduction",
      `Study IDs` = "study_ids",
      Studies = "studies",
      Experiments = "experiments"
    )

  attr(feature_type_overview, "table_footer") <- paste(
    "Feature type: deep = learned representations from neural/deep-learning models;",
    "handcrafted = investigator-defined signal, morphology, statistical, spectral, or fiducial features;",
    "hybrid = combined handcrafted and learned/transform-based representations.",
    "Feature dimension: low = fewer than 20 features; medium = 20-100 features;",
    "high = more than 100 features; not coded = insufficient information to classify."
  )

  feature_extraction_details <- features_long |>
    dplyr::distinct(
      .data$modality_group,
      .data$id,
      .data$experience,
      .data$modality,
      .data$feature_type,
      .data$feature_dim,
      .data$dim_reduction,
      .data$Features
    ) |>
    dplyr::arrange(
      factor(.data$modality_group, levels = c("ECG", "PPG", "Other")),
      .data$id,
      .data$experience,
      .data$feature_type,
      .data$feature_dim,
      .data$dim_reduction,
      .data$Features
    )

  feature_selection_overview <- features_long |>
    dplyr::group_by(
      .data$modality_group,
      .data$feature_type,
      .data$feature_selection_category
    ) |>
    dplyr::summarise(
      studies = dplyr::n_distinct(.data$id),
      experiments = dplyr::n(),
      study_ids = collapse_values(.data$id),
      .groups = "drop"
    ) |>
    dplyr::arrange(
      factor(.data$modality_group, levels = c("ECG", "PPG", "Other")),
      .data$feature_type,
      .data$feature_selection_category
    ) |>
    dplyr::rename(
      Modality = "modality_group",
      `Feature type` = "feature_type",
      `Feature-selection category` = "feature_selection_category",
      `Study IDs` = "study_ids",
      Studies = "studies",
      Experiments = "experiments"
    )

  feature_selection_overview_supplement <- features_long |>
    summarise_features(
      .data$modality_group,
      .data$feature_type,
      .data$feature_dim,
      .data$dim_reduction,
      .data$feature_selection_category
    ) |>
    dplyr::rename(
      Modality = "modality_group",
      `Feature type` = "feature_type",
      `Feature dimension` = "feature_dim",
      `Dimensionality reduction` = "dim_reduction",
      `Feature-selection category` = "feature_selection_category",
      `Study IDs` = "study_ids",
      Studies = "studies",
      Experiments = "experiments"
    ) |>
    dplyr::select(-"Features")

  write_table_csv_docx(feature_type_overview, file.path(output_dir, "feature_type_overview.csv"))
  write_table_csv_docx(feature_extraction_details, file.path(output_dir, "feature_extraction_details.csv"))
  write_table_csv_docx(feature_selection_overview, file.path(output_dir, "feature_selection_overview.csv"))
  write_table_csv_docx(
    feature_selection_overview_supplement,
    file.path(output_dir, "feature_selection_overview_supplement.csv")
  )

  invisible(list(
    feature_type_overview = feature_type_overview,
    feature_selection_overview = feature_selection_overview,
    feature_selection_overview_supplement = feature_selection_overview_supplement
  ))
}
