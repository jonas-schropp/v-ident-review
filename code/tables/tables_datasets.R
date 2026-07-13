#' Export public and custom dataset summary tables
#'
#' @param signal_path string(1) Path to `signal.rds`.
#' @param auth_path string(1) Optional path to `auth.rds`; used to add the
#'   manuscript's situational-stability coding because that field is not stored
#'   in `signal.rds`.
#' @param output_dir string(1) Directory where CSV tables are written.
#'
#' @returns A named list of tibbles invisibly. The same tables are written to
#'   `output_dir` as CSV files.
#'
#' @details
#' The signal extraction data are the primary source for dataset name, study id,
#' genuine users, intruders, total participants, modality, acquisition time,
#' conditions, and permanence. Public datasets are identified as rows whose
#' signal `data set` column is not coded exactly as `own data` (after trimming
#' whitespace and ignoring case). Public rows are aggregated at the
#' dataset-name level and include `study_ids`; custom rows are kept at
#' dataset/study-id granularity and also include `study_ids` for a consistent
#' schema.
tables_datasets <- function(
    signal_path = here::here("data", "signal.rds"),
    auth_path = here::here("data", "auth.rds"),
    output_dir = here::here("results", "tables")
) {
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("readr", quietly = TRUE)
  requireNamespace("stringr", quietly = TRUE)

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  signal <- readr::read_rds(signal_path) |>
    dplyr::mutate(
      id = as.character(.data$id),
      dataset_name = dplyr::coalesce(as.character(.data$`data set`), "not reported")
    )

  if (file.exists(auth_path)) {
    auth <- readr::read_rds(auth_path) |>
      dplyr::mutate(id = as.character(.data$id)) |>
      dplyr::select("id", "experience", "modality", "situational stability") |>
      dplyr::distinct()

    signal <- signal |>
      dplyr::left_join(auth, by = c("id", "experience", "modality"))
  } else {
    signal <- signal |>
      dplyr::mutate(`situational stability` = NA_character_)
  }

  standardize_dataset_name <- function(dataset_name) {
    dplyr::case_when(
      stringr::str_detect(dataset_name, stringr::regex("ppg[- ]?dalia", ignore_case = TRUE)) ~ "PPG-DaLiA",
      stringr::str_detect(dataset_name, stringr::regex("troika", ignore_case = TRUE)) ~ "TROIKA",
      stringr::str_detect(dataset_name, stringr::regex("ieee\\s*spc", ignore_case = TRUE)) ~ "IEEE SPC",
      stringr::str_detect(dataset_name, stringr::regex("vollmer", ignore_case = TRUE)) ~ "Vollmer DB",
      stringr::str_detect(dataset_name, stringr::regex("students?", ignore_case = TRUE)) ~ "Students",
      stringr::str_detect(dataset_name, stringr::regex("blasco.*lopez", ignore_case = TRUE)) ~ "Blasco & Lopez",
      TRUE ~ dataset_name
    )
  }

  is_own_data <- function(dataset_name) {
    stringr::str_to_lower(stringr::str_trim(dataset_name)) == "own data"
  }

  normalize_missing <- function(x, missing_label = "Not reported") {
    x <- as.character(x)
    dplyr::case_when(
      is.na(x) | x %in% c("", "NA", "not reported") ~ missing_label,
      x == "?" ~ "Unclear",
      TRUE ~ x
    )
  }

  collapse_values <- function(x, missing_label = "Not reported", levels = NULL) {
    values <- unique(normalize_missing(x, missing_label = missing_label))
    values <- values[!(values %in% c("", "NA"))]
    if (length(values) == 0) {
      missing_label
    } else if (!is.null(levels)) {
      paste(levels[levels %in% values], collapse = " | ")
    } else {
      paste(sort(values), collapse = " | ")
    }
  }

  parse_genuine_users <- function(x) {
    x <- normalize_missing(x)
    dplyr::case_when(
      stringr::str_detect(x, stringr::regex("^\\d+ users?,", ignore_case = TRUE)) ~
        stringr::str_extract(x, "^\\d+"),
      stringr::str_detect(x, "^\\d+$") ~ x,
      TRUE ~ "Not reported"
    )
  }

  parse_intruders <- function(x) {
    x <- normalize_missing(x)
    dplyr::case_when(
      stringr::str_detect(x, stringr::regex("intruders?", ignore_case = TRUE)) ~
        stringr::str_extract(x, "(?<=, )\\d+(?= intruders?)"),
      TRUE ~ "Not reported"
    )
  }

  parse_total_participants <- function(x) {
    x <- normalize_missing(x)
    users <- suppressWarnings(as.integer(parse_genuine_users(x)))
    intruders <- suppressWarnings(as.integer(parse_intruders(x)))
    dplyr::case_when(
      !is.na(users) & !is.na(intruders) ~ as.character(users + intruders),
      stringr::str_detect(x, "^\\d+$") ~ x,
      TRUE ~ "Not reported"
    )
  }

  standardize_situational_stability <- function(x) {
    x <- normalize_missing(x, missing_label = "Unclear")
    dplyr::case_when(
      x %in% c("no", "no different activity levels") ~ "None",
      x %in% c("all during driving task", "small (idle+touchpad)", "small (idle+typing)",
               "small (working at desk + resting)", "yes") ~ "Limited",
      x %in% c("Different activity levels (TROIKA DB)", "different activity levels",
               "different situations simulated for respiration",
               "moderate (several situations including stress)",
               "some (arm movement and sitting)", "some (rest and exercise)",
               "walking + no controlled environment imposed") ~ "Moderate",
      x %in% c("high - several settings (resting, standing, walking, walking uphill)",
               "high, data from everyday life ( days)", "yes, everyday activities",
               "no controlled environment imposed (real-life conditions)",
               "no controlled environment imposed (real-life conditions) or different activity levels, not sufficiently described") ~ "High",
      TRUE ~ "Unclear"
    )
  }

  standardize_permanence <- function(x) {
    x <- normalize_missing(x)
    dplyr::case_when(
      x == "low (one day)" ~ "One day",
      x == "medium (multiple days)" ~ "Multiple days",
      x == "high (> one week)" ~ "High (> one week)",
      TRUE ~ "Not reported"
    )
  }

  situational_levels <- c("None", "Limited", "Moderate", "High", "Unclear")
  permanence_levels <- c("Single session", "One day", "Multiple days", "High (> one week)", "Not reported")

  dataset_rows <- signal |>
    dplyr::mutate(public_dataset_name = dplyr::if_else(is_own_data(.data$dataset_name), NA_character_, standardize_dataset_name(.data$dataset_name))) |>
    dplyr::transmute(
      study_id = .data$id,
      dataset_name = .data$dataset_name,
      public_dataset_name = .data$public_dataset_name,
      `number of individuals` = .data$`number of individuals`,
      `Genuine users` = parse_genuine_users(.data$`number of individuals`),
      Intruders = parse_intruders(.data$`number of individuals`),
      `Total participants` = parse_total_participants(.data$`number of individuals`),
      modality = .data$modality,
      `acquisition time` = .data$`acquisition time`,
      conditions = .data$conditions,
      `situational stability` = standardize_situational_stability(.data$`situational stability`),
      permanence = standardize_permanence(.data$permanence),
      `Raw number of individuals` = normalize_missing(.data$`number of individuals`),
      `Raw situational stability` = normalize_missing(.data$`situational stability`, missing_label = "Unclear"),
      `Raw permanence` = normalize_missing(.data$permanence)
    ) |>
    dplyr::distinct()

  public_datasets <- dataset_rows |>
    dplyr::filter(!is.na(.data$public_dataset_name)) |>
    dplyr::group_by(dataset_name = .data$public_dataset_name) |>
    dplyr::summarise(
      study_ids = paste(sort(unique(.data$study_id)), collapse = "; "),
      N = collapse_values(.data$`Genuine users`),
      modality = collapse_values(.data$modality),
      `acquisition time` = collapse_values(.data$`acquisition time`),
      conditions = collapse_values(.data$conditions),
      `situational stability` = collapse_values(.data$`situational stability`, missing_label = "Unclear", levels = situational_levels),
      permanence = collapse_values(.data$permanence, levels = permanence_levels),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$dataset_name)

  custom_datasets <- dataset_rows |>
    dplyr::filter(is.na(.data$public_dataset_name)) |>
    dplyr::group_by(dataset_name = .data$dataset_name, study_id = .data$study_id) |>
    dplyr::summarise(
      `Genuine users` = collapse_values(.data$`Genuine users`),
      Intruders = collapse_values(.data$Intruders),
      `Total participants` = collapse_values(.data$`Total participants`),
      modality = collapse_values(.data$modality),
      `acquisition time` = collapse_values(.data$`acquisition time`),
      conditions = collapse_values(.data$conditions),
      `situational stability` = collapse_values(.data$`situational stability`, missing_label = "Unclear", levels = situational_levels),
      permanence = collapse_values(.data$permanence, levels = permanence_levels),
      study_ids = paste(sort(unique(.data$study_id)), collapse = "; "),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$dataset_name, as.numeric(.data$study_id), .data$study_id)

  custom_dataset_details <- dataset_rows |>
    dplyr::filter(is.na(.data$public_dataset_name)) |>
    dplyr::select(
      dataset_name, study_id,
      `Raw number of individuals`, `Raw situational stability`, `Raw permanence`
    ) |>
    dplyr::distinct() |>
    dplyr::arrange(.data$dataset_name, as.numeric(.data$study_id), .data$study_id)

  write_table_csv_docx(public_datasets, file.path(output_dir, "public_datasets.csv"))
  write_table_csv_docx(custom_datasets, file.path(output_dir, "custom_datasets.csv"))
  write_table_csv_docx(custom_dataset_details, file.path(output_dir, "custom_dataset_details.csv"))

  invisible(list(
    public_datasets = public_datasets,
    custom_datasets = custom_datasets,
    custom_dataset_details = custom_dataset_details
  ))
}
