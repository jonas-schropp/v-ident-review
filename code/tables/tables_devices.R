#' Export wearable device summary table
#'
#' @param signal_path string(1) Path to `signal.rds`.
#' @param output_dir string(1) Directory where CSV tables are written.
#'
#' @returns A tibble invisibly. The same table is written to
#'   wearable device tables in `output_dir`.
#'
#' @details
#' The signal data are passed through `code_device()` from
#' `code/data-cleaning/code-signal.R` before summarising so device labels such
#' as own-device variants and Hexoskin spelling variants are grouped in the
#' same way as the main data-cleaning pipeline. Sampling-frequency and channel
#' text is collapsed within each normalized device/modality combination because
#' these acquisition details can vary across experiments using the same device
#' and modality.
get_wearable_devices <- function(
    signal_path = here::here("data", "signal.rds"),
    output_dir = here::here("results", "tables")
) {
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("here", quietly = TRUE)
  requireNamespace("readr", quietly = TRUE)
  requireNamespace("tibble", quietly = TRUE)
  requireNamespace("stringr", quietly = TRUE)
  suppressPackageStartupMessages(require("dplyr", quietly = TRUE))

  if (!exists("code_device", mode = "function")) {
    source(here::here("code", "data-cleaning", "code-signal.R"))
  }

  collapse_values <- function(x) {
    values <- unlist(stringr::str_split(as.character(x), stringr::fixed(" | "))) |>
      stringr::str_squish()
    values[is.na(values) | values == ""] <- "not reported"
    paste(unique(values), collapse = " | ")
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  signal <- readr::read_rds(signal_path) |>
    dplyr::mutate(
      id = as.character(.data$id),
      original_device_location = .data$`device location`
    ) |>
    code_device() |>
    dplyr::mutate(
      `device location` = dplyr::coalesce(
        .data$`device location`,
        .data$original_device_location,
        "not reported"
      ),
      dplyr::across(
        dplyr::all_of(c(
          "device",
          "modality",
          "sampling frequency",
          "number of channels / electrodes"
        )),
        ~ dplyr::coalesce(as.character(.x), "not reported")
      )
    ) |>
    dplyr::select(-"original_device_location") |>
    dplyr::distinct(
      .data$id,
      .data$experience,
      .data$device,
      .data$`device location`,
      .data$modality,
      .data$`sampling frequency`,
      .data$`number of channels / electrodes`
    )

  wearable_devices_by_study <- signal |>
    dplyr::group_by(
      .data$id,
      .data$device,
      .data$modality
    ) |>
    dplyr::summarise(
      `number of experiments` = dplyr::n(),
      `device location` = collapse_values(.data$`device location`),
      `sampling frequency` = collapse_values(.data$`sampling frequency`),
      `number of channels / electrodes` = collapse_values(.data$`number of channels / electrodes`),
      .groups = "drop"
    )

  summarise_devices <- function(data) {
    data |>
      dplyr::group_by(
        .data$device,
        .data$modality
      ) |>
      dplyr::summarise(
        `device location` = collapse_values(.data$`device location`),
        `number of studies` = dplyr::n_distinct(.data$id),
        `number of experiments` = sum(.data$`number of experiments`),
        `sampling frequency` = collapse_values(.data$`sampling frequency`),
        `number of channels / electrodes` = collapse_values(.data$`number of channels / electrodes`),
        study_ids = paste(sort(unique(.data$id)), collapse = "; "),
        .groups = "drop"
      ) |>
      dplyr::select(
        "device",
        "device location",
        "modality",
        "number of studies",
        "number of experiments",
        "sampling frequency",
        "number of channels / electrodes",
        "study_ids"
      ) |>
      dplyr::arrange(
        .data$device,
        .data$modality,
        .data$`device location`
      )
  }

  commercial_devices <- wearable_devices_by_study |>
    dplyr::filter(!stringr::str_detect(.data$device, stringr::regex("own device", ignore_case = TRUE))) |>
    summarise_devices()

  custom_devices <- wearable_devices_by_study |>
    dplyr::filter(stringr::str_detect(.data$device, stringr::regex("own device", ignore_case = TRUE))) |>
    summarise_devices()

  write_table_csv_docx(commercial_devices, file.path(output_dir, "wearable_devices_commercial.csv"))
  write_table_csv_docx(custom_devices, file.path(output_dir, "wearable_devices_custom.csv"))

  invisible(list(
    wearable_devices_commercial = commercial_devices,
    wearable_devices_custom = custom_devices
  ))
}
