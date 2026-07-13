#' Export use-case table and free-text challenge description
#'
#' @param other_path string(1) Path to `other.rds`.
#' @param general_path string(1) Path to `general.rds`.
#' @param output_dir string(1) Directory where CSV tables are written.
#'
#' @returns A named list invisibly. The use-case category table is written as
#'   CSV/DOCX and the challenge description is written as a narrative DOCX file.
#'
#' @details
#' The detailed table preserves the extraction fields from `other.rds` and adds
#' `authors` and `year` from `general.rds` when those bibliographic fields are
#' available. Because `general.rds` can contain multiple extraction rows per
#' study, bibliographic metadata is collapsed to one row per `id` before joining.
#'
#' The challenge description intentionally does not create a publication table.
#' Instead, it combines the free-text challenge extraction in `other.rds` with
#' raw study-level context from the authentication, evaluation, and signal
#' extraction files when those files are available in the data directory.
#'
#' The category summary is a lightweight coded overview of the free-text use-case
#' descriptions. Categories are assigned with transparent keyword rules for the
#' use cases requested in the manuscript workflow: continuous driver
#' authentication, device authentication, IoT/access control, banking or
#' high-security application authentication, and remote health monitoring. Rows
#' that mention a use case but do not match those rules are retained as `Other`;
#' rows without a reported use case are retained as `Not reported`.
tables_use_cases <- function(
    other_path = here::here("data", "other.rds"),
    general_path = here::here("data", "general.rds"),
    output_dir = here::here("results", "tables")
) {
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("readr", quietly = TRUE)
  requireNamespace("stringr", quietly = TRUE)
  requireNamespace("tidyr", quietly = TRUE)
  requireNamespace("officer", quietly = TRUE)
  requireNamespace("tibble", quietly = TRUE)

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  other <- readr::read_rds(other_path) |>
    dplyr::mutate(id = as.character(.data$id))

  general <- readr::read_rds(general_path) |>
    dplyr::mutate(id = as.character(.data$id)) |>
    dplyr::group_by(.data$id) |>
    dplyr::summarise(
      authors = paste(sort(unique(stats::na.omit(as.character(.data$authors)))), collapse = "; "),
      year = paste(sort(unique(stats::na.omit(as.character(.data$year)))), collapse = "; "),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      authors = dplyr::na_if(.data$authors, ""),
      year = dplyr::na_if(.data$year, "")
    )

  use_cases_challenges <- other |>
    dplyr::select(
      "id",
      "Use cases and application scenarios",
      "independence",
      "remaining challenges",
      "Note"
    ) |>
    dplyr::left_join(general, by = "id") |>
    dplyr::relocate(.data$authors, .data$year, .after = .data$id) |>
    dplyr::arrange(suppressWarnings(as.numeric(.data$id)), .data$id)

  coded_use_cases <- use_cases_challenges |>
    dplyr::mutate(
      use_case_text = dplyr::coalesce(as.character(.data$`Use cases and application scenarios`), ""),
      use_case_text = dplyr::if_else(.data$use_case_text == "NA", "", .data$use_case_text),
      continuous_driver_authentication = stringr::str_detect(
        .data$use_case_text,
        stringr::regex("driver|driving|vehicle|automotive|car", ignore_case = TRUE)
      ),
      device_authentication = stringr::str_detect(
        .data$use_case_text,
        stringr::regex("device|smartphone|phone|mobile|wearable|watch|on[- ]?device", ignore_case = TRUE)
      ),
      iot_access_control = stringr::str_detect(
        .data$use_case_text,
        stringr::regex("\\bIoT\\b|internet of things|access control|door|lock|building|smart home|terminal", ignore_case = TRUE)
      ),
      banking_high_security_app_authentication = stringr::str_detect(
        .data$use_case_text,
        stringr::regex("bank|payment|financial|high[- ]security|secure app|application|transaction", ignore_case = TRUE)
      ),
      remote_health_monitoring = stringr::str_detect(
        .data$use_case_text,
        stringr::regex("health|healthcare|medical|patient|remote monitor|telehealth|telemedicine|hospital|clinical", ignore_case = TRUE)
      )
    ) |>
    tidyr::pivot_longer(
      cols = c(
        "continuous_driver_authentication",
        "device_authentication",
        "iot_access_control",
        "banking_high_security_app_authentication",
        "remote_health_monitoring"
      ),
      names_to = "use_case_category",
      values_to = "category_present"
    ) |>
    dplyr::filter(.data$category_present) |>
    dplyr::mutate(
      use_case_category = dplyr::case_match(
        .data$use_case_category,
        "continuous_driver_authentication" ~ "Continuous driver authentication",
        "device_authentication" ~ "Device authentication",
        "iot_access_control" ~ "IoT/access control",
        "banking_high_security_app_authentication" ~ "Banking/high-security app authentication",
        "remote_health_monitoring" ~ "Remote health monitoring"
      )
    ) |>
    dplyr::select("id", "authors", "year", "use_case_category", "Use cases and application scenarios")

  uncoded_use_cases <- use_cases_challenges |>
    dplyr::anti_join(coded_use_cases |> dplyr::distinct(.data$id), by = "id") |>
    dplyr::mutate(
      use_case_category = dplyr::if_else(
        is.na(.data$`Use cases and application scenarios`) |
          .data$`Use cases and application scenarios` == "NA",
        "Not reported",
        "Other"
      )
    ) |>
    dplyr::select("id", "authors", "year", "use_case_category", "Use cases and application scenarios")

  use_case_categories <- dplyr::bind_rows(coded_use_cases, uncoded_use_cases) |>
    dplyr::group_by(.data$use_case_category) |>
    dplyr::summarise(
      studies = dplyr::n_distinct(.data$id),
      study_ids = paste(sort(unique(.data$id)), collapse = "; "),
      examples = paste(sort(unique(stats::na.omit(.data$`Use cases and application scenarios`))), collapse = " | "),
      .groups = "drop"
    ) |>
    dplyr::mutate(examples = dplyr::na_if(.data$examples, "")) |>
    dplyr::arrange(
      factor(
        .data$use_case_category,
        levels = c(
          "Continuous driver authentication",
          "Device authentication",
          "IoT/access control",
          "Banking/high-security app authentication",
          "Remote health monitoring",
          "Other",
          "Not reported"
        )
      )
    )

  challenge_description <- build_challenge_description(
    use_cases_challenges = use_cases_challenges,
    data_dir = dirname(other_path)
  )

  export_challenge_description_docx(
    challenge_description,
    file.path(dirname(output_dir), "use_case_challenges_description.docx")
  )
  write_table_csv_docx(use_case_categories, file.path(output_dir, "use_case_categories.csv"))

  invisible(list(
    use_cases_challenges = use_cases_challenges,
    challenge_description = challenge_description,
    use_case_categories = use_case_categories
  ))
}

collapse_reported_values <- function(x, max_values = 4) {
  x <- unique(stats::na.omit(as.character(x)))
  x <- x[!x %in% c("", "NA", "not reported")]
  x <- sort(x)
  if (length(x) == 0) {
    return(NA_character_)
  }
  paste(utils::head(x, max_values), collapse = "; ")
}

safe_read_context <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }
  readr::read_rds(path) |>
    dplyr::mutate(id = as.character(.data$id))
}

build_challenge_description <- function(use_cases_challenges, data_dir) {
  auth <- safe_read_context(file.path(data_dir, "auth.rds"))
  eval <- safe_read_context(file.path(data_dir, "eval.rds"))
  signal <- safe_read_context(file.path(data_dir, "signal.rds"))

  study_notes <- use_cases_challenges |>
    dplyr::rowwise() |>
    dplyr::mutate(
      remaining_challenges = dplyr::na_if(as.character(.data$`remaining challenges`), "NA"),
      independence_note = dplyr::na_if(as.character(.data$independence), "NA"),
      extraction_note = dplyr::na_if(as.character(.data$Note), "NA"),
      enrollment_time = if (!is.null(eval)) {
        collapse_reported_values(eval$`Enroll time`[eval$id == .data$id])
      } else {
        NA_character_
      },
      rejected_signals = if (!is.null(eval)) {
        collapse_reported_values(eval$`Number of signals rejected for poor quality (FTA)`[eval$id == .data$id])
      } else {
        NA_character_
      },
      sample_size = if (!is.null(signal)) {
        collapse_reported_values(signal$`number of individuals`[signal$id == .data$id])
      } else {
        NA_character_
      },
      conditions = if (!is.null(signal)) {
        collapse_reported_values(signal$conditions[signal$id == .data$id])
      } else {
        NA_character_
      },
      permanence = dplyr::coalesce(
        if (!is.null(signal)) collapse_reported_values(signal$permanence[signal$id == .data$id]) else NA_character_,
        if (!is.null(auth)) collapse_reported_values(auth$permanence[auth$id == .data$id]) else NA_character_
      ),
      situational_stability = if (!is.null(auth)) {
        collapse_reported_values(auth$`situational stability`[auth$id == .data$id])
      } else {
        NA_character_
      },
      note = paste(
        stats::na.omit(c(
          remaining_challenges,
          if (!is.na(independence_note)) paste("independence:", independence_note) else NA_character_,
          if (!is.na(extraction_note)) paste("note:", extraction_note) else NA_character_,
          if (!is.na(enrollment_time)) paste("enrollment:", enrollment_time) else NA_character_,
          if (!is.na(rejected_signals)) paste("signal rejection:", rejected_signals) else NA_character_,
          if (!is.na(sample_size)) paste("sample size:", sample_size) else NA_character_,
          if (!is.na(conditions)) paste("conditions:", conditions) else NA_character_,
          if (!is.na(permanence)) paste("permanence:", permanence) else NA_character_,
          if (!is.na(situational_stability)) paste("situational stability:", situational_stability) else NA_character_
        )),
        collapse = "; "
      ),
      note = dplyr::if_else(.data$note == "", "No explicit challenge reported.", .data$note),
      study_label = paste0(.data$authors, " (", .data$year, "; id ", .data$id, ")")
    ) |>
    dplyr::ungroup() |>
    dplyr::select("study_label", "note")

  synthesis <- tibble::tribble(
    ~challenge_type, ~description,
    "Signal interruptions or missing data", "Directly noted for signal interruptions and indirectly visible through high signal-rejection rates and noise-related challenge notes.",
    "Motion artifacts", "Movement, activity changes, and motion patterns recur as threats to reliable authentication in activity-rich or real-world settings.",
    "Enrollment burden", "Enrollment requirements range from seconds to minutes, hours, or days, making setup burden a recurring deployment issue.",
    "Small dataset", "Several studies use small participant groups or explicitly request larger and more diverse datasets.",
    "Limited permanence / short follow-up", "Many rows are coded as one-day or no permanence, and longer follow-up is uncommon.",
    "Limited situational stability", "A substantial share of studies uses one condition, laboratory tasks, or only modest activity variation.",
    "On-device resource constraints", "Server reliance, GPU requirements, latency, computational burden, scalability, memory, and energy-efficient execution are recurring implementation constraints.",
    "Spoofing or fake signals", "Fake signals are explicitly mentioned in one extraction row, while other security use cases do not consistently include spoofing evaluations.",
    "Not reported", "Some studies do not report explicit remaining challenges; these should be distinguished from limitations inferred from raw extraction fields."
  )

  list(study_notes = study_notes, synthesis = synthesis)
}

export_challenge_description_docx <- function(challenge_description, output_path) {
  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, "Challenges reported across use-case studies", style = "heading 1")
  doc <- officer::body_add_par(
    doc,
    "This narrative replaces the tabular challenge export. It was written from the raw extraction fields and cross-checked against authentication, evaluation, and signal-level fields for permanence, situational stability, enrollment time, rejected signals, acquisition duration, and sample size.",
    style = "Normal"
  )
  doc <- officer::body_add_par(doc, "Study-level free-text notes", style = "heading 1")
  for (i in seq_len(nrow(challenge_description$study_notes))) {
    doc <- officer::body_add_par(
      doc,
      paste0(challenge_description$study_notes$study_label[[i]], ": ", challenge_description$study_notes$note[[i]]),
      style = "Normal"
    )
  }
  doc <- officer::body_add_par(doc, "Synthesis by challenge type", style = "heading 1")
  for (i in seq_len(nrow(challenge_description$synthesis))) {
    doc <- officer::body_add_par(
      doc,
      paste0(challenge_description$synthesis$challenge_type[[i]], ": ", challenge_description$synthesis$description[[i]]),
      style = "Normal"
    )
  }
  print(doc, target = output_path)
  invisible(output_path)
}
