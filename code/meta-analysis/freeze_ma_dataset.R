#!/usr/bin/env Rscript
# Build the canonical meta-analysis dataset from the cleaned RDS exports.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || is.na(x[1])) y else x
script_path <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1] %||% ".")
root <- normalizePath(file.path(dirname(script_path), "../.."), mustWork = FALSE)
if (!dir.exists(file.path(root, "data"))) root <- normalizePath(getwd(), mustWork = TRUE)
source(file.path(root, "code", "data-cleaning", "code-auth.R"))
data_dir <- file.path(root, "data")
out_dir <- file.path(root, "results", "meta-analysis")
epsilon <- 1e-6
key_cols <- c("id", "experience")
signal_predictors <- c("ECG", "EDA", "PPG", "SCG", "VHF", "microphone", "multimodal")
na_values <- c("", "NA", "N/A", "na", "n/a", "not reported", "NR", "nr", "None", "none")

#' Normalize extracted meta-analysis cell values
#'
#' @param x Vector of raw extracted values.
#'
#' @returns Character vector with missing values blanked, trailing `.0` removed from integer-like numbers, and whitespace squished.
norm_value <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- ifelse(grepl("^[+-]?[0-9]+\\.0$", x), sub("\\.0$", "", x), x)
  str_squish(x)
}
#' Detect normalized missing-value labels
#'
#' @param x Vector of raw extracted values.
#'
#' @returns Logical vector indicating values treated as missing in the meta-analysis freeze.
is_missing <- function(x) norm_value(x) %in% na_values
#' Read a cleaned RDS table for meta-analysis assembly
#'
#' @param path Path to a cleaned RDS export.
#'
#' @returns Data frame preserving non-syntactic column names.
read_table <- function(path) {
  as.data.frame(readRDS(path), stringsAsFactors = FALSE, check.names = FALSE)
}
#' Build id-experience join keys
#'
#' @param df Data frame containing the canonical key columns.
#'
#' @returns Character vector of row keys used to join cleaned extraction tables.
key_of_df <- function(df) do.call(paste, c(df[key_cols], sep = "\r"))
#' Parse a percentage value and parenthesized uncertainty
#'
#' @param value Raw extracted value, optionally containing parenthesized uncertainty.
#'
#' @returns Named numeric vector with `mean` and `uncertainty` components.
parse_uncertainty <- function(value) {
  text <- norm_value(value)
  if (text %in% na_values) return(c(mean = NA_real_, uncertainty = NA_real_))
  clean <- gsub(",", "", text)
  nums <- str_extract(clean, "[-+]?\\d+(?:\\.\\d+)?")
  mean <- suppressWarnings(as.numeric(nums))
  unc <- str_match(clean, "\\(([-+]?\\d+(?:\\.\\d+)?)")[,2]
  c(mean = mean, uncertainty = suppressWarnings(as.numeric(unc)))
}
#' Parse the leading percentage value
#'
#' @param value Raw extracted percentage value.
#'
#' @returns Numeric percentage mean, or `NA_real_` when unavailable.
parse_percent <- function(value) parse_uncertainty(value)[["mean"]]

#' Collapse sparse algorithm classes for meta-analysis
#'
#' @param algorithm_class Character vector of cleaned algorithm classes.
#'
#' @returns Character vector with sparse classes collapsed to `Other algorithm`.
collapse_algorithm_class_for_meta <- function(algorithm_class) {
  case_when(
    algorithm_class %in% c("lm", "dist", "prob") ~ "Other algorithm",
    is.na(algorithm_class) | algorithm_class == "" ~ NA_character_,
    TRUE ~ algorithm_class
  )
}


#' Classify and transform an extracted authentication outcome
#'
#' @param row Named list or data-frame row from the evaluation table.
#'
#' @returns List of outcome source, inclusion, probability, boundary correction, and logit fields.
classify_outcome <- function(row) {
  acc <- parse_percent(row[["accuracy"]]); eer <- parse_percent(row[["EER"]])
  has_other <- !is_missing(row[["F1"]]) || !is_missing(row[["AUC"]])
  source <- "excluded"; pct <- NA_real_; reason <- "no usable outcome information"
  if (!is.na(acc) && acc >= 0 && acc <= 100) { source <- "direct accuracy"; pct <- acc; reason <- "included" }
  else if (!is.na(eer) && eer >= 0 && eer <= 100) { source <- "converted EER"; pct <- 100 - eer; reason <- "included; accuracy missing and EER is a valid percentage" }
  else if (has_other) { source <- "other"; reason <- "excluded; only non-accuracy/non-EER outcome available" }
  prob <- if (is.na(pct)) NA_real_ else pct / 100
  corrected <- if (is.na(prob)) NA_real_ else min(max(prob, epsilon), 1 - epsilon)
  list(accuracy_percent_raw_parsed = acc, eer_percent_raw_parsed = eer, outcome_source = source,
       outcome_percent = pct, outcome_probability = prob,
       boundary_epsilon = if (!is.na(corrected) && corrected != prob) epsilon else NA_real_,
       outcome_probability_corrected = corrected,
       outcome_logit = if (is.na(corrected)) NA_real_ else qlogis(corrected),
       inclusion_status = if (!is.na(pct)) "included" else "excluded",
       exclusion_reason = if (!is.na(pct)) "" else reason, outcome_decision_note = reason)
}

#' Read manual standard-error decisions
#'
#' @param path Path to the standard-error decision CSV.
#'
#' @returns List of decision rows split by study id.
read_se_decisions <- function(path) {
  rows <- read_csv(path, show_col_types = FALSE, col_types = cols(.default = col_character())) %>% mutate(across(everything(), norm_value))
  split(rows, rows$id)
}
#' Classify and transform reported outcome uncertainty
#'
#' @param row Named list or data-frame row from the evaluation table.
#' @param outcome Outcome classification returned by `classify_outcome()`.
#' @param decisions Standard-error decisions returned by `read_se_decisions()`.
#'
#' @returns List of standard-error provenance and probability/logit SE fields.
classify_standard_error <- function(row, outcome, decisions) {
  acc <- parse_uncertainty(row[["accuracy"]]); eer <- parse_uncertainty(row[["EER"]])
  source <- outcome$outcome_source
  reported <- if (source == "direct accuracy") acc[["uncertainty"]] else if (source == "converted EER") eer[["uncertainty"]] else NA_real_
  reported_source <- if (source == "direct accuracy" && !is.na(acc[["uncertainty"]])) "accuracy" else if (source == "converted EER" && !is.na(eer[["uncertainty"]])) "EER" else ""
  dec <- decisions[[norm_value(row[["id"]])]]
  if (is.na(reported)) { category <- "no reported uncertainty"; note <- "No parenthesized uncertainty was reported in the selected accuracy/EER field."; denom <- NA_real_; primary <- sensitivity <- FALSE }
  else if (is.null(dec)) { category <- "unclear reporting"; note <- "Parenthesized uncertainty was reported, but no manual audit decision accepts it as usable."; denom <- NA_real_; primary <- sensitivity <- FALSE }
  else { category <- dec$se_provenance_category; note <- dec$se_decision_note; denom <- suppressWarnings(as.numeric(dec$se_conversion_denominator)); primary <- dec$include_se_primary == "1"; sensitivity <- dec$include_se_sensitivity == "1" }
  candidate <- if (is.na(reported)) NA_real_ else if (!is.na(denom) && denom != 0) reported / sqrt(denom) else reported
  to_logit <- function(percent) if (is.na(percent) || is.na(outcome$outcome_probability_corrected)) NA_real_ else (percent / 100) / (outcome$outcome_probability_corrected * (1 - outcome$outcome_probability_corrected))
  list(uncertainty_reported_source = reported_source, uncertainty_reported_percent = reported,
       se_provenance_category = category, se_conversion_denominator = denom,
       se_probability_primary = if (primary) candidate / 100 else NA_real_, se_logit_primary = if (primary) to_logit(candidate) else NA_real_, include_se_primary = as.integer(primary && !is.na(candidate)),
       se_probability_sensitivity = if (sensitivity) candidate / 100 else NA_real_, se_logit_sensitivity = if (sensitivity) to_logit(candidate) else NA_real_, include_se_sensitivity = as.integer(sensitivity && !is.na(candidate)),
       uncertainty_exclusion_flag = as.integer(!primary && !is.na(reported)), se_decision_note = note)
}


#' Parse the number of true users
#'
#' @param x Character vector describing users, participants, subjects, or patients.
#'
#' @returns Integer vector of parsed true-user counts.
parse_true_users <- function(x) {
  x <- as.character(x)
  out <- rep(NA_integer_, length(x))
  missing <- is.na(x) | trimws(x) %in% c("", "NA", "N/A", "na", "not described", "?", "not reported")
  text <- tolower(trimws(x))
  m <- regexec("(\\d+)\\s*(?:true\\s*)?(?:users?|participants?|subjects?|patients?)\\b", text)
  hits <- regmatches(text, m)
  for (i in seq_along(hits)) {
    if (length(hits[[i]]) > 1) out[[i]] <- as.integer(hits[[i]][[2]])
  }
  fallback <- is.na(out) & !missing
  nums <- regmatches(text[fallback], regexpr("\\d+", text[fallback]))
  out[fallback] <- suppressWarnings(as.integer(nums))
  out
}

#' Convert yes/true flags to binary integers
#'
#' @param x Vector of raw binary-like values.
#'
#' @returns Integer vector coded 1 for yes/true/1 and 0 otherwise.
as_binary <- function(x) {
  case_when(
    is.na(x) ~ 0L,
    tolower(trimws(as.character(x))) %in% c("1", "yes", "true") ~ 1L,
    TRUE ~ 0L
  )
}

#' Convert extracted values to a factor with missing labels removed
#'
#' @param x Vector of raw values.
#' @param levels Allowed factor levels.
#' @param ordered Logical; create an ordered factor when `TRUE`.
#'
#' @returns Factor or ordered factor with standardized missing values set to `NA`.
factor_if_present <- function(x, levels, ordered = FALSE) {
  x <- as.character(x)
  x[x %in% c("", "NA", "N/A", "na", "not reported", "not described", "?")] <- NA_character_
  if (ordered) ordered(x, levels = levels) else factor(x, levels = levels)
}

#' Recode external validation to intruder status
#'
#' @param x Character vector of external-validation descriptions.
#'
#' @returns Character vector with `intruders`, `no intruders`, or `NA`.
recode_intruders <- function(x) {
  text <- str_to_lower(str_trim(ifelse(is.na(x), "", as.character(x))))
  case_when(
    text %in% c("", "na", "n/a", "not reported", "not described", "?", "unclear") ~ NA_character_,
    str_detect(text, "not described|unclear|\\?") ~ NA_character_,
    str_detect(text, "no intruders?|no unseen") | text == "no" ~ "no intruders",
    str_detect(text, "intruders?|impost[eo]rs?|non-users?|non-patients?") ~ "intruders",
    TRUE ~ NA_character_
  )
}

#' Derive analysis-ready variables for the meta-analysis data set
#'
#' @param ma Included raw meta-analysis rows.
#' @param eps Boundary epsilon used for probability clipping.
#'
#' @returns Data frame of modeled meta-analysis variables with scaled log user counts.
derive_analysis_variables <- function(ma, eps = epsilon) {
  res <- ma %>%
    mutate(
      id = factor(.data$id),
      ECG = as_binary(.data$ECG),
      EDA = as_binary(.data$EDA),
      PPG = as_binary(.data$PPG),
      SCG = as_binary(.data$SCG),
      VHF = as_binary(.data$VHF),
      microphone = as_binary(.data$microphone),
      multimodal = as_binary(.data$multimodal),
      n = parse_true_users(.data$number_of_individuals),
      logn = log(.data$n),
      activity = factor_if_present(
        .data$conditions,
        levels = c("one condition", "multiple conditions", "everyday activities")
      ),
      duration = factor_if_present(
        .data$permanence,
        levels = c("low (one day)", "medium (multiple days)", "high (> one week)"),
        ordered = TRUE
      ),
      auth_time = factor_if_present(
        .data$authentication_time,
        levels = sort(unique(as.character(.data$authentication_time)))
      ),
      enrol_time = factor_if_present(
        .data$enroll_time,
        levels = sort(unique(as.character(.data$enroll_time)))
      ),
      intruders = factor(
        recode_intruders(.data$external_validation),
        levels = c("no intruders", "intruders")
      ),
      algo = factor(
        .data$algorithm_family,
        levels = c("Other algorithm", "dl", "dl_temp", "tree", "kernel")
      ),
      algo_other = .data$algorithm,
      Other = if_else(.data$VHF == 1L | .data$microphone == 1L |
                        .data$SCG == 1L | .data$EDA == 1L, 1L, 0L),
      mean = pmin(pmax(.data$outcome_probability, eps), 1 - eps),
      ylogit = .data$outcome_logit,
      se = .data$se_probability_primary,
      se2 = .data$se_probability_sensitivity,
      se_logit = if_else(is.na(.data$se_logit_primary), 0, .data$se_logit_primary),
      has_se = .data$include_se_primary,
      se2_logit = if_else(is.na(.data$se_logit_sensitivity), 0, .data$se_logit_sensitivity),
      has_se2 = .data$include_se_sensitivity
    ) %>%
    filter(!is.na(.data$mean)) %>%
    select(
      any_of(c("experiment_id", "source_row", "stable_key")),
      id,
      any_of("experience"),
      any_of(c("modality", "conditions", "permanence", "algorithm_family")),
      any_of("number_of_individuals"),
      n,
      logn,
      any_of(c(
        "outcome_source", "outcome_percent", "outcome_probability",
        "outcome_probability_corrected", "boundary_epsilon",
        "outcome_decision_note", "uncertainty_reported_source",
        "uncertainty_reported_percent", "se_provenance_category",
        "se_conversion_denominator", "uncertainty_exclusion_flag",
        "se_decision_note"
      )),
      mean,
      ylogit,
      se,
      se2,
      se_logit,
      has_se,
      se2_logit,
      has_se2,
      ECG,
      PPG,
      Other,
      multimodal,
      algo,
      algo_other,
      intruders,
      activity,
      duration,
      auth_time,
      enrol_time,
      any_of(c(
        "device", "device_location", "feature_type", "feature_dim",
        "dim_reduction", "noise_reduction", "segmentation_type",
        "normalizationyn", "filteringyn", "external_validation"
      ))
    )

  if (all(is.na(res$logn))) {
    res$logn <- NA_real_
  } else {
    res$logn <- as.numeric(scale(res$logn))
  }

  res
}

#' Build the canonical meta-analysis data bundle
#'
#' @param data_dir Directory containing cleaned extraction RDS files.
#' @param se_decisions_path Path to standard-error decision CSV.
#' @param eps Boundary epsilon used for outcome probabilities.
#'
#' @returns List containing analysis data, excluded rows, provenance, summaries, and source inputs.
build_ma_data <- function(
    data_dir = file.path("data"),
    se_decisions_path = file.path(data_dir, "meta-analysis", "standard_error_decisions.csv"),
    eps = epsilon
) {
  inputs <- c(eval="eval.rds", signal="signal.rds", auth="auth.rds", features="features.rds", preproc="preproc.rds")
  tables <- lapply(file.path(data_dir, inputs), read_table)
  names(tables) <- names(inputs)
  tables$auth <- code_algorithm(tables$auth) %>%
    mutate(algorithm_family = collapse_algorithm_class_for_meta(.data$algorithm_class))
  for (nm in names(tables)) {
    d <- duplicated(key_of_df(tables[[nm]]))
    if (any(d)) stop(nm, " contains duplicate id/experience keys")
  }
  lookups <- lapply(tables[names(tables) != "eval"], function(df) split(df, key_of_df(df)))
  se_decisions <- read_se_decisions(se_decisions_path)
  rows <- list(); excluded <- list()
  for (i in seq_len(nrow(tables$eval))) {
    ev <- as.list(tables$eval[i,]); k <- paste(unlist(ev[key_cols]), collapse="\r")
    getrow <- function(nm) if (!is.null(lookups[[nm]][[k]])) as.list(lookups[[nm]][[k]][1,]) else list()
    sig <- getrow("signal"); auth <- getrow("auth"); feat <- getrow("features"); prep <- getrow("preproc")
    val <- function(x, name) norm_value(x[[name]] %||% "")
    algorithm <- val(auth, "algorithm")
    algorithm_family <- val(auth, "algorithm_family")
    if (algorithm_family == "") algorithm_family <- NA_character_
    clean_part <- function(x) gsub("^-|-$", "", gsub("[^A-Za-z0-9]+", "-", x))
    outcome <- classify_outcome(ev); se <- classify_standard_error(ev, outcome, se_decisions)
    row <- c(list(experiment_id=sprintf("%s_%s", val(ev,"id"), clean_part(val(ev,"experience") %||% "NA")), source_row=i, id=val(ev,"id"), experience=val(ev,"experience"), modality=val(ev,"modality"), stable_key=paste(val(ev,"id"), val(ev,"experience"), sep="|"), algorithm=algorithm, algorithm_family=algorithm_family, algorithm_details=val(auth,"Algorithm Details"), number_of_individuals=val(sig,"number of individuals"), conditions=val(sig,"conditions"), device=val(sig,"device"), sampling_frequency=val(sig,"sampling frequency"), number_of_channels_electrodes=val(sig,"number of channels / electrodes"), acquisition_time=val(sig,"acquisition time"), duration=val(sig,"acquisition time")), setNames(lapply(signal_predictors, function(p) val(sig,p)), signal_predictors), list(age_and_gender_reported=val(sig,"age and gender reported"), device_location=val(sig,"device location"), permanence=val(sig,"permanence"), authentication_time=val(ev,"authentication time"), enroll_time=val(ev,"Enroll time"), internal_validation=val(ev,"internal"), external_validation=val(ev,"external"), accuracy_raw=val(ev,"accuracy"), eer_raw=val(ev,"EER"), f1_raw=val(ev,"F1"), auc_raw=val(ev,"AUC"), features_raw=val(feat,"Features"), feature_type=val(feat,"feature_type"), feature_dim=val(feat,"feature_dim"), dim_reduction=val(feat,"dim_reduction"), noise_reduction=val(prep,"noise reduction"), segmentation=val(prep,"segmentation"), segmentation_type=val(prep,"segmentation_type"), normalization=val(prep,"normalization"), normalizationyn=val(prep,"normalizationyn"), filteringyn=val(prep,"filteringyn")), outcome, se)
    if (row$inclusion_status == "included") rows[[length(rows)+1]] <- row else excluded[[length(excluded)+1]] <- row
  }

  included_raw <- bind_rows(lapply(rows, as.data.frame))
  excluded_raw <- bind_rows(lapply(excluded, as.data.frame))
  provenance <- bind_rows(lapply(c(rows, excluded), as.data.frame))
  ma_data <- derive_analysis_variables(included_raw, eps = eps)

  list(
    ma_data = ma_data,
    included_raw = included_raw,
    excluded = excluded_raw,
    standard_error_provenance = provenance,
    outcome_source_summary = as.data.frame(table(outcome_source = provenance$outcome_source)) %>% rename(rows = Freq),
    inputs = tables
  )
}

#' Write canonical meta-analysis outputs
#'
#' @param ma_bundle Bundle returned by `build_ma_data()`.
#' @param out_dir Directory for RDS, CSV, and dictionary outputs.
#'
#' @returns Invisibly returns the analysis-ready meta-analysis data frame.
write_ma_data_outputs <- function(ma_bundle, out_dir = file.path("results", "meta-analysis")) {
  dir.create(out_dir, recursive=TRUE, showWarnings=FALSE)
  saveRDS(ma_bundle$ma_data, file.path(out_dir, "ma_data.rds"))
  write_csv(ma_bundle$ma_data, file.path(out_dir, "ma_data.csv"), na="")
  saveRDS(ma_bundle$excluded, file.path(out_dir, "row_exclusion_log.rds"))
  write_csv(ma_bundle$excluded, file.path(out_dir, "row_exclusion_log.csv"), na="")
  saveRDS(ma_bundle$standard_error_provenance, file.path(out_dir, "standard_error_provenance.rds"))
  write_csv(ma_bundle$standard_error_provenance, file.path(out_dir, "standard_error_provenance.csv"), na="")
  write_csv(ma_bundle$outcome_source_summary, file.path(out_dir,"outcome_source_summary.csv"))

  fields <- names(ma_bundle$ma_data)
  dict <- tibble(variable=fields, source="canonical meta-analysis data", definition="Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding.")
  write_csv(dict, file.path(out_dir,"ma_data_dictionary.csv"))
  md <- c("# Meta-analysis data dictionary", "", "Inputs: `data/eval.rds`, `data/signal.rds`, `data/auth.rds`, `data/features.rds`, and `data/preproc.rds`. RDS files are preferred because they preserve R types; CSV mirrors are written only as inspection/export artifacts.", "", sprintf("Eligible experiment rows are the %s unique `id` + `experience` rows in `data/eval.rds`; `id` identifies the study and `experience` identifies the experiment within that study. Rows are included only when accuracy can be obtained directly or from valid EER conversion.", nrow(ma_bundle$inputs$eval)), "", "`ma_data.rds` is the single canonical analysis-ready meta-analysis data set used for imputation and modeling. Standard-error provenance is written to `standard_error_provenance.rds` and mirrored to `standard_error_provenance.csv`.", "", "| Variable | Source | Definition |", "|---|---|---|", sprintf("| `%s` | %s | %s |", dict$variable, dict$source, dict$definition))
  writeLines(md, file.path(out_dir,"ma_data_dictionary.md"))
  invisible(ma_bundle$ma_data)
}
