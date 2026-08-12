#!/usr/bin/env Rscript
# Finalize pre-specified predictor codings and cell-count tables for Phase 3.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

args <- commandArgs(FALSE)
script_path <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
if (is.na(script_path)) script_path <- "code/meta-analysis/finalize_predictor_coding.R"
root <- normalizePath(file.path(dirname(script_path), "../.."), mustWork = FALSE)
if (!dir.exists(file.path(root, "results", "meta-analysis"))) root <- normalizePath(getwd(), mustWork = TRUE)
out_dir <- file.path(root, "results", "meta-analysis")
docs_dir <- file.path(root, "docs")
missing_values <- c("", "NA", "N/A", "na", "not described", "?", "unclear")
modality_flags <- c("ECG", "PPG", "SCG", "EDA", "VHF", "microphone", "HR", "GSR", "BVP", "breathing")

#' Normalize a key component for predictor-coding joins
#'
#' @param v Vector to normalize.
#'
#' @returns Character vector with missing values blanked and whitespace squished.
norm_key_part <- function(v) str_squish(ifelse(is.na(as.character(v)), "", as.character(v)))
#' Build signal-table join keys
#'
#' @param d Data frame containing `id`, `experience`, and `modality`.
#'
#' @returns Character vector of composite signal keys.
signal_join_key <- function(d) do.call(paste, c(lapply(d[c("id", "experience", "modality")], norm_key_part), sep = "\r"))
#' Parse yes/no flags
#'
#' @param v Vector of raw flag values.
#'
#' @returns Logical vector indicating yes/true/1 values.
yn <- function(v) str_to_lower(str_trim(ifelse(is.na(as.character(v)), "", as.character(v)))) %in% c("1", "yes", "true")
#' Parse true-user counts for predictor coding
#'
#' @param x Character scalar describing user counts.
#'
#' @returns Integer count or `NA_integer_`.
parse_true_users <- function(x) {
  if (is.na(x) || str_trim(x) %in% missing_values) return(NA_integer_)
  s <- str_to_lower(x)
  m <- str_match(s, "(\\d+)\\s*(?:true\\s*)?(?:users?|participants?|subjects?|patients?)\\b")[,2]
  if (!is.na(m)) return(as.integer(m))
  nums <- str_extract_all(s, "\\d+")[[1]]
  if (length(nums)) as.integer(nums[1]) else NA_integer_
}
#' Classify a row into meta-analysis modality family
#'
#' @param r One-row data frame or named list with modality flags.
#'
#' @returns One of `ECG`, `PPG`, or `Other`.
modality_family <- function(r) if (yn(r[["ECG"]])) "ECG" else if (yn(r[["PPG"]])) "PPG" else "Other"
#' Infer modality family from a modality string
#'
#' @param s Raw modality string.
#'
#' @returns One of `ECG`, `PPG`, or `Other`.
modality_from_string <- function(s) {
  parts <- str_split(ifelse(is.na(s), "", s), "[/,+;]| and ")[[1]]
  has_ecg <- any(str_detect(str_to_upper(parts), "ECG")); has_ppg <- any(str_detect(str_to_upper(parts), "PPG|BVP"))
  if (has_ecg) "ECG" else if (has_ppg) "PPG" else "Other"
}
#' Determine signal-derived multimodality
#'
#' @param r One-row signal data frame or named list.
#'
#' @returns Logical indicating more than one signal modality.
signal_multimodal <- function(r) {
  if ("multimodal" %in% names(r) && !(str_trim(r[["multimodal"]]) %in% missing_values)) return(yn(r[["multimodal"]]))
  sum(vapply(c("ECG", "PPG", "SCG", "EDA", "VHF", "microphone", "HR", "breathing"), function(c) c %in% names(r) && yn(r[[c]]), logical(1))) > 1
}
#' Parse authentication-coded multimodality
#'
#' @param r One-row authentication data frame or named list.
#'
#' @returns `TRUE`, `FALSE`, or `NA` for unclear coding.
auth_multimodal <- function(r) {
  v <- str_to_lower(str_trim(r[[intersect(c("Multimodal", "multimodal"), names(r))[1]]]))
  if (v %in% c("yes", "1", "true")) TRUE else if (v %in% c("no", "0", "false")) FALSE else NA
}
#' Recode permanence for predictor-count tables
#'
#' @param v Raw permanence value.
#' @param collapse Logical; collapse medium and high permanence when `TRUE`.
#'
#' @returns Character permanence category or `NA_character_`.
permanence <- function(v, collapse = FALSE) {
  if (is.na(v) || v %in% missing_values) return(NA_character_)
  if (v == "low (one day)") return(v)
  if (v %in% c("medium (multiple days)", "high (> one week)")) return(if (collapse) "medium/high (> one day)" else v)
  NA_character_
}
#' Recode intruder status for predictor-count tables
#'
#' @param v Raw external-validation value.
#'
#' @returns Character intruder category or `NA_character_`.
unseen <- function(v) {
  s <- str_to_lower(str_trim(ifelse(is.na(v), "", v)))
  if (s %in% missing_values || str_detect(s, "not described|unclear|\\?")) return(NA_character_)
  if (str_detect(s, "no intruder|no unseen") || s == "no") return("no intruders")
  if (str_detect(s, "intruder|imposter|impostor|non-user|non-patient")) return("intruders")
  NA_character_
}
#' Count experiments and studies by predictor category
#'
#' @param data Meta-analysis data containing `id`.
#' @param predictor Predictor name to report.
#' @param values Category values to count.
#'
#' @returns Tibble of experiment and study counts by category.
count_table <- function(data, predictor, values) {
  v <- values; v[is.na(v)] <- "Missing"; v[v == ""] <- "Missing"
  tibble(id = data$id, category = v) %>% group_by(category) %>% summarize(experiment_count = n(), study_count = n_distinct(id), .groups = "drop") %>% mutate(predictor = predictor, .before = 1) %>% arrange(category)
}
#' Count predictor categories at experiment and study levels
#'
#' @param data Meta-analysis data containing `id`.
#' @param predictor Predictor name to report.
#' @param values Category values to count.
#'
#' @returns Tibble of counts by predictor, level, and category.
category_counts_by_level <- function(data, predictor, values) {
  v <- values; v[is.na(v)] <- "Missing"; v[v == ""] <- "Missing"
  exp <- tibble(category = v) %>% count(category, name = "count") %>% mutate(predictor = predictor, level = "experiment", .before = 1)
  stu <- tibble(id = data$id, category = v) %>% distinct() %>% count(category, name = "count") %>% mutate(predictor = predictor, level = "study", .before = 1)
  bind_rows(exp, stu) %>% arrange(level, category)
}

ma <- read_csv(file.path(out_dir, "ma_data.csv"), show_col_types = FALSE, col_types = cols(.default = col_character()))
sig <- read_csv(file.path(root, "data", "signal.csv"), show_col_types = FALSE, col_types = cols(.default = col_character()))
auth <- read_csv(file.path(root, "data", "auth.csv"), show_col_types = FALSE, col_types = cols(.default = col_character()))
sig_by_key <- split(sig, signal_join_key(sig))
for (i in seq_len(nrow(ma))) {
  sr <- sig_by_key[[signal_join_key(ma[i,])]]
  if (!is.null(sr)) for (c in c("permanence", modality_flags)) if (c %in% names(sr) && (!c %in% names(ma) || is.na(ma[[c]][i]) || ma[[c]][i] == "")) ma[[c]][i] <- sr[[c]][1]
}
true_users <- vapply(ma$number_of_individuals, parse_true_users, integer(1)); logs <- log(true_users[!is.na(true_users) & true_users > 0])
center <- mean(logs); scale <- sd(logs)
write_csv(tibble(parameter = c("log_true_users_center", "log_true_users_scale", "rows_with_parsed_true_users"), value = c(sprintf("%.10f", center), sprintf("%.10f", scale), as.character(length(logs)))), file.path(out_dir, "predictor_scaling_parameters.csv"))
row_list <- split(ma, seq_len(nrow(ma)))
mod_vals <- vapply(row_list, function(r) if (any(modality_flags %in% names(r))) modality_family(r) else modality_from_string(r$modality), character(1))
tables <- bind_rows(
  count_table(ma, "log_true_users_scaled", ifelse(is.na(true_users), NA, "parsed")),
  count_table(ma, "modality_ecg_ppg_other", mod_vals),
  count_table(ma, "conditions", ifelse(ma$conditions %in% missing_values, NA, ma$conditions)),
  count_table(ma, "permanence_3_level", vapply(ma$permanence, permanence, character(1))),
  count_table(ma, "permanence_collapsed", vapply(ma$permanence, permanence, character(1), collapse = TRUE)),
  count_table(ma, "algorithm_family", ifelse(ma$algorithm_family %in% missing_values, NA, ma$algorithm_family)),
  count_table(ma, "intruders", vapply(ma$external_validation, unseen, character(1))),
  count_table(ma, "feature_type", ifelse(ma$feature_type %in% missing_values, NA, ma$feature_type))
)
write_csv(tables, file.path(out_dir, "predictor_cell_counts.csv"))
lvl <- bind_rows(
  category_counts_by_level(ma, "modality_ecg_ppg_other", mod_vals),
  category_counts_by_level(ma, "conditions", ifelse(ma$conditions %in% missing_values, NA, ma$conditions)),
  category_counts_by_level(ma, "permanence_3_level", vapply(ma$permanence, permanence, character(1))),
  category_counts_by_level(ma, "algorithm_family", ifelse(ma$algorithm_family %in% missing_values, NA, ma$algorithm_family))
)
write_csv(lvl, file.path(out_dir, "predictor_category_counts_by_level.csv"))
sigmm <- table(vapply(split(sig, seq_len(nrow(sig))), function(r) if (signal_multimodal(r)) "multimodal" else "unimodal", character(1)))
authmm <- table(vapply(split(auth, seq_len(nrow(auth))), function(r) { x <- auth_multimodal(r); if (isTRUE(x)) "multimodal" else if (identical(x, FALSE)) "unimodal" else "Missing" }, character(1)))
write_csv(bind_rows(tibble(source="signal_derived", category=names(sigmm), experiment_count=as.integer(sigmm)), tibble(source="auth_coded", category=names(authmm), experiment_count=as.integer(authmm))), file.path(out_dir, "multimodality_source_comparison.csv"))
doc <- sprintf('# Final predictor-coding specification (Phase 3)\n\nGenerated from `results/meta-analysis/ma_data.csv`, `data/signal.csv`, and `data/auth.csv`.\n\n## Approved codings\n\n- **Number of true users:** parse the number before user/participant/subject/patient labels where available; for mixed strings such as users plus intruders, retain the true-user count and ignore intruder counts. Analyze as `log(true_users)`, centered at **%.6f** and scaled by **%.6f**. Store these values in `predictor_scaling_parameters.csv`.\n- **Modality:** use cleaned binary indicators where available; analysis family is pre-specified as **ECG vs PPG vs Other**. Keep **multimodal status** as a separate predictor.\n- **Multimodality:** signal-derived multimodality is preferred over authentication-coded multimodality because it follows the cleaned signal indicators. Because multimodal rows are sparse, pre-specify it for primary single-variable modeling and test its behavior in candidate multivariable models before deciding whether to retain it in the final primary multivariable model or move it to sensitivity/descriptive reporting.\n- **Algorithm class:** derive meta-analysis `algorithm_family` from the same `code_algorithm()` function used by the study tables. Keep `dl`, `dl_temp`, `tree`, and `kernel` as separate classes; collapse the lower-count table classes `lm`, `dist`, and `prob` to `Other algorithm` only for meta-analysis.\n- **Conditions/activity:** use current `conditions` with levels `one condition`, `multiple conditions`, and `everyday activities`; `NA` is missing and is handled by multilevel multiple imputation.\n- **Permanence/duration:** use current `permanence`. Fit a univariate three-level model first; if high permanence is unstable, collapse medium and high into `medium/high (> one day)` for primary multivariable modeling and keep three levels descriptively/sensitivity only.\n- **Intruders:** use the existing external-validation intruder information collapsed to `intruders`, `no intruders`, or missing. Unclear, not-described, and mixed uncertainty labels are collapsed to missing.\n- **Feature type:** approve only for a single-variable predictor. Filtering, normalization, segmentation, and dimensionality reduction are descriptive only because they are sparse and not consistently quantitative.\n- **Authentication/enrollment time:** descriptive/exploratory only unless a transparent recode separates actual time from sample/window/heartbeat quantities.\n', center, scale)
writeLines(doc, file.path(docs_dir, "predictor-coding-specification.md"))
