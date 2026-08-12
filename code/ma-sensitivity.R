# Sensitivity analyses for the Bayesian meta-analysis
# =============================================================================
# Mirrors code/meta-analysis-script.R but perturbs ONE pre-registered element at
# a time, reusing the same fitting / diagnostics / LOO / table / figure machinery.
# Protocol sensitivity analyses:
#   (i)  Likelihood: Gaussian, or Student-t with nu fixed = 4
#        (main analysis: Student-t with nu estimated).
#   (ii) Sigma model with/without study-level random effects and/or design
#        covariates (main analysis: design covariates, no RE). The three
#        remaining cells of the 2x2 grid are fit here.
#   (iv) SE handling: additionally include studies whose reported SEs are of
#        unknown origin (the `se2_logit` sensitivity SE set; 45 vs 33 studies).
#   (v)  Complete-case analysis (no imputation) for models whose predictors carry
#        missing values (intruders 14%, duration 4%, activity 1%).
# (iii) feature-set handling is not repeated here: sparse categories were already
#       collapsed during data preparation (algorithm "Other algorithm"), so it is
#       covered by the main coding.
#
# Only the perturbed element changes; everything else (formulas, priors, seed,
# iterations, effect coding) matches the main analysis. Fits cache to
# ma-models/sensitivity/<variant>/ (git-ignored); tables/figures write to
# results/meta-analysis/sensitivity/<variant>/ and results/figures/sensitivity/.
#
# RUNTIME: fitting every variant over all models on 20 imputations is a very long
# job (tens of hours). It is resumable — cached models reload instantly — so edit
# `sensitivity_to_run` / `sensitivity_models` below to run a subset at a time.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(here)
  library(ggplot2)
  library(brms)
})

# Compile Stan models into a stable, git-ignored directory rather than the
# volatile per-session R tempdir. Benefits: executables persist across sessions
# (faster reruns), and a single folder can be allow-listed in antivirus if it
# false-flags freshly compiled binaries (a known cmdstanr issue on Windows).
options(cmdstanr_write_stan_file_dir = here::here("ma-models", ".stan-cache"))
dir.create(here::here("ma-models", ".stan-cache"), recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# Shared machinery (identical to the main analysis)
# -----------------------------------------------------------------------------
ma_files <- c(
  "freeze_ma_dataset.R", "ma_imputation.R", "brms-helpers.R",
  "fit-ma.R", "ma-loo.R", "tidy_ma_summary.R"
)
lapply(here::here("code", "meta-analysis", ma_files), source)
source(here::here("code", "tables", "export_table.R"))
source(here::here("code", "tables", "apa.R"))
source(here::here("code", "tables", "ma-tables.R"))
source(here::here("code", "tables", "ma-sensitivity-tables.R"))
invisible(lapply(
  list.files(here::here("code", "data-visualization"), pattern = "\\.R$", full.names = TRUE),
  source
))

# -----------------------------------------------------------------------------
# Data (same frozen imputations as the main analysis; original data for complete-case)
# -----------------------------------------------------------------------------
ma_imputation <- readr::read_rds(here::here("data", "meta-analysis", "ma_imputed_data.rds"))
midat   <- add_effects(completed_ma_data(ma_imputation))
ma_data <- readRDS(here::here("results", "meta-analysis", "ma_data.rds"))

# -----------------------------------------------------------------------------
# Baseline model specification (mirrors code/meta-analysis-script.R)
# The mean-model right-hand sides are kept separately from the response so the SE
# sensitivity (iv) can swap the se() variable without touching the moderators.
# -----------------------------------------------------------------------------
ma_moderator_rhs <- list(
  null         = "1 + (1 | id)",
  activity     = "1 + multiC_within + multiC_between + everyC_within + everyC_between + (1 + multiC_within | id)",
  duration     = "1 + duration + (1 | id)",
  modality     = "1 + ECG + PPG + (1 | id)",
  algorithm    = "1 + algo + (1 | id)",
  multimodal   = "1 + multimodal + (1 | id)",
  intruders    = "1 + intruders + (1 | id)",
  feature_type = "1 + feature_type + (1 | id)",
  # Activity uses the same within/between coding as the single-predictor activity
  # model (see meta-analysis-script.R) so adjusted and marginal activity estimates
  # are comparable and the coding is consistent across models.
  full         = "1 + ECG + PPG + algo + multimodal + duration + multiC_within + multiC_between + everyC_within + everyC_between + (1 | id)"
)
ma_f_sigma_main <- "sigma ~ 1 + logn + activity + duration + multimodal"

# `by` grid for tidy_ma_summary(), and predictors used per model for complete-case.
ma_summary_by <- list(
  null = NULL, activity = "activity", duration = "duration", modality = c("ECG", "PPG"),
  algorithm = "algo", multimodal = "multimodal", intruders = "intruders",
  feature_type = "feature_type", full = NULL
)
ma_model_predictors <- list(
  null = character(0), activity = "activity", duration = "duration",
  modality = c("ECG", "PPG"), algorithm = "algo", multimodal = "multimodal",
  intruders = "intruders", feature_type = "feature_type",
  full = c("ECG", "PPG", "algo", "multimodal", "duration", "activity")
)
# Per-model iterations, matching the main analysis (weak models used 6000).
ma_model_iter <- function(model) if (model %in% c("multimodal", "intruders", "full")) 6000L else 4000L

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------

#' Assemble a mean-model formula from a moderator RHS and a chosen SE variable.
build_f_mean <- function(rhs, se_var = "se_logit") {
  sprintf("ylogit | se(%s, sigma = TRUE) ~ %s", se_var, rhs)
}

# Does a formula (RHS) contain fixed-effect predictors beyond the intercept / RE?
.has_fixed_terms <- function(rhs) {
  x <- gsub("\\([^()]*\\)", "", rhs)                 # drop grouping "(... | id)" terms
  terms <- trimws(strsplit(x, "\\+")[[1]])
  length(setdiff(terms, c("", "0", "1"))) > 0
}
.has_re <- function(f) grepl("\\|", f)

#' Build the pre-registered prior set, adapted to the mean/sigma structure and
#' likelihood of a given variant. Reproduces the main analysis prior exactly for
#' the main configuration (Student-t, covariate sigma without RE).
#'
#' @param rhs Mean-model right-hand side.
#' @param f_sigma Sigma sub-model formula.
#' @param family "student" or "gaussian".
#' @param nu NULL to estimate nu (Student-t); a number to fix it (e.g. 4).
build_ma_prior <- function(rhs, f_sigma, family = c("student", "gaussian"), nu = NULL) {
  family <- match.arg(family)
  p <- c(
    prior(normal(0, 1),        class = "Intercept"),
    prior(student_t(3, 0, 0.5), class = "sd")
  )
  if (.has_fixed_terms(rhs)) p <- c(p, prior(normal(0, 0.35), class = "b"))
  p <- c(p, prior(normal(-1, 1), class = "Intercept", dpar = "sigma"))
  if (.has_fixed_terms(sub(".*~", "", f_sigma))) {
    p <- c(p, prior(normal(0, 0.5), class = "b", dpar = "sigma"))
  }
  if (.has_re(f_sigma)) {
    p <- c(p, prior(student_t(3, 0, 0.5), class = "sd", dpar = "sigma"))
  }
  if (family == "student") {
    p <- c(p, if (is.null(nu)) prior(exponential(1), class = "nu")
              else brms::set_prior(paste0("constant(", nu, ")"), class = "nu"))
  }
  p
}

# brms exports student(); gaussian() comes from stats. Both resolve with brms attached.
ma_family_obj <- function(kind) switch(kind, student = brms::student(), gaussian = stats::gaussian())

# Covariates in the (main) sigma sub-model. Complete-case analysis must be
# complete on these too, since every model includes them in the sigma model.
ma_sigma_covariates <- c("logn", "activity", "duration", "multimodal")

#' Recodings for protocol sensitivity analysis (iii), feature-set handling.
#'
#' The protocol asks for rare categories to be collapsed or dropped and the
#' models re-estimated. Sparse *algorithm* classes were already collapsed once
#' during data preparation, which is a fixed coding decision rather than a
#' robustness check, so these functions provide the alternative codings that are
#' actually re-estimated. Rarity is judged at the STUDY level, which is what
#' matters with 23 clusters.
#'
#' `feature_type` has only 3 contributing studies in the "hybrid" cell and 5 in
#' "deep"; collapsing to handcrafted vs any-deep-involving retains all
#' experiments while removing the thin cell.
#' @param d A completed data frame.
#' @returns `d` with the recoded column.
ma_recode_feature_collapse <- function(d) {
  x <- as.character(d$feature_type)
  d$feature_type <- factor(
    ifelse(is.na(x), NA_character_, ifelse(x == "handcrafted", "handcrafted", "deep-based")),
    levels = c("handcrafted", "deep-based")
  )
  d
}

#' `duration` has 2 studies in "high (> one week)" and 3 in "medium (multiple
#' days)". This collapses to the pre-prepared two-level contrast (one day vs
#' longer). Note duration also appears in the sigma sub-model of every model, so
#' this recoding changes all models, not just the duration model.
#' @rdname ma_recode_feature_collapse
ma_recode_duration_collapse <- function(d) {
  x <- as.character(d$duration)
  d$duration <- ordered(
    ifelse(is.na(x), NA_character_,
           ifelse(x == "low (one day)", "low (one day)", "medium/high (> one day)")),
    levels = c("low (one day)", "medium/high (> one day)")
  )
  d
}

#' Single common complete-case dataset for a set of models (no imputation).
#'
#' Listwise deletion on the union of the run's mean predictors and the sigma
#' covariates, so every model in the variant shares the same observations. Using
#' one common sample (rather than per-model complete cases) keeps N constant and
#' makes the cross-model LOO comparisons valid. Effect-coded like the imputed data.
common_complete_case_data <- function(models) {
  vars <- intersect(union(unlist(ma_model_predictors[models]), ma_sigma_covariates), names(ma_data))
  d <- ma_data
  if (length(vars)) d <- d[stats::complete.cases(d[, vars, drop = FALSE]), , drop = FALSE]
  add_effects(list(d))[[1]]
}

# -----------------------------------------------------------------------------
# Variant runner: fit a model set under one perturbation, reusing all machinery.
# -----------------------------------------------------------------------------
run_ma_variant <- function(variant, models,
                           se_var = "se_logit", f_sigma = ma_f_sigma_main,
                           family = "student", nu = NULL, data = "imputed",
                           recode = NULL, refit = FALSE) {
  models <- unique(c("null", models))  # null is the LOO / R2_meta reference
  cache_dir <- here::here("ma-models", "sensitivity", variant)
  res_dir   <- here::here("results", "meta-analysis", "sensitivity", variant)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(res_dir,   recursive = TRUE, showWarnings = FALSE)
  fam_obj <- ma_family_obj(family)

  # Complete-case (v) fits one common listwise-complete dataset shared by every
  # model; all other variants use the imputed data. `reference_data` supplies the
  # cluster ids and factor levels for the downstream summaries/figures.
  cc_data <- if (identical(data, "complete_case")) common_complete_case_data(models) else NULL
  # Feature-set sensitivity analyses (iii) supply a `recode` applied to every
  # completed data set after effect coding, so both the mean and sigma sub-models
  # see the alternative category coding.
  imputed_data <- if (is.null(recode)) midat else lapply(midat, recode)
  if (!is.null(recode) && !is.null(cc_data)) cc_data <- recode(cc_data)
  reference_data <- if (is.null(cc_data)) imputed_data[[1]] else cc_data

  fits <- list(); diagnostics <- list(); loos <- list()
  for (m in models) {
    message(sprintf(">>> [%s] fitting model '%s'", variant, m))
    rhs    <- ma_moderator_rhs[[m]]
    f_mean <- build_f_mean(rhs, se_var)
    pri    <- build_ma_prior(rhs, f_sigma, family = family, nu = nu)
    dat    <- if (is.null(cc_data)) imputed_data else cc_data

    ma <- fit_diagnose_combine_ma(
      dat, f_mean = f_mean, f_sigma = f_sigma, iter = ma_model_iter(m),
      file = file.path(cache_dir, paste0(m, ".rds")),
      family = fam_obj, prior = pri, refit = refit
    )
    loo <- loo_ma_fits(ma$fits, resp = "ylogit", re_formula = NA)
    readr::write_csv(ma$diagnostics, file.path(res_dir, paste0(m, "_diagnostics.csv")))
    readr::write_csv(format_ma_loo(loo, model_name = m), file.path(res_dir, paste0(m, "_loo.csv")))
    fits[[m]] <- ma$fit; diagnostics[[m]] <- ma$diagnostics; loos[[m]] <- loo
  }

  write_variant_outputs(variant, models, fits, diagnostics, loos, res_dir, reference_data)
  invisible(list(fits = fits, diagnostics = diagnostics, loos = loos))
}

#' Per-variant APA tables + forest figure, reusing the main-analysis builders.
#' `reference_data` provides the cluster ids and factor levels (imputed midat[[1]]
#' for most variants; the common complete-case dataset for the complete-case variant).
write_variant_outputs <- function(variant, models, fits, diagnostics, loos, res_dir, reference_data) {
  # LOO comparison vs the variant's own null.
  loo_comparisons <- dplyr::bind_rows(lapply(setdiff(names(loos), "null"), function(nm) {
    compare_ma_loo(loos[[nm]], loos$null, cluster = reference_data$id, model_names = c(nm, "null"))
  }))
  loo_table <- dplyr::bind_rows(lapply(names(loos), function(nm) format_ma_loo(loos[[nm]], model_name = nm)))
  readr::write_csv(loo_table, file.path(res_dir, "loo_summary.csv"))
  readr::write_csv(loo_comparisons, file.path(res_dir, "loo_comparison.csv"))

  # Heterogeneity + R2_meta + predictions via tidy_ma_summary().
  summaries <- lapply(models, function(nm) {
    tidy_ma_summary(fits[[nm]], data = reference_data, by = ma_summary_by[[nm]], fit_null = fits$null)
  })
  names(summaries) <- models
  het <- dplyr::bind_rows(lapply(models, function(nm) dplyr::mutate(summaries[[nm]]$heterogeneity, model = nm, .before = 1)))
  r2  <- dplyr::bind_rows(lapply(models, function(nm) {
    x <- summaries[[nm]]$r2_meta; if (is.null(x)) NULL else dplyr::mutate(x, model = nm, .before = 1)
  }))
  readr::write_csv(het, file.path(res_dir, "heterogeneity_metrics.csv"))
  readr::write_csv(r2,  file.path(res_dir, "r2_meta.csv"))

  # APA tables (same builders as the main analysis).
  ma_table_diagnostics(diagnostics, file.path(res_dir, "diagnostics_table.csv"), model_order = models)
  ma_table_heterogeneity(het, r2, file.path(res_dir, "heterogeneity_table.csv"), model_order = models)
  ma_table_loo(loo_table, loo_comparisons, file.path(res_dir, "loo_table.csv"), model_order = models)

  constructs <- intersect(names(ma_effect_specs), models)
  if (length(constructs)) {
    acc <- dplyr::bind_rows(lapply(constructs, function(cn)
      ma_summarise_accuracy(ma_construct_accuracy(fits[[cn]], reference_data, cn))))
    # Same levels re-predicted from this variant's full model, so the table carries the
    # univariate and multivariate columns the forest figure already overlays. Variants
    # that refit only one model (feature_collapse) have no full fit, and fall back to the
    # single-column form.
    adj <- NULL
    if (!is.null(fits$full)) {
      adj_constructs <- ma_full_model_constructs(fits$full, constructs, reference_data)
      if (length(adj_constructs)) {
        adj <- dplyr::bind_rows(lapply(adj_constructs, function(cn)
          ma_summarise_accuracy(ma_construct_accuracy(fits$full, reference_data, cn))))
      }
    }
    ma_table_predicted_accuracy(acc, file.path(res_dir, "predicted_accuracy_table.csv"),
                                adjusted_summary = adj)
    if (!is.null(adj)) {
      readr::write_csv(
        dplyr::full_join(
          dplyr::rename(acc, marg_med = median, marg_lo = lower, marg_hi = upper),
          dplyr::rename(adj, adj_med = median, adj_lo = lower, adj_hi = upper),
          by = c("construct", ".level")
        ),
        file.path(res_dir, "predicted_accuracy_marginal_adjusted.csv")
      )
    }

    fig_dir <- here::here("results", "figures", "sensitivity")
    dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
    plt <- ma_effect_forest_all(fits[constructs], reference_data, constructs = constructs,
                                fit_null = fits$null, full_fit = fits$full)
    plt <- plt + ggplot2::labs(subtitle = paste0("Sensitivity: ", variant))
    # No note inside the figure; a *_note.txt companion is written for the manuscript.
    ma_save_forest(plt, file.path(fig_dir, paste0("ma_forest_", variant, ".tiff")),
                   width = 8, height = 9.5, overlay = !is.null(fits$full))
  }
  invisible(TRUE)
}

# -----------------------------------------------------------------------------
# Variant definitions. Each changes exactly one pre-registered element; all other
# fields fall back to the main-analysis baseline.
# -----------------------------------------------------------------------------
sensitivity_specs <- list(
  # (i) Likelihood
  gaussian        = list(family = "gaussian"),
  student_nu4     = list(family = "student", nu = 4),
  # (ii) Sigma model (main = design covariates, no RE; the other 3 cells here)
  sigma_intercept = list(f_sigma = "sigma ~ 1"),
  sigma_re        = list(f_sigma = "sigma ~ 1 + (1 | id)"),
  sigma_cov_re    = list(f_sigma = "sigma ~ 1 + logn + activity + duration + multimodal + (1 | id)"),
  # (iii) Feature-set handling: collapse rare categories and re-estimate.
  #   feature_collapse only changes the feature_type model (feature_type appears
  #   in no other mean model and not in sigma), so only that model is refit.
  #   duration_collapse changes every model, because duration is in the sigma
  #   sub-model of all of them.
  feature_collapse  = list(recode = ma_recode_feature_collapse, models = "feature_type"),
  duration_collapse = list(recode = ma_recode_duration_collapse),
  # (iv) SE handling: additionally include SEs of unknown origin
  se_unknown      = list(se_var = "se2_logit"),
  # (v) Complete-case analysis (no imputation)
  complete_case   = list(data = "complete_case")
)

# -----------------------------------------------------------------------------
# Execution. Edit these two vectors to control what runs (both default to
# everything, matching the main-analysis style). Runs are resumable via the cache.
# -----------------------------------------------------------------------------
# Default to every variant / every model, but respect values pre-set by a caller
# (e.g. a driver that sources this script) so subsets can be run without edits.
if (!exists("sensitivity_to_run")) sensitivity_to_run <- names(sensitivity_specs)
if (!exists("sensitivity_models")) sensitivity_models <- names(ma_moderator_rhs)

# Guard: set `ma_sensitivity_setup_only <- TRUE` before source() to load the
# functions/data without launching the (long) fitting run. Normal Rscript runs
# leave it unset and execute everything, matching the main-analysis style.
if (exists("ma_sensitivity_setup_only") && isTRUE(ma_sensitivity_setup_only)) {
  message("ma-sensitivity.R sourced in setup-only mode; no models fit.")
} else {

for (v in sensitivity_to_run) {
  spec <- sensitivity_specs[[v]]
  message(sprintf("\n===== SENSITIVITY VARIANT: %s =====", v))
  # Outputs are written to disk inside run_ma_variant(); discard the (large)
  # returned fit objects and free memory before the next variant to avoid
  # accumulating many combined fits across variants.
  run_ma_variant(
    variant = v,
    # A spec may restrict its own model set (e.g. a recoding that only affects
    # one model); otherwise the run-wide selection applies.
    models  = spec$models  %||% sensitivity_models,
    se_var  = spec$se_var  %||% "se_logit",
    f_sigma = spec$f_sigma %||% ma_f_sigma_main,
    family  = spec$family  %||% "student",
    nu      = spec$nu,
    data    = spec$data    %||% "imputed",
    recode  = spec$recode
  )
  gc()
}

# -----------------------------------------------------------------------------
# Cross-variant comparison: between-study SD (tau) per model under the main
# analysis and every sensitivity variant that has been run. This is the headline
# robustness summary (does the heterogeneity conclusion hold?).
# -----------------------------------------------------------------------------
assemble_tau_comparison <- function() {
  read_tau <- function(path, label) {
    if (!file.exists(path)) return(NULL)
    d <- suppressMessages(readr::read_csv(path, show_col_types = FALSE))
    d <- d[d$param == "tau", c("model", "median")]
    stats::setNames(d, c("model", label))
  }
  main <- read_tau(here::here("results", "meta-analysis", "heterogeneity_metrics.csv"), "main")
  out <- main
  # Scan every registered variant, not just the ones run in this session, so a
  # partial run cannot overwrite the table with a subset of the columns.
  for (v in names(sensitivity_specs)) {
    tv <- read_tau(here::here("results", "meta-analysis", "sensitivity", v, "heterogeneity_metrics.csv"), v)
    if (!is.null(tv)) out <- merge(out, tv, by = "model", all = TRUE)
  }
  if (!is.null(out)) {
    out <- out[order(match(out$model, names(ma_moderator_rhs))), ]
    readr::write_csv(out, here::here("results", "meta-analysis", "sensitivity", "tau_comparison.csv"))
  }
  out
}
tau_comparison <- assemble_tau_comparison()
print(tau_comparison)

# Cross-variant supplementary tables (tau, LOO, R2_meta, convergence) as model x
# specification matrices. Reads only the per-variant CSVs written above, so it can also
# be re-run on its own without loading brms:
#   source("code/tables/apa.R"); source("code/tables/ma-tables.R")
#   source("code/tables/ma-sensitivity-tables.R"); ma_write_sensitivity_tables()
ma_write_sensitivity_tables()

message("\n>>> Sensitivity analyses complete.")

}  # end setup-only guard
