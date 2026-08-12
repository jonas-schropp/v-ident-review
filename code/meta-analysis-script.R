# Meta-analysis pipeline
#
# This script mirrors `code/analysis-script.R` for the meta-analysis workflow.
# It builds the canonical meta-analysis dataset from existing cleaned extraction
# data, imputes analysis data, runs single-predictor and model-based Bayesian
# meta-analysis models, calculates model summaries/metrics, and exports figures
# and tables for publication.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(here)
  library(ggplot2)  # figures section calls ggsave() at top level
})

# Compile Stan models into a stable, git-ignored directory rather than the
# volatile per-session R tempdir, so executables persist across sessions and a
# single folder can be allow-listed in antivirus if it false-flags freshly
# compiled binaries (a known cmdstanr issue on Windows).
options(cmdstanr_write_stan_file_dir = here::here("ma-models", ".stan-cache"))
dir.create(here::here("ma-models", ".stan-cache"), recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# Meta-analysis functions
# -----------------------------------------------------------------------------

ma_files <- c(
  "freeze_ma_dataset.R",
  "ma_imputation.R",
  "brms-helpers.R",
  "fit-ma.R",
  "ma-loo.R",
  "tidy_ma_summary.R"
)
lapply(here::here("code", "meta-analysis", ma_files), source)
source(here::here("code", "tables", "export_table.R"))
source(here::here("code", "tables", "apa.R"))
source(here::here("code", "tables", "ma-tables.R"))

# -----------------------------------------------------------------------------
# Canonical meta-analysis dataset
# -----------------------------------------------------------------------------

ma_bundle <- build_ma_data(
  data_dir = here::here("data"),
  se_decisions_path = here::here("data", "meta-analysis", "standard_error_decisions.csv")
)
ma_data <- write_ma_data_outputs(
  ma_bundle,
  out_dir = here::here("results", "meta-analysis")
)

# Finalize pre-specified predictor coding diagnostics and documentation.
# This script writes predictor cell counts, scaling parameters, multimodality
# source comparison, and `docs/predictor-coding-specification.md`.
source(here::here("code", "meta-analysis", "finalize_predictor_coding.R"))

viz_dir <- here::here("code", "data-visualization")
viz_files <- list.files(viz_dir, pattern = "\\.R$", full.names = TRUE)
lapply(viz_files, source)

# -----------------------------------------------------------------------------
# Multiple imputation and within/between effect coding for clustered predictors.
# Keep this list explicit so that variables carried in `ma_data` for reporting are
# not silently used as imputation predictors. `Other` is intentionally omitted
# from the imputation model because ECG/PPG/Other are deterministic modality
# dummies; with an intercept, ECG and PPG identify Other as the reference group.
imputation_variables <- c(
  'id', 'logn', 'Other', 'PPG', 'ECG', 'multimodal',
  'algo', 'activity', 'duration', 'intruders', 'feature_type', 'ylogit'
)

# -2 = cluster id
# 1 = target variables containing missing data
# 2 = predictors with fixed effect on all targets (completely observed)
# 3 = predictors with random effect on all targets (completely observed)
# 0 = variables not featured in the model
imputation_type <- c(
  id         = -2,
  logn       = 1,
  Other = 0, PPG = 2, ECG = 2, multimodal = 2,
  algo = 2,
  activity   = 1,      # nominal 3-level
  duration   = 1,      # ordered 3-level
  intruders = 1, # nominal 2-level approved sensitivity predictor
  feature_type = if (
    "feature_type" %in% names(ma_data) && anyNA(ma_data$feature_type)
  ) 1 else 2,
  ylogit       = 2 # include the outcome as auxiliary
)

imputation_engine <- "jomo"  # alternatives: "jomo", "mice"
imputation_mice_maxit <- 20
imputation_use_ylogit_within_between <- FALSE

ma_imputation <- switch(
  imputation_engine,
  jomo = impute_ma(
    ma_data,
    m = 20,
    variables = imputation_variables,
    type = imputation_type,
    n.iter = 100,
    seed = 1,
    n.burn = 5000,
    check_collinearity = TRUE
  ),
  mice = impute_ma_mice(
    ma_data,
    m = 2,
    variables = imputation_variables,
    type = imputation_type,
    maxit = imputation_mice_maxit,
    seed = 1,
    donors = 5,
    check_collinearity = TRUE,
    use_ylogit_within_between = imputation_use_ylogit_within_between
  ),
  stop("Unsupported imputation engine: ", imputation_engine, call. = FALSE)
) 

readr::write_rds(
  ma_imputation, 
  here::here("data", "meta-analysis", "ma_imputed_data.rds")
  )

# -----------------------------------------------------------------------------
# Read Data for Meta Analysis
# -----------------------------------------------------------------------------

ma_imputation <- readr::read_rds(
  here::here("data", "meta-analysis", "ma_imputed_data.rds")
)

midat <- add_effects(
  completed_ma_data(
    ma_imputation
    )
  )

# -----------------------------------------------------------------------------
# ICC diagnostics for within/between predictor coding
# -----------------------------------------------------------------------------

icc_specs <- list(
  multimodal = c("multimodal", "factor"),
  intruders = c("intruders", "factor"),
  multiC = c("multiC", "factor"),
  everyC = c("everyC", "factor"),
  multiD = c("multiD", "factor"),
  manyD = c("manyD", "factor"),
  logn = c("logn", "continuous"),
  ECG = c("ECG", "factor"),
  PPG = c("PPG", "factor"),
  dl = c("dl", "factor"),
  dl_temp = c("dl_temp", "factor"),
  tree = c("tree", "factor"),
  kernel = c("kernel", "factor")
)

iccs <- lapply(icc_specs, function(spec) {
  pool_icc(midat, var = spec[[1]], id = "id", type = spec[[2]], transform = "logit")
})

icc_table <- bind_rows(lapply(names(iccs), function(nm) {
  x <- iccs[[nm]]
  tibble(
    predictor = nm,
    estimate = x$estimate,
    conf_low = x$conf_low,
    conf_high = x$conf_high,
    df = x$df,
    M_eff = x$M_eff,
    Ubar = x$Ubar,
    B = x$B,
    T = x$T
  )
}))

icc_table_export <- icc_table %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

attr(icc_table_export, "flextable_header_labels") <- c(
  predictor = "Predictor",
  estimate = "ICC",
  conf_low = "95% CI LL",
  conf_high = "95% CI UL",
  df = "df",
  M_eff = "Effective m",
  Ubar = "Within-imputation variance",
  B = "Between-imputation variance",
  T = "Total variance"
)
attr(icc_table_export, "table_footer") <- paste(
  "Note. ICC = intraclass correlation coefficient; CI = confidence interval;",
  "LL = lower limit; UL = upper limit. Values are rounded to two significant digits."
)

write_table_csv_docx(
  icc_table_export,
  here::here("results", "meta-analysis", "icc_diagnostics.csv")
)




# -----------------------------------------------------------------------------
# Single-predictor and model-based meta-analysis
# -----------------------------------------------------------------------------

summary_artifact_dir <- here::here("results", "meta-analysis")

summary_artifact_path <- function(kind, model) {
  file.path(summary_artifact_dir, paste0(model, "_", kind, ".csv"))
}

# Fitted models are cached under ma-models/ (git-ignored, local disk only, never
# pushed). Each model's compiled Stan program is precompiled once and saved as
# <model>.compiled.rds, and the fitted per-imputation objects are saved as
# <model>.rds. On rerun, fit_diagnose_combine_ma() reloads each model from disk
# instead of recompiling and resampling; every downstream use below (diagnostics,
# LOO, summaries, plots, tables) therefore operates on these disk-backed fits.
ma_models_dir <- here::here("ma-models")
dir.create(ma_models_dir, recursive = TRUE, showWarnings = FALSE)
ma_model_path <- function(model) file.path(ma_models_dir, paste0(model, ".rds"))

save_ma_summary_artifacts <- function(model, ma_model, loo) {
  readr::write_csv(ma_model$diagnostics, summary_artifact_path("diagnostics", model))
  readr::write_csv(format_ma_loo(loo, model_name = model), summary_artifact_path("loo", model))
  invisible(model)
}


f_sigma <- as.formula("sigma ~ 1 + logn + activity + duration + multimodal")

# Null model.
f_null <- "ylogit | se(se_logit, sigma = TRUE) ~ 1 + (1|id)"
ma_null <- fit_diagnose_combine_ma(
  midat, f_mean = f_null, 
  f_sigma = f_sigma, iter = 4000, file = ma_model_path("null")
  )
fit_null_components <- ma_null$fits
fit_null_diagnostics <- ma_null$diagnostics
fit_null <- ma_null$fit
# Legacy combined-fit workflow used only the first imputed data set:
# loo_null <- loo(fit_null, resp = "ylogit", re_formula = NA)
loo_null <- loo_ma_fits(fit_null_components, resp = "ylogit", re_formula = NA)
save_ma_summary_artifacts("null", ma_null, loo_null)

# Activity model.
# Symmetric within/between (Mundlak) coding: both non-reference levels get a
# within- and a between-study term. Activity is the one predictor with real
# within-study variation (varies within 8/23 studies), and multiC in particular
# (ICC ~0.47), so a random slope is retained only on multiC_within. everyC barely
# varies within study (within-study SD ~0.03), so no random slope on everyC_within.
f_activity <- "ylogit | se(se_logit, sigma = TRUE) ~
  1 + multiC_within + multiC_between + everyC_within + everyC_between +
  (1 + multiC_within | id)"
ma_activity <- fit_diagnose_combine_ma(midat, f_mean = f_activity, f_sigma = f_sigma, iter = 4000, file = ma_model_path("activity"))
fit_activity_components <- ma_activity$fits
fit_activity_diagnostics <- ma_activity$diagnostics
fit_activity <- ma_activity$fit
# Legacy combined-fit workflow used only the first imputed data set:
# loo_activity <- loo(fit_activity, resp = "ylogit", re_formula = NA)
loo_activity <- loo_ma_fits(fit_activity_components, resp = "ylogit", re_formula = NA)
save_ma_summary_artifacts("activity", ma_activity, loo_activity)

# Duration model.
# duration varies within only 3/23 studies, so a random slope on it is not
# identifiable; use a fixed (between-study) duration effect + study random intercept.
f_duration <- "ylogit | se(se_logit, sigma = TRUE) ~
  1 + duration + (1 | id)"
ma_duration <- fit_diagnose_combine_ma(midat, f_mean = f_duration, f_sigma = f_sigma, iter = 4000, file = ma_model_path("duration"))
fit_duration_components <- ma_duration$fits
fit_duration_diagnostics <- ma_duration$diagnostics
fit_duration <- ma_duration$fit
# Legacy combined-fit workflow used only the first imputed data set:
# loo_duration <- loo(fit_duration, resp = "ylogit", re_formula = NA)
loo_duration <- loo_ma_fits(fit_duration_components, resp = "ylogit", re_formula = NA)
save_ma_summary_artifacts("duration", ma_duration, loo_duration)

# Modality model.
# Modality is a study-level attribute: ECG/PPG vary within only 2-3/23 studies
# (ICC ~0.95-0.99). Random slopes on cluster-invariant predictors are confounded
# with the random intercept, so keep ECG/PPG as fixed (between-study) effects.
f_modality <- "ylogit | se(se_logit, sigma = TRUE) ~
  1 + ECG + PPG + (1 | id)"
ma_modality <- fit_diagnose_combine_ma(midat, f_mean = f_modality, f_sigma = f_sigma, iter = 4000, file = ma_model_path("modality"))
fit_modality_components <- ma_modality$fits
fit_modality_diagnostics <- ma_modality$diagnostics
fit_modality <- ma_modality$fit
# Legacy combined-fit workflow used only the first imputed data set:
# loo_modality <- loo(fit_modality, resp = "ylogit", re_formula = NA)
loo_modality <- loo_ma_fits(fit_modality_components, resp = "ylogit", re_formula = NA)
save_ma_summary_artifacts("modality", ma_modality, loo_modality)

# Algorithm-family model.
# algo varies within only 6/23 studies; a 5-level random slope (a 5x5 RE
# covariance, 10 correlations) is badly over-parameterized for 23 studies. Use a
# fixed algorithm-family effect + study random intercept.
f_algorithm <- "ylogit | se(se_logit, sigma = TRUE) ~
  1 + algo + (1 | id)"
ma_algorithm <- fit_diagnose_combine_ma(midat, f_mean = f_algorithm, f_sigma = f_sigma, iter = 4000, file = ma_model_path("algorithm"))
fit_algorithm_components <- ma_algorithm$fits
fit_algorithm_diagnostics <- ma_algorithm$diagnostics
fit_algorithm <- ma_algorithm$fit
# Legacy combined-fit workflow used only the first imputed data set:
# loo_algorithm <- loo(fit_algorithm, resp = "ylogit", re_formula = NA)
loo_algorithm <- loo_ma_fits(fit_algorithm_components, resp = "ylogit", re_formula = NA)
save_ma_summary_artifacts("algorithm", ma_algorithm, loo_algorithm)

# Multimodality model.
# multimodal varies within only 3/23 studies; random slope not identifiable.
f_multimodal <- "ylogit | se(se_logit, sigma = TRUE) ~
  1 + multimodal + (1 | id)"
# iter raised to 6000: at 4000 a minority of imputations missed the strict
# convergence thresholds (marginal R-hat/ESS, no divergences).
ma_multimodal <- fit_diagnose_combine_ma(midat, f_mean = f_multimodal, f_sigma = f_sigma, iter = 6000, file = ma_model_path("multimodal"))
fit_multimodal_components <- ma_multimodal$fits
fit_multimodal_diagnostics <- ma_multimodal$diagnostics
fit_multimodal <- ma_multimodal$fit
# Legacy combined-fit workflow used only the first imputed data set:
# loo_multimodal <- loo(fit_multimodal, resp = "ylogit", re_formula = NA)
loo_multimodal <- loo_ma_fits(fit_multimodal_components, resp = "ylogit", re_formula = NA)
save_ma_summary_artifacts("multimodal", ma_multimodal, loo_multimodal)

# Intruders sensitivity model.
# intruders varies within only 1/23 studies (ICC ~0.96); random slope not
# identifiable, so use a fixed (between-study) effect + study random intercept.
f_intruders <- "ylogit | se(se_logit, sigma = TRUE) ~
  1 + intruders + (1 | id)"
# iter raised to 6000 (see multimodal note).
ma_intruders <- fit_diagnose_combine_ma(midat, f_mean = f_intruders, f_sigma = f_sigma, iter = 6000, file = ma_model_path("intruders"))
fit_intruders_components <- ma_intruders$fits
fit_intruders_diagnostics <- ma_intruders$diagnostics
fit_intruders <- ma_intruders$fit
# Legacy combined-fit workflow used only the first imputed data set:
# loo_intruders <- loo(fit_intruders, resp = "ylogit", re_formula = NA)
loo_intruders <- loo_ma_fits(fit_intruders_components, resp = "ylogit", re_formula = NA)
save_ma_summary_artifacts("intruders", ma_intruders, loo_intruders)
# Feature-type single-predictor model.
# Run only when each coded feature-type category has adequate study-level
# representation in the finalized predictor cell-count diagnostics.
feature_type_counts <- readr::read_csv(
  here::here("results", "meta-analysis", "predictor_cell_counts.csv"),
  show_col_types = FALSE
) %>%
  dplyr::filter(.data$predictor == "feature_type", .data$category != "Missing")

run_feature_type_model <- nrow(feature_type_counts) >= 2 &&
  all(feature_type_counts$study_count >= 3) &&
  "feature_type" %in% names(midat[[1]])

if (run_feature_type_model) {
  # feature_type never varies within a study (0/23), so a random slope on it is
  # estimated purely from the prior. Use a fixed effect + study random intercept.
  f_feature_type <- "ylogit | se(se_logit, sigma = TRUE) ~
    1 + feature_type + (1 | id)"
  ma_feature_type <- fit_diagnose_combine_ma(midat, f_mean = f_feature_type, f_sigma = f_sigma, iter = 4000, file = ma_model_path("feature_type"))
  fit_feature_type_components <- ma_feature_type$fits
  fit_feature_type_diagnostics <- ma_feature_type$diagnostics
  fit_feature_type <- ma_feature_type$fit
  # Legacy combined-fit workflow used only the first imputed data set:
  # loo_feature_type <- loo(fit_feature_type, resp = "ylogit", re_formula = NA)
  loo_feature_type <- loo_ma_fits(fit_feature_type_components, resp = "ylogit", re_formula = NA)
  save_ma_summary_artifacts("feature_type", ma_feature_type, loo_feature_type)
}

# Full model.
# The previous full random-effect covariance (a 12-dimensional RE, 66
# correlations over 23 studies) was massively over-parameterized and predicted
# far worse than the null (LOO elpd_diff ~ -184). All predictors are
# predominantly between-study, so enter them as fixed effects with a single
# study random intercept (standard multilevel meta-regression).
# Activity enters with the same within/between (Mundlak) coding as the
# single-predictor activity model rather than as a raw factor. Activity is the one
# moderator with real within-study variation (8/23 studies, ICC ~0.47), so the
# decomposition is the meaningful specification; using it here also keeps the
# adjusted and marginal activity estimates directly comparable.
f_full <- "ylogit | se(se_logit, sigma = TRUE) ~ 1 +
  ECG + PPG + algo + multimodal + duration +
  multiC_within + multiC_between + everyC_within + everyC_between +
  (1 | id)"
# iter raised to 6000 (see multimodal note).
ma_full <- fit_diagnose_combine_ma(midat, f_mean = f_full, f_sigma = f_sigma, iter = 6000, file = ma_model_path("full"))
fit_full_components <- ma_full$fits
fit_full_diagnostics <- ma_full$diagnostics
fit_full <- ma_full$fit
# Legacy combined-fit workflow used only the first imputed data set:
# loo_full <- loo(fit_full, resp = "ylogit", re_formula = NA)
loo_full <- loo_ma_fits(fit_full_components, resp = "ylogit", re_formula = NA)
save_ma_summary_artifacts("full", ma_full, loo_full)

model_names <- c(
  "null", "activity", "duration", "modality", "algorithm", "multimodal",
  "intruders", "full"
)
if (run_feature_type_model) {
  model_names <- append(model_names, "feature_type", after = 6)
}

# Reassemble collective analysis objects in memory. The fitted brms objects are
# cached on local disk under ma-models/ (git-ignored, never pushed because they are
# too large for the repository); only downstream summary tables and figures are
# written to results/. The in-memory `fit_*` objects below were reloaded from those
# cached files whenever the models had already been fit.
fits <- list(
  null = fit_null,
  activity = fit_activity,
  duration = fit_duration,
  modality = fit_modality,
  algorithm = fit_algorithm,
  multimodal = fit_multimodal,
  intruders = fit_intruders,
  full = fit_full
)
loos <- list(
  null = loo_null,
  activity = loo_activity,
  duration = loo_duration,
  modality = loo_modality,
  algorithm = loo_algorithm,
  multimodal = loo_multimodal,
  intruders = loo_intruders,
  full = loo_full
)
if (run_feature_type_model) {
  fits <- append(fits, list(feature_type = fit_feature_type), after = 6)
  loos <- append(loos, list(feature_type = loo_feature_type), after = 6)
}
fits <- fits[model_names]
loos <- loos[model_names]

loo_comparisons <- dplyr::bind_rows(lapply(setdiff(names(loos), "null"), function(nm) {
  compare_ma_loo(
    loos[[nm]],
    loos$null,
    cluster = midat[[1]]$id,
    model_names = c(nm, "null")
  )
}))
loo_table <- dplyr::bind_rows(lapply(names(loos), function(nm) {
  format_ma_loo(loos[[nm]], model_name = nm)
}))
readr::write_csv(loo_table, here::here("results", "meta-analysis", "loo_summary.csv"))
readr::write_csv(loo_comparisons, here::here("results", "meta-analysis", "loo_comparison.csv"))

# -----------------------------------------------------------------------------
# Metrics and publication tables
# -----------------------------------------------------------------------------

summary_specs <- list(
  null = NULL,
  activity = "activity",
  duration = "duration",
  modality = c("ECG", "PPG"),
  algorithm = "algo",
  multimodal = "multimodal",
  intruders = "intruders",
  feature_type = "feature_type",
  full = NULL
)

model_summaries <- lapply(names(fits), function(nm) {
  tidy_ma_summary(
    fits[[nm]],
    data = midat[[1]],
    by = summary_specs[[nm]],
    fit_null = fit_null
  )
})
names(model_summaries) <- names(fits)

heterogeneity_table <- bind_rows(lapply(names(model_summaries), function(nm) {
  model_summaries[[nm]]$heterogeneity %>% mutate(model = nm, .before = 1)
}))

r2_table <- bind_rows(lapply(names(model_summaries), function(nm) {
  x <- model_summaries[[nm]]$r2_meta
  if (is.null(x)) return(NULL)
  x %>% mutate(model = nm, .before = 1)
}))

prediction_table <- bind_rows(lapply(names(model_summaries), function(nm) {
  model_summaries[[nm]]$predictions %>% mutate(model = nm, .before = 1)
}))

readr::write_csv(heterogeneity_table, here::here("results", "meta-analysis", "heterogeneity_metrics.csv"))
readr::write_csv(r2_table, here::here("results", "meta-analysis", "r2_meta.csv"))
readr::write_csv(prediction_table, here::here("results", "meta-analysis", "posterior_predictions.csv"))

# -----------------------------------------------------------------------------
# APA publication tables (CSV + DOCX; see code/tables/apa.R + ma-tables.R)
# -----------------------------------------------------------------------------

# Single-predictor constructs with a registered effect spec (drives both the
# predicted-accuracy table and the forest/raincloud figures). Reused verbatim by
# the sensitivity analyses.
forest_constructs <- intersect(names(ma_effect_specs), names(fits))

accuracy_summary <- dplyr::bind_rows(lapply(forest_constructs, function(cn) {
  ma_summarise_accuracy(ma_construct_accuracy(fits[[cn]], midat[[1]], cn))
}))

# Same levels re-predicted from the full model, i.e. adjusted for the other
# moderators. Built exactly as ma_effect_forest_all() builds its hollow points, so
# the table and the forest figure report the same two quantities; reporting only the
# univariate column invites the two to be read as contradicting each other.
adjusted_constructs <- ma_full_model_constructs(fits$full, forest_constructs, midat[[1]])
adjusted_summary <- dplyr::bind_rows(lapply(adjusted_constructs, function(cn) {
  ma_summarise_accuracy(ma_construct_accuracy(fits$full, midat[[1]], cn))
}))

ma_table_predicted_accuracy(
  accuracy_summary,
  here::here("results", "meta-analysis", "predicted_accuracy_table.csv"),
  adjusted_summary = adjusted_summary
)
readr::write_csv(
  dplyr::full_join(
    dplyr::rename(accuracy_summary, marg_med = median, marg_lo = lower, marg_hi = upper),
    dplyr::rename(adjusted_summary, adj_med = median, adj_lo = lower, adj_hi = upper),
    by = c("construct", ".level")
  ),
  here::here("results", "meta-analysis", "predicted_accuracy_marginal_adjusted.csv")
)
ma_table_heterogeneity(
  heterogeneity_table, r2_table,
  here::here("results", "meta-analysis", "heterogeneity_table.csv")
)
ma_table_loo(
  loo_table, loo_comparisons,
  here::here("results", "meta-analysis", "loo_table.csv")
)

# Convergence diagnostics summary across all fitted models.
diagnostics_by_model <- setNames(
  lapply(model_names, function(nm) get(paste0("ma_", nm))$diagnostics),
  model_names
)
ma_table_diagnostics(
  diagnostics_by_model,
  here::here("results", "meta-analysis", "diagnostics_table.csv")
)

# -----------------------------------------------------------------------------
# Figures (colour-blind pastel; theme + palette in code/data-visualization/ma-theme.R)
# -----------------------------------------------------------------------------

figure_dir <- here::here("results", "figures")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# Forest: model-predicted accuracy (median + 95% CrI) for every moderator level,
# with a dashed reference line at the null-model grand mean.
plt_forest <- ma_effect_forest_all(
  fits[forest_constructs], midat[[1]],
  constructs = forest_constructs, fit_null = fit_null,
  full_fit = fits$full
)
# Saved as a LZW-compressed TIFF (lossless, ~100x smaller than the uncompressed
# ragg default). The figure carries no embedded note; the manuscript note is
# written alongside as ma_forest_effects_note.txt for the Word caption.
ma_save_forest(
  plt_forest,
  file.path(figure_dir, "ma_forest_effects.tiff"),
  width = 8, height = 9.5, overlay = !is.null(fits$full)
)

# Rainclouds: half-violin + half-boxplot of the predicted-accuracy posterior per
# level, one figure per construct.
for (cn in forest_constructs) {
  plt_rc <- ma_raincloud(
    ma_construct_accuracy(fits[[cn]], midat[[1]], cn),
    title = paste0("Predicted accuracy by ", tolower(ma_construct_labels[[cn]]))
  )
  ggsave(
    file.path(figure_dir, paste0("ma_raincloud_", cn, ".tiff")),
    plot = plt_rc, width = 7, height = 4.5, dpi = 300, bg = "white",
    device = "tiff", compression = "lzw"
  )
}
