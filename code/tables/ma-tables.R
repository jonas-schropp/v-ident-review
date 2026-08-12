#' APA publication tables for the Bayesian meta-analysis
#'
#' Builders that turn the tidy result objects produced by the meta-analysis
#' pipeline into APA-styled DOCX + CSV tables. They take the same in-memory
#' objects the script already computes, so the main analysis and the sensitivity
#' analyses can call them without re-fitting. Rendering is delegated to
#' `write_apa_table()` in `apa.R`.
#'
#' Depends on: dplyr, tidyr, and apa.R.

#' Display labels for models / constructs.
ma_model_labels <- c(
  null = "Null", activity = "Activity", duration = "Duration", modality = "Modality",
  algorithm = "Algorithm family", multimodal = "Multimodality", intruders = "Intruders",
  feature_type = "Feature type", full = "Full"
)

#' Format an estimate with a credible interval, e.g. "88.0 [80.2, 92.3]".
#' @param m,lo,hi Numeric estimate and interval bounds.
#' @param digits Decimal places.
#' @param pct Multiply by 100 (for probabilities shown as percentages).
ma_fmt_est_ci <- function(m, lo, hi, digits = 2, pct = FALSE) {
  mult <- if (pct) 100 else 1
  f <- paste0("%.", digits, "f")
  sprintf(paste0(f, " [", f, ", ", f, "]"), mult * m, mult * lo, mult * hi)
}

.ma_model_label <- function(x) ifelse(x %in% names(ma_model_labels), ma_model_labels[x], x)

#' APA table: model-predicted accuracy (%) by moderator level
#'
#' With `adjusted_summary` supplied the table carries both estimates side by side:
#' the univariate (single-predictor) prediction and the multivariate (full-model)
#' prediction for the same level. That is the pairing the forest figure draws, so
#' table and figure then report the same two quantities and cannot be read as
#' disagreeing. Levels absent from the full model (moderators pre-specified as
#' single-predictor / sensitivity terms) show an em dash.
#'
#' @param accuracy_summary Combined `ma_summarise_accuracy()` output across
#'   constructs: columns `construct`, `.level`, `median`, `lower`, `upper` (0-1).
#' @param csv_path Output CSV path (DOCX written alongside).
#' @param prob CI mass (for the note).
#' @param adjusted_summary Optional same-shaped summary from the full model.
#'
#' @returns Invisibly, the display data.frame.
ma_table_predicted_accuracy <- function(accuracy_summary, csv_path, prob = 0.95,
                                        adjusted_summary = NULL) {
  pct <- round(100 * prob)
  uni <- ma_fmt_est_ci(accuracy_summary$median, accuracy_summary$lower,
                       accuracy_summary$upper, digits = 1, pct = TRUE)
  disp <- data.frame(
    Construct = as.character(accuracy_summary$construct),
    Level     = as.character(accuracy_summary$.level),
    check.names = FALSE
  )
  if (is.null(adjusted_summary)) {
    disp$Accuracy <- uni
    header <- c(Accuracy = sprintf("Predicted accuracy %% [%d%% CrI]", pct))
    note <- sprintf(paste0("Posterior median predicted authentication accuracy for a new study; ",
                           "brackets give the %d%% credible interval. Each estimate comes from a ",
                           "separate single-predictor model. Population-level (marginal-of-study) ",
                           "predictions."), pct)
  } else {
    adj <- as.data.frame(adjusted_summary)
    key <- paste(as.character(accuracy_summary$construct), as.character(accuracy_summary$.level))
    hit <- match(key, paste(as.character(adj$construct), as.character(adj$.level)))
    disp$Univariate <- uni
    disp$Multivariate <- ifelse(
      is.na(hit), "—",
      ma_fmt_est_ci(adj$median[hit], adj$lower[hit], adj$upper[hit], digits = 1, pct = TRUE))
    header <- c(Univariate   = sprintf("Univariate model %% [%d%% CrI]", pct),
                Multivariate = sprintf("Multivariate model %% [%d%% CrI]", pct))
    note <- sprintf(paste0(
      "Posterior median predicted authentication accuracy for a new study; brackets give the ",
      "%d%% credible interval. Univariate estimates come from a separate single-predictor model ",
      "for each construct, so the remaining moderators are not conditioned on. Multivariate ",
      "estimates come from the single model containing sensing modality, algorithm family, ",
      "multimodality, study duration and activity condition, with the other moderators held at ",
      "their reference levels; this is why they sit roughly three to four percentage points below ",
      "the univariate estimates and why the reference level of each construct takes the same ",
      "value, that being the same all-references cell of the model. Intruder validation and ",
      "feature type were pre-specified as single-predictor and sensitivity terms and are not in ",
      "the multivariate model, so no adjusted estimate exists for them (—). Predictions are ",
      "population-level (marginal with respect to the study random effect)."), pct)
  }
  write_apa_table(
    disp, csv_path,
    title = "Model-predicted authentication accuracy by moderator level",
    note = note, header_labels = header, group_cols = "Construct"
  )
  invisible(disp)
}

#' APA table: between-study heterogeneity and R2_meta by model
#'
#' @param heterogeneity_table Tidy table with `model`, `param` (incl. "tau",
#'   "tau2"), `median`, `lower`, `upper`.
#' @param r2_table Tidy table with `model`, `median`, `lower`, `upper` (R2_meta).
#' @param csv_path Output CSV path.
#' @param model_order Optional character vector to order rows.
#'
#' @returns Invisibly, the display data.frame.
ma_table_heterogeneity <- function(heterogeneity_table, r2_table, csv_path, model_order = NULL) {
  het <- as.data.frame(heterogeneity_table)
  tau  <- het[het$param == "tau",  c("model", "median", "lower", "upper")]
  tau2 <- het[het$param == "tau2", c("model", "median", "lower", "upper")]
  tau$tau   <- ma_fmt_est_ci(tau$median,  tau$lower,  tau$upper,  digits = 2)
  tau2$tau2 <- ma_fmt_est_ci(tau2$median, tau2$lower, tau2$upper, digits = 2)
  out <- merge(tau[c("model", "tau")], tau2[c("model", "tau2")], by = "model", all = TRUE)
  if (!is.null(r2_table) && nrow(r2_table) > 0) {
    r2 <- as.data.frame(r2_table)
    r2$r2 <- ma_fmt_est_ci(r2$median, r2$lower, r2$upper, digits = 2)
    out <- merge(out, r2[c("model", "r2")], by = "model", all.x = TRUE)
  } else {
    out$r2 <- NA_character_
  }
  ord <- if (!is.null(model_order)) model_order else names(ma_model_labels)
  out <- out[order(match(out$model, ord)), ]
  disp <- data.frame(
    Model = .ma_model_label(out$model),
    tau = out$tau, tau2 = out$tau2, r2 = ifelse(is.na(out$r2), "—", out$r2),
    check.names = FALSE
  )
  write_apa_table(
    disp, csv_path,
    title = "Between-study heterogeneity and variance explained by model",
    note = paste0("τ = between-study SD (logit scale); τ² = between-study variance; ",
                  "R²_meta = proportion of between-study heterogeneity explained relative to ",
                  "the null model. Brackets give 95% credible intervals."),
    header_labels = c(tau = "τ [95% CrI]", tau2 = "τ² [95% CrI]",
                      r2 = "R²_meta [95% CrI]")
  )
  invisible(disp)
}

#' APA table: leave-one-out model comparison
#'
#' @param loo_table `loo_summary`-style table: `model`, `pooled_elpd`, `pooled_se`,
#'   `max_pareto_k`.
#' @param loo_comparisons `loo_comparison`-style table: `model_a`, `elpd_diff`,
#'   `se_diff` (each row compares a model against the null).
#' @param csv_path Output CSV path.
#' @param model_order Optional row ordering.
#'
#' @returns Invisibly, the display data.frame.
ma_table_loo <- function(loo_table, loo_comparisons, csv_path, model_order = NULL) {
  lt <- as.data.frame(loo_table)
  cmp <- as.data.frame(loo_comparisons)
  ord <- if (!is.null(model_order)) model_order else names(ma_model_labels)
  lt <- lt[order(match(lt$model, ord)), ]
  elpd <- sprintf("%.1f (%.1f)", lt$pooled_elpd, lt$pooled_se)
  diff_map <- setNames(sprintf("%.1f (%.1f)", cmp$elpd_diff, cmp$se_diff), cmp$model_a)
  delta <- ifelse(lt$model == "null", "— (reference)",
                  ifelse(lt$model %in% names(diff_map), diff_map[lt$model], NA_character_))
  disp <- data.frame(
    Model = .ma_model_label(lt$model),
    elpd = elpd,
    delta = delta,
    pareto = sprintf("%.2f", lt$max_pareto_k),
    check.names = FALSE
  )
  write_apa_table(
    disp, csv_path,
    title = "Leave-one-out cross-validation model comparison",
    note = paste0("ELPD = expected log pointwise predictive density (approximate LOO); higher is ",
                  "better. ΔELPD is relative to the null model with cluster-robust SE (positive ",
                  "favours the row model). Max Pareto-k < 0.7 indicates reliable LOO. Pooled across ",
                  "imputations."),
    header_labels = c(elpd = "ELPD (SE)", delta = "ΔELPD vs null (SE)",
                      pareto = "Max Pareto-k")
  )
  invisible(disp)
}

#' APA table: sampler convergence diagnostics by model
#'
#' @param diagnostics_by_model Named list of per-model diagnostic tibbles (one
#'   row per imputation) as returned by `diagnose_ma_fits()` / stored in
#'   `fit_diagnose_combine_ma()$diagnostics`.
#' @param csv_path Output CSV path.
#' @param model_order Optional row ordering.
#'
#' @returns Invisibly, the display data.frame.
ma_table_diagnostics <- function(diagnostics_by_model, csv_path, model_order = NULL) {
  rows <- lapply(names(diagnostics_by_model), function(nm) {
    d <- as.data.frame(diagnostics_by_model[[nm]])
    data.frame(
      model       = nm,
      max_rhat    = max(d$max_rhat, na.rm = TRUE),
      min_ess     = min(d$min_bulk_ess, na.rm = TRUE),
      divergences = sum(d$divergences, na.rm = TRUE),
      converged   = sprintf("%d/%d", sum(d$converged, na.rm = TRUE), nrow(d)),
      stringsAsFactors = FALSE
    )
  })
  tab <- do.call(rbind, rows)
  ord <- if (!is.null(model_order)) model_order else names(ma_model_labels)
  tab <- tab[order(match(tab$model, ord)), ]
  disp <- data.frame(
    Model = .ma_model_label(tab$model),
    rhat  = sprintf("%.3f", tab$max_rhat),
    ess   = sprintf("%.0f", tab$min_ess),
    div   = as.character(tab$divergences),
    conv  = tab$converged,
    check.names = FALSE
  )
  write_apa_table(
    disp, csv_path,
    title = "Sampler convergence diagnostics by model",
    note = paste0("Worst value across imputations. R-hat = potential scale-reduction factor; ",
                  "ESS = minimum bulk effective sample size; Divergences = total post-warmup ",
                  "divergent transitions; Converged = imputations meeting all thresholds ",
                  "(R-hat ≤ 1.01, bulk/tail ESS ≥ 100 per chain, 0 divergences, ",
                  "0 max-treedepth hits, E-BFMI ≥ 0.30)."),
    header_labels = c(rhat = "Max R-hat", ess = "Min bulk ESS", div = "Divergences",
                      conv = "Converged (of m)")
  )
  invisible(disp)
}
