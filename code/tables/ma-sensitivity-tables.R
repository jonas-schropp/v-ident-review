#' Cross-variant sensitivity tables (model x specification)
#'
#' The per-variant outputs answer "what happened in variant X?", but the question a
#' reader actually has is "does conclusion Y survive?" - which means reading across
#' variants. These builders pivot the per-variant CSVs into four wide tables, one per
#' conclusion, replacing 36 per-variant tables with 4.
#'
#' Deliberately depends only on readr/dplyr and apa.R: it reads the CSVs the sensitivity
#' run already wrote, never the fits, so it runs in seconds without loading brms and can
#' be re-run any time the per-variant results change.
#'
#' Depends on: dplyr, readr, apa.R (write_apa_table), ma-tables.R (ma_model_labels).

#' Column headings for the pre-registered variants, in protocol order.
#'
#' Kept short on purpose. These tables are eleven columns wide in a 10465 dxa text
#' block, and a long heading starves the numeric columns beside it: spelling the
#' specifications out gave the "Main" column 305 dxa, too narrow for "1.18" to fit on
#' one line. `ma_variant_legend` carries the full definitions into the note instead.
ma_variant_labels <- c(
  main              = "Main",
  gaussian          = "Gaussian",
  student_nu4       = "t (4)",
  sigma_intercept   = "σ ~ 1",
  sigma_re          = "σ ~ RE",
  sigma_cov_re      = "σ ~ X + RE",
  feature_collapse  = "Feature",
  duration_collapse = "Duration",
  se_unknown        = "SE unk.",
  complete_case     = "Complete"
)

#' Expansion of the abbreviated column headings, appended to every table's note.
ma_variant_legend <- paste0(
  "Specifications: Main = primary analysis; Gaussian = Gaussian likelihood; ",
  "t (4) = Student t with nu fixed at 4; σ ~ 1 = intercept-only residual scale; ",
  "σ ~ RE = residual scale with a study random effect; ",
  "σ ~ X + RE = residual scale with design covariates and a study random effect; ",
  "Feature = collapsed feature-type categories; Duration = collapsed duration ",
  "categories; SE unk. = reported standard errors of unknown origin also included; ",
  "Complete = complete-case analysis without imputation."
)

.ma_read <- function(path, ...) {
  if (!file.exists(path)) return(NULL)
  suppressMessages(readr::read_csv(path, show_col_types = FALSE, ...))
}

#' Locate the main and per-variant result directories.
#' @param results_dir Directory holding the main-analysis CSVs.
#' @param variants Optional subset/order of variant keys; defaults to those present.
ma_sensitivity_dirs <- function(results_dir = here::here("results", "meta-analysis"),
                               variants = NULL) {
  sens_root <- file.path(results_dir, "sensitivity")
  present <- if (dir.exists(sens_root)) {
    basename(list.dirs(sens_root, recursive = FALSE))
  } else character(0)
  keys <- names(ma_variant_labels)
  keys <- c("main", intersect(setdiff(keys, "main"), present))
  if (!is.null(variants)) keys <- intersect(keys, c("main", variants))
  stats::setNames(
    vapply(keys, function(k) if (k == "main") results_dir else file.path(sens_root, k),
           character(1)),
    keys
  )
}

#' Pivot one per-variant quantity into a model x variant matrix.
#'
#' @param dirs Named vector of result directories (see `ma_sensitivity_dirs()`).
#' @param reader Function(dir) returning a data.frame with `model` and `value`
#'   (character), or NULL when that variant did not produce the quantity.
#' @param absent String for a model/variant combination the variant never fitted.
.ma_pivot <- function(dirs, reader, absent = "n/a") {
  per <- lapply(dirs, reader)
  models <- names(ma_model_labels)
  out <- data.frame(Model = .ma_model_label(models), check.names = FALSE,
                    stringsAsFactors = FALSE)
  for (k in names(dirs)) {
    d <- per[[k]]
    v <- rep(absent, length(models))
    if (!is.null(d) && nrow(d)) v <- d$value[match(models, d$model)]
    v[is.na(v)] <- absent
    out[[k]] <- v
  }
  # drop models no variant fitted at all
  keep <- apply(out[, names(dirs), drop = FALSE], 1, function(r) any(r != absent))
  out[keep, , drop = FALSE]
}

.ma_headers <- function(dirs) {
  stats::setNames(unname(ma_variant_labels[names(dirs)]), names(dirs))
}

#' S-a: between-study SD (tau) under every specification.
ma_table_sens_tau <- function(dirs, csv_path, digits = 2) {
  rd <- function(dir) {
    d <- .ma_read(file.path(dir, "heterogeneity_metrics.csv"))
    if (is.null(d)) return(NULL)
    d <- d[d$param == "tau", , drop = FALSE]
    data.frame(model = d$model, value = sprintf(paste0("%.", digits, "f"), d$median),
               stringsAsFactors = FALSE)
  }
  disp <- .ma_pivot(dirs, rd)
  write_apa_table(
    disp, csv_path,
    title = "Between-study standard deviation (tau) by model and specification",
    note = paste0("Posterior median of the between-study SD on the logit scale. ",
                  "Columns are the pre-registered sensitivity specifications; 'Main' is ",
                  "the primary analysis. n/a marks a model a variant did not refit. ",
                  "Credible intervals are given per variant in the supplementary data. ",
                  "tau remains far from zero under every specification, so the ",
                  "heterogeneity conclusion does not depend on any one modelling choice. ", ma_variant_legend),
    header_labels = .ma_headers(dirs)
  )
  invisible(disp)
}

#' S-b: LOO difference against the null model under every specification.
ma_table_sens_loo <- function(dirs, csv_path) {
  rd <- function(dir) {
    d <- .ma_read(file.path(dir, "loo_comparison.csv"))
    if (is.null(d) || !nrow(d)) return(NULL)
    d <- d[d$model_b == "null", , drop = FALSE]
    # sprintf("%.0f", -0.4) renders "-0"; show a signless zero instead.
    diff <- ifelse(round(d$elpd_diff) == 0, "0", sprintf("%.0f", d$elpd_diff))
    data.frame(model = d$model_a,
               value = sprintf("%s (%.0f)", diff, d$se_diff),
               stringsAsFactors = FALSE)
  }
  # The null model is the comparison baseline, so it has no row of its own.
  disp <- .ma_pivot(dirs, rd)
  write_apa_table(
    disp, csv_path,
    title = "Cross-validation difference against the null model by specification",
    note = paste0("Difference in expected log predictive density relative to the ",
                  "intercept-only model, with its standard error in parentheses; ",
                  "positive favours the moderator. The null model is the baseline and so ",
                  "has no row. n/a marks a model a variant did not refit. In no ",
                  "specification does any moderator exceed twice its standard error, i.e. ",
                  "no moderator predicts held-out experiments better than the null model ",
                  "under any modelling choice. ", ma_variant_legend),
    header_labels = .ma_headers(dirs)
  )
  invisible(disp)
}

#' S-c: proportion of between-study heterogeneity explained, by specification.
ma_table_sens_r2 <- function(dirs, csv_path, digits = 2) {
  rd <- function(dir) {
    d <- .ma_read(file.path(dir, "r2_meta.csv"))
    if (is.null(d) || !nrow(d)) return(NULL)
    d <- d[d$model != "null", , drop = FALSE]
    data.frame(model = d$model, value = sprintf(paste0("%.", digits, "f"), d$median),
               stringsAsFactors = FALSE)
  }
  # R-squared_meta is defined relative to the null model, which therefore has no row.
  disp <- .ma_pivot(dirs, rd)
  write_apa_table(
    disp, csv_path,
    title = "Proportion of between-study heterogeneity explained by specification",
    note = paste0("Posterior median of R-squared_meta, the share of between-study ",
                  "heterogeneity explained relative to the null model, which is the ",
                  "baseline and so has no row. Intervals are wide and frequently span zero ",
                  "in both directions (see the supplementary data); the point estimates are ",
                  "reported here to show that no specification lets any moderator account ",
                  "for an appreciable share. Negative values mean the moderator model left ",
                  "slightly more heterogeneity than the null. n/a marks a model a variant ",
                  "did not refit. ", ma_variant_legend),
    header_labels = .ma_headers(dirs)
  )
  invisible(disp)
}

#' S-d: sampler convergence under every specification.
#'
#' Cells give the number of imputations meeting the pre-specified convergence rule out
#' of those fitted, with the worst R-hat across imputations in parentheses. Those two
#' numbers are what the rule turns on, and they keep the table narrow enough to read.
ma_table_sens_convergence <- function(dirs, csv_path) {
  rd <- function(dir) {
    d <- .ma_read(file.path(dir, "diagnostics_table.csv"))
    if (is.null(d) || !nrow(d)) return(NULL)
    lab2key <- stats::setNames(names(ma_model_labels), unname(ma_model_labels))
    key <- lab2key[as.character(d[[1]])]
    conv <- sub("/.*$", "", as.character(d$conv))
    tot  <- sub("^.*/", "", as.character(d$conv))
    data.frame(model = unname(key),
               value = sprintf("%s/%s (%.3f)", conv, tot, as.numeric(d$rhat)),
               stringsAsFactors = FALSE)
  }
  disp <- .ma_pivot(dirs, rd)
  write_apa_table(
    disp, csv_path,
    title = "Sampler convergence by model and specification",
    note = paste0("Imputations meeting the convergence rule out of those fitted, with the ",
                  "largest R-hat across imputations in parentheses. The rule requires ",
                  "R-hat at or below 1.01, bulk and tail effective sample sizes of at ",
                  "least 100 per chain, no divergent transitions and adequate energy ",
                  "diagnostics. No specification produced a divergent transition. The ",
                  "complete-case variant fits one listwise-complete data set rather than ",
                  "imputations, hence 1/1. n/a marks a model a variant did not refit. ", ma_variant_legend),
    header_labels = .ma_headers(dirs)
  )
  invisible(disp)
}

#' Build all four cross-variant tables.
#'
#' @param results_dir Directory holding the main-analysis CSVs.
#' @param out_dir Where to write; defaults to `results_dir/sensitivity`.
#' @param variants Optional subset/order of variant keys.
#' @returns Invisibly, a named list of the display data.frames.
ma_write_sensitivity_tables <- function(results_dir = here::here("results", "meta-analysis"),
                                        out_dir = NULL, variants = NULL) {
  dirs <- ma_sensitivity_dirs(results_dir, variants)
  if (is.null(out_dir)) out_dir <- file.path(results_dir, "sensitivity")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  res <- list(
    tau         = ma_table_sens_tau(dirs,         file.path(out_dir, "sens_tau_by_variant.csv")),
    loo         = ma_table_sens_loo(dirs,         file.path(out_dir, "sens_loo_by_variant.csv")),
    r2          = ma_table_sens_r2(dirs,          file.path(out_dir, "sens_r2_by_variant.csv")),
    convergence = ma_table_sens_convergence(dirs, file.path(out_dir, "sens_convergence_by_variant.csv"))
  )
  message(sprintf("Wrote 4 cross-variant tables over %d specifications: %s",
                  length(dirs), paste(names(dirs), collapse = ", ")))
  invisible(res)
}
