#' Multiple-imputation-aware LOO for meta-analysis brms fits
#'
#' Runs `loo()` separately for every completed-imputation fit and pools expected
#' log predictive density (ELPD) by averaging pointwise ELPD values across
#' imputations. This is a pragmatic improvement over calling `loo()` on a
#' combined `brmsfit_multiple`, which can use only the first imputed data set
#' when predictors differ across imputations. It does not cross-validate the
#' imputation procedure itself. Because `ylogit` is used as an auxiliary
#' predictor when imputing missing covariates, fully leakage-free predictive
#' validation would require rerunning imputation inside every validation fold
#' while withholding validation outcomes.
#'
#' @param fits A list of per-imputation `brmsfit` objects returned by `fit_ma()`;
#'   a single ordinary `brmsfit` is also accepted for non-imputed/null models.
#' @param resp Response name passed to `loo()`. Defaults to `"ylogit"`.
#' @param re_formula Group-level formula passed to `loo()`. Defaults to `NA`.
#' @param moment_match Logical passed to `loo()`.
#' @param ... Additional arguments passed to `loo()`.
#' @param .loo_fun Testing hook; defaults to `loo::loo`.
#'
#' @returns An `ma_loo` list containing all per-imputation LOO objects, averaged
#'   pointwise ELPD, pooled totals/SE, and Pareto-k diagnostics.
loo_ma_fits <- function(
  fits,
  resp = "ylogit",
  re_formula = NA,
  moment_match = FALSE,
  ...,
  .loo_fun = loo::loo
) {
  if (inherits(fits, "brmsfit_multiple")) {
    stop(
      "`loo_ma_fits()` requires the original per-imputation `brmsfit` list returned by `fit_ma()`, not a combined-only `brmsfit_multiple` object.",
      call. = FALSE
    )
  }
  if (inherits(fits, "brmsfit") && !inherits(fits, "ma_brmsfit_list")) {
    fits <- list(fits)
  }
  validate_ma_brmsfit_list(fits)

  loo_objects <- lapply(fits, function(fit) {
    .loo_fun(fit, resp = resp, re_formula = re_formula, moment_match = moment_match, ...)
  })
  build_ma_loo_result(loo_objects, model_name = deparse(substitute(fits)))
}

build_ma_loo_result <- function(loo_objects, model_name = NULL) {
  pointwise <- lapply(loo_objects, ma_pointwise_matrix)
  n_obs <- vapply(pointwise, nrow, integer(1))
  if (length(unique(n_obs)) != 1L) stop("All imputation-specific LOO objects must have the same number of observations.", call. = FALSE)
  ids <- lapply(pointwise, rownames)
  if (!all(vapply(ids[-1], identical, logical(1), ids[[1]]))) {
    stop("All imputation-specific LOO objects must contain the same observations in the same order.", call. = FALSE)
  }
  elpd_mat <- do.call(cbind, lapply(pointwise, function(x) x[, ma_elpd_column(x)]))
  elpd_pointwise <- rowMeans(elpd_mat)
  names(elpd_pointwise) <- ids[[1]]
  pareto <- lapply(loo_objects, ma_pareto_k)
  max_pareto <- if (length(unlist(pareto, use.names = FALSE)) == 0L) NA_real_ else max(unlist(pareto, use.names = FALSE), na.rm = TRUE)
  problematic <- vapply(pareto, function(k) any(k > 0.7, na.rm = TRUE), logical(1))
  per_imp_elpd <- vapply(pointwise, function(x) sum(x[, ma_elpd_column(x)]), numeric(1))
  pointwise_out <- data.frame(observation = ids[[1]] %||% as.character(seq_along(elpd_pointwise)))
  pointwise_out[[ma_elpd_column(pointwise[[1]])]] <- elpd_pointwise
  se_col <- ma_elpd_column(pointwise[[1]], se = TRUE, required = FALSE)
  if (!is.na(se_col)) pointwise_out[[se_col]] <- NA_real_
  out <- list(
    loo = loo_objects,
    pointwise = pointwise_out,
    elpd_pointwise = elpd_pointwise,
    elpd_total = sum(elpd_pointwise),
    se_elpd = sqrt(length(elpd_pointwise) * stats::var(elpd_pointwise)),
    per_imputation_total_elpd = per_imp_elpd,
    pareto_k = pareto,
    pareto_summary = data.frame(imputation = seq_along(pareto), max_pareto_k = vapply(pareto, function(k) if (length(k)) max(k, na.rm = TRUE) else NA_real_, numeric(1)), problematic = problematic),
    max_pareto_k = max_pareto,
    n_problematic_imputations = sum(problematic),
    nimp = length(loo_objects),
    nobs = length(elpd_pointwise),
    model_name = model_name
  )
  class(out) <- c("ma_loo", "list")
  out
}

`%||%` <- function(x, y) if (is.null(x)) y else x

ma_elpd_column <- function(pointwise, se = FALSE, required = TRUE) {
  candidates <- if (isTRUE(se)) c("se_elpd_loo", "se_elpd_kfold") else c("elpd_loo", "elpd_kfold")
  found <- intersect(candidates, colnames(pointwise))
  if (length(found) > 0L) return(found[[1]])
  if (isTRUE(required)) {
    stop(
      sprintf("Pointwise matrix must contain one of: %s.", paste(sprintf("`%s`", candidates), collapse = ", ")),
      call. = FALSE
    )
  }
  NA_character_
}

ma_pointwise_matrix <- function(x) {
  pw <- x$pointwise
  if (is.null(pw)) stop("LOO/K-fold object is missing a `pointwise` component.", call. = FALSE)
  pw <- as.matrix(pw)
  ma_elpd_column(pw)
  ma_elpd_column(pw, se = TRUE, required = FALSE)
  pw
}

ma_pareto_k <- function(x) {
  k <- x$diagnostics$pareto_k %||% x$pareto_k %||% numeric()
  as.numeric(k)
}

#' Compare multiple-imputation-aware meta-analysis LOO results
compare_ma_loo <- function(loo_model_a, loo_model_b, cluster = NULL, model_names = c("model_a", "model_b")) {
  if (!inherits(loo_model_a, "ma_loo") || !inherits(loo_model_b, "ma_loo")) stop("Both inputs must be `ma_loo` objects.", call. = FALSE)
  if (length(model_names) != 2L) stop("`model_names` must contain exactly two names.", call. = FALSE)
  if (loo_model_a$nimp != loo_model_b$nimp && loo_model_a$nimp > 1L && loo_model_b$nimp > 1L) {
    stop("LOO results have mismatched numbers of imputations; use matching per-imputation model results or a documented single-fit null model.", call. = FALSE)
  }
  if (loo_model_a$nobs != loo_model_b$nobs || !identical(names(loo_model_a$elpd_pointwise), names(loo_model_b$elpd_pointwise))) {
    stop("LOO results must contain the same observations in the same order.", call. = FALSE)
  }
  d_i <- loo_model_a$elpd_pointwise - loo_model_b$elpd_pointwise
  if (is.null(cluster)) {
    se_diff <- sqrt(length(d_i) * stats::var(d_i))
    n_clusters <- NA_integer_
  } else {
    if (length(cluster) != length(d_i)) stop("`cluster` must have one value per observation.", call. = FALSE)
    d_g <- as.numeric(rowsum(d_i, group = cluster, reorder = FALSE))
    se_diff <- sqrt(length(d_g) * stats::var(d_g))
    n_clusters <- length(d_g)
  }
  out <- data.frame(
    model_a = model_names[[1]], model_b = model_names[[2]],
    elpd_a = loo_model_a$elpd_total, elpd_b = loo_model_b$elpd_total,
    elpd_diff = sum(d_i), se_diff = se_diff,
    direction = paste("positive favors", model_names[[1]]),
    nimp_a = loo_model_a$nimp, nimp_b = loo_model_b$nimp,
    nobs = length(d_i), n_clusters = n_clusters
  )
  class(out) <- c("ma_loo_comparison", class(out))
  out
}

#' Format meta-analysis LOO diagnostics and optional comparison
format_ma_loo <- function(x, model_name = x$model_name %||% "model", comparison = NULL) {
  if (!inherits(x, "ma_loo")) stop("`x` must be an `ma_loo` object.", call. = FALSE)
  rng <- range(x$per_imputation_total_elpd, na.rm = TRUE)
  tab <- data.frame(
    model = model_name,
    nimp = x$nimp,
    mean_per_imputation_elpd = mean(x$per_imputation_total_elpd),
    min_per_imputation_elpd = rng[[1]],
    max_per_imputation_elpd = rng[[2]],
    pooled_elpd = x$elpd_total,
    pooled_se = x$se_elpd,
    max_pareto_k = x$max_pareto_k,
    n_problematic_imputations = x$n_problematic_imputations
  )
  if (!is.null(comparison)) cbind(tab, comparison[, c("elpd_diff", "se_diff", "n_clusters"), drop = FALSE]) else tab
}

#' Multiple-imputation-aware grouped K-fold for meta-analysis brms fits
kfold_ma_fits <- function(fits, completed_data, group = "id", K = 5, folds = NULL, joint = "group", ..., .kfold_fun = brms::kfold) {
  validate_ma_brmsfit_list(fits)
  if (is.null(folds)) folds <- loo::kfold_split_grouped(K = K, x = completed_data[[1]][[group]])
  kfold_objects <- lapply(fits, function(fit) .kfold_fun(fit, K = K, folds = folds, group = group, joint = joint, ...))
  out <- build_ma_loo_result(kfold_objects, model_name = deparse(substitute(fits)))
  out$folds <- folds
  class(out) <- c("ma_kfold", class(out))
  out
}
