#' Fit Meta Analysis
#'
#' Fits the pre-specified Bayesian meta-analysis model separately to every
#' completed imputed data set and returns the individual `brmsfit` objects for
#' per-imputation diagnostics before posterior draws are combined.
#'
#' @param midat A completed-data object accepted by `brms::brm_multiple()`, most
#'   commonly a list of completed imputed data frames returned by
#'   `completed_ma_data()`/`read_ma_imputed_data()`. Legacy `mitml.list` inputs
#'   and ordinary lists are passed through to `brm_multiple()` unchanged.
#' @param f_mean Formula for the mean model. The default is
#'   `ylogit | se(se_logit, sigma = TRUE) ~ 1 + mi(logn) + (1|id)`.
#' @param f_sigma Formula for the distributional sigma model. The default is
#'   `sigma ~ 1 + (1|id)`.
#' @param n.iter,iter Total Stan iterations per chain. `iter` is retained as a
#'   backwards-compatible alias for older scripts; if supplied, it overrides
#'   `n.iter`.
#' @param n.cores Number of cores passed to the Stan fitting process through
#'   `brms::brm_multiple()`. This function does not set up an additional
#'   `future` plan across imputations, which avoids accidental nested
#'   parallelism/oversubscription in the existing workflow.
#' @param n.warmup Number of Stan warmup iterations per chain.
#' @param control List of Stan control settings passed unchanged to `brms`.
#' @param seed Integer seed for reproducible sampling. Passed to `brm_multiple()`
#'   so reruns of the same cached model reproduce the same draws.
#' @param file Optional path (under `ma-models/`) at which to cache the fitted
#'   `ma_brmsfit_list`. When the file exists and `refit = FALSE`, the fits are
#'   read back from disk and no compilation or sampling happens. When absent, the
#'   model is fit and then written to this path.
#' @param model_file Optional path for the precompiled (compile-only) Stan model
#'   artifact. Defaults to `file` with a `.compiled.rds` suffix.
#' @param refit Logical; if `TRUE`, ignore any cached fit at `file` and refit.
#' @param reuse_compiled Logical; if `TRUE` (default), reuse a precompiled Stan
#'   model saved at `model_file` when it can be reloaded, otherwise compile once
#'   this session and save it there.
#' @param family Optional `brms` family overriding the default `student()`. Used
#'   by the sensitivity analyses (e.g. `gaussian()`, or `student()` with nu fixed
#'   through `prior`). `NULL` keeps `student()`.
#' @param prior Optional `brms` prior specification used verbatim instead of the
#'   pre-registered default prior set. Used by the sensitivity analyses (varying
#'   likelihood/sigma structure). `NULL` builds the defaults.
#' @param overwrite Logical; controls what happens when a newly fitted model would
#'   be written to an already-existing `file`. If `TRUE` (default), the cached fit
#'   at `file` is overwritten. If `FALSE`, the existing file is left untouched and
#'   the new fit is written beside it under a versioned name (`<stem>-1.rds`,
#'   `<stem>-2.rds`, ...), keeping `file` as the canonical cache that later runs
#'   reload. Only takes effect when a new fit is actually produced (i.e. `file`
#'   does not yet exist, or `refit = TRUE`); it never affects the precompiled
#'   `model_file`, which is structural and always safe to overwrite.
#'
#' @details
#' `fit_ma()` compiles the Stan program once (via a `chains = 0` precompile that
#' is saved to `model_file`) and hands that compiled model to
#' `brms::brm_multiple(combine = FALSE, fit = <precompiled>, recompile = FALSE)`,
#' so the single compiled executable is reused across every imputation. The
#' return value is a lightweight `ma_brmsfit_list`: one ordinary `brmsfit` per
#' imputation in the same order as `midat`. When `file` is supplied, the fitted
#' list is cached to disk and transparently reloaded on later runs instead of
#' being refit. Assess convergence with `diagnose_ma_fits()` and only then call
#' `combine_ma_fits()` to create the equally weighted posterior draw mixture used
#' for summaries, predictions, plots, and hypothesis tests.
#'
#' PREDICTORS:
#' 1. Modalities are currently grouped into "ECG", "PPG", "Other".
#' 2. `multimodal` is a binary identifier: were one or more modalities used in
#'    the same experiment?
#' 3. `log_n` is the log-transformed number of true users of the system.
#' 4. `intruders` is the approved sensitivity predictor collapsed to
#'    "intruders" vs "no intruders" and effect-coded downstream.
#' 5. ML Models use table algorithm classes: "dl", "dl_temp", "tree", and "kernel"; sparse "lm", "dist", and "prob" classes are grouped as "Other algorithm" for meta-analysis.
#'
#' @returns A list with class `c("ma_brmsfit_list", "list")`, containing one
#'   `brmsfit` per imputed data set. Lightweight attributes include `nimp` and
#'   the original call. The recommended workflow is:
#'   `ma_fits <- fit_ma(midat); ma_diagnostics <- diagnose_ma_fits(ma_fits);
#'   ma_fit <- combine_ma_fits(ma_fits)`.
#'
#' @import dplyr
#' @import brms
#'
fit_ma <- function(
  midat,
  f_mean = as.formula("ylogit | se(se_logit, sigma = TRUE) ~ 1 + mi(logn) + (1|id)"),
  f_sigma = as.formula("sigma ~ 1 + (1|id)"),
  n.iter = 4000,
  n.cores = 8,
  n.warmup = 1000,
  control = list(adapt_delta = 0.95,
                 max_treedepth = 12),
  iter = NULL,
  seed = 1234,
  file = NULL,
  model_file = NULL,
  refit = FALSE,
  reuse_compiled = TRUE,
  overwrite = TRUE,
  family = NULL,
  prior = NULL
  ) {

  library(dplyr)
  library(brms)

  if (!is.null(iter)) {
    n.iter <- iter
  }

  # 1) Reuse a cached fit list from disk whenever possible: no compile, no sampling.
  if (!is.null(file) && !isTRUE(refit) && file.exists(file)) {
    fits <- readRDS(file)
    validate_ma_brmsfit_list(fits)
    class(fits) <- c("ma_brmsfit_list", "list")
    attr(fits, "nimp") <- length(fits)
    attr(fits, "loaded_from") <- file
    message("fit_ma(): loaded cached fits from ", file)
    return(fits)
  }

  # Likelihood: Student-t by default. Sensitivity analyses override `family`
  # (e.g. gaussian(), or student() with nu fixed via the `prior` argument).
  if (is.null(family)) family <- student()

  # Priors. When `prior` is supplied (e.g. by the sensitivity analyses) it is
  # used verbatim; otherwise build the pre-registered default prior set.
  if (is.null(prior)) {
    pri <- c(
      prior(normal(0,1),           class="Intercept" ),

      prior(student_t(3,0,0.5),    class="sd"),
      prior(normal(-1,1),          class="Intercept", dpar="sigma"),
      prior(normal(0,0.5),         class="b",         dpar="sigma"),
      #prior(student_t(3,0,0.5),    class="sd",        dpar="sigma"),
      prior(exponential(1),        class="nu")
    )

    if (f_mean != "ylogit | se(se_logit, sigma = TRUE) ~ 1 + (1|id)") {
      pri <- c(pri,
               prior(normal(0,0.35), class="b"))
    }
  } else {
    pri <- prior
  }

  # A single completed data frame is also accepted (e.g. profiling one imputation).
  data_list <- if (is.data.frame(midat)) list(midat) else midat

  # 2) Pre-compilation. Reuse a saved compiled model if it reloads cleanly,
  #    otherwise compile once (chains = 0, no sampling) and save it to disk.
  if (is.null(model_file) && !is.null(file)) {
    model_file <- sub("\\.rds$", ".compiled.rds", file)
    if (identical(model_file, file)) model_file <- paste0(file, ".compiled.rds")
  }

  compile_base <- function() {
    m <- brm(
      bf(f_mean, f_sigma),
      data      = data_list[[1]],
      family    = family,
      prior     = pri,
      backend   = "cmdstanr",
      chains    = 0,                 # compile only, no sampling
      control   = control,
      save_pars = save_pars(all = TRUE),
      seed      = seed,
      silent    = 2,
      refresh   = 0
    )
    if (!is.null(model_file)) {
      dir.create(dirname(model_file), recursive = TRUE, showWarnings = FALSE)
      saveRDS(m, model_file)
    }
    m
  }

  reused_base <- FALSE
  base_model <- NULL
  if (isTRUE(reuse_compiled) && !is.null(model_file) && file.exists(model_file)) {
    base_model <- tryCatch(readRDS(model_file), error = function(e) NULL)
    if (inherits(base_model, "brmsfit")) reused_base <- TRUE else base_model <- NULL
  }
  if (is.null(base_model)) base_model <- compile_base()

  # 3) Fit every imputation, reusing the one compiled model with recompile = FALSE.
  #    A model just compiled this session always reuses cleanly. A reloaded model
  #    may point at a stale executable (cmdstanr executables are not portable
  #    across sessions), so if reuse fails we compile once more and retry.
  fit_all <- function(base) {
    brm_multiple(
      bf(f_mean, f_sigma),
      data       = data_list,    # list from mice/jomo/mitml or legacy completed data list
      family     = family,
      prior      = pri,
      backend    = "cmdstanr",
      chains     = 4,
      cores = n.cores,
      iter = n.iter,
      warmup = n.warmup,
      control    = control,
      save_pars  = save_pars(all = TRUE),
      combine    = FALSE,
      fit        = base,
      recompile  = FALSE,
      seed       = seed
    )
  }
  fits <- if (reused_base) {
    tryCatch(fit_all(base_model), error = function(e) {
      message("fit_ma(): precompiled model at ", model_file,
              " could not be reused (", conditionMessage(e), "); recompiling.")
      fit_all(compile_base())
    })
  } else {
    fit_all(base_model)
  }

  if (!is.list(fits)) {
    stop("`brm_multiple(combine = FALSE)` did not return a list of fits.", call. = FALSE)
  }
  class(fits) <- c("ma_brmsfit_list", "list")
  attr(fits, "nimp") <- length(fits)
  attr(fits, "call") <- match.call()

  # 4) Cache the fitted list so later runs skip compilation and sampling. When
  #    `overwrite = FALSE` and `file` already exists, keep the old cache as the
  #    canonical path and write this fit to a versioned sibling instead.
  if (!is.null(file)) {
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    target <- if (!isTRUE(overwrite) && file.exists(file)) {
      ma_versioned_path(file)
    } else {
      file
    }
    saveRDS(fits, target)
    attr(fits, "saved_to") <- target
    message("fit_ma(): saved fits to ", target)
  }
  fits
}

#' Find a non-colliding versioned path beside an existing file
#'
#' Given `dir/name.rds`, returns the first of `dir/name-1.rds`, `dir/name-2.rds`,
#' ... that does not yet exist. Used by `fit_ma(overwrite = FALSE)` to archive a
#' new fit without clobbering the canonical cache.
#'
#' @param path Existing file path to version beside.
#'
#' @returns A character path that does not currently exist on disk.
ma_versioned_path <- function(path) {
  ext <- tools::file_ext(path)
  stem <- tools::file_path_sans_ext(path)
  k <- 1L
  repeat {
    candidate <- if (nzchar(ext)) sprintf("%s-%d.%s", stem, k, ext) else sprintf("%s-%d", stem, k)
    if (!file.exists(candidate)) return(candidate)
    k <- k + 1L
  }
}

#' Validate a list of meta-analysis brms fits
#'
#' @param fits Candidate list of `brmsfit` objects.
#'
#' @returns Invisibly returns `TRUE` when `fits` is valid; otherwise errors.
validate_ma_brmsfit_list <- function(fits) {
  if (!is.list(fits) || length(fits) < 1L) {
    stop("`fits` must be a non-empty list of brmsfit objects.", call. = FALSE)
  }
  ok <- vapply(fits, inherits, logical(1), what = "brmsfit")
  if (!all(ok)) {
    stop("Every element of `fits` must inherit from `brmsfit`.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Summarise finite values or return missing
#'
#' @param x Numeric vector to filter to finite values.
#' @param fun Summary function applied to finite values.
#'
#' @returns The summary from `fun(x)` or `NA_real_` when no finite values remain.
finite_or_na <- function(x, fun) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) NA_real_ else fun(x)
}

#' Select posterior variables for meta-analysis convergence diagnostics
#'
#' @param fit A `brmsfit` object.
#'
#' @details Besides generated quantities, priors, and bookkeeping draws, this also
#'   drops the internal Cholesky-factor parameters (`L_1`, `L_2`, ..., `Lrescor`).
#'   Those factors contain structurally fixed entries (the upper-triangle zeros and
#'   the unit leading diagonal) that are constant across draws, so R-hat and ESS are
#'   undefined for them. Including them made every model with a random-slope/
#'   correlation structure spuriously fail the convergence check even though the
#'   interpretable random-effect correlations (`cor_*`) and standard deviations
#'   (`sd_*`) — which carry the same information and are retained here — mixed well.
#'
#' @param drop_constant Logical; if `TRUE` (default) also drop parameters that are
#'   constant across all draws. R-hat and ESS are undefined for those, so counting
#'   them as convergence failures is spurious. This catches fixed parameters in
#'   general - e.g. `nu` when the Student-t degrees of freedom are held at a
#'   constant in a sensitivity analysis - not just the named Cholesky factors.
#'   The dropped names are returned in the `"constant_variables"` attribute so the
#'   count can be reported rather than silently hidden.
#'
#' @returns Character vector of posterior variable names excluding generated, prior,
#'   Cholesky-factor, constant, and bookkeeping draws.
selected_ma_draw_variables <- function(fit, drop_constant = TRUE) {
  draws <- posterior::as_draws_array(fit)
  vars <- posterior::variables(draws)
  excluded <- grepl("^(log_lik|ll|ppred|yrep|Yrep|pred|prior_|lprior|lp__)(\\[|$|_)", vars) |
    grepl("^(L_[0-9]+|Lrescor)(\\[|$)", vars) |
    grepl("^\\.chain$|^\\.iteration$|^\\.draw$", vars)
  vars <- vars[!excluded]
  if (!isTRUE(drop_constant) || length(vars) == 0L) return(vars)
  sds <- posterior::summarise_draws(
    draws[, , vars, drop = FALSE], sd = function(x) stats::sd(x)
  )
  keep <- is.finite(sds$sd) & sds$sd > 0
  out <- sds$variable[keep]
  attr(out, "constant_variables") <- sds$variable[!keep]
  out
}

#' Summarise HMC diagnostics for a meta-analysis fit
#'
#' @param fit A `brmsfit` object.
#' @param bfmi_threshold Minimum acceptable per-chain E-BFMI.
#'
#' @returns A tibble with per-chain divergences, treedepth hits, E-BFMI, and low-E-BFMI flags.
summarise_ma_hmc <- function(fit, bfmi_threshold = 0.30) {
  nuts <- brms::nuts_params(fit)
  if (!is.data.frame(nuts) || nrow(nuts) == 0L) {
    return(tibble::tibble(
      chain = integer(), divergences = integer(), max_treedepth_hits = integer(),
      ebfmi = numeric(), low_ebfmi = logical()
    ))
  }
  max_td <- brms::control_params(fit)$max_treedepth
  if (is.null(max_td)) max_td <- Inf
  chains <- sort(unique(nuts$Chain))
  ebfmi <- vapply(chains, function(ch) {
    energy <- nuts$Value[nuts$Parameter == "energy__" & nuts$Chain == ch]
    if (
      length(energy) < 2L || isTRUE(stats::var(energy) == 0)
      ) NA_real_ else mean(diff(energy)^2) / stats::var(energy)
  }, 
  numeric(1))
  tibble::tibble(
    chain = chains,
    divergences = vapply(chains, function(ch) sum(nuts$Parameter == "divergent__" & nuts$Chain == ch & nuts$Value > 0, na.rm = TRUE), integer(1)),
    max_treedepth_hits = vapply(chains, function(ch) sum(nuts$Parameter == "treedepth__" & nuts$Chain == ch & nuts$Value >= max_td, na.rm = TRUE), integer(1)),
    ebfmi = ebfmi,
    low_ebfmi = !is.na(ebfmi) & ebfmi < bfmi_threshold
  )
}

#' Diagnose per-imputation meta-analysis brms fits
#'
#' @param fits A non-empty list of ordinary `brmsfit` objects, normally returned
#'   by `fit_ma()`.
#' @param rhat_threshold Maximum acceptable defined R-hat value.
#' @param ess_per_chain_threshold Minimum acceptable bulk and tail ESS per chain.
#'   The applied threshold is this value multiplied by the number of chains in
#'   each fit.
#' @param bfmi_threshold Minimum acceptable per-chain E-BFMI.
#' @param detailed If `FALSE`, return a one-row-per-imputation tibble. If `TRUE`,
#'   return a list containing the summary tibble plus per-parameter and HMC
#'   diagnostic tibbles.
#'
#' @returns A machine-readable tibble, or a detailed list when requested.
#'
diagnose_ma_fits <- function(
  fits,
  rhat_threshold = 1.01,
  ess_per_chain_threshold = 100,
  bfmi_threshold = 0.30,
  detailed = FALSE
) {
  validate_ma_brmsfit_list(fits)
  param_tabs <- vector("list", length(fits))
  hmc_tabs <- vector("list", length(fits))

  rows <- lapply(seq_along(fits), function(i) {
    fit <- fits[[i]]
    draws <- posterior::as_draws_array(fit)
    vars <- selected_ma_draw_variables(fit)
    dims <- dim(draws)
    n_iter <- dims[[1]]; n_chains <- dims[[2]]
    param_diag <- posterior::summarise_draws(
      draws[, , vars, drop = FALSE],
      posterior::default_convergence_measures()
    )
    param_diag <- tibble::as_tibble(param_diag)[, c("variable", "rhat", "ess_bulk", "ess_tail")]
    param_tabs[[i]] <<- param_diag
    hmc <- summarise_ma_hmc(fit, bfmi_threshold)
    hmc_tabs[[i]] <<- hmc

    ess_threshold <- ess_per_chain_threshold * n_chains
    defined_rhat <- param_diag$rhat[!is.na(param_diag$rhat)]
    max_rhat <- finite_or_na(defined_rhat, max)
    min_bulk <- finite_or_na(param_diag$ess_bulk, min)
    min_tail <- finite_or_na(param_diag$ess_tail, min)
    worst_rhat <- if (is.na(max_rhat)) NA_character_ else param_diag$variable[which.max(replace(param_diag$rhat, is.na(param_diag$rhat), -Inf))]
    lowest_bulk <- if (is.na(min_bulk)) NA_character_ else param_diag$variable[which.min(replace(param_diag$ess_bulk, is.na(param_diag$ess_bulk), Inf))]
    lowest_tail <- if (is.na(min_tail)) NA_character_ else param_diag$variable[which.min(replace(param_diag$ess_tail, is.na(param_diag$ess_tail), Inf))]
    divergences <- sum(hmc$divergences, na.rm = TRUE)
    td_hits <- sum(hmc$max_treedepth_hits, na.rm = TRUE)
    min_ebfmi <- finite_or_na(hmc$ebfmi, min)
    low_ebfmi <- sum(hmc$low_ebfmi, na.rm = TRUE)
    converged <- n_chains >= 2L &&
      all(defined_rhat <= rhat_threshold) &&
      all(!is.na(param_diag$ess_bulk) & param_diag$ess_bulk >= ess_threshold) &&
      all(!is.na(param_diag$ess_tail) & param_diag$ess_tail >= ess_threshold) &&
      divergences == 0L && td_hits == 0L && low_ebfmi == 0L

    tibble::tibble(
      imputation = i,
      n_chains = n_chains,
      post_warmup_draws = n_iter * n_chains,
      n_parameters = length(vars),
      # Parameters held constant by construction (e.g. fixed nu, Cholesky
      # constants). Excluded from the convergence check because R-hat/ESS are
      # undefined for them; counted here so the exclusion stays visible.
      n_constant_parameters = length(attr(vars, "constant_variables")),
      max_rhat = max_rhat,
      worst_rhat_parameter = worst_rhat,
      n_rhat_over_threshold = sum(defined_rhat > rhat_threshold),
      n_rhat_missing = sum(is.na(param_diag$rhat)),
      min_bulk_ess = min_bulk,
      lowest_bulk_ess_parameter = lowest_bulk,
      n_bulk_ess_below_threshold = sum(is.na(param_diag$ess_bulk) | param_diag$ess_bulk < ess_threshold),
      min_tail_ess = min_tail,
      lowest_tail_ess_parameter = lowest_tail,
      n_tail_ess_below_threshold = sum(is.na(param_diag$ess_tail) | param_diag$ess_tail < ess_threshold),
      divergences = divergences,
      max_treedepth_hits = td_hits,
      min_ebfmi = min_ebfmi,
      n_chains_low_ebfmi = low_ebfmi,
      converged = converged
    )
  })
  summary <- dplyr::bind_rows(rows)
  failed <- summary$imputation[!summary$converged]
  if (length(failed) > 0L) {
    warning("Meta-analysis diagnostics failed for imputation(s): ", paste(failed, collapse = ", "), call. = FALSE)
  }
  if (isTRUE(detailed)) list(summary = summary, parameters = param_tabs, hmc = hmc_tabs) else summary
}

#' Get diagnostic posterior variable names from a meta-analysis fit
#'
#' @param fit A `brmsfit` object.
#'
#' @returns Character vector of selected posterior variable names. Attributes are
#'   stripped so the value can be compared across fits with `identical()`.
ma_fit_variable_names <- function(fit) {
  v <- selected_ma_draw_variables(fit)
  attributes(v) <- NULL
  v
}
#' Count post-warmup draws in a meta-analysis fit
#'
#' @param fit A `brmsfit` object.
#'
#' @returns Number of post-warmup draws across chains.
ma_fit_draw_count <- function(fit) prod(dim(posterior::as_draws_array(fit))[1:2])
#' Count chains in a meta-analysis fit
#'
#' @param fit A `brmsfit` object.
#'
#' @returns Number of posterior chains.
ma_fit_chain_count <- function(fit) dim(posterior::as_draws_array(fit))[[2]]

#' Environment-independent key for a fit's model formula
#'
#' Deparses the mean formula and every distributional-parameter formula so two
#' fits of the same model compare equal even when their stored formula objects
#' carry different environments (as happens with parallel `future` fitting).
#'
#' @param fit A `brmsfit` object.
#'
#' @returns A single character string identifying the model formula.
ma_fit_formula_key <- function(fit) {
  bf <- fit$formula
  parts <- c(list(bf$formula), if (!is.null(bf$pforms)) bf$pforms else list())
  paste(
    vapply(parts, function(f) paste(deparse(f), collapse = " "), character(1)),
    collapse = " || "
  )
}

#' Environment-independent key for a fit's response family
#'
#' @param fit A `brmsfit` object.
#'
#' @returns A single character string identifying the family and link.
ma_fit_family_key <- function(fit) paste(fit$family$family, fit$family$link, sep = "|")

#' Combine diagnosed meta-analysis fits
#'
#' @param fits A non-empty list of ordinary per-imputation `brmsfit` objects.
#' @param check_convergence Logical; if `TRUE`, run `diagnose_ma_fits()` before
#'   combining.
#' @param require_convergence Logical; if `TRUE`, stop when diagnostics fail.
#'
#' @details The combined object is an equally weighted mixture of posterior draws
#' from the imputations. Parameter estimates and intervals from this mixture are
#' meaningful. Overall R-hat and ESS computed after combination are not
#' meaningful because chains from different imputations target different
#' completed-data posteriors; convergence must be assessed on the original list
#' returned by `fit_ma()`. The `brmsfit_multiple` class and `nimp` attribute are
#' restored so `summary()` follows `brms` multiple-imputation behavior and
#' suppresses misleading combined R-hat/ESS output.
#'
#' @returns A combined `brmsfit_multiple` object.
#'
combine_ma_fits <- function(
  fits,
  check_convergence = TRUE,
  require_convergence = FALSE
) {
  validate_ma_brmsfit_list(fits)
  # Compare model structure by CONTENT, not object identity. When brm_multiple()
  # fits under a parallel `future` plan, each imputation is produced in a separate
  # worker process, so the stored formula/family objects carry worker-specific
  # environments and identical() would spuriously fail even though every
  # imputation fits the same model. Deparse the formula parts and read the family
  # name/link instead (both environment-independent).
  formula_keys <- vapply(fits, ma_fit_formula_key, character(1))
  family_keys  <- vapply(fits, ma_fit_family_key, character(1))
  params <- lapply(fits, ma_fit_variable_names)
  chains <- vapply(fits, ma_fit_chain_count, integer(1))
  draws <- vapply(fits, ma_fit_draw_count, numeric(1))

  if (
    length(unique(formula_keys)) != 1L
    ) stop("All fits must use compatible formulas.", call. = FALSE)
  if (
    length(unique(family_keys)) != 1L
    ) stop("All fits must use compatible families.", call. = FALSE)
  if (
    !all(vapply(params[-1], identical, logical(1), params[[1]]))
    ) stop("All fits must use compatible parameter sets.", call. = FALSE)
  if (
    length(unique(chains)) != 1L
    ) stop("All fits must use the same number of chains.", call. = FALSE)
  if (
    length(unique(draws)) != 1L
    ) stop(
      "All fits must have equal post-warmup draw counts so imputations are equally weighted.", 
      call. = FALSE)

  if (isTRUE(check_convergence)) {
    dx <- diagnose_ma_fits(fits)
    failed <- dx$imputation[!dx$converged]
    if (length(failed) > 0L) {
      msg <- paste0("Meta-analysis diagnostics failed for imputation(s): ", paste(failed, collapse = ", "))
      if (isTRUE(require_convergence)) stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
    }
  }

  combined <- brms::combine_models(mlist = unclass(fits), check_data = FALSE)
  attr(combined, "nimp") <- length(fits)
  class(combined) <- unique(c("brmsfit_multiple", class(combined)))
  combined
}



#' Fit, diagnose, and combine meta-analysis models
#'
#' @param midat Completed-data object passed to `fit_ma()`.
#' @param ... Additional arguments passed to `fit_ma()`, including `file`,
#'   `model_file`, `refit`, and `seed` for on-disk caching and precompilation.
#'
#' @details When a `file` argument is supplied, the per-imputation fits are
#' cached to (and transparently reloaded from) that path by `fit_ma()`.
#' Diagnostics and the combined fit are always recomputed from the (possibly
#' disk-loaded) fits, so downstream use never depends on stale summaries.
#'
#' @returns A list containing per-imputation fits, diagnostics, and the combined fit.
fit_diagnose_combine_ma <- function(midat, ...) {
  component_fits <- fit_ma(midat, ...)
  diagnostics <- diagnose_ma_fits(component_fits)
  # Diagnostics are already captured above; avoid re-traversing posterior draws.
  combined_fit <- combine_ma_fits(
    component_fits,
    check_convergence = FALSE,
    require_convergence = FALSE
  )
  list(fits = component_fits, diagnostics = diagnostics, fit = combined_fit)
}