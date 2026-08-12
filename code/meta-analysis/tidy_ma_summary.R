#' Complete a prediction grid with all covariates a brms model needs
#'
#' brms validates the whole model (every distributional parameter) when
#' predicting, so a `newdata` grid must contain every covariate used anywhere in
#' the model - including those that appear only in the sigma submodel (e.g.
#' `activity`, `duration`). This helper fills any covariate missing from
#' `newdata` at a reference value (median for numeric, reference level for
#' factors, `FALSE`/0 for binary) and sets the known-SE column to 0, leaving the
#' predictor(s) the caller varied untouched. Shared by `tidy_ma_summary()` and the
#' effect-plot helpers so predictions are constructed identically everywhere.
#'
#' @param fit A `brmsfit`.
#' @param newdata Partial newdata: one row per level to predict, with the target
#'   predictor column(s) already set.
#'
#' @returns `newdata` with all remaining model covariates and `se_logit`/`has_se`.
ma_complete_newdata <- function(fit, newdata) {
  newdata <- as.data.frame(newdata, stringsAsFactors = FALSE)
  if (!"se_logit" %in% names(newdata)) newdata$se_logit <- 0
  model_data <- fit$data
  if (!is.null(model_data)) {
    if ("has_se" %in% names(model_data) && !"has_se" %in% names(newdata)) newdata$has_se <- 0L
    resp_var <- tryCatch(as.character(stats::formula(fit)$resp), error = function(e) character(0))
    fill_vars <- setdiff(names(model_data), c(names(newdata), resp_var, "ylogit"))
    for (v in fill_vars) {
      x <- model_data[[v]]
      newdata[[v]] <- if (is.factor(x)) {
        factor(levels(x)[1], levels = levels(x), ordered = is.ordered(x))
      } else if (is.logical(x)) {
        FALSE
      } else if (is.numeric(x)) {
        stats::median(x, na.rm = TRUE)
      } else {
        x[1]
      }
    }
  }
  newdata
}

#' Tidy summary for Bayesian meta-analysis (brms) on logit-accuracy
#'
#' @param fit brmsfit with main response "y_logit" (student/gaussian on logit
#' scale), optional sigma and imputation submodels.
#' @param data data.frame used to fit (only needed if you want default 'at' 
#' medians/modes).
#' @param by character vector of variable names to make a prediction grid over 
#' (e.g., "algo").
#' @param at named list of fixed values for covariates in predictions (e.g., 
#' `list(logn = median(data$logn, na.rm=TRUE), multimodal = 0))`. 
#' If omitted for numeric vars present in the model, medians are used; for 
#' factors, the reference level; for binaries, the mode.
#' @param prob credible interval width (default 0.95).
#' @param re one of c("population","study"). "population" marginalizes over REs 
#' (re_formula = NA), "study" keeps them (re_formula = NULL).
#' @param fit_null optional brmsfit for the null (intercept + (1|id) + simplest 
#' sigma).
#'
#' @return list(heterogeneity, r2_meta, predictions)
tidy_ma_summary <- function(
    fit, data = NULL, by = NULL, at = list(),
    prob = 0.95, re = c("population","study"),
    fit_null = NULL
    ) {
  
  stopifnot(inherits(fit, "brmsfit"))
  re <- match.arg(re)
  re_formula <- if (re == "population") NA else NULL
  
  suppressPackageStartupMessages({
    library(dplyr); library(tibble); library(brms); library(purrr)
  })
  
  draws <- brms::as_draws_df(fit)
  vnames <- names(draws)
  
  # Helper to find parameter column robustly
  find_col <- function(pattern, exclude = NULL) {
    idx <- grepl(pattern, vnames)
    if (!is.null(exclude)) idx <- idx & !grepl(exclude, vnames)
    vnames[idx]
  }
  
  # brms sanitizes response to "ylogit" (underscore removed)
  # τ (sd of random intercept for mean of ylogit)
  #tau_cols <- find_col(
  #  "^sd_.*ylogit.*__Intercept$|^sd_.*ylogit.*Intercept$", 
  #  exclude = "sigma"
  #  )
  #if (length(tau_cols) == 0) {
  #  stop("Could not find τ (sd for ylogit random intercept). Inspect names(fit) via as_draws_df().")
  #}
  #tau_draws <- as.numeric(draws[[tau_cols[1]]])
  
  # robust τ (between-study SD) extractor for brms fits
  .find_tau_draws <- function(fit) {
    dr <- brms::as_draws_df(fit); vn <- names(dr)
    
    # 1) Prefer non-sigma random-intercept SDs by name (works for your fit0)
    cand <- vn[grepl("^sd_.*__Intercept$", vn) & !grepl("__sigma", vn)]
    if (length(cand)) return(as.numeric(dr[[cand[1]]]))
    
    # 2) If multivariate/resp-tagged (e.g., ylogit), still try to avoid sigma
    cand2 <- vn[grepl("^sd_.*Intercept$", vn) & !grepl("sigma", vn)]
    if (length(cand2)) return(as.numeric(dr[[cand2[1]]]))
    
    # 3) Fallback via VarCorr draws
    VC <- brms::VarCorr(fit, summary = FALSE)
    # pick the first grouping term, take sd of intercept
    grp <- names(VC)[1]
    if (!is.null(VC[[grp]])) {
      arr <- VC[[grp]][[1]]   # iterations x coefs x ...
      dn  <- dimnames(arr)[[2]]
      j   <- which(grepl("Intercept", dn, fixed = TRUE))
      if (length(j)) return(as.numeric(arr[, j[1], 1]))
    }
    
    stop("Could not find τ (sd of random intercept) in this fit.")
  }
  tau_draws <- .find_tau_draws(fit)
  tau2_draws <- tau_draws^2
  
  # Optional heterogeneity in sigma (if you had (1|id) in sigma)
  # --- tau_sigma (study RE in sigma) -> more permissive name match ---
  find_sigma_tau <- function(fit) {
    dr <- brms::as_draws_df(fit); vn <- names(dr)
    cand <- vn[grepl("^sd_.*__sigma.*Intercept$", vn)]
    if (length(cand)) return(as.numeric(dr[[cand[1]]]))
    # fallback via VarCorr if available
    VC <- try(brms::VarCorr(fit, summary = FALSE), silent = TRUE)
    if (!inherits(VC, "try-error")) {
      for (grp in names(VC)) {
        if (!is.null(VC[[grp]][["sigma"]])) {
          arr <- VC[[grp]][["sigma"]]; dn <- dimnames(arr)[[2]]
          j <- which(grepl("Intercept", dn, fixed = TRUE))
          if (length(j)) return(as.numeric(arr[, j[1], 1]))
        }
      }
    }
    NULL
  }
  tau_sigma_draws <- find_sigma_tau(fit)
  
  # Student-t df for ylogit (if present)
  nu_cols <- find_col("^nu(_.*)?ylogit$")
  nu_draws <- if (length(nu_cols)) as.numeric(draws[[nu_cols[1]]]) else NULL
  
  heterogeneity <- tibble(
    param = c("tau", "tau2", if (!is.null(tau_sigma_draws)) "tau_sigma", if (!is.null(nu_draws)) "nu"),
    median = c(median(tau_draws), median(tau2_draws),
               if (!is.null(tau_sigma_draws)) median(tau_sigma_draws),
               if (!is.null(nu_draws)) median(nu_draws)),
    lower  = c(quantile(tau_draws, (1-prob)/2), quantile(tau2_draws, (1-prob)/2),
               if (!is.null(tau_sigma_draws)) quantile(tau_sigma_draws, (1-prob)/2),
               if (!is.null(nu_draws)) quantile(nu_draws, (1-prob)/2)),
    upper  = c(quantile(tau_draws, 1-(1-prob)/2), quantile(tau2_draws, 1-(1-prob)/2),
               if (!is.null(tau_sigma_draws)) quantile(tau_sigma_draws, 1-(1-prob)/2),
               if (!is.null(nu_draws)) quantile(nu_draws, 1-(1-prob)/2))
  )
  
  # R2_meta if a null is provided (fit_null or tau2_null)
  r2_meta <- NULL
  if (!is.null(fit_null)) {
    tau0_draws  <- .find_tau_draws(fit_null)
    r2_draws    <- 1 - (tau2_draws / (tau0_draws^2))
    r2_meta <- tibble::tibble(
      metric = "R2_meta",
      median = median(r2_draws),
      lower  = quantile(r2_draws, (1 - prob) / 2),
      upper  = quantile(r2_draws, 1 - (1 - prob) / 2)
    )
  }
  
  # ---- Predictions (back-transformed accuracy) ----
  # Build newdata grid
  build_grid <- function(fit, data, by, at) {
    # If no 'by', make a single row using medians/modes or provided 'at'
    if (is.null(by) || !length(by)) {
      row <- list()
      # Use provided 'at'
      if (length(at)) row <- at
      # Fallback to data for typical covariates
      if (!is.null(data)) {
        add_default <- function(var) {
          if (!is.null(row[[var]])) return()
          if (!var %in% names(data)) return()
          x <- data[[var]]
          if (is.numeric(x)) row[[var]] <<- stats::median(x, na.rm = TRUE)
          else if (is.factor(x)) row[[var]] <<- levels(x)[1]
          else if (all(x %in% c(0,1,NA))) row[[var]] <<- as.integer(stats::median(x, na.rm=TRUE) > 0.5)
        }
        # Common suspects
        for (v in c("logn","multimodal","algo","ECG","PPG","EDA","SCG","VHF","microphone")) add_default(v)
      }
      return(tibble::as_tibble(row, .name_repair = "minimal"))
    }
    # Expand grid over 'by' variables
    if (is.null(data)) stop("Provide 'data' or a fully-specified 'at' when using 'by'.")
    grid <- data %>% dplyr::select(dplyr::all_of(by)) %>% distinct()
    # Fill any missing levels in factors from full set of levels
    for (b in by) {
      if (is.factor(data[[b]])) {
        grid <- grid %>%
          tidyr::complete(!!rlang::sym(b) := levels(data[[b]]))
      }
    }
    # Bind 'at' constants across rows
    if (length(at)) {
      for (nm in names(at)) grid[[nm]] <- at[[nm]]
    } else {
      # reasonable defaults
      if ("logn" %in% names(data) && !"logn" %in% names(grid))
        grid$logn <- stats::median(data$logn, na.rm = TRUE)
      if ("multimodal" %in% names(data) && !"multimodal" %in% names(grid))
        grid$multimodal <- as.integer(stats::median(data$multimodal, na.rm=TRUE) > 0.5)
      for (m in c("ECG","PPG","EDA","SCG","VHF","microphone")) {
        if (m %in% names(data) && !m %in% names(grid)) grid[[m]] <- 0L
      }
    }
    tibble::as_tibble(grid)
  }
  
  newdata <- ma_complete_newdata(fit, build_grid(fit, data, by, at))

  # --- predict the correct response explicitly ---
  mu_draws <- brms::posterior_epred(
    fit,
    newdata    = newdata,
    resp       = "ylogit",
    dpar       = "mu",
    re_formula = re_formula
  )
  acc_draws <- plogis(mu_draws)
  
  q_lo <- (1 - prob)/2; q_hi <- 1 - q_lo
  pred <- tibble::as_tibble(newdata) %>%
    mutate(.row = dplyr::row_number()) %>%
    left_join(tibble::tibble(
      .row = seq_len(ncol(acc_draws)),
      acc_median = apply(acc_draws, 2, stats::median, na.rm = TRUE),
      acc_mean   = apply(acc_draws, 2, mean, na.rm = TRUE),
      acc_lower  = apply(acc_draws, 2, stats::quantile, probs = q_lo, na.rm = TRUE),
      acc_upper  = apply(acc_draws, 2, stats::quantile, probs = q_hi, na.rm = TRUE),
      ylogit_med = apply(mu_draws,   2, stats::median, na.rm = TRUE)
    ), by = ".row") %>%
    select(-.row)
  
  list(
    heterogeneity = heterogeneity,
    r2_meta       = r2_meta,
    predictions   = pred
  )
}