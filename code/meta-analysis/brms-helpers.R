#' Posterior draws → predicted probabilities for selected predictors (brms)
#'
#' Extracts posterior draws of fixed-effect coefficients from a `brmsfit` and
#' computes predicted probabilities by adding an optional baseline (Intercept and/or
#' user-specified fixed terms) and applying the inverse-logit. Works with models
#' fit via `brm_multiple` as well.
#'
#' @param fit A `brmsfit` (including results from `brm_multiple`).
#' @param vars Either:
#'   * character vector of coefficient names (each returned separately), or
#'   * a named list of character vectors (each list element is summed as a group).
#'   Use *mean-part* coefficient names as shown by `fixef(fit)`, e.g. "dl_between",
#'   "kernel_between". Do **not** include the "b_" prefix.
#' @param include_intercept Logical; if `TRUE` and the model has `b_Intercept`,
#'   it is added to the linear predictor. Default `FALSE` (pure contrasts).
#' @param add_terms Optional named numeric vector of additional fixed terms to
#'   include as baseline, e.g. `c(logn = log(40), multimodal = 1)`. Each name must
#'   match a mean-part coefficient in `fixef(fit)`.
#' @param transform Logical; if `TRUE` (default) return probabilities via `plogis`.
#'   If `FALSE`, return linear predictor draws (y-logit scale).
#' @param probs Numeric vector of quantiles to summarise (default `c(.025,.5,.975)`).
#' @param tidy Logical; if `TRUE` (default) return a long data.frame with draws and
#'   a summary table. If `FALSE`, return a list with raw matrices per group.
#'
#' @return If `tidy=TRUE`, a list with:
#' \itemize{
#'   \item `draws`: data.frame with columns `group`, `draw`, `eta`, `prob`
#'   \item `summary`: data.frame with `group`, `eta_mean`, `eta_lo`, `eta_hi`,
#'         and if `transform=TRUE`, `p_mean`, `p_lo`, `p_hi`
#' }
#' If `tidy=FALSE`, a list of numeric matrices (rows = draws, cols = groups).
#'
#' @examples
#' # One predictor (pure contrast on logit scale, then prob)
#' res <- predictor_draws_to_prob(fit, "dl_between", include_intercept = FALSE)
#' # Many predictors separately
#' res <- predictor_draws_to_prob(fit, c("dl_between", "kernel_between"))
#' # Grouped predictors + baseline logn
#' res <- predictor_draws_to_prob(
#'   fit,
#'   vars = list(Deep = c("dl_between", "dl_temp_between"), Classical = c("kernel_between", "tree_between")),
#'   include_intercept = TRUE,
#'   add_terms = c(logn = log(40), multimodal = 0)
#' )
predictor_draws_to_prob <- function(
    fit,
    vars,
    include_intercept = FALSE,
    add_terms = NULL,
    transform = TRUE,
    probs = c(.025, .5, .975),
    tidy = TRUE
){
  stopifnot(inherits(fit, "brmsfit"))
  
  # Pull all draws as a data.frame (works for brm_multiple too)
  dr <- posterior::as_draws_df(fit)
  
  # Mean-part fixed effects columns start with 'b_' but not 'b_sigma_'
  b_cols <- grep("^b_(?!sigma_)", colnames(dr), perl = TRUE, value = TRUE)
  
  # Helper: fetch a coefficient draw by bare name (no b_), error if missing
  coef_draws <- function(name){
    col <- paste0("b_", name)
    if (!col %in% b_cols) stop("Coefficient '", name, "' not found in mean part (try names(fixef(fit))).")
    as.numeric(dr[[col]])
  }
  
  # Baseline (eta0): optional Intercept + optional add_terms
  eta0 <- rep(0, nrow(dr))
  if (include_intercept && "b_Intercept" %in% b_cols) {
    eta0 <- eta0 + as.numeric(dr[["b_Intercept"]])
  }
  if (!is.null(add_terms)) {
    stopifnot(is.numeric(add_terms), !is.null(names(add_terms)))
    for (nm in names(add_terms)) {
      eta0 <- eta0 + add_terms[[nm]] * coef_draws(nm)
    }
  }
  
  # Normalise `vars` to a named list of character vectors
  if (is.list(vars)) {
    groups <- vars
    if (is.null(names(groups))) names(groups) <- paste0("grp", seq_along(groups))
  } else if (is.character(vars)) {
    groups <- as.list(vars)
    names(groups) <- vars
  } else {
    stop("`vars` must be a character vector or a named list of character vectors.")
  }
  
  # Build draws per group
  group_eta <- lapply(groups, function(vs){
    eta <- eta0
    for (nm in vs) eta <- eta + coef_draws(nm)
    eta
  })
  
  if (!tidy) return(group_eta)
  
  # Tidy draws and summaries
  out_draws <- do.call(rbind, lapply(seq_along(group_eta), function(i){
    eta <- group_eta[[i]]
    data.frame(
      group = names(group_eta)[i],
      draw  = seq_along(eta),
      eta   = eta,
      prob  = if (transform) plogis(eta) else NA_real_,
      row.names = NULL
    )
  }))
  
  # Summaries
  sum_list <- lapply(seq_along(group_eta), function(i){
    eta <- group_eta[[i]]
    qs  <- stats::quantile(eta, probs = probs)
    row <- data.frame(
      group   = names(group_eta)[i],
      eta_mean= mean(eta),
      eta_lo  = qs[1],
      eta_med = qs[2],
      eta_hi  = qs[3],
      stringsAsFactors = FALSE
    )
    if (transform) {
      p <- plogis(eta)
      qs_p <- stats::quantile(p, probs = probs)
      row$p_mean <- mean(p); row$p_lo <- qs_p[1]; row$p_med <- qs_p[2]; row$p_hi <- qs_p[3]
    }
    row
  })
  out_sum <- do.call(rbind, sum_list)
  
  list(draws = out_draws, summary = out_sum)
}




#' Pooled ICC for a predictor across clustered studies with multiple imputations
#'
#' Computes the intraclass correlation coefficient (ICC, one-way random-effects / ICC[1])
#' for a predictor measured across experiments clustered in studies, for a list of
#' imputed datasets. Works for both continuous and binary predictors and pools
#' estimates across imputations using Rubin's Rules. Within-imputation variance is
#' estimated by a leave-one-cluster jackknife. Pooling is performed on the logit
#' scale by default to respect the [0,1] bounds.
#'
#' @param milist A \code{list} of imputed data.frames (each with identical columns).
#' @param var A \code{character} scalar: column name of the predictor whose ICC you want.
#' @param id A \code{character} scalar: column name of the cluster/study ID.
#' @param type \code{"continuous"} or \code{"factor"}. For \code{"factor"}, the
#'   predictor must be binary; factors are coerced to \{0,1\} using the first
#'   level as 0 and the second as 1. If already numeric, values must be 0/1.
#' @param transform \code{"logit"} (default) or \code{"none"}.
#'   If \code{"logit"}, ICCs are pooled on the logit scale via delta-method
#'   variance transformation and back-transformed for reporting.
#' @param clamp_eps Small epsilon used to clamp ICCs away from 0/1 before logit
#'   transforms to avoid infinities. Default \code{1e-6}.
#' @param na.rm Logical; if \code{TRUE} (default), rows with \code{NA} in \code{var}
#'   or \code{id} are dropped within each imputed dataset.
#'
#' @return A \code{list} with elements:
#' \itemize{
#'   \item \code{estimate}: pooled ICC (on original 0–1 scale).
#'   \item \code{conf_low}, \code{conf_high}: 95\% CI limits (original scale).
#'   \item \code{df}: Rubin's Rules degrees of freedom used for the CI.
#'   \item \code{M_eff}: number of imputations actually used (after dropping failures).
#'   \item \code{Ubar}, \code{B}, \code{T}: within-, between-, and total variances on the pooling scale.
#'   \item \code{per_imputation}: data.frame with ICC and SE per imputation (original scale).
#' }
#'
#' @details
#' The per-imputation ICC is computed via method-of-moments:
#' \deqn{ICC = (MSB - MSW) / (MSB + (kbar - 1) MSW),}
#' where MSB and MSW are between- and within-cluster mean squares, and \eqn{kbar}
#' is the mean cluster size (handles unbalanced designs). The within-imputation
#' variance of the ICC is estimated by leave-one-cluster jackknife:
#' \deqn{Var_{JK}(\hat\theta) = \frac{J-1}{J} \sum_{j=1}^J (\hat\theta_{(-j)} - \bar\theta_{(-\cdot)})^2.}
#'
#' For binary predictors, the same estimator is applied to the \{0,1\} numeric
#' values (i.e., ICC on the observed probability scale). If you prefer a latent-logit
#' ICC, fit a logistic random-intercept model separately; this helper focuses on a
#' model-free diagnostic ICC suited to within/between splitting decisions.
#'
#' Rubin's Rules pooling is performed on the logit scale by default:
#' each ICC and its jackknife variance are delta-transformed, pooled, then
#' back-transformed for reporting.
#'
#' @examples
#' # Suppose you have a list of imputed data.frames `milist`, each with columns:
#' #   id = study id, x = predictor (continuous or binary).
#' # Continuous example:
#' # res <- pool_icc(milist, var = "duration", id = "id", type = "continuous")
#' # Binary (factor) example:
#' # res <- pool_icc(milist, var = "algo_CNN", id = "id", type = "factor")
#' # str(res)
#'
#' @importFrom stats var qt
#' @export
pool_icc <- function(milist, var, id, type = c("continuous", "factor"),
                     transform = c("logit", "none"),
                     clamp_eps = 1e-6, na.rm = TRUE) {
  type <- match.arg(type)
  transform <- match.arg(transform)
  
  if (!is.list(milist) || length(milist) < 1L) {
    stop("`milist` must be a non-empty list of data.frames.")
  }
  
  icc_one <- function(df) {
    if (!is.data.frame(df)) stop("Each element of `milist` must be a data.frame.")
    if (!all(c(var, id) %in% names(df))) {
      stop("Columns `var` and/or `id` not found in a dataset.")
    }
    d <- df[, c(var, id)]
    names(d) <- c("x", "id")
    if (na.rm) d <- stats::na.omit(d)
    if (!is.factor(d$id)) d$id <- factor(d$id)
    
    # Coerce predictor as needed
    if (type == "factor") {
      if (is.factor(d$x)) {
        if (nlevels(d$x) != 2L) stop("For type='factor', the predictor must be binary (2 levels).")
        d$x <- as.integer(d$x) - 1L  # first level -> 0, second -> 1
      } else {
        # must be numeric 0/1
        if (is.logical(d$x)) d$x <- as.integer(d$x)
        if (!is.numeric(d$x)) stop("For type='factor', predictor must be a factor or numeric 0/1.")
        ux <- unique(na.omit(d$x))
        if (!all(ux %in% c(0,1))) stop("Numeric factor predictor must be coded 0/1.")
      }
    } else {
      # continuous: ensure numeric
      if (!is.numeric(d$x)) stop("For type='continuous', predictor must be numeric.")
    }
    
    # Cluster sizes and means
    n_j <- table(d$id)
    J <- length(n_j)
    if (J < 2L) stop("Need at least 2 clusters to compute ICC.")
    N <- nrow(d)
    xbar <- mean(d$x)
    xbar_j <- tapply(d$x, d$id, mean)
    # Sums of squares
    SSB <- sum(n_j * (xbar_j - xbar)^2)
    # Within: sum over clusters of sum((x_ij - xbar_j)^2)
    SSW <- sum(tapply(d$x, d$id, function(v) sum((v - mean(v))^2)))
    MSB <- SSB / (J - 1)
    MSW <- SSW / (N - J)
    kbar <- mean(as.numeric(n_j))  # mean cluster size
    
    # MoM ICC (one-way random effects), clamp to [0,1]
    icc_hat <- (MSB - MSW) / (MSB + (kbar - 1) * MSW)
    icc_hat <- max(0, min(1, icc_hat))
    
    # Jackknife variance over clusters
    theta_minus <- numeric(J)
    ids <- levels(d$id)
    for (j in seq_len(J)) {
      d_j <- d[d$id != ids[j], , drop = FALSE]
      grps <- split(d_j$x, droplevels(d_j$id), drop = TRUE)
      
      J2 <- length(grps); N2 <- length(d_j$x)
      if (J2 < 2L) next
      
      n_j2     <- lengths(grps)
      xbar_j2  <- sapply(grps, mean)
      xbar2    <- mean(d_j$x)
      SSW2     <- sum(sapply(grps, function(v) sum((v - mean(v))^2)))
      SSB2     <- sum(n_j2 * (xbar_j2 - xbar2)^2)
      MSB2     <- SSB2 / (J2 - 1)
      MSW2     <- SSW2 / (N2 - J2)
      kbar2    <- mean(n_j2)
      theta_minus[j] <- (MSB2 - MSW2) / (MSB2 + (kbar2 - 1) * MSW2)
    }
    theta_minus <- theta_minus[is.finite(theta_minus)]
    if (length(theta_minus) >= 2L) {
      theta_bar <- mean(theta_minus, na.rm = TRUE)
      var_jk <- ((J - 1) / J) * sum((theta_minus - theta_bar)^2, na.rm = TRUE)
      se_jk <- sqrt(max(var_jk, 0))
    } else {
      var_jk <- NA_real_
      se_jk <- NA_real_
    }
    
    list(icc = icc_hat, var = var_jk, se = se_jk, J = J, N = N)
  }
  
  # Compute per-imputation ICC + jackknife variance
  per_imp <- lapply(milist, function(df) {
    out <- try(icc_one(df), silent = TRUE)
    if (inherits(out, "try-error")) {
      list(icc = NA_real_, var = NA_real_, se = NA_real_, J = NA_integer_, N = NA_integer_)
    } else out
  })
  icc_vec <- vapply(per_imp, `[[`, numeric(1), "icc")
  var_vec <- vapply(per_imp, `[[`, numeric(1), "var")
  se_vec  <- vapply(per_imp, `[[`, numeric(1), "se")
  J_vec   <- vapply(per_imp, `[[`, numeric(1), "J")
  N_vec   <- vapply(per_imp, `[[`, numeric(1), "N")
  
  ok <- is.finite(icc_vec) & icc_vec >= 0 & icc_vec <= 1 & is.finite(var_vec)
  if (!any(ok)) stop("Failed to compute ICC and variance on any imputed dataset.")
  
  icc_vec <- icc_vec[ok]
  var_vec <- var_vec[ok]
  se_vec  <- se_vec[ok]
  J_vec   <- J_vec[ok]
  N_vec   <- N_vec[ok]
  M_eff <- length(icc_vec)
  
  # Pool via Rubin's Rules (optionally on logit scale)
  if (transform == "logit") {
    # clamp away from 0/1 for transform
    icc_c <- pmin(pmax(icc_vec, clamp_eps), 1 - clamp_eps)
    z <- qlogis(icc_c)
    # delta-method variance on logit scale
    # Var[logit(p)] ≈ Var[p] / (p^2 (1-p)^2)
    var_z <- var_vec / (icc_c^2 * (1 - icc_c)^2)
    # Rubin pooling
    Qbar <- mean(z)
    Ubar <- mean(var_z, na.rm = TRUE)
    B <- stats::var(z)
    Tvar <- Ubar + (1 + 1/M_eff) * B
    # df per Barnard-Rubin
    r <- (1 + 1/M_eff) * B / Ubar
    df <- if (is.finite(r) && r > 0) (M_eff - 1) * (1 + 1/r)^2 else M_eff - 1
    se_total <- sqrt(Tvar)
    # 95% CI on logit scale
    tcrit <- stats::qt(0.975, df = df)
    lo_z <- Qbar - tcrit * se_total
    hi_z <- Qbar + tcrit * se_total
    # back-transform
    icc_hat <- plogis(Qbar)
    conf_low <- plogis(lo_z)
    conf_high <- plogis(hi_z)
  } else {
    Qbar <- mean(icc_vec)
    Ubar <- mean(var_vec, na.rm = TRUE)
    B <- stats::var(icc_vec)
    Tvar <- Ubar + (1 + 1/M_eff) * B
    r <- (1 + 1/M_eff) * B / Ubar
    df <- if (is.finite(r) && r > 0) (M_eff - 1) * (1 + 1/r)^2 else M_eff - 1
    se_total <- sqrt(Tvar)
    tcrit <- stats::qt(0.975, df = df)
    icc_hat <- Qbar
    conf_low <- max(0, Qbar - tcrit * se_total)
    conf_high <- min(1, Qbar + tcrit * se_total)
  }
  
  list(
    estimate = icc_hat,
    conf_low = conf_low,
    conf_high = conf_high,
    df = df,
    M_eff = M_eff,
    Ubar = Ubar, B = B, T = Tvar,
    per_imputation = data.frame(
      imputation = seq_len(M_eff),
      icc = icc_vec,
      se = se_vec,
      J = J_vec,
      N = N_vec
    ),
    pooling_scale = transform
  )
}