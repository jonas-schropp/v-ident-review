#' Add Effect Coding
#'
#' @param milist the list of imputed data sets
#'
#' @returns the milist with effect coded predictors
#'
add_effects <- function(milist) {

  require(dplyr)
  milist_attributes <- attributes(milist)

  for (m in seq_along(milist)) {
    milist[[m]] <- milist[[m]] %>%
      mutate(
        oneC = activity == 'one condition',
        multiC = activity == 'multiple conditions',
        everyC = activity %in% c('everyday activities', 'everyday conditions'),

        multiC_between = ave(multiC, id),
        multiC_within  = multiC - multiC_between,
        everyC_between = ave(everyC, id),
        everyC_within  = everyC - everyC_between,

        multiD = duration %in% c("medium (multiple days)", "multiple days"),
        manyD = duration %in% c("high (> one week)", ">1 week"),

        multiD_between = ave(multiD, id),
        multiD_within  = multiD - multiD_between,
        manyD_between = ave(manyD, id),
        manyD_within  = manyD - manyD_between,

        ECG_between = ave(ECG, id),
        PPG_between = ave(PPG, id),

        ECG_within = ECG - ECG_between,
        PPG_within = PPG - PPG_between,

        multimodal_between = ave(multimodal, id),
        multimodal_within = multimodal - multimodal_between,

        intruders = intruders == "intruders",
        intruders_between = ave(intruders, id),
        intruders_within = intruders - intruders_between,

        # algo
        dl = algo == "dl",
        dl_temp = algo == "dl_temp",
        tree = algo == "tree",
        kernel = algo == "kernel",

        dl_between = ave(dl, id),
        dl_temp_between = ave(dl_temp, id),
        tree_between = ave(tree, id),
        kernel_between = ave(kernel, id),

        dl_within = dl - dl_between,
        dl_temp_within = dl_temp - dl_temp_between,
        tree_within = tree - tree_between,
        kernel_within = kernel - kernel_between
      )
  }

  attributes(milist) <- milist_attributes
  return(milist)

}




#' Check the fixed-effect imputation design for exact collinearity
#'
#' `jomoImpute()` includes an intercept for fixed effects. This helper therefore
#' detects rank deficiency in completely observed fixed-effect predictors before
#' the imputation run, which catches dummy sets such as ECG + PPG + Other.
#'
#' @param dat_imp Data frame passed to `jomoImpute()`.
#' @param type Named integer vector using `jomoImpute()` type codes.
#'
#' @returns Invisibly returns `TRUE` when the design is full rank.
#'
check_imputation_collinearity <- function(dat_imp, type) {
  fixed_vars <- names(type)[type %in% c(2, 3)]
  fixed_vars <- fixed_vars[fixed_vars %in% names(dat_imp)]

  if (length(fixed_vars) < 1L) {
    return(invisible(TRUE))
  }

  complete_fixed <- fixed_vars[vapply(dat_imp[fixed_vars], function(x) all(!is.na(x)), logical(1))]

  if (length(complete_fixed) < 1L) {
    return(invisible(TRUE))
  }

  design_data <- dat_imp[complete_fixed]
  design <- stats::model.matrix(~ ., data = design_data)
  rank <- qr(design)$rank

  if (rank < ncol(design)) {
    stop(
      "Imputation fixed-effect design is rank deficient. ",
      "Review the imputation variable list/type codes and omit a reference level ",
      "from deterministic dummy sets (for example, include ECG and PPG while ",
      "leaving Other as the documented modality reference).",
      call. = FALSE
    )
  }

  invisible(TRUE)
}



#' Completed-data access for meta-analysis imputations
#'
#' Complete meta-analysis imputations for downstream modeling
#'
#' The on-disk imputation artifact is intentionally the compact imputation object
#' (`mids` for the `mice` engine, `mitml`/`jomo` object for the jomo engine)
#' rather than the expanded list of completed data frames. This helper materializes
#' the completed-data list only when it is needed for analysis.
#'
#' @param imputed An object returned by `impute_ma()` or `impute_ma_mice()`, or a
#'   legacy list of completed data frames.
#'
#' @returns The completed-data list.
completed_ma_data <- function(imputed) {
  if (inherits(imputed, "mids")) {
    return(complete_mice_ma_data(imputed))
  }

  if (inherits(imputed, "mitml")) {
    completed_list <- mitml::mitmlComplete(imputed, "all")
    original_data <- attr(imputed, "original_data")
    imputation_data <- attr(imputed, "imputation_data")

    if (!is.null(original_data) && !is.null(imputation_data)) {
      completed_list <- lapply(completed_list, function(d) {
        cbind(d, original_data[, setdiff(names(original_data), names(imputation_data)), drop = FALSE])[, names(original_data), drop = FALSE]
      })
    }

    return(completed_list)
  }

  imputed
}

#' Read compact meta-analysis imputations and materialize completed data
#'
#' @param path Path to the saved compact imputation RDS.
#' @param effects Logical; if `TRUE`, recalculate within/between effect coding in
#'   each completed data set.
#'
#' @returns A list of completed data frames ready for meta-analysis modeling.
read_ma_imputed_data <- function(path, effects = TRUE) {
  out <- completed_ma_data(readRDS(path))
  if (isTRUE(effects)) {
    out <- add_effects(out)
  }
  out
}

#' Validate shared meta-analysis imputation inputs
#'
#' @inheritParams impute_ma
#'
#' @returns A list containing the imputation data, aligned type vector, and the
#'   cluster variable name.
validate_ma_imputation_inputs <- function(data, variables, type) {
  if (missing(variables) || missing(type)) {
    stop("`variables` and `type` must be supplied by the calling script.", call. = FALSE)
  }

  missing_variables <- setdiff(variables, names(data))
  if (length(missing_variables) > 0L) {
    stop(
      "Imputation variables are missing from `data`: ",
      paste(missing_variables, collapse = ", "),
      call. = FALSE
    )
  }

  missing_types <- setdiff(variables, names(type))
  if (length(missing_types) > 0L) {
    stop(
      "Imputation type codes are missing for: ",
      paste(missing_types, collapse = ", "),
      call. = FALSE
    )
  }

  dat_imp <- data[, variables, drop = FALSE]
  type <- type[names(dat_imp)]
  cluster_vars <- names(type)[type == -2]
  if (length(cluster_vars) != 1L) {
    stop("Exactly one imputation variable must be coded -2 as the paper/cluster ID.", call. = FALSE)
  }
  if (anyNA(dat_imp[[cluster_vars]])) {
    stop("The paper/cluster ID used for multilevel imputation contains missing values.", call. = FALSE)
  }

  fixed_vars <- names(type)[type %in% c(2, 3)]
  incomplete_fixed <- fixed_vars[vapply(dat_imp[fixed_vars], anyNA, logical(1))]
  if (length(incomplete_fixed) > 0L) {
    stop(
      "Variables coded 2 or 3 must be completely observed: ",
      paste(incomplete_fixed, collapse = ", "),
      call. = FALSE
    )
  }

  target_vars <- names(type)[type == 1]
  empty_targets <- target_vars[vapply(dat_imp[target_vars], function(x) all(is.na(x)), logical(1))]
  if (length(empty_targets) > 0L) {
    stop(
      "Imputation targets must have at least one observed value: ",
      paste(empty_targets, collapse = ", "),
      call. = FALSE
    )
  }

  list(data = dat_imp, type = type, cluster = cluster_vars)
}


#' Replace factor levels with syntactic labels for `mice` internals
#'
#' Some `miceadds` two-level methods build lme4 formula strings from the dummy
#' names generated for factor predictors. Factor levels containing spaces or
#' punctuation can therefore produce non-syntactic formula terms. Keep the data
#' values semantically equivalent during imputation, but restore the original
#' analysis labels before returning completed data sets.
#'
#' @param dat_imp Data frame passed to `mice()`.
#'
#' @returns A list with sanitized data and original/sanitized level mappings.
#'
sanitize_mice_factor_levels <- function(dat_imp) {
  mappings <- list()

  for (nm in names(dat_imp)) {
    if (!is.factor(dat_imp[[nm]])) {
      next
    }

    original_levels <- levels(dat_imp[[nm]])
    safe_levels <- make.names(original_levels, unique = TRUE)

    if (!identical(original_levels, safe_levels)) {
      levels(dat_imp[[nm]]) <- safe_levels
      mappings[[nm]] <- list(original = original_levels, safe = safe_levels)
    }
  }

  list(data = dat_imp, mappings = mappings)
}

#' Restore factor levels sanitized for `mice` internals
#'
#' @param dat Completed imputed data frame.
#' @param mappings Level mappings returned by `sanitize_mice_factor_levels()`.
#'
#' @returns `dat` with original factor labels restored where applicable.
#'
restore_mice_factor_levels <- function(dat, mappings) {
  for (nm in intersect(names(mappings), names(dat))) {
    if (is.factor(dat[[nm]])) {
      levels(dat[[nm]]) <- mappings[[nm]]$original
    }
  }

  dat
}

#' Build the target-specific `mice` method and predictor matrix for MA imputation
#'
#' @inheritParams impute_ma_mice
#'
#' @returns A list with data, method, predictorMatrix, target-method mapping, and
#'   names of internal columns used only during fitting.
prepare_mice_ma_spec <- function(
    data,
    variables,
    type,
    donors = 5,
    check_collinearity = TRUE,
    use_ylogit_within_between = FALSE) {

  validated <- validate_ma_imputation_inputs(data, variables, type)
  dat_imp <- validated$data
  type <- validated$type
  cluster <- validated$cluster
  original_cluster <- dat_imp[[cluster]]
  dat_imp[[cluster]] <- match(original_cluster, unique(original_cluster))

  if ("feature_type" %in% names(dat_imp)) {
    dat_imp$feature_type <- factor(dat_imp$feature_type, levels = c("handcrafted", "deep", "hybrid"))
  }

  sanitized <- sanitize_mice_factor_levels(dat_imp)
  dat_imp <- sanitized$data
  factor_level_mappings <- sanitized$mappings

  if (isTRUE(use_ylogit_within_between) && "ylogit" %in% names(dat_imp)) {
    if (anyNA(dat_imp$ylogit)) {
      stop("`use_ylogit_within_between` requires completely observed `ylogit`.", call. = FALSE)
    }
    dat_imp$.ylogit_between <- ave(dat_imp$ylogit, dat_imp[[cluster]], FUN = mean)
    dat_imp$.ylogit_within <- dat_imp$ylogit - dat_imp$.ylogit_between
    type <- c(type, .ylogit_between = 2, .ylogit_within = 2)
  }

  target_vars <- names(type)[type == 1]
  method <- rep("", ncol(dat_imp))
  names(method) <- names(dat_imp)
  internal_ordered <- list()
  internal_binary <- list()

  for (target in target_vars) {
    x <- dat_imp[[target]]
    if (is.ordered(x)) {
      internal_ordered[[target]] <- list(levels = levels(x), class = class(x))
      dat_imp[[target]] <- as.integer(x)
      method[target] <- "2l.pmm"
    } else if (is.logical(x)) {
      internal_binary[[target]] <- list(class = "logical", levels = NULL)
      dat_imp[[target]] <- as.integer(x)
      method[target] <- "2l.bin"
    } else if (is.factor(x) && nlevels(x) == 2L) {
      internal_binary[[target]] <- list(class = class(x), levels = levels(x))
      dat_imp[[target]] <- as.integer(x) - 1L
      method[target] <- "2l.bin"
    } else if (is.factor(x)) {
      method[target] <- "polyreg"
    } else if (is.numeric(x) || is.integer(x)) {
      method[target] <- "2l.pmm"
    } else {
      stop("Unsupported imputation target class for `", target, "`: ", paste(class(x), collapse = "/"), call. = FALSE)
    }
  }

  poly_targets <- target_vars[method[target_vars] == "polyreg"]
  internal_paper_factor <- character(0)
  if (length(poly_targets) > 0L) {
    internal_paper_factor <- ".paper_fixed_effect"
    dat_imp[[internal_paper_factor]] <- factor(dat_imp[[cluster]])
    method[internal_paper_factor] <- ""
    type <- c(type, .paper_fixed_effect = 2)
  }

  if (length(poly_targets) > 0L) {
    for (target in poly_targets) {
      completely_missing_papers <- tapply(is.na(dat_imp[[target]]), dat_imp[[cluster]], all)
      n_missing_papers <- sum(completely_missing_papers)
      if (n_missing_papers > 0L) {
        warning(
          "Multinomial target `", target, "` is missing for every experiment in ",
          n_missing_papers, " paper(s); paper fixed effects cannot supply paper-specific information there.",
          call. = FALSE
        )
      }
      random_slope_predictors <- names(type)[type == 3]
      if (length(random_slope_predictors) > 0L) {
        warning(
          "Multinomial target `", target, "` uses `polyreg`, which cannot represent type-3 random slopes: ",
          paste(random_slope_predictors, collapse = ", "),
          call. = FALSE
        )
      }
    }
  }

  predictor_matrix <- matrix(0, nrow = ncol(dat_imp), ncol = ncol(dat_imp), dimnames = list(names(dat_imp), names(dat_imp)))
  predictor_vars <- names(type)[type %in% c(1, 2, 3)]
  predictor_vars <- setdiff(predictor_vars, internal_paper_factor)

  for (target in target_vars) {
    if (method[target] == "polyreg") {
      vars <- setdiff(predictor_vars, target)
      predictor_matrix[target, vars] <- 1
      predictor_matrix[target, cluster] <- 0
      predictor_matrix[target, internal_paper_factor] <- 1
    } else {
      vars <- setdiff(predictor_vars, target)
      predictor_matrix[target, vars] <- 1
      random_slope_vars <- vars[type[vars] == 3]
      if (length(random_slope_vars) > 0L) {
        # For `miceadds::2l.pmm`, code 2 fits fixed effects with random
        # slopes; code 4 would additionally include cluster means. Keep the MA
        # type-3 predictors on code 2 to represent random slopes without adding
        # contextual aggregation that triggered the fragile aggregate() path in
        # `2l.contextual.pmm`.
        predictor_matrix[target, random_slope_vars] <- 2
      }
      predictor_matrix[target, cluster] <- -2
    }
  }

  if (isTRUE(check_collinearity)) {
    check_imputation_collinearity(validated$data, validated$type)
  }

  mapping <- data.frame(variable = target_vars, method = unname(method[target_vars]), stringsAsFactors = FALSE)
  attr(mapping, "donors") <- donors

  list(
    data = dat_imp,
    method = method,
    predictorMatrix = predictor_matrix,
    mapping = mapping,
    cluster = cluster,
    original_cluster = original_cluster,
    internal_columns = c(internal_paper_factor, names(type)[grepl("^\\.ylogit_", names(type))]),
    ordered = internal_ordered,
    binary = internal_binary,
    factor_level_mappings = factor_level_mappings
  )
}

#' Multiple imputation for MA with `mice`/`miceadds`
#'
#' @param data data set with missing values
#' @param m Number of imputations
#' @param variables Character vector of columns to pass to `mice()`.
#' @param type Named integer vector using the meta-analysis imputation type codes.
#' @param maxit Number of chained-equation iterations.
#' @param seed Optional random seed passed to `mice()`.
#' @param donors Number of donors used by predictive mean matching.
#' @param check_collinearity Logical; if `TRUE`, fail early when the fixed-effect
#'   imputation design is rank deficient.
#' @param use_ylogit_within_between Logical; if `TRUE`, add deterministic
#'   paper-level mean and experiment-level deviation of the complete `ylogit`
#'   outcome as internal auxiliary predictors for the mice engine only.
#' @param ... Additional arguments passed to `mice::mice()`.
#'
#' @returns A list of `m` completed data frames. The underlying `mids` object,
#'   `loggedEvents`, predictor matrix, and method mapping are retained as
#'   attributes for diagnostics.
#'
impute_ma_mice <- function(
    data,
    m,
    variables,
    type,
    maxit = 20,
    seed = NULL,
    donors = 5,
    check_collinearity = TRUE,
    use_ylogit_within_between = FALSE,
    ...) {

  requireNamespace("mice", quietly = TRUE)
  requireNamespace("miceadds", quietly = TRUE)
  require("miceadds", quietly = TRUE)

  spec <- prepare_mice_ma_spec(
    data = data,
    variables = variables,
    type = type,
    donors = donors,
    check_collinearity = check_collinearity,
    use_ylogit_within_between = use_ylogit_within_between
  )

  print(spec$mapping)

  imp <- mice::mice(
    spec$data,
    m = m,
    maxit = maxit,
    method = spec$method,
    predictorMatrix = spec$predictorMatrix,
    seed = seed,
    donors = donors,
    printFlag = FALSE,
    ...
  )

  attr(imp, "ma_imputation_spec") <- spec
  attr(imp, "original_data") <- data
  attr(imp, "method_mapping") <- spec$mapping
  attr(imp, "predictorMatrix") <- spec$predictorMatrix
  attr(imp, "imputation_engine") <- "mice"
  imp
}

#' Materialize completed data from a compact meta-analysis `mids` object
#'
#' @param imp A `mids` object returned by `impute_ma_mice()`.
#'
#' @returns A list of completed data frames with original labels and columns.
complete_mice_ma_data <- function(imp) {
  spec <- attr(imp, "ma_imputation_spec")
  data <- attr(imp, "original_data")

  if (is.null(spec) || is.null(data)) {
    return(mice::complete(imp, action = "all"))
  }

  lapply(seq_len(imp$m), function(i) {
    d <- mice::complete(imp, action = i)
    d[[spec$cluster]] <- spec$original_cluster
    for (target in names(spec$binary)) {
      if (identical(spec$binary[[target]]$class, "logical")) {
        d[[target]] <- as.logical(d[[target]])
      } else {
        d[[target]] <- factor(d[[target]], levels = c(0, 1), labels = spec$binary[[target]]$levels)
      }
    }
    for (target in names(spec$ordered)) {
      d[[target]] <- factor(
        d[[target]],
        levels = seq_along(spec$ordered[[target]]$levels),
        labels = spec$ordered[[target]]$levels,
        ordered = TRUE
      )
    }
    d <- restore_mice_factor_levels(d, spec$factor_level_mappings)
    d <- d[, setdiff(names(d), spec$internal_columns), drop = FALSE]
    cbind(d, data[, setdiff(names(data), names(d)), drop = FALSE])[, names(data), drop = FALSE]
  })
}

#' Multiple Imputation for MA
#'
#' @param data data set with missing values
#' @param m Number of imputations
#' @param variables Character vector of columns to pass to `jomoImpute()`.
#' @param type Named integer vector using `jomoImpute()` type codes.
#' @param check_collinearity Logical; if `TRUE`, fail early when the fixed-effect
#'   imputation design is rank deficient (for example, all levels of a dummy-coded
#'   factor are included with an intercept).
#'
#' @returns the mice object to be used with `complete()`
#'
impute_ma <- function(
    data, 
    m = 20, 
    variables, 
    type, 
    n.iter = 1000, 
    seed = 1, 
    n.burn = 5000,
    check_collinearity = TRUE) {

  library(mice)
  library(mitml)

  if (missing(variables) || missing(type)) {
    stop("`variables` and `type` must be supplied by the calling script.", call. = FALSE)
  }

  missing_variables <- setdiff(variables, names(data))
  if (length(missing_variables) > 0L) {
    stop(
      "Imputation variables are missing from `data`: ",
      paste(missing_variables, collapse = ", "),
      call. = FALSE
    )
  }

  missing_types <- setdiff(variables, names(type))
  if (length(missing_types) > 0L) {
    stop(
      "Imputation type codes are missing for: ",
      paste(missing_types, collapse = ", "),
      call. = FALSE
    )
  }

  if ("feature_type" %in% names(data)) {
    data$feature_type <- factor(
      data$feature_type,
      levels = c("handcrafted", "deep", "hybrid")
    )
  }

  dat_imp <- data[, variables, drop = FALSE]
  type <- type[names(dat_imp)]

  if (isTRUE(check_collinearity)) {
    check_imputation_collinearity(dat_imp, type)
  }
  imputation_vars <- intersect( variables, names(data) )

  dat_imp <- data[, imputation_vars, drop = FALSE]



  imp <- mitml::jomoImpute(
    data   = dat_imp,
    type   = type,
    m      = m,
    n.iter = n.iter,
    seed = seed,
    n.burn = n.burn,
    random.L1  = "mean"
  )

  attr(imp, "original_data") <- data
  attr(imp, "imputation_data") <- dat_imp
  attr(imp, "imputation_engine") <- "jomo"
  return(imp)
}




#########
# Standard Errors for Studies 55, 189, 459, 486 and 374 can be calculated:
#########

#' Standard errors for Accuracy - study 55
#'
#' @param path Path to the study 55 workbook.
#'
#' @returns A tibble with per-device standard deviations and standard errors for each algorithm.
#'
sd55 <- function(
  path = "data/meta-analysis/data55.xlsx"
) {

  d55 <- readxl::read_xlsx(path)
  d55 %>%
    group_by(Device) %>%
    summarize(kNN = sd(kNN),
              RF = sd(RF),
              MLP = sd(MLP),
              LR = sd(LR),
              NB = sd(NB),
              sekNN = kNN / sqrt(28),
              seRF = RF / sqrt(28),
              seMLP = MLP / sqrt(28),
              seLR = LR / sqrt(28),
              seNB = NB / sqrt(28)
              )

}


#' Standard errors for Accuracy and EER - study 189
#'
#' @param path Path to the study 189 workbook.
#'
#' @returns A list-like summary of means, standard deviations, and standard errors from the workbook sheets.
sd189 <- function(
    path = "data/meta-analysis/data189.xlsx"
) {

  d1891 <- readxl::read_xlsx(path, sheet = 1) %>%
    summarize(
      md1 = mean(as.double(`1day`)),
      md2 = mean(as.double(`2days`)),
      md3 = mean(as.double(`3days`)),
      sd1 = sd(as.double(`1day`)),
      sd2 = sd(as.double(`2days`)),
      sd3 = sd(as.double(`3days`)),
      se1 = sd1 / sqrt(30),
      se2 = sd2 / sqrt(30),
      se3 = sd3 / sqrt(30)
    )
  d1892 <- readxl::read_xlsx(path, sheet = 2) %>%
    summarize(
      m = mean(as.double(EER)),
      sd = sd(as.double(EER)),
      se = sd / sqrt(30)
    )

  c(d1891, d1892)

}


#' Standard errors for Accuracy and EER - study 459
#'
#' @param path Path to the study 459 workbook.
#'
#' @returns A list-like summary of means, standard deviations, and standard errors from the workbook sheets.
sd459 <- function(
    path = "data/meta-analysis/data459.xlsx"
) {

  d4591 <- readxl::read_xlsx(path, sheet = 1) %>%
    summarize(
      md1 = mean(as.double(a)),
      md2 = mean(as.double(b)),
      md3 = mean(as.double(c)),
      md4 = mean(as.double(d)),
      sd1 = sd(as.double(a)),
      sd2 = sd(as.double(b)),
      sd3 = sd(as.double(c)),
      sd4 = sd(as.double(d)),
      se1 = sd1 / sqrt(30),
      se2 = sd2 / sqrt(30),
      se3 = sd3 / sqrt(30),
      se4 = sd4 / sqrt(30)
    )
  d4592 <- readxl::read_xlsx(path, sheet = 2) %>%
    summarize(
      m = mean(as.double(EER)),
      sd = sd(as.double(EER)),
      se = sd / sqrt(30)
    )

  c(d4591, d4592)

}



#' Standard errors for Accuracy - study 486
#'
#' @param path Path to the study 486 CSV.
#'
#' @returns A grouped tibble of means, standard deviations, and standard errors by coefficient component.
sd486 <- function(
    path = "data/meta-analysis/data-486.csv"
) {

  readr::read_csv(path) %>%
    mutate(
      id = rep(1:30, each=2),
      co = rep(c("approximation", "detail"), 30)
    ) %>%
    group_by(co) %>%
    summarize(
      m = mean(as.double(y)),
      sd = sd(as.double(y)),
      se = sd(as.double(y)) / sqrt(30)
    )

}


#' Standard errors for Accuracy - study 374
#'
#' @param path Path to the study 374 workbook.
#'
#' @returns A tibble with the mean, standard deviation, and standard error for accuracy.
sd374 <- function(
    path = "data/meta-analysis/data374.xlsx"
) {

  readxl::read_xlsx(path, sheet = 1) %>%
    summarize(
      m = mean(as.double(ACC)),
      s = sd(as.double(ACC)),
      se = s / sqrt(20)
    )

}


#'  Calculate effective correction for micro SE
#'
#'  @param m number of windows per user
#'  @param rho inflation factor/intraclass correlation used in the design-effect adjustment.
#'
#'  @returns Effective number of independent windows after correlation adjustment.
calc_m_eff <- function(m, rho) {

  m / (1 + (m - 1) * rho)

}
