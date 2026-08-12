library(testthat)

source("code/meta-analysis/fit-ma.R")

test_that("fit_ma requests separate brms fits and records lightweight metadata", {
  body_text <- paste(deparse(body(fit_ma)), collapse = "\n")

  expect_match(body_text, "combine\\s*=\\s*FALSE")
  expect_match(body_text, "brm_multiple")
  expect_match(body_text, "ma_brmsfit_list")
  expect_match(body_text, 'attr\\(fits, "nimp"\\)')
  expect_match(body_text, 'attr\\(fits, "call"\\)')
})

test_that("diagnose_ma_fits exposes required machine-readable columns", {
  formals_expected <- c(
    "fits", "rhat_threshold", "ess_per_chain_threshold", "bfmi_threshold", "detailed"
  )
  expect_named(formals(diagnose_ma_fits), formals_expected)

  body_text <- paste(deparse(body(diagnose_ma_fits)), collapse = "\n")
  required_columns <- c(
    "imputation", "n_chains", "post_warmup_draws", "n_parameters",
    "max_rhat", "worst_rhat_parameter", "n_rhat_over_threshold",
    "n_rhat_missing", "min_bulk_ess", "lowest_bulk_ess_parameter",
    "n_bulk_ess_below_threshold", "min_tail_ess",
    "lowest_tail_ess_parameter", "n_tail_ess_below_threshold",
    "divergences", "max_treedepth_hits", "min_ebfmi",
    "n_chains_low_ebfmi", "converged"
  )
  for (column in required_columns) {
    expect_match(body_text, column, fixed = TRUE)
  }
  expect_match(body_text, "posterior::as_draws_array", fixed = TRUE)
  expect_match(body_text, "posterior::summarise_draws", fixed = TRUE)
  expect_match(body_text, "posterior::default_convergence_measures", fixed = TRUE)
})

test_that("HMC diagnostics count divergences, treedepth hits and E-BFMI by chain", {
  body_text <- paste(deparse(body(summarise_ma_hmc)), collapse = "\n")
  expect_match(body_text, "brms::nuts_params", fixed = TRUE)
  expect_match(body_text, "brms::control_params", fixed = TRUE)
  expect_match(body_text, "divergent__", fixed = TRUE)
  expect_match(body_text, "treedepth__", fixed = TRUE)
  expect_match(body_text, "energy__", fixed = TRUE)
  expect_match(body_text, "mean(diff(energy)^2) / stats::var(energy)", fixed = TRUE)
})

test_that("diagnostics handle undefined R-hat and one-chain fits conservatively", {
  body_text <- paste(deparse(body(diagnose_ma_fits)), collapse = "\n")
  expect_match(body_text, "finite_or_na", fixed = TRUE)
  expect_match(body_text, "n_rhat_missing", fixed = TRUE)
  expect_match(body_text, "n_chains >= 2L", fixed = TRUE)
})

test_that("convergence check excludes structurally constant parameters", {
  # Cholesky factors and any parameter fixed by construction (e.g. nu held at a
  # constant in a sensitivity analysis) have undefined R-hat/ESS, so counting them
  # as failures would spuriously fail otherwise-healthy models.
  body_text <- paste(deparse(body(selected_ma_draw_variables)), collapse = "\n")
  expect_match(body_text, "L_[0-9]+|Lrescor")
  expect_match(body_text, "drop_constant", fixed = TRUE)
  expect_match(body_text, "constant_variables", fixed = TRUE)
  expect_true("drop_constant" %in% names(formals(selected_ma_draw_variables)))

  # The exclusion must stay visible in the reported diagnostics.
  expect_match(paste(deparse(body(diagnose_ma_fits)), collapse = "\n"),
               "n_constant_parameters", fixed = TRUE)

  # Names compared across imputations must be attribute-free so identical() works.
  expect_match(paste(deparse(body(ma_fit_variable_names)), collapse = "\n"),
               "attributes(v) <- NULL", fixed = TRUE)
})

test_that("combine_ma_fits validates compatibility and equal imputation weights", {
  expect_named(formals(combine_ma_fits), c("fits", "check_convergence", "require_convergence"))
  body_text <- paste(deparse(body(combine_ma_fits)), collapse = "\n")

  expect_match(body_text, "compatible formulas", fixed = TRUE)
  expect_match(body_text, "compatible families", fixed = TRUE)
  expect_match(body_text, "compatible parameter sets", fixed = TRUE)
  expect_match(body_text, "same number of chains", fixed = TRUE)
  expect_match(body_text, "equal post-warmup draw counts", fixed = TRUE)
  expect_match(body_text, "brms::combine_models", fixed = TRUE)
  expect_match(body_text, "check_data = FALSE", fixed = TRUE)
  expect_match(body_text, "brmsfit_multiple", fixed = TRUE)
})

test_that("fit_ma precompiles once and caches fitted models to disk", {
  cache_formals <- c("seed", "file", "model_file", "refit", "reuse_compiled", "overwrite")
  expect_true(all(cache_formals %in% names(formals(fit_ma))))
  expect_true(isTRUE(formals(fit_ma)$overwrite))

  body_text <- paste(deparse(body(fit_ma)), collapse = "\n")
  # Reload a cached fit list instead of refitting.
  expect_match(body_text, "readRDS(file)", fixed = TRUE)
  # Precompile the Stan program once, without sampling.
  expect_match(body_text, "chains = 0", fixed = TRUE)
  # Reuse the single compiled model across imputations (no per-imputation recompile).
  expect_match(body_text, "recompile = FALSE", fixed = TRUE)
  # Persist the fitted list for later runs, versioning when overwrite = FALSE.
  expect_match(body_text, "saveRDS(fits, target)", fixed = TRUE)
  expect_match(body_text, "ma_versioned_path(file)", fixed = TRUE)
})

test_that("ma_versioned_path returns non-colliding siblings", {
  d <- withr::local_tempdir()
  base <- file.path(d, "null.rds")
  writeLines("x", base)
  first <- ma_versioned_path(base)
  expect_identical(basename(first), "null-1.rds")

  writeLines("x", first)
  second <- ma_versioned_path(base)
  expect_identical(basename(second), "null-2.rds")

  # Works for extension-less paths too.
  noext <- file.path(d, "model")
  writeLines("x", noext)
  expect_identical(basename(ma_versioned_path(noext)), "model-1")
})

test_that("meta-analysis workflow diagnoses before combining and caches fits to ma-models", {
  # The fit -> diagnose -> combine ordering lives in fit_diagnose_combine_ma().
  fit_ma_src <- paste(readLines("code/meta-analysis/fit-ma.R", warn = FALSE), collapse = "\n")
  expect_match(fit_ma_src, "component_fits <- fit_ma", fixed = TRUE)
  expect_match(fit_ma_src, "diagnostics <- diagnose_ma_fits(component_fits)", fixed = TRUE)
  expect_match(fit_ma_src, "combined_fit <- combine_ma_fits", fixed = TRUE)

  # The script caches every model under ma-models/ and reloads from there downstream.
  script <- paste(readLines("code/meta-analysis-script.R", warn = FALSE), collapse = "\n")
  expect_match(script, "save_ma_summary_artifacts", fixed = TRUE)
  expect_match(script, 'ma_models_dir <- here::here("ma-models")', fixed = TRUE)
  expect_match(script, "file = ma_model_path(\"null\")", fixed = TRUE)
  expect_match(script, "cached on local disk under ma-models", fixed = TRUE)
})
