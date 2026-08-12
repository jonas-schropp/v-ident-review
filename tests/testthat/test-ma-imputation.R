library(testthat)

source("code/meta-analysis/ma_imputation.R")

test_that("imputation collinearity check catches complete dummy sets", {
  dat_imp <- data.frame(
    id = c(1, 1, 2),
    ECG = c(1, 0, 0),
    PPG = c(0, 1, 0),
    Other = c(0, 0, 1),
    ylogit = c(0.1, 0.2, 0.3)
  )
  type <- c(id = -2, ECG = 2, PPG = 2, Other = 2, ylogit = 2)

  expect_error(
    check_imputation_collinearity(dat_imp, type),
    "rank deficient"
  )
})

test_that("imputation collinearity check accepts omitted reference levels", {
  dat_imp <- data.frame(
    id = c(1, 1, 2),
    ECG = c(1, 0, 0),
    PPG = c(0, 1, 0),
    ylogit = c(0.1, 0.2, 0.3)
  )
  type <- c(id = -2, ECG = 2, PPG = 2, ylogit = 2)

  expect_true(check_imputation_collinearity(dat_imp, type))
})

test_that("mice is the configured meta-analysis imputation engine", {
  script <- readLines("code/meta-analysis-script.R", warn = FALSE)
  expect_true(any(grepl('imputation_engine <- "mice"', script, fixed = TRUE)))
  expect_false(any(grepl("auth_time = 1", script, fixed = TRUE)))
  expect_false(any(grepl("enrol_time = 1", script, fixed = TRUE)))
  expect_true(exists("impute_ma"))
  expect_true(exists("impute_ma_mice"))
})

test_that("mice predictor matrix respects MA type codes", {
  dat <- data.frame(
    id = c(1, 1, 2, 2, 3, 3),
    logn = c(1, NA, 2, 3, 4, NA),
    Other = c(0, 0, 1, 0, 0, 1),
    PPG = c(1, 1, 0, 1, 1, 0),
    ECG = c(0, 0, 0, 0, 0, 0),
    activity = factor(c("one condition", NA, "multiple conditions", "everyday activities", NA, "one condition")),
    duration = ordered(c("short", NA, "medium", "long", "short", NA), levels = c("short", "medium", "long")),
    intruders = factor(c("seen", NA, "intruders", "seen", NA, "intruders")),
    ylogit = seq(0.1, 0.6, by = 0.1),
    se = rep(0.1, 6)
  )
  variables <- c("id", "logn", "Other", "PPG", "ECG", "activity", "duration", "intruders", "ylogit")
  type <- c(id = -2, logn = 1, Other = 0, PPG = 2, ECG = 2, activity = 1, duration = 1, intruders = 1, ylogit = 2)

  spec <- suppressWarnings(prepare_mice_ma_spec(dat, variables, type, check_collinearity = FALSE))
  pm <- spec$predictorMatrix

  expect_equal(spec$method[c("logn", "duration")], c(logn = "2l.pmm", duration = "2l.pmm"))
  expect_equal(spec$method[["intruders"]], "2l.bin")
  expect_equal(spec$method[["activity"]], "polyreg")
  expect_equal(pm["logn", "id"], -2)
  expect_equal(pm["duration", "id"], -2)
  expect_equal(pm["activity", "id"], 0)
  expect_equal(pm["activity", ".paper_fixed_effect"], 1)
  expect_true(all(pm[, "Other"] == 0))
  expect_true("Other" %in% names(dat))

  type_with_random_slope <- type
  type_with_random_slope["PPG"] <- 3
  spec_with_random_slope <- suppressWarnings(
    prepare_mice_ma_spec(dat, variables, type_with_random_slope, check_collinearity = FALSE)
  )
  expect_equal(spec_with_random_slope$method[["logn"]], "2l.pmm")
  expect_equal(spec_with_random_slope$predictorMatrix["logn", "PPG"], 2)
  expect_equal(spec_with_random_slope$predictorMatrix["duration", "PPG"], 2)
})


test_that("mice factor labels are sanitized internally and restored after imputation", {
  dat <- data.frame(
    id = c(1, 1, 2, 2, 3, 3),
    logn = c(1, NA, 2, 3, 4, NA),
    activity = factor(c("one condition", NA, "multiple conditions", "everyday activities", NA, "one condition")),
    algo = factor(c("Other", "DLTemp", "tree Ensemble", "SVM", "CNN model", "ANN")),
    ylogit = seq(0.1, 0.6, by = 0.1)
  )
  variables <- c("id", "logn", "activity", "algo", "ylogit")
  type <- c(id = -2, logn = 1, activity = 1, algo = 2, ylogit = 2)

  spec <- suppressWarnings(prepare_mice_ma_spec(dat, variables, type, check_collinearity = FALSE))

  expect_equal(levels(spec$data$activity), make.names(levels(dat$activity), unique = TRUE))
  expect_equal(levels(spec$data$algo), make.names(levels(dat$algo), unique = TRUE))
  expect_true(all(!grepl(" ", levels(spec$data$activity))))
  expect_true(all(!grepl(" ", levels(spec$data$algo))))

  restored <- restore_mice_factor_levels(spec$data, spec$factor_level_mappings)
  expect_identical(levels(restored$activity), levels(dat$activity))
  expect_identical(levels(restored$algo), levels(dat$algo))
})

test_that("mice spec converts factor cluster ids before using two-level predictors", {
  dat <- data.frame(
    id = factor(c("paper-a", "paper-a", "paper-b", "paper-b", "paper-c", "paper-c")),
    logn = c(1, NA, 2, 3, 4, NA),
    PPG = c(1, 1, 0, 1, 1, 0),
    activity = factor(c("one condition", NA, "multiple conditions", "everyday activities", NA, "one condition")),
    ylogit = seq(0.1, 0.6, by = 0.1)
  )
  variables <- c("id", "logn", "PPG", "activity", "ylogit")
  type <- c(id = -2, logn = 1, PPG = 2, activity = 1, ylogit = 2)

  spec <- suppressWarnings(prepare_mice_ma_spec(dat, variables, type, check_collinearity = FALSE))

  expect_type(spec$data$id, "integer")
  expect_equal(spec$data$id, c(1L, 1L, 2L, 2L, 3L, 3L))
  expect_equal(spec$original_cluster, dat$id)
  expect_equal(spec$predictorMatrix["logn", "id"], -2)
})

test_that("mice imputations preserve observed values, categories, row order, and reproducibility", {
  skip_if_not_installed("mice")
  skip_if_not_installed("miceadds")

  dat <- data.frame(
    id = c(1, 1, 2, 2, 3, 3, 4, 4),
    logn = c(1, NA, 2, 3, 4, NA, 5, 6),
    Other = c(0, 0, 1, 0, 0, 1, 0, 0),
    PPG = c(1, 1, 0, 1, 1, 0, 1, 1),
    ECG = c(0, 0, 0, 0, 0, 0, 0, 0),
    activity = factor(c("one condition", NA, "multiple conditions", "everyday activities", NA, "one condition", "multiple conditions", "everyday activities")),
    duration = ordered(c("short", NA, "medium", "long", "short", NA, "medium", "long"), levels = c("short", "medium", "long")),
    intruders = factor(c("seen", NA, "intruders", "seen", NA, "intruders", "seen", "intruders")),
    ylogit = seq(0.1, 0.8, by = 0.1),
    se = rep(0.1, 8)
  )
  variables <- c("id", "logn", "Other", "PPG", "ECG", "activity", "duration", "intruders", "ylogit")
  type <- c(id = -2, logn = 1, Other = 0, PPG = 2, ECG = 2, activity = 1, duration = 1, intruders = 1, ylogit = 2)

  imp1 <- suppressWarnings(impute_ma_mice(dat, 2, variables, type, maxit = 2, seed = 42, check_collinearity = FALSE))
  imp2 <- suppressWarnings(impute_ma_mice(dat, 2, variables, type, maxit = 2, seed = 42, check_collinearity = FALSE))
  comp1 <- completed_ma_data(imp1)
  comp2 <- completed_ma_data(imp2)

  expect_s3_class(imp1, "mids")
  expect_identical(imp1, imp2)
  expect_length(comp1, 2)
  expect_identical(comp1, comp2)
  expect_false(".paper_fixed_effect" %in% names(comp1[[1]]))
  expect_true(all(vapply(comp1, function(d) all(!is.na(d[variables[type[variables] == 1]])), logical(1))))
  expect_identical(rownames(comp1[[1]]), rownames(dat))
  expect_equal(comp1[[1]]$logn[!is.na(dat$logn)], dat$logn[!is.na(dat$logn)])
  expect_identical(levels(comp1[[1]]$activity), levels(dat$activity))
  expect_true(is.ordered(comp1[[1]]$duration))
  expect_identical(levels(comp1[[1]]$duration), levels(dat$duration))
  expect_true(all(comp1[[1]]$activity %in% levels(dat$activity)))
  expect_true(all(comp1[[1]]$duration %in% levels(dat$duration)))
  expect_equal(attr(imp1, "predictorMatrix")["logn", "id"], -2)
})

test_that("effect coding is recalculated inside each completed data set", {
  dat <- data.frame(
    id = c(1, 1, 2, 2),
    activity = factor(c("one condition", "multiple conditions", "everyday activities", "one condition")),
    duration = ordered(c("short", "medium (multiple days)", "high (> one week)", "short")),
    ECG = c(1, 0, 0, 1),
    PPG = c(0, 1, 0, 0),
    multimodal = c(0, 1, 1, 0),
    intruders = factor(c("seen", "intruders", "seen", "intruders")),
    algo = factor(c("DLTemp", "SVM", "CNN", "ANN"))
  )
  out <- add_effects(list(dat, dat))
  expect_equal(out[[1]]$multiC_between, ave(out[[1]]$multiC, out[[1]]$id))
  expect_equal(out[[2]]$ECG_within, out[[2]]$ECG - ave(out[[2]]$ECG, out[[2]]$id))
})


test_that("saved compact mice imputations are read as completed data with effects", {
  skip_if_not_installed("mice")
  skip_if_not_installed("miceadds")

  dat <- data.frame(
    id = c(1, 1, 2, 2, 3, 3, 4, 4),
    logn = c(1, NA, 2, 3, 4, NA, 5, 6),
    PPG = c(1, 1, 0, 1, 1, 0, 1, 1),
    ECG = c(0, 0, 0, 0, 0, 0, 0, 0),
    multimodal = c(0, 0, 1, 0, 0, 1, 0, 0),
    activity = factor(c("one condition", NA, "multiple conditions", "everyday activities", NA, "one condition", "multiple conditions", "everyday activities")),
    duration = ordered(c("short", NA, "medium (multiple days)", "high (> one week)", "short", NA, "medium (multiple days)", "high (> one week)")),
    intruders = factor(c("seen", NA, "intruders", "seen", NA, "intruders", "seen", "intruders")),
    algo = factor(c("Other", "DLTemp", "treeEnsemble", "SVM", "CNN", "ANN", "Other", "SVM")),
    ylogit = seq(0.1, 0.8, by = 0.1),
    se = rep(0.1, 8)
  )
  variables <- c("id", "logn", "PPG", "ECG", "multimodal", "activity", "duration", "intruders", "algo", "ylogit")
  type <- c(id = -2, logn = 1, PPG = 2, ECG = 2, multimodal = 2, activity = 1, duration = 1, intruders = 1, algo = 2, ylogit = 2)

  imp <- suppressWarnings(impute_ma_mice(dat, 2, variables, type, maxit = 1, seed = 99, check_collinearity = FALSE))
  path <- tempfile(fileext = ".rds")
  saveRDS(imp, path)

  expect_s3_class(readRDS(path), "mids")
  completed <- read_ma_imputed_data(path)
  expect_length(completed, 2)
  expect_true("multiC_between" %in% names(completed[[1]]))
  expect_false(".paper_fixed_effect" %in% names(completed[[1]]))
})
