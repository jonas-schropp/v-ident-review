library(testthat)
library(dplyr)

source("code/data-cleaning/code-auth.R")
source("code/data-cleaning/code-features.R")
source("code/data-cleaning/code-eval.R")
source("code/data-cleaning/code-preproc.R")

expect_no_new_na_in_existing_cols <- function(before, after) {
  common_cols <- intersect(names(before), names(after))

  for (nm in common_cols) {
    before_na <- is.na(before[[nm]])
    after_na <- is.na(after[[nm]])

    introduced <- sum(!before_na & after_na)
    expect_equal(
      introduced,
      0,
      info = paste("Column introduced new NA values:", nm)
    )
  }
}

test_that("auth coding functions preserve existing fields and produce valid categories", {
  auth <- tibble::tibble(
    `Authentication algorithm` = c("distance based", "NA", "CNN"),
    `Algorithm Details` = c("template matching with DTW", "none", "conv net"),
    Multimodal = c("yes", "no", "unknown"),
    Fusion = c("feature level", "late", ""),
    continuous = c("yes", "no", "?"),
    permanence = c("1 day between tests", "yes, 8 weeks", "other"),
    `situational stability` = c("all during driving task", "yes, everyday activities", "other")
  )

  coded <- auth %>%
    code_algorithm() %>%
    code_multimodal() %>%
    code_fusion() %>%
    code_continuous() %>%
    code_permanence() %>%
    code_situational_stability()

  expect_no_new_na_in_existing_cols(auth, coded)

  expect_setequal(unique(stats::na.omit(coded$algorithm_class)), c("dist", "dl"))
  expect_setequal(unique(stats::na.omit(coded$multimodal)), c("Yes", "No"))
  expect_setequal(unique(stats::na.omit(coded$fusion)), c("early fusion"))
  expect_setequal(unique(stats::na.omit(coded$continuous_coded)), c("Yes", "No"))
  expect_setequal(unique(stats::na.omit(coded$permanence_coded)), c("low (one day)", "high (> one week)"))
  expect_setequal(unique(stats::na.omit(coded$situational_stability_coded)), c("one condition", "everyday activities"))
})

test_that("feature coding functions return expected classes and avoid invalid labels", {
  features <- tibble::tibble(
    Features = c("fiducial and qrs features", "temporal convolutional network", "lstm + pqrst"),
    `Feature selection` = c("none", "automatic feature extraction via DL", "pca"),
    `Feature number selected` = c("3+5", "150", "NA"),
    `Feature number considered` = c("7+16", "200", "21 + 40")
  )

  coded <- features %>%
    code_feature_type() %>%
    code_feature_dim() %>%
    code_dim_reduction()

  expect_no_new_na_in_existing_cols(features, coded)

  expect_true(all(stats::na.omit(as.character(coded$feature_type)) %in% c("handcrafted", "deep", "hybrid")))
  expect_true(all(stats::na.omit(as.character(coded$feature_dim)) %in% c("low", "medium", "high")))
  expect_true(all(coded$dim_reduction %in% c("yes", "no")))
})

test_that("preprocessing coding functions do not create new missingness in original columns", {
  preproc <- tibble::tibble(
    `noise reduction` = c("none", "bandpass filter"),
    segmentation = c("window with overlap", "beat detection by R-peak"),
    normalization = c("no", "z-score")
  )

  coded <- preproc %>%
    code_filtering() %>%
    code_segmentation_type() %>%
    code_normalization()

  expect_no_new_na_in_existing_cols(preproc, coded)
  expect_true(all(coded$filteringyn %in% c("yes", "no")))
  expect_true(all(coded$segmentation_type %in% c("none", "window", "beat")))
  expect_true(all(coded$normalizationyn %in% c("yes", "no")))
})

test_that("evaluation coding functions handle missing/unknown values predictably", {
  eval <- tibble::tibble(
    internal = c("not described", "training day 1, testing day 2", "LO-intruder-O- CV"),
    external = c("?", "14 intruders", "no"),
    `memory performance` = c("", "not reported", "EER only")
  )

  coded <- eval %>%
    code_internal_validation() %>%
    code_external_validation() %>%
    code_memory_reported()

  expect_no_new_na_in_existing_cols(eval, coded)

  expect_true(all(coded$internal_validation_coded %in% c(
    "unclear / not reported",
    "within-user resampling",
    "temporal/session holdout",
    "impostor-aware validation"
  )))
  expect_true(all(coded$external_validation_coded %in% c("Yes", "No")))
  expect_true(all(coded$memory_reported %in% c("reported", "not reported")))
})
