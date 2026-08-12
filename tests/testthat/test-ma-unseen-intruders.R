library(testthat)
library(dplyr)

source("code/meta-analysis/freeze_ma_dataset.R")
source("code/meta-analysis/ma_imputation.R")

test_that("external validation is recoded to approved intruder levels", {
  expect_equal(
    recode_intruders(c(
      "14 intruders", "20 non-users", "no intruders", "no", "?",
      "not described", "10 patients 20 non-patients?", "10 users 20 non-users?"
    )),
    c(
      "intruders", "intruders", "no intruders", "no intruders",
      NA_character_, NA_character_, NA_character_, NA_character_
    )
  )
})

test_that("analysis data exposes the intruder predictor once", {
  raw <- tibble::tibble(
    id = c("1", "1", "2"),
    experience = c("a", "b", "a"),
    modality = "ECG",
    conditions = "one condition",
    permanence = "low (one day)",
    algorithm_family = "kernel",
    algorithm = "SVM",
    number_of_individuals = "20 users",
    outcome_probability = c(0.8, 0.7, 0.6),
    outcome_logit = qlogis(c(0.8, 0.7, 0.6)),
    se_probability_primary = NA_real_,
    se_probability_sensitivity = NA_real_,
    se_logit_primary = NA_real_,
    se_logit_sensitivity = NA_real_,
    include_se_primary = 0L,
    include_se_sensitivity = 0L,
    ECG = 1L,
    EDA = 0L,
    PPG = 0L,
    SCG = 0L,
    VHF = 0L,
    microphone = 0L,
    multimodal = 0L,
    authentication_time = "10s",
    enroll_time = "20s",
    external_validation = c("14 intruders", "no intruders", "?")
  )

  ma <- derive_analysis_variables(raw)

  expect_s3_class(ma$intruders, "factor")
  expect_equal(levels(ma$intruders), c("no intruders", "intruders"))
  expect_equal(as.character(ma$intruders), c("intruders", "no intruders", NA_character_))
  expect_false("unseen_intruders" %in% names(ma))
})

test_that("effect coding adds intruder within and between terms", {
  dat <- tibble::tibble(
    id = factor(c("1", "1", "2")),
    activity = factor(c("one condition", "multiple conditions", "one condition")),
    duration = ordered(c("low (one day)", "medium (multiple days)", "low (one day)")),
    ECG = c(1, 1, 0),
    PPG = c(0, 0, 1),
    multimodal = c(0, 0, 1),
    intruders = factor(
      c("intruders", "no intruders", "no intruders"),
      levels = c("no intruders", "intruders")
    ),
    algo = factor(c("kernel", "kernel", "dl"), levels = c("Other algorithm", "dl", "dl_temp", "tree", "kernel"))
  )

  coded <- add_effects(list(dat))[[1]]

  expect_equal(coded$intruders, c(TRUE, FALSE, FALSE))
  expect_equal(coded$intruders_between, c(0.5, 0.5, 0))
  expect_equal(coded$intruders_within, c(0.5, -0.5, 0))
})

test_that("meta-analysis algorithm coding reuses table coding and collapses sparse classes", {
  raw <- tibble::tibble(
    id = c("1", "2", "3", "4", "5", "6", "7"),
    experience = "a",
    `Authentication algorithm` = c("CNN", "LSTM", "RF", "SVM", "LR", "kNN", "NB"),
    `Algorithm Details` = ""
  )

  coded <- code_algorithm(raw) %>%
    dplyr::mutate(algorithm_family = collapse_algorithm_class_for_meta(.data$algorithm_class))

  expect_equal(coded$algorithm_class, c("dl", "dl_temp", "tree", "kernel", "lm", "dist", "prob"))
  expect_equal(coded$algorithm_family, c("dl", "dl_temp", "tree", "kernel", "Other algorithm", "Other algorithm", "Other algorithm"))
})
