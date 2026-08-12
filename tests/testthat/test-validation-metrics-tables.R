library(testthat)

source("code/tables/tables_validation_metrics.R")

write_table_csv_docx <- function(x, csv_path) {
  readr::write_csv(x, csv_path)
  invisible(x)
}

test_that("validation strategy table keeps ambiguous descriptions unclear", {
  eval <- tibble::tibble(
    id = c("189", "205", "495", "500"),
    experiment = paste("experiment", seq_len(4)),
    modality = "ECG",
    internal = c(
      "60/40, not clear whether random or across time",
      "train/test, unclear how split, only one valid user",
      "10-fold CV, unclear whether within participants or not",
      "random split"
    ),
    external = "no intruders",
    EER = NA_character_,
    accuracy = NA_character_,
    F1 = NA_character_,
    AUC = NA_character_,
    `memory performance` = NA_character_,
    `authentication time` = NA_character_,
    `Enroll time` = NA_character_,
    `further metrics` = NA_character_
  )

  eval_path <- tempfile(fileext = ".rds")
  output_dir <- tempfile()
  dir.create(output_dir)
  readr::write_rds(eval, eval_path)

  tables <- tables_validation_metrics(eval_path, output_dir)

  unclear_ids <- tables$validation_strategies |>
    dplyr::filter(.data$validation_category == "Unclear or not reported") |>
    dplyr::pull(.data$study_ids)

  expect_true(all(c("189", "205", "495") %in% strsplit(unclear_ids, "; ", fixed = TRUE)[[1]]))
  expect_true(any(tables$validation_strategies$validation_category == "Within-subject / within-session split"))
})

test_that("validation supplement preserves raw internal and external fields", {
  eval <- tibble::tibble(
    id = c("1", "2", "3"),
    experiment = paste("experiment", seq_len(3)),
    modality = "ECG",
    internal = c(NA_character_, "not described", "not described"),
    external = c(NA_character_, "?", "not described"),
    EER = NA_character_,
    accuracy = NA_character_,
    F1 = NA_character_,
    AUC = NA_character_,
    `memory performance` = NA_character_,
    `authentication time` = NA_character_,
    `Enroll time` = NA_character_,
    `further metrics` = NA_character_
  )

  eval_path <- tempfile(fileext = ".rds")
  output_dir <- tempfile()
  dir.create(output_dir)
  readr::write_rds(eval, eval_path)

  tables <- tables_validation_metrics(eval_path, output_dir)

  expect_equal(nrow(tables$validation_strategies_supplement), 3)
  expect_true(any(is.na(tables$validation_strategies_supplement$internal)))
  expect_true(any(tables$validation_strategies_supplement$external == "?"))
  expect_true(any(tables$validation_strategies_supplement$external == "not described"))
})
