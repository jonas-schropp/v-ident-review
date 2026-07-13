library(testthat)
library(readr)

parse_percent_value <- function(x) {
  x <- trimws(as.character(x))
  missing_values <- c("", "NA", "N/A", "na", "n/a", "not reported", "NR", "nr", "None", "none")
  has_number <- grepl("[-+]?\\d+(?:\\.\\d+)?", x)
  ifelse(
    is.na(x) | x %in% missing_values | !has_number,
    NA_real_,
    as.numeric(sub(".*?([-+]?\\d+(?:\\.\\d+)?).*", "\\1", gsub(",", "", x)))
  )
}

test_that("reported accuracy and EER percentages are jointly plausible", {
  eval <- read_rds(test_path("..", "..", "data", "eval.rds"))

  accuracy <- parse_percent_value(eval$accuracy)
  eer <- parse_percent_value(eval$EER)
  both_reported <- !is.na(accuracy) & !is.na(eer)

  expect_true(any(both_reported), info = "This check must cover studies reporting both accuracy and EER.")
  expect_true(
    all(accuracy[both_reported] >= 0 & accuracy[both_reported] <= 100),
    info = "Accuracy values in the RDS extraction should already be percentages between 0 and 100."
  )
  expect_true(
    all(eer[both_reported] >= 0 & eer[both_reported] <= 100),
    info = "EER values in the RDS extraction should already be percentages between 0 and 100."
  )
  expect_lte(
    max(accuracy[both_reported] + eer[both_reported], na.rm = TRUE),
    110,
    info = "Accuracy and EER need not be exact complements, but high accuracy cannot coexist with high EER in the same experiment."
  )
})
