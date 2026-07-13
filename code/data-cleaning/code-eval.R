# Evaluation coding

#' Code internal validation category
#'
#' Creates `internal_validation_coded` with categories:
#' unclear / not reported, within-user resampling,
#' temporal/session holdout, impostor-aware validation.
#'
#' @param eval Data frame containing evaluation extraction data.
#'
#' @return Input data frame with `internal_validation_coded` added.
code_internal_validation <- function(eval) {

  eval %>%
    mutate(
      internal_validation_coded = case_when(
        internal %in% c(
          "not described", "NA", "train/test not specified",
          "train/test, unclear how split, only one valid user",
          "60/40, not clear whether random or across time",
          "day 1 60/40, not clear whether random or across time, days 2+3 testing",
          "10-fold CV, unclear whether within participants or not",
          "Train T1 + T2, Test T2 - maybe some sort of 10-fold CV?"
        ) ~ "unclear / not reported",
        internal %in% c(
          "5-Fold CV, 3 intruders held out in each fold, 50/50 training/testing within fold",
          "5-Fold CV, 3 intruders held out in each fold, 60/40 training/testing within fold",
          "5-Fold CV, 3 intruders held out in each fold, 70/30 training/testing within fold",
          "nested: 10-fold CV within users + 10/90 split",
          "nested: 10-fold CV within users + 30/70 split",
          "3-fold-CV",
          "70/30 random split",
          "20 x Monte Carlo CV 50/50 across time",
          "LOO-CV",
          "8-fold CV over sessions within users",
          "6-fold CV within participants (5/6 breathing events)",
          "Train T1, Test T2",
          "80/20, 1 valid user",
          "80/20, 2 valid users",
          "80/20, 3 valid users",
          "80/20, 4 valid users",
          "80/20, 5 valid users",
          "80/20 split within 11 users",
          "6-fold CV within conditions and participants",
          "LOO-CV at inhalation event level (6 fold CV)",
          "5-fold CV within users"
        ) ~ "within-user resampling",
        internal %in% c(
          "time-based splitting",
          "training day 1, testing day 2",
          "training day 1, testing day 3",
          "split between tasks",
          "split between sessions",
          "4 days training, day 5 testing"
        ) ~ "temporal/session holdout",
        internal %in% c(
          "LO-intruder-O- CV",
          "75% selected for training, rest for testing against intruders"
        ) ~ "impostor-aware validation",
        TRUE ~ "unclear / not reported"
      )
    )

}

#' Code whether external validation is reported
#'
#' Creates `external_validation_coded` as Yes/No based on whether
#' unseen impostors/non-users are included.
#'
#' @param eval Data frame containing evaluation extraction data.
#'
#' @return Input data frame with `external_validation_coded` added.
code_external_validation <- function(eval) {

  eval %>%
    mutate(
      external_validation_coded = case_when(
        external %in% c(
          "14 intruders", "3 intruders", "38 intruders", "6 intruders",
          "12 intruders", "2 intruders", "10 patients 20 non-patients?",
          "10 users 20 non-users?"
        ) ~ "Yes",
        external %in% c(
          "no intruders", "no", "no unseen imposters", "NA", "not described", "?"
        ) ~ "No",
        TRUE ~ "No"
      )
    )

}

#' Code whether memory performance is reported
#'
#' Creates `memory_reported` as reported/not reported based on
#' `memory performance`.
#'
#' @param eval Data frame containing evaluation extraction data.
#'
#' @return Input data frame with `memory_reported` added.
code_memory_reported <- function(eval) {

  eval %>%
    mutate(
      memory_reported = case_when(
        `memory performance` %in% c("NA", "not reported", "") ~ "not reported",
        TRUE ~ "reported"
      )
    )

}
