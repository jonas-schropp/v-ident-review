# Authentication model and protocol coding

#' Code algorithm family variables for authentication studies
#'
#' Creates a normalized `algorithm` variable and a higher-level
#' `algorithm_class` variable from `Authentication algorithm` and
#' `Algorithm Details` in `auth.csv`.
#'
#' @param auth Data frame containing authentication extraction data.
#'
#' @return Input data frame with `algorithm` and `algorithm_class` added.
code_algorithm <- function(auth) {

  auth |>
    dplyr::mutate(
      algorithm = dplyr::case_when(
        `Authentication algorithm` %in% c("FF-NN", "MLP", "CNN", "RNN", "LSTM", "Autoencoder", "GBM", "RF", "DT", "SVM", "LR", "LDA", "kNN", "NB") ~ `Authentication algorithm`,
        `Authentication algorithm` == "distance based" ~ "unspecified distance metric",
        `Authentication algorithm` %in% c("TCM + LSTM + template matching", "CNN + LSTM", "CRNN") ~ "TCM/CNN+LSTM/CRNN",
        `Authentication algorithm` %in% c("NA", "") & grepl("template matching", `Algorithm Details`, ignore.case = TRUE) ~ "unspecified distance metric",
        `Authentication algorithm` %in% c("NA", "") ~ NA_character_,
        .default = `Authentication algorithm`
      ),
      algorithm_class = dplyr::case_when(
        algorithm %in% c("FF-NN", "MLP", "CNN") ~ "dl",
        algorithm %in% c("RNN", "LSTM", "TCM/CNN+LSTM/CRNN", "Autoencoder") ~ "dl_temp",
        algorithm %in% c("GBM", "RF", "DT") ~ "tree",
        algorithm == "SVM" ~ "kernel",
        algorithm %in% c("LR", "LDA") ~ "lm",
        algorithm %in% c("kNN", "unspecified distance metric") ~ "dist",
        algorithm == "NB" ~ "prob"
      )
    )

}

#' Code multimodal use as Yes/No
#'
#' @param auth Data frame containing authentication extraction data.
#'
#' @return Input data frame with normalized `multimodal` variable.
code_multimodal <- function(auth) {

  auth %>%
    mutate(
      multimodal = case_when(
        tolower(Multimodal) == "yes" ~ "Yes",
        tolower(Multimodal) == "no" ~ "No",
        TRUE ~ NA_character_
      )
    )

}

#' Code fusion strategy
#'
#' Harmonizes fusion coding to `early fusion` when feature-level fusion is
#' reported. Leaves other values missing by design.
#'
#' @param auth Data frame containing authentication extraction data.
#'
#' @return Input data frame with normalized `fusion` variable.
code_fusion <- function(auth) {

  auth %>%
    mutate(
      fusion = case_when(
        tolower(Fusion) %in% c("early fusion (at feature level)", "feature level") ~ "early fusion",
        TRUE ~ NA_character_
      )
    )

}

#' Code whether authentication is continuous
#'
#' @param auth Data frame containing authentication extraction data.
#'
#' @return Input data frame with normalized `continuous_coded` variable.
code_continuous <- function(auth) {

  auth %>%
    mutate(
      continuous_coded = case_when(
        tolower(continuous) == "yes" ~ "Yes",
        tolower(continuous) == "no" ~ "No",
        TRUE ~ NA_character_
      )
    )

}

#' Code permanence level
#'
#' Uses study-specific pattern matching to classify permanence as
#' `low (one day)`, `medium (multiple days)`, or `high (> one week)`.
#'
#' @param auth Data frame containing authentication extraction data.
#'
#' @return Input data frame with normalized `permanence_coded` variable.
code_permanence <- function(auth) {

  auth %>%
    mutate(
      permanence_coded = case_when(
        permanence %in% c("1 day between tests", "2h", "3h", "3h, involves adaptive retraining", "some (6 hour long session)", "no") ~ "low (one day)",
        permanence %in% c("2 consecutive days tested", "3 consecutive days tested", "yes, over several days", "unclear, several days") ~ "medium (multiple days)",
        permanence %in% c("yes, 30 days", "yes, 8 weeks") ~ "high (> one week)",
        TRUE ~ NA_character_
      )
    )

}

#' Code situational stability level
#'
#' Uses study-specific text coding to classify contextual variation as
#' `one condition`, `multiple conditions`, or `everyday activities`.
#'
#' @param auth Data frame containing authentication extraction data.
#'
#' @return Input data frame with normalized `situational_stability_coded`.
code_situational_stability <- function(auth) {

  auth %>%
    mutate(
      situational_stability_coded = case_when(
        `situational stability` %in% c(
          "all during driving task", "small (idle+touchpad)", "small (idle+typing)",
          "small (working at desk + resting)", "no different activity levels", "yes"
        ) ~ "one condition",
        `situational stability` %in% c(
          "Different activity levels (TROIKA DB)", "different activity levels",
          "different situations simulated for respiration",
          "high - several settings (resting, standing, walking, walking uphill)",
          "moderate (several situations including stress)",
          "some (arm movement and sitting)", "some (rest and exercise)",
          "unclear, several situations not further described",
          "walking + no controlled environment imposed"
        ) ~ "multiple conditions",
        `situational stability` %in% c(
          "high, data from everyday life ( days)", "yes, everyday activities",
          "no controlled environment imposed (real-life conditions)",
          "no controlled environment imposed (real-life conditions) or different activity levels, not sufficiently described"
        ) ~ "everyday activities",
        TRUE ~ NA_character_
      )
    )

}
