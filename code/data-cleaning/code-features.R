# Feature coding

#' Code feature type
#'
#' Creates a mutually-exclusive `feature_type` factor with categories
#' handcrafted, deep, and hybrid.
#'
#' @param features data frame with feature extraction data
#'
#' @return `features` with `feature_type`.
code_feature_type <- function(features) {

  features %>%
    mutate(
      .features_lower = tolower(Features),
      .selection_lower = tolower(`Feature selection`),
      .deep_feature =
        grepl("automatic feature extraction via dl", .features_lower) |
        grepl("implicit via dl", .features_lower) |
        grepl("waveform of single ecg signals in sequence", .features_lower) |
        grepl("temporal convolutional network", .features_lower) |
        grepl("lstm", .features_lower) |
        grepl("cnn", .features_lower) |
        grepl("implicitly extracted", .features_lower) |
        grepl("automatic \\(lstm\\)|automatic \\(ann\\)|implicit via training",
              .selection_lower),
      .handcrafted_feature =
        grepl("fiducial|pqrst|qrs|rr|statistical|wavelet|entropy|hjorth",
              .features_lower) |
        grepl("autocorrelation|dct|mfcc|gfcc|bfcc|lpc|rplp|spectral", .features_lower) |
        grepl("instantaneous frequency|mel-spectrogram|scalogram|spectrogram",
              .features_lower),
      feature_type = factor(
        case_when(
          .deep_feature & .handcrafted_feature ~ "hybrid",
          .deep_feature ~ "deep",
          TRUE ~ "handcrafted"
        ),
        levels = c("handcrafted", "deep", "hybrid")
      )
    ) %>%
    select(-.features_lower, -.selection_lower, -.deep_feature, -.handcrafted_feature)

}

#' Code feature dimensionality
#'
#' Creates ordinal `feature_dim` from the number of features used:
#' low (<20), medium (20-100), high (>100).
#'
#' @param features data frame with feature extraction data
#'
#' @return `features` with `feature_dim`.
code_feature_dim <- function(features) {

  features %>%
    mutate(
      .n_selected = case_when(
        `Feature number selected` %in% c("NA", "not specified", "?", "time series") ~ NA_real_,
        `Feature number selected` == "6 (GSR) and 6 (ST)" ~ 12,
        `Feature number selected` == "3+5" ~ 8,
        TRUE ~ suppressWarnings(as.numeric(`Feature number selected`))
      ),
      .n_considered = case_when(
        `Feature number considered` %in% c("NA", "not specified", "?", "complete time series") ~ NA_real_,
        `Feature number considered` == "12 (GSR) and 6 (ST)" ~ 18,
        `Feature number considered` == "7+16" ~ 23,
        `Feature number considered` == "21 + 40" ~ 61,
        TRUE ~ suppressWarnings(as.numeric(`Feature number considered`))
      ),
      .n_features = if_else(!is.na(.n_selected), .n_selected, .n_considered),
      feature_dim = ordered(
        case_when(
          is.na(.n_features) ~ NA_character_,
          .n_features < 20 ~ "low",
          .n_features <= 100 ~ "medium",
          .n_features > 100 ~ "high"
        ),
        levels = c("low", "medium", "high")
      )
    ) %>%
    select(-.n_selected, -.n_considered, -.n_features)

}

#' Code whether dimensionality reduction was used
#'
#' Creates `dim_reduction` as yes/no from the feature selection description.
#'
#' @param features data frame with feature extraction data
#'
#' @return `features` with `dim_reduction`.
code_dim_reduction <- function(features) {

  features %>%
    mutate(
      .selection_lower = tolower(`Feature selection`),
      .features_lower = tolower(Features),
      dim_reduction = if_else(
        grepl("pca|principal component|singular value decomposition|svd|autoencoder",
              .selection_lower) |
          grepl("autoencoder", .features_lower),
        "yes", "no"
      )
    ) %>%
    select(-.selection_lower, -.features_lower)

}
