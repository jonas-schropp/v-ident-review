# Preprocessing coding

#' Code whether filtering/noise reduction is reported
#'
#' Creates `filteringyn` as a yes/no indicator based on the
#' `noise reduction` field.
#'
#' @param preproc data frame with preprocessing extraction data
#'
#' @return `preproc` with `filteringyn`.
code_filtering <- function(preproc) {

  preproc %>%
    mutate(
      filteringyn = if_else(
        `noise reduction` %in% c("none", "not described", "NA"),
        "no", "yes"
      )
    )

}

#' Code segmentation type
#'
#' Creates `segmentation_type` with categories window, beat, or none
#' from the `segmentation` text.
#'
#' @param preproc data frame with preprocessing extraction data
#'
#' @return `preproc` with `segmentation_type`.
code_segmentation_type <- function(preproc) {

  preproc %>%
    mutate(
      segmentation_type = case_when(
        segmentation %in% c("none", "not described", "NA") ~ "none",
        grepl("window|windows|overlap|stride|interval", segmentation,
              ignore.case = TRUE) ~ "window",
        grepl("beat|beats|heartbeat|R-peak|R peaks|QRS|fiducial|cycle", segmentation,
              ignore.case = TRUE) ~ "beat",
        TRUE ~ "none"
      )
    )

}

#' Code whether normalization is reported
#'
#' Creates `normalizationyn` as a yes/no indicator based on the
#' `normalization` field.
#'
#' @param preproc data frame with preprocessing extraction data
#'
#' @return `preproc` with `normalizationyn`.
code_normalization <- function(preproc) {

  preproc %>%
    mutate(
      normalizationyn = if_else(
        normalization %in% c(
          "no", "not described", "not mentioned", "not reported",
          "not specified", "No explicit normalization (implicit via wavelet)",
          "NA"
        ),
        "no", "yes"
      )
    )

}
