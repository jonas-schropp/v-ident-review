#' Reusable effect plots for the Bayesian meta-analysis
#'
#' Two publication figure types, both on the interpretable predicted-accuracy (%)
#' scale and both driven by the same posterior draws so they always agree:
#'   * `ma_effect_forest()` - compact forest of posterior median predicted
#'     accuracy + 95% credible interval per predictor level.
#'   * `ma_raincloud()` - gghalves half-violin + half-boxplot of the posterior of
#'     predicted accuracy per predictor level.
#'
#' The building blocks are predictor-agnostic. Each model contributes its
#' prediction grid through `ma_effect_specs`, a registry mapping a construct name
#' to a function(data) that returns one row per level with the model's predictor
#' columns set (other covariates are filled at reference by `ma_complete_newdata()`).
#' Sensitivity analyses reuse everything here and only add/replace a spec entry.
#'
#' Depends on: ggplot2, gghalves, dplyr, scales, and the palette/theme in
#' `ma-theme.R`. `ma_complete_newdata()` is defined in `tidy_ma_summary.R`.

#' Prediction-grid registry: construct -> function(data) -> one row per level
#'
#' `.level` is the (ordered) label shown on the plots; the remaining columns set
#' the model's predictor(s) for that level. Kept in sync with the model formulas
#' in `meta-analysis-script.R` (predictors that vary within study use their
#' within/between columns; the rest are entered directly).
ma_effect_specs <- list(
  modality = function(data) data.frame(
    .level = factor(c("Other", "ECG", "PPG"), levels = c("Other", "ECG", "PPG")),
    ECG = c(0, 1, 0), PPG = c(0, 0, 1)
  ),
  algorithm = function(data) {
    levs <- levels(data$algo)
    labs <- unname(ifelse(levs %in% names(ma_algo_labels), ma_algo_labels[levs], levs))
    # `.level` shows the readable label; `algo` keeps the model's factor codes.
    data.frame(.level = factor(labs, levels = labs), algo = factor(levs, levels = levs))
  },
  duration = function(data) {
    levs <- levels(data$duration)
    data.frame(.level = factor(levs, levels = levs), duration = ordered(levs, levels = levs))
  },
  multimodal = function(data) data.frame(
    .level = factor(c("Unimodal", "Multimodal"), levels = c("Unimodal", "Multimodal")),
    multimodal = c(0, 1)
  ),
  intruders = function(data) data.frame(
    .level = factor(c("No intruders", "Intruders"), levels = c("No intruders", "Intruders")),
    intruders = c(FALSE, TRUE)
  ),
  feature_type = function(data) {
    # Derive levels from the data rather than hardcoding, so alternative codings
    # (e.g. the collapsed feature-set sensitivity analysis) work unchanged.
    x <- data$feature_type
    levs <- if (is.factor(x)) levels(droplevels(x)) else sort(unique(stats::na.omit(as.character(x))))
    pref <- c("handcrafted", "deep", "hybrid")           # canonical order when present
    levs <- c(intersect(pref, levs), setdiff(levs, pref))
    data.frame(.level = factor(levs, levels = levs), feature_type = factor(levs, levels = levs))
  },
  activity = function(data) {
    levs <- levels(data$activity)
    data.frame(
      .level = factor(levs, levels = levs),
      multiC_within = 0, everyC_within = 0,
      multiC_between = as.integer(levs == "multiple conditions"),
      everyC_between = as.integer(grepl("everyday", levs))
    )
  }
)

#' Readable labels for the collapsed algorithm-family levels
#' (source: code/tables/tables_authentication_algorithms.R). "Other algorithm"
#' collapses the sparse linear/distance/probability classes for the meta-analysis.
ma_algo_labels <- c(
  "Other algorithm" = "Other algorithm",
  dl = "Deep learning",
  dl_temp = "Temporal deep learning",
  tree = "Tree-based methods",
  kernel = "Kernel-based methods"
)

#' Human-readable construct labels for facet strips / titles.
ma_construct_labels <- c(
  modality = "Modality", algorithm = "Algorithm family", duration = "Study duration",
  multimodal = "Multimodality", intruders = "Intruders", feature_type = "Feature type",
  activity = "Activity"
)

#' Posterior draws of predicted accuracy for a prediction grid
#'
#' @param fit A `brmsfit`.
#' @param newdata One row per level, with a `.level` label column and the target
#'   predictor(s) set. Other covariates are completed at reference values.
#' @param re_formula Passed to `posterior_epred()`. `NA` (default) gives
#'   population-level (new-study) predictions.
#' @param ndraws Optional thinning for lighter plots.
#'
#' @returns Long data.frame with columns `.level`, `.draw`, `accuracy` (0-1).
ma_accuracy_draws <- function(fit, newdata, re_formula = NA, ndraws = NULL) {
  lev_order <- if (is.factor(newdata$.level)) levels(newdata$.level) else unique(as.character(newdata$.level))
  nd <- ma_complete_newdata(fit, newdata)
  ep <- brms::posterior_epred(fit, newdata = nd, re_formula = re_formula, ndraws = ndraws)
  acc <- stats::plogis(ep)               # ep is on the logit(accuracy) response scale
  lvl <- as.character(nd$.level)
  data.frame(
    .level   = factor(rep(lvl, each = nrow(acc)), levels = lev_order),
    .draw    = rep(seq_len(nrow(acc)), times = ncol(acc)),
    accuracy = as.vector(acc),
    stringsAsFactors = FALSE
  )
}

#' Posterior predicted-accuracy draws for a named construct (via the registry)
#'
#' @param fit A `brmsfit` for the model of `construct`.
#' @param data One completed imputed data.frame (for factor levels), e.g. `midat[[1]]`.
#' @param construct Name in `ma_effect_specs`.
#' @param specs Spec registry (override for sensitivity analyses).
#' @param re_formula Passed through to `ma_accuracy_draws()`.
#'
#' @returns `ma_accuracy_draws()` output with an added `construct` column.
ma_construct_accuracy <- function(fit, data, construct, specs = ma_effect_specs, re_formula = NA) {
  spec <- specs[[construct]]
  if (is.null(spec)) stop("No effect spec registered for construct '", construct, "'.", call. = FALSE)
  draws <- ma_accuracy_draws(fit, spec(data), re_formula = re_formula)
  draws$construct <- unname(ma_construct_labels[construct] %||% construct)
  draws
}

#' Summarise predicted-accuracy draws to median + credible interval per level
#'
#' @param draws Output of `ma_accuracy_draws()` / `ma_construct_accuracy()`.
#' @param prob Credible interval mass (default 0.95).
#'
#' @returns data.frame with `.level` (+ `construct` if present), `median`, `lower`, `upper`.
ma_summarise_accuracy <- function(draws, prob = 0.95) {
  a <- (1 - prob) / 2
  grp <- intersect(c("construct", ".level"), names(draws))
  draws |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grp))) |>
    dplyr::summarise(
      median = stats::median(accuracy),
      lower  = stats::quantile(accuracy, a),
      upper  = stats::quantile(accuracy, 1 - a),
      .groups = "drop"
    )
}

#' Manuscript note text for the predicted-accuracy forest
#'
#' The figures deliberately carry no embedded note (journals require it beneath
#' the figure in the manuscript). This returns the note text to paste into the
#' manuscript caption; the pipeline also writes it next to each figure as a
#' `*_note.txt` companion file.
#'
#' @param prob Credible interval mass.
#' @param overlay Logical; `TRUE` for the two-model (marginal + adjusted) figure,
#'   `FALSE` for a single-model figure.
#'
#' @returns A single character string.
ma_forest_note <- function(prob = 0.95, overlay = TRUE) {
  pct <- round(100 * prob)
  if (!overlay) {
    return(sprintf(paste0(
      "Note. Points are posterior medians of the model-predicted authentication accuracy for a new ",
      "study, with all other covariates held at reference values; bars give %d%% credible intervals. ",
      "Numbers to the right of each row report the median [%d%% CrI] in percentage points. ",
      "Predictions are population-level (marginal with respect to the study random effect). ",
      "The dashed vertical line marks the null-model grand mean."), pct, pct))
  }
  sprintf(paste0(
    "Note. Points are posterior medians of the model-predicted authentication accuracy for a new ",
    "study, with all other covariates held at reference values; bars give %d%% credible intervals. ",
    "Filled points with dashed bars are estimates from the single-predictor (marginal) model for each ",
    "moderator; hollow points with solid bars are estimates from the full model, adjusted for the ",
    "other moderators in that model. Italic numbers report the adjusted median [%d%% CrI] in ",
    "percentage points. Intruders and feature type were pre-specified as single-predictor / ",
    "sensitivity terms and are therefore not included in the full model, so no adjusted estimate is ",
    "shown for them; their marginal estimates are reported in the predicted-accuracy table. ",
    "Predictions are population-level (marginal with respect to the study random effect). ",
    "The dashed vertical line marks the null-model grand mean."), pct, pct)
}

#' Constructs for which a (multi-predictor) full model can give adjusted estimates
#'
#' A construct is only estimable from the full model if that model actually
#' contains its predictor(s) in the mean part; otherwise predicting from it would
#' hold the construct at its reference value and return an identical (flat)
#' estimate for every level. Checks the fit's mean-part fixed-effect names, so
#' e.g. `algo` matches `algodl`/`algotree` and `duration` matches `duration.L`.
#'
#' @param full_fit A `brmsfit` for the multi-predictor model.
#' @param constructs Candidate construct names.
#' @param data One completed data frame (to evaluate the specs).
#' @param specs Spec registry supplying each construct's predictor columns.
#'
#' @returns The subset of `constructs` the full model can adjust for.
ma_full_model_constructs <- function(full_fit, constructs, data, specs = ma_effect_specs) {
  fx <- rownames(brms::fixef(full_fit))
  fx <- fx[!grepl("^sigma_", fx)]                     # mean part only
  Filter(function(cn) {
    cols <- setdiff(names(specs[[cn]](data)), ".level")
    any(vapply(cols, function(v) any(startsWith(fx, v)), logical(1)))
  }, constructs)
}

#' Forest plot of predicted accuracy (%) + credible interval per level
#'
#' When `summary_df` has a `.source` column (e.g. "Single-predictor" / "Full
#' model"), both estimates for each level are drawn dodged on the same row -
#' filled point + dashed bar for the marginal model, hollow point + solid bar for
#' the adjusted one (shape and linetype redundantly encoded) - while
#' only ONE numeric column is printed (the adjusted estimate, in italics), so the
#' numbers are never duplicated. Colour continues to encode construct, so the
#' marginal/adjusted distinction uses shape and font instead of hue.
#'
#' @param summary_df Output of `ma_summarise_accuracy()` (must have `.level`,
#'   `median`, `lower`, `upper`; optional `construct` to facet by, optional
#'   `.source` to overlay two model types).
#' @param title,subtitle Plot title/subtitle.
#' @param ref Optional reference line (e.g. null-model grand-mean accuracy).
#' @param prob Credible interval mass (used by `ma_forest_note()`).
#' @param caption Optional caption drawn inside the figure. Defaults to `NULL`:
#'   journals normally require the note to sit beneath the figure in the
#'   manuscript rather than inside the image, so use `ma_forest_note()` to get the
#'   note text for the manuscript caption instead of embedding it here.
#'
#' @returns A ggplot object.
ma_effect_forest <- function(summary_df, title = NULL, subtitle = NULL, ref = NULL,
                             prob = 0.95, caption = NULL) {
  has_construct <- "construct" %in% names(summary_df)
  has_source <- ".source" %in% names(summary_df)
  aes_col <- if (has_construct) "construct" else ".level"
  # Numeric label per row, e.g. "88.0 [80.2, 92.3]" (percentage points).
  summary_df$.value_label <- sprintf(
    "%.1f [%.1f, %.1f]", 100 * summary_df$median, 100 * summary_df$lower, 100 * summary_df$upper
  )
  dodge <- if (has_source) ggplot2::position_dodge(width = 0.6) else ggplot2::position_identity()

  p <- ggplot2::ggplot(
    summary_df,
    ggplot2::aes(x = median, y = .level, colour = .data[[aes_col]],
                 group = if (has_source) .source else NULL)
  )
  if (!is.null(ref)) {
    p <- p + ggplot2::geom_vline(xintercept = ref, linetype = "dashed",
                                 colour = "grey55", linewidth = 0.4)
  }
  # One pointrange layer (rather than separate linerange + point) so that
  # position_dodge can resolve the vertical orientation correctly.
  # Filled solid point = single-predictor (marginal); hollow ring = full model
  # (adjusted). Colour still encodes construct, so the model type uses shape only.
  p <- if (has_source) {
    p + ggplot2::geom_pointrange(
      ggplot2::aes(xmin = lower, xmax = upper, shape = .source, linetype = .source),
      fill = "white", size = 0.45, linewidth = 0.7, stroke = 0.7,
      orientation = "y", position = dodge
    ) +
      ggplot2::scale_shape_manual(
        values = stats::setNames(c(16, 21), levels(summary_df$.source)), guide = "none") +
      # Redundant encoding (shape + linetype) so the two models stay separable in
      # grayscale and for colour-vision-deficient readers.
      ggplot2::scale_linetype_manual(
        values = stats::setNames(c("dashed", "solid"), levels(summary_df$.source)), guide = "none")
  } else {
    p + ggplot2::geom_pointrange(
      ggplot2::aes(xmin = lower, xmax = upper),
      size = 0.5, linewidth = 0.7, orientation = "y"
    )
  }
  # Right-hand numeric column. With both model types shown, only ONE set of
  # numbers is printed - the adjusted (full-model) estimate, in italics to match
  # the hollow point - so the column is not duplicated. Levels with no adjusted
  # estimate (constructs the full model does not contain) carry no number. The
  # text is centred on the row (not dodged) since there is a single number.
  label_df <- if (has_source) summary_df[summary_df$.source == levels(summary_df$.source)[2], , drop = FALSE] else summary_df
  p <- p +
    ggplot2::geom_text(
      data = label_df,
      mapping = ggplot2::aes(y = .level, label = .value_label),
      x = Inf, hjust = 1, size = 2.9, colour = "grey20",
      fontface = if (has_source) 3 else 1,
      inherit.aes = FALSE, show.legend = FALSE
    ) +
    scale_colour_ma() +
    ggplot2::scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = ggplot2::expansion(mult = c(0.03, 0.34))
    ) +
    ggplot2::labs(
      title = title, subtitle = subtitle,
      x = "Predicted accuracy (new study)", y = NULL,
      caption = caption
    ) +
    theme_ma(grid = "x") +
    ggplot2::guides(colour = "none")
  if (has_construct) {
    p <- p + ggplot2::facet_grid(rows = ggplot2::vars(construct),
                                 scales = "free_y", space = "free_y", switch = "y") +
      ggplot2::theme(strip.placement = "outside",
                     strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 1))
  }
  p
}

#' Raincloud (half-violin + half-boxplot) of predicted-accuracy posteriors
#'
#' @param draws Output of `ma_accuracy_draws()` / `ma_construct_accuracy()`.
#' @param title,subtitle Plot title/subtitle.
#' @param flip Horizontal layout (default TRUE).
#'
#' @returns A ggplot object.
ma_raincloud <- function(draws, title = NULL, subtitle = NULL, flip = TRUE) {
  p <- ggplot2::ggplot(draws, ggplot2::aes(x = .level, y = accuracy,
                                           fill = .level, colour = .level)) +
    gghalves::geom_half_violin(side = "r", alpha = 0.65, colour = NA,
                               trim = FALSE, scale = "width", width = 0.9) +
    gghalves::geom_half_boxplot(side = "l", width = 0.4, nudge = 0.02,
                                outlier.shape = NA, alpha = 0.9, linewidth = 0.4) +
    scale_fill_ma() + scale_colour_ma() +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::labs(title = title, subtitle = subtitle,
                  x = NULL, y = "Predicted accuracy (new study)") +
    theme_ma(grid = if (flip) "x" else "y") +
    ggplot2::guides(fill = "none", colour = "none")
  if (flip) p <- p + ggplot2::coord_flip()
  p
}

#' Build the combined predicted-accuracy forest across single-predictor models
#'
#' @param fits Named list of `brmsfit` (or combined multiple fits) keyed by construct.
#' @param data One completed imputed data.frame (for factor levels).
#' @param constructs Which constructs to include (defaults to the registered set
#'   present in `fits`).
#' @param fit_null Optional null `brmsfit` to draw a grand-mean reference line.
#' @param full_fit Optional multi-predictor (full) `brmsfit`. When supplied, each
#'   construct the full model contains is also shown as an adjusted estimate,
#'   dodged against its single-predictor (marginal) estimate. Constructs absent
#'   from the full model (e.g. pre-specified single-predictor/sensitivity terms)
#'   show the marginal estimate only.
#' @param prob Credible interval mass.
#'
#' @returns A ggplot forest faceted by construct.
ma_effect_forest_all <- function(fits, data, constructs = NULL, fit_null = NULL,
                                 full_fit = NULL, prob = 0.95) {
  if (is.null(constructs)) constructs <- intersect(names(ma_effect_specs), names(fits))
  summ <- dplyr::bind_rows(lapply(constructs, function(cn) {
    ma_summarise_accuracy(ma_construct_accuracy(fits[[cn]], data, cn), prob = prob)
  }))

  if (!is.null(full_fit)) {
    adj_constructs <- ma_full_model_constructs(full_fit, constructs, data = data)
    if (length(adj_constructs)) {
      adj <- dplyr::bind_rows(lapply(adj_constructs, function(cn) {
        ma_summarise_accuracy(ma_construct_accuracy(full_fit, data, cn), prob = prob)
      }))
      summ$.source <- "Single-predictor"
      adj$.source  <- "Full model"
      summ <- dplyr::bind_rows(summ, adj)
      summ$.source <- factor(summ$.source, levels = c("Single-predictor", "Full model"))
    }
  }
  # Preserve construct display order.
  summ$construct <- factor(summ$construct, levels = unname(ma_construct_labels[constructs]))
  ref <- NULL
  if (!is.null(fit_null)) {
    nd <- ma_complete_newdata(fit_null, data.frame(.level = factor("overall")))
    ref <- stats::median(stats::plogis(brms::posterior_epred(fit_null, newdata = nd, re_formula = NA)))
  }
  ma_effect_forest(summ, title = "Predicted accuracy by moderator level",
                   subtitle = "Population-level (new-study) posterior predictions",
                   ref = ref, prob = prob)
}

#' Save a forest figure together with its manuscript note
#'
#' Writes the figure as a LZW-compressed TIFF (lossless, ~100x smaller than the
#' uncompressed default) and the note text as a `<stem>_note.txt` companion, since
#' the note must appear beneath the figure in the manuscript rather than inside
#' the image.
#'
#' @param plot A ggplot object from `ma_effect_forest_all()`.
#' @param path Output `.tiff` path.
#' @param width,height,dpi Passed to `ggplot2::ggsave()`.
#' @param overlay,prob Passed to `ma_forest_note()`.
#'
#' @returns Invisibly, the note text.
ma_save_forest <- function(plot, path, width = 7.5, height = 9, dpi = 300,
                           overlay = TRUE, prob = 0.95) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(path, plot = plot, width = width, height = height, dpi = dpi,
                  bg = "white", device = "tiff", compression = "lzw")
  note <- ma_forest_note(prob = prob, overlay = overlay)
  writeLines(note, sub("\\.tiff?$", "_note.txt", path, ignore.case = TRUE))
  invisible(note)
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || is.na(x[1])) y else x
