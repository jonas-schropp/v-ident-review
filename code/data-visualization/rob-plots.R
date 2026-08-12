#' Risk-of-bias traffic-light figures (PROBAST+AI and BRAIA-VS)
#'
#' Replaces the 27-column rating tables, which could not be read: at the journal's
#' 18.46 cm text width, 26 study columns leave about 0.5 cm each, so the ratings were a
#' grid of single letters and the pattern in them was invisible.
#'
#' Domains are rows and studies are columns, which is the transpose of the usual robvis
#' layout. That is deliberate: the domain names ("Governance, Fairness & Safety",
#' "Participants and Data Sources") only fit as row labels, where they have the full width
#' of the page minus the grid. Study IDs are two to three digits and fit as columns.
#'
#' Rating is encoded twice, by fill and by glyph, so the figures survive greyscale
#' printing and colour-vision deficiency - the same redundancy used in the forest plots.
#' "Unclear" is labelled "not reported": these instruments record an unclear rating when a
#' study gives too little information to judge, which is a property of the study, not of
#' the checklist.
#'
#' REGENERATE BEFORE SUBMISSION. The column labels are the review's internal study IDs
#' (28, 36, 43, ...), which are placeholders for the citation numbers the manuscript will
#' use once the reference list is final. When that mapping exists, add it to the CSV or
#' join it here and re-run `rob_write_figures()`; the figures are the only place those
#' internal IDs appear in a form a reader would try to look up.
#'
#' Depends on: ggplot2, dplyr, readr, ma-theme.R.

#' Rating levels, in severity order, with their glyphs and fills.
#'
#' Fills come from the project palette (Paul Tol light, colour-blind safe): mint for low,
#' yellow for moderate, orange for high, grey for unclear. Orange rather than a true red
#' keeps the figure inside the palette used by every other figure in the paper.
rob_levels <- c("Low", "Moderate", "High", "Unclear")
rob_glyphs <- c(Low = "+", Moderate = "−", High = "×", Unclear = "?")
rob_fills  <- c(Low      = unname(ma_palette_values["mint"]),
                Moderate = unname(ma_palette_values["yellow"]),
                High     = unname(ma_palette_values["orange"]),
                Unclear  = unname(ma_palette_values["grey"]))
rob_labels <- c(Low = "Low risk", Moderate = "Moderate risk", High = "High risk",
                Unclear = "Unclear — not reported")

#' Read the hand-made risk-of-bias assessments.
#'
#' @param path CSV with instrument, stage, domain_code, domain, study_id, rating.
#' @returns A data.frame with `rating` a factor in severity order.
rob_read <- function(path = here::here("data", "bias-assessment", "risk_of_bias.csv")) {
  d <- suppressMessages(readr::read_csv(path, show_col_types = FALSE))
  need <- c("instrument", "stage", "domain_code", "domain", "study_id", "rating")
  stopifnot(all(need %in% names(d)))
  bad <- setdiff(unique(d$rating), rob_levels)
  if (length(bad)) stop("unexpected rating(s): ", paste(bad, collapse = ", "))
  d$rating <- factor(d$rating, levels = rob_levels)
  d$study_id <- as.character(d$study_id)
  d
}

#' Traffic-light grid for one instrument.
#'
#' @param d Output of `rob_read()`, already filtered to one instrument.
#' @param study_order Optional study ordering; defaults to numeric order of the IDs.
#' @param facet_stage Facet rows by `stage` (PROBAST+AI assesses development and
#'   evaluation separately, and the two share domain names).
#'
#' @returns A ggplot object.
rob_plot <- function(d, study_order = NULL, facet_stage = FALSE) {
  if (is.null(study_order)) {
    study_order <- unique(d$study_id)
    study_order <- study_order[order(suppressWarnings(as.numeric(study_order)),
                                     study_order, na.last = TRUE)]
  }
  d$study_id <- factor(d$study_id, levels = study_order)
  # rows read top-to-bottom in domain order, so reverse for the discrete y scale
  lab <- paste(d$domain_code, d$domain)
  d$row <- factor(lab, levels = rev(unique(lab[order(d$domain_code)])))
  d$glyph <- rob_glyphs[as.character(d$rating)]
  used <- intersect(rob_levels, unique(as.character(d$rating)))

  p <- ggplot2::ggplot(d, ggplot2::aes(x = study_id, y = row, fill = rating)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = glyph), size = 3, colour = ma_ink) +
    # Only the levels this instrument actually uses. PROBAST+AI grades low/high/unclear -
    # "moderate" is a BRAIA-VS category - so carrying a shared four-level legend would put
    # a category in the PROBAST+AI figure that the instrument does not have. The key border
    # keeps the pale yellow legible where it does appear.
    ggplot2::scale_fill_manual(values = rob_fills, labels = rob_labels[used],
                               breaks = used, drop = TRUE, name = NULL,
                               guide = ggplot2::guide_legend(
                                 override.aes = list(colour = "grey55", linewidth = 0.3))) +
    ggplot2::scale_x_discrete(position = "top", expand = c(0, 0)) +
    ggplot2::scale_y_discrete(expand = c(0, 0)) +
    ggplot2::labs(x = "Study ID", y = NULL) +
    theme_ma(grid = "none") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 7),
      axis.text.y = ggplot2::element_text(hjust = 0),
      panel.border = ggplot2::element_blank(),
      legend.position = "bottom"
    )
  if (facet_stage && length(unique(d$stage)) > 1) {
    p <- p + ggplot2::facet_grid(rows = ggplot2::vars(stage), scales = "free_y",
                                space = "free_y", switch = "y")
  }
  p
}

#' Write both instruments' figures as LZW-compressed TIFFs.
#'
#' LZW keeps these near 100 kB; the ragg default would be tens of megabytes.
#'
#' @param out_dir Directory for the TIFFs.
#' @param path CSV to read.
#' @returns Invisibly, the file paths written.
rob_write_figures <- function(out_dir = here::here("results", "figures"),
                              path = here::here("data", "bias-assessment",
                                                "risk_of_bias.csv")) {
  d <- rob_read(path)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  specs <- list(
    list(instrument = "PROBAST+AI", file = "rob_probast_ai.tiff",
         facet = TRUE,  height = 3.9),
    list(instrument = "BRAIA-VS",   file = "rob_braia_vs.tiff",
         facet = FALSE, height = 3.4)
  )
  out <- character(0)
  for (s in specs) {
    di <- d[d$instrument == s$instrument, , drop = FALSE]
    if (!nrow(di)) next
    p <- rob_plot(di, facet_stage = s$facet)
    f <- file.path(out_dir, s$file)
    ggplot2::ggsave(f, plot = p, width = 7.3, height = s$height, dpi = 300,
                    bg = "white", device = "tiff", compression = "lzw")
    # A PNG companion as well: TIFF is what the journal wants, but Word handles PNG
    # more predictably and it is what the manuscript's other figures use.
    png <- sub("\\.tiff?$", ".png", f, ignore.case = TRUE)
    ggplot2::ggsave(png, plot = p, width = 7.3, height = s$height, dpi = 300, bg = "white")
    out <- c(out, f, png)
    message(sprintf("%-11s %d domains x %d studies -> %s",
                    s$instrument, length(unique(di$domain_code)),
                    length(unique(di$study_id)), basename(f)))
  }
  invisible(out)
}
