#' Shared colour system and theme for meta-analysis figures
#'
#' A single place to define the look of every meta-analysis figure so that the
#' main analysis and the sensitivity analyses stay visually consistent. The
#' categorical palette is Paul Tol's "light" qualitative scheme: it is
#' colour-blind safe (deutan/protan/tritan) and reads as a soft, modern pastel.
#'
#' Use `scale_fill_ma()` / `scale_colour_ma()` on any discrete aesthetic, and add
#' `theme_ma()` to every plot. `ma_palette()` returns the raw hex values when a
#' manual scale is needed.

# Paul Tol "light" qualitative palette (colour-blind safe, pastel).
ma_palette_values <- c(
  blue   = "#77AADD",
  orange = "#EE8866",
  yellow = "#EEDD88",
  pink   = "#FFAABB",
  cyan   = "#99DDFF",
  mint   = "#44BB99",
  pear   = "#BBCC33",
  olive  = "#AAAA00",
  grey   = "#BBBBBB"
)

#' Meta-analysis categorical palette
#'
#' @param n Optional number of colours. If `n` exceeds the base palette length
#'   the palette is interpolated with `colorRampPalette()`.
#' @param named Logical; keep the descriptive colour names.
#'
#' @returns Character vector of hex colours.
ma_palette <- function(n = NULL, named = FALSE) {
  v <- if (named) ma_palette_values else unname(ma_palette_values)
  if (is.null(n)) return(v)
  if (n <= length(v)) return(v[seq_len(n)])
  grDevices::colorRampPalette(unname(ma_palette_values))(n)
}

# A darker companion colour for point estimates / medians so they read clearly
# on top of the pastel fills.
ma_ink <- "#333333"

.ma_pal_fun <- function() function(n) ma_palette(n)

#' Discrete ggplot2 fill/colour scales using the meta-analysis palette
#' @param ... Passed to `ggplot2::discrete_scale()`.
scale_fill_ma <- function(...) {
  ggplot2::discrete_scale(aesthetics = "fill", palette = .ma_pal_fun(), ...)
}
scale_colour_ma <- function(...) {
  ggplot2::discrete_scale(aesthetics = "colour", palette = .ma_pal_fun(), ...)
}
#' @rdname scale_colour_ma
scale_color_ma <- scale_colour_ma

#' Publication theme for meta-analysis figures
#'
#' Clean, minimal, print-friendly. `grid` controls which reference gridlines are
#' drawn ("x" suits horizontal forest plots, "y" suits vertical rainclouds).
#'
#' @param base_size Base font size.
#' @param base_family Base font family (default "" = device default).
#' @param grid One of "x", "y", "both", or "none".
#'
#' @returns A ggplot2 theme object.
theme_ma <- function(base_size = 12, base_family = "", grid = c("x", "y", "both", "none")) {
  grid <- match.arg(grid)
  gx <- grid %in% c("x", "both")
  gy <- grid %in% c("y", "both")
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title    = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.05)),
      plot.subtitle = ggplot2::element_text(colour = "grey35", margin = ggplot2::margin(b = 6)),
      plot.caption  = ggplot2::element_text(colour = "grey45", size = ggplot2::rel(0.8), hjust = 0),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = if (gx) ggplot2::element_line(colour = "grey88", linewidth = 0.3) else ggplot2::element_blank(),
      panel.grid.major.y = if (gy) ggplot2::element_line(colour = "grey88", linewidth = 0.3) else ggplot2::element_blank(),
      axis.title    = ggplot2::element_text(colour = "grey20"),
      axis.text     = ggplot2::element_text(colour = "grey25"),
      strip.text    = ggplot2::element_text(face = "bold", colour = "grey15"),
      legend.position = "right",
      legend.title  = ggplot2::element_text(face = "bold")
    )
}
