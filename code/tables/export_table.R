#' Export a table as CSV and DOCX
#'
#' @param x A data frame or tibble to export.
#' @param csv_path string(1) Path where the CSV table should be written.
#'
#' @returns Invisibly returns `x`. The table is written to `csv_path` and to a
#'   matching `.docx` file using `flextable`.
write_table_csv_docx <- function(x, csv_path) {
  requireNamespace("flextable", quietly = TRUE)
  requireNamespace("readr", quietly = TRUE)

  footer <- attr(x, "table_footer", exact = TRUE)

  sanitize_xml_text <- function(value) {
    if (is.character(value)) {
      return(gsub("[\001-\010\013\014\016-\037]", " ", value, useBytes = TRUE))
    }

    if (is.factor(value)) {
      levels(value) <- sanitize_xml_text(levels(value))
      return(value)
    }

    value
  }

  x <- as.data.frame(x)
  names(x) <- sanitize_xml_text(names(x))
  x[] <- lapply(x, sanitize_xml_text)

  readr::write_csv(x, csv_path)

  docx_path <- sub("\\.csv$", ".docx", csv_path, ignore.case = TRUE)
  if (identical(docx_path, csv_path)) {
    docx_path <- paste0(csv_path, ".docx")
  }

  ft <- flextable::flextable(x)

  header_labels <- attr(x, "flextable_header_labels", exact = TRUE)
  if (!is.null(header_labels)) {
    ft <- flextable::set_header_labels(ft, values = as.list(header_labels))
  }

  group_cols <- attr(x, "flextable_group_cols", exact = TRUE)
  group_cols <- intersect(group_cols, names(x))
  if (length(group_cols) > 0) {
    ft <- ft |>
      flextable::merge_v(j = group_cols) |>
      flextable::valign(j = group_cols, valign = "top", part = "body") |>
      flextable::bold(j = group_cols, bold = TRUE, part = "body") |>
      flextable::bg(j = group_cols, bg = "#F2F2F2", part = "body")
  }

  ft <- ft |>
    flextable::theme_booktabs() |>
    flextable::set_table_properties(layout = "autofit")

  footer <- attr(x, "table_footer", exact = TRUE)
  if (!is.null(footer)) {
    footer <- sanitize_xml_text(footer)
    ft <- flextable::add_footer_lines(ft, values = footer)
  }

  flextable::save_as_docx(
    flextable::autofit(ft),
    path = docx_path
  )

  invisible(x)
}
