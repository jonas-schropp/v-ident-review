#' APA-styled tables via flextable + officer
#'
#' Reusable helpers to render analysis tables in APA (7th ed.) style and write
#' them to DOCX (for the manuscript) alongside a raw CSV (for inspection).
#' APA table conventions applied: serif font, no vertical rules, horizontal rules
#' only above/below the header and at the foot of the table, left-aligned stub
#' (first) column, plain (non-bold) headers, and an italic "Note." line.
#'
#' Depends on: flextable, officer, readr.

# Remove XML control characters flextable/officer cannot serialise.
.apa_sanitize <- function(value) {
  if (is.character(value)) return(gsub("[\001-\010\013\014\016-\037]", " ", value, useBytes = TRUE))
  if (is.factor(value)) { levels(value) <- .apa_sanitize(levels(value)); return(value) }
  value
}

#' Build an APA-styled flextable
#'
#' @param x A data.frame/tibble (already formatted for display).
#' @param title Table title (rendered as the flextable caption).
#' @param note Optional note; "Note. " is prepended in italics per APA.
#' @param header_labels Optional named vector mapping column names -> display labels.
#' @param group_cols Optional stub columns to merge vertically (e.g. "Construct").
#' @param font,size Font family and point size (APA allows Times New Roman 12,
#'   Calibri 11, Arial 11; default Times New Roman 11).
#'
#' @returns A `flextable` object.
apa_flextable <- function(x, title = NULL, note = NULL, header_labels = NULL,
                          group_cols = NULL, font = "Times New Roman", size = 11) {
  requireNamespace("flextable"); requireNamespace("officer")
  x <- as.data.frame(x)
  names(x) <- .apa_sanitize(names(x))
  x[] <- lapply(x, .apa_sanitize)

  thick <- officer::fp_border(color = "black", width = 1.2)
  thin  <- officer::fp_border(color = "black", width = 0.6)

  ft <- flextable::flextable(x)
  if (!is.null(header_labels)) {
    ft <- flextable::set_header_labels(ft, values = as.list(header_labels))
  }
  ft <- ft |>
    flextable::border_remove() |>
    flextable::hline_top(part = "header", border = thick) |>
    flextable::hline_bottom(part = "header", border = thin) |>
    flextable::hline_bottom(part = "body", border = thick) |>
    flextable::bold(part = "header", bold = FALSE) |>
    flextable::font(fontname = font, part = "all") |>
    flextable::fontsize(size = size, part = "all") |>
    flextable::align(align = "center", part = "all") |>
    flextable::align(j = 1, align = "left", part = "all") |>
    flextable::padding(padding.top = 3, padding.bottom = 3, part = "all") |>
    flextable::line_spacing(space = 1.2, part = "all")

  group_cols <- intersect(group_cols, names(x))
  if (length(group_cols) > 0) {
    ft <- ft |>
      flextable::merge_v(j = group_cols) |>
      flextable::valign(j = group_cols, valign = "top", part = "body")
  }
  if (!is.null(title)) ft <- flextable::set_caption(ft, caption = title)
  if (!is.null(note)) {
    ft <- flextable::add_footer_lines(
      ft, values = flextable::as_paragraph(flextable::as_i("Note. "), note)
    )
    ft <- flextable::fontsize(ft, size = size - 1, part = "footer")
    ft <- flextable::font(ft, fontname = font, part = "footer")
  }
  flextable::set_table_properties(ft, layout = "autofit", width = 1)
}

#' Write a table as CSV + APA-styled DOCX
#'
#' @param x Display-ready data.frame.
#' @param csv_path Path for the CSV; the DOCX is written alongside with `.docx`.
#' @param raw Optional unformatted data.frame to write to CSV instead of `x`
#'   (e.g. numeric columns rather than display strings).
#' @inheritParams apa_flextable
#'
#' @returns Invisibly, the DOCX path.
write_apa_table <- function(x, csv_path, title = NULL, note = NULL, header_labels = NULL,
                            group_cols = NULL, font = "Times New Roman", size = 11,
                            raw = NULL) {
  requireNamespace("readr")
  readr::write_csv(if (is.null(raw)) as.data.frame(x) else as.data.frame(raw), csv_path, na = "")
  docx_path <- sub("\\.csv$", ".docx", csv_path, ignore.case = TRUE)
  if (identical(docx_path, csv_path)) docx_path <- paste0(csv_path, ".docx")
  ft <- apa_flextable(x, title = title, note = note, header_labels = header_labels,
                      group_cols = group_cols, font = font, size = size)
  flextable::save_as_docx(ft, path = docx_path)
  invisible(docx_path)
}
