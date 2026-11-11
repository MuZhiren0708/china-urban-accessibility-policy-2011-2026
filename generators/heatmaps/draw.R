# Reproduce Figures A2, A3 and A5 from the exported clause-linked coding tables.
# Visual reference: the user's Pearson heatmap template.
# Colour rule: preserve the existing A2/A3/A5 yellow-green-cyan-blue palette.
# Dependencies: base R only.
# Run: Rscript draw_A2_A3_A5_template_heatmaps.R

if (.Platform$OS.type == "windows") {
  invisible(suppressWarnings(Sys.setlocale("LC_CTYPE", "English_United States.utf8")))
}

argv <- commandArgs(FALSE)
file_arg <- argv[grepl("^--file=", argv)]
root <- if (length(file_arg)) {
  dirname(normalizePath(sub("^--file=", "", file_arg[1]), mustWork = TRUE))
} else {
  normalizePath(getwd(), mustWork = TRUE)
}

table_dir <- file.path(root, "tables")
svg_dir <- file.path(root, "svg")
preview_dir <- file.path(root, "preview")
dir.create(svg_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(preview_dir, recursive = TRUE, showWarnings = FALSE)

read_table <- function(name) {
  read.csv(file.path(table_dir, name), fileEncoding = "UTF-8",
           stringsAsFactors = FALSE, check.names = FALSE)
}

a2 <- read_table("A2_key_document_actor_matrix.csv")
a3 <- read_table("A3_period_basis_max_mean_counts_evidence.csv")
a5 <- read_table("A5_local_minus_2023_law_scores.csv")

stopifnot(nrow(a2) == 55L, nrow(a3) == 25L, nrow(a5) == 50L,
          all(a2$score %in% 0:3), all(a3$max_score %in% 0:3),
          all(a5$difference %in% -2:2),
          length(unique(a2$id)) == 11L,
          length(unique(a3$period)) == 5L,
          length(unique(a5$place)) == 10L)

matrix_from_long <- function(data, row_key, col_key, value_key,
                             row_levels, col_levels) {
  out <- matrix(NA_real_, nrow = length(row_levels), ncol = length(col_levels),
                dimnames = list(row_levels, col_levels))
  for (i in seq_len(nrow(data))) {
    out[match(data[[row_key]][i], row_levels),
        match(data[[col_key]][i], col_levels)] <- data[[value_key]][i]
  }
  stopifnot(!anyNA(out))
  out
}

# Preserve the audited ordering in the exported tables.
a2_rows <- unique(a2$id)
a2_cols <- unique(a2$actor)
a2_values <- matrix_from_long(a2, "id", "actor", "score", a2_rows, a2_cols)
a2_row_labels <- a2$label[match(a2_rows, a2$id)]
a2_col_labels <- a2$actor_label[match(a2_cols, a2$actor)]
a2_row_labels <- gsub("\\*", "", a2_row_labels)

a3_rows <- unique(a3$basis)
a3_cols <- unique(a3$period)
a3_values <- matrix_from_long(a3, "basis", "period", "max_score", a3_rows, a3_cols)
a3_row_labels <- a3$basis_label[match(a3_rows, a3$basis)]
a3_col_labels <- a3$period_label[match(a3_cols, a3$period)]
a3_col_labels <- gsub("\\*", "", a3_col_labels)

a5_rows <- unique(a5$place)
a5_cols <- unique(a5$dimension)
a5_values <- matrix_from_long(a5, "place", "dimension", "difference", a5_rows, a5_cols)
a5_row_labels <- a5_rows
a5_col_labels <- a5$dimension_label[match(a5_cols, a5$dimension)]
a5_baseline <- a5$baseline_score[match(a5_cols, a5$dimension)]
stopifnot(identical(unique(a5$baseline_id), "N27"), !anyNA(a5_baseline))

# Exact palette family used by the current Figures A2, A3 and A5.
# A5 retains the same sequential palette on its signed -2 to +2 scale.
original_palette_anchors <- c(
  "#FFFFD9", "#EDF8B1", "#C7E9B4", "#7FCDBB", "#41B6C4",
  "#1D91C0", "#225EA8", "#253494", "#081D58"
)
original_palette <- grDevices::colorRampPalette(
  original_palette_anchors, space = "Lab"
)(256)
write.csv(data.frame(position = seq(0, 1, length.out = length(original_palette)),
                     colour = original_palette),
          file.path(table_dir, "original_heatmap_palette.csv"), row.names = FALSE)

colour_for <- function(value, limits) {
  index <- round((value - limits[1]) / diff(limits) * (length(original_palette) - 1)) + 1L
  original_palette[pmax(1L, pmin(length(original_palette), index))]
}

contrast_colour <- function(fill) {
  rgb <- grDevices::col2rgb(fill) / 255
  luminance <- 0.2126 * rgb[1, ] + 0.7152 * rgb[2, ] + 0.0722 * rgb[3, ]
  ifelse(luminance < 0.53, "white", "#222222")
}

wrap_label <- function(x, width) {
  vapply(x, function(item) paste(strwrap(item, width = width), collapse = "\n"), character(1))
}

draw_colourbar <- function(limits, x, bottom, top, title) {
  n <- length(original_palette)
  breaks <- seq(bottom, top, length.out = n + 1L)
  for (i in seq_len(n)) {
    rect(x, breaks[i], x + 0.022, breaks[i + 1],
         col = original_palette[i], border = NA, xpd = NA)
  }
  rect(x, bottom, x + 0.022, top, border = "#333333", lwd = 0.8, xpd = NA)
  ticks <- seq(limits[1], limits[2])
  tick_y <- bottom + (ticks - limits[1]) / diff(limits) * (top - bottom)
  segments(x + 0.022, tick_y, x + 0.028, tick_y, col = "#333333", xpd = NA)
  tick_labels <- ifelse(limits[1] < 0 & ticks > 0,
                        paste0("+", ticks), as.character(ticks))
  text(x + 0.034, tick_y, tick_labels, adj = c(0, 0.5),
       family = "Times New Roman", cex = 0.76, xpd = NA)
  text(x + 0.092, (bottom + top) / 2, title, srt = 90,
       family = "Times New Roman", font = 3, cex = 0.76, xpd = NA)
}

draw_heatmap <- function(values, row_labels, col_labels, title, limits,
                         bar_title, layout,
                         show_plus = FALSE, baseline = NULL) {
  par(mar = rep(0, 4), family = "Times New Roman", xaxs = "i", yaxs = "i")
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i")

  left <- layout$left
  right <- layout$right
  bottom <- layout$bottom
  top <- layout$top
  nr <- nrow(values)
  nc <- ncol(values)
  cell_w <- (right - left) / nc
  cell_h <- (top - bottom) / nr

  # Figure names belong in the manuscript captions, outside the artwork.

  for (i in seq_len(nr)) {
    for (j in seq_len(nc)) {
      x0 <- left + (j - 1) * cell_w
      x1 <- x0 + cell_w
      y1 <- top - (i - 1) * cell_h
      y0 <- y1 - cell_h
      fill <- colour_for(values[i, j], limits)
      rect(x0, y0, x1, y1, col = fill, border = "white", lwd = 0.9)
      value_label <- if (show_plus && values[i, j] > 0) {
        paste0("+", values[i, j])
      } else {
        as.character(values[i, j])
      }
      text((x0 + x1) / 2, (y0 + y1) / 2, value_label,
           family = "Times New Roman", font = 2, cex = layout$value_cex,
           col = contrast_colour(fill))
    }
  }
  rect(left, bottom, right, top, border = "#4A4A4A", lwd = 0.8)

  row_y <- top - (seq_len(nr) - 0.5) * cell_h
  text(left - 0.014, row_y, wrap_label(row_labels, layout$row_wrap),
       adj = c(1, 0.5), family = "Times New Roman", font = 3,
       cex = layout$row_cex, xpd = NA)

  col_x <- left + (seq_len(nc) - 0.5) * cell_w
  text(col_x - 0.006, bottom - 0.025, gsub("\n", " ", col_labels),
       srt = 45, adj = c(1, 0.5), family = "Times New Roman", font = 3,
       cex = layout$col_cex, xpd = NA)

  if (!is.null(baseline)) {
    base_y <- layout$baseline_y
    text(left - 0.014, base_y, "2023 law baseline\n(0-2 scale)",
         adj = c(1, 0.5), family = "Times New Roman", font = 3,
         cex = layout$baseline_cex, col = "#596773", xpd = NA)
    for (j in seq_len(nc)) {
      x0 <- left + (j - 1) * cell_w
      x1 <- x0 + cell_w
      rect(x0, base_y - 0.028, x1, base_y + 0.028,
           col = "#F0F2F4", border = "white", lwd = 0.9, xpd = NA)
      text((x0 + x1) / 2, base_y, baseline[j], family = "Times New Roman",
           cex = layout$baseline_cex, col = "#596773", xpd = NA)
    }
  }

  draw_colourbar(limits, layout$bar_x, bottom, top, bar_title)
}

layouts <- list(
  A2 = list(left = 0.25, right = 0.79, bottom = 0.20, top = 0.89,
            bar_x = 0.845, title_cex = 1.12, value_cex = 0.92,
            row_cex = 0.75, col_cex = 0.73, row_wrap = 27),
  A3 = list(left = 0.25, right = 0.79, bottom = 0.20, top = 0.89,
            bar_x = 0.845, title_cex = 1.12, value_cex = 1.00,
            row_cex = 0.88, col_cex = 0.84, row_wrap = 22),
  A5 = list(left = 0.21, right = 0.79, bottom = 0.20, top = 0.89,
            bar_x = 0.845, title_cex = 1.12, value_cex = 0.90,
            row_cex = 0.82, col_cex = 0.73, row_wrap = 22)
)

figures <- list(
  list(id = "A2", stem = "A2_key_document_actor_authority",
       width = 10.5, height = 8.5,
       draw = function() draw_heatmap(
         a2_values, a2_row_labels, a2_col_labels,
         "Authority scores of key policy documents", c(0, 3),
         "Authority score", layouts$A2)),
  list(id = "A3", stem = "A3_period_basis_intensity",
       width = 9.5, height = 7.6,
       draw = function() draw_heatmap(
         a3_values, a3_row_labels, a3_col_labels,
         "Maximum judgment-basis score by publication period", c(0, 3),
         "Maximum score", layouts$A3)),
  list(id = "A5", stem = "A5_local_configuration_difference",
       width = 10.5, height = 8.5,
       draw = function() draw_heatmap(
         a5_values, a5_row_labels, a5_col_labels,
         "Local configuration differences from the 2023 law", c(-2, 2),
         "Local - 2023 law", layouts$A5,
         show_plus = TRUE))
)

for (figure in figures) {
  svg_path <- file.path(svg_dir, paste0(figure$stem, ".svg"))
  grDevices::svg(svg_path, width = figure$width, height = figure$height,
                 pointsize = 12, family = "Times New Roman", bg = "white", onefile = TRUE)
  figure$draw()
  dev.off()

  png_path <- file.path(preview_dir, paste0(figure$stem, ".png"))
  grDevices::png(png_path, width = figure$width, height = figure$height,
                 units = "in", res = 180, type = "cairo", bg = "white")
  figure$draw()
  dev.off()

  svg_text <- paste(readLines(svg_path, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  if (grepl("[\\p{Han}]", svg_text, perl = TRUE)) {
    stop("Chinese character found in English SVG: ", svg_path)
  }
}

validation <- data.frame(
  figure = c("A2", "A3", "A5"),
  rows = c(nrow(a2_values), nrow(a3_values), nrow(a5_values)),
  columns = c(ncol(a2_values), ncol(a3_values), ncol(a5_values)),
  minimum = c(min(a2_values), min(a3_values), min(a5_values)),
  maximum = c(max(a2_values), max(a3_values), max(a5_values)),
  source_rows = c(nrow(a2), nrow(a3), nrow(a5)),
  stringsAsFactors = FALSE
)
write.csv(validation, file.path(table_dir, "template_heatmap_validation.csv"), row.names = FALSE)

message("SVG and PNG files written to: ", root)
message("Validated source rows: A2=55, A3=25, A5=50; no coding values or original palette anchors changed.")
