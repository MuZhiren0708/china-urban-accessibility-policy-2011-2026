# Reproducible advanced figures for the 59-document urban accessibility corpus.
# Run from any directory:
#   "C:/Program Files/R/R-4.6.1/bin/Rscript.exe" draw_all_advanced_figures.R
#
# The script uses only base R. All visible figure text is English.

args <- commandArgs(trailingOnly = TRUE)
script_arg <- grep("^--file=", commandArgs(), value = TRUE)
# R 4.6 on Windows may start in the non-multibyte C locale even when the
# system code page is UTF-8. Set an explicit UTF-8 locale before reading the
# Chinese source titles retained in the audit tables.
if (.Platform$OS.type == "windows") {
  try(Sys.setlocale("LC_ALL", "Chinese (Simplified)_China.utf8"), silent = TRUE)
}
root <- if (length(args)) {
  normalizePath(args[1], winslash = "/", mustWork = TRUE)
} else if (length(script_arg)) {
  dirname(normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/", mustWork = TRUE))
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

data_dir <- file.path(root, "data")
svg_dir <- file.path(root, "svg")
table_dir <- file.path(root, "tables")
dir.create(svg_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(table_dir, showWarnings = FALSE, recursive = TRUE)

read_utf8_csv <- function(path) {
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
           fileEncoding = "UTF-8-BOM")
}

scores <- read_utf8_csv(file.path(data_dir, "reviewed_59_document_scores.csv"))
sample59 <- read_utf8_csv(file.path(data_dir, "policy_sample_59.csv"))
mechanisms <- read_utf8_csv(file.path(data_dir, "mechanism_coding_80.csv"))
province_labels <- read_utf8_csv(file.path(data_dir, "province_labels.csv"))
china <- read_utf8_csv(file.path(data_dir, "china_vertices.csv"))
land <- read_utf8_csv(file.path(data_dir, "world_land_vertices.csv"))

score_ids <- as.character(scores$id)
sample_ids <- as.character(sample59$id)
mechanisms <- mechanisms[match(score_ids, mechanisms$id), , drop = FALSE]
sample59 <- sample59[match(score_ids, sample59$id), , drop = FALSE]

# The paired 16-field matrix replaces the earlier binary assignments.
# The 21 explicit decisions and five supplementary source verifications are
# recorded separately in binary_provenance.csv; ordinal scores are unchanged.
final_mechanism_path <- file.path(data_dir, "mechanism_final_adjudicated.csv")
flow_ready <- file.exists(final_mechanism_path)
if (flow_ready) {
  final_mechanisms <- read_utf8_csv(final_mechanism_path)
  binary_fields <- c("CONS", "DES", "TRY", "EVAL", "SUP", "RESP", "LINK", "ADOPT",
                     "POWER", "QUOTA", "SUPPORT", "FILTER", "CHOICE", "SATGATE", "MTRY", "CONDITION")
  stopifnot(!anyDuplicated(final_mechanisms$id), all(score_ids %in% final_mechanisms$id),
            all(c(binary_fields,"eligibility_flag") %in% names(final_mechanisms)))
  j <- match(score_ids, final_mechanisms$id)
  for (v in binary_fields) mechanisms[[v]] <- as.integer(final_mechanisms[[v]][j])
  mechanisms$eligibility_flag <- as.integer(final_mechanisms$eligibility_flag[j])
  eligible <- mechanisms$eligibility_flag == 1L
  stopifnot(!anyNA(eligible),all(as.matrix(mechanisms[eligible,binary_fields]) %in% 0:1))
}

stopifnot(nrow(scores) == 59L,
          !anyDuplicated(score_ids),
          identical(score_ids, as.character(mechanisms$id)),
          identical(score_ids, as.character(sample59$id)),
          sum(sample59$scope == "National") == 24L,
          sum(sample59$scope == "Subnational") == 35L,
          sum(as.logical(scores$strict)) == 56L)

actor_codes <- c("TECH", "DEV", "ADM", "THIRD", "USER")
actor_labels <- c("Technical review\nbodies", "Project\nowners",
                  "Administrative\nauthorities", "Third-party\nevaluators", "Users")
basis_codes <- c("TECHNICAL", "DOCUMENTS", "EXPERIENCE", "TRACE", "PUBLIC")
basis_labels <- c("Technical\ncompliance", "Procedural\ndocumentation",
                  "User\nexperience", "Responsibility\ntraceability",
                  "Public\ndisclosure")
period_codes <- paste0("P", 1:5)
period_labels <- c("2011-2013", "2014-2016", "2017-2019", "2020-2022", "2023-2026")

period_from_year <- function(x) {
  paste0("P", cut(as.integer(x), breaks = c(2010, 2013, 2016, 2019, 2022, 2026),
                  labels = FALSE, include.lowest = TRUE))
}
stopifnot(all(scores$period == period_from_year(scores$year)))

actor_cols <- paste0("actor_", actor_codes)
basis_cols <- paste0("basis_", basis_codes)
for (v in c(actor_cols, basis_cols)) scores[[v]] <- as.integer(scores[[v]])
for (v in c("CONS", "DES", "TRY", "EVAL", "SUP", "RESP", "LINK",
            "ADOPT", "POWER", "SATGATE")) mechanisms[[v]] <- as.integer(mechanisms[[v]])

# ---------- SVG helpers ----------

esc <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub('"', "&quot;", x, fixed = TRUE)
}
f <- function(x) formatC(x, digits = 2, format = "f", decimal.mark = ".")
alpha_hex <- function(col, alpha = 1) {
  rgbv <- col2rgb(col) / 255
  rgb(rgbv[1], rgbv[2], rgbv[3], alpha = alpha)
}
rect_svg <- function(x, y, w, h, fill = "none", stroke = "none", sw = 1,
                     rx = 0, opacity = NULL) {
  sprintf('<rect x="%s" y="%s" width="%s" height="%s" rx="%s" fill="%s" stroke="%s" stroke-width="%s"%s/>',
          f(x), f(y), f(w), f(h), f(rx), fill, stroke, f(sw),
          if (is.null(opacity)) "" else paste0(' opacity="', f(opacity), '"'))
}
line_svg <- function(x1, y1, x2, y2, col = "#7B8B95", sw = 1,
                     dash = NULL, opacity = NULL) {
  sprintf('<line x1="%s" y1="%s" x2="%s" y2="%s" stroke="%s" stroke-width="%s"%s%s/>',
          f(x1), f(y1), f(x2), f(y2), col, f(sw),
          if (is.null(dash)) "" else paste0(' stroke-dasharray="', dash, '"'),
          if (is.null(opacity)) "" else paste0(' opacity="', f(opacity), '"'))
}
circle_svg <- function(cx, cy, r, fill = "none", stroke = "none", sw = 1,
                       opacity = NULL) {
  sprintf('<circle cx="%s" cy="%s" r="%s" fill="%s" stroke="%s" stroke-width="%s"%s/>',
          f(cx), f(cy), f(r), fill, stroke, f(sw),
          if (is.null(opacity)) "" else paste0(' opacity="', f(opacity), '"'))
}
path_svg <- function(d, fill = "none", stroke = "none", sw = 1,
                     opacity = NULL, linecap = "round", linejoin = "round") {
  sprintf('<path d="%s" fill="%s" stroke="%s" stroke-width="%s" stroke-linecap="%s" stroke-linejoin="%s"%s/>',
          d, fill, stroke, f(sw), linecap, linejoin,
          if (is.null(opacity)) "" else paste0(' opacity="', f(opacity), '"'))
}
text_svg <- function(x, y, label, size = 18, weight = "normal", anchor = "start",
                     col = "#203746", italic = FALSE, rotate = NULL) {
  transform <- if (is.null(rotate)) "" else sprintf(' transform="rotate(%s %s %s)"', f(rotate), f(x), f(y))
  sprintf('<text x="%s" y="%s" font-family="Times New Roman" font-size="%s" font-weight="%s" text-anchor="%s" fill="%s"%s%s>%s</text>',
          f(x), f(y), f(size), weight, anchor, col,
          if (italic) ' font-style="italic"' else "", transform, esc(label))
}
multiline_svg <- function(x, y, labels, size = 18, weight = "normal",
                          anchor = "start", col = "#203746", leading = 1.16,
                          italic = FALSE) {
  labels <- as.character(labels)
  # Separate text elements with absolute y coordinates render consistently in
  # browsers, Microsoft Word, WPS and Windows SVG previewers. Some Office SVG
  # engines collapse relative tspan dy values and visibly overlap the lines.
  line_step <- size * max(leading, 1.70)
  paste(vapply(seq_along(labels), function(i) {
    sprintf('<text x="%s" y="%s" font-family="Times New Roman" font-size="%s" font-weight="%s" text-anchor="%s" fill="%s"%s>%s</text>',
            f(x), f(y + (i - 1) * line_step), f(size), weight, anchor, col,
            if (italic) ' font-style="italic"' else "", esc(labels[i]))
  }, ""), collapse = "")
}
svg_document <- function(width, height, title, desc, body, defs = "") {
  c('<?xml version="1.0" encoding="UTF-8" standalone="no"?>',
    sprintf('<svg xmlns="http://www.w3.org/2000/svg" width="%s" height="%s" viewBox="0 0 %s %s" role="img" aria-labelledby="title desc">',
            width, height, width, height),
    paste0('<title id="title">', esc(title), '</title>'),
    paste0('<desc id="desc">', esc(desc), '</desc>'),
    paste0('<defs><style>text{font-family:"Times New Roman";letter-spacing:0}</style>', defs, '</defs>'),
    rect_svg(0, 0, width, height, "#FFFFFF"), body, '</svg>')
}
write_svg <- function(name, width, height, title, desc, body, defs = "") {
  out <- file.path(svg_dir, name)
  if (name == "F2_user_input_to_consequence_sankey.svg" && !flow_ready) return(out)
  writeLines(svg_document(width, height, title, desc, body, defs), out, useBytes = TRUE)
  out
}

ink <- "#203746"
muted <- "#667985"
grid <- "#D8E0E5"
panel <- "#F6F8F9"
border <- "#8798A3"
pastel_palette <- c(purple = "#B69ACB", blue = "#A8C8E8", green = "#9DD3C7",
                    yellow = "#EAD18D", pink = "#E8A4AE")
actor_palette <- c(TECH = unname(pastel_palette["blue"]), DEV = unname(pastel_palette["yellow"]),
                   ADM = unname(pastel_palette["green"]), THIRD = unname(pastel_palette["purple"]),
                   USER = unname(pastel_palette["pink"]))
period_palette <- c(P1 = "#3B4CC0", P2 = "#9AB9F5", P3 = "#DDDCDC",
                    P4 = "#F3AD93", P5 = "#B40426")
score_palette <- c("0" = "#3B4CC0", "1" = "#9AB9F5",
                   "2" = "#F3AD93", "3" = "#B40426")

# ---------- Figure 1: clustered heatmap ----------

score_matrix <- as.matrix(scores[, c(actor_cols, basis_cols)])
rownames(score_matrix) <- scores$id
hc <- hclust(dist(score_matrix, method = "manhattan"), method = "average")
hc_col <- hclust(dist(t(score_matrix), method = "manhattan"), method = "average")
ord <- hc$order
col_ord <- hc_col$order
ordered <- scores[ord, , drop = FALSE]
ordered_matrix <- score_matrix[ord, col_ord, drop = FALSE]

cluster_table <- data.frame(order = seq_len(nrow(ordered)), id = ordered$id,
                            year = ordered$year, period = ordered$period,
                            scope = sample59$scope[match(ordered$id, sample59$id)],
                            strict = as.logical(ordered$strict), ordered_matrix,
                            check.names = FALSE)
write.csv(cluster_table, file.path(table_dir, "F1_clustered_policy_profiles.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

W <- 1840; H <- 1880
top <- 250; cell_h <- 24.5; heat_x <- 520; cell_w <- 100
heat_y <- top; heat_w <- ncol(ordered_matrix) * cell_w
rank_by_original <- integer(nrow(scores)); rank_by_original[ord] <- seq_along(ord)
leaf_y <- top + (seq_len(nrow(scores)) - 0.5) * cell_h
node_cache <- vector("list", nrow(hc$merge))

node_coord <- function(ref) {
  if (ref < 0) {
    idx <- -ref
    return(c(x = 390, y = leaf_y[rank_by_original[idx]], height = 0))
  }
  if (!is.null(node_cache[[ref]])) return(node_cache[[ref]])
  a <- node_coord(hc$merge[ref, 1]); b <- node_coord(hc$merge[ref, 2])
  x <- 390 - hc$height[ref] / max(hc$height) * 290
  node_cache[[ref]] <<- c(x = x, y = mean(c(a["y"], b["y"])), height = hc$height[ref])
  node_cache[[ref]]
}

body <- character()

for (i in seq_len(nrow(hc$merge))) {
  parent <- node_coord(i)
  a <- node_coord(hc$merge[i, 1]); b <- node_coord(hc$merge[i, 2])
  body <- c(body,
            line_svg(a["x"], a["y"], parent["x"], a["y"], "#7A8C97", 1),
            line_svg(b["x"], b["y"], parent["x"], b["y"], "#7A8C97", 1),
            line_svg(parent["x"], a["y"], parent["x"], b["y"], "#7A8C97", 1))
}

# Column dendrogram. The row and column trees now match the rectangular
# reference template while retaining the reviewed 0-3 values.
col_rank_by_original <- integer(ncol(score_matrix)); col_rank_by_original[col_ord] <- seq_along(col_ord)
col_leaf_x <- heat_x + (seq_len(ncol(score_matrix)) - 0.5) * cell_w
col_node_cache <- vector("list", nrow(hc_col$merge))
col_node_coord <- function(ref) {
  if (ref < 0) {
    idx <- -ref
    return(c(x = col_leaf_x[col_rank_by_original[idx]], y = 150, height = 0))
  }
  if (!is.null(col_node_cache[[ref]])) return(col_node_cache[[ref]])
  a <- col_node_coord(hc_col$merge[ref, 1]); b <- col_node_coord(hc_col$merge[ref, 2])
  y <- 150 - hc_col$height[ref] / max(hc_col$height) * 115
  col_node_cache[[ref]] <<- c(x = mean(c(a["x"], b["x"])), y = y, height = hc_col$height[ref])
  col_node_cache[[ref]]
}
for (i in seq_len(nrow(hc_col$merge))) {
  parent <- col_node_coord(i)
  a <- col_node_coord(hc_col$merge[i, 1]); b <- col_node_coord(hc_col$merge[i, 2])
  body <- c(body,
            line_svg(a["x"], a["y"], a["x"], parent["y"], "#7A8C97", 1),
            line_svg(b["x"], b["y"], b["x"], parent["y"], "#7A8C97", 1),
            line_svg(a["x"], parent["y"], b["x"], parent["y"], "#7A8C97", 1))
}

column_labels <- c(actor_labels, basis_labels)[col_ord]
for (j in seq_along(column_labels)) {
  x <- heat_x + (j - 0.5) * cell_w
  label_lines <- strsplit(column_labels[j], "\n", fixed = TRUE)[[1]]
  label_size <- if (max(nchar(label_lines)) > 12) 14 else 17
  body <- c(body, multiline_svg(x, 190, label_lines,
                                label_size, "bold", "middle", ink, 1.05))
}

for (i in seq_len(nrow(ordered_matrix))) {
  y <- heat_y + (i - 1) * cell_h
  body <- c(body, text_svg(500, y + 17, sprintf("%s  %d", ordered$id[i], ordered$year[i]),
                           23, anchor = "end", col = ink))
  for (j in seq_len(ncol(ordered_matrix))) {
    v <- ordered_matrix[i, j]
    fill <- score_palette[as.character(v)]
    body <- c(body, rect_svg(heat_x + (j - 1) * cell_w, y, cell_w, cell_h,
                              fill, "#FFFFFF", 0.7),
              text_svg(heat_x + (j - 0.5) * cell_w, y + 17, v, 22,
                       "bold", "middle", if (v %in% c(0, 3)) "#FFFFFF" else ink))
  }
  strip_x <- heat_x + heat_w + 20
  scope_col <- if (sample59$scope[match(ordered$id[i], sample59$id)] == "National") "#34495E" else "#D3A54C"
  body <- c(body,
            rect_svg(strip_x, y, 32, cell_h, period_palette[ordered$period[i]], "#FFFFFF", 0.5),
            rect_svg(strip_x + 38, y, 32, cell_h, scope_col, "#FFFFFF", 0.5),
            rect_svg(strip_x + 76, y, 32, cell_h,
                     if (as.logical(ordered$strict[i])) "#4B8B68" else "#FFFFFF",
                     "#4B8B68", 0.8))
}

strip_x <- heat_x + heat_w + 20
body <- c(body,
          text_svg(strip_x + 16, 230, "Period", 12, "bold", "middle", ink, rotate = -55),
          text_svg(strip_x + 54, 230, "Level", 12, "bold", "middle", ink, rotate = -55),
          text_svg(strip_x + 92, 230, "Strict", 12, "bold", "middle", ink, rotate = -55),
          text_svg(55, H - 105, "Ordinal strength", 14, "bold"))
for (v in 0:3) {
  x <- 185 + v * 105
  body <- c(body, rect_svg(x, H - 126, 36, 25, score_palette[as.character(v)], "#FFFFFF", 0.8),
            text_svg(x + 47, H - 107, as.character(v), 14))
}
body <- c(body,
          text_svg(610, H - 107, "Period strip", 14, "bold"))
for (p in seq_along(period_codes)) {
  x <- 710 + (p - 1) * 96
  body <- c(body, rect_svg(x, H - 126, 30, 25, period_palette[period_codes[p]], "#FFFFFF", 0.8),
            text_svg(x + 38, H - 107, paste0("P", p), 13))
}
body <- c(body,
          rect_svg(1250, H - 126, 30, 25, "#34495E"), text_svg(1288, H - 107, "National", 13),
          rect_svg(1375, H - 126, 30, 25, "#D3A54C"), text_svg(1413, H - 107, "Subnational", 13),
          text_svg(W - 55, H - 55, "Distance: Manhattan | Linkage: average", 12, anchor = "end", col = muted))

f1a <- write_svg("F1a_policy_document_cluster_heatmap_rectangular.svg", W, H,
                 "Rectangular two-way clustered heatmap",
                 "Rectangular heatmap with row and column dendrograms for 59 policy documents and ten reviewed dimensions.", body)

# Circular alternative using the same row clustering and the same reviewed
# values. The inner radial tree corresponds exactly to the rectangular row tree.
polar_xy <- function(cx, cy, radius, angle) {
  radius <- as.numeric(radius)
  angle <- as.numeric(angle)
  c(x = cx + radius * cos(angle), y = cy + radius * sin(angle))
}
annular_sector <- function(cx, cy, r_inner, r_outer, a1, a2) {
  p1 <- polar_xy(cx, cy, r_outer, a1); p2 <- polar_xy(cx, cy, r_outer, a2)
  p3 <- polar_xy(cx, cy, r_inner, a2); p4 <- polar_xy(cx, cy, r_inner, a1)
  large <- as.integer(abs(a2 - a1) > pi)
  sprintf("M %s %s A %s %s 0 %d 1 %s %s L %s %s A %s %s 0 %d 0 %s %s Z",
          f(p1["x"]), f(p1["y"]), f(r_outer), f(r_outer), large, f(p2["x"]), f(p2["y"]),
          f(p3["x"]), f(p3["y"]), f(r_inner), f(r_inner), large, f(p4["x"]), f(p4["y"]))
}
radial_arc <- function(cx, cy, radius, a1, a2) {
  if (a2 < a1) { z <- a1; a1 <- a2; a2 <- z }
  p1 <- polar_xy(cx, cy, radius, a1); p2 <- polar_xy(cx, cy, radius, a2)
  sprintf("M %s %s A %s %s 0 %d 1 %s %s",
          f(p1["x"]), f(p1["y"]), f(radius), f(radius), as.integer(a2 - a1 > pi),
          f(p2["x"]), f(p2["y"]))
}

Wc <- 1900; Hc <- 1800; cx <- 980; cy <- 900
circle_matrix <- score_matrix[ord, , drop = FALSE]
n_doc <- nrow(circle_matrix); n_track <- ncol(circle_matrix)
angle_edges <- seq(-60, 240, length.out = n_doc + 1) * pi / 180
doc_angles <- (angle_edges[-1] + angle_edges[-length(angle_edges)]) / 2
tree_leaf_r <- 315; tree_root_r <- 105
circ_node_cache <- vector("list", nrow(hc$merge))
circ_node_coord <- function(ref) {
  if (ref < 0) {
    idx <- -ref; theta <- doc_angles[rank_by_original[idx]]
    return(c(theta = theta, radius = tree_leaf_r))
  }
  if (!is.null(circ_node_cache[[ref]])) return(circ_node_cache[[ref]])
  a <- circ_node_coord(hc$merge[ref, 1]); b <- circ_node_coord(hc$merge[ref, 2])
  radius <- tree_leaf_r - hc$height[ref] / max(hc$height) * (tree_leaf_r - tree_root_r)
  circ_node_cache[[ref]] <<- c(theta = mean(c(a["theta"], b["theta"])), radius = radius)
  circ_node_cache[[ref]]
}

cbody <- character()
# Radial dendrogram.
for (i in seq_len(nrow(hc$merge))) {
  parent <- circ_node_coord(i)
  a <- circ_node_coord(hc$merge[i, 1]); b <- circ_node_coord(hc$merge[i, 2])
  ap1 <- polar_xy(cx, cy, a["radius"], a["theta"]); ap2 <- polar_xy(cx, cy, parent["radius"], a["theta"])
  bp1 <- polar_xy(cx, cy, b["radius"], b["theta"]); bp2 <- polar_xy(cx, cy, parent["radius"], b["theta"])
  cbody <- c(cbody,
             line_svg(ap1["x"], ap1["y"], ap2["x"], ap2["y"], "#40566B", 2.2),
             line_svg(bp1["x"], bp1["y"], bp2["x"], bp2["y"], "#40566B", 2.2),
             path_svg(radial_arc(cx, cy, parent["radius"], a["theta"], b["theta"]),
                       "none", "#40566B", 2.2))
}

# Ten annular tracks, inner to outer, with a small angular gutter between documents.
ring_inner <- 340; track_width <- 21
for (j in seq_len(n_track)) {
  r1 <- ring_inner + (j - 1) * track_width
  r2 <- r1 + track_width
  for (i in seq_len(n_doc)) {
    v <- circle_matrix[i, j]
    cbody <- c(cbody, path_svg(annular_sector(cx, cy, r1, r2,
                                              angle_edges[i] + 0.0025,
                                              angle_edges[i + 1] - 0.0025),
                                   score_palette[as.character(v)], "#FFFFFF", 0.42))
  }
}

# Document identifiers around the outer rim.
label_radius <- ring_inner + n_track * track_width + 22
for (i in seq_len(n_doc)) {
  theta <- doc_angles[i]; p <- polar_xy(cx, cy, label_radius, theta)
  deg <- theta * 180 / pi
  if (cos(theta) < 0) {
    rotation <- deg + 180; anchor <- "end"
  } else {
    rotation <- deg; anchor <- "start"
  }
  cbody <- c(cbody, text_svg(p["x"], p["y"], ordered$id[i], 9.5, "normal", anchor, ink,
                             rotate = rotation))
}

dimension_names <- c("Technical review bodies", "Project owners", "Administrative authorities",
                     "Third-party evaluators", "Users", "Technical compliance",
                     "Procedural documentation", "User experience",
                     "Responsibility traceability", "Public disclosure")
cbody <- c(cbody, text_svg(75, 405, "RINGS: INNER TO OUTER", 14, "bold"))
for (j in seq_along(dimension_names)) {
  y <- 438 + (j - 1) * 31
  cbody <- c(cbody, circle_svg(88, y - 5, 8, if (j <= 5) "#3B4CC0" else "#B40426"),
             text_svg(108, y, paste0(j, ". ", dimension_names[j]), 13))
}
cbody <- c(cbody, text_svg(1615, 650, "ORDINAL STRENGTH", 14, "bold"))
for (v in 0:3) {
  y <- 680 + v * 55
  cbody <- c(cbody, rect_svg(1615, y, 42, 34, score_palette[as.character(v)], "#FFFFFF", 0.8),
             text_svg(1672, y + 24, v, 14, "bold"))
}

f1b <- write_svg("F1b_policy_document_cluster_heatmap_circular.svg", Wc, Hc,
                 "Circular clustered heatmap",
                 "Circular heatmap of 59 policy documents with an inner radial dendrogram and ten concentric reviewed dimensions.", cbody)
unlink(file.path(svg_dir, "F1_policy_document_cluster_heatmap.svg"))
f1 <- c(f1a, f1b)

# ---------- Figure 2: user input to institutional consequence ----------

if (flow_ready) mechanisms <- mechanisms[mechanisms$eligibility_flag == 1L, , drop = FALSE]
input_codes <- c("CONS", "DES", "TRY", "EVAL", "SUP")
input_labels <- c("Planning or rule\nconsultation", "Design-stage\ninput",
                  "Trial use or\nexperience", "Assessment\ninput",
                  "Supervision or\ncomplaint")
outcome_codes <- c("SATGATE", "ADOPT", "LINK", "RESP", "NONE", "POWER")
outcome_labels <- c("Satisfaction as a\nqualification gate", "Adoption-status\nfeedback",
                    "Input linked to\nimprovement", "Response or\nfeedback",
                    "No specified\ndownstream\nprovision", "Public final\ndecision power")
status_codes <- c("QUALIFIED", "RESPONSIVE", "UNSPECIFIED", "DECISIVE")
status_labels <- c("Qualification-linked", "Responsive but\nnon-decisive",
                   "No downstream\nguarantee", "Final public\ndecision authority")
status_from_outcome <- c(SATGATE = "QUALIFIED", ADOPT = "RESPONSIVE",
                         LINK = "RESPONSIVE", RESP = "RESPONSIVE",
                         NONE = "UNSPECIFIED", POWER = "DECISIVE")

strongest_outcome <- function(row) {
  if (row[["POWER"]] == 1) return("POWER")
  if (row[["SATGATE"]] == 1) return("SATGATE")
  if (row[["ADOPT"]] == 1) return("ADOPT")
  if (row[["LINK"]] == 1) return("LINK")
  if (row[["RESP"]] == 1) return("RESP")
  "NONE"
}
doc_outcome <- apply(mechanisms, 1, strongest_outcome)
if (flow_ready) {
  document_outcomes <- mechanisms[,c("id", binary_fields),drop=FALSE]
  document_outcomes$strongest_downstream <- doc_outcome
  document_outcomes$entry_count <- rowSums(mechanisms[,input_codes])
  write.csv(document_outcomes,file.path(table_dir,"F2_final_document_assignments.csv"),row.names=FALSE)
  pair_records <- do.call(rbind,lapply(input_codes,function(ic){
    ids <- which(mechanisms[[ic]] == 1)
    data.frame(id=mechanisms$id[ids],input=ic,outcome=doc_outcome[ids],
               status=unname(status_from_outcome[doc_outcome[ids]]),stringsAsFactors=FALSE)
  }))
  write.csv(pair_records,file.path(table_dir,"F2_pair_records.csv"),row.names=FALSE)
}

flow_rows <- do.call(rbind, lapply(input_codes, function(ic) {
  ids <- which(mechanisms[[ic]] == 1)
  tab <- table(factor(doc_outcome[ids], levels = outcome_codes))
  data.frame(input = ic, outcome = outcome_codes, n = as.integer(tab), stringsAsFactors = FALSE)
}))
flow_rows$status <- unname(status_from_outcome[flow_rows$outcome])
flow_rows <- flow_rows[flow_rows$n > 0, , drop = FALSE]
write.csv(flow_rows, file.path(table_dir, "F2_user_input_consequence_flows.csv"), row.names = FALSE)

source_palette <- setNames(unname(pastel_palette), input_codes)
outcome_palette <- c(SATGATE = "#9B7DB5", ADOPT = "#7FA9D6", LINK = "#78B9AA",
                     RESP = "#D3B25A", NONE = "#D39A73", POWER = "#B40426")
status_palette <- c(QUALIFIED = "#8062A4", RESPONSIVE = "#5B9F92",
                    UNSPECIFIED = "#B87556", DECISIVE = "#B40426")

W <- 1820; H <- 1100
x1 <- 90; x2 <- 770; x3 <- 1450; node_w <- 280
plot_top <- 125; plot_bottom <- 900; gap1 <- 22; gap2 <- 17; gap3 <- 22
source_totals <- setNames(vapply(input_codes, function(x) sum(flow_rows$n[flow_rows$input == x]), numeric(1)), input_codes)
outcome_totals <- setNames(vapply(outcome_codes, function(x) sum(flow_rows$n[flow_rows$outcome == x]), numeric(1)), outcome_codes)
status_totals <- setNames(vapply(status_codes, function(x) sum(flow_rows$n[flow_rows$status == x]), numeric(1)), status_codes)
total_flow <- sum(source_totals)
if (flow_ready) {
  node_totals <- rbind(data.frame(column="Entry",code=names(source_totals),n=as.integer(source_totals)),
                       data.frame(column="Downstream",code=names(outcome_totals),n=as.integer(outcome_totals)),
                       data.frame(column="Status",code=names(status_totals),n=as.integer(status_totals)))
  node_totals$denominator <- total_flow
  write.csv(node_totals,file.path(table_dir,"F2_node_totals.csv"),row.names=FALSE)
  stopifnot(nrow(pair_records)==total_flow,
            sum(document_outcomes$entry_count)==total_flow,
            all(as.integer(table(factor(pair_records$input,levels=input_codes)))==source_totals))
}

stack_positions <- function(codes, totals, top, bottom, gap, min_height = 96) {
  positive <- codes[totals[codes] > 0]
  scale <- (bottom - top - gap * (length(positive) - 1) - min_height * length(positive)) / sum(totals[positive])
  y <- setNames(rep(NA_real_, length(codes)), codes)
  h <- setNames(rep(0, length(codes)), codes); cursor <- top
  for (code in positive) {
    y[code] <- cursor
    h[code] <- min_height + totals[code] * scale
    cursor <- cursor + h[code] + gap
  }
  list(y = y, h = h, scale = scale, min_height = min_height, positive = positive)
}
source_pos <- stack_positions(input_codes, source_totals, plot_top, plot_bottom, gap1)
outcome_pos <- stack_positions(outcome_codes, outcome_totals, plot_top, plot_bottom, gap2)
status_pos <- stack_positions(status_codes, status_totals, plot_top, plot_bottom, gap3)

# One shared pixels-per-pair scale prevents ribbons changing width between columns.
shared_scale <- min(source_pos$scale, outcome_pos$scale, status_pos$scale)
equal_scale_positions <- function(pos, codes, totals) {
  positive <- codes[totals[codes] > 0]
  heights <- pos$min_height + totals[positive] * shared_scale
  gap <- (plot_bottom - plot_top - sum(heights)) / (length(positive) - 1)
  cursor <- plot_top
  for (code in positive) {
    pos$y[code] <- cursor
    pos$h[code] <- heights[code]
    cursor <- cursor + heights[code] + gap
  }
  pos$scale <- shared_scale
  pos
}
source_pos <- equal_scale_positions(source_pos, input_codes, source_totals)
outcome_pos <- equal_scale_positions(outcome_pos, outcome_codes, outcome_totals)
status_pos <- equal_scale_positions(status_pos, status_codes, status_totals)
stopifnot(total_flow == sum(outcome_totals), total_flow == sum(status_totals),
          source_pos$scale == outcome_pos$scale, outcome_pos$scale == status_pos$scale)
write.csv(data.frame(column=c("entry","provision","status"),
  pixels_per_pair=rep(shared_scale,3), total_pairs=rep(total_flow,3),
  unique_documents=rep(length(unique(mechanisms$id[rowSums(mechanisms[,input_codes])>0])),3)),
  file.path(table_dir,"F2_flow_scale_validation.csv"),row.names=FALSE)

band_path <- function(xa, ya1, ya2, xb, yb1, yb2) {
  c1 <- xa + (xb - xa) * 0.42; c2 <- xa + (xb - xa) * 0.58
  sprintf("M %s %s C %s %s %s %s %s %s L %s %s C %s %s %s %s %s %s Z",
          f(xa), f(ya1), f(c1), f(ya1), f(c2), f(yb1), f(xb), f(yb1),
          f(xb), f(yb2), f(c2), f(yb2), f(c1), f(ya2), f(xa), f(ya2))
}

# Column headers and pale directional arrows follow the supplied cluster-mapping
# template. The longer middle heading receives a wider band and generous padding.
header_y <- 22; header_h <- 56
header_side_w <- 300; header_mid_w <- 420
header_x1 <- x1 + (node_w - header_side_w) / 2
header_x2 <- x2 + (node_w - header_mid_w) / 2
header_x3 <- x3 + (node_w - header_side_w) / 2
body <- c(rect_svg(header_x1, header_y, header_side_w, header_h, "#F4E7EE", "#E8D8E1", 0.8, rx = 4),
          rect_svg(header_x2, header_y, header_mid_w, header_h, "#E5EEF8", "#D3E0EF", 0.8, rx = 4),
          rect_svg(header_x3, header_y, header_side_w, header_h, "#E7F2EA", "#D5E6DA", 0.8, rx = 4),
          text_svg(x1 + node_w / 2, 60, "ENTRY MECHANISM", 22, "bold", "middle"),
          text_svg(x2 + node_w / 2, 60, "STRONGEST PROVISION", 22, "bold", "middle"),
          text_svg(x3 + node_w / 2, 60, "DECISION STATUS", 22, "bold", "middle"),
          path_svg("M 405 50 L 675 50 L 659 38 M 675 50 L 659 62", "none", "#D5DDE7", 3.2),
          path_svg("M 1145 50 L 1435 50 L 1419 38 M 1435 50 L 1419 62", "none", "#D5DDE7", 3.2))

# First-stage flows: entry mechanism to strongest downstream provision.
source_offset <- setNames(rep(0, length(input_codes)), input_codes)
outcome_in_offset <- setNames(rep(0, length(outcome_codes)), outcome_codes)
for (r in seq_len(nrow(flow_rows))) {
  ic <- flow_rows$input[r]; oc <- flow_rows$outcome[r]; n <- flow_rows$n[r]
  y1a <- source_pos$y[ic] + source_pos$min_height / 2 + source_offset[ic] * source_pos$scale
  y1b <- y1a + n * source_pos$scale
  y2a <- outcome_pos$y[oc] + outcome_pos$min_height / 2 + outcome_in_offset[oc] * outcome_pos$scale
  y2b <- y2a + n * outcome_pos$scale
  body <- c(body, path_svg(band_path(x1 + node_w, y1a, y1b, x2, y2a, y2b),
                            source_palette[ic], "none", opacity = 0.34))
  source_offset[ic] <- source_offset[ic] + n
  outcome_in_offset[oc] <- outcome_in_offset[oc] + n
}

# Second-stage flows aggregate each observed provision into its institutional status.
outcome_out_offset <- setNames(rep(0, length(outcome_codes)), outcome_codes)
status_offset <- setNames(rep(0, length(status_codes)), status_codes)
for (oc in outcome_codes[outcome_totals[outcome_codes] > 0]) {
  st <- status_from_outcome[oc]; n <- outcome_totals[oc]
  y2a <- outcome_pos$y[oc] + outcome_pos$min_height / 2 + outcome_out_offset[oc] * outcome_pos$scale
  y2b <- y2a + n * outcome_pos$scale
  y3a <- status_pos$y[st] + status_pos$min_height / 2 + status_offset[st] * status_pos$scale
  y3b <- y3a + n * status_pos$scale
  body <- c(body, path_svg(band_path(x2 + node_w, y2a, y2b, x3, y3a, y3b),
                            outcome_palette[oc], "none", opacity = 0.32))
  outcome_out_offset[oc] <- outcome_out_offset[oc] + n
  status_offset[st] <- status_offset[st] + n
}

# Nodes and labels are drawn last so text remains legible above the bands.
# Centre each multiline label from its actual line count. This keeps enlarged
# labels inside their nodes instead of relying on a fixed vertical offset.
centered_multiline_svg <- function(x, y, h, labels, size, weight = "bold",
                                   anchor = "middle", col = ink,
                                   leading = 1.70) {
  labels <- as.character(labels)
  line_step <- size * max(leading, 1.70)
  first_baseline <- y + h / 2 - line_step * (length(labels) - 1) / 2 + size * 0.32
  multiline_svg(x, first_baseline, labels, size, weight, anchor, col, leading)
}

for (k in seq_along(input_codes)) {
  code <- input_codes[k]; y <- source_pos$y[code]; h <- source_pos$h[code]
  lines <- c(strsplit(input_labels[k], "\n", fixed = TRUE)[[1]], paste0("documents = ", source_totals[code]))
  body <- c(body, rect_svg(x1, y, node_w, h, source_palette[code], source_palette[code], 1.1, 4, 0.72),
            centered_multiline_svg(x1 + node_w / 2, y, h, lines, 20))
}
for (k in seq_along(outcome_codes)) {
  code <- outcome_codes[k]
  if (outcome_totals[code] > 0) {
    y <- outcome_pos$y[code]; h <- outcome_pos$h[code]
    lines <- c(strsplit(outcome_labels[k], "\n", fixed = TRUE)[[1]], paste0("pairs = ", outcome_totals[code]))
    body <- c(body, rect_svg(x2, y, node_w, h, outcome_palette[code], outcome_palette[code], 1.1, 4, 0.62),
              centered_multiline_svg(x2 + node_w / 2, y, h, lines, 19))
  }
}
for (k in seq_along(status_codes)) {
  code <- status_codes[k]
  if (status_totals[code] > 0) {
    y <- status_pos$y[code]; h <- status_pos$h[code]
    lines <- c(strsplit(status_labels[k], "\n", fixed = TRUE)[[1]], paste0("pairs = ", status_totals[code]))
    body <- c(body, rect_svg(x3, y, node_w, h, status_palette[code], status_palette[code], 1.1, 4, 0.62),
              centered_multiline_svg(x3 + node_w / 2, y, h, lines, 20))
  }
}

# Explicit zero nodes preserve the central substantive result without a separate legend.
zero_y <- 940
zero_h <- 100
body <- c(body,
          rect_svg(x2, zero_y, node_w, zero_h, "#FFFFFF", outcome_palette["POWER"], 1.4, 4),
          centered_multiline_svg(x2 + node_w / 2, zero_y, zero_h,
                                 c("Public final", "decision power", "n = 0"), 17.5),
          rect_svg(x3, zero_y, node_w, zero_h, "#FFFFFF", status_palette["DECISIVE"], 1.4, 4),
          centered_multiline_svg(x3 + node_w / 2, zero_y, zero_h,
                                 c("Final public", "decision authority", "n = 0"), 17.5))

f2 <- write_svg("F2_user_input_to_consequence_sankey.svg", W, H,
                "From user input to institutional consequence",
                "Three-column Sankey diagram of document-level user entry mechanisms, strongest downstream provisions and institutional decision status.", body)

# ---------- Figure 3: actor-mechanism UpSet ----------

actor_binary <- scores[, actor_cols, drop = FALSE] >= 2
pattern <- apply(actor_binary, 1, function(z) paste(as.integer(z), collapse = ""))
pattern_levels <- names(sort(table(pattern), decreasing = TRUE))
combo_rows <- do.call(rbind, lapply(seq_along(pattern_levels), function(i) {
  pat <- pattern_levels[i]
  idx <- which(pattern == pat)
  row <- data.frame(rank = i, pattern = pat, n = length(idx), stringsAsFactors = FALSE)
  for (p in period_codes) row[[p]] <- sum(scores$period[idx] == p)
  for (j in seq_along(actor_codes)) row[[actor_codes[j]]] <- as.integer(substr(pat, j, j))
  row
}))
write.csv(combo_rows, file.path(table_dir, "F3_actor_mechanism_combinations.csv"), row.names = FALSE)

upset_period_palette <- c(P1 = "#FFFFD8", P2 = "#99D6B8", P3 = "#59B3B8",
                          P4 = "#227FB8", P5 = "#071D58")
upset_actor_palette <- c(TECH = "#071D58", DEV = "#227FB8", ADM = "#59B3B8",
                         THIRD = "#99D6B8", USER = "#FFFFD8")
W <- 1740; H <- 900
left <- 285; right <- 70; top <- 35; bar_bottom <- 435
ncombo <- nrow(combo_rows); col_w <- (W - left - right) / ncombo
max_n <- max(combo_rows$n)
body <- character()

for (g in pretty(c(0, max_n), n = 5)) {
  y <- bar_bottom - g / max_n * (bar_bottom - top)
  body <- c(body, line_svg(left, y, W - right, y, grid, 1),
            text_svg(left - 14, y + 5, g, 12, anchor = "end", col = muted))
}

for (i in seq_len(ncombo)) {
  x <- left + (i - 1) * col_w + col_w * 0.16
  bw <- col_w * 0.68
  cursor_y <- bar_bottom
  for (p in period_codes) {
    count <- combo_rows[[p]][i]
    if (count > 0) {
      hh <- count / max_n * (bar_bottom - top)
      body <- c(body, rect_svg(x, cursor_y - hh, bw, hh, upset_period_palette[p], "#FFFFFF", 0.6))
      cursor_y <- cursor_y - hh
    }
  }
  body <- c(body, text_svg(x + bw / 2, cursor_y - 9, combo_rows$n[i], 13, "bold", "middle"))
}

matrix_top <- 535; row_gap <- 58
for (j in seq_along(actor_codes)) {
  y <- matrix_top + (j - 1) * row_gap
  body <- c(body, multiline_svg(left - 24, y - 7,
                                strsplit(actor_labels[j], "\n", fixed = TRUE)[[1]],
                                14, "bold", "end", ink, 1.0),
            line_svg(left, y, W - right, y, "#E7ECEF", 1))
}
for (i in seq_len(ncombo)) {
  cx <- left + (i - 0.5) * col_w
  active <- which(as.integer(strsplit(combo_rows$pattern[i], "")[[1]]) == 1)
  if (length(active) > 1) {
    body <- c(body, line_svg(cx, matrix_top + (min(active) - 1) * row_gap,
                            cx, matrix_top + (max(active) - 1) * row_gap, ink, 3))
  }
  for (j in seq_along(actor_codes)) {
    active_j <- combo_rows[[actor_codes[j]]][i] == 1
    dot_fill <- if (active_j) upset_actor_palette[actor_codes[j]] else "#FFFFFF"
    dot_stroke <- if (active_j && actor_codes[j] == "USER") "#B8B86C" else if (active_j) upset_actor_palette[actor_codes[j]] else "#C8D1D6"
    body <- c(body, circle_svg(cx, matrix_top + (j - 1) * row_gap, 9,
                              dot_fill, dot_stroke, 1.4))
  }
}
body <- c(body, text_svg(95, 235, "Documents", 15, "bold", "middle", ink, rotate = -90),
          text_svg(left, 490, "Actor combinations", 15, "bold"))
for (p in seq_along(period_codes)) {
  x <- 875 + (p - 1) * 158
  body <- c(body, rect_svg(x, H - 45, 24, 18, upset_period_palette[period_codes[p]], "#9BA8AE", 0.5),
            text_svg(x + 32, H - 30, period_labels[p], 12))
}

f3 <- write_svg("F3_actor_mechanism_upset.svg", W, H,
                "Combinations of procedural authority across policy documents",
                "UpSet plot of combinations of five actor groups with ordinal authority scores of two or more.", body)

# ---------- Figure 4: five-period subnational diffusion maps ----------

for (v in c("lon", "lat", "polygon_id", "ring_id", "point_id")) china[[v]] <- as.numeric(china[[v]])
china$code <- as.character(china$code)
china$feature_id <- as.character(china$feature_id)
province_labels$code <- as.character(province_labels$code)
sample59$province_code <- as.character(sample59$province_code)
sample59$period <- period_from_year(sample59$year)
local_docs <- sample59[sample59$scope == "Subnational", , drop = FALSE]

map_counts <- do.call(rbind, lapply(period_codes, function(p) {
  tab <- table(local_docs$province_code[local_docs$period == p])
  data.frame(period = p, code = province_labels$code,
             n = as.integer(tab[match(province_labels$code, names(tab))]),
             stringsAsFactors = FALSE)
}))
map_counts$n[is.na(map_counts$n)] <- 0L
write.csv(map_counts, file.path(table_dir, "F4_period_province_document_counts.csv"), row.names = FALSE)

R_earth <- 6371007.18
albers <- function(lon, lat) {
  r <- pi / 180
  n <- (sin(25 * r) + sin(47 * r)) / 2
  C <- cos(25 * r)^2 + 2 * n * sin(25 * r)
  rho <- R_earth * sqrt(C - 2 * n * sin(lat * r)) / n
  rho0 <- R_earth * sqrt(C - 2 * n * sin(35 * r)) / n
  theta <- n * (lon - 105) * r
  cbind(x = rho * sin(theta), y = rho0 - rho * cos(theta))
}
make_view <- function(bounds, box, pad = 0.035) {
  xy <- albers(bounds[, 1], bounds[, 2])
  xr <- range(xy[, 1]); yr <- range(xy[, 2])
  xr <- xr + c(-1, 1) * diff(xr) * pad
  yr <- yr + c(-1, 1) * diff(yr) * pad
  sc <- min(box[3] / diff(xr), box[4] / diff(yr))
  cx <- mean(xr); cy <- mean(yr)
  transform <- function(lon, lat) {
    q <- albers(lon, lat)
    cbind(x = box[1] + box[3] / 2 + (q[, 1] - cx) * sc,
          y = box[2] + box[4] / 2 - (q[, 2] - cy) * sc)
  }
  transform
}

ring_key <- function(d) paste(d$polygon_id, d$ring_id, sep = "_")
simplify_idx <- function(idx, step = 4L) unique(c(idx[seq(1, length(idx), by = step)], tail(idx, 1)))
shape_path <- function(d, transform, closed = TRUE) {
  groups <- split(seq_len(nrow(d)), ring_key(d))
  paste(vapply(groups, function(idx) {
    idx <- simplify_idx(idx)
    q <- transform(d$lon[idx], d$lat[idx])
    paste0("M ", paste(paste(f(q[, 1]), f(q[, 2])), collapse = " L "), if (closed) " Z" else "")
  }, ""), collapse = " ")
}
china_shapes <- split(china, china$feature_id)
map_palette <- c("0" = "#EEEDEA", "1" = "#C4DDEC", "2" = "#78AECB",
                 "3" = "#447C9F", "4" = "#244E6B")
map_points <- china[china$geometry != "MultiLineString" & china$lat >= 17.5, c("lon", "lat")]

draw_map_panel <- function(period, box) {
  tr <- make_view(as.matrix(map_points), box, pad = 0.025)
  parts <- character()
  for (d in china_shapes) {
    d <- d[d$lat >= 17.5, , drop = FALSE]
    if (nrow(d) < 2) next
    is_line <- d$geometry[1] == "MultiLineString"
    code <- d$code[1]
    n <- map_counts$n[map_counts$period == period & map_counts$code == code]
    if (!length(n)) n <- 0
    n <- min(4, n)
    fill <- if (is_line) "none" else map_palette[as.character(n)]
    stroke <- if (is_line) "#8EA0AA" else "#7D8E98"
    parts <- c(parts, path_svg(shape_path(d, tr, !is_line), fill, stroke,
                               if (is_line) 0.55 else 0.65))
  }
  parts
}

draw_scs_inset <- function(box) {
  bounds <- as.matrix(expand.grid(lon = seq(105, 125, length.out = 40),
                                  lat = seq(3, 24.5, length.out = 40)))
  tr <- make_view(bounds, box, pad = 0)
  parts <- character()
  for (d in china_shapes) {
    if (max(d$lat, na.rm = TRUE) > 25 || min(d$lon, na.rm = TRUE) < 105 ||
        max(d$lon, na.rm = TRUE) > 125) next
    is_line <- d$geometry[1] == "MultiLineString"
    parts <- c(parts, path_svg(shape_path(d, tr, !is_line),
                               if (is_line) "none" else "#E7EDF0",
                               "#8798A3", if (is_line) 0.55 else 0.65))
  }
  parts
}

W <- 1840; H <- 1160
boxes <- list(c(70, 170, 540, 350), c(650, 170, 540, 350), c(1230, 170, 540, 350),
              c(360, 630, 540, 350), c(940, 630, 540, 350))
body <- c(text_svg(55, 58, "Temporal diffusion of subnational accessibility policies", 30, "bold"),
          text_svg(55, 90, "Period-specific distribution of 35 retained subnational documents", 16, col = muted),
          line_svg(55, 112, W - 55, 112, grid, 1))
for (i in seq_along(period_codes)) {
  b <- boxes[[i]]
  body <- c(body, rect_svg(b[1] - 12, b[2] - 38, b[3] + 24, b[4] + 56, "#FBFCFC", grid, 0.8, 4),
            text_svg(b[1], b[2] - 12, period_labels[i], 18, "bold"),
            text_svg(b[1] + b[3], b[2] - 12,
                     paste0("n = ", sum(local_docs$period == period_codes[i])),
                     14, "bold", "end", muted),
            draw_map_panel(period_codes[i], b))
}
body <- c(body, text_svg(1515, 685, "Documents in period", 14, "bold"))
for (n in 0:4) {
  y <- 710 + n * 38
  body <- c(body, rect_svg(1515, y, 34, 24, map_palette[as.character(n)], border, 0.6),
            text_svg(1562, y + 18, if (n < 4) n else "4 or more", 13))
}
body <- c(body,
          text_svg(1515, 925, "South China Sea", 13, "bold"),
          rect_svg(1510, 940, 255, 165, "#FBFCFC", grid, 0.8),
          draw_scs_inset(c(1515, 945, 245, 155)),
          text_svg(W - 55, H - 24, "Counts describe the retained corpus, not implementation intensity.",
                         12.5, anchor = "end", col = muted))

f4 <- write_svg("F4_temporal_policy_diffusion_maps.svg", W, H,
                "Temporal diffusion of subnational accessibility policies",
                "Five small-multiple maps show the period-specific distribution of 35 subnational policy documents across China.", body)

# Replace the compact small-multiple with five separate full cartographic plates.
# Each plate follows the same layout as Figure 6: locator, province list, main
# map, scale bar, north arrow, South China Sea inset and count legend.
unlink(f4)
land$lon <- as.numeric(land$lon); land$lat <- as.numeric(land$lat)
land$feature_id <- as.character(land$feature_id)
land_shapes <- split(land, land$feature_id)

locator_transform <- function(lon, lat) {
  cbind(x = 220 + (lon - 105) * cos(35 * pi / 180) * 3.4,
        y = 205 - (lat - 35) * 3.4)
}

polygon_area <- function(x, y) {
  j <- c(2:length(x), 1)
  abs(sum(x * y[j] - x[j] * y)) / 2
}
points_in_polygon <- function(px, py, x, y) {
  inside <- rep(FALSE, length(px)); j <- length(x)
  for (i in seq_along(x)) {
    crosses <- ((y[i] > py) != (y[j] > py)) &
      (px < (x[j] - x[i]) * (py - y[i]) / (y[j] - y[i] + 1e-12) + x[i])
    inside <- xor(inside, crosses); j <- i
  }
  inside
}
distance_to_boundary <- function(px, py, x, y) {
  best <- rep(Inf, length(px)); j <- length(x)
  for (i in seq_along(x)) {
    vx <- x[i] - x[j]; vy <- y[i] - y[j]; denom <- vx * vx + vy * vy
    t <- if (denom > 0) ((px - x[j]) * vx + (py - y[j]) * vy) / denom else 0
    t <- pmax(0, pmin(1, t))
    dx <- px - (x[j] + t * vx); dy <- py - (y[j] + t * vy)
    best <- pmin(best, sqrt(dx * dx + dy * dy)); j <- i
  }
  best
}
interior_label_point <- function(code, transform) {
  d <- china[china$code == code & china$geometry != "MultiLineString", ]
  rings <- split(seq_len(nrow(d)), ring_key(d))
  projected <- lapply(rings, function(idx) transform(d$lon[idx], d$lat[idx]))
  areas <- vapply(projected, function(q) polygon_area(q[, 1], q[, 2]), numeric(1))
  poly <- projected[[which.max(areas)]]
  keep <- unique(round(seq(1, nrow(poly), length.out = min(320, nrow(poly)))))
  poly <- poly[keep, , drop = FALSE]
  if (any(poly[1, ] != poly[nrow(poly), ])) poly <- rbind(poly, poly[1, ])
  x <- poly[, 1]; y <- poly[, 2]; search_box <- c(range(x), range(y)); best <- c(mean(x), mean(y))
  for (iteration in seq_len(4)) {
    gx <- seq(search_box[1], search_box[2], length.out = if (iteration == 1) 41 else 23)
    gy <- seq(search_box[3], search_box[4], length.out = if (iteration == 1) 41 else 23)
    cand <- expand.grid(x = gx, y = gy)
    cand <- cand[points_in_polygon(cand$x, cand$y, x, y), , drop = FALSE]
    if (nrow(cand)) best <- unlist(cand[which.max(distance_to_boundary(cand$x, cand$y, x, y)), ], use.names = FALSE)
    hx <- diff(range(gx)) / 8; hy <- diff(range(gy)) / 8
    search_box <- c(best[1] - hx, best[1] + hx, best[2] - hy, best[2] + hy)
  }
  c(x = best[1], y = best[2])
}

map_palette <- c("0" = "#ECECE8", "1" = "#D5E5EF", "2" = "#9FC3D8",
                 "3" = "#5B8DB0", "4" = "#2F5D7C")
main_box <- c(455, 70, 1130, 690)
main_transform <- make_view(as.matrix(map_points), main_box, pad = 0.018)
scs_bounds <- as.matrix(expand.grid(lon = seq(105, 125, length.out = 40),
                                    lat = seq(3, 24.5, length.out = 40)))
scs_transform <- make_view(scs_bounds, c(1390, 590, 190, 205), pad = 0)
main_shape_paths <- lapply(china_shapes, function(d) shape_path(d, main_transform, d$geometry[1] != "MultiLineString"))
scs_shape_paths <- lapply(china_shapes, function(d) shape_path(d, scs_transform, d$geometry[1] != "MultiLineString"))
province_xy <- main_transform(as.numeric(province_labels$label_lon), as.numeric(province_labels$label_lat))
rownames(province_xy) <- province_labels$code
map_scale_half <- 500000 * (1130 / diff(range(albers(map_points$lon, map_points$lat)[, 1])))

draw_land_locator <- function() {
  unlist(lapply(land_shapes, function(d) {
    path_svg(shape_path(d, locator_transform), "#F0EED7", "#A5ACA5", 0.55)
  }), use.names = FALSE)
}
draw_china_locator <- function() {
  unlist(lapply(china_shapes, function(d) {
    if (d$geometry[1] == "MultiLineString") return(character())
    path_svg(shape_path(d, locator_transform), "#DED78E", "#B7B47D", 0.35)
  }), use.names = FALSE)
}
draw_period_china <- function(period, inset = FALSE) {
  paths <- if (inset) scs_shape_paths else main_shape_paths
  unlist(lapply(seq_along(china_shapes), function(i) {
    d <- china_shapes[[i]]
    is_line <- d$geometry[1] == "MultiLineString"
    code <- d$code[1]
    n <- map_counts$n[map_counts$period == period & map_counts$code == code]
    if (!length(n)) n <- 0
    fill <- if (is_line) "none" else map_palette[as.character(min(4, n))]
    path_svg(paths[[i]], fill, if (inset) "#8798A3" else "#73828B",
             if (is_line) 0.5 else if (inset) 0.55 else 0.75)
  }), use.names = FALSE)
}

period_names <- c("F4a_policy_diffusion_2011_2013.svg",
                  "F4b_policy_diffusion_2014_2016.svg",
                  "F4c_policy_diffusion_2017_2019.svg",
                  "F4d_policy_diffusion_2020_2022.svg",
                  "F4e_policy_diffusion_2023_2026.svg")
f4 <- character(length(period_codes))
map_label_positions <- list()
for (period_i in seq_along(period_codes)) {
  period <- period_codes[period_i]; counts <- map_counts[map_counts$period == period, ]
  labels_period <- merge(province_labels, counts, by = "code", all.x = TRUE, sort = FALSE)
  labels_period$n[is.na(labels_period$n)] <- 0L
  defs_map <- paste0('<clipPath id="locatorClip"><circle cx="220" cy="205" r="135"/></clipPath>',
                     '<clipPath id="mainClip"><rect x="455" y="70" width="1130" height="690"/></clipPath>',
                     '<clipPath id="scsClip"><rect x="1390" y="590" width="190" height="205"/></clipPath>')
  map_body <- c(rect_svg(20, 20, 1620, 875, "none", "#6E777C", 1.3),
                text_svg(220, 48, "LOCATION", 16, "bold", "middle"),
                '<g clip-path="url(#locatorClip)">',
                circle_svg(220, 205, 135, "#CDE6F4"), draw_land_locator(), draw_china_locator(),
                text_svg(220, 207, "China", 18, "bold", "middle"),
                multiline_svg(317, 235, c("Pacific", "Ocean"), 12, "normal", "middle", "#678DA3", 1.05, TRUE),
                '</g>', circle_svg(220, 205, 135, "none", "#8799A3", 1.2),
                line_svg(350, 165, 455, 120, "#A7AFB3", 1, "3 5"),
                line_svg(350, 255, 455, 700, "#A7AFB3", 1, "3 5"),
                '<g clip-path="url(#mainClip)">', draw_period_china(period), '</g>')

  # Place count labels inside provincial visual centres; compact municipalities use callouts.
  for (i in which(labels_period$n > 0 & !labels_period$code %in% c("110000", "120000", "310000"))) {
    label_code <- labels_period$code[i]
    q <- interior_label_point(label_code, main_transform)
    label_polygon <- china[china$code == label_code & china$geometry != "MultiLineString",,drop=FALSE]
    label_rings <- split(seq_len(nrow(label_polygon)),ring_key(label_polygon))
    inside_own_province <- any(vapply(label_rings,function(idx) {
      poly <- main_transform(label_polygon$lon[idx],label_polygon$lat[idx])
      points_in_polygon(q["x"],q["y"],poly[,1],poly[,2])
    },logical(1)))
    stopifnot(inside_own_province)
    map_label_positions[[length(map_label_positions)+1L]] <- data.frame(
      period=period,code=label_code,x=q["x"],y=q["y"],inside_own_province=inside_own_province)

    map_body <- c(map_body, text_svg(q["x"], q["y"] + 5, labels_period$n[i], 18, "bold", "middle",
                                     if (labels_period$n[i] >= 3) "#FFFFFF" else ink))
  }
  callouts <- list(list(code = "110000", dx = 155, dy = -20),
                   list(code = "120000", dx = 75, dy = 5),
                   list(code = "310000", dx = 65, dy = 13))
  for (a in callouts) {
    p <- labels_period[labels_period$code == a$code & labels_period$n > 0, ]
    if (!nrow(p)) next
    q <- province_xy[p$code, ]
    tx <- q["x"] + a$dx; ty <- q["y"] + a$dy
    map_body <- c(map_body, line_svg(q["x"], q["y"], tx - 5, ty - 6, border, 0.9),
                  text_svg(tx, ty, sprintf("%s (%d)", p$name_en, p$n), 14, "bold"))
  }

  # North arrow, scale bar and South China Sea inset.
  half <- map_scale_half
  map_body <- c(map_body,
                text_svg(1545, 92, "N", 18, "bold", "middle"),
                '<path d="M 1545 103 L 1533 139 L 1545 131 L 1557 139 Z" fill="#FFFFFF" stroke="#263B49" stroke-width="1.4"/>',
                '<path d="M 1545 103 L 1545 131 L 1533 139 Z" fill="#263B49"/>',
                rect_svg(1390, 590, 190, 205, "#FFFFFF"),
                '<g clip-path="url(#scsClip)">', draw_period_china(period, TRUE), '</g>',
                rect_svg(1390, 590, 190, 205, "none", border, 1),
                text_svg(1485, 817, "South China Sea", 13, anchor = "middle", col = muted),
                rect_svg(515, 800, half, 8, ink, ink, 0.8),
                rect_svg(515 + half, 800, half, 8, "#FFFFFF", ink, 0.8),
                text_svg(515, 787, "0", 13, anchor = "middle"),
                text_svg(515 + half, 787, "500", 13, anchor = "middle"),
                text_svg(515 + 2 * half, 787, "1,000 km", 13, anchor = "middle"))

  # Period-specific province list.
  map_body <- c(map_body, text_svg(50, 395, "DOCUMENTS BY PROVINCE-LEVEL UNIT", 15, "bold"),
                line_svg(50, 407, 420, 407, "#BFC7CC", 0.8))
  listed <- labels_period[labels_period$n > 0, ]
  for (i in seq_len(nrow(listed))) {
    column <- (i - 1) %/% 11; row <- (i - 1) %% 11
    x <- 50 + column * 190; y <- 433 + row * 24
    map_body <- c(map_body, text_svg(x, y, listed$name_en[i], 14),
                  text_svg(x + 165, y, listed$n[i], 14, "bold", "end"))
  }

  # The period is embedded in the legend heading, not as a figure title.
  map_body <- c(map_body,
                text_svg(515, 850, paste0("Included subnational documents | ", period_labels[period_i]), 17, "bold"))
  for (n in 1:4) {
    x <- 515 + (n - 1) * 95
    map_body <- c(map_body, rect_svg(x, 864, 40, 22, map_palette[as.character(n)], border, 0.7),
                  text_svg(x + 50, 881, n, 15))
  }
  map_body <- c(map_body, rect_svg(940, 864, 40, 22, map_palette["0"], border, 0.7),
                text_svg(990, 881, "No included document", 15))

  f4[period_i] <- write_svg(period_names[period_i], 1660, 915,
                      paste("Subnational policy distribution", period_labels[period_i]),
                      paste("Period-specific distribution of retained subnational policy documents in", period_labels[period_i]),
                      map_body, defs_map)
}

write.csv(do.call(rbind,map_label_positions),
  file.path(table_dir,"F4_numeric_label_positions.csv"),row.names=FALSE)

# ---------- Figure 5: actor-basis co-occurrence network ----------

edge_rows <- do.call(rbind, lapply(seq_along(actor_codes), function(i) {
  do.call(rbind, lapply(seq_along(basis_codes), function(j) {
    data.frame(actor = actor_codes[i], basis = basis_codes[j],
               n = sum(scores[[actor_cols[i]]] >= 2 & scores[[basis_cols[j]]] >= 2),
               stringsAsFactors = FALSE)
  }))
}))
write.csv(edge_rows, file.path(table_dir, "F5_actor_basis_cooccurrence_edges.csv"), row.names = FALSE)

W <- 1720; H <- 850
left_x <- 330; right_x <- 1390
actor_y <- setNames(seq(115, 655, length.out = 5), actor_codes)
basis_y <- setNames(seq(115, 655, length.out = 5), basis_codes)
basis_palette <- c(TECHNICAL = unname(pastel_palette["blue"]), DOCUMENTS = unname(pastel_palette["purple"]),
                   EXPERIENCE = unname(pastel_palette["pink"]), TRACE = unname(pastel_palette["yellow"]),
                   PUBLIC = unname(pastel_palette["green"]))
max_edge <- max(edge_rows$n)
body <- c(rect_svg(left_x - 160, 18, 320, 43, "#E5EEF8", "none", rx = 2),
          rect_svg(right_x - 160, 18, 320, 43, "#E7F2EA", "none", rx = 2),
          text_svg(left_x, 47, "DECISION ACTORS", 14, "bold", "middle", ink),
          text_svg(right_x, 47, "JUDGMENT BASES", 14, "bold", "middle", ink))

for (r in seq_len(nrow(edge_rows))) {
  if (edge_rows$n[r] == 0) next
  a <- edge_rows$actor[r]; b <- edge_rows$basis[r]
  y1 <- actor_y[a]; y2 <- basis_y[b]
  sw <- 0.8 + 12 * edge_rows$n[r] / max_edge
  d <- sprintf("M %s %s C %s %s %s %s %s %s",
               f(left_x + 140), f(y1), f(left_x + 450), f(y1),
               f(right_x - 450), f(y2), f(right_x - 140), f(y2))
  body <- c(body, path_svg(d, "none", actor_palette[a], sw, opacity = 0.32))
}

for (i in seq_along(actor_codes)) {
  code <- actor_codes[i]; y <- actor_y[code]
  body <- c(body, rect_svg(left_x - 140, y - 36, 280, 72, actor_palette[code], actor_palette[code], 1.3, 4, 0.72),
            multiline_svg(left_x, y - 8, strsplit(actor_labels[i], "\n", fixed = TRUE)[[1]],
                          14.5, "bold", "middle", ink, 1.22))
}
for (j in seq_along(basis_codes)) {
  code <- basis_codes[j]; y <- basis_y[code]
  body <- c(body, rect_svg(right_x - 140, y - 36, 280, 72, basis_palette[code], basis_palette[code], 1.3, 4, 0.72),
            multiline_svg(right_x, y - 8, strsplit(basis_labels[j], "\n", fixed = TRUE)[[1]],
                          14.5, "bold", "middle", ink, 1.22))
}

body <- c(body,
          text_svg(610, 790, "Edge width", 13, "bold"),
          line_svg(715, 785, 810, 785, "#8E99A1", 0.8 + 12 * 10 / max_edge, opacity = 0.55),
          text_svg(825, 790, "10", 12),
          line_svg(875, 785, 970, 785, "#8E99A1", 0.8 + 12 * 30 / max_edge, opacity = 0.55),
          text_svg(985, 790, "30", 12),
          line_svg(1035, 785, 1130, 785, "#8E99A1", 0.8 + 12 * 50 / max_edge, opacity = 0.55),
          text_svg(1145, 790, "50 documents", 12))

f5 <- write_svg("F5_actor_basis_bipartite_network.svg", W, H,
                "Co-occurrence of empowered actors and judgment bases",
                "Bipartite network linking five actor groups and five judgment bases using document-level co-occurrence at ordinal scores of two or more.", body)

# ---------- Figure 6: robustness across corpus and threshold choices ----------

robust_rows <- do.call(rbind, lapply(1:3, function(threshold) {
  do.call(rbind, lapply(seq_along(actor_codes), function(i) {
    primary <- scores[[actor_cols[i]]] >= threshold
    strict_idx <- as.logical(scores$strict)
    data.frame(threshold = threshold, actor = actor_codes[i],
               primary_n = sum(primary), primary_denominator = nrow(scores),
               primary_pct = 100 * mean(primary),
               strict_n = sum(primary[strict_idx]), strict_denominator = sum(strict_idx),
               strict_pct = 100 * mean(primary[strict_idx]),
               difference_pp = 100 * mean(primary[strict_idx]) - 100 * mean(primary),
               stringsAsFactors = FALSE)
  }))
}))
write.csv(robust_rows, file.path(table_dir, "F6_actor_role_robustness.csv"), row.names = FALSE)

W <- 1740; H <- 790
panel_x <- c(170, 710, 1250); panel_w <- 380
row_y <- seq(145, 585, length.out = 5)
body <- character()

for (t in 1:3) {
  px <- panel_x[t]
  body <- c(body, text_svg(px + panel_w / 2, 36, paste0("Score at least ", t), 17, "bold", "middle"))
  for (tick in seq(0, 100, 25)) {
    x <- px + tick / 100 * panel_w
    body <- c(body, line_svg(x, 70, x, 625, "#E6EAED", 0.9),
              text_svg(x, 651, tick, 12, anchor = "middle", col = muted))
  }
  d <- robust_rows[robust_rows$threshold == t, ]
  for (i in seq_along(actor_codes)) {
    y <- row_y[i]
    x1 <- px + d$primary_pct[i] / 100 * panel_w
    x2 <- px + d$strict_pct[i] / 100 * panel_w
    body <- c(body, line_svg(px, y, px + panel_w, y, "#EDF0F2", 1),
              line_svg(x1, y, x2, y, "#A9A9A9", 1.7),
              circle_svg(x2, y, 11, "#2C7FB8", "#FFFFFF", 1.2, 0.78),
              circle_svg(x1, y, 6.5, "#E53935", "#FFFFFF", 1.1, 0.92))
    if (t == 1) {
      body <- c(body, multiline_svg(px - 28, y - 7,
                                    strsplit(actor_labels[i], "\n", fixed = TRUE)[[1]],
                                    14, "bold", "end", ink, 1.0))
    }
  }
  body <- c(body, text_svg(px + panel_w / 2, 693, "Documents (%)", 13, "bold", "middle"))
}
body <- c(body,
          circle_svg(675, 744, 6.5, "#E53935", "#FFFFFF", 1), text_svg(693, 749, "Primary corpus (n = 59)", 13),
          circle_svg(925, 744, 11, "#2C7FB8", "#FFFFFF", 1.2, 0.78), text_svg(947, 749, "Strict corpus (n = 56)", 13))

f6 <- write_svg("F6_actor_role_robustness.svg", W, H,
                "Robustness of actor-role estimates",
                "Dumbbell plots compare actor-role prevalence in the 59-document primary corpus and 56-document strict corpus at three ordinal thresholds.", body)

# ---------- Captions, provenance, and validation ----------

captions <- c(
  "# English figure captions",
  "",
  "**Figure F1a. Policy-document archetypes in urban accessibility governance: rectangular alternative.** Two-way hierarchical clustering of the 59-document primary analytical corpus and ten reviewed ordinal dimensions using Manhattan distance and average linkage. Cell values are ordinal strengths (0-3). Side strips report publication period, national/subnational scope and inclusion in the strict 56-document sensitivity corpus.",
  "",
  "**Figure F1b. Policy-document archetypes in urban accessibility governance: circular alternative.** The same 59-document row clustering is drawn as an inner radial dendrogram; ten concentric heatmap tracks display the five decision actors and five judgment bases using the same ordinal scale (0-3).",
  "",
  "**Figure F2. From user input to institutional consequence.** Three-column Sankey bands link each coded entry mechanism to the strongest downstream provision observed in the same document and then aggregate that provision into qualification-linked, responsive but non-decisive, unspecified, or decisive status. Documents may contribute to more than one entry mechanism. Bands therefore represent document-level co-coding, not clause-level causal pathways. Public final decision power is shown as zero because no document was coded as explicitly granting such power.",
  "",
  "**Figure F3. Combinations of procedural authority across policy documents.** UpSet plot of actor combinations with ordinal authority scores of at least 2. Bar height is the number of documents in each combination; stacked colours show publication periods.",
  "",
  "**Figure F4a. Subnational accessibility-policy distribution, 2011-2013.** Period-specific counts of retained subnational documents by province-level jurisdiction.",
  "",
  "**Figure F4b. Subnational accessibility-policy distribution, 2014-2016.** Period-specific counts of retained subnational documents by province-level jurisdiction.",
  "",
  "**Figure F4c. Subnational accessibility-policy distribution, 2017-2019.** Period-specific counts of retained subnational documents by province-level jurisdiction.",
  "",
  "**Figure F4d. Subnational accessibility-policy distribution, 2020-2022.** Period-specific counts of retained subnational documents by province-level jurisdiction.",
  "",
  "**Figure F4e. Subnational accessibility-policy distribution, 2023-2026.** Period-specific counts of retained subnational documents by province-level jurisdiction. Across F4a-F4e, city and district documents are allocated once to their province-level jurisdiction. Counts describe the retained corpus rather than implementation intensity or the complete stock of policy in each jurisdiction.",
  "",
  "**Figure F5. Co-occurrence of empowered actors and judgment bases.** Edge width reports the number of documents in which an actor score and a judgment-basis score both reach at least 2. Co-occurrence is measured at document level and does not establish that a particular actor directly applied a particular basis.",
  "",
  "**Figure F6. Robustness of actor-role estimates.** Dumbbells compare actor-role prevalence between the 59-document primary corpus and the 56-document strict sensitivity corpus at ordinal thresholds of 1, 2 and 3."
)
writeLines(captions, file.path(root, "Figure_captions_EN.md"), useBytes = TRUE)

provenance <- data.frame(
  figure = c("F1a", "F1b", "F2", "F3", "F4a", "F4b", "F4c", "F4d", "F4e", "F5", "F6"),
  source = c("reviewed_59_document_scores.csv; policy_sample_59.csv",
             "reviewed_59_document_scores.csv; policy_sample_59.csv",
             "mechanism_final_adjudicated.csv; binary_provenance.csv",
             "reviewed_59_document_scores.csv",
             rep("policy_sample_59.csv; province_labels.csv; china_vertices.csv; world_land_vertices.csv", 5),
             "reviewed_59_document_scores.csv",
             "reviewed_59_document_scores.csv"),
  analytical_rule = c("Two-way Manhattan distance; average linkage; ten ordinal dimensions",
                      "Row-wise Manhattan distance; average linkage; ten concentric ordinal tracks",
                      "Five entry codes; strongest downstream hierarchy POWER > SATGATE > ADOPT > LINK > RESP > none; downstream status aggregation",
                      "Actor score >= 2; exact five-actor intersections",
                      rep("Period-specific subnational document counts; Albers equal-area conic display", 5),
                      "Document-level actor-basis co-occurrence where both scores >= 2",
                      "Actor prevalence in primary and strict corpora at thresholds 1, 2 and 3"),
  stringsAsFactors = FALSE)
write.csv(provenance, file.path(root, "Source_provenance.csv"), row.names = FALSE)

files <- c(f1, f2, f3, f4, f5, f6)
if (!flow_ready) files <- files[file.exists(files)]
svg_text <- vapply(files, function(x) paste(readLines(x, warn = FALSE, encoding = "UTF-8"), collapse = "\n"), "")
validation <- data.frame(
  check = c("Primary corpus documents", "Strict corpus documents", "National documents",
            "Subnational documents", "Province-level units represented", "SVG files generated",
            "Chinese characters in visible SVG output", "Public final decision power count"),
  value = c(nrow(scores), sum(as.logical(scores$strict)), sum(sample59$scope == "National"),
            sum(sample59$scope == "Subnational"),
            length(unique(local_docs$province_code)), length(files),
            sum(grepl("[\u4e00-\u9fff]", svg_text)), sum(mechanisms$POWER == 1)),
  expected = c(59, 56, 24, 35, 24, if (flow_ready) 11 else 10, 0, 0),
  stringsAsFactors = FALSE)
validation$pass <- as.character(validation$value) == as.character(validation$expected)
write.csv(validation, file.path(root, "Validation.csv"), row.names = FALSE)
if (!all(validation$pass)) stop("Validation failed; inspect Validation.csv")

session <- c(capture.output(sessionInfo()), "", paste("Generated:", format(Sys.time(), tz = "Asia/Shanghai")))
writeLines(session, file.path(root, "R_sessionInfo.txt"), useBytes = TRUE)

cat("Created eleven advanced SVG figures in:\n", svg_dir, "\n", sep = "")
cat("Validated 59 primary documents, 56 strict documents, 24 national and 35 subnational documents.\n")
