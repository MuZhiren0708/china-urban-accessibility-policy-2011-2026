# Figure A1: clause-grounded allocation of verification authority in urban
# accessibility facility delivery. Run with base R only:
# Rscript draw_A1_verification_chain.R
# The source IDs below correspond to the retained clause trace in
# outputs/Cities_A1_Urban_Verification_Nature_Style_20261014/tables/.

args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
script_dir <- if (length(file_arg)) {
  dirname(normalizePath(sub("^--file=", "", file_arg[1]), mustWork = TRUE))
} else {
  getwd()
}

out_dir <- script_dir
svg_path <- file.path(out_dir, "A1_verification_chain_four_column.svg")
trace_path <- file.path(out_dir, "A1_source_mapping.csv")
caption_path <- file.path(out_dir, "A1_caption_EN.txt")

palette <- c(
  orange = "#EAA15B",
  blue = "#BBCEFA",
  green = "#E2F0CF",
  yellow = "#FCF8C2",
  ink = "#263238",
  link = "#9CA2A6"
)

W <- 1560
H <- 930

stages <- list(
  list(
    id = "drawing_review", y = 250,
    label = c("1  Drawing review"),
    actor = c("Review body (where required):", "reject non-compliant drawings"),
    user = c("Users: design input encouraged;", "not a drawing-review vote"),
    sources = "N27 Art. 15; N27 Art. 17"
  ),
  list(
    id = "completion_acceptance", y = 375,
    label = c("2  Completion", "acceptance"),
    actor = c("Project owner: organize", "completion acceptance"),
    user = c("Users: trial-use input encouraged;", "required locally for listed projects"),
    sources = "B01 Art. 16; N27 Art. 17; L14 Art. 14; L17 Art. 17"
  ),
  list(
    id = "completion_filing", y = 500,
    label = c("3  Completion filing"),
    actor = c("Filing authority: refuse filing", "if acceptance is absent or failed"),
    user = c("Users: no explicit filing", "decision power in reviewed clauses"),
    sources = "N27 Art. 16"
  ),
  list(
    id = "handover", y = 625,
    label = c("4  Handover for use"),
    actor = c("Project owner: no handover", "without successful acceptance"),
    user = c("Users: no explicit handover", "decision power in reviewed clauses"),
    sources = "N27 Art. 14"
  ),
  list(
    id = "city_assessment", y = 785,
    label = c("Parallel city-level", "assessment"),
    actor = c("Local authority: commission", "third-party assessment"),
    user = c("Residents: views and satisfaction", "inform model-city appraisal"),
    sources = "N27 Art. 60; N25 model-city appraisal"
  )
)

# The two branches keep periodic city assessment distinct from the four
# project-delivery gates. Consultation and trial use are inputs, not vetoes.
stopifnot(length(stages) == 5L)
stopifnot(identical(vapply(stages[1:4], `[[`, "", "id"),
                    c("drawing_review", "completion_acceptance",
                      "completion_filing", "handover")))

xml_escape <- function(s) {
  s <- gsub("&", "&amp;", s, fixed = TRUE)
  s <- gsub("<", "&lt;", s, fixed = TRUE)
  s <- gsub(">", "&gt;", s, fixed = TRUE)
  s <- gsub('"', "&quot;", s, fixed = TRUE)
  s
}

fmt <- function(x) formatC(x, format = "f", digits = 1)

rect_svg <- function(x, y, width, height, fill) {
  sprintf('<rect x="%s" y="%s" width="%s" height="%s" fill="%s"/>',
          fmt(x), fmt(y), fmt(width), fmt(height), fill)
}

text_svg <- function(lines, cx, cy, size = 20, weight = "normal",
                     line_height = 1.17) {
  n <- length(lines)
  ys <- cy + (seq_len(n) - (n + 1) / 2) * size * line_height
  paste(vapply(seq_along(lines), function(i) {
    sprintf(paste0('<text x="%s" y="%s" text-anchor="middle" ',
                   'dominant-baseline="central" font-family="Times New Roman" ',
                   'font-size="%s" font-weight="%s" fill="%s">%s</text>'),
            fmt(cx), fmt(ys[i]), fmt(size), weight, palette[["ink"]],
            xml_escape(lines[i]))
  }, character(1)), collapse = "\n")
}

node_svg <- function(x, cy, width, height, fill, lines, size = 20,
                     weight = "normal") {
  paste(
    rect_svg(x, cy - height / 2, width, height, fill),
    text_svg(lines, x + width / 2, cy, size, weight),
    sep = "\n"
  )
}

curve_svg <- function(x1, y1, x2, y2) {
  dx <- x2 - x1
  sprintf(paste0('<path d="M %s %s C %s %s, %s %s, %s %s" ',
                 'fill="none" stroke="%s" stroke-width="1.65" ',
                 'stroke-dasharray="1.6 4.1" stroke-linecap="round"/>'),
          fmt(x1), fmt(y1), fmt(x1 + dx * 0.48), fmt(y1),
          fmt(x1 + dx * 0.52), fmt(y2), fmt(x2), fmt(y2),
          palette[["link"]])
}

parts <- c(
  '<?xml version="1.0" encoding="UTF-8"?>',
  sprintf(paste0('<svg xmlns="http://www.w3.org/2000/svg" ',
                 'width="%s" height="%s" viewBox="0 0 %s %s" role="img" ',
                 'aria-labelledby="fig-title fig-desc">'), W, H, W, H),
  '<title id="fig-title">Verification authority in urban accessibility facility delivery</title>',
  paste0('<desc id="fig-desc">A four-column policy hierarchy with pastel orange, ',
         'blue, green and yellow nodes. Four project gates branch from project ',
         'verification; city-level assessment is a separate parallel branch. ',
         'Each gate shows the institutional decision actor and the user position.</desc>'),
  sprintf('<rect width="%s" height="%s" fill="#FFFFFF"/>', W, H)
)

# Thin dotted Bezier connectors are drawn behind the flat rectangles.
parts <- c(parts, '<g id="links">')
parts <- c(parts,
           curve_svg(355, 500, 430, 437.5),
           curve_svg(355, 500, 430, 785))
for (stage in stages[1:4]) {
  parts <- c(parts, curve_svg(650, 437.5, 735, stage$y))
}
parts <- c(parts, curve_svg(650, 785, 735, 785))
for (stage in stages) {
  parts <- c(parts,
             curve_svg(980, stage$y, 1085, stage$y - 29),
             curve_svg(980, stage$y, 1085, stage$y + 29))
}
parts <- c(parts, '</g>')

parts <- c(parts, '<g id="column-headings">',
           node_svg(145, 111, 210, 36, palette[["orange"]],
                    "Policy basis", 22, "bold"),
           node_svg(430, 111, 220, 36, palette[["blue"]],
                    "Governance scale", 22, "bold"),
           node_svg(735, 111, 245, 36, palette[["green"]],
                    "Decision stage", 22, "bold"),
           node_svg(1085, 111, 335, 36, palette[["yellow"]],
                    "Actor and user position", 22, "bold"),
           '</g>')

parts <- c(parts, '<g id="policy-hierarchy">',
           node_svg(145, 500, 210, 94, palette[["orange"]],
                    c("Urban accessibility", "facility delivery"), 21),
           node_svg(430, 437.5, 220, 76, palette[["blue"]],
                    c("Project", "verification"), 21),
           node_svg(430, 785, 220, 76, palette[["blue"]],
                    c("City-level", "oversight"), 21))

for (stage in stages) {
  parts <- c(parts,
             node_svg(735, stage$y, 245, 70, palette[["green"]],
                      stage$label, 20),
             node_svg(1085, stage$y - 29, 335, 49, palette[["yellow"]],
                      stage$actor, 17.8),
             node_svg(1085, stage$y + 29, 335, 49, palette[["yellow"]],
                      stage$user, 17.8))
}
parts <- c(parts, '</g>', '</svg>')

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
writeLines(parts, svg_path, useBytes = TRUE)

trace <- data.frame(
  stage = vapply(stages, `[[`, "", "id"),
  actor_or_decision_rule = vapply(stages, function(s) paste(s$actor, collapse = " "), ""),
  user_position = vapply(stages, function(s) paste(s$user, collapse = " "), ""),
  source = vapply(stages, `[[`, "", "sources"),
  stringsAsFactors = FALSE
)
utils::write.csv(trace, trace_path, row.names = FALSE, fileEncoding = "UTF-8")

caption <- paste(
  "Figure A1. Allocation of verification authority in urban accessibility facility delivery.",
  "The first four stages denote project delivery: drawing review, completion acceptance, completion filing and handover for use.",
  "Drawing review applies where it is legally required (N27, Art. 15).",
  "The separate city-level branch represents periodic third-party assessment (N27, Art. 60) and model-city appraisal (N25), not a fifth mandatory project gate.",
  "Dotted links indicate conceptual association, not an observed sequence or transfer of authority.",
  "National law encourages user input in design and acceptance (N27, Art. 17); the reviewed Guangzhou and Shenzhen provisions require trial-use invitations for specified projects (L14, Art. 14; L17, Art. 17).",
  "Neither input is an explicitly assigned final approval or veto in the reviewed clauses.",
  "Satisfaction in N25 is a model-city criterion, not a facility-level acceptance standard.",
  "This schematic synthesizes retained policy clauses rather than observed implementation.",
  sep = " "
)
writeLines(caption, caption_path, useBytes = TRUE)

message("Wrote ", normalizePath(svg_path, winslash = "/"))
message("Wrote ", normalizePath(trace_path, winslash = "/"))
message("Wrote ", normalizePath(caption_path, winslash = "/"))
