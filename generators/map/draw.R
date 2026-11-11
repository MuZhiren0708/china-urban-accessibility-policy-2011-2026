# China policy-document distribution, based on the retained primary sample.
# Run: Rscript draw_policy_distribution.R
# Base R only. Keep the bundled data/ directory beside this script.
# Geometry is supplied as vertex CSVs so the script needs no GIS packages.

argv <- commandArgs(FALSE)
fa <- argv[grepl("^--file=", argv)]
root <- if (length(fa)) dirname(normalizePath(sub("^--file=", "", fa[1]))) else getwd()
input <- function(x) file.path(root, "data", x)
read_input <- function(x) read.csv(input(x), encoding = "UTF-8", stringsAsFactors = FALSE, colClasses = "character")

policies <- read_input("policy_sample_59.csv")
labels <- read_input("province_labels.csv")
china <- read_input("china_vertices.csv")
land <- read_input("world_land_vertices.csv")
labels$label_lon <- as.numeric(labels$label_lon)
labels$label_lat <- as.numeric(labels$label_lat)
for (v in c("lon", "lat")) {
  china[[v]] <- as.numeric(china[[v]])
  land[[v]] <- as.numeric(land[[v]])
}
china$code <- ifelse(is.na(china$code), "", as.character(china$code))
local <- policies[policies$scope == "Subnational", ]
national <- policies[policies$scope == "National", ]

stopifnot(nrow(policies) == 59, !anyDuplicated(policies$id),
          nrow(local) == 35, nrow(national) == 24,
          length(unique(local$province_code)) == 24,
          all(local$province_code %in% labels$code),
          all(is.finite(china$lon)), all(is.finite(china$lat)))

freq <- table(local$province_code)
labels$n <- as.integer(freq[labels$code])
labels$n[is.na(labels$n)] <- 0L
stopifnot(sum(labels$n) == 35, max(labels$n) == 4, sum(labels$n > 0) == 24)
write.csv(labels[, c("code", "name_en", "n")], file.path(root, "province_document_counts.csv"), row.names = FALSE)

W <- 1660
H <- 915
pal <- c("0" = "#ECECE8", "1" = "#D5E5EF", "2" = "#9FC3D8",
         "3" = "#5B8DB0", "4" = "#2F5D7C")
ink <- "#253B49"
muted <- "#647681"
border <- "#73828B"
R <- 6371007.18

# Spherical Albers equal-area conic: central meridian 105 E, latitude of
# origin 35 N, standard parallels 25 and 47 N. This is a small-scale map.
albers <- function(lon, lat) {
  r <- pi / 180
  n <- (sin(25 * r) + sin(47 * r)) / 2
  C <- cos(25 * r)^2 + 2 * n * sin(25 * r)
  rho <- R * sqrt(C - 2 * n * sin(lat * r)) / n
  rho0 <- R * sqrt(C - 2 * n * sin(35 * r)) / n
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
  list(transform = transform, scale = sc, box = box)
}

main_pts <- china[china$geometry != "MultiLineString" & china$lat >= 17.5, c("lon", "lat")]
main <- make_view(as.matrix(main_pts), c(475, 135, 1115, 710), pad = 0.018)
inset_bounds <- as.matrix(expand.grid(lon = seq(105, 125, length.out = 40), lat = seq(3, 24.5, length.out = 40)))
scs <- make_view(inset_bounds, c(1400, 650, 190, 220), pad = 0)
locator <- function(lon, lat) cbind(x = 250 + (lon - 105) * cos(35 * pi / 180) * 4,
                                   y = 290 - (lat - 35) * 4)

esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub('"', "&quot;", x, fixed = TRUE)
}
f <- function(x) formatC(x, digits = 2, format = "f", decimal.mark = ".")
rect <- function(x, y, w, h, fill, stroke = "none", sw = 1) {
  sprintf('<rect x="%s" y="%s" width="%s" height="%s" fill="%s" stroke="%s" stroke-width="%s"/>',
          f(x), f(y), f(w), f(h), fill, stroke, f(sw))
}
line <- function(x1, y1, x2, y2, col = border, sw = 1, dash = NULL) {
  sprintf('<line x1="%s" y1="%s" x2="%s" y2="%s" stroke="%s" stroke-width="%s"%s/>',
          f(x1), f(y1), f(x2), f(y2), col, f(sw),
          if (is.null(dash)) "" else paste0(' stroke-dasharray="', dash, '"'))
}
text <- function(x, y, label, size = 18, weight = "normal", anchor = "start",
                 col = ink, italic = FALSE) {
  sprintf(paste0('<text x="%s" y="%s" font-family="Times New Roman" ',
                 'font-size="%s" font-weight="%s" text-anchor="%s" fill="%s"%s>%s</text>'),
          f(x), f(y), f(size), weight, anchor, col,
          if (italic) ' font-style="italic"' else "", esc(label))
}

ring_key <- function(d) paste(d$polygon_id, d$ring_id, sep = "_")
shape_path <- function(d, transform, closed = TRUE) {
  groups <- split(seq_len(nrow(d)), ring_key(d))
  paste(vapply(groups, function(idx) {
    q <- transform(d$lon[idx], d$lat[idx])
    paste0("M ", paste(paste(f(q[, 1]), f(q[, 2])), collapse = " L "), if (closed) " Z" else "")
  }, ""), collapse = " ")
}
path <- function(d, fill, stroke = border, sw = 0.8) {
  sprintf('<path d="%s" fill="%s" fill-rule="evenodd" stroke="%s" stroke-width="%s" stroke-linejoin="round"/>',
          d, fill, stroke, f(sw))
}
china_shapes <- split(china, china$feature_id)
land_shapes <- split(land, land$feature_id)
draw_china <- function(transform, solid = FALSE, sw = 0.8) {
  vapply(china_shapes, function(d) {
    is_line <- d$geometry[1] == "MultiLineString"
    if (solid && is_line) return("")
    count <- labels$n[match(d$code[1], labels$code)]
    fill <- if (is_line) "none" else if (solid) "#DED78E" else pal[as.character(count)]
    if (is.na(fill)) stop("Unmatched province code")
    path(shape_path(d, transform, !is_line), fill,
         if (solid) "#B7B47D" else border, if (solid) 0.35 else sw)
  }, "")
}
draw_land <- function(transform, fill, stroke, sw) {
  vapply(land_shapes, function(d) path(shape_path(d, transform), fill, stroke, sw), "")
}

# Find an interior visual centre for each province. The point is selected from
# the largest land polygon by maximising its distance from the provincial
# boundary, then refined locally. This keeps count labels away from borders.
polygon_area <- function(x, y) {
  j <- c(2:length(x), 1)
  abs(sum(x * y[j] - x[j] * y)) / 2
}

points_in_polygon <- function(px, py, x, y) {
  inside <- rep(FALSE, length(px))
  j <- length(x)
  for (i in seq_along(x)) {
    crosses <- ((y[i] > py) != (y[j] > py)) &
      (px < (x[j] - x[i]) * (py - y[i]) / (y[j] - y[i] + 1e-12) + x[i])
    inside <- xor(inside, crosses)
    j <- i
  }
  inside
}

distance_to_boundary <- function(px, py, x, y) {
  best <- rep(Inf, length(px))
  j <- length(x)
  for (i in seq_along(x)) {
    vx <- x[i] - x[j]; vy <- y[i] - y[j]
    denom <- vx * vx + vy * vy
    t <- if (denom > 0) ((px - x[j]) * vx + (py - y[j]) * vy) / denom else 0
    t <- pmax(0, pmin(1, t))
    dx <- px - (x[j] + t * vx)
    dy <- py - (y[j] + t * vy)
    best <- pmin(best, sqrt(dx * dx + dy * dy))
    j <- i
  }
  best
}

interior_label_point <- function(code, transform) {
  d <- china[china$code == code & china$geometry != "MultiLineString", ]
  rings <- split(seq_len(nrow(d)), ring_key(d))
  projected <- lapply(rings, function(idx) transform(d$lon[idx], d$lat[idx]))
  areas <- vapply(projected, function(q) polygon_area(q[, 1], q[, 2]), numeric(1))
  poly <- projected[[which.max(areas)]]

  # A few hundred boundary vertices are sufficient for print-scale placement.
  keep <- unique(round(seq(1, nrow(poly), length.out = min(350, nrow(poly)))))
  poly <- poly[keep, , drop = FALSE]
  if (any(poly[1, ] != poly[nrow(poly), ])) poly <- rbind(poly, poly[1, ])
  x <- poly[, 1]; y <- poly[, 2]

  xr <- range(x); yr <- range(y)
  best <- c(mean(xr), mean(yr)); best_distance <- -Inf
  search_box <- c(xr, yr)
  for (iteration in seq_len(4)) {
    gx <- seq(search_box[1], search_box[2], length.out = if (iteration == 1) 45 else 25)
    gy <- seq(search_box[3], search_box[4], length.out = if (iteration == 1) 45 else 25)
    candidates <- expand.grid(x = gx, y = gy)
    inside <- points_in_polygon(candidates$x, candidates$y, x, y)
    candidates <- candidates[inside, , drop = FALSE]
    if (nrow(candidates)) {
      distances <- distance_to_boundary(candidates$x, candidates$y, x, y)
      winner <- which.max(distances)
      best <- unlist(candidates[winner, ], use.names = FALSE)
      best_distance <- distances[winner]
    }
    half_x <- diff(range(gx)) / 8
    half_y <- diff(range(gy)) / 8
    search_box <- c(best[1] - half_x, best[1] + half_x,
                    best[2] - half_y, best[2] + half_y)
  }
  c(x = best[1], y = best[2], clearance = best_distance)
}

svg <- c('<?xml version="1.0" encoding="UTF-8"?>',
         sprintf('<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" viewBox="0 0 %d %d" role="img" aria-labelledby="title desc">', W, H, W, H),
         '<title id="title">Geographical distribution of the urban accessibility policy corpus in China</title>',
         '<desc id="desc">Thirty-five subnational policy documents in the primary analytical sample are aggregated to 24 province-level units. Twenty-four national documents are not mapped. Grey areas have no included subnational document.</desc>',
         '<defs>',
         '<clipPath id="locatorClip"><circle cx="250" cy="290" r="160"/></clipPath>',
         '<clipPath id="mainClip"><rect x="475" y="135" width="1115" height="710"/></clipPath>',
         '<clipPath id="scsClip"><rect x="1400" y="650" width="190" height="220"/></clipPath>',
         '</defs>', rect(0, 0, W, H, "#FFFFFF"),
         rect(20, 20, W - 40, H - 40, "none", "#6E777C", 1.4),
         '<g transform="translate(0,-75)">',
         text(250, 125, "LOCATION", 17, "bold", "middle"))

svg <- c(svg, '<g clip-path="url(#locatorClip)">',
         '<circle cx="250" cy="290" r="160" fill="#CDE6F4"/>',
         draw_land(locator, "#F0EED7", "#A5ACA5", 0.65),
         draw_china(locator, solid = TRUE),
         text(250, 291, "China", 20, "bold", "middle"),
         text(363, 328, "Pacific", 13, anchor = "middle", col = "#678DA3", italic = TRUE),
         text(363, 345, "Ocean", 13, anchor = "middle", col = "#678DA3", italic = TRUE),
         '</g>', '<circle cx="250" cy="290" r="160" fill="none" stroke="#8799A3" stroke-width="1.3"/>',
         line(404, 245, 480, 210, "#A7AFB3", 1.1, "3 5"),
         line(404, 350, 480, 760, "#A7AFB3", 1.1, "3 5"))

# Province fills use document counts, including city/district texts assigned
# once to their province-level jurisdiction. No policy score is mapped.
svg <- c(svg, '<g clip-path="url(#mainClip)">', draw_china(main$transform), '</g>')
for (i in which(labels$n > 0 & !labels$code %in% c("110000", "120000", "310000"))) {
  q <- interior_label_point(labels$code[i], main$transform)
  svg <- c(svg, text(q["x"], q["y"] + 5, as.character(labels$n[i]), 19, "bold", "middle",
                     col = if (labels$n[i] >= 3) "#FFFFFF" else ink))
}

callouts <- list(
  # Place Beijing in the open north-eastern margin so it does not cover
  # neighbouring province counts or the Tianjin callout.
  list(code = "110000", dx = 165, dy = -18),
  list(code = "120000", dx = 77, dy = 2),
  list(code = "310000", dx = 67, dy = 13)
)
for (a in callouts) {
  p <- labels[labels$code == a$code, ]
  q <- main$transform(p$label_lon, p$label_lat)
  tx <- q[1, 1] + a$dx; ty <- q[1, 2] + a$dy
  svg <- c(svg, line(q[1, 1], q[1, 2], tx - 5, ty - 6, border, 0.9),
           text(tx, ty, sprintf("%s (%d)", p$name_en, p$n), 16, "bold"))
}

# North arrow and a projected-distance scale bar.
svg <- c(svg, text(1555, 153, "N", 19, "bold", "middle"),
         '<path d="M 1555 164 L 1542 202 L 1555 193 L 1568 202 Z" fill="#FFFFFF" stroke="#263B49" stroke-width="1.5"/>',
         '<path d="M 1555 164 L 1555 193 L 1542 202 Z" fill="#263B49"/>')
sbx <- 530; sby <- 826; half <- 500000 * main$scale
svg <- c(svg, rect(sbx, sby, half, 8, ink, ink, 0.8),
         rect(sbx + half, sby, half, 8, "#FFFFFF", ink, 0.8),
         text(sbx, sby - 10, "0", 14, anchor = "middle"),
         text(sbx + half, sby - 10, "500", 14, anchor = "middle"),
         text(sbx + 2 * half, sby - 10, "1,000 km", 14, anchor = "middle"))

svg <- c(svg, rect(1400, 650, 190, 220, "#FFFFFF"),
         '<g clip-path="url(#scsClip)">',
         draw_land(scs$transform, "#F3F3F0", "#B3BABD", 0.5),
         draw_china(scs$transform, sw = 0.6), '</g>',
         rect(1400, 650, 190, 220, "none", border, 1),
         text(1495, 891, "South China Sea", 14, anchor = "middle", col = muted))

svg <- c(svg, text(55, 496, "DOCUMENTS BY PROVINCE-LEVEL UNIT", 16, "bold"),
         line(55, 509, 440, 509, "#BFC7CC", 0.8))
listed <- labels[labels$n > 0, ]
for (i in seq_len(nrow(listed))) {
  column <- (i - 1) %/% 12
  row <- (i - 1) %% 12
  x <- 55 + column * 202
  y <- 537 + row * 25
  svg <- c(svg, text(x, y, listed$name_en[i], 16),
           text(x + 175, y, as.character(listed$n[i]), 16, "bold", "end"))
}

svg <- c(svg, text(530, 913, "Included subnational documents", 19, "bold"))
for (n in 1:4) {
  x <- 530 + (n - 1) * 103
  svg <- c(svg, rect(x, 931, 44, 23, pal[as.character(n)], border, 0.7),
           text(x + 56, 949, as.character(n), 17))
}
svg <- c(svg, rect(1000, 931, 44, 23, pal["0"], border, 0.7),
         text(1056, 949, "No included document", 17),
         '</g>', '</svg>')

outfile <- file.path(root, "China_policy_document_distribution_Word_compatible.svg")
writeLines(svg, outfile, useBytes = TRUE)

caption <- paste(
  "Geographical distribution of the primary analytical policy corpus in China (2011-2026).",
  "The map aggregates 35 subnational documents to 24 province-level units; 24 national documents complete the 59-document primary analytical sample.",
  "City and district documents are assigned once to their province-level jurisdiction, including Songjiang District to Shanghai.",
  "Grey areas indicate that no subnational document from that unit was included in this corpus; they do not indicate the absence of accessibility policy.",
  "Counts refer to documents, with retained historical versions counted separately, rather than the number of currently effective policies.",
  "The original registration set contains 80 documents, but the map uses the primary urban analytical sample consistently with the results figures.",
  "The corpus cut-off date is 7 October 2026.",
  "The primary sample retains L34; the strict sensitivity sample excludes it, reducing Jiangxi from two documents to one.",
  "Boundary data: Tianditu-derived GeoJSON.CN, dataset dated 16 December 2025 (CGCS2000). Locator land: Natural Earth, public domain.",
  "The boundaries are the data provider's geometry; no new cartographic approval is implied.",
  sep = " "
)
writeLines(caption, file.path(root, "Figure_caption_EN.txt"), useBytes = TRUE)
checks <- data.frame(check = c("Primary sample", "National documents", "Subnational documents",
                               "Represented province-level units", "Maximum province count", "Duplicate IDs"),
                     value = c(nrow(policies), nrow(national), nrow(local), sum(labels$n > 0), max(labels$n), anyDuplicated(policies$id)))
write.csv(checks, file.path(root, "Data_validation.csv"), row.names = FALSE)
message("SVG written: ", normalizePath(outfile, winslash = "/"))
message("Verified: 59 primary documents = 24 national + 35 subnational; 24 province-level units.")
