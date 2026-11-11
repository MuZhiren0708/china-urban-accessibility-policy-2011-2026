# ALL manuscript and supplementary figures, latest reviewed versions (2026-11-07).
# Run: Rscript generate_all_figures.R
# Or source("generate_all_figures.R") from RStudio.
# Base R only. Keep data/, generators/, and source_evidence/ beside this file.
# All 18 SVGs are rebuilt from the bundled numerical/evidence/geometry tables.
# Stage 1 reconstructs and checks the final data, then draws the baseline figures.
# Stage 2 applies the latest reviewed typography and original-proportion layouts.
# For ten type-only revisions, source_svg/ is refreshed from this run's baseline
# SVGs before editing text, so non-text geometry remains exactly unchanged.
local({
  bundle_frames <- vapply(sys.frames(),function(fr) if(!is.null(fr$ofile)) as.character(fr$ofile) else "",character(1))
  bundle_frames <- bundle_frames[nzchar(bundle_frames)]
  bundle_args <- grep("^--file=",commandArgs(FALSE),value=TRUE)
  bundle_root <- if(length(bundle_frames)) dirname(normalizePath(tail(bundle_frames,1),winslash="/",mustWork=TRUE)) else if(length(bundle_args)) dirname(normalizePath(sub("^--file=","",bundle_args[1]),winslash="/",mustWork=TRUE)) else normalizePath(getwd(),winslash="/",mustWork=TRUE)
  bundle_entry <- file.path(bundle_root,"generate_all_figures.R")
  for(d in c("audit","data","svg","source_svg","preview")) dir.create(file.path(bundle_root,d),showWarnings=FALSE,recursive=TRUE)
  message("Stage 1/2: reconstruct data and draw all eighteen figures.")
  local({
    commandArgs <- function(trailingOnly=FALSE) if(trailingOnly) character() else paste0("--file=",bundle_entry)

# Reproduce the revised manuscript figures from the bundled source tables.
# Run: Rscript generate_all_figures.R
# Base R only. Keep generators/ and source_evidence/ beside this file.
# Binary mechanisms: two independent human coders plus documented decisions and source verification.
# The supplied ordinal actor/basis classifications are a separate unchanged layer.
argv <- commandArgs(FALSE)
ff <- grep("^--file=",argv,value=TRUE)
master_dir <- bundle_root
if(.Platform$OS.type=="windows") invisible(suppressWarnings(Sys.setlocale("LC_CTYPE","English_United States.utf8")))
# Rebuild final binary assignments from the untouched paired human results.
# No adjudication or source-check value is inferred from a plotting outcome.
reconcile_binary_inputs <- function(master_dir) {
  input_dir <- file.path(master_dir, "source_evidence")
  output_dir <- file.path(master_dir, "generators", "advanced", "data")
  read_input <- function(name) read.csv(file.path(input_dir, name),
    stringsAsFactors=FALSE, check.names=FALSE, fileEncoding="UTF-8-BOM",
    na.strings=c("", "NA"))
  paired <- read_input("binary_original_matrix.csv")
  adjudication <- read_input("binary_original_adjudication.csv")
  source_checks <- read_input("binary_supplementary_source_checks.csv")
  scores <- read.csv(file.path(output_dir,"reviewed_59_document_scores.csv"),
    stringsAsFactors=FALSE, check.names=FALSE, fileEncoding="UTF-8-BOM")
  codes <- sub("_C1$", "", grep("_C1$",names(paired),value=TRUE))
  stopifnot(nrow(paired)==66L, length(codes)==16L,
    nrow(adjudication)==26L, nrow(source_checks)==5L,
    !anyDuplicated(paired$ID), nrow(scores)==59L)
  final <- data.frame(id=paired$ID,stringsAsFactors=FALSE)
  for(code in codes) final[[code]] <- as.integer(paired[[paste0(code,"_C1")]])
  n_human_decisions <- 0L
  for(i in seq_len(nrow(adjudication))) {
    id <- adjudication[["文档ID"]][i]; code <- adjudication[["代码"]][i]
    text <- adjudication[["裁决与说明"]][i]
    # The revised source CSV has eight columns. The source-check category is
    # not a completed human decision; do not interpret its tentative SUP=0.
    if ("类别" %in% names(adjudication) && adjudication[["类别"]][i] == "补充核验") next
    pat <- paste0(code,"\\s*=\\s*([01])")
    hit <- regmatches(text, regexec(pat,text,perl=TRUE))[[1]]
    if(length(hit)==2L) {
      j <- match(id,final$id); stopifnot(!is.na(j),code %in% codes)
      final[[code]][j] <- as.integer(hit[2])
      n_human_decisions <- n_human_decisions+1L
    }
  }
  stopifnot(n_human_decisions==21L)
  for(i in seq_len(nrow(source_checks))) {
    j <- match(source_checks$id[i],final$id); code <- source_checks$code[i]
    stopifnot(!is.na(j),code %in% codes,source_checks$final_value[i] %in% 0:1)
    final[[code]][j] <- as.integer(source_checks$final_value[i])
  }
  final$eligibility_flag <- as.integer(final$id %in% scores$id)
  final$strict_flag <- 0L
  j <- match(scores$id,final$id); stopifnot(!anyNA(j))
  final$strict_flag[j] <- as.integer(as.logical(scores$strict))
  urban <- final[final$eligibility_flag==1L,,drop=FALSE]
  stopifnot(nrow(urban)==59L,sum(urban$strict_flag)==56L,
    !anyNA(urban[,codes,drop=FALSE]),
    all(as.matrix(urban[,codes,drop=FALSE]) %in% 0:1))
  # Agreement is computed BEFORE adjudication on unchanged C1 and C2.
  agreement <- function(ids, selected=codes) {
    x <- paired[paired$ID %in% ids,,drop=FALSE]
    a <- unlist(x[,paste0(selected,"_C1"),drop=FALSE],use.names=FALSE)
    b <- unlist(x[,paste0(selected,"_C2"),drop=FALSE],use.names=FALSE)
    ok <- !is.na(a)&!is.na(b); a <- a[ok];b <- b[ok]
    n <- length(a); observed <- mean(a==b)
    expected <- mean(a==1)*mean(b==1)+mean(a==0)*mean(b==0)
    kappa <- if(abs(1-expected)<1e-14) NA_real_ else (observed-expected)/(1-expected)
    data.frame(paired_cells=n,disagreements=sum(a!=b),agreement=observed,kappa=kappa)
  }
  reliability <- do.call(rbind,lapply(c("ALL",codes),function(code) {
    out <- agreement(scores$id,if(code=="ALL") codes else code)
    cbind(code=code,out,stringsAsFactors=FALSE)
  }))
  stopifnot(reliability$paired_cells[1]==944L,
    reliability$disagreements[1]==24L,
    abs(reliability$kappa[1]-0.9199773947442779)<1e-12,
    is.na(reliability$kappa[reliability$code=="POWER"]))
  dir.create(file.path(master_dir,"tables"),showWarnings=FALSE,recursive=TRUE)
  write.csv(reliability,file.path(master_dir,"tables","binary_pre_adjudication_reliability_59.csv"),
    row.names=FALSE,na="NA",fileEncoding="UTF-8")
  target <- file.path(output_dir,"mechanism_final_adjudicated.csv")
  if(file.exists(target)) {
    previous <- read.csv(target,stringsAsFactors=FALSE,check.names=FALSE,
      fileEncoding="UTF-8-BOM",na.strings=c("","NA"))
    p <- previous[match(final$id,previous$id),names(final),drop=FALSE]
    stopifnot(identical(p$id,final$id))
    for(name in setdiff(names(final),"id"))
      stopifnot(isTRUE(all.equal(as.numeric(p[[name]]),as.numeric(final[[name]]),check.attributes=FALSE)))
  }
  write.csv(final,target,row.names=FALSE,na="NA",fileEncoding="UTF-8")
  # Keep cell-level provenance aligned with the revised source adjudication file.
  provenance_path <- file.path(output_dir,"binary_provenance.csv")
  provenance <- read.csv(provenance_path,stringsAsFactors=FALSE,check.names=FALSE,
    fileEncoding="UTF-8-BOM",na.strings=c("","NA"))
  if(!"author_confirmation" %in% names(provenance)) provenance$author_confirmation <- ""
  for(i in seq_len(nrow(adjudication))) {
    id <- adjudication[["文档ID"]][i]; code <- adjudication[["代码"]][i]
    k <- which(provenance$id==id & provenance$code==code)
    stopifnot(length(k)==1L)
    provenance$final[k] <- final[[code]][match(id,final$id)]
    if(adjudication[["类别"]][i]=="补充核验") {
      provenance$status[k] <- "supplementary-author-source-verification"
      j <- which(source_checks$id==id & source_checks$code==code)
      stopifnot(length(j)==1L)
      provenance$locator[k] <- source_checks$locator[j]
      provenance$source_url[k] <- source_checks$source_url[j]
      if("author_confirmation" %in% names(source_checks))
        provenance$author_confirmation[k] <- source_checks$author_confirmation[j]
    } else {
      provenance$status[k] <- "recorded-human-adjudication"
      provenance$rationale[k] <- adjudication[["裁决与说明"]][i]
    }
  }
  stopifnot(sum(provenance$status=="recorded-human-adjudication")==21L,
    sum(provenance$status=="supplementary-author-source-verification")==5L)
  write.csv(provenance,provenance_path,row.names=FALSE,na="NA",fileEncoding="UTF-8")
  invisible(final)
}
reconcile_binary_inputs(master_dir)
flow_ready <- file.exists(file.path(master_dir,"generators/advanced/data/mechanism_final_adjudicated.csv"))
for(d in c("svg","tables","preview")) dir.create(file.path(master_dir,d),recursive=TRUE,showWarnings=FALSE)

# Validation precedes figure generation. No semantic recoding is performed here.
read_data <- function(p) read.csv(p,fileEncoding="UTF-8",stringsAsFactors=FALSE,check.names=FALSE)
data_root <- file.path(master_dir,"generators/advanced/data")
s <- read_data(file.path(data_root,"reviewed_59_document_scores.csv"))
m <- read_data(file.path(data_root,"mechanism_coding_80.csv"))
stopifnot(nrow(m)==80, !anyDuplicated(m$id), nrow(s)==59, !anyDuplicated(s$id),
          sum(s$strict)==56, all(s$id %in% m$id))
# Preserve the registered corpus, while updating the reviewed urban binary cells.
fp <- file.path(data_root,"mechanism_final_adjudicated.csv")
if(file.exists(fp)) {
  fm <- read_data(fp)
  final_binary <- c("CONS","DES","TRY","EVAL","SUP","RESP","LINK","ADOPT","POWER","QUOTA","SUPPORT","FILTER","CHOICE","SATGATE","MTRY","CONDITION")
  jj <- match(m$id,fm$id); present <- which(!is.na(jj))
  for(v in final_binary) m[[v]][present] <- fm[[v]][jj[present]]
}
m$level[m$id=="L01"] <- "省"
write.csv(m,file.path(master_dir,"tables/corpus_register_80_level_harmonized.csv"),row.names=FALSE,fileEncoding="UTF-8")
actor <- c("TECH","DEV","ADM","THIRD","USER")
basis <- c("TECHNICAL","DOCUMENTS","EXPERIENCE","TRACE","PUBLIC")
source_scores <- read_data(file.path(master_dir,"source_evidence/source_document_scores.csv"))
for(i in seq_len(nrow(s))) for(v in c(paste0("actor_",actor),paste0("basis_",basis))) {
  j <- match(s$id[i],source_scores$id)
  stopifnot(!is.na(j),s[[v]][i]==source_scores[[v]][j])
}
ht <- file.path(master_dir,"generators/heatmaps/tables")
a2 <- read_data(file.path(ht,"A2_key_document_actor_matrix.csv"))
for(i in seq_len(nrow(a2))) stopifnot(a2$score[i]==source_scores[[paste0("actor_",a2$actor[i])]][match(a2$id[i],source_scores$id)])
a3 <- read_data(file.path(ht,"A3_period_basis_max_mean_counts_evidence.csv"))
for(i in seq_len(nrow(a3))) {
  z <- s[s$period==a3$period[i],]
  v <- z[[paste0("basis_",a3$basis[i])]]
  stopifnot(a3$max_score[i]==max(v),a3$n[i]==length(v),abs(a3$mean_score[i]-mean(v))<1e-10)
  for(k in 0:3) stopifnot(a3[[paste0("n_score_",k)]][i]==sum(v==k))
  vs <- v[z$strict]
  stopifnot(a3$strict_n[i]==length(vs),a3$strict_max[i]==max(vs),abs(a3$strict_mean[i]-mean(vs))<1e-10)
}
a5 <- read_data(file.path(ht,"A5_local_minus_2023_law_scores.csv"))
local_source <- read_data(file.path(master_dir,"source_evidence/local_dimension_scores.csv"))
for(i in seq_len(nrow(a5))) {
  b <- local_source[[a5$dimension[i]]][local_source$id=="N27"]
  v <- local_source[[a5$dimension[i]]][local_source$id==a5$id[i]]
  stopifnot(length(v)==1,length(b)==1,a5$local_score[i]==v,a5$baseline_score[i]==b,a5$difference[i]==v-b)
}
levs <- c("国家","省","市","区")
flags <- list(Registered=rep(TRUE,80),Dedicated=m$corpus=="核心",Contextual=m$corpus=="背景",
              Mechanism_primary=m$main==1,Mechanism_strict=m$strict==1,
              Urban_primary=m$id %in% s$id,Urban_strict=m$id %in% s$id[s$strict])
composition <- data.frame(Government_level=c("National","Provincial","Municipal","District"))
for(n in names(flags)) composition[[n]] <- sapply(levs,function(l) sum(flags[[n]] & m$level==l,na.rm=TRUE))
composition <- rbind(composition,c("Total",colSums(composition[,-1])))
write.csv(composition,file.path(master_dir,"tables/Table_1A_recomputed.csv"),row.names=FALSE)
period <- cut(as.integer(m$year),c(2010,2013,2016,2019,2022,2026),labels=paste0("P",1:5))
period_composition <- data.frame(Publication_period=paste0("P",1:5))
for(n in names(flags)) period_composition[[n]] <- sapply(paste0("P",1:5),function(p)sum(flags[[n]] & period==p,na.rm=TRUE))
write.csv(period_composition,file.path(master_dir,"tables/Table_1B_recomputed.csv"),row.names=FALSE)
write.csv(a3,file.path(master_dir,"tables/Figure_4_maxima_and_distributions.csv"),row.names=FALSE)
write.csv(data.frame(check=c("80 register IDs","59 score IDs","56 strict IDs","source score equality","Figure 3 source equality","Figure 4 maxima and frequencies","Figure 6 signed differences"),pass=TRUE),file.path(master_dir,"tables/data_audit.csv"),row.names=FALSE)


local({
.embedded <- file.path(master_dir,"generators/map/draw.R")
commandArgs <- function(trailingOnly=FALSE) if(trailingOnly) character() else paste0("--file=",.embedded)
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

})


local({
.embedded <- file.path(master_dir,"generators/chain/draw.R")
commandArgs <- function(trailingOnly=FALSE) if(trailingOnly) character() else paste0("--file=",.embedded)
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

})


local({
.embedded <- file.path(master_dir,"generators/heatmaps/draw.R")
commandArgs <- function(trailingOnly=FALSE) if(trailingOnly) character() else paste0("--file=",.embedded)
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

})


local({
.embedded <- file.path(master_dir,"generators/advanced/draw.R")
commandArgs <- function(trailingOnly=FALSE) if(trailingOnly) character() else paste0("--file=",.embedded)
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

# This block executes inside the advanced generator so all plots share scores.
# The oblique profiles are independent counts, not stacked or fitted trends.
counts <- sapply(period_codes,function(p) colSums(scores[scores$period==p,actor_cols]>=2))
rownames(counts) <- actor_codes
den <- as.integer(table(factor(scores$period,levels=period_codes)))
profile_col <- c(ADM="#3F825A",USER="#B94F32",DEV="#BD7C25",TECH="#3673A6",THIRD="#78569F")
depth <- c("THIRD","TECH","DEV","USER","ADM")
names_full <- c(TECH="Technical review bodies",DEV="Project owners",ADM="Administrative authorities",THIRD="Third-party evaluators",USER="Users")
project <- function(x,y,z) cbind(x=170+(x-1)*150+(y-1)*60,y=720-(y-1)*90-z*15)
seg3 <- function(x,y,z,col="#253B49",sw=1,dash=NULL) {
  q<-project(x,y,z)
  paste(vapply(seq_len(nrow(q)-1),function(i) line_svg(q[i,1],q[i,2],q[i+1,1],q[i+1,2],col,sw,dash),""),collapse="")
}
wb <- character()
for(x in 1:5) wb<-c(wb,seg3(c(x,x),c(1,5),c(0,0),"#ABB3B8",0.7,"5 5"),seg3(c(x,x),c(5,5),c(0,20),"#ABB3B8",0.7,"5 5"))
for(z in seq(0,20,5)) wb<-c(wb,seg3(c(1,5),c(5,5),c(z,z),"#ABB3B8",0.7,"5 5"),seg3(c(1,1),c(1,5),c(z,z),"#ABB3B8",0.7,"5 5"))
profile_rows<-list()
for(j in 5:1) {
  a<-depth[j];v<-counts[a,];q<-project(c(1,1:5,5),rep(j,7),c(0,v,0))
  d<-paste0("M ",paste(apply(q,1,function(r)paste(f(r),collapse=" ")),collapse=" L ")," Z")
  wb<-c(wb,path_svg(d,profile_col[a],"none",opacity=0.28),seg3(1:5,rep(j,5),v,profile_col[a],2.4))
  q<-project(1:5,rep(j,5),v)
  for(i in 1:5) wb<-c(wb,circle_svg(q[i,1],q[i,2],3.1,"white",profile_col[a],1.1))
  profile_rows[[a]]<-data.frame(actor=a,period=period_codes,n=as.integer(v),denominator=den)
}
wb<-c(wb,seg3(c(1,5),c(1,1),c(0,0),sw=1.8),seg3(c(5,5),c(1,5),c(0,0),sw=1.8),seg3(c(1,1),c(1,1),c(0,20),sw=1.8))
for(i in 1:5) {
 q<-project(i,1,0)
 wb<-c(wb,multiline_svg(q[1,1],q[1,2]+32,c(period_labels[i],paste0("(n = ",den[i],")")),16,anchor="middle",leading=1.7))
 a<-depth[i];q<-project(5,i,0)
 wb<-c(wb,text_svg(q[1,1]+20,q[1,2]+8,names_full[a],17))
}
for(z in seq(0,20,5)) {q<-project(1,1,z);wb<-c(wb,text_svg(q[1,1]-25,q[1,2]+5,z,17,anchor="end"))}
wb<-c(wb,text_svg(470,837,"Publication period",21,anchor="middle"),text_svg(55,565,"Documents with procedural roles",19,anchor="middle",rotate=-90))
write_svg("F7_actor_period_profiles.svg",1320,870,"Actor-period counts","Independent profiles; counts at actor score at least 2; n=59; final interval ends 7 October 2026.",wb)
write.csv(do.call(rbind,profile_rows),file.path(table_dir,"F7_actor_period_counts.csv"),row.names=FALSE)

# Corpus construction, not an invented PRISMA search-hit or duplicate-removal flow.
flowbox <- function(x,y,w,h,labels,fill) c(rect_svg(x,y,w,h,fill,"#8798A3",1,4),centered_multiline_svg(x+w/2,y,h,labels,21))
sb<-c(flowbox(55,45,480,95,c("Registered document versions","n = 80"),"#E5EEF8"),
 flowbox(55,205,480,95,c("Dedicated accessibility documents","n = 67"),"#E5EEF8"),
 flowbox(55,365,480,95,c("Complete mechanism records","n = 65"),"#E5EEF8"),
 flowbox(55,525,480,95,c("Urban analytical corpus","n = 59"),"#D9EDE6"),
 flowbox(55,725,480,95,c("Strict urban sensitivity corpus","n = 56"),"#D9EDE6"),
 flowbox(650,125,575,100,c("Contextual documents: n = 13","Retained for interpretation, not core counts"),"#FAF0DA"),
 flowbox(650,285,575,100,c("Missing required annexes: n = 2","N10; N23"),"#FAF0DA"),
 flowbox(650,445,575,125,c("Outside the physical urban scope: n = 6","N37; N08; N17; N18; N38; N42"),"#FAF0DA"),
 flowbox(650,650,575,155,c("Different documentary statuses: n = 3","N36: model provisions; N33: practice list","L34: published, not yet effective at cutoff"),"#FAF0DA"))
for(pair in list(c(140,205),c(300,365),c(460,525),c(620,725))) sb<-c(sb,line_svg(295,pair[1],295,pair[2],sw=1.8))
for(z in list(c(175,175),c(335,335),c(495,507),c(675,727))) sb<-c(sb,line_svg(295,z[1],650,z[2],sw=1.4))
write_svg("Figure_S1_corpus_construction.svg",1280,870,"Construction of the analytical corpus","Known registered-document counts only. No reconstructed search-hit or deduplication counts.",sb)

})


# Collect the numbered manuscript outputs; the circular plot is an optional alternative.
manifest <- data.frame(file=c("Figure_01_policy_distribution.svg","Figure_02_verification_chain.svg","Figure_03_actor_authority.svg","Figure_04_basis_maximum.svg","Figure_05_participation_sankey.svg","Figure_06_local_difference.svg","Figure_S1_corpus_construction.svg","Figure_S2a_distribution_2011_2013.svg","Figure_S2b_distribution_2014_2016.svg","Figure_S2c_distribution_2017_2019.svg","Figure_S2d_distribution_2020_2022.svg","Figure_S2e_distribution_2023_2026.svg","Figure_S3_actor_combinations.svg","Figure_S4_actor_basis_network.svg","Figure_S5_cluster_heatmap.svg","Alternative_Figure_S5_circular.svg","Figure_S6_actor_period_profiles.svg","Figure_S7_robustness.svg"),source=c("map/China_policy_document_distribution_Word_compatible.svg","chain/A1_verification_chain_four_column.svg","heatmaps/svg/A2_key_document_actor_authority.svg","heatmaps/svg/A3_period_basis_intensity.svg","advanced/svg/F2_user_input_to_consequence_sankey.svg","heatmaps/svg/A5_local_configuration_difference.svg","advanced/svg/Figure_S1_corpus_construction.svg","advanced/svg/F4a_policy_diffusion_2011_2013.svg","advanced/svg/F4b_policy_diffusion_2014_2016.svg","advanced/svg/F4c_policy_diffusion_2017_2019.svg","advanced/svg/F4d_policy_diffusion_2020_2022.svg","advanced/svg/F4e_policy_diffusion_2023_2026.svg","advanced/svg/F3_actor_mechanism_upset.svg","advanced/svg/F5_actor_basis_bipartite_network.svg","advanced/svg/F1a_policy_document_cluster_heatmap_rectangular.svg","advanced/svg/F1b_policy_document_cluster_heatmap_circular.svg","advanced/svg/F7_actor_period_profiles.svg","advanced/svg/F6_actor_role_robustness.svg"),stringsAsFactors=FALSE)
if (!flow_ready) manifest <- manifest[manifest$file != "Figure_05_participation_sankey.svg",,drop=FALSE]
for(i in seq_len(nrow(manifest))) stopifnot(file.copy(file.path(master_dir,"generators",manifest$source[i]),file.path(master_dir,"svg",manifest$file[i]),overwrite=TRUE))
write.csv(manifest,file.path(master_dir,"figure_manifest.csv"),row.names=FALSE)
# Embed the exact plotted numeric input as accessible SVG metadata.  Cairo
# outlines Times New Roman glyphs in the three simple heatmaps; the CSV metadata
# preserves machine-readable values while keeping those glyphs portable in Word.
numeric_sources <- c(
 "generators/map/province_document_counts.csv",
 "generators/chain/A1_source_mapping.csv",
 "generators/heatmaps/tables/A2_key_document_actor_matrix.csv",
 "generators/heatmaps/tables/A3_period_basis_max_mean_counts_evidence.csv",
 "generators/advanced/tables/F2_user_input_consequence_flows.csv",
 "generators/heatmaps/tables/A5_local_minus_2023_law_scores.csv",
 "tables/Table_1A_recomputed.csv",
 rep("generators/advanced/tables/F4_period_province_document_counts.csv",5),
 "generators/advanced/tables/F3_actor_mechanism_combinations.csv",
 "generators/advanced/tables/F5_actor_basis_cooccurrence_edges.csv",
 rep("generators/advanced/tables/F1_clustered_policy_profiles.csv",2),
 "generators/advanced/tables/F7_actor_period_counts.csv",
 "generators/advanced/tables/F6_actor_role_robustness.csv")
names(numeric_sources) <- c("Figure_01_policy_distribution.svg","Figure_02_verification_chain.svg",
 "Figure_03_actor_authority.svg","Figure_04_basis_maximum.svg","Figure_05_participation_sankey.svg",
 "Figure_06_local_difference.svg","Figure_S1_corpus_construction.svg",
 "Figure_S2a_distribution_2011_2013.svg","Figure_S2b_distribution_2014_2016.svg",
 "Figure_S2c_distribution_2017_2019.svg","Figure_S2d_distribution_2020_2022.svg",
 "Figure_S2e_distribution_2023_2026.svg","Figure_S3_actor_combinations.svg",
 "Figure_S4_actor_basis_network.svg","Figure_S5_cluster_heatmap.svg",
 "Alternative_Figure_S5_circular.svg","Figure_S6_actor_period_profiles.svg","Figure_S7_robustness.svg")
xml_data_escape <- function(x) {
 x <- gsub("&","&amp;",x,fixed=TRUE)
 x <- gsub("<","&lt;",x,fixed=TRUE)
 gsub(">","&gt;",x,fixed=TRUE)
}
for(i in seq_len(nrow(manifest))) {
 file <- manifest$file[i]; rel <- numeric_sources[file]
 exact <- readLines(file.path(master_dir,rel),warn=FALSE,encoding="UTF-8")
 meta <- c(sprintf('<metadata id="analysis-data" data-source="%s">',rel),xml_data_escape(exact),'</metadata>')
 target <- file.path(master_dir,"svg",file)
 lines <- readLines(target,warn=FALSE,encoding="UTF-8")
 stopifnot(tail(lines,1)=="</svg>")
 writeLines(c(head(lines,-1),meta,tail(lines,1)),target,useBytes=TRUE)
}

writeLines(capture.output(sessionInfo()),file.path(master_dir,"R_sessionInfo.txt"))
cat("Validated and exported",nrow(manifest),"SVG files.\n")


  })


  # Fresh geometry-preserving typography inputs: produced by Stage 1 above.
  typography_inputs <- c("Figure_01_policy_distribution.svg","Figure_S2a_distribution_2011_2013.svg","Figure_S2b_distribution_2014_2016.svg","Figure_S2c_distribution_2017_2019.svg","Figure_S2d_distribution_2020_2022.svg","Figure_S2e_distribution_2023_2026.svg","Figure_S3_actor_combinations.svg","Figure_S4_actor_basis_network.svg","Figure_S5_cluster_heatmap.svg","Figure_S7_robustness.svg")
  for(n in typography_inputs) stopifnot(file.copy(file.path(bundle_root,"svg",n),file.path(bundle_root,"source_svg",n),overwrite=TRUE))
  message("Stage 2/2: apply the latest reviewed type sizes and layouts.")


# -------- Latest revision: Figure_02_verification_chain.R --------

# Figure 2: enlarged text and compact four-column layout.
# Run this file with Rscript, or source it from the package directory.
# Base R only; all policy statements and source identifiers are retained below.
local({
  caller <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- args[grepl("^--file=", args)]
  script_dir <- bundle_root
  svg_dir <- file.path(script_dir, "svg")
  data_dir <- file.path(script_dir, "data")
  dir.create(svg_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

  palette <- c(orange = "#EAA15B", blue = "#BBCEFA", green = "#E2F0CF",
               yellow = "#FCF8C2", ink = "#263238", link = "#9CA2A6")
  # At a displayed width of 170 mm, 28 px is 9.64 pt, 30 px is 10.33 pt.
  W <- 1400
  H <- 920
  x <- c(basis = 20, scale = 285, stage = 567, position = 900)
  width <- c(basis = 225, scale = 245, stage = 280, position = 480)
  heading_cy <- 35
  project_cy <- 402
  city_cy <- 829
  basis_cy <- 510
  stages <- list(
    list(id = "drawing_review", y = 153,
         label = "1  Drawing review",
         actor = c("Review body (where required):", "reject non-compliant drawings"),
         user = c("Users: design input encouraged;", "not a drawing-review vote"),
         sources = "N27 Art. 15; N27 Art. 17"),
    list(id = "completion_acceptance", y = 319,
         label = c("2  Completion", "acceptance"),
         actor = c("Project owner: organize", "completion acceptance"),
         user = c("Users: trial-use input encouraged;", "required locally for listed projects"),
         sources = "B01 Art. 16; N27 Art. 17; L14 Art. 14; L17 Art. 17"),
    list(id = "completion_filing", y = 485,
         label = "3  Completion filing",
         actor = c("Filing authority: refuse filing", "if acceptance is absent or failed"),
         user = c("Users: no explicit filing", "decision power in reviewed clauses"),
         sources = "N27 Art. 16"),
    list(id = "handover", y = 651,
         label = "4  Handover for use",
         actor = c("Project owner: no handover", "without successful acceptance"),
         user = c("Users: no explicit handover", "decision power in reviewed clauses"),
         sources = "N27 Art. 14"),
    list(id = "city_assessment", y = city_cy,
         label = c("Parallel city-level", "assessment"),
         actor = c("Local authority: commission", "third-party assessment"),
         user = c("Residents: views and satisfaction", "inform model-city appraisal"),
         sources = "N27 Art. 60; N25 model-city appraisal")
  )
  stopifnot(length(stages) == 5L,
            identical(vapply(stages[1:4], `[[`, "", "id"),
                      c("drawing_review", "completion_acceptance", "completion_filing", "handover")))
  escape <- function(s) {
    s <- gsub("&", "&amp;", s, fixed = TRUE)
    s <- gsub("<", "&lt;", s, fixed = TRUE)
    s <- gsub(">", "&gt;", s, fixed = TRUE)
    gsub('"', "&quot;", s, fixed = TRUE)
  }
  f <- function(v) formatC(v, format = "f", digits = 1)
  text <- function(lines, cx, cy, size = 28, weight = "normal", line_height = 1.1) {
    ys <- cy + (seq_along(lines) - (length(lines) + 1) / 2) * size * line_height
    paste(vapply(seq_along(lines), function(i) {
      sprintf(paste0('<text x="%s" y="%s" text-anchor="middle" dominant-baseline="central" ',
                     'font-family="Times New Roman" font-size="%s" font-weight="%s" fill="%s">%s</text>'),
              f(cx), f(ys[i]), f(size), weight, palette[["ink"]], escape(lines[i]))
    }, character(1)), collapse = "\n")
  }
  node <- function(id, xx, cy, ww, hh, fill, lines, size = 28, weight = "normal") {
    paste0('<g class="node" id="', id, '">\n',
           sprintf('<rect x="%s" y="%s" width="%s" height="%s" fill="%s"/>',
                   f(xx), f(cy - hh / 2), f(ww), f(hh), fill), '\n',
           text(lines, xx + ww / 2, cy, size, weight), '\n</g>')
  }
  curve <- function(x1, y1, x2, y2) {
    dx <- x2 - x1
    sprintf(paste0('<path d="M %s %s C %s %s, %s %s, %s %s" fill="none" ',
                   'stroke="%s" stroke-width="1.9" stroke-dasharray="1.8 4.2" stroke-linecap="round"/>'),
            f(x1), f(y1), f(x1 + dx * 0.48), f(y1),
            f(x1 + dx * 0.52), f(y2), f(x2), f(y2), palette[["link"]])
  }
  parts <- c('<?xml version="1.0" encoding="UTF-8"?>',
             sprintf('<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" viewBox="0 0 %d %d" role="img" aria-labelledby="fig-title fig-desc">', W, H, W, H),
             '<title id="fig-title">Verification authority in urban accessibility facility delivery</title>',
             '<desc id="fig-desc">A compact four-column policy hierarchy. Four project gates branch from project verification; city-level assessment is a separate parallel branch. Each gate shows the institutional decision actor and the user position. All policy meanings and source identifiers are unchanged.</desc>',
             sprintf('<rect width="%d" height="%d" fill="#FFFFFF"/>', W, H),
             '<g id="links">',
             curve(x[["basis"]] + width[["basis"]], basis_cy, x[["scale"]], project_cy),
             curve(x[["basis"]] + width[["basis"]], basis_cy, x[["scale"]], city_cy))
  for (stage in stages[1:4]) {
    parts <- c(parts, curve(x[["scale"]] + width[["scale"]], project_cy, x[["stage"]], stage$y))
  }
  parts <- c(parts, curve(x[["scale"]] + width[["scale"]], city_cy, x[["stage"]], city_cy))
  for (stage in stages) {
    parts <- c(parts,
               curve(x[["stage"]] + width[["stage"]], stage$y, x[["position"]], stage$y - 40),
               curve(x[["stage"]] + width[["stage"]], stage$y, x[["position"]], stage$y + 40))
  }
  parts <- c(parts, '</g>', '<g id="column-headings">',
             node("heading-policy-basis", x[["basis"]], heading_cy, width[["basis"]], 50, palette[["orange"]], "Policy basis", 30, "bold"),
             node("heading-governance-scale", x[["scale"]], heading_cy, width[["scale"]], 50, palette[["blue"]], "Governance scale", 30, "bold"),
             node("heading-decision-stage", x[["stage"]], heading_cy, width[["stage"]], 50, palette[["green"]], "Decision stage", 30, "bold"),
             node("heading-actor-and-user", x[["position"]], heading_cy, width[["position"]], 50, palette[["yellow"]], "Actor and user position", 30, "bold"),
             '</g>', '<g id="policy-hierarchy">',
             node("basis", x[["basis"]], basis_cy, width[["basis"]], 128, palette[["orange"]], c("Urban", "accessibility", "facility delivery"), 30),
             node("project-verification", x[["scale"]], project_cy, width[["scale"]], 100, palette[["blue"]], c("Project", "verification"), 31),
             node("city-oversight", x[["scale"]], city_cy, width[["scale"]], 100, palette[["blue"]], c("City-level", "oversight"), 31))
  for (stage in stages) {
    parts <- c(parts,
               node(stage$id, x[["stage"]], stage$y, width[["stage"]], 90, palette[["green"]], stage$label, 30),
               node(paste0(stage$id, "-actor"), x[["position"]], stage$y - 40, width[["position"]], 72, palette[["yellow"]], stage$actor, 28),
               node(paste0(stage$id, "-user"), x[["position"]], stage$y + 40, width[["position"]], 72, palette[["yellow"]], stage$user, 28))
  }
  trace <- data.frame(stage = vapply(stages, `[[`, "", "id"),
                      actor_or_decision_rule = vapply(stages, function(s) paste(s$actor, collapse = " "), ""),
                      user_position = vapply(stages, function(s) paste(s$user, collapse = " "), ""),
                      source = vapply(stages, `[[`, "", "sources"), stringsAsFactors = FALSE)
  metadata_csv <- paste(capture.output(write.csv(trace, row.names = FALSE)), collapse = "\n")
  parts <- c(parts, '</g>', paste0('<metadata id="source-mapping">', escape(metadata_csv), '</metadata>'), '</svg>')
  target <- file.path(svg_dir, "Figure_02_verification_chain.svg")
  writeLines(parts, target, useBytes = TRUE)
  write.csv(trace, file.path(data_dir, "Figure_02_source_mapping.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  audit <- data.frame(figure = 2, canvas_width = W, canvas_height = H,
                      minimum_font_px = 28, maximum_font_px = 31,
                      minimum_pt_at_170_mm = 28 / W * (170 / 25.4 * 72),
                      maximum_pt_at_170_mm = 31 / W * (170 / 25.4 * 72),
                      outer_horizontal_margin_px = 20,
                      policy_statements_unchanged = TRUE,
                      city_assessment_separate_from_project_gates = TRUE)
  write.csv(audit, file.path(script_dir, "Figure_02_change_audit.csv"), row.names = FALSE)
  message("Wrote ", normalizePath(target, winslash = "/"))
})




# -------- Latest revision: Figure_03_actor_authority.R --------

# Typography revision on the original publication canvas.
# Standalone: Rscript Figure_03_actor_authority.R
# Base R only. Keep data/ beside this R file.
local({
if (.Platform$OS.type == "windows") invisible(suppressWarnings(Sys.setlocale("LC_CTYPE", "English_United States.utf8")))
find_root <- function() {
  frames <- vapply(sys.frames(), function(fr) if (!is.null(fr$ofile)) as.character(fr$ofile) else "", character(1))
  frames <- frames[nzchar(frames)]
  if (length(frames)) return(dirname(normalizePath(tail(frames, 1), winslash="/", mustWork=TRUE)))
  argv <- grep("^--file=", commandArgs(FALSE), value=TRUE)
  if (length(argv)) return(dirname(normalizePath(sub("^--file=", "", argv[1]), winslash="/", mustWork=TRUE)))
  normalizePath(getwd(), winslash="/", mustWork=TRUE)
}
root <- bundle_root
dir.create(file.path(root, "svg"), showWarnings=FALSE, recursive=TRUE)
dir.create(file.path(root, "preview"), showWarnings=FALSE, recursive=TRUE)
dir.create(file.path(root, "audit"), showWarnings=FALSE, recursive=TRUE)
data_path <- file.path(root, "data", "A2_key_document_actor_matrix.csv")
d <- read.csv(data_path, fileEncoding="UTF-8", stringsAsFactors=FALSE, check.names=FALSE)
stem <- "Figure_03_actor_authority"
row_key <- "id";column_key <- "actor";value_key <- "score"
expected_rows <- 55L;expected_matrix_rows <- 11L;expected_range <- 0:3;limits <- c(0,3)
height_in <- 8.5;left <- .25;right <- .79;bottom <- .20;top <- .89
row_cex <- 1.13;column_cex <- 1.09;value_cex <- 1.31
bar_title <- "Authority score";show_plus <- FALSE
row_levels <- unique(d[[row_key]])
column_levels <- unique(d[[column_key]])
values <- matrix(NA_real_, length(row_levels), length(column_levels), dimnames=list(row_levels, column_levels))
for (i in seq_len(nrow(d))) values[match(d[[row_key]][i], row_levels), match(d[[column_key]][i], column_levels)] <- d[[value_key]][i]
stopifnot(!anyNA(values), all(values %in% expected_range), nrow(d)==expected_rows,
          nrow(values)==expected_matrix_rows, ncol(values)==5L)
row_labels <- c("Quality Regulation 279","Completion Filing\nRules 78","Drawing Review Rules 13\n(2018 revision)","GB 55019-2021","Accessibility Regulation\n2012","Accessibility Law 2023","Model-city Assessment\nRules 2022","Home-adaptation Quality\nNotice 2023","Urban Accessibility\nNotice 7 (2025)","Guangzhou 2020","Shenzhen 2021")
column_labels <- d$actor_label[match(column_levels,d$actor)]
stopifnot(length(row_labels)==nrow(values), length(column_labels)==ncol(values))
palette_anchors <- c("#FFFFD9", "#EDF8B1", "#C7E9B4", "#7FCDBB", "#41B6C4", "#1D91C0", "#225EA8", "#253494", "#081D58")
palette <- grDevices::colorRampPalette(palette_anchors, space="Lab")(256)
colour_for <- function(v) palette[pmax(1L,pmin(256L,round((v-limits[1])/diff(limits)*255)+1L))]
contrast <- function(fill) {
  rgb <- grDevices::col2rgb(fill)/255
  ifelse(0.2126*rgb[1,]+0.7152*rgb[2,]+0.0722*rgb[3,]<0.53,"white","#222222")
}
width_in <- 10.5
column_gap <- .015
colourbar_cex <- 1.08
print_scale <- (170/25.4)/width_in
geometry_audit <- NULL
draw_figure <- function() {
  par(mar=rep(0,4),family="Times New Roman",xaxs="i",yaxs="i",cex=1,ps=12)
  plot.new();plot.window(c(0,1),c(0,1),xaxs="i",yaxs="i")
  cell_w <- (right-left)/ncol(values);cell_h <- (top-bottom)/nrow(values)
  row_y <- top-(seq_len(nrow(values))-.5)*cell_h
  column_x <- left+(seq_len(ncol(values))-.5)*cell_w
  for(i in seq_len(nrow(values))) for(j in seq_len(ncol(values))) {
    x0 <- left+(j-1)*cell_w;y1 <- top-(i-1)*cell_h
    fill <- colour_for(values[i,j])
    rect(x0,y1-cell_h,x0+cell_w,y1,col=fill,border="white",lwd=.9)
    lab <- if(show_plus && values[i,j]>0) paste0("+",values[i,j]) else as.character(values[i,j])
    text(x0+cell_w/2,y1-cell_h/2,lab,cex=value_cex,font=2,col=contrast(fill))
  }
  rect(left,bottom,right,top,border="#4A4A4A",lwd=.8)
  text(left-.014,row_y,row_labels,adj=c(1,.5),cex=row_cex,font=3,xpd=NA)
  text(column_x-.006,bottom-column_gap,column_labels,srt=45,adj=c(1,.5),cex=column_cex,font=3,xpd=NA)
  bar_x <- .845;bar_w <- .022
  breaks <- seq(bottom,top,length.out=257)
  for(i in 1:256) rect(bar_x,breaks[i],bar_x+bar_w,breaks[i+1],col=palette[i],border=NA,xpd=NA)
  rect(bar_x,bottom,bar_x+bar_w,top,border="#333333",lwd=.8,xpd=NA)
  ticks <- seq(limits[1],limits[2]);tick_y <- bottom+(ticks-limits[1])/diff(limits)*(top-bottom)
  segments(bar_x+bar_w,tick_y,bar_x+.028,tick_y,col="#333333",xpd=NA)
  tick_labels <- ifelse(show_plus & ticks>0,paste0("+",ticks),as.character(ticks))
  text(bar_x+.034,tick_y,tick_labels,adj=c(0,.5),cex=colourbar_cex,xpd=NA)
  text(bar_x+.092,(bottom+top)/2,bar_title,srt=90,cex=colourbar_cex,font=3,xpd=NA)
  # Measure actual Cairo glyphs, not approximate character counts.
  row_height <- strheight(row_labels,units="inches",cex=row_cex,font=3)
  row_width <- strwidth(row_labels,units="inches",cex=row_cex,font=3)
  col_width <- strwidth(column_labels,units="inches",cex=column_cex,font=3)
  col_height <- strheight(column_labels,units="inches",cex=column_cex,font=3)
  rotated_y_extent <- (col_width+col_height)/sqrt(2)
  stopifnot(max(row_height)<cell_h*height_in*.94,
            max(row_width)<(left-.026)*width_in,
            max(rotated_y_extent)<(bottom-column_gap-.007)*height_in,
            12*min(row_cex,column_cex,colourbar_cex)*print_scale>=8,
            12*value_cex*print_scale>=10)
  geometry_audit <<- data.frame(figure=stem,width_mm=width_in*25.4,height_mm=height_in*25.4,display_width_mm=170,
       row_label_pt=12*row_cex,column_label_pt=12*column_cex,value_pt=12*value_cex,
       colourbar_label_pt=12*colourbar_cex,print_row_label_pt=12*row_cex*print_scale,
       print_column_label_pt=12*column_cex*print_scale,print_value_pt=12*value_cex*print_scale,
       print_colourbar_label_pt=12*colourbar_cex*print_scale,max_row_label_height_pt=max(row_height)*72,
       row_spacing_pt=cell_h*height_in*72,max_row_label_width_pt=max(row_width)*72,
       max_rotated_column_height_pt=max(rotated_y_extent)*72,
       bottom_margin_pt=bottom*height_in*72,stringsAsFactors=FALSE)
}
svg_path <- file.path(root,"svg",paste0(stem,".svg"))
grDevices::svg(svg_path,width=width_in,height=height_in,pointsize=12,family="Times New Roman",bg="white",onefile=TRUE)
draw_figure();dev.off()
grDevices::png(file.path(root,"preview",paste0(stem,".png")),width=width_in,height=height_in,units="in",res=180,type="cairo",bg="white",pointsize=12)
draw_figure();dev.off()
write.csv(geometry_audit,file.path(root,"audit",paste0(stem,"_typography.csv")),row.names=FALSE)
long_values <- expand.grid(row=seq_len(nrow(values)),column=seq_len(ncol(values)))
long_values$row_key <- row_levels[long_values$row]
long_values$column_key <- column_levels[long_values$column]
long_values$value <- values[cbind(long_values$row,long_values$column)]
write.csv(long_values,file.path(root,"audit",paste0(stem,"_matrix.csv")),row.names=FALSE)
xml_escape <- function(s) {
  s <- gsub("&","&amp;",s,fixed=TRUE);s <- gsub("<","&lt;",s,fixed=TRUE)
  gsub(">","&gt;",s,fixed=TRUE)
}
csv_data <- paste(readLines(data_path,encoding="UTF-8",warn=FALSE),collapse="\n")
metadata <- paste0('<metadata id="analysis-data"><rendering font-family="Times New Roman" glyphs="Cairo vector outlines" print-width-mm="170"/>',
   '<source path="data/',"A2_key_document_actor_matrix.csv",'">',xml_escape(csv_data),'</source></metadata>')
svg_lines <- readLines(svg_path,encoding="UTF-8",warn=FALSE)
svg_lines <- sub("</svg>",paste0(metadata,"</svg>"),svg_lines,fixed=TRUE)
writeLines(svg_lines,svg_path,useBytes=TRUE)
message("Wrote ",stem,". Labels at 170 mm: ",round(12*row_cex*print_scale,2),"/",round(12*column_cex*print_scale,2)," pt. Numbers: ",round(12*value_cex*print_scale,2)," pt.")
})




# -------- Latest revision: Figure_04_basis_maximum.R --------

# Typography revision on the original publication canvas.
# Standalone: Rscript Figure_04_basis_maximum.R
# Base R only. Keep data/ beside this R file.
local({
if (.Platform$OS.type == "windows") invisible(suppressWarnings(Sys.setlocale("LC_CTYPE", "English_United States.utf8")))
find_root <- function() {
  frames <- vapply(sys.frames(), function(fr) if (!is.null(fr$ofile)) as.character(fr$ofile) else "", character(1))
  frames <- frames[nzchar(frames)]
  if (length(frames)) return(dirname(normalizePath(tail(frames, 1), winslash="/", mustWork=TRUE)))
  argv <- grep("^--file=", commandArgs(FALSE), value=TRUE)
  if (length(argv)) return(dirname(normalizePath(sub("^--file=", "", argv[1]), winslash="/", mustWork=TRUE)))
  normalizePath(getwd(), winslash="/", mustWork=TRUE)
}
root <- bundle_root
dir.create(file.path(root, "svg"), showWarnings=FALSE, recursive=TRUE)
dir.create(file.path(root, "preview"), showWarnings=FALSE, recursive=TRUE)
dir.create(file.path(root, "audit"), showWarnings=FALSE, recursive=TRUE)
data_path <- file.path(root, "data", "A3_period_basis_max_mean_counts_evidence.csv")
d <- read.csv(data_path, fileEncoding="UTF-8", stringsAsFactors=FALSE, check.names=FALSE)
stem <- "Figure_04_basis_maximum"
row_key <- "basis";column_key <- "period";value_key <- "max_score"
expected_rows <- 25L;expected_matrix_rows <- 5L;expected_range <- 0:3;limits <- c(0,3)
height_in <- 7.6;left <- .25;right <- .79;bottom <- .20;top <- .89
row_cex <- 1.16;column_cex <- 1.17;value_cex <- 1.36
bar_title <- "Maximum score";show_plus <- FALSE
row_levels <- unique(d[[row_key]])
column_levels <- unique(d[[column_key]])
values <- matrix(NA_real_, length(row_levels), length(column_levels), dimnames=list(row_levels, column_levels))
for (i in seq_len(nrow(d))) values[match(d[[row_key]][i], row_levels), match(d[[column_key]][i], column_levels)] <- d[[value_key]][i]
stopifnot(!anyNA(values), all(values %in% expected_range), nrow(d)==expected_rows,
          nrow(values)==expected_matrix_rows, ncol(values)==5L)
row_labels <- c("Technical compliance","Procedural\ndocumentation","User experience","Responsibility\ntraceability","Public disclosure")
column_labels <- gsub("\\*","",d$period_label[match(column_levels,d$period)])
stopifnot(length(row_labels)==nrow(values), length(column_labels)==ncol(values))
palette_anchors <- c("#FFFFD9", "#EDF8B1", "#C7E9B4", "#7FCDBB", "#41B6C4", "#1D91C0", "#225EA8", "#253494", "#081D58")
palette <- grDevices::colorRampPalette(palette_anchors, space="Lab")(256)
colour_for <- function(v) palette[pmax(1L,pmin(256L,round((v-limits[1])/diff(limits)*255)+1L))]
contrast <- function(fill) {
  rgb <- grDevices::col2rgb(fill)/255
  ifelse(0.2126*rgb[1,]+0.7152*rgb[2,]+0.0722*rgb[3,]<0.53,"white","#222222")
}
width_in <- 9.5
column_gap <- .025
colourbar_cex <- 1.08
print_scale <- (170/25.4)/width_in
geometry_audit <- NULL
draw_figure <- function() {
  par(mar=rep(0,4),family="Times New Roman",xaxs="i",yaxs="i",cex=1,ps=12)
  plot.new();plot.window(c(0,1),c(0,1),xaxs="i",yaxs="i")
  cell_w <- (right-left)/ncol(values);cell_h <- (top-bottom)/nrow(values)
  row_y <- top-(seq_len(nrow(values))-.5)*cell_h
  column_x <- left+(seq_len(ncol(values))-.5)*cell_w
  for(i in seq_len(nrow(values))) for(j in seq_len(ncol(values))) {
    x0 <- left+(j-1)*cell_w;y1 <- top-(i-1)*cell_h
    fill <- colour_for(values[i,j])
    rect(x0,y1-cell_h,x0+cell_w,y1,col=fill,border="white",lwd=.9)
    lab <- if(show_plus && values[i,j]>0) paste0("+",values[i,j]) else as.character(values[i,j])
    text(x0+cell_w/2,y1-cell_h/2,lab,cex=value_cex,font=2,col=contrast(fill))
  }
  rect(left,bottom,right,top,border="#4A4A4A",lwd=.8)
  text(left-.014,row_y,row_labels,adj=c(1,.5),cex=row_cex,font=3,xpd=NA)
  text(column_x-.006,bottom-column_gap,column_labels,srt=45,adj=c(1,.5),cex=column_cex,font=3,xpd=NA)
  bar_x <- .845;bar_w <- .022
  breaks <- seq(bottom,top,length.out=257)
  for(i in 1:256) rect(bar_x,breaks[i],bar_x+bar_w,breaks[i+1],col=palette[i],border=NA,xpd=NA)
  rect(bar_x,bottom,bar_x+bar_w,top,border="#333333",lwd=.8,xpd=NA)
  ticks <- seq(limits[1],limits[2]);tick_y <- bottom+(ticks-limits[1])/diff(limits)*(top-bottom)
  segments(bar_x+bar_w,tick_y,bar_x+.028,tick_y,col="#333333",xpd=NA)
  tick_labels <- ifelse(show_plus & ticks>0,paste0("+",ticks),as.character(ticks))
  text(bar_x+.034,tick_y,tick_labels,adj=c(0,.5),cex=colourbar_cex,xpd=NA)
  text(bar_x+.092,(bottom+top)/2,bar_title,srt=90,cex=colourbar_cex,font=3,xpd=NA)
  # Measure actual Cairo glyphs, not approximate character counts.
  row_height <- strheight(row_labels,units="inches",cex=row_cex,font=3)
  row_width <- strwidth(row_labels,units="inches",cex=row_cex,font=3)
  col_width <- strwidth(column_labels,units="inches",cex=column_cex,font=3)
  col_height <- strheight(column_labels,units="inches",cex=column_cex,font=3)
  rotated_y_extent <- (col_width+col_height)/sqrt(2)
  stopifnot(max(row_height)<cell_h*height_in*.94,
            max(row_width)<(left-.026)*width_in,
            max(rotated_y_extent)<(bottom-column_gap-.007)*height_in,
            12*min(row_cex,column_cex,colourbar_cex)*print_scale>=8,
            12*value_cex*print_scale>=10)
  geometry_audit <<- data.frame(figure=stem,width_mm=width_in*25.4,height_mm=height_in*25.4,display_width_mm=170,
       row_label_pt=12*row_cex,column_label_pt=12*column_cex,value_pt=12*value_cex,
       colourbar_label_pt=12*colourbar_cex,print_row_label_pt=12*row_cex*print_scale,
       print_column_label_pt=12*column_cex*print_scale,print_value_pt=12*value_cex*print_scale,
       print_colourbar_label_pt=12*colourbar_cex*print_scale,max_row_label_height_pt=max(row_height)*72,
       row_spacing_pt=cell_h*height_in*72,max_row_label_width_pt=max(row_width)*72,
       max_rotated_column_height_pt=max(rotated_y_extent)*72,
       bottom_margin_pt=bottom*height_in*72,stringsAsFactors=FALSE)
}
svg_path <- file.path(root,"svg",paste0(stem,".svg"))
grDevices::svg(svg_path,width=width_in,height=height_in,pointsize=12,family="Times New Roman",bg="white",onefile=TRUE)
draw_figure();dev.off()
grDevices::png(file.path(root,"preview",paste0(stem,".png")),width=width_in,height=height_in,units="in",res=180,type="cairo",bg="white",pointsize=12)
draw_figure();dev.off()
write.csv(geometry_audit,file.path(root,"audit",paste0(stem,"_typography.csv")),row.names=FALSE)
long_values <- expand.grid(row=seq_len(nrow(values)),column=seq_len(ncol(values)))
long_values$row_key <- row_levels[long_values$row]
long_values$column_key <- column_levels[long_values$column]
long_values$value <- values[cbind(long_values$row,long_values$column)]
write.csv(long_values,file.path(root,"audit",paste0(stem,"_matrix.csv")),row.names=FALSE)
xml_escape <- function(s) {
  s <- gsub("&","&amp;",s,fixed=TRUE);s <- gsub("<","&lt;",s,fixed=TRUE)
  gsub(">","&gt;",s,fixed=TRUE)
}
csv_data <- paste(readLines(data_path,encoding="UTF-8",warn=FALSE),collapse="\n")
metadata <- paste0('<metadata id="analysis-data"><rendering font-family="Times New Roman" glyphs="Cairo vector outlines" print-width-mm="170"/>',
   '<source path="data/',"A3_period_basis_max_mean_counts_evidence.csv",'">',xml_escape(csv_data),'</source></metadata>')
svg_lines <- readLines(svg_path,encoding="UTF-8",warn=FALSE)
svg_lines <- sub("</svg>",paste0(metadata,"</svg>"),svg_lines,fixed=TRUE)
writeLines(svg_lines,svg_path,useBytes=TRUE)
message("Wrote ",stem,". Labels at 170 mm: ",round(12*row_cex*print_scale,2),"/",round(12*column_cex*print_scale,2)," pt. Numbers: ",round(12*value_cex*print_scale,2)," pt.")
})




# -------- Latest revision: Figure_05_participation_sankey.R --------

# Figure 5, original proportions: adjust only text size and line spacing.
# Original 1820 x 1100 canvas, all boxes and ribbon paths are retained exactly.
# Base R. Keep data/ beside this file. Rscript Figure_05_participation_sankey.R
local({
argv <- grep("^--file=", commandArgs(FALSE), value=TRUE)
frames <- vapply(sys.frames(), function(fr) if (!is.null(fr$ofile)) as.character(fr$ofile) else "", character(1))
frames <- frames[nzchar(frames)]
root <- bundle_root
svg_dir <- file.path(root,"svg"); dir.create(svg_dir, recursive=TRUE, showWarnings=FALSE)
audit_dir <- file.path(root,"audit"); dir.create(audit_dir, recursive=TRUE, showWarnings=FALSE)
flow_csv <- file.path(root,"data","Figure_05_participation_flows.csv")
flow_rows <- read.csv(flow_csv, stringsAsFactors=FALSE)
ink <- "#203746"
pastel_palette <- c(purple="#B69ACB", blue="#A8C8E8", green="#9DD3C7", yellow="#EAD18D", pink="#E8A4AE")

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
  line_step <- size * leading
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
node_totals <- rbind(data.frame(column="Entry",code=names(source_totals),n=as.integer(source_totals)),
                     data.frame(column="Downstream",code=names(outcome_totals),n=as.integer(outcome_totals)),
                     data.frame(column="Status",code=names(status_totals),n=as.integer(status_totals)))
expected_totals <- read.csv(file.path(root,"data","Figure_05_node_totals.csv"), stringsAsFactors=FALSE)
stopifnot(total_flow==136, identical(node_totals,expected_totals[,c("column","code","n")]),
          all(flow_rows$status==unname(status_from_outcome[flow_rows$outcome])))

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
          text_svg(x1 + node_w / 2, 60, "ENTRY MECHANISM", 28, "bold", "middle"),
          text_svg(x2 + node_w / 2, 60, "STRONGEST PROVISION", 28, "bold", "middle"),
          text_svg(x3 + node_w / 2, 60, "DECISION STATUS", 28, "bold", "middle"),
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
                                   leading = 31 / 27) {
  labels <- as.character(labels)
  line_step <- size * leading
  first_baseline <- y + h / 2 - line_step * (length(labels) - 1) / 2 + size * 0.32
  multiline_svg(x, first_baseline, labels, size, weight, anchor, col, leading)
}

for (k in seq_along(input_codes)) {
  code <- input_codes[k]; y <- source_pos$y[code]; h <- source_pos$h[code]
  lines <- c(strsplit(input_labels[k], "\n", fixed = TRUE)[[1]], paste0("documents = ", source_totals[code]))
  body <- c(body, rect_svg(x1, y, node_w, h, source_palette[code], source_palette[code], 1.1, 4, 0.72),
            centered_multiline_svg(x1 + node_w / 2, y, h, lines, 27))
}
for (k in seq_along(outcome_codes)) {
  code <- outcome_codes[k]
  if (outcome_totals[code] > 0) {
    y <- outcome_pos$y[code]; h <- outcome_pos$h[code]
    lines <- c(strsplit(outcome_labels[k], "\n", fixed = TRUE)[[1]], paste0("pairs = ", outcome_totals[code]))
    body <- c(body, rect_svg(x2, y, node_w, h, outcome_palette[code], outcome_palette[code], 1.1, 4, 0.62),
              centered_multiline_svg(x2 + node_w / 2, y, h, lines, 27))
  }
}
for (k in seq_along(status_codes)) {
  code <- status_codes[k]
  if (status_totals[code] > 0) {
    y <- status_pos$y[code]; h <- status_pos$h[code]
    lines <- c(strsplit(status_labels[k], "\n", fixed = TRUE)[[1]], paste0("pairs = ", status_totals[code]))
    body <- c(body, rect_svg(x3, y, node_w, h, status_palette[code], status_palette[code], 1.1, 4, 0.62),
              centered_multiline_svg(x3 + node_w / 2, y, h, lines, 27))
  }
}

# Explicit zero nodes preserve the central substantive result without a separate legend.
zero_y <- 940
zero_h <- 100
body <- c(body,
          rect_svg(x2, zero_y, node_w, zero_h, "#FFFFFF", outcome_palette["POWER"], 1.4, 4),
          centered_multiline_svg(x2 + node_w / 2, zero_y, zero_h,
                                 c("Public final", "decision power", "n = 0"), 27),
          rect_svg(x3, zero_y, node_w, zero_h, "#FFFFFF", status_palette["DECISIVE"], 1.4, 4),
          centered_multiline_svg(x3 + node_w / 2, zero_y, zero_h,
                                 c("Final public", "decision authority", "n = 0"), 27))

target <- file.path(svg_dir,"Figure_05_participation_sankey.svg")
svg <- svg_document(W,H,"From user input to institutional consequence",
  "Three-column Sankey diagram of document-level user entry mechanisms, strongest downstream provisions and institutional decision status.", body)
data_text <- paste(readLines(flow_csv,encoding="UTF-8",warn=FALSE),collapse="\n")
svg <- c(head(svg,-1L),paste0('<metadata id="analysis-data" data-source="data/Figure_05_participation_flows.csv"><![CDATA[',data_text,']]></metadata>'),tail(svg,1L))
writeLines(svg,target,useBytes=TRUE)
writeLines(c("Actual R run: original 1820 x 1100 canvas retained.",
  "All three headings: 22 to 28 px; node labels: 17.5-20 to 27 px.",
  sprintf("All 136 document-mechanism pairs retained. Common ribbon scale: %.8f px per pair.",shared_scale)),
  file.path(audit_dir,"Figure_05_typography.txt"))
message("Wrote ",target,"; original 1820 x 1100; type 27-28 px.")
})




# -------- Latest revision: Figure_06_local_difference.R --------

# Typography revision on the original publication canvas.
# Standalone: Rscript Figure_06_local_difference.R
# Base R only. Keep data/ beside this R file.
local({
if (.Platform$OS.type == "windows") invisible(suppressWarnings(Sys.setlocale("LC_CTYPE", "English_United States.utf8")))
find_root <- function() {
  frames <- vapply(sys.frames(), function(fr) if (!is.null(fr$ofile)) as.character(fr$ofile) else "", character(1))
  frames <- frames[nzchar(frames)]
  if (length(frames)) return(dirname(normalizePath(tail(frames, 1), winslash="/", mustWork=TRUE)))
  argv <- grep("^--file=", commandArgs(FALSE), value=TRUE)
  if (length(argv)) return(dirname(normalizePath(sub("^--file=", "", argv[1]), winslash="/", mustWork=TRUE)))
  normalizePath(getwd(), winslash="/", mustWork=TRUE)
}
root <- bundle_root
dir.create(file.path(root, "svg"), showWarnings=FALSE, recursive=TRUE)
dir.create(file.path(root, "preview"), showWarnings=FALSE, recursive=TRUE)
dir.create(file.path(root, "audit"), showWarnings=FALSE, recursive=TRUE)
data_path <- file.path(root, "data", "A5_local_minus_2023_law_scores.csv")
d <- read.csv(data_path, fileEncoding="UTF-8", stringsAsFactors=FALSE, check.names=FALSE)
stem <- "Figure_06_local_difference"
row_key <- "place";column_key <- "dimension";value_key <- "difference"
expected_rows <- 50L;expected_matrix_rows <- 10L;expected_range <- -2:2;limits <- c(-2,2)
stopifnot(identical(unique(d$baseline_id),"N27"))
height_in <- 8.5;left <- .21;right <- .79;bottom <- .20;top <- .89
row_cex <- 1.13;column_cex <- 1.06;value_cex <- 1.33
bar_title <- "Local - 2023 law";show_plus <- TRUE
row_levels <- unique(d[[row_key]])
column_levels <- unique(d[[column_key]])
values <- matrix(NA_real_, length(row_levels), length(column_levels), dimnames=list(row_levels, column_levels))
for (i in seq_len(nrow(d))) values[match(d[[row_key]][i], row_levels), match(d[[column_key]][i], column_levels)] <- d[[value_key]][i]
stopifnot(!anyNA(values), all(values %in% expected_range), nrow(d)==expected_rows,
          nrow(values)==expected_matrix_rows, ncol(values)==5L)
row_labels <- row_levels
column_labels <- d$dimension_label[match(column_levels,d$dimension)]
stopifnot(length(row_labels)==nrow(values), length(column_labels)==ncol(values))
palette_anchors <- c("#FFFFD9", "#EDF8B1", "#C7E9B4", "#7FCDBB", "#41B6C4", "#1D91C0", "#225EA8", "#253494", "#081D58")
palette <- grDevices::colorRampPalette(palette_anchors, space="Lab")(256)
colour_for <- function(v) palette[pmax(1L,pmin(256L,round((v-limits[1])/diff(limits)*255)+1L))]
contrast <- function(fill) {
  rgb <- grDevices::col2rgb(fill)/255
  ifelse(0.2126*rgb[1,]+0.7152*rgb[2,]+0.0722*rgb[3,]<0.53,"white","#222222")
}
width_in <- 10.5
column_gap <- .015
colourbar_cex <- 1.08
print_scale <- (170/25.4)/width_in
geometry_audit <- NULL
draw_figure <- function() {
  par(mar=rep(0,4),family="Times New Roman",xaxs="i",yaxs="i",cex=1,ps=12)
  plot.new();plot.window(c(0,1),c(0,1),xaxs="i",yaxs="i")
  cell_w <- (right-left)/ncol(values);cell_h <- (top-bottom)/nrow(values)
  row_y <- top-(seq_len(nrow(values))-.5)*cell_h
  column_x <- left+(seq_len(ncol(values))-.5)*cell_w
  for(i in seq_len(nrow(values))) for(j in seq_len(ncol(values))) {
    x0 <- left+(j-1)*cell_w;y1 <- top-(i-1)*cell_h
    fill <- colour_for(values[i,j])
    rect(x0,y1-cell_h,x0+cell_w,y1,col=fill,border="white",lwd=.9)
    lab <- if(show_plus && values[i,j]>0) paste0("+",values[i,j]) else as.character(values[i,j])
    text(x0+cell_w/2,y1-cell_h/2,lab,cex=value_cex,font=2,col=contrast(fill))
  }
  rect(left,bottom,right,top,border="#4A4A4A",lwd=.8)
  text(left-.014,row_y,row_labels,adj=c(1,.5),cex=row_cex,font=3,xpd=NA)
  text(column_x-.006,bottom-column_gap,column_labels,srt=45,adj=c(1,.5),cex=column_cex,font=3,xpd=NA)
  bar_x <- .845;bar_w <- .022
  breaks <- seq(bottom,top,length.out=257)
  for(i in 1:256) rect(bar_x,breaks[i],bar_x+bar_w,breaks[i+1],col=palette[i],border=NA,xpd=NA)
  rect(bar_x,bottom,bar_x+bar_w,top,border="#333333",lwd=.8,xpd=NA)
  ticks <- seq(limits[1],limits[2]);tick_y <- bottom+(ticks-limits[1])/diff(limits)*(top-bottom)
  segments(bar_x+bar_w,tick_y,bar_x+.028,tick_y,col="#333333",xpd=NA)
  tick_labels <- ifelse(show_plus & ticks>0,paste0("+",ticks),as.character(ticks))
  text(bar_x+.034,tick_y,tick_labels,adj=c(0,.5),cex=colourbar_cex,xpd=NA)
  text(bar_x+.092,(bottom+top)/2,bar_title,srt=90,cex=colourbar_cex,font=3,xpd=NA)
  # Measure actual Cairo glyphs, not approximate character counts.
  row_height <- strheight(row_labels,units="inches",cex=row_cex,font=3)
  row_width <- strwidth(row_labels,units="inches",cex=row_cex,font=3)
  col_width <- strwidth(column_labels,units="inches",cex=column_cex,font=3)
  col_height <- strheight(column_labels,units="inches",cex=column_cex,font=3)
  rotated_y_extent <- (col_width+col_height)/sqrt(2)
  stopifnot(max(row_height)<cell_h*height_in*.94,
            max(row_width)<(left-.026)*width_in,
            max(rotated_y_extent)<(bottom-column_gap-.007)*height_in,
            12*min(row_cex,column_cex,colourbar_cex)*print_scale>=8,
            12*value_cex*print_scale>=10)
  geometry_audit <<- data.frame(figure=stem,width_mm=width_in*25.4,height_mm=height_in*25.4,display_width_mm=170,
       row_label_pt=12*row_cex,column_label_pt=12*column_cex,value_pt=12*value_cex,
       colourbar_label_pt=12*colourbar_cex,print_row_label_pt=12*row_cex*print_scale,
       print_column_label_pt=12*column_cex*print_scale,print_value_pt=12*value_cex*print_scale,
       print_colourbar_label_pt=12*colourbar_cex*print_scale,max_row_label_height_pt=max(row_height)*72,
       row_spacing_pt=cell_h*height_in*72,max_row_label_width_pt=max(row_width)*72,
       max_rotated_column_height_pt=max(rotated_y_extent)*72,
       bottom_margin_pt=bottom*height_in*72,stringsAsFactors=FALSE)
}
svg_path <- file.path(root,"svg",paste0(stem,".svg"))
grDevices::svg(svg_path,width=width_in,height=height_in,pointsize=12,family="Times New Roman",bg="white",onefile=TRUE)
draw_figure();dev.off()
grDevices::png(file.path(root,"preview",paste0(stem,".png")),width=width_in,height=height_in,units="in",res=180,type="cairo",bg="white",pointsize=12)
draw_figure();dev.off()
write.csv(geometry_audit,file.path(root,"audit",paste0(stem,"_typography.csv")),row.names=FALSE)
long_values <- expand.grid(row=seq_len(nrow(values)),column=seq_len(ncol(values)))
long_values$row_key <- row_levels[long_values$row]
long_values$column_key <- column_levels[long_values$column]
long_values$value <- values[cbind(long_values$row,long_values$column)]
write.csv(long_values,file.path(root,"audit",paste0(stem,"_matrix.csv")),row.names=FALSE)
xml_escape <- function(s) {
  s <- gsub("&","&amp;",s,fixed=TRUE);s <- gsub("<","&lt;",s,fixed=TRUE)
  gsub(">","&gt;",s,fixed=TRUE)
}
csv_data <- paste(readLines(data_path,encoding="UTF-8",warn=FALSE),collapse="\n")
metadata <- paste0('<metadata id="analysis-data"><rendering font-family="Times New Roman" glyphs="Cairo vector outlines" print-width-mm="170"/>',
   '<source path="data/',"A5_local_minus_2023_law_scores.csv",'">',xml_escape(csv_data),'</source></metadata>')
svg_lines <- readLines(svg_path,encoding="UTF-8",warn=FALSE)
svg_lines <- sub("</svg>",paste0(metadata,"</svg>"),svg_lines,fixed=TRUE)
writeLines(svg_lines,svg_path,useBytes=TRUE)
message("Wrote ",stem,". Labels at 170 mm: ",round(12*row_cex*print_scale,2),"/",round(12*column_cex*print_scale,2)," pt. Numbers: ",round(12*value_cex*print_scale,2)," pt.")
})




# -------- Latest revision: Figure_S3_actor_combinations.R --------

# Enlarge type only: original SVG canvas and non-text graphics remain identical.
# Base R. Keep source_svg/ and data/ beside this script.
# The source SVG embeds the original numeric data; no plot geometry is rebuilt.
local({
  frames <- vapply(sys.frames(),function(fr) if(!is.null(fr$ofile)) as.character(fr$ofile) else "",character(1))
  frames <- frames[nzchar(frames)]
  file_arg <- grep("^--file=",commandArgs(FALSE),value=TRUE)
  root <- bundle_root
  stem <- "Figure_S3_actor_combinations"
  dir.create(file.path(root,"svg"),recursive=TRUE,showWarnings=FALSE)
  dir.create(file.path(root,"audit"),recursive=TRUE,showWarnings=FALSE)
  source <- paste(readLines(file.path(root,"source_svg",paste0(stem,".svg")),encoding="UTF-8",warn=FALSE),collapse="\n")
  params <- read.csv(file.path(root,"data",paste0(stem,"_typography.csv")),stringsAsFactors=FALSE,check.names=FALSE)
  pattern <- "<text\\b[^>]*>[^<]*</text>"
  matches <- gregexpr(pattern,source,perl=TRUE)
  texts <- regmatches(source,matches)[[1]]
  stopifnot(length(texts)==nrow(params),identical(as.integer(params$index),seq_along(texts)),all(params$new_size>params$old_size))
  attr_value <- function(s,key) sub(paste0('.*\\b',key,'="([^"]*)".*'),'\\1',s,perl=TRUE)
  set_attr <- function(s,key,value) sub(paste0('\\b',key,'="[^"]*"'),paste0(key,'="',formatC(value,format="f",digits=2,decimal.mark="."),'"'),s,perl=TRUE)
  for(i in seq_along(texts)) {
    stopifnot(abs(as.numeric(attr_value(texts[i],"font-size"))-params$old_size[i])<.001,
      abs(as.numeric(attr_value(texts[i],"x"))-params$old_x[i])<.001,
      abs(as.numeric(attr_value(texts[i],"y"))-params$old_y[i])<.001)
    texts[i] <- set_attr(texts[i],"font-size",params$new_size[i])
    texts[i] <- set_attr(texts[i],"x",params$new_x[i])
    texts[i] <- set_attr(texts[i],"y",params$new_y[i])
  }
  result <- source
  regmatches(result,matches) <- list(texts)
  # Removing text nodes must leave the source and output byte-identical.
  stopifnot(identical(gsub(pattern,"",source,perl=TRUE),gsub(pattern,"",result,perl=TRUE)))
  target <- file.path(root,"svg",paste0(stem,".svg"))
  writeLines(result,target,useBytes=TRUE)
  report <- data.frame(figure=stem,text_nodes=length(texts),minimum_type_scale=min(params$new_size/params$old_size),maximum_type_scale=max(params$new_size/params$old_size),all_non_text_SVG_content_identical=TRUE)
  write.csv(report,file.path(root,"audit",paste0(stem,"_R_validation.csv")),row.names=FALSE)
  message("Wrote ",stem,"; original geometry retained; font scale ",round(report$minimum_type_scale,3),"-",round(report$maximum_type_scale,3))
})




# -------- Latest revision: Figure_S4_actor_basis_network.R --------

# Enlarge type only: original SVG canvas and non-text graphics remain identical.
# Base R. Keep source_svg/ and data/ beside this script.
# The source SVG embeds the original numeric data; no plot geometry is rebuilt.
local({
  frames <- vapply(sys.frames(),function(fr) if(!is.null(fr$ofile)) as.character(fr$ofile) else "",character(1))
  frames <- frames[nzchar(frames)]
  file_arg <- grep("^--file=",commandArgs(FALSE),value=TRUE)
  root <- bundle_root
  stem <- "Figure_S4_actor_basis_network"
  dir.create(file.path(root,"svg"),recursive=TRUE,showWarnings=FALSE)
  dir.create(file.path(root,"audit"),recursive=TRUE,showWarnings=FALSE)
  source <- paste(readLines(file.path(root,"source_svg",paste0(stem,".svg")),encoding="UTF-8",warn=FALSE),collapse="\n")
  params <- read.csv(file.path(root,"data",paste0(stem,"_typography.csv")),stringsAsFactors=FALSE,check.names=FALSE)
  pattern <- "<text\\b[^>]*>[^<]*</text>"
  matches <- gregexpr(pattern,source,perl=TRUE)
  texts <- regmatches(source,matches)[[1]]
  stopifnot(length(texts)==nrow(params),identical(as.integer(params$index),seq_along(texts)),all(params$new_size>params$old_size))
  attr_value <- function(s,key) sub(paste0('.*\\b',key,'="([^"]*)".*'),'\\1',s,perl=TRUE)
  set_attr <- function(s,key,value) sub(paste0('\\b',key,'="[^"]*"'),paste0(key,'="',formatC(value,format="f",digits=2,decimal.mark="."),'"'),s,perl=TRUE)
  for(i in seq_along(texts)) {
    stopifnot(abs(as.numeric(attr_value(texts[i],"font-size"))-params$old_size[i])<.001,
      abs(as.numeric(attr_value(texts[i],"x"))-params$old_x[i])<.001,
      abs(as.numeric(attr_value(texts[i],"y"))-params$old_y[i])<.001)
    texts[i] <- set_attr(texts[i],"font-size",params$new_size[i])
    texts[i] <- set_attr(texts[i],"x",params$new_x[i])
    texts[i] <- set_attr(texts[i],"y",params$new_y[i])
  }
  result <- source
  regmatches(result,matches) <- list(texts)
  # Removing text nodes must leave the source and output byte-identical.
  stopifnot(identical(gsub(pattern,"",source,perl=TRUE),gsub(pattern,"",result,perl=TRUE)))
  target <- file.path(root,"svg",paste0(stem,".svg"))
  writeLines(result,target,useBytes=TRUE)
  report <- data.frame(figure=stem,text_nodes=length(texts),minimum_type_scale=min(params$new_size/params$old_size),maximum_type_scale=max(params$new_size/params$old_size),all_non_text_SVG_content_identical=TRUE)
  write.csv(report,file.path(root,"audit",paste0(stem,"_R_validation.csv")),row.names=FALSE)
  message("Wrote ",stem,"; original geometry retained; font scale ",round(report$minimum_type_scale,3),"-",round(report$maximum_type_scale,3))
})




# -------- Latest revision: Figure_S5_cluster_heatmap.R --------

# Font-only revision of the rectangular clustered heatmap.
# All original graphic geometry, colours, matrix values and SVG dimensions stay fixed.
# Keep source_svg/ beside this R file. Base R only.
local({
  frames <- vapply(sys.frames(),function(fr) if(!is.null(fr$ofile)) as.character(fr$ofile) else "",character(1))
  frames <- frames[nzchar(frames)]
  argv <- grep("^--file=",commandArgs(FALSE),value=TRUE)
  root <- bundle_root
  source_path <- file.path(root,"source_svg","Figure_S5_cluster_heatmap.svg")
  svg_path <- file.path(root,"svg","Figure_S5_cluster_heatmap.svg")
  dir.create(dirname(svg_path),recursive=TRUE,showWarnings=FALSE)
  dir.create(file.path(root,"audit"),recursive=TRUE,showWarnings=FALSE)
  original <- paste(readLines(source_path,encoding="UTF-8",warn=FALSE),collapse="\n")
  matches <- gregexpr('<text\\b[^>]*>[^<]*</text>',original,perl=TRUE)
  texts <- regmatches(original,matches)[[1]]
  stopifnot(length(texts)==685L)
  attribute <- function(text,name) {
    m <- regexec(paste0(' ',name,'="([^"]*)"'),text,perl=TRUE)
    v <- regmatches(text,m)[[1]]
    if(length(v)<2L) NA_character_ else v[2]
  }
  set_attribute <- function(text,name,value) {
    old <- paste0(' ',name,'="[^"]*"')
    if(grepl(old,text,perl=TRUE)) sub(old,paste0(' ',name,'="',value,'"'),text,perl=TRUE)
    else sub('<text ',paste0('<text ',name,'="',value,'" '),text,fixed=TRUE)
  }
  old_size <- as.numeric(vapply(texts,attribute,"",name="font-size"))
  labels <- sub('^<text[^>]*>([^<]*)</text>$','\\1',texts,perl=TRUE)
  new <- texts
  audit <- vector("list",length(texts))
  for(i in seq_along(texts)) {
    x <- as.numeric(attribute(texts[i],"x")); y <- as.numeric(attribute(texts[i],"y"))
    size <- old_size[i]; kind <- "column heading"
    if(y>=250 && y<=1700) {
      # The original 24.5 px row spacing limits vertical enlargement.
      # Increase ID weight and use 24 px for row IDs and cell values.
      size <- 24;kind <- if(x==500) "row ID" else "matrix value"
      new[i] <- set_attribute(new[i],"y",sprintf("%.2f",y+1.2))
      if(x==500) {
        new[i] <- set_attribute(new[i],"font-weight","bold")
        new[i] <- set_attribute(new[i],"x","510.00")
      }
    } else if(y>=1700) {
      kind <- "legend";size <- if(y>1800) 18 else 20
      if(labels[i] %in% c("Ordinal strength","Period strip"))
        new[i] <- set_attribute(new[i],"y","1740.00")
    } else if(y==230) {
      kind <- "annotation strip heading";size <- 18
      sx <- c(Period=1562,Level=1609,Strict=1657)[labels[i]]
      new[i] <- set_attribute(new[i],"x",sprintf("%.2f",sx))
      new[i] <- set_attribute(new[i],"y","224.00")
      new[i] <- set_attribute(new[i],"transform",sprintf("rotate(-55.00 %.2f 224.00)",sx))
    } else {
      size <- if(size==14) 18 else 20
      # Tiny text-only horizontal adjustments retain all words while keeping
      # the wider bold headers visibly separated over the unchanged columns.
      header_x <- c(`570`=540.64,`670`=651.79,`770`=769.43,`870`=872.25,`970`=946.85,
                    `1070`=1033.75,`1170`=1155.27,`1270`=1270.20,`1370`=1391.37,`1470`=1495.32)
      new[i] <- set_attribute(new[i],"x",sprintf("%.2f",header_x[as.character(x)]))
    }
    new[i] <- set_attribute(new[i],"font-size",sprintf("%.2f",size))
    audit[[i]] <- data.frame(label=labels[i],kind=kind,before_px=old_size[i],after_px=size,
                             size_ratio=size/old_size[i],stringsAsFactors=FALSE)
  }
  regmatches(original,matches) <- list(new)
  writeLines(original,svg_path,useBytes=TRUE)
  write.csv(do.call(rbind,audit),file.path(root,"audit","Figure_S5_type_changes.csv"),row.names=FALSE,fileEncoding="UTF-8")
  message("Wrote Figure_S5_cluster_heatmap.svg; original 1840 x 1880 and all graphic shapes retained.")
})




# -------- Latest revision: Figure_S7_robustness.R --------

# Enlarge type only: original SVG canvas and non-text graphics remain identical.
# Base R. Keep source_svg/ and data/ beside this script.
# The source SVG embeds the original numeric data; no plot geometry is rebuilt.
local({
  frames <- vapply(sys.frames(),function(fr) if(!is.null(fr$ofile)) as.character(fr$ofile) else "",character(1))
  frames <- frames[nzchar(frames)]
  file_arg <- grep("^--file=",commandArgs(FALSE),value=TRUE)
  root <- bundle_root
  stem <- "Figure_S7_robustness"
  dir.create(file.path(root,"svg"),recursive=TRUE,showWarnings=FALSE)
  dir.create(file.path(root,"audit"),recursive=TRUE,showWarnings=FALSE)
  source <- paste(readLines(file.path(root,"source_svg",paste0(stem,".svg")),encoding="UTF-8",warn=FALSE),collapse="\n")
  params <- read.csv(file.path(root,"data",paste0(stem,"_typography.csv")),stringsAsFactors=FALSE,check.names=FALSE)
  pattern <- "<text\\b[^>]*>[^<]*</text>"
  matches <- gregexpr(pattern,source,perl=TRUE)
  texts <- regmatches(source,matches)[[1]]
  stopifnot(length(texts)==nrow(params),identical(as.integer(params$index),seq_along(texts)),all(params$new_size>params$old_size))
  attr_value <- function(s,key) sub(paste0('.*\\b',key,'="([^"]*)".*'),'\\1',s,perl=TRUE)
  set_attr <- function(s,key,value) sub(paste0('\\b',key,'="[^"]*"'),paste0(key,'="',formatC(value,format="f",digits=2,decimal.mark="."),'"'),s,perl=TRUE)
  for(i in seq_along(texts)) {
    stopifnot(abs(as.numeric(attr_value(texts[i],"font-size"))-params$old_size[i])<.001,
      abs(as.numeric(attr_value(texts[i],"x"))-params$old_x[i])<.001,
      abs(as.numeric(attr_value(texts[i],"y"))-params$old_y[i])<.001)
    texts[i] <- set_attr(texts[i],"font-size",params$new_size[i])
    texts[i] <- set_attr(texts[i],"x",params$new_x[i])
    texts[i] <- set_attr(texts[i],"y",params$new_y[i])
  }
  result <- source
  regmatches(result,matches) <- list(texts)
  # Removing text nodes must leave the source and output byte-identical.
  stopifnot(identical(gsub(pattern,"",source,perl=TRUE),gsub(pattern,"",result,perl=TRUE)))
  target <- file.path(root,"svg",paste0(stem,".svg"))
  writeLines(result,target,useBytes=TRUE)
  report <- data.frame(figure=stem,text_nodes=length(texts),minimum_type_scale=min(params$new_size/params$old_size),maximum_type_scale=max(params$new_size/params$old_size),all_non_text_SVG_content_identical=TRUE)
  write.csv(report,file.path(root,"audit",paste0(stem,"_R_validation.csv")),row.names=FALSE)
  message("Wrote ",stem,"; original geometry retained; font scale ",round(report$minimum_type_scale,3),"-",round(report$maximum_type_scale,3))
})




# -------- Latest revision: Maps_01_S2a_to_S2e_FontOnly.R --------

# Six maps with larger Times New Roman text, original graphics unchanged.
# Base R only. Keep source_svg/ beside this file.
# The templates retain the audited original map data and vector paths; this script
# changes only text font-size and, where line spacing requires, text coordinates.
# Run: Rscript Maps_01_S2a_to_S2e_FontOnly.R
local({
  frames <- vapply(sys.frames(),function(fr) if(!is.null(fr$ofile)) as.character(fr$ofile) else "",character(1))
  frames <- frames[nzchar(frames)]
  args <- grep("^--file=",commandArgs(FALSE),value=TRUE)
  root <- bundle_root
  input <- file.path(root,"source_svg")
  files <- list.files(input,pattern="^Figure_(01_|S2[a-e]_).*\\.svg$",full.names=TRUE)
  stopifnot(length(files)==6L)
  dir.create(file.path(root,"svg"),recursive=TRUE,showWarnings=FALSE)
  dir.create(file.path(root,"audit"),recursive=TRUE,showWarnings=FALSE)
  attribute <- function(line,key) {
    m <- regexec(paste0(key,'="([^" ]+)"'),line,perl=TRUE)
    hit <- regmatches(line,m)[[1]]
    if(length(hit)>1L) hit[2] else NA_character_
  }
  change <- function(line,key,value) sub(paste0(key,'="[^"]*"'),paste0(key,'="',value,'"'),line,perl=TRUE)
  audit <- list()
  for(file in files) {
    original <- readLines(file,encoding="UTF-8",warn=FALSE)
    lines <- original
    is_main <- startsWith(basename(file),"Figure_01")
    idx <- which(grepl("<text\\s",lines,perl=TRUE))
    changes <- list(); ntext <- 0L
    for(k in seq_along(idx)) {
      i <- idx[k]
      locations <- gregexpr("<text\\s[^>]*>.*?</text>",original[i],perl=TRUE)
      tokens <- regmatches(original[i],locations)[[1]]
      edited <- tokens
      for(j in seq_along(tokens)) {
      line <- tokens[j]; before <- line; ntext <- ntext+1L
      label <- sub("^.*?<text[^>]*>(.*?)</text>.*$","\\1",line,perl=TRUE)
      oldsize <- as.numeric(attribute(line,"font-size"))
      x <- as.numeric(attribute(line,"x"));y <- as.numeric(attribute(line,"y"))
      size <- oldsize*1.4
      # The two-column inventory keeps its original row spacing and x positions.
      # A 20 px inventory label fits its fixed 24/25 px row spacing.
      table <- if(is_main) (x<450 && y>=520 && y<=820) else (x<450 && y>=420 && y<810)
      if(table) size <- 20
      if(grepl("^DOCUMENTS BY",label)) size <- if(is_main) 21 else 22
      if(grepl("^Included subnational",label)) size <- if(is_main) 26 else 24
      if(grepl("^No included",label)) size <- 22
      if(label %in% c("Pacific","Ocean")) size <- if(is_main) 18 else 17
      if(grepl("^(Beijing|Tianjin|Shanghai) \\(",label)) size <- 22
      if(label=="N") size <- 26
      if(label=="LOCATION") size <- 24
      if(label=="China") size <- 28
      if(label=="South China Sea") size <- 19
      if(grepl("^[0-9]$",label) && x>800 && y<830 && oldsize>=18) size <- 27
      # Preserve the original centre in two narrow provinces. These four glyphs
      # use 24 rather than 27 px, retaining enlargement over 18/19 px while
      # leaving breathing room to the existing province boundary.
      narrow_label <- label=="1" && (
        (is_main && ((abs(x-1080.55)<.01 && abs(y-480.38)<.01) ||
                     (abs(x-1090.23)<.01 && abs(y-627.13)<.01))) ||
        (startsWith(basename(file),"Figure_S2c_") && abs(x-1066.63)<.01 && abs(y-405.81)<.01) ||
        (startsWith(basename(file),"Figure_S2d_") && abs(x-1076.10)<.01 && abs(y-548.17)<.01))
      if(narrow_label) size <- 24
      if(grepl("^[0-9,.]+( km)?$",label) && x>450 && y %in% c(787,816)) size <- 19
      # Preserve the centre of the locator's two-line ocean label while opening
      # the gap only between those two text baselines; no map geometry moves.
      if(label=="Pacific") y <- y-3
      if(label=="Ocean") y <- y+3
      line <- change(line,"font-size",sprintf("%.2f",size))
      if(y != as.numeric(attribute(before,"y"))) line <- change(line,"y",sprintf("%.2f",y))
      edited[j] <- line
      changes[[ntext]] <- data.frame(file=basename(file),text_index=ntext,label=label,
        font_before=oldsize,font_after=size,font_ratio=size/oldsize,
        x_before=as.numeric(attribute(before,"x")),x_after=x,
        y_before=as.numeric(attribute(before,"y")),y_after=y,stringsAsFactors=FALSE)
      }
      value <- original[i];regmatches(value,locations) <- list(edited);lines[i] <- value
    }
    # Each source line without text is copied verbatim, preserving all polygons,
    # geographic projections, colours, leader lines, scale bars and legend boxes.
    stopifnot(identical(original[-idx],lines[-idx]))
    outfile <- file.path(root,"svg",basename(file))
    writeLines(lines,outfile,useBytes=TRUE)
    audit[[basename(file)]] <- do.call(rbind,changes)
    message("Wrote ",basename(file),"; ",ntext," labels enlarged, original graphics retained.")
  }
  write.csv(do.call(rbind,audit),file.path(root,"audit","Maps_font_size_changes.csv"),row.names=FALSE,fileEncoding="UTF-8")
})




  # Confirm complete numbered output, including the optional circular alternative.
  final_manifest <- read.csv(file.path(bundle_root,"figure_manifest.csv"),stringsAsFactors=FALSE)
  stopifnot(nrow(final_manifest)==18L,!anyDuplicated(final_manifest$file))
  final_manifest$latest_revision <- ifelse(final_manifest$file %in% typography_inputs,"2026-11-07 larger type, original geometry",ifelse(grepl("^Figure_0[2-6]_",final_manifest$file),"2026-11-07 latest reviewed main-figure layout","2026-11-05 checked data and figure"))
  final_manifest$source_input <- final_manifest$source
  final_manifest$source <- NULL
  for(n in final_manifest$file) stopifnot(file.exists(file.path(bundle_root,"svg",n)),file.info(file.path(bundle_root,"svg",n))$size>100)
  write.csv(final_manifest,file.path(bundle_root,"figure_manifest_latest.csv"),row.names=FALSE,fileEncoding="UTF-8")
  writeLines(capture.output(sessionInfo()),file.path(bundle_root,"audit","R_sessionInfo.txt"))
  message("Completed: 18 latest SVGs in svg/. All input and numerical checks passed.")
})
