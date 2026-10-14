## plot_coverage_map(): one sampled/not-sampled pie per ICES area, or -
## with `group` - a labelled box per area holding a grid of pies (one
## per group level), overlapping boxes nudged apart. ggplot only;
## auto-build from raw catch or take a coverage table; biology via
## distributions.

if (!requireNamespace("maps", quietly = TRUE)) {
  cat("maps package not installed - skipping\nDONE  0 failure(s)\n")
} else {

FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}

raw <- data.frame(
  catchCategory  = rep(c("Lan", "Lan", "Dis"), 6),
  total          = c(500, 300, 20, 200, 50, 8, 90, 40, 3,
                     150, 70, 6, 30, 10, 1, 60, 25, 2),
  domainCatchDis = rep(c("d", "", "d"), 6),
  areaValue = rep(c("27.4.a", "27.4.b", "27.7.d", "27.7.e", "27.3.a.20",
                    "99.9.z"), each = 3),
  vesselFlagCountry = rep(c("BE", "FR"), 9),
  seasonValue       = rep(1:3, 6),
  metier4   = rep(c("OTB", "TBB", "GNS"), 6),
  stringsAsFactors = FALSE)

is_gg <- function(expr) tryCatch({
  x <- suppressWarnings(force(expr))
  inherits(x, "ggplot")
}, error = function(e) conditionMessage(e))

## ---- 1. centroid lookup ------------------------------------------
## one pie per area the map knows: the code with or without its "27.", and one it does
## not know is named in a warning and left off
pies <- function(p) {
  l <- p$layers[[which(vapply(p$layers, function(l) all(c("pid", "slice") %in% names(l$data)), NA))[1]]]
  length(unique(l$data$pid))
}
codes <- data.frame(catchCategory = c("Lan", "Dis"), total = c(100, 10), domainCatchDis = "d",
                    stringsAsFactors = FALSE)
codes <- do.call(rbind, lapply(c("27.4.a", "4.b", "27.7.d", "99.9.z"), function(a) transform(codes, areaValue = a)))
mp <- suppressWarnings(plot_coverage_map(codes))
ok(inherits(mp, "ggplot") && pies(mp) == 3,
   "the map places 27. and bare codes (one pie each) and leaves an unknown one off")
ok(grepl("99.9.z", tryCatch(plot_coverage_map(codes), warning = function(w) conditionMessage(w))),
   "an unknown area is warned about by name")

## ---- 2. from raw catch --------------------------------------------
ok(isTRUE(is_gg(plot_coverage_map(raw))), "returns a ggplot from raw catch data")
ok(!"engine" %in% names(formals(plot_coverage_map)),
   "there is no engine to choose")

## ---- 3. group = several pies per area -----------------------------
ok(isTRUE(is_gg(plot_coverage_map(raw, group = "seasonValue"))), "group = Season")
ok(isTRUE(is_gg(plot_coverage_map(raw, group = "vesselFlagCountry"))), "group = Country")
ok(isTRUE(is_gg(plot_coverage_map(raw, group = "metier4"))), "group = metier4")

## ---- 4. from a coverage table ------------------------------------
sc <- build_coverage_strata(raw, c("areaValue", "seasonValue"))
ok(isTRUE(is_gg(plot_coverage_map(sc, group = "seasonValue"))),
   "a build_coverage_strata() table is used as it stands")
ok(isTRUE(all.equal(
     ggplot2::ggplot_build(suppressWarnings(plot_coverage_map(raw, group = "seasonValue")))$data,
     ggplot2::ggplot_build(suppressWarnings(plot_coverage_map(sc, group = "seasonValue")))$data)),
   "and draws the same map as the raw catch it came from")

## ---- 5. bogus-only areas -> nothing to map ------------------
bogus <- raw; bogus$areaValue <- "99.9.z"
r <- suppressMessages(suppressWarnings(plot_coverage_map(bogus)))
ok(is.null(r), "no mappable area -> NULL with a message, not an error")

## ---- 6. a coarser area level sits where its data is ----------------
## ICESSubArea / ICESDivision: the mean of the centroids of the areaValues that have
## data in it - 7.d and 7.e pull "27.7" to the Channel, not to the middle of the whole
## of 7 - and a level the list has no centroid for (3) has one that way too
where <- function(p) {                       # the (area, lon, lat) of the pies: the label layer
  d <- p$layers[[length(p$layers)]]$data
  d[order(d$area), c("area", "lon", "lat")]
}
anchors <- function(p) {                     # the same for a grouped map: where each box is tied to
  l <- p$layers[[which(vapply(p$layers, function(l) "cx0" %in% names(l$data), NA))[1]]]$data
  l[order(l$area), c("area", "cx0", "cy0")]
}
mk <- function(areas, sub, w = 100, dis = TRUE) {
  d <- data.frame(catchCategory = "Lan", total = w, domainCatchDis = "d", areaValue = areas,
                  ICESSubArea = sub, stringsAsFactors = FALSE)
  if (dis) rbind(d, transform(d, catchCategory = "Dis", total = w / 10)) else d
}
heard <- function(expr) {
  w <- character()
  v <- withCallingHandlers(expr, warning = function(x) { w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") })
  list(value = v, warnings = w)
}
near <- function(a, b) isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = 1e-9))

h7 <- heard(plot_coverage_map(mk(c("27.7.d", "27.7.e"), "27.7"), area = "ICESSubArea"))
w7 <- where(h7$value)
ok(near(w7$lon, (0.5 + -4.0) / 2) && near(w7$lat, (50.2 + 49.8) / 2) && !length(h7$warnings),
   "27.7 with data from 7.d and 7.e only sits at the mean of those two centroids - not at the table's 7 (-8, 51)")
h3 <- heard(plot_coverage_map(mk(c("27.3.a.20", "27.3.a.21"), "27.3"), area = "ICESSubArea"))
w3 <- where(h3$value)
ok(near(w3$lon, (10.2 + 11.6) / 2) && near(w3$lat, (58.2 + 56.9) / 2) && !length(h3$warnings),
   "27.3 sits between 27.3.a.20 and 27.3.a.21 - it has a centroid, and nothing is dropped or warned about")
ok(near(unlist(where(plot_coverage_map(mk("27.7", "27.7"), area = "ICESSubArea"))[c("lon", "lat")]), c(-8, 51)),
   "a level whose only areaValue is the level itself takes the listed centroid of that area")
three <- mk(c("27.7.d", "27.7.e", "27.7.f"), "27.7")
three <- three[!(three$areaValue == "27.7.f" & three$catchCategory == "Lan"), ]      # 7.f: discards, no landings
ok(near(where(plot_coverage_map(three, area = "ICESSubArea"))$lon, (0.5 + -4.0) / 2),
   "an areaValue with no weight behind the pie (here: no landings) does not pull the centroid")
ok(near(where(plot_coverage_map(mk(c("27.7.d", "27.7.zz"), "27.7"), area = "ICESSubArea"))$lon, 0.5),
   "an areaValue with no known centroid is left out of the mean, not the whole level")

## the level with nothing finer behind it: a table aggregated to the subarea alone
tb <- build_coverage_strata(mk(c("27.3.a.20", "27.3.a.21", "27.7.d"), c("27.3", "27.3", "27.7")), "ICESSubArea")
hb <- heard(plot_coverage_map(tb, area = "ICESSubArea"))
wb <- where(hb$value)
ok(nrow(wb) == 2 && !anyNA(wb) && wb$lon[wb$area == "27.3"] > 10 && wb$lon[wb$area == "27.3"] < 25 &&
     wb$lat[wb$area == "27.3"] > 55 && wb$lat[wb$area == "27.3"] < 65 && !length(hb$warnings),
   "27.3 has a centroid of its own too - the mean of the listed areas inside it - when nothing finer is there to average")
ok(near(unlist(wb[wb$area == "27.7", c("lon", "lat")]), c(-8, 51)),
   "... and a listed one (27.7) keeps its listed centroid")

## divisions, and the box anchors of a grouped map
dv <- mk(c("27.3.a.20", "27.3.a.21", "27.7.d", "27.7.e"), c("27.3", "27.3", "27.7", "27.7"))
dv$ICESDivision <- sub("^(27\\.[0-9]+\\.[a-z]).*", "\\1", dv$areaValue)
dv$g <- rep(c("A", "B"), length.out = nrow(dv))
ha <- anchors(suppressWarnings(plot_coverage_map(dv, area = "ICESDivision", group = "g")))
ok(near(ha$cx0[ha$area == "27.3.a"], (10.2 + 11.6) / 2) && near(ha$cy0[ha$area == "27.3.a"], (58.2 + 56.9) / 2) &&
     near(ha$cx0[ha$area == "27.7.e"], -4.0),
   "ICESDivision: the mean of the areaValues with data in it (27.3.a), a division that is an areaValue as it is (27.7.e)")

## the caption says so when a level was placed by averaging, and only then
ok(grepl("pie position = mean of the areaValue centroids", h7$value$labels$caption, fixed = TRUE) &&
     !grepl("pie position", plot_coverage_map(raw)$labels$caption, fixed = TRUE),
   "the caption says the pies sit at the mean of the areaValue centroids - only for a coarser level")

## nothing about the centroid left to chance for a level nobody lists, with no finer area either
nol <- mk("99.9.z", "99.9")
ok(grepl("99.9", tryCatch(plot_coverage_map(nol, area = "ICESSubArea"), warning = function(w) conditionMessage(w)), fixed = TRUE) &&
     is.null(suppressMessages(suppressWarnings(plot_coverage_map(nol, area = "ICESSubArea")))),
   "a level with no centroid anywhere is still warned about by name and left off")

## ---- 7. source guards --------------------------------------
body_ns <- gsub("[[:space:]]", "",
                paste(deparse(body(plot_coverage_map)), collapse = ""))
ok(grepl('requireNamespace("maps"', body_ns, fixed = TRUE),
   "the maps dependency is guarded")
fills <- suppressWarnings(plot_coverage_map(raw))$scales$get_scales("fill")$palette(1)
ok(identical(names(fills), c("sampled", "unsampled")) && identical(fills[["unsampled"]], "grey80") &&
     identical(fills[["sampled"]], "#41AE76"),
   "pie slices are the sampled / not-sampled shares")
ok(grepl("boxed<-has(group_var)&&ngrp>1", body_ns, fixed = TRUE),
   "a group_var with >1 level triggers the boxed grid layout")
ok(grepl("ox<-(hwl[i]+hwl[j]+gap)-abs(ac$bx[i]-ac$bx[j])", body_ns, fixed = TRUE),
   "overlapping boxes are separated")

cat("\nDONE ", FAIL, "failure(s)", "\n")
if (FAIL) quit(status = 1L)
}
