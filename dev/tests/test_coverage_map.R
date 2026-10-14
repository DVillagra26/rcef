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

## ---- 6. source guards --------------------------------------
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
