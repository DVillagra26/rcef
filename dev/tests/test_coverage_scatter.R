## plot_coverage_scatter() / plot_coverage_scatter_base() and the
## plot_coverage(type = "scatter") dispatch: one point per stratum,
## weight on log x, a sampling metric on y, sized by a second metric,
## coloured by status, sparse labels.

FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}

raw <- data.frame(
  fleetValue = rep(c("OTB", "SDN", "GNS", "TBB"), each = 3),
  Country    = rep(c("BE", "FR", "NL"), 4),
  Season     = rep(1:3, 4),
  landings   = c(400, 8, 0.2, 300, 40, 1, 90, 3, 0.5, 250, 12, 0.9),
  landings_with_discards = c(390, 4, 0, 60, 40, 0, 10, 3, 0, 250, 2, 0),
  discards   = c(35, 0.4, 0, 6, 4, 0, 1, 0.3, 0, 24, 0.2, 0),
  coverage   = c(98, 50, 0, 20, 100, 0, 11, 100, 0, 100, 17, 0),
  discard_rate = 0.09, n_measurements = c(0,0,0,120,80,0,0,0,0,200,0,0),
  numTrips   = c(9, 2, NA, 6, 3, NA, 1, 4, NA, 8, 1, NA),
  stringsAsFactors = FALSE)
sc <- classify_strata(raw, threshold = 50, min_trips = 2)

draws <- function(expr) tryCatch({
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  x <- force(expr)
  if (inherits(x, "ggplot")) "gg" else "base"
}, error = function(e) conditionMessage(e))

## ---- 1. both engines + the dispatcher --------------------------------
ok(identical(draws(plot_coverage_scatter(sc)), "gg"),
   "plot_coverage_scatter() returns a ggplot")
ok(identical(draws(plot_coverage_scatter_base(sc)), "base"),
   "plot_coverage_scatter_base() draws without error")
ok(identical(draws(plot_coverage(sc, type = "scatter")), "gg"),
   "plot_coverage(type = 'scatter') dispatches to the ggplot builder")
ok(identical(draws(plot_coverage(sc, type = "scatter", engine = "base")), "base"),
   "plot_coverage(type = 'scatter', engine = 'base') dispatches to the base builder")

## ---- 2. y / size metrics are configurable --------------------------
ok(identical(draws(plot_coverage_scatter(sc, y = "n_measurements",
                                         size = "n_measurements")), "gg"),
   "y and size can both be n_measurements (biology)")
ok(identical(draws(plot_coverage(sc, type = "scatter",
                                 scatter_y = "n_measurements",
                                 scatter_size = "numTrips", min_meas = 100,
                                 engine = "base")), "base"),
   "dispatch threads scatter_y / scatter_size / min_meas through")
ok(grepl("not in the table", draws(plot_coverage_scatter(sc, y = "nope"))),
   "a missing y column is a clear error")

## ---- 3. faceting + n_per_screen ------------------------------------
ok(identical(draws(plot_coverage_scatter_base(sc, facet_var = "Season")), "base"),
   "base engine facets")
ok(identical(draws(plot_coverage_scatter_base(sc, facet_var = "Season",
                                              n_per_screen = 1)), "base"),
   "base engine honours n_per_screen")
ok(identical(draws(plot_coverage_scatter(sc, facet_var = "Season")), "gg"),
   "ggplot engine facets")

## ---- 4. raw catch data auto-builds (no y needed for scatter) ------
rawcatch <- data.frame(
  catchCategory = c("Lan", "Lan", "Dis", "Lan", "Lan"),
  total = c(1000, 500, 20, 300, 50),
  domainCatchDis = c("d1", "", "d1", "d2", ""),
  fleetValue = c("OTB", "OTB", "OTB", "SDN", "SDN"),
  Country = c("BE", "BE", "BE", "FR", "FR"),
  Season = c(1, 1, 1, 2, 2), stringsAsFactors = FALSE)
ok(identical(draws(plot_coverage(rawcatch, type = "scatter", engine = "base")),
             "base"),
   "plot_coverage(type='scatter') builds strata from raw catch data, no y")

## ---- 5. source guards --------------------------------------------
body_ns <- gsub("[[:space:]]", "",
                paste(deparse(body(plot_coverage_scatter)), collapse = ""))
ok(grepl("scale_x_log10", body_ns, fixed = TRUE),
   "x axis is log-scaled")
ok(grepl('!duplicated(.lab)', body_ns, fixed = TRUE),
   "labels are de-duplicated by fleet name before slicing")

cat("\nDONE ", FAIL, "failure(s)", "\n")
