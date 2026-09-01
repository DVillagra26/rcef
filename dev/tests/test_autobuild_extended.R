## Auto-build convenience extended to two more entry points, matching
## plot_coverage()'s established pattern:
##
## 1. plot_discard_rate(catch_data, ...) - detects raw catch data
##    (missing discard_rate) and calls build_strata() internally,
##    instead of requiring the caller to do it first.
##
## 2. plot_biology_coverage(catch_data, distributions, ...) - detects
##    raw catch data (missing cat_key) and runs the WHOLE biology
##    pipeline internally (biology_link(), stratum_measurements(),
##    add_sufficiency(), classify_strata()), instead of requiring four
##    separate calls before the plot itself.
##
## Both preserve the old calling convention exactly: passing an
## already-built table works unchanged either way.

FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}
suppressPackageStartupMessages(library(dplyr))

fx <- list.files(system.file("extdata", package = "rcef"),
                 "bll_2025_1catches_CEF.*\\.csv$", recursive = TRUE,
                 full.names = TRUE)
fd <- list.files(system.file("extdata", package = "rcef"),
                 "bll_2025_1distributions_CEF.*\\.csv$", recursive = TRUE,
                 full.names = TRUE)
if (length(fx) && length(fd)) {
  cd <- rcef:::derive_fields(utils::read.csv(fx[1], stringsAsFactors = FALSE))
  dd <- utils::read.csv(fd[1], stringsAsFactors = FALSE)

  ## ---- 1. plot_discard_rate() auto-builds from raw catch data --------
  p1 <- plot_discard_rate(cd, type = "raw", x = "fleetValue", colour = "Country")
  strata_dr <- build_strata(cd, c("fleetValue", "Country"))
  p2 <- plot_discard_rate(strata_dr, type = "raw", x = "fleetValue", colour = "Country")
  ok(identical(p1$data, p2$data),
     "plot_discard_rate(catch_data, ...) matches the old build_strata()-first route exactly")

  p3 <- plot_discard_rate(cd, type = "box", x = "fleetValue", fill = "Country")
  ok(inherits(p3, "gg"), "type='box' also auto-builds from raw catch data")

  r <- tryCatch({
    grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
    plot_discard_rate(cd, type = "raw", x = "fleetValue", colour = "Country",
                      engine = "base")
    TRUE
  }, error = function(e) conditionMessage(e))
  ok(isTRUE(r), paste("engine='base' also auto-builds:", r))

  ## ---- 2. plot_biology_coverage() auto-builds from catch + dist ------
  dims_len <- c("catchCategory", "fleetValue", "Country")
  bl <- biology_link(cd, dd, dims_len, type = "LengthTotal")
  ms <- stratum_measurements(cd, dd, dims_len, type = "LengthTotal")
  bl <- add_sufficiency(bl, ms, 50, 0)
  bio_len <- bl %>% dplyr::mutate(landings = weight,
                                  landings_with_discards = weight_measured,
                                  discards = 0, discard_rate = NA_real_)
  bio_len <- classify_strata(bio_len, 50, "length", 0, 2)

  p4 <- plot_biology_coverage(cd, dd, type = "bars", y = "fleetValue",
                              fill = "Country", weight_label = "catch")
  p5 <- plot_biology_coverage(bio_len, type = "bars", y = "fleetValue",
                              fill = "Country", weight_label = "catch")
  ok(identical(p4$data, p5$data),
     "plot_biology_coverage(catch_data, distributions, ...) matches the old four-step route exactly")

  ## the old single-argument calling convention (no distributions) is
  ## completely unaffected - the exact call every existing caller uses
  p6 <- plot_biology_coverage(bio_len, type = "heatmap", y = "fleetValue",
                              threshold = 50, what = "length", min_meas = 0,
                              min_trips = 2, weight_label = "catch")
  ok(inherits(p6, "gg"),
     "the old single-argument form (strata only, no distributions) still works unchanged")

  ## age works through the same new interface
  p7 <- plot_biology_coverage(cd, dd, type = "bars", y = "fleetValue",
                              what = "age", weight_label = "catch")
  ok(inherits(p7, "gg"), "what='age' works through the new two-dataset interface too")

  r2 <- tryCatch({
    grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
    plot_biology_coverage(cd, dd, type = "bars", y = "fleetValue",
                          fill = "Country", weight_label = "catch", engine = "base")
    TRUE
  }, error = function(e) conditionMessage(e))
  ok(isTRUE(r2), paste("engine='base' also auto-builds for biology coverage:", r2))
}

cat("\nDONE ", FAIL, "failure(s)", "\n")
