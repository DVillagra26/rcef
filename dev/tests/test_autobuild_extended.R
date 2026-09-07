## Auto-build convenience on the split plot functions:
##
## 1. plot_discard_rate(catch_data, ...) - detects raw catch data
##    (missing discard_rate) and calls build_strata() internally.
##
## 2. plot_coverage_bar(catch_data, what = "length", distributions = ...)
##    - detects raw catch data (missing cat_key) and runs the WHOLE
##    biology pipeline internally (biology_link(), stratum_measurements(),
##    add_sufficiency(), classify_strata()).
##
## Both preserve the pre-built calling convention: passing an
## already-classified table works unchanged.

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
  p1 <- plot_discard_rate(cd, x = "fleetValue", colour = "Country")
  strata_dr <- build_strata(cd, c("fleetValue", "Country"))
  p2 <- plot_discard_rate(strata_dr, x = "fleetValue", colour = "Country")
  ok(identical(p1$data, p2$data),
     "plot_discard_rate(catch_data, ...) matches the old build_strata()-first route exactly")

  r <- tryCatch({
    grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
    plot_discard_rate(cd, x = "fleetValue", colour = "Country", engine = "base")
    TRUE
  }, error = function(e) conditionMessage(e))
  ok(isTRUE(r), paste("engine='base' also auto-builds:", r))

  ## ---- 2. plot_coverage_bar() auto-builds from catch + dist ---------
  dims_len <- c("catchCategory", "fleetValue", "Country")
  bl <- biology_link(cd, dd, dims_len, "Lan", type = "LengthTotal")
  ms <- stratum_measurements(cd, dd, dims_len, "Lan", type = "LengthTotal")
  bl <- add_sufficiency(bl, ms, 50, 0)
  bio_len <- bl %>% dplyr::mutate(landings = weight,
                                  landings_with_discards = weight_measured,
                                  discards = 0, discard_rate = NA_real_)
  bio_len <- classify_strata(bio_len, 50, "length", 0, 2)

  p4 <- plot_coverage_bar(cd, y = "fleetValue", fill = "Country",
                          what = "length", catchCategory = "Lan",
                          distributions = dd, weight_label = "catch")
  p5 <- plot_coverage_bar(bio_len, y = "fleetValue", fill = "Country",
                          what = "length", weight_label = "catch")
  ok(identical(p4$data, p5$data),
     "plot_coverage_bar(catch, what='length', distributions=) matches the four-step route")

  ## a pre-classified table (no distributions) still works
  p6 <- plot_coverage_heat(bio_len, y = "fleetValue", what = "length",
                           threshold = 50, min_meas = 0, min_trips = 2,
                           weight_label = "catch")
  ok(inherits(p6, "gg"),
     "a pre-classified length table (no distributions) still works")

  ## age through the same interface
  p7 <- plot_coverage_bar(cd, y = "fleetValue", what = "age",
                          distributions = dd, weight_label = "catch")
  ok(inherits(p7, "gg"), "what='age' works through the two-dataset interface too")

  ## catchCategory = "Dis" links to a different catch part
  p8 <- plot_coverage_bar(cd, y = "fleetValue", what = "length",
                          catchCategory = "Dis", distributions = dd,
                          weight_label = "catch")
  ok(inherits(p8, "gg") && !isTRUE(all.equal(p4$data, p8$data)),
     "catchCategory = 'Dis' links to the discard part, a different figure")

  r2 <- tryCatch({
    grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
    plot_coverage_bar(cd, y = "fleetValue", fill = "Country", what = "length",
                      distributions = dd, weight_label = "catch",
                      engine = "base")
    TRUE
  }, error = function(e) conditionMessage(e))
  ok(isTRUE(r2), paste("engine='base' also auto-builds for length coverage:", r2))
}

cat("\nDONE ", FAIL, "failure(s)", "\n")
