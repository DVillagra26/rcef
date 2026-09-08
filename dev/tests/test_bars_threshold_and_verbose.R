## plot_coverage_bar(): min_cov / min_trip do not apply, and the base
## engine colours the bars by the fill variable.
##
## 1. A bar chart shows sampled vs unsampled weight; a threshold or a
##    trip floor changes nothing it draws. The function does not accept
##    min_cov / min_trip at all, so passing either is R's normal
##    unused-argument error.
## 2. plot_coverage_bars_base() colours by var_palette() - the SAME
##    per-level assignment the ggplot engine uses - not a fixed
##    sampled/unsampled pair.

FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}

strata_raw <- data.frame(
  metier4 = rep(c("OTB", "SDN", "SSC"), 2),
  Country = rep(c("BE", "FR"), each = 3),
  landings = c(100, 80, 60, 40, 30, 20),
  landings_with_discards = c(80, 60, 30, 40, 15, 0),
  discards = c(8, 6, 3, 4, 1.5, 0),
  coverage = c(80, 75, 50, 100, 50, 0),
  discard_rate = c(0.09, 0.09, 0.09, 0.09, 0.09, NA),
  n_measurements = 0, numTrips = c(3, 3, 1, 3, 1, 0),
  stringsAsFactors = FALSE)
## Already stratum-level - classified directly so build_coverage_strata()
## recognises it (via cat_key) as already built.
strata <- classify_strata(strata_raw, 50, min_trips = 2)

## ---- 1. min_cov / min_trip are not accepted by the bar chart -----
e1 <- tryCatch({
  plot_coverage_bar(strata, y = "metier4", fill = "Country", min_cov = 42)
  NA
}, error = function(e) conditionMessage(e))
ok(is.character(e1) && grepl("unused argument", e1) && grepl("min_cov", e1),
   "plot_coverage_bar() does not accept min_cov")

e2 <- tryCatch({
  plot_coverage_bar(strata, y = "metier4", min_trip = 3); NA
}, error = function(e) conditionMessage(e))
ok(is.character(e2) && grepl("unused argument", e2) && grepl("min_trip", e2),
   "nor min_trip")

grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
p_ok <- tryCatch(plot_coverage_bar(strata, y = "metier4", fill = "Country"),
                 error = function(e) e)
ok(inherits(p_ok, "ggplot"),
   "plot_coverage_bar() with no min_cov / min_trip builds normally")

## ---- 2. base bars colour by the fill variable ---------------------
reset_palettes()
invisible(var_palette("Country", c("BE", "FR")))
r <- tryCatch({
  plot_coverage_bar(strata, y = "metier4", fill = "Country", engine = "base")
  TRUE
}, error = function(e) conditionMessage(e))
ok(isTRUE(r), paste("the base engine's bar chart runs:", r))

body_txt <- paste(deparse(body(plot_coverage_bar)), collapse = "\n")
ok(grepl("var_palette(", body_txt, fixed = TRUE),
   "the base bar-chart branch colours by var_palette()")

cat("\nDONE ", FAIL, "failure(s)", "\n")
