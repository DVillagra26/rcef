## plot_coverage_bar(): threshold / min_trips do not apply, and the base
## engine colours the bars by the fill variable.
##
## 1. A bar chart shows sampled vs unsampled weight; a threshold or a
##    trip floor changes nothing it draws. Passing either is now a hard
##    ERROR with a message that points at plot_coverage_heat() /
##    plot_coverage_bubble(), not a silent ignore.
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
## Already stratum-level - classified directly so resolve_coverage_strata()
## recognises it (via cat_key) as already built.
strata <- classify_strata(strata_raw, 50, min_trips = 2)

## ---- 1. threshold / min_trips are an error for the bar chart --------
e1 <- tryCatch({
  plot_coverage_bar(strata, y = "metier4", fill = "Country", threshold = 42)
  NA
}, error = function(e) conditionMessage(e))
ok(is.character(e1) && grepl("threshold / min_trips do not apply", e1),
   "plot_coverage_bar() with `threshold` set is an error")
ok(grepl("plot_coverage_heat", e1),
   "and the message points at the threshold-based views")

e2 <- tryCatch({
  plot_coverage_bar(strata, y = "metier4", min_trips = 3); NA
}, error = function(e) conditionMessage(e))
ok(is.character(e2) && grepl("threshold / min_trips do not apply", e2),
   "min_trips is an error too")

grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
p_ok <- tryCatch(plot_coverage_bar(strata, y = "metier4", fill = "Country"),
                 error = function(e) e)
ok(inherits(p_ok, "ggplot"),
   "plot_coverage_bar() with no threshold / min_trips builds normally")

## ---- 2. verbose names the view and the fill column -----------------
out <- capture.output(
  plot_coverage_bar(strata, y = "metier4", fill = "Country", verbose = TRUE))
ok(any(grepl("plot_coverage_bar", out)), "verbose names the function")
ok(any(grepl("fill   : Country", out)), "verbose reports the fill column")
ok(any(grepl("not used by a bar chart", out)),
   "verbose still says threshold / min_trips do not apply here")
ok(any(grepl("strata rows being drawn", out)),
   "verbose reports how many rows are drawn")

out2 <- capture.output(
  plot_coverage_heat(strata, y = "metier4", verbose = TRUE))
ok(any(grepl("threshold  : 50", out2, fixed = TRUE)),
   "a verbose heatmap run reports the resolved threshold (default 50)")
ok(any(grepl("min_trips  : 2", out2, fixed = TRUE)),
   "and the resolved min_trips (default 2)")

## ---- 3. base bars colour by the fill variable ---------------------
reset_palettes()
invisible(var_palette("Country", c("BE", "FR")))
r <- tryCatch({
  plot_coverage_bar(strata, y = "metier4", fill = "Country", engine = "base")
  TRUE
}, error = function(e) conditionMessage(e))
ok(isTRUE(r), paste("the base engine's bar chart runs:", r))

body_txt <- paste(deparse(body(plot_coverage_bars_base)), collapse = "\n")
ok(grepl("var_palette(", body_txt, fixed = TRUE),
   "plot_coverage_bars_base() uses var_palette()")

cat("\nDONE ", FAIL, "failure(s)", "\n")
