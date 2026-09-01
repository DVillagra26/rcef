## The base-R coverage plots were missing the standard reading aids
## every ggplot panel in this package carries by default: a box around
## the plotting area, real tick marks, and (for bars specifically) a
## visible outline on each bar - it had been set to white, invisible
## against the white background. Fixed across all three coverage types.

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
strata <- classify_strata(strata_raw, 50, min_trips = 2)

for (fn in c("plot_heatmap_base", "plot_coverage_bars_base", "plot_bubbles_base")) {
  body_txt <- paste(deparse(body(get(fn))), collapse = "\n")
  ok(grepl("box()", body_txt, fixed = TRUE),
     paste(fn, "draws a box() around the plotting area"))
}

for (fn in c("plot_heatmap_base", "plot_bubbles_base")) {
  body_txt <- paste(deparse(body(get(fn))), collapse = "\n")
  ok(!grepl("tick = FALSE", gsub(" ", "", body_txt, fixed = TRUE), fixed = TRUE) &&
       !grepl("tick=FALSE", gsub(" ", "", body_txt, fixed = TRUE), fixed = TRUE),
     paste(fn, "no longer disables its own tick marks"))
}

bars_body <- paste(deparse(body(plot_coverage_bars_base)), collapse = "\n")
ok(grepl('border=OUTLINE', gsub(" ", "", bars_body), fixed = TRUE),
   "plot_coverage_bars_base() outlines bars in OUTLINE (black), not white")
ok(!grepl('border="white"', bars_body, fixed = TRUE),
   "the old invisible white border is gone")

## ---- runs cleanly, all three types --------------------------------
for (call in list(
  quote(plot_coverage_bars_base(strata, "metier4", "Country")),
  quote(plot_heatmap_base(strata, "metier4", "Country")),
  quote(plot_bubbles_base(strata, "metier4", "Country"))
)) {
  r <- tryCatch({
    grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
    eval(call)
    TRUE
  }, error = function(e) conditionMessage(e))
  ok(isTRUE(r), paste("runs cleanly:", deparse(call), "->", r))
}

cat("\nDONE ", FAIL, "failure(s)", "\n")
