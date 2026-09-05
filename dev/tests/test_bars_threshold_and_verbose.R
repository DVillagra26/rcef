## Two real gaps in plot_coverage(), reported directly against generated
## figures:
##
## 1. type = "bars" doesn't use threshold or min_trips at all - the
##    ggplot version filters on cat_key != "none" (landings/discards
##    alone decide that) and colours by the fill variable, not status.
##    Passing either in was accepted and silently ignored before, which
##    let a caller believe a value they set was doing something. Now
##    forced to NA and reported as such when verbose = TRUE.
##
## 2. plot_coverage_bars_base() coloured bars green/grey for sampled
##    status, ignoring the fill variable entirely - so a Country-filled
##    bars plot showed no country colour at all, unlike its ggplot
##    counterpart. Fixed to use var_palette(), the SAME per-level colour
##    assignment the ggplot engine uses, so the two engines are
##    genuinely comparable rather than only structurally similar.

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
## Already stratum-level (aggregated), not raw catch data - classified
## directly so resolve_coverage_strata() recognises it (via cat_key) as
## already built, rather than trying to re-aggregate it with
## build_strata(), which expects row-level CEF columns this fixture
## does not have.
strata <- classify_strata(strata_raw, 50, min_trips = 2)

## ---- 1. threshold/min_trips are ignored for type = "bars" -----------
p1 <- suppressWarnings(plot_coverage(strata, type = "bars", y = "metier4",
                                     fill = "Country", threshold = 20))
p2 <- suppressWarnings(plot_coverage(strata, type = "bars", y = "metier4",
                                     fill = "Country", threshold = 95,
                                     min_trips = 100))
ok(identical(p1$data, p2$data),
   "changing threshold/min_trips does not change the bars plot's data at all")

## setting threshold / min_trips on a bars plot now warns (always, not
## just under verbose); leaving them alone does not.
w_set <- tryCatch(plot_coverage(strata, type = "bars", y = "metier4",
                                fill = "Country", threshold = 42),
                  warning = function(w) conditionMessage(w))
ok(is.character(w_set) && grepl("do not affect type = \"bars\"", w_set),
   "type = \"bars\" with a threshold set raises a warning")
w_none <- tryCatch({
  plot_coverage(strata, type = "bars", y = "metier4", fill = "Country"); NA
}, warning = function(w) conditionMessage(w))
ok(is.na(w_none), "type = \"bars\" with no threshold set raises no warning")

out <- capture.output(suppressWarnings(
  plot_coverage(strata, type = "bars", y = "metier4", fill = "Country",
               threshold = 42, verbose = TRUE)))
ok(any(grepl("not used by type", out)),
   "a verbose bars run states the threshold is not used")
ok(!any(grepl("42", out)),
   "and does not echo the ignored value back as if it mattered")

out2 <- capture.output(
  plot_coverage(strata, type = "heatmap", y = "metier4", verbose = TRUE))
ok(any(grepl("threshold  : 50", out2, fixed = TRUE)),
   "a verbose heatmap run reports the real, resolved threshold (defaulted to 50)")
ok(any(grepl("min_trips  : 2", out2, fixed = TRUE)),
   "and the real, resolved min_trips (defaulted to 2)")

## ---- 2. verbose reports the plot shape accurately --------------------
out3 <- capture.output(suppressWarnings(
  plot_coverage(strata, type = "bars", y = "metier4", fill = "Country",
               verbose = TRUE)))
ok(any(grepl('type = "bars"', out3, fixed = TRUE)) &&
     any(grepl("fill   : Country", out3, fixed = TRUE)),
   "verbose correctly names the type and the fill column used")
ok(any(grepl("strata rows being drawn", out3)),
   "verbose reports how many rows are actually drawn")

## ---- 3. the base engine's bars plot colours by the fill variable -----
reset_palettes()
pal_ref <- var_palette("Country", c("BE", "FR"))
r <- tryCatch({
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  plot_coverage(strata, type = "bars", y = "metier4", fill = "Country",
               engine = "base")
  TRUE
}, error = function(e) conditionMessage(e))
ok(isTRUE(r), paste("the base engine's bars plot runs without error:", r))

body_txt <- paste(deparse(body(plot_coverage_bars_base)), collapse = "\n")
ok(grepl("var_palette(", body_txt, fixed = TRUE),
   paste("plot_coverage_bars_base() uses var_palette() - the SAME colour",
         "assignment the ggplot engine uses, not a fixed sampled/unsampled pair"))
ok(!grepl('col=c("#41AE76","#D9D9D9")', gsub(" ", "", body_txt), fixed = TRUE),
   "the old fixed two-colour scheme (ignoring the fill variable) is gone")

cat("\nDONE ", FAIL, "failure(s)", "\n")
