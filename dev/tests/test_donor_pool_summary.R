## Items 5/6: the two-resolution exclusion dropdown and the "data that
## will be used to raise" summary.
##
## Three pieces, tested at the level where each can be checked precisely:
##
##  1. roll_up_excluded_keys() - a stratum picked out at one resolution
##     (as submitted) has to translate to the resolution the donor pool
##     actually runs at (as aggregated), by lookup against a table that
##     carries both, not by guessing.
##  2. donor_pool_summary() - the exact scenario the request was written
##     around: three strata that each individually clear an (absent)
##     per-stratum bar, but whose COMBINED pool does not reach a sensible
##     coverage bar. A per-stratum bar cannot catch this by construction;
##     this is the thing that catches it.
##  3. The "Discard rate" sort option, which existed in code but was
##     gated behind a flag no caller ever set - present in every
##     exclude_box() that has a rate control, unreachable from any screen.

FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}

## ---- 1. roll_up_excluded_keys() ---------------------------------------
fine <- data.frame(
  Country = c("BE", "BE", "BE"),
  fleetValue = c("TBB_DEF_120-219", "TBB_DEF_70-99", "OTB_DEF_100"),
  areaValue = "27.4.c", Season = "1",
  metier4 = c("TBB", "TBB", "OTB"),
  weight = c(10, 20, 30), stringsAsFactors = FALSE)
from_dims <- c("Country", "fleetValue", "areaValue", "Season")

sub_key <- stratum_keys(fine, from_dims)[1]
agg <- roll_up_excluded_keys(fine, sub_key, from_dims, "metier4")
ok(identical(agg, "TBB"),
   "a submitted-level pick rolls up to the aggregated group it belongs to")

## two submitted keys mapping to the SAME aggregated group collapse to one
both_tbb <- stratum_keys(fine, from_dims)[1:2]
agg2 <- roll_up_excluded_keys(fine, both_tbb, from_dims, "metier4")
ok(identical(agg2, "TBB"), "duplicate rolled-up keys are deduplicated")

## empty / no-match / degenerate inputs do not error
ok(length(roll_up_excluded_keys(fine, character(0), from_dims, "metier4")) == 0,
   "no named keys gives an empty roll-up")
ok(length(roll_up_excluded_keys(fine, "nonexistent-key", from_dims, "metier4")) == 0,
   "a key with no match gives an empty roll-up, not an error")
ok(length(roll_up_excluded_keys(fine, sub_key, character(0), "metier4")) == 0,
   "an empty from_dims gives an empty roll-up rather than matching everything")
ok(length(roll_up_excluded_keys(NULL, sub_key, from_dims, "metier4")) == 0,
   "a NULL strata table does not error")

## ---- 2. donor_pool_summary(): the OTB + SDN + SSC scenario ------------
##
## Three strata, no per-stratum floor set (cov_min = NA/off), so all
## three individually pass and enter the pool - and the pool itself is
## still thin. This is not a hypothetical: it is the exact case in the
## request that a per-stratum bar structurally cannot catch, because it
## is never asked the pool-level question.
thin <- data.frame(
  metier4 = c("OTB", "SDN", "SSC"),
  landings = c(100, 100, 100),
  landings_with_discards = c(20, 10, 5),
  discards = c(2, 1, 0.5),
  coverage = c(20, 10, 5),
  discard_rate = c(0.02, 0.01, 0.005),
  n_measurements = c(0, 0, 0), stringsAsFactors = FALSE)

s <- donor_pool_summary(thin, "metier4")
ok(s$n_strata == 3, "no per-stratum floor set - all three strata enter the pool")
ok(abs(s$coverage - (20 + 10 + 5) / 3) < 0.05,
   "the pool coverage is the landings-weighted mean, all landings equal here")
ok(!isTRUE(s$ok), "and the pool itself is flagged, even though nothing was excluded")

## the SAME pool, warning switched off, is not flagged
s_off <- donor_pool_summary(thin, "metier4", warn_cov_min = NA)
ok(isTRUE(s_off$ok), "switching the sanity threshold off stops the warning")

## a healthy pool is not flagged
healthy <- data.frame(
  metier4 = c("OTB", "SDN"), landings = c(100, 100),
  landings_with_discards = c(80, 70), discards = c(8, 7),
  coverage = c(80, 70), discard_rate = c(0.08, 0.07),
  n_measurements = c(0, 0), stringsAsFactors = FALSE)
s_h <- donor_pool_summary(healthy, "metier4")
ok(abs(s_h$coverage - 75) < 0.05, "a healthy pool's coverage is computed correctly")
ok(isTRUE(s_h$ok), "and it is not flagged")

## a per-stratum floor that empties the pool is reported, not errored
s_empty <- donor_pool_summary(thin, "metier4", cov_min = 50)
ok(s_empty$n_strata == 0,
   "a per-stratum floor that excludes everything leaves an empty pool")
ok(!isTRUE(s_empty$ok), "and an empty pool is always flagged")

## the two thresholds are genuinely independent: a per-stratum floor of
## 0 (off) still lets the pool-level check fire
s_mixed <- donor_pool_summary(thin, "metier4", cov_min = NA, warn_cov_min = 15)
ok(s_mixed$n_strata == 3, "an off per-stratum floor excludes nothing")
ok(!isTRUE(s_mixed$ok),
   "but the pool (11.7%) still fails a 15% sanity threshold independently")

## NULL / empty strata do not error
ok(is.null(donor_pool_summary(NULL, "metier4")), "NULL strata gives NULL")
ok(is.null(donor_pool_summary(thin[0, ], "metier4")), "empty strata gives NULL")

## ---- 3. the sort-by-rate gate --------------------------------------
app <- system.file("app/app.R", package = "rcef")
if (nzchar(app)) {
  txt <- paste(readLines(app, warn = FALSE), collapse = "\n")
  ok(grepl('if (!is.null(rate_ui)) c("Discard rate" = "rate")',
           txt, fixed = TRUE),
     "sort-by-discard-rate is gated on the rate control being present")
  ok(!grepl('if ("rate" %in% metrics) c("Discard rate" = "rate")',
            txt, fixed = TRUE),
     "not on a metrics flag no caller ever set")
  ## the two-resolution dropdowns and the used-to-raise summary exist
  for (k in c('output[[paste0(p, "_xui_sub")]]',
              'output[[paste0(p, "_used_warn")]]',
              'output[[paste0(p, "_used_tbl")]]',
              "fine_tbl <- reactive", "named_resolved <- reactive"))
    ok(grepl(k, txt, fixed = TRUE), paste("exclusion_server defines:", k))
}

cat("\nDONE ", FAIL, "failure(s)", "\n")
