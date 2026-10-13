## Items 5/6: the two-resolution exclusion dropdown and the sort option.
##
## Two pieces, tested at the level where each can be checked precisely:
##
##  1. roll_up_excluded_keys() - a stratum picked out at one resolution
##     (as submitted) has to translate to the resolution the donor pool
##     actually runs at (as aggregated), by lookup against a table that
##     carries both, not by guessing.
##  2. The "Discard rate" sort option, which existed in code but was
##     gated behind a flag no caller ever set - present in every
##     exclude_box() that has a rate control, unreachable from any screen.

FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}

## ---- 1. roll_up_excluded_keys() ---------------------------------------
fine <- data.frame(
  vesselFlagCountry = c("BE", "BE", "BE"),
  fleetValue = c("TBB_DEF_120-219", "TBB_DEF_70-99", "OTB_DEF_100"),
  areaValue = "27.4.c", seasonValue = "1",
  metier4 = c("TBB", "TBB", "OTB"),
  weight = c(10, 20, 30), stringsAsFactors = FALSE)
from_dims <- c("vesselFlagCountry", "fleetValue", "areaValue", "seasonValue")

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

## ---- 2. the sort-by-rate gate --------------------------------------
app <- system.file("app/app.R", package = "rcef")
if (nzchar(app)) {
  txt <- paste(readLines(app, warn = FALSE), collapse = "\n")
  ## D/L sits beside the discard rate under the same gate: both need the
  ## discard columns the rate control implies
  ok(grepl('if (!is.null(rate_ui)) c("Discard rate" = "rate", "D/L" = "dl")',
           txt, fixed = TRUE),
     "sort-by-discard-rate is gated on the rate control being present")
  ok(!grepl('if ("rate" %in% metrics) c("Discard rate" = "rate")',
            txt, fixed = TRUE),
     "not on a metrics flag no caller ever set")
  ## one picker over the strata as reported, and its list of picks
  for (k in c('output[[paste0(p, "_xui")]]',
              'output[[paste0(p, "_xlist")]]',
              "fine_tbl <- reactive", "named_resolved <- reactive"))
    ok(grepl(k, txt, fixed = TRUE), paste("exclusion_server defines:", k))
  ## ...and no longer the aggregated twin or the always-recomputing
  ## "used to raise" summary
  for (k in c('_xui_sub', '_used_warn', '_used_tbl', 'pool_d <- debounce'))
    ok(!grepl(k, txt, fixed = TRUE), paste("exclusion_server no longer has:", k))
}

cat("\nDONE ", FAIL, "failure(s)", "\n")
