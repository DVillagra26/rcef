## matchedDataCond must read as the thresholds that were set, not as the
## list of strata that happen to fail them today.
source("dev/tests/fixture.R")
FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}
dims <- c("metier4", "vesselFlagCountry", "ICESDivision", "seasonValue")

## --- discards ---------------------------------------------------------
r <- new_rule("G1", "beam", list(list(var = "metier4", values = "TBB")),
              cov_min = 50, dr_max = 0.9)
txt <- source_cond_string(r, dims)
ok(grepl("coverage >= 50", txt, fixed = TRUE),
   "a coverage bar is written as a comparison")
ok(grepl("discard_rate <= 0.9", txt, fixed = TRUE),
   "a rate bar is written as a comparison")
ok(!grepl("!paste(", txt, fixed = TRUE),
   "and NOT as a list of stratum keys - the whole point")

## a hand-picked stratum has no metric that expresses it, so it stays a
## key - but it is short, because a person typed it
r2 <- new_rule("G1", "", list(), cov_min = 50,
               excluded = "TBB | Germany | 27.4 | 2")
t2 <- source_cond_string(r2, dims)
ok(grepl("coverage >= 50", t2, fixed = TRUE), "the bar is still arithmetic")
ok(grepl("TBB | Germany", t2, fixed = TRUE),
   "a hand-picked stratum is named explicitly")
ok(lengths(regmatches(t2, gregexpr('"', t2)))[1] < 20,
   "and the list is short, not two hundred keys")

## dims MUST be threaded, or a hand-picked exclusion silently vanishes
ok(!grepl("Germany", source_cond_string(r2, NULL)),
   "without dims the key cannot be written...")
cd <- conds_from_rules(list(r2), list(), dims)
ok(grepl("Germany", rlang::quo_text(cd$matchedDataCond$G1)),
   "...so conds_from_rules must pass dims through, and does")

## nothing set means nothing added
r3 <- new_rule("G1", "", list(list(var = "metier4", values = "TBB")))
ok(identical(source_cond_string(r3, dims), 'metier4 %in% "TBB"'),
   "no bars leaves the clause untouched")

## --- lengths and ages -------------------------------------------------
mc <- alloc_matched_cond(list(catchCategory = "Dis"), character(0), dims,
                         cov_min = 40, meas_min = 25)
ok(grepl("coverage >= 40", mc$text, fixed = TRUE),
   "the biology donor condition carries the coverage bar")
ok(grepl("n_measurements >= 25", mc$text, fixed = TRUE),
   "and the measurement bar")
ok(!grepl("!paste(", mc$text, fixed = TRUE), "and no key list")
ok(identical(deparse(rlang::quo_get_expr(mc$quo)), deparse(str2lang(mc$text))),
   "text and quosure stay the same expression")

## --- the columns the conditions need must exist -----------------------
## `coverage >= 50` is not merely wrong without them, it errors.
d <- data.frame(metier4 = c("TBB", "TBB"), ICESDivision = "27.4.b",
                catchCategory = c("Lan", "Dis"), total = c(1000, 200),
                domainCatchDis = c("d1", "d1"), stringsAsFactors = FALSE)
ok(inherits(try(rlang::eval_tidy(rlang::quo(coverage >= 50), data = d),
                silent = TRUE), "try-error"),
   "without the join the condition errors")
dm <- add_stratum_metrics(d, c("metier4", "ICESDivision"))
ok(is.logical(rlang::eval_tidy(rlang::quo(coverage >= 50), data = dm)),
   "with the join it evaluates")

## and the emitted script has to say so, or it will not run
em <- code_manual_rule("manual_1", dims, list(r))
ok(any(grepl("add_stratum_metrics", em)),
   "the emitted script joins the metrics before using them")
ok(!any(grepl("!paste\\(", em)),
   "and carries no key list")

em2 <- code_manual_rule("manual_1", dims, list(r3))
ok(!any(grepl("add_stratum_metrics", em2)),
   "a rule with no bars does not drag the join in for nothing")

## --- the metric columns must survive cef_prepare() -------------------
##
## A matchedDataCond of `coverage >= 50 & n_measurements >= 100` errored
## with "object 'coverage' not found" because cef_prepare() joined the
## stratum table a SECOND time, colliding coverage into coverage.x /
## coverage.y so no plain `coverage` existed. This is the exact failing
## condition from the field.
fx <- "fixture"
if (length(fx)) {
  cd <- fx_catch()
  prep <- rcef:::cef_prepare(cd, c("metier4", "vesselFlagCountry", "ICESDivision", "seasonValue"))
  ok("coverage" %in% names(prep), "cef_prepare keeps a plain coverage column")
  ok("n_measurements" %in% names(prep), "and a plain n_measurements column")
  ok(!any(grepl("coverage\\.|discard_rate\\.", names(prep))),
     "no coverage.x / coverage.y collision")
  r <- tryCatch(dplyr::filter(prep, coverage >= 50 & n_measurements >= 100),
                error = function(e) e)
  ok(is.data.frame(r),
     "the exact failing condition now evaluates instead of erroring")
}

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)