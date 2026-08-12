## decision_tree() / decision_tree2().
##
## These were a strict superset of one another - decision_tree2() called
## decision_tree() and appended running totals - and the app held a
## reactive for each, called with identical arguments. Every threshold
## change therefore walked the whole cascade twice to build one table
## that was a prefix of the other.
##
## Now one function with `cumulative =`. decision_tree2() is kept as a
## thin delegate so saved scripts naming it keep running. What has to
## stay true is that the merge changed no numbers.

FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}

ex <- system.file("extdata", package = "rcef")
fx <- list.files(ex, "catches_CEF.*\\.csv$", recursive = TRUE, full.names = TRUE)
if (!length(fx)) {
  cat("no CEF fixture available - skipping\n")
} else {
  cd <- rcef:::derive_fields(utils::read.csv(fx[1], stringsAsFactors = FALSE))
  dm <- c("metier4", "Country", "ICESDivision", "Season")
  st <- classify_strata(build_strata(cd, dm), 50)
  rec <- recommend_raising(st, dm, rep(50, 4), 50, "nested",
                           "Catch All by All", dm)

  plain <- decision_tree(rec, dm, rep(50, 4), 50, "nested", dm)
  rich  <- decision_tree(rec, dm, rep(50, 4), 50, "nested", dm,
                         cumulative = TRUE)
  old   <- decision_tree2(rec, dm, rep(50, 4), 50, "nested", dm)

  ## ---- the delegate returns exactly what it always did ---------------
  ok(identical(rich, old),
     "decision_tree2() is byte-identical to cumulative = TRUE")
  ok(is.data.frame(old) && nrow(old) > 0, "and still returns a real table")

  ## ---- the plain form is untouched -----------------------------------
  ok(identical(plain, rich[, names(plain), drop = FALSE]),
     "the plain table is an exact prefix of the cumulative one")
  ok(!any(c("pct_strata", "cum_pct_strata", "cum_pct_landings") %in%
            names(plain)),
     "cumulative = FALSE adds no columns")
  ok(all(c("pct_strata", "cum_pct_strata", "cum_pct_landings") %in%
           names(rich)),
     "cumulative = TRUE adds all three")

  ## ---- check_partition reads both ------------------------------------
  ##
  ## The app now passes the RICH table where it used to pass the plain
  ## one. check_partition() reads only n_strata and landings, but if that
  ## ever stopped being true the app would silently start reporting a
  ## broken partition.
  ok(identical(check_partition(rec, plain), check_partition(rec, rich)),
     "check_partition gives the same answer on either table")
  ok(isTRUE(check_partition(rec, rich)$n_ok),
     "and the cascade still partitions the strata")
  ok(isTRUE(check_partition(rec, rich)$l_ok),
     "and the landings still add back to the total")

  ## ---- the running totals mean what they say -------------------------
  ok(abs(utils::tail(rich$cum_pct_strata, 1) - 100) < 0.51,
     "cumulative strata reach 100% at the fallback")
  ok(abs(utils::tail(rich$cum_pct_landings, 1) - 100) < 0.51,
     "cumulative landings reach 100% at the fallback")
  ok(!is.unsorted(rich$cum_pct_strata), "cumulative strata never decrease")
  ok(!is.unsorted(rich$cum_pct_landings), "cumulative landings never decrease")
  ok(nrow(rich) == nrow(plain), "the merge invented no extra steps")

  ## a single-row cascade must not divide by zero or recycle
  one <- rich[1, , drop = FALSE]
  ok(all(is.finite(one$cum_pct_strata)), "one-row totals are finite")
}

cat("\nDONE ", FAIL, "failure(s)", "\n")
