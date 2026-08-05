## Landings / discards thresholds, and zero-weight strata.
## Run after pkgload::load_all(".") or with the package installed.
FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}

## --- spec normalisation, all three accepted shapes -------------------
ok(identical(as_cc_spec(50), list(Lan = 50, Dis = 50)),
   "a bare number applies to both categories")
ok(identical(as_cc_spec(c(50, 40))$Dis, c(50, 40)),
   "a bare vector applies to both categories")
ok(identical(unname(cc_step_value(list(Lan = c(50, 40), Dis = c(30, 20)), 2)),
             c(40, 20)),
   "a Lan/Dis list is read per rung")
ok(identical(unname(cc_step_value(list(Lan = c(50, 40), Dis = c(30, 20)), 9)),
             c(40, 20)),
   "a rung past the end of the spec reuses the last value")
ok(identical(cc_bracket(c("Lan", "Dis", "BMS")), c("Lan", "Dis", "Dis")),
   "BMS is bracketed with discards")

## --- the steps carry a pair ------------------------------------------
st <- alloc_steps("metier4", thresholds = list(Lan = c(60, 50), Dis = c(30, 25)),
                  keep_cc = TRUE, min_meas = list(Lan = c(100, 50), Dis = c(20, 10)))
ok(identical(unname(st[[1]]$threshold), c(60, 30)), "rung 1 carries both bars")
ok(identical(unname(st[[1]]$min_meas), c(100, 20)), "rung 1 carries both counts")

st_old <- alloc_steps("metier4", c(50, 40))
ok(identical(unname(st_old[[1]]$threshold), c(50, 50)),
   "a plain vector still works (old saved scenarios)")

## --- the cascade reads the bar off the stratum, not the pool ---------
bl <- data.frame(
  catchCategory   = c("Lan", "Dis", "Lan", "Dis"),
  metier4         = c("TBB", "TBB", "OTB", "OTB"),
  weight          = c(100,  50,  100,   0),
  weight_measured = c( 45,  20,   90,   0),
  coverage        = c( 45,  40,   90,  NA),
  n_measurements  = c(300, 120,  900,   0),
  stringsAsFactors = FALSE)

a <- assign_alloc_cascade(bl, "metier4", st)

ok(!identical(a$alloc_step[1], 1L),
   "45% landings does NOT clear the 60% own-stratum landings bar")
ok(identical(a$alloc_step[2], 1L),
   "40% discards DOES clear the 30% discard bar - the point of the split")
ok(identical(a$alloc_step[3], 1L), "90% landings clears its own bar")

## --- the measurement bar bites too -----------------------------------
st_m <- alloc_steps("metier4", thresholds = list(Lan = 10, Dis = 10),
                    keep_cc = TRUE, min_meas = list(Lan = 500, Dis = 500))
am <- assign_alloc_cascade(bl, "metier4", st_m)
ok(!identical(am$alloc_step[1], 1L),
   "coverage alone is not enough when the measurement count is short")
ok(identical(am$alloc_step[3], 1L),
   "900 measurements clears a 500 bar")

## --- zero weight is not an unresolved stratum ------------------------
ok(isTRUE(a$alloc_empty[4]), "a zero-weight stratum is flagged empty")
ok(identical(a$alloc_level[4], "no catch to allocate"),
   "a zero-weight stratum is not called 'not allocated'")
tb <- alloc_cascade_table(a, st)
ok(!any(is.na(tb$step) & tb$pool == "not allocated" &
        tb$n_strata > sum(is.na(a$alloc_step) & !a$alloc_empty)),
   "the unresolved row excludes the zero-weight strata")
ok(any(grepl("no catch", tb$pool)), "zero-weight strata get their own row")

## --- add_sufficiency takes a pair, and still takes a scalar ----------
sf <- add_sufficiency(bl, NULL, min_coverage = list(Lan = 60, Dis = 30),
                      min_measurements = 0)
ok(identical(sf$ok_coverage, c(FALSE, TRUE, TRUE, FALSE)),
   "add_sufficiency applies the pair per row")
sf2 <- add_sufficiency(bl, NULL, min_coverage = 60, min_measurements = 0)
ok(identical(sf2$ok_coverage, c(FALSE, FALSE, TRUE, FALSE)),
   "add_sufficiency still accepts a single number")

## --- the emitted code carries the pair -------------------------------
ok(identical(fmt_cc_spec(list(Lan = c(50, 40), Dis = c(30, 20))),
             "list(Lan = c(50, 40), Dis = c(30, 20))"),
   "fmt_cc_spec round-trips a pair")

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
