## build_coverage_strata(): the table behind the coverage / discard-rate
## plots, usable on its own.
##
## Checked against numbers worked out by hand on a tiny catch, then on the
## bundled example data for length / age. What it must NOT carry is as
## much a part of the contract as what it must: no status, category or
## raising decision - thresholds are a plotting choice.

source("dev/tests/fixture.R")
FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}
near <- function(a, b, tol = 1e-9) isTRUE(all.equal(a, b, tolerance = tol,
                                                     check.attributes = FALSE))
suppressPackageStartupMessages(library(dplyr))

## ---- 1. discard table, worked by hand ---------------------------------
## OTB: 1000 kg landed with a discard domain + 3000 kg without, 200 kg
##      discarded  -> landings 4 t, covered 1 t, discards 0.2 t
## TBB: 4000 kg landed, all covered, 100 kg discarded
## SSC: 500 kg landed, no discard estimate at all
## A BMS row must not be counted as a discard.
cd <- data.frame(
  catchCategory  = c("Lan", "Lan", "Dis", "Lan", "Lan", "Dis", "Lan", "BMS"),
  metier4        = c("OTB", "OTB", "OTB", "TBB", "TBB", "TBB", "SSC", "OTB"),
  vesselFlagCountry = "BE",
  total          = c(1000, 3000, 200, 2000, 2000, 100, 500, 50),
  domainCatchDis = c("d1", NA, "d1", "d2", "d2", "d2", NA, NA),
  numTrips       = c(2, 2, 2, 5, 5, 5, NA, NA),
  variableUnit   = "kg",
  stringsAsFactors = FALSE)

t1 <- build_coverage_strata(cd, c("metier4", "vesselFlagCountry"))
o <- t1[t1$metier4 == "OTB", ]; b <- t1[t1$metier4 == "TBB", ]
s <- t1[t1$metier4 == "SSC", ]
ok(nrow(t1) == 3, "one row per stratum")
ok(near(o$total, 0.2) && near(o$weightingTotal, 1) && near(o$landings, 4),
   "OTB: discards 0.2 t, covered landings 1 t, all landings 4 t (tonnes, BMS excluded)")
ok(near(o$rate, 0.2) && near(o$coverage, 25),
   "OTB: rate = discards / covered landings (0.2), coverage = 25%")
ok(near(b$rate, 0.025) && near(b$coverage, 100) && b$numTrips == 5,
   "TBB: rate 0.025, fully covered, 5 trips")
ok(is.na(s$total) && is.na(s$rate) && near(s$coverage, 0),
   "SSC: no discard estimate -> total and rate are NA (not 0), coverage 0")
ok(near(s$landings, 0.5) && near(s$weightingTotal, 0),
   "SSC: its landings are still counted")
## the same discards said the other way: D / (L + D), not D / L
ok(near(o$discardRate, 0.2 / 1.2) && near(b$discardRate, 0.1 / 4.1) &&
     is.na(s$discardRate),
   "discardRate = discards / (covered landings + discards); NA where there is no estimate")

## ---- 1b. a landing has an estimate when a discard row pairs with it ----
## The way catchRaising() decides it: a discard row with a weight, of the landing's domain, country, year, stock and
## origin / variable type. 1000 kg landed in each stratum, with
##   A: a WGEstimate discard row of its domain                               -> covered
##   B: a domain whose only discard row is Official (0 kg beside the estimate) -> no estimate, the raising raises it
##   C: a discard row of its domain with no weight (NA)                      -> no estimate
##   D: discards, but of another domain                                      -> no estimate
##   E: a discard row of its domain from another country                     -> no estimate
pc <- data.frame(
  catchCategory     = c(rep("Lan", 5), rep("Dis", 5)),
  metier4           = rep(c("A", "B", "C", "D", "E"), 2),
  vesselFlagCountry = c(rep("BE", 5), "BE", "BE", "BE", "BE", "NL"),
  year = 2023L, stock = "syn", speciesCode = "1", workingGroup = "WG",
  originType        = c(rep("WGEstimate", 6), "Official", rep("WGEstimate", 3)),
  variableType      = "WeightLive", variableUnit = "kg",
  total             = c(rep(1000, 5), 100, 0, NA, 50, 70),
  domainCatchDis    = c("dA", "dB", "dC", "dD", "dE", "dA", "dB", "dC", "dX", "dE"),
  stringsAsFactors = FALSE)
tp <- build_coverage_strata(pc, c("metier4"))
cv <- setNames(tp$coverage, tp$metier4)
ok(near(cv[["A"]], 100) && all(cv[c("B", "C", "D", "E")] == 0),
   "only the landings whose domain has a WGEstimate discard row with a weight, of their own country, are covered")
ok(near(tp$weightingTotal[tp$metier4 == "A"], 1) && near(tp$rate[tp$metier4 == "A"], 0.1) &&
     is.na(tp$rate[tp$metier4 == "B"]),
   "B: the Official 0 kg row makes no rate (it was 0 at full coverage before)")
ok(near(tp$landings[tp$metier4 == "B"], 1),
   "and the landings of an uncovered stratum are still counted")
## without the columns that pair (a table of your own), the domain alone does
pd <- pc[, c("catchCategory", "metier4", "total", "domainCatchDis", "variableUnit")]
td <- build_coverage_strata(pd, "metier4")
ok(near(td$coverage[td$metier4 == "A"], 100) && near(td$coverage[td$metier4 == "B"], 100) &&
     near(td$coverage[td$metier4 == "E"], 100) && near(td$coverage[td$metier4 == "D"], 0),
   "a catch with no country / origin columns pairs on the domain alone")

## the columns follow CAR / CAS, and carry no judgement of the sampling
want <- c("catchCategory", "variableUnit", "total", "weightingCategory",
          "weightingTotal", "rate", "discardRate", "landings", "coverage",
          "numTrips")
ok(all(want %in% names(t1)), "the CAR / CAS-style value columns are all there")
ok(identical(names(t1), c("metier4", "vesselFlagCountry", want)),
   "the stratum columns first, then the value columns in a fixed order")
ok(all(t1$catchCategory == "Dis") && all(t1$weightingCategory == "Lan") &&
     all(t1$variableUnit == "t"),
   "constant columns say what total / weightingTotal are, and the unit")
ok(!any(c("cat_key", "Status", "status", "Raising_Decision",
          "has_disc_data", "stratum_id", "discard_rate") %in% names(t1)),
   "no status, category, decision or bespoke helper columns")

## ---- 2. the aggregation level is whatever dims names ------------------
t2 <- build_coverage_strata(cd, "vesselFlagCountry")
ok(nrow(t2) == 1 && near(t2$total, 0.3) && near(t2$weightingTotal, 5) &&
     near(t2$landings, 8.5) && near(t2$rate, 0.06),
   "dims = 'vesselFlagCountry' pools the three strata: 0.3 t / 5 t = 0.06")
ok(near(t2$discardRate, 0.3 / 5.3),
   "and D/(L+D) is re-derived from the summed weights too: 0.3 / 5.3")
ok(near(t2$coverage, 5 / 8.5 * 100) && t2$numTrips == 5,
   "pooled coverage is re-derived from the summed weights; trips = the largest")
t3 <- build_coverage_strata(cd, c("metier4", "vesselFlagCountry", "domainCatchDis"))
ok(nrow(t3) > nrow(t1), "a finer dims gives more, smaller strata")

## ---- 3. arguments -----------------------------------------------------
err <- function(expr) tryCatch({ force(expr); "no error" },
                               error = function(e) conditionMessage(e))

## ---- 3a. dims left out: nothing pooled, the finest the data has -------
## `cd` only carries "vesselFlagCountry" of the four default columns
## (fleetValue, areaValue, vesselFlagCountry, seasonValue) - metier4 is
## not one of them, so leaving dims out must NOT default to metier4 x
## vesselFlagCountry; it collapses to vesselFlagCountry alone, exactly
## like naming it (t2, above).
ok(identical(build_coverage_strata(cd), t2),
   "no dims given: defaults to whichever of fleetValue/areaValue/vesselFlagCountry/seasonValue the catch has (just vesselFlagCountry here)")
cd_nocty <- cd; cd_nocty$vesselFlagCountry <- NULL
ok(grepl("none of the default stratum", err(build_coverage_strata(cd_nocty))),
   "no dims, and none of the four default columns present -> a clear error")
## on real, derive_fields()-ed catch all four are present, so the default is
## genuinely the finest: as many rows as naming them all explicitly
ac0 <- fx_catch()
finest <- c("fleetValue", "areaValue", "vesselFlagCountry", "seasonValue")
ok(identical(build_coverage_strata(ac0), build_coverage_strata(ac0, finest)),
   "on real catch (all four default columns present), no dims matches naming them all")
ok(nrow(build_coverage_strata(ac0)) >= nrow(build_coverage_strata(ac0, c("metier4", "vesselFlagCountry"))),
   "... which is at least as fine as a named, coarser table")
ad0 <- fx_dist()
ok(identical(build_coverage_strata(ac0, distributions = ad0, what = "length"),
             build_coverage_strata(ac0, finest, ad0, what = "length")),
   "the default applies to length / age too")

ok(grepl("catchCategory does not apply", err(
     build_coverage_strata(cd, "metier4", catchCategory = "Lan"))),
   "catchCategory on a discard table is an error")
ok(grepl("not in `catch`: nope", err(build_coverage_strata(cd, "nope"))),
   "a dims column that is not in the catch is named")
ok(grepl("cannot be a stratum column", err(
     build_coverage_strata(cd, c("catchCategory", "metier4")))),
   "catchCategory as a stratum column is an error for discards")
ok(grepl("needs the CEF catch columns", err(
     build_coverage_strata(build_coverage_strata(cd, c("metier4", "vesselFlagCountry")), "metier4"))),
   "an already-aggregated table is refused, not silently 'nothing to describe'")
ok(grepl("needs `distributions`", err(
     build_coverage_strata(cd, "metier4", what = "length"))),
   "length without distributions says what is missing")
ok(grepl("must be", err(build_coverage_strata(cd, "metier4", what = "nonsense"))),
   "an unknown `what` is an error naming the valid ones")
ok(near(build_coverage_strata(cd, c("metier4", "vesselFlagCountry"), what = "Discards")$total,
        t1$total),
   "what accepts the plural and mixed case")
ok(is.null(build_coverage_strata(cd[cd$catchCategory == "BMS", ], "metier4")),
   "nothing but BMS -> NULL, not an error")

## ---- 4. length / age on the bundled example data ----------------------
test_catch <- fx_catch()
test_dist <- fx_dist()
ac <- test_catch; ad <- test_dist
dm <- c("metier4", "vesselFlagCountry")

l_all <- build_coverage_strata(ac, dm, ad, what = "length")
ok(setequal(unique(l_all$catchCategory), c("Lan", "Dis")),
   "catchCategory = 'All' (the default) gives a Lan and a Dis row per stratum")
ok(all(l_all$distributionType == "LengthTotal") &&
     all(l_all$variableUnit == "t"),
   "length rows say LengthTotal and tonnes")
ok(all(c("total", "coverage", "numMeasurements", "numTrips") %in% names(l_all)) &&
     !any(c("cat_key", "Status", "n_measurements", "weight") %in% names(l_all)),
   "length columns: total, coverage, numMeasurements, numTrips - no status")

l_lan <- build_coverage_strata(ac, dm, ad, what = "length", catchCategory = "Lan")
ok(all(l_lan$catchCategory == "Lan") &&
     near(l_lan$total, l_all$total[l_all$catchCategory == "Lan"]),
   "catchCategory = 'Lan' is exactly the Lan rows of 'All'")
ok(near(sum(l_lan$total),
        sum(convert_units(ac$total[ac$catchCategory == "Lan"],
                          ac$variableUnit[ac$catchCategory == "Lan"], "t"))),
   "the Lan totals add up to the catch's landings, in tonnes")
ok(all(l_all$coverage[!is.na(l_all$coverage)] >= 0 &
         l_all$coverage[!is.na(l_all$coverage)] <= 100 + 1e-9) &&
     all(l_all$numMeasurements >= 0),
   "coverage is a percentage; measurement counts are not negative")

a_lan <- build_coverage_strata(ac, dm, ad, what = "age", catchCategory = "Lan")
ok(all(a_lan$distributionType == "Age"),
   "what = 'age' links the Age distributions")
no_age <- build_coverage_strata(ac, dm, ad[ad$distributionType != "Age", ],
                                what = "age", catchCategory = "Lan")
ok(all(no_age$coverage %in% c(0, NA)) && all(no_age$numMeasurements == 0),
   "no distributions of the requested type -> every stratum has coverage 0, none linked")

## a measurement count is per catch part, not the stratum's total copied
## onto both rows (which would double count once the parts are pooled)
tot_meas <- build_coverage_strata(ac, dm, ad, what = "length",
                                  catchCategory = "Dis")$numMeasurements
ok(near(l_all$numMeasurements[l_all$catchCategory == "Dis"], tot_meas),
   "a Dis row's numMeasurements is the Dis part's own")

## ---- 5. a table is recognised by its own columns ----------------------
ok(identical(coverage_table_what(t1), "discard") &&
     identical(coverage_table_what(l_lan), "length") &&
     identical(coverage_table_what(a_lan), "age"),
   "coverage_table_what() tells the three kinds apart")
ok(is.na(coverage_table_what(cd)) && is.na(coverage_table_what(ac)),
   "raw catch is not mistaken for a coverage table")
withcov <- ac; withcov$coverage <- 50
ok("coverage" %in% names(withcov) && is.na(coverage_table_what(withcov)),
   "even raw catch that carries a row-level coverage column")

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)