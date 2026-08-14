## Item 7: numTrips.
##
## Coverage alone says how much of the landings weight carries a discard
## estimate. It says nothing about how many TRIPS that rests on, and a
## stratum 100% covered by one unusually large trip is a thinner basis
## than the same coverage spread across a dozen. This suite checks three
## layers: the colour classification exactly matches the specified
## tables, the aggregation is directionally correct (max within a build,
## sum across pooled strata), and the donor-exclusion pipeline (slider,
## sort, dropdown label, pool summary) is wired the same way
## measurements already is.

FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}

## ---- 1. discard classification: every specified combination ----------
d <- data.frame(
  landings = 100, discards = 10,
  coverage = c(100, 70, 30, 70, 30),
  numTrips = c(3, 2, 3, 1, 1), stringsAsFactors = FALSE)
d <- classify_strata(d, threshold = 50)
expect_d <- c("full", "safe", "warn_lowcov", "warn_lowtrip", "warn_red")
ok(identical(d$cat_key, expect_d),
   "every discard combination lands in the specified category")

## boundary: exactly 2 trips is NOT "more than 2" (full needs > min_trips)
b <- classify_strata(data.frame(landings = 100, discards = 0,
                                coverage = 100, numTrips = 2), 50)
ok(b$cat_key == "safe",
   "100% coverage with exactly 2 trips is safe, not full (needs > 2)")

## ---- 2. length/age classification: all 8 corners ----------------------
l <- data.frame(
  coverage       = c(100, 70, 30, 70, 70, 70, 30, 30),
  numTrips       = c(3,   3,  3,  3,  1,  1,  3,  1),
  n_measurements = c(150, 150, 150, 50, 150, 50, 50, 150),
  stringsAsFactors = FALSE)
l <- classify_strata(l, threshold = 50, what = "length", min_meas = 100)
expect_l <- c("full_ok", "safe_ok", "yellowgreen", "yellow", "orangeyellow",
             "orange_lowtrip", "orange_lowcov", "orange_lowcovtrip")
ok(identical(l$cat_key, expect_l),
   "every length/age corner lands in the specified category")

## the all-fail corner
r <- classify_strata(data.frame(coverage = 30, numTrips = 1,
                                n_measurements = 50), 50, "length", 100)
ok(r$cat_key == "red", "coverage, trips and measurements all failing is red")

## nine categories, three distinguishable oranges, as asked. Matched on
## "orange_" (with the underscore) specifically - "orangeyellow" is a
## real, separate fourth category and correctly has its own colour, but
## it is not one of the three oranges the request asked for.
lk <- cat_lookup(50, "length", min_meas = 100)
ok(nrow(lk) == 9, "nine length/age categories")
oranges <- lk$colour[grepl("^orange_", lk$cat_key)]
ok(length(oranges) == 3 && length(unique(oranges)) == 3,
   "three genuinely distinct oranges")

## ---- 3. unknown trips are treated as failing, not passing -------------
u <- classify_strata(data.frame(landings = 100, discards = 10,
                                coverage = 70, numTrips = NA), 50)
ok(u$cat_key == "warn_lowtrip",
   "no reported trip count fails the trips test (unknown is not adequate)")
## and the same stratum with no numTrips COLUMN at all behaves identically
u2 <- classify_strata(data.frame(landings = 100, discards = 10,
                                 coverage = 70), 50)
ok(u2$cat_key == "warn_lowtrip",
   "a table with no numTrips column at all gets the same treatment")

## ---- 4. aggregation direction: max within a build, sum across a pool -
fx <- list.files(system.file("extdata", package = "rcef"),
                 "bll_2025_1catches_CEF.*\\.csv$", recursive = TRUE,
                 full.names = TRUE)
if (length(fx)) {
  cd <- rcef:::derive_fields(utils::read.csv(fx[1], stringsAsFactors = FALSE))
  ok("numTrips" %in% names(build_strata(cd, c("metier4", "Country"))),
     "numTrips reaches build_strata()'s output")

  ## a synthetic case where max() vs sum() give different, checkable answers
  fine <- data.frame(metier4 = c("A", "A", "B"), landings = c(50, 50, 50),
                     landings_with_discards = c(40, 40, 30),
                     discards = c(4, 4, 3), coverage = c(80, 80, 60),
                     discard_rate = c(0.08, 0.08, 0.06),
                     n_measurements = c(0, 0, 0), numTrips = c(5, 6, 3),
                     stringsAsFactors = FALSE)
  ## stratum_metrics pools DIFFERENT strata (A and B) - trips should sum
  m <- stratum_metrics(fine, "metier4")
  a_row <- m[m$key == "A", ]
  ok(a_row$numTrips == 11,
     "stratum_metrics sums trips across pooled DIFFERENT strata (5+6)")
}

## ---- 5. the donor-exclusion pipeline -----------------------------------
strata <- data.frame(
  metier4 = c("OTB", "SDN", "SSC"),
  landings = c(100, 100, 100), landings_with_discards = c(80, 70, 60),
  discards = c(8, 7, 6), coverage = c(80, 70, 60),
  discard_rate = c(0.08, 0.07, 0.06), n_measurements = c(0, 0, 0),
  numTrips = c(1, 1, 1), stringsAsFactors = FALSE)

ch <- exclusion_choices(strata, "metier4", order_by = "trips")
ok(length(ch) == 3, "exclusion_choices sorts by trips without erroring")
## exclusion_choices() returns a named vector: names() are the display
## labels ("OTB   (cov 80%, ..., 1 trips, ...)"), values are the raw
## keys ("OTB") a selectizeInput stores. The label is what the reader
## sees, so that is what has to carry the trip count.
ok(all(grepl("trips", names(ch))),
   "and shows the trip count in each label")

ov <- exclusion_overview(strata, "metier4", trips_min = 2)
ok(nrow(ov) == 3 && all(ov$reason == "too few trips"),
   "a trips floor excludes strata below it, with the right reason")

s <- donor_pool_summary(strata, "metier4", warn_trips_min = 5)
ok(s$numTrips == 3 && !isTRUE(s$ok),
   "the pool's own trip total (summed) is checked against the sanity bar")
s_off <- donor_pool_summary(strata, "metier4", warn_trips_min = NA)
ok(isTRUE(s_off$ok), "switching the trips sanity check off stops the warning")

expr <- exclusion_expr(trips_min = 2)
ok(identical(expr, "numTrips >= 2"), "trips reaches the emitted condition")

## flag_donor_exclusion: NA is left in as a potential donor, matching
## coverage/measurements' existing convention for THIS function
fd <- data.frame(coverage = c(80, 80), numTrips = c(1, NA))
fd <- flag_donor_exclusion(fd, trips_min = 2)
ok(identical(fd$donor_ok, c(FALSE, TRUE)),
   "a known-thin stratum is excluded; an unknown one is left in as a donor")

## ---- 6. the UI is wired everywhere coverage is, unlike measurements ---
app <- system.file("app/app.R", package = "rcef")
if (nzchar(app)) {
  txt <- paste(readLines(app, warn = FALSE), collapse = "\n")
  for (k in c('exclude_box(ns("x"), metrics = c("coverage", "trips", "measurements")',
              'exclude_box(ns("ca"), metrics = c("coverage", "trips", "measurements")',
              '"ar", metrics = c("coverage", "trips")',
              '"mg", metrics = c("coverage", "trips")',
              'exclude_box("ca", metrics = c("coverage", "trips")'))
    ok(grepl(k, txt, fixed = TRUE), paste("UI wiring:", k))
  for (k in c('metrics   = c("coverage", "trips", "rate")',
              'metrics   = c("coverage", "trips"))',
              'metrics   = c("coverage", "trips", "measurements")'))
    ok(grepl(k, txt, fixed = TRUE), paste("server wiring:", k))
  ok(grepl('output[[paste0(p, "_xtrips_ui")]]', txt, fixed = TRUE),
     "the trips slider has a server renderer")
}

cat("\nDONE ", FAIL, "failure(s)", "\n")
