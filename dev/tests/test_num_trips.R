## The sampling status the heatmap and the bubble grid colour a cell by.
##
## Coverage alone says how much of the landings weight carries a discard
## estimate. It says nothing about how many TRIPS that rests on, and a
## stratum 100% covered by one unusually large trip is a thinner basis
## than the same coverage spread across a dozen; for length and age the
## measurements are a third question. This suite checks, through the plots
## themselves and tables written by hand, that every combination of the
## active thresholds lands in its own status, that the legend lists exactly
## the reachable ones, and that a trips floor recolours the figure.
##
## A cell's status is the `cat_key` column of the figure's data; its colour is
## read back from the figure as it is drawn.

source("dev/tests/fixture.R")
FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}
suppressPackageStartupMessages(library(ggplot2))
warn <- function(expr) tryCatch({ force(expr); "no warning" },
                                warning = function(w) conditionMessage(w))

## The plots take a build_coverage_strata() table (CAR / CAS-style columns), so
## a stratum can be written down by hand: one row each, in the order of `id`.
disc_tbl <- function(coverage, numTrips, total = 10, landings = 100)
  data.frame(id = sprintf("s%02d", seq_along(coverage)),
             catchCategory = "Dis", variableUnit = "t", total = total,
             weightingCategory = "Lan",
             weightingTotal = landings * coverage / 100,
             rate = ifelse(coverage > 0, total / (landings * coverage / 100), NA_real_),
             landings = landings, coverage = coverage, numTrips = numTrips,
             stringsAsFactors = FALSE)
bio_tbl <- function(coverage, numTrips, numMeasurements, total = 100,
                    type = "LengthTotal")
  data.frame(id = sprintf("s%02d", seq_along(coverage)),
             catchCategory = "Lan", distributionType = type, variableUnit = "t",
             total = total, coverage = coverage, numMeasurements = numMeasurements,
             numTrips = numTrips, stringsAsFactors = FALSE)
status <- function(p) as.character(p$data$cat_key)
## the colour of each status the legend lists, best to worst: the zero-size
## mark drawn last for every one of them
key_colours <- function(p) {
  d <- ggplot_build(p)$data[[length(p$layers)]]
  setNames(d$fill, p$scales$get_scales("fill")$get_limits())
}

## ---- 1. discards: every combination of the two active thresholds ---------
## Two ACTIVE targets - coverage vs min_cov and trips vs min_trip - so the
## warning shade is a straight count of how many were missed: yellow for one,
## orange for both. Which one(s) failed is a distinct status (warn_cov /
## warn_trip / warn_cov_trip), the same colour within a count. Red is not
## reachable from a threshold miss: it is reserved for "no discard estimate".
h <- plot_coverage_grid(disc_tbl(c(100, 70, 30, 70, 30), c(3, 2, 3, 1, 1)),
                        y = "id", min_cov = 50, min_trip = 2)
ok(identical(status(h), c("full", "safe", "warn_cov", "warn_trip", "warn_cov_trip")),
   "every discard combination lands in the specified status")
fills <- ggplot_build(h)$data[[1]]$fill
ok(fills[3] == fills[4] && fills[3] != fills[5] && length(unique(fills)) == 4,
   "warn_cov and warn_trip share the one-missed colour; both missed is a colour of its own")
ok(identical(names(key_colours(h)),
             c("full", "safe", "warn_cov", "warn_trip", "warn_cov_trip",
               "no_dist", "disc_only")),
   "the legend lists every reachable status, best to worst, with no 'none' among them")

## red only when the stratum carries NO discard estimate (total is NA)
nd <- plot_coverage_grid(disc_tbl(0, 0, total = NA), y = "id", min_cov = 50, min_trip = 2)
ok(identical(status(nd), "no_dist"),
   "a stratum with no discard estimate at all is red (no_dist), not a warning")
## the same numbers WITH an estimate (nobody found any discards) are just a double miss
wd <- plot_coverage_grid(disc_tbl(0, 0, total = 0), y = "id", min_cov = 50, min_trip = 2)
ok(identical(status(wd), "warn_cov_trip"),
   "with an estimate present, missing both thresholds is warn_cov_trip (orange), not red")

## boundary: exactly 2 trips is NOT "more than 2" (full needs > min_trip)
b <- plot_coverage_grid(disc_tbl(100, 2), y = "id", min_cov = 50, min_trip = 2)
ok(identical(status(b), "safe"),
   "100% coverage with exactly 2 trips is safe, not full (needs > 2)")

## coverage only: trips left out of the verdict, and out of the legend
co <- plot_coverage_grid(disc_tbl(c(100, 70, 30), c(1, 1, 1)), y = "id", min_cov = 50)
ok(identical(status(co), c("full", "safe", "warn_cov")) &&
     identical(names(key_colours(co)), c("full", "safe", "warn_cov", "no_dist", "disc_only")),
   "min_trip left at NULL: trips judge nothing and the legend has no trips statuses")

## nothing landed and nothing discarded: nothing to judge, so nothing is drawn
z <- disc_tbl(c(100, 80), c(3, 3), total = c(10, 0), landings = c(100, 0))
ok(identical(status(plot_coverage_grid(z, y = "id", min_cov = 50, min_trip = 2)), "full"),
   "a cell with no landings and no discards draws no tile at all")
## discards but no landings: its own blue status, not a threshold miss
do <- plot_coverage_grid(type = "bubble", disc_tbl(c(100, 0), c(3, 3), total = c(10, 5), landings = c(100, 0)),
                           y = "id", min_cov = 50, min_trip = 2)
ok(identical(status(do), c("full", "disc_only")),
   "discards without landings is disc_only, on the bubble grid too")

## ---- 2. length / age: which target(s) were missed -------------------------
## Three ACTIVE targets - coverage, trips, measurements - so the warning shade
## is a count (yellow / orange / dark orange) but the status is the SPECIFIC
## combination: one for every non-empty subset of the three.
l <- bio_tbl(coverage        = c(100, 70, 30, 70, 70, 70, 30, 30),
             numTrips        = c(3,   3,  3,  3,  1,  1,  3,  1),
             numMeasurements = c(150, 150, 150, 50, 150, 50, 50, 150))
hl <- plot_coverage_grid(l, y = "id", min_cov = 50, min_trip = 2, min_fish = 100)
ok(identical(status(hl), c("full", "safe", "warn_cov", "warn_meas", "warn_trip",
                           "warn_trip_meas", "warn_cov_meas", "warn_cov_trip")),
   "each length / age corner lands in its specific missed-target status")
kc <- key_colours(hl)
ok(identical(names(kc), c("full", "safe", "warn_cov", "warn_trip", "warn_meas",
                          "warn_cov_trip", "warn_cov_meas", "warn_trip_meas",
                          "warn_cov_trip_meas", "no_dist")),
   "ten length / age statuses (2 + 7 combinations + 1), single misses before double before triple")
miss_n <- lengths(strsplit(sub("^warn_", "", names(kc)[grepl("^warn_", names(kc))]), "_"))
by_count <- split(unname(kc[grepl("^warn_", names(kc))]), miss_n)
ok(all(vapply(by_count, function(v) length(unique(v)) == 1, logical(1))) &&
     length(unique(unlist(lapply(by_count, `[`, 1)))) == 3 &&
     !any(unlist(by_count) %in% kc[c("full", "safe", "no_dist")]),
   "every combination with the same missed-count shares one colour; the counts differ; none reuses full / safe / no_dist")

## all three missed -> dark orange, NOT red
r <- plot_coverage_grid(bio_tbl(30, 1, 50), y = "id", min_cov = 50, min_trip = 2, min_fish = 100)
ok(identical(status(r), "warn_cov_trip_meas") && ggplot_build(r)$data[[1]]$fill != kc[["no_dist"]],
   "coverage, trips and measurements all missed is warn_cov_trip_meas, and not the red")

## red only when NO distribution is linked at all
nl <- suppressWarnings(plot_coverage_grid(bio_tbl(0, 0, 0), y = "id", min_cov = 50, min_trip = 2, min_fish = 100))
ok(identical(status(nl), "no_dist"),
   "a length stratum with no linked distribution is red (no_dist)")

## a stratum with nothing in it has nothing to sample - not a failure, not red,
## and not drawn; real weight is unaffected
zl <- bio_tbl(c(NA, NA, 80), c(NA, NA, 3), c(0, 0, 150), total = c(0, NA, 100))
hz <- plot_coverage_grid(zl, y = "id", min_cov = 50, min_trip = 2, min_fish = 100)
ok(nrow(hz$data) == 1 && identical(status(hz), "safe") && !any(c("s01", "s02") %in% hz$data$id),
   "zero weight and NA weight draw nothing; real weight is classified as usual")
## the same applies to age
ok(is.null(suppressWarnings(plot_coverage_grid(bio_tbl(NA, NA, 0, total = 0, type = "Age"),
                                               y = "id", min_cov = 50, min_trip = 2, min_fish = 100))),
   "age with no weight anywhere: nothing to draw")
ok(identical(names(key_colours(plot_coverage_grid(bio_tbl(80, 3, 150, type = "Age"), y = "id",
                                                 min_cov = 50, min_fish = 100))),
             c("full", "safe", "warn_cov", "warn_meas", "warn_cov_meas", "no_dist")),
   "age: the same scheme, with no trips question when min_trip is NULL")

## ---- 3. unknown trips are treated as failing, not passing -------------------
u <- suppressWarnings(plot_coverage_grid(disc_tbl(70, NA), y = "id", min_cov = 50, min_trip = 2))
ok(identical(status(u), "warn_trip") &&
     grepl("every stratum's numTrips is unknown", warn(
       plot_coverage_grid(disc_tbl(70, NA), y = "id", min_cov = 50, min_trip = 2))),
   "no reported trip count fails the trips test (unknown is not adequate), and says why when no stratum has one")
ok(identical(status(plot_coverage_grid(disc_tbl(c(70, 70), c(NA, 3)), y = "id",
                                       min_cov = 50, min_trip = 0)),
             c("warn_trip", "safe")),
   "an unknown trip count fails even at min_trip = 0; a known one passes")
ok(grepl("numMeasurements is 0", warn(
     plot_coverage_grid(bio_tbl(80, 3, 0), y = "id", min_cov = 50, min_fish = 100))),
   "a measurement floor no stratum can meet (every count is 0) is flagged")

## ---- 4. numTrips is a sufficiency control on the coverage plots -----------
##
## Not just wired to the classification - the whole chain from a trips floor
## down to the rendered pixels has to actually respond: the heatmap has to
## recolour.
strata <- disc_tbl(c(80, 70, 60), c(1, 1, 1), total = c(8, 7, 6), landings = 100)
strata$metier4 <- c("OTB", "SDN", "SSC")
p_lo <- plot_coverage_grid(strata, y = "metier4", min_cov = 50, min_trip = 1)
p_hi <- plot_coverage_grid(strata, y = "metier4", min_cov = 50, min_trip = 5)
ok(!is.null(p_lo) && !is.null(p_hi), "the heatmap renders at both trips floors")
b_lo <- ggplot_build(p_lo)$data[[1]]$fill
b_hi <- ggplot_build(p_hi)$data[[1]]$fill
ok(!identical(b_lo, b_hi),
   "the rendered TILE COLOURS differ between trips floors 1 and 5")

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
