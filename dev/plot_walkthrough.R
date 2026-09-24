## ===========================================================
## plot_walkthrough.R
##
## NOT a test - nothing here asserts pass/fail (the actual test suite is
## dev/tests/*.R, run via the "ok"/"FAIL" pattern and checked by
## dev/check_*.R). This is a template you run by hand to LOOK at every
## plot family the package has and confirm it draws sensibly: discard
## coverage, length coverage, discard rate and length distributions -
## each with a few options to compare against each other. ggplot2 only:
## every plot function returns a ggplot object.
##
## Table first: the coverage and discard-rate figures are drawn from the
## table build_coverage_strata() returns, and every one of them takes
## either the raw catch (the table is built for you, at exactly the
## plotted columns) or that table itself. Section 1 first shows the
## tables on their own - discard rates and coverage per stratum, at
## whatever aggregation you name - and then the same numbers as figures.
##
## Data: exactly two tables, loaded once below - `catch_data` (raw CEF
## catch, already through derive_fields()) and `distribution_data` (its
## length + age distributions, both types in the one table). Everything
## from section 1 on reads only those two names. Swap in your own
## derive_fields(read.csv(...)) / read.csv(...) for `catch_data` /
## `distribution_data` to run the same figures elsewhere - nothing past
## this point needs to change either way.
##
## LENGTH only, no AGE: this stock has no age data, so an age coverage or
## distribution plot would draw an empty or misleading panel rather than
## erroring - there is nothing here to catch that mistake for you. Length
## plots also stay on catchCategory = "Lan" throughout: this script only
## ever has raw catch_data, never a raised or allocated table, so a
## length plot on catchCategory = "Dis" would show the DISCARDS' own,
## un-raised length sample as if it were the whole picture - a coverage
## plot would look fully sampled when it isn't, and a distribution plot
## would show a shape that has not been weighted up to what was actually
## discarded.
##
## Nothing here filters the data first (except one demo call that narrows
## to a single metier on purpose - see 2a). The app lets you narrow a
## figure to a subset before drawing it (a country, a year, an area);
## this script otherwise always plots everything in the loaded table. If
## you want a subset, filter the catch / distributions frame yourself
## before the calls below - every function here takes a plain data
## frame, so a dplyr::filter() upstream is all that is needed.
##
## Every figure below is drawn with print() and nothing else - no pdf()/
## png()/jpeg() device, no dev.off(). Run it in an interactive session
## (RStudio, or R itself) and each print() lands in the live graphics
## window / Plots pane, one after another, to actually look at.
## ===========================================================

library(dplyr)
library(rcef)


## =====================================================================
##  0. LOAD - InterCatch "output" format (StockOverview / Numbers /
##     MeanWeigth) -> CEF -> derive_fields(), the same conversion the
##     app's "Load from IC" route runs. Best-effort and entirely local:
##       - reads inst/extdata/data_overviews_format/bll/2025 (never
##         committed, .gitignore'd, present on this machine only).
##       - funICoutCEF() auto-builds a stock_relation via funMakeRelation(),
##         which calls the ICES vocab/stock-list web services - needs the
##         icesVocab + icesSD packages AND a network connection.
##     Either missing means there is nothing to plot below - swap in your
##     own catch_data / distribution_data (see the header above) instead.
## =====================================================================
bll_dir  <- file.path("inst", "extdata", "data_overviews_format", "bll")
bll_year <- 2025
catch_data <- NULL; distribution_data <- NULL

if (dir.exists(file.path(bll_dir, bll_year)) &&
    requireNamespace("icesVocab", quietly = TRUE) &&
    requireNamespace("icesSD", quietly = TRUE)) {
  bll_ic <- tryCatch(
    funICoutCEF(dat_path = bll_dir, years = bll_year, output_format = "to_list"),
    error = function(e) {
      message("load failed - funICoutCEF(): ", conditionMessage(e))
      NULL
    })
  if (!is.null(bll_ic) && !is.null(bll_ic$catches) && nrow(bll_ic$catches)) {
    # derive_fields() is what turns raw CEF into strata-ready data: it
    # parses metier4/5/6 + gear/target/mesh out of fleetValue and the
    # ICES area levels (Subarea/Division/Subdivision) out of areaValue,
    # plus Country/Season/Stratum/FleetType - the same columns every
    # plot function below reads on y / x / fill / facet.
    catch_data <- derive_fields(as.data.frame(bll_ic$catches))
    distribution_data <- if (!is.null(bll_ic$distributions))
      as.data.frame(bll_ic$distributions) else NULL
    cat(sprintf(
      "bll %s: %d catch rows after derive_fields()%s\n", bll_year,
      nrow(catch_data),
      if (!is.null(distribution_data))
        sprintf(", %d distribution rows (%d LengthTotal + %d Age)",
               nrow(distribution_data),
               sum(distribution_data$distributionType == "LengthTotal"),
               sum(distribution_data$distributionType == "Age"))
      else ""))

  }
} else {
  cat("load skipped - bll extdata and/or icesVocab+icesSD not available\n")
}

if (is.null(catch_data)) {
  cat("\nNothing to plot: catch_data did not load (see the message above).\n",
     "Point bll_dir / bll_year at your own data, or set catch_data /\n",
     "distribution_data by hand, and re-source this script.\n")
} else {


## =====================================================================
##  0b. THE TABLES - build_coverage_strata(), no plotting involved
## =====================================================================
## One row per stratum, at whatever aggregation `dims` names. The columns
## follow the CAR / CAS output tables (total = discards, weightingTotal =
## the landings that carry a discard estimate, rate = total /
## weightingTotal, numTrips ...), in tonnes. Nothing in it judges the
## sampling - no status, no decision; thresholds are a plotting choice.
cov_mc <- build_coverage_strata(catch_data, c("metier4", "Country"))
print(head(cov_mc[order(-cov_mc$landings), ], 10))

## the aggregation is yours: coarser ...
print(build_coverage_strata(catch_data, "Season"))
## ... or finer
cov_fine <- build_coverage_strata(catch_data,
                                  c("fleetValue", "areaValue", "Season"))
cat(nrow(cov_fine), "strata at fleetValue x areaValue x Season\n")

## length coverage: one row per stratum and catch part; the weight of the
## part, the share of it with a distribution linked, and the measurements
cov_len <- build_coverage_strata(catch_data, c("metier4", "Country"),
                                 distribution_data, what = "length",
                                 catchCategory = "Lan")
print(head(cov_len[order(-cov_len$total), ], 10))

## and every figure below takes the table as readily as the raw catch:
## plot_coverage_heat(cov_mc, y = "metier4", x = "Country") draws the same
## figure as plot_coverage_heat(catch_data, y = "metier4", x = "Country")


## =====================================================================
##  1. DISCARD COVERAGE - plot_coverage_heat / _bubble / _bar / _map
## =====================================================================
## Each takes raw catch (or the table above) and, given raw catch, builds
## the table itself. `y` is required. heat / bubble put the second
## category on `x`; bars put it on `fill`. min_cov / min_trip colour the
## heat / bubble status - a bar chart ignores them (passing them is an
## error). The status is worked out here from the thresholds, not stored
## in the table. The status legend always lists every REACHABLE category
## - a stratum with no landed/caught weight at all draws no cell (nothing
## to judge), so "no landings" is not one of them.

## ---- 1a. bars: sampled vs not-sampled weight + cumulative staircase -
p1a <- plot_coverage_bar(catch = catch_data,
                         y = "fleetValue", 
                         fill = "Country", 
                         facet = "Season")
print(p1a)

## ---- 1b. heatmap: status per metier x country x season ------------
p1b <- plot_coverage_heat(catch_data, 
                          y = "metier4", 
                          x = "Country",
                          facet = "Season",
                          min_cov = 50)
print(p1b)

p1b <- plot_coverage_heat(catch_data, y = "metier4", x = "Country",
                          facet = "Season",
                          min_cov = 50, min_trip = 2)
print(p1b)

## ---- 1c. bubbles: weighted status grid ---------NULL## ---- 1c. bubbles: weighted status grid ---------------------------
p1c <- plot_coverage_bubble(catch_data, y = "metier4", x = "Country",
                            min_cov = 50, min_trip = NULL)
print(p1c)

## ---- 1d-1f. coverage map - one sampled/not-sampled pie per FAO
## area, radius = landings; `group` puts a labelled box of pies per area.
  p1d <- plot_coverage_map(catch_data)
  print(p1d)

  p1e <- plot_coverage_map(catch_data, group = "Season")
  print(p1e)

  p1f <- plot_coverage_map(catch_data, group = "Country")
  print(p1f)


## =====================================================================
##  2. LENGTH COVERAGE - what = "length", catchCategory = "Lan"
## =====================================================================
## Pass `distribution_data` and what = "length". catchCategory is always
## "Lan" here - see the header for why: this script never raises or
## allocates, so a discard-side length plot would show the discards'
## OWN, un-raised sampling as if it were the final picture.

## ---- 2a. length coverage of the LANDINGS, bars (TBB only, on purpose -
## a smaller y to make the bars readable) --------------------------
p2a <- plot_coverage_bar(catch_data, y = "metier4", fill = "Country",
                         what = "length", catchCategory = "Lan",
                         # distributions attach to catch by domainBiology, so
                         # filtering the catch alone is enough (`metier4` is a
                         # catch-table column, not one of the distributions)
                         distributions = distribution_data)
print(p2a)

## ---- 2b. length coverage of the LANDINGS, heatmap ---------------
p2b <- plot_coverage_heat(catch_data, y = "fleetValue", x = "Country",
                          facet = "Season", what = "length",
                          catchCategory = "Lan", distributions = distribution_data,
                          min_cov = 70, min_fish = 100, min_trip = NULL)
print(p2b)

## ---- 2c. length coverage of the LANDINGS, bubbles ---------------
p2c <- plot_coverage_bubble(catch_data, y = "metier4", x = "Country",
                            what = "length", catchCategory = "Lan",
                            distributions = distribution_data,
                            min_cov = 50, min_fish = 100, min_trip = NULL)
print(p2c)


## =====================================================================
##  3. DISCARD RATE - plot_discard_rate(), raw per-stratum points
## =====================================================================
## Auto-builds the table from raw catch (or takes it). One view: a point
## is one stratum, its `rate` = discards / covered landings, exactly as
## the CAS output reports it - the same numbers as
## build_coverage_strata(catch_data, c("metier4", "Country", "areaValue")).
## A stratum with no discard estimate has no rate and gets no point. The
## y axis is pseudo-log: a few tiny strata give ratios of many hundred
## percent that would flatten everything else on a plain axis.

p3a <- plot_discard_rate(catch_data, x = "metier4", colour = "Country",
                         facet = "areaValue", jitter = TRUE)
print(p3a)


## =====================================================================
##  4. LENGTH DISTRIBUTION - plot_numbers / plot_meanweight
## =====================================================================
## plot_numbers_at_length_age(): a histogram, variableType is always
## Number. plot_meanweight_at_length_age(): a line, always. Both take a
## prepared table OR raw catch + `distributions` (then the link + summary
## run inside, `catchCategory` choosing the part). Numbers in thousands
## (N3), length in cm. Same catch_data / distribution_data throughout -
## LengthTotal only, no age data for this stock (see the header).

## ---- 4a. numbers at length, stacked histogram by country -------
p4a <- plot_numbers_at_length_age(catch_data, 
                                  distribution_data,
                                  distributionType = "LengthTotal", 
                                  catchCategory = "Lan", 
                                  facet = "metier4",
                                  fill = "Country", stacked = TRUE)
print(p4a)

## ---- 4c. mean weight at length, one line per country (landings) --
p4c <- plot_meanweight_at_length_age(catch_data, distribution_data, distributionType = "LengthTotal",
         catchCategory = "Lan", colour = "Country", facet = "Season")
print(p4c)

## ---- 4d. catchCategory sweep - default "All" ("catch"), then the
## landings and the discards, for numbers AND mean weight. The catch
## part appears in the plot title. (Unlike 4a-4c above, this is a
## side-by-side comparison of the argument itself, so "Dis" belongs
## here too - it is clearly labelled as the un-raised discard sample,
## not presented as the finished picture.)
for (cc in c("All", "Lan", "Dis")) {
  print(plot_numbers_at_length_age(catch_data, distribution_data,
          distributionType = "LengthTotal", catchCategory = cc, fill = "metier4"))
  print(plot_meanweight_at_length_age(catch_data, distribution_data,
          distributionType = "LengthTotal", catchCategory = cc, colour = "metier4"))
}
## and the default really is "All" - no catchCategory passed
stopifnot(grepl("\\(catch\\)",
  plot_numbers_at_length_age(catch_data, distribution_data,
    distributionType = "LengthTotal", fill = "Country")$labels$title))
cat("4d: catchCategory default is \"All\" (title says \"catch\")\n")


## =====================================================================
##  6. FLEXIBLE THRESHOLDS - min_trip / min_fish are each independently
##     optional (NULL = dropped from the classification and the legend
##     entirely, a number = active). Coverage (min_cov) is always on.
##     The colour is a straight count of how many ACTIVE targets a
##     stratum missed - yellow (1), orange (2), dark orange (3, only
##     reachable with trips AND measurements both active) - but each
##     SPECIFIC combination is still its own category and its own label.
##     Red stays reserved for "no discard estimate" / "no distribution
##     linked at all" on a stratum that HAS weight - never for a
##     threshold miss, and never for a stratum with no weight at all
##     (that draws no cell to begin with).
## =====================================================================
# 
# say <- function(l) cat("\n---- 6", l, "----\n")
# 
# ## ---- 6a. discard: 1 param (coverage only) vs 2 params (+ trips) ------
# say("a. discard coverage - min_trip = NULL drops trips from the scheme")
# lk_d1 <- cat_lookup(50, "discard", min_trips = NULL)
# lk_d2 <- cat_lookup(50, "discard")            # min_trip = 2, active (default)
# cat("  1 param categories:", paste(lk_d1$cat_key, collapse = ", "), "\n")
# cat("  2 param categories:", paste(lk_d2$cat_key, collapse = ", "), "\n")
# # 1 param: only "warn_cov" is reachable (coverage is the only thing that
# # CAN fail). 2 params: warn_cov / warn_trip / warn_cov_trip - one row per
# # specific combination, not per count - so 3 warning rows, not 2.
# stopifnot(nrow(lk_d1) == 6L, nrow(lk_d2) == 8L,
#          sum(grepl("^warn_", lk_d1$cat_key)) == 1L,
#          sum(grepl("^warn_", lk_d2$cat_key)) == 3L,
#          "warn_cov_trip" %in% lk_d2$cat_key,
#          !"warn_cov_trip" %in% lk_d1$cat_key)
# 
# p6a1 <- plot_coverage_heat(catch_data, y = "metier4", x = "Country",
#                            min_trip = NULL)
# print(p6a1)
# p6a2 <- plot_coverage_heat(catch_data, y = "metier4", x = "Country")
# print(p6a2)
# cat("  both heatmaps drew:", !is.null(p6a1) && !is.null(p6a2), "\n")
# 
# ## ---- 6b. length: 1, 2 (either extra) or 3 active parameters -----
# say("b. length coverage - trips and measurements toggle independently")
# lk_l1 <- cat_lookup(50, "length", min_trips = NULL, min_meas = NULL)
# lk_l2t <- cat_lookup(50, "length", min_meas = NULL)            # + trips only
# lk_l2m <- cat_lookup(50, "length", min_meas = 25, min_trips = NULL) # + meas only
# lk_l3  <- cat_lookup(50, "length", min_meas = 25)               # + both
# cat("  coverage only        :", paste(lk_l1$cat_key, collapse = ", "), "\n")
# cat("  + trips               :", paste(lk_l2t$cat_key, collapse = ", "), "\n")
# cat("  + measurements        :", paste(lk_l2m$cat_key, collapse = ", "), "\n")
# cat("  + trips & measurements:", paste(lk_l3$cat_key, collapse = ", "), "\n")
# # one row per active PARAMETER COUNT choose k (k = 1..n_active), so
# # n_active = 1 -> 1 warn row, 2 -> 3, 3 -> 7 (the "all missed" key,
# # warn_cov_trip_meas, only exists once trips AND measurements are both on)
# stopifnot(nrow(lk_l1) == 5L, nrow(lk_l2t) == 7L, nrow(lk_l2m) == 7L,
#          nrow(lk_l3) == 11L,
#          !"warn_cov_trip_meas" %in% lk_l2t$cat_key,
#          "warn_cov_trip_meas" %in% lk_l3$cat_key)
# 
# p6b1 <- plot_coverage_heat(catch_data, y = "metier4", x = "Country",
#                            what = "length", catchCategory = "Lan",
#                            distributions = distribution_data,
#                            min_trip = NULL)                     # coverage only
# print(p6b1)
# p6b2 <- plot_coverage_heat(catch_data, y = "metier4", x = "Country",
#                            what = "length", catchCategory = "Lan",
#                            distributions = distribution_data,
#                            min_fish = 25)                       # + measurements
# print(p6b2)
# cat("  both length heatmaps drew:", !is.null(p6b1) && !is.null(p6b2), "\n")
# 
# ## ---- 6c. red stays reserved for "no estimate/distribution at all" ---
# ## Setting the thresholds absurdly high must NOT turn every stratum red -
# ## it should crowd them into the worst-active warning shade instead. Red
# ## (no_dist / no discard estimate) only ever comes from has_disc_data /
# ## has_dist, never from a threshold comparison, and only on a stratum
# ## that has weight to begin with.
# say("c. extreme thresholds still resolve to a warning shade, not all-red")
# sc_extreme <- classify_strata(build_strata(catch_data, c("metier4", "Country")),
#                               1500, min_trips = 1000)
# tb <- table(sc_extreme$cat_key)
# print(tb)
# n_no_dist <- if ("no_dist" %in% names(tb)) tb[["no_dist"]] else 0L
# stopifnot(n_no_dist < sum(tb))   # not everything fell through to red
# cat("  no_dist only where has_disc_data is FALSE:",
#    all(!sc_extreme$has_disc_data[sc_extreme$cat_key == "no_dist"]), "\n")
# 
# cat("\nDone - every figure above was print()'d to the active graphics\n",
#    "device as it was drawn; nothing was saved to a file.\n")
# 
}  # end: if (!is.null(catch_data))

