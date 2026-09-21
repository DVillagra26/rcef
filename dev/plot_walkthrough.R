## ===========================================================
## plot_walkthrough.R
##
## NOT a test - nothing here asserts pass/fail (the actual test suite is
## dev/tests/*.R, run via the "ok"/"FAIL" pattern and checked by
## dev/check_*.R). This is a template you run by hand to LOOK at every
## plot family the package has and confirm it draws sensibly: discard
## coverage, length coverage, AGE coverage, discard rate, length
## distributions and age distributions - each with a few options to
## compare against each other, ggplot2 side by side with the base-R
## engine, and the general-purpose arguments (auto-build from raw data,
## palette, n_per_screen).
##
## Data: exactly two tables, loaded once below - `catch_data` (raw CEF
## catch, already through derive_fields()) and `distribution_data` (its
## length + age distributions, both types in the one table). Everything
## from section 1 on reads only those two names; LENGTH vs AGE is just
## `distributionType` / `what` on the same pair, not a second data set.
## Swap in your own derive_fields(read.csv(...)) / read.csv(...) for
## `catch_data` / `distribution_data` to run the same figures elsewhere -
## nothing past this point needs to change either way.
##
## Nothing here filters the data first. The app lets you narrow a figure
## to a subset before drawing it (a country, a year, an area); this
## script always plots everything in the loaded table. If you want a
## subset, filter the catch / distributions frame yourself before the
## calls below - every function here takes a plain data frame, so a
## dplyr::filter() upstream is all that is needed.
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
##  1. DISCARD COVERAGE - plot_coverage_heat / _bubble / _bar / _map
## =====================================================================
## Each takes raw catch and builds + classifies the strata internally.
## `y` is required. heat / bubble put the second category on `x`; bars
## put it on `fill`. min_cov / min_trip colour the heat / bubble status -
## a bar chart ignores them (passing them is an error). Every variant is
## drawn in BOTH engines. The status legend always lists every REACHABLE
## category - a stratum with no landed/caught weight at all draws no
## cell (nothing to judge), so "no landings" is not one of them.

## ---- 1a. bars: sampled vs not-sampled weight + cumulative staircase -
p1a <- plot_coverage_bar(catch_data, y = "metier4", fill = "Country")
print(p1a)
png("11a_discard_bars_base.png", width = 1300, height = 800, res = 110)
plot_coverage_bar(catch_data, y = "metier4", fill = "Country",
                  engine = "base", palette = "colorblind")
invisible(dev.off())

## ---- 1b. heatmap: status per metier x country x season ------------
p1b <- plot_coverage_heat(catch_data, y = "metier4", x = "Country",
                          facet = "Season")
print(p1b)
png("11d_discard_heatmap_base.png", width = 1400, height = 900, res = 110)
plot_coverage_heat(catch_data, y = "metier4", x = "Country",
                   facet = "Season", engine = "base")
invisible(dev.off())

plot_coverage_heat(catch_data, y = "metier4", x = "Country",
                   facet = "Season", engine = "base",min_cov = 50, min_trip = 0)

plot_coverage_heat(catch_data, y = "metier4", x = "Country",
                   facet = "Season", engine = "base",min_cov = 50, min_trip = 100)

## ---- 1c. bubbles: weighted status grid ---------------------------
p1c <- plot_coverage_bubble(catch_data, y = "metier4", x = "Country",
                            min_cov = 50, min_trip = 2)
print(p1c)
png("11e_discard_bubbles_base.png", width = 1300, height = 800, res = 110)
plot_coverage_bubble(catch_data, y = "metier4", x = "Country",
                     min_cov = 50, engine = "base")
invisible(dev.off())

## ---- 1d-1f. coverage map - one sampled/not-sampled pie per FAO
## area, radius = landings; `group` puts a labelled box of pies per area.
if (requireNamespace("maps", quietly = TRUE)) {
  p1d <- plot_coverage_map(catch_data, engine = "ggplot")
  print(p1d)
  png("11i_coverage_map_base.png", width = 1000, height = 950, res = 110)
  plot_coverage_map(catch_data, engine = "base")
  invisible(dev.off())

  p1e <- plot_coverage_map(catch_data, group = "Season", engine = "ggplot")
  print(p1e)
  png("11j_coverage_map_season_base.png", width = 1050, height = 1000, res = 110)
  plot_coverage_map(catch_data, group = "Season", engine = "base")
  invisible(dev.off())

  p1f <- plot_coverage_map(catch_data, group = "Country", engine = "ggplot")
  print(p1f)
  png("11k_coverage_map_country_base.png", width = 1050, height = 1000, res = 110)
  plot_coverage_map(catch_data, group = "Country", engine = "base")
  invisible(dev.off())
} else message("1d-1f: 'maps' not installed - coverage map skipped")

## ---- 1g. n_per_screen - split a faceted base figure across devices -
grDevices::pdf("13f_discard_heatmap_screens.pdf", width = 10, height = 7)
plot_coverage_heat(catch_data, y = "metier4", x = "Country",
                   facet = "Season", engine = "base", n_per_screen = 2)
invisible(dev.off())


## =====================================================================
##  2. LENGTH & AGE COVERAGE - what = "length" / "age", + catchCategory
## =====================================================================
## Pass `distribution_data` and a biology `what`; `catchCategory` chooses
## the landings ("Lan", default), discards ("Dis") or BMS ("BMS") part.
## LENGTH and AGE are the SAME catch_data / distribution_data pair - only
## `what` (and `min_fish`, tuned to each unit's typical sample size)
## changes. 2a-2c LENGTH, landings then discards.  2d-2h AGE.

## ---- 2a. length coverage of the LANDINGS, bars -------------------
p2a <- plot_coverage_bar(catch_data %>% filter(metier4 == "TBB"), y = "metier4", fill = "Country",
                         what = "length", catchCategory = "Dis",
                         # distributions attach to catch by domainBiology, so
                         # filtering the catch alone is enough (`gear` is a
                         # catch-table column, not one of the distributions)
                         distributions = distribution_data)
print(p2a)
png("12a_length_bars_base.png", width = 1300, height = 800, res = 110)
plot_coverage_bar(catch_data, y = "metier4", fill = "Country",
                  what = "length", catchCategory = "Dis",
                  distributions = distribution_data,
                  engine = "base")
invisible(dev.off())

## ---- 2b. length coverage of the DISCARDS, heatmap ---------------
p2b <- plot_coverage_heat(catch_data, y = "metier4", x = "Country",
                          facet = "Season", what = "length",
                          catchCategory = "Dis", distributions = distribution_data,
                          min_cov = 50, min_fish = 25, min_trip = 2)
print(p2b)
png("12b_length_heatmap_base.png", width = 1400, height = 900, res = 110)
plot_coverage_heat(catch_data, y = "metier4", x = "Country", facet = "Season",
                   what = "length", catchCategory = "Dis",
                   distributions = distribution_data, min_cov = 50,
                   min_fish = 0,
                   engine = "base")
invisible(dev.off())

## ---- 2c. length coverage, bubbles (landings) -------------------
p2c <- plot_coverage_bubble(catch_data, y = "metier4", x = "Country",
                            what = "length", distributions = distribution_data,
                            min_cov = 50, min_fish = 25, min_trip = 2)
print(p2c)
png("12c_length_bubbles_base.png", width = 1300, height = 800, res = 110)
plot_coverage_bubble(catch_data, y = "metier4", x = "Country",
                     what = "length", distributions = distribution_data,
                     min_cov = 50, min_fish = 25, min_trip = 2, engine = "base")
invisible(dev.off())

## ---- 2d-2h. AGE coverage, all four views + the map --------------
p2d <- plot_coverage_bar(catch_data, y = "metier4", fill = "Country",
                         what = "age", distributions = distribution_data)
print(p2d)
png("12d_age_bars_base.png", width = 1300, height = 800, res = 110)
plot_coverage_bar(catch_data, y = "metier4", fill = "Country", what = "age",
                  distributions = distribution_data,
                  engine = "base")
invisible(dev.off())

p2e <- plot_coverage_heat(catch_data, y = "metier4", x = "Country",
                          facet = "Season", what = "age",
                          distributions = distribution_data, min_cov = 50,
                          min_fish = 15, min_trip = 2)
print(p2e)
png("12e_age_heatmap_base.png", width = 1400, height = 900, res = 110)
plot_coverage_heat(catch_data, y = "metier4", x = "Country", facet = "Season",
                   what = "age", distributions = distribution_data, min_cov = 50,
                   min_fish = 15, min_trip = 2,
                   engine = "base")
invisible(dev.off())

p2f <- plot_coverage_bubble(catch_data, y = "metier4", x = "Country",
                            what = "age", distributions = distribution_data,
                            min_cov = 50, min_fish = 15, min_trip = 2)
print(p2f)
png("12f_age_bubbles_base.png", width = 1300, height = 800, res = 110)
plot_coverage_bubble(catch_data, y = "metier4", x = "Country", what = "age",
                     distributions = distribution_data, min_cov = 50, min_fish = 15,
                     min_trip = 2, engine = "base")
invisible(dev.off())

if (requireNamespace("maps", quietly = TRUE)) {
  p2h <- plot_coverage_map(catch_data, what = "age", distributions = distribution_data,
                           group = "Season",
                           engine = "ggplot")
  print(p2h)
  png("12h_age_map_season_base.png", width = 1050, height = 1000, res = 110)
  plot_coverage_map(catch_data, what = "age", distributions = distribution_data,
                    group = "Season", engine = "base")
  invisible(dev.off())
}


## =====================================================================
##  3. DISCARD RATE - plot_discard_rate(), raw per-stratum points
## =====================================================================
## Auto-builds strata from raw catch (no discard_rate column). One view
## now - the boxplot was dropped. A stratum with no landings gets no
## point (its rate would not mean anything) - sampled_for_discards()
## already excludes it.

p3a <- plot_discard_rate(catch_data, x = "metier4", colour = "Country",
                         facet = "Season", jitter = TRUE)
print(p3a)
png("13a_discard_rate_raw_base.png", width = 1400, height = 900, res = 110)
plot_discard_rate(catch_data, x = "metier4", colour = "Country",
                  engine = "base")
invisible(dev.off())


## =====================================================================
##  4. LENGTH & AGE DISTRIBUTION - plot_numbers / plot_meanweight
## =====================================================================
## plot_numbers_at_length_age(): a histogram, variableType is always
## Number. plot_meanweight_at_length_age(): a line, always.
## Both take a prepared table OR raw catch + `distributions` (then the
## link + summary run inside, `catchCategory` choosing the part).
## Numbers in thousands (N3), length in cm, age in years. Same
## catch_data / distribution_data throughout - LENGTH vs AGE is just
## `distributionType`.
##   4a-4c LENGTH   4d-4f AGE

## ---- 4a. numbers at length, stacked histogram by country -------
p4a <- plot_numbers_at_length_age(catch_data, distribution_data,
         distributionType = "LengthTotal",
         fill = "Country", stacked = TRUE)
print(p4a)
png("14a_length_hist_base.png", width = 1300, height = 800, res = 110)
plot_numbers_at_length_age(catch_data, distribution_data,
  distributionType = "LengthTotal", fill = "Country",
  engine = "base", palette = "colorblind")
invisible(dev.off())

## ---- 4b. numbers at length, dodged instead of stacked ---------
p4b <- plot_numbers_at_length_age(catch_data, distribution_data,
         fill = "Country", stacked = FALSE)
print(p4b)
png("14b_length_hist_dodge_base.png", width = 1200, height = 900, res = 110)
plot_numbers_at_length_age(catch_data, distribution_data,
  fill = "Country", stacked = FALSE, engine = "base")
invisible(dev.off())

## ---- 4c. mean weight at length, one line per country (discards) --
p4c <- plot_meanweight_at_length_age(catch_data, distribution_data,
         catchCategory = "Dis", colour = "Country")
print(p4c)
png("14c_length_meanweight_base.png", width = 1200, height = 800, res = 110)
plot_meanweight_at_length_age(catch_data, distribution_data, colour = "Country", engine = "base")
invisible(dev.off())

## ---- 4d-4f. AGE distribution --------------------------------
p4d <- plot_numbers_at_length_age(catch_data, distribution_data,
         distributionType = "Age", fill = "Country",
         stacked = TRUE)
print(p4d)
png("14d_age_hist_base.png", width = 1300, height = 800, res = 110)
plot_numbers_at_length_age(catch_data, distribution_data, distributionType = "Age",
  fill = "Country", engine = "base",
  palette = "colorblind")
invisible(dev.off())

p4e <- plot_numbers_at_length_age(catch_data, distribution_data,
         distributionType = "Age", fill = "Country", stacked = FALSE)
print(p4e)
png("14e_age_hist_dodge_base.png", width = 1200, height = 900, res = 110)
plot_numbers_at_length_age(catch_data, distribution_data, distributionType = "Age",
  fill = "Country", stacked = FALSE, engine = "base")
invisible(dev.off())

p4f <- plot_meanweight_at_length_age(catch_data, distribution_data,
         distributionType = "Age", colour = "Country")
print(p4f)
png("14f_age_meanweight_base.png", width = 1200, height = 800, res = 110)
plot_meanweight_at_length_age(catch_data, distribution_data, distributionType = "Age",
  colour = "Country", engine = "base")
invisible(dev.off())

## ---- 4g-4j. catchCategory sweep - default "All" ("catch"), then the
## landings and the discards, for numbers AND mean weight, length AND age.
## The catch part appears in the plot title.
for (dt in c("LengthTotal", "Age")) {
  for (cc in c("All", "Lan", "Dis")) {
    print(plot_numbers_at_length_age(catch_data, distribution_data,
            distributionType = dt, catchCategory = cc, fill = "Country"))
    print(plot_meanweight_at_length_age(catch_data, distribution_data,
            distributionType = dt, catchCategory = cc, colour = "Country"))
  }
}
## and the default really is "All" - no catchCategory passed
stopifnot(grepl("\\(catch\\)",
  plot_numbers_at_length_age(catch_data, distribution_data,
    distributionType = "Age", fill = "Country")$labels$title))
cat("4g-4j: catchCategory default is \"All\" (title says \"catch\")\n")


## =====================================================================
##  5. ARGUMENT REACTIONS - wrong / partial calls
## =====================================================================
## The split functions error or warn when an argument does not fit.
## Each call is wrapped so the walkthrough
## runs to the end; watch the "  error:" / "  warning:" lines.

say <- function(l) cat("\n---- 5", l, "----\n")
try_show <- function(expr) tryCatch(
  withCallingHandlers({ force(expr); invisible(NULL) },
    warning = function(w) { message("  warning: ", conditionMessage(w))
      invokeRestart("muffleWarning") }),
  error = function(e) message("  error: ", conditionMessage(e)))

grDevices::pdf(tempfile())

say("a. bar chart given min_cov -> ERROR (arg not accepted)")
try_show(plot_coverage_bar(catch_data, y = "metier4", min_cov = 80))

say("b. bar chart given min_trip -> ERROR (arg not accepted)")
try_show(plot_coverage_bar(catch_data, y = "metier4", min_trip = 5))

say("c. catchCategory on a discard coverage plot -> ERROR")
try_show(plot_coverage_heat(catch_data, y = "metier4", catchCategory = "Lan"))

say("d. what = 'discards' (plural) is accepted as an alias")
try_show(plot_coverage_heat(catch_data, y = "metier4", what = "discards"))

say("e. what = 'nonsense' -> ERROR naming the valid values")
try_show(plot_coverage_bar(catch_data, y = "metier4", what = "nonsense"))

say("f. no y -> ERROR")
try_show(plot_coverage_heat(catch_data, x = "Country"))

say("g. ggplot heat given a colorblind palette -> warns (status is fixed)")
try_show(plot_coverage_heat(catch_data, y = "metier4", engine = "ggplot",
  palette = "colorblind"))

say("h. numbers plot given `n_per_screen` on the ggplot engine -> warns")
try_show(plot_numbers_at_length_age(catch_data, distribution_data,
  engine = "ggplot", n_per_screen = 1))

say("i. distributionType with no distributions table -> warns")
n5 <- length_numbers(link_dist_to_strata(catch_data, distribution_data,
        "metier4", "Lan", type = "LengthTotal"), "metier4")
try_show(plot_numbers_at_length_age(n5, distributionType = "Age",
  fill = "metier4"))

say("j. GOOD CALLS - no error / warning expected")
try_show(plot_coverage_bar(catch_data, y = "metier4", fill = "Country"))
try_show(plot_coverage_heat(catch_data, y = "metier4", x = "Country",
  what = "length", catchCategory = "Dis", distributions = distribution_data))
try_show(plot_discard_rate(catch_data, x = "metier4", colour = "Country"))
try_show(plot_meanweight_at_length_age(catch_data, distribution_data,
  colour = "Country"))
cat("  (no error: / warning: lines in section j = correct)\n")

## palette that reaches the ggplot engine
say("k. ggplot heat; bar, colorblind palette - drawn")
p5k <- plot_coverage_heat(catch_data, y = "metier4", x = "Country",
  engine = "ggplot")
print(p5k)
p5l <- plot_coverage_bar(catch_data, y = "metier4", fill = "Country",
  engine = "ggplot", palette = "colorblind")
print(p5l)
invisible(grDevices::dev.off())


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

say <- function(l) cat("\n---- 6", l, "----\n")

## ---- 6a. discard: 1 param (coverage only) vs 2 params (+ trips) ------
say("a. discard coverage - min_trip = NULL drops trips from the scheme")
lk_d1 <- cat_lookup(50, "discard", min_trips = NULL)
lk_d2 <- cat_lookup(50, "discard")            # min_trip = 2, active (default)
cat("  1 param categories:", paste(lk_d1$cat_key, collapse = ", "), "\n")
cat("  2 param categories:", paste(lk_d2$cat_key, collapse = ", "), "\n")
# 1 param: only "warn_cov" is reachable (coverage is the only thing that
# CAN fail). 2 params: warn_cov / warn_trip / warn_cov_trip - one row per
# specific combination, not per count - so 3 warning rows, not 2.
stopifnot(nrow(lk_d1) == 6L, nrow(lk_d2) == 8L,
         sum(grepl("^warn_", lk_d1$cat_key)) == 1L,
         sum(grepl("^warn_", lk_d2$cat_key)) == 3L,
         "warn_cov_trip" %in% lk_d2$cat_key,
         !"warn_cov_trip" %in% lk_d1$cat_key)

grDevices::pdf(tempfile())
p6a1 <- plot_coverage_heat(catch_data, y = "metier4", x = "Country",
                           min_trip = NULL)
p6a2 <- plot_coverage_heat(catch_data, y = "metier4", x = "Country")
invisible(dev.off())
cat("  both heatmaps drew:", !is.null(p6a1) && !is.null(p6a2), "\n")

## ---- 6b. length/age: 1, 2 (either extra) or 3 active parameters -----
say("b. length coverage - trips and measurements toggle independently")
lk_l1 <- cat_lookup(50, "length", min_trips = NULL, min_meas = NULL)
lk_l2t <- cat_lookup(50, "length", min_meas = NULL)            # + trips only
lk_l2m <- cat_lookup(50, "length", min_meas = 25, min_trips = NULL) # + meas only
lk_l3  <- cat_lookup(50, "length", min_meas = 25)               # + both
cat("  coverage only        :", paste(lk_l1$cat_key, collapse = ", "), "\n")
cat("  + trips               :", paste(lk_l2t$cat_key, collapse = ", "), "\n")
cat("  + measurements        :", paste(lk_l2m$cat_key, collapse = ", "), "\n")
cat("  + trips & measurements:", paste(lk_l3$cat_key, collapse = ", "), "\n")
# one row per active PARAMETER COUNT choose k (k = 1..n_active), so
# n_active = 1 -> 1 warn row, 2 -> 3, 3 -> 7 (the "all missed" key,
# warn_cov_trip_meas, only exists once trips AND measurements are both on)
stopifnot(nrow(lk_l1) == 5L, nrow(lk_l2t) == 7L, nrow(lk_l2m) == 7L,
         nrow(lk_l3) == 11L,
         !"warn_cov_trip_meas" %in% lk_l2t$cat_key,
         "warn_cov_trip_meas" %in% lk_l3$cat_key)

grDevices::pdf(tempfile())
p6b1 <- plot_coverage_heat(catch_data, y = "metier4", x = "Country",
                           what = "length", distributions = distribution_data,
                           min_trip = NULL)                     # coverage only
p6b2 <- plot_coverage_heat(catch_data, y = "metier4", x = "Country",
                           what = "length", distributions = distribution_data,
                           min_fish = 25)                       # + measurements
invisible(dev.off())
cat("  both length heatmaps drew:", !is.null(p6b1) && !is.null(p6b2), "\n")

## ---- 6c. red stays reserved for "no estimate/distribution at all" ---
## Setting the thresholds absurdly high must NOT turn every stratum red -
## it should crowd them into the worst-active warning shade instead. Red
## (no_dist / no discard estimate) only ever comes from has_disc_data /
## has_dist, never from a threshold comparison, and only on a stratum
## that has weight to begin with.
say("c. extreme thresholds still resolve to a warning shade, not all-red")
sc_extreme <- classify_strata(build_strata(catch_data, c("metier4", "Country")),
                              1500, min_trips = 1000)
tb <- table(sc_extreme$cat_key)
print(tb)
n_no_dist <- if ("no_dist" %in% names(tb)) tb[["no_dist"]] else 0L
stopifnot(n_no_dist < sum(tb))   # not everything fell through to red
cat("  no_dist only where has_disc_data is FALSE:",
   all(!sc_extreme$has_disc_data[sc_extreme$cat_key == "no_dist"]), "\n")

cat("\nDone - ggplot figures in Rplots.pdf; base-R figures are the PNGs",
   "written alongside this script:\n",
   "  11* discard coverage | 12* length + age coverage | 13* discard rate\n",
   "  14* length + age distribution\n")

}  # end: if (!is.null(catch_data))
