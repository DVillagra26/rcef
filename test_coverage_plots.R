## ===========================================================
## test_coverage_plots.R
##
## A standalone script for trying out rcef's exploratory plots without
## running the Shiny app. Loads two bundled example CEF files (catches
## + distributions) and walks through every plot family the package
## has: discard coverage, length coverage, discard rate, and length
## distributions - each with a few options to compare against each
## other, ggplot2 side by side with the base-R engine, and demonstrates
## the newer general-purpose arguments (auto-build from raw data,
## verbose, palette, angle, n_per_screen).
##
## Nothing here filters the data first. The app lets you narrow a
## figure to a subset before drawing it (a fleet, a year, an area); this
## script always plots everything in the loaded table. If you want a
## subset, filter catch_data / distributions yourself before the calls
## below - every function here takes a plain data frame, so a
## dplyr::filter() upstream is all that is needed.
## ===========================================================

library(dplyr)
library(rcef)

## ---- Load two example tables --------------------------------------
## Bundled with the package, so this runs with no local paths to edit.
## Swap these two lines for your own CEF files to try the same figures
## on real data - everything below is unchanged either way.
fx <- system.file("extdata", "data_CEF_format",
                  "bll_2025_1catches_CEF_v17.1.csv", package = "rcef")
fd <- system.file("extdata", "data_CEF_format",
                  "bll_2025_1distributions_CEF_v17.1.csv", package = "rcef")
catch_data    <- derive_fields(read.csv(fx, stringsAsFactors = FALSE))
distributions <- read.csv(fd, stringsAsFactors = FALSE)


## =====================================================================
##  1. DISCARD COVERAGE - one call, no build_strata()/classify_strata()
## =====================================================================

## ---- 1a. the one-step call, verbose --------------------------------
## plot_coverage() builds and classifies the strata internally. verbose
## = TRUE prints what it is about to draw: the columns on each role,
## the threshold/min_trips actually used (or, for "bars", why neither
## applies), and how many rows are drawn. The bars plot now carries a
## cumulative-% -of-landings staircase against a secondary top axis, and
## "Sampled" / "Not sampled" block callouts. threshold / min_trips are
## optional here - bars ignore them.
p1a <- plot_coverage(catch_data, type = "bars", y = "fleetValue",
                     fill = "Country", verbose = TRUE)
print(p1a)

## ---- 1b. same figure, base-R engine, same colours, colorblind palette
## engine = "base" instead of ggplot2; palette = "colorblind" swaps in
## the Okabe-Ito palette instead of the package default. Both engines
## read the SAME per-level colour cache, so switching palette= changes
## both consistently.
png("11a_discard_bars_base.png", width = 1300, height = 800, res = 110)
plot_coverage(catch_data, type = "bars", y = "fleetValue", fill = "Country",
             engine = "base", palette = "colorblind", verbose = TRUE)
invisible(dev.off())

## ---- 1c. threshold and min_trips genuinely do nothing for "bars" ----
## (setting them on a bars plot now warns - see section 5f; suppressed
## here since the point of 1c is only that the DATA is identical)
p1c <- suppressWarnings(plot_coverage(catch_data, type = "bars",
                     y = "fleetValue", fill = "Country",
                     threshold = 99, min_trips = 500))
cat("1c: bars plot unchanged by an extreme threshold:",
   identical(p1a$data, p1c$data), "\n\n")

## ---- 1d. heatmap, both engines - threshold left NULL, defaults to 50%
p1d <- plot_coverage(catch_data, type = "heatmap", y = "fleetValue",
                     fill = "Country", facet = "Season", verbose = TRUE)
print(p1d)

png("11d_discard_heatmap_base.png", width = 1400, height = 900, res = 110)
plot_coverage(catch_data, type = "heatmap", y = "fleetValue", fill = "Country",
             facet = "Season", engine = "base")
invisible(dev.off())

## ---- 1e. bubbles, both engines ---------------------------------------
p1e <- plot_coverage(catch_data, type = "bubbles", y = "fleetValue",
                     fill = "Country", threshold = 50, min_trips = 2)
print(p1e)

png("11e_discard_bubbles_base.png", width = 1300, height = 800, res = 110)
plot_coverage(catch_data, type = "bubbles", y = "fleetValue", fill = "Country",
             threshold = 50, min_trips = 2, engine = "base")
invisible(dev.off())

## ---- 1g. data-availability scatter - "where is the catch vs where is
## the sampling", from the data alone, before any raising. One point per
## stratum: weight on a log x, a sampling metric on y (dashed line =
## threshold), point size = a second metric, colour = status. Only the
## heaviest not-yet-safe strata are labelled. No `y` needed - scatter
## has numeric axes; `scatter_y` / `scatter_size` pick the metrics
## (biology can use "n_measurements"). BOTH engines each time.
p1g <- plot_coverage(catch_data, type = "scatter", verbose = TRUE)
print(p1g)
png("11g_discard_scatter_base.png", width = 1250, height = 780, res = 110)
plot_coverage(catch_data, type = "scatter", engine = "base")
invisible(dev.off())

## ---- 1h. same, biology: y = fish measured, size = trips -------------
p1h <- plot_coverage(catch_data, type = "scatter", what = "length",
                     distributions = distributions, scatter_y = "n_measurements",
                     scatter_size = "numTrips", min_meas = 100)
print(p1h)
png("11h_length_scatter_meas_base.png", width = 1250, height = 780, res = 110)
plot_coverage(catch_data, type = "scatter", what = "length",
              distributions = distributions, scatter_y = "n_measurements",
              scatter_size = "numTrips", min_meas = 100, engine = "base")
invisible(dev.off())

## ---- 1i-1k. coverage map - one sampled/not-sampled pie per ICES area,
## radius = landings. With group_var each area gets a labelled box
## holding a grid of pies, one per level (Season / Country / metier4).
## Every variant is drawn in BOTH engines. Needs the suggested `maps`
## package; skipped if it is not installed.
if (requireNamespace("maps", quietly = TRUE)) {
  ## 1i. one pie per area
  p1i <- plot_coverage_map(catch_data, engine = "ggplot", verbose = TRUE)
  print(p1i)
  png("11i_coverage_map_base.png", width = 1000, height = 950, res = 110)
  plot_coverage_map(catch_data, engine = "base")
  invisible(dev.off())

  ## 1j. boxed grid, one pie per Season per area
  p1j <- plot_coverage_map(catch_data, group_var = "Season", engine = "ggplot")
  print(p1j)
  png("11j_coverage_map_season_base.png", width = 1050, height = 1000, res = 110)
  plot_coverage_map(catch_data, group_var = "Season", engine = "base")
  invisible(dev.off())

  ## 1k. boxed grid by Country
  p1k <- plot_coverage_map(catch_data, group_var = "Country", engine = "ggplot")
  print(p1k)
  png("11k_coverage_map_country_base.png", width = 1050, height = 1000, res = 110)
  plot_coverage_map(catch_data, group_var = "Country", engine = "base")
  invisible(dev.off())

  ## 1l. biology coverage on the map (length), by Season
  p1l <- plot_coverage_map(catch_data, what = "length", distributions = distributions,
                           group_var = "Season", engine = "ggplot")
  print(p1l)
  png("11l_length_map_season_base.png", width = 1050, height = 1000, res = 110)
  plot_coverage_map(catch_data, what = "length", distributions = distributions,
                    group_var = "Season", engine = "base")
  invisible(dev.off())
} else message("1i-1l: 'maps' not installed - coverage map skipped")

## ---- 1f. n_per_screen - split a faceted figure across several separate
## figures instead of one crowded one. With no device already open, this
## opens n_per_screen-worth of SEPARATE devices (several windows/pages
## in the plot viewer, one per group of facet values) - shown here saved
## to one PNG per screen so it is visible without an interactive session.
grDevices::pdf("13f_discard_heatmap_screens.pdf", width = 10, height = 7)
plot_coverage(catch_data, type = "heatmap", y = "fleetValue", fill = "Country",
             facet = "Season", engine = "base", n_per_screen = 2)
invisible(dev.off())


## =====================================================================
##  2. LENGTH / AGE COVERAGE - one call, no biology pipeline of your own
## =====================================================================
## plot_coverage() now takes `distributions` and a biology `what`
## directly: it runs biology_link() / stratum_measurements() /
## add_sufficiency() / classify_strata() internally. (plot_biology_coverage()
## is kept as a thin forwarder for old call sites.) An already-built
## classified table still short-circuits straight through.

## ---- 2a. length coverage, bars - now with the cumulative-% staircase -
p2a <- plot_coverage(catch_data, type = "bars", y = "fleetValue",
                     fill = "Country", what = "length",
                     distributions = distributions, weight_label = "catch",
                     verbose = TRUE)
print(p2a)

png("12a_length_bars_base.png", width = 1300, height = 800, res = 110)
plot_coverage(catch_data, type = "bars", y = "fleetValue", fill = "Country",
              what = "length", distributions = distributions,
              weight_label = "catch", engine = "base")
invisible(dev.off())

## ---- 2b. length coverage, heatmap - abbreviated status legend -------
p2b <- plot_coverage(catch_data, type = "heatmap", y = "fleetValue",
                     fill = "Country", facet = "Season", what = "length",
                     distributions = distributions, threshold = 50,
                     min_meas = 100, min_trips = 2, weight_label = "catch")
print(p2b)

png("12b_length_heatmap_base.png", width = 1400, height = 900, res = 110)
plot_coverage(catch_data, type = "heatmap", y = "fleetValue", fill = "Country",
              facet = "Season", what = "length", distributions = distributions,
              threshold = 50, min_meas = 100, min_trips = 2,
              weight_label = "catch", engine = "base")
invisible(dev.off())

## ---- 2c. length coverage, bubbles - trips shown on sub-threshold cells
p2c <- plot_coverage(catch_data, type = "bubbles", y = "fleetValue",
                     fill = "Country", what = "length",
                     distributions = distributions, threshold = 50,
                     min_meas = 100, min_trips = 2, weight_label = "catch")
print(p2c)

png("12c_length_bubbles_base.png", width = 1300, height = 800, res = 110)
plot_coverage(catch_data, type = "bubbles", y = "fleetValue", fill = "Country",
              what = "length", distributions = distributions, threshold = 50,
              min_meas = 100, min_trips = 2, weight_label = "catch",
              engine = "base")
invisible(dev.off())

## ---- 2d. AGE coverage - what = "age" reads the Age distribution rows -
## Uses the pok/ age fixture if present (not shipped); skipped otherwise.
age_c <- file.path(dirname(fx), "pok", "pok_2022_catches_CEF_v17.1.csv")
age_d <- file.path(dirname(fd), "pok", "pok_2022_distributions_CEF_v17.1.csv")
have_age <- file.exists(age_c) && file.exists(age_d)
if (have_age) {
  acatch <- derive_fields(read.csv(age_c, stringsAsFactors = FALSE))
  adist  <- read.csv(age_d, stringsAsFactors = FALSE)
  p2d <- plot_coverage(acatch, type = "heatmap", y = "fleetValue",
                       fill = "Country", what = "age", distributions = adist,
                       weight_label = "catch")
  print(p2d)
} else message("2d: age fixture not present (inst/extdata/.../pok/) - skipped")


## =====================================================================
##  3. DISCARD RATE - one call, no build_strata() of your own either
## =====================================================================
## plot_discard_rate(catch_data, ...) builds strata internally too -
## detected by the absence of a discard_rate column, the same
## auto-build convenience as the coverage functions.

## ---- 3a. one point per stratum, coloured by country -------------------
p3a <- plot_discard_rate(catch_data, type = "raw", x = "fleetValue",
                         colour = "Country", facet = "Season", jitter = TRUE)
print(p3a)

## ---- 3a-base. the same figure, base-R engine, angle = 0 (horizontal
## labels instead of the default 45 degrees) --------------------------
png("13a_discard_rate_raw_base.png", width = 1400, height = 900, res = 110)
plot_discard_rate(catch_data, type = "raw", x = "fleetValue", colour = "Country",
                  engine = "base", angle = 45, verbose = TRUE)
invisible(dev.off())

## ---- 3b. the same data as a boxplot, one panel per country -----------
p3b <- plot_discard_rate(catch_data, type = "box", x = "fleetValue",
                         fill = "Season", facet = "Country",
                         show_points = TRUE, show_pooled = TRUE)
print(p3b)

## ---- 3b-base. the same boxplot, base-R engine, faceted - the legend
## now appears whether faceted or not (it used to be single-panel only)
png("13b_discard_rate_box_base.png", width = 1400, height = 900, res = 110)
plot_discard_rate(catch_data, type = "box", x = "fleetValue", fill = "Country",
                  facet = "Season", show_points = TRUE, show_pooled = TRUE,
                  engine = "base")
invisible(dev.off())


## =====================================================================
##  4. LENGTH / AGE DISTRIBUTION - direct on catch + distributions
## =====================================================================
## No link_dist_to_strata() / length_numbers() / length_meanweight() of
## your own: plot_biology_distribution(catch_data, distributions,
##   variableType = "Number" | "WeightLive",
##   distributionType = "LengthTotal" | "Age", ...) does it internally.
## Numbers come out in thousands (N3), length in cm; the axis labels are
## taken from the data's own units.

## ---- 4a. numbers at length, stacked histogram by fleet ---------------
p4a <- plot_biology_distribution(catch_data, distributions,
         variableType = "Number", distributionType = "LengthTotal",
         type = "histogram", fill = "fleetValue", stacked = TRUE)
print(p4a)

## ---- 4a-base. the same histogram, base-R engine, colorblind palette --
png("14a_length_hist_base.png", width = 1300, height = 800, res = 110)
plot_biology_distribution(catch_data, distributions, variableType = "Number",
  distributionType = "LengthTotal", type = "histogram", fill = "fleetValue",
  engine = "base", palette = "colorblind", verbose = TRUE)
invisible(dev.off())

## ---- 4b. the same numbers, as a smoothed density instead ------------
p4b <- plot_biology_distribution(catch_data, distributions,
         variableType = "Number", distributionType = "LengthTotal",
         type = "density", fill = "fleetValue", smooth = 0.5)
print(p4b)

## ---- 4b-base. the same density, base-R engine ----------------------
png("14b_length_density_base.png", width = 1200, height = 900, res = 110)
plot_biology_distribution(catch_data, distributions, variableType = "Number",
  distributionType = "LengthTotal", type = "density", fill = "fleetValue",
  smooth = 0.5, engine = "base")
invisible(dev.off())

## ---- 4c. mean weight at length, one line per fleet -----------------
p4c <- plot_biology_distribution(catch_data, distributions,
         variableType = "WeightLive", distributionType = "LengthTotal",
         type = "line", colour = "fleetValue", show_points = TRUE)
print(p4c)

## ---- 4c-base. the same mean-weight line, base-R engine -------------
png("14c_length_meanweight_base.png", width = 1200, height = 800, res = 110)
plot_biology_distribution(catch_data, distributions, variableType = "WeightLive",
  distributionType = "LengthTotal", type = "line", colour = "fleetValue",
  show_points = TRUE, engine = "base")
invisible(dev.off())

## ---- 4d. AGE distribution - same call, distributionType = "Age" ----
if (have_age) {
  png("14d_age_hist_base.png", width = 1300, height = 800, res = 110)
  plot_biology_distribution(acatch, adist, variableType = "Number",
    distributionType = "Age", type = "histogram", fill = "fleetValue",
    engine = "base", verbose = TRUE)
  invisible(dev.off())
} else message("4d: age fixture not present - skipped")

## =====================================================================
##  5. ARGUMENT REACTIONS - deliberately wrong / partial calls
## =====================================================================
## Every plot dispatcher now warns (always, not only under verbose) when
## an argument is set that does not fit the requested plot - e.g. a
## mean-weight "histogram", a discard-rate "raw" plot given a box-only
## `fill`, a "bars" plot given a threshold. It still draws the closest
## sensible figure. Each call below is wrapped so the walkthrough runs to
## the end and prints the warning inline; watch the "  warning: " lines.

say <- function(label) cat("\n---- 5", label, "----\n")
try_show <- function(expr) withCallingHandlers(
  { force(expr); invisible(NULL) },
  warning = function(w) {
    message("  warning: ", conditionMessage(w)); invokeRestart("muffleWarning")
  })

grDevices::pdf(tempfile())   # soak the figures; we only want the messages here

say("a. mean weight asked for as a histogram -> warns, draws a line")
try_show(plot_biology_distribution(catch_data, distributions,
  variableType = "WeightLive", type = "histogram", verbose = TRUE))

say("b. numbers asked for as a line -> warns, draws a histogram")
try_show(plot_biology_distribution(catch_data, distributions,
  quantity = "numbers", type = "line"))

say("c. mean weight given a numbers-only `fill` -> warns")
try_show(plot_biology_distribution(catch_data, distributions,
  variableType = "WeightLive", type = "line", fill = "Country"))

say("d. numbers given a mean-weight-only `colour` -> warns")
try_show(plot_biology_distribution(catch_data, distributions,
  variableType = "Number", type = "histogram", colour = "Country"))

say("e. distributionType set but no distributions table -> warns")
n5 <- length_numbers(
  link_dist_to_strata(catch_data, distributions, "fleetValue", "Lan",
                      type = "LengthTotal"), "fleetValue")
try_show(plot_biology_distribution(n5, quantity = "numbers",
  distributionType = "Age", fill = "fleetValue"))

say("f. bars given a threshold / min_trips -> warns (bars ignore them)")
try_show(plot_coverage(catch_data, type = "bars", y = "fleetValue",
  threshold = 80, min_trips = 5))

say("g. non-scatter type given scatter_y -> warns")
try_show(plot_coverage(catch_data, type = "heatmap", y = "fleetValue",
  scatter_y = "n_measurements"))

say("h. scatter given a `y` -> warns (scatter has numeric axes)")
try_show(plot_coverage(catch_data, type = "scatter", y = "fleetValue"))

say("i. ggplot heatmap given palette = colorblind -> warns (status colours fixed)")
try_show(plot_coverage(catch_data, type = "heatmap", y = "fleetValue",
  engine = "ggplot", palette = "colorblind"))

say("j. discard-rate raw given a box-only `fill` -> warns")
try_show(plot_discard_rate(catch_data, type = "raw", x = "fleetValue",
  fill = "Country"))

say("k. discard-rate box given a raw-only `colour` -> warns")
try_show(plot_discard_rate(catch_data, type = "box", x = "fleetValue",
  colour = "Country"))

say("l. coverage map given palette = colorblind -> warns")
try_show(plot_coverage_map(catch_data, palette = "colorblind"))

say("m. GOOD CALLS - these must print NO warning")
try_show(plot_coverage(catch_data, type = "bars", y = "fleetValue",
  fill = "Country"))
try_show(plot_coverage(catch_data, type = "scatter", what = "length",
  distributions = distributions, scatter_y = "n_measurements"))
try_show(plot_discard_rate(catch_data, type = "box", x = "fleetValue",
  fill = "Country", show_points = TRUE, show_pooled = TRUE))
try_show(plot_biology_distribution(catch_data, distributions,
  variableType = "Number", type = "density", fill = "fleetValue", smooth = 0.6))
cat("  (no 'warning:' lines in section m = correct)\n")

## angle / palette that now genuinely reach the ggplot engine
say("n. ggplot heatmap, angle = 90 + a real recolour target (bars) - drawn, no warning")
p5n <- plot_coverage(catch_data, type = "heatmap", y = "fleetValue",
  fill = "Country", engine = "ggplot", angle = 90)
print(p5n)
p5o <- plot_coverage(catch_data, type = "bars", y = "fleetValue",
  fill = "Country", engine = "ggplot", palette = "colorblind")
print(p5o)
invisible(grDevices::dev.off())

cat("\nDone - ggplot2 figures are in Rplots.pdf; base-R figures are the",
   "individual PNGs (and one PDF, 13f, showing n_per_screen) written",
   "alongside this script.\n")
