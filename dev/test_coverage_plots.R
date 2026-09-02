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
## applies), and how many rows are drawn.
p1a <- plot_coverage(catch_data, type = "bars", y = "fleetValue",
                     fill = "Country", threshold = 50, min_trips = 2,
                     verbose = TRUE)
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
p1c <- plot_coverage(catch_data, type = "bars", y = "fleetValue",
                     fill = "Country", threshold = 99, min_trips = 500)
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
##  2. LENGTH COVERAGE - one call, two datasets, no biology_link() /
##     stratum_measurements() / add_sufficiency() of your own
## =====================================================================
## plot_biology_coverage(catch_data, distributions, ...) runs the whole
## pipeline internally - the same one-step convenience discard coverage
## already had. Passing an already-built table (the old convention)
## still works unchanged if you have one from elsewhere.

## ---- 2a. length coverage, bars, both engines ------------------------
p2a <- plot_biology_coverage(catch_data, distributions, type = "bars",
                             y = "fleetValue", fill = "Country",
                             weight_label = "catch", verbose = TRUE)
print(p2a)

png("12a_length_bars_base.png", width = 1300, height = 800, res = 110)
plot_biology_coverage(catch_data, distributions, type = "bars",
                      y = "fleetValue", fill = "Country",
                      weight_label = "catch", engine = "base")
invisible(dev.off())

## ---- 2b. length coverage, heatmap, both engines ---------------------
p2b <- plot_biology_coverage(catch_data, distributions, type = "heatmap",
                             y = "fleetValue", fill = "Country", facet = "Season",
                             threshold = 50, what = "length", min_meas = 100,
                             min_trips = 2, weight_label = "catch")
print(p2b)

png("12b_length_heatmap_base.png", width = 1400, height = 900, res = 110)
plot_biology_coverage(catch_data, distributions, type = "heatmap",
                      y = "fleetValue", fill = "Country", facet = "Season",
                      threshold = 50, what = "length", min_meas = 100,
                      min_trips = 2, weight_label = "catch", engine = "base")
invisible(dev.off())

## ---- 2c. length coverage, bubbles, both engines ----------------------
p2c <- plot_biology_coverage(catch_data, distributions, type = "bubbles",
                             y = "fleetValue", fill = "Country",
                             threshold = 50, what = "length", min_meas = 100,
                             min_trips = 2, weight_label = "catch")
print(p2c)

png("12c_length_bubbles_base.png", width = 1300, height = 800, res = 110)
plot_biology_coverage(catch_data, distributions, type = "bubbles",
                      y = "fleetValue", fill = "Country", threshold = 50,
                      what = "length", min_meas = 100, min_trips = 2,
                      weight_label = "catch", engine = "base")
invisible(dev.off())

## ---- 2d. age coverage - what = "age" instead of "length" -------------
p2d <- plot_biology_coverage(catch_data, distributions, type = "bars",
                             y = "fleetValue", what = "age",
                             weight_label = "catch")
print(p2d)


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
##  4. LENGTH DISTRIBUTION - one call, two datasets again
## =====================================================================
## No link_dist_to_strata() / length_numbers() / length_meanweight() of
## your own - plot_biology_distribution(catch_data, distributions, ...)
## does it internally.
dims_biology <- c("catchCategory", "fleetValue", "Country", "Season")
linked <- link_dist_to_strata(catch_data, distributions, dims_biology,
                              "Lan", "LengthTotal")

## ---- 4a. numbers at length, stacked histogram by fleet ----------------
d_num <- length_numbers(linked, "fleetValue")
p4a <- plot_biology_distribution(d_num, quantity = "numbers", type = "histogram",
                                 fill = "fleetValue", stacked = TRUE, smooth = 1,
                                 class_label = "Length (cm)")
print(p4a)

## ---- 4a-base. the same histogram, base-R engine, colorblind palette ---
png("14a_length_hist_base.png", width = 1300, height = 800, res = 110)
plot_biology_distribution(d_num, quantity = "numbers", type = "histogram",
                          fill = "fleetValue", class_label = "Length (cm)",
                          engine = "base", palette = "colorblind", verbose = TRUE)
invisible(dev.off())

## ---- 4b. the same numbers, as a smoothed density instead --------------
p4b <- plot_biology_distribution(d_num, quantity = "numbers", type = "density",
                                 fill = "fleetValue", smooth = 0.5,
                                 class_label = "Length (cm)")
print(p4b)

## ---- 4b-base. the same density, base-R engine -------------------------
## Each curve is drawn as a filled, vertically offset ribbon with a
## black outline (graphics::polygon()), rather than ggplot2's
## geom_ribbon() - the same weighted kernel density underneath, same
## "each curve integrates to one" comparison.
png("14b_length_density_base.png", width = 1200, height = 900, res = 110)
plot_biology_distribution(d_num, quantity = "numbers", type = "density",
                          fill = "fleetValue", smooth = 0.5,
                          class_label = "Length (cm)", engine = "base")
invisible(dev.off())

## ---- 4c. mean weight at length, one line per fleet ---------------------
d_mw <- length_meanweight(linked, "fleetValue")
p4c <- plot_biology_distribution(d_mw, quantity = "meanweight", type = "line",
                                 colour = "fleetValue", show_points = TRUE,
                                 class_label = "Length (cm)")
print(p4c)

## ---- 4c-base. the same mean-weight line, base-R engine -----------------
png("14c_length_meanweight_base.png", width = 1200, height = 800, res = 110)
plot_biology_distribution(d_mw, quantity = "meanweight", type = "line",
                          colour = "fleetValue", show_points = TRUE,
                          class_label = "Length (cm)", engine = "base")
invisible(dev.off())

cat("\nDone - ggplot2 figures are in Rplots.pdf; base-R figures are the",
   "individual PNGs (and one PDF, 13f, showing n_per_screen) written",
   "alongside this script.\n")
