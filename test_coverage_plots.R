## ===========================================================
## test_coverage_plots.R
##
## A standalone script for trying out rcef's exploratory plots without
## running the Shiny app. Loads two bundled example CEF files (catches
## + distributions) and walks through several ways to look at the same
## data - discard coverage, length coverage, discard rate, and length
## distributions - each with a few options to compare against each
## other, ggplot2 side by side with the base-R engine where one exists.
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
library(ggplot2)
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
##  1. DISCARD COVERAGE - several ways to look at the same figure
## =====================================================================

## ---- 1a. the one-step call, verbose --------------------------------
## No build_strata()/classify_strata() beforehand - plot_coverage() does
## both internally. verbose = TRUE prints what it is about to draw: the
## columns on each role, the threshold and min_trips actually used (or,
## for "bars", why neither applies), and how many rows are drawn.
p1a <- plot_coverage(catch_data, type = "bars", y = "fleetValue",
                     fill = "Country", threshold = 50, min_trips = 2,
                     verbose = TRUE)
print(p1a)

## ---- 1b. same figure, base-R engine, same colours -------------------
## engine = "base" instead of ggplot2. Bars are coloured by the SAME
## per-country palette the ggplot version uses (var_palette()), so the
## two are genuinely comparable, not just structurally similar - swap
## engine = "base" for engine = "ggplot" above and the colours line up.
png("11a_discard_bars_base.png", width = 1200, height = 800, res = 110)
plot_coverage(catch_data, type = "bars", y = "fleetValue", fill = "Country",
             engine = "base", verbose = TRUE)
invisible(dev.off())

## ---- 1c. threshold and min_trips genuinely do nothing for "bars" ----
## Confirms 1a/1b's point directly: an extreme threshold changes nothing
## about a bars plot, because it never classifies status-by-threshold in
## the first place - only landings/discards decide which rows are drawn.
p1c <- plot_coverage(catch_data, type = "bars", y = "fleetValue",
                     fill = "Country", threshold = 99, min_trips = 500)
cat("1c: bars plot unchanged by an extreme threshold:",
   identical(p1a$data, p1c$data), "\n\n")

## ---- 1d. heatmap and bubbles DO use the threshold -------------------
## Left NULL here on purpose - both default to 50% / 2 trips, reported
## by verbose = TRUE exactly like 1a did, but this time the numbers are
## real (heatmap/bubbles colour every cell by a status the threshold
## sets).
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

png("11e_discard_bubbles_base.png", width = 1100, height = 750, res = 110)
plot_coverage(catch_data, type = "bubbles", y = "fleetValue", fill = "Country",
              facet = "ICESSubArea",
             threshold = 50, min_trips = 2, engine = "base")
invisible(dev.off())


## =====================================================================
##  2. LENGTH COVERAGE - the same three options, for biology
## =====================================================================
## Length strata need more than build_strata() alone (biology_link(),
## stratum_measurements(), add_sufficiency()), so plot_biology_coverage()
## still expects an already-classified table - this is the one auto-
## build plot_coverage() does NOT do for you (see its own docs for why).
dims_len <- c("catchCategory", "metier4", "Country", "Season")
bl  <- biology_link(catch_data, distributions, dims_len, type = "LengthTotal")
ms  <- stratum_measurements(catch_data, distributions, dims_len, type = "LengthTotal")
bl  <- add_sufficiency(bl, ms, 50, 100)
bio_len <- bl %>%
  dplyr::mutate(landings = weight, landings_with_discards = weight_measured,
               discards = 0, discard_rate = NA_real_)
bio_len <- classify_strata(bio_len, 50, "length", 100, 2)

## ---- 2a. length coverage, bars, both engines ------------------------
p2a <- plot_biology_coverage(bio_len, type = "bars", y = "metier4",
                             fill = "Country", weight_label = "catch",
                             verbose = TRUE)
print(p2a)

png("12a_length_bars_base.png", width = 1200, height = 800, res = 110)
plot_biology_coverage(bio_len, type = "bars", y = "metier4", fill = "Country",
                      weight_label = "catch", engine = "base")
invisible(dev.off())

## ---- 2b. length coverage, heatmap, both engines ---------------------
p2b <- plot_biology_coverage(bio_len, type = "heatmap", y = "metier4",
                             fill = "Country", facet = "Season",
                             threshold = 50, what = "length", min_meas = 100,
                             min_trips = 2, weight_label = "catch")
print(p2b)

png("12b_length_heatmap_base.png", width = 1400, height = 900, res = 110)
plot_biology_coverage(bio_len, type = "heatmap", y = "metier4", fill = "Country",
                      facet = "Season", threshold = 50, what = "length",
                      min_meas = 100, min_trips = 2, weight_label = "catch",
                      engine = "base")
invisible(dev.off())

## ---- 2c. length coverage, bubbles, both engines ----------------------
p2c <- plot_biology_coverage(bio_len, type = "bubbles", y = "metier4",
                             fill = "Country", threshold = 50, what = "length",
                             min_meas = 100, min_trips = 2, weight_label = "catch")
print(p2c)

png("12c_length_bubbles_base.png", width = 1100, height = 750, res = 110)
plot_biology_coverage(bio_len, type = "bubbles", y = "metier4", fill = "Country",
                      threshold = 50, what = "length", min_meas = 100,
                      min_trips = 2, weight_label = "catch", engine = "base")
invisible(dev.off())


## =====================================================================
##  3. DISCARD RATE - per-stratum points and a boxplot summary, both
##     engines
## =====================================================================
## From an already-built strata table (discard_rate is one of
## build_strata()'s own columns, so no separate classification step is
## needed here, unlike coverage).
strata_dr <- build_strata(catch_data, c("metier4", "Country", "Season"))

## ---- 3a. one point per stratum, coloured by country -------------------
p3a <- plot_discard_rate(strata_dr, type = "raw", x = "fleetValue",
                         colour = "Country", facet = "Season", jitter = TRUE)
print(p3a)

## ---- 3a-base. the same figure, base-R engine, same colours ------------
png("13a_discard_rate_raw_base.png", width = 1400, height = 900, res = 110)
plot_discard_rate(strata_dr, type = "raw", x = "metier4", colour = "Country",
                  engine = "base", verbose = TRUE)
invisible(dev.off())

## ---- 3b. the same data as a boxplot, one panel per country -----------
p3b <- plot_discard_rate(strata_dr, type = "box", x = "metier4",
                         fill = "Season", facet = "Country",
                         show_points = TRUE, show_pooled = TRUE)
print(p3b)

## ---- 3b-base. the same boxplot, base-R engine -------------------------
## Red diamonds are the catch-weighted pooled rate, same as the ggplot
## version's - a useful check that a boxplot's per-stratum spread and
## the number actually used downstream are not the same thing.
png("13b_discard_rate_box_base.png", width = 1200, height = 800, res = 110)
plot_discard_rate(strata_dr, type = "box", x = "metier4", fill = "Country", facet= "Season",
                  show_points = TRUE, show_pooled = TRUE, engine = "base")
invisible(dev.off())


## =====================================================================
##  4. LENGTH DISTRIBUTION - numbers at length, and mean weight
## =====================================================================
## Also ggplot2 only. link_dist_to_strata() (not biology_link()) here -
## it keeps every length CLASS as its own row, which a coverage figure
## does not need but a distribution figure is built from.
dims_biology <- c("catchCategory", "fleetValue", "Country", "Season")
linked <- link_dist_to_strata(catch_data, distributions, dims_biology,
                              "Lan", "LengthTotal")

## ---- 4a. numbers at length, stacked histogram by fleet ----------------
d_num <- length_numbers(linked, c("fleetValue", "Country"))
p4a <- plot_biology_distribution(d_num, quantity = "numbers", type = "histogram",
                                 fill = "fleetValue", stacked = TRUE, smooth = 1,
                                 class_label = "Length (cm)")
print(p4a)

## ---- 4a-base. the same histogram, base-R engine, same colours ---------
png("14a_length_hist_base.png", width = 1300, height = 800, res = 110)
plot_biology_distribution(d_num, quantity = "numbers", type = "histogram",
                          fill = "Country", class_label = "Length (cm)",
                          engine = "base", verbose = TRUE)
invisible(dev.off())

## ---- 4b. the same numbers, as a smoothed density instead --------------
p4b <- plot_biology_distribution(d_num, quantity = "numbers", type = "density",
                                 fill = "fleetValue", smooth = 0.5,
                                 class_label = "Length (cm)")
print(p4b)

## ---- 4b-base. the same density, base-R engine -------------------------
## Each curve is drawn as a filled, vertically offset ribbon
## (graphics::polygon()) rather than ggplot2's geom_ribbon() - the same
## weighted kernel density underneath, same "each curve integrates to
## one" comparison.
png("14b_length_density_base.png", width = 1200, height = 800, res = 110)
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
   "individual PNGs written alongside this script.\n")
