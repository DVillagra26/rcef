## ===========================================================
## test_coverage_plots.R
##
## A standalone script for trying out rcef's exploratory plots without
## running the Shiny app. It walks through every plot family the package
## has - discard coverage, length coverage, AGE coverage, discard rate,
## length distributions and age distributions - each with a few options
## to compare against each other, ggplot2 side by side with the base-R
## engine, and the general-purpose arguments (auto-build from raw data,
## verbose, palette, n_per_screen).
##
## Data: the bundled `anchoveta_catch` / `anchoveta_dist` example set - a
## fully synthetic Southeast-Pacific anchoveta CEF pair, lazy-loaded with
## the package, so this script is self-contained (no local file paths, no
## git-ignored folders). `anchoveta_catch` is already through
## derive_fields(); `anchoveta_dist` carries both LengthTotal and Age
## rows, so the LENGTH and AGE blocks below run off the same two tables.
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

## ---- Load the example tables ------------------------------------
## Bundled with the package. Swap in derive_fields(read.csv("your CEF
## catches")) and read.csv("your CEF distributions") to run the same
## figures on real data - everything below is unchanged either way.
data(anchoveta_catch)
data(anchoveta_dist)
catch_data    <- anchoveta_catch
distributions <- anchoveta_dist

## the AGE blocks use the very same tables - anchoveta_dist has Age rows
age_catch <- anchoveta_catch
age_dist  <- anchoveta_dist

cat(sprintf("anchoveta: %d catch rows, %d LengthTotal + %d Age distribution rows\n",
            nrow(catch_data),
            sum(distributions$distributionType == "LengthTotal"),
            sum(distributions$distributionType == "Age")))


## =====================================================================
##  1. DISCARD COVERAGE - plot_coverage_heat / _bubble / _bar / _map
## =====================================================================
## Each takes raw catch and builds + classifies the strata internally.
## `y` is required. heat / bubble put the second category on `x`; bars
## put it on `fill`. min_cov / min_trip colour the heat / bubble status -
## a bar chart ignores them (passing them is an error). Every variant is
## drawn in BOTH engines. The status legend always lists every category.

## ---- 1a. bars: sampled vs not-sampled weight + cumulative staircase -
p1a <- plot_coverage_bar(catch_data, y = "metier4", fill = "Country",
                         verbose = TRUE)
print(p1a)
png("11a_discard_bars_base.png", width = 1300, height = 800, res = 110)
plot_coverage_bar(catch_data, y = "metier4", fill = "Country",
                  engine = "base", palette = "colorblind")
invisible(dev.off())

## ---- 1b. heatmap: status per metier x country x season ------------
p1b <- plot_coverage_heat(catch_data, y = "metier4", x = "Country",
                          facet = "Season", verbose = TRUE)
print(p1b)
png("11d_discard_heatmap_base.png", width = 1400, height = 900, res = 110)
plot_coverage_heat(catch_data, y = "fleetValue", x = "Country",
                   facet = "Season", engine = "base", min_cov = 50, min_trip = 9)
invisible(dev.off())

## ---- 1c. bubbles: weighted status grid ---------------------------
p1c <- plot_coverage_bubble(catch_data, y = "metier4", x = "Country",
                            min_cov = 50, min_trip = 2)
print(p1c)
png("11e_discard_bubbles_base.png", width = 1300, height = 800, res = 110)
plot_coverage_bubble(catch_data, y = "metier4", x = "Country",
                     min_cov = 50, min_trip = 2, engine = "base")
invisible(dev.off())

## ---- 1d-1f. coverage map - one sampled/not-sampled pie per FAO
## area, radius = landings; `group` puts a labelled box of pies per area.
if (requireNamespace("maps", quietly = TRUE)) {
  p1d <- plot_coverage_map(catch_data, engine = "ggplot", verbose = TRUE)
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
## Pass `distributions` and a biology `what`; `catchCategory` chooses the
## landings ("Lan", default), discards ("Dis") or BMS ("BMS") part.
## 2a-2c LENGTH, landings then discards.  2d-2h AGE.

## ---- 2a. length coverage of the LANDINGS, bars -------------------
p2a <- plot_coverage_bar(catch_data, y = "metier4", fill = "Country",
                         what = "length", catchCategory = "Lan",
                         distributions = distributions,
                         verbose = TRUE)
print(p2a)
png("12a_length_bars_base.png", width = 1300, height = 800, res = 110)
plot_coverage_bar(catch_data, y = "metier4", fill = "Country",
                  what = "length", catchCategory = "Lan",
                  distributions = distributions,
                  engine = "base")
invisible(dev.off())

## ---- 2b. length coverage of the DISCARDS, heatmap ---------------
p2b <- plot_coverage_heat(catch_data, y = "metier4", x = "Country",
                          facet = "Season", what = "length",
                          catchCategory = "Dis", distributions = distributions,
                          min_cov = 50, min_fish = 25, min_trip = 2)
print(p2b)
png("12b_length_heatmap_base.png", width = 1400, height = 900, res = 110)
plot_coverage_heat(catch_data, y = "metier4", x = "Country", facet = "Season",
                   what = "length", catchCategory = "Dis",
                   distributions = distributions, min_cov = 50,
                   min_fish = 25, min_trip = 2,
                   engine = "base")
invisible(dev.off())

## ---- 2c. length coverage, bubbles (landings) -------------------
p2c <- plot_coverage_bubble(catch_data, y = "metier4", x = "Country",
                            what = "length", distributions = distributions,
                            min_cov = 50, min_fish = 25, min_trip = 2)
print(p2c)
png("12c_length_bubbles_base.png", width = 1300, height = 800, res = 110)
plot_coverage_bubble(catch_data, y = "metier4", x = "Country",
                     what = "length", distributions = distributions,
                     min_cov = 50, min_fish = 25, min_trip = 2, engine = "base")
invisible(dev.off())

## ---- 2d-2h. AGE coverage, all four views + the map --------------
p2d <- plot_coverage_bar(age_catch, y = "metier4", fill = "Country",
                         what = "age", distributions = age_dist, verbose = TRUE)
print(p2d)
png("12d_age_bars_base.png", width = 1300, height = 800, res = 110)
plot_coverage_bar(age_catch, y = "metier4", fill = "Country", what = "age",
                  distributions = age_dist,
                  engine = "base")
invisible(dev.off())

p2e <- plot_coverage_heat(age_catch, y = "metier4", x = "Country",
                          facet = "Season", what = "age",
                          distributions = age_dist, min_cov = 50,
                          min_fish = 15, min_trip = 2)
print(p2e)
png("12e_age_heatmap_base.png", width = 1400, height = 900, res = 110)
plot_coverage_heat(age_catch, y = "metier4", x = "Country", facet = "Season",
                   what = "age", distributions = age_dist, min_cov = 50,
                   min_fish = 15, min_trip = 2,
                   engine = "base")
invisible(dev.off())

p2f <- plot_coverage_bubble(age_catch, y = "metier4", x = "Country",
                            what = "age", distributions = age_dist,
                            min_cov = 50, min_fish = 15, min_trip = 2)
print(p2f)
png("12f_age_bubbles_base.png", width = 1300, height = 800, res = 110)
plot_coverage_bubble(age_catch, y = "metier4", x = "Country", what = "age",
                     distributions = age_dist, min_cov = 50, min_fish = 15,
                     min_trip = 2, engine = "base")
invisible(dev.off())

if (requireNamespace("maps", quietly = TRUE)) {
  p2h <- plot_coverage_map(age_catch, what = "age", distributions = age_dist,
                           group = "Season",
                           engine = "ggplot")
  print(p2h)
  png("12h_age_map_season_base.png", width = 1050, height = 1000, res = 110)
  plot_coverage_map(age_catch, what = "age", distributions = age_dist,
                    group = "Season", engine = "base")
  invisible(dev.off())
}


## =====================================================================
##  3. DISCARD RATE - plot_discard_rate(), raw per-stratum points
## =====================================================================
## Auto-builds strata from raw catch (no discard_rate column). One view
## now - the boxplot was dropped.

p3a <- plot_discard_rate(catch_data, x = "metier4", colour = "Country",
                         facet = "Season", jitter = TRUE, verbose = TRUE)
print(p3a)
png("13a_discard_rate_raw_base.png", width = 1400, height = 900, res = 110)
plot_discard_rate(catch_data, x = "metier4", colour = "Country",
                  engine = "base")
invisible(dev.off())


## =====================================================================
##  4. LENGTH & AGE DISTRIBUTION - plot_numbers / plot_meanweight
## =====================================================================
## plot_numbers_at_length_age(): histogram / density, variableType is
## always Number. plot_meanweight_at_length_age(): a line, always.
## Both take a prepared table OR raw catch + `distributions` (then the
## link + summary run inside, `catchCategory` choosing the part).
## Numbers in thousands (N3), length in cm, age in years.
##   4a-4c LENGTH   4d-4f AGE

## ---- 4a. numbers at length, stacked histogram by country -------
p4a <- plot_numbers_at_length_age(catch_data, distributions,
         distributionType = "LengthTotal", type = "histogram",
         fill = "Country", stacked = TRUE)
print(p4a)
png("14a_length_hist_base.png", width = 1300, height = 800, res = 110)
plot_numbers_at_length_age(catch_data, distributions,
  distributionType = "LengthTotal", type = "histogram", fill = "Country",
  engine = "base", palette = "colorblind", verbose = TRUE)
invisible(dev.off())

## ---- 4b. numbers at length, smoothed density -------------------
p4b <- plot_numbers_at_length_age(catch_data, distributions,
         type = "density", fill = "Country", smooth = 0.5)
print(p4b)
png("14b_length_density_base.png", width = 1200, height = 900, res = 110)
plot_numbers_at_length_age(catch_data, distributions, type = "density",
  fill = "Country", smooth = 0.5, engine = "base")
invisible(dev.off())

## ---- 4c. mean weight at length, one line per country (discards) --
p4c <- plot_meanweight_at_length_age(catch_data, distributions,
         catchCategory = "Dis", colour = "Country", show_points = TRUE)
print(p4c)
png("14c_length_meanweight_base.png", width = 1200, height = 800, res = 110)
plot_meanweight_at_length_age(catch_data, distributions, colour = "Country",
  show_points = TRUE, engine = "base")
invisible(dev.off())

## ---- 4d-4f. AGE distribution --------------------------------
p4d <- plot_numbers_at_length_age(age_catch, age_dist,
         distributionType = "Age", type = "histogram", fill = "Country",
         stacked = TRUE)
print(p4d)
png("14d_age_hist_base.png", width = 1300, height = 800, res = 110)
plot_numbers_at_length_age(age_catch, age_dist, distributionType = "Age",
  type = "histogram", fill = "Country", engine = "base",
  palette = "colorblind", verbose = TRUE)
invisible(dev.off())

p4e <- plot_numbers_at_length_age(age_catch, age_dist,
         distributionType = "Age", type = "density", fill = "Country",
         smooth = 0.6)
print(p4e)
png("14e_age_density_base.png", width = 1200, height = 900, res = 110)
plot_numbers_at_length_age(age_catch, age_dist, distributionType = "Age",
  type = "density", fill = "Country", smooth = 0.6, engine = "base")
invisible(dev.off())

p4f <- plot_meanweight_at_length_age(age_catch, age_dist,
         distributionType = "Age", colour = "Country", show_points = TRUE,
         show_spread = TRUE)
print(p4f)
png("14f_age_meanweight_base.png", width = 1200, height = 800, res = 110)
plot_meanweight_at_length_age(age_catch, age_dist, distributionType = "Age",
  colour = "Country", show_points = TRUE, engine = "base")
invisible(dev.off())

## ---- 4g-4j. catchCategory sweep - default "All" ("catch"), then the
## landings and the discards, for numbers AND mean weight, length AND age.
## The catch part appears in the plot title.
for (dt in c("LengthTotal", "Age")) {
  for (cc in c("All", "Lan", "Dis")) {
    print(plot_numbers_at_length_age(catch_data, distributions,
            distributionType = dt, catchCategory = cc, fill = "Country"))
    print(plot_meanweight_at_length_age(catch_data, distributions,
            distributionType = dt, catchCategory = cc, colour = "Country"))
  }
}
## and the default really is "All" - no catchCategory passed
stopifnot(grepl("\\(catch\\)",
  plot_numbers_at_length_age(catch_data, distributions,
    distributionType = "Age", fill = "Country")$labels$title))
cat("4g-4j: catchCategory default is \"All\" (title says \"catch\")\n")


## =====================================================================
##  5. ARGUMENT REACTIONS - wrong / partial calls
## =====================================================================
## The split functions error or warn (always, not only under verbose)
## when an argument does not fit. Each call is wrapped so the walkthrough
## runs to the end; watch the "  error:" / "  warning:" lines.

say <- function(l) cat("\n---- 5", l, "----\n")
try_show <- function(expr) tryCatch(
  withCallingHandlers({ force(expr); invisible(NULL) },
    warning = function(w) { message("  warning: ", conditionMessage(w))
      invokeRestart("muffleWarning") }),
  error = function(e) message("  error: ", conditionMessage(e)))

grDevices::pdf(tempfile())

say("a. bar chart given a threshold -> ERROR (points at heat / bubble)")
try_show(plot_coverage_bar(catch_data, y = "metier4", min_cov = 80))

say("b. bar chart given min_trip -> ERROR")
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

say("h. numbers plot given `smooth` on a histogram -> warns")
try_show(plot_numbers_at_length_age(catch_data, distributions,
  type = "histogram", smooth = 0.5))

say("i. distributionType with no distributions table -> warns")
n5 <- length_numbers(link_dist_to_strata(catch_data, distributions,
        "metier4", "Lan", type = "LengthTotal"), "metier4")
try_show(plot_numbers_at_length_age(n5, distributionType = "Age",
  fill = "metier4"))

say("j. GOOD CALLS - no error / warning expected")
try_show(plot_coverage_bar(catch_data, y = "metier4", fill = "Country"))
try_show(plot_coverage_heat(catch_data, y = "metier4", x = "Country",
  what = "length", catchCategory = "Dis", distributions = distributions))
try_show(plot_discard_rate(catch_data, x = "metier4", colour = "Country"))
try_show(plot_meanweight_at_length_age(catch_data, distributions,
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

cat("\nDone - ggplot figures in Rplots.pdf; base-R figures are the PNGs",
   "written alongside this script:\n",
   "  11* discard coverage | 12* length + age coverage | 13* discard rate\n",
   "  14* length + age distribution\n")
