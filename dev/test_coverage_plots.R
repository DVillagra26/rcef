## ===========================================================
## test_coverage_plots.R
##
## A standalone script for trying out rcef's exploratory plots without
## running the Shiny app. It walks through every plot family the package
## has - discard coverage, length coverage, AGE coverage, discard rate,
## length distributions and age distributions - each with a few options
## to compare against each other, ggplot2 side by side with the base-R
## engine, and the general-purpose arguments (auto-build from raw data,
## verbose, palette, angle, n_per_screen).
##
## Data (both loaded up front):
##   bll  turbot, a LengthTotal distribution table - used for the
##        DISCARD and LENGTH views (`catch_data` / `distributions`).
##   pok  saithe, an Age distribution table - used for the AGE views
##        (`age_catch` / `age_dist`). The pok/ folder is git-ignored and
##        not shipped in the tarball; when it is absent every age block
##        is skipped with a note and the rest of the script still runs.
##
## Nothing here filters the data first. The app lets you narrow a
## figure to a subset before drawing it (a fleet, a year, an area); this
## script always plots everything in the loaded table. If you want a
## subset, filter the catch / distributions frame yourself before the
## calls below - every function here takes a plain data frame, so a
## dplyr::filter() upstream is all that is needed.
## ===========================================================

library(dplyr)
library(rcef)

## ---- Load the example tables ------------------------------------
## Bundled with the package, so this runs with no local paths to edit.
## Swap the read.csv() paths for your own CEF files to try the same
## figures on real data - everything below is unchanged either way.
ext <- system.file("extdata", "data_CEF_format", package = "rcef")

## bll: discard + length
catch_data    <- derive_fields(read.csv(
  file.path(ext, "bll_2025_1catches_CEF_v17.1.csv"), stringsAsFactors = FALSE))
distributions <- read.csv(
  file.path(ext, "bll_2025_1distributions_CEF_v17.1.csv"),
  stringsAsFactors = FALSE)

## pok: age. The Age CEF table lives in a git-ignored pok/ subfolder and
## is NOT in the package tarball, so `system.file()` only finds it from a
## source checkout, never from an installed / install_github() build.
## Rather than test one fixed path, probe the likely spots (plus an
## RCEF_AGE_DIR override) and, for each, confirm the distributions file
## really carries Age rows - the top-level pok_2022_*.csv that IS shipped
## is length data, not age, so "the file exists" is not enough.
find_age_data <- function() {
  dirs <- c(Sys.getenv("RCEF_AGE_DIR"),
            file.path(ext, "pok"),
            file.path("inst", "extdata", "data_CEF_format", "pok"),
            file.path("..", "inst", "extdata", "data_CEF_format", "pok"))
  for (d in dirs[nzchar(dirs)]) {
    cf <- file.path(d, "pok_2022_catches_CEF_v17.1.csv")
    df <- file.path(d, "pok_2022_distributions_CEF_v17.1.csv")
    if (!file.exists(cf) || !file.exists(df)) next
    dd <- tryCatch(read.csv(df, stringsAsFactors = FALSE),
                   error = function(e) NULL)
    if (!is.null(dd) && "Age" %in% dd$distributionType)
      return(list(catch = cf, dist = dd, dir = d))
  }
  NULL
}
age_src  <- find_age_data()
have_age <- !is.null(age_src)
if (have_age) {
  age_catch <- derive_fields(read.csv(age_src$catch, stringsAsFactors = FALSE))
  age_dist  <- age_src$dist
  cat(sprintf("age data: pok (%s), %d catch rows, %d Age distribution rows\n",
              age_src$dir, nrow(age_catch),
              sum(age_dist$distributionType == "Age")))
} else {
  age_catch <- age_dist <- NULL
  message("NOTE: no pok Age CEF data found (git-ignored pok/ subfolder, ",
          "not in the tarball). Set RCEF_AGE_DIR to a folder holding ",
          "pok_2022_{catches,distributions}_CEF_v17.1.csv to enable the ",
          "AGE blocks; they are skipped for now.")
}

## =====================================================================
##  1. DISCARD COVERAGE - plot_coverage_heat / _bubble / _bar / _map
## =====================================================================
## Each takes raw catch OR an already-classified table and builds it
## internally. `y` is required. heat / bubble put the second category on
## `x`; bars put it on `fill`. threshold / min_trips colour the heat /
## bubble status - a bar chart ignores them (passing them is an error).
## Every variant is drawn in BOTH engines.

## ---- 1a. bars: sampled vs not-sampled weight + cumulative staircase -
p1a <- plot_coverage_bar(catch_data, y = "fleetValue", fill = "Country",
                         verbose = TRUE)
print(p1a)
png("11a_discard_bars_base.png", width = 1300, height = 800, res = 110)
plot_coverage_bar(catch_data, y = "fleetValue", fill = "Country",
                  engine = "base", palette = "colorblind")
invisible(dev.off())

## ---- 1b. heatmap: status per fleet x country x season --------------
p1b <- plot_coverage_heat(catch_data, y = "fleetValue", x = "Country",
                          facet = "Season", verbose = TRUE)
print(p1b)
png("11d_discard_heatmap_base.png", width = 1400, height = 900, res = 110)
plot_coverage_heat(catch_data, y = "fleetValue", x = "Country",
                   facet = "Season", engine = "base")
invisible(dev.off())

## ---- 1c. bubbles: weighted status grid ---------------------------
p1c <- plot_coverage_bubble(catch_data, y = "fleetValue", x = "Country",
                            min_cov = 50, min_trip = 2)
print(p1c)
png("11e_discard_bubbles_base.png", width = 1300, height = 800, res = 110)
plot_coverage_bubble(catch_data, y = "fleetValue", x = "Country",
                     min_cov = 50, min_trip = 2, engine = "base")
invisible(dev.off())

## ---- 1d-1f. coverage map - one sampled/not-sampled pie per ICES
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
plot_coverage_heat(catch_data, y = "fleetValue", x = "Country",
                   facet = "Season", engine = "base", n_per_screen = 2)
invisible(dev.off())


## =====================================================================
##  2. LENGTH & AGE COVERAGE - what = "length" / "age", + catchCategory
## =====================================================================
## Pass `distributions` and a biology `what`; `catchCategory` chooses the
## landings ("Lan", default), discards ("Dis") or BMS ("BMS") part.
## 2a-2c LENGTH (bll), landings then discards.  2d-2h AGE (pok).

## ---- 2a. length coverage of the LANDINGS, bars -------------------
p2a <- plot_coverage_bar(catch_data, y = "fleetValue", fill = "Country",
                         what = "length", catchCategory = "Lan",
                         distributions = distributions, weight_label = "catch",
                         verbose = TRUE)
print(p2a)
png("12a_length_bars_base.png", width = 1300, height = 800, res = 110)
plot_coverage_bar(catch_data, y = "fleetValue", fill = "Country",
                  what = "length", catchCategory = "Lan",
                  distributions = distributions, weight_label = "catch",
                  engine = "base")
invisible(dev.off())

## ---- 2b. length coverage of the DISCARDS, heatmap ---------------
p2b <- plot_coverage_heat(catch_data, y = "fleetValue", x = "Country",
                          facet = "Season", what = "length",
                          catchCategory = "Dis", distributions = distributions,
                          min_cov = 50, min_fish = 100, min_trip = 2,
                          weight_label = "discards")
print(p2b)
png("12b_length_heatmap_base.png", width = 1400, height = 900, res = 110)
plot_coverage_heat(catch_data, y = "fleetValue", x = "Country", facet = "Season",
                   what = "length", catchCategory = "Dis",
                   distributions = distributions, min_cov = 50,
                   min_fish = 100, min_trip = 2, weight_label = "discards",
                   engine = "base")
invisible(dev.off())

## ---- 2c. length coverage, bubbles (landings) -------------------
p2c <- plot_coverage_bubble(catch_data, y = "fleetValue", x = "Country",
                            what = "length", distributions = distributions,
                            min_cov = 50, min_fish = 100, min_trip = 2,
                            weight_label = "catch")
print(p2c)
png("12c_length_bubbles_base.png", width = 1300, height = 800, res = 110)
plot_coverage_bubble(catch_data, y = "fleetValue", x = "Country",
                     what = "length", distributions = distributions,
                     min_cov = 50, min_fish = 100, min_trip = 2,
                     weight_label = "catch", engine = "base")
invisible(dev.off())

## ---- 2d-2h. AGE coverage, all four views + the map (pok) --------
if (have_age) {
  p2d <- plot_coverage_bar(age_catch, y = "fleetValue", fill = "Country",
                           what = "age", distributions = age_dist,
                           weight_label = "catch", verbose = TRUE)
  print(p2d)
  png("12d_age_bars_base.png", width = 1300, height = 800, res = 110)
  plot_coverage_bar(age_catch, y = "fleetValue", fill = "Country", what = "age",
                    distributions = age_dist, weight_label = "catch",
                    engine = "base")
  invisible(dev.off())

  p2e <- plot_coverage_heat(age_catch, y = "fleetValue", x = "Country",
                            facet = "Season", what = "age",
                            distributions = age_dist, min_cov = 50,
                            min_fish = 50, min_trip = 2, weight_label = "catch")
  print(p2e)
  png("12e_age_heatmap_base.png", width = 1400, height = 900, res = 110)
  plot_coverage_heat(age_catch, y = "fleetValue", x = "Country", facet = "Season",
                     what = "age", distributions = age_dist, min_cov = 50,
                     min_fish = 50, min_trip = 2, weight_label = "catch",
                     engine = "base")
  invisible(dev.off())

  p2f <- plot_coverage_bubble(age_catch, y = "fleetValue", x = "Country",
                              what = "age", distributions = age_dist,
                              min_cov = 50, min_fish = 50, min_trip = 2,
                              weight_label = "catch")
  print(p2f)
  png("12f_age_bubbles_base.png", width = 1300, height = 800, res = 110)
  plot_coverage_bubble(age_catch, y = "fleetValue", x = "Country", what = "age",
                       distributions = age_dist, min_cov = 50, min_fish = 50,
                       min_trip = 2, weight_label = "catch", engine = "base")
  invisible(dev.off())

  if (requireNamespace("maps", quietly = TRUE)) {
    p2h <- plot_coverage_map(age_catch, what = "age", distributions = age_dist,
                             group = "Season", weight_label = "catch",
                             engine = "ggplot")
    print(p2h)
    png("12h_age_map_season_base.png", width = 1050, height = 1000, res = 110)
    plot_coverage_map(age_catch, what = "age", distributions = age_dist,
                      group = "Season", weight_label = "catch", engine = "base")
    invisible(dev.off())
  }
} else message("2d-2h: age blocks skipped (no pok data)")


## =====================================================================
##  3. DISCARD RATE - plot_discard_rate(), raw per-stratum points
## =====================================================================
## Auto-builds strata from raw catch (no discard_rate column). One view
## now - the boxplot was dropped.

p3a <- plot_discard_rate(catch_data, x = "fleetValue", colour = "Country",
                         facet = "Season", jitter = TRUE, verbose = TRUE)
print(p3a)
png("13a_discard_rate_raw_base.png", width = 1400, height = 900, res = 110)
plot_discard_rate(catch_data, x = "fleetValue", colour = "Country",
                  engine = "base", angle = 45)
invisible(dev.off())


## =====================================================================
##  4. LENGTH & AGE DISTRIBUTION - plot_numbers / plot_meanweight
## =====================================================================
## plot_numbers_at_length_age(): histogram / density, variableType is
## always Number. plot_meanweight_at_length_age(): a line, always.
## Both take a prepared table OR raw catch + `distributions` (then the
## link + summary run inside, `catchCategory` choosing the part).
## Numbers in thousands (N3), length in cm, age in years.
##   4a-4c LENGTH (bll)   4d-4f AGE (pok)

## ---- 4a. numbers at length, stacked histogram by fleet ----------
p4a <- plot_numbers_at_length_age(catch_data, distributions,
         distributionType = "LengthTotal", type = "histogram",
         fill = "fleetValue", stacked = TRUE)
print(p4a)
png("14a_length_hist_base.png", width = 1300, height = 800, res = 110)
plot_numbers_at_length_age(catch_data, distributions,
  distributionType = "LengthTotal", type = "histogram", fill = "fleetValue",
  engine = "base", palette = "colorblind", verbose = TRUE)
invisible(dev.off())

## ---- 4b. numbers at length, smoothed density -------------------
p4b <- plot_numbers_at_length_age(catch_data, distributions,
         type = "density", fill = "fleetValue", smooth = 0.5)
print(p4b)
png("14b_length_density_base.png", width = 1200, height = 900, res = 110)
plot_numbers_at_length_age(catch_data, distributions, type = "density",
  fill = "fleetValue", smooth = 0.5, engine = "base")
invisible(dev.off())

## ---- 4c. mean weight at length, one line per fleet (discards) --
p4c <- plot_meanweight_at_length_age(catch_data, distributions,
         catchCategory = "Dis", colour = "fleetValue", show_points = TRUE)
print(p4c)
png("14c_length_meanweight_base.png", width = 1200, height = 800, res = 110)
plot_meanweight_at_length_age(catch_data, distributions, colour = "fleetValue",
  show_points = TRUE, engine = "base")
invisible(dev.off())

## ---- 4d-4f. AGE distribution (pok) ---------------------------
if (have_age) {
  p4d <- plot_numbers_at_length_age(age_catch, age_dist,
           distributionType = "Age", type = "histogram", fill = "fleetValue",
           stacked = TRUE)
  print(p4d)
  png("14d_age_hist_base.png", width = 1300, height = 800, res = 110)
  plot_numbers_at_length_age(age_catch, age_dist, distributionType = "Age",
    type = "histogram", fill = "fleetValue", engine = "base",
    palette = "colorblind", verbose = TRUE)
  invisible(dev.off())

  p4e <- plot_numbers_at_length_age(age_catch, age_dist,
           distributionType = "Age", type = "density", fill = "fleetValue",
           smooth = 0.6)
  print(p4e)
  png("14e_age_density_base.png", width = 1200, height = 900, res = 110)
  plot_numbers_at_length_age(age_catch, age_dist, distributionType = "Age",
    type = "density", fill = "fleetValue", smooth = 0.6, engine = "base")
  invisible(dev.off())

  p4f <- plot_meanweight_at_length_age(age_catch, age_dist,
           distributionType = "Age", colour = "fleetValue", show_points = TRUE,
           show_spread = TRUE)
  print(p4f)
  png("14f_age_meanweight_base.png", width = 1200, height = 800, res = 110)
  plot_meanweight_at_length_age(age_catch, age_dist, distributionType = "Age",
    colour = "fleetValue", show_points = TRUE, engine = "base")
  invisible(dev.off())
} else message("4d-4f: age blocks skipped (no pok data)")


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
try_show(plot_coverage_bar(catch_data, y = "fleetValue", min_cov = 80))

say("b. bar chart given min_trips -> ERROR")
try_show(plot_coverage_bar(catch_data, y = "fleetValue", min_trip = 5))

say("c. catchCategory on a discard coverage plot -> ERROR")
try_show(plot_coverage_heat(catch_data, y = "fleetValue", catchCategory = "Lan"))

say("d. what = 'discards' (plural) is accepted as an alias")
try_show(plot_coverage_heat(catch_data, y = "fleetValue", what = "discards"))

say("e. what = 'nonsense' -> ERROR naming the valid values")
try_show(plot_coverage_bar(catch_data, y = "fleetValue", what = "nonsense"))

say("f. no y -> ERROR")
try_show(plot_coverage_heat(catch_data, x = "Country"))

say("g. ggplot heat given a colorblind palette -> warns (status is fixed)")
try_show(plot_coverage_heat(catch_data, y = "fleetValue", engine = "ggplot",
  palette = "colorblind"))

say("h. numbers plot given `smooth` on a histogram -> warns")
try_show(plot_numbers_at_length_age(catch_data, distributions,
  type = "histogram", smooth = 0.5))

say("i. distributionType with no distributions table -> warns")
n5 <- length_numbers(link_dist_to_strata(catch_data, distributions,
        "fleetValue", "Lan", type = "LengthTotal"), "fleetValue")
try_show(plot_numbers_at_length_age(n5, distributionType = "Age",
  fill = "fleetValue"))

say("j. GOOD CALLS - no error / warning expected")
try_show(plot_coverage_bar(catch_data, y = "fleetValue", fill = "Country"))
try_show(plot_coverage_heat(catch_data, y = "fleetValue", x = "Country",
  what = "length", catchCategory = "Dis", distributions = distributions))
try_show(plot_discard_rate(catch_data, x = "fleetValue", colour = "Country"))
try_show(plot_meanweight_at_length_age(catch_data, distributions,
  colour = "fleetValue"))
cat("  (no error: / warning: lines in section j = correct)\n")

## angle / palette that reach the ggplot engine
say("k. ggplot heat, angle = 90; bar, colorblind palette - drawn")
p5k <- plot_coverage_heat(catch_data, y = "fleetValue", x = "Country",
  engine = "ggplot", angle = 90)
print(p5k)
p5l <- plot_coverage_bar(catch_data, y = "fleetValue", fill = "Country",
  engine = "ggplot", palette = "colorblind")
print(p5l)
invisible(grDevices::dev.off())

cat("\nDone - ggplot figures in Rplots.pdf; base-R figures are the PNGs",
   "written alongside this script:\n",
   "  11* discard coverage | 12* length + age coverage | 13* discard rate\n",
   "  14* length + age distribution\n",
   if (!have_age) "  (age blocks were skipped - no pok data)\n" else "")
