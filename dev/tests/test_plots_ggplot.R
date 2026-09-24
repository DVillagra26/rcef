## The coverage / discard-rate / distribution plots: ggplot only, and
## table-first.
##
## 1. There is no drawing engine to choose and no per-screen splitting -
##    every plot returns a ggplot (or NULL when there is nothing to draw).
## 2. The coverage and discard-rate plots take raw catch OR the table
##    build_coverage_strata() returns from it, and draw the SAME figure
##    from either: what you read as a table is what you see as a figure.
## 3. The sampling status is a colour worked out at plot time from the
##    thresholds; it is not a column of the table.

FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}
suppressPackageStartupMessages({library(dplyr); library(ggplot2)})
err  <- function(expr) tryCatch({ force(expr); "no error" },
                                error = function(e) conditionMessage(e))
warn <- function(expr) tryCatch({ force(expr); "no warning" },
                                warning = function(w) conditionMessage(w))
same <- function(a, b) isTRUE(all.equal(a$data, b$data, check.attributes = FALSE))
drawable <- function(p) inherits(p, "ggplot") &&
  !inherits(try(ggplot_build(p), silent = TRUE), "try-error")

data("anchoveta_catch", package = "rcef", envir = environment())
data("anchoveta_dist",  package = "rcef", envir = environment())
cd <- anchoveta_catch; dd <- anchoveta_dist

## ---- 1. one engine: no `engine`, no `n_per_screen` ----------------------
fns <- c("plot_coverage_heat", "plot_coverage_bubble", "plot_coverage_bar",
         "plot_coverage_map", "plot_discard_rate",
         "plot_numbers_at_length_age", "plot_meanweight_at_length_age")
gone <- c("engine", "n_per_screen")
ok(!any(vapply(fns, function(f) any(gone %in% names(formals(get(f)))),
               logical(1))),
   "none of the seven plot functions takes `engine` or `n_per_screen`")
ok(grepl("unused argument", err(plot_coverage_heat(cd, y = "metier4", engine = "base"))),
   "engine = 'base' is now simply an unused argument")
gone_fns <- c("split_facets_to_screens", "grid_base", "axis_x_cat", "axis_y_cat",
              "legend_overlay", "legend_status_base", "facet_key_base",
              "plot_arg_report", "sampled_for_discards", "collapse_cells")
ok(!any(vapply(gone_fns, exists, logical(1), where = asNamespace("rcef"))),
   "the base-R plumbing and the old collapse / arg-report helpers are gone")

## ---- 2. discard coverage: raw catch and the table draw the same figure --
tab <- build_coverage_strata(cd, c("metier4", "Country"))
h_raw <- plot_coverage_heat(cd,  y = "metier4", x = "Country", min_cov = 50)
h_tab <- plot_coverage_heat(tab, y = "metier4", x = "Country", min_cov = 50)
ok(drawable(h_raw) && same(h_raw, h_tab), "heatmap: raw catch == table")
b_raw <- plot_coverage_bubble(cd,  y = "metier4", x = "Country", min_cov = 50, min_trip = 2)
b_tab <- plot_coverage_bubble(tab, y = "metier4", x = "Country", min_cov = 50, min_trip = 2)
ok(drawable(b_raw) && same(b_raw, b_tab), "bubbles: raw catch == table")
r_raw <- plot_coverage_bar(cd,  y = "metier4", fill = "Country")
r_tab <- plot_coverage_bar(tab, y = "metier4", fill = "Country")
ok(drawable(r_raw) && same(r_raw, r_tab), "bars: raw catch == table")

## a table finer than the plot is pooled to the plotted columns: weights
## add up and coverage is re-derived from them, so the figure is the same
## (only numTrips differs - pooled strata SUM their trips, whereas the
## coarser table takes the largest count among its raw rows)
fine <- build_coverage_strata(cd, c("metier4", "Country", "Season"))
h_fine <- plot_coverage_heat(fine, y = "metier4", x = "Country", min_cov = 50)
agree <- c("metier4", "Country", "landings", "coverage", "cat_key")
ok(isTRUE(all.equal(h_raw$data[agree], h_fine$data[agree],
                    check.attributes = FALSE)),
   "a table built finer (+ Season) is pooled to metier4 x Country: same weights, coverage and colours")
ok(same(plot_coverage_heat(fine, y = "metier4", x = "Country", facet = "Season"),
        plot_coverage_heat(cd,   y = "metier4", x = "Country", facet = "Season")),
   "... and with the finer column facetted, still the same as from raw catch")

## the status is a plot-time colour, not a table column
ok(!any(c("cat_key", "Status") %in% names(tab)) &&
     all(c("cat_key", "Status") %in% names(h_raw$data)),
   "cat_key / Status live in the figure's data, not in the coverage table")

## thresholds are applied at plot time: same table, different colours
t_lo <- ggplot_build(plot_coverage_heat(tab, y = "metier4", x = "Country",
                                        min_cov = 10))$data[[1]]$fill
t_hi <- ggplot_build(plot_coverage_heat(tab, y = "metier4", x = "Country",
                                        min_cov = 95))$data[[1]]$fill
ok(!identical(t_lo, t_hi), "the same table recolours when min_cov changes")

## ---- 3. arguments that do not fit --------------------------------------
ok(grepl("unused argument", err(plot_coverage_bar(cd, y = "metier4", min_cov = 80))),
   "a bar chart takes no min_cov")
ok(grepl("unused argument", err(plot_coverage_bar(cd, y = "metier4", min_trip = 5))),
   "nor min_trip")
ok(grepl("catchCategory does not apply", err(
     plot_coverage_heat(cd, y = "metier4", catchCategory = "Lan"))),
   "catchCategory on a discard plot is an error")
ok(grepl("catchCategory does not apply", err(
     plot_coverage_heat(tab, y = "metier4", catchCategory = "Lan"))),
   "... also when the input is a discard table")
ok(grepl("must be", err(plot_coverage_bar(cd, y = "metier4", what = "nonsense"))),
   "an unknown `what` names the valid ones")
ok(grepl("y", err(plot_coverage_heat(cd, x = "Country"))), "no y is an error")
ok(grepl("not in the coverage table: Season", err(
     plot_coverage_heat(tab, y = "metier4", x = "Season"))),
   "a plotted column the table lacks is named, with what it does have")
ok(grepl("palette does not recolour", warn(
     plot_coverage_heat(cd, y = "metier4", palette = "colorblind"))),
   "a colourblind palette on the status heatmap warns (its colours are fixed)")
ok(drawable(plot_coverage_bar(cd, y = "metier4", fill = "Country",
                              palette = "colorblind")),
   "... but recolours the bars")
ok(drawable(plot_coverage_heat(cd, y = "metier4", what = "discards")),
   "what = 'discards' (plural) is accepted")

## ---- 4. legend spec the app rebuilds its status legend from -------------
sl <- attr(h_raw, "status_legend")
ok(is.list(sl) && identical(sl$what, "discard") && sl$threshold == 50 &&
     is.null(sl$min_trips) && identical(sl$key, "tile"),
   "the heatmap records its status legend spec (min_trips NULL by default)")
ok(inherits(status_legend_plot(sl$threshold, sl$what, sl$min_meas,
                               sl$key, sl$size, sl$min_trips), "ggplot"),
   "and status_legend_plot() rebuilds the legend from it")

## ---- 5. length / age coverage ------------------------------------------
lt <- build_coverage_strata(cd, c("metier4", "Country"), dd, what = "length",
                            catchCategory = "Lan")
lh_raw <- plot_coverage_heat(cd, dd, y = "metier4", x = "Country", what = "length",
                             catchCategory = "Lan", min_cov = 50, min_fish = 100)
lh_tab <- plot_coverage_heat(lt, y = "metier4", x = "Country",
                             min_cov = 50, min_fish = 100)
ok(drawable(lh_raw) && same(lh_raw, lh_tab),
   "length heatmap: raw catch + distributions == the length table (which brings its own what)")
ok(grepl("length coverage table but what", err(
     plot_coverage_heat(lt, y = "metier4", what = "age"))),
   "a `what` that contradicts the table is an error")
ok(grepl("only used when building from raw", warn(
     plot_coverage_heat(lt, dd, y = "metier4", min_cov = 50))),
   "distributions handed in alongside a table are noted as ignored")
la <- build_coverage_strata(cd, c("metier4", "Country"), dd, what = "length")
ok(grepl("\\(catch\\)", plot_coverage_heat(la, y = "metier4")$labels$title) &&
     grepl("\\(landings\\)", plot_coverage_heat(la, y = "metier4",
                                                catchCategory = "Lan")$labels$title) &&
     grepl("\\(discards\\)", plot_coverage_heat(la, y = "metier4",
                                                catchCategory = "Dis")$labels$title),
   "a Lan+Dis table titles itself 'catch'; choosing a part filters it and says which")
ok(drawable(plot_coverage_bar(cd, dd, y = "metier4", fill = "Country",
                              what = "length", catchCategory = "Lan")) &&
     drawable(plot_coverage_bubble(cd, dd, y = "metier4", x = "Country",
                                   what = "age", min_cov = 50)),
   "length bars and age bubbles build from raw catch + distributions")

## ---- 6. the map ---------------------------------------------------------
if (requireNamespace("maps", quietly = TRUE)) {
  mp <- suppressWarnings(plot_coverage_map(cd))
  ok(inherits(mp, "ggplot"), "the coverage map returns a ggplot (no base version)")
  mt <- build_coverage_strata(cd, c("areaValue", "Season"))
  ok(inherits(suppressWarnings(plot_coverage_map(mt, group = "Season")), "ggplot"),
     "and draws from a coverage table")
  ok(!any(c("min_cov", "min_trip", "min_fish") %in% names(formals(plot_coverage_map))),
     "the map takes no thresholds (it never used them)")
}

## ---- 7. discard rate ----------------------------------------------------
d_raw <- plot_discard_rate(cd,  x = "metier4", colour = "Country")
d_tab <- plot_discard_rate(tab, x = "metier4", colour = "Country")
ok(drawable(d_raw) && same(d_raw, d_tab), "discard rate: raw catch == table")
ok(all(!is.na(d_raw$data$rate)) && nrow(d_raw$data) == sum(!is.na(tab$rate)),
   "one point per stratum that has a rate; strata with no discard estimate are not drawn")
ok(identical(ggplot_build(d_raw)$layout$panel_scales_y[[1]]$trans$name,
             "pseudo_log"),
   "the y axis is pseudo-log, so a few huge ratios do not flatten the rest")
ok("size" %in% names(d_raw$mapping),
   "point size is mapped to landings")
ok(grepl("discard coverage table", err(plot_discard_rate(lt, x = "metier4"))),
   "a length coverage table is not a discard-rate input")
ok(is.null(plot_discard_rate(cd[cd$catchCategory == "Lan", ], x = "metier4")),
   "no discard estimate anywhere -> NULL, not an error")

## ---- 8. distribution plots ---------------------------------------------
ok(drawable(plot_numbers_at_length_age(cd, dd, fill = "Country")) &&
     drawable(plot_meanweight_at_length_age(cd, dd, colour = "Country")),
   "numbers and mean weight build from raw catch + distributions")
ok(grepl("only matter when `distributions`", warn(
     plot_numbers_at_length_age(data.frame(.class = 1:3, number = 1:3),
                                distributionType = "Age"))),
   "distributionType without distributions warns")
ok(is.null(plot_meanweight_at_length_age(NULL)), "no data -> NULL")
## a gap of NA sd in the middle of the ribbon must build (the base-R
## version once drew a wedge across it; ggplot breaks the ribbon)
gap <- data.frame(.class = c(20, 25, 30, 35, 40, 45, 50),
                  mean_weight = c(100, 150, 200, 260, 330, 410, 500),
                  sd_weight = c(10, 12, NA, NA, 20, 22, 25), fleetValue = "A")
ok(drawable(plot_meanweight_at_length_age(gap, colour = "fleetValue")),
   "a mean-weight ribbon with an NA gap in the middle builds")

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
