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
ok(grepl("catch strata", plot_coverage_heat(la, y = "metier4")$labels$title) &&
     grepl("landed strata", plot_coverage_heat(la, y = "metier4",
                                               catchCategory = "Lan")$labels$title) &&
     grepl("discarded strata", plot_coverage_heat(la, y = "metier4",
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
## from raw catch the table is built at the finest resolution the catch
## carries, together with the plotted columns - so a point is one raw
## stratum, never several pooled under one shared (x, colour) name
tab_fine <- build_coverage_strata(cd, c("fleetValue", "areaValue", "Country",
                                        "Season", "metier4"))
d_raw <- plot_discard_rate(cd,       x = "metier4", colour = "Country")
d_tab <- plot_discard_rate(tab_fine, x = "metier4", colour = "Country")
ok(drawable(d_raw) && same(d_raw, d_tab),
   "discard rate: raw catch == the finest table, unioned with the plotted columns")
ok(nrow(d_raw$data) > sum(!is.na(tab$rate)),
   "raw catch is not pooled to just the plotted (x, colour) columns")
ok(all(!is.na(d_raw$data$rate)) && nrow(d_raw$data) == sum(!is.na(tab_fine$rate)),
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

## both discard rates are columns of the table: D/L is `rate`, D/(L+D) is
## `discardRate`; a stratum nobody looked at has neither
ok(all(is.na(tab$rate[is.na(tab$total)])) &&
     all(is.na(tab$discardRate[is.na(tab$total)])),
   "a stratum with no discard estimate has neither rate")
has_ld <- !is.na(tab$total) & (tab$weightingTotal + tab$total) > 0
ok("discardRate" %in% names(tab) &&
     isTRUE(all.equal(tab$discardRate[has_ld],
                      (tab$total / (tab$weightingTotal + tab$total))[has_ld])) &&
     all(tab$discardRate <= 1, na.rm = TRUE),
   "the table carries D/(L+D) as discardRate beside D/L as rate, and it never exceeds 100%")
fine_dr <- build_coverage_strata(cd, c("metier4", "Country", "Season"))
pooled <- coverage_cells(fine_dr, NULL, c("metier4", "Country"), "discard", FALSE,
                         NULL, "test")$cells
pooled <- pooled[match(paste(tab$metier4, tab$Country),
                       paste(pooled$metier4, pooled$Country)), ]
ok(isTRUE(all.equal(pooled$rate, tab$rate)) &&
     isTRUE(all.equal(pooled$discardRate, tab$discardRate)),
   "a finer table pooled to metier4 x Country re-derives both rates from the summed weights")

## the y variable
d_ld <- plot_discard_rate(cd, x = "metier4", colour = "Country", rate = "D/(L+D)")
ok(drawable(d_ld) && isTRUE(all.equal(d_ld$data$.y, d_ld$data$discardRate * 100)) &&
     isTRUE(all.equal(d_raw$data$.y, d_raw$data$rate * 100)),
   "rate = 'D/L' plots `rate`, rate = 'D/(L+D)' plots `discardRate` (both in percent)")
ok(!identical(ggplot_build(d_ld)$layout$panel_scales_y[[1]]$trans$name, "pseudo_log") &&
     identical(d_ld$labels$y, "Discard rate (D/(L+D))") &&
     identical(d_raw$labels$y, "Discard rate (D/L)"),
   "D/(L+D) is bounded, so it gets an ordinary axis; the axis is named for the rate")
ok(identical(sort(paste(d_ld$data$metier4, d_ld$data$Country)),
             sort(paste(d_raw$data$metier4, d_raw$data$Country))),
   "both rates draw the same strata")
ok(grepl("should be one of", err(plot_discard_rate(cd, x = "metier4", rate = "nonsense"))),
   "an unknown `rate` names the valid ones")

## the weighted mean of each x level: a thin line behind the points (no
## diamond any more), weighted by the landings
avglines   <- function(p) Filter(function(l) inherits(l$geom, "GeomErrorbar"), p$layers)
pointlayer <- function(p) Filter(function(l) inherits(l$geom, "GeomPoint"), p$layers)
idx <- function(p, l) which(vapply(p$layers, identical, logical(1), y = l))
lin <- avglines(d_raw); pts <- pointlayer(d_raw)
lv  <- build_coverage_strata(cd, "metier4")
mm  <- merge(lin[[1]]$data, lv[c("metier4", "rate")], by = "metier4")
ok(length(lin) == 1 && nrow(mm) == nrow(lin[[1]]$data) &&
     isTRUE(all.equal(mm$.y, mm$rate * 100)),
   "one line per x level, at the landings-weighted mean: the D/L of that level built directly")
ok(!any(vapply(d_raw$layers, function(l) identical(l$aes_params$shape, 23), logical(1))) &&
     identical(lin[[1]]$aes_params$linewidth, 0.4) &&
     idx(d_raw, lin[[1]]) < idx(d_raw, pts[[1]]),
   "no diamond; the mean line is thin (linewidth 0.4) and drawn before the points (behind them)")
dia_ld <- avglines(d_ld)[[1]]$data
ok(all(vapply(seq_len(nrow(dia_ld)), function(i) {
     s <- d_ld$data[d_ld$data$metier4 == dia_ld$metier4[i], ]
     isTRUE(all.equal(stats::weighted.mean(s$.y, s$weightingTotal), dia_ld$.y[i]))
   }, logical(1))),
   "for D/(L+D) the line is the mean of the plotted rates weighted by landings")
d_fac <- plot_discard_rate(cd, x = "metier4", colour = "Country", facet = "Season")
lin_f <- avglines(d_fac)[[1]]$data
ok(all(c("metier4", "Season") %in% names(lin_f)) &&
     all(paste(lin_f$metier4, lin_f$Season) %in%
           paste(d_fac$data$metier4, d_fac$data$Season)),
   "with a facet there is one mean-line level per x level in each panel")

## the reference: a line, and the strata above it named IN FULL - every
## stratum column, each as name=value, since a point is now an individual
## raw stratum and "MIS | Denmark" alone could be any area or season
ref   <- stats::median(d_raw$data$.y) / 100
d_ref <- plot_discard_rate(cd, x = "metier4", colour = "Country", reference = ref)
lab   <- d_ref$data$.lab != ""
ok(any(vapply(d_ref$layers, function(l) inherits(l$geom, "GeomHline"), logical(1))) &&
     !any(vapply(d_raw$layers, function(l) inherits(l$geom, "GeomHline"), logical(1))),
   "a reference draws a line; without one there is none")
ok(any(lab) && all(d_ref$data$.y[lab] > ref * 100) && all(d_ref$data$.y[!lab] <= ref * 100),
   "exactly the strata above the reference are labelled")
sv <- setdiff(names(tab_fine), c("catchCategory", "variableUnit", "weightingCategory",
                                 "total", "weightingTotal", "rate", "discardRate",
                                 "landings", "coverage", "numTrips"))
exp_lab <- do.call(paste, c(lapply(sv, function(v)
                              sprintf("%s=%s", v, d_ref$data[[v]])), sep = " | "))
ok(identical(d_ref$data$.lab[lab], exp_lab[lab]) &&
     all(grepl("metier4=", exp_lab[lab])) && all(grepl("Country=", exp_lab[lab])),
   "a label names every stratum column, each as name=value, joined with ' | '")
ok(grepl("fraction between 0 and 1", err(plot_discard_rate(
     cd, x = "metier4", rate = "D/(L+D)", reference = 30))) &&
     grepl("single number", err(plot_discard_rate(cd, x = "metier4",
                                                  reference = c(0.1, 0.2)))) &&
     grepl("single number", err(plot_discard_rate(cd, x = "metier4",
                                                  reference = "a"))),
   "a reference that is not one fraction is an error (30 for D/(L+D) is named as such)")

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

## ---- 9. titles, subtitles and captions -----------------------------------
## The title says what is sampled and over which weight. The subtitle says
## only what a stratum is (which columns are plotted, so which are pooled
## over). The calculation itself - and anything a legend already explains -
## moved to a short, multi-line caption instead. ggplot2 does not wrap, so
## both carry their own line breaks; read them with those flattened.
ttl  <- function(p) p$labels$title
flat <- function(s) gsub("\n", " ", s, fixed = TRUE)
sbt  <- function(p) flat(p$labels$subtitle)
cap  <- function(p) flat(p$labels$caption)
says     <- function(p, s) grepl(s, sbt(p), fixed = TRUE)
says_cap <- function(p, s) grepl(s, cap(p), fixed = TRUE)

ok(identical(ttl(r_raw), "Discard sampling of landed weight") &&
     identical(sbt(r_raw), "Bar = metier4 x Country, pooled over finer fleet levels (fleetValue), area (areaValue) and season (Season).") &&
     says_cap(r_raw, "staircase = cumulative % of landings weight"),
   "bars: the subtitle names only the strata; the staircase explanation is in the caption")
ok(identical(ttl(h_raw), "Discard sampling of landed strata") &&
     identical(sbt(h_raw), "Stratum = metier4 x Country, pooled over finer fleet levels (fleetValue), area (areaValue) and season (Season).") &&
     says_cap(h_raw, "tile = landed weight (t)") &&
     says_cap(h_raw, "%covered = landed weight w/ discard est. / landed weight"),
   "heatmap: the subtitle names only the strata; the tile value and % covered are in the caption")
ok(identical(ttl(b_raw), ttl(h_raw)) &&
     says_cap(b_raw, "%covered = landed weight w/ discard est. / landed weight") &&
     !says_cap(b_raw, "Bubble area") && !says_cap(b_raw, "bubble area"),
   "bubbles: framed like the heatmap; bubble AREA is not re-explained (the size legend already shows it)")
h_fac <- plot_coverage_heat(cd, y = "metier4", x = "Country", facet = "Season", min_cov = 50)
ok(identical(sbt(h_fac), "Stratum = metier4 x Country x Season, pooled over finer fleet levels (fleetValue) and area (areaValue)."),
   "a facet column is part of the stratum, so it is no longer pooled over")
ok(identical(sbt(h_raw), sbt(h_tab)),
   "the subtitle reads the same from raw catch and from the coverage table")
ok(identical(ttl(lh_raw), "Length sampling of landed strata") &&
     says_cap(lh_raw, "%covered = landed weight w/ length dist. / landed weight") &&
     identical(ttl(plot_coverage_bar(cd, dd, y = "metier4", what = "age",
                                     catchCategory = "Dis")),
               "Age sampling of discarded weight") &&
     identical(ttl(plot_coverage_bar(cd, dd, y = "metier4", what = "length")),
               "Length sampling of catch weight"),
   "titles start with what is sampled (Length / Age / Discard) and end with the weight (landed / discarded / catch)")
ok(grepl("coverage >= 50%", h_raw$labels$caption) &&
     !grepl("trips", h_raw$labels$caption) &&
     grepl("trips >= 2", b_raw$labels$caption),
   "the caption names the thresholds the colours were judged against (only the active ones)")
if (requireNamespace("maps", quietly = TRUE)) {
  mp <- suppressWarnings(plot_coverage_map(cd))
  ok(identical(ttl(mp), "Discard sampling of landed weight by areaValue") &&
       identical(sbt(mp), "Area = areaValue, pooled over fleet (fleetValue), country (Country) and season (Season).") &&
       says_cap(mp, "radius = landed weight (t)") &&
       says_cap(mp, "coloured slice: %covered = landed weight w/ discard est. / landed weight"),
     "map: the subtitle names only the area strata; the radius and coloured slice are in the caption")
  mp_g <- suppressWarnings(plot_coverage_map(cd, group = "Season"))
  ok(identical(sbt(mp_g), "Area = areaValue x Season, pooled over fleet (fleetValue) and country (Country)."),
     "a map group column is part of what each pie is")
}
ok(identical(ttl(d_raw), "Discard rate (D/L) per stratum") &&
     identical(ttl(d_ld), "Discard rate (D/(L+D)) per stratum") &&
     says_cap(d_raw, "D/L = discards / landings(w/ est.)") &&
     says_cap(d_ld, "D/(L+D) = discards / (landings(w/ est.) + discards)") &&
     identical(sbt(d_raw), "Stratum = fleetValue x areaValue x Country x Season x metier4 (nothing pooled).") &&
     says_cap(d_raw, "line = landings-wtd mean per metier4") &&
     !says_cap(d_raw, "dashed") && says_cap(d_ref, "dashed = reference; labels name the strata above it"),
   "discard rate: a formula caption for the rate and the line; the subtitle says nothing is pooled since a point is now a raw stratum; the reference note appears only when used")
ok(identical(sbt(d_fac), sbt(d_raw)),
   "a discard-rate facet column changes nothing further to pool - the strata were already the finest available")
nm <- plot_numbers_at_length_age(cd, dd, catchCategory = "Lan", fill = "Country",
                                 facet = "Season")
ok(identical(sbt(nm), "Stratum = Country x Season, pooled over fleet (fleetValue) and area (areaValue).") &&
     says_cap(nm, "bars = numbers summed over strata") &&
     says_cap(nm, "stacked by Country"),
   "numbers at length: the subtitle names only the strata; the bar meaning and stacking are in the caption")
mw <- plot_meanweight_at_length_age(cd, dd, catchCategory = "Lan", colour = "Country",
                                    facet = "Season")
ok(identical(sbt(mw), "Stratum = Country x Season, pooled over fleet (fleetValue) and area (areaValue).") &&
     says_cap(mw, "line = numbers-wtd mean weight") &&
     says_cap(mw, "band = +/-1 sd across grouped strata"),
   "mean weight: the subtitle names only the strata; the line and band meaning are in the caption")

## the wording helpers
ok(identical(strata_sentence(c("metier4", "Country", "Season")),
             "Stratum = metier4 x Country x Season, pooled over finer fleet levels (fleetValue) and area (areaValue).") &&
     identical(strata_sentence(c("fleetValue", "areaValue", "Country", "Season")),
               "Stratum = fleetValue x areaValue x Country x Season (nothing pooled).") &&
     identical(strata_sentence("metier4", data.frame(metier4 = "x", Country = "y")),
               "Stratum = metier4, pooled over country (Country)."),
   "strata_sentence(): names the shown columns and what is pooled - only families the data has")
ok(identical(strata_sentence(c("donor_group", "domainCatchDis", "metier4")),
             "Stratum = donor_group x domainCatchDis x metier4 (nothing pooled).") &&
     identical(strata_sentence("Stratum"), "Stratum = Stratum (nothing pooled)."),
   "a domain (or Stratum) column already names fleet, area, country and season: nothing is pooled")
ok(identical(cov_title("age", "landings", "weight"), "Age sampling of landed weight") &&
     identical(cov_title("length", "catch", "strata"), "Length sampling of catch strata") &&
     identical(cov_title("discard", "landings", "weight", by = c("a", "b")),
               "Discard sampling of landed weight by a and b") &&
     identical(covered_phrase("age", "discards"),
               "%covered = discarded weight w/ age dist. / discarded weight"),
   "cov_title() / covered_phrase(): the sampled thing first, then the weight")
wrapped <- strsplit(sub_wrap(paste(rep("word", 60), collapse = " "), "short"), "\n")[[1]]
ok(all(nchar(wrapped) <= 96) && identical(wrapped[length(wrapped)], "short"),
   "sub_wrap() wraps each paragraph on its own")
ok(identical(thr_caption(50, 2, 1000),
             "Thresholds: coverage >= 50%, trips >= 2, measured fish >= 1,000") &&
     identical(thr_caption(50), "Thresholds: coverage >= 50%"),
   "thr_caption(): coverage always, trips and fish only when active")

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
