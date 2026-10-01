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
         "plot_coverage_map", "plot_DR",
         "plot_NaLoA", "plot_MWaLoA")
gone <- c("engine", "n_per_screen")
ok(!any(vapply(fns, function(f) any(gone %in% names(formals(get(f)))),
               logical(1))),
   "none of the seven plot functions takes `engine` or `n_per_screen`")
ok(grepl("unused argument", err(plot_coverage_heat(cd, y = "metier4", engine = "base"))),
   "engine = 'base' is now simply an unused argument")
gone_fns <- c("split_facets_to_screens", "grid_base", "axis_x_cat", "axis_y_cat",
              "legend_overlay", "legend_status_base", "facet_key_base",
              "plot_arg_report", "sampled_for_discards", "collapse_cells",
              "covered_phrase", "thr_caption",
              "plot_numbers_at_length_age", "plot_meanweight_at_length_age")
ok(!any(vapply(gone_fns, exists, logical(1), where = asNamespace("rcef"))),
   "the base-R plumbing, the old wording helpers and the old function names are gone")

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
ok(grepl("min_fish does not apply to what = \"discard\"", err(
     plot_coverage_heat(cd, y = "metier4", min_fish = 100))) &&
     grepl("min_fish does not apply to what = \"discard\"", err(
       plot_coverage_bubble(cd, y = "metier4", min_fish = 100))),
   "min_fish on a discard heat/bubble is an error")
ok(grepl("min_fish does not apply", err(
     plot_coverage_heat(tab, y = "metier4", min_fish = 100))),
   "... also when the input is a discard table")
ok(drawable(plot_coverage_heat(cd, y = "metier4")),
   "min_fish left at its default (NULL) is fine on a discard plot")

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
ok(identical(plot_coverage_heat(la, y = "metier4")$labels$title,
             "Catch - Sampling of Length") &&
     identical(plot_coverage_heat(la, y = "metier4",
                                  catchCategory = "Lan")$labels$title,
               "Landings - Sampling of Length") &&
     identical(plot_coverage_heat(la, y = "metier4",
                                  catchCategory = "Dis")$labels$title,
               "Discards - Sampling of Length"),
   "a Lan+Dis table titles itself 'Catch'; choosing a part filters it and says which")
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
d_raw <- plot_DR(cd,       x = "metier4", colour = "Country")
d_tab <- plot_DR(tab_fine, x = "metier4", colour = "Country")
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
ok(min(d_raw$data$landings) >= max(d_raw$data$landings) * 0.15 - 1e-9,
   "no bubble sizes below 15% of the largest - a tiny stratum's colour stays readable")
ok(grepl("discard coverage table", err(plot_DR(lt, x = "metier4"))),
   "a length coverage table is not a discard-rate input")
ok(is.null(plot_DR(cd[cd$catchCategory == "Lan", ], x = "metier4")),
   "no discard estimate anywhere -> NULL, not an error")

## both discard rates are columns of the table: the ratio is `rate`, the
## rate is `discardRate`; a stratum nobody looked at has neither
ok(all(is.na(tab$rate[is.na(tab$total)])) &&
     all(is.na(tab$discardRate[is.na(tab$total)])),
   "a stratum with no discard estimate has neither rate")
has_ld <- !is.na(tab$total) & (tab$weightingTotal + tab$total) > 0
ok("discardRate" %in% names(tab) &&
     isTRUE(all.equal(tab$discardRate[has_ld],
                      (tab$total / (tab$weightingTotal + tab$total))[has_ld])) &&
     all(tab$discardRate <= 1, na.rm = TRUE),
   "the table carries the rate as discardRate beside the ratio as rate, and it never exceeds 100%")
fine_dr <- build_coverage_strata(cd, c("metier4", "Country", "Season"))
pooled <- coverage_cells(fine_dr, NULL, c("metier4", "Country"), "discard", FALSE,
                         NULL, "test")$cells
pooled <- pooled[match(paste(tab$metier4, tab$Country),
                       paste(pooled$metier4, pooled$Country)), ]
ok(isTRUE(all.equal(pooled$rate, tab$rate)) &&
     isTRUE(all.equal(pooled$discardRate, tab$discardRate)),
   "a finer table pooled to metier4 x Country re-derives both rates from the summed weights")

## the y variable
d_ld <- plot_DR(cd, x = "metier4", colour = "Country", rate = "rate")
ok(drawable(d_ld) && isTRUE(all.equal(d_ld$data$.y, d_ld$data$discardRate * 100)) &&
     isTRUE(all.equal(d_raw$data$.y, d_raw$data$rate * 100)),
   "rate = 'ratio' plots `rate`, rate = 'rate' plots `discardRate` (both in percent)")
ok(!identical(ggplot_build(d_ld)$layout$panel_scales_y[[1]]$trans$name, "pseudo_log") &&
     identical(d_ld$labels$y, "Discard rate (D/(L+D))") &&
     identical(d_raw$labels$y, "Discard ratio (D/L)"),
   "the rate is bounded, so it gets an ordinary axis; the axis is named for the rate, formula included")
ok(identical(sort(paste(d_ld$data$metier4, d_ld$data$Country)),
             sort(paste(d_raw$data$metier4, d_raw$data$Country))),
   "both rates draw the same strata")
ok(grepl("should be one of", err(plot_DR(cd, x = "metier4", rate = "nonsense"))),
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
   "one line per x level, at the landings-weighted mean: the ratio of that level built directly")
ok(!any(vapply(d_raw$layers, function(l) identical(l$aes_params$shape, 23), logical(1))) &&
     identical(lin[[1]]$aes_params$linewidth, 0.4) &&
     idx(d_raw, lin[[1]]) < idx(d_raw, pts[[1]]),
   "no diamond; the mean line is thin (linewidth 0.4) and drawn before the points (behind them)")
dia_ld <- avglines(d_ld)[[1]]$data
ok(all(vapply(seq_len(nrow(dia_ld)), function(i) {
     s <- d_ld$data[d_ld$data$metier4 == dia_ld$metier4[i], ]
     isTRUE(all.equal(stats::weighted.mean(s$.y, s$weightingTotal), dia_ld$.y[i]))
   }, logical(1))),
   "for the rate the line is the mean of the plotted rates weighted by landings")
d_fac <- plot_DR(cd, x = "metier4", colour = "Country", facet = "Season")
lin_f <- avglines(d_fac)[[1]]$data
ok(all(c("metier4", "Season") %in% names(lin_f)) &&
     all(paste(lin_f$metier4, lin_f$Season) %in%
           paste(d_fac$data$metier4, d_fac$data$Season)),
   "with a facet there is one mean-line level per x level in each panel")

## the reference: a line, and the strata above it named IN FULL but WITHOUT
## naming the variables - bare values, only the finest column of each
## family (metier4 dropped in favour of the finer fleetValue), joined by
## "|" - a point is now an individual raw stratum, so its label identifies
## it fully, and "MIS|Denmark" alone could still be any area or season
ref   <- stats::median(d_raw$data$.y) / 100
d_ref <- plot_DR(cd, x = "metier4", colour = "Country", reference = ref)
lab   <- d_ref$data$.lab != ""
ok(any(vapply(d_ref$layers, function(l) inherits(l$geom, "GeomHline"), logical(1))) &&
     !any(vapply(d_raw$layers, function(l) inherits(l$geom, "GeomHline"), logical(1))),
   "a reference draws a line; without one there is none")
ok(any(lab) && all(d_ref$data$.y[lab] > ref * 100) && all(d_ref$data$.y[!lab] <= ref * 100),
   "exactly the strata above the reference are labelled")
sv <- setdiff(names(tab_fine), c("catchCategory", "variableUnit", "weightingCategory",
                                 "total", "weightingTotal", "rate", "discardRate",
                                 "landings", "coverage", "numTrips"))
lab_vars <- finest_per_family(sv)
exp_lab <- do.call(paste, c(lapply(lab_vars, function(v) d_ref$data[[v]]), sep = "|"))
ok(identical(d_ref$data$.lab[lab], exp_lab[lab]) &&
     !any(grepl("=", exp_lab[lab])) &&
     identical(lab_vars, c("fleetValue", "areaValue", "Country", "Season")),
   "a label is bare values, finest-per-family only (metier4 dropped for fleetValue), joined with '|'")
ok(grepl("fraction between 0 and 1", err(plot_DR(
     cd, x = "metier4", rate = "rate", reference = 30))) &&
     grepl("single number", err(plot_DR(cd, x = "metier4",
                                                  reference = c(0.1, 0.2)))) &&
     grepl("single number", err(plot_DR(cd, x = "metier4",
                                                  reference = "a"))),
   "a reference that is not one fraction is an error (30 for the rate is named as such)")

## ---- 8. distribution plots ---------------------------------------------
ok(drawable(plot_NaLoA(cd, dd, fill = "Country")) &&
     drawable(plot_MWaLoA(cd, dd, colour = "Country")),
   "numbers and mean weight build from raw catch + distributions")
ok(grepl("only matter when `distributions`", warn(
     plot_NaLoA(data.frame(.class = 1:3, number = 1:3),
               distributionType = "Age"))),
   "distributionType without distributions warns")
ok(is.null(plot_MWaLoA(NULL)), "no data -> NULL")
## a gap of NA sd in the middle of the ribbon must build (the base-R
## version once drew a wedge across it; ggplot breaks the ribbon)
gap <- data.frame(.class = c(20, 25, 30, 35, 40, 45, 50),
                  mean_weight = c(100, 150, 200, 260, 330, 410, 500),
                  sd_weight = c(10, 12, NA, NA, 20, 22, 25), fleetValue = "A")
ok(drawable(plot_MWaLoA(gap, colour = "fleetValue")),
   "a mean-weight ribbon with an NA gap in the middle builds")

## ---- 9. titles, captions and the Thresholds box --------------------------
## The title says what is sampled and over which weight. There is no
## subtitle any more: the caption's first line says what a stratum is
## (grouping = ..., grouped over ...), and the rest is a short, formula-like
## explanation of the marks - never repeating what a legend already shows.
## On the heatmap and bubble grid the active thresholds move into their own
## boxed guide (ggplot2::guide_custom()), bottom right under the in-panel
## status legend, rather than living in the caption at all.
ttl  <- function(p) p$labels$title
flat <- function(s) gsub("\n", " ", s, fixed = TRUE)
cap  <- function(p) flat(p$labels$caption)
says_cap <- function(p, s) grepl(s, cap(p), fixed = TRUE)
box_text <- function(p) {
  g <- p$guides$guides$thresholds
  if (is.null(g)) NA_character_ else flat(g$params$grob$label)
}

ok(identical(ttl(r_raw), "Sampling of Discards") &&
     is.null(r_raw$labels$subtitle) &&
     says_cap(r_raw, "grouping = metier4 x Country, grouped over finer fleet levels (fleetValue), area (areaValue), and season.") &&
     says_cap(r_raw, "staircase = cumulative % of landings weight (top axis).") &&
     says_cap(r_raw, "bar = landings weight (bottom axis)."),
   "bars: no subtitle; the caption names the grouping, the staircase and the bar")
ok(identical(ttl(h_raw), "Sampling of Discards") &&
     is.null(h_raw$labels$subtitle) &&
     says_cap(h_raw, "grouping = metier4 x Country, grouped over finer fleet levels (fleetValue), area (areaValue), and season.") &&
     says_cap(h_raw, "tile number = landings weight (t).") &&
     !says_cap(h_raw, "coverage >="),
   "heatmap: the caption names the grouping and the tile; the thresholds are not in it")
ok(identical(box_text(h_raw),
             "coverage >= 50%: share of landed weight with discard estimates. labels: each unmet threshold is shown as a label."),
   "heatmap: the Thresholds box states the active coverage threshold as a formula, and nothing inactive")
ok(identical(ttl(b_raw), ttl(h_raw)) &&
     says_cap(b_raw, "bubble size = landings weight (t).") &&
     !says_cap(b_raw, "Bubble area") && !says_cap(b_raw, "bubble area"),
   "bubbles: framed like the heatmap; bubble AREA is not re-explained (the size legend already shows it)")
ok(grepl("trips >= 2:", box_text(b_raw), fixed = TRUE),
   "bubbles: an active trips threshold gets its own line in the box, with its formula")
h_fac <- plot_coverage_heat(cd, y = "metier4", x = "Country", facet = "Season", min_cov = 50)
ok(says_cap(h_fac, "grouping = metier4 x Country x Season, grouped over finer fleet levels (fleetValue) and area (areaValue)."),
   "a facet column is part of the stratum, so it is no longer grouped over")
ok(identical(cap(h_raw), cap(h_tab)),
   "the caption reads the same from raw catch and from the coverage table")
ok(identical(ttl(lh_raw), "Landings - Sampling of Length") &&
     grepl("measured fish >= 100: fish measured for NaL/MWaL estimates", box_text(lh_raw)) &&
     identical(ttl(plot_coverage_bar(cd, dd, y = "metier4", what = "age",
                                     catchCategory = "Dis")),
               "Discards - Sampling of Age") &&
     identical(ttl(plot_coverage_bar(cd, dd, y = "metier4", what = "length")),
               "Catch - Sampling of Length"),
   "titles name the weight first (Landings / Discards / Catch), then what is sampled - dropped entirely for discard coverage, always the landings")
ok(grepl("coverage >= 50%", box_text(h_raw)) &&
     !grepl("trips", box_text(h_raw)) &&
     grepl("trips >= 2", box_text(b_raw)),
   "the box names the thresholds the colours were judged against (only the active ones)")
if (requireNamespace("maps", quietly = TRUE)) {
  mp <- suppressWarnings(plot_coverage_map(cd))
  ok(identical(ttl(mp), "Sampling of Discards by areaValue") &&
       is.null(mp$labels$subtitle) &&
       says_cap(mp, "grouping = areaValue, grouped over fleet (fleetValue), country, and season.") &&
       says_cap(mp, "area = areaValue.") &&
       says_cap(mp, "bubble size = landings weight (t).") &&
       says_cap(mp, "coverage slice (green) = share of landed weight with discard estimates."),
     "map: no subtitle; the caption names the grouping, the area, the size and the coloured slice")
  mp_g <- suppressWarnings(plot_coverage_map(cd, group = "Season"))
  ok(says_cap(mp_g, "grouping = Season x areaValue, grouped over fleet (fleetValue) and country."),
     "a map group column is part of what each pie is, named before the area")
}
ok(identical(ttl(d_raw), "Discard ratio per stratum") &&
     identical(ttl(d_ld), "Discard rate per stratum") &&
     says_cap(d_raw, "Discard ratio = discard/landings weight.") &&
     says_cap(d_ld, "Discard rate = discard/catch weight.") &&
     says_cap(d_raw, "grouping = no grouping, raw data as in CEF (CN) data.") &&
     says_cap(d_raw, "horizontal line = landings weighted average per metier4.") &&
     !says_cap(d_raw, "dashed") &&
     says_cap(d_ref, "dashed line = reference arbitrary threshold in the same units as y axis."),
   "discard rate: a formula caption for the rate and the line; grouping says nothing is grouped since a point is now a raw stratum; the reference note appears only when used")
ok(says_cap(d_ref, sprintf("labels = strata ID exceeding reference line (%s).",
                          paste(lab_vars, collapse = "|"))),
   "discard rate: the labels line names the same finest-per-family columns the labels themselves use")
ok(identical(cap(d_fac), cap(d_raw)),
   "a discard-rate facet column changes nothing further to group - the strata were already the finest available")
nm <- plot_NaLoA(cd, dd, catchCategory = "Lan", fill = "Country", facet = "Season")
ok(says_cap(nm, "grouping = Country x Season, grouped over fleet (fleetValue) and area (areaValue).") &&
     says_cap(nm, "bars = Landings NaL summed over strata.") &&
     says_cap(nm, "stacked by Country."),
   "numbers at length: the caption names the grouping, the bars (with the NaL acronym) and the stacking")
mw <- plot_MWaLoA(cd, dd, catchCategory = "Lan", colour = "Country", facet = "Season")
ok(says_cap(mw, "grouping = Country x Season, grouped over fleet (fleetValue) and area (areaValue).") &&
     says_cap(mw, "dots and lines = Landings MWaL weight averaged by NaL.") &&
     says_cap(mw, "ribbon = +/- 1sd across grouped strata."),
   "mean weight: the caption names the grouping, the dots/line (MWaL averaged by NaL) and the ribbon")
mw_af <- plot_MWaLoA(cd, dd, distributionType = "Age", catchCategory = "Lan",
                     colour = "Country", facet = "Season", show_samples = TRUE)
ok(says_cap(mw_af, "dots and lines = Landings MWaA weight averaged by NaA (left axis).") &&
     says_cap(mw_af, "bars = fish measured (right axis).") &&
     says_cap(mw_af, "bars scaled per panel (right axis exact for the panel with the most fish)."),
   "mean weight at age, faceted with bars: the left-axis note appears, and bars are scaled per panel")

## the wording helpers
ok(identical(strata_sentence("metier4"),
             "grouping = metier4, grouped over finer fleet levels (fleetValue), area (areaValue), country, and season.") &&
     identical(strata_sentence(c("metier4", "Country", "Season")),
               "grouping = metier4 x Country x Season, grouped over finer fleet levels (fleetValue) and area (areaValue).") &&
     identical(strata_sentence(c("fleetValue", "areaValue", "Country", "Season")),
               "grouping = no grouping, raw data as in CEF (CN) data.") &&
     identical(strata_sentence("metier4", data.frame(metier4 = "x", Country = "y")),
               "grouping = metier4, grouped over country."),
   "strata_sentence(): names the shown columns and what is grouped over - only families the data has; country/season carry no parenthetical")
ok(identical(strata_sentence(c("donor_group", "domainCatchDis", "metier4")),
             "grouping = no grouping, raw data as in CEF (CN) data.") &&
     identical(strata_sentence("Stratum"), "grouping = no grouping, raw data as in CEF (CN) data."),
   "a domain (or Stratum) column already names fleet, area, country and season: nothing is grouped over")
ok(identical(cov_title("age", "landings"), "Landings - Sampling of Age") &&
     identical(cov_title("length", "catch"), "Catch - Sampling of Length") &&
     identical(cov_title("discard", "landings", by = c("a", "b")),
               "Sampling of Discards by a and b") &&
     identical(share_phrase("age", "discards"),
               "share of discarded weight with age distributions"),
   "cov_title(): the weight first (dropped for discard coverage), then what is sampled / share_phrase()")
wrapped <- strsplit(sub_wrap(paste(rep("word", 60), collapse = " "), "short"), "\n")[[1]]
ok(all(nchar(wrapped) <= 96) && identical(wrapped[length(wrapped)], "short"),
   "sub_wrap() wraps each paragraph on its own")
ok(identical(thr_lines(50, 2, 1000, "length", "landings"),
             c("coverage >= 50%: share of landed weight with length distributions.",
               "trips >= 2: sampled trips behind the length distributions, summed across strata.",
               "measured fish >= 1,000: fish measured for NaL/MWaL estimates, summed across strata.",
               "labels: each unmet threshold is shown as a label.")) &&
     identical(thr_lines(50, NULL, NULL, "discard", "landings"),
               c("coverage >= 50%: share of landed weight with discard estimates.",
                 "labels: each unmet threshold is shown as a label.")),
   "thr_lines(): coverage always, trips and fish only when active, fish never for discard")
ok(identical(finest_per_family(c("fleetValue", "areaValue", "Country", "Season", "metier4")),
             c("fleetValue", "areaValue", "Country", "Season")) &&
     identical(finest_per_family(c("metier4", "gear", "Country")),
               c("metier4", "gear", "Country")) &&
     identical(finest_per_family(c("ICESDivision", "areaValue")), "areaValue"),
   "finest_per_family(): drops a coarser column when a finer one is present; ties (metier4/gear) both kept")

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
