## plot_coverage_bars_base: the horizontal sampled/not-sampled bars with
## - a cumulative-% -of-landings staircase read against a secondary top
##   axis (0-100 %), with faint 90/95/100 % markers,
## - "Sampled (NN%)" / "Not sampled (NN%)" callouts past the bars, the
##   label read from the data per contiguous block (rle over draw order),
##   NOT assumed sampled-on-top,
## - a dashed horizontal separator between the two blocks (the piece the
##   base port had been missing) AND the vertical line where the bars end,
## - a y axis via axis_y_cat() (ticks + every row label, text shrunk to
##   fit rather than thinned), full-panel grey guide lines, a left margin
##   sized to the labels, the fill legend in the bottom-right corner.
##
## Source-text assertions strip ALL whitespace from both the deparsed
## body and the needle, so deparse()'s spacing choices cannot flap them.

FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}

strata_raw <- data.frame(
  metier4 = rep(c("OTB", "SDN", "SSC"), 2),
  Country = rep(c("BE", "FR"), each = 3),
  landings = c(100, 80, 60, 40, 30, 20),
  landings_with_discards = c(80, 60, 30, 40, 15, 0),
  discards = c(8, 6, 3, 4, 1.5, 0),
  coverage = c(80, 75, 50, 100, 50, 0),
  discard_rate = c(0.09, 0.09, 0.09, 0.09, 0.09, NA),
  n_measurements = 0, numTrips = c(3, 3, 1, 3, 1, 0),
  stringsAsFactors = FALSE)
strata <- classify_strata(strata_raw, 50, min_trips = 2)

draws_ok <- function(...) tryCatch({
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  plot_coverage_bars_base(strata, ...)
  TRUE
}, error = function(e) conditionMessage(e))

## ---- 1. runs without error, across the shapes that hit different paths -
r1 <- draws_ok("metier4", "Country")
ok(isTRUE(r1), paste("runs with a fill variable:", r1))
r2 <- draws_ok("metier4")
ok(isTRUE(r2), paste("runs with no fill variable at all:", r2))
r3 <- draws_ok("metier4", "Country", facet_var = "Country")
ok(isTRUE(r3), paste("runs faceted:", r3))
## also the ggplot engine builds
gg <- tryCatch(inherits(plot_coverage_bars(strata, "metier4", "Country"), "ggplot"),
               error = function(e) conditionMessage(e))
ok(isTRUE(gg), paste("ggplot engine builds:", gg))

strip <- function(s) gsub("[[:space:]]", "", s)
body_ns <- strip(paste(deparse(body(plot_coverage_bars_base)), collapse = ""))
has <- function(s) grepl(strip(s), body_ns, fixed = TRUE)

## ---- 2. block label read from the data, mapped to Sampled/Not sampled -
ok(has("blocks <- rle(grp)") && has("lvl <- blocks$values[i]"),
   "each block's label is read from the data per contiguous run (rle over draw order)")
ok(has('disp <- if (identical(lvl, wo)) "Not sampled" else "Sampled"'),
   "the callout wording is Sampled / Not sampled, decided against the wo level")
ok(has('paste0(disp, "\\n(", round(pctb), "%)")'),
   "the callout is 'Sampled'/'Not sampled' + '(NN%)' on a second line")
ok(has("pctb <- sum(dd$landings[dd$sampled == lvl]) / total * 100"),
   "each block's percentage is its share of the panel's total weight")

## ---- 3. cumulative-% staircase + secondary top axis ----------------
ok(has("cumv <- cumsum(dd$landings) / total * 100"),
   "the staircase is the cumulative % of landings walking down from the top row")
ok(has('graphics::lines(c(0, mx(cumv))') && has('type = "s"'),
   "it is drawn as a step line (type = 's')")
ok(has("mx <- function(p) p / 100 * dmax"),
   "a helper maps a percentage onto the weight x scale")
ok(has("graphics::axis(3, at = mx(c(0, 25, 50, 75, 100))"),
   "a secondary top axis shows 0-100 % cumulative landings")
ok(has('graphics::mtext("Cumulative % of landings", side = 3'),
   "the top axis is titled")
ok(has("mx(c(90, 95, 100))"),
   "faint 90 / 95 / 100 % reference markers are drawn")

## ---- 4. separators: horizontal between blocks + vertical at bar end -
ok(has("graphics::segments(0, yb, xmax, yb, lty = 2"),
   "a dashed horizontal separator runs between the sampled and not-sampled blocks")
ok(has("graphics::segments(lx, y0, lx, y1, lty = 2"),
   "a dashed vertical line marks where the bars end and the callouts begin")

## ---- 5. y-axis ticks + guide lines + widened region ----------------
ok(has("axis_y_cat(bp[, 1], key[ord], cex = 0.7)"),
   "y axis drawn by axis_y_cat: ticks + every row label, shrunk not thinned")
ok(has("names.arg = NA, axisnames = FALSE"),
   "barplot's own (thinned) names are suppressed in favour of axis_y_cat")
ok(has("xmax <- dmax * 1.3"),
   "the plot region is widened well past the longest bar for the staircase/callouts")
ok(has("grey88") && has("grey92"),
   "full-panel grey guide lines (x ticks + per row) are drawn")
ok(has("y0 <- bp[1, 1] - pitch / 2") && has("y1 <- bp[nrow(bp), 1] + pitch / 2"),
   "guide lines / separators are clamped to the bar band")

## ---- 6. title left-aligned; fill legend off the bars --------------
ok(has("font = 2, adj = 0"),
   "the title is left-margined (adj = 0), not base R's default centred")
ok(!has('legend("topright"') && !has("x = max(dd$landings) * 0.55"),
   "the fill legend is not placed inside the bars' own area, top corner")
ok(has('graphics::legend("bottomright", legend = lv, fill = pal[lv]') &&
     has('bg = grDevices::adjustcolor("white", 0.85)'),
   "the fill legend sits in the bottom-right corner on a semi-opaque box")

cat("\nDONE ", FAIL, "failure(s)", "\n")
