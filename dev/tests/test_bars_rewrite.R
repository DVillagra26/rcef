## The bars plot rewrite (plot_coverage_bars_base): a base-R port that
## tracks the ggplot reference (plot_coverage_bars) - a dashed vertical
## separator past the bars, horizontal "With / Without ... sampling (NN%)"
## callouts in the right margin, full-panel grey guide lines, a
## left-aligned title/subtitle split, a left margin sized to the labels,
## and the fill legend moved off the bars into the margin.
##
## A real bug caught while reviewing this by hand, not by running it:
## the "With"/"Without" callouts were placed with a HARDCODED top/bottom
## assumption (ymid_top always got "wi", ymid_bot always got "wo"),
## which is backwards whenever barplot()'s own bottom-to-top drawing
## order does not happen to put "wi" on top - which it usually does not,
## since the size sort decides which block ends up literally uppermost.
## Fixed by reading each block's label straight off the data
## (rle(grp)$values) at that block's own rows.
##
## Source-text assertions strip ALL whitespace from both the deparsed
## body and the needle before matching, so deparse()'s own spacing
## choices cannot make them flap.

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

strip <- function(s) gsub("[[:space:]]", "", s)
body_ns <- strip(paste(deparse(body(plot_coverage_bars_base)), collapse = ""))
has <- function(s) grepl(strip(s), body_ns, fixed = TRUE)

## ---- 2. the label-swap bug is fixed - checked at the source ----------
ok(has("blocks <- rle(grp)") && has("lvl <- blocks$values[i]"),
   "each callout's label is read from the data per contiguous block (rle(grp)$values)")
ok(!has("ymid_top") && !has("ymid_bot"),
   "the old hardcoded ymid_top / ymid_bot positional mapping (the bug) is gone")

## ---- 3. the callouts carry the per-block share of the panel total ----
ok(has("pctb <- sum(dd$landings[dd$sampled == lvl]) / total * 100"),
   "each block's percentage is its share of the panel's total weight")
ok(has('paste0(lvl, "\\n(", round(pctb), "%)")'),
   "the callout is 'With/Without ... sampling' + '(NN%)' on a second line")

## ---- 4. xlim is extended past the data's own max --------------------
ok(has("xmax <- max(dd$landings) *"),
   "xlim extends past the largest bar")

## ---- 5. full-panel guide lines, clamped to the bar band -------------
ok(has("grey88") && has("grey92"),
   "vertical (per x tick) and horizontal (per row) grey guide lines are drawn")
ok(has("y0 <- bp[1, 1] - pitch / 2") && has("y1 <- bp[nrow(bp), 1] + pitch / 2"),
   "guide lines and separator are clamped to the bar band, not the raw device usr")
ok(has("graphics::segments(lx, y0, lx, y1, lty = 2"),
   "the dashed separator spans exactly that clamped band")

## ---- 6. title and subtitle are both left-aligned (adj = 0) ---------
ok(has("font = 2, adj = 0"),
   "the title is left-margined (adj = 0), not base R's default centred")
ok(has('adj = 0, col = "grey30"'),
   "the subtitle is also left-margined")

## ---- 7. the fill legend sits in the margin, off the bars -----------
ok(!has('legend("topright"') && !has("x = max(dd$landings) * 0.55"),
   "the legend is not placed inside the bars' own area")
ok(has("graphics::legend(x = xmax + dmax * 0.06,") && has("xpd = NA"),
   "the legend is anchored in the right margin (past xmax), drawn with xpd = NA")

cat("\nDONE ", FAIL, "failure(s)", "\n")
