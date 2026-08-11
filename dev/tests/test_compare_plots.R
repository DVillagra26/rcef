## The comparison of saved length/age allocations.
##
## Two changes are locked in here:
##   1. all three values are drawable from the same data, because the tab
##      now shows numbers, stock weight and mean weight together rather
##      than asking which one to look at;
##   2. faceting takes two variables and free scales, and a facet naming
##      a column the comparison does not carry is IGNORED rather than
##      passed to ggplot, where it errored only at draw time and showed
##      up as an empty panel with no message.
##
## ggplot_build() is called on every plot: constructing a ggplot object
## validates almost nothing, so a test that only checks inherits(p,
## "ggplot") passes on a plot that cannot be drawn.

FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}
suppressPackageStartupMessages(library(ggplot2))

set.seed(42)
mk <- function(nm, shift) data.frame(
  .cls = rep(10:30, 4), .src = "allocated",
  catchCategory = rep(c("Lan", "Dis"), each = 42),
  Country = rep(c("BE", "NL"), each = 21, length.out = 84),
  numbers = runif(84, 100, 1000) + shift,
  tonnes  = runif(84, 1, 9),
  allocation = nm, stringsAsFactors = FALSE)
cmp <- rbind(mk("alloc_1", 0), mk("alloc_2", 300))
cmp$mean_weight <- cmp$tonnes * 1e6 / cmp$numbers

drawable <- function(p)
  !is.null(p) && inherits(p, "ggplot") &&
  !inherits(try(ggplot_build(p), silent = TRUE), "try-error")

## ---- 1. all three values draw ---------------------------------------
for (v in c("numbers", "tonnes", "meanweight"))
  ok(drawable(plot_allocation_comparison(cmp, v, facet_var = "catchCategory",
                                         what = "Length")),
     paste("the", v, "view draws"))

## ---- 2. two facets, and they reach the layout ------------------------
p2 <- plot_allocation_comparison(cmp, "numbers", facet_var = "catchCategory",
                                 facet2 = "Country", what = "Length")
ok(drawable(p2), "two facets draw")
ok(inherits(p2$facet, "FacetGrid"),
   "two facets become a grid, not a wrapped interaction")
p1 <- plot_allocation_comparison(cmp, "numbers", facet_var = "catchCategory",
                                 what = "Length")
ok(inherits(p1$facet, "FacetWrap"), "one facet still wraps")

## the grid really has both variables on it
lay <- ggplot_build(p2)$layout$layout
ok(all(c("catchCategory", "Country") %in% names(lay)),
   "both facet variables appear in the panel layout")
ok(nrow(lay) == 4, "2 categories x 2 countries = 4 panels")

## ---- 3. a facet the data does not carry is ignored, not fatal --------
ok(drawable(plot_allocation_comparison(cmp, "numbers",
                                       facet_var = "NoSuchColumn",
                                       what = "Length")),
   "an unknown facet is dropped rather than erroring at draw time")
ok(drawable(plot_allocation_comparison(cmp, "meanweight",
                                       facet_var = "catchCategory",
                                       facet2 = "AlsoMissing",
                                       what = "Length")),
   "an unknown second facet is dropped too")
ok(inherits(plot_allocation_comparison(cmp, "numbers",
                                       facet_var = "catchCategory",
                                       facet2 = "catchCategory",
                                       what = "Length")$facet, "FacetWrap"),
   "the same variable twice collapses to one facet, not a 1-wide grid")

## ---- 4. free scales are honoured -------------------------------------
pf <- plot_allocation_comparison(cmp, "numbers", facet_var = "catchCategory",
                                 what = "Length", free_x = TRUE, free_y = TRUE)
ok(drawable(pf), "free x and y draw")
ok(isTRUE(pf$facet$params$free$x) && isTRUE(pf$facet$params$free$y),
   "free_x / free_y reach the facet parameters")
pfix <- plot_allocation_comparison(cmp, "numbers", facet_var = "catchCategory",
                                   what = "Length", free_x = FALSE,
                                   free_y = FALSE)
ok(!isTRUE(pfix$facet$params$free$y), "fixed scales stay fixed")

## ---- 5. mean weight is numbers-weighted, not averaged ----------------
##
## Collapsing a facet must re-weight by the numbers behind each class.
## A plain mean would let a class measured on 3 fish move the curve as
## far as one measured on 3000.
d <- plot_allocation_comparison(cmp, "meanweight", what = "Length")$data
one <- cmp[cmp$.cls == 10 & cmp$allocation == "alloc_1", ]
expect <- sum(one$mean_weight * one$numbers) / sum(one$numbers)
got <- d$v[d$.cls == 10 & d$allocation == "alloc_1"]
ok(length(got) == 1 && abs(got - expect) < 1e-6,
   "mean weight across a collapsed facet is numbers-weighted")
ok(abs(got - mean(one$mean_weight)) > 1e-9,
   "and is NOT the unweighted mean")

## ---- 6. empty input is handled ---------------------------------------
ok(is.null(plot_allocation_comparison(cmp[0, ], "numbers", what = "Length")),
   "no rows gives NULL rather than an error")
ok(is.null(plot_allocation_comparison(NULL, "numbers", what = "Length")),
   "NULL input gives NULL")

cat("\nDONE ", FAIL, "failure(s)", "\n")
