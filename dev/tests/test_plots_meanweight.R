## The mean-weight line plot, and mean weight in the allocation comparison.
FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}
suppressPackageStartupMessages(library(ggplot2))

set.seed(1)
d <- expand.grid(.class = c(20, 22, 24), vesselFlagCountry = c("BE", "DE"),
                 seasonValue = 1:4, stringsAsFactors = FALSE)
d$mean_weight <- 100 + d$.class * 8 + rnorm(nrow(d), 0, 25)
d$sd_weight <- 12

p <- plot_MWaLoA(d, colour = "vesselFlagCountry", facet = "seasonValue")
ok(inherits(p, "ggplot"), "the mean-weight line plot builds")
ok(!inherits(try(ggplot_build(p), silent = TRUE), "try-error"), "and renders")

ok(is.null(plot_MWaLoA(NULL)), "no data gives NULL, not an error")
ok(is.null(plot_MWaLoA(d[0, ])), "no rows gives NULL")

## --- mean weight in the comparison -----------------------------------
cmp <- expand.grid(.cls = c(20, 22), allocation = c("a", "b"),
                   catchCategory = c("Lan", "Dis"), stringsAsFactors = FALSE)
cmp$numbers <- c(100, 200, 50, 400, 10, 20, 5, 40)
cmp$tonnes  <- cmp$numbers * 250 / 1e6
cmp$mean_weight <- cmp$tonnes * 1e6 / cmp$numbers

for (v in c("numbers", "tonnes", "meanweight")) {
  pp <- plot_allocation_comparison(cmp, v, "catchCategory", "Length")
  ok(inherits(pp, "ggplot"), paste("comparison builds for", v))
}

## A mean weight is not extensive: summing it across the grouping would
## give a number with no meaning, so it must be re-weighted by numbers.
cmp2 <- data.frame(.cls = c(20, 20), allocation = "a",
                   catchCategory = c("Lan", "Dis"),
                   numbers = c(1000, 10), tonnes = NA_real_,
                   mean_weight = c(200, 1000), stringsAsFactors = FALSE)
pm <- plot_allocation_comparison(cmp2, "meanweight", NULL, "Length")
v <- ggplot_build(pm)$data[[1]]$y[1]
ok(abs(v - (200 * 1000 + 1000 * 10) / 1010) < 1e-6,
   "mean weight is numbers-weighted across the grouping, not averaged flat")
ok(v < 300, "so a 10-fish stratum does not drag the mean to 600")

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
