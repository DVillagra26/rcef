## The mean-weight line plot.
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

## `shape` opens the averaged strata up: one series - one line - per colour level
## AND per shape level, not one line per colour that zigzags through the shapes
ps <- plot_MWaLoA(d, colour = "vesselFlagCountry", shape = "seasonValue")
line_layer <- which(vapply(ps$layers, function(l) inherits(l$geom, "GeomLine"), logical(1)))
ok(length(unique(ggplot_build(ps)$data[[line_layer]]$group)) == 2 * 4,
   "shape: one line per colour level and per shape level")
ps1 <- plot_MWaLoA(d, shape = "seasonValue")
line_layer <- which(vapply(ps1$layers, function(l) inherits(l$geom, "GeomLine"), logical(1)))
ok(length(unique(ggplot_build(ps1)$data[[line_layer]]$group)) == 4,
   "shape alone: one line per shape level")

ok(is.null(plot_MWaLoA(NULL)), "no data gives NULL, not an error")
ok(is.null(plot_MWaLoA(d[0, ])), "no rows gives NULL")

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
