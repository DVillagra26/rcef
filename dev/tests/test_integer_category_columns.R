## seasonValue is an integer quarter code in the CEF data. derive_fields()
## used to hide that behind a Season = as.character(seasonValue) alias; with
## the alias gone the raw column is used directly, so everything that takes a
## categorical column has to cope with an integer one: a plot has to treat it
## as a category (an integer mapped straight onto a fill or shape scale errors,
## and on an axis it is read as continuous).

source("dev/tests/fixture.R")
FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}
suppressPackageStartupMessages(library(ggplot2))
drawable <- function(p)
  !is.null(p) && inherits(p, "ggplot") &&
  !inherits(try(ggplot_build(p), silent = TRUE), "try-error")
quiet <- function(expr) {
  out <- NULL
  invisible(utils::capture.output(out <- suppressWarnings(suppressMessages(expr))))
  out
}

test_catch <- fx_catch()
test_dist <- fx_dist()
c23 <- subset(test_catch, year == 2023)
ok(is.integer(c23$seasonValue) && !"Season" %in% names(c23) &&
     !"Country" %in% names(c23),
   "fixture: seasonValue is an integer, and there is no Season / Country alias")

## ---- category_cols() -----------------------------------------------------
d <- data.frame(n = c(10L, 2L, 10L, 2L), s = c("b", "a", "b", "a"),
                f = factor(c("x", "y", "x", "y")), stringsAsFactors = FALSE)
dd <- category_cols(d, c("n", "s", "f", "missing", NA, ""))
ok(is.factor(dd$n) && identical(levels(dd$n), c("2", "10")),
   "category_cols(): a numeric column becomes a factor, in numeric order (2 before 10)")
ok(identical(dd$s, d$s) && identical(dd$f, d$f),
   "category_cols(): character and factor columns are left as they are")
ok(identical(category_cols(d, NULL), d) && identical(category_cols(d, "nope"), d),
   "category_cols(): nothing named, or a column the data lacks, is a no-op")

## ---- every plot takes it in every role -------------------------------------
ok(drawable(plot_DR(c23, x = "metier4", colour = "seasonValue")),
   "plot_DR(): colour = seasonValue")
ok(drawable(plot_DR(c23, x = "metier4", shape = "seasonValue")),
   "plot_DR(): shape = seasonValue")
p_x <- plot_DR(c23, x = "seasonValue")
ok(drawable(p_x) && is.factor(p_x$data$seasonValue),
   "plot_DR(): x = seasonValue, drawn as categories (a factor, not a continuous axis)")
ok(drawable(plot_DR(c23, x = "metier4", facet = "seasonValue")),
   "plot_DR(): facet = seasonValue")
h_y <- plot_coverage_heat(c23, y = "seasonValue", x = "metier4")
ok(drawable(h_y) && is.factor(h_y$data$seasonValue),
   "plot_coverage_heat(): y = seasonValue is a discrete axis")
ok(drawable(plot_coverage_bubble(c23, y = "seasonValue", x = "metier4")),
   "plot_coverage_bubble(): y = seasonValue")
ok(drawable(plot_coverage_bar(c23, y = "metier4", fill = "seasonValue")),
   "plot_coverage_bar(): fill = seasonValue")
ok(drawable(quiet(plot_coverage_map(c23, group = "seasonValue"))),
   "plot_coverage_map(): group = seasonValue")
ok(drawable(plot_NaLoA(c23, test_dist, fill = "seasonValue")),
   "plot_NaLoA(): fill = seasonValue")
ok(drawable(plot_MWaLoA(c23, test_dist, colour = "vesselFlagCountry",
                        shape = "seasonValue")),
   "plot_MWaLoA(): shape = seasonValue")
dc <- quiet(build_discard_clusters(c23, c("metier4", "vesselFlagCountry", "seasonValue"), k = 3))
ok(drawable(plot_discard_clusters(dc)) && drawable(plot_discard_cluster_variables(dc)),
   "build_discard_clusters(): seasonValue among the dims")

## ---- after raising ------------------------------------------------------
raised <- quiet(fx_raise(c23, fx_groups("own"), type = "discards",
                         log_file = tempfile(fileext = ".txt")))
ok(drawable(plot_catches(raised, x = "seasonValue")),
   "plot_catches(): x = seasonValue, of a raised catch")
ok(drawable(plot_catches(c23, x = "seasonValue")),
   "plot_catches(): x = seasonValue, of a raw catch")
ok(drawable(plot_DR(raised, x = "metier4", colour = "seasonValue")),
   "plot_DR(): colour = seasonValue, of a raised catch")
ok(drawable(plot_DR(raised, x = "seasonValue")),
   "plot_DR(): x = seasonValue, of a raised catch")

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
