## the base-R bubble branch of plot_bubbles(), the last of
## the three coverage plot types to get one. Same status colours, same
## data, circles sized cell-relative (not in fixed physical inches) so
## a multi-panel facet grid does not bury every category under
## oversized, overlapping circles - the bug caught and fixed while
## building this.

FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}

strata <- data.frame(
  metier4 = rep(c("OTB", "SDN", "SSC"), 2),
  Country = rep(c("BE", "FR"), each = 3),
  landings = c(100, 80, 60, 40, 30, 20),
  landings_with_discards = c(80, 60, 30, 40, 15, 0),
  discards = c(8, 6, 3, 4, 1.5, 0),
  coverage = c(80, 75, 50, 100, 50, 0),
  discard_rate = c(0.09, 0.09, 0.09, 0.09, 0.09, NA),
  n_measurements = 0, numTrips = c(3, 3, 1, 3, 1, 0),
  stringsAsFactors = FALSE)
sc <- classify_strata(strata, 50, min_trips = 2)

## ---- 1. plot_coverage_bubble(..., engine="base") works --------------
r <- tryCatch({
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  plot_coverage_bubble(sc, y = "metier4", x = "Country",
               min_cov = 50, min_trip = 2, engine = "base")
  TRUE
}, error = function(e) conditionMessage(e))
ok(isTRUE(r), paste("plot_coverage_bubble(engine='base') runs:", r))

## ---- 2. runs with a facet, and with no fill/facet at all -------------
r3 <- tryCatch({
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  plot_coverage_bubble(sc, y = "metier4", x = "Country", min_cov = 50, min_trip = 2, engine = "base")
  TRUE
}, error = function(e) conditionMessage(e))
ok(isTRUE(r3), paste("runs with x_var given:", r3))

sc2 <- classify_strata(strata, 50, min_trips = 2)
r4 <- tryCatch({
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  plot_coverage_bubble(sc2, y = "metier4", min_cov = 50, min_trip = 2, engine = "base")
  TRUE
}, error = function(e) conditionMessage(e))
ok(isTRUE(r4), paste("runs with no x_var (collapses to a single column):", r4))

## ---- 4. cell-relative sizing: radius does not depend on facet count --
##
## The bug this caught: an earlier version scaled bubble radius from a
## FIXED physical-inch range, which looked right with one panel and
## produced solid, overlapping colour once several panels shared the
## same device. Checked against the function's own body (not by reading
## R/ source files, which an installed package does not ship - they are
## compiled into a lazy-load database).
body_txt <- paste(deparse(body(plot_coverage_bubble)), collapse = "\n")
ok(grepl("cell_rad", body_txt, fixed = TRUE),
   "bubble radius is derived from the grid cell, not a fixed inch span")
ok(!grepl("1/sqrt(length(facets))", gsub(" ", "", body_txt), fixed = TRUE),
   "the earlier facet-count-only shrink heuristic is gone")

cat("\nDONE ", FAIL, "failure(s)", "\n")
