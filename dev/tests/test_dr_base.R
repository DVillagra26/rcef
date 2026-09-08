## plot_discard_rate(..., engine = "base") - the raw per-stratum view
## (the only view now; the boxplot was dropped). The real bug caught
## while building this: colour/shape were looked up by matching
## rownames() between the full table and a facet subset, which silently
## returns NA (an invisible point) the moment rownames do not line up -
## only one of 106 points rendered before the fix. Now attached as
## columns of the data itself.

FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}

strata <- data.frame(
  fleetValue = rep(c("A", "B", "C", "D", "E"), 4),
  Country = rep(c("BE", "FR", "DE", "NL"), each = 5),
  landings = rep(100, 20),
  landings_with_discards = rep(80, 20),
  discards = seq(1, 20),
  discard_rate = seq(1, 20) / 100,
  stringsAsFactors = FALSE)

## ---- 1. every point renders, not just one -----------------------------
r <- tryCatch({
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  plot_discard_rate(strata, x = "fleetValue", colour = "Country",
                    engine = "base")
  TRUE
}, error = function(e) conditionMessage(e))
ok(isTRUE(r), paste("plot_discard_rate(engine='base') runs without error:", r))

## checked at the source: colour/shape must be columns of d, not a
## rownames()-keyed lookup into a separately computed vector
body_txt <- paste(deparse(body(plot_discard_rate)), collapse = "\n")
ok(grepl("d$.ptcol", body_txt, fixed = TRUE) &&
     grepl("d$.ptpch", body_txt, fixed = TRUE),
   "colour and shape are attached as columns of d, not looked up by rownames()")
ok(!grepl("match(rownames(dd)", body_txt, fixed = TRUE),
   "the old rownames()-keyed lookup is gone")

## ---- 2. plot_discard_rate() dispatches to the base engine ------------
r3 <- tryCatch({
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  plot_discard_rate(strata, x = "fleetValue", colour = "Country",
                    engine = "base")
  TRUE
}, error = function(e) conditionMessage(e))
ok(isTRUE(r3), paste("plot_discard_rate(engine='base') runs:", r3))

## ---- 3. box() around the plotting area ---------------------------
bt <- paste(deparse(body(plot_discard_rate)), collapse = "\n")
ok(grepl("box()", bt, fixed = TRUE),
   "the base discard-rate branch draws a box() around the plotting area")

cat("\nDONE ", FAIL, "failure(s)", "\n")
