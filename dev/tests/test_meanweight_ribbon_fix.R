## The mean-weight SD ribbon drew spurious diagonal wedges across a
## fleet's whole length range whenever sd_weight was NA for some
## classes but not others (a single stratum, no spread to compute).
## graphics::polygon() does not skip NA vertices the way lines() does
## - it drew a straight connecting segment from the last valid point
## before a gap to the first valid point after it, which is exactly a
## wide diagonal wedge across the gap. Fixed by drawing one polygon per
## CONTIGUOUS run of non-NA points, so a gap breaks the ribbon instead
## of bridging it.

FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}

## a class range with an NA gap IN THE MIDDLE of otherwise-valid sd_weight
d <- data.frame(
  .class = c(20, 25, 30, 35, 40, 45, 50),
  mean_weight = c(100, 150, 200, 260, 330, 410, 500),
  sd_weight = c(10, 12, NA, NA, 20, 22, 25),
  fleetValue = "A",
  stringsAsFactors = FALSE)

r <- tryCatch({
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  plot_length_meanweight_base(d, colour_var = "fleetValue")
  TRUE
}, error = function(e) conditionMessage(e))
ok(isTRUE(r), paste("runs without error on a ribbon with a gap in the middle:", r))

## checked at the source: the fix draws per-run polygons, not one
## polygon whose vertex list still includes NA coordinates
body_txt <- paste(deparse(body(plot_length_meanweight_base)), collapse = "\n")
ok(grepl("run <- cumsum", body_txt, fixed = TRUE),
   "the fix splits into contiguous runs before building each ribbon polygon")
ok(grepl("nrow(seg)", body_txt, fixed = TRUE) && grepl("< 2", body_txt, fixed = TRUE),
   "a run of fewer than 2 points (nothing to draw a band between) is skipped")

## a group with sd_weight entirely NA still draws its line, just no ribbon
d2 <- d; d2$sd_weight <- NA_real_
r2 <- tryCatch({
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  plot_length_meanweight_base(d2, colour_var = "fleetValue")
  TRUE
}, error = function(e) conditionMessage(e))
ok(isTRUE(r2), paste("an entirely-NA sd_weight column still runs (line only, no ribbon):", r2))

cat("\nDONE ", FAIL, "failure(s)", "\n")
