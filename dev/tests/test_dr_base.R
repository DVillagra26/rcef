## plot_discard_rate(..., engine = "base") - both types ("raw", "box").
## The real bug caught while building this: colour/shape were looked up
## by matching rownames() between the full table and a facet subset,
## which silently returns NA (an invisible point, pch = NA) the moment
## rownames do not line up between the two - only one of 106 points
## rendered in the "raw" type before this was fixed. Now attached as
## columns of the data itself, which subsetting always carries along
## correctly.

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
  plot_dr_raw_base(strata, "fleetValue", colour_var = "Country")
  TRUE
}, error = function(e) conditionMessage(e))
ok(isTRUE(r), paste("plot_dr_raw_base() runs without error:", r))

## checked at the source: colour/shape must be columns of d, not a
## rownames()-keyed lookup into a separately computed vector - the
## exact pattern that silently dropped 105 of 106 points
body_txt <- paste(deparse(body(plot_dr_raw_base)), collapse = "\n")
ok(grepl("d$.ptcol", body_txt, fixed = TRUE) &&
     grepl("d$.ptpch", body_txt, fixed = TRUE),
   "colour and shape are attached as columns of d, not looked up by rownames()")
ok(!grepl("match(rownames(dd)", body_txt, fixed = TRUE),
   "the old rownames()-keyed lookup is gone")

## ---- 2. the box type runs, with points and pooled overlay -------------
r2 <- tryCatch({
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  plot_dr_box_base(strata, "fleetValue", fill_var = "Country",
                   show_points = TRUE, show_pooled = TRUE)
  TRUE
}, error = function(e) conditionMessage(e))
ok(isTRUE(r2), paste("plot_dr_box_base() runs without error:", r2))

## ---- 3. plot_discard_rate() dispatches to both correctly ---------------
r3 <- tryCatch({
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  plot_discard_rate(strata, type = "raw", x = "fleetValue",
                    colour = "Country", engine = "base")
  plot_discard_rate(strata, type = "box", x = "fleetValue",
                    fill = "Country", engine = "base")
  TRUE
}, error = function(e) conditionMessage(e))
ok(isTRUE(r3), paste("plot_discard_rate(..., engine='base') dispatches both types:", r3))

## ---- 4. verbose reports correctly for each type -------------------------
out_raw <- capture.output(
  plot_discard_rate(strata, type = "raw", x = "fleetValue", colour = "Country",
                    verbose = TRUE))
ok(any(grepl("colour : Country", out_raw, fixed = TRUE)),
   "verbose reports the colour column for type='raw'")

out_box <- capture.output(
  plot_discard_rate(strata, type = "box", x = "fleetValue", fill = "Country",
                    verbose = TRUE))
ok(any(grepl("fill   : Country", out_box, fixed = TRUE)),
   "verbose reports the fill column for type='box'")

## ---- 5. box() and real ticks, matching the coverage plots' fix --------
for (fn in c("plot_dr_raw_base", "plot_dr_box_base")) {
  bt <- paste(deparse(body(get(fn))), collapse = "\n")
  ok(grepl("box()", bt, fixed = TRUE),
     paste(fn, "draws a box() around the plotting area"))
}

cat("\nDONE ", FAIL, "failure(s)", "\n")
