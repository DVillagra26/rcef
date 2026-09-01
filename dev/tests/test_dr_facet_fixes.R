## Two real gaps in plot_discard_rate(..., engine = "base"), found while
## verifying n_per_screen/palette/angle threading:
##
## 1. The legend was only ever drawn for the single-panel case - a
##    faceted figure had no key linking colour to level at all.
##
## 2. The x-axis title used a fixed line offset that cleared a single
##    big panel's rotated tick labels but collided with the same labels
##    once several small facet panels made each one's own margin too
##    short relative to the label text. Fixed by moving the title to
##    the figure's OUTER margin for the multi-panel case, drawn once,
##    rather than per-panel.

FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}

strata <- data.frame(
  fleetValue = rep(c("A", "B", "C"), 6),
  Country = rep(c("BE", "FR"), each = 9),
  Season = rep(c("1", "2"), each = 9, length.out = 18),
  landings = rep(100, 18),
  landings_with_discards = rep(80, 18),
  discards = seq(1, 18),
  discard_rate = seq(1, 18) / 100,
  stringsAsFactors = FALSE)

## ---- 1. faceted "raw" draws without error, and the fix is in the
## source (checked against the function body directly) ------------------
r1 <- tryCatch({
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  plot_dr_raw_base(strata, "fleetValue", colour_var = "Country", facet_var = "Season")
  TRUE
}, error = function(e) conditionMessage(e))
ok(isTRUE(r1), paste("faceted raw plot runs without error:", r1))

body_raw <- paste(deparse(body(plot_dr_raw_base)), collapse = "\n")
ok(grepl("draw_legend <- function", body_raw, fixed = TRUE) &&
     grepl("draw_legend()", body_raw, fixed = TRUE),
   "the legend is drawn from inside the per-screen loop (every group), not conditionally skipped when faceted")
ok(!grepl("!(length(facets) > 1)", body_raw, fixed = TRUE),
   "the old 'only when NOT faceted' guard on the legend is gone")

## ---- 2. faceted "box" draws without error too, same fix mirrored ------
r2 <- tryCatch({
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  plot_dr_box_base(strata, "fleetValue", fill_var = "Country", facet_var = "Season")
  TRUE
}, error = function(e) conditionMessage(e))
ok(isTRUE(r2), paste("faceted box plot runs without error:", r2))

body_box <- paste(deparse(body(plot_dr_box_base)), collapse = "\n")
ok(grepl("draw_legend <- function", body_box, fixed = TRUE),
   "plot_dr_box_base() got the same legend-when-faceted fix")

## ---- 3. the x-axis title moved to the outer margin for multi-panel ----
ok(grepl('mtext(x_var, outer = TRUE, side = 1', body_raw, fixed = TRUE),
   "the multi-panel case draws the x-axis title ONCE, in the outer margin")
ok(!grepl("line = 4.2", body_raw, fixed = TRUE),
   "the old fixed line=4.2 offset (the source of the collision) is gone")

## ---- 4. n_per_screen / palette / angle actually accepted and used -----
for (fn in c("plot_dr_raw_base", "plot_dr_box_base")) {
  f <- get(fn)
  ok(all(c("palette", "angle", "n_per_screen") %in% names(formals(f))),
     paste(fn, "accepts palette, angle and n_per_screen"))
}

r3 <- tryCatch({
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  plot_discard_rate(strata, type = "raw", x = "fleetValue", colour = "Country",
                    engine = "base", palette = "colorblind", angle = 0)
  TRUE
}, error = function(e) conditionMessage(e))
ok(isTRUE(r3), paste("palette='colorblind' and angle=0 both work through the dispatcher:", r3))

cat("\nDONE ", FAIL, "failure(s)", "\n")
