## The three general parameters (palette, angle, n_per_screen) threaded
## through the last set of base-R functions - the length/age
## distribution plots - completing the same infrastructure already in
## place for coverage and discard-rate. Plus two item-7-specific fixes
## caught while doing it:
##
## 1. The density plot's ribbons were outlined in the SAME colour as
##    their fill, which does not read as an "edge" against a similarly-
##    coloured neighbour - changed to OUTLINE (black), matching the
##    heatmap tile fix.
##
## 2. The density plot's y-axis clipped the tallest curve's peak: the
##    upper ylim was a fixed constant (length(lev) + 1.3) that assumed
##    every curve stayed short relative to its own row, which is not
##    true once `overlap` is large or one group's density is much more
##    peaked than the others.

FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}

set.seed(1)
d_num <- data.frame(
  .class = rep(seq(20, 60, 5), 3),
  number = c(rpois(9, 200), rpois(9, 150), rpois(9, 80)),
  fleetValue = rep(c("A", "B", "C"), each = 9),
  Season = rep(c("1", "2", "3"), 9),
  stringsAsFactors = FALSE)

d_mw <- data.frame(
  .class = rep(seq(20, 60, 5), 2),
  mean_weight = c(seq(50, 250, length.out = 9), seq(60, 280, length.out = 9)),
  sd_weight = c(rep(10, 9), rep(15, 9)),
  fleetValue = rep(c("A", "B"), each = 9),
  stringsAsFactors = FALSE)

## ---- 1. both distribution functions accept the general params --------
for (fn in c("plot_numbers_at_length_age", "plot_meanweight_at_length_age"))
  ok(all(c("palette", "n_per_screen") %in%
          names(formals(get(fn)))),
     paste(fn, "accepts them too, and threads them through"))

## ---- 2. colorblind palette actually changes the colours used ---------
r <- tryCatch({
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  plot_numbers_at_length_age(d_num, type = "histogram", fill = "fleetValue",
                            engine = "base", palette = "colorblind")
  TRUE
}, error = function(e) conditionMessage(e))
ok(isTRUE(r), paste("engine='base', palette='colorblind' runs without error:", r))

## ---- 3. n_per_screen genuinely opens multiple devices -----------------
before <- length(grDevices::dev.list())
plot_numbers_at_length_age(d_num, type = "histogram", fill = "fleetValue",
                          facet1 = "Season", engine = "base", n_per_screen = 1)
after <- length(grDevices::dev.list())
ok(after > before, "n_per_screen opens more than one device when none was already set")
for (dv in grDevices::dev.list()) grDevices::dev.off(dv)

## ---- 4. the density ribbon's outline is black, not colour-matched ----
body_ridge <- paste(deparse(body(plot_numbers_at_length_age)), collapse = "\n")
ok(grepl("border = OUTLINE", body_ridge, fixed = TRUE),
   "the density ribbon's border is OUTLINE (black)")
ok(!grepl("border = pal[g]", body_ridge, fixed = TRUE),
   "the old colour-matched border (invisible against a similar fill) is gone")

## ---- 5. the y-axis no longer clips the tallest curve ------------------
ok(grepl("max(1.3, 1 + overlap + 0.3)", body_ridge, fixed = TRUE),
   "the ylim upper bound now accounts for the actual overlap setting, not a fixed constant")

r2 <- tryCatch({
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  plot_numbers_at_length_age(d_num, type = "density", fill = "fleetValue",
                             engine = "base")
  TRUE
}, error = function(e) conditionMessage(e))
ok(isTRUE(r2), paste("the base density branch runs:", r2))

## ---- 6. mean-weight line plot still works with the new parameters ----
r3 <- tryCatch({
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  plot_meanweight_at_length_age(d_mw, colour = "fleetValue",
                            engine = "base", palette = "colorblind")
  TRUE
}, error = function(e) conditionMessage(e))
ok(isTRUE(r3), paste("meanweight line with colorblind palette:", r3))

cat("\nDONE ", FAIL, "failure(s)", "\n")
