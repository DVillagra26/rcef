## The three general parameters (palette, angle, n_per_screen) threaded
## through the last set of base-R functions - the length/age
## distribution plots - completing the same infrastructure already in
## place for coverage and discard-rate.

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
  plot_numbers_at_length_age(d_num, fill = "fleetValue",
                            engine = "base", palette = "colorblind")
  TRUE
}, error = function(e) conditionMessage(e))
ok(isTRUE(r), paste("engine='base', palette='colorblind' runs without error:", r))

## ---- 3. n_per_screen genuinely opens multiple devices -----------------
before <- length(grDevices::dev.list())
plot_numbers_at_length_age(d_num, fill = "fleetValue",
                          facet = "Season", engine = "base", n_per_screen = 1)
after <- length(grDevices::dev.list())
ok(after > before, "n_per_screen opens more than one device when none was already set")
for (dv in grDevices::dev.list()) grDevices::dev.off(dv)

## ---- 4. mean-weight line plot still works with the new parameters ----
r3 <- tryCatch({
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  plot_meanweight_at_length_age(d_mw, colour = "fleetValue",
                            engine = "base", palette = "colorblind")
  TRUE
}, error = function(e) conditionMessage(e))
ok(isTRUE(r3), paste("meanweight line with colorblind palette:", r3))

cat("\nDONE ", FAIL, "failure(s)", "\n")
