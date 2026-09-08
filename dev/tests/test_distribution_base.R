## plot_numbers_at_length_age() / plot_meanweight_at_length_age(),
## engine = "base" - the histogram and the mean-weight line. Completes
## the base-R port of every plotting function in the package.

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
  stringsAsFactors = FALSE)

d_mw <- data.frame(
  .class = rep(seq(20, 60, 5), 2),
  mean_weight = c(seq(50, 250, length.out = 9), seq(60, 280, length.out = 9)),
  sd_weight = c(rep(10, 9), rep(15, 9)),
  fleetValue = rep(c("A", "B"), each = 9),
  stringsAsFactors = FALSE)

## ---- 1. histogram and mean-weight line run without error -------------
r1 <- tryCatch({
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  plot_numbers_at_length_age(d_num, fill = "fleetValue", engine = "base")
  TRUE
}, error = function(e) conditionMessage(e))
ok(isTRUE(r1), paste("histogram runs:", r1))

r3 <- tryCatch({
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  plot_meanweight_at_length_age(d_mw, colour = "fleetValue",
                            engine = "base")
  TRUE
}, error = function(e) conditionMessage(e))
ok(isTRUE(r3), paste("meanweight line runs:", r3))

## ---- 2. facet_key_base() combines up to two facets correctly ---------
dd <- data.frame(f1 = c("x", "x", "y"), f2 = c("a", "b", "a"))
k <- facet_key_base(dd, "f1", "f2")
ok(identical(k, c("x | a", "x | b", "y | a")),
   "two facet columns combine into one composite key per row")
ok(identical(facet_key_base(dd, "f1"), c("x", "x", "y")),
   "one facet column alone is used unmodified")
ok(all(is.na(facet_key_base(dd))),
   "no facet columns given returns NA - no faceting")

## ---- 3. faceted calls run too -----------------------------------------
d_num2 <- d_num
d_num2$Season <- rep(c("1", "2", "3"), length.out = nrow(d_num2))
r4 <- tryCatch({
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  plot_numbers_at_length_age(d_num2, fill = "fleetValue",
                            facet = "Season", engine = "base")
  TRUE
}, error = function(e) conditionMessage(e))
ok(isTRUE(r4), paste("faceted histogram runs:", r4))

## ---- 4. no fill/colour still works (single series) --------------------
r5 <- tryCatch({
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  plot_numbers_at_length_age(d_num, engine = "base")
  plot_meanweight_at_length_age(d_mw, engine = "base")
  TRUE
}, error = function(e) conditionMessage(e))
ok(isTRUE(r5), paste("no grouping column given still runs:", r5))

## ---- 5. every base branch draws a box() ------------------------------
for (fn in c("plot_numbers_at_length_age", "plot_meanweight_at_length_age")) {
  bt <- paste(deparse(body(get(fn))), collapse = "\n")
  ok(grepl("box()", bt, fixed = TRUE),
     paste(fn, "base branch draws a box() around the plotting area"))
}

## ---- 6. mean weight always draws its points; the sd band only when
##        strata are averaged (no shape) ---------------------------------
bt_mw <- paste(deparse(body(plot_meanweight_at_length_age)), collapse = "\n")
ok(grepl("band <- !has(shape_var)", bt_mw, fixed = TRUE),
   "the +/- 1 sd band is gated on there being no shape column, nothing else")
ok(!grepl("show_points", bt_mw, fixed = TRUE) &&
   !grepl("show_spread", bt_mw, fixed = TRUE),
   "the show_points / show_spread toggles are gone")

cat("\nDONE ", FAIL, "failure(s)", "\n")
