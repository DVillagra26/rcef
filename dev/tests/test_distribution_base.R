## plot_numbers_at_length_age() / plot_meanweight_at_length_age(),
## engine = "base" - histogram, density and the mean-weight line:
## histogram, density (ridges), and meanweight line. Completes the
## base-R port of every plotting function in the package.

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

## ---- 1. all three types run without error -----------------------------
r1 <- tryCatch({
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  plot_numbers_at_length_age(d_num, type = "histogram", fill = "fleetValue",
                            engine = "base")
  TRUE
}, error = function(e) conditionMessage(e))
ok(isTRUE(r1), paste("histogram runs:", r1))

r2 <- tryCatch({
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  plot_numbers_at_length_age(d_num, type = "density", fill = "fleetValue",
                            smooth = 1, engine = "base")
  TRUE
}, error = function(e) conditionMessage(e))
ok(isTRUE(r2), paste("density runs:", r2))

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
  plot_numbers_at_length_age(d_num2, type = "histogram", fill = "fleetValue",
                            facet1 = "Season", engine = "base")
  TRUE
}, error = function(e) conditionMessage(e))
ok(isTRUE(r4), paste("faceted histogram runs:", r4))

## ---- 4. no fill/colour still works (single series) --------------------
r5 <- tryCatch({
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  plot_numbers_at_length_age(d_num, type = "histogram", engine = "base")
  plot_meanweight_at_length_age(d_mw, engine = "base")
  TRUE
}, error = function(e) conditionMessage(e))
ok(isTRUE(r5), paste("no grouping column given still runs:", r5))

## ---- 5. every new function draws a box() -------------------------------
for (fn in c("plot_length_hist_base", "plot_length_ridges_base",
            "plot_length_meanweight_base")) {
  bt <- paste(deparse(body(get(fn))), collapse = "\n")
  ok(grepl("box()", bt, fixed = TRUE),
     paste(fn, "draws a box() around the plotting area"))
}

## ---- 6. verbose reports correctly for each quantity/type ---------------
out_h <- capture.output(
  plot_numbers_at_length_age(d_num, type = "histogram", fill = "fleetValue",
                            verbose = TRUE))
ok(any(grepl("fill  : fleetValue", out_h, fixed = TRUE)),
   "verbose reports the fill column for numbers/histogram")

out_mw <- capture.output(
  plot_meanweight_at_length_age(d_mw, colour = "fleetValue",
                            verbose = TRUE))
ok(any(grepl("colour: fleetValue", out_mw, fixed = TRUE)),
   "verbose reports the colour column for meanweight/line")

cat("\nDONE ", FAIL, "failure(s)", "\n")
