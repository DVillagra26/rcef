## Length bins (plot_NaLoA()'s and plot_MWaLoA()'s bin_width) and plot_NaLoA()'s
## plus_group - plus the title wording both plots share ("<CatchCategory> -
## <Metric> at <Length|Age>") and the smaller title font.
##
## Length bins pool like a plus group (numbers add, mean weights are number-weighted,
## pooled per distribution first) but apply to every class along the axis, not just
## a tail - these checks mirror test_meanweight_plus_samples.R's own plus-group
## section closely, so a reader already familiar with those can see the two are
## siblings. Everything is read from the data the plots draw (`p$data`: numbers
## in thousands, mean weights in g), on a hand-made catch and distributions.

source("dev/tests/fixture.R")
FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}
near <- function(a, b, tol = 1e-9) isTRUE(all.equal(a, b, tolerance = tol,
                                                     check.attributes = FALSE))
suppressPackageStartupMessages({library(dplyr); library(ggplot2)})
err  <- function(expr) tryCatch({ force(expr); "no error" },
                                error = function(e) conditionMessage(e))
warn <- function(expr) tryCatch({ force(expr); "no warning" },
                                warning = function(w) conditionMessage(w))
drawable <- function(p) inherits(p, "ggplot") &&
  !inherits(try(ggplot_build(p), silent = TRUE), "try-error")

catch_row <- function(country, domain, season = "1", cc = "Lan")
  data.frame(vesselFlagCountry = country, fleetValue = "TBB_DEF_70-99_0_0",
             areaValue = "27.4.a", seasonValue = season, catchCategory = cc,
             domainBiology = domain, total = 1000, variableUnit = "kg",
             stringsAsFactors = FALSE)
dist_rows <- function(country, domain, classes, numbers, weights, nmeas,
                      type = "LengthTotal", cc = "Lan") {
  base <- data.frame(vesselFlagCountry = country, year = 2022, catchCategory = cc,
                     domainBiology = domain, distributionType = type,
                     distributionUnit = if (type == "Age") "y" else "cm",
                     distributionClass = classes, numMeasurements = nmeas,
                     stringsAsFactors = FALSE)
  rbind(cbind(base, variableType = "Number", variableUnit = "N",
              valueType = "Total", value = numbers),
        cbind(base, variableType = "WeightLive", variableUnit = "g",
              valueType = "Mean", value = weights))
}

## ---- 1. binning, on a hand-made fixture ----------------------------------------
## classes 10..19 (cm), bin_width = 5 -> bins at 10 and 15
d10 <- dist_rows("A", "d1", 10:19, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                 c(100, 110, 120, 130, 140, 150, 160, 170, 180, 190), 55)
c1 <- catch_row("A", "d1")
nb5 <- plot_NaLoA(c1, d10, catchCategory = "Lan", bin_width = 5)$data
n0  <- plot_NaLoA(c1, d10, catchCategory = "Lan")$data
ok(identical(sort(unique(nb5$.class)), c(10, 15)),
   "bin_width = 5 on classes 10-19 gives two bins, at 10 and 15")
ok(near(nb5$number[nb5$.class == 10] * 1e3, sum(1:5)) &&
     near(nb5$number[nb5$.class == 15] * 1e3, sum(6:10)),
   "numbers add up within each bin (1+2+3+4+5 = 15, 6+7+8+9+10 = 40)")
mb5 <- plot_MWaLoA(c1, d10, catchCategory = "Lan", bin_width = 5)$data
w10 <- sum((1:5) * c(100, 110, 120, 130, 140)) / sum(1:5)
w15 <- sum((6:10) * c(150, 160, 170, 180, 190)) / sum(6:10)
ok(near(mb5$mean_weight[mb5$.class == 10], w10) &&
     near(mb5$mean_weight[mb5$.class == 15], w15),
   "each bin's mean weight is number-weighted over its own original classes")
ok(near(mb5$n_fish, c(sum(1:5), sum(6:10))),
   "and the bin holds the fish of its classes (55 shared out over the numbers)")
ok(identical(mb5$bin_width, c(5, 5)) && identical(nb5$bin_width, c(5, 5)) &&
     !"bin_width" %in% names(plot_MWaLoA(c1, d10, catchCategory = "Lan")$data) &&
     !"bin_width" %in% names(n0),
   "the summary carries the bin width used as a `bin_width` column, none when unbinned")
ok(near(sum(nb5$number), sum(n0$number)),
   "binning conserves the numbers")
ok(identical(sort(unique(n0$.class)), as.numeric(10:19)),
   "unbinned, every class is there")

## ---- 2. plot_MWaLoA(bin_width = ...) --------------------------------------------
test_catch <- fx_catch()
test_dist <- fx_dist()
cd <- test_catch; dd <- test_dist
cap  <- function(p) gsub("\n", " ", p$labels$caption, fixed = TRUE)
says_cap <- function(p, s) grepl(s, cap(p), fixed = TRUE)
xlab <- function(p) ggplot_build(p)$layout$panel_params[[1]]$x$get_labels()

p0  <- plot_MWaLoA(cd, dd, catchCategory = "Lan", colour = "vesselFlagCountry")
pbw <- plot_MWaLoA(cd, dd, catchCategory = "Lan", colour = "vesselFlagCountry", bin_width = 2)
ok(drawable(pbw) && length(unique(pbw$data$.class)) < length(unique(p0$data$.class)),
   "bin_width = 2 draws, with fewer distinct classes than the unbinned plot")
ok(all(diff(sort(unique(pbw$data$.class))) == 2),
   "the binned classes are exactly 2cm apart")
labs2 <- xlab(pbw)
ok(all(grepl("^[0-9]+-[0-9]+$", labs2)),
   "every break reads as a lo-hi range, not a bare number")
ok(says_cap(pbw, "length classes pooled into 2cm bins") && !says_cap(p0, "pooled into"),
   "the caption says the bin width, only when binned")

## a bin width on ages is an error: it is for lengths
ok(grepl("only applies to lengths", err(plot_MWaLoA(
     cd, dd, distributionType = "Age", colour = "vesselFlagCountry", bin_width = 1))),
   "bin_width on ages is an error: it is for lengths")
ok(grepl("bin_width", err(plot_MWaLoA(cd, dd, bin_width = 0))),
   "bin_width = 0 is not a single positive number")
ok(grepl("bin_width", err(plot_MWaLoA(cd, dd, bin_width = "5"))),
   "... and a width that is not numeric is one too")
tab_len <- p0$data                       # an already-summarised table
ok(grepl("only applies when `distributions`", warn(plot_MWaLoA(
     tab_len, colour = "vesselFlagCountry", bin_width = 2))),
   "bin_width on an already-summarised table warns: it cannot be pooled again")

## ---- 3. plot_NaLoA(): plus_group and bin_width -----------------------------------
n0  <- plot_NaLoA(cd, dd, distributionType = "Age", fill = "vesselFlagCountry")
np3 <- plot_NaLoA(cd, dd, distributionType = "Age", fill = "vesselFlagCountry",
                  plus_group = 3)
ok(drawable(np3) && max(np3$data$.class) == 3 && max(n0$data$.class) > 3,
   "plot_NaLoA(): plus_group = 3 stops the age axis at the plus class")
ok("3+" %in% xlab(np3) && !any(grepl("+", xlab(n0), fixed = TRUE)),
   "plot_NaLoA(): the pooled class is labelled 3+; no plus without the option")
ok(says_cap(np3, "3+ = ages 3+ pooled") && !says_cap(n0, "pooled"),
   "plot_NaLoA(): the caption says which ages are pooled")
ok(grepl("only applies to ages", err(plot_NaLoA(cd, dd, fill = "vesselFlagCountry", plus_group = 3))),
   "plot_NaLoA(): a plus group on lengths is an error")

nbw <- plot_NaLoA(cd, dd, fill = "vesselFlagCountry", bin_width = 2)
ok(drawable(nbw) && all(diff(sort(unique(nbw$data$.class))) == 2),
   "plot_NaLoA(): bin_width = 2 pools the length classes into 2cm bins")
ok(all(grepl("^[0-9]+-[0-9]+$", xlab(nbw))),
   "plot_NaLoA(): every break reads as a lo-hi range")
ok(says_cap(nbw, "length classes pooled into 2cm bins"),
   "plot_NaLoA(): the caption says the bin width")
ok(grepl("only applies to lengths", err(plot_NaLoA(
     cd, dd, distributionType = "Age", fill = "vesselFlagCountry", bin_width = 1))),
   "plot_NaLoA(): a bin width on ages is an error")
ok(near(sum(plot_NaLoA(cd, dd, fill = "vesselFlagCountry")$data$number),
        sum(nbw$data$number), tol = 1e-6),
   "plot_NaLoA(): binning conserves the total numbers")

## ---- 4. title wording: "<CatchCategory> - <Metric> at <Length|Age>, <the data>" --
## the last words say what the data is: distributions as they were sampled, with no
## estimate in them, are "only sampled" (an allocation with estimates says "sampled + estimated")
ok(identical(plot_MWaLoA(cd, dd, catchCategory = "Lan",
                         colour = "vesselFlagCountry")$labels$title,
             "Landings - Mean Weight at Length, only sampled") &&
     identical(plot_MWaLoA(cd, dd, catchCategory = "Dis", distributionType = "Age",
                           colour = "vesselFlagCountry")$labels$title,
               "Discards - Mean Weight at Age, only sampled"),
   "plot_MWaLoA(): title is '<CatchCategory> - Mean Weight at <Length|Age>, only sampled'")
n_lan <- plot_NaLoA(cd, dd, catchCategory = "Lan", fill = "vesselFlagCountry")
ok(identical(n_lan$labels$title, "Landings - Numbers at Length, only sampled") &&
     identical(plot_NaLoA(cd, dd, catchCategory = "Dis", distributionType = "Age",
                          fill = "vesselFlagCountry")$labels$title,
               "Discards - Numbers at Age, only sampled"),
   "plot_NaLoA(): title is '<CatchCategory> - Numbers at <Length|Age>, only sampled'")
ok(identical(plot_MWaLoA(tab_len, colour = "vesselFlagCountry")$labels$title,
             "Mean Weight at Length") &&
     identical(plot_NaLoA(n_lan$data, fill = "vesselFlagCountry")$labels$title,
               "Numbers at Length"),
   "an already-summarised table (no distributions) drops the leading category")
ok(identical(p0$theme$plot.title$size, 11) && identical(n0$theme$plot.title$size, 11),
   "the title font is smaller than app_theme()'s default")

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
