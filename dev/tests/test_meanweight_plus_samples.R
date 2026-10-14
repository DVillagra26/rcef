## Mean weight at age: the plus group, the fish behind each point, and the
## country-aware link between catch and distributions they rest on.
##
## 1. A distribution belongs to the country that submitted it. A domainBiology
##    carries no country, so two countries can use the same one - each with its
##    own distributions - and the link must not hand each the other's.
## 2. n_fish: numMeasurements is one count per distribution, shared out over
##    its classes in proportion to the numbers; a distribution counts once
##    however many stratum keys it is linked to.
## 3. The plus group pools the ages from `plus_group` upward inside each
##    distribution (numbers add, mean weights are number-weighted), labelled
##    "7+", and the plot says so.
##
## All of it is read from what the plots draw (`p$data`: numbers in thousands,
## mean weights in g, fish counts) and from the coverage table, on a tiny
## hand-made catch and distributions.

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

## ---- tiny hand-made catch and distributions -----------------------------
catch_row <- function(country, domain, season = "1", cc = "Lan")
  data.frame(vesselFlagCountry = country, fleetValue = "TBB_DEF_70-99_0_0",
             areaValue = "27.4.a", seasonValue = season, catchCategory = cc,
             domainBiology = domain, total = 1000, variableUnit = "kg",
             stringsAsFactors = FALSE)
## a distribution: numbers and mean weights at the given classes
dist_rows <- function(country, domain, classes, numbers, weights, nmeas,
                      type = "Age", cc = "Lan") {
  base <- data.frame(vesselFlagCountry = country, year = 2022, catchCategory = cc,
                     domainBiology = domain, distributionType = type,
                     distributionUnit = if (type == "Age") "y" else "mm",
                     distributionClass = classes, numMeasurements = nmeas,
                     stringsAsFactors = FALSE)
  rbind(cbind(base, variableType = "Number", variableUnit = "N",
              valueType = "Total", value = numbers),
        cbind(base, variableType = "WeightLive", variableUnit = "g",
              valueType = "Mean", value = weights))
}
## what the plots draw, for ages of the landings
numbers <- function(catch, dist, ...) plot_NaLoA(catch, dist, distributionType = "Age",
                                                 catchCategory = "Lan", ...)$data
mweight <- function(catch, dist, ...) plot_MWaLoA(catch, dist, distributionType = "Age",
                                                 catchCategory = "Lan", ...)$data
D <- "IC_1_27.4_TBB_DEF_70-99_0_0_all"

## ---- 1. the link is country-aware ---------------------------------------
cA <- catch_row("A", D); cB <- catch_row("B", D); cC <- catch_row("C", D)
dist2 <- rbind(dist_rows("A", D, 1:3, c(10, 30, 60), c(100, 200, 300), 100),
               dist_rows("B", D, 1:3, c(1000, 1000, 1000), c(10, 20, 30), 50))
n2 <- numbers(rbind(cA, cB), dist2, fill = "vesselFlagCountry")
by_cty <- tapply(n2$number * 1e3, n2$vesselFlagCountry, sum)
ok(near(by_cty[["A"]], 100) && near(by_cty[["B"]], 3000),
   "a domain two countries share: each gets its own distribution, not the other's")
cov3 <- build_coverage_strata(rbind(cA, cB, cC), "vesselFlagCountry", dist2, what = "age",
                              catchCategory = "Lan")
ok(near(cov3$coverage[cov3$vesselFlagCountry == "A"], 100) &&
     near(cov3$coverage[cov3$vesselFlagCountry == "B"], 100) &&
     near(cov3$coverage[cov3$vesselFlagCountry == "C"], 0),
   "coverage: a country with the domain but no distribution of its own is not 'measured' because others have one")
ok(near(build_coverage_strata(rbind(cA, cB), "catchCategory", dist2, what = "age",
                              catchCategory = "Lan")$numMeasurements, 150),
   "measurements: two countries' distributions of one domain add (100 + 50), not max")
nocty <- dist2[, setdiff(names(dist2), "vesselFlagCountry")]
ok(identical(biology_match_keys(cA, nocty), c("domainBiology", "catchCategory")) &&
     identical(biology_match_keys(cA, dist2),
               c("domainBiology", "catchCategory", "vesselFlagCountry")),
   "the country is matched on only when both tables carry it")
ok(!is.null(numbers(cA, nocty)),
   "... so a distributions table without a country still links, by domain alone")

## the bundled example shares domain names across countries (Chile and China
## are both "chi"): each country's numbers must be its own distributions'
test_catch <- fx_catch()
test_dist <- fx_dist()
cd <- test_catch; dd <- test_dist
nx <- plot_NaLoA(cd, dd, catchCategory = "Lan", fill = "vesselFlagCountry")$data
own_keys <- unique(paste(cd$vesselFlagCountry, cd$domainBiology)[
  cd$catchCategory == "Lan" & !is.na(cd$domainBiology) & nzchar(cd$domainBiology)])
pdx <- prep_distributions(dd, "LengthTotal")
pdx <- pdx[pdx$.var_kind == "numbers" & pdx$catchCategory == "Lan" &
             paste(pdx$vesselFlagCountry, pdx$domainBiology) %in% own_keys, ]
# a distribution counts once for every stratum (fleet x season) of its country that names it
dom_tot <- tapply(pdx$.value_std, paste(pdx$vesselFlagCountry, pdx$domainBiology), sum)
km <- unique(cd[cd$catchCategory == "Lan" & !is.na(cd$domainBiology) & nzchar(cd$domainBiology),
                c("vesselFlagCountry", "domainBiology", "fleetValue", "seasonValue")])
want <- tapply(dom_tot[paste(km$vesselFlagCountry, km$domainBiology)], km$vesselFlagCountry, sum, na.rm = TRUE)
got  <- tapply(nx$number * 1e3, nx$vesselFlagCountry, sum)
ok(length(want) > 1 && identical(sort(names(got)), sort(names(want))) &&
     near(as.numeric(got[names(want)]), as.numeric(want)),
   "on the bundled example, where countries share domain names, each country's numbers are its own distributions'")

## ---- 1b. a record repeated exactly is not a second sample ----------------
d1 <- dist_rows("A", "d1", 1:3, c(10, 30, 60), c(100, 200, 300), 100)
rep2 <- rbind(d1, d1)
ok(grepl("repeat another row exactly", warn(numbers(catch_row("A", "d1"), rep2))),
   "a distributions table that repeats rows exactly warns")
ok(near(suppressWarnings(numbers(catch_row("A", "d1"), rep2))$number * 1e3, c(10, 30, 60)) &&
     near(suppressWarnings(mweight(catch_row("A", "d1"), rep2))$n_fish, c(10, 30, 60)),
   "... and the repeats are dropped: numbers and fish are counted once")
ok(identical(warn(numbers(catch_row("A", "d1"), d1)), "no warning"),
   "no warning when nothing repeats")

## ---- 2. n_fish -----------------------------------------------------------
mw1 <- mweight(catch_row("A", "d1"), d1)
ok(near(mw1$n_fish, c(10, 30, 60)) && near(mw1$mean_weight, c(100, 200, 300)),
   "numMeasurements 100 shared over numbers 10 / 30 / 60: 10, 30 and 60 fish per class")
two <- rbind(d1, dist_rows("A", "d2", 1:3, c(5, 5, 10), c(110, 210, 310), 20))
ok(near(mweight(rbind(catch_row("A", "d1"), catch_row("A", "d2")), two)$n_fish,
        c(10 + 5, 30 + 5, 60 + 10)),
   "two distributions in one point: their fish add")
seas <- rbind(catch_row("A", "d1", "1"), catch_row("A", "d1", "2"))
ok(near(mweight(seas, d1, colour = "vesselFlagCountry")$n_fish, c(10, 30, 60)),
   "a distribution linked to two stratum keys (two seasons) is counted once in a point that pools them")
ok(near(mweight(seas, d1, colour = "seasonValue")$n_fish, rep(c(10, 30, 60), each = 2)),
   "... and once in each point that keeps them apart")
z <- d1; z$numMeasurements <- 0
ok(all(mweight(catch_row("A", "d1"), z)$n_fish == 0),
   "a distribution that reports 0 measurements adds no fish")
nm <- d1[, setdiff(names(d1), "numMeasurements")]
ok(all(is.na(mweight(catch_row("A", "d1"), nm)$n_fish)),
   "no numMeasurements column: the count is unknown (NA), not 0")

## ---- 3. the plus group ---------------------------------------------------
d5 <- dist_rows("A", "d1", 1:5, c(10, 20, 30, 40, 50),
                c(100, 200, 300, 400, 500), 150)
c5 <- catch_row("A", "d1")
n5 <- numbers(c5, d5, plus_group = 4); m5 <- mweight(c5, d5, plus_group = 4)
ok(identical(sort(unique(n5$.class)), c(1, 2, 3, 4)) &&
     near(n5$number * 1e3, c(10, 20, 30, 90)),
   "ages 4 and 5 become class 4: numbers add up (40 + 50 = 90); ages below are untouched")
ok(near(m5$mean_weight, c(100, 200, 300, (40 * 400 + 50 * 500) / 90)),
   "the plus class weighs the number-weighted mean of its ages")
ok(near(m5$n_fish, c(10, 20, 30, 90)),
   "and holds the fish of its ages (150 shared out over the numbers)")
ok(identical(suppressWarnings(mweight(c5, d5, plus_group = 99)), mweight(c5, d5)) &&
     grepl("no age reaches", warn(mweight(c5, d5, plus_group = 99))),
   "no age reaches the start: nothing changes, and it says so")
ok(identical(m5$plus, c(FALSE, FALSE, FALSE, TRUE)) && !"plus" %in% names(mweight(c5, d5)),
   "the summary marks the pooled class in a `plus` column, and has none when nothing was pooled")
ok(near(sum(numbers(c5, d5, plus_group = 2)$number), sum(numbers(c5, d5)$number)),
   "pooling conserves the numbers")
## two distributions: pooled inside each, then averaged over strata by numbers;
## the spread is across the strata (455.6 g and 45 g), not across the ages
b5 <- dist_rows("A", "d2", 1:5, rep(100, 5), c(10, 20, 30, 40, 50), 300)
mb <- mweight(rbind(catch_row("A", "d1"), catch_row("A", "d2")), rbind(d5, b5), plus_group = 4)
plus <- mb[mb$.class == 4, ]
v1 <- (40 * 400 + 50 * 500) / 90; v2 <- (100 * 40 + 100 * 50) / 200
m_all <- (90 * v1 + 200 * v2) / 290
ok(near(plus$mean_weight, m_all) && plus$n_strata == 2 &&
     near(plus$sd_weight, sqrt((90 * (v1 - m_all)^2 + 200 * (v2 - m_all)^2) / 290)),
   "two distributions: pooled inside each first, so the band is the spread across strata")

## ---- 4. the plot ---------------------------------------------------------
cap  <- function(p) gsub("\n", " ", p$labels$caption, fixed = TRUE)
says_cap <- function(p, s) grepl(s, cap(p), fixed = TRUE)
xlab <- function(p) ggplot_build(p)$layout$panel_params[[1]]$x$get_labels()

p0 <- plot_MWaLoA(cd, dd, distributionType = "Age",
                  catchCategory = "Lan", colour = "vesselFlagCountry")
pp3 <- plot_MWaLoA(cd, dd, distributionType = "Age",
                   catchCategory = "Lan", colour = "vesselFlagCountry",
                   plus_group = 3)
ok(max(p0$data$.class) == 7 && max(pp3$data$.class) == 3 && drawable(pp3),
   "plus_group = 3 stops the age axis at the plus class (ages 0-7 become 0-3+)")
ok("3+" %in% xlab(pp3) && !any(grepl("+", setdiff(xlab(pp3), "3+"), fixed = TRUE)) &&
     !any(grepl("+", xlab(p0), fixed = TRUE)),
   "the pooled class is labelled 3+ on the axis, the others plain; no plus without the option")
ok(says_cap(pp3, "3+ = ages 3+ pooled (per-distribution mean)") &&
     !says_cap(p0, "pooled"),
   "the caption says which ages are pooled, and how")
ok(identical(pp3$theme$legend.position, "right"),
   "with a plus group the legend moves outside the panel")

pb <- plot_MWaLoA(cd, dd, distributionType = "Age",
                  catchCategory = "Lan", colour = "vesselFlagCountry",
                  show_samples = TRUE)
ok(drawable(pb) && inherits(pb$layers[[1]]$geom, "GeomCol") &&
     !inherits(pb$scales$get_scales("y")$secondary.axis, "waiver") &&
     !inherits(p0$scales$get_scales("y")$secondary.axis, "AxisSecondary"),
   "show_samples puts bars first (behind the lines) and a second axis on the right; without it neither")
top <- max(c(pb$data$mean_weight, pb$data$mean_weight + pb$data$sd_weight), na.rm = TRUE)
bars <- layer_data(pb, 1)
ok(near(max(bars$ymax), 0.75 * top, tol = 1e-6),
   "the tallest stack of bars reaches 75% of the tallest line")
ok(identical(bars$colour[1], "black") && identical(bars$linewidth[1], 0.25),
   "the sample-size bars carry a black outline")
tot_n <- tapply(pb$data$n_fish, pb$data$.class, sum, na.rm = TRUE)
top_b <- tapply(bars$ymax, bars$x, max)
top_b <- top_b[tot_n[names(top_b)] > 0]
r <- as.numeric(top_b) / as.numeric(tot_n[names(top_b)])
ok(length(r) > 1 && all(is.finite(r)) && near(r, rep(r[1], length(r))),
   "bar heights are proportional to the fish at each age, summed over the colour groups")
ok(says_cap(pb, "bars = fish measured (right axis)") &&
     identical(pb$theme$legend.position, "right") && !says_cap(p0, "bars ="),
   "the caption explains the bars")
pn <- plot_MWaLoA(cd, dd, distributionType = "Age",
                  catchCategory = "Lan", show_samples = TRUE,
                  plus_group = 3)
ok(drawable(pn) && inherits(pn$layers[[1]]$geom, "GeomCol") && "3+" %in% xlab(pn) &&
     max(pn$data$.class) == 3,
   "bars and the plus group together, with no colour column")
pl <- plot_MWaLoA(cd, dd, distributionType = "LengthTotal",
                  catchCategory = "Lan", colour = "vesselFlagCountry",
                  facet = "seasonValue", show_samples = TRUE)
ok(drawable(pl) && says_cap(pl, "bars = fish measured (right axis)"),
   "and on lengths, faceted")

## a summarised table (what a plot draws as `p$data`) carries n_fish, so it can show the bars
tab <- p0$data
pt <- plot_MWaLoA(tab, colour = "vesselFlagCountry", show_samples = TRUE)
ok("n_fish" %in% names(tab) && drawable(pt) &&
     inherits(pt$layers[[1]]$geom, "GeomCol") &&
     says_cap(pt, "bars = fish measured (right axis)"),
   "a summarised table draws the bars from its n_fish column")

## ---- 5. arguments that do not fit ----------------------------------------
ok(grepl("plus_group", err(plot_MWaLoA(
     cd, dd, distributionType = "Age", plus_group = TRUE))),
   "plus_group = TRUE is not a single number - an error that names plus_group")
ok(grepl("plus_group", err(plot_MWaLoA(
     cd, dd, distributionType = "Age", plus_group = "7"))),
   "... and a start that is not numeric is one too")
ok(grepl("only applies to ages", err(plot_MWaLoA(
     cd, dd, colour = "vesselFlagCountry", plus_group = 3))),
   "a plus group on lengths is now an error: it is for ages")
ok(grepl("only applies when `distributions`", warn(plot_MWaLoA(
     tab, colour = "vesselFlagCountry", plus_group = 3))),
   "a plus group on an already-summarised table warns: it cannot be pooled again")
ok(grepl("no age reaches", warn(plot_MWaLoA(
     cd, dd, distributionType = "Age", plus_group = 50))),
   "a start beyond the oldest age says there is no plus group")
ok(grepl("nothing to draw", warn(plot_MWaLoA(
     tab[, setdiff(names(tab), "n_fish")], colour = "vesselFlagCountry", show_samples = TRUE))),
   "show_samples on a table without n_fish says why nothing is drawn")
ok(grepl("show_samples.*TRUE or FALSE", err(plot_MWaLoA(
     cd, dd, show_samples = "yes"))),
   "show_samples is TRUE or FALSE")

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
