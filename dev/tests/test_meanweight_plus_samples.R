## Mean weight at age: the plus group, the fish behind each point, and the
## country-aware link between catch and distributions they rest on.
##
## 1. A distribution belongs to the country that submitted it. A domainBiology
##    carries no country, so two countries can use the same one - each with its
##    own distributions - and the link must not hand each the other's.
## 2. n_fish: numMeasurements is one count per distribution, shared out over
##    its classes in proportion to the numbers; a distribution counts once
##    however many stratum keys it is linked to.
## 3. The plus group pools the ages from `plus_group_start` upward inside each
##    distribution (numbers add, mean weights are number-weighted), labelled
##    "7+", and the plot says so.

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
D <- "IC_1_27.4_TBB_DEF_70-99_0_0_all"
dims_c <- c("catchCategory", "Country")

## ---- 1. the link is country-aware ---------------------------------------
cA <- catch_row("A", D); cB <- catch_row("B", D); cC <- catch_row("C", D)
dist2 <- rbind(dist_rows("A", D, 1:3, c(10, 30, 60), c(100, 200, 300), 100),
               dist_rows("B", D, 1:3, c(1000, 1000, 1000), c(10, 20, 30), 50))
lk <- link_dist_to_strata(rbind(cA, cB), dist2, dims_c, "Lan", "Age")
tot <- function(l, cty) sum(l$.value_std[l$Country == cty & l$.var_kind == "numbers"])
ok(near(tot(lk, "A"), 100) && near(tot(lk, "B"), 3000),
   "a domain two countries share: each gets its own distribution, not the other's")
bl <- biology_link(rbind(cA, cB, cC), dist2, "Country", "Lan", "Age")
ok(near(bl$coverage[bl$Country == "A"], 100) &&
     near(bl$coverage[bl$Country == "B"], 100) &&
     near(bl$coverage[bl$Country == "C"], 0),
   "coverage: a country with the domain but no distribution of its own is not 'measured' because others have one")
extra <- rbind(dist2, dist_rows("C", D, 1:3, c(5, 5, 5), c(1, 2, 3), 15))
orph <- dist_without_catch(rbind(cA, cB), extra, "Lan", "Age")
ok(!is.null(orph) && identical(as.character(orph$vesselFlagCountry), "C"),
   "a distribution from a country whose catch never names the domain is an orphan, though others use it")
sm <- stratum_measurements(rbind(cA, cB), dist2, "catchCategory", "Lan", "Age")
ok(near(sm$n_measurements, 150),
   "measurements: two countries' distributions of one domain add (100 + 50), not max")
nocty <- dist2[, setdiff(names(dist2), "vesselFlagCountry")]
ok(identical(biology_match_keys(cA, nocty), c("domainBiology", "catchCategory")) &&
     identical(biology_match_keys(cA, dist2),
               c("domainBiology", "catchCategory", "vesselFlagCountry")),
   "the country is matched on only when both tables carry it")
ok(!is.null(link_dist_to_strata(cA, nocty, dims_c, "Lan", "Age")),
   "... so a distributions table without a country still links, by domain alone")

## the bundled example shares domain names across countries (Chile and China
## are both "chi"): each country's numbers must be its own distributions'
data("anchoveta_catch", package = "rcef", envir = environment())
data("anchoveta_dist",  package = "rcef", envir = environment())
cd <- anchoveta_catch; dd <- anchoveta_dist
lkx <- link_dist_to_strata(cd, dd, dims_c, "Lan", "LengthTotal")
own_keys <- unique(paste(cd$vesselFlagCountry, cd$domainBiology)[
  cd$catchCategory == "Lan" & !is.na(cd$domainBiology) & nzchar(cd$domainBiology)])
pdx <- prep_distributions(dd, "LengthTotal")
pdx <- pdx[pdx$.var_kind == "numbers" & pdx$catchCategory == "Lan" &
             paste(pdx$vesselFlagCountry, pdx$domainBiology) %in% own_keys, ]
want <- tapply(pdx$.value_std, pdx$vesselFlagCountry, sum)
got  <- tapply(lkx$.value_std[lkx$.var_kind == "numbers"],
               lkx$Country[lkx$.var_kind == "numbers"], sum)
ok(length(want) > 1 && identical(sort(names(got)), sort(names(want))) &&
     near(as.numeric(got[names(want)]), as.numeric(want)),
   "on the bundled example, where countries share domain names, each country's numbers are its own distributions'")

## ---- 1b. a record repeated exactly is not a second sample ----------------
d1 <- dist_rows("A", "d1", 1:3, c(10, 30, 60), c(100, 200, 300), 100)
rep2 <- rbind(d1, d1)
ok(grepl("repeat another row exactly", warn(
     link_dist_to_strata(catch_row("A", "d1"), rep2, dims_c, "Lan", "Age"))),
   "a distributions table that repeats rows exactly warns")
lkd <- suppressWarnings(link_dist_to_strata(catch_row("A", "d1"), rep2, dims_c,
                                            "Lan", "Age"))
ok(nrow(lkd) == nrow(d1) &&
     near(length_numbers(lkd, character(0))$number, c(10, 30, 60)) &&
     near(length_meanweight(lkd, character(0))$n_fish, c(10, 30, 60)),
   "... and the repeats are dropped: numbers and fish are counted once")
ok(identical(warn(link_dist_to_strata(catch_row("A", "d1"), d1, dims_c, "Lan", "Age")),
             "no warning"),
   "no warning when nothing repeats")

## ---- 2. n_fish -----------------------------------------------------------
lk1 <- link_dist_to_strata(catch_row("A", "d1"), d1, dims_c, "Lan", "Age")
mw1 <- length_meanweight(lk1, character(0))
ok(near(mw1$n_fish, c(10, 30, 60)) && near(mw1$mean_weight, c(100, 200, 300)),
   "numMeasurements 100 shared over numbers 10 / 30 / 60: 10, 30 and 60 fish per class")
two <- rbind(d1, dist_rows("A", "d2", 1:3, c(5, 5, 10), c(110, 210, 310), 20))
lk2 <- link_dist_to_strata(rbind(catch_row("A", "d1"), catch_row("A", "d2")),
                           two, dims_c, "Lan", "Age")
ok(near(length_meanweight(lk2, character(0))$n_fish, c(10 + 5, 30 + 5, 60 + 10)),
   "two distributions in one point: their fish add")
seas <- rbind(catch_row("A", "d1", "1"), catch_row("A", "d1", "2"))
lk3 <- link_dist_to_strata(seas, d1, c("catchCategory", "Country", "Season"),
                           "Lan", "Age")
ok(nrow(lk3) == 2 * nrow(d1) &&
     near(length_meanweight(lk3, "Country")$n_fish, c(10, 30, 60)),
   "a distribution linked to two stratum keys (two seasons) is counted once in a point that pools them")
ok(near(length_meanweight(lk3, "Season")$n_fish, rep(c(10, 30, 60), each = 2)),
   "... and once in each point that keeps them apart")
z <- d1; z$numMeasurements <- 0
lkz <- link_dist_to_strata(catch_row("A", "d1"), z, dims_c, "Lan", "Age")
ok(all(length_meanweight(lkz, character(0))$n_fish == 0),
   "a distribution that reports 0 measurements adds no fish")
nm <- d1[, setdiff(names(d1), "numMeasurements")]
lkn <- link_dist_to_strata(catch_row("A", "d1"), nm, dims_c, "Lan", "Age")
ok(all(is.na(length_meanweight(lkn, character(0))$n_fish)),
   "no numMeasurements column: the count is unknown (NA), not 0")

## ---- 3. the plus group ---------------------------------------------------
d5 <- dist_rows("A", "d1", 1:5, c(10, 20, 30, 40, 50),
                c(100, 200, 300, 400, 500), 150)
lk5 <- link_dist_to_strata(catch_row("A", "d1"), d5, dims_c, "Lan", "Age")
pp <- pool_plus_group(lk5, 4, dims_c)
n5 <- length_numbers(pp, character(0)); m5 <- length_meanweight(pp, character(0))
ok(identical(sort(unique(pp$.class)), c(1, 2, 3, 4)) &&
     near(n5$number, c(10, 20, 30, 90)),
   "ages 4 and 5 become class 4: numbers add up (40 + 50 = 90); ages below are untouched")
ok(near(m5$mean_weight, c(100, 200, 300, (40 * 400 + 50 * 500) / 90)),
   "the plus class weighs the number-weighted mean of its ages")
ok(near(m5$n_fish, c(10, 20, 30, 90)),
   "and holds the fish of its ages (150 shared out over the numbers)")
ok(identical(pool_plus_group(lk5, 99, dims_c), lk5),
   "no age reaches the start: nothing changes")
ok(!".plus" %in% names(lk5) && all(pp$.plus == (pp$.class == 4)) &&
     any(pp$.plus) && !all(pp$.plus),
   "the pooled rows are marked (.plus): the class-4 rows and only those")
ok(identical(m5$plus, c(FALSE, FALSE, FALSE, TRUE)) &&
     !"plus" %in% names(length_meanweight(lk5, character(0))),
   "the summary keeps the mark as a `plus` column, and has none for a link that was not pooled")
ok(near(sum(length_numbers(pool_plus_group(lk5, 2, dims_c), character(0))$number),
        sum(length_numbers(lk5, character(0))$number)),
   "pooling conserves the numbers")
## two distributions: pooled inside each, then averaged over strata by numbers;
## the spread is across the strata (455.6 g and 45 g), not across the ages
b5 <- dist_rows("A", "d2", 1:5, rep(100, 5), c(10, 20, 30, 40, 50), 300)
lkb <- link_dist_to_strata(rbind(catch_row("A", "d1"), catch_row("A", "d2")),
                           rbind(d5, b5), dims_c, "Lan", "Age")
mb <- length_meanweight(pool_plus_group(lkb, 4, dims_c), character(0))
plus <- mb[mb$.class == 4, ]
v1 <- (40 * 400 + 50 * 500) / 90; v2 <- (100 * 40 + 100 * 50) / 200
m_all <- (90 * v1 + 200 * v2) / 290
ok(near(plus$mean_weight, m_all) && plus$n_strata == 2 &&
     near(plus$sd_weight, sqrt((90 * (v1 - m_all)^2 + 200 * (v2 - m_all)^2) / 290)),
   "two distributions: pooled inside each first, so the band is the spread across strata")

## ---- 4. the plot ---------------------------------------------------------
sbt <- function(p) gsub("\n", " ", p$labels$subtitle, fixed = TRUE)
says <- function(p, s) grepl(s, sbt(p), fixed = TRUE)
xlab <- function(p) ggplot_build(p)$layout$panel_params[[1]]$x$get_labels()

p0 <- plot_meanweight_at_length_age(cd, dd, distributionType = "Age",
                                    catchCategory = "Lan", colour = "Country")
pp3 <- plot_meanweight_at_length_age(cd, dd, distributionType = "Age",
                                     catchCategory = "Lan", colour = "Country",
                                     plus_group = TRUE, plus_group_start = 3)
ok(max(p0$data$.class) == 7 && max(pp3$data$.class) == 3 && drawable(pp3),
   "plus_group_start = 3 stops the age axis at the plus class (ages 0-7 become 0-3+)")
ok("3+" %in% xlab(pp3) && !any(grepl("+", setdiff(xlab(pp3), "3+"), fixed = TRUE)) &&
     !any(grepl("+", xlab(p0), fixed = TRUE)),
   "the pooled class is labelled 3+ on the axis, the others plain; no plus without the option")
ok(says(pp3, "Ages 3 and older are pooled into a plus group (3+)") &&
     says(pp3, "pooled within each stratum first") && !says(p0, "plus group"),
   "the subtitle says which ages are pooled, and how")
ok(identical(pp3$theme$legend.position, "right"),
   "with a plus group the legend moves outside the panel")

pb <- plot_meanweight_at_length_age(cd, dd, distributionType = "Age",
                                    catchCategory = "Lan", colour = "Country",
                                    show_samples = TRUE)
ok(drawable(pb) && inherits(pb$layers[[1]]$geom, "GeomCol") &&
     !inherits(pb$scales$get_scales("y")$secondary.axis, "waiver") &&
     !inherits(p0$scales$get_scales("y")$secondary.axis, "AxisSecondary"),
   "show_samples puts bars first (behind the lines) and a second axis on the right; without it neither")
top <- max(c(pb$data$mean_weight, pb$data$mean_weight + pb$data$sd_weight), na.rm = TRUE)
bars <- layer_data(pb, 1)
ok(near(max(bars$ymax), 0.4 * top, tol = 1e-6),
   "the tallest stack of bars reaches 40% of the tallest line")
tot_n <- tapply(pb$data$n_fish, pb$data$.class, sum, na.rm = TRUE)
top_b <- tapply(bars$ymax, bars$x, max)
top_b <- top_b[tot_n[names(top_b)] > 0]
r <- as.numeric(top_b) / as.numeric(tot_n[names(top_b)])
ok(length(r) > 1 && all(is.finite(r)) && near(r, rep(r[1], length(r))),
   "bar heights are proportional to the fish at each age, summed over the colour groups")
ok(says(pb, "Bars (right axis) = fish measured at each age") &&
     says(pb, "shared out over its ages in proportion to its numbers") &&
     identical(pb$theme$legend.position, "right") && !says(p0, "Bars"),
   "the subtitle explains the bars")
pn <- plot_meanweight_at_length_age(cd, dd, distributionType = "Age",
                                    catchCategory = "Lan", show_samples = TRUE,
                                    plus_group = TRUE, plus_group_start = 3)
ok(drawable(pn) && inherits(pn$layers[[1]]$geom, "GeomCol") && "3+" %in% xlab(pn) &&
     max(pn$data$.class) == 3,
   "bars and the plus group together, with no colour column")
pl <- plot_meanweight_at_length_age(cd, dd, distributionType = "LengthTotal",
                                    catchCategory = "Lan", colour = "Country",
                                    facet = "Season", show_samples = TRUE)
ok(drawable(pl) && says(pl, "fish measured at each length"),
   "and on lengths, faceted")

## a summary built from a pooled link says so in its `plus` column, so a plot
## of it labels the class without being told (how the app draws a plus group)
dims_a <- c("catchCategory", "Country")
tab_pp <- length_meanweight(pool_plus_group(
  link_dist_to_strata(cd, dd, dims_a, "Lan", "Age"), 3, dims_a), "Country")
pt_pp <- plot_meanweight_at_length_age(tab_pp, colour = "Country")
ok(max(pt_pp$data$.class) == 3 && "3+" %in% xlab(pt_pp) &&
     says(pt_pp, "Ages 3 and older are pooled into a plus group (3+)") &&
     identical(pt_pp$theme$legend.position, "right"),
   "a pooled summary is labelled 3+ from its `plus` column, with no plus_group argument")
ok(!any(grepl("+", xlab(plot_meanweight_at_length_age(
       length_meanweight(link_dist_to_strata(cd, dd, dims_a, "Lan", "Age"), "Country"),
       colour = "Country")), fixed = TRUE)),
   "and a summary that was not pooled shows no plus")

## a summarised table carries n_fish (length_meanweight), so it can show the bars
tab <- length_meanweight(link_dist_to_strata(cd, dd, c("catchCategory", "Country"),
                                             "Lan", "Age"), "Country")
pt <- plot_meanweight_at_length_age(tab, colour = "Country", show_samples = TRUE)
ok("n_fish" %in% names(tab) && drawable(pt) &&
     inherits(pt$layers[[1]]$geom, "GeomCol") &&
     says(pt, "Bars (right axis) = fish measured at each "),
   "a length_meanweight() table draws the bars from its n_fish column")

## ---- 5. arguments that do not fit ----------------------------------------
ok(grepl("plus_group_start", err(plot_meanweight_at_length_age(
     cd, dd, distributionType = "Age", plus_group = TRUE))),
   "plus_group = TRUE without plus_group_start is an error that names it")
ok(grepl("plus_group_start", err(plot_meanweight_at_length_age(
     cd, dd, distributionType = "Age", plus_group = TRUE, plus_group_start = "7"))),
   "... and a start that is not a number is one too")
ok(grepl("only applies to ages", warn(plot_meanweight_at_length_age(
     cd, dd, colour = "Country", plus_group = TRUE, plus_group_start = 3))),
   "a plus group on lengths warns: it is for ages")
ok(grepl("only used with plus_group = TRUE", warn(plot_meanweight_at_length_age(
     cd, dd, distributionType = "Age", plus_group_start = 3))),
   "a start without plus_group = TRUE warns rather than being ignored silently")
ok(grepl("only applies when `distributions`", warn(plot_meanweight_at_length_age(
     tab, colour = "Country", plus_group = TRUE, plus_group_start = 3))),
   "a plus group on an already-summarised table warns: it cannot be pooled again")
ok(grepl("no age reaches", warn(plot_meanweight_at_length_age(
     cd, dd, distributionType = "Age", plus_group = TRUE, plus_group_start = 50))),
   "a start beyond the oldest age says there is no plus group")
ok(grepl("nothing to draw", warn(plot_meanweight_at_length_age(
     tab[, setdiff(names(tab), "n_fish")], colour = "Country", show_samples = TRUE))),
   "show_samples on a table without n_fish says why nothing is drawn")
ok(grepl("TRUE or FALSE", err(plot_meanweight_at_length_age(
     cd, dd, show_samples = "yes"))),
   "show_samples and plus_group are each TRUE or FALSE")

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
