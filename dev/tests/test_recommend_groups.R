## recommend_groups(): the groups of the raising and of the allocation, chosen from a hierarchy of variables.
##
## Worked by hand on a tiny catch (levels, groups, conditions, thresholds), then through the engines - catchRaising()
## with the conditions it returns, on numbers worked out by hand - and on the test fixture for the lengths
## (catchAllocationAoL()). What it must guarantee is as much a part of the contract as what it chooses: the strata
## conditions share every row of the catch out between the groups, and no group is given a pool with nothing sampled in it.

source("dev/tests/fixture.R")
FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}
near <- function(a, b, tol = 1e-6) isTRUE(all.equal(a, b, tolerance = tol, check.attributes = FALSE))
err  <- function(expr) tryCatch({ force(expr); "no error" }, error = function(e) conditionMessage(e))
warn <- function(expr) { w <- NULL
  withCallingHandlers(force(expr), warning = function(x) { w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") })
  if (is.null(w)) "no warning" else paste(w, collapse = " | ") }
quiet <- function(expr) { out <- NULL; invisible(utils::capture.output(out <- suppressWarnings(suppressMessages(expr)))); out }
suppressPackageStartupMessages(library(dplyr))

## ---- 1. a small catch, worked by hand ----------------------------------
## hierarchy = area > metier, kg, one discard estimate per domain:
##   stratum    landings  with an estimate  discards  trips
##   A1 | OTB     4000        3000            200       4     -> 75 % covered
##   A1 | TBB     2000           0              -       2     -> nothing sampled
##   A2 | OTB     1000           0              -       1
##   A2 | SSC      500         100             10       1     -> 20 %
##   A3 | TBB     3000        3000 ... but the only discard row of its domain is Official, 0 kg: no estimate
##   A3 | OTB      800         800             40       3     -> 100 %
## the pools: area A1 = 6 t landed, 3 t sampled (50 %); A2 = 1.5 t, 0.1 t (6.7 %); A3 = 3.8 t, 0.8 t (21 %);
## everything = 11.3 t, 3.9 t (34.5 %)
hc <- data.frame(
  catchCategory  = c("Lan", "Lan", "Lan", "Lan", "Lan", "Lan", "Lan", "Lan", "Dis", "Dis", "Dis", "Dis"),
  areaValue      = c("A1", "A1", "A1", "A2", "A2", "A2", "A3", "A3", "A1", "A2", "A3", "A3"),
  metier4        = c("OTB", "OTB", "TBB", "OTB", "SSC", "SSC", "TBB", "OTB", "OTB", "SSC", "TBB", "OTB"),
  total          = c(3000, 1000, 2000, 1000, 100, 400, 3000, 800, 200, 10, 0, 40),
  domainCatchDis = c("d1", NA, NA, NA, "d3", NA, "d4", "d5", "d1", "d3", "d4", "d5"),
  numTrips       = c(4, 4, 2, 1, 1, 1, 2, 3, 4, 1, 2, 3),
  originType     = c(rep("WGEstimate", 10), "Official", "WGEstimate"),
  variableType   = "WeightLive", variableUnit = "kg", vesselFlagCountry = "BE", year = 2023L, stock = "syn",
  stringsAsFactors = FALSE)
hier <- c("areaValue", "metier4")

rec <- recommend_groups(hc, hier, list(cov = 30))
ok(inherits(rec, "rcef_groups") && identical(rec$what, "discard") && identical(rec$hierarchy, hier),
   "the result is an rcef_groups object for the discards, with the hierarchy as given")
ok(identical(names(rec$strataCond), names(rec$matchedDataCond)) &&
     identical(names(rec$strataCond), c("L2_1", "L2_2", "L1_1", "L0")),
   "one group per pool, named after its level and a number: L2_1, L2_2 (own strata), L1_1 (the area), L0 (everything)")
st <- rec$strata
lv <- function(a, m) st$level[st$areaValue == a & st$metier4 == m]
ok(nrow(st) == 6, "one row per stratum")
ok(lv("A1", "OTB") == 2 && lv("A3", "OTB") == 2,
   "the strata that reach 30 % on their own (75 %, 100 %) are raised from themselves")
ok(lv("A1", "TBB") == 1,
   "A1 | TBB, with nothing sampled, takes the area pool (50 %)")
ok(lv("A2", "OTB") == 0 && lv("A2", "SSC") == 0 && lv("A3", "TBB") == 0,
   "the other strata fall to the pool of everything (the pools of A2 and A3 reach 7 % and 21 %)")
ok(near(st$weight_t[st$areaValue == "A1" & st$metier4 == "OTB"], 4) &&
     near(st$covered_t[st$areaValue == "A1" & st$metier4 == "OTB"], 3) &&
     near(st$coverage[st$areaValue == "A1" & st$metier4 == "OTB"], 75),
   "the weights are in tonnes: 4 t landed, 3 t with an estimate, 75 %")
ok(near(st$coverage[st$areaValue == "A3" & st$metier4 == "TBB"], 0),
   "A3 | TBB: a domain whose only discard row is Official is no discard estimate (coverage 0)")
ok(near(st$pool_coverage[st$areaValue == "A1" & st$metier4 == "TBB"], 50) &&
     near(st$pool_coverage[st$level == 0][1], 34.5),
   "each stratum carries the coverage of the pool it was given (50 %; 34.5 % for everything)")

g <- rec$groups
ok(identical(g$group, c("L2_1", "L2_2", "L1_1", "L0")) && identical(g$n_strata, c(1L, 1L, 1L, 3L)) &&
     near(g$weight_t, c(4, 0.8, 2, 4.5)),
   "groups: the biggest first within a level, with the strata and landings they hold")
ok(g$pool[g$group == "L1_1"] == "areaValue = A1" && g$pool[g$group == "L0"] == "everything" &&
     g$pool[g$group == "L2_2"] == "areaValue = A3, metier4 = OTB",
   "groups say what their pool is")
ok(!any(c("numMeasurements", "pool_fish") %in% c(names(g), names(st))),
   "discards have no fish: the tables carry no fish count")
lt <- rec$levels
ok(identical(lt$level, c(2L, 1L, 0L)) && identical(lt$n_groups, c(2L, 1L, 1L)) && identical(lt$n_strata, c(2L, 1L, 3L)),
   "levels: how many groups and strata fall in each, from the narrowest pool to everything")
ok(near(lt$weight_t, c(4.8, 2, 4.5)) && near(lt$pct_weight, c(42.5, 17.7, 39.8)),
   "levels: the landings they take (4.8, 2 and 4.5 t of 11.3 t)")
ok(near(lt$uncovered_t, c(1, 2, 4.4)) && near(lt$pct_uncovered, c(13.5, 27, 59.5)) &&
     near(lt$cum_pct_uncovered, c(13.5, 40.5, 100)),
   "levels: what has no estimate of its own (1, 2 and 4.4 t) and the share of it each level settles")
ok(any(grepl("Groups for the raising of the discards: 6 strata in 4 groups", utils::capture.output(print(rec)), fixed = TRUE)) &&
     any(grepl("areaValue > metier4", utils::capture.output(print(rec)), fixed = TRUE)),
   "print() says what the groups are for and the hierarchy")

## ---- 2. the conditions -------------------------------------------------
## strataCond must share EVERY row of the catch out - the raising leaves out what is in no group and stops on an overlap
member <- sapply(rec$strataCond, function(q) rlang::eval_tidy(q, data = hc))
ok(all(rowSums(member) == 1), "every row of the catch is in exactly one group's strataCond")
ok(identical(unname(which(member[, "L2_1"])), c(1L, 2L, 9L)) && identical(unname(which(member[, "L1_1"])), 3L),
   "L2_1 holds the rows of A1 | OTB (landings and discards), L1_1 those of A1 | TBB")
ok(identical(unname(which(member[, "L0"])), c(4L, 5L, 6L, 7L, 10L, 11L)),
   "the pool of everything takes what no narrower group claimed (the complement, not a list)")
mm <- sapply(rec$matchedDataCond, function(q) rep_len(rlang::eval_tidy(q, data = hc), nrow(hc)))   # the pool of everything is just TRUE
ok(identical(unname(which(mm[, "L2_1"])), c(1L, 2L, 9L)) && identical(unname(which(mm[, "L1_1"])), c(1L, 2L, 3L, 9L)) &&
     all(mm[, "L0"]),
   "matchedDataCond: the rows the ratio is taken from - the stratum, its area, all of it - which overlap")
ok(all(vapply(c(rec$strataCond, rec$matchedDataCond), rlang::is_quosure, NA)),
   "the conditions are quosures, as catchRaising() takes them")
# an NA in a hierarchy variable is a value of its own, in the keys and in the conditions
hn <- hc; hn$metier4[hn$areaValue == "A1" & hn$metier4 == "TBB"] <- NA
rn <- recommend_groups(hn, hier, list(cov = 30))
mn <- sapply(rn$strataCond, function(q) rlang::eval_tidy(q, data = hn))
mq <- rep_len(rlang::eval_tidy(rn$matchedDataCond[["L1_1"]], data = hn), nrow(hn))
ok(all(rowSums(mn) == 1) && isTRUE(rn$strata$level[is.na(rn$strata$metier4)] == 1) && identical(unname(which(mq)), c(1L, 2L, 3L, 9L)),
   "an NA in a hierarchy variable is a value of its own: every row still in exactly one group, and the area pool takes the NA stratum")

## ---- 3. the thresholds -------------------------------------------------
r50 <- NULL
w50 <- warn(r50 <- recommend_groups(hc, hier, list(cov = 50)))
ok(grepl("pool of everything does not reach the thresholds", w50) && grepl("34.5%", w50, fixed = TRUE),
   "when even everything is below the coverage, the strata fall in it all the same and a warning says so")
ok(identical(sort(unique(r50$strata$level)), c(0L, 1L, 2L)) &&
     r50$strata$level[r50$strata$areaValue == "A1" & r50$strata$metier4 == "TBB"] == 1L,
   "a pool that is exactly at the coverage reaches it (A1: 50 % >= 50)")
# trips: A1 | OTB has 4, the area pool 4 + 2 = 6 (strata add up); A3 | OTB has 3, its area pool 5 but only 21 %
rt <- recommend_groups(hc, hier, list(cov = 30, trips = 5))
ok(rt$strata$level[rt$strata$areaValue == "A1" & rt$strata$metier4 == "OTB"] == 1L &&
     rt$strata$level[rt$strata$areaValue == "A3" & rt$strata$metier4 == "OTB"] == 0L,
   "trips: A1 | OTB (4 trips) goes to its area (6 trips), A3 | OTB (3 trips, area 21 %) to everything")
ok(near(rt$thresholds$trips, 5) && is.na(rt$thresholds$fish) && near(rt$thresholds$cov, 30),
   "the thresholds as used are returned")
ok(grepl("trips >= 5", paste(utils::capture.output(print(rt)), collapse = "\n"), fixed = TRUE),
   "and printed")
nt <- hc; nt$numTrips <- NA_real_
ok(grepl("no numTrips", warn(recommend_groups(nt, hier, list(cov = 30, trips = 5)))),
   "trips asked for and the catch has none: a warning, not a silent pass")
ok(grepl("which discards have none", warn(r_f <- recommend_groups(hc, hier, list(cov = 30, fish = 100)))) &&
     is.na(r_f$thresholds$fish),
   "fish for discards is not used, and says so")
# without A2 | SSC the area A2 has no estimate at all: whatever the coverage asked for, it has no ratio to give
h2 <- hc[!(hc$areaValue == "A2" & hc$metier4 == "SSC"), ]
r0 <- recommend_groups(h2, hier, list(cov = 0))
lv0 <- function(a, m) r0$strata$level[r0$strata$areaValue == a & r0$strata$metier4 == m]
ok(lv0("A2", "OTB") == 0L && lv0("A1", "TBB") == 1L && lv0("A3", "TBB") == 1L && lv0("A1", "OTB") == 2L,
   "cov = 0 does not take a pool with nothing sampled in it (A2 | OTB goes to everything; the others to the narrowest pool with a sample)")
ok(identical(suppressWarnings(recommend_groups(hc, hier))$thresholds$cov, 50),
   "the default coverage is 50")

## ---- 4. what is refused ------------------------------------------------
ok(grepl("no column 'nope'", err(recommend_groups(hc, c("areaValue", "nope")))),
   "a hierarchy variable that is not in the catch is named")
ok(grepl("names the variables", err(recommend_groups(hc, character()))),
   "an empty hierarchy is refused")
ok(grepl("catchCategory", err(recommend_groups(hc, c("areaValue", "catchCategory")))),
   "catchCategory is not a stratum variable")
ok(grepl("cannot be empty", err(recommend_groups(hc, hier, list(cov = NA)))) &&
     grepl("cannot be empty", err(recommend_groups(hc, hier, list(cov = 120)))),
   "the coverage cannot be empty or out of 0 to 100")
ok(grepl("takes cov, trips and fish", err(recommend_groups(hc, hier, list(cov = 50, nope = 1)))),
   "an unknown threshold is named")
ok(grepl("named list", err(recommend_groups(hc, hier, 50))),
   "thresholds that is not a list is refused")
ok(grepl("is a number, or NA", err(recommend_groups(hc, hier, list(cov = 50, trips = -1)))),
   "trips and fish are a number or NA")
two <- hc; two$year[1:3] <- 2022L
ok(grepl("more than one year", err(recommend_groups(two, hier))),
   "more than one year is refused: the groups are made within one year and one stock")
ok(grepl("nothing to make groups of", err(recommend_groups(hc[hc$catchCategory == "BMS", ], hier))) ||
     grepl("nothing to make groups of|no landings", err(recommend_groups(hc[0, ], hier))),
   "no landings or discards: nothing to make groups of")

## ---- 5. through the raising engine ------------------------------------
## The same catch in the full CEF shape (a row of the test fixture as the template), raised with the conditions as they
## come. Worked by hand (kg): A1 | OTB 1000 x 200/3000; A1 | TBB 2000 x the ratio of the area (200/3000); A2 | OTB 1000,
## A2 | SSC 400 and A3 | TBB 3000 x the ratio of everything, (200 + 10 + 40)/(3000 + 100 + 800) - the Official 0 kg is no
## estimate, so A3 | TBB is raised, and its domain is not a donor; A3 | OTB has all its landings sampled.
cf <- as.data.frame(fx_catch()); tmpl <- cf[cf$year == 2023 & cf$catchCategory == "Lan", ][1, ]
ec <- do.call(rbind, lapply(seq_len(nrow(hc)), function(i) {
  r <- tmpl
  for (v in c("catchCategory", "areaValue", "metier4", "total", "domainCatchDis", "numTrips", "originType", "variableType", "variableUnit"))
    r[[v]] <- hc[[v]][i]
  r$domainCatchBMS <- NA_character_; r$domainBiology <- NA_character_; r$vesselFlagCountry <- "BE"
  r }))
ec$year <- 2023L
rec_e <- recommend_groups(ec, hier, list(cov = 30))
ok(identical(rec_e$strata$level[order(rec_e$strata$areaValue, rec_e$strata$metier4)], rec$strata$level[order(rec$strata$areaValue, rec$strata$metier4)]),
   "the groups of the full CEF shape are the same")
raised <- as.data.frame(quiet(catchRaising(ec, rec_e$strataCond, rec_e$matchedDataCond, type = "discards",
                                           originType = "WGEstimate", variableType = "WeightLive",
                                           logFile = tempfile(fileext = ".txt"), verbose = FALSE)))
rd <- raised[raised$catchCategory == "Dis" & raised$importedOrRaised == "raised", ]
ok(nrow(rd) == 5 && !anyNA(rd$total) && !any(is.nan(rd$total)),
   "five landings had no estimate and were raised, none to a NaN (every group had a donor)")
got <- tapply(rd$total, paste(rd$areaValue, rd$metier4), sum)
r_all <- (200 + 10 + 40) / (3000 + 100 + 800)
want <- c("A1 OTB" = 1000 * 200 / 3000, "A1 TBB" = 2000 * 200 / 3000, "A2 OTB" = 1000 * r_all, "A2 SSC" = 400 * r_all,
          "A3 TBB" = 3000 * r_all)
ok(near(as.numeric(got[names(want)]), as.numeric(want)),
   "the raised discards are the landings without an estimate times the ratio of their group")
ok(near(sum(raised$total[raised$catchCategory == "Lan"]), sum(hc$total[hc$catchCategory == "Lan"])),
   "the landings are untouched")
ok(setequal(unique(raised$DrGroup), names(rec_e$strataCond)),
   "every group of the recommendation was used by the raising")
ok(identical(sort(unique(raised$DrGroup[raised$areaValue == "A1" & raised$metier4 == "TBB"])), "L1_1"),
   "and A1 | TBB was raised in the group of its area")

## the same on the test fixture, a year at a time, at the finest hierarchy: every raised discard has a ratio
cy <- cf[cf$year == 2023, ]
fh <- c("areaValue", "mesh", "vesselFlagCountry", "seasonValue")
rec_f <- suppressWarnings(recommend_groups(cy, fh, list(cov = 50)))
raised_f <- as.data.frame(quiet(catchRaising(cy, rec_f$strataCond, rec_f$matchedDataCond, type = "discards",
                                             logFile = tempfile(fileext = ".txt"), verbose = FALSE)))
ok(!any(is.nan(raised_f$total)) && all(raised_f$DrGroup %in% names(rec_f$strataCond)),
   "fixture: no raised discard is NaN, and every row is in a recommended group")
ok(sum(raised_f$catchCategory %in% c("Lan", "Dis") & raised_f$importedOrRaised == "raised") ==
     sum(raised_f$catchCategory == "Dis" & raised_f$importedOrRaised == "raised"),
   "fixture: raising only adds discards")
ok(near(sum(raised_f$total[raised_f$catchCategory == "Lan"]), sum(cy$total[cy$catchCategory == "Lan"])),
   "fixture: the landings are untouched")

## ---- 6. lengths --------------------------------------------------------
al   <- fx_allocation(2023)
dy   <- as.data.frame(fx_dist()); dy <- dy[dy$year == 2023, ]
dl   <- dy[dy$distributionType == "LengthTotal" &
             paste(dy$vesselFlagCountry, dy$catchCategory, dy$domainBiology) %in%
               paste(al$catch1$vesselFlagCountry, al$catch1$catchCategory, al$catch1$domainBiology), ]
lh   <- c("areaValue", "metier4")
rl   <- NULL
warn_l <- warn(rl <- recommend_groups(al$catch1, lh, list(cov = 50, trips = NA, fish = 100), distribution_data = dl))
ok(identical(rl$what, "length") && "catchCategory" %in% names(rl$strata) &&
     all(c("category", "level") %in% names(rl$levels)) && all(c("category", "level") %in% names(rl$groups)) &&
     !any(c("category", "catchCategory") %in% names(rec$levels)) && !("category" %in% names(rec$groups)),
   "lengths: landings and discards are grouped on their own (a category column); the groups of the discards have none")
w_len <- warn(recommend_groups(al$catch1, lh, list(cov = 50, fish = 1e9), distribution_data = dl))
ok(grepl("Lan: coverage", w_len) && grepl("Dis: coverage", w_len) && grepl(" fish)", w_len, fixed = TRUE) &&
     grepl("allocated from it all the same", w_len),
   "lengths: when even everything falls short, the warning names the catch part and what it lacks")
ok(all(grepl("^(Lan|Dis)_L[0-9]", names(rl$strataCond))),
   "lengths: groups are named after their catch part, then the level (Lan_L1_1 ...)")
ok(all(grepl("catchCategory ==", vapply(rl$strataCond, rlang::quo_text, ""))),
   "lengths: the conditions say which catch part (landings and discards are grouped on their own)")
ok(all(rl$strata$numMeasurements >= 0) && "pool_fish" %in% names(rl$strata) && near(rl$thresholds$fish, 100),
   "lengths: the measurements behind each stratum and pool are in the table")
ok(any(rl$strata$level > 0) && all(rl$strata$pool_fish[rl$strata$level > 0] >= 100),
   "lengths: a pool that is not the pool of everything has at least the fish asked for")
ok(all(c("numMeasurements", "pool_fish") %in% c(names(rl$groups), names(rl$strata))) &&
     "numMeasurements" %in% names(rl$groups) && "pool_fish" %in% names(rl$strata),
   "lengths: the groups and the strata carry the fish behind them")
alloc <- quiet(catchAllocationAoL(catch_data = al$catch1, distribution_data = dl,
                                  condition_alloc_st_list = rl$strataCond, condition_matched_data_list = rl$matchedDataCond,
                                  distributionType = "LengthTotal", verbose = FALSE, logFile = tempfile(fileext = ".txt"),
                                  assembled_output = TRUE))
ok(is.list(alloc) && all(c("distribution", "catch") %in% names(alloc)) && nrow(alloc$distribution) > 0,
   "lengths: catchAllocationAoL() runs on the conditions (own-stratum groups included)")
ok(all(na.omit(unique(alloc$catch$allocGroup)) %in% names(rl$strataCond)),
   "lengths: the allocation groups are the recommended ones")
ok(near(sum(alloc$catch$total[alloc$catch$catchCategory == "Lan"], na.rm = TRUE),
        sum(al$catch1$total[al$catch1$catchCategory == "Lan"], na.rm = TRUE)),
   "lengths: the landings come out of the allocation as they went in")
ra <- suppressWarnings(recommend_groups(al$catch1, lh, list(cov = 50), distribution_data = dy[dy$distributionType == "Age", ],
                                        distributionType = "Age"))
ok(identical(ra$what, "age"), "ages: distributionType = 'Age' makes the groups of the ages")

## ---- 7. looking at the levels ------------------------------------------
pg <- list(plot_groups(rec), plot_groups(rec, x = "areaValue"), plot_groups(rec, x = "metier4", share = TRUE),
           plot_groups(rl), plot_river(rec, left_var = "areaValue"))
ok(all(vapply(pg, inherits, NA, what = "ggplot")), "plot_groups() and plot_river() draw a recommendation")
built <- tryCatch({ lapply(pg, ggplot2::ggplot_build); TRUE }, error = function(e) conditionMessage(e))
ok(isTRUE(built), "... and they build")
txt <- unlist(lapply(ggplot2::ggplot_build(pg[[1]])$data, function(d) if ("label" %in% names(d)) as.character(d$label)))
ok(setequal(txt, c("2 strata", "1 stratum", "3 strata")),
   "the levels plot says how many strata are in each level (2, 1 and 3 in the small catch), in the singular for one")
ok(grepl("Which areaValue is raised at which level", pg[[5]]$labels$title, fixed = TRUE),
   "plot_river() on a recommendation: which stratum is raised at which level")
f <- tempfile(fileext = ".png")
invisible(plot_groups(rec, save = f))
ok(file.exists(f) && file.size(f) > 1000, "plot_groups(save =) writes the figure")
ok(grepl("not an rcef_groups|recommend_groups", err(plot_groups(hc)), ignore.case = TRUE),
   "plot_groups() on anything else points to recommend_groups()")

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
