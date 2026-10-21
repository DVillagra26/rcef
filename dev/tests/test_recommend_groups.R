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
##   A3 | OTB      800         800             40       3     -> 100 %: nothing to raise
## the pools: area A1 = 6 t landed, 3 t sampled (50 %); A2 = 1.5 t, 0.1 t (6.7 %); A3 = 3.8 t, 0.8 t (21 %);
## everything = 11.3 t, 3.9 t (34.5 %). The levels, narrowest first: L1 the stratum as reported (the catch has a
## country that the hierarchy leaves out, so the strata of the hierarchy are not the reported ones: L2 is the stratum
## summed to the hierarchy, L3 the area, L4 All by All).
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
     identical(names(rec$strataCond), c("L1", "L2_1", "L3_1", "L4")),
   "one group per pool, named after its level and a number: L1 (nothing to raise), L2_1 (an own stratum), L3_1 (the area), L4 (All by All)")
st <- rec$strata
lv <- function(a, m) st$level[st$areaValue == a & st$metier4 == m]
ok(nrow(st) == 6, "one row per stratum")
ok(lv("A1", "OTB") == 2,
   "the stratum that reaches 30 % on its own (75 %) is raised from itself: L2, the own stratum aggregated to the hierarchy")
ok(lv("A3", "OTB") == 1,
   "A3 | OTB, all of whose landings have an estimate, has nothing to raise: it stays at L1, the stratum as reported")
ok(lv("A1", "TBB") == 3,
   "A1 | TBB, with nothing sampled, takes the area pool (50 %): L3")
ok(lv("A2", "OTB") == 4 && lv("A2", "SSC") == 4 && lv("A3", "TBB") == 4,
   "the other strata fall to All by All, L4 (the pools of A2 and A3 reach 7 % and 21 %)")
ok(near(st$weight_t[st$areaValue == "A1" & st$metier4 == "OTB"], 4) &&
     near(st$covered_t[st$areaValue == "A1" & st$metier4 == "OTB"], 3) &&
     near(st$raised_t[st$areaValue == "A1" & st$metier4 == "OTB"], 1) &&
     near(st$coverage[st$areaValue == "A1" & st$metier4 == "OTB"], 75),
   "the weights are in tonnes: 4 t landed, 3 t with an estimate, 1 t to raise, 75 %")
ok(near(st$coverage[st$areaValue == "A3" & st$metier4 == "TBB"], 0),
   "A3 | TBB: a domain whose only discard row is Official is no discard estimate (coverage 0)")
ok(near(st$pool_coverage[st$areaValue == "A1" & st$metier4 == "TBB"], 50) &&
     near(st$pool_coverage[st$level == 4][1], 34.5) && is.na(st$pool_coverage[st$level == 1]),
   "each stratum carries the coverage of the pool it was given (50 %; 34.5 % for All by All; none where nothing is raised)")

g <- rec$groups
ok(identical(g$group, c("L1", "L2_1", "L3_1", "L4")) && identical(g$n_strata, c(1L, 1L, 1L, 3L)) &&
     near(g$weight_t, c(0.8, 4, 2, 4.5)) && near(g$raised_t, c(0, 1, 2, 4.4)),
   "groups: in the order of the levels, with the strata, the landings and the weight they raise")
ok(g$pool[g$group == "L3_1"] == "areaValue = A1" && g$pool[g$group == "L4"] == "All by All" &&
     g$pool[g$group == "L1"] == "nothing to raise" &&
     g$pool[g$group == "L2_1"] == "areaValue = A1, metier4 = OTB",
   "groups say what their pool is")
ok(!any(c("numMeasurements", "pool_fish") %in% c(names(g), names(st))),
   "discards have no fish: the tables carry no fish count")
lt <- rec$levels
ok(identical(lt$level, 1:4) && identical(lt$label, c("own stratum (raw)", "own stratum (aggregated)",
                                                      "pooled across metier4", "All by All")),
   "levels: L1 the stratum as reported, L2 the stratum aggregated, L3 pooled across the last variable, L4 All by All")
ok(identical(lt$n_groups, c(1L, 1L, 1L, 1L)) && identical(lt$n_strata, c(1L, 1L, 1L, 3L)),
   "levels: how many groups and strata fall in each")
ok(near(lt$covered_t, c(3.9, 0, 0, 0)) && near(lt$raised_t, c(0, 1, 2, 4.4)) &&
     near(lt$weight_t, c(3.9, 1, 2, 4.4)) && near(lt$pct_weight, c(34.5, 8.8, 17.7, 38.9)),
   "levels: the landings that rest on each (3.9 t have an estimate of their own, then 1, 2 and 4.4 t raised, of 11.3 t)")
ok(near(lt$pct_raised, c(0, 13.5, 27, 59.5)) && near(lt$cum_pct_raised, c(0, 13.5, 40.5, 100)),
   "levels: the share of what is raised that each level settles, and the running total")
pr <- utils::capture.output(print(rec))
ok(any(grepl("Groups for the raising of the discards: 6 strata in 4 groups", pr, fixed = TRUE)) &&
     any(grepl("areaValue > metier4", pr, fixed = TRUE)),
   "print() says what the groups are for and the hierarchy")
ok(any(grepl("Landings 11.3 t: 3.9 t (34.5%) have a discard estimate and are imported as reported; 7.4 t (65.5%) are raised",
             pr, fixed = TRUE)),
   "print() says first what needs no raising at all - the landings with a discard estimate, 3.9 t of 11.3 t - before the levels")
ok(identical(which(grepl("^Landings", pr)) < which(grepl("level", pr))[1], TRUE),
   "... and that comes before the table of levels")

## the hierarchy is the order the variables are written in: the first is the one that matters most
ok(identical(recommend_groups(hc, c("areaValue", "metier4"), list(cov = 30))$hierarchy, hier) &&
     identical(recommend_groups(hc, c(" areaValue ", "metier4"), list(cov = 30))$hierarchy, hier),
   "the hierarchy comes back as it was written (spaces around a name do not matter)")
rr <- recommend_groups(hc, c("metier4", "areaValue"), list(cov = 30))
ok(identical(rr$hierarchy, rev(hier)) &&
     rr$strata$level[rr$strata$areaValue == "A2" & rr$strata$metier4 == "OTB"] == 3L && lv("A2", "OTB") == 4L,
   "the order is the order written: with the metier first A2 | OTB is raised from the OTB of every area (L3), with the area first it falls to All by All (L4)")

## ---- 2. the conditions -------------------------------------------------
## strataCond must share EVERY row of the catch out - the raising leaves out what is in no group and stops on an overlap
member <- sapply(rec$strataCond, function(q) rlang::eval_tidy(q, data = hc))
ok(all(rowSums(member) == 1), "every row of the catch is in exactly one group's strataCond")
ok(identical(unname(which(member[, "L2_1"])), c(1L, 2L, 9L)) && identical(unname(which(member[, "L3_1"])), 3L) &&
     identical(unname(which(member[, "L1"])), c(8L, 12L)),
   "L2_1 holds the rows of A1 | OTB (landings and discards), L3_1 those of A1 | TBB, L1 those of A3 | OTB")
ok(identical(unname(which(member[, "L4"])), c(4L, 5L, 6L, 7L, 10L, 11L)),
   "All by All takes what no narrower group claimed (the complement, not a list)")
mm <- sapply(rec$matchedDataCond, function(q) rep_len(rlang::eval_tidy(q, data = hc), nrow(hc)))   # All by All is just TRUE
ok(identical(unname(which(mm[, "L2_1"])), c(1L, 2L, 9L)) && identical(unname(which(mm[, "L3_1"])), c(1L, 2L, 3L, 9L)) &&
     all(mm[, "L4"]),
   "matchedDataCond: the rows the ratio is taken from - the stratum, its area, all of it - which overlap")
ok(all(vapply(c(rec$strataCond, rec$matchedDataCond), rlang::is_quosure, NA)),
   "the conditions are quosures, as catchRaising() takes them")
# an NA in a hierarchy variable is a value of its own, in the keys and in the conditions
hn <- hc; hn$metier4[hn$areaValue == "A1" & hn$metier4 == "TBB"] <- NA
rn <- recommend_groups(hn, hier, list(cov = 30))
mn <- sapply(rn$strataCond, function(q) rlang::eval_tidy(q, data = hn))
mq <- rep_len(rlang::eval_tidy(rn$matchedDataCond[["L3_1"]], data = hn), nrow(hn))
ok(all(rowSums(mn) == 1) && isTRUE(rn$strata$level[is.na(rn$strata$metier4)] == 3) && identical(unname(which(mq)), c(1L, 2L, 3L, 9L)),
   "an NA in a hierarchy variable is a value of its own: every row still in exactly one group, and the area pool takes the NA stratum")

## ---- 3. the thresholds -------------------------------------------------
r50 <- NULL
w50 <- warn(r50 <- recommend_groups(hc, hier, list(cov = 50)))
ok(grepl("pool of All by All does not reach the thresholds", w50) && grepl("34.5%", w50, fixed = TRUE),
   "when even All by All is below the coverage, the strata fall in it all the same and a warning says so")
ok(identical(sort(unique(r50$strata$level)), c(1L, 2L, 3L, 4L)) &&
     r50$strata$level[r50$strata$areaValue == "A1" & r50$strata$metier4 == "TBB"] == 3L,
   "a pool that is exactly at the coverage reaches it (A1: 50 % >= 50)")
# trips: A1 | OTB has 4, the area pool 4 + 2 = 6 (strata add up); A3 | OTB has nothing to raise
rt <- recommend_groups(hc, hier, list(cov = 30, trips = 5))
ok(rt$strata$level[rt$strata$areaValue == "A1" & rt$strata$metier4 == "OTB"] == 3L &&
     rt$strata$level[rt$strata$areaValue == "A3" & rt$strata$metier4 == "OTB"] == 1L,
   "trips: A1 | OTB (4 trips) goes to its area (6 trips); A3 | OTB has nothing to raise whatever its trips")
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
ok(lv0("A2", "OTB") == 4L && lv0("A1", "TBB") == 3L && lv0("A3", "TBB") == 3L && lv0("A1", "OTB") == 2L,
   "cov = 0 does not take a pool with nothing sampled in it (A2 | OTB goes to All by All; the others to the narrowest pool with a sample)")
ok(identical(suppressWarnings(recommend_groups(hc, hier))$thresholds$cov, 50),
   "the default coverage is 50")

## ---- 3b. the stratum as reported: when the hierarchy is the reported stratum, L1 is the own stratum ---------------
## the same catch with the four variables of a reported stratum, all of them in the hierarchy: nothing to aggregate
rp <- hc; rp$fleetValue <- paste0(hc$metier4, "_DEF"); rp$seasonValue <- 1L
hier4 <- c("areaValue", "fleetValue", "vesselFlagCountry", "seasonValue")
r4 <- recommend_groups(rp, hier4, list(cov = 30))
ok(identical(r4$levels$level, 1:5) &&
     identical(r4$levels$label, c("own stratum (raw)", "pooled across seasonValue",
                                  "pooled across vesselFlagCountry and seasonValue",
                                  "pooled across fleetValue, vesselFlagCountry and seasonValue", "All by All")),
   "the hierarchy is the reported stratum: no level of its own for the aggregated stratum, L1 is the own stratum and the numbers close up")
l4 <- function(a, m) r4$strata$level[r4$strata$areaValue == a & r4$strata$fleetValue == paste0(m, "_DEF")]
ok(l4("A1", "OTB") == 1L && l4("A3", "OTB") == 1L && l4("A1", "TBB") == 4L && l4("A2", "OTB") == 5L,
   "... a stratum that reaches the thresholds on its own is L1 (as are those with nothing to raise), the area pool is L4, All by All L5")
ok(identical(names(r4$strataCond), c("L1", "L1_1", "L4_1", "L5")),
   "... L1 is the group of the strata with nothing to raise, L1_1 the own stratum that raises from itself")
m4 <- sapply(r4$strataCond, function(q) rlang::eval_tidy(q, data = rp))
ok(all(rowSums(m4) == 1), "... and every row is still in exactly one group")
ok(near(r4$levels$covered_t[1], 3.9) && near(r4$levels$raised_t[1], 1),
   "... L1 holds both what has an estimate (3.9 t) and what is raised from the own stratum (1 t)")

## ---- 4. what is refused ------------------------------------------------
ok(grepl("no column 'nope'", err(recommend_groups(hc, c("areaValue", "nope")))),
   "a hierarchy variable that is not in the catch is named")
ok(grepl("names the variables", err(recommend_groups(hc, character()))) &&
     grepl("names the variables", err(recommend_groups(hc, ""))),
   "an empty hierarchy is refused")
ok(grepl("names the variables", err(recommend_groups(hc, c("areaValue", "")))) &&
     grepl("names the variables", err(recommend_groups(hc, c("areaValue", NA)))),
   "an empty or missing name in the hierarchy is refused")
ok(grepl("'areaValue' more than once", err(recommend_groups(hc, c("areaValue", "metier4", "areaValue")))),
   "a variable named twice is refused")
ok(grepl("name the variables one by one", err(recommend_groups(hc, "areaValue, metier4"))) &&
     grepl("name the variables one by one", err(recommend_groups(hc, "areaValue > metier4"))),
   "one text with the variables in it (commas, >) is pointed to the vector of names")
ok(grepl("drop_catchCategory", err(recommend_groups(hc, c("areaValue", "catchCategory")))),
   "catchCategory is not one of the variables of the hierarchy: the error points to drop_catchCategory")
ok(grepl("TRUE or FALSE", err(recommend_groups(hc, hier, drop_catchCategory = NA))) &&
     grepl("TRUE or FALSE", err(recommend_groups(hc, hier, drop_catchCategory = "yes"))),
   "drop_catchCategory is TRUE or FALSE")
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
ok(identical(names(recommend_groups(hc, hier, list(cov = 30), drop_catchCategory = FALSE)$strataCond),
             names(rec$strataCond)),
   "drop_catchCategory has no meaning for discards: the same groups either way")

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
ok(identical(sort(unique(raised$DrGroup[raised$areaValue == "A1" & raised$metier4 == "TBB"])), "L3_1") &&
     identical(sort(unique(raised$DrGroup[raised$areaValue == "A3" & raised$metier4 == "OTB"])), "L1"),
   "and A1 | TBB was raised in the group of its area, A3 | OTB sits in the group of what needs no raising")

## a landing of no weight and no estimate, in a stratum of its own, is nothing to raise (L1) and still needs a ratio to be multiplied with:
## the group of what needs no raising draws on everything, or its zero would come out as 0 x NaN
ecz <- rbind(ec, transform(ec[1, ], areaValue = "A9", total = 0, domainCatchDis = NA_character_))
rz <- recommend_groups(ecz, "areaValue", list(cov = 30))
raised_z <- as.data.frame(quiet(catchRaising(ecz, rz$strataCond, rz$matchedDataCond, type = "discards",
                                             originType = "WGEstimate", variableType = "WeightLive",
                                             logFile = tempfile(fileext = ".txt"), verbose = FALSE)))
ok(identical(rz$strata$level[rz$strata$areaValue == "A9"], 1L) && identical(unname(rz$strata$group[rz$strata$areaValue == "A9"]), "L1") &&
     all(rep_len(rlang::eval_tidy(rz$matchedDataCond[["L1"]], data = ecz), nrow(ecz))),
   "the group of what needs no raising draws on everything")
ok(!any(is.nan(raised_z$total)) && sum(raised_z$importedOrRaised == "raised") == 6,
   "a landing of no weight and no estimate gets a ratio all the same: no NaN (six landings raised, the zero one among them)")

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
ok(near(sum(rec_f$levels$weight_t), sum(rec_f$strata$weight_t)) &&
     near(sum(rec_f$levels$raised_t) + rec_f$levels$covered_t[1], sum(rec_f$strata$weight_t)),
   "fixture: the levels account for all the landings, those with an estimate and those raised")

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
w_len <- warn(recommend_groups(al$catch1, lh, list(cov = 50, fish = 1e9), distribution_data = dl, drop_catchCategory = FALSE))
ok(grepl("Lan: coverage", w_len) && grepl("Dis: coverage", w_len) && grepl(" fish)", w_len, fixed = TRUE) &&
     grepl("allocated from it all the same", w_len),
   "lengths: when even All by All falls short, the warning names the catch part and what it lacks")
ok(all(grepl("^(Lan|Dis)_L[0-9]", names(rl$strataCond)[nzchar(rl$groups$category)])) &&
     all(grepl("^L[0-9]+$", names(rl$strataCond)[!nzchar(rl$groups$category)])),
   "lengths: groups are named after their catch part, then the level (Lan_L2_1 ...); the All by All of both parts has only the level")
ok(all(grepl("catchCategory", vapply(rl$strataCond, rlang::quo_text, ""))),
   "lengths: the conditions say which catch part (landings and discards are grouped on their own)")
ok(all(rl$strata$numMeasurements >= 0) && "pool_fish" %in% names(rl$strata) && near(rl$thresholds$fish, 100),
   "lengths: the measurements behind each stratum and pool are in the table")
last_l <- max(rl$levels$level)
ok(any(rl$strata$level > 1 & rl$strata$level < last_l - 1) &&
     all(rl$strata$pool_fish[rl$strata$raised_t > 0 & rl$strata$level < last_l - 1] >= 100),
   "lengths: a pool that is not an All by All has at least the fish asked for")
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

## ---- 6b. the catch part: the last variable to be pooled across, or never ---------------------------------------
## nothing reaches 1e9 fish: every stratum falls to the last level of its kind
nope <- list(cov = 50, fish = 1e9)
r_t <- r_f2 <- NULL
w_t <- warn(r_t  <- recommend_groups(al$catch1, lh, nope, distribution_data = dl))
w_f <- warn(r_f2 <- recommend_groups(al$catch1, lh, nope, distribution_data = dl, drop_catchCategory = FALSE))
ok(identical(r_t$levels$label[r_t$levels$category == "Lan"],
             c("own stratum (raw)", "own stratum (aggregated)", "pooled across metier4",
               "All by All (per catch part)", "All by All")) &&
     identical(r_f2$levels$label[r_f2$levels$category == "Lan"],
               c("own stratum (raw)", "own stratum (aggregated)", "pooled across metier4",
                 "All by All (per catch part)")),
   "lengths: the catch part is the last to be pooled across (a level for the pool of the catch part, then All by All) - or never (drop_catchCategory = FALSE: the last level keeps it)")
ok(isTRUE(r_t$drop_catchCategory) && identical(r_f2$drop_catchCategory, FALSE),
   "... and the result says which")
ok(grepl("pool of All by All does not reach the thresholds", w_t) && !grepl("Lan: coverage", w_t) &&
     grepl("Lan: coverage", w_f) && grepl("Dis: coverage", w_f),
   "... the warning of drop_catchCategory = TRUE speaks of the All by All of both parts, that of FALSE of each catch part")
nt_need <- r_t$strata$raised_t > 0
ok(all(r_t$strata$level[nt_need] == max(r_t$levels$level)) &&
     all(r_f2$strata$level[r_f2$strata$raised_t > 0] == max(r_f2$levels$level)),
   "... when nothing reaches the thresholds every stratum that needs it falls to the last level")
ok(sum(grepl("^L[0-9]+$", names(r_t$strataCond)[r_t$groups$level == max(r_t$levels$level)])) == 1L &&
     sum(r_f2$groups$level == max(r_f2$levels$level)) == 2L,
   "... one All by All of landings and discards together (drop_catchCategory = TRUE) or one for each catch part (FALSE)")
mt <- sapply(r_t$strataCond, function(q) rep_len(rlang::eval_tidy(q, data = al$catch1), nrow(al$catch1)))
mf <- sapply(r_f2$strataCond, function(q) rep_len(rlang::eval_tidy(q, data = al$catch1), nrow(al$catch1)))
inl <- al$catch1$catchCategory %in% c("Lan", "Dis")
ok(all(rowSums(mt)[inl] == 1) && all(rowSums(mf)[inl] == 1) && all(rowSums(mt)[!inl] == 0) && all(rowSums(mf)[!inl] == 0),
   "... either way every landing and discard row is in exactly one group, and the others (BMS) in none")
ok(all(grepl("catchCategory ==", vapply(r_f2$strataCond, rlang::quo_text, ""), fixed = TRUE)),
   "drop_catchCategory = FALSE: every group is made within one catch part")
a_t <- quiet(catchAllocationAoL(catch_data = al$catch1, distribution_data = dl,
                                condition_alloc_st_list = r_t$strataCond, condition_matched_data_list = r_t$matchedDataCond,
                                distributionType = "LengthTotal", verbose = FALSE, logFile = tempfile(fileext = ".txt"),
                                assembled_output = TRUE))
ok(is.list(a_t) && nrow(a_t$distribution) > 0 &&
     near(sum(a_t$catch$total[a_t$catch$catchCategory == "Lan"], na.rm = TRUE),
          sum(al$catch1$total[al$catch1$catchCategory == "Lan"], na.rm = TRUE)),
   "drop_catchCategory = TRUE: the allocation runs on a group of landings and discards together")

## ---- 7. looking at the levels ------------------------------------------
pg <- list(plot_recommended_groups(rec), plot_recommended_groups(rec, x = "areaValue"),
           plot_recommended_groups(rec, x = "metier4", share = TRUE),
           plot_recommended_groups(rl), plot_river(rec, left_var = "areaValue"),
           plot_recommended_groups(rec, share = TRUE), plot_recommended_groups(rl, x = "areaValue"),
           plot_recommended_groups(rl, share = TRUE))
ok(all(vapply(pg, inherits, NA, what = "ggplot")), "plot_recommended_groups() and plot_river() draw a recommendation")
built <- tryCatch({ lapply(pg, ggplot2::ggplot_build); TRUE }, error = function(e) conditionMessage(e))
ok(isTRUE(built), "... and they build")
txt <- unlist(lapply(ggplot2::ggplot_build(pg[[1]])$data, function(d) if ("label" %in% names(d)) as.character(d$label)))
ok(setequal(txt, c("3 strata", "1 stratum")) && sum(txt == "3 strata") == 2L,
   "the levels plot says how many strata each bar holds (3 with an estimate of their own, 1, 1 and 3 in the small catch), in the singular for one")
ok(identical(levels(pg[[1]]$data$part), c("Imported", "Raised")) && near(sum(pg[[1]]$data$t), 11.3) &&
     near(sum(pg[[1]]$data$t[pg[[1]]$data$part == "Imported"]), 3.9),
   "the levels plot is the landings: 3.9 t imported (solid) and 7.4 t raised (hatched), 11.3 t")
ok(grepl("Discard raising groups", pg[[1]]$labels$title, fixed = TRUE) && grepl("landings", pg[[1]]$labels$title, fixed = TRUE) &&
     grepl("Length allocation groups", pg[[4]]$labels$title, fixed = TRUE) &&
     grepl("Age allocation groups", plot_recommended_groups(ra)$labels$title, fixed = TRUE),
   "the title says what is grouped - the discard raising, the length allocation, the age allocation - and it is not an argument")
ok(!("title" %in% names(formals(plot_recommended_groups))),
   "plot_recommended_groups() has no title argument")
ok(near(sum(pg[[6]]$data$v), 100) && near(sum(pg[[1]]$data$v), 11.3) && grepl("%", pg[[6]]$labels$x, fixed = TRUE),
   "share = TRUE works without x: the bars are percent of the landings (100 in all), not tonnes")
ok(nlevels(pg[[4]]$data$panel) == 2L && identical(levels(pg[[4]]$data$panel), c("Landings", "Discards")) &&
     near(as.numeric(tapply(pg[[8]]$data$v, pg[[8]]$data$panel, sum)), c(100, 100)),
   "lengths: landings and discards are two panels, each 100 % with share = TRUE")
ok(inherits(pg[[3]]$layers[[1]]$geom, "GeomColHatch") && near(sum(pg[[3]]$data$t), 11.3),
   "with x the bars are stacked by level and hatched where raised, in tonnes or (share = TRUE) bars of 100 %")
ok(grepl("by areaValue", pg[[2]]$labels$title, fixed = TRUE),
   "with x the title says by what")
ok(grepl("Which areaValue is raised at which level", pg[[5]]$labels$title, fixed = TRUE) &&
     near(sum(pg[[5]]$data$w), 7.4),
   "plot_river() on a recommendation: which stratum is raised at which level (the 7.4 t that are raised)")
f <- tempfile(fileext = ".png")
invisible(plot_recommended_groups(rec, save = f))
ok(file.exists(f) && file.size(f) > 1000, "plot_recommended_groups(save =) writes the figure")
ok(grepl("recommend_groups", err(plot_recommended_groups(hc)), ignore.case = TRUE),
   "plot_recommended_groups() on anything else points to recommend_groups()")
ok(grepl("`facet` needs `x`", err(plot_recommended_groups(rec, facet = "areaValue")), fixed = TRUE) &&
     grepl("no column 'nope'", err(plot_recommended_groups(rec, x = "nope"))) &&
     grepl("TRUE or FALSE", err(plot_recommended_groups(rec, share = "yes"))),
   "its arguments are checked: facet needs x, x is a column of the strata, share is TRUE or FALSE")
ok(!exists("plot_groups", mode = "function"), "plot_groups() is gone: it is plot_recommended_groups()")

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
