## Landings / discards thresholds, and zero-weight strata.
## Run after pkgload::load_all(".") or with the package installed.
FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}

## --- spec normalisation, all three accepted shapes -------------------
ok(identical(as_cc_spec(50), list(Lan = 50, Dis = 50)),
   "a bare number applies to both categories")
ok(identical(as_cc_spec(c(50, 40))$Dis, c(50, 40)),
   "a bare vector applies to both categories")
ok(identical(unname(cc_step_value(list(Lan = c(50, 40), Dis = c(30, 20)), 2)),
             c(40, 20)),
   "a Lan/Dis list is read per rung")
ok(identical(unname(cc_step_value(list(Lan = c(50, 40), Dis = c(30, 20)), 9)),
             c(40, 20)),
   "a rung past the end of the spec reuses the last value")
ok(identical(cc_bracket(c("Lan", "Dis", "BMS")), c("Lan", "Dis", "Dis")),
   "BMS is bracketed with discards")

## --- the steps carry a pair ------------------------------------------
st <- alloc_steps("metier4", thresholds = list(Lan = c(60, 50), Dis = c(30, 25)),
                  keep_cc = TRUE, min_meas = list(Lan = c(100, 50), Dis = c(20, 10)))
ok(identical(unname(st[[1]]$threshold), c(60, 30)), "rung 1 carries both bars")
ok(identical(unname(st[[1]]$min_meas), c(100, 20)), "rung 1 carries both counts")

st_old <- alloc_steps("metier4", c(50, 40))
ok(identical(unname(st_old[[1]]$threshold), c(50, 50)),
   "a plain vector still works (old saved scenarios)")

## --- the cascade reads the bar off the stratum, not the pool ---------
bl <- data.frame(
  catchCategory   = c("Lan", "Dis", "Lan", "Dis"),
  metier4         = c("TBB", "TBB", "OTB", "OTB"),
  weight          = c(100,  50,  100,   0),
  weight_measured = c( 45,  20,   90,   0),
  coverage        = c( 45,  40,   90,  NA),
  n_measurements  = c(300, 120,  900,   0),
  stringsAsFactors = FALSE)

a <- assign_alloc_cascade(bl, "metier4", st)

ok(!identical(a$alloc_step[1], 1L),
   "45% landings does NOT clear the 60% own-stratum landings bar")
ok(identical(a$alloc_step[2], 1L),
   "40% discards DOES clear the 30% discard bar - the point of the split")
ok(identical(a$alloc_step[3], 1L), "90% landings clears its own bar")

## --- the measurement bar bites too -----------------------------------
st_m <- alloc_steps("metier4", thresholds = list(Lan = 10, Dis = 10),
                    keep_cc = TRUE, min_meas = list(Lan = 500, Dis = 500))
am <- assign_alloc_cascade(bl, "metier4", st_m)
ok(!identical(am$alloc_step[1], 1L),
   "coverage alone is not enough when the measurement count is short")
ok(identical(am$alloc_step[3], 1L),
   "900 measurements clears a 500 bar")

## --- zero weight is not an unresolved stratum ------------------------
ok(isTRUE(a$alloc_empty[4]), "a zero-weight stratum is flagged empty")
ok(identical(a$alloc_level[4], "no catch to allocate"),
   "a zero-weight stratum is not called 'not allocated'")
tb <- alloc_cascade_table(a, st)
ok(!any(is.na(tb$step) & tb$pool == "not allocated" &
        tb$n_strata > sum(is.na(a$alloc_step) & !a$alloc_empty)),
   "the unresolved row excludes the zero-weight strata")
ok(any(grepl("no catch", tb$pool)), "zero-weight strata get their own row")

## --- add_sufficiency takes a pair, and still takes a scalar ----------
sf <- add_sufficiency(bl, NULL, min_coverage = list(Lan = 60, Dis = 30),
                      min_measurements = 0)
ok(identical(sf$ok_coverage, c(FALSE, TRUE, TRUE, FALSE)),
   "add_sufficiency applies the pair per row")
sf2 <- add_sufficiency(bl, NULL, min_coverage = 60, min_measurements = 0)
ok(identical(sf2$ok_coverage, c(FALSE, FALSE, TRUE, FALSE)),
   "add_sufficiency still accepts a single number")

## --- the emitted code carries the pair -------------------------------
ok(identical(fmt_cc_spec(list(Lan = c(50, 40), Dis = c(30, 20))),
             "list(Lan = c(50, 40), Dis = c(30, 20))"),
   "fmt_cc_spec round-trips a pair")

## --- the last-resort rung ---------------------------------------------
##
## Every rung above has a bar, so a stratum thinner than all of them ends
## unallocated - and "unallocated" is not neutral downstream: the weight
## never reaches the numbers at length and the totals quietly stop adding
## up. The ladder therefore ends with one rung that has no bar.
lt <- alloc_steps("Country", thresholds = list(Lan = 60, Dis = 30),
                  keep_cc = TRUE)
ok(identical(lt[[length(lt)]]$id, "last"), "the ladder ends with a last rung")
ok(all(is.infinite(lt[[length(lt)]]$threshold)),
   "the last rung has no coverage bar")
ok(all(lt[[length(lt)]]$min_meas == 0), "and no measurement bar")
ok(!isTRUE(lt[[length(lt)]]$drop_cc),
   "with keep_cc it pools within the category - discards never take landings")

lf <- alloc_steps("Country", thresholds = list(Lan = 60, Dis = 30),
                  keep_cc = FALSE)
ok(isTRUE(lf[[length(lf)]]$drop_cc),
   "without keep_cc the last rung pools both categories")
ok(sum(vapply(lf, function(x) identical(x$id, "last"), logical(1))) == 1,
   "exactly one last rung, whichever way keep_cc is set")

bl2 <- data.frame(
  catchCategory   = c("Lan", "Dis", "Dis"),
  Country         = c("BE", "BE", "NL"),
  weight          = c(100, 50, 40),
  weight_measured = c(90, 2, 1),
  n_measurements  = c(900, 12, 6),
  stringsAsFactors = FALSE)
a2 <- assign_alloc_cascade(bl2, "Country", lt)
ok(a2$alloc_step[1] == 1, "well-sampled landings still resolve on their own data")
ok(all(!is.na(a2$alloc_step)), "nothing is left unallocated")
ok(all(a2$alloc_level[2:3] == lt[[length(lt)]]$label),
   "the thin discard strata fall to the last rung, not off the end")

## "no threshold" is not "no data"
bl3 <- data.frame(catchCategory = "Dis", Country = "BE",
                  weight = 10, weight_measured = 0, n_measurements = 0,
                  stringsAsFactors = FALSE)
a3 <- assign_alloc_cascade(bl3, "Country", lt)
ok(is.na(a3$alloc_step[1]),
   "a pool with nothing measured in it does NOT pass the last rung")

## --- the drop must be positional -------------------------------------
##
## alloc_steps() is called with the ROLE names the user picked ("Fleet"),
## assign_alloc_cascade() with the COLUMN names those map to
## ("metier4"). Matching the drop by NAME therefore dropped nothing
## whenever the two differed, so every rung silently stayed at full
## precision - and a last rung with no threshold could still leave
## strata unresolved, which is what gave it away.
roles <- c("Fleet", "Country"); cols <- c("metier4", "Country")
sp <- alloc_steps(roles, thresholds = list(Lan = 50, Dis = 20), keep_cc = TRUE)
ok(isTRUE(sp[[length(sp)]]$drop_all),
   "the last rung is flagged drop_all, not given a list of names")
ok(sp[[2]]$drop_n == 1, "each rung records HOW MANY keys it drops")

set.seed(2)
b <- expand.grid(catchCategory = c("Lan", "Dis"),
                 metier4 = c("TBB", "OTB", "GNS"),
                 Country = c("BE", "NL", "DE"), stringsAsFactors = FALSE)
b$weight <- runif(nrow(b), 5, 200)
b$weight_measured <- ifelse(b$catchCategory == "Lan", b$weight * .8,
                            b$weight * .02)
b$n_measurements <- ifelse(b$catchCategory == "Lan", 500, 5)
ac <- assign_alloc_cascade(b, cols, sp)
ok(sum(is.na(ac$alloc_step) & !ac$alloc_empty) == 0,
   "with role names differing from column names, the last rung still resolves everything")

## --- donor bars are a DIFFERENT question from rung thresholds --------
sd <- alloc_steps(roles, thresholds = list(Lan = 50, Dis = 20), keep_cc = TRUE,
                  donor_cov_min = 50, donor_meas_min = 100)
ok(!is.na(sd[[1]]$donor_cov_min), "the bars are carried on every rung")
ad <- assign_alloc_cascade(b, cols, sd)
ok(sum(is.na(ad$alloc_step) & !ad$alloc_empty) > 0,
   "excluding every discard donor does leave discards unresolved - honest, not fudged")
ok(any(grepl("donor-pool bars", na.omit(ad$alloc_reason))),
   "and the reason says it was the donor bars, not the rung thresholds")

## a stratum failing the donor bars is still ALLOCATED, just not used
lan <- ac$catchCategory == "Lan"
ok(all(!is.na(ac$alloc_step[lan])), "landings all resolve")

tbl <- alloc_cascade_table(ad, sd)
ok(all(c("donor_cov_min", "donor_meas_min") %in% names(tbl)),
   "the cascade table shows the donor bars beside the thresholds")

## --- the threshold follows the FINEST KEY STILL POOLED --------------
##
## Keys are read in the order picked and dropped in that SAME order:
## the first key picked is the first one pooled across, the last key
## picked is kept the longest. This was reversed before - the first key
## picked was retained longest and the LAST one was dropped first, the
## opposite of "read top to bottom, widen top to bottom" - so a
## threshold set on the box the reader read as "last, coarsest" was
## actually driving the FIRST widening step, and vice versa.
kk <- c("Fleet", "Country", "Season", "Area")   # picked in this order
st <- alloc_steps(kk, thresholds = list(Lan = c(50, 60, 70, 80),
                                        Dis = c(50, 60, 70, 80)),
                  keep_cc = TRUE)
lbl <- vapply(st, `[[`, "", "label")
thr <- vapply(st, function(x) unname(x$threshold["Lan"]), numeric(1))
own <- which(vapply(st, function(x) identical(x$id, "own"), logical(1)))
ok(thr[own] == 80,
   "own stratum (every key kept) takes the last key's (Area's) bar")
## Fleet, picked first, is dropped first: the next rung keeps
## Country+Season+Area and takes Country's bar (the new finest key)
w1 <- which(grepl("Country \\+ Season \\+ Area", lbl))[1]
ok(!is.na(w1) && thr[w1] == 60,
   "dropping Fleet first, the pool down to Country+Season+Area takes Country's bar")
## Area, picked last, is kept until only it remains, and takes its own bar
w_area <- which(lbl == "catchCategory + Area")
ok(length(w_area) == 1 && thr[w_area] == 80,
   "the pool keeping only Area (dropped last) takes Area's bar")

## an explicit own bar overrides the finest-key default
st2 <- alloc_steps(kk, thresholds = list(own = c(Lan = 55, Dis = 55),
                                         Lan = c(50, 60, 70, 80),
                                         Dis = c(50, 60, 70, 80)),
                   keep_cc = TRUE)
ok(unname(st2[[own]]$threshold["Lan"]) == 55,
   "a spec carrying $own puts that bar on the own rung, not Area's")

## the grouping column names what each rung keeps, in the same
## "same X, Y - pooled across all Z" prose the discard cascade uses -
## not the "X + Y" join the pool LABEL uses, which is a different column
## with a different job (naming the pool, not describing it)
bl <- data.frame(catchCategory = "Lan", Fleet = "TBB", Country = "BE",
                 Season = 1, Area = "27.4", weight = 100,
                 weight_measured = 90, n_measurements = 500,
                 stringsAsFactors = FALSE)
a <- assign_alloc_cascade(bl, kk, st)
tb <- alloc_cascade_table(a, st, keys = kk)
ok("grouping" %in% names(tb), "the cascade table has a grouping column")
ok(tb$grouping[tb$step == 1] == "the stratum itself",
   "the own rung groups by the stratum itself")
ok(any(grepl("same Country, Season and Area", tb$grouping, fixed = TRUE)),
   "a widened rung names the keys it still distinguishes")

## keys and dims are NOT the same thing, and conflating them was the bug:
## a column in dims but not in keys (never chosen as a cascade key) has
## to show up as "pooled across", or the summary understates how much a
## rung actually collapses. Fleet/Country/Season/Area are the cascade's
## own keys; Stratum is part of the strata definition but was never
## offered as a key.
tb2 <- alloc_cascade_table(a, st, keys = kk, dims = c("Stratum", kk))
w1 <- tb2$grouping[!tb2$grouping %in%
  c("the stratum itself", "everything pooled", "all data in the category")][1]
ok(grepl("pooled across all Stratum", w1, fixed = TRUE) ||
     grepl("pooled across all .*Stratum", w1),
   "a dim that is not a cascade key still shows as pooled across")
ok(!any(grepl("catchCategory", tb2$grouping, fixed = TRUE)),
   "catchCategory never appears in grouping text - scope already says so")

## keys = NULL (the pre-fix caller shape) must not error
tb3 <- alloc_cascade_table(a, st)
ok(is.data.frame(tb3) && "grouping" %in% names(tb3),
   "omitting keys/dims still produces a table, using the steps' own keys")

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
