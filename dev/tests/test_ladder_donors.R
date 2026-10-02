## The ladder: what widens, whose data is measured against a threshold,
## and what the last rung is allowed to draw on.
FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}

## Only one stratum in the whole table has measurements. Everything else
## has catch and nothing measured, so nothing can resolve on its own.
mk <- function() {
  bl <- expand.grid(metier4 = c("TBB", "OTB", "GNS"), seasonValue = 1:4,
                    catchCategory = "Dis", stringsAsFactors = FALSE)
  bl$weight <- 10; bl$weight_measured <- 0; bl$n_measurements <- 0
  bl$weight_measured[1] <- 9; bl$n_measurements[1] <- 500   # TBB | 1
  bl$weight_measured[2] <- 8; bl$n_measurements[2] <- 400   # OTB | 1
  bl
}
bl <- mk()
th <- list(Lan = 50, Dis = 50)
cols <- c("metier4", "seasonValue")

## --- the rungs must actually widen ------------------------------------
##
## alloc_steps() is called with the ROLE names the user picked ("Fleet")
## and assign_alloc_cascade() with the COLUMNS they map to ("metier4").
## Dropping keys by NAME therefore dropped nothing whenever the two
## differed, every rung silently stayed at full precision, and a rung
## with no threshold at all could still leave strata unresolved.
a <- assign_alloc_cascade(bl, cols, alloc_steps(c("Fleet", "seasonValue"), th))
ok(sum(is.na(a$alloc_step) & !a$alloc_empty) == 0,
   "role names differing from column names still widens the ladder")
ok(identical(a$alloc_step[1], 1L), "the measured stratum resolves on its own data")
ok(all(a$alloc_step[3:12] > 1L), "the rest resolve further up, not at rung 1")

## --- the threshold measures the DONOR POOL, not the target ------------
##
## Every stratum below has 0% coverage of its own. If the bar were read
## against the target none of them could ever resolve, whatever the pool
## contained.
ok(all(!is.na(a$alloc_step[3:12])),
   "a stratum with no coverage of its own is still allocated to")

## --- the last rung has no bar, so nothing is left behind --------------
last <- alloc_steps(c("Fleet", "seasonValue"), th)
last <- last[[length(last)]]
ok(all(is.infinite(last$threshold)), "the last rung has no rung threshold")
ok(isTRUE(last$drop_all) || last$drop_n >= 2,
   "and pools across every key")

## --- donor bars are a separate question ------------------------------
##
## They say who may DONATE. A stratum failing them is dropped from the
## pool sums; it is still allocated to from whatever is left.
st_b <- alloc_steps(c("Fleet", "seasonValue"), th,
                    donor_cov_min = 50, donor_meas_min = 100)
ab <- assign_alloc_cascade(bl, cols, st_b)
ok(sum(is.na(ab$alloc_step) & !ab$alloc_empty) == 0,
   "with two donors clearing the bars everything is allocated")

bl3 <- bl; bl3$n_measurements[1:2] <- 10          # both donors too thin
a3 <- assign_alloc_cascade(bl3, cols, st_b)
ok(sum(is.na(a3$alloc_step) & !a3$alloc_empty) == nrow(bl3),
   "bars strict enough to empty the pool leave everything unallocated")
ok(any(grepl("donor-pool bars", na.omit(a3$alloc_reason))),
   "and the reason says it was the bars, not missing data")

bl4 <- bl; bl4$weight_measured <- 0; bl4$n_measurements <- 0
a4 <- assign_alloc_cascade(bl4, cols, alloc_steps(c("Fleet", "seasonValue"), th))
ok(any(grepl("no measured data", na.omit(a4$alloc_reason))),
   "with nothing measured anywhere the reason says so instead")

## --- naming donors out by hand, like the discard catch-all ------------
st_x <- alloc_steps(c("Fleet", "seasonValue"), th, donor_excluded = "TBB | 1")
ax <- assign_alloc_cascade(bl, cols, st_x)
ok(sum(is.na(ax$alloc_step) & !ax$alloc_empty) == 0,
   "excluding one donor by name still leaves the other to draw on")

st_x2 <- alloc_steps(c("Fleet", "seasonValue"), th,
                     donor_excluded = c("TBB | 1", "OTB | 1"))
ax2 <- assign_alloc_cascade(bl, cols, st_x2)
ok(sum(is.na(ax2$alloc_step) & !ax2$alloc_empty) == nrow(bl),
   "excluding every donor by name empties the pool")
ok(any(grepl("donor-pool bars", na.omit(ax2$alloc_reason))),
   "and that is reported as a bars problem, which it is")

## --- the case that motivated the last rung ---------------------------
##
## Discards where NO pool at any rung can ever meet the bar: overall
## discard coverage is 6%, the bar is 20%. Rungs 2-4 must resolve
## nothing, and the last rung must pick up everything that is left.
## Before this existed, that weight simply never reached the numbers at
## length and the totals quietly stopped adding up.
b2 <- expand.grid(metier4 = c("TBB", "OTB", "GNS"), vesselFlagCountry = c("BE", "NL"),
                  seasonValue = 1:4, catchCategory = "Dis", stringsAsFactors = FALSE)
b2$weight <- 100; b2$weight_measured <- 0; b2$n_measurements <- 0
b2$weight_measured[1:3] <- c(60, 50, 40)
b2$n_measurements[1:3]  <- c(300, 250, 200)
c2 <- c("metier4", "vesselFlagCountry", "seasonValue")
s2 <- alloc_steps(c("Fleet", "vesselFlagCountry", "seasonValue"),
                  list(Lan = 50, Dis = 20), keep_cc = TRUE)
a2 <- assign_alloc_cascade(b2, c2, s2)
tb <- alloc_cascade_table(a2, s2)

ok(sum(b2$weight_measured) / sum(b2$weight) * 100 < 20,
   "the fixture really is below the bar everywhere")
ok(all(tb$n_strata[tb$step %in% 2:4] == 0),
   "no rung with a threshold resolves anything - none can")
ok(tb$n_strata[tb$step == length(s2)] == 21,
   "the last rung picks up every remaining stratum")
ok(sum(is.na(a2$alloc_step) & !a2$alloc_empty) == 0, "nothing is left behind")
ok(all(a2$alloc_step[1:3] == 1L),
   "and the strata that DID meet the bar still resolved on their own data")

## --- keep_cc decides what the last rung is allowed to pool ------------
b3 <- expand.grid(metier4 = c("TBB", "OTB", "GNS"), vesselFlagCountry = c("BE", "NL"),
                  seasonValue = 1:4, catchCategory = c("Lan", "Dis"),
                  stringsAsFactors = FALSE)
b3$weight <- 100; b3$weight_measured <- 0; b3$n_measurements <- 0
lan <- b3$catchCategory == "Lan"
b3$weight_measured[lan] <- 80; b3$n_measurements[lan] <- 900
dis3 <- which(!lan)[1:3]
b3$weight_measured[dis3] <- c(60, 50, 40); b3$n_measurements[dis3] <- c(300, 250, 200)

sk <- alloc_steps(c("Fleet", "vesselFlagCountry", "seasonValue"), list(Lan = 50, Dis = 20),
                  keep_cc = TRUE)
ak <- assign_alloc_cascade(b3, c2, sk)
on_last <- !is.na(ak$alloc_step) & ak$alloc_step == length(sk)
ok(sum(is.na(ak$alloc_step) & !ak$alloc_empty) == 0,
   "keep_cc = TRUE: nothing unallocated")
ok(identical(unique(ak$catchCategory[on_last]), "Dis"),
   "and the last rung is reached only by discards")
ok(all(ak$alloc_step[lan] < length(sk)),
   "landings never touch it - they met the bar on their own")

## With the category droppable the joined rungs resolve the discards
## first, so the last resort is never needed. That is the point: it is
## the last resort, not the first thing tried after the category goes.
sf <- alloc_steps(c("Fleet", "vesselFlagCountry", "seasonValue"), list(Lan = 50, Dis = 20),
                  keep_cc = FALSE)
af <- assign_alloc_cascade(b3, c2, sf)
ok(sum(is.na(af$alloc_step) & !af$alloc_empty) == 0,
   "keep_cc = FALSE: nothing unallocated either")
ok(!any(!is.na(af$alloc_step) & af$alloc_step == length(sf)),
   "and the last rung is not needed at all - the joined rungs suffice")

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
