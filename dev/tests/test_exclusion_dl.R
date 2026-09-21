## D/L (discards / landings) next to the discard rate in the exclusion
## lists. DR divides by landings PLUS discards, D/L by landings alone; some
## people read one more naturally than the other, so both are offered and
## neither replaces the other.

FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}

s <- data.frame(
  metier4  = c("OTB", "OTB", "SDN", "TBB"),
  Country  = c("BE",  "FR",  "BE",  "BE"),
  landings = c(100,   50,    40,    10),
  discards = c(10,    5,     20,    0),
  coverage = c(80,    60,    90,    100),
  stringsAsFactors = FALSE)
s$discard_rate <- s$discards / (s$landings + s$discards)

## ---- the metric --------------------------------------------------------
m <- stratum_metrics(s, "metier4")
ok("dl_ratio" %in% names(m), "stratum_metrics() carries dl_ratio")
ok(isTRUE(all.equal(m$dl_ratio[m$key == "SDN"], 20 / 40)),
   "a stratum's D/L is its discards over its landings")
## OTB is two rows pooled: a ratio of SUMS (15 / 150), not a mean of the
## two per-row ratios (0.1 and 0.1 happen to agree, so change one)
s2 <- s; s2$discards[s2$Country == "FR"] <- 25
m2 <- stratum_metrics(s2, "metier4")
ok(isTRUE(all.equal(m2$dl_ratio[m2$key == "OTB"], (10 + 25) / (100 + 50))),
   "pooled rows give sum(discards) / sum(landings), not a mean of ratios")
ok(isTRUE(all.equal(m$dl_ratio[m$key == "TBB"], 0)),
   "no discards on real landings is a D/L of 0, not missing")
ok(is.na(stratum_metrics(transform(s, landings = 0), "metier4")$dl_ratio[1]),
   "no landings has no D/L")

## a biology link has no discards, so no D/L - and no D/L in the label
bio <- data.frame(metier4 = c("OTB", "SDN"), weight = c(5, 6), coverage = c(50, 60))
ok(all(is.na(stratum_metrics(bio, "metier4")$dl_ratio)),
   "a table with no discards column has no D/L")
ok(!any(grepl("D/L", names(exclusion_choices(bio, "metier4")))),
   "... and the dropdown label does not mention it")

## ---- the dropdown ------------------------------------------------------
ch <- exclusion_choices(s, "metier4")
ok(all(grepl("DR ", names(ch))) && all(grepl("D/L ", names(ch))),
   "labels carry BOTH the discard rate and D/L")
sdn <- names(ch)[ch == "SDN"]
ok(grepl("DR 33.3%", sdn, fixed = TRUE) && grepl("D/L 50%", sdn, fixed = TRUE),
   "SDN reads DR 33.3% (20/60) and D/L 50% (20/40) - the two really differ")
ok(identical(unname(exclusion_choices(s, "metier4", order_by = "dl")),
             c("SDN", "OTB", "TBB")) ||
   identical(unname(exclusion_choices(s, "metier4", order_by = "dl"))[1], "SDN"),
   "order_by = 'dl' puts the highest discards / landings first")
ok(!"D/L" %in% names(exclusion_choices(s, "metier4", show = c("coverage", "rate"))) &&
   !any(grepl("D/L", names(exclusion_choices(s, "metier4", show = c("coverage", "rate"))))),
   "show = can leave D/L out")

## ---- the pool summary --------------------------------------------------
ps <- donor_pool_summary(s, "metier4")
ok(isTRUE(all.equal(ps$dl_ratio, sum(s$discards) / sum(s$landings))),
   "the pooled D/L is total discards over total landings")
ok(is.na(donor_pool_summary(bio, "metier4")$dl_ratio),
   "... and missing where there are no discards")
ok(is.na(donor_pool_summary(s, "metier4", cov_min = 200)$dl_ratio) &&
   donor_pool_summary(s, "metier4", cov_min = 200)$n_strata == 0L,
   "an emptied pool reports no D/L")

cat("\nDONE ", FAIL, "failure(s)", "\n")
