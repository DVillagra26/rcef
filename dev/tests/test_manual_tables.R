## The four tables both manual builders now show, and the donor tagging
## behind two of them.
FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}

## ---- discard side ---------------------------------------------------
s <- data.frame(
  metier4      = c("TBB", "TBB", "OTB", "GNS"),
  ICESDivision = c("27.4.b", "27.4.c", "27.4.b", "27.4.b"),
  landings     = c(100, 50, 80, 5),
  coverage     = c(80, 10, 40, 90),
  discard_rate = c(0.20, 0.60, 0.30, 0.05),
  stringsAsFactors = FALSE)
rules <- list(new_rule("G1", "beam", list(list(var = "metier4", values = "TBB"))))

pr <- rules_progress(s, rules)
ok(nrow(pr) == 2, "one row per rule plus the remainder")
ok(identical(pr$group[2], "STILL MISSING"), "the remainder is the last row")
ok(pr$n_strata[1] == 2 && pr$n_strata[2] == 2, "strata split between claimed and not")
ok(is.na(pr$cum_pct_weight[2]),
   "the running total stops before the remainder - it is not another step")
ok(abs(pr$pct_weight[1] - 63.8) < 0.1, "percent of weight claimed")

un <- rules_unassigned(s, rules, dims = c("metier4", "ICESDivision"))
ok(nrow(un) == 2, "the unclaimed strata are listed")
ok(identical(un$metier4[1], "OTB"), "heaviest first")
ok(is.null(rules_unassigned(s, list(new_rule("G1", "all", list())))),
   "a catch-all leaves nothing unassigned")

## ---- biology side ---------------------------------------------------
mk <- function(lab, a, m) list(label = lab,
  alloc = alloc_cond_string(a), matched = alloc_cond_string(m),
  alloc_expr = alloc_quo(a), matched_expr = alloc_quo(m))

st <- data.frame(catchCategory = c("Lan", "Dis", "Lan", "Dis"),
                 metier4 = c("TBB", "TBB", "OTB", "OTB"),
                 weight = c(100, 50, 80, 20),
                 weight_measured = c(80, 10, 40, 5),
                 stringsAsFactors = FALSE)
g <- list(GA1 = mk("beam", list(metier4 = "TBB"), list()),
          GA2 = mk("rest", list(), list(catchCategory = "Lan")))
a <- assign_alloc_groups(st, g)

ok(identical(a$alloc_group, c("GA1", "GA1", "GA2", "GA2")),
   "allocation is first-match-wins: one group per stratum")

## This is the bug the whole donor column was built on. A clause-less
## group evaluates to the literal TRUE, which comes back length 1, not
## length nrow(). `hit & is.na(...)` recycles it silently; `which(hit)`
## does not, and the catch-all's pool came out as a single stratum.
ok(all(a$n_donates_to >= 1),
   "a clause-less donor condition covers every stratum, not just the first")
ok(identical(a$usedToAllocGroups_txt[1], "GA1, GA2"),
   "a stratum can lend to several groups")
ok(identical(a$usedToAllocGroups_txt[2], "GA1"),
   "a discard stratum is not in a landings-only pool")

ov <- alloc_overview(a, g)
ok(nrow(ov) == 2, "one row per group")
ok(ov$n_strata_allocated[1] == 2, "GA1 allocates two strata")
ok(ov$n_strata_in_pool[1] == 4,
   "GA1 draws on all four - the pool may be wider than what it allocates")
ok(ov$n_strata_in_pool[1] > ov$n_strata_allocated[1],
   "donated exceeds allocated, which is the normal case")

## an unassigned stratum shows up as its own row
g1 <- list(GA1 = mk("beam", list(metier4 = "TBB"), list()))
ov1 <- alloc_overview(assign_alloc_groups(st, g1), g1)
ok(any(ov1$group == "(unassigned)"), "unallocated strata get their own row")
ok(ov1$n_strata_allocated[ov1$group == "(unassigned)"] == 2,
   "and the count is right")

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
