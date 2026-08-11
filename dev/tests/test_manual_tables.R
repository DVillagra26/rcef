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

## ---- the conditions the ENGINE receives -----------------------------
##
## catchAllocationAoL() treats allocStrataCond as a partition and refuses
## the whole run with "N records assigned to more than one stratum" if it
## is not. The app resolves overlaps first-match-wins, so the raw clauses
## overlap by design and MUST be complemented on the way out. The emitter
## did this; run_allocation() did not, so the script in the R tab was
## right while the run behind the button failed - the worst way round for
## the two to disagree.
cc <- alloc_conds_from_groups(list(
  GA1 = list(alloc = 'metier4 == "TBB"', matched = "TRUE"),
  GA2 = list(alloc = "TRUE",             matched = "coverage >= 50")))

txt <- vapply(cc$allocStrataCond,
              function(q) paste(deparse(rlang::quo_get_expr(q)), collapse = " "),
              character(1))
ok(!grepl("!", txt[["GA1"]]), "the first group is its own clause, untouched")
ok(grepl("^!\\(", txt[["GA2"]]),
   "a catch-all becomes NOT everything above it, never a bare TRUE")

mtxt <- vapply(cc$bioMatchedCond,
               function(q) paste(deparse(rlang::quo_get_expr(q)), collapse = " "),
               character(1))
ok(identical(mtxt[["GA2"]], "coverage >= 50"),
   "donor conditions are NOT complemented - pools may overlap")

## the two halves of a partition must never both claim the same row
d <- data.frame(metier4 = c("TBB", "OTB"), stringsAsFactors = FALSE)
hits <- vapply(cc$allocStrataCond,
               function(q) sum(rlang::eval_tidy(q, data = d)), numeric(1))
ok(sum(hits) == nrow(d), "every row is claimed exactly once")

## and the metric columns the readable conditions need
dm <- add_stratum_metrics(
  data.frame(metier4 = c("TBB", "TBB"), ICESDivision = "27.4.b",
             catchCategory = c("Lan", "Dis"), total = c(1000, 200),
             domainCatchDis = c("d1", "d1"), stringsAsFactors = FALSE),
  c("metier4", "ICESDivision"))
ok(all(c("coverage", "discard_rate", "stratum_weight") %in% names(dm)),
   "add_stratum_metrics puts the metrics on the CEF rows")
ok(!is.null(rlang::eval_tidy(rlang::quo(coverage >= 50), data = dm)),
   "so `coverage >= 50` is evaluable where the engine evaluates it")

## --- the simplified strataCond a human reads --------------------------
##
## Non-catch-all clauses stay exactly as written: first-match-wins makes
## "& !(everything above)" redundant on screen for them. The CATCH-ALL is
## the exception and must be closed, because catchRaising() applies the
## conditions independently rather than in order - a bare TRUE there
## re-claims every stratum the rules above already took and the run dies
## with "N records assigned to more than one stratum".
rr <- list(
  new_rule("G1", "", list(list(var = "metier4", values = "TBB"))),
  new_rule("G2", "", list(list(var = "metier4", values = c("OTB", "SSC")))),
  new_rule("G3", "catch-all", list()))
cc <- conds_from_rules(rr, list(), "metier4")

simple <- vapply(cc$strataCond_simple,
                 function(q) paste(deparse(rlang::quo_get_expr(q)), collapse=" "),
                 character(1))
ok(!any(grepl("!", simple[c("G1", "G2")])),
   "the named clauses carry no complement noise")
ok(!identical(simple[["G3"]], "TRUE"),
   "the catch-all is NOT a bare TRUE - that is the double-assignment bug")
ok(grepl("metier4", simple[["G3"]]) && grepl("^!\\(", simple[["G3"]]),
   "the catch-all is the negation of the clauses above it")

## and the readable form is ALREADY a partition - no second call needed
d <- data.frame(metier4 = c("TBB", "OTB", "GNS"), stringsAsFactors = FALSE)
hs <- vapply(cc$strataCond_simple,
             function(q) sum(rlang::eval_tidy(q, data = d)), numeric(1))
ok(sum(hs) == nrow(d), "the emitted clauses claim every row exactly once")
ok(hs[["G3"]] == 1, "the catch-all picks up exactly the unclaimed GNS row")

## a TRUE with nothing above it really does mean everything
ok(identical(unname(close_catch_all(list(G1 = "TRUE"), as_quo = FALSE)), "TRUE"),
   "a lone catch-all stays TRUE")
## and the donor side is never closed
mtxt <- vapply(cc$matchedDataCond,
               function(q) paste(deparse(rlang::quo_get_expr(q)), collapse=" "),
               character(1))
ok(identical(mtxt[["G3"]], "TRUE"),
   "matchedDataCond keeps its TRUE - donors may be reused")

## complement_conds still turns overlapping clauses into a partition
part <- complement_conds(cc$strataCond_simple)
hits <- vapply(part, function(q) sum(rlang::eval_tidy(q, data = d)), numeric(1))
ok(sum(hits) == nrow(d), "the complemented clauses claim every row exactly once")
ok(hits[["G3"]] == 1, "complement_conds agrees on the unclaimed GNS row")

## --- the two focused strata tables -----------------------------------
##
## The old single diagnosis table answered two questions at once - what
## is being raised, and what raises it - so it did neither cleanly. These
## split them, and a TRUE / fallback level reads as "Catch All by All",
## never the bare word TRUE.
ex <- system.file("extdata", package = "rcef")
fx <- list.files(ex, "catches_CEF.*\\.csv$", recursive = TRUE, full.names = TRUE)
if (length(fx)) {
  cd <- rcef:::derive_fields(utils::read.csv(fx[1], stringsAsFactors = FALSE))
  dm <- c("metier4", "Country", "ICESDivision", "Season")
  st <- classify_strata(build_strata(cd, dm), 50)
  rec <- recommend_raising(st, dm, rep(50, 4), 50, "nested", "Catch All by All", dm)

  rt <- strata_raise_table(rec, dm)
  ok(!is.null(rt) && "raised_by" %in% names(rt),
     "strata_raise_table has a raised_by column")
  ok(!"raise_condition" %in% names(rt),
     "and drops the raw condition - it is the donor tab's concern")
  ok(!any(rt$raised_by == "TRUE", na.rm = TRUE),
     "no bare TRUE - a fallback reads as Catch All by All")
  ok(any(rt$raised_by == "Catch All by All"),
     "the fallback level is named in full")
  ok(all(diff(rt$landings) <= 1e-9, na.rm = TRUE) || is.unsorted(-rt$landings) == FALSE,
     "sorted heaviest first")

  dt <- strata_donor_table(rec, dm)
  ok(!is.null(dt) && "usable_donor" %in% names(dt),
     "strata_donor_table flags which strata may donate")
  ok("donor_group" %in% names(dt), "and names the donor group")
  ok(!"raised_by" %in% names(dt),
     "the donor table does not carry the raised-by column")
}

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
