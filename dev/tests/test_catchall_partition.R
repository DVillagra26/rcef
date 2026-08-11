## The bug this suite exists for
## =============================
## A manual group set ends with a catch-all. Written the obvious way that
## clause is `quo(TRUE)`, and the app ran fine, because conds_from_rules()
## complements internally. The EMITTED SCRIPT carried the bare TRUE, and
## running it by hand gave:
##
##   Error in check_group_conditions(...):
##       * Error: 775 records assigned to more than one stratum!
##
## because catchRaising() and catchAllocationAoL() do NOT evaluate the
## conditions in order - each is applied independently and then the engine
## checks that no record was claimed twice. TRUE claims everything,
## including what the rules above already took.
##
## Everything below runs the REAL engine check, not a reimplementation of
## it: a test that only inspects the strings would have passed against the
## broken version too, since the strings were the intended output.

FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}

## A catch table with three metiers, so a two-rule set leaves a genuine
## remainder for the catch-all to pick up. Built with the RAW CEF column
## names, because cef_prepare() runs derive_fields() and that insists on
## fleetValue / areaValue / vesselFlagCountry / seasonValue. Handing it a
## table that already carries metier4 makes the engine error on the
## missing columns instead of on the conditions, which would let a broken
## catch-all "pass" for entirely the wrong reason.
raw <- data.frame(
  fleetValue        = rep(c("TBB_DEF_70-99", "OTB_DEF_120-219",
                            "GNS_DEF_120-219"), each = 4),
  areaValue         = "27.4.b",
  vesselFlagCountry = rep(c("BE", "NL"), 6),
  seasonValue       = rep(c("1", "2"), each = 2, length.out = 12),
  catchCategory     = rep(c("Lan", "Dis"), 6),
  total             = 100,
  variableUnit      = "kg",
  domainCatchDis    = "d1",
  stringsAsFactors  = FALSE)
cd <- derive_fields(raw)

rules <- list(
  new_rule("G1", "beam",  list(list(var = "metier4", values = "TBB"))),
  new_rule("G2", "otter", list(list(var = "metier4", values = "OTB"))),
  new_rule("G5", "Catch All by All", list()))

cc <- conds_from_rules(rules, list(), "metier4")

## ---- 1. the readable clauses ARE a partition -------------------------
##
## strataCond_simple is what code_manual_rule() prints into the script, so
## it is the thing that has to hold up, not the internal strataCond.
hits <- vapply(cc$strataCond_simple,
               function(q) sum(rlang::eval_tidy(q, data = cd)), numeric(1))
ok(sum(hits) == nrow(cd), "every catch row is claimed exactly once in total")
ok(all(hits > 0), "no group is left empty")
ok(hits[["G5"]] == 4, "the catch-all claims exactly the GNS rows")

## the pairwise check the engine actually performs
m <- vapply(cc$strataCond_simple,
            function(q) as.logical(rlang::eval_tidy(q, data = cd)),
            logical(nrow(cd)))
ok(max(rowSums(m)) == 1, "no row matches two clauses - the engine's own test")

## ---- 2. the ENGINE agrees -------------------------------------------
##
## check_group_conditions() is the function that raised the original
## error. Running it is the only assertion here that cannot rot.
prepped <- cef_prepare(raw, "metier4")
eng <- tryCatch({
  check_group_conditions(catch_data = prepped,
                         condition_list = cc$strataCond_simple,
                         logFile = file.path(tempdir(), "cgc.txt"),
                         append = FALSE)
  "ok"
}, error = function(e) conditionMessage(e))
ok(identical(eng, "ok"),
   paste("check_group_conditions accepts the emitted strataCond;",
         if (identical(eng, "ok")) "" else eng))

## and it REJECTS the old shape, so this suite fails if the fix regresses
broken <- cc$strataCond_simple
broken[["G5"]] <- rlang::new_quosure(quote(TRUE), env = globalenv())
bad <- tryCatch({
  check_group_conditions(catch_data = prepped,
                         condition_list = broken,
                         logFile = file.path(tempdir(), "cgc2.txt"),
                         append = FALSE)
  "accepted"
}, error = function(e) conditionMessage(e))
## and it must be rejected for the RIGHT reason - the double assignment,
## not some unrelated complaint about the fixture
ok(!identical(bad, "accepted") && grepl("more than one stratum", bad),
   paste("a bare TRUE catch-all is rejected as a double assignment;",
         if (identical(bad, "accepted")) "it was ACCEPTED" else ""))

## ---- 3. the donor side is deliberately NOT closed --------------------
mt <- vapply(cc$matchedDataCond,
             function(q) paste(deparse(rlang::quo_get_expr(q)), collapse = " "),
             character(1))
ok(identical(mt[["G5"]], "TRUE"),
   "matchedDataCond keeps its TRUE - a donor may feed several groups")

## ---- 4. the same rule on the length/age side -------------------------
##
## allocStrataCond had the identical defect via allocStrataSimple.
groups <- list(
  GA1 = list(alloc = 'catchCategory %in% c("Dis", "BMS")', matched = "TRUE"),
  GA2 = list(alloc = "TRUE", matched = "TRUE"))
cs <- alloc_conds_from_groups(groups)

ok(!identical(unname(cs$allocStrataSimple[["GA2"]]), "TRUE"),
   "the allocation catch-all is closed too")
q <- lapply(cs$allocStrataSimple, function(t)
  rlang::new_quosure(rlang::parse_expr(t), env = globalenv()))
am <- vapply(q, function(x) as.logical(rlang::eval_tidy(x, data = cd)),
             logical(nrow(cd)))
ok(max(rowSums(am)) == 1, "no catch row is allocated to two groups")
ok(sum(am) == nrow(cd), "every catch row is allocated exactly once")

## the emitted text is what a reader sees, so check that too
## Only the allocStrataCond block: bioMatchedCond legitimately prints
## quo(TRUE) for every group, so grepping the whole emission would fail
## on correct output.
txt   <- paste(emit_alloc_conditions(groups), collapse = "\n")
block <- sub(".*allocStrataCond <-", "", sub("bioMatchedCond <-.*", "", txt))
ok(!grepl("GA2 = quo\\(TRUE\\)", block),
   "the emitted allocStrataCond does not print quo(TRUE) for the catch-all")
ok(grepl("GA2 = quo\\(TRUE\\)", txt),
   "but bioMatchedCond still does - donors are not a partition")
ok(grepl("dist = distributions", txt) || !grepl("add_stratum_metrics", txt),
   "when metrics are joined, the distributions go with them")

## ---- 5. named clauses stay clean ------------------------------------
##
## The whole point of closing ONLY the catch-all is that the other lines
## keep reading like the rules they are.
s2 <- vapply(cc$strataCond_simple,
             function(q) paste(deparse(rlang::quo_get_expr(q)), collapse = " "),
             character(1))
ok(!grepl("!", s2[["G1"]]) && !grepl("!", s2[["G2"]]),
   "G1 and G2 carry no complement noise")
ok(grepl("^!\\(", s2[["G5"]]), "only the catch-all is negated")

cat("\nDONE ", FAIL, "failure(s)", "\n")
