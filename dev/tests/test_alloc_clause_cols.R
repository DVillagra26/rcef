## A manual allocation group whose clause names a column the strata table
## does not carry used to take NOTHING, silently: assign_alloc_groups()
## turns the "object not found" into an all-FALSE, so the only symptom was
## an empty assignment (seen as "I picked the TBB group and nothing was
## assigned" - the default strata were vesselFlagCountry / fleetValue /
## areaValue / seasonValue, none of which is metier4). It must now say so, and must still
## assign normally once the column is there.

source("dev/tests/fixture.R")
FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}

test_catch <- fx_catch()
test_dist <- fx_dist()
mk <- function(cl) list(label = "g", alloc = alloc_cond_string(cl),
  matched = alloc_cond_string(cl), excluded = character(0),
  alloc_expr = alloc_quo(cl), matched_expr = alloc_quo(cl))
g <- list(GA1 = mk(list(metier4 = "PS")))

## strata that DO carry the column: assigned, and no complaint
with_col <- suppressWarnings(biology_link(test_catch, test_dist,
  c("catchCategory", "metier4", "vesselFlagCountry"), NULL, "LengthTotal"))
w1 <- NULL
a1 <- withCallingHandlers(assign_alloc_groups(with_col, g, TRUE),
  warning = function(w) { w1 <<- conditionMessage(w); invokeRestart("muffleWarning") })
ok(sum(a1$alloc_group %in% "GA1") > 0 &&
   all(a1$alloc_group[a1$metier4 == "PS"] == "GA1"),
   "a clause on a column the strata carry assigns every matching stratum")
ok(is.null(w1), "... and does not warn")
ok(!length(attr(a1, "missing_clause_cols")), "... and records no missing columns")

## strata WITHOUT it (the default app strata): nothing assigned - but loudly
without_col <- suppressWarnings(biology_link(test_catch, test_dist,
  c("catchCategory", "vesselFlagCountry", "fleetValue"), NULL, "LengthTotal"))
ok(!"metier4" %in% names(without_col), "(fixture: the strata really lack metier4)")
w2 <- NULL
a2 <- withCallingHandlers(assign_alloc_groups(without_col, g, TRUE),
  warning = function(w) { w2 <<- conditionMessage(w); invokeRestart("muffleWarning") })
ok(sum(a2$alloc_group %in% "GA1") == 0, "a clause on a missing column matches nothing")
ok(!is.null(w2) && grepl("GA1", w2) && grepl("metier4", w2),
   "... and the warning names the group and the missing column")
ok(identical(attr(a2, "missing_clause_cols")$GA1, "metier4"),
   "... and the result records which column was missing, per group")

## the stratum metrics are computed columns, not dimensions - a matched
## condition naming them against a table that has them stays quiet
g2 <- list(GA1 = list(label = "g", alloc = "TRUE", matched = "coverage >= 0",
  excluded = character(0), alloc_expr = alloc_quo(list()),
  matched_expr = rlang::new_quosure(quote(coverage >= 0), rlang::global_env())))
w3 <- NULL
withCallingHandlers(assign_alloc_groups(with_col, g2, TRUE),
  warning = function(w) { w3 <<- conditionMessage(w); invokeRestart("muffleWarning") })
ok(is.null(w3), "a condition on coverage (present in the link) does not warn")

cat("\nDONE ", FAIL, "failure(s)", "\n")