## The length/age hierarchical Code tab.
##
## It used to print the resolved allocStrataCond / bioMatchedCond and
## nothing else: a wall of clauses that says which strata ended up
## together but not the rule that put them there, and which cannot be
## re-run on next year's data because the answer is baked into it. The
## discard cascade has always emitted the RECIPE first. This is the
## biology twin, and these assertions are what keeps the two in step.

FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}

spec <- list(
  keys = c("catchCategory", "metier4", "vesselFlagCountry", "ICESDivision"),
  thresholds = list(Lan = c(50, 50, 40), Dis = c(30, 30, 20)),
  min_meas = list(Lan = 100, Dis = 50), keep_cc = TRUE,
  donor_cov_min = 50, donor_meas_min = 100,
  cols = c("catchCategory", "metier4", "vesselFlagCountry", "ICESDivision"),
  bms = TRUE, dims = c("catchCategory", "metier4", "vesselFlagCountry"))

groups <- list(
  GA1 = list(alloc = 'catchCategory %in% c("Dis", "BMS")',
             matched = "coverage >= 50 & n_measurements >= 100"),
  GA2 = list(alloc = "TRUE", matched = "TRUE"))

txt <- paste(emit_alloc_recipe_code(spec, groups, "Length"), collapse = "\n")

## ---- 1. the recipe is stated, in the discard cascade's shape ---------
ok(grepl("Approach A - hierarchical length allocation", txt),
   "the banner names the approach")
for (line in c("Strata defined by", "Hierarchy", "Coverage bars",
               "Measurement bars", "Donor coverage min", "Donor measurements"))
  ok(grepl(line, txt, fixed = TRUE), paste("the banner states:", line))
ok(grepl("catchCategory > metier4 > vesselFlagCountry > ICESDivision", txt, fixed = TRUE),
   "the hierarchy reads finest-first with separators, like the discard side")

## Landings and discards are sampled differently and carry separate
## bars; one number in the banner would be true for half the data.
ok(grepl("landings 50%, discards 30%", txt, fixed = TRUE),
   "coverage bars are shown per catch category")
ok(grepl("landings 100, discards 50", txt, fixed = TRUE),
   "measurement bars are shown per catch category")

## ---- 2. the calls that reproduce it are there ------------------------
for (fn in c("alloc_steps(", "assign_alloc_cascade(", "cascade_to_groups(",
             "biology_link(", "classify_strata("))
  ok(grepl(fn, txt, fixed = TRUE), paste("the recipe calls", fn))
ok(grepl("keep_cc = TRUE", txt, fixed = TRUE), "keep_cc reaches alloc_steps()")
ok(grepl("bms = TRUE", txt, fixed = TRUE), "the BMS choice is recorded")

## ---- 3. and the conditions it resolves to, once ----------------------
ok(grepl("allocStrataCond", txt), "the resolved allocStrataCond is shown")
ok(grepl("bioMatchedCond", txt), "the resolved bioMatchedCond is shown")
ok(length(gregexpr("allocStrataCond <-", txt)[[1]]) == 1,
   "allocStrataCond is assigned exactly once")

## the catch-all must still be closed here - this is the block a user copies
ok(!grepl("GA2 = quo(TRUE)", sub("bioMatchedCond.*", "", txt), fixed = TRUE),
   "the emitted catch-all is not a bare TRUE")

## ---- 4. emit_alloc_code() must not repeat them -----------------------
##
## The script prints the recipe (which ends with the conditions) and then
## the allocation call. If the call emitted the lists again the script
## would carry two allocStrataCond blocks, the second overwriting the
## first - legal R, unreadable, and an invitation to edit the wrong one.
both <- paste(c(emit_alloc_recipe_code(spec, groups, "Length"),
                emit_alloc_code(groups, "LengthTotal",
                                with_conditions = FALSE)), collapse = "\n")
ok(length(gregexpr("allocStrataCond <-", both)[[1]]) == 1,
   "recipe + allocation call assign allocStrataCond only once")
ok(grepl("catchAllocationAoL(", both, fixed = TRUE),
   "and the allocation call is still there")
ok(length(gregexpr("allocStrataCond <-",
                   paste(emit_alloc_code(groups, "LengthTotal"),
                         collapse = "\n"))[[1]]) == 1,
   "the default still emits the conditions, for callers that need them")

## ---- 5. an allocation saved before specs existed ---------------------
old <- emit_alloc_recipe_code(NULL, groups, "Length")
ok(length(old) && grepl("allocStrataCond", paste(old, collapse = "\n")),
   "no spec falls back to the conditions rather than erroring")
ok(!grepl("Approach A", paste(old, collapse = "\n")),
   "and does not claim a recipe it does not have")

## ---- 6. Age says age -------------------------------------------------
a <- paste(emit_alloc_recipe_code(spec, groups, "Age"), collapse = "\n")
ok(grepl("hierarchical age allocation", a), "the Age tab says age")
ok(grepl('"Age"', a, fixed = TRUE), "and links the Age distributions")

cat("\nDONE ", FAIL, "failure(s)", "\n")
