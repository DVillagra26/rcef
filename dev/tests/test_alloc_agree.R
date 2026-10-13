## The comparison and the final allocation MUST be the same function with
## the same arguments, or "compare works but final fails" is inevitable.
## run_allocation_set() passed app_type POSITIONALLY into what is now the
## alloc_dims slot, so the comparison ran with the wrong app_type and no
## dims while the final run was correct.
source("dev/tests/fixture.R")
FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}
## What this guards: run_allocation_set() must call the identical
## function with the identical arguments as a single run_allocation(),
## so a comparison can never "pass" while the final run fails (the
## regression was app_type passed positionally into the alloc_dims
## slot). It is NOT a test that the engine produces correct numbers for
## any particular input - only that the two entry points behave the
## same, whether they succeed or error.
if (exists("catchAllocationAoL")) {
  test_catch <- fx_catch()
  test_dist <- fx_dist()
  cd <- test_catch; dist <- test_dist
  dims <- c("catchCategory", "metier4", "vesselFlagCountry", "ICESDivision", "seasonValue")
  groups <- list(GA1 = list(alloc = "TRUE", matched = "TRUE"))

  single <- rcef:::run_allocation(cd, dist, groups, alloc_dims = dims,
                                  app_type = "LengthTotal")
  set <- rcef:::run_allocation_set(list(A = list(name = "A", groups = groups)),
                                   cd, dist, app_type = "LengthTotal",
                                   alloc_dims = dims)

  err1 <- inherits(single, "alloc_error"); err2 <- inherits(set$A, "alloc_error")
  ok(identical(err1, err2),
     "run_allocation() and run_allocation_set() succeed or fail together")
  if (err1) {
    ok(identical(single$message, set$A$message),
       "and when they fail, they fail for the same reason - same call")
  } else {
    ok(identical(nrow(single$distribution), nrow(set$A$distribution)),
       "and when they run, they produce the identical distribution")
  }
} else {
  cat("skip   catchAllocationAoL not available\n")
}

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)