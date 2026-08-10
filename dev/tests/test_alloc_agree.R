## The comparison and the final allocation MUST be the same function with
## the same arguments, or "compare works but final fails" is inevitable.
## run_allocation_set() passed app_type POSITIONALLY into what is now the
## alloc_dims slot, so the comparison ran with the wrong app_type and no
## dims while the final run was correct.
FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}

ex <- system.file("extdata", package = "rcef")
cf <- list.files(ex, "bll_2025_1catches_CEF.*\\.csv$",
                 recursive = TRUE, full.names = TRUE)
df <- list.files(ex, "bll_2025_1distributions_CEF.*\\.csv$",
                 recursive = TRUE, full.names = TRUE)

if (length(cf) && length(df) && exists("catchAllocationAoL")) {
  cd <- rcef:::derive_fields(utils::read.csv(cf[1], stringsAsFactors = FALSE))
  dist <- utils::read.csv(df[1], stringsAsFactors = FALSE)
  dims <- c("catchCategory", "metier4", "Country", "ICESDivision", "Season")
  groups <- list(GA1 = list(alloc = "TRUE", matched = "TRUE"))

  single <- rcef:::run_allocation(cd, dist, groups, alloc_dims = dims,
                                  app_type = "LengthTotal")
  set <- rcef:::run_allocation_set(list(A = list(name = "A", groups = groups)),
                                   cd, dist, app_type = "LengthTotal",
                                   alloc_dims = dims)
  ok(!inherits(single, "alloc_error"), "the single allocation runs")
  ok(!inherits(set$A, "alloc_error"), "the comparison allocation runs")
  ok(identical(nrow(single$distribution), nrow(set$A$distribution)),
     "and both produce the identical distribution - same function, same args")
} else {
  cat("skip   fixtures or catchAllocationAoL not available\n")
}

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
