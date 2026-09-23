## grouping_vars() must not be masked.
##
## This was named group_vars() and dplyr exports one too. The app
## attaches dplyr, so inside the server the package's function was
## shadowed - grouping_vars() came back empty, every picker showed only
## "-- none --", and nothing errored to say why. A silent empty result
## is the worst failure shape available, so it gets its own assertion.
##
## Split out of the old test_ladder_mode.R when raising_steps() dropped
## its ladder/nested mode choice (nested is now the only cascade) - this
## check has nothing to do with that and outlived it.

FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}

ok(!exists("group_vars", where = asNamespace("rcef"), inherits = FALSE),
   "the masked name is gone from the package")
ok(is.function(rcef::grouping_vars), "the vocabulary is exported unmasked")
if (requireNamespace("dplyr", quietly = TRUE)) {
  ok(!identical(rcef::grouping_vars, dplyr::group_vars),
     "and is not dplyr's function")
  ## the shape the app runs in: dplyr attached, rcef attached
  f <- get("grouping_vars", envir = asNamespace("rcef"))
  fx <- "anchoveta"
  if (length(fx)) {
    cd <- {data("anchoveta_catch", package = "rcef", envir = environment()); anchoveta_catch}
    ok(length(f(cd)) > 5,
       "and returns a real vocabulary with dplyr on the search path")
  }
}

cat("\nDONE ", FAIL, "failure(s)", "\n")
