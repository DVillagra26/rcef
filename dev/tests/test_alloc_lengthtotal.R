## The length / age allocation speaks CEF's own vocabulary end to end:
## distributionType is "LengthTotal" or "Age" - what the distributions file
## holds - in catchAllocationAoL() itself, in what it returns and in the
## aggregations built on that.
##
## It used to be "Length" inside the vendored function, and every result
## carried "Length" while the distributions file said "LengthTotal".
##
## Uses the synthetic test fixture, as test_workflow_example.R does.

source("dev/tests/fixture.R")
FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}
quiet <- function(expr) {
  out <- NULL
  invisible(utils::capture.output(out <- suppressMessages(expr)))
  out
}
suppressPackageStartupMessages(library(ggplot2))
lf <- tempfile(fileext = ".txt")

test_catch <- fx_catch(); test_dist <- fx_dist()
c23 <- subset(test_catch, year == 2023)
d23 <- subset(test_dist, year == 2023)
raised <- quiet(fx_raise(c23, fx_groups("own"), type = "discards", log_file = lf))

## ---- 1. the function itself ---------------------------------------------------
vals <- function(f) eval(formals(f)$distributionType)
for (fn in c("catchAllocationAoL", "grp_AoL_alloc_N", "grp_AoL_alloc_WL",
             "grp_AoL_alloc_condition"))
  ok(identical(vals(get(fn)), c("Age", "LengthTotal")),
     sprintf("%s(): distributionType is c(\"Age\", \"LengthTotal\")", fn))

## the function's own length() checks: the condition lists must agree in length,
## and unnamed ones are named GA1, GA2, ...
d_lt <- as.data.frame(d23[d23$distributionType == "LengthTotal", ])
one  <- list(rlang::quo(TRUE))
two  <- list(rlang::quo(TRUE), rlang::quo(TRUE))
r_un <- quiet(catchAllocationAoL(catch_data = as.data.frame(raised),
                                 distribution_data = d_lt,
                                 condition_alloc_st_list = one,
                                 condition_matched_data_list = one,
                                 distributionType = "LengthTotal",
                                 verbose = FALSE, logFile = lf))
ok(is.list(r_un) && nrow(r_un$distribution) > 0 &&
     "GA1" %in% r_un$distribution$allocGroup,
   "catchAllocationAoL(): unnamed condition lists are accepted and named GA1, ...")
msg <- tryCatch({ quiet(catchAllocationAoL(catch_data = as.data.frame(raised),
                                           distribution_data = d_lt,
                                           condition_alloc_st_list = two,
                                           condition_matched_data_list = one,
                                           distributionType = "LengthTotal",
                                           verbose = FALSE, logFile = lf)); "" },
                error = function(e) conditionMessage(e))
ok(grepl("is not TRUE", msg, fixed = TRUE),
   "catchAllocationAoL(): condition lists of different lengths are refused by the length check")

## ---- 2. length: what goes in and what comes out -------------------------------
al <- quiet(fx_allocate(raised, d23, "LengthTotal", log_file = lf))
ok(nrow(al$distribution) > 0,
   "LengthTotal: the raised catch is allocated with CEF's own value, no translation")
ok(identical(sort(unique(al$distribution$distributionType)), "LengthTotal"),
   "the allocated distributions carry LengthTotal - not Length")
nn <- fx_numbers(al); mw <- fx_meanweight(al)
nc <- grep("^N_", names(nn), value = TRUE)
mc <- grep("^W_", names(mw), value = TRUE)
ok(length(nc) > 3 && all(grepl("^N_LengthTotal_", nc)),
   "the aggregated numbers are named N_LengthTotal_<class>")
ok(length(mc) > 3 && all(grepl("^W_LengthTotal_", mc)),
   "the aggregated mean weights are named W_LengthTotal_<class>")
long <- fx_pivot_long(nn, "N", ".val")
ok(!is.null(long) && all(long$.aol == "LengthTotal") && max(long$.cls) > 20,
   "the class and the type read back out of those names: LengthTotal, on the file's own mm scale")

## ---- 3. age: unchanged ---------------------------------------------------------
aa <- quiet(fx_allocate(raised, d23, "Age", log_file = lf))
nna <- fx_numbers(aa)
ok(nrow(aa$distribution) > 0 &&
     all(grepl("^N_Age_", grep("^N_", names(nna), value = TRUE))),
   "Age: allocated, and the numbers are N_Age_<class>")
ok(max(fx_pivot_long(nna, "N", ".val")$.cls) <= 10,
   "ages are plain years")

## ---- 4. the bridge that served the old value is gone ------------------------------
ok(!any(vapply(c("ALLOC_TYPE", "alloc_type_of", "prep_for_alloc"), exists, NA)),
   "the vocabulary bridge (ALLOC_TYPE, alloc_type_of(), prep_for_alloc()) is gone")

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
