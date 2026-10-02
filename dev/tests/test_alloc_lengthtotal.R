## The length / age allocation speaks CEF's own vocabulary end to end:
## distributionType is "LengthTotal" or "Age" - what the distributions file
## holds - in catchAllocationAoL() itself, in what it returns, in the
## aggregations built on that and in the scripts that reproduce them.
##
## It used to be "Length" inside the vendored function. run_allocation()
## rewrote the column on the way in, the emitted TAF script did the same
## before calling a function it then asked for "LengthTotal" - so the script
## it wrote could not run - and every result carried "Length".
##
## Uses the bundled anchoveta example, as test_anchoveta_example.R does.

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

data(anchoveta_catch); data(anchoveta_dist)
c23 <- subset(anchoveta_catch, year == 2023)
d23 <- subset(anchoveta_dist, year == 2023)
dims <- c("metier4", "ICESDivision", "seasonValue")
grp <- quiet(define_raising_groups_auto(c23, hierarchy = dims, thresholds = c(50, 40, 30)))
raised <- quiet(run_native_raising(c23, dims, grp, type = "discards", log_file = lf))
groups_alloc <- list(ALL = list(alloc = "TRUE", matched = "TRUE"))

## ---- 1. the function itself ---------------------------------------------------
vals <- function(f) eval(formals(f)$distributionType)
for (fn in c("catchAllocationAoL", "grp_AoL_alloc_N", "grp_AoL_alloc_WL",
             "grp_AoL_alloc_condition"))
  ok(identical(vals(get(fn)), c("Age", "LengthTotal")),
     sprintf("%s(): distributionType is c(\"Age\", \"LengthTotal\")", fn))

## the function's own length() checks: the condition lists must agree in length,
## and unnamed ones are named GA1, GA2, ... (run_allocation() always names them,
## so this is the only way those two lines run)
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
al <- quiet(run_allocation(raised, d23, groups_alloc, app_type = "LengthTotal",
                           verbose = FALSE, log_file = lf))
ok(!inherits(al, "alloc_error") && nrow(al$distribution) > 0,
   "LengthTotal: the raised catch is allocated with CEF's own value, no translation")
ok(identical(al$type, "LengthTotal") && is.null(al$app_type),
   "the result says LengthTotal, in one field")
ok(identical(sort(unique(al$distribution$distributionType)), "LengthTotal"),
   "the allocated distributions carry LengthTotal - not Length")
nn <- agg_numbers(al); mw <- agg_meanweight(al)
nc <- grep("^N_", names(nn), value = TRUE)
mc <- grep("^W_", names(mw), value = TRUE)
ok(length(nc) > 3 && all(grepl("^N_LengthTotal_", nc)),
   "the aggregated numbers are named N_LengthTotal_<class>")
ok(length(mc) > 3 && all(grepl("^W_LengthTotal_", mc)),
   "the aggregated mean weights are named W_LengthTotal_<class>")
long <- alloc_pivot_long(nn, "N", ".val")
ok(!is.null(long) && all(long$.aol == "LengthTotal"),
   "alloc_pivot_long() reads those names back: the type is LengthTotal")
cmpn <- alloc_compare_numbers(al)
ok(max(long$.cls) > 20 && max(cmpn$.cls) <= 20,
   "length classes are shown in cm: the mm classes are divided by ten for LengthTotal")
sw <- alloc_stock_weight(al)
ok(!is.null(sw) && max(sw$.cls) <= 20 && all(is.finite(sw$.tonnes)),
   "and so is the stock weight")
ok(inherits(plot_alloc_numbers(cmpn, "catchCategory", "Length"), "ggplot") &&
     inherits(plot_alloc_stockweight(sw, "catchCategory", "Length"), "ggplot"),
   "the two comparison plots build from them")

## ---- 3. age: unchanged ---------------------------------------------------------
aa <- quiet(run_allocation(raised, d23, groups_alloc, app_type = "Age",
                           verbose = FALSE, log_file = lf))
nna <- agg_numbers(aa)
ok(!inherits(aa, "alloc_error") && identical(aa$type, "Age") &&
     all(grepl("^N_Age_", grep("^N_", names(nna), value = TRUE))),
   "Age: allocated, and the numbers are N_Age_<class>")
ok(max(alloc_compare_numbers(aa)$.cls) <= 10,
   "ages are not divided by ten")

## ---- 4. the old value is gone, with the bridge that served it -------------------
old <- quiet(run_allocation(raised, d23, groups_alloc, app_type = "Length",
                            verbose = FALSE, log_file = lf))
ok(inherits(old, "alloc_error") && grepl("No rows of distributionType 'Length'", old$message),
   "'Length' is no longer a distributionType: it finds no rows, and the message says which value")
ok(!any(vapply(c("ALLOC_TYPE", "alloc_type_of", "prep_for_alloc"), exists, NA)),
   "the vocabulary bridge (ALLOC_TYPE, alloc_type_of(), prep_for_alloc()) is gone")

## ---- 5. the scripts that reproduce it ----------------------------------------------
agg_len <- paste(emit_alloc_agg_code("Length"), collapse = "\n")
ok(grepl("distributionType = \"LengthTotal\"", agg_len, fixed = TRUE) &&
     grepl("type = \"LengthTotal\"", agg_len, fixed = TRUE) &&
     !grepl("distributionType = \"Length\"", agg_len, fixed = TRUE) &&
     !grepl("type = \"Length\"", agg_len, fixed = TRUE),
   "emit_alloc_agg_code(): the aggregation calls say LengthTotal")
agg_age <- paste(emit_alloc_agg_code("Age"), collapse = "\n")
ok(grepl("distributionType = \"Age\"", agg_age, fixed = TRUE) &&
     !grepl("LengthTotal", agg_age, fixed = TRUE),
   "and Age says Age")
script <- paste(taf_model_allocate_length_age(list()), collapse = "\n")
ok(!grepl("<- \"Length\"", script, fixed = TRUE) &&
     !grepl("Rewriting the column", script, fixed = TRUE),
   "the emitted allocate script no longer rewrites the distributions column")

# run what the app emits, the way a TAF script would: allocate, then aggregate
env <- new.env()
env$catch_data_raised <- as.data.frame(raised)
env$distributions     <- d_lt
env$quo               <- rlang::quo
code <- c(emit_alloc_code(groups_alloc, "LengthTotal"), "",
          emit_alloc_agg_code("Length"))
res <- try(quiet(eval(parse(text = code), envir = env)), silent = TRUE)
ok(!inherits(res, "try-error") && is.data.frame(env$numbers_at) &&
     any(grepl("^N_LengthTotal_", names(env$numbers_at))) &&
     is.data.frame(env$meanw_at_g) && is.data.frame(env$stock_weight),
   "the code the app emits for a length allocation runs and gives the same tables")
if (inherits(res, "try-error")) cat("        ", conditionMessage(attr(res, "condition")), "\n")

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
