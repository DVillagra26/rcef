## The bundled anchoveta example must run the WHOLE workflow, not just the plots.
##
## It once carried originType = "Estimate". The raising and allocation
## engines - and the app's originType picker - accept only "WGEstimate" or
## "Official", so a raising run on it found no estimates and returned every
## row as imported: no error, no warning. Every table and plot worked, which
## is why it went unnoticed. These checks push the example through discard
## raising (automatic and manual groups), length and age allocation and both
## aggregations.
##
## They use nothing but library(rcef). The app attaches tidyr, and that hid
## a missing importFrom(tidyr, replace_na): headless, the numbers-at-length
## aggregation failed with 'could not find function "replace_na"'.

FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}
quiet <- function(expr) {                 # the engine prints its own diagnostics banner
  out <- NULL
  invisible(utils::capture.output(out <- suppressMessages(expr)))
  out
}
lf <- tempfile(fileext = ".txt")          # keep the engines' log out of the working directory

data(anchoveta_catch); data(anchoveta_dist)
cc <- anchoveta_catch; dd <- anchoveta_dist

ok(!"package:tidyr" %in% search() && !"package:dplyr" %in% search(),
   "run headless: neither tidyr nor dplyr is attached")

## ---- 1. the data contract the engines read -----------------------------
ok(identical(unique(cc$originType), "WGEstimate"),
   "every catch row is originType 'WGEstimate' (what catchRaising() and the app select on)")
ok(identical(sort(unique(cc$year)), 2020:2023) && length(unique(cc$stock)) == 1,
   "one stock, four years")
ok(all(cc$catchCategory %in% c("Lan", "Dis", "BMS")) && all(c("Lan", "Dis") %in% cc$catchCategory),
   "catch parts are Lan / Dis / BMS")
ok(all(c("vesselFlagCountry", "seasonValue", "metier4", "ICESDivision", "Stratum") %in% names(cc)),
   "already through derive_fields()")
lan <- cc[cc$catchCategory == "Lan", ]; dis <- cc[cc$catchCategory == "Dis", ]
ok(all(dis$domainCatchDis %in% lan$domainCatchDis),
   "every discard estimate links to landings through domainCatchDis")
ok(any(!nzchar(lan$domainCatchDis)) && any(nzchar(lan$domainCatchDis)),
   "some landings carry a discard estimate (donors) and some do not (to raise)")
ok(all(c("LengthTotal", "Age") %in% dd$distributionType) &&
     all(dd$domainBiology %in% cc$domainBiology),
   "length and age distributions, every one linked to a catch stratum")
ok(!any(duplicated(dd)),
   "no distribution row repeats another exactly (a repeat is counted twice in every sum)")

## ---- 2. discard raising, one year -------------------------------------
c23 <- subset(cc, year == 2023); d23 <- subset(dd, year == 2023)
dims <- c("metier4", "ICESDivision", "seasonValue")
n_to_raise <- sum(c23$catchCategory == "Lan" & !nzchar(c23$domainCatchDis))

grp <- quiet(define_raising_groups_auto(c23, hierarchy = dims, thresholds = c(50, 40, 30)))
raised <- quiet(run_native_raising(c23, dims, grp, type = "discards", log_file = lf))
rr <- raised[raised$importedOrRaised %in% "raised", ]
ok(nrow(rr) == n_to_raise,
   sprintf("automatic groups: one raised discard row for each of the %d landings without an estimate (%d)",
           n_to_raise, nrow(rr)))
ok(all(rr$catchCategory == "Dis") && all(is.finite(rr$total)) && all(rr$total >= 0),
   "automatic groups: raised rows are discards with finite, non-negative totals")
ok(sum(raised$importedOrRaised %in% "imported") > 0 &&
     sum(rr$total) > 0,
   "automatic groups: imported rows kept, and something was raised")

rules <- list(new_rule("PS",  raise = list(list(var = "metier4", values = "PS"))),
              new_rule("MPT", raise = list(list(var = "metier4", values = "MPT"))),
              new_rule("REST"))
grp_m <- quiet(define_raising_groups_manual(build_strata(c23, dims), rules))
raised_m <- quiet(run_native_raising(c23, dims, grp_m, type = "discards", log_file = lf))
rm <- raised_m[raised_m$importedOrRaised %in% "raised", ]
ok(nrow(rm) == n_to_raise && all(is.finite(rm$total)),
   "manual groups: the same landings are raised, all totals finite")

err <- tryCatch({ quiet(run_native_raising(cc, dims, grp, type = "discards", log_file = lf)); "" },
                error = function(e) conditionMessage(e))
ok(grepl("Several years", err),
   "all four years at once: the engine refuses (one stock and year at a time)")

## ---- 3. length and age allocation ------------------------------------
groups_alloc <- list(ALL = list(alloc = "TRUE", matched = "TRUE"))
for (ty in c("LengthTotal", "Age")) {
  al <- quiet(run_allocation(raised, d23, groups_alloc, app_type = ty, verbose = FALSE, log_file = lf))
  ok(!inherits(al, "alloc_error") && nrow(al$distribution) > 0,
     sprintf("%s: the raised catch is allocated", ty))
  if (inherits(al, "alloc_error")) { cat("        ", al$message, "\n"); next }
  nn <- agg_numbers(al); mw <- agg_meanweight(al)
  ok(!is.null(nn) && nrow(nn) > 0 && sum(nn[[grep("^N_", names(nn))[1]]], na.rm = TRUE) > 0,
     sprintf("%s: numbers at class aggregate", ty))
  ok(!is.null(mw) && nrow(mw) > 0,
     sprintf("%s: mean weight at class aggregates", ty))
}

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
