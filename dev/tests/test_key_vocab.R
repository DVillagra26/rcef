## The hierarchical cascade's methodological correction: key vocabulary,
## grouped and curated, plus the two warning checks.

FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}

## ---- key_vocab_grouped(): structure and curation -----------------------
fx <- list.files(system.file("extdata", package = "rcef"),
                 "bll_2025_1catches_CEF.*\\.csv$", recursive = TRUE,
                 full.names = TRUE)
if (length(fx)) {
  cd <- rcef:::derive_fields(utils::read.csv(fx[1], stringsAsFactors = FALSE))
  kv <- key_vocab_grouped(cd)
  ok(identical(names(kv), c("Fleet", "Country", "Area", "Season")),
     "four families, in this order")
  ok(identical(unname(kv$Fleet),
              c("fleetValue", "metier6", "metier5", "metier4")),
     "Fleet is finest-first: fleetValue, metier6, metier5, metier4")
  ok(identical(unname(kv$Area),
              c("areaValue", "ICESSubdivision", "ICESDivision", "ICESSubArea")),
     "Area is finest-first: areaValue, subdivision, division, subarea")
  ok(identical(unname(kv$Country), "Country"), "Country is one variable")
  ok(identical(unname(kv$Season), "Season"), "Season is one variable")
  ## the curation: nothing outside these ten variables
  all_vars <- unlist(kv, use.names = FALSE)
  ok(!any(c("gear", "target", "mesh", "FleetType", "vesselFlagCountry",
           "PSU", "Stratum") %in% all_vars),
     "obsolete / non-cascade variables are not offered")
  ok(length(all_vars) == 10, "exactly ten variables across the four families")
}

## ---- key_vs_strata_warnings(): key finer than the strata definition ---
ok(length(key_vs_strata_warnings(c("fleetValue", "Country"),
                                 c("metier5", "Season"))) == 1,
   "fleetValue after a metier5 strata definition warns")
ok(length(key_vs_strata_warnings(c("metier6", "Country"),
                                 c("metier5", "Season"))) == 1,
   "metier6 (finer than metier5) also warns")
ok(length(key_vs_strata_warnings(c("metier4", "Country"),
                                 c("metier5", "Season"))) == 0,
   "metier4 (coarser than metier5) does not warn")
ok(length(key_vs_strata_warnings(c("fleetValue", "Country"),
                                 c("Season"))) == 0,
   "a fleet key with NO fleet variable in the strata definition does not warn")
ok(length(key_vs_strata_warnings(c("areaValue"), c("ICESSubArea"))) == 1,
   "the same check applies to the Area family")
ok(length(key_vs_strata_warnings(character(0), c("metier5"))) == 0,
   "no keys picked yet - nothing to warn about")
ok(length(key_vs_strata_warnings(c("", "Country"), c("metier5"))) == 0,
   "a blank (unfilled) key slot is skipped, not treated as a violation")

## ---- key_order_warnings(): coarse-then-fine within the key sequence ---
ok(length(key_order_warnings(c("ICESSubArea", "Country", "areaValue"))) == 1,
   "areaValue after ICESSubArea (finer after coarser) warns")
ok(length(key_order_warnings(c("areaValue", "Country", "ICESSubArea"))) == 0,
   "areaValue then ICESSubArea (coarsening) does not warn")
ok(length(key_order_warnings(c("fleetValue", "areaValue", "metier4",
                               "ICESSubArea"))) == 0,
   "two independent families, each individually coarsening, do not conflict")
ok(length(key_order_warnings(c("metier4", "metier5"))) == 1,
   "metier5 (finer than metier4) after it warns, even adjacent")
ok(length(key_order_warnings(c("metier4", "metier4"))) == 0,
   "the SAME variable picked twice is not a fineness violation (equal rank)")
ok(length(key_order_warnings(c("Country", "Season"))) == 0,
   "single-variable families never conflict with themselves")
ok(length(key_order_warnings(character(0))) == 0, "no keys - nothing to warn about")

## ---- the raising-cascade order itself, tied back to these checks ------
##
## The whole point of the order fix: a key list built respecting BOTH
## warnings (fine-to-coarse within each family, and never finer than the
## strata definition) is exactly the key list that widens sensibly when
## walked in the NEW order (Key 2 first).
keys <- c("metier4", "Country", "areaValue", "ICESSubArea", "Season")
ok(length(key_order_warnings(keys)) == 0,
   "a well-formed key list (as documented for raising_steps) has no order warnings")
st <- raising_steps(keys, rep(50, 5), self_threshold = 50, mode = "nested",
                    dims = keys)
ok(identical(st[[2]]$id, "2") && grepl("^Country", st[[2]]$label),
   "and metier4 (Key 2) is indeed the first one dropped in the cascade")

cat("\nDONE ", FAIL, "failure(s)", "\n")
