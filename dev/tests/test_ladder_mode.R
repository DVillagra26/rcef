## The cascade with real column names as keys.
##
## Keys used to be four abstract dimensions resolved through a "level of
## detail" control, so a cascade could never widen from metier6 to
## metier4 - both are "Fleet". Naming columns directly removes that, but
## it also breaks the old pool shape:
##
##   "nested"  pool k = keys[1..k]. Key 1 is kept in EVERY pool. Right
##             for independent dimensions; degenerate the moment two keys
##             are resolutions of the same family, and completely
##             degenerate with Stratum first, since every pool then
##             contains - and collapses to - the stratum itself.
##
##   "ladder"  pool i = keys[i] alone, in the order given, finest first.
##             A rung may be coarser than the one above it in the same
##             family, which is the point.
##
## `mode` was also dead: raising_steps() hardcoded mode <- "nested" over
## whatever the caller passed.

FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}

keys <- c("Stratum", "metier4", "Country", "areaValue", "ICESSubArea", "Season")

## ---- 1. the two modes really differ ----------------------------------
lad <- raising_steps(keys, rep(50, 6), 50, "ladder", keys)
nes <- raising_steps(keys, rep(50, 6), 50, "nested", keys)
lab <- function(st) vapply(st, `[[`, character(1), "label")
ok(!identical(lab(lad), lab(nes)), "mode is honoured, not overridden")

## ladder rungs are single keys
lk <- Filter(Negate(is.null), lapply(lad, `[[`, "keys"))
ok(all(lengths(lk) == 1), "every ladder rung is exactly one key")
ok(identical(unlist(lk), keys[-1]),
   "and they appear in the order given, Stratum folded into own-stratum")

## nested rungs accumulate and always retain the LAST key - the drop
## order runs key 1 first now (read top to bottom, widen top to
## bottom), so the key picked last is the one kept the longest
nk <- Filter(Negate(is.null), lapply(nes, `[[`, "keys"))
ok(all(vapply(nk, function(k) keys[length(keys)] %in% k, logical(1))),
   "every nested rung retains the last key")

## ---- 2. Stratum is not offered twice ---------------------------------
##
## "own stratum" and a Stratum key are the same rung. Emitted twice, the
## second always resolves nothing because the first took everything.
ok(sum(lab(lad) == "Stratum") == 0,
   "a Stratum key collapses into the own-stratum rung")
ok(sum(lab(lad) == "own stratum") == 1, "which is present exactly once")
## without a self threshold there is no own-stratum rung, so Stratum
## must survive as a pool in its own right
nos <- raising_steps(keys, rep(50, 6), NA, "ladder", keys)
ok("Stratum" %in% lab(nos),
   "with no own-stratum bar, Stratum stays as its own rung")

## ---- 3. it resolves against real data --------------------------------
fx <- list.files(system.file("extdata", package = "rcef"),
                 "catches_CEF.*\\.csv$", recursive = TRUE, full.names = TRUE)
if (!length(fx)) {
  cat("no CEF fixture - skipping the data checks\n")
} else {
  cd <- rcef:::derive_fields(utils::read.csv(fx[1], stringsAsFactors = FALSE))

  ok(all(keys %in% names(cd)), "every key names a real column")
  ok("Stratum" %in% names(cd), "derive_fields() creates Stratum")
  ok(length(unique(cd$Stratum)) == nrow(unique(cd[, c("vesselFlagCountry",
       "fleetValue", "areaValue", "seasonValue")])),
     "Stratum is exactly the four native CEF keys")

  st <- classify_strata(build_strata(cd, keys), 50)
  rl <- recommend_raising(st, keys, rep(50, 6), 50, "ladder",
                          "Catch All by All", keys)
  ok(sum(is.na(rl$raise_step)) == 0, "every stratum is assigned a rung")

  tl <- decision_tree(rl, keys, rep(50, 6), 50, "ladder", keys,
                      cumulative = TRUE)
  ok(abs(utils::tail(tl$cum_pct_landings, 1) - 100) < 0.51,
     "the ladder accounts for all the landings")

  ## the substantive claim: intermediate rungs actually resolve strata.
  ## Under nested with Stratum first they all resolved ZERO and
  ## everything fell to the catch-all - the bug this mode exists for.
  mid <- tl[!tl$step %in% c("0", "Fallback"), ]
  ok(sum(mid$n_strata) > 0, "intermediate rungs resolve strata")
  ok(sum(mid$n_strata[mid$pool %in% c("metier4", "Country")]) > 0,
     "the fleet and country rungs both do work")

  rn <- recommend_raising(st, keys, rep(50, 6), 50, "nested",
                          "Catch All by All", keys)
  tn <- decision_tree(rn, keys, rep(50, 6), 50, "nested", keys,
                      cumulative = TRUE)
  fb <- function(t) t$n_strata[t$step == "Fallback"]
  # "ladder" mode existed because nested mode was broken by a Stratum-
  # first key list: Stratum, retained in every pool under the OLD drop
  # order (drop from the coarse end, keep key 1 throughout), meant every
  # nested rung WAS the stratum, so nothing widened and everything fell
  # to the catch-all. Reversing the drop order so key 1 is dropped FIRST
  # fixed exactly this - nested mode now cascades through metier4,
  # Country, areaValue, ICESSubArea, Season same as ladder does, and
  # both leave nothing to the catch-all on this fixture. Ladder no
  # longer has to be strictly better; it is enough that neither mode is
  # broken by Stratum being first any more.
  ok(fb(tl) <= fb(tn),
     "the ladder is never WORSE than nested here, now that nested is not broken")

  ## two resolutions of one family must be distinguishable
  fam <- c("Stratum", "metier6", "metier4")
  sf <- classify_strata(build_strata(cd, fam), 50)
  rf <- recommend_raising(sf, fam, rep(50, 3), 50, "ladder",
                          "Catch All by All", fam)
  tf <- decision_tree(rf, fam, rep(50, 3), 50, "ladder", fam)
  ok(all(c("metier6", "metier4") %in% tf$pool),
     "metier6 and metier4 are separate rungs, not one 'Fleet'")

  ## ---- 4. the vocabulary ---------------------------------------------
  gv <- grouping_vars(cd)
  ok(all(keys %in% gv), "grouping_vars() offers every key in the example")
  ok(!("catchCategory" %in% gv),
     "and withholds catchCategory, which build_strata() splits on")
  ok(!("total" %in% gv), "and the measured quantity")
  ok(identical(unname(gv[1]), "Stratum"), "Stratum is offered first")
  ok(length(unique(cd$Stratum)) > 400,
     "Stratum exceeds the level cap, so the exemption is load-bearing")
}



## ---- 5. the vocabulary must not be masked ----------------------------
##
## This was named group_vars() and dplyr exports one too. The app
## attaches dplyr, so inside the server the package's function was
## shadowed - grouping_vars() came back empty, every picker showed only
## "-- none --", and nothing errored to say why. A silent empty result
## is the worst failure shape available, so it gets its own assertion.
ok(!exists("group_vars", where = asNamespace("rcef"), inherits = FALSE),
   "the masked name is gone from the package")
ok(is.function(rcef::grouping_vars), "the vocabulary is exported unmasked")
if (requireNamespace("dplyr", quietly = TRUE)) {
  ok(!identical(rcef::grouping_vars, dplyr::group_vars),
     "and is not dplyr's function")
  ## the shape the app runs in: dplyr attached, rcef attached
  f <- get("grouping_vars", envir = asNamespace("rcef"))
  fx2 <- list.files(system.file("extdata", package = "rcef"),
                    "catches_CEF.*\\.csv$", recursive = TRUE, full.names = TRUE)
  if (length(fx2)) {
    cd2 <- rcef:::derive_fields(utils::read.csv(fx2[1], stringsAsFactors = FALSE))
    ok(length(f(cd2)) > 5,
       "and returns a real vocabulary with dplyr on the search path")
  }
}

cat("\nDONE ", FAIL, "failure(s)", "\n")
