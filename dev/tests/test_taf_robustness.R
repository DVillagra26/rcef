## Script robustness: two real gaps found by actually RUNNING the
## exported TAF scripts as subprocesses, not just checking that
## taf_bundle() produces non-empty text.
##
## 1. model_explore_length_age.R used ONE shared dims_biology for both
##    length and age, even though the app lets them be fully independent
##    (own_key_strata_server() runs separately for "len" and "age").
##    Traced to the ctx builder reading dims_len from
##    panel_dims("len_lv") - a "level of detail" panel that never
##    existed anywhere in the app - so it silently used a hardcoded
##    fallback regardless of what was actually configured, and there was
##    no dims_age field at all.
##
## 2. model_allocate_length_age.R legitimately refuses to fall back to
##    unraised data (documented in its own header: allocating over
##    unraised catch silently drops the whole discard fraction), but
##    failed with a raw "cannot open the connection" when
##    catch_data_raised.rds did not exist yet - true, but not why.

FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}

## ---- 1. length and age get independent dims in the emitted script ----
ctx <- list(load_method = "cef",
           dims_len = c("catchCategory", "metier4", "vesselFlagCountry"),
           dims_age = c("catchCategory", "areaValue", "seasonValue"))
lines <- taf_model_explore_length_age(ctx)
ok(any(grepl('dims_biology_length <- c("catchCategory", "metier4", "vesselFlagCountry")',
             lines, fixed = TRUE)),
   "length's own dims are emitted under their own name")
ok(any(grepl('dims_biology_age    <- c("catchCategory", "areaValue", "seasonValue")',
             lines, fixed = TRUE)),
   "age's own dims are emitted separately, not reused from length")
ok(any(grepl("dims_biology_length", lines) &
        grepl('type = "LengthTotal"', lines)),
   "the length block reads dims_biology_length")
ok(!any(grepl("dims_biology,", lines, fixed = TRUE)),
   "the old single shared dims_biology variable is gone")

## falls back to dims_len when dims_age was never supplied, not to a
## hardcoded default that could disagree with what the user configured
ctx2 <- list(load_method = "cef", dims_len = c("catchCategory", "metier5"))
lines2 <- taf_model_explore_length_age(ctx2)
ok(any(grepl('dims_biology_age    <- c("catchCategory", "metier5")',
             lines2, fixed = TRUE)),
   "with no dims_age supplied, age falls back to dims_len - not a separate hardcoded default")

## ---- 2. a missing raised-catch file fails with a clear message -------
lines3 <- taf_model_allocate_length_age(list())
ok(any(grepl('if (!file.exists("model/catch_data_raised.rds"))',
             lines3, fixed = TRUE)),
   "the script checks for the file before trying to read it")
ok(any(grepl("Run a Final raising", lines3, fixed = TRUE)),
   "and explains what to do about it, not just that the file is missing")
## the emitted stop() call is itself syntactically valid R with correctly
## escaped nested quotes - the exact class of bug that has bitten this
## codebase before (CSS strings ending early on an unescaped quote)
ok(isTRUE(tryCatch({ parse(text = paste(lines3, collapse = "\n")); TRUE },
                   error = function(e) FALSE)),
   "the whole emitted script, including the new stop() call, parses as valid R")

## ---- 3. actually run it: the file-missing path fails cleanly ---------
if (nzchar(Sys.which("Rscript"))) {
  d <- tempfile(); dir.create(d)
  old <- getwd(); on.exit(setwd(old), add = TRUE)
  setwd(d)
  writeLines(lines3, "model_allocate_length_age.R")
  res <- tryCatch(
    suppressWarnings(system2("Rscript", "model_allocate_length_age.R",
                             stdout = TRUE, stderr = TRUE, stdin = "")),
    error = function(e) character(0))
  ok(any(grepl("Run a Final raising", res, fixed = TRUE)),
     "running the script for real produces the clear message, not a raw readRDS error")
  ok(!any(grepl("cannot open the connection", res, fixed = TRUE)),
     "and not the confusing gzfile/connection error underneath it")
}

## ---- 4. no stale reference to the removed fleet_level/area_level -----
app <- system.file("app/app.R", package = "rcef")
if (nzchar(app)) {
  txt <- paste(readLines(app, warn = FALSE), collapse = "\n")
  ok(!grepl('dims_len = tc(unique(c("catchCategory", panel_dims("len_lv"))))',
            txt, fixed = TRUE),
     "the ctx builder no longer contains the broken len_lv expression itself")
  ok(grepl("dims_len = tc(len_alloc$dims_of())", txt, fixed = TRUE),
     "dims_len is built from the length module's own live dims_of()")
  ok(grepl("dims_age = tc(age_alloc$dims_of())", txt, fixed = TRUE),
     "and dims_age from the age module's own - independently")
}
taf_txt <- paste(readLines(system.file("R", "rcef_taf.R", package = "rcef",
                                       mustWork = FALSE) %||%
                           "R/rcef_taf.R", warn = FALSE), collapse = "\n")
ok(!grepl("fleet_level", taf_txt, fixed = TRUE) &&
     !grepl("ctx$area_level", taf_txt, fixed = TRUE),
   "no stale fleet_level/area_level reference left in the TAF emitters")

cat("\nDONE ", FAIL, "failure(s)", "\n")
