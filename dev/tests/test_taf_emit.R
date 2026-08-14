## The seven TAF emitters, exercised end to end.
##
## This suite exists because of a bug it would have caught. `taf_header()`
## was called seven times across rcef_taf.R and was never defined - a gap
## check_all.sh never saw, because nothing in the active suite called the
## TAF emitters (only dev/tests/legacy/test_taf.R did, and that suite has
## been unrunnable since before this package existed: it sources
## "0_Functions_utils.R" from a pre-package layout that no longer exists,
## so it stop()s on line one before reaching any TAF assertion).
##
## taf_bundle() wraps every part in tryCatch(), so the missing function
## did not crash anything - it silently replaced every downloaded script
## with "This script could not be generated from the current session."
## That is the failure mode to guard against here: not an error, a
## plausible-looking wrong answer.
##
## This is a deliberately SMALL port, not a line-for-line copy of the
## legacy suite. The legacy ctx (external_dir, load_lines, group_lines,
## ...) predates the current taf_data(ctx) signature (input_desc, stock,
## year, meth) enough that porting it verbatim would assert against
## fields the current implementation never reads - failures for the
## wrong reason, which is worse than no test at all. taf_parts_of() and
## TAF_STAGE, which the legacy suite also touched, were confirmed to
## have zero callers anywhere in R/ or inst/app.R and were removed as
## dead code (see dev/tests/legacy/README.md).

FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}

parses <- function(x) isTRUE(tryCatch({ parse(text = x); TRUE },
                                       error = function(e) FALSE))

## ---- 1. the constants agree ------------------------------------------
ok(length(TAF_FILES) == 7, "seven TAF files")
ok(identical(names(TAF_FILES)[1], "data"), "data.R runs first")
ok(identical(unname(TAF_FILES[["report"]]), "report.R"), "report.R last")
ok(all(names(TAF_FILES) %in% names(TAF_BLURB)), "every part has a blurb")
ok(all(names(TAF_FILES) %in% names(TAF_LABEL)), "every part has a label")

## ---- 2. every emitter runs and returns real code ----------------------
##
## The substantive check: not just "does it error", but "did it fall
## back to the tryCatch stub". A part that errors is loud; a part that
## silently apologises is the one that got past check_all.sh before.
ctx <- list(input_desc = "the CEF files", stock = "bll.27.3a47de",
           year = "2024", meth = "cef")
b <- taf_bundle(ctx, kept = list())

ok(identical(names(b), names(TAF_FILES)), "bundle has all parts in run order")
for (p in names(b)) {
  ok(parses(b[[p]]), sprintf("generated %s parses", TAF_FILES[[p]]))
  ok(!any(grepl("could not be generated", b[[p]], fixed = TRUE)),
     sprintf("%s is real content, not the error fallback", TAF_FILES[[p]]))
}
ok(length(b$report) > 5, "report.R is a stub, not blank")

## ---- 3. taf_header() itself -------------------------------------------
h <- taf_header("data", "Read and derive the fields",
                reads = "catches_CEF.csv", writes = "data/catch_data.rds",
                notes = "a note")
ok(is.character(h) && length(h) > 0, "taf_header returns lines")
ok(any(grepl("data.R", h, fixed = TRUE)), "names the script")
ok(any(grepl("Read and derive the fields", h, fixed = TRUE)), "states the purpose")
ok(any(grepl("catches_CEF.csv", h, fixed = TRUE)), "lists what it reads")
ok(any(grepl("data/catch_data.rds", h, fixed = TRUE)), "lists what it writes")
## reads/writes/notes are all optional
ok(parses(paste(taf_header("x", "y"), collapse = "\n")),
   "taf_header with no reads/writes/notes still produces valid R comments")

## ---- 4. concat and write -----------------------------------------------
cc <- taf_concat(b)
ok(sum(grepl("^## >>> FILE:", cc)) == 7, "concat labels all seven files")
ok(parses(cc), "the concatenated set parses as one script")

td <- file.path(tempdir(), paste0("tafout_", as.integer(Sys.time())))
p <- taf_write(b, td)
ok(length(p) == 7 && all(file.exists(p)), "taf_write lays down seven files")
ok(identical(sort(basename(p)), sort(unname(TAF_FILES))),
   "written names match TAF_FILES")

p2 <- taf_write(b, file.path(tempdir(), "tafsel"), c("data", "output"))
ok(length(p2) == 2, "taf_write honours a subset")

zf <- file.path(tempdir(), "t.zip")
mode <- tryCatch(taf_zip(b, names(TAF_FILES), zf), error = function(e) NA)
ok(!is.na(mode) && file.exists(zf) && file.size(zf) > 0,
   sprintf("taf_zip produced a file (%s)", mode %||% "NA"))

## ---- 5. the empty-session bundle still works ---------------------------
##
## No data loaded, nothing kept - the state the app is in before anyone
## has done anything. This must not be where a missing function or a
## divide-by-zero hides.
b0 <- taf_bundle(list(), list())
for (p in names(b0)) {
  ok(parses(b0[[p]]), sprintf("empty-session %s parses", TAF_FILES[[p]]))
  ok(!any(grepl("could not be generated", b0[[p]], fixed = TRUE)),
     sprintf("empty-session %s is not the error fallback", TAF_FILES[[p]]))
}

## ---- 6. call builders ---------------------------------------------------
ok(identical(taf_q(NULL), "NULL"), "taf_q(NULL)")
ok(identical(taf_q(""), "NULL"), "taf_q('')")
ok(identical(taf_q(NA), "NULL"), "taf_q(NA)")
ok(identical(taf_q("Season"), '"Season"'), "taf_q('Season')")
ok(identical(taf_call("f", list()), "f()"), "taf_call with no args")
ok(identical(taf_call("f", list(x = 1)), "f(x = 1)"), "taf_call one arg")
ok(parses(paste(taf_call("f", list("d", a = 1, b = 2)), collapse = "\n")),
   "a wrapped multi-arg call still parses")

cat("\nDONE ", FAIL, "failure(s)", "\n")
