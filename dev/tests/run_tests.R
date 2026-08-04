# ============================================================
#  run_tests.R
#  Run every suite and report honestly.
# ------------------------------------------------------------
#  WHY THIS EXISTS
#  Counting FAIL lines is not a test result. Three of the shipped suites
#  once reported "0 failures" while every one of them had halted on its
#  first line, because the working directory was wrong: no failures, no
#  passes, no output at all. A suite that dies produces exactly the same
#  FAIL count as a suite that passes.
#
#  So a suite counts as passing here only if it printed its DONE marker.
#  Anything else - an R error, a missing file, an early stop() - is a
#  failure, and the last few lines of its output are shown.
#
#  USAGE  Rscript run_tests.R            (from either the app root or
#                                         R_development)
# ============================================================

APP_DIR <- local({
  for (p in c(".", "R_development", file.path("..", "R_development")))
    if (file.exists(file.path(p, "0_Functions_utils.R"))) return(p)
  stop("cannot find 0_Functions_utils.R from ", normalizePath("."))
})

SUITES <- c(
  test_taf            = "the TAF script set: seven files, correct order, run in place",
  test_taf_wiring     = "app.R structure: slots, gates, sections, call signatures",
  test_taf_output     = "emitted figure blocks parse and carry their prep",
  test_taf_gates      = "the Apply gate, run as live reactive logic",
  test_manual_groups  = "manual rules partition the strata, in app AND in script",
  test_aesthetics     = "palette separation, stable colours, one outline",
  test_anchoveta_data = "the demo data actually exercises the workflow")

# the data suite needs its input; generate it if it is not there yet
data_dir <- local({
  for (p in c("test_data_anchoveta", file.path("..", "test_data_anchoveta")))
    if (dir.exists(p)) return(p)
  NA_character_
})
if (is.na(data_dir)) {
  cat("demo data not found - generating it first\n")
  system2("Rscript", c(file.path(APP_DIR, "make_test_data_anchoveta.R"),
                       "test_data_anchoveta"), stdout = NULL)
  data_dir <- "test_data_anchoveta"
}

res <- data.frame(suite = names(SUITES), status = NA_character_,
                  passes = NA_integer_, fails = NA_integer_,
                  stringsAsFactors = FALSE)

for (i in seq_along(SUITES)) {
  nm <- names(SUITES)[i]
  f <- file.path(APP_DIR, paste0(nm, ".R"))
  args <- c(f, if (nm == "test_anchoveta_data") data_dir else NULL)
  out <- suppressWarnings(system2("Rscript", args, stdout = TRUE, stderr = TRUE))
  done <- any(grepl(paste0("^DONE ", nm), out))
  nf <- sum(grepl("^FAIL", out)); np <- sum(grepl("^pass", out))
  res$passes[i] <- np; res$fails[i] <- nf
  # the marker is the load-bearing part: without it the suite did not finish,
  # whatever its FAIL count says
  res$status[i] <- if (!done) "DID NOT FINISH" else if (nf > 0) "FAILURES" else "ok"
  cat(sprintf("%-22s %-15s %3d passed, %d failed   %s\n", nm, res$status[i],
              np, nf, SUITES[[i]]))
  if (res$status[i] != "ok") {
    cat("   ---- last lines ----\n")
    cat(paste0("   ", tail(out, 12), collapse = "\n"), "\n")
  }
}

cat(sprintf("\n%d suites | %d checks passed | %d failed | %d did not finish\n",
            nrow(res), sum(res$passes), sum(res$fails),
            sum(res$status == "DID NOT FINISH")))
good <- all(res$status == "ok")
cat(if (good) "=== ALL SUITES PASSED ===\n" else "=== NOT CLEAN ===\n")
cat("DONE run_tests\n")
if (!good) quit(status = 1)
