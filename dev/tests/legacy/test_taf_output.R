# --- locate the modules -------------------------------------------------
# The shipped suites each assumed a working directory and silently halted on
# line one when run from the other place - which then looked like "0 test
# failures". Resolve the folder instead of assuming it.
APP_DIR <- local({
  for (p in c(".", "R_development", file.path("..", "R_development")))
    if (file.exists(file.path(p, "0_Functions_utils.R"))) return(p)
  stop("cannot find 0_Functions_utils.R from ", normalizePath("."))
})
src_ <- function(f) source(file.path(APP_DIR, f))

ok <- TRUE
say <- function(...) cat(sprintf(...), "\n")
chk <- function(c, m) { if (!isTRUE(c)) { ok <<- FALSE; say("FAIL: %s", m) } else say("pass: %s", m) }
suppressMessages(library(dplyr))
stub <- function(...) invisible(TRUE); environment(stub) <- globalenv()
assign("library", stub, envir = globalenv()); assign("require", stub, envir = globalenv())
src_("0_Functions_utils.R")
eval(Filter(function(e) is.call(e) && identical(as.character(e[[2]]), "fmt_vals"),
            as.list(parse(file.path(APP_DIR, "0_Functions_groups.R"))))[[1]])
src_("0_Functions_taf.R")

## reproduce the prep block the biology coverage plot emits, exactly as the
## sprintf()s in app.R build it
dims <- c("catchCategory", "metier4", "Country")
prep <- c(
  sprintf("dims_biology <- %s", fmt_vals(dims)),
  sprintf('bl <- biology_link(catch_for_biology, distributions, dims_biology, %s, "%s")',
          taf_q("Lan"), "LengthTotal"),
  sprintf('ms <- stratum_measurements(catch_for_biology, distributions, dims_biology, %s, "%s")',
          taf_q("Lan"), "LengthTotal"),
  sprintf("bl <- add_sufficiency(bl, ms, %s, %s)", 50, 30),
  sprintf("%s <- bl %%>%% mutate(landings = weight,", "bio_sc"),
  "        landings_with_discards = weight_measured,",
  "        discards = 0, discard_rate = NA_real_)",
  sprintf('%s <- classify_strata(%s, %s, "%s", %s)', "bio_sc", "bio_sc", 50, "length", 30))
cat(paste(prep, collapse = "\n"), "\n\n")
chk(isTRUE(tryCatch({ parse(text = prep); TRUE }, error = function(e) conditionMessage(e))),
    "biology coverage prep block parses")
chk(any(grepl("%>%", prep, fixed = TRUE)), "the pipe survived sprintf escaping")

## and the whole kept block as taf_output() will lay it down
kept <- list(list(uid = 1, section = "model_explore_length_age",
  title = "Length coverage - bubbles",
  note = "Landings at 50% coverage / 30 measurements", stamp = "11:20:03",
  prep = prep,
  call = taf_call("plot_biology_coverage", list("bio_sc", type = '"bubbles"',
    y = taf_q("metier4"), fill = taf_q("Country"), facet = taf_q(NULL),
    threshold = 50, what = '"length"', weight_label = taf_q("landings"),
    min_meas = 30))))
## the biology figure now belongs to the biology exploration script
o <- taf_plot_blocks(kept, "model_explore_length_age")
chk(isTRUE(tryCatch({ parse(text = o); TRUE }, error = function(e) conditionMessage(e))),
    "the emitted figure block parses")
chk(any(grepl("bio_sc <- classify_strata", o, fixed = TRUE)), "prep travels with it")
chk(length(taf_plot_blocks(kept, "output")) == 0,
    "and it does not also appear in output.R")
chk(any(grepl('ggsave(file.path("output"', o, fixed = TRUE)),
    "but it is still saved into output/")
cat("\n--- the generated block ---\n")
i <- grep("^## ---- Figure 1", o); cat(paste(o[i:length(o)], collapse = "\n"), "\n")
cat("\n"); say("=== %s ===", if (ok) "ALL CHECKS PASSED" else "THERE WERE FAILURES")
cat("DONE test_taf_output\n")
