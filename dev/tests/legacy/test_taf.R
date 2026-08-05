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
chk <- function(cond, msg) { if (!isTRUE(cond)) { ok <<- FALSE; say("FAIL: %s", msg) }
                             else say("pass: %s", msg) }

## ---- 1. every file parses -------------------------------------------
fs <- list.files(APP_DIR, pattern = "\\.R$")
for (f in fs) {
  r <- tryCatch({ parse(file.path(APP_DIR, f)); TRUE }, error = function(e) conditionMessage(e))
  chk(isTRUE(r), sprintf("parse %s%s", f, if (isTRUE(r)) "" else paste0(" -> ", r)))
}

## ---- 2. paren / brace balance in app.R ------------------------------
depth_walk <- function(path) {
  txt <- readLines(path, warn = FALSE)
  d <- c(0, 0, 0); instr <- ""; esc <- FALSE; incom <- FALSE
  for (ln in txt) {
    incom <- FALSE
    for (ch in strsplit(ln, "")[[1]]) {
      if (esc) { esc <- FALSE; next }
      if (nzchar(instr)) {
        if (ch == "\\") { esc <- TRUE; next }
        if (ch == instr) instr <- ""
        next
      }
      if (incom) next
      if (ch %in% c('"', "'")) { instr <- ch; next }
      if (ch == "#") { incom <- TRUE; next }
      if (ch == "(") d[1] <- d[1] + 1 else if (ch == ")") d[1] <- d[1] - 1
      else if (ch == "{") d[2] <- d[2] + 1 else if (ch == "}") d[2] <- d[2] - 1
      else if (ch == "[") d[3] <- d[3] + 1 else if (ch == "]") d[3] <- d[3] - 1
    }
  }
  d
}
## Absolute zero is the wrong target here: the shipped app.R has always
## carried a +20 [ count that parse() is perfectly happy with, so the thing
## worth testing is that an edit did not CHANGE the balance.
d <- depth_walk(file.path(APP_DIR, "app.R"))
chk(identical(d, c(0, 0, 20)),
    sprintf("app.R bracket balance unchanged from baseline -> %s",
            paste(d, collapse = "/")))

## ---- 3. the TAF module ------------------------------------------------
## sourced with the siblings it leans on (%||%, fmt_vals) exactly as app.R
## sources them; on its own it would silently fall back to its error stub,
## which parses cleanly and would hide a real breakage.
suppressMessages(library(dplyr))
## The data modules attach plotting packages this suite does not need and
## the sandbox does not have. Stub the attach so a missing optional package
## cannot masquerade as a broken module.
local({
  stub <- function(...) invisible(TRUE)
  environment(stub) <- globalenv()
  assign("library", stub, envir = globalenv())
  assign("require", stub, envir = globalenv())
})
src_("0_Functions_utils.R")
## fmt_vals() lives in 0_Functions_groups.R, which also attaches ggalluvial
## for the river plots. Pulling out the one definition keeps this suite
## runnable without the plotting stack.
eval(Filter(function(e) is.call(e) && identical(as.character(e[[2]]), "fmt_vals"),
            as.list(parse(file.path(APP_DIR, "0_Functions_groups.R"))))[[1]])
src_("0_Functions_taf.R")
chk(exists("%||%") && exists("fmt_vals"), "the TAF module's dependencies are loaded")
chk(length(TAF_FILES) == 7, "seven TAF files")
chk(identical(names(TAF_FILES)[1], "data"), "data.R runs first")
chk(identical(unname(TAF_FILES[["report"]]), "report.R"), "report.R last")
chk(length(taf_parts_of("model")) == 4, "four model sub-scripts")
chk(all(names(TAF_FILES) %in% names(TAF_BLURB)), "every part has a blurb")
chk(all(names(TAF_FILES) %in% names(TAF_LABEL)), "every part has a label")

## ---- 4. a bundle from a realistic context ----------------------------
ctx <- list(
  external_dir = "/srv/ices", input_desc = "CEF_catches.csv",
  load_lines = 'catch_data <- read.csv("CEF_catches.csv")',
  derive_lines = "catch_data$Country <- catch_data$vesselFlagCountry",
  stock = "bll.27.3a47de", year = 2025,
  dims_disc = c("metier4", "Country", "ICESDivision", "Season"),
  fleet_level = "metier4", area_level = "ICESDivision",
  threshold = 60, dr_min = 0.01, dr_max = 0.9,
  group_lines = c("conds_a <- conds_from_auto(rec_a)",
                  "raised_a <- catchRaising(catch_data, conds_a$strataCond,",
                  "                         conds_a$matchedDataCond)"),
  compare_lines = "comparison <- bind_rows(raised_a)",
  final_lines = "catch_data_raised <- raised_a",
  dims_len = c("catchCategory", "metier4", "Country"),
  len_threshold = 50, len_min_meas = 30,
  age_threshold = 40, age_min_meas = 25,
  len_alloc_lines = "alloc_length <- alloc_res",
  age_alloc_lines = "alloc_age <- alloc_res")

kept <- list(
  list(uid = 1, section = "model_explore_discards",
       title = "Discard coverage - heatmap", note = "threshold 60%",
       stamp = "10:04:11",
       prep = "strata_cov <- classify_strata(build_strata(catch_data, dims), 60)",
       call = taf_call("plot_coverage", list("strata_cov", type = '"heatmap"',
              y = taf_q("metier4"), fill = taf_q("Season"),
              facet = taf_q(NULL), threshold = 60, what = '"discard"'))),
  list(uid = 2, section = "model_explore_discards",
       title = "Discard coverage - bubbles", note = "same, other view",
       stamp = "10:05:02",
       prep = "strata_cov <- classify_strata(build_strata(catch_data, dims), 60)",
       call = taf_call("plot_coverage", list("strata_cov", type = '"bubbles"',
              y = taf_q("metier4"), fill = taf_q("Country"),
              facet = taf_q(NULL), threshold = 60, what = '"discard"'))),
  list(uid = 3, section = "model_explore_length_age",
       title = "Length coverage - bubbles", note = "landings", stamp = "10:11:40",
       prep = "bio_sc <- classify_strata(bio_length, 50, \"length\", 30)",
       call = taf_call("plot_biology_coverage", list("bio_sc",
              type = '"bubbles"', y = taf_q("metier4"), threshold = 50,
              what = '"length"', min_meas = 30))),
  list(uid = 4, section = "output", title = "Raised catch composition",
       note = "adopted allocation: A", stamp = "10:31:07",
       prep = "d_fin <- derive_fields(catch_data_raised)",
       call = taf_call("plot_raised_composition",
                       list("d_fin", taf_q("Country"), taf_q(NULL)))))

b <- taf_bundle(ctx, kept)
chk(identical(names(b), names(TAF_FILES)), "bundle has all parts in run order")
for (p in names(b)) {
  r <- tryCatch({ parse(text = b[[p]]); TRUE }, error = function(e) conditionMessage(e))
  chk(isTRUE(r), sprintf("generated %s parses%s", TAF_FILES[[p]],
                         if (isTRUE(r)) "" else paste0(" -> ", r)))
}

## the interconnection the user asked for, checked as text
chk(any(grepl('saveRDS(catch_data, "data/catch_data.rds")', b$data, fixed = TRUE)),
    "data.R writes catch_data.rds")
chk(any(grepl('readRDS("data/catch_data.rds")', b$model_explore_discards, fixed = TRUE)),
    "explore discards reads data.R's output")
chk(any(grepl('model/strata_discards.rds', b$model_allocate_raise_discards, fixed = TRUE)),
    "allocate discards reads the strata")
chk(any(grepl('readRDS("model/catch_data_raised.rds")',
              b$model_allocate_length_age, fixed = TRUE)),
    "length/age allocation reads the RAISED catches")
chk(!any(grepl('readRDS("data/catch_data.rds")', b$model_allocate_length_age, fixed = TRUE)),
    "length/age allocation does NOT read the unraised catches")
## ---- figures land in the script that computes what they show ---------
chk(any(grepl("^p01 <- plot_coverage", b$model_explore_discards)),
    "discard coverage figure is in model_explore_discards.R")
chk(any(grepl("^p02 <- plot_coverage", b$model_explore_discards)),
    "the second cut of it is there too")
chk(any(grepl("^p03 <- plot_biology_coverage", b$model_explore_length_age)),
    "biology figure is in model_explore_length_age.R")
chk(any(grepl("^p04 <- plot_raised_composition", b$output)),
    "result figure is in output.R")
chk(!any(grepl("plot_coverage\\(", b$output)),
    "output.R no longer carries the exploratory coverage plots")
chk(!any(grepl("plot_raised_composition", b$model_explore_discards)),
    "and the explore script does not carry the result plot")

## every figure, wherever drawn, is written into output/
for (p in c("model_explore_discards", "model_explore_length_age", "output"))
  chk(any(grepl('ggsave(file.path("output"', b[[p]], fixed = TRUE)),
      sprintf("%s writes its figures to output/", TAF_FILES[[p]]))
## numbering is global, so two scripts cannot both claim 01_
nums <- unlist(lapply(b, function(x)
  regmatches(x, regexpr("^save_plot\\(p[0-9]+", x))))
chk(!any(duplicated(nums)) && length(nums) == 4,
    sprintf("figure numbering is unique across the set (%s)",
            paste(gsub("save_plot\\(", "", nums), collapse = " ")))
chk(length(b$report) > 5, "report.R is a stub, not blank")

## ---- data.R carries exactly one input route --------------------------
routes <- list(
  cef = list(load_method = "cef", load_lines = c('cefPath <- "/data"',
       'catch_data <- read.csv(file.path(cefPath, "catches.csv"))')),
  ic_exchange = list(load_method = "ic_exchange",
       load_lines = c("res <- funIntercatchCEF(dat_path = \"/ic\")",
                      "catch_data <- res$catches")),
  rdbes = list(load_method = "rdbes",
       load_lines = c("cef <- db_fetch_cef(stock = \"x\")",
                      "catch_data <- cef$catches")))
for (nm in names(routes)) {
  d <- taf_data(routes[[nm]])
  chk(isTRUE(tryCatch({ parse(text = d); TRUE }, error = function(e) FALSE)),
      sprintf("data.R for route '%s' parses", nm))
  chk(any(grepl(TAF_METHOD_LABEL[[nm]], d, fixed = TRUE)),
      sprintf("data.R for '%s' names its route", nm))
  others <- setdiff(names(routes), nm)
  chk(!any(vapply(others, function(o)
        any(grepl(routes[[o]]$load_lines[1], d, fixed = TRUE)), logical(1))),
      sprintf("data.R for '%s' carries no other route", nm))
}
## the hand-rolled .ices_lvl() shim is gone; the package function is used
d <- taf_data(routes$cef)
chk(!any(grepl(".ices_lvl", d, fixed = TRUE)),
    "data.R no longer defines its own ICES area splitter")
chk(any(grepl("catch_data <- derive_fields(catch_data)", d, fixed = TRUE)),
    "data.R calls the package's derive_fields() instead")

## ---- 5. concat and write ---------------------------------------------
cc <- taf_concat(b)
chk(sum(grepl("^## >>> FILE:", cc)) == 7, "concat labels all seven files")
chk(isTRUE(tryCatch({ parse(text = cc); TRUE }, error = function(e) FALSE)),
    "the concatenated set parses as one script")
td <- file.path(tempdir(), "tafout")
p <- taf_write(b, td)
chk(length(p) == 7 && all(file.exists(p)), "taf_write lays down seven files")
chk(identical(sort(basename(p)), sort(unname(TAF_FILES))), "written names match TAF_FILES")
sel <- c("data", "output")
p2 <- taf_write(b, file.path(tempdir(), "tafsel"), sel)
chk(length(p2) == 2, "taf_write honours a subset")
zf <- file.path(tempdir(), "t.zip")
mode <- taf_zip(b, names(TAF_FILES), zf)
chk(file.exists(zf) && file.size(zf) > 0, sprintf("taf_zip produced a file (%s)", mode))

## ---- 6. empty-session bundle still works -----------------------------
b0 <- taf_bundle(list(), list())
for (p in names(b0)) {
  r <- tryCatch({ parse(text = b0[[p]]); TRUE }, error = function(e) conditionMessage(e))
  chk(isTRUE(r), sprintf("empty-session %s parses%s", TAF_FILES[[p]],
                         if (isTRUE(r)) "" else paste0(" -> ", r)))
}
chk(any(grepl("No result figures were kept", b0$output)),
    "empty output.R says so")

## ---- 7. call builders -------------------------------------------------
chk(identical(taf_q(NULL), "NULL"), "taf_q(NULL)")
chk(identical(taf_q(""), "NULL"), "taf_q('')")
chk(identical(taf_q(NA), "NULL"), "taf_q(NA)")
chk(identical(taf_q("Season"), '"Season"'), "taf_q('Season')")
chk(identical(taf_call("f", list()), "f()"), "taf_call with no args")
chk(identical(taf_call("f", list(x = 1)), "f(x = 1)"), "taf_call one arg")
chk(length(taf_call("f", list("d", a = 1, b = 2))) == 3, "taf_call wraps")
chk(isTRUE(tryCatch({ parse(text = taf_call("f", list("d", a = 1, b = 2))); TRUE },
                    error = function(e) FALSE)), "wrapped call parses")

cat("\n"); say("=== %s ===", if (ok) "ALL CHECKS PASSED" else "THERE WERE FAILURES")
cat("DONE test_taf\n")
