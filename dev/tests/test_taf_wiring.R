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
src <- readLines(file.path(APP_DIR, "app.R"), warn = FALSE)
one <- paste(src, collapse = "\n")

## ---- every capture-enabled slot has a keep_plot registration ---------
## The failure this guards against is the quiet one: a button rendered with
## no observer behind it looks fine and does nothing when pressed.
##
## Matching on text does not work here. Both sides name the same control
## through a local ns() closure, but the UI factory and the server function
## spell it differently - ns("plot") in one, paste0(p, "_cov_plot") in the
## other - and they resolve to the same string only once p and ns are bound.
## So the ids are RESOLVED against a placeholder prefix, using whichever
## ns() the enclosing function defines, and compared as strings.
resolve_ids <- function(path, target, id_at = 2L, filter = NULL) {
  out <- character(0)
  walk <- function(e, ns_fun) {
    if (!is.call(e) && !is.pairlist(e)) return(invisible(NULL))
    # a local  ns <- function(x) paste0(...)  shadows anything outer
    if (is.call(e) && identical(as.character(e[[1]]), "{"))
      for (st in as.list(e)[-1])
        if (is.call(st) && length(st) >= 3 && identical(as.character(st[[1]]), "<-") &&
            is.name(st[[2]]) && identical(as.character(st[[2]]), "ns"))
          ns_fun <- eval(st[[3]], list2env(list(p = "<P>", src = "<P>"),
                                           parent = globalenv()))
    if (is.call(e) && is.name(e[[1]]) && identical(as.character(e[[1]]), target)) {
      keep <- is.null(filter) || isTRUE(filter(e))
      if (keep) {
        env <- list2env(list(p = "<P>", src = "<P>",
                             ns = if (is.null(ns_fun)) function(x) x else ns_fun),
                        parent = globalenv())
        v <- tryCatch(eval(as.list(e)[[id_at]], env),
                      error = function(err) paste(deparse(as.list(e)[[id_at]]),
                                                  collapse = ""))
        out <<- c(out, as.character(v))
      }
    }
    for (i in seq_along(e))
      if (!identical(e[[i]], quote(expr = ))) walk(e[[i]], ns_fun)
    invisible(NULL)
  }
  # a function body carries its own ns(); start each top-level definition fresh
  for (e in as.list(parse(path))) walk(e, NULL)
  out
}

slot_ids <- resolve_ids(file.path(APP_DIR, "app.R"), "plot_slot", 2L, function(e) {
  cap <- as.list(e)[["capture"]]
  !is.null(cap) && isTRUE(cap) })
reg_ids <- resolve_ids(file.path(APP_DIR, "app.R"), "keep_plot", 2L)

chk(length(slot_ids) == 9, sprintf("nine plots offer capture (found %s)",
                                   length(slot_ids)))
chk(length(reg_ids) == 9, sprintf("nine keep_plot registrations (found %s)",
                                  length(reg_ids)))
say("slots     : %s", paste(sort(slot_ids), collapse = " | "))
say("registered: %s", paste(sort(reg_ids), collapse = " | "))
chk(identical(sort(slot_ids), sort(reg_ids)),
    "every capture slot is registered, and every registration has a slot")
chk(!any(duplicated(reg_ids)), "no control is registered twice")

## ---- the TAF tab renders and carries every id ------------------------
suppressMessages({
  library(shiny); library(htmltools)
  stub <- function(...) invisible(TRUE); environment(stub) <- globalenv()
  assign("library", stub, envir = globalenv())
  assign("require", stub, envir = globalenv())
})
src_("0_Functions_utils.R")
eval(Filter(function(e) is.call(e) && identical(as.character(e[[2]]), "fmt_vals"),
            as.list(parse(file.path(APP_DIR, "0_Functions_groups.R"))))[[1]])
src_("0_Functions_taf.R")
src_("app_ui_helpers.R")

## pull the UI-side definitions out of app.R without starting the server
exprs <- as.list(parse(file.path(APP_DIR, "app.R")))
want <- c("copy_btn", "taf_ui", "kept_box", "log_ui", "plot_slot", "plot_tools")
for (e in exprs) {
  if (is.call(e) && length(e) >= 3 && identical(as.character(e[[1]]), "<-") &&
      is.name(e[[2]]) && as.character(e[[2]]) %in% want) eval(e)
}
chk(all(vapply(want, exists, logical(1))), "UI factories extracted")

for (p in names(TAF_FILES)) {
  h <- tryCatch(doRenderTags(taf_ui(p)), error = function(e) "")
  chk(grepl(paste0("taf_", p, "_txt"), h, fixed = TRUE),
      sprintf("taf_ui(%s) has its text output", p))
  chk(grepl(paste0("dl_taf_", p), h, fixed = TRUE),
      sprintf("taf_ui(%s) has its download button", p))
}
## The kept-plot manager moved to the whole-set view: kept plots now go to
## three different scripts, so a list living on one file's tab would be
## describing other files' contents.
h_out <- doRenderTags(taf_ui("output"))
chk(!grepl("kept_list", h_out, fixed = TRUE),
    "the output tab no longer owns the kept list")

h_log <- doRenderTags(log_ui())
for (id in c("taf_parts", "dl_taf_sel", "dl_taf_all", "dl_log", "log_txt",
             "log_dir", "log_write_now", "dl_state", "up_state",
             "kept_list", "kept_clear"))
  chk(grepl(id, h_log, fixed = TRUE), sprintf("log_ui carries %s", id))
chk(!grepl("log_name", h_log, fixed = TRUE),
    "the dead single-file name box is gone from log_ui")

## the server must not still reference the input that the UI stopped creating
chk(!grepl("input\\$log_name", one), "no server reference to input$log_name left")
chk(!grepl("log_path\\(\\)", one), "no reference to the removed log_path() left")

## ---- toolbar renders both buttons ------------------------------------
h_t <- doRenderTags(plot_slot("demo", capture = TRUE))
chk(grepl("demo_keep", h_t, fixed = TRUE), "toolbar has the keep button")
chk(grepl("demo_png", h_t, fixed = TRUE), "toolbar has the save-image button")
chk(grepl("demo_ui", h_t, fixed = TRUE), "toolbar keeps the plot slot itself")
h_p <- doRenderTags(plot_slot("plain"))
chk(!grepl("plain_keep", h_p, fixed = TRUE), "capture is off by default")

## ---- the script tab structure ----------------------------------------
chk(grepl('tabPanel("Live script (full)"', one, fixed = TRUE), "Live script (full) tab")
for (nm in c('tabPanel("data"', 'tabPanel("model"', 'tabPanel("output"',
             'tabPanel("report"'))
  chk(grepl(nm, one, fixed = TRUE), sprintf("%s) tab", nm))
chk(grepl('tabsetPanel(id = "sec6m"', one, fixed = TRUE),
    "model has its own sub-tabset")
chk(length(gregexpr('taf_ui\\("', one)[[1]]) == 7, "seven taf_ui() calls in the tab tree")

## ---- apply gates: a bar without a gate is a dead button --------------
bars  <- resolve_ids(file.path(APP_DIR, "app.R"), "apply_bar", 2L)
gates <- resolve_ids(file.path(APP_DIR, "app.R"), "gate_of",   2L)
say("apply bars : %s", paste(sort(bars), collapse = " | "))
say("gates      : %s", paste(sort(gates), collapse = " | "))
chk(identical(sort(bars), sort(gates)),
    "every Apply bar has a gate, and every gate has a bar")
chk(!any(duplicated(bars)), "no panel has two Apply bars")
chk(length(bars) == 9, sprintf("nine gated panels (found %s)", length(bars)))

## the note output each bar renders must be filled by gate_of()
chk(grepl('uiOutput(paste0(p, "_apply_note")', one, fixed = TRUE),
    "apply_bar renders the staleness note")
chk(grepl('output[[paste0(p, "_apply_note")]] <- renderUI(', one, fixed = TRUE),
    "gate_of fills it")

## ---- one input route at a time ---------------------------------------
for (id in c("run_ic", "cef_load_dir", "db_request"))
  chk(grepl(sprintf("observeEvent(input$%s, with_load_guard(", id), one, fixed = TRUE),
      sprintf("%s goes through the load guard", id))
chk(grepl("observeEvent(input$file, with_load_guard(", one, fixed = TRUE),
    "the catches upload goes through it too")
chk(grepl("input$load_replace_ok", one, fixed = TRUE), "the confirm button is wired")
chk(grepl("clear_all_data()", one, fixed = TRUE), "confirming clears the old data")
## the four routes must be the four the emitter knows about
chk(all(c("ic_exchange", "ic_output", "cef", "rdbes") %in% names(TAF_METHOD_LABEL)),
    "the emitter knows all four routes")
for (m in c('"ic_output"', '"ic_exchange"', '"cef"', '"rdbes"'))
  chk(grepl(m, one, fixed = TRUE), sprintf("route %s is claimed somewhere", m))

## ---- sufficiency sits under the icons, and not on the bars view ------
chk(grepl("input.cov_type != 'bars' && input.cov_fill != ''", one, fixed = TRUE),
    "discard sufficiency is conditional on a non-bars view")
chk(grepl('sprintf("input.%s != \'bars\'", ns("type"))', one, fixed = TRUE),
    "biology sufficiency is conditional on a non-bars view")
## and it must come AFTER the plot picker in the sidebar
cov_i <- grep('uiOutput("cov_type_bar")', src, fixed = TRUE)[1]
thr_i <- grep('threshold_box("threshold"', src, fixed = TRUE)[1]
chk(!is.na(cov_i) && !is.na(thr_i) && thr_i > cov_i,
    "discard sufficiency is below the plot icons")
pick_i <- grep('pick_bar(ns("type"),', src, fixed = TRUE)[1]
bthr_i <- grep('threshold_box(paste0(p, "_thr_cov")', src, fixed = TRUE)[1]
chk(!is.na(pick_i) && !is.na(bthr_i) && bthr_i > pick_i,
    "biology sufficiency is below the plot icons")

## ---- kept plots know which script they go to -------------------------
## Counted off the parsed calls, not off the text: the default in
## keep_plot()'s own signature is also `section = "output"`, and a text
## count silently includes it and then reports the wrong total.
secs <- character(0)
for (e in as.list(parse(file.path(APP_DIR, "app.R")))) {
  wk <- function(x) {
    if (is.call(x) && is.name(x[[1]]) && identical(as.character(x[[1]]), "keep_plot")) {
      sc <- as.list(x)[["section"]]
      secs <<- c(secs, if (is.null(sc)) "output" else as.character(sc))
    }
    if (is.call(x) || is.pairlist(x))
      for (i in seq_along(x)) if (!identical(x[[i]], quote(expr = ))) wk(x[[i]])
  }
  wk(e)
}
chk(length(secs) == 9, sprintf("all nine registrations name a section (%s)",
                               length(secs)))
chk(all(secs %in% names(TAF_FILES)),
    sprintf("every section is a real TAF part (%s)",
            paste(unique(secs), collapse = ", ")))
chk(sum(secs == "model_explore_discards") == 2, "two discard-exploration plots")
chk(sum(secs == "model_explore_length_age") == 3, "three biology-exploration plots")
chk(sum(secs == "output") == 4, "four result plots")

## ---- the toolbar is an overlay, not a bar in the flow ---------------
chk(grepl('class = if (isTRUE(capture)) "plotwrap has-tools"', one, fixed = TRUE),
    "a capture slot gets the positioning class")
chk(grepl(".plottools{position:absolute", one, fixed = TRUE),
    "and the toolbar is absolutely positioned")
chk(grepl("right:2px", one, fixed = TRUE), "in the top-right corner")
chk(grepl(".plotwrap.has-tools{position:relative}", one, fixed = TRUE),
    "with the wrapper as its positioning context")

## ---- switching plot TYPE must bypass the gate ------------------------
## The failure mode is silent: leave the type in the signature and the
## picture stops changing when you press Bubbles, which reads as a bug in
## the plot rather than as the gate doing its job.
lives <- resolve_ids(file.path(APP_DIR, "app.R"), "gate_of", 2L, function(e)
  !is.null(as.list(e)[["live"]]))
chk(length(lives) == 4,
    sprintf("four panels let a setting bypass the gate (found %s)", length(lives)))
for (v in c("cov_live <- reactive(input$cov_type)",
            "dr_live <- reactive(input$dr_type)",
            'cov_live <- reactive(i_("cov_type"))',
            'n_live <- reactive(i_("n_type"))'))
  chk(grepl(v, one, fixed = TRUE), sprintf("live type defined: %s", v))
## and it must NOT also sit in the frozen signature, or it would be gated
chk(!grepl("input$cov_type, input$threshold", one, fixed = TRUE),
    "cov_type is out of the discard coverage signature")
chk(!grepl("list(input$dr_type,", one, fixed = TRUE),
    "dr_type is out of the discard rate signature")
chk(!grepl('i_("cov_type"), i_("thr_cov")', one, fixed = TRUE),
    "cov_type is out of the biology coverage signature")
chk(!grepl('i_("n_cc"), i_("n_type")', one, fixed = TRUE),
    "n_type is out of the biology numbers signature")

## ---- compare-allocations tab ----------------------------------------
chk(grepl('c("Fleet", ""), stacked = TRUE)', one, fixed = TRUE),
    "the comparison X axis defaults to Fleet, so a plot shows at once")
cmp_chip <- grep('role_chips("cmp"', src, fixed = TRUE)[1]
cmp_app  <- grep('apply_bar("cmp")', src, fixed = TRUE)[1]
cmp_plot <- grep('plot_slot("cmp_plot"', src, fixed = TRUE)[1]
chk(!is.na(cmp_app) && cmp_app > cmp_chip,
    "its Apply sits below the axis chips, as on the exploration tabs")
chk(cmp_app < cmp_plot, "and in the sidebar, not above the plot")
chk(length(grep('apply_bar("cmp")', src, fixed = TRUE)) == 1,
    "there is only one of it")

## ---- the manual decision ladder --------------------------------------
for (id in c("man_ladder", "man_check", "man_decision", "man_river_d"))
  chk(grepl(id, one, fixed = TRUE), sprintf("manual ladder piece present: %s", id))
chk(grepl('output[[paste0("man_river_", j)]] <- renderPlot', one, fixed = TRUE),
    "each rule gets its own partial river")
chk(grepl("rule_decision_table(man_strata()", one, fixed = TRUE),
    "the ladder reads the effective decision table")
chk(grepl("rule_river_data(man_strata()", one, fixed = TRUE),
    "and the rivers read the same rule assignment")

## ---- donor-exclusion wording ----------------------------------------
chk(sum(grepl("Drop specific stratum/strata out of the donor group based",
              src, fixed = TRUE)) == 2,
    "both donor-exclusion notes use the new wording")
chk(!any(grepl("Or drop a named stratum outright", src, fixed = TRUE)),
    "and the old wording is gone")

## ---- every call matches the callee's formals -------------------------
## The bug this exists for: gate_of() was called with `live = ...` in nine
## places while its own definition had no such argument. Every textual
## check passed - the calls were there, the ids matched, the sections were
## right - and the app died on startup with "unused argument (live =
## cov_live)". Nothing that reads app.R as TEXT can catch that; it needs
## the call compared against the definition.
local({
  defs <- list()
  calls <- list()
  walk <- function(e, depth = 0) {
    if (is.call(e) && length(e) >= 3 && identical(as.character(e[[1]]), "<-") &&
        is.name(e[[2]]) && is.call(e[[3]]) &&
        identical(as.character(e[[3]][[1]]), "function")) {
      nm <- as.character(e[[2]])
      # ALL definitions of the name are kept. Short local helpers get
      # redefined in several scopes with different arities - row() and rd()
      # both exist twice - and this walk cannot tell which scope a call is
      # in. Accepting a call that matches ANY definition keeps the check
      # sound (it still catches an argument no version accepts) at the cost
      # of missing a mismatch that another scope's version would allow.
      defs[[nm]] <<- c(defs[[nm]], list(names(as.list(e[[3]][[2]]))))
    }
    if (is.call(e) && is.name(e[[1]]))
      calls[[length(calls) + 1]] <<- e
    if (is.call(e) || is.pairlist(e))
      for (i in seq_along(e)) if (!identical(e[[i]], quote(expr = ))) walk(e[[i]])
  }
  for (e in as.list(parse(file.path(APP_DIR, "app.R")))) walk(e)

  bad <- character(0)
  for (cl in calls) {
    fn <- as.character(cl[[1]])
    fs <- defs[[fn]]
    if (is.null(fs)) next                     # not one of app.R's own
    if (any(vapply(fs, function(f) "..." %in% f, logical(1)))) next
    given <- names(as.list(cl)[-1])
    if (is.null(given)) next
    given <- given[nzchar(given)]
    if (!length(given)) next
    okc <- any(vapply(fs, function(f) !length(setdiff(given, f)), logical(1)))
    if (!okc)
      bad <- c(bad, sprintf("%s(%s) - no definition of %s() accepts that; formals: %s",
                            fn, paste(given, collapse = ", "), fn,
                            paste(vapply(fs, paste, character(1), collapse = "/"),
                                  collapse = " | ")))
  }
  bad <- unique(bad)
  chk(length(bad) == 0,
      sprintf("every named argument matches its function's formals%s",
              if (length(bad)) paste0(":\n       ",
                                      paste(bad, collapse = "\n       ")) else ""))

  ## and the positional count is not over the limit either
  over <- character(0)
  for (cl in calls) {
    fn <- as.character(cl[[1]]); fs <- defs[[fn]]
    if (is.null(fs)) next
    if (any(vapply(fs, function(f) "..." %in% f, logical(1)))) next
    n <- length(as.list(cl)) - 1
    if (!any(vapply(fs, function(f) n <= length(f), logical(1))))
      over <- unique(c(over, sprintf("%s: %d args, widest definition takes %d",
                                     fn, n,
                                     max(vapply(fs, length, integer(1))))))
  }
  chk(length(over) == 0,
      sprintf("no call passes more arguments than its function accepts%s",
              if (length(over)) paste0(": ", paste(over, collapse = "; ")) else ""))
  say("      checked %s calls against %s local definitions",
      length(calls), length(defs))
})

cat("\n"); say("=== %s ===", if (ok) "ALL CHECKS PASSED" else "THERE WERE FAILURES")
cat("DONE test_taf_wiring\n")
