# ============================================================
#  test_taf_gates.R
#  The Apply gate is pure reactive logic, so it is the one piece that
#  cannot be checked by reading the file. This runs it.
#
#  IMPORTANT: gate_of() and gated() are EXTRACTED FROM app.R, not copied
#  into this file. An earlier version of this suite carried its own copy
#  and passed green while app.R's gate_of() was missing the `live`
#  argument entirely - the app died on startup with "unused argument
#  (live = cov_live)" and the test never noticed, because it was testing
#  itself. A test that reimplements the thing it is testing tests nothing.
# ============================================================
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
chk <- function(c, m) { if (!isTRUE(c)) { ok <<- FALSE; say("FAIL: %s", m) }
                        else say("pass: %s", m) }
suppressMessages(library(shiny))

# ---- pull the two definitions out of the server, as written ----------
grab_defs <- function(path, names) {
  found <- list()
  walk <- function(e) {
    if (is.call(e) && length(e) >= 3 && identical(as.character(e[[1]]), "<-") &&
        is.name(e[[2]]) && as.character(e[[2]]) %in% names)
      found[[as.character(e[[2]])]] <<- e
    if (is.call(e) || is.pairlist(e))
      for (i in seq_along(e)) if (!identical(e[[i]], quote(expr = ))) walk(e[[i]])
  }
  for (e in as.list(parse(path))) walk(e)
  found
}
defs <- grab_defs(file.path(APP_DIR, "app.R"), c("gate_of", "gated"))
chk(length(defs) == 2, sprintf("found gate_of and gated in app.R (%s)",
                               paste(names(defs), collapse = ", ")))

# the signatures the app is actually calling them with
fml <- function(nm) names(as.list(eval(defs[[nm]][[3]], globalenv())))
chk("live" %in% fml("gate_of"), "gate_of() takes a `live` argument")
chk("live" %in% fml("gated"),   "gated() takes a `live` argument")
chk("refresh" %in% fml("gate_of"), "gate_of() takes a `refresh` argument")

app <- function(input, output, session) {
  rv <- reactiveValues(data_version = 0, result = NULL)
  draws <- reactiveVal(0)              # counts how often the plot was rebuilt

  # the real definitions, evaluated here so they close over input/rv
  for (e in defs) eval(e)

  # `type` stands for the plot-type toggle: NOT in the signature, passed
  # as `live` instead
  sig <- reactive(list(input$y, input$fill))
  lv  <- reactive(input$type)
  g   <- gate_of("cov", sig, refresh = reactive(rv$result), live = lv)
  obj <- gated(g, { draws(draws() + 1)
                    paste(input$type, input$y, input$fill) }, live = lv)

  exportTestValues(drawn = obj(), n = draws(), stale = g$stale())
}

testServer(app, {
  session$setInputs(type = "heat", y = "Fleet", fill = "Season", cov_apply = 0)

  # 1. the panel draws on arrival, without a press
  chk(identical(obj(), "heat Fleet Season"),
      "draws on arrival without pressing Apply")
  chk(draws() == 1, "drawn exactly once")
  chk(!isTRUE(g$stale()), "not stale on arrival")

  # 2. moving a control does NOT redraw
  session$setInputs(fill = "Country")
  n_before <- draws()
  chk(identical(obj(), "heat Fleet Season"),
      "a control change does not reach the plot before Apply")
  chk(identical(draws(), n_before), "and does not rebuild it")
  chk(isTRUE(g$stale()), "the panel reports itself stale")

  # 3. Apply picks the change up
  session$setInputs(cov_apply = 1)
  chk(identical(obj(), "heat Fleet Country"), "Apply picks up the change")
  chk(!isTRUE(g$stale()), "and clears the stale flag")

  # 4. a second press with nothing changed is a no-op for the output
  session$setInputs(cov_apply = 2)
  chk(identical(obj(), "heat Fleet Country"), "a redundant Apply changes nothing")

  # 5. NEW DATA must land without a press - the one exception, and getting
  #    it wrong makes a freshly loaded file appear not to load.
  #    (testServer only flushes on setInputs, so a bare reactiveValues write
  #    needs one asked for; in the app these happen inside observers and the
  #    framework flushes them.)
  session$setInputs(fill = "Area")
  rv$data_version <- 1
  session$flushReact()
  chk(identical(obj(), "heat Fleet Area"), "new data redraws without a press")
  chk(!isTRUE(g$stale()), "and leaves the panel up to date")

  # 6. a result the panel exists to show does the same
  session$setInputs(y = "Country")
  rv$result <- data.frame(a = 1)
  session$flushReact()
  chk(identical(obj(), "heat Country Area"),
      "a new result redraws without a press")

  # 7. THE PLOT TYPE BYPASSES THE GATE ENTIRELY
  session$setInputs(type = "bubbles")
  chk(identical(obj(), "bubbles Country Area"),
      "switching plot type redraws immediately, with no Apply")
  chk(!isTRUE(g$stale()), "and does not leave the panel looking stale")

  # 8. and it carries the frozen configuration with it, not a half-applied
  #    mixture of old type and new axis
  session$setInputs(fill = "Season")            # staged, not applied
  chk(isTRUE(g$stale()), "an axis edit after that is still gated")
  session$setInputs(type = "bars")
  chk(grepl("^bars ", obj()), "a type switch redraws at the new type")
  chk(!isTRUE(g$stale()), "and clears the stale flag, since it applied them")
})

cat("\n"); say("=== %s ===", if (ok) "ALL CHECKS PASSED" else "THERE WERE FAILURES")
cat("DONE test_taf_gates\n")
