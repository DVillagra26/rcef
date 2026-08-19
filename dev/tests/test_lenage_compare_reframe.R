## The length/age compare-allocation tab, reframed to match the discard
## side's framework: settings in a plain sidebarPanel on the left, no
## deck machinery, no filter box (there is nothing to filter a
## cross-allocation comparison BY that isn't already Facet / Facet 2 -
## each allocation's own strata definition already decided its data).

FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}

app <- system.file("app/app.R", package = "rcef")
if (nzchar(app)) {
  txt <- paste(readLines(app, warn = FALSE), collapse = "\n")
  ## the control_deck()/deck_page() machinery for this tab is gone
  o <- regexpr('control_deck(ns("cmp")', txt, fixed = TRUE)
  ok(o < 0, "control_deck() is no longer used for the compare tab")
  ok(grepl('apply_bar(ns("cmp"))', txt, fixed = TRUE),
     "apply_bar() drives the same gate id control_deck() used to")
  ok(grepl("Every saved allocation is re-run against the data currently",
           txt, fixed = TRUE),
     "the explanatory note survived the rewrite")
}

if (requireNamespace("shiny", quietly = TRUE) && nzchar(app)) {
  ev <- new.env(parent = globalenv())
  wd <- getwd(); setwd(dirname(app))
  suppressWarnings(suppressMessages(source(app, local = ev)))
  setwd(wd)
  h <- as.character(ev$ui)

  ## scope to the length compare tab specifically: from its facet-role
  ## picker (in the sidebar) through its last plot's ui wrapper
  i <- regexpr("len_rz_cmpr", h, fixed = TRUE)
  j <- regexpr("len_rz_cmpplotmw_ui", h, fixed = TRUE)
  ok(i > 0 && j > i, "the length compare tab renders")
  seg <- substr(h, i - 1000, j + 200)

  ok(grepl("col-sm-3", seg, fixed = TRUE),
     "settings sit in a 3-column sidebarPanel, same width as discards'")
  ok(grepl("simplecard", seg, fixed = TRUE),
     "styled consistently with the rest of the app's one-shot panels")
  ok(!grepl("Filter this figure", seg, fixed = TRUE),
     "no filter box - matches the discard side exactly")
  ok(!grepl("deckwrap", seg, fixed = TRUE),
     "no leftover deck machinery")
  ok(grepl("Run all allocations", seg, fixed = TRUE) &&
       grepl("btn-run", seg, fixed = TRUE),
     "the run action is present and coloured like every other run action")

  ## the age tab got the identical treatment (len_raise_ui() is shared)
  ai <- regexpr("age_rz_cmpr", h, fixed = TRUE)
  aj <- regexpr("age_rz_cmpplotmw_ui", h, fixed = TRUE)
  aseg <- substr(h, ai - 1000, aj + 200)
  ok(ai > 0 && aj > ai && grepl("col-sm-3", aseg, fixed = TRUE) &&
       !grepl("Filter this figure", aseg, fixed = TRUE),
     "the age compare tab got the same treatment (shared UI function)")
}

cat("\nDONE ", FAIL, "failure(s)", "\n")
