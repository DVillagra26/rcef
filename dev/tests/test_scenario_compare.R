## Item 2, finally reproduced and fixed: the compare-allocation crash.
##
## "Error in vapply: values must be length 1, but FUN(X[[1]]) result is
## length 0" - output$cmp_note, app.R. Root cause: scenario_auto() and
## scenario_manual() were still called with input$ar_fleet / input$ar_area
## / input$mg_fleet / input$mg_area - the old level-of-detail pickers,
## removed when the strata-definition box replaced them. Those inputs no
## longer exist anywhere, so they were always NULL, every saved scenario
## got fleet_level = NULL / area_level = NULL, and
## paste(x$fleet_level, x$area_level) is character(0) for a NULL/NULL
## pair - fed into vapply's length-1 contract, that is the crash exactly.
##
## dims was already being saved correctly (from ar_dims()/mg_dims()) and
## carries the same information more completely, so fleet_level/area_level
## are removed rather than repaired.

FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}

## ---- 1. the exact failure mode, reproduced at the R level -------------
bad <- list(dims = "metier4")  ## fleet_level/area_level absent, as a
                                ## scenario built the OLD way would be
r <- tryCatch(paste(bad$fleet_level, bad$area_level), error = function(e) e)
ok(is.character(r) && length(r) == 0,
   "confirms the mechanism: paste(NULL, NULL) is character(0), not an error -")
ok(length(tryCatch(vapply(list(bad), function(x)
      paste(x$fleet_level, x$area_level), character(1)),
      error = function(e) "caught")) == 1 &&
   inherits(tryCatch(vapply(list(bad), function(x)
      paste(x$fleet_level, x$area_level), character(1)),
      error = function(e) e), "error"),
   "so vapply(..., character(1)) throws exactly this error on it")

## ---- 2. scenario_auto() / scenario_manual() no longer have the field -
sc <- scenario_auto("A", c("metier4", "Country"), c("metier4", "Country"),
                    c(50, 50))
ok(!"fleet_level" %in% names(sc) && !"area_level" %in% names(sc),
   "scenario_auto() no longer carries fleet_level/area_level")
ok(identical(sc$dims, c("metier4", "Country")),
   "dims is still saved correctly - it always was")

scm <- scenario_manual("B", c("metier4"), list(), list())
ok(!"fleet_level" %in% names(scm) && !"area_level" %in% names(scm),
   "nor does scenario_manual()")

## ---- 3. the comparison itself: dims-based, and length-1 always --------
scenarios <- list(A = sc, B = scenario_auto("C", c("metier4"), c("metier4"), 50))
keys <- vapply(scenarios, function(x) paste(sort(x$dims), collapse = ","),
               character(1))
ok(length(keys) == length(scenarios),
   "the dims-based comparison key is always length 1 per scenario")
ok(length(unique(keys)) == 2,
   "and correctly detects that these two scenarios differ")

same <- list(A = sc, B = scenario_auto("D", c("Country", "metier4"),
                                       c("metier4"), 50))
keys2 <- vapply(same, function(x) paste(sort(x$dims), collapse = ","),
                character(1))
ok(length(unique(keys2)) == 1,
   "and treats the same dims in a different order as the same definition")

## ---- 4. the app no longer references the old inputs --------------------
app <- system.file("app/app.R", package = "rcef")
if (nzchar(app)) {
  txt <- paste(readLines(app, warn = FALSE), collapse = "\n")
  for (bad_ref in c("input$ar_fleet", "input$ar_area",
                    "input$mg_fleet", "input$mg_area"))
    ok(!grepl(bad_ref, txt, fixed = TRUE),
       paste("the dead input reference is gone:", bad_ref))
  ok(grepl('paste(sort(x$dims), collapse = ",")', txt, fixed = TRUE),
     "cmp_note compares on dims instead")
}

cat("\nDONE ", FAIL, "failure(s)", "\n")
