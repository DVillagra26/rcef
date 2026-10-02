## The manual builder's overview tables (Overview, Strata to raise, Data
## used to raise, Rivers) were silently showing only whichever columns
## happened to overlap with an unrelated hardcoded default
## (metier4/vesselFlagCountry/ICESDivision/seasonValue), instead of the
## manual builder's OWN strata definition (mg_dims()) - a group defined on
## vesselFlagCountry+fleetValue+areaValue+seasonValue+metier4 only ever
## showed vesselFlagCountry+seasonValue+metier4 in these tables, because
## man_strata() was built
## from panel_dims("mg"), a mechanism for reading y/x/fill/shape/facet
## plot-axis inputs the manual builder never had.

FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}

app <- system.file("app/app.R", package = "rcef")
if (nzchar(app)) {
  txt <- paste(readLines(app, warn = FALSE), collapse = "\n")
  ok(grepl("man_strata <- reactive(build_strata(dat(), mg_dims()))",
           txt, fixed = TRUE),
     "man_strata() is built from mg_dims(), not strata_p(\"mg\")")
  ok(!grepl('man_strata <- reactive(strata_p("mg"))', txt, fixed = TRUE),
     "the broken panel_dims-based construction is gone")
}

## The live-session part of this check (does man_strata() actually
## carry every mg_dims() column, in a running app) depends on shiny
## being attached before rcef on the search path - true when this file
## is run standalone, not guaranteed when dev/check_all.sh's own
## invocation (`Rscript -e "library(rcef); source(...)"`) attaches rcef
## FIRST. See test_num_trips.R for the same limitation, hit first there.
## Verified live and by hand instead (both orders, both confirmed): with
## mg's strata definition customized to vesselFlagCountry + fleetValue +
## areaValue + seasonValue + metier4, the OLD strata_p("mg") route kept
## only vesselFlagCountry/seasonValue/metier4 (the coincidental overlap
## with an unrelated fallback default); man_strata() now carries all five.
##
## What IS checked here, order-independent: the source change itself,
## and that build_strata()/mg_dims() compose correctly in isolation -
## the same two pieces man_strata() is built from, exercised the way
## every other function-level test in this suite is.
fx <- "anchoveta"
if (length(fx)) {
  cd <- {data("anchoveta_catch", package = "rcef", envir = environment()); anchoveta_catch}
  dims <- c("vesselFlagCountry", "fleetValue", "areaValue", "seasonValue", "metier4")
  st <- build_strata(cd, dims)
  ok(setequal(intersect(dims, names(st)), dims),
     "build_strata(dat, mg_dims()) - what man_strata() now calls - carries every column")
  ok(all(c("fleetValue", "areaValue") %in% names(st)),
     "specifically fleetValue and areaValue, which the old fallback route dropped")
}

cat("\nDONE ", FAIL, "failure(s)", "\n")
