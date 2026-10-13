## The strata are always the strata AS REPORTED.
##
## There used to be a "Definition of the strata" mechanism - a top box on
## the discard manual builder and a "Use as provided / Customize" widget
## inside Key 1 of the three hierarchical builders - that let a builder
## regroup them. It is gone: a stratum that changes with a screen's
## settings made every coverage bar, exclusion and group mean something
## different from one screen to the next. A stratum is vesselFlagCountry +
## fleetValue + areaValue + seasonValue, everywhere, discard or length/age.
##
## Exercised through the live server, not just a UI render - a wiring gap
## here is invisible to parse() and to a plain render.

source("dev/tests/fixture.R")
FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}

if (!requireNamespace("shiny", quietly = TRUE)) {
  cat("shiny unavailable - skipping\n")
} else {
  ev <- new.env(parent = globalenv())
  ap <- system.file("app/app.R", package = "rcef")
  if (!nzchar(ap)) stop("rcef is not installed; R CMD INSTALL first")
  wd <- getwd(); setwd(dirname(ap))
  suppressWarnings(suppressMessages(source(ap, local = ev)))
  setwd(wd)
  app <- shiny::shinyApp(ev$ui, ev$server)
  h <- as.character(ev$ui)
  ok(nchar(h) > 3e5, "the UI renders")

  ## ---- 1. the definition widgets are gone --------------------------
  ok(!grepl("Definition of the strata", h, fixed = TRUE),
     "no 'Definition of the strata' box anywhere")
  ok(!grepl("Customize strata definition", h, fixed = TRUE) &&
     !grepl("mg_sd_sdmode", h, fixed = TRUE),
     "the discard manual builder no longer offers Use as provided / Customize")
  ar <- substr(h, regexpr("ar_bdeck", h, fixed = TRUE),
               regexpr("mg_bdeck", h, fixed = TRUE))
  mg <- substr(h, regexpr("mg_bdeck", h, fixed = TRUE), nchar(h))

  ## ---- 2. the decks renumber around the removed step 1 -------------
  ok(grepl("2. Exclusion rules for donor strata", ar, fixed = TRUE),
     "the discard cascade's exclusions box is step 2, right after Key 1")
  for (k in c("1. Define the groups", "2. Exclusion rules for donor strata",
              "3. Assign the clause", "4. Include a Catch All by All"))
    ok(grepl(k, mg, fixed = TRUE), paste("the manual builder states:", k))
  ok(!grepl("5. Include a Catch All", mg, fixed = TRUE),
     "the catch-all is step 4 now, not 5")

  ## ---- 3. the exclusion box lost its aggregated list and pool summary
  ok(!grepl("As aggregated", h, fixed = TRUE) &&
     !grepl("As submitted", h, fixed = TRUE),
     "one exclusion list (strata as reported), not an aggregated/submitted pair")
  ok(!grepl("Summary of data that will be used to raise", h, fixed = TRUE),
     "the always-recomputing 'used to raise' summary is gone")
  ## the ids the UI declares must be the ones the server renders into
  ## (paste0(p, "_xlist") / "_xui") - a mismatch here means the picks are
  ## kept but never shown, which is exactly what shipped once
  for (id in c("mg_xui", "mg_xlist", "ar_xui", "ar_xlist", "ca_xui", "ca_xlist",
               "len_ga_x_xui", "len_ga_x_xlist", "age_ga_x_xui", "age_ga_x_xlist"))
    ok(grepl(sprintf('id="%s"', id), h, fixed = TRUE),
       paste("the exclusion picker UI slot exists:", id))

  test_catch <- fx_catch()
  test_dist <- fx_dist()
  DEF <- c("vesselFlagCountry", "fleetValue", "areaValue", "seasonValue")

  shiny::testServer(app, {
    rv$cef_catches <- test_catch; rv$dist <- test_dist
    rv$load_method <- "cef"
    session$setInputs(sep = ",", stock = "", year = "", threshold = 50,
                      hier_2 = "seasonValue")

    ## ---- 4. Key 1 says what it is, with nothing to change ----------
    hk <- output$hier_keys$html %||% ""
    ok(grepl("Own stratum", hk, fixed = TRUE) &&
         grepl("Strata as reported", hk, fixed = TRUE) &&
         !grepl("Use as provided", hk, fixed = TRUE) &&
         !grepl("Customize", hk, fixed = TRUE),
       "the discard cascade's Key 1 states the reported strata, no choice")
    lk <- output$len_ga_keys$html %||% ""
    ok(grepl("Strata as reported", lk, fixed = TRUE) &&
         !grepl("Use as provided", lk, fixed = TRUE),
       "the length hierarchical builder's Key 1 is the same")
    ak <- output$age_ga_keys$html %||% ""
    ok(grepl("Strata as reported", ak, fixed = TRUE) &&
         !grepl("Use as provided", ak, fixed = TRUE),
       "... and the age one")

    ## ---- 5. every builder's strata ARE the reported ones -----------
    ok(identical(as_reported_strata(), DEF),
       "the strata resolve to vesselFlagCountry + fleetValue + areaValue + seasonValue")
    ok(identical(ar_strata_dims(), DEF) && identical(mg_strata_dims(), DEF),
       "the discard cascade and the discard manual builder use them")
    ## a cascade key or a clause column is ADDED to the table, it does not
    ## replace what a stratum is
    ok(all(DEF %in% ar_dims()), "the cascade's strata still contain the reported four")
    ok(all(DEF %in% mg_dims()), "the manual builder's strata do too")
  })
}

cat("\nDONE ", FAIL, "failure(s)", "\n")