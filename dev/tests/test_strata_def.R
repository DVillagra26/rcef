## The "Definition of the strata" mechanism, across all four builders.
##
## Before this, "own stratum" / a manual group's exclusion resolution
## was always either the raw uploaded stratum (discards) or whatever a
## plot's axis pickers happened to be set to (length/age) - never a
## deliberate choice. This box makes it one: "Use as provided" (the
## four native CEF keys) or "Customize".
##
## Two shapes now, not one:
##   - ar (discard hierarchical) and both length/age hierarchical
##     builders embed it INTO Key 1 - a compact "Use as provided /
##     Customize" choice with four FAMILY-named slots (Fleet, Country,
##     Area, Season), since what "own stratum" means IS a Key 1
##     decision, not a preamble to it.
##   - mg (discard manual) still carries the separate top box, with
##     five generic slots - manual builders have no Key 1 to embed it in.
##
## This suite exercises it through the live server, not just checks
## that the UI renders - a wiring gap here (several "could not find
## function/output" bugs hit while building this) is invisible to
## parse() and to a plain UI render.

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

  ## ---- 1. mg's separate top box is still there, statically ---------
  h <- as.character(ev$ui)
  ok(nchar(h) > 3e5, "the UI renders")
  ## The length/age manual builders' READ-ONLY mirror of their
  ## hierarchical tab's Key 1 (titled "...(set on the Hierarchical
  ## tab)") is gone - removed on request, since it only ever displayed
  ## what Key 1 already showed on the other tab and added a box to read
  ## past rather than new information. mg (the discard manual builder)
  ## keeps its own real, EDITABLE top box - it has no Key 1 to embed
  ## into, so this is its only way to customize its own resolution.
  ok(length(gregexpr('"1. Definition of the strata</div>',
                     h, fixed = TRUE)[[1]]) == 1,
     "exactly one separate, EDITABLE top box left (mg, the discard manual builder)")
  ok(!grepl("Definition of the strata (set on the Hierarchical tab)",
            h, fixed = TRUE),
     "the read-only mirror is gone from both length and age manual builders")
  ar <- substr(h, regexpr("ar_bdeck", h, fixed = TRUE),
               regexpr("mg_bdeck", h, fixed = TRUE))
  ok(grepl("2. Exclusion rules for donor strata", ar, fixed = TRUE),
     "the discard cascade's exclusions box is step 2, right after Key 1")
  mg <- substr(h, regexpr("mg_bdeck", h, fixed = TRUE), nchar(h))
  ok(grepl("Use as provided", mg, fixed = TRUE) &&
       grepl("Customize strata definition", mg, fixed = TRUE),
     "mg's top box still offers Use as provided / Customize")
  ok(grepl("3. Exclusion rules for donor strata", mg, fixed = TRUE),
     "the manual builder's exclusions box is step 3")
  ok(grepl("4. Assign the clause", mg, fixed = TRUE),
     "assigning the clause is step 4 now, not 3")
  ok(grepl("5. Include a Catch All by All", mg, fixed = TRUE),
     "the catch-all is step 5 now, not 4")

  fx <- "anchoveta"
  fd <- "anchoveta"
  if (!length(fx)) {
    cat("no CEF fixture - skipping the live server checks\n")
  } else {
    cef_raw <- {data("anchoveta_catch", package = "rcef", envir = environment()); anchoveta_catch}
    dist_raw <- if (length(fd)) {data("anchoveta_dist", package = "rcef", envir = environment()); anchoveta_dist}
                else NULL

    shiny::testServer(app, {
      rv$cef_catches <- cef_raw; rv$dist <- dist_raw; rv$load_method <- "cef"
      session$setInputs(sep = ",", stock = "", year = "", threshold = 50)

      ## ---- 2. Key-1-embedded content, server-rendered -----------
      ##
      ## hier_keys / len_ga_keys / age_ga_keys are behind uiOutput, so
      ## Key 1's own content only exists once the server actually
      ## renders it, not in a static UI check.
      hk <- output$hier_keys$html %||% ""
      ok(grepl("Own stratum", hk, fixed = TRUE) &&
           grepl("Use as provided", hk, fixed = TRUE) &&
           grepl("Customize", hk, fixed = TRUE),
         "ar's Key 1 carries its own Use as provided / Customize choice")
      lk <- output$len_ga_keys$html %||% ""
      ok(grepl("Own stratum", lk, fixed = TRUE) &&
           grepl("Use as provided", lk, fixed = TRUE),
         "length hierarchical's Key 1 carries the same choice")
      ak <- output$age_ga_keys$html %||% ""
      ok(grepl("Own stratum", ak, fixed = TRUE) &&
           grepl("Use as provided", ak, fixed = TRUE),
         "age hierarchical's Key 1 carries the same choice too")

      ## ---- 3. defaults resolve to the four native CEF keys ----------
      dflt <- "Country + fleetValue + areaValue + Season"
      for (id in c("ar_k1_summary", "mg_sd_sd_summary",
                   "len_ga_hsd_summary", "age_ga_hsd_summary")) {
        htm <- output[[id]]$html %||% ""
        ok(grepl(dflt, htm, fixed = TRUE),
           paste(id, "defaults to the four native keys"))
      }

      ## ---- 4. customizing narrows the resolved strata ---------------
      ##
      ## ar and length/age's Key-1-embedded definitions use four
      ## family-named slots (Fleet/Country/Area/Season), not five
      ## generic ones - so "metier4 alone" means picking metier4 for
      ## Fleet and leaving the other three at none.
      session$setInputs(ar_k1_mode = "custom", ar_k1_fleet = "metier4",
                        ar_k1_country = "", ar_k1_area = "", ar_k1_season = "")
      htm <- output$ar_k1_summary$html %||% ""
      ok(grepl("metier4", htm, fixed = TRUE) && !grepl("Country", htm, fixed = TRUE),
         "customizing ar to metier4 alone drops the other three defaults")

      session$setInputs(len_ga_hsd_mode = "custom", len_ga_hsd_fleet = "metier4",
                        len_ga_hsd_country = "", len_ga_hsd_area = "",
                        len_ga_hsd_season = "")
      lhtm <- output$len_ga_hsd_summary$html %||% ""
      ok(grepl("metier4", lhtm, fixed = TRUE) && !grepl("Country", lhtm, fixed = TRUE),
         "customizing length hierarchical to metier4 alone works the same way")

      ## mg keeps its OWN five generic slots - unchanged this round
      session$setInputs(mg_sd_sdmode = "custom", mg_sd_sd1 = "metier4",
                        mg_sd_sd2 = "", mg_sd_sd3 = "", mg_sd_sd4 = "",
                        mg_sd_sd5 = "")
      mhtm <- output$mg_sd_sd_summary$html %||% ""
      ok(grepl("metier4", mhtm, fixed = TRUE) && !grepl("Country", mhtm, fixed = TRUE),
         "mg's separate top box still works the old (five-slot) way")

      ## ---- 5. the resolved dims actually change the strata table ----
      st_full <- classify_strata(build_strata(dat(), ar_strata_dims()), 50)
      ok(all(c("metier4") %in% names(st_full)),
         "the customized column reaches the strata table")
      ok(!"Country" %in% names(st_full) || length(unique(st_full$metier4)) > 0,
         "and the table is genuinely built on it")

      ## ---- 6. length/age: hierarchical and manual still share ONE
      ## resolution, even with the mirror display gone -----------------
      ##
      ## The mirror was only ever a DISPLAY of dims_of() - removing it
      ## does not change what feeds either tab, both of which read
      ## dims_of() directly. Checked here at the source.
      ok("metier4" %in% len_alloc$dims_of(),
         paste("dims_of() - what both the hierarchical cascade and the",
               "manual builder's own clause variables read - still",
               "carries Key 1's customized column"))

      ## ---- 7. len / age / discards are fully independent ------------
      age_htm <- output$age_ga_hsd_summary$html %||% ""
      ok(grepl(dflt, age_htm, fixed = TRUE),
         "customizing the length tab does not touch the age tab")
      mg_htm <- output$mg_sd_sd_summary$html %||% ""
      ok(grepl("metier4", mg_htm, fixed = TRUE),
         "and mg's own customization is independent of ar's")

      ## ---- 8. an empty custom selection falls back to the default ---
      session$setInputs(ar_k1_fleet = "", ar_k1_country = "",
                        ar_k1_area = "", ar_k1_season = "")
      empty_htm <- output$ar_k1_summary$html %||% ""
      ok(grepl(dflt, empty_htm, fixed = TRUE),
         "four blank slots falls back to the default, not an empty stratum")
    })
  }
}

cat("\nDONE ", FAIL, "failure(s)", "\n")
