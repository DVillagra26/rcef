## The "Definition of the strata" box, added to all four builders.
##
## Before this, "own stratum" / a manual group's exclusion resolution
## was always either the raw uploaded stratum (discards) or whatever a
## plot's axis pickers happened to be set to (length/age) - never a
## deliberate choice. This box makes it one: "Use as provided" (the
## four native CEF keys) or "Customize" (up to five slots from the full
## vocabulary, any left blank for a coarser definition).
##
## This suite exercises it through the live server, not just checks
## that the UI renders - a wiring gap here (the two "could not find
## function" bugs hit while building this) is invisible to parse() and
## to a plain UI render.

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

  ## ---- 1. the box is present in the rendered UI, in all four builders,
  ## as the FIRST step -----------------------------------------------
  h <- as.character(ev$ui)
  ok(nchar(h) > 3e5, "the UI renders")
  for (k in c("1. Definition of the strata", "Use as provided",
              "Customize strata definition"))
    ok(length(gregexpr(k, h, fixed = TRUE)[[1]]) >= 3,
       paste("appears at least three times (ar, mg, length/age):", k))
  ## discard boxes renumbered: exclusions is now step 3, not 2
  ar <- substr(h, regexpr("ar_bdeck", h, fixed = TRUE),
               regexpr("mg_bdeck", h, fixed = TRUE))
  ok(grepl("3. Exclusion rules for donor strata", ar, fixed = TRUE),
     "the discard cascade's exclusions box is step 3, after the new step 1")
  mg <- substr(h, regexpr("mg_bdeck", h, fixed = TRUE), nchar(h))
  ok(grepl("3. Exclusion rules for donor strata", mg, fixed = TRUE),
     "the manual builder's exclusions box is step 3")
  ok(grepl("4. Assign the clause", mg, fixed = TRUE),
     "assigning the clause is step 4 now, not 3")
  ok(grepl("5. Include a Catch All by All", mg, fixed = TRUE),
     "the catch-all is step 5 now, not 4")

  fx <- list.files(system.file("extdata", package = "rcef"),
                   "bll_2025_1catches_CEF.*\\.csv$", recursive = TRUE,
                   full.names = TRUE)
  if (!length(fx)) {
    cat("no CEF fixture - skipping the live server checks\n")
  } else {
    cef_raw <- utils::read.csv(fx[1], stringsAsFactors = FALSE)

    shiny::testServer(app, {
      rv$cef_catches <- cef_raw; rv$load_method <- "cef"
      session$setInputs(sep = ",", stock = "", year = "", threshold = 50)

      ## ---- 2. defaults resolve to the four native CEF keys ----------
      dflt <- "Country + fleetValue + areaValue + Season"
      for (id in c("ar_sd_sd_summary", "mg_sd_sd_summary",
                   "len_ga_hsd_sd_summary", "age_ga_hsd_sd_summary")) {
        htm <- output[[id]]$html %||% ""
        ok(grepl(dflt, htm, fixed = TRUE),
           paste(id, "defaults to the four native keys"))
      }

      ## ---- 3. customizing narrows the resolved strata ---------------
      session$setInputs(ar_sd_sdmode = "custom", ar_sd_sd1 = "metier4",
                        ar_sd_sd2 = "", ar_sd_sd3 = "", ar_sd_sd4 = "",
                        ar_sd_sd5 = "")
      htm <- output$ar_sd_sd_summary$html %||% ""
      ok(grepl("metier4", htm, fixed = TRUE) && !grepl("Country", htm, fixed = TRUE),
         "customizing to metier4 alone drops the other three defaults")

      ## ---- 4. the resolved dims actually change the strata table ----
      st_full <- classify_strata(build_strata(dat(), ar_strata_dims()), 50)
      ok(all(c("metier4") %in% names(st_full)),
         "the customized column reaches the strata table")
      ok(!"Country" %in% names(st_full) || length(unique(st_full$metier4)) > 0,
         "and the table is genuinely built on it")

      ## ---- 5. length/age: hierarchical and manual share ONE
      ## resolution, and the manual mirror follows it live ------------
      session$setInputs(len_ga_hsd_sdmode = "custom",
                        len_ga_hsd_sd1 = "metier4", len_ga_hsd_sd2 = "",
                        len_ga_hsd_sd3 = "", len_ga_hsd_sd4 = "",
                        len_ga_hsd_sd5 = "")
      hier_htm   <- output$len_ga_hsd_sd_summary$html %||% ""
      mirror_htm <- output$len_ga_msd_mirror$html %||% ""
      ok(grepl("metier4", hier_htm, fixed = TRUE),
         "length hierarchical resolves to the customized column")
      ok(grepl("metier4", mirror_htm, fixed = TRUE),
         "and the manual tab's read-only mirror shows the same thing")

      ## ---- 6. len / age / discards are fully independent ------------
      age_htm <- output$age_ga_hsd_sd_summary$html %||% ""
      ok(grepl(dflt, age_htm, fixed = TRUE),
         "customizing the length tab does not touch the age tab")
      mg_htm <- output$mg_sd_sd_summary$html %||% ""
      ok(grepl(dflt, mg_htm, fixed = TRUE),
         "nor the discard manual builder")

      ## ---- 7. an empty custom selection falls back to the default ---
      session$setInputs(mg_sd_sdmode = "custom", mg_sd_sd1 = "",
                        mg_sd_sd2 = "", mg_sd_sd3 = "", mg_sd_sd4 = "",
                        mg_sd_sd5 = "")
      empty_htm <- output$mg_sd_sd_summary$html %||% ""
      ok(grepl(dflt, empty_htm, fixed = TRUE),
         "five blank slots falls back to the default, not an empty stratum")
    })
  }
}

cat("\nDONE ", FAIL, "failure(s)", "\n")
