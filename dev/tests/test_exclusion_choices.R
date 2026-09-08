## The "exclude a specific stratum" dropdown.
##
## It rendered as an empty list. exclusion_server() is handed two
## functions - strata_of() and dims_of() - and they have to agree:
## exclusion_choices() asks stratum_metrics() to group the strata table
## BY dims, so any dim that is not a column of that table yields nothing.
##
## They had drifted. strata_of was strata_p(), which aggregates to
## panel_dims() - the plot role inputs. A builder has no plot roles, so
## panel_dims() fell back to DEF_DIMS while dims_of() returned the
## cascade keys. The table came back without Stratum, areaValue or
## ICESSubArea, and the dropdown was empty with no error.
##
## Nothing about that is visible in the source, so this is checked by
## running the server.

FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}

fx <- "anchoveta"

if (!requireNamespace("shiny", quietly = TRUE) || !length(fx)) {
  cat("shiny or the CEF fixture unavailable - skipping\n")
} else {
  ## ---- 1. the pure function is fine on matching dims ----------------
  cd <- {data("anchoveta_catch", package = "rcef", envir = environment()); anchoveta_catch}
  keys <- c("Stratum", "metier4", "Country", "areaValue",
            "ICESSubArea", "Season")
  st <- classify_strata(build_strata(cd, keys), 50)
  ok(length(exclusion_choices(st, keys, order_by = "weight")) == nrow(st),
     "one choice per stratum when the table carries the dims")

  ## and WRONG when they do not. Not empty - that is what made this hard
  ## to spot. stratum_metrics() groups by whichever dims it can find, so
  ## a table built on the wrong columns still yields a plausible-looking
  ## list, just of the wrong strata at the wrong resolution, and picking
  ## one excludes something the engine never sees.
  st2 <- classify_strata(build_strata(cd, c("metier4", "Country")), 50)
  wrong <- exclusion_choices(st2, keys, order_by = "weight")
  ok(length(wrong) != nrow(st),
     "a mismatched table gives a different list, not an error")
  ok(length(wrong) > 0,
     "and not an empty one either - it fails silently and plausibly")

  ## ---- 2. the app wires them consistently ---------------------------
  ev <- new.env(parent = globalenv())
  ap <- system.file("app/app.R", package = "rcef")
  wd <- getwd(); setwd(dirname(ap))
  suppressWarnings(suppressMessages(source(ap, local = ev)))
  setwd(wd)
  app <- shiny::shinyApp(ev$ui, ev$server)
  ## NOT called `raw`: testServer() evaluates its expression inside the
  ## server environment, where `raw` is one of the app's own reactives.
  ## Assigning rv$cef_catches <- raw there stores the FUNCTION, derived()
  ## then fails its column check, and every output comes back empty -
  ## which looks exactly like the bug under test.
  cef_raw <- {data("anchoveta_catch", package = "rcef", envir = environment()); anchoveta_catch}

  shiny::testServer(app, {
    rv$cef_catches <- cef_raw; rv$load_method <- "cef"
    session$setInputs(sep = ",", stock = "", year = "", threshold = 50,
                      hier_2 = "metier4", hier_3 = "Country",
                      hier_4 = "areaValue", hier_5 = "ICESSubArea",
                      hier_6 = "Season", thr_1 = c(50, 100),
                      ar_xsort = "weight", mg_xsort = "weight",
                      ca_xsort = "weight")

    nopt <- function(id) {
      htm <- output[[id]]$html %||% ""
      length(regmatches(htm, gregexpr("<option ", htm))[[1]])
    }
    for (id in c("ar_xui", "mg_xui", "ca_xui"))
      ok(nopt(id) > 0, paste(id, "offers strata to exclude"))

    ## The list must be one entry per stratum at the CASCADE's
    ## resolution. dat() cannot be called directly here - it runs
    ## validate(), which throws outside a render context - so this is
    ## read off the rendered output instead.
    n1 <- nopt("ar_xui")
    ok(n1 > 400,
       "the cascade list is at stratum resolution, not a coarser one")

    ## Dropping keys must not empty or coarsen the list. The COUNT does
    ## not change, and should not: Stratum is always in ar_dims() and
    ## already identifies every stratum, so the coarser keys add columns
    ## to group by without splitting anything further. That is the whole
    ## point of building strata as "the uploaded stratum plus whatever
    ## the keys name" - what a key changes is the POOLS, not the strata.
    session$setInputs(hier_4 = "", hier_5 = "", hier_6 = "")
    n2 <- nopt("ar_xui")
    ok(n2 > 0, "still populated after dropping keys")
    ok(n2 == n1,
       "and still one entry per uploaded stratum - keys change pools, not strata")
  })
}

cat("\nDONE ", FAIL, "failure(s)", "\n")
