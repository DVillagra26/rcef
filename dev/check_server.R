## ---------------------------------------------------------------
##  dev/check_server.R
##
##  RUNS the server function. check_app.R only builds the UI, which is
##  why it passed while the app died on launch: a module server called a
##  helper that was defined a thousand lines further down, the server
##  function aborted, and every definition after that point never ran.
##  Nothing that only renders tags can see that.
##
##  This starts a real session, lets the reactive graph settle, and fails
##  on any error raised while the server body executes.
##
##  Usage:  Rscript dev/check_server.R
## ---------------------------------------------------------------
for (pk in c("shiny", "DT", "rcef"))
  if (!requireNamespace(pk, quietly = TRUE))
    stop("check_server.R needs ", pk)

d <- system.file("app", package = "rcef")
if (!nzchar(d)) stop("rcef is not installed; R CMD INSTALL first")

fail <- 0L
say <- function(good, what, extra = "") {
  if (good) cat("ok    ", what, "\n")
  else { fail <<- fail + 1L; cat("FAIL  ", what, "\n", extra, "\n") }
}

res <- tryCatch({
  shiny::testServer(d, {
    session$flushReact()
    ## the helpers a module server calls directly while the body runs
    for (fn in c("exclusion_server", "flt_inputs", "explore_dat",
                 "filter_server", "excl_num", "dmap", "strata_p",
                 "strata_x", "dat"))
      if (!exists(fn, inherits = TRUE))
        stop("helper not defined in the server body: ", fn)
    TRUE
  })
  TRUE
}, error = function(e) conditionMessage(e))

say(isTRUE(res), "the server function runs to completion", res)

## ---------------------------------------------------------------
##  The filter, end to end, on a real CEF table.
##
##  Not "the box renders" - that a filter narrows the FIGURE data and
##  leaves the RAISING data alone, and that changing it makes the gate
##  go stale so Apply has something to apply. Every one of those was
##  wrong at some point and nothing caught it.
## ---------------------------------------------------------------
fx <- list.files(system.file("extdata", package = "rcef"),
                 pattern = "catches_CEF.*\\.csv$", recursive = TRUE,
                 full.names = TRUE)
if (!length(fx)) {
  cat("skip   no CEF fixture installed; filter round-trip not run\n")
} else {
  res2 <- tryCatch({
    shiny::testServer(d, {
      rv$cef_catches <- utils::read.csv(fx[1], stringsAsFactors = FALSE)
      session$flushReact()
      dd <- dat()
      lv <- expf_levels()
      if (!length(lv)) stop("no filterable columns in the fixture")
      v   <- names(lv)[1]
      one <- lv[[v]][1]

      before <- cov_sig()
      n_all  <- nrow(dd)
      n_x0   <- nrow(explore_dat("cov"))

      rv$flt[["cov"]] <- stats::setNames(list(one), v)
      session$flushReact()

      if (!identical(flt_inputs("cov"), clean_filters(stats::setNames(list(one), v))))
        stop("flt_inputs did not pick the filter up from rv$flt")
      n_x1 <- nrow(explore_dat("cov"))
      if (!(n_x1 < n_x0)) stop("the filter did not narrow explore_dat()")
      if (nrow(dat()) != n_all) stop("the filter reached dat() - it must not")
      if (identical(cov_sig(), before))
        stop("cov_sig() did not change, so Apply can never redraw")
      if (nrow(strata_x("cov")) >= nrow(strata_p("cov")))
        stop("strata_x() is not narrower than strata_p()")

      rv$flt[["cov"]] <- list()
      session$flushReact()
      if (nrow(explore_dat("cov")) != n_x0) stop("clearing the filter did not restore")
      TRUE
    })
    TRUE
  }, error = function(e) conditionMessage(e))
  say(isTRUE(res2),
      "a filter narrows the figure data, not the raising data, and moves the gate",
      res2)
}

cat(sprintf("\nDONE  %d failure(s)\n", fail))
if (fail) quit(status = 1L)
