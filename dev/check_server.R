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

cat(sprintf("\nDONE  %d failure(s)\n", fail))
if (fail) quit(status = 1L)
