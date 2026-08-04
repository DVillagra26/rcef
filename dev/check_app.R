## ---------------------------------------------------------------
##  dev/check_app.R
##
##  Sources inst/app/app.R against the INSTALLED package and renders the
##  full UI. This is the check that catches an object missing from the
##  NAMESPACE: check_exports.R proves the export list is complete,
##  this proves the app can actually be built from it.
##
##  Nothing is launched - shinyApp() only constructs an object.
##
##  Usage:  Rscript dev/check_app.R
## ---------------------------------------------------------------
d <- system.file("app", package = "rcef")
if (!nzchar(d)) stop("rcef is not installed; R CMD INSTALL first")

owd <- setwd(d); on.exit(setwd(owd))
e <- new.env(parent = globalenv())

r <- tryCatch({ sys.source("app.R", envir = e, keep.source = TRUE); TRUE },
              error = function(err) conditionMessage(err))
if (!isTRUE(r)) { cat("FAIL  app.R did not source:\n      ", r, "\n"); quit(status = 1L) }
cat("ok    app.R sources against the installed package\n")

for (o in c("ui", "server"))
  if (!exists(o, envir = e)) { cat("FAIL  no object '", o, "'\n"); quit(status = 1L) }
cat(sprintf("ok    ui is %s, server is %s\n",
            class(get("ui", envir = e))[1], class(get("server", envir = e))[1]))

h <- tryCatch(htmltools::doRenderTags(get("ui", envir = e)),
              error = function(err) { cat("FAIL  UI render:", conditionMessage(err), "\n")
                                      quit(status = 1L) })
cat(sprintf("ok    ui renders to %d chars of HTML\n", nchar(h)))
cat("\nDONE  0 failures\n")
