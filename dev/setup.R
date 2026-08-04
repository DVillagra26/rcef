## ---------------------------------------------------------------
##  dev/setup.R -- working-tree development, no install needed.
##  Run once per session from the repo root.
## ---------------------------------------------------------------
if (!requireNamespace("pkgload", quietly = TRUE))
  stop("install.packages('pkgload')")

pkgload::load_all(".", export_all = FALSE)   # R/ as if installed
cat("rcef loaded from source.\n")
cat("  shiny::runApp('inst/app')   run the app against the working tree\n")
cat("  devtools::document()        regenerate NAMESPACE + man/\n")
cat("  Rscript dev/check_syntax.R  parse() every file\n")
