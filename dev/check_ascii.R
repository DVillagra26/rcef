## ---------------------------------------------------------------
##  dev/check_ascii.R
##
##  R CMD build on a non-UTF-8 Windows locale dies with an opaque
##  "inferior call failed" the moment a source or vignette file carries a
##  character outside ASCII - an em-dash, an arrow, a box-drawing glyph.
##  It built fine here (UTF-8) and failed on the user's machine, which is
##  the worst way to find out. This fails loudly, in the sandbox, naming
##  the file and the bytes.
##
##  .rds and other binaries are skipped; everything meant to be read as
##  text must be 7-bit clean.
##
##  Usage:  Rscript dev/check_ascii.R
## ---------------------------------------------------------------
files <- list.files(c("R", "inst/app", "vignettes", "man"),
                    pattern = "\\.(R|r|Rmd|md|Rd)$", full.names = TRUE,
                    recursive = TRUE)
files <- c(files, "DESCRIPTION", "NAMESPACE")
files <- files[file.exists(files)]

fail <- 0L
for (f in files) {
  raw <- readBin(f, "raw", file.info(f)$size)
  hi <- which(as.integer(raw) > 127)
  if (length(hi)) {
    fail <- fail + 1L
    # first offending line for a useful pointer
    txt <- readLines(f, warn = FALSE, encoding = "UTF-8")
    bad <- which(vapply(txt, function(l)
      any(utf8ToInt(l) > 127), logical(1)))[1]
    cat(sprintf("FAIL   %-40s %d non-ASCII byte(s), first at line %s\n",
                f, length(hi), if (is.na(bad)) "?" else bad))
  }
}
if (fail == 0L) cat("ok    ", length(files), "text files are ASCII-clean\n")
cat(sprintf("\nDONE  %d file(s) with non-ASCII\n", fail))
if (fail) quit(status = 1L)
