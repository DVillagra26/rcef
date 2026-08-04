## ---------------------------------------------------------------
##  dev/check_exports.R
##
##  Guards the move from the hand-written blanket NAMESPACE to
##  roxygen-generated exports. Lists every top-level function under R/
##  that would STOP being exported.
##
##  Usage (repo root):  Rscript dev/check_exports.R
## ---------------------------------------------------------------
files <- list.files("R", "\\.R$", full.names = TRUE)

defs <- unlist(lapply(files, function(f) {
  x <- readLines(f, warn = FALSE)
  m <- regmatches(x, regexpr("^[A-Za-z._][A-Za-z0-9._]*(?=\\s*<-\\s*function)",
                             x, perl = TRUE))
  m[!startsWith(m, ".")]
}))

## an @export anywhere in the roxygen block immediately above a definition
tagged <- unlist(lapply(files, function(f) {
  x <- readLines(f, warn = FALSE)
  hit <- grep("^[A-Za-z._][A-Za-z0-9._]*\\s*<-\\s*function", x)
  out <- character(0)
  for (i in hit) {
    j <- i - 1L
    while (j >= 1L && grepl("^\\s*#+'", x[j])) {
      if (grepl("@export", x[j])) {
        out <- c(out, sub("\\s*<-.*$", "", x[i])); break
      }
      j <- j - 1L
    }
  }
  out
}))

miss <- sort(setdiff(unique(defs), unique(tagged)))
cat(sprintf("defined : %d\ntagged  : %d\nUNTAGGED: %d\n\n",
            length(unique(defs)), length(unique(tagged)), length(miss)))
if (length(miss)) {
  cat("These would DISAPPEAR from the NAMESPACE if you ran document():\n")
  cat(paste0("  ", miss, collapse = "\n"), "\n")
}
cat(sprintf("\nDONE  %d untagged\n", length(miss)))
