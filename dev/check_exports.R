## ---------------------------------------------------------------
##  dev/check_exports.R
##
##  NAMESPACE is roxygen-generated now, so the risk has flipped: it is no
##  longer that document() hides things, it is that a NEW function gets
##  added without an @export tag and silently never reaches the app.
##
##  This lists every top-level function under R/ that carries no @export
##  in the roxygen block attached to it. It should print zero.
##
##  Usage (repo root):  Rscript dev/check_exports.R
## ---------------------------------------------------------------
files <- list.files("R", "\\.R$", full.names = TRUE)

untagged <- character(0)
stubs    <- character(0)

for (f in files) {
  x   <- readLines(f, warn = FALSE)
  hit <- grep("^`?[A-Za-z._][A-Za-z0-9._]*`?\\s*<-\\s*function", x)
  for (i in hit) {
    nm <- sub("\\s*<-.*$", "", x[i])
    j <- i - 1L; blk <- character(0)
    while (j >= 1L && grepl("^\\s*#+'", x[j])) { blk <- c(x[j], blk); j <- j - 1L }
    if (!any(grepl("@export", blk)))
      untagged <- c(untagged, sprintf("%s::%s", f, nm))
    else if (any(grepl("@noRd", blk)))
      stubs <- c(stubs, sprintf("%s::%s", f, nm))
  }
}

cat(sprintf("NOT EXPORTED : %d\n", length(untagged)))
if (length(untagged)) cat(paste0("  ", untagged, collapse = "\n"), "\n")

cat(sprintf("\nEXPORTED BUT UNDOCUMENTED (@noRd) : %d\n", length(stubs)))
cat("  These work, but have no man page. Replacing @noRd with a real\n")
cat("  roxygen block is the documentation backlog; see dev/DOC_BACKLOG.txt\n")

cat(sprintf("\nDONE  %d untagged, %d undocumented\n", length(untagged), length(stubs)))
if (length(untagged)) quit(status = 1L)
