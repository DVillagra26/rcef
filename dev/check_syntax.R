## Definitive syntax check. parse() is the oracle: a bracket counter will
## happily accept a truncated function body.
## Usage (from the repo root):  Rscript dev/check_syntax.R

root <- if (dir.exists("R") && file.exists("DESCRIPTION")) "." else ".."
if (!dir.exists(file.path(root, "R")))
  stop("Run this from the repository root: Rscript dev/check_syntax.R")

files <- c(
  list.files(file.path(root, "R"),        "\\.R$", full.names = TRUE),
  list.files(file.path(root, "inst/app"), "\\.R$", full.names = TRUE),
  list.files(file.path(root, "dev"),      "\\.R$", full.names = TRUE,
             recursive = TRUE))

fail <- 0L
for (f in files) {
  e <- tryCatch({ parse(f); NULL }, error = function(e) conditionMessage(e))
  rel <- sub("^\\./", "", f)
  if (is.null(e)) {
    cat(sprintf("ok    %s\n", rel))
  } else {
    fail <- fail + 1L
    cat(sprintf("FAIL  %s\n      %s\n", rel, e))
  }
}
cat(sprintf("\nDONE  %d file(s), %d failure(s)\n", length(files), fail))
if (fail) quit(status = 1L)
