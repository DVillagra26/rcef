## ---------------------------------------------------------------
##  dev/check_exports.R
##
##  NAMESPACE is roxygen-generated. The failure mode that costs a whole
##  debugging session is a top-level object with no @export tag: it
##  vanishes from the NAMESPACE and the app dies at runtime with
##  "object 'X' not found", far from the cause.
##
##  This compares every top-level object defined under R/ against what
##  the NAMESPACE actually exports. FUNCTIONS AND CONSTANTS BOTH - the
##  constants are the ones that get forgotten (OUTPUT_DESC, CEF_ORDER,
##  LEN_CLASS_TO_MM ...), because they carry no roxygen block by habit.
##
##  parse() is used rather than regex: a backticked name like `%||%` or a
##  multi-line definition defeats pattern matching.
##
##  Usage (repo root):  Rscript dev/check_exports.R
## ---------------------------------------------------------------
files <- list.files("R", "\\.R$", full.names = TRUE)

defined <- list()
for (f in files) {
  for (e in parse(f)) {
    if (!is.call(e)) next
    if (!as.character(e[[1]])[1] %in% c("<-", "=")) next
    if (!(is.name(e[[2]]) || is.character(e[[2]]))) next
    nm <- as.character(e[[2]])
    if (startsWith(nm, ".")) next
    rhs <- e[[3]]
    kind <- if (is.call(rhs) && as.character(rhs[[1]])[1] == "function")
              "function" else "object"
    defined[[length(defined) + 1L]] <- list(file = f, name = nm, kind = kind)
  }
}
nms <- vapply(defined, `[[`, "", "name")

ns <- readLines("NAMESPACE", warn = FALSE)
ex <- gsub('^"|"$', "",
           sub("^export\\((.*)\\)$", "\\1", grep("^export\\(", ns, value = TRUE)))

miss <- defined[!nms %in% ex]

cat(sprintf("defined under R/ : %d  (%d functions, %d constants)\n",
            length(unique(nms)),
            sum(vapply(defined, `[[`, "", "kind") == "function"),
            sum(vapply(defined, `[[`, "", "kind") == "object")))
cat(sprintf("exported         : %d\n", length(ex)))
cat(sprintf("NOT EXPORTED     : %d\n", length(miss)))
for (m in miss)
  cat(sprintf("  %-8s %s::%s\n", m$kind, m$file, m$name))

## how much of what IS exported still has no man page
stubs <- 0L
for (f in files) {
  x <- readLines(f, warn = FALSE)
  stubs <- stubs + sum(grepl("@noRd", x))
}
cat(sprintf("\nexported without a man page (@noRd): %d  -> dev/DOC_BACKLOG.txt\n",
            stubs))
cat(sprintf("\nDONE  %d not exported\n", length(miss)))
if (length(miss)) quit(status = 1L)
