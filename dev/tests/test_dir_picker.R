## The folder browser.
##
## The ask was to use the upload dialog for "point at folder", because
## finding a folder outside the project was painful. A browser cannot
## return an absolute path from that dialog, and an absolute path is what
## the emitted script needs to run anywhere. So the explorer stays
## server-side and the navigation problem is solved directly: shortcuts,
## a breadcrumb, recents, and a preview of the files in the folder.
##
## The breadcrumb is the part that can quietly break, because "the root"
## is not one thing. These cases are the ones that produce crumbs leading
## nowhere, and they are all reachable from ILVO's network shares.

FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}

e <- new.env()
src <- system.file("app/app_ui_helpers.R", package = "rcef")
if (!nzchar(src)) stop("rcef is not installed; R CMD INSTALL first")
suppressWarnings(suppressMessages(source(src, local = e)))
cr <- function(p) unlist(unname(e$dir_picker_crumbs(p)))

## ---- unix ------------------------------------------------------------
ok(identical(cr("/home/claude/rcef"),
             c("/", "/home", "/home/claude", "/home/claude/rcef")),
   "a unix path keeps / as its own clickable root")
ok(identical(cr("/"), "/"), "the unix root alone is one crumb")

## ---- windows drives --------------------------------------------------
##
## "C:" is NOT the root of C: - it means the current directory on that
## drive, which is wherever the process last happened to be. dir.exists()
## can return TRUE for it while listing something else entirely.
ok(identical(cr("C:/work/bll/data"),
             c("C:/", "C:/work", "C:/work/bll", "C:/work/bll/data")),
   "a drive root keeps its slash and the rest does not double it")
ok(!any(grepl("//", cr("C:/work/bll"), fixed = TRUE)),
   "no doubled separators anywhere in a drive path")
ok(identical(cr("C:/"), "C:/"), "a bare drive root is one crumb")

## ---- UNC shares ------------------------------------------------------
##
## //server/share is the root TOGETHER; //server on its own is not a
## place, so it must never appear as a crumb.
u <- cr("//server/share/data/cef")
ok(identical(u, c("//server/share", "//server/share/data",
                  "//server/share/data/cef")),
   "a UNC share roots at //server/share")
ok(!any(u == "/server"), "the server name never appears as a lone crumb")
ok(identical(cr("//server/share"), "//server/share"),
   "a bare share is one crumb")

## ---- relative and odd ------------------------------------------------
ok(identical(cr("relative/path"), c("relative", "relative/path")),
   "a relative path gets no invented leading slash")
ok(length(cr(".")) >= 1, "'.' produces at least one crumb")

## every crumb must be a prefix of the path it came from, or clicking it
## navigates somewhere the user was never in
for (p in c("/home/claude/rcef", "C:/work/bll/data", "//server/share/data")) {
  cs <- cr(p)
  ok(all(vapply(cs, function(x) startsWith(gsub("/+$", "", p),
                                           gsub("/+$", "", x)), logical(1))),
     paste("every crumb is a prefix of", p))
  ok(identical(gsub("/+$", "", cs[length(cs)]), gsub("/+$", "", p)),
     paste("the last crumb is the path itself for", p))
}

## ---- shortcuts -------------------------------------------------------
sc <- e$dir_picker_shortcuts()
ok(length(sc) > 0, "there is at least one shortcut")
ok("Project" %in% names(sc), "the working directory is offered")
ok(all(nzchar(unlist(sc))), "no shortcut is an empty path")

## ---- recents ---------------------------------------------------------
##
## Session-scoped, most recent first, no duplicates - a folder chosen
## twice should not fill the list with itself.
e$dir_picker_recent("/tmp/one"); e$dir_picker_recent("/tmp/two")
e$dir_picker_recent("/tmp/one")
r <- e$dir_picker_recent()
ok(identical(r[1], "/tmp/one"), "the most recent folder is first")
ok(length(r) == length(unique(r)), "recents carry no duplicates")
for (i in 1:10) e$dir_picker_recent(paste0("/tmp/d", i))
ok(length(e$dir_picker_recent()) <= 6, "recents stay capped")

cat("\nDONE ", FAIL, "failure(s)", "\n")
