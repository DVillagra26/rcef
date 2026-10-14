## Length / age coverage on a catch whose fields a csv round trip retyped.
##
## write.taf() / read.taf() give ICESSubArea "27.4" back as the number 27.4 (and a mesh of
## "0" as the integer 0). The biology link derives the catch's fields afresh, as text, so
## build_coverage_strata() used to read its extra columns off the caller's retyped catch and
## then fail joining the two:
##    Can't join `x$ICESSubArea` with `y$ICESSubArea` due to incompatible types.
## It now reads them off the same derived table. What is checked: every combination of text
## and number on the catch and on the distributions, the table itself (the same numbers
## whichever way the column is typed) and the plots that draw it.

source("dev/tests/fixture.R")
FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}
err <- function(expr) tryCatch({ suppressWarnings(suppressMessages(expr)); "" }, error = function(e) conditionMessage(e))
quiet <- function(expr) suppressWarnings(suppressMessages(expr))
drawable <- function(p) !is.null(p) && inherits(p, "ggplot") && !inherits(try(ggplot2::ggplot_build(p), silent = TRUE), "try-error")

cd <- derive_fields(subset(fx_catch(), year == 2023))
dd <- quiet(derive_fields(cd, subset(fx_dist(), year == 2023), what = "distribution"))
retype <- function(x) { x$ICESSubArea <- as.numeric(x$ICESSubArea); x }
ok(is.character(cd$ICESSubArea) && is.character(dd$ICESSubArea) && is.numeric(retype(cd)$ICESSubArea),
   "fixture: the derived ICESSubArea is text, and the retyped one a number")

combo <- list("catch number, distributions number" = list(retype(cd), retype(dd)),
              "catch number, distributions text"   = list(retype(cd), dd),
              "catch text, distributions number"   = list(cd, retype(dd)),
              "catch text, distributions text"     = list(cd, dd))
dims <- c("metier4", "ICESSubArea")
tab <- function(C, D, what, cc = NULL) {
  t <- quiet(build_coverage_strata(C, dims, D, what = what, catchCategory = cc))
  t$ICESSubArea <- as.character(t$ICESSubArea)
  t[order(t$catchCategory, t$metier4, t$ICESSubArea), , drop = FALSE]
}

for (what in c("length", "age")) {
  ref <- tab(cd, dd, what)
  ok(!is.null(ref) && nrow(ref) > 0, sprintf("what = \"%s\": the reference table has rows", what))
  for (nm in names(combo)) {
    C <- combo[[nm]][[1]]; D <- combo[[nm]][[2]]
    e <- err(build_coverage_strata(C, dims, D, what = what))
    ok(!nzchar(e), sprintf("what = \"%s\", %s: no join error%s", what, nm, if (nzchar(e)) paste0(" - ", e) else ""))
    if (!nzchar(e))
      ok(isTRUE(all.equal(tab(C, D, what), ref, check.attributes = FALSE)),
         sprintf("what = \"%s\", %s: the same table as with text", what, nm))
  }
}

## the extra columns still come along, and with the right values
t1 <- tab(retype(cd), retype(dd), "length", "Lan")
pinned <- t1[!is.na(t1$ICESDivision), ]
ok(all(c("ICESDivision", "vesselFlagCountry") %in% names(t1)) && nrow(pinned) > 0 &&
     all(startsWith(pinned$ICESDivision, paste0(pinned$ICESSubArea, "."))),
   "the other derived columns are carried: where a stratum pins one division it is there, inside its sub-area")

## the plots that draw it, with the retyped column as fill, x and facet
Cn <- retype(cd); Dn <- retype(dd)
ok(drawable(plot_coverage_bar(Cn, Dn, y = "metier4", fill = "ICESSubArea", what = "length", catchCategory = "Lan")),
   "plot_coverage_bar(what = \"length\", fill = \"ICESSubArea\"): the call that failed")
ok(drawable(plot_coverage_bar(Cn, Dn, y = "metier4", fill = "ICESSubArea", what = "age")), "... and at age")
ok(drawable(plot_coverage_heat(Cn, Dn, y = "metier4", x = "ICESSubArea", what = "length")), "plot_coverage_heat(x = \"ICESSubArea\")")
ok(drawable(plot_coverage_bubble(Cn, Dn, y = "metier4", x = "vesselFlagCountry", facet = "ICESSubArea", what = "length")),
   "plot_coverage_bubble(facet = \"ICESSubArea\")")
ok(drawable(plot_coverage_bar(Cn, y = "metier4", fill = "ICESSubArea")), "the discard coverage too")
ok(drawable(plot_NaLoA(Cn, Dn, fill = "ICESSubArea")) && drawable(plot_MWaLoA(Cn, Dn, colour = "ICESSubArea")),
   "plot_NaLoA() and plot_MWaLoA() with the retyped column as fill / colour")

## a mesh of "0" read back as the integer 0
c0 <- cd[cd$mesh %in% "0", ]; d0 <- dd[dd$mesh %in% "0", ]
c0$mesh <- as.integer(c0$mesh); d0$mesh <- as.integer(d0$mesh)
ok(nrow(c0) > 0 && !nzchar(err(build_coverage_strata(c0, c("metier4", "mesh"), d0, what = "length"))),
   "a mesh column that came back as an integer does not break the join either")

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
