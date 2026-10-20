## Empty text is missing. A CEF csv read with read.csv() or read.taf() has "" in a text column where the
## file has an empty field; read with readr, or read.csv(na.strings = ""), it has NA. derive_fields() hands
## both back the same, as NA, because the raising, the allocation and catch_domain() tell a missing id by NA:
## a catch row with no domainBiology that kept "" shares the domain "" with every other one and is taken for
## ONE distribution. (Found on the real bll 2025 data read with read.taf(): 933 of 987 catch rows shared "",
## and 728 of the 1286 allocated catch rows had no distribution.)
##
## What is pinned: the catch and the distributions come back the same whichever way the file was read; no
## text column keeps ""; what is not text is untouched, rows and order too; catch_domain() gives every row
## with no domain its own stratum domain; calling it again changes nothing; a tibble works as well.

source("dev/tests/fixture.R")
FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}
quiet <- function(expr) suppressWarnings(suppressMessages(expr))

derived <- c("metier4", "metier5", "metier6", "target", "mesh", "metier4_mesh",
             "ICESSubArea", "ICESDivision", "ICESSubdivision", "Stratum")
cd0 <- subset(fx_catch(), year == 2023)
cd0 <- cd0[, setdiff(names(cd0), derived)]                    # the catch as the CEF file has it
dd0 <- subset(fx_dist(), year == 2023)
dd0 <- dd0[, setdiff(names(dd0), c(derived, "fleetValue", "areaValue", "seasonValue"))]

## the same file, read the two ways: empty text stays "" (read.csv, read.taf) or is NA (na.strings = "", readr)
reread <- function(x, na.strings) {
  f <- tempfile(fileext = ".csv"); on.exit(unlink(f))
  utils::write.csv(x, f, row.names = FALSE, na = "")
  utils::read.csv(f, na.strings = na.strings, stringsAsFactors = FALSE)
}
cat_plain <- reread(cd0, "NA");  cat_na <- reread(cd0, "")
dis_plain <- reread(dd0, "NA");  dis_na <- reread(dd0, "")
is_text <- function(x) vapply(x, is.character, NA)
has_empty <- function(x) any(vapply(x[is_text(x)], function(v) any(v == "", na.rm = TRUE), NA))

ok(sum(cat_plain$domainBiology == "", na.rm = TRUE) > 0 && !has_empty(cat_na) && has_empty(cat_plain),
   "premise: read the plain way the catch has empty text in domainBiology, read with na.strings = \"\" it has none")
ok(any(rcef::catch_domain(cat_plain) == "", na.rm = TRUE),
   "premise: left as it is, catch_domain() gives those rows the domain \"\" (the trap)")

## ---- the catch ----------------------------------------------------------------------------
out_plain <- quiet(derive_fields(cat_plain))
out_na    <- quiet(derive_fields(cat_na))
ok(identical(out_plain, out_na),
   "the catch comes back the same whether the file was read with \"\" or with NA for empty text")
ok(!has_empty(out_plain), "no text column of the derived catch keeps the empty text")
not_text <- names(cat_plain)[!is_text(cat_plain)]
ok(length(not_text) > 5L && identical(out_plain[not_text], cat_plain[not_text]),
   "what is not text is untouched: same values, same rows, same order")
ok(sum(is.na(out_plain$domainBiology)) ==
     sum(is.na(cat_plain$domainBiology)) + sum(cat_plain$domainBiology == "", na.rm = TRUE),
   "every empty domainBiology is NA now, and no other row is")
ok(identical(quiet(derive_fields(out_plain)), out_plain), "calling it again changes nothing")

dom <- rcef::catch_domain(out_plain)
no_dom <- is.na(out_plain$domainBiology)
ok(!any(dom == "", na.rm = TRUE) && all(startsWith(dom[no_dom], "[alloc]_")) && identical(dom[!no_dom], out_plain$domainBiology[!no_dom]),
   "catch_domain(): a row with no domain gets the domain of its stratum, a row with one keeps it")
ok(length(unique(dom[no_dom])) > 1L,
   "... and the rows with no domain no longer share one (they did, as \"\")")

## ---- the distributions --------------------------------------------------------------------
dd_plain <- quiet(derive_fields(out_plain, dis_plain, what = "distribution"))
dd_na    <- quiet(derive_fields(out_na, dis_na, what = "distribution"))
ok(identical(dd_plain, dd_na),
   "the distributions come back the same whichever way the files were read")
ok(!has_empty(dd_plain), "no text column of the derived distributions keeps the empty text")
ok(nrow(dd_plain) == nrow(dis_plain) && identical(dd_plain$distributionClass, dis_plain$distributionClass) &&
     identical(dd_plain$value, dis_plain$value),
   "the distributions keep their rows and their order")
ok(identical(quiet(derive_fields(out_plain, dd_plain, what = "distribution")), dd_plain),
   "deriving the distributions again changes nothing")
## the fixture's distributions have no blank cell of their own: give a few rows some
dx <- dis_plain[1:6, ]
dx$note <- c("", "a", "", "", "b", "")
dx$domainBiology[2] <- ""
rx <- quiet(derive_fields(out_plain, dx, what = "distribution"))
ok(nrow(rx) == 6L && identical(rx$note, c(NA, "a", NA, NA, "b", NA)) && is.na(rx$domainBiology[2]) &&
     identical(rx$domainBiology[-2], dx$domainBiology[-2]),
   "empty text in a distribution row becomes NA too, the other cells and the rows stay")

## ---- the odd ones -------------------------------------------------------------------------
odd <- data.frame(fleetValue = c("OTB_DEF_70-99_0_0", "All"), areaValue = c("27.4.b", "27.4"),
                  vesselFlagCountry = c("A", "B"), seasonValue = 1:2,
                  comment = c("", ""), domainBiology = c("D1", ""), stringsAsFactors = FALSE)
o <- quiet(derive_fields(odd))
ok(is.character(o$comment) && all(is.na(o$comment)) && identical(o$domainBiology, c("D1", NA)),
   "a text column that is empty throughout stays a (NA) text column; only the empty cells become NA")
tb <- tibble::as_tibble(cat_plain)
ot <- quiet(derive_fields(tb))
ok(inherits(ot, "tbl_df") && !has_empty(ot) && identical(as.data.frame(ot), out_plain),
   "a tibble works as well, and comes back a tibble")
ok(is.data.frame(quiet(derive_fields(cat_plain[0, ]))) && nrow(quiet(derive_fields(cat_plain[0, ]))) == 0L,
   "a catch with no rows still comes back")

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
