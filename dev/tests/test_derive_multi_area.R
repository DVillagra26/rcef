## derive_fields() and a record that names several areas - or several fleets.
##
## The CEF lets a discard record cover a set of areas whose landings are reported one by one: the Belgian brill
## discards of TBB_DEF_70-99 are ONE record for "27.4.b;27.4.c;27.7.d;27.7.e" (RDBES download, 2024 and 2025: 1
## record of about 1500, beside 14 landings records of one area each, all with the same domainCatchDis). The
## Swedish DisBMS records of the same download list two fleets ("GNS_DEF_all_0_0_all;GTR_DEF_all_0_0_all").
## The codes used to be split on "." / "_" as if the field held one path, so the division of the Belgian record
## came out as "27.4.b;27" - no area at all - and the Swedish metier4 as "GNS", GTR dropped without a word.
## Each code of the list is now cut on its own and a derived column holds the DISTINCT pieces, sorted and
## ";"-joined (a set: the same areas listed in another order are the same value; pieces that agree at a coarser
## level collapse into one: "27.7.d;27.7.e" is "27.7" as a subarea).
##
## What is pinned: the Belgian record; mixed resolutions; a set is a set (order, repeats, collapse to a plain
## code); a record with one area and one fleet is exactly what it was (against an independent computation on
## the whole fixture); the Swedish fleets, the gear and mesh of each kept together; blanks, empty pieces,
## missing codes; the record stays one row, areaValue / fleetValue / Stratum as reported; calling it again, or
## with a column of the caller's own, changes nothing; the distributions read the same strings off the catch
## (NA where the catch rows of a distribution disagree); factor, tibble, no rows.

source("dev/tests/fixture.R")
FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}
heard <- function(expr) {
  msgs <- character(); warns <- character()
  val <- withCallingHandlers(expr,
    message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") },
    warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") })
  list(value = val, messages = msgs, warnings = warns)
}
levels3 <- c("ICESSubArea", "ICESDivision", "ICESSubdivision")
mk <- function(area, fleet = "TBB_DEF_70-99_0_0_all", country = "BE", season = 2025L, ...)
  data.frame(vesselFlagCountry = country, fleetValue = fleet, seasonValue = season, areaValue = area,
             stringsAsFactors = FALSE, ...)
pieces <- function(x) strsplit(x, ";", fixed = TRUE)

## ---- the Belgian record -------------------------------------------------------------------
be <- derive_fields(mk("27.4.b;27.4.c;27.7.d;27.7.e"))
ok(identical(be$ICESSubArea, "27.4;27.7"), "ICESSubArea: the distinct subareas (27.4;27.7)")
ok(identical(be$ICESDivision, "27.4.b;27.4.c;27.7.d;27.7.e"), "ICESDivision: the four divisions")
ok(identical(be$ICESSubdivision, "27.4.b;27.4.c;27.7.d;27.7.e"),
   "ICESSubdivision: these areas stop at the division, so it repeats (as a single 27.4.b does)")
ok(nrow(be) == 1L && identical(be$areaValue, "27.4.b;27.4.c;27.7.d;27.7.e") &&
     identical(be$Stratum, "BE | TBB_DEF_70-99_0_0_all | 27.4.b;27.4.c;27.7.d;27.7.e | 2025"),
   "the record stays one row, and areaValue and Stratum are as reported")
ok(identical(be$metier4, "TBB") && identical(be$metier4_mesh, "TBB_70-99"),
   "the fleet fields of a one-fleet record are untouched")

## ---- mixed resolutions --------------------------------------------------------------------
mx <- derive_fields(mk("27.3.a.20;27.4.b;27.7"))
ok(identical(mx$ICESSubArea, "27.3;27.4;27.7") && identical(mx$ICESDivision, "27.3.a;27.4.b;27.7") &&
     identical(mx$ICESSubdivision, "27.3.a.20;27.4.b;27.7"),
   "each area at its own resolution: a subdivision, a division and a subarea in one list")

## ---- a set is a set -----------------------------------------------------------------------
st <- derive_fields(mk(c("27.7.e;27.4.b;27.7.d;27.4.c", "27.7.d;27.7.d", "27.7.d;27.7.e", "27.4.b;27.4.b.1")))
ok(identical(st$ICESSubArea[1], be$ICESSubArea) && identical(st$ICESDivision[1], be$ICESDivision),
   "the same areas listed in another order give the same sets")
ok(identical(st$ICESSubArea[2], "27.7") && identical(st$ICESDivision[2], "27.7.d") && identical(st$ICESSubdivision[2], "27.7.d"),
   "a repeated area counts once - and a list of one area is the plain code")
ok(identical(st$ICESSubArea[3], "27.7") && identical(st$ICESDivision[3], "27.7.d;27.7.e"),
   "areas that agree at a coarser level collapse into one value there (27.7.d;27.7.e is 27.7 as a subarea)")
ok(identical(st$ICESDivision[4], "27.4.b") && identical(st$ICESSubdivision[4], "27.4.b;27.4.b.1"),
   "a division and its own subdivision in one list: one division, two subdivisions")
## every area of areaValue is under exactly the piece its own cut gives, at every level
cut_to <- function(a, n) vapply(strsplit(a, ".", fixed = TRUE), function(z) paste(z[seq_len(min(n, length(z)))], collapse = "."), "")
areas <- c("27.4.b;27.4.c;27.7.d;27.7.e", "27.4.b", "27.3.a.20;27.4.b;27.7", "27.3.a.21", "27.7.d;27.7.e", "27.4")
ln <- derive_fields(mk(areas, season = seq_along(areas)))
under <- vapply(seq_len(nrow(ln)), function(i) {
  a <- pieces(ln$areaValue[i])[[1]]
  all(cut_to(a, 2) %in% pieces(ln$ICESSubArea[i])[[1]], cut_to(a, 3) %in% pieces(ln$ICESDivision[i])[[1]],
      cut_to(a, 4) %in% pieces(ln$ICESSubdivision[i])[[1]])
}, NA)
ok(all(under), "every area of a record is under a piece of every level - membership by strsplit() works")
ok(all(lengths(pieces(ln$ICESSubArea)) <= lengths(pieces(ln$ICESDivision))) &&
     all(lengths(pieces(ln$ICESDivision)) <= lengths(pieces(ln$ICESSubdivision))),
   "a finer level never has fewer pieces than a coarser one")

## ---- a record with one area and one fleet is what it was ---------------------------------
derived <- c("metier4", "metier5", "metier6", "target", "mesh", "metier4_mesh", levels3, "Stratum")
fx <- fx_catch()
fx <- fx[, setdiff(names(fx), derived)]
old_cut <- function(a, n) vapply(strsplit(as.character(a), ".", fixed = TRUE), function(z)
  if (!length(z) || all(is.na(z))) NA_character_ else paste(z[seq_len(min(n, length(z)))], collapse = "."), "")
old_tok <- function(f, n) vapply(strsplit(as.character(f), "_", fixed = TRUE), function(z) paste(z[seq_len(min(n, length(z)))], collapse = "_"), "")
fd <- derive_fields(fx)
ok(nrow(fx) > 100L && !any(grepl(";", fx$areaValue, fixed = TRUE)) && !any(grepl(";", fx$fleetValue, fixed = TRUE)),
   "premise: the fixture has no record that lists areas or fleets")
ok(identical(fd$ICESSubArea, old_cut(fx$areaValue, 2)) && identical(fd$ICESDivision, old_cut(fx$areaValue, 3)) &&
     identical(fd$ICESSubdivision, old_cut(fx$areaValue, 4)),
   "the whole fixture (one area per record): the three levels are the cuts of the path, as they always were")
ok(identical(fd$metier4, old_tok(fx$fleetValue, 1)) && identical(fd$metier5, old_tok(fx$fleetValue, 2)) &&
     identical(fd$metier6, old_tok(fx$fleetValue, 3)),
   "the whole fixture (one fleet per record): metier4 to metier6 are the first 1, 2 and 3 parts, as they always were")
ok(identical(unlist(derive_fields(mk("27.4.b.1"))[levels3], use.names = FALSE), c("27.4", "27.4.b", "27.4.b.1")) &&
     identical(unlist(derive_fields(mk("27.4"))[levels3], use.names = FALSE), rep("27.4", 3)) &&
     identical(unlist(derive_fields(mk("27"))[levels3], use.names = FALSE), rep("27", 3)),
   "27.4.b.1 is cut at 2, 3 and 4 parts; a coarser code repeats")

## ---- blanks, empty pieces, nothing --------------------------------------------------------
odd <- derive_fields(mk(c("27.4.b; 27.4.c ;", " 27.7.d ;; 27.7.e", ";", NA, "", " ")))
ok(identical(odd$ICESSubArea[1:2], c("27.4", "27.7")) &&
     identical(odd$ICESDivision[1:2], c("27.4.b;27.4.c", "27.7.d;27.7.e")),
   "blanks around a ';' are ignored and so are empty pieces (a trailing ';', ';;')")
ok(all(is.na(odd$ICESSubArea[3:6])) && all(is.na(odd$ICESDivision[3:6])) && all(is.na(odd$ICESSubdivision[3:6])),
   "no area at all (';', NA, empty, blank) gives NA at every level")
ok(identical(odd$areaValue[1:2], c("27.4.b; 27.4.c ;", " 27.7.d ;; 27.7.e")),
   "... and areaValue is left as it came")

## ---- several fleets (the Swedish DisBMS records) -----------------------------------------
se <- derive_fields(mk("27.3.a.21", fleet = "GNS_DEF_all_0_0_all;GTR_DEF_all_0_0_all", country = "SE"))
ok(identical(se$metier4, "GNS;GTR") && identical(se$metier5, "GNS_DEF;GTR_DEF") &&
     identical(se$metier6, "GNS_DEF_all;GTR_DEF_all"),
   "metier4 to metier6: the distinct fleets at each level (GTR is no longer dropped)")
ok(identical(se$target, "DEF") && identical(se$mesh, "all"),
   "target and mesh: the two fleets agree, so one value")
ok(identical(se$metier4_mesh, "GNS_all;GTR_all"),
   "metier4_mesh: the gear and mesh of each fleet stay together")
ok(identical(se$fleetValue, "GNS_DEF_all_0_0_all;GTR_DEF_all_0_0_all") &&
     identical(se$Stratum, "SE | GNS_DEF_all_0_0_all;GTR_DEF_all_0_0_all | 27.3.a.21 | 2025") && nrow(se) == 1L,
   "one row, fleetValue and Stratum as reported")
sm <- derive_fields(mk("27.4.b", fleet = c("GTR_DEF_70-99_0_0;GNS_DEF_100-119_0_0", "OTB_DEF_70-99;OTB_DEF_100-119", "OTB_DEF_70-99;OTB_DEF_70-99")))
ok(identical(sm$mesh, c("100-119;70-99", "100-119;70-99", "70-99")) &&
     identical(sm$metier4_mesh, c("GNS_100-119;GTR_70-99", "OTB_100-119;OTB_70-99", "OTB_70-99")) &&
     identical(sm$metier4, c("GNS;GTR", "OTB", "OTB")),
   "different meshes: the pairs stay together (GNS_100-119;GTR_70-99), a repeated fleet counts once, one gear is one gear")
ok(identical(derive_fields(mk("27.4.b", fleet = "GNS_DEF;"))$metier4_mesh, "GNS") &&
     is.na(derive_fields(mk("27.4.b", fleet = NA))$metier4),
   "a trailing ';' is ignored, and no fleet at all is NA")

## ---- again, and the caller's own columns --------------------------------------------------
ok(identical(derive_fields(be), be) && identical(derive_fields(se), se), "calling it again changes nothing")
own <- mk("27.4.b;27.4.c;27.7.d;27.7.e", ICESDivision = "mine", metier4 = "BEAM")
od  <- derive_fields(own)
ok(identical(od$ICESDivision, "mine") && identical(od$ICESSubArea, "27.4;27.7") && identical(od$metier4, "BEAM") &&
     identical(od$metier4_mesh, "BEAM_70-99"),
   "a column the catch already has is kept, the others are derived from what it has (a regrouped metier4 flows into metier4_mesh)")

## ---- the types ----------------------------------------------------------------------------
xf <- mk(areas, season = seq_along(areas)); xf$areaValue <- factor(xf$areaValue)
ok(identical(derive_fields(xf)[levels3], ln[levels3]), "a factor areaValue gives the same")
tb <- tibble::as_tibble(mk(areas, season = seq_along(areas)))
ot <- derive_fields(tb)
ok(inherits(ot, "tbl_df") && identical(as.data.frame(ot)[levels3], ln[levels3]), "a tibble works as well, and comes back a tibble")
z <- derive_fields(mk("27.4.b")[0, ])
ok(nrow(z) == 0L && all(levels3 %in% names(z)) && all(vapply(z[levels3], is.character, NA)),
   "no rows: the columns are there, as text")

## ---- the distributions --------------------------------------------------------------------
mk_catch <- function(domain, cat, area, fleet = "TBB_DEF_70-99_0_0_all", season = 2025L)
  data.frame(year = 2025L, workingGroup = "WGX", stock = "stk.27", speciesCode = "SPP", catchCategory = cat,
             vesselFlagCountry = "BE", fleetValue = fleet, areaValue = area, seasonValue = season,
             domainBiology = domain, stringsAsFactors = FALSE)
mk_dist <- function(domain, cat = "Dis")
  data.frame(year = 2025L, workingGroup = "WGX", stock = "stk.27", speciesCode = "SPP", catchCategory = cat,
             vesselFlagCountry = "BE", domainBiology = domain, distributionType = "LengthTotal",
             distributionClass = 100, variableType = "Number", value = 5, stringsAsFactors = FALSE)
catch <- rbind(
  mk_catch("DM1", "Dis", "27.4.b;27.4.c;27.7.d;27.7.e"),                      # the listed record, its own distribution
  mk_catch("DM2", "Dis", "27.4.b",       fleet = "OTB_DEF_70-99_0_0_all"),     # one distribution, two records ...
  mk_catch("DM2", "Dis", "27.7.d;27.7.e", fleet = "OTB_DEF_70-99_0_0_all"))    # ... of different areas
dist <- rbind(mk_dist("DM1"), mk_dist("DM1"), mk_dist("DM2"))
h <- heard(derive_fields(catch, dist, what = "distribution"))
dd <- h$value
ok(nrow(dd) == 3L && identical(dd$ICESSubArea[1:2], rep("27.4;27.7", 2)) &&
     identical(dd$ICESDivision[1:2], rep("27.4.b;27.4.c;27.7.d;27.7.e", 2)) &&
     identical(dd$areaValue[1:2], rep("27.4.b;27.4.c;27.7.d;27.7.e", 2)),
   "a distribution takes the set of areas of its catch record, at every level")
ok(is.na(dd$areaValue[3]) && is.na(dd$ICESDivision[3]) && is.na(dd$ICESSubArea[3]) && identical(dd$metier4[3], "OTB"),
   "a distribution of two records with different areas: the areas are NA, what they share (the fleet) is kept")
ok(length(h$messages) == 1L && grepl("1 distribution(s) are used", h$messages, fixed = TRUE) && !length(h$warnings),
   "... said once")

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
