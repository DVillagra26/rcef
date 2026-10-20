## derive_fields() on the distributions: derive_fields(catch_data,
## distribution_data, what = "distribution").
##
## A distribution has no fleet, area or season of its own - those belong to the
## catch - so the fields come from the catch rows it is linked to through
## domainBiology (with the catch category, the country, the year ... as the
## allocation matches them). What is pinned here: the catch call is untouched;
## the distributions come back with the same rows in the same order and the
## right fields; what happens where the link is ambiguous or missing; the
## mistakes that used to be a bare "Missing expected column(s)"; and that a
## distributions table that carries the fields still plots, tabulates and
## allocates exactly as one that does not.

source("dev/tests/fixture.R")
FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}
quiet <- function(expr) {
  out <- NULL
  invisible(utils::capture.output(out <- suppressMessages(expr)))
  out
}
# the value of expr, with the messages and warnings it gave
heard <- function(expr) {
  msgs <- character(); warns <- character()
  val <- withCallingHandlers(expr,
    message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") },
    warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") })
  list(value = val, messages = msgs, warnings = warns)
}
err <- function(expr) tryCatch({ expr; "" }, error = function(e) conditionMessage(e))
lf <- tempfile(fileext = ".txt")
# The fixture is a CEF file read with read.csv(): its blank cells are "". derive_fields() hands every
# empty text back as NA, so "the distributions as they went in" is the fixture with its blanks as NA.
blank_na <- function(x) {
  x[] <- lapply(x, function(v) if (is.character(v)) replace(v, which(v == ""), NA_character_) else v)
  x
}

## ---- a catch and its distributions, small enough to read -----------------------
mk_catch <- function(domain, cat, country, fleet, area, season, year = 2023L)
  data.frame(year = year, workingGroup = "WGX", stock = "stk.27", speciesCode = "SPP",
             catchCategory = cat, vesselFlagCountry = country,
             fleetValue = fleet, areaValue = area, seasonValue = season,
             domainBiology = domain, stringsAsFactors = FALSE)
catch <- rbind(
  mk_catch("D1", "Lan", "A", "OTB_DEF_70-99_0_0",    "27.4.b",     1L),
  mk_catch("D1", "Lan", "B", "TBB_DEF_120-219_0_0",  "27.7.d",     3L),  # same domain name, other country
  mk_catch("D2", "Dis", "A", "GNS_DEF_100-119_0_0",  "27.3.a.21",  2L),
  mk_catch("D3", "Lan", "A", "OTB_DEF_70-99_0_0",    "27.4.b",     1L),  # one domain ...
  mk_catch("D3", "Lan", "A", "OTB_DEF_70-99_0_0",    "27.4.b",     2L),  # ... two seasons
  mk_catch(NA,   "Lan", "A", "All",                  "27.4",       4L),  # a stratum with no biology
  mk_catch("D4", "Lan", "A", "DRB_MOL_0_0_0",        "27.7.e",     1L, year = 2022L),
  mk_catch("D4", "Lan", "A", "MIS_MIS_0_0_0",        "27.7.f",     1L, year = 2023L))  # D4 means two things in two years

mk_dist <- function(domain, cat, country, type = "LengthTotal", cls = 100, year = 2023L)
  data.frame(year = year, workingGroup = "WGX", stock = "stk.27", speciesCode = "SPP",
             catchCategory = cat, vesselFlagCountry = country, domainBiology = domain,
             distributionType = type, distributionClass = cls,
             variableType = "Number", value = 5, stringsAsFactors = FALSE)
dist <- rbind(
  mk_dist("D1", "Lan", "A"),                       # 1
  mk_dist("D1", "Lan", "A", cls = 110),            # 2
  mk_dist("D1", "Lan", "B"),                       # 3  B's D1 is not A's
  mk_dist("D2", "Dis", "A"),                       # 4
  mk_dist("D3", "Lan", "A", type = "Age", cls = 1),# 5  two seasons behind it
  mk_dist("DX", "Lan", "A"),                       # 6  no catch row has this domain
  mk_dist("D1", "Dis", "A"),                       # 7  D1 exists, but only as landings
  mk_dist(NA,   "Lan", "A"),                       # 8  no domain at all
  mk_dist("D4", "Lan", "A", year = 2022L),         # 9
  mk_dist("D4", "Lan", "A", year = 2023L))         # 10
n_dist <- nrow(dist)
derived_cols <- c("metier4", "metier5", "metier6", "target", "mesh", "metier4_mesh",
                  "ICESSubArea", "ICESDivision", "ICESSubdivision", "Stratum")
added_cols   <- c("fleetValue", "areaValue", "seasonValue", derived_cols)

## ---- the catch call is what it was ------------------------------------------------
dc <- derive_fields(catch)
ok(identical(setdiff(names(dc), names(catch)), derived_cols) && nrow(dc) == nrow(catch),
   "derive_fields(catch): adds the same ten columns, one row per row")
ok(identical(dc$metier4_mesh, c("OTB_70-99", "TBB_120-219", "GNS_100-119", "OTB_70-99",
                                "OTB_70-99", "All", "DRB_0", "MIS_0")) &&
     identical(dc$Stratum[2], "B | TBB_DEF_120-219_0_0 | 27.7.d | 3"),
   "derive_fields(catch): the values are the ones it always gave")
ok(identical(derive_fields(catch_data = catch), dc) &&
     identical(derive_fields(catch, what = "catch"), dc),
   "derive_fields(catch_data = ) and what = \"catch\" are the same call")

## ---- the distributions --------------------------------------------------------------
h  <- heard(derive_fields(catch, dist, what = "distribution"))
out <- h$value
ok(nrow(out) == n_dist && identical(out[names(dist)], dist),
   "what = \"distribution\": every row, in the order it came, and every column untouched")
ok(identical(setdiff(names(out), names(dist)), added_cols),
   "the distributions get fleetValue, areaValue, seasonValue and the ten derived columns - and no country (they have their own)")
ok(identical(out$vesselFlagCountry, dist$vesselFlagCountry),
   "vesselFlagCountry stays the distributions' own")

r1 <- out[1, ]                                    # A's D1: OTB, 27.4.b, season 1
ok(identical(r1$fleetValue, "OTB_DEF_70-99_0_0") && identical(r1$areaValue, "27.4.b") &&
     identical(r1$seasonValue, 1L) && identical(r1$metier4, "OTB") &&
     identical(r1$metier4_mesh, "OTB_70-99") && identical(r1$mesh, "70-99") &&
     identical(r1$ICESDivision, "27.4.b") && identical(r1$ICESSubArea, "27.4") &&
     identical(r1$Stratum, "A | OTB_DEF_70-99_0_0 | 27.4.b | 1"),
   "a distribution takes the fleet, area and season of its catch rows, and the fields derived from them")
ok(identical(unlist(out[1, added_cols]), unlist(out[2, added_cols])),
   "all the rows of one distribution get the same fields")
r3 <- out[3, ]                                    # B's D1: not A's
ok(identical(r3$metier4, "TBB") && identical(r3$areaValue, "27.7.d") &&
     identical(r3$Stratum, "B | TBB_DEF_120-219_0_0 | 27.7.d | 3"),
   "the same domainBiology in another country is another stratum: the country is part of the link")
r4 <- out[4, ]
ok(identical(r4$metier4, "GNS") && identical(r4$ICESSubdivision, "27.3.a.21") &&
     identical(r4$seasonValue, 2L),
   "a discard distribution is linked to the discard catch")
ok(identical(out$metier4[9], "DRB") && identical(out$metier4[10], "MIS") &&
     identical(out$areaValue[c(9, 10)], c("27.7.e", "27.7.f")),
   "the same domainBiology in two years is two strata: the year is part of the link")

# one domainBiology, two seasons behind it
r5 <- out[5, ]
ok(is.na(r5$seasonValue) && is.na(r5$Stratum),
   "a domain used by two seasons: the season is NA - and so is the stratum, which needs it")
ok(identical(r5$fleetValue, "OTB_DEF_70-99_0_0") && identical(r5$metier4_mesh, "OTB_70-99") &&
     identical(r5$ICESDivision, "27.4.b"),
   "... but what both seasons share is kept")
ok(is.integer(out$seasonValue) && is.character(out$metier4) && is.character(out$Stratum),
   "the columns keep their types (seasonValue stays an integer)")
ok(length(h$messages) == 1L && grepl("1 distribution key", h$messages) &&
     grepl("D3", h$messages, fixed = TRUE),
   "the ambiguity is said once, naming the domain")

# no catch row behind it
orph <- c(6L, 7L, 8L)
ok(all(is.na(out[orph, added_cols])) && !anyNA(out[-c(orph, 5L), added_cols]),
   "no catch row behind it (unknown domain, other category, no domain): NA throughout - and nowhere else")
ok(length(h$warnings) == 1L && grepl(sprintf("3 of %d distribution row", n_dist), h$warnings),
   "... with one warning that counts them")

# rows and order
perm <- rev(seq_len(n_dist))
op <- quiet(suppressWarnings(derive_fields(catch, dist[perm, ], what = "distribution")))
ok(identical(rownames(op), rownames(dist)[perm]) &&
     identical(op[added_cols], out[perm, added_cols]),
   "shuffled in, shuffled out: each row keeps its own fields")
ok(identical(quiet(suppressWarnings(derive_fields(catch, dist[0, ], what = "distribution")))[0, ],
             out[0, ]),
   "a distributions table with no rows gives no rows, and the columns")
ok(inherits(quiet(suppressWarnings(derive_fields(catch, tibble::as_tibble(dist),
                                                 what = "distribution"))), "tbl_df"),
   "a tibble comes back a tibble")
ok(identical(suppressWarnings(suppressMessages(derive_fields(catch, out, what = "distribution"))), out),
   "calling it again, on the result, changes nothing")
ok(identical(suppressWarnings(suppressMessages(derive_fields(catch, dist, what = "dist"))), out),
   "what = \"dist\" is enough")

# the catch given already derived, or the distributions without a country
ok(identical(suppressWarnings(suppressMessages(derive_fields(dc, dist, what = "distribution"))), out),
   "a catch that was derived already gives the same distributions")
no_cty <- dist[setdiff(names(dist), "vesselFlagCountry")]
oc <- suppressWarnings(suppressMessages(derive_fields(catch, no_cty, what = "distribution")))
ok(identical(oc$vesselFlagCountry[4], "A") && is.na(oc$vesselFlagCountry[1]) &&
     identical(oc$metier4[4], "GNS") && is.na(oc$Stratum[1]),
   "distributions with no country of their own get it from the catch - NA where two countries share the domain")

## ---- the mistakes it names --------------------------------------------------------------
m <- err(derive_fields(dist))
ok(grepl("distributions table", m) && grepl("what = \"distribution\"", m, fixed = TRUE),
   "derive_fields(distributions): says it is a distributions table, and what to call instead")
m <- err(derive_fields(dist, catch, what = "distribution"))
ok(grepl("distributions table", m),
   "... also when the two are the wrong way round")
m <- err(derive_fields(catch, dist))
ok(grepl("what = \"distribution\"", m, fixed = TRUE),
   "distribution_data without what = \"distribution\": refused, with the call that works")
m <- err(derive_fields(catch, what = "distribution"))
ok(grepl("needs `distribution_data`", m, fixed = TRUE),
   "what = \"distribution\" without distribution_data: asks for it")
m <- err(derive_fields(catch, "dist", what = "distribution"))
ok(grepl("needs `distribution_data`", m, fixed = TRUE), "... and for a data frame")
m <- err(derive_fields(catch, dist[setdiff(names(dist), "domainBiology")], what = "distribution"))
ok(grepl("`distribution_data` has no 'domainBiology'", m, fixed = TRUE),
   "distributions with no domainBiology: named")
m <- err(derive_fields(catch[setdiff(names(catch), "domainBiology")], dist, what = "distribution"))
ok(grepl("`catch_data` has no 'domainBiology'", m, fixed = TRUE),
   "a catch with no domainBiology: named")
m <- err(derive_fields(catch[setdiff(names(catch), "fleetValue")]))
ok(identical(m, "Missing expected column(s): fleetValue"),
   "a catch with no fleetValue: the message it always gave")

## ---- the bundled example ---------------------------------------------------------------------
test_catch <- fx_catch(); test_dist <- fx_dist()
c23 <- subset(test_catch, year == 2023)
d23 <- subset(test_dist, year == 2023)
ha  <- heard(derive_fields(c23, d23, what = "distribution"))
dd23 <- ha$value
ok(nrow(dd23) == nrow(d23) && identical(dd23[names(d23)], blank_na(d23)) && length(ha$warnings) == 0L,
   "fixture: all the rows back, nothing orphaned (the blank cells as NA)")
ok(!anyNA(dd23[c("fleetValue", "areaValue", "metier4", "metier4_mesh", "ICESDivision")]),
   "fixture: fleet, area and the fields from them are there for every row")
# the example's distributions cover whole domains (all seasons): season is the one thing they cannot say
ok(anyNA(dd23$seasonValue) && length(ha$messages) == 1L,
   "fixture: a domain spans seasons, so seasonValue is NA there - and said")
k <- paste(dd23$domainBiology, dd23$catchCategory, dd23$vesselFlagCountry)
one_season <- tapply(c23$seasonValue, paste(c23$domainBiology, c23$catchCategory, c23$vesselFlagCountry),
                     function(s) length(unique(s)) == 1L)
ok(identical(!is.na(dd23$seasonValue), as.vector(one_season[k])),
   "fixture: seasonValue is there exactly where the domain has one season")

## ---- deriving the distributions changes nothing downstream ----------------------------------------
cv0 <- build_coverage_strata(c23, c("metier4", "vesselFlagCountry"), distributions = d23, what = "length")
cv1 <- build_coverage_strata(c23, c("metier4", "vesselFlagCountry"), distributions = dd23, what = "length")
ok(identical(cv0, cv1), "build_coverage_strata(what = \"length\"): the same table")
p0 <- plot_NaLoA(c23, d23, fill = "metier4")
p1 <- plot_NaLoA(c23, dd23, fill = "metier4")
ok(isTRUE(all.equal(as.data.frame(p0$data), as.data.frame(p1$data))),
   "plot_NaLoA(): the same data")
q0 <- plot_MWaLoA(c23, d23, colour = "metier4")
q1 <- plot_MWaLoA(c23, dd23, colour = "metier4")
ok(isTRUE(all.equal(as.data.frame(q0$data), as.data.frame(q1$data))),
   "plot_MWaLoA(): the same data")

raised <- quiet(fx_raise(c23, fx_groups("own"), type = "discards", log_file = lf))
alloc <- function(dist, ty) quiet(fx_allocate(raised, dist, ty, log_file = lf))
for (ty in c("LengthTotal", "Age")) {
  a0 <- alloc(blank_na(d23), ty)          # plain: no derived field, the blank cells as NA like the derived ones
  a1 <- alloc(dd23, ty)
  ok(nrow(a1$distribution) > 0 && identical(a1$distribution[names(a0$distribution)], a0$distribution) &&
       identical(a1$catch, a0$catch),
     sprintf("allocation (%s): derived distributions give exactly what plain ones give", ty))
  ok(isTRUE(all.equal(fx_numbers(a0), fx_numbers(a1))) &&
       isTRUE(all.equal(fx_meanweight(a0), fx_meanweight(a1))),
     sprintf("allocation (%s): and the same numbers and mean weights", ty))
}

## ... but not once a csv round trip has retyped one of them: "87.1" is read back as the number 87.1,
## and the raised catch, still in memory, has it as text - the engine cannot combine the two. A catch and
## its distributions are written and read the same way (both to csv, or neither).
dd_num <- dd23
dd_num$ICESSubArea <- as.numeric(dd_num$ICESSubArea)
ok(!anyNA(dd_num$ICESSubArea) && is.character(raised$ICESSubArea),
   "fixture: ICESSubArea is a number on the distributions and text on the raised catch")
ok(grepl("ICESSubArea", err(alloc(dd_num, "LengthTotal")), fixed = TRUE),
   "allocation: the engine names the column it cannot combine")

## ---- the fields that are derived -----------------------------------------------------
ok(identical(setdiff(names(suppressWarnings(suppressMessages(derive_fields(catch, no_cty, what = "distribution")))),
                     names(no_cty)),
             c("fleetValue", "areaValue", "vesselFlagCountry", "seasonValue", derived_cols)),
   "derive_fields(what = \"distribution\"): adds what it adds to a catch - the country only where the distributions lack it")

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
