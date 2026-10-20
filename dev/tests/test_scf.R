## The stock coordination format: catchCombined() (CC) and distributionsCombined() (DC).
##
## What is pinned: the two tables have the fields of the SCF v4, in its order and of its types, the ones the
## data does not fill `NA`; the numbers are what a table built by hand says they are (a case small enough to
## do on paper - pooled weights, a class no fish fall in, a plus group, the sexes, the quarters); the header
## (the working group year, version, calendar year and the start date of a model year) is checked; the
## sampling (PSU, numPSUs, numTrips, numMeasurements) is taken from the data, a distribution counted once;
## the codes the SCF does not accept are warned about; and the distributions are what
## catchAtAoLAggregation() and meanWoLatAoLAggregation() add up, which are made of the same two functions
## as the DC is.

source("dev/tests/fixture.R")
FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}
# the value of expr, with the warnings it gave
heard <- function(expr) {
  warns <- character()
  val <- withCallingHandlers(expr, warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") })
  list(value = val, warnings = warns)
}
# the value of expr, with the messages it gave
said <- function(expr) {
  msgs <- character()
  val <- withCallingHandlers(expr, message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") })
  list(value = val, messages = msgs)
}
err <- function(expr) tryCatch({ suppressWarnings(expr); "" }, error = function(e) conditionMessage(e))
near <- function(a, b, tol = 1e-9) length(a) == length(b) && isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = tol))
has <- function(txt, pattern) any(grepl(pattern, txt, fixed = TRUE))
suppressPackageStartupMessages(library(dplyr))

wgy <- 2024L   # the working group year every call needs: the data are of 2023
cc_fields <- c("recordType", "workingGroupYear", "version", "versionDescription", "calendarYear", "startDateYear", "year",
               "workingGroup", "stock", "catchCategory", "seasonType", "seasonValue", "areaType", "areaValue", "fleetValue",
               "variableType", "variableUnit", "total", "variance", "PSU", "numPSUs", "numTrips")
dc_fields <- c("recordType", "workingGroupYear", "version", "versionDescription", "calendarYear", "startDateYear", "year",
               "workingGroup", "stock", "catchCategory", "seasonType", "seasonValue", "areaType", "areaValue", "fleetValue",
               "distributionType", "distributionUnit", "distributionClass", "ageGroupPlus", "attributeType",
               "attributeValue", "variableType", "variableUnit", "valueType", "value", "variance", "PSU", "numPSUs",
               "numTrips", "numMeasurements")
ctype <- function(d) vapply(d, function(v) class(v)[1], "")

## ---- A case small enough to work out by hand ------------------------------------------------------
## Four catch rows of one stock (two landings in two quarters, discards, and an official row that is not the
## working group's), a landing that is a count, and for each a distribution.
catch <- data.frame(
  vesselFlagCountry = "BE", year = 2023L, workingGroup = "WGX", stock = "stk.27.1", speciesCode = 100L,
  catchCategory = c("Lan", "Lan", "Dis", "Lan", "Lan"),
  domainBiology = c("D1", "D2", "D3", NA, NA),
  originType = c("WGEstimate", "WGEstimate", "WGEstimate", "Official", "WGEstimate"),
  variableType = c("WeightLive", "WeightLive", "WeightLive", "WeightLive", "Number"),
  variableUnit = c("kg", "kg", "kg", "kg", "N"),
  total = c(1000, 3000, 500, 777, 50),
  seasonType = "Quarter", seasonValue = c(1L, 2L, 1L, 1L, 1L),
  areaType = "ICESArea", areaValue = c("27.4.b", "27.4.b", "27.4.c", "27.4.b", "27.4.b"),
  fleetValue = c("OTB", "OTB", "TBB", "OTB", "OTB"), stringsAsFactors = FALSE)
dist <- function(domain, type, cls, n, w, unit_cls = "mm", attr = NA_character_, attr_type = NA_character_) {
  keys <- data.frame(vesselFlagCountry = "BE", year = 2023L, workingGroup = "WGX", stock = "stk.27.1", speciesCode = 100L,
                     catchCategory = if (domain == "D3") "Dis" else "Lan", domainBiology = domain,
                     distributionType = type, distributionUnit = unit_cls, distributionClass = cls,
                     attributeType = attr_type, attributeValue = attr, stringsAsFactors = FALSE)
  rbind(cbind(keys, variableType = "Number", variableUnit = "N", valueType = "Total", value = n),
        cbind(keys, variableType = "WeightLive", variableUnit = "g", valueType = "Mean", value = w))
}
distributions <- rbind(
  # lengths: D1 and D2 are landings in two quarters, D3 discards. Class 130 has no fish; at class 120 D2 has none
  # either, and its mean weight (99) must weigh nothing
  dist("D1", "LengthTotal", c(100L, 110L, 120L, 130L), c(1000, 2000, 3000, 0), c(10, 20, 30, 40)),
  dist("D2", "LengthTotal", c(100L, 110L, 120L), c(3000, 1000, 0), c(30, 20, 99)),
  dist("D3", "LengthTotal", c(100L, 110L), c(500, 500), c(5, 6)),
  # ages, by sex: D1 has females and males, D2 females
  dist("D1", "Age", 1:3, c(1000, 2000, 3000), c(100, 200, 300), "y", "F", "Sex"),
  dist("D1", "Age", 1:3, c(500, 500, 500), c(90, 190, 290), "y", "M", "Sex"),
  dist("D2", "Age", 1:3, c(1000, 1000, 1000), c(120, 220, 320), "y", "F", "Sex"))
combine <- function(...) distributionsCombined(workingGroupYear = wgy, distributions, catch, originType_catch = "WGEstimate",
                                               variableType_catch = "WeightLive", ...)
row_of <- function(d, ...) { k <- list(...); d[Reduce(`&`, lapply(names(k), function(n) d[[n]] %in% k[[n]])), , drop = FALSE] }

## ---- CC: the fields, the numbers --------------------------------------------------------------------
cc <- catchCombined(workingGroupYear = wgy, catch, originType_catch = "WGEstimate")
ok(identical(names(cc), cc_fields), "CC: the fields of the SCF, in its order")
ok(identical(unname(ctype(cc)),
             c("character", "integer", "integer", "character", "character", "character", "integer", "character",
               "character", "character", "character", "integer", "character", "character", "character", "character",
               "character", "numeric", "numeric", "character", "integer", "integer")),
   "CC: ... and of its types (a year, a version, a season value and a count are whole numbers, a total is not)")
ok(nrow(cc) == 3 && near(row_of(cc, catchCategory = "Lan", variableType = "WeightLive")$total, 4000) &&
     near(row_of(cc, catchCategory = "Dis")$total, 500) && near(row_of(cc, variableType = "Number")$total, 50),
   "CC: the working group's rows added up by category and variable - the official row and the domains do not split them")
ok(all(cc$recordType == "CC") && all(cc$workingGroupYear == wgy) && is.integer(cc$workingGroupYear) &&
     all(cc$version == 1L) && all(cc$calendarYear == "Y") &&
     all(is.na(cc[c("versionDescription", "startDateYear", "seasonType", "seasonValue", "areaType", "areaValue",
                    "fleetValue", "variance", "PSU", "numPSUs", "numTrips")])),
   "CC: the header written on every row, the fields the data does not fill NA")
ok(setequal(cc$variableUnit, c("kg", "N")), "CC: unit = NULL leaves the units as they are")
cct <- heard(catchCombined(workingGroupYear = wgy, catch, originType_catch = "WGEstimate", unit = "t"))
ok(!length(cct$warnings) && near(row_of(cct$value, catchCategory = "Lan", variableType = "WeightLive")$total, 4) &&
     near(row_of(cct$value, catchCategory = "Dis")$total, 0.5) && near(row_of(cct$value, variableType = "Number")$total, 50) &&
     all(row_of(cct$value, variableType = "WeightLive")$variableUnit == "t"),
   "CC: unit = \"t\" turns the weights to tonnes and leaves the numbers")
c2 <- catch; c2$total[1] <- 1234.5678
ok(near(row_of(catchCombined(workingGroupYear = wgy, c2, originType_catch = "WGEstimate", unit = "t", round = 2), catchCategory = "Lan",
               variableType = "WeightLive")$total, 4.23),
   "CC: round")

## origin and variable
both <- heard(catchCombined(workingGroupYear = wgy, catch))
ok(any(grepl("counted twice", both$warnings)) && near(row_of(both$value, catchCategory = "Lan", variableType = "WeightLive")$total, 4777),
   "CC: the default takes both origins, said to be counted twice when the catch has both")
ok(!length(heard(catchCombined(workingGroupYear = wgy, catch, originType_catch = c("WGEstimate", "Official")))$warnings),
   "CC: ... and not said when both are asked for")
off <- catchCombined(workingGroupYear = wgy, catch, originType_catch = "Official")
ok(nrow(off) == 1 && near(off$total, 777), "CC: originType_catch = \"Official\" is that row alone")
ok(nrow(catchCombined(workingGroupYear = wgy, catch, originType_catch = "WGEstimate", variableType_catch = "WeightLive")) == 2,
   "CC: variableType_catch picks the variable")
ok(has(err(catchCombined(workingGroupYear = wgy, catch, originType_catch = "Official", variableType_catch = "Number")), "no catch rows"),
   "CC: nothing to add up is an error that says what was asked for")

## grouping: a season, an area, a fleet - each with its type
g1 <- catchCombined(workingGroupYear = wgy, catch, grouping = c("catchCategory", "seasonValue"), originType_catch = "WGEstimate",
                    variableType_catch = "WeightLive")
ok(nrow(g1) == 3 && all(g1$seasonType == "Quarter") && near(row_of(g1, catchCategory = "Lan", seasonValue = 2L)$total, 3000) &&
     near(row_of(g1, catchCategory = "Lan", seasonValue = 1L)$total, 1000) && near(sum(g1$total), 4500),
   "CC: grouping by seasonValue reports the quarters, with their seasonType, and adds up to the same")
g2 <- catchCombined(workingGroupYear = wgy, catch, grouping = c("catchCategory", "areaValue", "fleetValue"), originType_catch = "WGEstimate",
                    variableType_catch = "WeightLive")
ok(nrow(g2) == 2 && all(g2$areaType == "ICESArea") && setequal(g2$areaValue, c("27.4.b", "27.4.c")) &&
     setequal(g2$fleetValue, c("OTB", "TBB")),
   "CC: ... an area with its areaType, a fleet")
ok(identical(names(g2), cc_fields) && identical(names(g1), cc_fields), "CC: whatever the grouping, the same fields")
e <- err(catchCombined(workingGroupYear = wgy, catch, grouping = c("catchCategory", "metier4"), originType_catch = "WGEstimate"))
ok(has(e, "metier4") && has(e, "seasonValue") && has(e, "areaValue") && has(e, "fleetValue"),
   "CC: a column the SCF does not report by is refused, and the ones it does are named")
ok(has(err(catchCombined(workingGroupYear = wgy, catch[setdiff(names(catch), "seasonValue")], grouping = "seasonValue", originType_catch = "WGEstimate")),
       "no column 'seasonValue'"),
   "CC: a column the catch lacks is named")

## the header: version, calendar year
h <- catchCombined(workingGroupYear = wgy, catch, originType_catch = "WGEstimate", version = 3, versionDescription = "benchmark")
ok(all(h$version == 3L) && is.integer(h$version) && all(h$versionDescription == "benchmark"), "CC: version and versionDescription")
hn <- catchCombined(workingGroupYear = wgy, catch, originType_catch = "WGEstimate", calendarYear = "N", startDateYear = "07-01")
ok(all(hn$calendarYear == "N") && all(hn$startDateYear == "07-01"),
   "CC: calendarYear = \"N\" writes the start of the model year on every row")
ok(has(err(catchCombined(workingGroupYear = wgy, catch, originType_catch = "WGEstimate", calendarYear = "N")), "startDateYear") &&
     has(err(catchCombined(workingGroupYear = wgy, catch, originType_catch = "WGEstimate", calendarYear = "N", startDateYear = "13-45")), "MM-DD") &&
     has(err(catchCombined(workingGroupYear = wgy, catch, originType_catch = "WGEstimate", calendarYear = "X")), "calendarYear") &&
     has(err(catchCombined(workingGroupYear = wgy, catch, originType_catch = "WGEstimate", version = 1.5)), "version"),
   "CC: a model year needs its start date as MM-DD; calendarYear is Y or N; the version is a whole number")
hy <- heard(catchCombined(workingGroupYear = wgy, catch, originType_catch = "WGEstimate", startDateYear = "07-01"))
ok(any(grepl("left blank", hy$warnings)) && all(is.na(hy$value$startDateYear)),
   "CC: a start date with calendarYear = \"Y\" is left blank, and said")

## the working group year: no default, a four-digit year, on every row, and not before the data
ok(has(err(catchCombined(catch, originType_catch = "WGEstimate")), "workingGroupYear") &&
     has(err(distributionsCombined(distributions, catch, originType_catch = "WGEstimate")), "workingGroupYear") &&
     has(err(catchCombined(catch[0, ], originType_catch = "WGEstimate")), "workingGroupYear"),
   "CC and DC: the working group year has no default, and is checked before anything is added up")
ok(all(vapply(list("2024", 24, 2024.5, c(2024, 2025), NA_real_),
              function(v) has(err(catchCombined(catch, originType_catch = "WGEstimate", workingGroupYear = v)),
                              "four-digit year"), NA)),
   "CC: ... it is one four-digit year")
late <- heard(catchCombined(catch, originType_catch = "WGEstimate", workingGroupYear = 2022))
ok(any(grepl("come after the working group year 2022", late$warnings, fixed = TRUE)) &&
     all(late$value$workingGroupYear == 2022L),
   "CC: data that come after the working group year are warned about")
ok(any(grepl("come after the working group year 2022",
             heard(distributionsCombined(distributions, catch, originType_catch = "WGEstimate",
                                         variableType_catch = "WeightLive", distributionType = "LengthTotal",
                                         workingGroupYear = 2022))$warnings, fixed = TRUE)),
   "DC: ... as are the distributions'")

## the codes of the SCF
cw <- catch; cw$catchCategory[1] <- "LAN"; cw$areaType <- "FAO"
w <- heard(catchCombined(workingGroupYear = wgy, cw, grouping = c("catchCategory", "areaValue"), originType_catch = "WGEstimate"))
ok(any(grepl("catchCategory 'LAN'", w$warnings)) && any(grepl("areaType 'FAO'", w$warnings)),
   "CC: a catch category or an area type the SCF does not accept is warned about")
ok(any(grepl("does not go with", heard(catchCombined(workingGroupYear = wgy, transform(catch, variableUnit = "NE3"),
                                                              originType_catch = "WGEstimate"))$warnings)),
   "CC: a unit that does not go with its variable (NE3 for a weight) is warned about")

## ---- CC: the sampling - PSU, numPSUs, numTrips ------------------------------------------------------------
## The discards of three strata - two sampled, by 5 and 3 trips, and one only estimated by the raising, which carries the
## 4 trips of the landings it was made from - and the landings of two, one of them sampled by 7 trips.
cs <- data.frame(
  vesselFlagCountry = "BE", year = 2023L, workingGroup = "WGX", stock = "stk.27.1", speciesCode = 100L,
  catchCategory = c("Dis", "Dis", "Dis", "Lan", "Lan"), domainBiology = c("S1", "S2", "S3", "S4", "S5"),
  originType = "WGEstimate", variableType = "WeightLive", variableUnit = "kg", total = c(100, 200, 300, 1000, 2000),
  PSU = c("FishingTrip", "FishingTrip", "FishingTrip", "FishingTrip", NA),
  numPSUs = c(5, 3, 4, 7, NA), numTrips = c(5, 3, 4, 7, NA),
  importedOrRaised = c("imported", "imported", "raised", "imported", "imported"), stringsAsFactors = FALSE)
samp_cc <- function(x, ...) catchCombined(x, originType_catch = "WGEstimate", workingGroupYear = wgy, ...)
cs_cc <- samp_cc(cs)
ok(nrow(cs_cc) == 2 && identical(names(cs_cc), cc_fields) && near(row_of(cs_cc, catchCategory = "Dis")$total, 600) &&
     near(row_of(cs_cc, catchCategory = "Lan")$total, 3000),
   "CC sampling: the totals are as before - a raised row still has its weight")
d_cs <- row_of(cs_cc, catchCategory = "Dis"); l_cs <- row_of(cs_cc, catchCategory = "Lan")
ok(d_cs$numPSUs == 8L && d_cs$numTrips == 8L && d_cs$PSU == "FishingTrip" && is.integer(cs_cc$numPSUs) && is.integer(cs_cc$numTrips) &&
     l_cs$numPSUs == 7L && l_cs$numTrips == 7L && l_cs$PSU == "FishingTrip",
   "CC sampling: numPSUs and numTrips are summed over the rows added up, a raised row's counts (4) left out, and PSU is that of the rows with a count")
only_none <- samp_cc(cs[cs$domainBiology %in% c("S3", "S5"), ])
ok(all(is.na(only_none[c("PSU", "numPSUs", "numTrips")])),
   "CC sampling: NA, not 0, when no row reports a count - a raised row's PSU is not named either")
ok(row_of(samp_cc(cs[setdiff(names(cs), "importedOrRaised")]), catchCategory = "Dis")$numTrips == 12L,
   "CC sampling: a catch that does not say which rows are raised counts them all")
ok(all(is.na(samp_cc(cs[setdiff(names(cs), c("PSU", "numPSUs", "numTrips"))])[c("PSU", "numPSUs", "numTrips")])),
   "CC sampling: a catch without the columns gets NA")
cs0 <- cs; cs0$numTrips[1:2] <- 0
ok(row_of(samp_cc(cs0), catchCategory = "Dis")$numTrips == 0L, "CC sampling: a count of 0 is a count")
cs3 <- cs; cs3$PSU[2] <- "Location;LandingEvent"
ok(row_of(samp_cc(cs3), catchCategory = "Dis")$PSU == "FishingTrip;LandingEvent;Location",
   "CC sampling: several types of sampling unit are one sorted list, separated by ;")
cs4 <- cs; cs4$PSU[5] <- "Location"; cs4$PSU[2] <- ""
ok(row_of(samp_cc(cs4), catchCategory = "Lan")$PSU == "FishingTrip" && row_of(samp_cc(cs4), catchCategory = "Dis")$PSU == "FishingTrip" &&
     row_of(samp_cc(cs4), catchCategory = "Dis")$numPSUs == 8L,
   "CC sampling: the type of a row with no number of PSUs is not named, and an empty type is no type")
ok(any(grepl("PSU 'Trip' is not a code the SCF accepts", heard(samp_cc(transform(cs, PSU = replace(PSU, 1, "Trip"))))$warnings, fixed = TRUE)),
   "CC sampling: a type of sampling unit the SCF does not accept is warned about")
ok(near(sort(samp_cc(cs, unit = "t")$total), c(0.6, 3)) && all(samp_cc(cs, unit = "t")$numTrips[order(samp_cc(cs, unit = "t")$catchCategory)] == c(8L, 7L)),
   "CC sampling: converting the units leaves the counts as they are")

## ---- DC: the fields, the numbers --------------------------------------------------------------------
dl <- combine(distributionType = "LengthTotal")
ok(identical(names(dl), dc_fields), "DC: the fields of the SCF, in its order")
ok(identical(unname(ctype(dl)),
             c("character", "integer", "integer", "character", "character", "character", "integer", "character",
               "character", "character", "character", "integer", "character", "character", "character", "character",
               "character", "integer", "integer", "character", "character", "character", "character", "character",
               "numeric", "numeric", "character", "integer", "integer", "integer")),
   "DC: ... and of its types (an empty attribute is a string, a class a whole number)")
ok(nrow(dl) == 10 && setequal(dl$catchCategory, c("Lan", "Dis")) && all(dl$distributionUnit == "mm") &&
     all(dl$distributionType == "LengthTotal") && all(is.na(dl$ageGroupPlus)),
   "DC: a Number and a WeightLive row for every class of every category - lengths in mm")
n_row <- function(d, cat, cls, vt, ...) row_of(d, catchCategory = cat, distributionClass = cls, variableType = vt, ...)
ok(near(n_row(dl, "Lan", 100L, "Number")$value, 4) && near(n_row(dl, "Lan", 110L, "Number")$value, 3) &&
     near(n_row(dl, "Lan", 120L, "Number")$value, 3) && near(n_row(dl, "Dis", 100L, "Number")$value, 0.5) &&
     all(row_of(dl, variableType = "Number")$variableUnit == "NE3") && all(row_of(dl, variableType = "Number")$valueType == "Total"),
   "DC: the numbers of the domains added up, in thousands (NE3), as a Total")
ok(near(n_row(dl, "Lan", 100L, "WeightLive")$value, (10 * 1000 + 30 * 3000) / 4000) &&
     near(n_row(dl, "Lan", 110L, "WeightLive")$value, 20) && near(n_row(dl, "Lan", 120L, "WeightLive")$value, 30) &&
     near(n_row(dl, "Dis", 110L, "WeightLive")$value, 6) &&
     all(row_of(dl, variableType = "WeightLive")$variableUnit == "g") && all(row_of(dl, variableType = "WeightLive")$valueType == "Mean"),
   "DC: the mean weights weighted by the numbers - a domain with no fish at a class (D2 at 120) weighs nothing - in g, as a Mean")
ok(!130L %in% dl$distributionClass && !any(is.na(dl$value)) && all(dl$value > 0),
   "DC: a class no fish fall in (130) is not reported: it has no mean weight")
ok(near(n_row(combine(distributionType = "LengthTotal", weighting = "CATON"), "Lan", 120L, "WeightLive")$value, (30 * 1000 + 99 * 3000) / 4000),
   "DC: weighting = \"CATON\" weights by the catch of each domain, as meanWoLatAoLAggregation()")

## the units and the rounding
du <- combine(distributionType = "LengthTotal", unit = c("N", "kg"))
ok(near(n_row(du, "Lan", 100L, "Number")$value, 4000) && near(n_row(du, "Lan", 100L, "WeightLive")$value, 0.025) &&
     all(row_of(du, variableType = "Number")$variableUnit == "N") && all(row_of(du, variableType = "WeightLive")$variableUnit == "kg"),
   "DC: unit = c(\"N\", \"kg\")")
dn <- combine(distributionType = "LengthTotal", unit = NULL)
ok(near(n_row(dn, "Lan", 100L, "Number")$value, 4000) && all(row_of(dn, variableType = "Number")$variableUnit == "N") &&
     all(row_of(dn, variableType = "WeightLive")$variableUnit == "g"),
   "DC: unit = NULL leaves the numbers in N and the weights in g, which the SCF accepts")
ok(near(row_of(combine(distributionType = "Age", round = 1), attributeValue = "F", distributionClass = 2L,
                 variableType = "WeightLive")$value, 206.7),
   "DC: round")

## ages: sex, the plus group
da <- combine(distributionType = "Age")
ok(nrow(da) == 12 && setequal(da$attributeValue, c("F", "M")) && all(da$attributeType == "Sex") && all(da$distributionUnit == "y") &&
     all(da$distributionType == "Age"),
   "DC: ages by sex - the attribute is part of the default grouping")
a_row <- function(d, sex, cls, vt) row_of(d, attributeValue = sex, distributionClass = cls, variableType = vt)
ok(near(a_row(da, "F", 2L, "Number")$value, 3) && near(a_row(da, "F", 2L, "WeightLive")$value, (200 * 2000 + 220 * 1000) / 3000) &&
     near(a_row(da, "M", 1L, "Number")$value, 0.5) && near(a_row(da, "M", 1L, "WeightLive")$value, 90),
   "DC: ... the sexes apart, each added up on its own")
dp <- combine(distributionType = "Age", maxAoL = 2)
ok(nrow(dp) == 8 && all(dp$ageGroupPlus == 2L) && max(dp$distributionClass) == 2L &&
     near(a_row(dp, "F", 2L, "Number")$value, 3 + 4) &&
     near(a_row(dp, "F", 2L, "WeightLive")$value, (200 * 2000 + 220 * 1000 + 300 * 3000 + 320 * 1000) / 7000),
   "DC: maxAoL = 2 is a plus group: ages 2 and older in one class, ageGroupPlus on every row of the distribution")
dc <- combine(distributionType = "Age", maxAoL = 2, plusGroup = FALSE)
ok(nrow(dc) == 8 && all(is.na(dc$ageGroupPlus)) && max(dc$distributionClass) == 2L && near(a_row(dc, "F", 2L, "Number")$value, 3),
   "DC: ... and plusGroup = FALSE only cuts the classes off")
ok(!any(combine(distributionType = "Age", minAoL = 2)$distributionClass == 1L), "DC: minAoL drops the classes below it")
ok(has(err(combine(distributionType = "LengthTotal", maxAoL = 110)), "no plus group for lengths") &&
     max(combine(distributionType = "LengthTotal", maxAoL = 110, plusGroup = FALSE)$distributionClass) == 110L,
   "DC: the SCF has no plus group for lengths - maxAoL then only cuts, with plusGroup = FALSE")
ok(near(a_row(combine(distributionType = "Age", grouping = "catchCategory"), NA_character_, 1L, "Number")$value, 2.5) &&
     all(is.na(combine(distributionType = "Age", grouping = "catchCategory")$attributeType)),
   "DC: grouping without the attributes pools the sexes, and leaves them NA")

## several distribution types at once, each with its own classes
dm <- combine(distributionType = c("Age", "LengthTotal"), maxAoL = c(Age = 2), plusGroup = c(Age = TRUE, LengthTotal = FALSE))
ok(setequal(dm$distributionType, c("Age", "LengthTotal")) && nrow(dm) == nrow(da) - 4 + nrow(dl) &&
     max(dm$distributionClass[dm$distributionType == "Age"]) == 2L && !130L %in% dm$distributionClass &&
     max(dm$distributionClass[dm$distributionType == "LengthTotal"]) == 120L,
   "DC: distributionType can be several, and maxAoL / plusGroup one for each")
ok(has(err(combine(distributionType = c("Age", "LengthTotal"), maxAoL = c(1, 2))), "one value, or one per distribution type"),
   "DC: ... a vector that is not named is refused")

## season, area: the same grouping as the CC
dq <- combine(distributionType = "LengthTotal", grouping = c("catchCategory", "seasonValue"))
ok(all(dq$seasonType == "Quarter") && near(n_row(dq, "Lan", 100L, "Number", seasonValue = 1L)$value, 1) &&
     near(n_row(dq, "Lan", 100L, "Number", seasonValue = 2L)$value, 3) &&
     near(n_row(dq, "Lan", 100L, "WeightLive", seasonValue = 2L)$value, 30),
   "DC: grouping by seasonValue reports the quarters, with their seasonType, as the CC does")
ok(has(err(combine(distributionType = "LengthTotal", grouping = c("catchCategory", "metier4"))), "metier4"),
   "DC: a column the SCF does not report by is refused")

## the link to the catch
lost <- heard(distributionsCombined(workingGroupYear = wgy, distributions, catch[catch$domainBiology %in% c("D1", NA), ], originType_catch = "WGEstimate",
                                    variableType_catch = "WeightLive", distributionType = "LengthTotal"))
ok(any(grepl("2 of the 3 distributions with fish have no catch weight to go with", lost$warnings)) && nrow(lost$value) < nrow(dl),
   "DC: a distribution with fish and no catch weight to go with is left out, and said")
nodist <- heard(distributionsCombined(workingGroupYear = wgy, distributions[distributions$domainBiology != "D2", ], catch, originType_catch = "WGEstimate",
                                      variableType_catch = "WeightLive", distributionType = "LengthTotal"))
ok(any(grepl("1 of the 3 catch strata with a weight have no LengthTotal distribution", nodist$warnings)) &&
     has(nodist$warnings, "3000 kg") && has(nodist$warnings, "BE | D2"),
   "DC: a catch weight with no distribution to go with is said, with how much of it and which stratum")
ok(has(err(distributionsCombined(workingGroupYear = wgy, distributions[setdiff(names(distributions), "distributionClass")], catch,
                                 originType_catch = "WGEstimate")), "no column 'distributionClass'"),
   "DC: a column the distributions lack is named")
ok(any(grepl("counted twice", heard(distributionsCombined(workingGroupYear = wgy, distributions, catch, variableType_catch = "WeightLive",
                                                           distributionType = "LengthTotal"))$warnings)),
   "DC: the default origin warns as the CC's does")
hd <- combine(distributionType = "LengthTotal", version = 2, versionDescription = "x", calendarYear = "N", startDateYear = "04-01")
ok(all(hd$version == 2L) && all(hd$calendarYear == "N") && all(hd$startDateYear == "04-01") && all(hd$recordType == "DC") &&
     all(hd$workingGroupYear == wgy) && is.integer(hd$workingGroupYear),
   "DC: the same header as the CC")

## ---- DC: the sampling - PSU, numPSUs, numTrips, numMeasurements -------------------------------------------------
## D1 was sampled by 5 trips (100 fish measured), D2 by 3 (40); D3, the discards, by nobody. The CEF repeats the counts of
## a distribution on each of its classes.
samp <- list(D1 = c(5, 100), D2 = c(3, 40))
ds <- distributions
ds$PSU <- ifelse(ds$domainBiology %in% names(samp), "FishingTrip", NA_character_)
ds$numPSUs <- ds$numTrips <- vapply(ds$domainBiology, function(d) if (d %in% names(samp)) samp[[d]][1] else NA_real_, 0)
ds$numMeasurements <- vapply(ds$domainBiology, function(d) if (d %in% names(samp)) samp[[d]][2] else NA_real_, 0)
samp_dc <- function(dist_x, ...) distributionsCombined(dist_x, catch, originType_catch = "WGEstimate", variableType_catch = "WeightLive",
                                                       workingGroupYear = wgy, ...)
sampling <- c("PSU", "numPSUs", "numTrips", "numMeasurements")
ok(all(is.na(dl[sampling])), "DC sampling: NA when the distributions do not have the columns")
sl <- samp_dc(ds, distributionType = "LengthTotal")
lan <- row_of(sl, catchCategory = "Lan"); dis <- row_of(sl, catchCategory = "Dis")
ok(identical(names(sl), dc_fields) && all(lan$numPSUs == 8L) && all(lan$numTrips == 8L) && all(lan$numMeasurements == 140L) &&
     all(lan$PSU == "FishingTrip") && all(vapply(sl[c("numPSUs", "numTrips", "numMeasurements")], is.integer, NA)),
   "DC sampling: the counts of the distributions a class is added up from are summed (5 + 3 trips, 100 + 40 fish), on the numbers and on the mean weights")
ok(all(is.na(dis[sampling])), "DC sampling: NA for a distribution nobody sampled - not 0, and no type of sampling unit")
ok(near(sl$value[order(sl$catchCategory, sl$variableType, sl$distributionClass)],
        dl$value[order(dl$catchCategory, dl$variableType, dl$distributionClass)]),
   "DC sampling: the numbers and the mean weights are those without the counts")
ds_est <- ds; ds_est[ds_est$domainBiology == "D2", sampling] <- NA
lan_est <- row_of(samp_dc(ds_est, distributionType = "LengthTotal"), catchCategory = "Lan")
ok(all(lan_est$numPSUs == 5L) && all(lan_est$numMeasurements == 100L) && all(lan_est$PSU == "FishingTrip"),
   "DC sampling: a distribution that was estimated (no counts) adds none to the ones that were sampled")

## a plus group puts several classes of a distribution in one row: that distribution is counted once
sp <- samp_dc(ds, distributionType = "Age", maxAoL = 2)
sp_f <- row_of(sp, attributeValue = "F", distributionClass = 2L, variableType = "Number")
sp_m <- row_of(sp, attributeValue = "M", distributionClass = 2L, variableType = "Number")
ok(sp_f$numPSUs == 8L && sp_f$numMeasurements == 140L && sp_m$numPSUs == 5L && sp_m$numMeasurements == 100L &&
     row_of(sp, attributeValue = "F", distributionClass = 1L, variableType = "Number")$numPSUs == 8L,
   "DC sampling: a plus group counts a distribution once, not once for each class it pools (D1 and D2 for the females, D1 for the males)")
ds_cls <- ds; ds_cls$numMeasurements <- ds_cls$numMeasurements + ds_cls$distributionClass
sp_c <- row_of(samp_dc(ds_cls, distributionType = "Age", maxAoL = 2), attributeValue = "F", distributionClass = 2L, variableType = "Number")
ok(sp_c$numMeasurements == (100 + 2) + (100 + 3) + (40 + 2) + (40 + 3) && sp_c$numPSUs == 8L,
   "DC sampling: counts that differ from class to class are all added up in the plus group")
ds_psu <- ds; ds_psu$PSU[ds_psu$domainBiology %in% "D2"] <- "Location"
sx_f <- row_of(samp_dc(ds_psu, distributionType = "Age"), attributeValue = "F", distributionClass = 1L, variableType = "Number")
sx_m <- row_of(samp_dc(ds_psu, distributionType = "Age"), attributeValue = "M", distributionClass = 1L, variableType = "Number")
sx_p <- row_of(samp_dc(ds_psu, distributionType = "Age", grouping = "catchCategory"), distributionClass = 1L, variableType = "Number")
ok(sx_f$PSU == "FishingTrip;Location" && sx_m$PSU == "FishingTrip" && sx_f$numPSUs == 8L && sx_m$numPSUs == 5L,
   "DC sampling: the types of sampling unit of the distributions of a row make a list (D1 and D2 for the females, D1 for the males)")
ok(sx_p$PSU == "FishingTrip;Location" && sx_p$numPSUs == 8L && sx_p$numMeasurements == 140L,
   "DC sampling: with the sexes pooled a distribution is still counted once - D1's trips are not its females' plus its males'")

## and the two go together: the same keys
ok(all(dl$year %in% cc$year) && all(dl$workingGroup %in% cc$workingGroup) && all(dl$stock %in% cc$stock) &&
     all(paste(dl$catchCategory) %in% cc$catchCategory),
   "CC and DC: every category of the DC is in the CC")

## ---- the real thing: an allocation of the fixture --------------------------------------------------------
fx <- fx_allocation()
al <- fx$len
lens <- distributionsCombined(workingGroupYear = wgy, al$distribution, al$catch, originType_catch = "WGEstimate", variableType_catch = "WeightLive",
                              distributionType = "LengthTotal", unit = c("NE3", "g"))
wide_n <- fx_pivot_long(fx_numbers(al, grouping = "catchCategory", round = NULL, unit = "NE3"), "N")
wide_w <- fx_pivot_long(fx_meanweight(al, grouping = "catchCategory", round = NULL, unit = "g"), "W")
key <- function(d, cls) paste(d$catchCategory, d[[cls]])
ln <- row_of(lens, variableType = "Number"); lw <- row_of(lens, variableType = "WeightLive")
wn <- wide_n[wide_n$value > 0, ]
ok(nrow(ln) > 5 && setequal(key(ln, "distributionClass"), key(wn, ".cls")) &&
     near(ln$value[match(key(wn, ".cls"), key(ln, "distributionClass"))], wn$value),
   "DC: the numbers are those of catchAtAoLAggregation(), class by class")
ww <- wide_w[is.finite(wide_w$value), ]
ok(setequal(key(lw, "distributionClass"), key(ww, ".cls")) &&
     near(lw$value[match(key(ww, ".cls"), key(lw, "distributionClass"))], ww$value),
   "DC: ... the mean weights those of meanWoLatAoLAggregation()")
raised_cc <- catchCombined(workingGroupYear = wgy, fx$raised, originType_catch = "WGEstimate", variableType_catch = "WeightLive")
ok(near(sort(raised_cc$total), sort(tapply(fx$raised$total, fx$raised$catchCategory, sum, na.rm = TRUE))),
   "CC: the raised catch of the engine added up by category")
## the fixture's raised rows carry numTrips (the landings' they were made from); the CC counts the imported rows' only
fr <- subset(fx$raised, originType == "WGEstimate" & variableType == "WeightLive")
trips <- tapply(ifelse(fr$importedOrRaised %in% "raised", NA, fr$numTrips), fr$catchCategory,
                function(x) if (all(is.na(x))) NA_integer_ else as.integer(sum(x, na.rm = TRUE)))
ok(any(fr$importedOrRaised == "raised" & !is.na(fr$numTrips)) && any(!is.na(trips)) &&
     identical(as.integer(raised_cc$numTrips[match(names(trips), raised_cc$catchCategory)]), as.integer(trips)) &&
     all(is.na(raised_cc$numPSUs)) && all(is.na(raised_cc$PSU)),
   "CC: the trips of the raised catch of the engine are those of its imported rows - the raised rows' are left out - and the PSUs it does not count are NA")
ages <- distributionsCombined(workingGroupYear = wgy, fx$age$distribution, fx$age$catch, originType_catch = "WGEstimate", variableType_catch = "WeightLive",
                              distributionType = "Age", maxAoL = 2)
ok(nrow(ages) > 0 && all(ages$distributionUnit == "y") && all(ages$ageGroupPlus == 2L) && max(ages$distributionClass) == 2L,
   "DC: the allocation of ages, with a plus group")
## the sampling of the allocation: a class holds the counts of the sampled distributions linked to it, the estimated ones have none
sel <- subset(al$catch, originType == "WGEstimate" & variableType == "WeightLive")
sel$.dom <- catch_domain(sel)
link_keys <- c("vesselFlagCountry", "year", "workingGroup", "stock", "speciesCode", "catchCategory")
link <- merge(subset(al$distribution, distributionType == "LengthTotal" & variableType == "Number"),
              unique(sel[c(link_keys, ".dom")]), by.x = c(link_keys, "domainBiology"), by.y = c(link_keys, ".dom"))
tr_hand <- tapply(link$numTrips, paste(link$catchCategory, link$distributionClass),
                  function(x) if (all(is.na(x))) NA_integer_ else as.integer(sum(x, na.rm = TRUE)))
me_hand <- tapply(link$numMeasurements, paste(link$catchCategory, link$distributionClass),
                  function(x) if (all(is.na(x))) NA_integer_ else as.integer(sum(x, na.rm = TRUE)))
ok(any(!is.na(tr_hand)) && any(is.na(tr_hand)) &&
     identical(as.integer(ln$numTrips), as.integer(tr_hand[key(ln, "distributionClass")])) &&
     identical(as.integer(ln$numMeasurements), as.integer(me_hand[key(ln, "distributionClass")])) &&
     identical(ln$numPSUs, ln$numTrips) && all(ln$PSU[! is.na(ln$numPSUs)] == "FishingTrip") && all(is.na(ln$PSU[is.na(ln$numPSUs)])),
   "DC: the sampling of the allocation is the sum of the sampled distributions linked to each class (none where all were estimated), the same on the mean weights")
ok(identical(as.integer(lw$numTrips[match(key(ln, "distributionClass"), key(lw, "distributionClass"))]), as.integer(ln$numTrips)),
   "DC: ... and the mean weights carry the counts of the numbers")

## ---- the raised catch the allocation was run on, instead of the allocation's own catch -------------------------
## The strata the allocation estimated a distribution for have no domainBiology in the raised catch, and the allocation
## gives them (and the distributions) one it makes up: "[alloc]_<season>_<area>_<unit>_<fleet>_<category>". A catch row with
## no domain is given the same, so the raised catch finds the estimated distributions as the catch of the result does -
## raised or imported, sampled or allocated.
dc_of <- function(catch_x) said(heard(distributionsCombined(workingGroupYear = wgy, al$distribution, catch_x, originType_catch = "WGEstimate",
                                                            variableType_catch = "WeightLive", distributionType = "LengthTotal",
                                                            unit = c("NE3", "g"))))
lens_raised <- dc_of(fx$catch1)
ok(!length(lens_raised$messages) && isTRUE(all.equal(lens_raised$value$warnings, dc_of(al$catch)$value$warnings)) &&
     isTRUE(all.equal(as.data.frame(lens_raised$value$value), as.data.frame(lens))),
   "DC: the raised catch the allocation was run on gives the DC of the allocation's own catch, and says what it says - no message")
made <- catch_domain(fx$catch1)
est <- unique(al$distribution$domainBiology[startsWith(al$distribution$domainBiology, "[alloc]")])
has_dom <- !is.na(fx$catch1$domainBiology)
ok(length(est) > 5 && all(est %in% made) && identical(made[has_dom], fx$catch1$domainBiology[has_dom]),
   "catch_domain(): every domain the allocation made up is made from the raised catch, and a domain the catch has is kept")
more <- dc_of(fx$raised)
ok(any(grepl("catch strata with a weight have no LengthTotal distribution", more$value$warnings)) &&
     isTRUE(all.equal(as.data.frame(more$value$value), as.data.frame(lens))),
   "DC: a raised catch with strata the allocation was not run on says so, and adds up the same")
wide_args <- list(grouping = "catchCategory", originType_catch = "WGEstimate", variableType_catch = "WeightLive",
                  distributionType = "LengthTotal")
wn_a <- do.call(catchAtAoLAggregation, c(list(al$distribution, al$catch), wide_args, unit = "NE3"))
wn_r <- do.call(catchAtAoLAggregation, c(list(al$distribution, fx$catch1), wide_args, unit = "NE3"))
ww_a <- do.call(meanWoLatAoLAggregation, c(list(al$distribution, al$catch), wide_args, unit = "g", plusGroup = FALSE))
ww_r <- do.call(meanWoLatAoLAggregation, c(list(al$distribution, fx$catch1), wide_args, unit = "g", plusGroup = FALSE))
ok(isTRUE(all.equal(wn_a, wn_r)) && isTRUE(all.equal(ww_a, ww_r)) && sum(wn_r[startsWith(names(wn_r), "N_")]) > 0,
   "catchAtAoLAggregation() and meanWoLatAoLAggregation(): the raised catch gives what the allocation's own catch does")
ok(identical(catchCombined(workingGroupYear = wgy, fx$raised, originType_catch = "WGEstimate"),
             catchCombined(workingGroupYear = wgy, transform(fx$raised, domainBiology = NA_character_), originType_catch = "WGEstimate")),
   "CC: does not use domainBiology - the catch is added up whatever domain the rows have")

## a case worked out by hand: a stratum with no domain, and the distribution the allocation made up for it
catch_a <- rbind(catch, data.frame(vesselFlagCountry = "BE", year = 2023L, workingGroup = "WGX", stock = "stk.27.1",
                                   speciesCode = 100L, catchCategory = "Lan", domainBiology = NA_character_,
                                   originType = "WGEstimate", variableType = "WeightLive", variableUnit = "kg", total = 2000,
                                   seasonType = "Quarter", seasonValue = 3L, areaType = "ICESArea", areaValue = "27.4.b",
                                   fleetValue = "OTB", stringsAsFactors = FALSE))
catch_a$fisheriesManagementUnit <- NA_character_
dist_a <- rbind(distributions, dist("[alloc]_3_27.4.b_NA_OTB_Lan", "LengthTotal", c(100L, 110L), c(2000, 500), c(10, 40)))
by_hand <- function(catch_x, origin = "WGEstimate")
  said(heard(distributionsCombined(workingGroupYear = wgy, dist_a, catch_x, originType_catch = origin,
                                   variableType_catch = "WeightLive", distributionType = "LengthTotal")))
a1 <- by_hand(catch_a)
a1d <- a1$value$value
ok(!length(a1$value$warnings) && !length(a1$messages) &&
     near(row_of(a1d, catchCategory = "Lan", distributionClass = 100L, variableType = "Number")$value, 6) &&
     near(row_of(a1d, catchCategory = "Lan", distributionClass = 110L, variableType = "Number")$value, 3.5) &&
     near(row_of(a1d, catchCategory = "Lan", distributionClass = 100L, variableType = "WeightLive")$value, (10 * 1000 + 30 * 3000 + 10 * 2000) / 6000) &&
     near(row_of(a1d, catchCategory = "Lan", distributionClass = 110L, variableType = "WeightLive")$value, (20 * 2000 + 20 * 1000 + 40 * 500) / 3500),
   "DC by hand: a stratum with no domainBiology is given the allocation's, and its estimated distribution is added up")
catch_b <- catch_a; catch_b$domainBiology[nrow(catch_b)] <- "[alloc]_3_27.4.b_NA_OTB_Lan"
a2 <- by_hand(catch_b)
ok(!length(a2$messages) && isTRUE(all.equal(as.data.frame(a1d), as.data.frame(a2$value$value))),
   "DC by hand: ... the same as when the catch already has that domain, which is then left alone")
a3 <- by_hand(catch_a[setdiff(names(catch_a), "fisheriesManagementUnit")])
ok(has(a3$value$warnings, "have no LengthTotal distribution") && has(a3$value$warnings, "have no catch weight"),
   "DC by hand: a catch that lacks what the domain is made from cannot be given it: the weight with no distribution and the distribution with no weight are both said")
## a stratum with a catch row of each origin counts its distribution once
catch_c <- rbind(catch_b, data.frame(vesselFlagCountry = "BE", year = 2023L, workingGroup = "WGX", stock = "stk.27.1",
                                     speciesCode = 100L, catchCategory = "Lan", domainBiology = NA_character_,
                                     originType = "Official", variableType = "WeightLive", variableUnit = "kg", total = 999,
                                     seasonType = "Quarter", seasonValue = 3L, areaType = "ICESArea", areaValue = "27.4.b",
                                     fleetValue = "OTB", fisheriesManagementUnit = NA_character_, stringsAsFactors = FALSE))
both <- by_hand(catch_c, origin = c("WGEstimate", "Official"))
ok(isTRUE(all.equal(as.data.frame(both$value$value), as.data.frame(a2$value$value))) &&
     length(both$value$warnings) == 1 && has(both$value$warnings, "777 kg"),
   "DC by hand: a stratum with a catch row of each origin counts its distribution once, not once for each row - and the Official row with none is said")

## ---- the architecture: one adding up, three ways to read it ---------------------------------------------------
ok(all(c("catchAtAoL", "spread_AoL") %in% all.names(body(catchAtAoLAggregation))) &&
     all(c("meanWoLatAoL", "spread_AoL") %in% all.names(body(meanWoLatAoLAggregation))) &&
     all(c("catchAtAoL", "meanWoLatAoL", "scf_table") %in% all.names(body(distributionsCombined))),
   "catchAtAoLAggregation(), meanWoLatAoLAggregation() and distributionsCombined() are made of the same two functions")
ok(all(vapply(list(catchAtAoL, meanWoLatAoL), function(f) all(c("catch_domain", "link_check") %in% all.names(body(f))), NA)),
   "... and the two of them link the catch of an allocation and the raised catch to the distributions the same way (catch_domain()), and say what does not go together")

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
