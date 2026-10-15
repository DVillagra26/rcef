## The stock coordination format: catchCombined() (CC) and distributionsCombined() (DC).
##
## What is pinned: the two tables have the fields of the SCF v3, in its order and of its types, the ones the
## data does not fill `NA`; the numbers are what a table built by hand says they are (a case small enough to
## do on paper - pooled weights, a class no fish fall in, a plus group, the sexes, the quarters); the header
## (version, calendar year and the start date of a model year) is checked; the codes the SCF does not accept
## are warned about; and the distributions are what catchAtAoLAggregation() and meanWoLatAoLAggregation()
## add up, which are made of the same two functions as the DC is.

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

cc_fields <- c("recordType", "version", "versionDescription", "calendarYear", "yearStartDate", "year", "workingGroup",
               "stock", "catchCategory", "seasonType", "seasonValue", "areaType", "areaValue", "fleetValue",
               "variableType", "variableUnit", "total", "variance", "numTrips")
dc_fields <- c("recordType", "version", "versionDescription", "calendarYear", "yearStartDate", "year", "workingGroup",
               "stock", "catchCategory", "seasonType", "seasonValue", "areaType", "areaValue", "fleetValue",
               "distributionType", "distributionUnit", "distributionClass", "ageGroupPlus", "attributeType",
               "attributeValue", "variableType", "variableUnit", "valueType", "value", "variance", "numTrips",
               "numMeasurements")
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
combine <- function(...) distributionsCombined(distributions, catch, originType_catch = "WGEstimate",
                                               variableType_catch = "WeightLive", ...)
row_of <- function(d, ...) { k <- list(...); d[Reduce(`&`, lapply(names(k), function(n) d[[n]] %in% k[[n]])), , drop = FALSE] }

## ---- CC: the fields, the numbers --------------------------------------------------------------------
cc <- catchCombined(catch, originType_catch = "WGEstimate")
ok(identical(names(cc), cc_fields), "CC: the fields of the SCF, in its order")
ok(identical(unname(ctype(cc)),
             c("character", "integer", "character", "character", "character", "integer", "character", "character",
               "character", "character", "integer", "character", "character", "character", "character", "character",
               "numeric", "numeric", "integer")),
   "CC: ... and of its types (a year, a version and a season value are whole numbers, a total is not)")
ok(nrow(cc) == 3 && near(row_of(cc, catchCategory = "Lan", variableType = "WeightLive")$total, 4000) &&
     near(row_of(cc, catchCategory = "Dis")$total, 500) && near(row_of(cc, variableType = "Number")$total, 50),
   "CC: the working group's rows added up by category and variable - the official row and the domains do not split them")
ok(all(cc$recordType == "CC") && all(cc$version == 1L) && all(cc$calendarYear == "Y") &&
     all(is.na(cc[c("versionDescription", "yearStartDate", "seasonType", "seasonValue", "areaType", "areaValue",
                    "fleetValue", "variance", "numTrips")])),
   "CC: the header written on every row, the fields the data does not fill NA")
ok(setequal(cc$variableUnit, c("kg", "N")), "CC: unit = NULL leaves the units as they are")
cct <- heard(catchCombined(catch, originType_catch = "WGEstimate", unit = "t"))
ok(!length(cct$warnings) && near(row_of(cct$value, catchCategory = "Lan", variableType = "WeightLive")$total, 4) &&
     near(row_of(cct$value, catchCategory = "Dis")$total, 0.5) && near(row_of(cct$value, variableType = "Number")$total, 50) &&
     all(row_of(cct$value, variableType = "WeightLive")$variableUnit == "t"),
   "CC: unit = \"t\" turns the weights to tonnes and leaves the numbers")
c2 <- catch; c2$total[1] <- 1234.5678
ok(near(row_of(catchCombined(c2, originType_catch = "WGEstimate", unit = "t", round = 2), catchCategory = "Lan",
               variableType = "WeightLive")$total, 4.23),
   "CC: round")

## origin and variable
both <- heard(catchCombined(catch))
ok(any(grepl("counted twice", both$warnings)) && near(row_of(both$value, catchCategory = "Lan", variableType = "WeightLive")$total, 4777),
   "CC: the default takes both origins, said to be counted twice when the catch has both")
ok(!length(heard(catchCombined(catch, originType_catch = c("WGEstimate", "Official")))$warnings),
   "CC: ... and not said when both are asked for")
off <- catchCombined(catch, originType_catch = "Official")
ok(nrow(off) == 1 && near(off$total, 777), "CC: originType_catch = \"Official\" is that row alone")
ok(nrow(catchCombined(catch, originType_catch = "WGEstimate", variableType_catch = "WeightLive")) == 2,
   "CC: variableType_catch picks the variable")
ok(has(err(catchCombined(catch, originType_catch = "Official", variableType_catch = "Number")), "no catch rows"),
   "CC: nothing to add up is an error that says what was asked for")

## grouping: a season, an area, a fleet - each with its type
g1 <- catchCombined(catch, grouping = c("catchCategory", "seasonValue"), originType_catch = "WGEstimate",
                    variableType_catch = "WeightLive")
ok(nrow(g1) == 3 && all(g1$seasonType == "Quarter") && near(row_of(g1, catchCategory = "Lan", seasonValue = 2L)$total, 3000) &&
     near(row_of(g1, catchCategory = "Lan", seasonValue = 1L)$total, 1000) && near(sum(g1$total), 4500),
   "CC: grouping by seasonValue reports the quarters, with their seasonType, and adds up to the same")
g2 <- catchCombined(catch, grouping = c("catchCategory", "areaValue", "fleetValue"), originType_catch = "WGEstimate",
                    variableType_catch = "WeightLive")
ok(nrow(g2) == 2 && all(g2$areaType == "ICESArea") && setequal(g2$areaValue, c("27.4.b", "27.4.c")) &&
     setequal(g2$fleetValue, c("OTB", "TBB")),
   "CC: ... an area with its areaType, a fleet")
ok(identical(names(g2), cc_fields) && identical(names(g1), cc_fields), "CC: whatever the grouping, the same fields")
e <- err(catchCombined(catch, grouping = c("catchCategory", "metier4"), originType_catch = "WGEstimate"))
ok(has(e, "metier4") && has(e, "seasonValue") && has(e, "areaValue") && has(e, "fleetValue"),
   "CC: a column the SCF does not report by is refused, and the ones it does are named")
ok(has(err(catchCombined(catch[setdiff(names(catch), "seasonValue")], grouping = "seasonValue", originType_catch = "WGEstimate")),
       "no column 'seasonValue'"),
   "CC: a column the catch lacks is named")

## the header: version, calendar year
h <- catchCombined(catch, originType_catch = "WGEstimate", version = 3, versionDescription = "benchmark")
ok(all(h$version == 3L) && is.integer(h$version) && all(h$versionDescription == "benchmark"), "CC: version and versionDescription")
hn <- catchCombined(catch, originType_catch = "WGEstimate", calendarYear = "N", yearStartDate = "07-01")
ok(all(hn$calendarYear == "N") && all(hn$yearStartDate == "07-01"),
   "CC: calendarYear = \"N\" writes the start of the model year on every row")
ok(has(err(catchCombined(catch, originType_catch = "WGEstimate", calendarYear = "N")), "yearStartDate") &&
     has(err(catchCombined(catch, originType_catch = "WGEstimate", calendarYear = "N", yearStartDate = "13-45")), "MM-DD") &&
     has(err(catchCombined(catch, originType_catch = "WGEstimate", calendarYear = "X")), "calendarYear") &&
     has(err(catchCombined(catch, originType_catch = "WGEstimate", version = 1.5)), "version"),
   "CC: a model year needs its start date as MM-DD; calendarYear is Y or N; the version is a whole number")
hy <- heard(catchCombined(catch, originType_catch = "WGEstimate", yearStartDate = "07-01"))
ok(any(grepl("left blank", hy$warnings)) && all(is.na(hy$value$yearStartDate)),
   "CC: a start date with calendarYear = \"Y\" is left blank, and said")

## the codes of the SCF
cw <- catch; cw$catchCategory[1] <- "LAN"; cw$areaType <- "FAO"
w <- heard(catchCombined(cw, grouping = c("catchCategory", "areaValue"), originType_catch = "WGEstimate"))
ok(any(grepl("catchCategory 'LAN'", w$warnings)) && any(grepl("areaType 'FAO'", w$warnings)),
   "CC: a catch category or an area type the SCF does not accept is warned about")
ok(any(grepl("does not go with", heard(catchCombined(transform(catch, variableUnit = "NE3"),
                                                              originType_catch = "WGEstimate"))$warnings)),
   "CC: a unit that does not go with its variable (NE3 for a weight) is warned about")

## ---- DC: the fields, the numbers --------------------------------------------------------------------
dl <- combine(distributionType = "LengthTotal")
ok(identical(names(dl), dc_fields), "DC: the fields of the SCF, in its order")
ok(identical(unname(ctype(dl)),
             c("character", "integer", "character", "character", "character", "integer", "character", "character",
               "character", "character", "integer", "character", "character", "character", "character", "character",
               "integer", "integer", "character", "character", "character", "character", "character", "numeric",
               "numeric", "integer", "integer")),
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
lost <- heard(distributionsCombined(distributions, catch[catch$domainBiology %in% c("D1", NA), ], originType_catch = "WGEstimate",
                                    variableType_catch = "WeightLive", distributionType = "LengthTotal"))
ok(any(grepl("domains have no catch row", lost$warnings)) && any(grepl("alloc$catch", lost$warnings, fixed = TRUE)) &&
     nrow(lost$value) < nrow(dl),
   "DC: distributions with no catch row to go with are left out, and said - with where the catch of an allocation is")
ok(has(err(distributionsCombined(distributions[setdiff(names(distributions), "distributionClass")], catch,
                                 originType_catch = "WGEstimate")), "no column 'distributionClass'"),
   "DC: a column the distributions lack is named")
ok(any(grepl("counted twice", heard(distributionsCombined(distributions, catch, variableType_catch = "WeightLive",
                                                           distributionType = "LengthTotal"))$warnings)),
   "DC: the default origin warns as the CC's does")
hd <- combine(distributionType = "LengthTotal", version = 2, versionDescription = "x", calendarYear = "N", yearStartDate = "04-01")
ok(all(hd$version == 2L) && all(hd$calendarYear == "N") && all(hd$yearStartDate == "04-01") && all(hd$recordType == "DC"),
   "DC: the same header as the CC")

## and the two go together: the same keys
ok(all(dl$year %in% cc$year) && all(dl$workingGroup %in% cc$workingGroup) && all(dl$stock %in% cc$stock) &&
     all(paste(dl$catchCategory) %in% cc$catchCategory),
   "CC and DC: every category of the DC is in the CC")

## ---- the real thing: an allocation of the fixture --------------------------------------------------------
fx <- fx_allocation()
al <- fx$len
lens <- distributionsCombined(al$distribution, al$catch, originType_catch = "WGEstimate", variableType_catch = "WeightLive",
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
raised_cc <- catchCombined(fx$raised, originType_catch = "WGEstimate", variableType_catch = "WeightLive")
ok(near(sort(raised_cc$total), sort(tapply(fx$raised$total, fx$raised$catchCategory, sum, na.rm = TRUE))),
   "CC: the raised catch of the engine added up by category")
ages <- distributionsCombined(fx$age$distribution, fx$age$catch, originType_catch = "WGEstimate", variableType_catch = "WeightLive",
                              distributionType = "Age", maxAoL = 2)
ok(nrow(ages) > 0 && all(ages$distributionUnit == "y") && all(ages$ageGroupPlus == 2L) && max(ages$distributionClass) == 2L,
   "DC: the allocation of ages, with a plus group")

## ---- the raised catch the allocation was run on, instead of the allocation's own catch -------------------------
## The strata the allocation estimated a distribution for have no domainBiology in the raised catch, and the allocation
## gives them (and the distributions) one it makes up: "[alloc]_<season>_<area>_<unit>_<fleet>_<category>". The raised
## catch is given the same, so the estimated distributions are found there too - and not for the catch of the result.
lens_raised <- said(heard(distributionsCombined(al$distribution, fx$raised, originType_catch = "WGEstimate",
                                                variableType_catch = "WeightLive", distributionType = "LengthTotal",
                                                unit = c("NE3", "g"))))
ok(!length(lens_raised$value$warnings) &&
     isTRUE(all.equal(as.data.frame(lens_raised$value$value), as.data.frame(lens))),
   "DC: the raised catch the allocation was run on gives the DC of the allocation's own catch, with no warning")
ok(length(lens_raised$messages) == 1 && has(lens_raised$messages, "[alloc]") &&
     !length(said(distributionsCombined(al$distribution, al$catch, originType_catch = "WGEstimate",
                                        variableType_catch = "WeightLive", distributionType = "LengthTotal"))$messages),
   "DC: ... said once, and never for the catch of the allocation result")
made <- suppressMessages(alloc_domain(fx$raised, al$distribution))
est <- unique(al$distribution$domainBiology[startsWith(al$distribution$domainBiology, "[alloc]")])
has_dom <- !is.na(fx$raised$domainBiology)
ok(length(est) > 5 && all(est %in% made$domainBiology) && identical(made$domainBiology[has_dom], fx$raised$domainBiology[has_dom]),
   "alloc_domain(): every domain the allocation made up is made from the raised catch, and a domain the catch has is kept")
wide_args <- list(grouping = "catchCategory", originType_catch = "WGEstimate", variableType_catch = "WeightLive",
                  distributionType = "LengthTotal")
wn_a <- do.call(catchAtAoLAggregation, c(list(al$distribution, al$catch), wide_args, unit = "NE3"))
wn_r <- suppressMessages(do.call(catchAtAoLAggregation, c(list(al$distribution, fx$raised), wide_args, unit = "NE3")))
ww_a <- do.call(meanWoLatAoLAggregation, c(list(al$distribution, al$catch), wide_args, unit = "g", plusGroup = FALSE))
ww_r <- suppressMessages(do.call(meanWoLatAoLAggregation, c(list(al$distribution, fx$raised), wide_args, unit = "g", plusGroup = FALSE)))
ok(isTRUE(all.equal(wn_a, wn_r)) && isTRUE(all.equal(ww_a, ww_r)) && sum(wn_r[startsWith(names(wn_r), "N_")]) > 0,
   "catchAtAoLAggregation() and meanWoLatAoLAggregation(): the raised catch gives what the allocation's own catch does")
ok(identical(catchCombined(fx$raised, originType_catch = "WGEstimate"),
             catchCombined(transform(fx$raised, domainBiology = NA_character_), originType_catch = "WGEstimate")),
   "CC: does not use domainBiology - the catch is added up whatever domain the rows have")

## a case worked out by hand: a stratum with no domain, and the distribution the allocation made up for it
catch_a <- rbind(catch, data.frame(vesselFlagCountry = "BE", year = 2023L, workingGroup = "WGX", stock = "stk.27.1",
                                   speciesCode = 100L, catchCategory = "Lan", domainBiology = NA_character_,
                                   originType = "WGEstimate", variableType = "WeightLive", variableUnit = "kg", total = 2000,
                                   seasonType = "Quarter", seasonValue = 3L, areaType = "ICESArea", areaValue = "27.4.b",
                                   fleetValue = "OTB", stringsAsFactors = FALSE))
catch_a$fisheriesManagementUnit <- NA_character_
dist_a <- rbind(distributions, dist("[alloc]_3_27.4.b_NA_OTB_Lan", "LengthTotal", c(100L, 110L), c(2000, 500), c(10, 40)))
by_hand <- function(catch_x) said(heard(distributionsCombined(dist_a, catch_x, originType_catch = "WGEstimate",
                                                              variableType_catch = "WeightLive", distributionType = "LengthTotal")))
a1 <- by_hand(catch_a)
a1d <- a1$value$value
ok(!length(a1$value$warnings) && length(a1$messages) == 1 &&
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
ok(has(a3$value$warnings, "fisheriesManagementUnit") && has(a3$value$warnings, "have no catch row"),
   "DC by hand: a catch that lacks what the domain is made from is said, and the distributions left out")
ok(!length(said(combine(distributionType = "LengthTotal"))$messages),
   "DC: no message when the distributions have no domain made up by an allocation")

## ---- the architecture: one adding up, three ways to read it ---------------------------------------------------
ok(all(c("catchAtAoL", "spread_AoL") %in% all.names(body(catchAtAoLAggregation))) &&
     all(c("meanWoLatAoL", "spread_AoL") %in% all.names(body(meanWoLatAoLAggregation))) &&
     all(c("catchAtAoL", "meanWoLatAoL", "scf_table") %in% all.names(body(distributionsCombined))),
   "catchAtAoLAggregation(), meanWoLatAoLAggregation() and distributionsCombined() are made of the same two functions")
ok(all(vapply(list(catchAtAoL, meanWoLatAoL, distributionsCombined), function(f) "alloc_domain" %in% all.names(body(f)), NA)),
   "... and the two of them and the DC read the catch of an allocation and the raised catch the same way (alloc_domain())")

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
