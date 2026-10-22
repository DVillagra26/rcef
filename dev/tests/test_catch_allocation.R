## The allocation setup of the stock coordination format: catchAllocation() (CAR and CAS).
##
## What is pinned: the two tables have the fields of the format, in its order and of its types; a case small enough to do on
## paper (the worked example of the format: six strata of a sole, two groups) gives the rates, the weights and the raised
## discards the example has; every raised discard names the group it was raised for and every donor the groups it feeds; the
## donors of a group give the ratio its raised discards were made with; the CAR adds up to the CC; the header is the CC's; the
## country is the ISO code; and what cannot be made - overlapping groups, groups that are not the raising's, a header
## without a year - is said.

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
err <- function(expr) tryCatch({ suppressWarnings(expr); "" }, error = function(e) conditionMessage(e))
near <- function(a, b, tol = 1e-9) length(a) == length(b) && isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = tol))
has <- function(txt, pattern) any(grepl(pattern, txt, fixed = TRUE))
quiet <- function(expr) { out <- NULL; invisible(utils::capture.output(out <- suppressWarnings(suppressMessages(expr)))); out }
suppressPackageStartupMessages(library(dplyr))
q <- rlang::quo

car_fields <- c("recordType", "workingGroupYear", "version", "versionDescription", "catchAllocationSchemeName",
                "catchAllocationGroupNameTarget", "catchAllocationGroupNameSource", "imported", "vesselFlagCountry",
                "calendarYear", "startDateYear", "year", "workingGroup", "stock", "speciesCode", "catchCategory", "seasonType",
                "seasonValue", "areaType", "areaValue", "fisheriesManagementUnit", "fleetType", "fleetValue", "variableType",
                "variableUnit", "total", "variance", "PSU", "numPSUs", "numTrips", "comment")
cas_fields <- c("recordType", "workingGroupYear", "version", "versionDescription", "catchAllocationSchemeName",
                "catchAllocationGroupName", "vesselFlagCountry", "calendarYear", "startDateYear", "year", "workingGroup", "stock",
                "speciesCode", "catchCategory", "seasonType", "seasonValue", "areaType", "areaValue", "fisheriesManagementUnit",
                "fleetType", "fleetValue", "rate", "averagingMethod", "weightingCategory", "weightingParameter",
                "weightingTotal", "comment")
ctype <- function(d) vapply(d, function(v) class(v)[1], "")

## ---- The worked example of the format: a sole, six strata, two groups -----------------------------------------
## G1 (GB-ENG and BE) takes its ratio from the discard estimates of GB-ENG in the first quarter and of BE in the year;
## G2 (IR) from the estimate of IR in the year. Three landings have no estimate and are raised.
cef <- function(country, season_type, season, area, landings, discards, domain)
  rbind(data.frame(recordType = "CN", vesselFlagCountry = country, year = 2024L, workingGroup = "WGCSE", stock = "sol.27.7fg",
                   speciesCode = 127160L, catchCategory = "Lan", seasonType = season_type, seasonValue = season,
                   areaType = "ICESArea", areaValue = area, fisheriesManagementUnit = NA_character_, metier6 = "TBB_DEF_70-99_0_0",
                   fleetType = "Fleet", fleetValue = "TBB_DEF_70-99_0_0_all", domainCatchDis = domain, domainCatchBMS = NA_character_,
                   domainBiology = NA_character_, originType = "WGEstimate", variableType = "WeightLive", variableUnit = "kg",
                   total = landings, variance = NA_real_, PSU = NA_character_, numPSUs = NA_integer_, numTrips = NA_integer_,
                   comment = NA_character_, stringsAsFactors = FALSE),
        if (!is.na(discards))
          data.frame(recordType = "CN", vesselFlagCountry = country, year = 2024L, workingGroup = "WGCSE", stock = "sol.27.7fg",
                     speciesCode = 127160L, catchCategory = "Dis", seasonType = season_type, seasonValue = season,
                     areaType = "ICESArea", areaValue = area, fisheriesManagementUnit = NA_character_, metier6 = "TBB_DEF_70-99_0_0",
                     fleetType = "Fleet", fleetValue = "TBB_DEF_70-99_0_0_all", domainCatchDis = domain, domainCatchBMS = NA_character_,
                     domainBiology = NA_character_, originType = "WGEstimate", variableType = "WeightLive", variableUnit = "kg",
                     total = discards, variance = NA_real_, PSU = "FishingTrip", numPSUs = 2L, numTrips = 2L,
                     comment = NA_character_, stringsAsFactors = FALSE))
sole <- rbind(cef("GB-ENG", "Quarter", 1L, "27.7.f", 249986.1, 5998.754, "d1"),
              cef("BE", "Year", 2024L, "27.7.f;27.7.g", 672669, 38354, "d2"),
              cef("IR", "Year", 2024L, "27.7.f", 210300, 12569, "d3"),
              cef("IR", "Quarter", 1L, "27.7.g", 5236, NA, NA),
              cef("GB-ENG", "Quarter", 1L, "27.7.g", 10888.5, NA, NA),
              cef("GB-ENG", "Quarter", 2L, "27.7.f", 37849.6, NA, NA),
              cef("GB-ENG", "Quarter", 2L, "27.7.g", 4302.5, NA, NA))
groups <- list(strataCond = list(G1 = q(vesselFlagCountry %in% c("GB-ENG", "BE")), G2 = q(vesselFlagCountry == "IR")))
sole_raised <- as.data.frame(quiet(catchRaising(sole, groups$strataCond, groups$strataCond, type = "discards",
                                                 originType = "WGEstimate", variableType = "WeightLive",
                                                 verbose = FALSE, logFile = tempfile(fileext = ".txt"))))
scheme <- "2025_stock_annex"
x <- catchAllocation(sole_raised, groups$strataCond, workingGroupYear = 2025, version = 1, versionDescription = "stock_annex",
                     catchAllocationSchemeName = scheme)
CAR <- x$CAR; CAS <- x$CAS

ok(is.list(x) && identical(names(x), c("CAR", "CAS")), "the result is a list of the two tables")
ok(identical(names(CAR), car_fields) && identical(names(CAS), cas_fields),
   "the fields of the CAR and of the CAS are those of the format, in its order")
ok(all(CAR$recordType == "CAR") && all(CAS$recordType == "CAS") &&
     all(c(CAR$workingGroupYear, CAS$workingGroupYear) == 2025L) && all(c(CAR$version, CAS$version) == 1L) &&
     all(c(CAR$versionDescription, CAS$versionDescription) == "stock_annex") &&
     all(c(CAR$calendarYear, CAS$calendarYear) == "Y") && all(is.na(c(CAR$startDateYear, CAS$startDateYear))),
   "the header is on every row of both")
ok(nrow(CAR) == 14L && sum(CAR$catchCategory == "Lan") == 7L && sum(CAR$catchCategory == "Dis" & CAR$imported == "Y") == 3L &&
     sum(CAR$imported == "N") == 4L,
   "the CAR has the seven landings, the three discards that were reported and the four the raising made")
ok(identical(CAR$imported == "N", CAR$catchCategory == "Dis" & !is.na(CAR$catchAllocationGroupNameTarget)) &&
     all(is.na(CAR$catchAllocationGroupNameTarget[CAR$imported == "Y"])),
   "imported is N for the discards the raising made and those only, which are the ones with a target group")

# the CAS of the example
key <- function(d) paste(d$vesselFlagCountry, d$seasonType, d$seasonValue, d$areaValue)
cas1 <- CAS[CAS$catchAllocationGroupName == "G1", ]
ok(nrow(CAS) == 3L && setequal(key(cas1), c("GB-ENG Quarter 1 27.7.f", "BE Year 2024 27.7.f;27.7.g")) &&
     identical(key(CAS[CAS$catchAllocationGroupName == "G2", ]), "IR Year 2024 27.7.f"),
   "the CAS has the three donors of the example, each under its group")
r <- setNames(CAS$rate, key(CAS)); w <- setNames(CAS$weightingTotal, key(CAS))
ok(near(r[["GB-ENG Quarter 1 27.7.f"]], 2.3996350197070956e-2) && near(r[["BE Year 2024 27.7.f;27.7.g"]], 5.7017641663284616e-2) &&
     near(r[["IR Year 2024 27.7.f"]], 5.9766999524488829e-2),
   "the rate of a donor is its discards over its landings (the rates of the example)")
ok(near(w[["GB-ENG Quarter 1 27.7.f"]], 249986.1) && near(w[["BE Year 2024 27.7.f;27.7.g"]], 672669) &&
     near(w[["IR Year 2024 27.7.f"]], 210300),
   "the weighting total is the landings of the donor (the weights of the example)")
ok(all(CAS$averagingMethod == "Weighted") && all(CAS$weightingCategory == "Lan") && all(CAS$weightingParameter == "WeightLive") &&
     all(CAS$catchAllocationSchemeName == scheme) && all(CAS$catchCategory == "Dis"),
   "the averaging is weighted by the landings (the way catchRaising() does it), the scheme and the category are written")

# the raised discards of the example: the landings times the ratio of the group
raised_row <- function(country, season, area) CAR$total[CAR$imported == "N" & CAR$vesselFlagCountry == country &
                                                         CAR$seasonValue == season & CAR$areaValue == area]
g1 <- (5998.754 + 38354) / (249986.1 + 672669)
ok(near(raised_row("GB-ENG", 1L, "27.7.g"), 523.4187314, 1e-8) && near(raised_row("GB-ENG", 2L, "27.7.f"), 1819.45994532345, 1e-8) &&
     near(raised_row("GB-ENG", 2L, "27.7.g"), 206.824548073272, 1e-8) && near(raised_row("IR", 1L, "27.7.g"), 5236 * 12569 / 210300) &&
     near(sum(cas1$rate * cas1$weightingTotal) / sum(cas1$weightingTotal), g1),
   "the raised discards are the landings times the ratio of the donors (the 523.4187 ... of the example)")

# who is raised for which group, who feeds it
dis <- CAR[CAR$catchCategory == "Dis", ]
tg <- setNames(dis$catchAllocationGroupNameTarget, key(dis)); sr <- setNames(dis$catchAllocationGroupNameSource, key(dis))
ok(identical(unname(tg[c("IR Quarter 1 27.7.g", "GB-ENG Quarter 1 27.7.g", "GB-ENG Quarter 2 27.7.f", "GB-ENG Quarter 2 27.7.g")]),
             c("G2", "G1", "G1", "G1")) && sum(!is.na(dis$catchAllocationGroupNameTarget)) == 4L,
   "a raised discard names the group it was raised for (the target) and no other discard does")
ok(identical(unname(sr[c("GB-ENG Quarter 1 27.7.f", "BE Year 2024 27.7.f;27.7.g", "IR Year 2024 27.7.f")]), c("G1", "G1", "G2")) &&
     sum(!is.na(dis$catchAllocationGroupNameSource)) == 3L,
   "a donor discard names the group it feeds (the source) and no other discard does")
lan <- CAR[CAR$catchCategory == "Lan", ]
ok(setequal(key(lan)[!is.na(lan$catchAllocationGroupNameSource)], c("GB-ENG Quarter 1 27.7.f", "BE Year 2024 27.7.f;27.7.g", "IR Year 2024 27.7.f")) &&
     all(is.na(lan$catchAllocationGroupNameTarget)),
   "the landings of a donor feed its group too; the others, and all landings as targets, have no group")
ok(all(is.na(CAR$catchAllocationSchemeName[CAR$imported == "Y"])) && all(CAR$catchAllocationSchemeName[CAR$imported == "N"] == scheme),
   "the scheme is written on the raised discards only")
ok(all(is.na(CAR$PSU[CAR$imported == "N"])) && all(is.na(CAR$numTrips[CAR$imported == "N"])) &&
     all(CAR$PSU[CAR$imported == "Y" & CAR$catchCategory == "Dis"] == "FishingTrip") &&
     all(CAR$numTrips[CAR$imported == "Y" & CAR$catchCategory == "Dis"] == 2L),
   "a raised discard has no sample of its own; the discards that were reported keep theirs")
types <- ctype(CAR)
ok(types[["workingGroupYear"]] == "integer" && types[["speciesCode"]] == "integer" && types[["seasonValue"]] == "integer" &&
     types[["total"]] == "numeric" && types[["numTrips"]] == "integer" && types[["catchAllocationGroupNameSource"]] == "character" &&
     ctype(CAS)[["rate"]] == "numeric" && ctype(CAS)[["weightingTotal"]] == "numeric",
   "the fields are of the types of the format")

# the CAR adds up to the CC, the CAS to the ratio of the raising
cc <- catchCombined(sole_raised, originType_catch = "WGEstimate", variableType_catch = "WeightLive", workingGroupYear = 2025)
ok(near(CAR %>% group_by(catchCategory) %>% summarise(t = sum(total)) %>% arrange(catchCategory) %>% pull(t),
        cc %>% arrange(catchCategory) %>% pull(total)),
   "the CAR adds up to the CC")
ok(length(heard(catchAllocation(sole_raised, groups$strataCond, workingGroupYear = 2025, versionDescription = "stock_annex"))$warnings) == 0,
   "the donors give the ratio the discards were raised with: no warning")

## the format asks for the sampling of the imported discards: a stratum without it is counted
unsampled <- sole_raised; unsampled$numTrips <- NA_integer_
ok(has(heard(catchAllocation(unsampled, groups$strataCond, workingGroupYear = 2025, versionDescription = "x"))$warnings,
       "3 of the 3 imported discard (and BMS) strata have no PSU, numPSUs or numTrips"),
   "imported discards without their sampling are warned about, counted")

## ---- The unit, the rounding, the scheme -----------------------------------------------------------------------
xt <- catchAllocation(sole_raised, groups$strataCond, workingGroupYear = 2025, versionDescription = "stock_annex", unit = "t")
ok(all(xt$CAR$variableUnit == "t") && near(sum(xt$CAR$total) * 1000, sum(CAR$total)) && near(xt$CAS$weightingTotal * 1000, CAS$weightingTotal) &&
     near(xt$CAS$rate, CAS$rate),
   "unit = 't': the weights are in tonnes, the rates do not change")
xr <- catchAllocation(sole_raised, groups$strataCond, workingGroupYear = 2025, versionDescription = "stock_annex", round = 1)
ok(near(xr$CAR$total, round(CAR$total, 1)) && near(xr$CAS$weightingTotal, round(CAS$weightingTotal, 1)) && near(xr$CAS$rate, CAS$rate),
   "round rounds the weights and not the rates")
ok(all(xt$CAR$catchAllocationSchemeName[xt$CAR$imported == "N"] == "stock_annex") && all(xt$CAS$catchAllocationSchemeName == "stock_annex"),
   "the scheme name is the version description when it is not given")
ok(has(err(catchAllocation(sole_raised, groups$strataCond, workingGroupYear = 2025)), "catchAllocationSchemeName"),
   "no scheme name and no version description: said")

## ---- The header is the CC's, and is checked first --------------------------------------------------------------
ok(has(err(catchAllocation(sole_raised, groups$strataCond, versionDescription = "x")), "workingGroupYear"),
   "a call without workingGroupYear is refused (the header is checked before anything else)")
xn <- catchAllocation(sole_raised, groups$strataCond, workingGroupYear = 2025, versionDescription = "model year",
                      calendarYear = "N", startDateYear = "07-01")
ok(all(xn$CAR$calendarYear == "N") && all(xn$CAR$startDateYear == "07-01") && all(xn$CAS$startDateYear == "07-01"),
   "a model year is written on both tables")

## ---- The country is the ISO code -------------------------------------------------------------------------------
names_catch <- sole_raised
names_catch$vesselFlagCountry <- c("GB-ENG" = "UK (England)", "BE" = "Belgium", "IR" = "Ireland")[names_catch$vesselFlagCountry]
names_groups <- list(G1 = q(vesselFlagCountry %in% c("UK (England)", "Belgium")), G2 = q(vesselFlagCountry == "Ireland"))
xi <- heard(catchAllocation(names_catch, names_groups, workingGroupYear = 2025, versionDescription = "x"))
ok(setequal(unique(xi$value$CAR$vesselFlagCountry), c("GB-ENG", "BE", "IE")) && setequal(unique(xi$value$CAS$vesselFlagCountry), c("GB-ENG", "BE", "IE")) &&
     length(xi$warnings) == 0,
   "the names the converted InterCatch files give the countries are written as the ISO codes")
names_catch$vesselFlagCountry[names_catch$vesselFlagCountry == "Ireland"] <- "Atlantis"
xa <- heard(catchAllocation(names_catch, list(G1 = q(vesselFlagCountry %in% c("UK (England)", "Belgium")), G2 = q(vesselFlagCountry == "Atlantis")),
                            workingGroupYear = 2025, versionDescription = "x"))
ok(has(xa$warnings, "vesselFlagCountry 'Atlantis' is not an ISO 3166 code") && "Atlantis" %in% xa$value$CAR$vesselFlagCountry,
   "a country with no code is warned about and kept")

## ---- Donors that feed several groups, the matched data wider than the strata -----------------------------------------
# G1 takes its ratio from everything that has an estimate: every donor feeds it, and G2 still has its own
wide <- list(G1 = q(TRUE), G2 = q(vesselFlagCountry == "IR"))
sole_wide <- as.data.frame(quiet(catchRaising(sole, groups$strataCond, wide, type = "discards", verbose = FALSE,
                                              logFile = tempfile(fileext = ".txt"))))
xw <- catchAllocation(sole_wide, groups$strataCond, wide, workingGroupYear = 2025, versionDescription = "x")
srcw <- setNames(xw$CAR$catchAllocationGroupNameSource, paste(xw$CAR$vesselFlagCountry, xw$CAR$catchCategory, xw$CAR$seasonType))
ok(identical(unname(srcw[["IR Dis Year"]]), "G1; G2") && identical(unname(srcw[["BE Dis Year"]]), "G1") &&
     identical(unname(srcw[["IR Lan Year"]]), "G1; G2"),
   "a donor that feeds two groups has both, in the order of the groups, separated by '; '")
cw <- xw$CAS[xw$CAS$catchAllocationGroupName == "G1", ]
ok(nrow(cw) == 3L && near(sum(cw$rate * cw$weightingTotal) / sum(cw$weightingTotal), (5998.754 + 38354 + 12569) / (249986.1 + 672669 + 210300)),
   "the CAS of the group with the wide matched data has all three donors, whose ratio is the group's")
ok(nrow(xw$CAS) == 4L, "the CAS has a row for every donor of every group")

## ---- What the donors cannot give, what the groups cannot be ---------------------------------------------------------
# the conditions of another raising: the ratio of the donors is not the one the discards were made with
xm <- heard(catchAllocation(sole_wide, groups$strataCond, groups$strataCond, workingGroupYear = 2025, versionDescription = "x"))
ok(has(xm$warnings, "the donors of group 'G1' give a discard ratio of"), "donors that do not give the ratio of the raising are warned about")
# the groups of the raising are not those given
swapped <- list(G1 = groups$strataCond$G2, G2 = groups$strataCond$G1)
ok(has(heard(catchAllocation(sole_raised, swapped, workingGroupYear = 2025, versionDescription = "x"))$warnings, "DrGroup of the raising"),
   "groups that are not the raising's (its DrGroup) are warned about")
ok(has(err(catchAllocation(sole_raised, list(G1 = q(vesselFlagCountry == "BE")), workingGroupYear = 2025, versionDescription = "x")),
       "fall in none of the strata"), "raised discards in no group of the conditions: said")
overlap <- list(G1 = q(TRUE), G2 = q(vesselFlagCountry == "IR"))
ok(has(err(catchAllocation(sole_raised, overlap, workingGroupYear = 2025, versionDescription = "x")), "more than one group"),
   "strata that overlap are refused, as catchRaising() does")
ok(has(err(catchAllocation(sole_raised, groups$strataCond, list(G1 = q(TRUE)), workingGroupYear = 2025, versionDescription = "x")), "as long"),
   "conditions of a different number of groups are refused")
ok(has(err(catchAllocation(sole_raised, list(G1 = rep(TRUE, nrow(sole_raised))), workingGroupYear = 2025, versionDescription = "x")), "quosures"),
   "a logical vector, which refers to the rows before the raising, is refused")
ok(has(err(catchAllocation(sole_raised, groups$strataCond, workingGroupYear = 2025, versionDescription = "x", originType = "Official")), "no catch rows"),
   "an origin the catch has no rows of: said")
ok(has(err(catchAllocation(sole[names(sole) != "importedOrRaised"], groups$strataCond, workingGroupYear = 2025, versionDescription = "x")),
       "importedOrRaised"), "a catch that was not raised: said")
long_names <- setNames(groups$strataCond, c("a_group_whose_name_is_far_too_long_for_the_format", "G2"))
sole_long <- as.data.frame(quiet(catchRaising(sole, long_names, long_names, type = "discards", verbose = FALSE, logFile = tempfile(fileext = ".txt"))))
ok(has(heard(catchAllocation(sole_long, long_names, workingGroupYear = 2025, versionDescription = "x"))$warnings, "longer than the 25 characters"),
   "a group name longer than the 25 characters of the format is warned about")
# a group with no donor: its discards are NaN, they are not a result and the group is not in the tables
nodonor <- sole[!(sole$vesselFlagCountry == "IR" & sole$catchCategory == "Dis"), ]
raised_nd <- as.data.frame(quiet(catchRaising(nodonor, groups$strataCond, groups$strataCond, type = "discards", verbose = FALSE,
                                              logFile = tempfile(fileext = ".txt"))))
xd <- heard(catchAllocation(raised_nd, groups$strataCond, workingGroupYear = 2025, versionDescription = "x"))
ok(any(!is.finite(raised_nd$total)) && has(xd$warnings, "have no total and are not reported") && all(is.finite(xd$value$CAR$total)) &&
     !"G2" %in% xd$value$CAS$catchAllocationGroupName && !"G2" %in% xd$value$CAR$catchAllocationGroupNameTarget,
   "a group with no donor has NaN discards: they are left out, with a warning, and the group is not in the tables")

## ---- The fixture: three ways to raise the same catch ------------------------------------------------------------
cy <- fx_catch(); cy <- cy[cy$year == 2023, ]
for (scheme_name in c("own", "mixed", "all")) {
  gr <- fx_groups(scheme_name)
  rs <- as.data.frame(quiet(fx_raise(cy, gr, type = "discards", log_file = tempfile(fileext = ".txt"))))
  fx <- heard(catchAllocation(rs, gr$strataCond, gr$matchedDataCond, workingGroupYear = 2024, versionDescription = "fixture"))
  tabs <- fx$value
  A <- tabs$CAR; B <- tabs$CAS
  ccf <- catchCombined(rs, originType_catch = "WGEstimate", variableType_catch = "WeightLive", workingGroupYear = 2024)
  ok(near(A %>% group_by(catchCategory) %>% summarise(t = sum(total)) %>% arrange(catchCategory) %>% pull(t),
          ccf %>% arrange(catchCategory) %>% pull(total)),
     paste0("fixture, '", scheme_name, "' groups: the CAR adds up to the CC, catch category by catch category"))
  tg <- unique(A$catchAllocationGroupNameTarget[!is.na(A$catchAllocationGroupNameTarget)])
  sources <- unique(unlist(strsplit(A$catchAllocationGroupNameSource[!is.na(A$catchAllocationGroupNameSource)], "; ", fixed = TRUE)))
  ok(setequal(tg, names(gr$strataCond)) && all(sources %in% names(gr$strataCond)) && setequal(unique(B$catchAllocationGroupName), tg),
     paste0("fixture, '", scheme_name, "' groups: targets, sources and the groups of the CAS are the groups that raised something"))
  ok(identical(names(A), car_fields) && identical(names(B), cas_fields) && !anyDuplicated(A[c("vesselFlagCountry", "year", "seasonType",
       "seasonValue", "areaValue", "fleetValue", "catchCategory", "variableType")]) &&
       !anyDuplicated(B[c("catchAllocationGroupName", "vesselFlagCountry", "year", "seasonType", "seasonValue", "areaValue", "fleetValue")]),
     paste0("fixture, '", scheme_name, "' groups: the fields of the format, no stratum twice"))
  key_car <- paste(A$vesselFlagCountry, A$seasonValue, A$areaValue, A$fleetValue, A$catchCategory)
  link <- mapply(function(g, k) { s <- A$catchAllocationGroupNameSource[key_car == k]
                                 length(s) == 1L && !is.na(s) && g %in% strsplit(s, "; ", fixed = TRUE)[[1]] },
                 B$catchAllocationGroupName, paste(B$vesselFlagCountry, B$seasonValue, B$areaValue, B$fleetValue, B$catchCategory))
  ok(all(link), paste0("fixture, '", scheme_name, "' groups: every row of the CAS is a donor of the CAR that names the group in its source"))
  place <- c("vesselFlagCountry", "seasonType", "seasonValue", "areaValue", "fleetValue")
  used <- rs %>% filter(importedOrRaised == "raised", is.finite(total), catchCategory == "Dis") %>%
    left_join(rs %>% filter(catchCategory == "Lan", importedOrRaised == "imported") %>% select(all_of(place), lan = total) %>%
                group_by(across(all_of(place))) %>% summarise(lan = sum(lan), .groups = "drop"),
              by = place) %>%
    group_by(DrGroup) %>% summarise(ratio = sum(total) / sum(lan), .groups = "drop") %>% arrange(DrGroup)
  given <- B %>% group_by(catchAllocationGroupName) %>%
    summarise(ratio = sum(rate * weightingTotal) / sum(weightingTotal), .groups = "drop") %>% arrange(catchAllocationGroupName)
  ok(identical(used$DrGroup, given$catchAllocationGroupName) && near(used$ratio, given$ratio, 1e-7) &&
       !any(grepl("give a discard ratio", fx$warnings)),
     paste0("fixture, '", scheme_name, "' groups: the donors of each group give the ratio its discards were raised with"))
}

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
