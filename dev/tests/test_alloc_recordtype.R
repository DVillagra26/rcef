## The distributions catchAllocationAoL() estimates are distribution rows, not copies of the catch rows they are made from.
##
## What is pinned: every row of the result is a "DN" record, for the numbers and the mean weights, for length and age; an
## estimated row carries no variance (the catch's does not come with it) and nothing else of the catch's sample (trips,
## PSUs); the sampled rows keep what they had; and the estimated values are the ones worked out by hand - the numbers are
## the pooled sampled numbers scaled by the stratum's catch over the sampled catch, the mean weights the means of the
## sampled ones weighted by their numbers. Tables without these columns are allocated as before.

source("dev/tests/fixture.R")
FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}
quiet <- function(expr) { out <- NULL; invisible(utils::capture.output(out <- suppressWarnings(suppressMessages(expr)))); out }
near <- function(a, b) length(a) == length(b) && isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = 1e-9))
q <- rlang::quo

## ---- a small case: two strata sampled (D1, D2), two not (one without a domain, one with a domain nobody sampled) -------
## The catch has a variance and trips of its own, so that anything of it found in a distribution shows.
cn <- function(season, total, domain)
  data.frame(recordType = "CN", vesselFlagCountry = "AA", year = 2025L, workingGroup = "WGX", stock = "stk.27",
             speciesCode = "SPP", catchCategory = "Lan", seasonType = "Quarter", seasonValue = season, areaType = "ICESArea",
             areaValue = "27.4.a", fisheriesManagementUnit = NA_character_, fleetType = "Fleet", fleetValue = "TBB_x",
             domainCatchDis = NA_character_, domainCatchBMS = NA_character_, domainBiology = domain, originType = "WGEstimate",
             variableType = "WeightLive", variableUnit = "kg", total = total, variance = 12345, PSU = "FishingTrip",
             numPSUs = 7L, numTrips = 7L, comment = NA_character_, stringsAsFactors = FALSE)
catch <- rbind(cn(1L, 100, "D1"), cn(2L, 300, "D2"), cn(3L, 50, NA_character_), cn(4L, 80, "D4"))
dn <- function(domain, type, unit, classes, numbers, weights)
  rbind(data.frame(variableType = "Number", variableUnit = "N", valueType = "Total", value = numbers, distributionClass = classes),
        data.frame(variableType = "WeightLive", variableUnit = "g", valueType = "Mean", value = weights, distributionClass = classes)) |>
    transform(recordType = "DN", vesselFlagCountry = "AA", year = 2025L, workingGroup = "WGX", stock = "stk.27", speciesCode = "SPP",
              catchCategory = "Lan", domainBiology = domain, distributionType = type, distributionUnit = unit, ageGroupPlus = NA,
              attributeType = NA_character_, attributeValue = NA_character_, variance = 0.5, PSU = "FishingTrip", numPSUs = 3L,
              numTrips = 3L, numMeasurements = 40L)
dist <- rbind(dn("D1", "LengthTotal", "mm", c(200, 210), c(1000, 500), c(80, 100)),
              dn("D2", "LengthTotal", "mm", c(200, 210), c(3000, 1500), c(90, 110)),
              dn("D1", "Age", "year", c(1, 2), c(600, 900), c(50, 70)),
              dn("D2", "Age", "year", c(1, 2), c(2400, 600), c(60, 90)))
allocate <- function(catch, dist)
  quiet(catchAllocationAoL(catch_data = catch, distribution_data = dist, condition_alloc_st_list = list(ALL = q(TRUE)),
                           distributionType = c("LengthTotal", "Age"), originType_catch = "WGEstimate",
                           variableType_catch = "WeightLive", verbose = FALSE, logFile = tempfile(fileext = ".txt")))
res <- allocate(catch, dist)
d <- as.data.frame(res$distribution)
est <- d[d$sampledOrEstimated == "estimated", ]
smp <- d[d$sampledOrEstimated == "sampled", ]
ok(nrow(smp) == 16 && nrow(est) == 16 && setequal(est$domainBiology, c("D4", "[alloc]_3_27.4.a_NA_TBB_x_Lan")) &&
     nrow(unique(est[c("domainBiology", "distributionType", "variableType", "distributionClass")])) == 16,
   "the case: two sampled strata, two estimated ones - one that had no domain, one whose domain nobody sampled - numbers and mean weights, length and age")

## ---- the record type -------------------------------------------------------------------------------------------------
ok(all(d$recordType == "DN"), "every row of the distributions is a DN record")
ok(all(table(est$distributionType, est$variableType) > 0) && all(est$recordType == "DN"),
   "... the estimated numbers and mean weights, of length and of age")
ok(all(res$catch$recordType == "CN"), "... and the catch stays CN")

## ---- nothing of the catch row's sample comes with an estimated distribution --------------------------------------------
ok(all(is.na(est$variance)) && all(smp$variance == 0.5),
   "an estimated row has no variance (the catch's is not its own); a sampled row keeps its variance")
ok(all(is.na(est$numTrips)) && all(is.na(est$numPSUs)) && all(is.na(est$numMeasurements)) &&
     all(smp$numTrips == 3) && !any(vapply(est, function(v) any(v %in% 12345), NA)),
   "... nor the catch's trips or PSUs, nor anything else with the catch's values")
ok(identical(res$catch$domainBiology, c("D1", "D2", "[alloc]_3_27.4.a_NA_TBB_x_Lan", "D4")) &&
     all(est$vesselFlagCountry == "AA") && all(est$catchCategory == "Lan"),
   "an estimated row belongs to its stratum: the country, catch category and domain of the catch row it was made for")

## ---- the estimated values, worked out by hand ------------------------------------------------------------------------
v <- function(domain, type, var, classes)
  vapply(classes, function(k) {
    x <- est$value[est$domainBiology == domain & est$distributionType == type & est$variableType == var & est$distributionClass == k]
    if (length(x) == 1) x else NA_real_
  }, 0)
d3 <- "[alloc]_3_27.4.a_NA_TBB_x_Lan"
## numbers: the sampled numbers (D1 + D2) times the stratum's catch over the sampled catch (400 kg)
ok(near(v(d3, "LengthTotal", "Number", c(200, 210)), c(4000, 2000) * 50 / 400) &&
     near(v("D4", "LengthTotal", "Number", c(200, 210)), c(4000, 2000) * 80 / 400),
   "numbers at length: the sampled numbers scaled to the stratum's catch")
ok(near(v(d3, "Age", "Number", c(1, 2)), c(3000, 1500) * 50 / 400) &&
     near(v("D4", "Age", "Number", c(1, 2)), c(3000, 1500) * 80 / 400),
   "numbers at age: the same")
## mean weights: the sampled means weighted by their numbers, the same for every stratum without a sample
ok(near(v(d3, "LengthTotal", "WeightLive", c(200, 210)), c(87.5, 107.5)) &&
     near(v("D4", "LengthTotal", "WeightLive", c(200, 210)), c(87.5, 107.5)) &&
     near(v(d3, "Age", "WeightLive", c(1, 2)), c(58, 78)) && near(v("D4", "Age", "WeightLive", c(1, 2)), c(58, 78)),
   "mean weights at length and at age: the sampled means weighted by their numbers")

## ---- tables that do not have these columns ------------------------------------------------------------------------------
res2 <- allocate(catch[setdiff(names(catch), "variance")], dist[setdiff(names(dist), "recordType")])
d2 <- as.data.frame(res2$distribution)
ok(!"recordType" %in% names(d2) && all(is.na(d2$variance[d2$sampledOrEstimated == "estimated"])) &&
     near(sort(d2$value), sort(d$value)),
   "a catch without a variance and distributions without a recordType are allocated as before, no column added")

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
