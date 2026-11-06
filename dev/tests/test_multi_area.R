## flag_multi_area() and split_multi_area(): a discard record that names several areas (or fleets) in one field, and the
## check that the groups of a raising keep a domain whole.
##
## The Belgian discards of TBB_DEF_70-99 in the RDBES download are ONE record for "27.4.b;27.4.c;27.7.d;27.7.e", with 14
## landings records of one area each in the same domain (domainCatchDis). Left as it is, the record is a stratum of its own with no
## landings while the landings show no discards; and groups that tell the areas apart cut the domain, so catchRaising() raises the
## landings again and counts the discards twice (7,563 kg imported + 6,822 kg raised on the same landings, on the real data).
##
## What is pinned: what the flag finds and says (the table, the weights of the codes, the records that cannot be split); the split
## (the amount shared out by the landings of the domain and adding up, cells without weight, no weight at all, the weight
## argument, `by`, the domains re-keyed on the parts AND on their landings, the derived fields, the sample counts, the
## distributions - totals shared, means kept, one for every part, derived again - and a distribution that others use); a second call;
## a tibble; and the guard: catchRaising() / check_group_conditions() stop when the groups cut a domain with an estimate and
## not when there is no estimate to lose, and after the split the raising counts the discards once.

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
err <- function(expr) tryCatch({ suppressMessages(expr); "" }, error = function(e) conditionMessage(e))
near <- function(a, b, tol = 1e-9) length(a) == length(b) && all(abs(a - b) < tol)

## ---- a catch, small enough to read ----------------------------------------------------------
mk <- function(cat, area, season, total, dom, dbio = NA_character_, fleet = "OTB_DEF_70-99_0_0_all", country = "XX", numTrips = NA_real_)
  data.frame(recordType = "CN", vesselFlagCountry = country, year = 2025L, workingGroup = "WGX", stock = "stk.27", speciesCode = "SPP",
             catchCategory = cat, seasonType = ifelse(season == "2025", "Year", "Quarter"), seasonValue = season, areaType = "ICESArea",
             areaValue = area, fisheriesManagementUnit = NA_character_, metier6 = NA_character_, fleetType = "Metier6", fleetValue = fleet,
             domainCatchDis = dom, domainCatchBMS = NA_character_, domainBiology = dbio, originType = "WGEstimate",
             variableType = "WeightLive", variableUnit = "t", total = total, variance = NA_real_, PSU = NA_character_,
             numPSUs = NA_real_, numTrips = numTrips, stringsAsFactors = FALSE)
catch <- rbind(
  ## D1 - 3 areas, landings by quarter, one annual discard record with a distribution and a sample of 12 trips
  mk("Lan", "27.4.a", "1", 20, "D1"), mk("Lan", "27.4.a", "2", 10, "D1"), mk("Lan", "27.4.b", "1", 30, "D1"),
  mk("Lan", "27.4.b", "2", 10, "D1"), mk("Lan", "27.4.c", "1", 20, "D1"),
  mk("Dis", "27.4.a;27.4.b;27.4.c", "2025", 9, "D1", "B1", numTrips = 12),
  ## D2 - a listed area (27.4.d) with no landings
  mk("Lan", "27.4.a", "1", 40, "D2"), mk("Dis", "27.4.a;27.4.d", "2025", 4, "D2"),
  ## D3 - landings, but none in the listed areas
  mk("Lan", "27.4.e", "1", 10, "D3"), mk("Dis", "27.4.a;27.4.b", "2025", 2, "D3"),
  ## D4 - a LANDINGS record that lists areas
  mk("Lan", "27.4.a;27.4.b", "1", 50, "D4"),
  ## D5 - a discard record with no domain
  mk("Dis", "27.4.a;27.4.b", "2025", 3, NA_character_),
  ## D6 - landings outside the list too
  mk("Lan", "27.4.a", "1", 30, "D6"), mk("Lan", "27.4.f", "1", 10, "D6"), mk("Dis", "27.4.a;27.4.b", "2025", 8, "D6"))
catch$effort <- ifelse(catch$catchCategory == "Lan", c(50, 50, 100, 0, 200, NA, 10, NA, 5, NA, 7, NA, 3, 1, NA), NA_real_)
cls <- c(100, 110, 120)
base <- data.frame(vesselFlagCountry = "XX", year = 2025L, workingGroup = "WGX", stock = "stk.27", speciesCode = "SPP", catchCategory = "Dis",
                   domainBiology = "B1", distributionType = "LengthTotal", numTrips = 4, numPSUs = 4, stringsAsFactors = FALSE)
dist <- rbind(
  cbind(base[rep(1, 3), ], distributionClass = cls, variableType = "Number", variableUnit = "NE3", valueType = "Total", value = c(1, 2, 1), numMeasurements = c(10, 20, 10)),
  cbind(base[rep(1, 3), ], distributionClass = cls, variableType = "WeightLive", variableUnit = "g", valueType = "Mean", value = cls * 0.5, numMeasurements = c(10, 20, 10)))
catch_raw <- catch
catch <- derive_fields(catch)
dist_d <- derive_fields(catch, dist, what = "distribution")
dis_t <- function(x, dom = NULL) sum(x$total[x$catchCategory == "Dis" & (is.null(dom) | grepl(paste0("^", dom), x$domainCatchDis))], na.rm = TRUE)

## ---- flag_multi_area() ----------------------------------------------------------------------
h <- heard(flag_multi_area(catch, dist))
fl <- h$value
ok(nrow(fl) == 6L && identical(fl$row, c(6L, 8L, 10L, 11L, 12L, 15L)) &&
     all(c("row", "vesselFlagCountry", "year", "catchCategory", "areaValue", "listed", "total", "variableUnit", "domain", "cells",
           "landings_rows", "landings_t", "landings_outside_t", "weights", "distribution_rows", "split", "note") %in% names(fl)),
   "one row per listed record, with the row it is in, its fields, its domain, the landings and what the split would do")
ok(identical(fl$split, c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE)) &&
     grepl("landings record", fl$note[4]) && grepl("no domain", fl$note[5]) && grepl("^equal parts: the weight", fl$note[3]) && fl$note[1] == "",
   "which can be split: not a landings record, not one with no domain; no weight in any code = equal parts, said")
ok(identical(fl$weights[1], "27.4.a 30.0 t | 27.4.b 40.0 t | 27.4.c 20.0 t") && identical(fl$landings_rows[1], 5L) && near(fl$landings_t[1], 90) &&
     near(fl$landings_outside_t[c(1, 3, 6)], c(0, 10, 10)),
   "the weight of each code is the landings of the domain in it; the landings outside the list are counted")
ok(identical(fl$distribution_rows, c(6L, 0L, 0L, 0L, 0L, 0L)), "the distribution rows of each record are counted")
ok(length(h$messages) == 1L && grepl("6 of 15 catch records name several", h$messages) && grepl("27.4.a 30.0 t | 27.4.b 40.0 t", h$messages, fixed = TRUE) &&
     grepl("cannot be split: a landings record", h$messages) && grepl("split_multi_area()", h$messages, fixed = TRUE) && !length(h$warnings),
   "it says so, once: the records, the weights, what cannot be split, and what to do")
ce <- attr(fl, "cells")
ok(is.data.frame(ce) && near(sum(ce$share[ce$row == 6L]), 1) && near(ce$share[ce$row == 6L], c(30, 40, 20) / 90) && all(is.na(ce$share[ce$row == 11L])),
   "attr(, \"cells\"): the codes of each record and their shares (NA where it cannot be split)")
ok(is.null(suppressMessages(flag_multi_area(catch))$distribution_rows), "no distribution_rows column when none were given")
none <- heard(flag_multi_area(catch[!grepl(";", catch$areaValue), ]))
ok(nrow(none$value) == 0L && grepl("no record names several", none$messages) && nrow(attr(none$value, "cells")) == 0L,
   "nothing listed: an empty table and a message")
fx <- fx_catch()
ok(nrow(suppressMessages(flag_multi_area(fx))) == 0L && nrow(suppressMessages(flag_multi_area(fx[0, ]))) == 0L, "the fixture lists nothing; no rows is fine too")
fe <- suppressMessages(flag_multi_area(catch, weight = "equal"))
ok(identical(fe$weights[1], "27.4.a 1 | 27.4.b 1 | 27.4.c 1") && all(fe$note[fe$split] == "equal parts"), "weight = \"equal\": the same weight for every code")
fc <- suppressMessages(flag_multi_area(catch, weight = "effort"))
ok(identical(fc$weights[1], "27.4.a 100 | 27.4.b 100 | 27.4.c 200"), "weight = a column: its sum over the landings of the domain in each code")
ok(grepl("neither", err(flag_multi_area(catch, weight = "nope"))) && grepl("not numeric", err(flag_multi_area(catch, weight = "domainCatchDis"))) &&
     grepl("landings.*equal", err(flag_multi_area(catch, weight = 3))) && grepl("none of the fields", err(flag_multi_area(catch[, !names(catch) %in% c("areaValue", "fleetValue", "seasonValue")]))) &&
     grepl("needs the CEF catch columns", err(flag_multi_area(catch[, c("areaValue", "total")]))),
   "a weight that is none of the three, a column that is not numeric, no field to look at, a catch without the CEF columns: errors that say what is wrong")
neg <- catch; neg$effort[1:2] <- -50
ok(grepl("negative", err(flag_multi_area(neg, weight = "effort"))), "a negative weight is an error")
fb <- suppressMessages(flag_multi_area(catch, by = "fleetValue"))
ok(nrow(fb) == 0L, "by = the fields to look at: a record that lists areas is not flagged when only the fleet is looked at")

## ---- split_multi_area(): the amount ---------------------------------------------------------
h <- heard(split_multi_area(catch, dist_d))
s <- h$value; cc <- s$catch; dd <- s$distribution
ok(identical(names(s), c("catch", "distribution")) && is.data.frame(cc) && is.data.frame(dd), "it returns the catch and the distribution")
ok(length(h$warnings) == 3L && any(grepl("row 11.*landings record", h$warnings)) && any(grepl("row 12.*no domain", h$warnings)) &&
     any(grepl("row 10.*equal parts", h$warnings)),
   "warnings: the two it leaves as they are, and the one it splits into equal parts for want of weight")
ok(length(h$messages) == 1L && grepl("4 record\\(s\\) split into 7", h$messages), "and one message: how many records became how many")
parts <- cc[!is.na(cc$splitShare), ]
d1 <- parts[grepl("^D1 ", parts$domainCatchDis), ]
ok(identical(d1$areaValue, c("27.4.a", "27.4.b", "27.4.c")) && near(d1$total, c(3, 4, 2)) && near(d1$splitShare, c(30, 40, 20) / 90),
   "D1: the amount is shared out by the landings of the domain in each area (30 / 40 / 20 t: 3 / 4 / 2 t of 9)")
ok(near(sum(d1$total), 9) && near(dis_t(cc, "D2"), 4) && near(dis_t(cc, "D3"), 2) && near(dis_t(cc, "D6"), 8),
   "what each record said is added up again by its parts")
ok(identical(parts$areaValue[grepl("^D2 ", parts$domainCatchDis)], "27.4.a") && near(parts$total[grepl("^D2 ", parts$domainCatchDis)], 4),
   "D2: an area with no landings gets no part, the others take the whole amount")
d3 <- parts[grepl("^D3 ", parts$domainCatchDis), ]
ok(identical(d3$areaValue, c("27.4.a", "27.4.b")) && near(d3$total, c(1, 1)), "D3: no weight in any area: equal parts")
d6 <- parts[grepl("^D6 ", parts$domainCatchDis), ]
ok(identical(d6$areaValue, "27.4.a") && near(d6$total, 8), "D6: the landings outside the list (27.4.f) take no share")
ok(nrow(cc) == nrow(catch) - 4L + 7L && identical(sum(grepl(";", cc$areaValue)), 2L) && all(is.na(cc$variance[!is.na(cc$splitShare)])),
   "the rows: 4 records became 7 parts; the two it could not split are still listed; variance (of the whole estimate) is NA on the parts")
pos <- which(cc$catchCategory == "Dis" & cc$domainCatchDis == "D1 [27.4.a]")
ok(identical(cc$domainCatchDis[pos:(pos + 2L)], c("D1 [27.4.a]", "D1 [27.4.b]", "D1 [27.4.c]")) && identical(cc$catchCategory[pos - 1L], "Lan"),
   "the parts stand where the record stood")
ok(identical(parts$splitFrom[1], "areaValue: 27.4.a;27.4.b;27.4.c") && all(is.na(cc$splitFrom[is.na(cc$splitShare)])),
   "splitFrom says what the part comes from; it is empty on every other record")

## ---- the domains ----------------------------------------------------------------------------
lan1 <- cc[cc$catchCategory == "Lan" & grepl("^D1", cc$domainCatchDis), ]
ok(identical(lan1$domainCatchDis, c("D1 [27.4.a]", "D1 [27.4.a]", "D1 [27.4.b]", "D1 [27.4.b]", "D1 [27.4.c]")) &&
     all(lan1$domainCatchDis %in% d1$domainCatchDis),
   "the landings of the domain follow their area: the domain is re-keyed on them as on the parts, so each part has its own landings")
ok(identical(cc$domainCatchDis[cc$catchCategory == "Lan" & cc$areaValue == "27.4.f"], "D6") &&
     identical(cc$domainCatchDis[cc$catchCategory == "Lan" & cc$areaValue == "27.4.a" & cc$total == 30], "D6 [27.4.a]"),
   "landings outside the list keep the old key")

## ---- the derived fields ---------------------------------------------------------------------
ok(identical(d1$ICESDivision, c("27.4.a", "27.4.b", "27.4.c")) && identical(d1$ICESSubArea, rep("27.4", 3)) &&
     identical(d1$Stratum, c("XX | OTB_DEF_70-99_0_0_all | 27.4.a | 2025", "XX | OTB_DEF_70-99_0_0_all | 27.4.b | 2025", "XX | OTB_DEF_70-99_0_0_all | 27.4.c | 2025")),
   "the fields derive_fields() put on the record are derived again from the one area each part has")
ok(identical(d1$metier4, rep("OTB", 3)) && identical(d1$metier4_mesh, rep("OTB_70-99", 3)), "the fleet fields, which did not change, are as they were")

## ---- the sample -----------------------------------------------------------------------------
ok(near(d1$numTrips, 12 * c(30, 40, 20) / 90) && near(sum(d1$numTrips), 12), "the 12 trips behind the record are shared out like the amount, so they add up to 12")
sr <- suppressWarnings(suppressMessages(split_multi_area(catch, dist_d, counts = "repeat")))
ok(identical(sr$catch$numTrips[!is.na(sr$catch$splitShare) & grepl("^D1 ", sr$catch$domainCatchDis)], rep(12, 3)) &&
     near(sr$distribution$numMeasurements[sr$distribution$distributionClass == 110 & sr$distribution$variableType == "Number"], rep(20, 3)),
   "counts = \"repeat\": every part keeps the whole sample")

## ---- the distribution -----------------------------------------------------------------------
db <- dd[grepl("^B1", dd$domainBiology), ]
ok(identical(sort(unique(db$domainBiology)), c("B1 [27.4.a]", "B1 [27.4.b]", "B1 [27.4.c]")) && nrow(db) == 18L && nrow(dd) == nrow(dist_d) + 12L,
   "the distribution is copied for every part under the part's domainBiology")
ok(all(db$domainBiology[db$areaValue == "27.4.b"] == "B1 [27.4.b]") && identical(unique(db$areaValue), c("27.4.a", "27.4.b", "27.4.c")) &&
     identical(unique(db$ICESDivision), c("27.4.a", "27.4.b", "27.4.c")),
   "... and is derived again from the split catch: each copy has the one area of its part")
ok(near(sum(db$value[db$valueType == "Total"]), sum(dist_d$value[dist_d$valueType == "Total"])) &&
     near(db$value[db$valueType == "Total" & db$distributionClass == 110], 2 * c(30, 40, 20) / 90),
   "the totals (the fish) are shared out by the same shares and add up to what they were")
ok(all(db$value[db$valueType == "Mean"] == db$distributionClass[db$valueType == "Mean"] * 0.5),
   "the mean weights stay as they are")
ok(near(sum(db$numMeasurements[db$valueType == "Total" & db$distributionClass == 110]), 20) &&
     near(sum(db$numTrips[db$valueType == "Total" & db$distributionClass == 110]), 4),
   "the fish measured and the trips behind a class are shared out too: the 20 fish and 4 trips of class 110 add up over the three parts")
catch_p <- cc[cc$catchCategory == "Dis" & grepl("^D1 ", cc$domainCatchDis), ]
ok(all(catch_p$domainBiology %in% db$domainBiology) && all(db$domainBiology %in% catch_p$domainBiology),
   "the parts and their distributions link one to one")
sd <- suppressWarnings(suppressMessages(split_multi_area(catch, dist)))$distribution
ok(identical(sd$domainBiology[1:3], c("B1 [27.4.a]", "B1 [27.4.b]", "B1 [27.4.c]")) && !any(c("areaValue", "ICESDivision") %in% names(sd)),
   "a distributions table that was not derived stays as it was: no fields of derive_fields() are put on it")
other <- mk("Dis", "27.4.z", "2025", 1, "D7", "B1"); other$effort <- NA_real_      # another discard record of the same country that uses B1
shared <- derive_fields(rbind(catch_raw, other))
hs <- heard(split_multi_area(shared, dist))
ok(any(grepl("used by other catch records as well", hs$warnings)) && !any(grepl("^B1 \\[", hs$value$distribution$domainBiology)) &&
     all(hs$value$catch$domainBiology[!is.na(hs$value$catch$splitShare) & grepl("^D1 ", hs$value$catch$domainCatchDis)] == "B1"),
   "a distribution other records use as well is not split: the parts share it, and it says so")

## ---- the weight, the fields ------------------------------------------------------------------
se <- suppressWarnings(suppressMessages(split_multi_area(catch, weight = "equal")))$catch
ok(near(se$total[!is.na(se$splitShare) & grepl("^D1 ", se$domainCatchDis)], c(3, 3, 3)), "weight = \"equal\": the same share for every area")
sc <- suppressWarnings(suppressMessages(split_multi_area(catch, weight = "effort")))$catch
ok(near(sc$total[!is.na(sc$splitShare) & grepl("^D1 ", sc$domainCatchDis)], 9 * c(100, 100, 200) / 400),
   "weight = a column: the amount is shared by its sum over the landings of the domain in each area")
only_fleet <- heard(split_multi_area(catch, by = "fleetValue"))
ok(nrow(only_fleet$value$catch) == nrow(catch) && grepl("nothing to split", only_fleet$messages), "by = \"fleetValue\": a record that lists areas is left as it is")

## ---- fleets (the Swedish DisBMS records of the RDBES download) --------------------------------
fleets <- rbind(
  mk("Lan", "27.3.a.21", "2", 14.6, "F1", fleet = "GTR_DEF_all_0_0_all", country = "SE"), mk("Lan", "27.3.a.21", "2", 4.1, "F1", fleet = "GNS_DEF_all_0_0_all", country = "SE"),
  mk("DisBMS", "27.3.a.21", "2", 37.3, "F1", fleet = "GNS_DEF_all_0_0_all;GTR_DEF_all_0_0_all", country = "SE"))
fleets$metier6 <- c("GTR_DEF_all_0_0", "GNS_DEF_all_0_0", NA)
fleets <- derive_fields(fleets)
sf <- suppressMessages(split_multi_area(fleets))$catch
pf <- sf[!is.na(sf$splitShare), ]
ok(identical(pf$fleetValue, c("GNS_DEF_all_0_0_all", "GTR_DEF_all_0_0_all")) && near(pf$total, 37.3 * c(4.1, 14.6) / 18.7) && near(sum(pf$total), 37.3),
   "a record that lists two fleets: one part for each, the amount shared out by the landings of each fleet")
ok(identical(pf$metier4, c("GNS", "GTR")) && identical(pf$metier4_mesh, c("GNS_all", "GTR_all")) && identical(pf$metier6, c("GNS_DEF_all_0_0", "GTR_DEF_all_0_0")) &&
     identical(pf$splitFrom[1], "fleetValue: GNS_DEF_all_0_0_all;GTR_DEF_all_0_0_all"),
   "the fleet fields (and the CEF's own metier6, cut like the other rows') are those of the part's fleet")
ok(identical(sf$domainCatchDis[sf$catchCategory == "Lan"], c("F1 [GTR_DEF_all_0_0_all]", "F1 [GNS_DEF_all_0_0_all]")), "the landings follow their fleet")

## ---- again, and the class --------------------------------------------------------------------
clean <- catch[catch$domainCatchDis %in% "D1", ]
s1 <- suppressMessages(split_multi_area(clean, dist_d[dist_d$domainBiology == "B1", ]))
h2 <- heard(split_multi_area(s1$catch, s1$distribution))
ok(identical(h2$value$catch, s1$catch) && identical(h2$value$distribution, s1$distribution) && grepl("nothing to split", h2$messages),
   "a second call has nothing to split and returns what it was given")
tb <- suppressMessages(split_multi_area(tibble::as_tibble(clean), tibble::as_tibble(dist)))
ok(inherits(tb$catch, "tbl_df") && inherits(tb$distribution, "tbl_df") && nrow(tb$catch) == nrow(s1$catch), "a tibble comes back a tibble")
nodist <- suppressMessages(split_multi_area(clean))
ok(is.null(nodist$distribution) && identical(nodist$catch$total, s1$catch$total), "without distributions it splits the catch alone and returns NULL for them")

## ---- the guard: groups that cut a domain -----------------------------------------------------
## AA: D_A (listed Dis 4 t over 27.4.a and 27.4.b, landings 30 and 10 t) and BB: its own domain in 27.4.a (Dis 2 t, landings 40 t) and landings in 27.4.b with
## no discard estimate (20 t) that the group of 27.4.b raises - at the rate of its donors: 1 t of discards on the 10 t of AA's 27.4.b landings.
raise_set <- rbind(
  mk("Lan", "27.4.a", "1", 30, "DA", country = "AA"), mk("Lan", "27.4.b", "1", 10, "DA", country = "AA"), mk("Dis", "27.4.a;27.4.b", "2025", 4, "DA", country = "AA"),
  mk("Lan", "27.4.a", "1", 40, "DB", country = "BB"), mk("Dis", "27.4.a", "2025", 2, "DB", country = "BB"),
  mk("Lan", "27.4.b", "1", 20, NA_character_, country = "BB"))
raise_set <- derive_fields(raise_set)
q <- rlang::quo
by_area <- list(Ga = q(areaValue == "27.4.a"), Gb = q(areaValue == "27.4.b"))
by_country <- list(GA = q(vesselFlagCountry == "AA"), GB = q(vesselFlagCountry == "BB"))
lf <- tempfile(fileext = ".txt")
e1 <- err(check_group_conditions(raise_set, by_area, logFile = NULL, append = TRUE))
ok(grepl("1 of 2 domains with a discard estimate are cut by the groups", e1) && grepl("AA | DA", e1, fixed = TRUE) &&
     grepl("discards 4 t in (no group)", e1, fixed = TRUE) && grepl("landings 40 t in Ga | Gb", e1, fixed = TRUE) && grepl("split_multi_area()", e1, fixed = TRUE),
   "check_group_conditions(): the groups by area cut the domain of the listed record - the estimate is in no group, its landings in two - and it says which domain and what to do")
e2 <- err(catchRaising(raise_set, by_area, verbose = FALSE, logFile = lf))
ok(grepl("cut by the groups", e2), "catchRaising() stops there, before anything is raised")
ok(!nzchar(err(check_group_conditions(raise_set, by_country, logFile = NULL, append = TRUE))), "groups that keep the domain whole (by country) pass")
no_est <- rbind(mk("Lan", "27.4.a", "1", 30, "DX", country = "AA"), mk("Lan", "27.4.b", "1", 10, "DX", country = "AA"), mk("Dis", "27.4.a", "2025", 2, "DB", country = "BB"),
                mk("Lan", "27.4.a", "1", 40, "DB", country = "BB"))
ok(!nzchar(err(check_group_conditions(derive_fields(no_est), by_area, logFile = NULL, append = TRUE))),
   "a domain with landings in two groups but no estimate has nothing to lose: no error")
sp <- suppressMessages(split_multi_area(raise_set))$catch
ok(!nzchar(err(check_group_conditions(sp, by_area, logFile = NULL, append = TRUE))), "after the split the groups by area do not cut a domain")
rs <- suppressWarnings(suppressMessages(catchRaising(sp, by_area, verbose = FALSE, logFile = lf)))
aa <- rs[rs$vesselFlagCountry == "AA" & rs$catchCategory == "Dis", ]
bb <- rs[rs$vesselFlagCountry == "BB" & rs$catchCategory == "Dis" & rs$importedOrRaised == "raised", ]
ok(near(sum(aa$total), 4000) && all(aa$importedOrRaised == "imported") && nrow(aa) == 2L,
   "raised after the split: the 4 t of the record are in the output once, as imported, in two parts (3 t + 1 t)")
ok(near(sum(bb$total), 2000) && identical(bb$areaValue, "27.4.b"),
   "... and the landings without an estimate (20 t, 27.4.b) are raised at the rate of the part of their area: 1 t on 10 t, so 2 t")
## why the domains are re-keyed: with the parts and the landings of both areas under ONE key the raising takes two discard rows for the domain and divides its
## rate between them - 1 t, not 2 t, for the same landings (3 t + 1 t of discards on the 40 t of landings of the domain: (3/40 + 1/40) / 2 = 0.05, not 0.1)
nokey <- sp; nokey$domainCatchDis <- sub(" \\[.*$", "", nokey$domainCatchDis)
rn <- suppressWarnings(suppressMessages(catchRaising(nokey, by_area, verbose = FALSE, logFile = lf)))
ok(near(sum(rn$total[rn$vesselFlagCountry == "BB" & rn$catchCategory == "Dis" & rn$importedOrRaised == "raised"]), 1000),
   "the parts under one domain key (not re-keyed) would halve the rate the landings without an estimate are raised at")

## what is a cut and what is not: a landing in a stratum that has no estimate of its domain, while the domain has one elsewhere
by_season <- list(S1 = q(seasonValue == "1"), S2 = q(seasonValue == "2"))
per_season <- derive_fields(rbind(mk("Lan", "27.4.a", "1", 10, "DS"), mk("Dis", "27.4.a", "1", 1, "DS"), mk("Lan", "27.4.a", "2", 20, "DS"), mk("Dis", "27.4.a", "2", 3, "DS")))
ok(!nzchar(err(check_group_conditions(per_season, by_season, logFile = NULL, append = TRUE))),
   "one domain with an estimate for each season, each stratum holding the estimate of its landings (the test fixture's way): no error")
one_est <- derive_fields(rbind(mk("Lan", "27.4.a", "1", 10, "DT"), mk("Dis", "27.4.a", "1", 1, "DT"), mk("Lan", "27.4.a", "2", 20, "DT")))
e3 <- err(check_group_conditions(one_est, by_season, logFile = NULL, append = TRUE))
ok(grepl("1 of 1 domains", e3) && grepl("discards 1 t in S1, landings 20 t in S2 (no estimate there)", e3, fixed = TRUE),
   "an estimate in one stratum and landings of the same domain in another that has none: cut - said with where each is")
ok(!nzchar(err(check_group_conditions(one_est, list(All = q(TRUE)), logFile = NULL, append = TRUE))), "the same catch in one group: no error")

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
