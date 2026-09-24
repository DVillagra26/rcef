## ===========================================================
## raising_walkthrough.R
##
## NOT a test - nothing here asserts pass/fail (the actual test suite is
## dev/tests/*.R). This is a template you run by hand to see the discard
## RAISING side of the package end to end, for the ONE scheme actually in
## use: the manual grouping (section 1). The hierarchical cascade and the
## scenario-to-scenario comparison used to live here too; they are now in
## the sibling dev/raising_comparison_schemes.R, which starts from the
## same catch_data and the same manual scheme as a baseline to compare
## against - this file stays about producing the real deliverable, not
## about exploring alternatives to it.
##
## Driven directly with the vendored RDBESStockCoord functions -
## check_group_conditions() and catchRaising() (R/RDBESStockCoord_utils.R,
## R/RDBESStockCoord_raising.R) - the same pattern ICES's own
## discard-raising scripts use: a named list of quosures per method
## (strataCond = which strata a group claims, matchedDataCond = which
## data its discard rate is estimated from), checked, then raised.
## Nothing wraps that call.
##
## Data: the same `catch_data` as plot_walkthrough.R - raw CEF catch,
## already through derive_fields(). Swap in your own; nothing past
## section 0 needs to change either way.
##
## Sections 4-5 build the two output data sets this whole file exists
## for - CC (one row per catchCategory, the stock-wide raised total) and
## CAR/CAS (every raised row, and the donor strata behind each group's
## rate) - following the schema of a real ICES stock-annex CAR/CAS
## export (CAR-CAS_detailed.csv, supplied 2026-09-23). Built inline here,
## as plain data frames, on purpose - not wrapped in functions and not
## written to csv yet. Once the shape is right, both become functions
## that build and save these two data sets; that is future work, not
## this script's job today.
##
## Every figure below is drawn with print() and nothing else - no pdf()/
## png()/jpeg() device, no dev.off(). Run it in an interactive session
## (RStudio, or R itself) and each print() lands in the live graphics
## window / Plots pane, one after another, to actually look at.
## ===========================================================

library(dplyr)
library(rcef)


## =====================================================================
##  0. LOAD - identical to plot_walkthrough.R's section 0. See that file
##     for what each piece does; not repeated here.
## =====================================================================
bll_dir  <- file.path("inst", "extdata", "data_overviews_format", "bll")
bll_year <- 2025
catch_data <- NULL; distribution_data <- NULL

if (dir.exists(file.path(bll_dir, bll_year)) &&
    requireNamespace("icesVocab", quietly = TRUE) &&
    requireNamespace("icesSD", quietly = TRUE)) {
  bll_ic <- tryCatch(
    funICoutCEF(dat_path = bll_dir, years = bll_year, output_format = "to_list"),
    error = function(e) {
      message("load failed - funICoutCEF(): ", conditionMessage(e))
      NULL
    })
  if (!is.null(bll_ic) && !is.null(bll_ic$catches) && nrow(bll_ic$catches)) {
    catch_data <- derive_fields(as.data.frame(bll_ic$catches))
    # catchRaising() multiplies `total` by a unit-conversion ratio inside
    # convert_field(), so it must be numeric - funICoutCEF()'s CEF output
    # brings the numeric CEF fields in as character. Coerce them once,
    # here, before raising touches them.
    num_fields <- intersect(c("total", "variance", "numPSUs", "numTrips"),
                            names(catch_data))
    for (f in num_fields)
      if (!is.numeric(catch_data[[f]]))
        catch_data[[f]] <- suppressWarnings(
          as.numeric(gsub("[, ]", "", as.character(catch_data[[f]]))))
    distribution_data <- if (!is.null(bll_ic$distributions))
      as.data.frame(bll_ic$distributions) else NULL
    cat(sprintf("bll %s: %d catch rows after derive_fields()\n",
               bll_year, nrow(catch_data)))
  }
} else {
  cat("load skipped - bll extdata and/or icesVocab+icesSD not available\n")
}
## No build_strata()/add_stratum_metrics() call here, unlike the
## comparison file - the manual scheme's conditions (below) are plain
## gear/catchCategory/fleetValue tests against catch_data as loaded, and
## sections 4-5 compute their own landings/discard sums directly off
## catch_data too. Nothing in this file reads a stratum_id.


## =====================================================================
##  1. MANUAL GROUPING - hand-written quosures, checked and raised with
##     check_group_conditions() / catchRaising() directly.
## =====================================================================
## Same six groups as the RDBESStockCoord example this is based on
## (gear is derive_fields()'s literal alias for metier4 - one column
## under two names; kept as `gear` here because that is the name the
## source conditions use). G1-G3 split the main gears, G4 is a REST
## group for everything else, G5 pulls out the fleetValue sentinel for
## incidental bycatch (IBC), G6 is BMS on its own - between them every
## row is claimed exactly once, discards or not.
strataCond_manual <- list(
  G1 = quo(gear == "TBB" & catchCategory != "BMS"),
  G2 = quo(gear %in% c("GTR", "GNS") & catchCategory != "BMS"),
  G3 = quo(gear %in% c("OTB", "SSC", "SDN") & catchCategory != "BMS"),
  G4 = quo(!gear %in% c("TBB", "GTR", "GNS", "OTB", "SSC", "SDN") &
             catchCategory != "BMS" & fleetValue != "MIS_MIS_0_0_0_IBC"),
  G5 = quo(fleetValue == "MIS_MIS_0_0_0_IBC" & catchCategory != "BMS"),
  G6 = quo(catchCategory == "BMS"))

## matchedDataCond: G1-G3, G5 and G6 are raised on their own strata (the
## same condition, reused). G4 (REST) has too little discard data of its
## own and draws on every gear instead - minus five strata whose own
## discard ratio the source example flagged as implausible (verified
## present in this bll 2025 data too: Sweden/27.3.a.21 and France/27.7.e
## both occur). Comment out the `!(...)` block to see their effect on
## G4's rate.
matchedDataCond_manual <- list(
  G1 = strataCond_manual$G1,
  G2 = strataCond_manual$G2,
  G3 = strataCond_manual$G3,
  G4 = quo(catchCategory != "BMS" &
             fleetValue != "MIS_MIS_0_0_0_IBC" &
             !((vesselFlagCountry == "Sweden" & areaValue == "27.3.a.21" &
                  fleetValue == "GNS_DEF_all_0_0_all" & seasonValue %in% c(2, 3)) |
                 (vesselFlagCountry == "Sweden" & areaValue == "27.3.a.21" &
                    fleetValue == "GTR_DEF_all_0_0_all" & seasonValue %in% c(2, 4)) |
                 (vesselFlagCountry == "France" & areaValue == "27.7.e" &
                    fleetValue == "GTR_DEF_100-119_0_0_all" & seasonValue == 2) |
                 (vesselFlagCountry == "Sweden" & areaValue == "27.3.a.21" &
                    fleetValue == "OTB_CRU_90-119_0_0_all" & seasonValue == 3) |
                 (vesselFlagCountry == "Sweden" & areaValue == "27.3.a.21" &
                    fleetValue == "OTB_CRU_70-89_2_35_all" & seasonValue == 1))),
  G5 = strataCond_manual$G5,
  G6 = strataCond_manual$G6)

## ---- 1a. condition diagnostics ---------------------------------------
## check_group_conditions() is what catchRaising() runs internally before
## it raises anything; running it here too, separately, is the "optional,
## automatically run with raising, but good practice" step of the
## RDBESStockCoord scripts - every stratum checked for both a gap
## (claimed by no group) and an overlap (claimed twice) before any weight
## moves.
cond_st <- check_group_conditions(catch_data, strataCond_manual,
                                  domain = "domainCatchDis",
                                  originType = "WGEstimate",
                                  variableType = "WeightLive",
                                  logFile = NULL, append = TRUE)
cond_md <- check_group_conditions(catch_data, matchedDataCond_manual,
                                  conditionType = "matched_data",
                                  domain = "domainCatchDis",
                                  originType = "WGEstimate",
                                  variableType = "WeightLive",
                                  logFile = NULL, append = TRUE)
strata_matrix <- do.call(cbind, cond_st)
cat(sprintf("manual: %d rows in no group, %d rows in more than one (both should be 0)\n",
           sum(rowSums(strata_matrix) == 0), sum(rowSums(strata_matrix) > 1)))
print(colSums(strata_matrix))

## ---- 1b. raise the catch ----------------------------------------------
## catchRaising() prints its own "Condition diagnostics" banner per
## condition pair, unconditionally (the check above, run again,
## internally) - that noise below is the ENGINE's own logging, not this
## script's.
raised_manual <- catchRaising(catch_data = catch_data,
                              condition_raising_st_list = strataCond_manual,
                              condition_matched_data_list = matchedDataCond_manual,
                              type = "discards",
                              originType = "WGEstimate",
                              variableType = "WeightLive")

cat(sprintf("manual raising: %d rows (%d imported, %d raised)\n",
           nrow(raised_manual),
           sum(raised_manual$importedOrRaised == "imported"),
           sum(raised_manual$importedOrRaised == "raised")))


## =====================================================================
##  2. OVERVIEW OF THE RAISED WEIGHT - imported vs raised, any aggregation
## =====================================================================
## plot_raised_composition() takes any catch-side column on x and an
## optional facet: landings/discards are always the fill, imported/
## raised is always the shading, and whatever you put on x or facet is
## the aggregation.

raised_overview_table(raised_manual)

## by metier
print(plot_raised_composition(raised_manual, x_var = "metier4"))
## by country, faceted by season - a finer aggregation than the group
## definitions themselves ever used
print(plot_raised_composition(raised_manual, x_var = "metier4", facet_var = "Country"))


## =====================================================================
##  3. DISCARD RATE ACROSS THE RAISING GROUPS - homogeneity / outliers
## =====================================================================
## Not how much weight moved (section 2), but whether the STRATA A GROUP
## DRAWS ITS RATE FROM agree with each other. plot_discard_rate() draws
## one point per stratum from any table carrying discard_rate; put the
## raising GROUP on x and every point under a group is one of the strata
## that fed its rate - a group with points scattered from 2% to 40% is
## not a group, it is a guess, no matter how well it covers the stock.
##
## matchedDataCond_manual filtered straight against catch_data, and each
## group's rows turned into one donor stratum per domainCatchDis + gear by
## build_coverage_strata() - the same granularity catchRaising() itself
## estimates a ratio from, and the same check the source example runs
## before raising ("Per-stratum spread behind each group average"). A
## stratum table at metier4+Country+Season cannot stand in here:
## catchCategory and fleetValue (both load-bearing above, G4's exclusions
## especially) do not survive that aggregation.
##
## The table's `rate` is discards / covered landings - the very number
## section 5's CAS output reports as `rate` (there per CEF row, here per
## donor stratum), so what is plotted here is what ends up in the CAS.
donor_detail_manual <- bind_rows(lapply(names(matchedDataCond_manual), function(g) {
  donors <- catch_data %>%
    filter(!!matchedDataCond_manual[[g]],
           !is.na(domainCatchDis),
           originType == "WGEstimate",
           variableType == "WeightLive") %>%
    build_coverage_strata(c("domainCatchDis", "gear"))
  # NULL for a group with no landings / discards among its donors at all
  # (G6 is BMS only; G5 has no WGEstimate discard record - see section 5)
  if (!is.null(donors)) mutate(donors, donor_group = g, .before = 1)
}))

print(plot_discard_rate(donor_detail_manual, x = "donor_group", colour = "gear",
                        jitter = TRUE))
donor_detail_manual %>% group_by(donor_group) %>%
  summarise(n = n(),
           mean_rate = round(weighted.mean(rate, w = weightingTotal, na.rm = TRUE), 3),
           sd_rate = round(sd(rate, na.rm = TRUE), 3),
           min_rate = round(min(rate, na.rm = TRUE), 3),
           max_rate = round(max(rate, na.rm = TRUE), 3),
           .groups = "drop") %>% print()


## =====================================================================
##  4. OUTPUT - CC (CATCH COMBINED): one row per catchCategory, the
##     stock-wide raised total
## =====================================================================
## The headline number this file exists to produce: total landings and
## total (sampled + raised) discards for the stock, in tonnes. One row
## per catchCategory actually present in raised_manual - usually Lan and
## Dis, but BMS too if the stock reports it; nothing below is hardcoded
## to just those two.
##
## car_version/car_version_desc/car_scheme_name identify THIS allocation
## scheme's run - a different scheme (the hierarchical one, or a
## different manual rule set) would carry a different version, so
## several runs' CC/CAR/CAS outputs can sit in the same downstream table
## without being confused for each other. All three are reused as-is in
## section 5 below - keep every use in sync if any of them changes.
car_version      <- 1
car_version_desc <- "Manual_Stock_Annex"
car_scheme_name  <- "Stock_Annex"

## year/workingGroup/stock come straight off the loaded data, not typed
## in by hand - assumed constant across the whole run (one stock, one
## year, per script run; the stopifnot catches it loudly if that is ever
## not true rather than silently picking the first value).
year_val        <- unique(catch_data$year)
workingGroup_val <- unique(catch_data$workingGroup)
stock_val       <- unique(catch_data$stock)
stopifnot(length(year_val) == 1, length(workingGroup_val) == 1,
         length(stock_val) == 1)

cc_out <- raised_manual %>%
  filter(originType == "WGEstimate", variableType == "WeightLive") %>%
  group_by(catchCategory) %>%
  summarise(total = round(sum(total, na.rm = TRUE) / 1000, 3), .groups = "drop") %>%
  transmute(recordType = "CC", version = car_version,
           versionDescription = car_version_desc,
           calendarYear = "Y", year = year_val,
           workingGroup = workingGroup_val, stock = stock_val,
           catchCategory,
           seasonType = NA_character_, seasonValue = NA_character_,
           areaType = NA_character_, areaValue = NA_character_,
           fleetValue = NA_character_,
           variableType = "WeightLive", variableUnit = "t",
           total, variance = NA_real_, numTrips = NA_integer_)
cat("\nCC (Catch Combined):\n")
print(cc_out)


## =====================================================================
##  5. OUTPUT - CAR / CAS (DETAILED): every raised row, and the donor
##     strata behind each group's rate
## =====================================================================
## Two tables sharing one allocation-scheme identity (recordType,
## version, versionDescription, catchAllocationSchemeName line up
## between them, the same values section 4 already set) - kept as two
## separate data frames for now, per-row CAR above and per-donor-stratum
## CAS below. Combining them into one CSV with CAS appended under CAR,
## and writing either to disk, is future work: this script builds the
## two data frames and stops there.

## ---- 5a. CAR - every row catchRaising() produced, tagged with the
## GROUP that raised it. catchAllocationGroupName is filled ONLY where a
## row was actually raised (blank on every imported row - nothing raised
## it, so there is nothing to name); `imported` is Y/N, matching the
## source RDBESStockCoord convention rather than rcef's own "imported"/
## "raised" wording. catchRaising() does not tag a raised row with the
## group that produced it (DrGroup is commented out in the vendored
## engine), so the tag is rebuilt here by re-testing every row against
## strataCond_manual - the same partition check_group_conditions()
## already confirmed in section 1 is exhaustive and non-overlapping, so
## exactly one group matches every row (imported or raised alike); only
## RAISED rows keep it, per the paragraph above.
raised_manual <- raised_manual %>%
  mutate(.car_group = case_when(
    !!strataCond_manual$G1 ~ "G1",
    !!strataCond_manual$G2 ~ "G2",
    !!strataCond_manual$G3 ~ "G3",
    !!strataCond_manual$G4 ~ "G4",
    !!strataCond_manual$G5 ~ "G5",
    !!strataCond_manual$G6 ~ "G6",
    TRUE ~ NA_character_))

car_out <- raised_manual %>%
  mutate(recordType = "CAR",
        version = car_version,
        versionDescription = car_version_desc,
        catchAllocationSchemeName = car_scheme_name,
        catchAllocationGroupName = if_else(importedOrRaised == "raised",
                                           .car_group, NA_character_),
        imported = if_else(importedOrRaised == "imported", "Y", "N")) %>%
  select(-.car_group, -importedOrRaised) %>%
  select(recordType, version, versionDescription, catchAllocationSchemeName,
        catchAllocationGroupName, imported, everything())

cat(sprintf("\nCAR: %d rows (%d imported, %d raised across %d groups)\n",
           nrow(car_out), sum(car_out$imported == "Y"), sum(car_out$imported == "N"),
           n_distinct(car_out$catchAllocationGroupName, na.rm = TRUE)))
print(head(car_out %>% arrange(desc(imported == "N"), catchAllocationGroupName), 10))

## ---- 5b. CAS - the donor strata behind each group's rate: only the
## imported Dis rows that satisfy at least one group's matchedDataCond -
## the ones catchRaising() actually pulled a rate from. Unlike CAR above,
## this is NOT a partition: G4's matchedDataCond draws on every gear
## (section 1's own comment explains why), so a stratum that is G1's OWN
## donor is ALSO one of G4's many donors - it appears once PER GROUP IT
## FEEDS, not once per row.
##
## weightingTotal/rate are per THIS stratum, matched on domainCatchDis -
## the column CEF itself uses to link a Dis estimate to its domain:
## weightingTotal is that domain's own landed weight (kg, same unit as
## `total` - unlike section 4's CC table, CAR/CAS stay in the CEF's
## native kg throughout), and rate is this row's discards over that
## landed weight (discards / landings, NOT the raising side's
## discard_rate = discards / (landings + discards) - matching the source
## RDBESStockCoord convention's unbounded dis_ratio, and the same `rate`
## build_coverage_strata() and plot_discard_rate() use, confirmed against
## the reference CAR-CAS_detailed.csv: its rate column is exactly
## total / weightingTotal, occasionally above 1).
landings_by_domain <- catch_data %>%
  filter(catchCategory == "Lan", originType == "WGEstimate",
        variableType == "WeightLive", !is.na(domainCatchDis)) %>%
  group_by(domainCatchDis) %>%
  summarise(.landings = sum(total, na.rm = TRUE), .groups = "drop")

cas_out <- bind_rows(lapply(names(matchedDataCond_manual), function(g)
  catch_data %>%
    filter(!!matchedDataCond_manual[[g]], catchCategory == "Dis",
          !is.na(domainCatchDis), originType == "WGEstimate",
          variableType == "WeightLive") %>%
    mutate(catchAllocationGroupName = g))) %>%
  left_join(landings_by_domain, by = "domainCatchDis") %>%
  mutate(recordType = "CAS",
        version = car_version,
        versionDescription = car_version_desc,
        catchAllocationSchemeName = car_scheme_name,
        imported = "Y",
        weightingCategory = "Lan",
        weightingParameter = "WeightLive",
        averagingMethod = "weighted",
        weightingTotal = round(.landings, 3),
        rate = round(total / .landings, 4)) %>%
  select(-.landings) %>%
  select(recordType, version, versionDescription, catchAllocationSchemeName,
        catchAllocationGroupName, imported, everything())

cat(sprintf("\nCAS: %d donor rows across %d groups (%d distinct strata - some strata donate to more than one group)\n",
           nrow(cas_out), n_distinct(cas_out$catchAllocationGroupName),
           n_distinct(cas_out$domainCatchDis)))
print(head(cas_out %>% arrange(catchAllocationGroupName), 10))

## every group CAR raised something for should have at least one CAS
## donor row, and vice versa - the two tables describing the same
## groups, not two different countings. A group CAN legitimately fail
## this (this data's G5/IBC does): catchRaising() and every donor filter
## above are restricted to originType == "WGEstimate", so a group whose
## own discard records all carry some OTHER originType (IBC's here are
## all "Official") has no WGEstimate donor at all - CAS then has no row
## for it, and its CAR rows raised on nothing get total = NaN, not a
## real weight. That is a genuine gap in the SCHEME (this group cannot
## really be raised as defined), not a bug in this table - printed by
## name below so it is never just a silent FALSE.
car_groups <- na.omit(unique(car_out$catchAllocationGroupName))
cas_groups <- unique(cas_out$catchAllocationGroupName)
ok_linked <- setequal(car_groups, cas_groups)
cat(sprintf("\nevery CAR group also appears in CAS, and vice versa: %s\n", ok_linked))
if (!ok_linked) {
  raised_no_donor <- setdiff(car_groups, cas_groups)
  donor_not_raised <- setdiff(cas_groups, car_groups)
  if (length(raised_no_donor))
    cat(sprintf("  raised in CAR but no CAS donor (raised total is NaN - check originType): %s\n",
               paste(raised_no_donor, collapse = ", ")))
  if (length(donor_not_raised))
    cat(sprintf("  donor in CAS but nothing raised under it in CAR: %s\n",
               paste(donor_not_raised, collapse = ", ")))
}


cat("\nDone - cc_out / car_out / cas_out are plain data frames in this\n",
   "session; nothing was written to a file.\n")
