## ===========================================================
## raising_walkthrough.R
##
## NOT a test - nothing here asserts pass/fail (the actual test suite is
## dev/tests/*.R). This is a template you run by hand to see the discard
## RAISING side of the package end to end.
##
## Driven directly with the vendored RDBESStockCoord functions -
## check_group_conditions() and catchRaising() (R/RDBESStockCoord_utils.R,
## R/RDBESStockCoord_raising.R) - the same pattern ICES's own
## discard-raising scripts use: a named list of quosures per method
## (strataCond = which strata a group claims, matchedDataCond = which
## data its discard rate is estimated from), checked, then raised.
## Nothing wraps that call. rcef's own group-definition helpers
## (recommend_raising(), and in section 5 only, new_rule()) still do the
## one thing RDBESStockCoord itself has no opinion on - HOW to define a
## group from a hierarchy or a rule set - but the raising call they feed
## is the same catchRaising() either way.
##
## Data: the same `catch_data` as plot_walkthrough.R - raw CEF catch,
## already through derive_fields(). Swap in your own; nothing past
## section 0 needs to change either way.
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
    # here, before either raising method below touches them.
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

if (is.null(catch_data)) {
  cat("\nNothing to raise: catch_data did not load (see the message above).\n",
     "Point bll_dir / bll_year at your own data, or set catch_data by\n",
     "hand, and re-source this script.\n")
} else {


## The stratum resolution the hierarchical method and the homogeneity
## check (section 4) share. "Own stratum" IS this set of columns - a
## stratum finer than this cannot be raised on its own data, because
## there IS no finer resolution defined.
dims <- c("metier4", "Country", "Season")
strata0 <- build_strata(catch_data, dims)
cat(sprintf("\n%d strata at metier4 + Country + Season, %s t landed\n",
           nrow(strata0), format(round(sum(strata0$landings), 1), big.mark = ",")))


## =====================================================================
##  1. MANUAL GROUPING - hand-written quosures, checked and raised with
##     check_group_conditions() / catchRaising() directly.
## =====================================================================
## One condition per group: strataCond says which strata a group claims,
## matchedDataCond which data its discard rate is estimated from - here
## the same set, so every group is raised on its own strata only. Real
## bll metier4 levels used: TBB, OTB/OTM (otter and otter-twin trawls), a
## catch-all last, written as the explicit complement (a bare TRUE would
## re-claim rows G1/G2 already took, and catchRaising() would stop with
## "records assigned to more than one stratum").
strataCond_manual <- list(
  G1 = quo(metier4 == "TBB"),
  G2 = quo(metier4 %in% c("OTB", "OTM")),
  G3 = quo(!metier4 %in% c("TBB", "OTB", "OTM")))
matchedDataCond_manual <- strataCond_manual

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
##  2. HIERARCHICAL GROUPING - recommend_raising()
## =====================================================================
## RDBESStockCoord has no notion of "cascade fine to coarse until
## coverage clears a bar" - that is rcef's own recommend_raising(), a
## cascade of pools coarsening from "own stratum" outward: a stratum with
## enough of its OWN sampling (self_threshold) is not pooled at all;
## otherwise it is pooled with strata sharing less and less of the
## hierarchy, in the order given, each level with its own coverage
## threshold. conds_from_auto() is the minimal glue turning that
## recommendation into the same strataCond/matchedDataCond shape as
## section 1 - the raising call itself is, again, catchRaising() directly.
hierarchy <- c("Country", "metier4", "Season")
thresholds <- c(70, 60, 40)          # one per hierarchy level
self_threshold <- 90                 # own-stratum bar, before any pooling

rec <- recommend_raising(strata0, hierarchy, thresholds,
                         self_threshold = self_threshold, dims = dims)

## ---- 2a. the cascade, step by step -------------------------------------
tree <- decision_tree(rec, hierarchy, thresholds, self_threshold,
                      dims = dims, cumulative = TRUE)
print(tree)
chk <- check_partition(rec, tree)
cat(sprintf("partition check: %s strata / %s landings accounted for%s\n",
           if (chk$n_ok) "all" else sprintf("%d of %d", chk$n_steps, chk$n_total),
           if (chk$l_ok) "all" else sprintf("%s of %s t", chk$l_steps, chk$l_total),
           if (chk$unassigned > 0)
             sprintf(" (%d unassigned)", chk$unassigned) else ""))

auto_conds <- conds_from_auto(rec)
## group_id per stratum, for the homogeneity check below - the same
## lookup conds_from_auto() itself already built (auto_conds$groups), no
## need to call raising_groups() a second time.
rec$group_id <- auto_conds$groups$group_id[match(rec$raise_condition,
                                                  auto_conds$groups$raise_condition)]
print(auto_conds$groups)

## ---- 2b. condition diagnostics, and the raise itself --------------------
## conds_from_auto() writes its conditions as `stratum_id %in% c(...)` -
## the id add_stratum_metrics() puts on each row. Without that column
## first, check_group_conditions()/catchRaising() error on "object
## 'stratum_id' not found"; with it, everything else here is identical to
## section 1.
catch_data_hier <- add_stratum_metrics(catch_data, dims)

cond_st_h <- check_group_conditions(catch_data_hier, auto_conds$strataCond,
                                    domain = "domainCatchDis",
                                    originType = "WGEstimate",
                                    variableType = "WeightLive",
                                    logFile = NULL, append = TRUE)
cond_md_h <- check_group_conditions(catch_data_hier, auto_conds$matchedDataCond,
                                    conditionType = "matched_data",
                                    domain = "domainCatchDis",
                                    originType = "WGEstimate",
                                    variableType = "WeightLive",
                                    logFile = NULL, append = TRUE)
strata_matrix_h <- do.call(cbind, cond_st_h)
cat(sprintf("hierarchical: %d rows in no group, %d rows in more than one (both should be 0)\n",
           sum(rowSums(strata_matrix_h) == 0), sum(rowSums(strata_matrix_h) > 1)))

raised_hier <- catchRaising(catch_data = catch_data_hier,
                            condition_raising_st_list = auto_conds$strataCond,
                            condition_matched_data_list = auto_conds$matchedDataCond,
                            type = "discards",
                            originType = "WGEstimate",
                            variableType = "WeightLive")
cat(sprintf("hierarchical raising: %d rows (%d imported, %d raised)\n",
           nrow(raised_hier),
           sum(raised_hier$importedOrRaised == "imported"),
           sum(raised_hier$importedOrRaised == "raised")))


## =====================================================================
##  3. OVERVIEW OF THE RAISED WEIGHT - imported vs raised, any aggregation
## =====================================================================
## plot_raised_composition() takes any catch-side column on x and an
## optional facet, same shape as the coverage plots: landings/discards
## are always the fill, imported/raised is always the shading, and
## whatever you put on x or facet is the aggregation. Shown for both
## methods so the same weight can be compared method to method.

raised_overview_table(raised_manual)
raised_overview_table(raised_hier)

grDevices::pdf(tempfile())
## by metier
print(plot_raised_composition(raised_manual, x_var = "metier4"))
print(plot_raised_composition(raised_hier,   x_var = "metier4"))
## by country, faceted by season - a finer aggregation than the group
## definitions themselves ever used
print(plot_raised_composition(raised_manual, x_var = "Country", facet_var = "Season"))
print(plot_raised_composition(raised_hier,   x_var = "Country", facet_var = "Season"))
invisible(dev.off())


## =====================================================================
##  4. DISCARD RATE ACROSS THE RAISING GROUPS - homogeneity / outliers
## =====================================================================
## The question this answers is different from section 3: not how much
## weight moved, but whether the STRATA A GROUP DRAWS ITS RATE FROM agree
## with each other. plot_discard_rate() already draws one point per
## stratum from any table carrying discard_rate; put the raising GROUP on
## x and every point under a group is one of the strata that fed its rate
## - a group with points scattered from 2% to 40% is not a group, it is a
## guess, no matter how well it covers the stock.

## ---- 4a. manual: matchedDataCond_manual filtered against strata0, the
## same per-group filter check_group_conditions() itself applies to
## catch_data - just against the STRATUM table instead of the CEF rows,
## since that is what already carries a discard_rate. Here the groups do
## not overlap, so each stratum lands in exactly one donor_group; a
## matchedDataCond with a shared donor pool across groups would need the
## same filter run once per group and stacked, exactly as below.
donor_long_manual <- bind_rows(lapply(names(matchedDataCond_manual), function(g)
  strata0 %>% filter(!!matchedDataCond_manual[[g]]) %>% mutate(donor_group = g)))

grDevices::pdf(tempfile())
print(plot_discard_rate(donor_long_manual, x = "donor_group", colour = "metier4",
                        jitter = TRUE))
invisible(dev.off())
donor_long_manual %>% group_by(donor_group) %>%
  summarise(n = n(), mean_DR = round(mean(discard_rate, na.rm = TRUE), 3),
           sd_DR = round(sd(discard_rate, na.rm = TRUE), 3),
           min_DR = round(min(discard_rate, na.rm = TRUE), 3),
           max_DR = round(max(discard_rate, na.rm = TRUE), 3),
           .groups = "drop") %>% print()

## ---- 4b. hierarchical: the cascade is a strict partition (one group per
## stratum), so the group IS the donor pool - no filtering needed
grDevices::pdf(tempfile())
print(plot_discard_rate(rec, x = "group_id", colour = "metier4", jitter = TRUE))
invisible(dev.off())
rec %>% group_by(group_id) %>%
  summarise(n = n(), mean_DR = round(mean(discard_rate, na.rm = TRUE), 3),
           sd_DR = round(sd(discard_rate, na.rm = TRUE), 3),
           min_DR = round(min(discard_rate, na.rm = TRUE), 3),
           max_DR = round(max(discard_rate, na.rm = TRUE), 3),
           .groups = "drop") %>% print()


## =====================================================================
##  5. COMPARING SCENARIOS
## =====================================================================
## A scenario is a whole grouping configuration under a name -
## scenario_auto() / scenario_manual() describe one, run_scenario() runs
## it end to end (build the strata, define the groups, apply the raising
## - the light STRATUM-level raising apply_raising() does, not the full
## CEF-row catchRaising() run above, which is what makes running several
## of these cheap enough to compare). compare_scenarios() lines the runs
## up on the same stratum keys; plot_comparison() draws them.
##
## scenario_manual() takes rules in new_rule() shape - the one place this
## script uses new_rule()/assign_rule_groups(), only because that is the
## input this particular comparison function needs, not because the
## raising in sections 1-2 above does. Same three groups as section 1.
manual_rules <- list(
  new_rule("G1", "Beam trawl (TBB)",
          raise = list(list(var = "metier4", values = "TBB", negate = FALSE))),
  new_rule("G2", "Otter trawls (OTB, OTM)",
          raise = list(list(var = "metier4", values = c("OTB", "OTM"),
                            negate = FALSE))),
  new_rule("G3", "Everything else"))

scenarios <- list(
  manual_used   = scenario_manual("Manual (as above)", dims, manual_rules, list()),
  hier_used     = scenario_auto("Hierarchical (as above)", dims, hierarchy,
                                thresholds, self_threshold = self_threshold),
  hier_stricter = scenario_auto("Hierarchical, stricter", dims, hierarchy,
                                thresholds + 15, self_threshold = self_threshold),
  hier_looser   = scenario_auto("Hierarchical, looser", dims, hierarchy,
                                pmax(thresholds - 20, 0), self_threshold = 70))

runs <- lapply(scenarios, run_scenario, catch_data = catch_data)
cmp  <- compare_scenarios(runs, dims)

## the wide table: one row per stratum, one column per scenario's
## estimated discards, so a single stratum's answer can be read straight
## across
ct <- comparison_table(cmp, dims)
print(head(ct[order(-rowSums(ct[grepl("^Imported$|Hierarchical|Manual", names(ct))],
                            na.rm = TRUE)), ], 10))

grDevices::pdf(tempfile())
print(plot_comparison(cmp, x_var = "metier4"))
print(plot_comparison(cmp, x_var = "Country", facet_var = "Season"))
invisible(dev.off())

cat("\nDone - every figure above went to a throwaway pdf() device (there is",
   "\nnothing worth keeping as a file here, unlike plot_walkthrough.R's PNGs -",
   "\nrun a block in an interactive session to actually look at one).\n")

}  # end: if (!is.null(catch_data))
