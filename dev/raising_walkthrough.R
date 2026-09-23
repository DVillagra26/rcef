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
##
## Every figure below is drawn with print() and nothing else - no pdf()/
## png()/jpeg() device, no dev.off(). Run it in an interactive session
## (RStudio, or R itself) and each print() lands in the live graphics
## window / Plots pane, one after another, to actually look at.
## ===========================================================

library(dplyr)
library(rcef)
# Section 2a's cascade plot is a one-off ggplot2 call (no rcef plot
# function takes a decision_tree() table), so its geoms/scales need
# ggplot2 attached - the same reason the RDBESStockCoord example attaches
# it.
library(ggplot2)


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


## The stratum resolution the hierarchical method and the homogeneity
## check (section 4) share. "Own stratum" IS this set of columns - a
## stratum finer than this cannot be raised on its own data, because
## there IS no finer resolution defined.
## The SAME columns, in the SAME order, as section 2's hierarchy below -
## not by accident: stratum_id (added to catch_data just below, read
## again inside recommend_raising() every time it builds its own strata)
## is `paste(dims values, sep = " | ")` - order-sensitive. dims and
## hierarchy naming the same columns in two different orders (or dims
## missing one hierarchy has) would build two different stratum_id
## strings for the same stratum, and every stratum_id %in% c(...)
## condition downstream would silently match nothing - keep this in
## sync with `hierarchy` (section 2) whenever either one changes.
dims <- c("Country", "Season", "mesh", "metier4")
strata0 <- build_strata(catch_data, dims)
cat(sprintf("\n%d strata at %s, %s t landed\n",
           nrow(strata0), paste(dims, collapse = " + "),
           format(round(sum(strata0$landings), 1), big.mark = ",")))
rm(strata0)  # read once, right here, for the line above - nothing below
             # reads it; recommend_raising() builds its own from catch_data

## stratum_id/coverage/discard_rate/n_measurements/numTrips added onto
## catch_data ITSELF, once, here - not a second "catch_data_hier" table
## kept alongside it. Only the hierarchical method (section 2 on) reads
## them (its conditions are stratum_id %in% c(...), written against
## whichever stratum a row belongs to), but they are harmless extra
## columns for section 1's plain gear/catchCategory conditions, so one
## catch_data serves both without a second name to keep track of.
catch_data <- add_stratum_metrics(catch_data, dims)
## dims itself stays in scope for section 5 (the shared key several
## scenarios' results get compared on) and for that add_stratum_metrics()
## call above - but not for recommend_raising()/decision_tree() below:
## they default it to hierarchy, the same three columns here, so passing
## it again would just be saying the same thing twice.


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
##  2. HIERARCHICAL GROUPING - recommend_raising()
## =====================================================================
## RDBESStockCoord has no notion of "cascade fine to coarse until
## coverage clears a bar" - that is rcef's own recommend_raising(), a
## cascade of pools coarsening from the stratum's own data outward:
## enough of its OWN sampling and it is not pooled at all; otherwise it
## is pooled with strata sharing less and less of the hierarchy, in the
## order given, each level with its own coverage threshold.
## conds_from_auto() is the minimal glue turning that recommendation
## into the same strataCond/matchedDataCond shape as section 1 - the
## raising call itself is, again, catchRaising() directly.
##
## thresholds is the own-stratum bar FIRST, then one per hierarchy
## level - length(hierarchy) + 1 values, not length(hierarchy); a single
## value (as here) recycles to that length, so all four read 50%.
hierarchy <- c("Country", "Season", "mesh", "metier4")
thresholds <- 50

## catch_data straight in - recommend_raising() builds its own strata
## (build_strata(catch_data, dims), dims defaulting to hierarchy here -
## the same columns `dims` above already used to stamp catch_data's own
## stratum_id) rather than being handed one pre-built. Pass an
## already-built stratum table instead (as section 5's scenarios do, at
## a dims wider than their own hierarchy) and it is used as is.
rec <- recommend_raising(catch_data, hierarchy, thresholds)

## ---- 2a. the cascade, step by step -------------------------------------
## The first row used to just say "own stratum" - true, but not
## checkable against the data the way every other row's "metier4 +
## Season" / "Season" already is. It now reads the same way: every dims
## column at once, nothing pooled - so the whole table is one
## consistent progression from all three kept to none.
tree <- decision_tree(rec, hierarchy, thresholds, cumulative = TRUE)
print(tree)
chk <- check_partition(rec, tree)
cat(sprintf("partition check: %s strata / %s landings accounted for%s\n",
           if (chk$n_ok) "all" else sprintf("%d of %d", chk$n_steps, chk$n_total),
           if (chk$l_ok) "all" else sprintf("%s of %s t", chk$l_steps, chk$l_total),
           if (chk$unassigned > 0)
             sprintf(" (%d unassigned)", chk$unassigned) else ""))

## How much of the landings each step actually settles - the same
## `tree` above, drawn instead of read: bars are the share FIRST solved
## AT that step (pct_landings), the line is the running total once every
## step up to and including it has applied (cum_pct_landings). Both
## columns are already there from decision_tree(..., cumulative = TRUE);
## nothing new is computed here, only plotted. No rcef plot function
## takes a decision_tree() table (its shape is one row per CASCADE STEP,
## not one row per stratum), so this is a small one-off ggplot rather
## than a reused function - styled with app_theme() to match the rest.
tree$step <- factor(tree$step, levels = tree$step)   # cascade order, not alphabetical
p_tree <- ggplot(tree, aes(x = step)) +
  geom_col(aes(y = pct_landings), fill = "#4292C6", width = 0.6) +
  geom_line(aes(y = cum_pct_landings, group = 1), colour = "#D7191C", linewidth = 0.8) +
  geom_point(aes(y = cum_pct_landings), colour = "#D7191C", size = 2) +
  geom_text(aes(y = cum_pct_landings, label = paste0(cum_pct_landings, "%")),
            vjust = -0.6, size = 3, colour = "#D7191C") +
  scale_x_discrete(labels = tree$pool) +
  scale_y_continuous(limits = c(0, 105), expand = c(0, 0)) +
  labs(title = "Landings resolved at each step of the hierarchical cascade",
       subtitle = "bars: share first solved at that step - line: running total solved so far",
       x = NULL, y = "% of total landings") +
  app_theme() +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))
print(p_tree)

auto_conds <- conds_from_auto(rec)
## group_id per stratum, for the homogeneity check below - the same
## lookup conds_from_auto() itself already built (auto_conds$groups), no
## need to call raising_groups() a second time.
rec$group_id <- auto_conds$groups$group_id[match(rec$raise_condition,
                                                  auto_conds$groups$raise_condition)]
print(auto_conds$groups)

## ---- 2b. condition diagnostics, and the raise itself --------------------
## conds_from_auto() writes its conditions as `stratum_id %in% c(...)` -
## the id already on catch_data since the add_stratum_metrics() call
## above section 1. Everything else here is identical to section 1.
cond_st_h <- check_group_conditions(catch_data, auto_conds$strataCond,
                                    domain = "domainCatchDis",
                                    originType = "WGEstimate",
                                    variableType = "WeightLive",
                                    logFile = NULL, append = TRUE)
cond_md_h <- check_group_conditions(catch_data, auto_conds$matchedDataCond,
                                    conditionType = "matched_data",
                                    domain = "domainCatchDis",
                                    originType = "WGEstimate",
                                    variableType = "WeightLive",
                                    logFile = NULL, append = TRUE)
strata_matrix_h <- do.call(cbind, cond_st_h)
cat(sprintf("hierarchical: %d rows in no group, %d rows in more than one (both should be 0)\n",
           sum(rowSums(strata_matrix_h) == 0), sum(rowSums(strata_matrix_h) > 1)))

raised_hier <- catchRaising(catch_data = catch_data,
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
##  2c. HIERARCHICAL METHOD, DONOR DISCARD RATE CAPPED - CATCH-ALL ONLY
## =====================================================================
## Same cascade mechanics as above (recommend_raising() ->
## conds_from_auto()), with one addition: a stratum whose own discard
## rate exceeds 70% may not DONATE to the catch-all/fallback pool - the
## widest, least targeted group, the one every stratum lands in only
## because nothing more specific worked for it, where a single outlier
## stratum can otherwise drag the whole pool's rate with it. Every group
## resolved at a more specific level (its own stratum, or any of the
## metier4/Season pools above) is untouched: an OTB-only pool still
## draws on every OTB discard rate there is, 70% or not - only a
## stratum that clears NONE of those and falls all the way to the
## catch-all has its donor pool capped once it gets there.
##
## Same 50% policy as section 2 throughout, own-stratum bar included -
## on THIS data (see section 2's own tree) the catch-all still picks up
## 119 strata that clear none of Country+Season+mesh+metier4 down to
## metier4 alone, so there is something real to cap below. If a
## hierarchy/threshold combination ever DOES resolve everything before
## the fallback step, fb_gid comes back empty and the block below skips
## the capping cleanly - a real, valid outcome the code has to handle,
## not a reason to special-case the thresholds just to force a demo.
thresholds_ex <- 50

rec_ex <- recommend_raising(catch_data, hierarchy, thresholds_ex)
tree_ex <- decision_tree(rec_ex, hierarchy, thresholds_ex, cumulative = TRUE)
print(tree_ex)

auto_conds_ex <- conds_from_auto(rec_ex)

## The catch-all is always the group whose raise_condition is the
## literal string "TRUE" (recommend_raising()'s own convention for the
## fallback step). At thresholds where nothing falls that far, it is
## simply not one of the group_ids below - and nothing gets capped,
## correctly, since there is nothing to cap.
fb_gid <- auto_conds_ex$groups$group_id[auto_conds_ex$groups$raise_condition == "TRUE"]

if (length(fb_gid)) {
  old_cond <- auto_conds_ex$matchedDataCond[[fb_gid]]
  new_cond <- quo(!!old_cond & (is.na(discard_rate) | discard_rate <= 0.7))

  ## What the cap actually changes: the catch-all's own aggregate donor
  ## ratio, before and after - the same filter-and-sum idiom section 4a
  ## already uses on catch_data, read straight off the two conditions
  ## rather than picked apart from catchRaising()'s output afterwards.
  fb_ratio <- function(cond) {
    d <- catch_data %>%
      filter(!!cond, !is.na(domainCatchDis), originType == "WGEstimate",
             variableType == "WeightLive")
    lan <- sum(d$total[d$catchCategory == "Lan"], na.rm = TRUE)
    dis <- sum(d$total[d$catchCategory == "Dis"], na.rm = TRUE)
    data.frame(n_domains = n_distinct(d$domainCatchDis),
              landings_t = round(lan / 1000, 2), discards_t = round(dis / 1000, 2),
              discard_rate = round(dis / (lan + dis), 3))
  }
  cat(sprintf("catch-all group is %s (%d strata)\n", fb_gid,
             auto_conds_ex$groups$n_strata[auto_conds_ex$groups$group_id == fb_gid]))
  cat("catch-all donor ratio, uncapped:\n");            print(fb_ratio(old_cond))
  cat("catch-all donor ratio, capped at DR <= 0.7:\n"); print(fb_ratio(new_cond))

  auto_conds_ex$matchedDataCond[[fb_gid]] <- new_cond
} else {
  cat("no stratum reached the catch-all at these thresholds - nothing to cap\n")
}

## check_group_conditions() will now WARN that the excluded high-DR
## strata's own discard estimates are "not matched to any raising
## stratum" - that is not a bug surfacing, it is the point: those
## strata's own data no longer informs anyone's rate, not even their
## own group's.
cond_st_ex <- check_group_conditions(catch_data, auto_conds_ex$strataCond,
                                     domain = "domainCatchDis",
                                     originType = "WGEstimate",
                                     variableType = "WeightLive",
                                     logFile = NULL, append = TRUE)
cond_md_ex <- check_group_conditions(catch_data, auto_conds_ex$matchedDataCond,
                                     conditionType = "matched_data",
                                     domain = "domainCatchDis",
                                     originType = "WGEstimate",
                                     variableType = "WeightLive",
                                     logFile = NULL, append = TRUE)
strata_matrix_ex <- do.call(cbind, cond_st_ex)
cat(sprintf("hierarchical, DR-capped catch-all: %d rows in no group, %d rows in more than one (both should be 0)\n",
           sum(rowSums(strata_matrix_ex) == 0), sum(rowSums(strata_matrix_ex) > 1)))

raised_hier_ex <- catchRaising(catch_data = catch_data,
                               condition_raising_st_list = auto_conds_ex$strataCond,
                               condition_matched_data_list = auto_conds_ex$matchedDataCond,
                               type = "discards",
                               originType = "WGEstimate",
                               variableType = "WeightLive")
cat(sprintf("hierarchical, DR-capped catch-all: %d rows (%d imported, %d raised)\n",
           nrow(raised_hier_ex),
           sum(raised_hier_ex$importedOrRaised == "imported"),
           sum(raised_hier_ex$importedOrRaised == "raised")))


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

## by metier
print(plot_raised_composition(raised_manual, x_var = "metier4"))
print(plot_raised_composition(raised_hier, x_var = "metier4"))
## by country, faceted by season - a finer aggregation than the group
## definitions themselves ever used
print(plot_raised_composition(raised_manual, x_var = "Country", facet_var = "Season"))
print(plot_raised_composition(raised_hier, x_var = "Country", facet_var = "Season"))


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

## ---- 4a. manual: matchedDataCond_manual filtered straight against
## catch_data, at the domainCatchDis granularity catchRaising() itself
## estimates a ratio from - the same check the source example runs
## before raising ("Per-stratum spread behind each group average"), fed
## into plot_discard_rate() instead of a hand-rolled table. A stratum
## table at metier4+Country+Season cannot stand in here: catchCategory
## and fleetValue (both load-bearing above, G4's exclusions especially)
## do not survive that aggregation. discard_rate is discards / (landings
## + discards) - rcef's own convention (what build_strata() computes,
## and what plot_discard_rate()'s percent axis expects), not the source
## example's unbounded discards/landings dis_ratio.
donor_detail_manual <- bind_rows(lapply(names(matchedDataCond_manual), function(g)
  catch_data %>%
    filter(!!matchedDataCond_manual[[g]],
           !is.na(domainCatchDis),
           originType == "WGEstimate",
           variableType == "WeightLive") %>%
    group_by(donor_group = g, domainCatchDis, gear) %>%
    summarise(landings = sum(total[catchCategory == "Lan"], na.rm = TRUE),
             discards = sum(total[catchCategory == "Dis"], na.rm = TRUE),
             discard_rate = discards / (landings + discards),
             .groups = "drop")))

print(plot_discard_rate(donor_detail_manual, x = "donor_group", colour = "gear",
                        jitter = TRUE))
donor_detail_manual %>% group_by(donor_group) %>%
  summarise(n = n(),
           mean_DR = round(weighted.mean(discard_rate, w = landings, na.rm = TRUE), 3),
           sd_DR = round(sd(discard_rate, na.rm = TRUE), 3),
           min_DR = round(min(discard_rate, na.rm = TRUE), 3),
           max_DR = round(max(discard_rate, na.rm = TRUE), 3),
           .groups = "drop") %>% print()

## ---- 4b. hierarchical: the cascade is a strict partition (one group per
## stratum), so the group IS the donor pool - no filtering needed
print(plot_discard_rate(rec, x = "group_id", colour = "metier4", jitter = TRUE))
rec %>% group_by(group_id) %>%
  summarise(n = n(), mean_DR = round(mean(discard_rate, na.rm = TRUE), 3),
           sd_DR = round(sd(discard_rate, na.rm = TRUE), 3),
           min_DR = round(min(discard_rate, na.rm = TRUE), 3),
           max_DR = round(max(discard_rate, na.rm = TRUE), 3),
           .groups = "drop") %>% print()


## =====================================================================
##  5. COMPARING SCENARIOS - what the hierarchy's SHAPE changes
## =====================================================================
## A scenario is a whole grouping configuration under a name -
## scenario_auto() / scenario_manual() describe one, run_scenario() runs
## it end to end (build the strata, define the groups, apply the raising
## - the light STRATUM-level raising apply_raising() does, not the full
## CEF-row catchRaising() run above, which is what makes running several
## of these cheap enough to compare). compare_scenarios() lines the runs
## up on the same stratum keys; plot_comparison() draws them.
##
## Threshold policy is held FIXED (50% at every level, section 2's own
## self_threshold too) and only the hierarchy itself varies - which keys
## get pooled over, in what order. "Nested" mode (recommend_raising()'s
## default) pools the FIRST key listed over soonest and keeps the LAST
## one longest - c("Country", "metier4") means "join countries first,
## then metier4 alone"; c("Country", "areaValue", "metier4") means "join
## countries, then areas, then metier4 alone". Every variant below keeps
## metier4 as the last, most-protected key except the first, which
## considers it alone from the start.
##
## scenario_auto()'s own `dims` can be WIDER than the shared comparison
## `dims` below (the Country > area > metier4 variant needs areaValue in
## its OWN strata) but never narrower: compare_scenarios() re-aggregates
## every run onto the fixed dims keys (section 0's Country+Season+mesh+
## metier4), so a run missing any one of those four on its own output
## would fail right there.
##
## scenario_manual() takes rules in new_rule() shape - the one place this
## script uses new_rule()/assign_rule_groups(), only because that is the
## input this particular comparison function needs, not because the
## raising in sections 1-2 above does. It runs at build_strata()'s dims
## resolution, where catchCategory has already been summed away into
## landings/discards and fleetValue no longer exists - so section 1's
## BMS/IBC groups and G4's hand-picked exclusion have no equivalent
## here. This is the closest structural analogue at that resolution:
## the same G1-G3 gear split, with a plain catch-all in place of G4-G6
## (new_rule() with no clause claims whatever is left).
manual_rules <- list(
  new_rule("G1", "TBB",
          raise = list(list(var = "metier4", values = "TBB", negate = FALSE))),
  new_rule("G2", "GTR / GNS",
          raise = list(list(var = "metier4", values = c("GTR", "GNS"),
                            negate = FALSE))),
  new_rule("G3", "OTB / SSC / SDN",
          raise = list(list(var = "metier4", values = c("OTB", "SSC", "SDN"),
                            negate = FALSE))),
  new_rule("G4", "Rest of the gears"))

dims_area <- c(dims, "areaValue")    # only the Country > area > metier4 variant needs it

scenarios <- list(
  manual_used = scenario_manual("Manual (gear split)", dims, manual_rules, list()),
  hier_used   = scenario_auto("Country > Season > mesh > metier4 (section 2's cascade)",
                              dims, hierarchy, thresholds,
                              self_threshold = thresholds[1]),
  hier_metier_only            = scenario_auto("metier4 only", dims,
                                              "metier4", 50, self_threshold = 50),
  hier_country_metier         = scenario_auto("Country > metier4", dims,
                                              c("Country", "metier4"), c(50, 50),
                                              self_threshold = 50),
  hier_country_season_metier  = scenario_auto("Country > Season > metier4", dims,
                                              c("Country", "Season", "metier4"),
                                              c(50, 50, 50), self_threshold = 50),
  hier_country_area_metier    = scenario_auto("Country > area > metier4", dims_area,
                                              c("Country", "areaValue", "metier4"),
                                              c(50, 50, 50), self_threshold = 50))

runs <- lapply(scenarios, run_scenario, catch_data = catch_data)
cmp  <- compare_scenarios(runs, dims)

## the wide table: one row per stratum, one column per scenario's
## estimated discards, so a single stratum's answer can be read straight
## across - sorted by every non-key column (Imported + one per scenario,
## named after `scenarios`'s own list names) summed, biggest strata first
ct <- comparison_table(cmp, dims)
val_cols <- setdiff(names(ct), dims)
print(head(ct[order(-rowSums(ct[val_cols], na.rm = TRUE)), ], 10))

print(plot_comparison(cmp, x_var = "metier4"))
print(plot_comparison(cmp, x_var = "Country", facet_var = "metier4"))


## =====================================================================
##  6. FINAL OUTPUT - one table, raised catch above, donor rates below
## =====================================================================
## Every imported/raised CEF row in one table, tagged with the GROUP
## that raised it (blank for imported rows - nothing raised them);
## underneath, one row per group showing the rate it actually applied
## and what that rate was computed from. catchRaising() does not tag its
## own output with the group that raised a row (its DrGroup is commented
## out in the vendored engine - not something this script turns on
## without touching RDBESStockCoord_raising.R directly), so the tag is
## added here, the same way section 4 finds a stratum's group: by its
## stratum_id, against rec's own group_id. Built off the hierarchical
## run (raised_hier / rec) since it is the richer of the two - 52 real
## donor groups here, against manual's 6.

## ---- 6a. upper table: every row, group-tagged --------------------------
raised_hier$stratum_id <- stratum_keys(raised_hier, dims)
final_raised <- raised_hier %>%
  mutate(group_id = if_else(importedOrRaised == "raised",
                            rec$group_id[match(stratum_id, rec$stratum_id)],
                            NA_character_)) %>%
  select(importedOrRaised, group_id, catchCategory, metier4, Country, Season,
         fleetValue, areaValue, domainCatchDis, total, everything())

cat(sprintf("%d rows: %d imported, %d raised across %d donor groups\n",
           nrow(final_raised),
           sum(final_raised$importedOrRaised == "imported"),
           sum(final_raised$importedOrRaised == "raised"),
           n_distinct(final_raised$group_id, na.rm = TRUE)))
print(head(final_raised %>%
             arrange(desc(importedOrRaised == "raised"), group_id), 15))

## ---- 6b. lower table: what raised each group, and from what -----------
## rate is discards / (landings + discards) over the group's OWN donor
## strata - the same quantity section 4b's mean_DR already is, at the
## group level instead of per donor stratum, shown here alongside the
## weight it was computed from.
final_donors <- rec %>%
  group_by(group_id) %>%
  summarise(n_donor_strata = n(),
           landings_t = round(sum(landings, na.rm = TRUE) / 1000, 2),
           discards_t = round(sum(discards, na.rm = TRUE) / 1000, 2),
           rate = round(sum(discards, na.rm = TRUE) /
                         (sum(landings, na.rm = TRUE) + sum(discards, na.rm = TRUE)), 4),
           .groups = "drop") %>%
  arrange(desc(landings_t))
print(final_donors)

## every raised row's group must appear in the donor table below it -
## the two halves of this output describing the same 52 groups, not two
## different countings
ok_linked <- setequal(na.omit(unique(final_raised$group_id)), final_donors$group_id)
cat(sprintf("\nevery raised row's group_id appears in the donor table below it: %s\n",
           ok_linked))


cat("\nDone - every figure above was print()'d to the active graphics\n",
   "device as it was drawn; nothing was saved to a file.\n")



