## ===========================================================
## raising_walkthrough.R
##
## NOT a test - nothing here asserts pass/fail (the actual test suite is
## dev/tests/*.R). This is a template you run by hand to see the discard
## RAISING side of the package end to end: define groups by both methods
## (manual rules, the automatic hierarchical cascade), raise the actual
## catch with each, and look at the result - the raised weight (imported
## vs raised, at whatever aggregation), whether the strata feeding a
## group's rate are homogeneous, and how the two methods (and a couple
## of variants of each) compare.
##
## Data: the same two objects as plot_walkthrough.R - `catch_data` (raw
## CEF catch, already through derive_fields()) and `distribution_data`
## (not actually needed for raising itself, loaded anyway since it comes
## for free and section 6's scenarios reuse the same catch_data). Swap in
## your own for elsewhere; nothing past section 0 needs to change either
## way.
## ===========================================================

library(dplyr)
library(rcef)
# plot_river()/plot_river_level() call geom_text(stat = "stratum") by name,
# which ggplot2 only resolves against the attached search path - qualifying
# every OTHER ggalluvial call inside rcef (geom_alluvium/geom_stratum) is not
# enough for that one. Attach it here, same as the app does.
library(ggalluvial)


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


## The stratum resolution every section below shares. "Own stratum" IS
## this set of columns - a stratum finer than this cannot be raised
## on its own data, because there IS no finer resolution defined.
dims <- c("metier4", "Country", "Season")
strata0 <- build_strata(catch_data, dims)
cat(sprintf("\n%d strata at metier4 + Country + Season, %s t landed\n",
           nrow(strata0), format(round(sum(strata0$landings), 1), big.mark = ",")))


## =====================================================================
##  1. MANUAL GROUPING - new_rule() + assign_rule_groups()
## =====================================================================
## A rule is one group: a `raise` clause (which strata it claims) and a
## `source` clause (which strata may donate to it - defaults to `raise`).
## Rules apply in order, first match wins, so a clause-less last rule is
## the catch-all. Real bll metier4 levels used below: TBB, OTB/OTM (otter
## and otter-twin trawls), everything else falls to the catch-all.

g_tbb <- new_rule("G1", "Beam trawl (TBB)",
                  raise = list(list(var = "metier4", values = "TBB", negate = FALSE)))
g_ott <- new_rule("G2", "Otter trawls (OTB, OTM)",
                  raise = list(list(var = "metier4", values = c("OTB", "OTM"),
                                    negate = FALSE)))
g_all <- new_rule("G3", "Everything else")     # no clause = catch-all
manual_rules <- list(g_tbb, g_ott, g_all)

strata_manual <- assign_rule_groups(strata0, manual_rules)

## ---- 1a. does the partition actually cover every stratum? -----------
prog <- rules_progress(strata0, manual_rules)
print(prog)
miss <- rules_unassigned(strata0, manual_rules, dims = dims)
cat(if (is.null(miss)) "every stratum is covered\n"
   else sprintf("%d strata still uncovered\n", nrow(miss)))

## ---- 1b. the river: which strata raise from which group -------------
## One ribbon per stratum, width = landings, running from the stratum to
## the group raising it - the visual the table above cannot give: how
## LOPSIDED the groups are, at a glance.
grDevices::pdf(tempfile())
print(plot_river(strata_manual, left_var = NULL, right_var = "toBeRaisedGroup",
                 left_label = stratum_label(strata_manual, dims),
                 stratum_axis = stratum_axis_label(dims),
                 title = "Manual grouping: which strata raise from which group"))
invisible(dev.off())

## ---- 1c. actually raise the catch ------------------------------------
## conds_from_rules() turns the rules into the two condition lists
## catchRaising() takes (the complements are made explicit, since
## first-match-wins is an app/script convention, not something the
## engine itself understands); run_native_raising() wraps catchRaising()
## with the column preparation it needs. catchRaising() prints its own
## "Condition diagnostics" banner per condition pair, unconditionally -
## that noise below is the ENGINE's own logging, not this script's.
manual_conds <- conds_from_rules(manual_rules, dims = dims)
raised_manual <- run_native_raising(catch_data, dims, manual_conds,
                                    type = "discards")
cat(sprintf("manual raising: %d rows (%d imported, %d raised)\n",
           nrow(raised_manual),
           sum(raised_manual$importedOrRaised == "imported"),
           sum(raised_manual$importedOrRaised == "raised")))


## =====================================================================
##  2. HIERARCHICAL GROUPING - recommend_raising()
## =====================================================================
## A cascade of pools, coarsening from "own stratum" outward: a stratum
## with enough of its OWN sampling (self_threshold) is not pooled at
## all; otherwise it is pooled with strata sharing less and less of the
## hierarchy, in the order given (the first key listed is the first one
## POOLED OVER, i.e. dropped soonest), each level with its own coverage
## threshold. Read top to bottom, widen top to bottom.
hierarchy <- c("Country", "metier4", "Season")
thresholds <- c(70, 60, 40)          # one per hierarchy level
self_threshold <- 90                 # own-stratum bar, before any pooling

rec <- recommend_raising(strata0, hierarchy, thresholds,
                         self_threshold = self_threshold, dims = dims)

## ---- 2a. the cascade, step by step -----------------------------------
tree <- decision_tree(rec, hierarchy, thresholds, self_threshold,
                      dims = dims, cumulative = TRUE)
print(tree)
chk <- check_partition(rec, tree)
cat(sprintf("partition check: %s strata / %s landings accounted for%s\n",
           if (chk$n_ok) "all" else sprintf("%d of %d", chk$n_steps, chk$n_total),
           if (chk$l_ok) "all" else sprintf("%s of %s t", chk$l_steps, chk$l_total),
           if (chk$unassigned > 0)
             sprintf(" (%d unassigned)", chk$unassigned) else ""))

## ---- 2b. the same river, this time for the cascade -------------------
grp_lookup <- raising_groups(rec)
rec$group_id <- grp_lookup$group_id[match(rec$raise_condition,
                                          grp_lookup$raise_condition)]
grDevices::pdf(tempfile())
print(plot_river(rec, left_var = NULL, right_var = "group_id",
                 left_label = stratum_label(rec, dims),
                 right_label = rule_label(rec),
                 stratum_axis = stratum_axis_label(dims),
                 title = "Hierarchical cascade: which strata raise from which group"))
invisible(dev.off())

## ---- 2c. raise the catch with the cascade ----------------------------
auto_conds <- conds_from_auto(rec)
raised_hier <- run_native_raising(catch_data, dims, auto_conds,
                                  type = "discards")
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
## stratum from any table carrying discard_rate; put the raising GROUP
## on x and every point under a group is one of the strata that fed its
## rate - a group with points scattered from 2% to 40% is not a group,
## it is a guess, no matter how well it covers the stock.

## ---- 4a. manual: unnest usedToRaiseGroup - a stratum can donate to
## more than one group, so it appears once per group it feeds
donor_long <- strata_manual %>%
  tidyr::unnest(usedToRaiseGroup) %>%
  rename(donor_group = usedToRaiseGroup)
grDevices::pdf(tempfile())
print(plot_discard_rate(donor_long, x = "donor_group", colour = "metier4",
                        jitter = TRUE))
invisible(dev.off())
donor_long %>% group_by(donor_group) %>%
  summarise(n = n(), mean_DR = round(mean(discard_rate, na.rm = TRUE), 3),
           sd_DR = round(sd(discard_rate, na.rm = TRUE), 3),
           min_DR = round(min(discard_rate, na.rm = TRUE), 3),
           max_DR = round(max(discard_rate, na.rm = TRUE), 3),
           .groups = "drop") %>% print()

## ---- 4b. hierarchical: the cascade is a strict partition (one group
## per stratum), so the group IS the donor pool - no unnesting needed
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
## it end to end (build the strata, define the groups, apply the
## raising - the light STRATUM-level raising apply_raising() does, not
## the full CEF-row catchRaising() run above, which is what makes running
## several of these cheap enough to compare). compare_scenarios() lines
## the runs up on the same stratum keys; plot_comparison() draws them.

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
