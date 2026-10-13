# =====================================================================
#  aaa-package.R
#
#  ORIGIN: this project. Package-level documentation and imports.
#  Named aaa- so it sorts first; nothing here depends on load order,
#  but it is the first file a reader should open.
#
#  tidyr::replace_na is imported for R/RDBESStockCoord_allocation.R,
#  which calls it inside across(~ replace_na(.x, 0)). A call inside a
#  formula is invisible to codetools, and the shiny app attaches tidyr
#  so it hid the gap: headless, catchAtAoLAggregation() failed with
#  'could not find function "replace_na"'.
# =====================================================================

#' rcef: discard raising and length/age catch allocation over CEF data
#'
#' @description
#' Implements the ICES discard-raising and length / age catch-allocation
#' workflows over CEF (RDBES) long-format data: exploring how well the data
#' is sampled, defining the groups that raise or allocate, and running the
#' RDBESStockCoord engines on them.
#'
#' @details
#' ## Where to start
#'
#' | To ... | Use |
#' |:-------|:----|
#' | read InterCatch files into CEF | [funICoutCEF()], [funIntercatchCEF()], and [funMakeRelation()] for the stock relation they need |
#' | add the strata-ready columns (`metier4`, `mesh`, ICES area levels, ...), to the catch or to its distributions | [derive_fields()] |
#' | see discard rates and sampling coverage per stratum, as a table | [build_coverage_strata()] |
#' | look at them | [coverage-plots] (heatmap, bubbles, bars), [plot_coverage_map()], [plot_DR()] |
#' | look at numbers and mean weight at length or age | [plot_NaLoA()], [plot_MWaLoA()] |
#' | find strata with a similar discard rate | [build_discard_clusters()], [plot_discard_clusters()], [plot_discard_cluster_variables()] |
#' | define raising groups automatically (a cascade that coarsens until coverage clears a bar) | [define_raising_groups_auto()] |
#' | define them by hand | [define_raising_groups_manual()] |
#' | check a group definition before raising or allocating | [check_group_conditions()] |
#' | raise the discards | [catchRaising()] |
#' | look at a raised catch, imported against raised | [plot_raised_catches()], [plot_raised_DR()], [plot_river()] |
#' | compare two or more raised catches, as a table or a figure | [raised_overview_table()], [plot_compare_raised()] |
#' | give every stratum a length or age distribution | [catchAllocationAoL()] |
#' | add the allocation up into catch-at-length / -age and mean-weight tables | [catchAtAoLAggregation()], [meanWoLatAoLAggregation()] |
#' | look at an allocation, sampled against estimated, at length or age | [plot_allocated_NaLoA()], [plot_allocated_MWaLoA()] |
#' | save any of the plots to a file | the `save` argument they all take, see [saving-plots] |
#'
#' The package exports more functions than these. The others are plumbing
#' between the ones above and have no help page.
#'
#' Four vignettes walk it through on real data, in this order:
#' `"part1-explore-sampling-coverage"` (every exploratory plot),
#' `"part2a-discard-raising-manual-groups"` and
#' `"part2b-discard-raising-hierarchical-groups"` (group allocation and
#' discard raising, and the CC, CAR and CAS tables) and
#' `"part3-compare-allocation-schemes"`.
#'
#' ## The chain
#'
#' Exploring the data first is its own short chain:
#'
#' ```
#' derive_fields() -> build_coverage_strata()
#'   -> plot_coverage_heat() / _bubble() / _bar() / _map() | plot_DR()
#' ```
#'
#' [build_coverage_strata()] is the discard-rate / sampling-coverage table
#' (CAR / CAS-style columns, no status) the plots draw; they take raw catch
#' or that table, so the same numbers can be read as a table and looked at
#' as a figure. Raising and allocation then run:
#'
#' ```
#' derive_fields() -> define_raising_groups_auto() | define_raising_groups_manual()
#'   -> catchRaising()          (discards)
#'   -> catchAllocationAoL()    (lengths and ages)
#'   -> catchAtAoLAggregation() / meanWoLatAoLAggregation()
#' ```
#'
#' Both grouping methods return the same `strataCond` / `matchedDataCond`
#' shape, which is what the two RDBESStockCoord engines consume, so one group
#' definition drives either. Groups written by hand against the columns of the
#' catch (`metier4 == "TBB"`, say) go to the engines as they are. The groups
#' the two `define_raising_groups_*()` functions return select strata by
#' `stratum_id` and may test `coverage` or `discard_rate`: columns of a
#' stratum, which raw catch rows do not carry. The catch has to carry them
#' before an engine evaluates the conditions row by row, and
#' `run_native_raising(catch, dims, groups)` adds them and then calls
#' `catchRaising()`.
#'
#' ## How the code is laid out
#'
#' The package holds three bodies of code, distinguished by file name so the
#' split is visible in git:
#'
#' * `R/RDBESStockCoord_*.R`: vendored from the ICES RDBESStockCoord working
#'   group code. Algorithms, argument lists and return shapes are upstream's.
#'   Local edits are packaging, plus one - `distributionType` takes CEF's own
#'   `"LengthTotal"` where upstream says `"Length"` - and are marked
#'   `## [pkg]`. The help pages of [catchRaising()], [catchAllocationAoL()]
#'   and the two aggregation functions are written here. Do not refactor
#'   these files; fix upstream and re-vendor.
#' * `R/rcef_*.R`: written here - strata, classification, coverage, grouping,
#'   summaries. Free to refactor.
#' * `R/viz_*.R`: written here - the theme and the plots. Pure ggplot2;
#'   separated so they can be dropped or split into their own package later
#'   without touching the analysis code.
#'
#' @name rcef-package
#' @aliases rcef
#' @import dplyr
#' @import ggplot2
#' @import rlang
#' @importFrom data.table setDT rbindlist dcast melt %like%
#' @importFrom tidyr pivot_longer pivot_wider replace_na
#' @importFrom stringr str_extract
#' @keywords internal
#' @md
"_PACKAGE"

#' Functions vendored from RDBESStockCoord
#'
#' Everything in \code{R/RDBESStockCoord_*.R}. Kept under one
#' \code{@@family} tag so \code{?catchRaising} links to its siblings and a
#' reader can see at a glance which functions are not ours to change.
#'
#' @name RDBESStockCoord
#' @keywords internal
#' @noRd
NULL

## NSE column names, so R CMD check does not report them as undefined
## globals. Regenerate with codetools::checkUsagePackage() after changes.
utils::globalVariables(c(
  ".", ".data", "SpeciesName", "StockCode", "ICESArea", "Species",
  "recordType", "vesselFlagCountry", "year", "workingGroup", "stock",
  "catchCategory", "WeightLive", "Number", "dataType", "distributionType",
  "metier4", "metier5", "metier6", "mesh", "fleetValue", "seasonValue",
  "ICESSubArea", "ICESDivision", "ICESSubdivision", "sampledOrEstimated",
  "importedOrRaised", "originType", "variableType", "groupName",
  # build_coverage_strata() / coverage_cells() column names
  "total", "weightingTotal", "landings", "numTrips", "coverage",
  "numMeasurements", "covered", ".covered", "rate"
))
