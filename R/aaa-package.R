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
#' is sampled, defining the groups that raise or allocate, running the
#' RDBESStockCoord engines, and writing the result out as a reproducible
#' script or an ICES TAF script set. A shiny application, started with
#' [run_app()], drives the same functions.
#'
#' @details
#' ## Where to start
#'
#' | To ... | Use |
#' |:-------|:----|
#' | read InterCatch files into CEF | [funICoutCEF()] |
#' | add the strata-ready columns (`Country`, `Season`, `metier4`, ...) | [derive_fields()] |
#' | see discard rates and sampling coverage per stratum, as a table | [build_coverage_strata()] |
#' | look at them | [coverage-plots] (heatmap, bubbles, bars), [plot_coverage_map()], [plot_discard_rate()] |
#' | look at numbers and mean weight at length or age | [plot_NaLoA()], [plot_MWaLoA()] |
#' | build the raising-side stratum table and judge it | [build_strata()], [classify_strata()] |
#' | define raising groups automatically (a cascade that coarsens until coverage clears a bar) | [recommend_raising()], [conds_from_auto()] |
#' | define them by hand | [new_rule()], [conds_from_rules()] |
#' | check a group definition before raising | [check_group_conditions()] |
#' | compare grouping schemes side by side | [scenario_auto()], [run_scenario()], [compare_scenarios()] |
#' | write the session out as an ICES TAF script set | [taf_bundle()] |
#' | use all of this through the application | [run_app()] |
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
#'   -> plot_coverage_heat() / _bubble() / _bar() / _map() | plot_discard_rate()
#' ```
#'
#' [build_coverage_strata()] is the discard-rate / sampling-coverage table
#' (CAR / CAS-style columns, no status) the plots draw; they take raw catch
#' or that table, so the same numbers can be read as a table and looked at
#' as a figure. Raising and allocation then run:
#'
#' ```
#' derive_fields() -> build_strata() -> classify_strata()
#'   -> define_raising_groups_auto() | define_raising_groups_manual()
#'   -> run_native_raising()   (discards; wraps catchRaising())
#'   -> catchAllocationAoL()   (lengths and ages)
#' ```
#'
#' Both grouping methods return the same `strataCond` / `matchedDataCond`
#' shape, which is what the two RDBESStockCoord engines consume, so one group
#' definition drives either. The group conditions are written against stratum
#' columns (`stratum_id`, `coverage`, `discard_rate`, ...) that raw catch rows
#' do not carry; `run_native_raising()` adds them before it calls
#' `catchRaising()`, so call the engine directly only on a table that already
#' has them.
#'
#' ## How the code is laid out
#'
#' The package holds three bodies of code, distinguished by file name so the
#' split is visible in git:
#'
#' * `R/RDBESStockCoord_*.R`: vendored from the ICES RDBESStockCoord working
#'   group code. Algorithms, argument lists and return shapes are upstream's.
#'   Local edits are packaging-only and are marked `## [pkg]`. Do not
#'   refactor these; fix upstream and re-vendor.
#' * `R/rcef_*.R`: written here - strata, classification, coverage, grouping,
#'   scenarios, script emission. Free to refactor.
#' * `R/viz_*.R`: written here - the theme and the plots. Pure ggplot2, no
#'   shiny; separated so they can be dropped or split into their own package
#'   later without touching the analysis code.
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
NULL

## NSE column names, so R CMD check does not report them as undefined
## globals. Regenerate with codetools::checkUsagePackage() after changes.
utils::globalVariables(c(
  ".", ".data", "SpeciesName", "StockCode", "ICESArea", "Species",
  "recordType", "vesselFlagCountry", "year", "workingGroup", "stock",
  "catchCategory", "WeightLive", "Number", "dataType", "distributionType",
  "metier4", "metier5", "metier6", "fleetValue", "seasonValue",
  "ICESSubArea", "ICESDivision", "ICESSubdivision", "sampledOrEstimated",
  "importedOrRaised", "originType", "variableType", "groupName",
  # build_coverage_strata() / coverage_cells() column names
  "total", "weightingTotal", "landings", "numTrips", "coverage",
  "numMeasurements", "covered", ".covered", "rate"
))
