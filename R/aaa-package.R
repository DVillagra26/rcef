# =====================================================================
#  aaa-package.R
#
#  ORIGIN: this project. Package-level documentation and imports.
#  Named aaa- so it sorts first; nothing here depends on load order,
#  but it is the first file a reader should open.
# =====================================================================

#' rcef: discard raising and length/age catch allocation over CEF data
#'
#' The package holds two clearly separated bodies of code, distinguished
#' by file name so the split is visible in git:
#'
#' \describe{
#'   \item{\code{R/RDBESStockCoord_*.R}}{Vendored from the ICES
#'     RDBESStockCoord working-group code. Algorithms, argument lists and
#'     return shapes are upstream's. Local edits are packaging-only and
#'     are marked \code{## [pkg]}. Do not refactor these; fix upstream and
#'     re-vendor.}
#'   \item{\code{R/rcef_*.R}}{Written here. Strata, classification,
#'     grouping, scenarios, script emission. Free to refactor.}
#'   \item{\code{R/viz_*.R}}{Written here. Theme and plots. Pure ggplot2,
#'     no shiny; separated so they can be dropped or split into their own
#'     package later without touching the analysis code.}
#' }
#'
#' The chain:
#' \preformatted{
#' derive_fields() -> build_strata() -> classify_strata()
#'   -> define_raising_groups_auto() | define_raising_groups_manual()
#'   -> catchRaising()         (discards)
#'   -> catchAllocationAoL()   (lengths and ages)
#' }
#'
#' Both grouping methods return the same \code{strataCond} /
#' \code{matchedDataCond} shape, which is what the two RDBESStockCoord
#' engines consume, so one group definition drives either.
#'
#' @name rcef-package
#' @aliases rcef
#' @import dplyr
#' @import ggplot2
#' @import rlang
#' @importFrom data.table setDT rbindlist dcast
#' @importFrom tidyr pivot_longer pivot_wider
#' @keywords internal
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
  "importedOrRaised", "originType", "variableType", "groupName"
))
