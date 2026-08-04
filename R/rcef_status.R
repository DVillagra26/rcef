# =====================================================================
#  rcef_status.R   (what the installed package can actually do)
#
#  ORIGIN: this project. Replaces R_development/external_loader.R.
#  The old loader source()d six scripts from a folder and reported what
#  it found. Nothing is sourced any more - the functions are IN the
#  package - so the only thing that can still be missing is an optional
#  dependency. Same list shape, so the app's Availability panel and the
#  TAF context need no change.
# =====================================================================

#' Optional packages, and what stops working without them
#'
#' @format A named list: package -> the capability it gates.
#' @export
RCEF_OPTIONAL <- list(
  icesVocab = "funMakeRelation(): ICES species and area vocabularies",
  icesSD    = "funMakeRelation(): the ICES stock database")

#' Where the workflow functions come from
#'
#' Kept in the shape the old `load_external()` returned, so callers that
#' read `$dir` or `$missing` keep working. Nothing is sourced: `found` is
#' the list of vendored source files as they sit inside the package.
#'
#' @return A list with `dir`, `found`, `missing`, `errors`, `packages_ok`
#'   and `packages_missing`.
#' @export
#' @examples
#' rcef_sources()$dir
rcef_sources <- function() {
  ok <- vapply(names(RCEF_OPTIONAL), requireNamespace, logical(1),
               quietly = TRUE)
  list(
    dir = tryCatch(system.file("R", package = "rcef"),
                   error = function(e) "rcef package"),
    found = c("RDBESStockCoord_relation.R", "RDBESStockCoord_intercatch_in.R",
              "RDBESStockCoord_intercatch_out.R", "RDBESStockCoord_utils.R",
              "RDBESStockCoord_raising.R", "RDBESStockCoord_allocation.R"),
    missing = character(0),
    errors = list(),
    packages_ok = names(RCEF_OPTIONAL)[ok],
    packages_missing = names(RCEF_OPTIONAL)[!ok])
}

#' Which workflow capabilities are available
#'
#' Every engine is vendored, so these are all TRUE in a correctly
#' installed package. `make_rel` is the exception: it needs `icesVocab`
#' and `icesSD`, which are Suggests.
#'
#' @param x Ignored; present so the old `external_status(EXT)` call
#'   signature still works.
#' @return A named list of logicals, plus `pkgs_missing`.
#' @export
#' @examples
#' rcef_status()$raising
rcef_status <- function(x = NULL) {
  has <- function(f) {
    e <- tryCatch(asNamespace("rcef"), error = function(e) globalenv())
    exists(f, envir = e, mode = "function")
  }
  src <- rcef_sources()
  list(
    ic_exchange = has("funIntercatchCEF"),
    ic_output   = has("funICoutCEF"),
    make_rel    = has("funMakeRelation") &&
                  !length(setdiff(c("icesVocab", "icesSD"),
                                  src$packages_ok)),
    raising     = has("catchRaising") && has("check_group_conditions"),
    group_cond  = has("check_group_conditions"),
    catch_alloc = has("catchAllocationAoL"),
    catch_agg   = has("catchAtAoLAggregation"),
    mean_agg    = has("meanWoLatAoLAggregation"),
    agelength   = has("catchAllocationAoL"),
    pkgs_missing = src$packages_missing)
}
