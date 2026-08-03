# ============================================================
#  external_loader.R
#  Which of the ICES routines are available, and where from.
# ------------------------------------------------------------
#  THIS FILE INVERTED WHEN THE ICES SCRIPTS WERE BUNDLED.
#
#  It used to source them from a folder on disk, and its job was to report
#  which ones failed to load. They now ship inside the package, so the
#  normal answer is "all of them, from rcef" and nothing needs sourcing.
#
#  What remains is the override: a user with a NEWER revision of the ICES
#  scripts than the snapshot bundled here can point at their copy and have
#  it win. That case is real - ICES revises these on its own schedule, and
#  a package release is a poor reason to be stuck on an old raising
#  routine.
# ============================================================

#' The ICES routines the workflow depends on
#'
#' Names, and what each is needed for. Used to report capability rather
#' than to load anything.
#'
#' @export
ICES_ROUTINES <- c(
  funMakeRelation         = "build stock_relation from ICES vocab + SD",
  funIntercatchCEF        = "InterCatch exchange format -> CEF",
  funICoutCEF             = "InterCatch output format -> CEF",
  check_group_conditions  = "validate raising group conditions",
  catchRaising            = "raise discards over the defined groups",
  catchAllocationAoL      = "allocate length/age distributions",
  catchAtAoLAggregation   = "aggregate numbers at age or length",
  meanWoLatAoLAggregation = "aggregate mean weight at age or length")

#' Packages the ICES routines need
#'
#' Suggests rather than Imports: several are ICES-specific and awkward to
#' install, and most of the workflow runs without them. Absence is
#' reported, not thrown.
#'
#' @export
ICES_PKGS <- c("data.table", "icesVocab", "icesSD", "RstoxData",
               "plyr", "tidyr", "dplyr", "stringr", "readr")

#' Report which ICES routines are available, and from where
#'
#' Checks the package namespace first, then the global environment, which
#' is where an override sourced by [use_external_ices()] lands. A routine
#' present in both is reported as overridden - a fact worth knowing before
#' an assessment rather than after.
#'
#' @return A data frame with `routine`, `purpose`, `available` and `source`
#'   (`"rcef"`, `"override"` or `"missing"`).
#' @seealso [use_external_ices()], [external_status()]
#' @export
#' @examples
#' ices_routine_status()
ices_routine_status <- function() {
  in_pkg <- vapply(names(ICES_ROUTINES),
                   function(f) exists(f, envir = asNamespace("rcef"),
                                      mode = "function", inherits = FALSE),
                   logical(1))
  in_gbl <- vapply(names(ICES_ROUTINES),
                   function(f) exists(f, envir = globalenv(),
                                      mode = "function", inherits = FALSE),
                   logical(1))
  data.frame(
    routine   = names(ICES_ROUTINES),
    purpose   = unname(ICES_ROUTINES),
    available = in_pkg | in_gbl,
    source    = ifelse(in_gbl, "override", ifelse(in_pkg, "rcef", "missing")),
    stringsAsFactors = FALSE, row.names = NULL)
}

#' Override the bundled ICES routines with a copy on disk
#'
#' The bundled versions are a snapshot. ICES revises these scripts on its
#' own schedule, so a stock raised with a stale copy would be wrong in a
#' way nothing here could detect. This is the escape hatch: source your own
#' copy into the global environment, where it takes precedence over the
#' package's.
#'
#' The override is deliberately loud. Silently swapping the routine that
#' produces an assessment's numbers is not something that should happen
#' without a line in the log saying so.
#'
#' @param dir Folder holding the ICES `.R` files.
#' @param files Which files to source; defaults to every `.R` in `dir`.
#' @return A data frame as returned by [ices_routine_status()], invisibly.
#' @seealso [ices_routine_status()]
#' @export
#' @examples
#' \dontrun{
#' use_external_ices("~/ICES/scripts/2026-03")
#' ices_routine_status()
#' }
use_external_ices <- function(dir, files = NULL) {
  if (!dir.exists(dir)) stop("no such folder: ", dir)
  fs <- if (is.null(files)) list.files(dir, pattern = "\\.R$", full.names = TRUE)
        else file.path(dir, files)
  fs <- fs[file.exists(fs)]
  if (!length(fs)) stop("no .R files found in ", dir)
  errs <- list()
  for (f in fs) {
    e <- tryCatch({ sys.source(f, envir = globalenv()); NULL },
                  error = function(e) conditionMessage(e))
    if (!is.null(e)) errs[[basename(f)]] <- e
  }
  st <- ices_routine_status()
  ov <- st$routine[st$source == "override"]
  message("sourced ", length(fs) - length(errs), " of ", length(fs),
          " files from ", dir)
  if (length(ov))
    message("OVERRIDDEN (these now come from disk, not from rcef): ",
            paste(ov, collapse = ", "))
  if (length(errs))
    warning("failed to source: ", paste(names(errs), collapse = ", "))
  invisible(st)
}

#' Which workflow features are usable
#'
#' @param x Ignored; kept so app code that passed a load result still works.
#' @return A named list of logicals, plus any missing packages.
#' @seealso [ices_routine_status()]
#' @export
external_status <- function(x = NULL) {
  st <- ices_routine_status()
  has <- function(f) isTRUE(st$available[st$routine == f])
  pk <- vapply(ICES_PKGS, requireNamespace, logical(1), quietly = TRUE)
  list(
    ic_exchange  = has("funIntercatchCEF"),
    ic_output    = has("funICoutCEF"),
    make_rel     = has("funMakeRelation"),
    raising      = has("catchRaising") && has("check_group_conditions"),
    group_cond   = has("check_group_conditions"),
    catch_alloc  = has("catchAllocationAoL"),
    catch_agg    = has("catchAtAoLAggregation"),
    mean_agg     = has("meanWoLatAoLAggregation"),
    agelength    = has("catchAllocationAoL"),
    pkgs_missing = names(pk)[!pk])
}

# ----------------------------------------------------------
#  Exporting the bundled routines
# ----------------------------------------------------------
# The ICES files carry no roxygen, and adding some would mean editing code
# this package only redistributes. Exporting them from here instead keeps
# R/ices_*.R byte-identical to what ICES shipped, which is what makes the
# provenance note in inst/scripts/README_ices_source.txt checkable.

#' The bundled ICES routines
#'
#' These are re-exported from the ICES scripts bundled in `R/ices_*.R`.
#' They are not this package's work and are documented here only so that
#' the exports are accounted for; for what the arguments mean, see the
#' ICES documentation for the corresponding script.
#'
#' @section Raising and allocation:
#' \describe{
#'   \item{`catchRaising()`}{raise discards over the defined groups}
#'   \item{`catchAllocationAoL()`}{allocate length or age distributions.
#'     Note it matches `distributionType` against `c("Age", "Length")`
#'     while CEF files write `"LengthTotal"`, so the column has to be
#'     rewritten before the call.}
#'   \item{`catchAtAoLAggregation()`}{aggregate numbers at age or length}
#'   \item{`meanWoLatAoLAggregation()`}{aggregate mean weight at age or length}
#'   \item{`check_group_conditions()`}{validate raising group conditions}
#' }
#'
#' @section Format conversion:
#' \describe{
#'   \item{`funIntercatchCEF()`}{InterCatch exchange format to CEF}
#'   \item{`funICoutCEF()`}{InterCatch output format to CEF}
#'   \item{`funCompareIntercatchCEF()`}{compare the two}
#'   \item{`funReadIntercatch()`}{read HI / SI / SD records}
#'   \item{`funMakeRelation()`}{build `stock_relation` from ICES vocab and SD}
#' }
#'
#' @section Strata helpers:
#' \describe{
#'   \item{`cef_add_strata()`, `cef_remove_strata()`,
#'     `cef_expand_strata_full()`, `cef_generate_strata_combinations()`}{
#'     add and manipulate strata columns on census data}
#'   \item{`cef_allocate_strata()`, `cef_compute_ref_numbers()`}{
#'     allocation over strata}
#'   \item{`raise_discards_strata()`, `split_L_with_without_D()`,
#'     `strata_coverage_summary()`, `sop()`}{raising and coverage helpers}
#' }
#'
#' @name ices_bundled_routines
#' @aliases catchAllocationAoL catchAtAoLAggregation meanWoLatAoLAggregation
#' @aliases convert_field fieldsToStrata unit2ratio
#' @aliases funCompareIntercatchCEF funReadIntercatch raise_discards_strata sop
#' @seealso [ices_routine_status()] for what is available,
#'   [use_external_ices()] to override with a newer copy.
#' @rawNamespace export(catchRaising, catchAllocationAoL,
#'   catchAtAoLAggregation, meanWoLatAoLAggregation, check_group_conditions,
#'   funIntercatchCEF, funICoutCEF, funCompareIntercatchCEF,
#'   funReadIntercatch, funMakeRelation, cef_add_strata, cef_remove_strata,
#'   cef_expand_strata_full, cef_generate_strata_combinations,
#'   cef_allocate_strata, cef_compute_ref_numbers, raise_discards_strata,
#'   split_L_with_without_D, strata_coverage_summary, sop, fieldsToStrata,
#'   convert_field, unit2ratio, unpack_list)
NULL
