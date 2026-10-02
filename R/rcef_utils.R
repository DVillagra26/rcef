# =====================================================================
#  rcef_utils.R   (vocabulary, derive_fields(), unit conversion, labellers)
#
#  ORIGIN: this project (ILVO / RCEF Workbench). Freely refactorable.
#  Was: R_development/0_Functions_utils.R
#  No shiny in this file. If it needs `input`, `session` or a tag,
#  it belongs in app/, not here.
# =====================================================================

# ============================================================
#  0_Functions_utils.R
#  Shared vocabulary, field derivation and unit handling.
# ------------------------------------------------------------
#  Package-ready data module, and the base of the stack: everything else
#  (classification, grouping, raising, plots) assumes these are loaded. It
#  holds no app state and draws no plots, so it can be sourced on its own in
#  RStudio to work with CEF catch tables at the console.
#
#  WHAT IS HERE
#    the vocabularies    which fleet and area levels a stratum may be built
#                        from, and which columns may be filtered on;
#    derive_fields()     split the CEF fleet and area codes into the nested
#                        levels an assessment actually groups by;
#    to_tonnes()         one weight unit for the whole app (tonnes);
#    the labellers       stratum_label(), stratum_axis_label(), rule_label(),
#                        class_axis_label(), facet_scales(), fmt_t3().
#
#  DEPENDENCIES  Imports: dplyr, rlang. The library block below is what the
#  app and the test scripts rely on being attached.
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(rlang)
  library(purrr); library(ggalluvial)
  # jsonlite is NOT attached: its validate() would mask shiny::validate()
  requireNamespace("jsonlite", quietly = TRUE)
})

# ----------------------------------------------------------
#  Small operators and formatters
# ----------------------------------------------------------

#' Default for a NULL or NA value
#'
#' Returns `b` when `a` is `NULL` or a single `NA`, else `a`. Used throughout
#' to give an absent app input or an unset field a sensible fallback.
#'
#' @param a The value to test.
#' @param b The fallback.
#' @return `a`, or `b` when `a` is NULL/NA.
#' @keywords internal
#' @noRd
#' @export
`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

#' Format a weight in tonnes for display
#'
#' Three decimals with a thousands separator, the convention used for every
#' weight shown by the app.
#'
#' @param x Numeric weight in tonnes.
#' @return A character vector.
#' @export
fmt_t3 <- function(x) formatC(x, format = "f", digits = 3, big.mark = ",")

# ----------------------------------------------------------
#  Vocabulary: what a stratum may be built from
# ----------------------------------------------------------

#' Every variable a stratum may be grouped, split or plotted by
#'
#' One vocabulary, used by the figure axes, the cascade keys and the
#' manual clause builder alike. Before this there were two: the four
#' abstract dimensions (Fleet, Country, Area, Season) resolved through a
#' "level of detail" control, and the raw columns. That indirection meant
#' a figure could only ever show one fleet resolution at a time, and a
#' cascade could not widen from `metier6` to `metier4` - both are "Fleet",
#' so the hierarchy had nothing to widen ALONG.
#'
#' Returning the real column names removes the indirection entirely: a
#' key or an axis IS a column, `metier4` and `metier6` are two different
#' keys, and a cascade can step from one to the other.
#'
#' Structural columns are never offered. [build_strata()] filters on
#' `catchCategory` to split landings from discards and sums `total` in the
#' unit `variableUnit` names, so grouping BY one of those does not error -
#' it quietly makes every stratum either landings or discards, leaving the
#' discard rate 0 or NA everywhere.
#'
#' @param d A prepared catch table (post [derive_fields()]).
#' @param max_levels Drop columns with more distinct values than this. A
#'   column with one value splits nothing; one with thousands makes every
#'   stratum its own group.
#' @param extra_drop Further columns to withhold.
#' @return A named character vector, `label = column`, ordered so the
#'   things people reach for most are first.
#' @seealso [derive_fields()], [build_strata()]
#' @note Named `grouping_vars()`, not `group_vars()`: dplyr exports a
#'   `group_vars()` and the app attaches dplyr, so the package's own
#'   function was masked. It failed silently - the vocabulary came back
#'   empty and every picker showed only "-- none --", with no error to
#'   say why.
#' @export

grouping_vars <- function(d, max_levels = 400, extra_drop = character(0)) {
  if (is.null(d) || !is.data.frame(d) || !ncol(d)) return(character(0))

  structural <- c("catchCategory", "total", "variableUnit", "variableType",
                  "variance", "domainCatchDis", "domainCatchBMS",
                  "domainBiology", "originType", "recordType",
                  "numPSUs", "numTrips", "comment", "year", "stock",
                  "speciesCode", "workingGroup", "fisheriesManagementUnit",
                  "areaType", "seasonType", "fleetType")

  cand <- setdiff(names(d), c(structural, extra_drop))
  keep <- vapply(cand, function(k) {
    v <- d[[k]]
    # Numeric is allowed too - seasonValue is an integer quarter code in
    # the raw CEF data, not a character column the way vesselFlagCountry
    # already is, and it is still a grouping variable like any other. The
    # level ceiling below is what keeps an actual measurement (weight,
    # say) from being offered: those have far more than max_levels values.
    if (!(is.character(v) || is.factor(v) || is.logical(v) || is.numeric(v)))
      return(FALSE)
    n <- length(unique(v))
    # Stratum is exempt from the ceiling. It is one level per uploaded
    # stratum by construction - hundreds of them - and that is the point:
    # it is the "own data" rung a cascade starts from. Capping it out
    # would remove the one key the whole hierarchy is relative to.
    if (identical(k, "Stratum")) return(n > 1)
    n > 1 && n <= max_levels
  }, logical(1))
  cand <- cand[keep]

  # Preferred order: the native stratum, then each family finest-last so
  # a cascade reads naturally when the keys are picked top to bottom.
  pref <- c("Stratum",
            "fleetValue", "metier6", "metier5", "metier4_mesh", "metier4",
            "gear", "target", "mesh", "FleetType",
            "vesselFlagCountry",
            "areaValue", "ICESSubdivision", "ICESDivision", "ICESSubArea",
            "seasonValue", "PSU")
  ordered <- c(intersect(pref, cand), sort(setdiff(cand, pref)))

  lab <- c(Stratum = "Stratum (as uploaded)",
           fleetValue = "fleetValue (full)",
           metier6 = "metier6 (gear_tgt_mesh)",
           metier5 = "metier5 (gear_target)",
           metier4_mesh = "metier4_mesh (gear_mesh)",
           metier4 = "metier4 (gear)",
           areaValue = "areaValue (full)",
           ICESSubdivision = "ICES subdivision (27.X.X.X)",
           ICESDivision = "ICES division (27.X.X)",
           ICESSubArea = "ICES subarea (27.X)",
           vesselFlagCountry = "Country", seasonValue = "Season")
  stats::setNames(ordered,
                  ifelse(is.na(lab[ordered]), ordered, lab[ordered]))
}

#' The cascade-key vocabulary, grouped into Fleet / Country / Area / Season
#'
#' A curated subset of [grouping_vars()] for the hierarchical cascade's
#' key pickers specifically. Two differences from the full vocabulary:
#'
#'   - Narrower. `gear`, `target`, `mesh`, `FleetType`, `vesselFlagCountry`
#'     and `PSU` are real columns [grouping_vars()] offers for plot axes
#'     and clause building, but they are not meaningful CASCADE keys - a
#'     rung grouped by `PSU` says nothing about fleet, area or season
#'     resolution, so it does not belong beside the four families a
#'     cascade actually widens through.
#'   - Grouped. Returned as a named list of named vectors -
#'     `list(Fleet = c(...), Country = c(...), Area = c(...), Season =
#'     c(...))` - which `selectInput(choices = )` renders as HTML
#'     `<optgroup>`s: each family gets a bold subtitle in the dropdown,
#'     so "which resolution of fleet" and "which resolution of area" read
#'     as two separate questions rather than ten items in one flat list.
#'
#' Each family is ordered finest-first (`fleetValue` before `metier4`;
#' `areaValue` before `ICESSubArea`), matching the order a cascade widens
#' in when keys are picked top to bottom.
#'
#' How fine a cascade-key variable is, within its family
#'
#' Fleet and Area are each a family of nested resolutions - `fleetValue`
#' is finer than `metier4`, `areaValue` finer than `ICESSubArea` - and
#' two variables from the SAME family only make sense in fine-to-coarse
#' order, never the reverse. Rank 1 is finest; Country and Season are
#' single-variable families and never conflict with themselves.
#'
#' @keywords internal
#' @export
KEY_FAMILY_RANK <- c(fleetValue = 1L, metier6 = 2L, metier5 = 3L, metier4 = 4L,
                     areaValue = 1L, ICESSubdivision = 2L, ICESDivision = 3L,
                     ICESSubArea = 4L, vesselFlagCountry = 1L, seasonValue = 1L)

#' Which family a cascade-key variable belongs to
#' @keywords internal
#' @export
KEY_FAMILY_OF <- c(fleetValue = "Fleet", metier6 = "Fleet", metier5 = "Fleet",
                   metier4 = "Fleet", areaValue = "Area",
                   ICESSubdivision = "Area", ICESDivision = "Area",
                   ICESSubArea = "Area", vesselFlagCountry = "Country",
                   seasonValue = "Season")

#' Warn when a cascade key is finer than the strata definition
#'
#' The strata table is already built at whatever resolution the strata
#' definition (Key 1) names - a metier5-based definition never carries
#' the raw `fleetValue` values, they were collapsed away when the strata
#' were built. A key asking for something FINER than that, in the same
#' family, can never split anything: every stratum already looks
#' identical at that resolution, so the rung resolves 0 strata and quietly
#' does nothing.
#'
#' @param keys Character vector of chosen cascade-key columns (Key 2
#'   onward - may contain `""` for unfilled slots, skipped).
#' @param strata_vars Character vector of columns the strata definition
#'   (Key 1) resolved to.
#' @return A character vector of warning messages, one per offending key,
#'   or `character(0)` if none.
#' @seealso [key_order_warnings()], [key_vocab_grouped()]
#' @export
key_vs_strata_warnings <- function(keys, strata_vars) {
  warn <- character(0)
  for (k in keys) {
    if (!nzchar(k) || is.na(KEY_FAMILY_OF[k])) next
    fam <- KEY_FAMILY_OF[[k]]
    same_fam <- strata_vars[strata_vars %in% names(KEY_FAMILY_OF) &
                              KEY_FAMILY_OF[strata_vars] == fam]
    if (!length(same_fam)) next
    strata_rank <- min(KEY_FAMILY_RANK[same_fam])
    if (KEY_FAMILY_RANK[[k]] < strata_rank)
      warn <- c(warn, sprintf(
        "%s is finer than the strata definition (%s) - the strata table has already collapsed this, so nothing will be grouped.",
        k, paste(same_fam, collapse = "/")))
  }
  warn
}

#' Warn when cascade keys go coarse-then-fine within the same family
#'
#' Widening is supposed to get progressively coarser: once the cascade
#' has tried grouping by `ICESSubArea` (coarse), trying `areaValue`
#' (finer) at a later key is backwards - the earlier, coarser key already
#' pooled everything that finer one could ever distinguish.
#'
#' @param keys Character vector of chosen cascade-key columns, IN THE
#'   ORDER they are tried (Key 2 first). May contain `""` for unfilled
#'   slots, skipped.
#' @return A character vector of warning messages, one per offending key,
#'   or `character(0)` if none.
#' @seealso [key_vs_strata_warnings()], [key_vocab_grouped()]
#' @export
key_order_warnings <- function(keys) {
  warn <- character(0)
  seen <- list()
  for (k in keys) {
    if (!nzchar(k) || is.na(KEY_FAMILY_OF[k])) next
    fam  <- KEY_FAMILY_OF[[k]]
    rank <- KEY_FAMILY_RANK[[k]]
    prev <- seen[[fam]]
    if (!is.null(prev) && rank < prev$rank)
      warn <- c(warn, sprintf(
        "%s is finer than %s, already tried earlier in the same %s family - nothing left for it to distinguish.",
        k, prev$var, fam))
    if (is.null(prev) || rank > prev$rank) seen[[fam]] <- list(rank = rank, var = k)
  }
  warn
}

#' The cascade-key vocabulary, grouped into Fleet / Country / Area / Season
#'
#' A curated subset of [grouping_vars()] for the hierarchical cascade's
#' key pickers specifically, returned as a named list of named vectors -
#' `list(Fleet = c(...), Country = c(...), Area = c(...), Season =
#' c(...))` - which `selectInput(choices = )` renders as HTML
#' `<optgroup>`s: a bold subtitle per family in the dropdown. Narrower
#' than the full vocabulary too - `gear`, `target`, `mesh`, `FleetType`,
#' `vesselFlagCountry` and `PSU` are real columns [grouping_vars()]
#' offers for plot axes and clause building, but are not meaningful
#' CASCADE keys. Each family is ordered finest-first.
#'
#' @param d A data frame (typically the loaded catch table).
#' @return A named list of named character vectors, each present family
#'   filtered to columns [grouping_vars()] would offer.
#' @seealso [grouping_vars()], [key_vs_strata_warnings()]
#' @export
key_vocab_grouped <- function(d) {
  gv <- grouping_vars(d)
  fam <- list(
    Fleet   = c("fleetValue (full)" = "fleetValue",
               "metier6 (gear_tgt_mesh)" = "metier6",
               "metier5 (gear_target)" = "metier5",
               "metier4 (gear)" = "metier4"),
    Country = c(Country = "vesselFlagCountry"),
    Area    = c("areaValue (full)" = "areaValue",
               "ICES subdivision (27.X.X.X)" = "ICESSubdivision",
               "ICES division (27.X.X)" = "ICESDivision",
               "ICES subarea (27.X)" = "ICESSubArea"),
    Season  = c(Season = "seasonValue"))
  out <- lapply(fam, function(v) v[v %in% gv])
  Filter(length, out)
}


#' Join a character vector as English prose: "a, b and c"
#'
#' Shared by the discard cascade's step descriptions and the length/age
#' allocation cascade's - both say "same X, Y - pooled across all Z" and
#' should read identically rather than each spelling out its own comma
#' joiner.
#'
#' @param v A character vector.
#' @return A single string.
#' @export
and_list <- function(v) {
  if (!length(v)) return("")
  if (length(v) == 1) return(v)
  paste(paste(v[-length(v)], collapse = ", "), "and", v[length(v)])
}

#' Columns that may be filtered on
#'
#' Every derived and native column the filter builder is allowed to offer.
#'
#' @keywords internal
#' @export
FILTER_VARS <- c("metier4", "metier5", "metier6", "fleetValue",
                 "target", "mesh", "metier4_mesh", "FleetType",
                 "vesselFlagCountry",
                 "ICESSubArea", "ICESDivision", "ICESSubdivision",
                 "areaValue", "seasonValue")

# ----------------------------------------------------------
#  Deriving the nested stratum fields
# ----------------------------------------------------------

#' Split an ICES area code into its nested levels
#'
#' An ICES area code is a nested path (`27.4.b.1`), so it is split into
#' subarea, division and subdivision. A code already coarser than the level
#' asked for REPEATS rather than becoming `NA`: `27.4` is its own division and
#' subdivision as far as the data goes, so a stratum reported at that
#' resolution is never silently dropped from a finer grouping.
#'
#' @param area Character vector of ICES area codes.
#' @return A list with `ICESSubArea`, `ICESDivision` and `ICESSubdivision`.
#' @seealso [derive_fields()]
#' @export
ices_area_levels <- function(area) {
  a <- as.character(area)
  parts <- strsplit(a, ".", fixed = TRUE)
  take <- function(n) vapply(parts, function(z) {
    if (!length(z) || all(is.na(z))) return(NA_character_)
    paste(z[seq_len(min(n, length(z)))], collapse = ".")
  }, character(1))
  out <- data.frame(ICESSubArea    = take(2),   # 27.X
                    ICESDivision   = take(3),   # 27.X.X
                    ICESSubdivision = take(4),  # 27.X.X.X
                    stringsAsFactors = FALSE)
  out[is.na(a) | !nzchar(a), ] <- NA_character_
  out
}

#' Derive the grouping columns from a CEF catch table
#'
#' Adds the columns an assessment groups by, none of which the CEF format
#' carries directly: the nested metier levels (`metier4` to `metier6`),
#' `target` and `mesh` from `fleetValue`; `metier4_mesh`, the gear and the
#' mesh joined (`"TBB_70-99"`; just the gear when the fleet code has no mesh
#' part); the three ICES area levels from `areaValue`; and `Stratum`, the four
#' native keys joined. Country and season are not among them -
#' `vesselFlagCountry` and `seasonValue` already name them, so a `Country` /
#' `Season` alias would only be a second name for the same column. Existing
#' columns are left untouched, so it is safe to call more than once.
#'
#' @param x A CEF catch table (data frame).
#' @return `x` with the derived columns added.
#' @seealso [ices_area_levels()], [build_strata()]
#' @examples
#' \dontrun{
#' catch <- derive_fields(catch)
#' strata <- build_strata(catch, c("metier4", "vesselFlagCountry"))
#' }
#' @export
derive_fields <- function(x) {
  stopifnot(is.data.frame(x))
  need <- c("fleetValue", "areaValue", "vesselFlagCountry", "seasonValue")
  miss <- setdiff(need, names(x))
  if (length(miss))
    stop("Missing expected column(s): ", paste(miss, collapse = ", "))

  tok <- strsplit(as.character(x$fleetValue), "_", fixed = TRUE)
  tok_at <- function(i) vapply(tok, function(z)
    if (length(z) >= i) z[i] else NA_character_, character(1))
  tok_join <- function(n) vapply(tok, function(z)
    paste(z[seq_len(min(n, length(z)))], collapse = "_"), character(1))

  x %>%
    mutate(
      metier4 = tok_join(1),                 # GNS
      metier5 = tok_join(2),                 # GNS_DEF
      metier6 = tok_join(3),                 # GNS_DEF_120-219
      target  = tok_at(2),
      mesh    = tok_at(3),
      # Gear and mesh together, e.g. TBB_70-99. A mesh size only means
      # something within its gear, so this is the level a fleet is usually
      # read at - and as one label it can sit on a single axis, fill or
      # filter. A fleet code with no mesh part ("All", "OTB_DEF") is just
      # its gear, the way metier5 / metier6 stop at the last part there
      # is, rather than becoming "TBB_NA" or dropping out as NA.
      metier4_mesh = ifelse(is.na(mesh) | !nzchar(mesh), metier4,
                            paste(metier4, mesh, sep = "_")),
      ICESSubArea     = ices_area_levels(areaValue)$ICESSubArea,
      ICESDivision    = ices_area_levels(areaValue)$ICESDivision,
      ICESSubdivision = ices_area_levels(areaValue)$ICESSubdivision,
      # The stratum AS UPLOADED: the four native CEF keys, joined. This
      # is the finest grouping the data itself defines, and it is the
      # only one that needs no interpretation - every other column here
      # is something rcef derived by splitting or mapping a native one.
      # Offering it as a grouping key lets a cascade start from "the
      # stratum on its own data" without that being a special case
      # bolted onto the engine.
      Stratum = paste(vesselFlagCountry, fleetValue, areaValue,
                      seasonValue, sep = " | "),
    )
}

# ----------------------------------------------------------
#  Units: one weight for the whole workflow
# ----------------------------------------------------------

#' Weight-unit conversion factors to tonnes
#'
#' @keywords internal
#' @export
WEIGHT_TO_TONNES <- c(kg = 1e-3, g = 1e-6, t = 1, tonnes = 1, KG = 1e-3, G = 1e-6)

#' Convert a CEF weight to tonnes
#'
#' Every weight in the app is carried and shown in tonnes, while CEF
#' `WeightLive` arrives in kilograms. Converts using the row's own unit where
#' one is given, and assumes kilograms when it is absent.
#'
#' @param total Numeric weights.
#' @param unit Character vector of units, recycled over `total`; `NULL`
#'   assumes kilograms.
#' @return Numeric weights in tonnes.
#' @export
to_tonnes <- function(total, unit = NULL) {
  if (is.null(unit)) return(as.numeric(total) * 1e-3)   # CEF default is kg
  f <- unname(WEIGHT_TO_TONNES[as.character(unit)])
  f[is.na(f)] <- 1e-3
  as.numeric(total) * f
}

# ----------------------------------------------------------
#  Labellers
# ----------------------------------------------------------

#' Join a stratum's key columns into one readable label
#'
#' @param df A stratum table.
#' @param dims The key columns to join.
#' @return A character vector, one label per row.
#' @export
stratum_label <- function(df, dims) {
  present <- intersect(dims, names(df))
  if (!length(present)) return(rep("(stratum)", nrow(df)))
  do.call(paste, c(lapply(present, function(k) as.character(df[[k]])),
                   sep = " | "))
}

#' Axis title naming the dimensions a stratum is made of
#'
#' @param dims The key columns.
#' @return A single string, e.g. `"Stratum (metier4 + vesselFlagCountry)"`.
#' @export
stratum_axis_label <- function(dims) {
  if (!length(dims)) return("Stratum")
  paste0("Stratum (", paste(dims, collapse = " + "), ")")
}

#' Human-readable meaning of each stratum's donor rule
#'
#' Turns a group id into the condition that defines it, so a reader sees
#' `metier4 %in% "SSC" & vesselFlagCountry %in% "Denmark"` rather than `G23`, with a
#' plain phrase for the special cases (not raised, raised on its own data,
#' raised from ALL) and the group id kept in brackets for cross-reference.
#'
#' @param rec_g A recommendation table carrying `raise_condition`,
#'   `raise_level` and optionally `group_id`.
#' @param dims The stratum dimensions passed to [recommend_raising()], so
#'   the own-stratum level (`raise_level` there is every dims column
#'   joined, not the word "own stratum") can still be recognised. `NULL`
#'   still matches the literal string, for older saved recommendations.
#' @return A character vector of labels.
#' @export
rule_label <- function(rec_g, dims = NULL) {
  cond <- rec_g$raise_condition
  lvl  <- rec_g$raise_level
  out <- cond
  self_lvl <- c("own stratum", if (!is.null(dims)) paste(dims, collapse = " + "))
  out[lvl == "no raising needed"] <- "not raised (100% sampled)"
  out[lvl %in% self_lvl]          <- "raised on its own data"
  out[cond == "TRUE"]             <- "raised from ALL strata"
  # add the group id in brackets so it can still be cross-referenced
  gid <- if ("group_id" %in% names(rec_g)) rec_g$group_id else NA
  ok <- !is.na(gid) & !(lvl %in% c("no raising needed"))
  out[ok] <- paste0(out[ok], "  [", gid[ok], "]")
  out
}

#' Facet scale string from two free-scale flags
#'
#' @param free_x,free_y Let each panel choose its own x / y range.
#' @return One of "fixed", "free_x", "free_y", "free".
#' @export
facet_scales <- function(free_x = FALSE, free_y = FALSE) {
  if (free_x && free_y) "free" else if (free_y) "free_y"
  else if (free_x) "free_x" else "fixed"
}

#' X-axis label for a length or age distribution
#'
#' @param type The app's distribution type, "LengthTotal" or "Age".
#' @return "Length (cm)" or "Age (years)".
#' @export
class_axis_label <- function(type = "LengthTotal")
  if (identical(type, "Age")) "Age (years)" else "Length (cm)"


#' Stratum columns a biology table may carry
#'
#' Used where a helper must keep "whatever stratum columns are present"
#' without knowing which grouping the caller chose.
#'
#' @format A character vector.
#' @export
DIMS_ALL_CANDIDATES <- c("metier4", "metier5", "metier6", "vesselFlagCountry",
                         "ICESSubArea", "ICESDivision", "ICESSubdivision",
                         "seasonValue", "FleetType", "target", "mesh",
                         "metier4_mesh")
