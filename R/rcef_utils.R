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

#' Fleet levels a stratum may be grouped by
#'
#' Named vector of the nested fleet resolutions, coarsest first, as
#' `label = column`. The names are what a user picks from; the values are the
#' columns [derive_fields()] creates.
#'
#' @keywords internal
#' @export
FLEET_LEVELS <- c("Metier 4 (gear)"          = "metier4",
                  "Metier 5 (gear_target)"   = "metier5",
                  "Metier 6 (gear_tgt_mesh)" = "metier6",
                  "Full fleetValue"          = "fleetValue")

#' Area levels a stratum may be grouped by
#'
#' Named vector of the nested ICES area resolutions, coarsest first, as
#' `label = column`.
#'
#' @keywords internal
#' @export
AREA_LEVELS  <- c("ICES subarea (27.X)"          = "ICESSubArea",
                  "ICES division (27.X.X)"       = "ICESDivision",
                  "ICES subdivision (27.X.X.X)"  = "ICESSubdivision",
                  "Full areaValue"               = "areaValue")

#' Columns that may be filtered on
#'
#' Every derived and native column the filter builder is allowed to offer.
#'
#' @keywords internal
#' @export
FILTER_VARS <- c("metier4", "metier5", "metier6", "fleetValue",
                 "target", "mesh", "FleetType", "Country",
                 "ICESSubArea", "ICESDivision", "ICESSubdivision",
                 "areaValue", "Season")

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
#' carries directly: the nested metier levels (`metier4` to `metier6`) plus
#' gear, target and mesh from `fleetValue`; the three ICES area levels from
#' `areaValue`; `Country` and `Season`; and a `stratum_id`. Existing columns
#' are left untouched, so it is safe to call more than once.
#'
#' @param x A CEF catch table (data frame).
#' @return `x` with the derived columns added.
#' @seealso [ices_area_levels()], [build_strata()]
#' @examples
#' \dontrun{
#' catch <- derive_fields(catch)
#' strata <- build_strata(catch, c("metier4", "Country"))
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
      Country = as.character(vesselFlagCountry),
      Season  = as.character(seasonValue),
      metier4 = tok_join(1),                 # GNS
      metier5 = tok_join(2),                 # GNS_DEF
      metier6 = tok_join(3),                 # GNS_DEF_120-219
      gear    = metier4,
      target  = tok_at(2),
      mesh    = tok_at(3),
      ICESSubArea     = ices_area_levels(areaValue)$ICESSubArea,
      ICESDivision    = ices_area_levels(areaValue)$ICESDivision,
      ICESSubdivision = ices_area_levels(areaValue)$ICESSubdivision,
      FleetType = case_when(
        (metier5 %in% c("OTB_DEF", "OTT_DEF") |
           metier4 %in% c("SDN", "SSC", "PTB")) &
          mesh %in% c(">=120", ">=220", "100-119", "120-219") ~ "TR1",
        TRUE ~ "Other")
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
#' @return A single string, e.g. `"Stratum (metier4 + Country)"`.
#' @export
stratum_axis_label <- function(dims) {
  if (!length(dims)) return("Stratum")
  paste0("Stratum (", paste(dims, collapse = " + "), ")")
}

#' Human-readable meaning of each stratum's donor rule
#'
#' Turns a group id into the condition that defines it, so a reader sees
#' `metier4 %in% "SSC" & Country %in% "Denmark"` rather than `G23`, with a
#' plain phrase for the special cases (not raised, raised on its own data,
#' raised from ALL) and the group id kept in brackets for cross-reference.
#'
#' @param rec_g A recommendation table carrying `raise_condition`,
#'   `raise_level` and optionally `group_id`.
#' @return A character vector of labels.
#' @export
rule_label <- function(rec_g) {
  cond <- rec_g$raise_condition
  lvl  <- rec_g$raise_level
  out <- cond
  out[lvl == "no raising needed"] <- "not raised (100% sampled)"
  out[lvl == "own stratum"]       <- "raised on its own data"
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
DIMS_ALL_CANDIDATES <- c("metier4", "metier5", "metier6", "Country",
                         "ICESSubArea", "ICESDivision", "ICESSubdivision",
                         "Season", "FleetType", "target", "mesh")
