# =====================================================================
#  rcef_raise_native.R   (CEF prepare/check + run_native_raising() around catchRaising())
#
#  ORIGIN: this project (ILVO / RCEF Workbench). Freely refactorable.
#  Was: R_development/mod_native_raising.R
#  No shiny in this file. If it needs `input`, `session` or a tag,
#  it belongs in app/, not here.
# =====================================================================

# ============================================================
#  mod_native_raising.R
#  Bridge: app-defined groups -> strataCond / matchedDataCond ->
#  catchRaising() from 0_Functions_discards_raising.R
#
#  The app's own apply_raising() works on the stratum table and gives a
#  quick answer for comparing setups. catchRaising() works on the full
#  CEF long table and is the production path: it keeps catch categories,
#  units, domains and the importedOrRaised flag. Both are offered, and
#  they should agree - if they do not, that is worth knowing, so the
#  comparison tab can hold results from either.
# ============================================================

# Canonical CEF column order.
#
# This is not cosmetic. grp_catch_raising() selects with the RANGE
#   dplyr::select(vesselFlagCountry:catchCategory, ...)
# so it silently depends on year, workingGroup, stock and speciesCode
# sitting between those two columns. A CEF table carrying the right
# columns in the wrong order fails with an opaque join error:
#   "Join columns in `y` must be present in the data."
# Everything handed to catchRaising() is therefore reordered first.
#' @noRd
#' @export
CEF_ORDER <- c("recordType", "vesselFlagCountry", "year", "workingGroup",
               "stock", "speciesCode", "catchCategory", "seasonType",
               "seasonValue", "areaType", "areaValue",
               "fisheriesManagementUnit", "metier6", "fleetType", "fleetValue",
               "domainCatchDis", "domainCatchBMS", "domainBiology",
               "originType", "variableType", "variableUnit", "total",
               "variance", "PSU", "numPSUs", "numTrips", "comment")

#' @noRd
#' @export
CEF_REQUIRED <- c("vesselFlagCountry", "year", "workingGroup", "stock",
                  "speciesCode", "catchCategory", "seasonType", "seasonValue",
                  "areaType", "areaValue", "fisheriesManagementUnit",
                  "metier6", "fleetType", "fleetValue", "domainCatchDis",
                  "domainCatchBMS", "domainBiology", "originType",
                  "variableType", "variableUnit", "total")

#' @noRd
#' @export
cef_reorder <- function(d) {
  present <- intersect(CEF_ORDER, names(d))
  d[, c(present, setdiff(names(d), present)), drop = FALSE]
}

#' @noRd
#' @export
cef_check <- function(catch_data) {
  miss <- setdiff(CEF_REQUIRED, names(catch_data))
  list(ok = length(miss) == 0, missing = miss)
}

# Add the derived fields, the stratum key, and the stratum-level
# quantities the conditions may refer to.
#
# discard_rate and coverage are properties of a STRATUM, not of a CEF
# row, but a matchedDataCond carrying a discard-rate window has to be
# evaluable row by row. So they are joined back onto every row of the
# stratum they belong to. Without this the generated matchedDataCond
# fails with "object 'discard_rate' not found".
#' @noRd
#' @export
cef_prepare <- function(catch_data, dims) {
  d <- derive_fields(catch_data)
  # catchRaising() multiplies `total` by a unit-conversion ratio, so it
  # must be numeric. CSV/InterCatch loads often bring it in as character
  # (thousands separators, blanks), which fails deep inside convert_field
  # with "non-numeric argument to binary operator". Coerce the numeric
  # CEF fields here, once, before anything downstream touches them.
  num_fields <- intersect(c("total", "variance", "numPSUs", "numTrips"),
                          names(d))
  for (f in num_fields)
    if (!is.numeric(d[[f]]))
      d[[f]] <- suppressWarnings(as.numeric(gsub("[, ]", "", as.character(d[[f]]))))
  # add_stratum_metrics() is the single source of the stratum-level
  # columns a donor condition can name (coverage, discard_rate,
  # n_measurements, stratum_weight) plus stratum_id. It was previously
  # joined a SECOND time here via build_strata(), which collided with the
  # already-present coverage/discard_rate into coverage.x / coverage.y -
  # so a condition saying `coverage >= 50` found no plain `coverage` and
  # errored with "object 'coverage' not found". One join, once.
  d <- add_stratum_metrics(d, dims)
  # landings / discards are wanted alongside for the overview tables
  st <- build_strata(d, dims) %>%
    select(stratum_id, landings, landings_with_discards, discards)
  # drop any landings/discards already present so the join cannot collide
  d <- d[, setdiff(names(d), c("landings", "landings_with_discards",
                               "discards")), drop = FALSE]
  cef_reorder(d %>% left_join(st, by = "stratum_id"))
}



# ---- run --------------------------------------------------------------
#' @noRd
#' @export
run_native_raising <- function(catch_data, dims, conds,
                               type = "discards",
                               originType = "WGEstimate",
                               variableType = "WeightLive",
                               log_file = NULL,
                               assembled_output = TRUE) {
  if (!exists("catchRaising", mode = "function"))
    stop("catchRaising() not loaded - point the external script folder at ",
         "0_Functions_discards_raising.R")
  ck <- cef_check(catch_data)
  if (!ck$ok)
    stop("The CEF table is missing columns catchRaising() needs: ",
         paste(ck$missing, collapse = ", "))

  # The donor conditions are written as arithmetic on stratum metrics -
  # `coverage >= 50`, `discard_rate <= 0.9` - so those columns have to be
  # ON the rows catchRaising() evaluates against. A CEF table has none of
  # them: coverage is a property of a stratum, computed across many rows.
  # Without this join the conditions are not merely wrong, they error.
  cd <- cef_prepare(add_stratum_metrics(catch_data, dims), dims)
  catchRaising(catch_data = cd,
               condition_raising_st_list = conds$strataCond,
               condition_matched_data_list = conds$matchedDataCond,
               type = type,
               originType = originType,
               variableType = variableType,
               logFile = log_file,
               assembled_output = assembled_output)
}

# Summarise a catchRaising result the same way the app summarises its own,
# so the two can sit side by side in the comparison
#' @noRd
#' @export
native_summary <- function(raised, dims) {
  raised %>%
    filter(toupper(catchCategory) %in% c("DIS")) %>%
    group_by(across(all_of(intersect(dims, names(.))))) %>%
    summarise(imported = sum(total[importedOrRaised %in% "imported"], na.rm = TRUE),
              raised   = sum(total[importedOrRaised %in% "raised"], na.rm = TRUE),
              estimated_discards = sum(total, na.rm = TRUE),
              .groups = "drop")
}

