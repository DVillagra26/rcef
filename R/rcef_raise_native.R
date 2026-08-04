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
CEF_ORDER <- c("recordType", "vesselFlagCountry", "year", "workingGroup",
               "stock", "speciesCode", "catchCategory", "seasonType",
               "seasonValue", "areaType", "areaValue",
               "fisheriesManagementUnit", "metier6", "fleetType", "fleetValue",
               "domainCatchDis", "domainCatchBMS", "domainBiology",
               "originType", "variableType", "variableUnit", "total",
               "variance", "PSU", "numPSUs", "numTrips", "comment")

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
  d$stratum_id <- do.call(paste, c(lapply(dims, function(k)
    as.character(d[[k]])), sep = " | "))
  st <- build_strata(d, dims) %>%
    select(stratum_id, landings, landings_with_discards, discards,
           coverage, discard_rate)
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

  cd <- cef_prepare(catch_data, dims)
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

# ---- emitted code -----------------------------------------------------
#' @noRd
#' @export
emit_native_code <- function(conds, dims, type = "discards",
                             originType = "WGEstimate",
                             variableType = "WeightLive",
                             log_file = "Log.txt") {
  q_txt <- function(l) vapply(seq_along(l), function(i)
    sprintf("    %s = quo(%s)%s", names(l)[i],
            wrap_lines(rlang::quo_text(l[[i]]), width = 66),
            if (i < length(l)) "," else ""), character(1))
  c("## ---- Raising with catchRaising() -------------------------",
    sprintf("dims <- %s", fmt_vals(dims)),
    "catch_data <- catch_data %>%",
    "    mutate(stratum_id = do.call(paste, c(across(all_of(dims)), sep = \" | \")))",
    "",
    "strataCond <- list(", q_txt(conds$strataCond), ")",
    "",
    "matchedDataCond <- list(", q_txt(conds$matchedDataCond), ")",
    "",
    "## Optional but good practice - checks the conditions before raising:",
    "cond_test  <- check_group_conditions(catch_data, strataCond,",
    '                                     conditionType = "strata", logFile = NULL)',
    "cond_test2 <- check_group_conditions(catch_data, matchedDataCond,",
    '                                     conditionType = "matched_data", logFile = NULL)',
    "",
    "catch_data_raised <- catchRaising(",
    "    catch_data                  = catch_data,",
    "    condition_raising_st_list   = strataCond,",
    "    condition_matched_data_list = matchedDataCond,",
    sprintf('    type = "%s", originType = "%s", variableType = "%s",',
            type, originType, variableType),
    sprintf('    logFile = "%s", assembled_output = TRUE)', log_file),
    "",
    "table(catch_data_raised$DrGroup, catch_data_raised$importedOrRaised)")
}
