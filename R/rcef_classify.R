# =====================================================================
#  rcef_classify.R   (build_strata(), classify_strata(), cat_lookup(), status_palette())
#
#  ORIGIN: this project (ILVO / RCEF Workbench). Freely refactorable.
#  Was: R_development/0_Functions_classify.R
#  No shiny in this file. If it needs `input`, `session` or a tag,
#  it belongs in app/, not here.
# =====================================================================

# ============================================================
#  0_Functions_classify.R
#  Classify catch strata by how well they are sampled.
# ------------------------------------------------------------
#  Package-ready data module. It answers one question for a stock: for each
#  stratum, is the sampling good enough to raise (or allocate) on the
#  stratum's own data, or must it borrow from somewhere else? The answer is
#  a category and a colour that the plots, the automatic recommendation and
#  the manual rule builder all read from, so the classification is defined
#  once and never drifts between screen and algorithm.
#
#  PIPELINE
#    build_strata(catch, dims)              raw CEF catch  -> one row per
#                                           stratum, with landings, the
#                                           landings that carry a discard
#                                           estimate, discards, coverage and
#                                           discard rate (all in tonnes).
#    classify_strata(strata, threshold,     strata + a coverage threshold
#                    what, min_meas)        (and, for length/age, a minimum
#                                           number of measurements) -> the
#                                           same table plus cat_key, Status
#                                           and Raising_Decision.
#    cat_lookup() / status_palette()        the category labels, decisions
#                                           and colours behind Status.
#
#  The classified table is the input to the group-definition functions in
#  0_Functions_groups.R, whose output in turn drives catchRaising() (for
#  discards) and catchAllocationAoL() (for lengths and ages).
#
#  DEPENDENCIES
#    Imports: dplyr.
#    Provided elsewhere in the package: to_tonnes() (unit conversion, used
#      by build_strata()).
# ============================================================

#' @keywords internal
#' @name rcef-classify-imports
#' @importFrom dplyr %>% across all_of arrange case_when count filter first
#'   group_by mutate n row_number select summarise
NULL

#' Sampling-status categories, decisions and colours
#'
#' The single source of truth for how a stratum's sampling is described. For
#' discards it is a one-axis scheme on coverage; for lengths and ages it is a
#' two-axis scheme on coverage AND the number of fish measured, so a fully
#' covered but thinly measured stratum is flagged rather than trusted.
#' Colours are shades of one palette (green = use own data, yellow / orange =
#' borrow, red = nothing sampled) so hue gives the verdict and shade the
#' severity.
#'
#' @param threshold Coverage threshold in percent that separates "enough"
#'   from "too little".
#' @param what "discard" (default), "length" or "age".
#' @param min_meas Minimum number of measurements for a length/age stratum to
#'   count as well measured. Ignored for discards.
#' @return A data frame with columns `cat_key`, `status`, `decision` and
#'   `colour`, one row per category.
#' @seealso [classify_strata()], [status_palette()]
#' @export
cat_lookup <- function(threshold, what = "discard", min_meas = 0) {
  # Length / age: a two-way status on coverage AND number of measurements,
  # so a fully covered but thinly measured stratum is flagged rather than
  # trusted. Colours are shades of the discard palette so a category is
  # read by hue (green = use own data, yellow / orange = borrow, red =
  # nothing sampled) and its severity by shade.
  if (what %in% c("length", "age")) {
    unit <- if (what == "age") "ages" else "lengths"
    borrow <- paste("Look for a group to raise with instead of the",
                    "stratum's own distribution")
    return(data.frame(
      cat_key = c("full_ok", "safe_ok", "full_thin", "safe_thin",
                  "low_ok", "low_thin", "nosamp"),
      status = c(
        sprintf("SAFE: 100%% sampled and > %s measurements", min_meas),
        sprintf("SAFE: > %s%% coverage and > %s measurements", threshold, min_meas),
        sprintf("WARNING: 100%% sampled but < %s measurements", min_meas),
        sprintf("WARNING: > %s%% coverage but < %s measurements", threshold, min_meas),
        sprintf("WARNING: < %s%% coverage but > %s measurements", threshold, min_meas),
        sprintf("WARNING: < %s%% coverage and < %s measurements", threshold, min_meas),
        sprintf("WARNING: no %s sampled", unit)),
      # decision and colour follow the cat_key order above: the two SAFE
      # categories first, so both read as green
      decision = c("No raising needed (fully sampled, well measured)",
                   "Raise with own strata",
                   borrow, borrow, borrow, borrow, borrow),
      # dark green, light green, pale yellow, light orange, yellow,
      # dark orange, red
      colour = c("#00441B", "#74C476", "#FFF2AE", "#FDAE61",
                 "#FFD92F", "#E66101", "#D7191C"),
      stringsAsFactors = FALSE))
  }
  data.frame(
    cat_key = c("full", "safe", "warn", "no_disc", "disc_only", "none"),
    status = c("SAFE: 100% sampled",
               paste0("SAFE: > ", threshold, "% coverage"),
               paste0("WARNING: < ", threshold, "% coverage"),
               "WARNING: no discard data",
               "SAFE: discards only / no landings",
               "No landings / no discards"),
    decision = c("No raising needed (fully sampled)",
                 "Direct raising",
                 "POOL: group with adjacent strata",
                 "BORROW: find similar strata",
                 "No raising needed (all catch discarded)",
                 "Excluded: no data"),
    colour = c("#00441B", "#41AE76", "#FFD92F",
               "#E66101", "#4292C6", "#F0F1F1"),
    stringsAsFactors = FALSE
  )
}

#' Named colour vector for the sampling statuses
#'
#' The `status -> colour` map from [cat_lookup()], ready to drop into
#' `ggplot2::scale_fill_manual()`. Passing the full vector (with
#' `limits = names(...)`) makes a plot's legend show every possible status,
#' not only those present in the data.
#'
#' @inheritParams cat_lookup
#' @return A character vector of hex colours named by status label.
#' @seealso [cat_lookup()], [classify_strata()]
#' @export
status_palette <- function(threshold, what = "discard", min_meas = 0) {
  lk <- cat_lookup(threshold, what, min_meas)
  # Map status text directly to colors as named vector
  setNames(lk$colour, lk$status)
}

#' Build the stratum table from CEF catch data
#'
#' Aggregates a long CEF catch table to one row per stratum, in tonnes. For
#' each stratum it records the total landings, the landings that carry a
#' discard estimate (`landings_with_discards`, the numerator of coverage),
#' the discards, the resulting coverage percent, and two discard-rate
#' definitions. Landings and discards are summed separately by catch
#' category, so a stratum with landings but no observed discards reads as
#' 0 percent covered rather than going missing.
#'
#' @param catch_data A long CEF catch table. Must contain `catchCategory`,
#'   `total`, `domainCatchDis` and every column named in `dims`; a
#'   `variableUnit` column is used for unit conversion when present.
#' @param dims Character vector of the columns that define a stratum
#'   (e.g. `c("metier4", "Country", "Season")`).
#' @return A data frame, one row per stratum, with `landings`,
#'   `landings_with_discards`, `discards`, `coverage`, `discard_rate`,
#'   `discard_rate_covered`, `has_disc_data`, `stratum_id` and the `dims`.
#' @details `discard_rate` uses the stratum's own total landings
#'   (`discards / (landings + discards)`); `discard_rate_covered` uses only
#'   the covered landings (`discards / (landings_with_discards + discards)`),
#'   which is the definition a donor pool is raised with. At full resolution
#'   the two coincide whenever a stratum is either fully covered or not
#'   covered at all.
#' @seealso [classify_strata()]
#' @export
build_strata <- function(catch_data, dims) {
  stopifnot(length(dims) >= 1, all(dims %in% names(catch_data)))

  u <- if ("variableUnit" %in% names(catch_data)) catch_data$variableUnit else NULL
  cd <- catch_data
  cd$.t <- to_tonnes(cd$total, u)          # everything in tonnes from here

  lan <- cd %>%
    filter(catchCategory == "Lan") %>%
    group_by(across(all_of(dims))) %>%
    summarise(
      landings = sum(.t, na.rm = TRUE),
      landings_with_discards = sum(
        .t[!is.na(domainCatchDis) & domainCatchDis != ""], na.rm = TRUE),
      .groups = "drop")

  dis <- cd %>%
    filter(catchCategory != "Lan") %>%
    group_by(across(all_of(dims))) %>%
    summarise(discards = sum(.t, na.rm = TRUE), .groups = "drop")

  full_join(lan, dis, by = dims) %>%
    mutate(
      has_disc_data = !is.na(discards),
      across(c(landings, landings_with_discards, discards),
             ~ ifelse(is.na(.x), 0, .x)),
      coverage = ifelse(landings > 0,
                        landings_with_discards / landings * 100, NA_real_),
      # A stratum's OWN rate uses its total landings:
      #     discards / (landings + discards)
      # Pooling is different - a donor pool may contain landings with no
      # discard estimate, so pool_rate() (used for raising and for the
      # red diamond on the boxplot) divides by the COVERED landings only:
      #     sum(discards) / (sum(landings_with_discards) + sum(discards))
      # At full resolution the two coincide whenever a stratum is either
      # fully covered or not covered at all.
      discard_rate = ifelse(landings + discards > 0,
                            discards / (landings + discards), NA_real_),
      # the pooled definition, kept alongside for comparison
      discard_rate_covered = ifelse(landings_with_discards + discards > 0,
                                    discards / (landings_with_discards + discards),
                                    NA_real_),
      # stable id used by the emitted strataCond
      stratum_id = do.call(paste, c(lapply(dims, function(k)
        as.character(get(k))), sep = " | "))
    ) %>%
    as.data.frame()
}

#' Classify strata against a coverage (and measurement) threshold
#'
#' Adds the sampling verdict to a stratum table: `cat_key` (a stable key),
#' `Status` (a factor with every level from [cat_lookup()], so legends stay
#' complete) and `Raising_Decision` (what to do about it). Discards use the
#' one-axis coverage scheme; lengths and ages use the two-axis
#' coverage-by-measurements scheme, where a thinly measured stratum keeps its
#' real coverage but is flagged rather than demoted.
#'
#' @param s A stratum table from [build_strata()] (discards) or the biology
#'   link (length/age), carrying at least `coverage`; length/age
#'   classification also reads `n_measurements`.
#' @param threshold Coverage threshold in percent.
#' @param what "discard" (default), "length" or "age".
#' @param min_meas Minimum number of measurements for length/age; ignored for
#'   discards.
#' @return `s` with `cat_key`, `Status` and `Raising_Decision` added.
#' @seealso [build_strata()], [cat_lookup()]
#' @export
classify_strata <- function(s, threshold, what = "discard", min_meas = 0) {
  lk <- cat_lookup(threshold, what, min_meas)

  # Length / age: classify on coverage AND number of measurements.
  if (what %in% c("length", "age")) {
    if (!"n_measurements" %in% names(s)) s$n_measurements <- 0
    return(s %>%
             mutate(
               n_measurements = ifelse(is.na(n_measurements), 0, n_measurements),
               cat_key = case_when(
                 is.na(coverage) | coverage <= 0       ~ "nosamp",
                 coverage >= 100 & n_measurements >= min_meas ~ "full_ok",
                 coverage >= 100                               ~ "full_thin",
                 coverage >  threshold & n_measurements >= min_meas ~ "safe_ok",
                 coverage >  threshold                               ~ "safe_thin",
                 n_measurements >= min_meas                         ~ "low_ok",
                 TRUE                                               ~ "low_thin"
               ),
               Status = factor(lk$status[match(cat_key, lk$cat_key)],
                               levels = unique(lk$status)),
               Raising_Decision = factor(lk$decision[match(cat_key, lk$cat_key)],
                                         levels = unique(lk$decision))
             ))
  }

  s %>%
    mutate(
      cat_key = case_when(
        landings <= 0 & discards <= 0     ~ "none",
        landings <= 0                     ~ "disc_only",
        is.na(coverage) | coverage <= 0   ~ "no_disc",
        coverage >= 100                   ~ "full",
        coverage >  threshold             ~ "safe",
        TRUE                              ~ "warn"
      ),
      Status = factor(lk$status[match(cat_key, lk$cat_key)],
                      levels = unique(lk$status)),
      Raising_Decision = factor(lk$decision[match(cat_key, lk$cat_key)],
                                levels = unique(lk$decision))
    )
}
