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
#' @noRd
#' @importFrom dplyr %>% across all_of arrange case_when count filter first
#'   group_by mutate n row_number select summarise
NULL

#' Sampling-status categories, decisions and colours
#'
#' The single source of truth for how a stratum's sampling is described.
#' Coverage alone says how much of the landings weight carries a discard
#' estimate; it says nothing about how many TRIPS that estimate rests
#' on, and a stratum 100% covered by one unusually large trip is a
#' thinner basis than the same coverage spread across a dozen. Every
#' scheme here is therefore at least two-axis: coverage and trips for
#' discards, coverage, trips and measurements for lengths and ages -
#' three sampling-effort questions asked at once, not folded into one
#' number.
#'
#' Colours are shades of one palette (green = use own data, yellow /
#' orange = borrow, red = nothing usable) so hue gives the verdict and
#' shade the severity. `min_trips` and the trips comparisons themselves
#' are unknown-averse: a stratum with no trip count reported is treated
#' as failing the trips test rather than passing it by default, the same
#' way a stratum outside every category collapses to the worst one
#' rather than the best.
#'
#' @param threshold Coverage threshold in percent that separates "enough"
#'   from "too little".
#' @param what "discard" (default), "length" or "age".
#' @param min_meas Minimum number of measurements for a length/age stratum to
#'   count as well measured. Ignored for discards.
#' @param min_trips Minimum number of trips for a stratum to count as
#'   adequately covered in EFFORT terms, separately from coverage in
#'   WEIGHT terms. Defaults to 2, matching ICES practice.
#' @return A data frame with columns `cat_key`, `status`, `decision` and
#'   `colour`, one row per category.
#' @seealso [classify_strata()], [status_palette()]
#' @export
cat_lookup <- function(threshold, what = "discard", min_meas = 0,
                       min_trips = 2) {
  # Length / age: coverage, trips and measurements together - eight
  # combinations of "does each of the three clear its own bar", with the
  # top corner (all three, generously) split once more into a genuinely
  # complete stratum (100% coverage, well beyond the trips floor) and an
  # ordinarily safe one. Nine categories, three shades of orange so the
  # middling ones read as siblings rather than as one undifferentiated
  # warning colour.
  if (what %in% c("length", "age")) {
    unit <- if (what == "age") "ages" else "lengths"
    borrow <- paste("Look for a group to raise with instead of the",
                    "stratum's own distribution")
    return(data.frame(
      cat_key = c("full_ok", "safe_ok", "yellowgreen", "yellow",
                  "orangeyellow", "orange_lowtrip", "orange_lowcov",
                  "orange_lowcovtrip", "red", "none"),
      status = c(
        sprintf("SAFE: 100%% sampled, more than %s trips and %s measurements",
                min_trips, min_meas),
        sprintf("SAFE: > %s%% coverage, %s+ trips and %s+ measurements",
                threshold, min_trips, min_meas),
        sprintf("WARNING: < %s%% coverage but %s+ trips and %s+ measurements",
                threshold, min_trips, min_meas),
        sprintf("WARNING: > %s%% coverage and %s+ trips but < %s measurements",
                threshold, min_trips, min_meas),
        sprintf("WARNING: > %s%% coverage and %s+ measurements but < %s trips",
                threshold, min_meas, min_trips),
        sprintf("WARNING: > %s%% coverage but < %s trips and < %s measurements",
                threshold, min_trips, min_meas),
        sprintf("WARNING: < %s%% coverage and < %s measurements but %s+ trips",
                threshold, min_meas, min_trips),
        sprintf("WARNING: < %s%% coverage and < %s trips but %s+ measurements",
                threshold, min_trips, min_meas),
        "WARNING: coverage, trips and measurements all fail their targets",
        "No catch to classify"),
      # decision follows the cat_key order: the two SAFE categories
      # first, so both read as green
      decision = c("No raising needed (fully sampled, well measured)",
                   "Raise with own strata",
                   borrow, borrow, borrow, borrow, borrow, borrow, borrow,
                   "Excluded: no data"),
      # dark green, green, yellow-green, yellow, yellow-orange,
      # three distinguishable oranges (light / medium / dark), red,
      # then the same near-white used for the discard side's "no
      # landings / no discards" - a stratum with nothing to measure is
      # not a sampling FAILURE (which is what red means everywhere
      # else), it is a stratum with nothing in it. Landing that in red
      # made every truly empty cell in a heatmap look identical to a
      # completely unsampled one, which is a much worse finding.
      colour = c("#00441B", "#41AE76", "#A6D96A", "#FFD92F",
                 "#FEC44F", "#FDAE61", "#F46D43", "#D66A35", "#D7191C",
                 "#F0F1F1"),
      stringsAsFactors = FALSE))
  }
  # Discards: coverage (weight terms) and trips (effort terms). A
  # stratum can be fully covered by weight and still rest on almost no
  # trips, which is exactly the case min_trips exists to separate from a
  # genuinely well-sampled one.
  data.frame(
    cat_key = c("full", "safe", "warn_lowcov", "warn_lowtrip", "warn_red",
                "disc_only", "none"),
    status = c(
      sprintf("SAFE: 100%% sampled and more than %s trips", min_trips),
      sprintf("SAFE: > %s%% sampled and %s+ trips", threshold, min_trips),
      sprintf("WARNING: < %s%% sampled but %s+ trips", threshold, min_trips),
      sprintf("WARNING: > %s%% sampled but < %s trips", threshold, min_trips),
      "WARNING: no discard sampled",
      "SAFE: discards only / no landings",
      "No landings / no discards"),
    decision = c("No raising needed (fully sampled)",
                 "Direct raising",
                 "POOL: group with adjacent strata",
                 "POOL: group with adjacent strata",
                 "BORROW: find similar strata",
                 "No raising needed (all catch discarded)",
                 "Excluded: no data"),
    colour = c("#00441B", "#41AE76", "#FFD92F", "#FDAE61", "#D7191C",
               "#4292C6", "#F0F1F1"),
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
status_palette <- function(threshold, what = "discard", min_meas = 0,
                           min_trips = 2) {
  lk <- cat_lookup(threshold, what, min_meas, min_trips)
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
build_strata <- function(catch_data, dims, dist = NULL) {
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
    as.data.frame() -> st

  # Measurement counts per stratum, when a distributions table is given.
  # A donor condition may say `n_measurements >= 100`; this is where that
  # number comes from. Without dist the column is 0 everywhere - present,
  # so the condition evaluates, but never satisfied, which is the honest
  # answer when no distributions were supplied.
  st$n_measurements <- 0
  if (!is.null(dist) && nrow(dist)) {
    dd <- as.data.frame(dist)
    key_col <- intersect(c("domainBiology"), names(dd))
    cc <- tryCatch(count_measurements_by_stratum(catch_data, dd, dims),
                   error = function(e) NULL)
    if (!is.null(cc)) {
      i <- match(st$stratum_id, cc$stratum_id)
      st$n_measurements <- ifelse(is.na(cc$n[i]), 0, cc$n[i])
    }
  }

  # Sampling effort, alongside sampling coverage: how many trips back
  # the estimate, not just how much of the landings weight it covers. A
  # stratum can be 100% covered by a single trip that happened to land
  # everything in one go, which is a thinner basis than the same
  # coverage spread across a dozen trips. max(), not sum(): numTrips is
  # reported per raw CEF row and the same trip count is often repeated
  # across several rows of the same sample (by catch category, by
  # length class in the wide files) - summing would count one trip
  # several times over. NA where the column is absent or nothing in the
  # stratum reports it, which the sampling-status classification below
  # treats as "unknown", not as zero.
  if ("numTrips" %in% names(cd)) {
    trips <- cd %>%
      group_by(across(all_of(dims))) %>%
      summarise(numTrips = suppressWarnings(max(numTrips, na.rm = TRUE)),
               .groups = "drop")
    trips$numTrips[!is.finite(trips$numTrips)] <- NA_real_
    st <- as.data.frame(st %>% left_join(trips, by = dims))
  } else st$numTrips <- NA_real_

  st
}

# Count distribution records per catch stratum, via domainBiology. A
# stratum's measurement count is how many distribution rows carry a
# domainBiology that the stratum's catch rows also carry - the same link
# biology_link() uses, reduced to a count.
#' @noRd
#' @export
count_measurements_by_stratum <- function(catch_data, dist, dims) {
  d <- as.data.frame(catch_data)
  if (!"domainBiology" %in% names(d) || !"domainBiology" %in% names(dist))
    return(NULL)
  d$stratum_id <- do.call(paste, c(lapply(dims, function(k)
    as.character(d[[k]])), sep = " | "))
  # rows of dist per domainBiology
  tab <- as.data.frame(table(domainBiology = as.character(dist$domainBiology)),
                       stringsAsFactors = FALSE)
  names(tab)[2] <- "nrec"
  key <- unique(d[, c("stratum_id", "domainBiology")])
  key$domainBiology <- as.character(key$domainBiology)
  key <- merge(key, tab, by = "domainBiology", all.x = TRUE)
  key$nrec[is.na(key$nrec)] <- 0
  agg <- stats::aggregate(nrec ~ stratum_id, data = key, FUN = sum)
  names(agg)[2] <- "n"
  agg
}

#' Classify strata against a coverage (and measurement) threshold
#'
#' Adds the sampling verdict to a stratum table: `cat_key` (a stable key),
#' `Status` (a factor with every level from [cat_lookup()], so legends stay
#' complete) and `Raising_Decision` (what to do about it). Discards are
#' classified on coverage and trips; lengths and ages on coverage, trips
#' AND measurements - three sampling-effort questions at once, none of
#' which substitutes for the others.
#'
#' @param s A stratum table from [build_strata()] (discards) or
#'   [biology_link()] (length/age), carrying at least `coverage`;
#'   length/age classification also reads `n_measurements`. Both carry
#'   `numTrips` when the loaded CEF file reported it; a stratum with no
#'   trip count is treated as failing the trips test, not passing it -
#'   unknown is not the same as adequate.
#' @param threshold Coverage threshold in percent.
#' @param what "discard" (default), "length" or "age".
#' @param min_meas Minimum number of measurements for length/age; ignored for
#'   discards.
#' @param min_trips Minimum number of trips. Defaults to 2.
#' @return `s` with `cat_key`, `Status` and `Raising_Decision` added.
#' @seealso [build_strata()], [cat_lookup()]
#' @export
classify_strata <- function(s, threshold, what = "discard", min_meas = 0,
                            min_trips = 2) {
  lk <- cat_lookup(threshold, what, min_meas, min_trips)
  if (!"numTrips" %in% names(s)) s$numTrips <- NA_real_
  # trips2: a stratum with no reported trip count is treated as failing
  # the trips test (< min_trips), never as passing it - the same
  # unknown-is-not-adequate stance the rest of the classification takes.
  trips2 <- ifelse(is.na(s$numTrips), min_trips - 1, s$numTrips)

  # Length / age: classify on coverage, trips AND measurements. Eight
  # combinations of the three yes/no tests, with the "all three, well
  # beyond the floor" corner split once more into a genuinely complete
  # stratum vs an ordinarily safe one.
  if (what %in% c("length", "age")) {
    if (!"n_measurements" %in% names(s)) s$n_measurements <- 0
    # Only strata a real weight column says have nothing in them. A
    # caller that never tracked weight at all (works from coverage
    # directly) gets the ORIGINAL behaviour - falling through to the
    # coverage/trips/measurement comparisons - rather than being
    # silently reclassified as "no catch" for a column it never had.
    no_catch <- if ("weight" %in% names(s))
      (is.na(s$weight) | s$weight <= 0) else rep(FALSE, nrow(s))
    return(s %>%
             mutate(
               n_measurements = ifelse(is.na(n_measurements), 0, n_measurements),
               cat_key = case_when(
                 # A stratum with nothing landed or discarded has
                 # nothing to sample - checked FIRST, same as the
                 # discard side's landings<=0 & discards<=0 case, or
                 # every one of these fell through every coverage/
                 # trips/measurement comparison (all NA on 0 weight)
                 # straight to the catch-all "red", making an empty
                 # stratum look identical to a completely failed one.
                 no_catch                                      ~ "none",
                 coverage >= 100 & trips2 > min_trips &
                   n_measurements >= min_meas                  ~ "full_ok",
                 coverage >= threshold & trips2 >= min_trips &
                   n_measurements >= min_meas                  ~ "safe_ok",
                 coverage <  threshold & trips2 >= min_trips &
                   n_measurements >= min_meas                  ~ "yellowgreen",
                 coverage >= threshold & trips2 >= min_trips &
                   n_measurements <  min_meas                  ~ "yellow",
                 coverage >= threshold & trips2 <  min_trips &
                   n_measurements >= min_meas                  ~ "orangeyellow",
                 coverage >= threshold & trips2 <  min_trips &
                   n_measurements <  min_meas                  ~ "orange_lowtrip",
                 coverage <  threshold & trips2 >= min_trips &
                   n_measurements <  min_meas                  ~ "orange_lowcov",
                 coverage <  threshold & trips2 <  min_trips &
                   n_measurements >= min_meas                  ~ "orange_lowcovtrip",
                 TRUE                                          ~ "red"
               ),
               Status = factor(lk$status[match(cat_key, lk$cat_key)],
                               levels = unique(lk$status)),
               Raising_Decision = factor(lk$decision[match(cat_key, lk$cat_key)],
                                         levels = unique(lk$decision))
             ))
  }

  # Discards: coverage (weight terms) and trips (effort terms). A
  # stratum can be fully covered by weight and still rest on very few
  # trips - min_trips is what separates that from genuinely well-sampled.
  s %>%
    mutate(
      cat_key = case_when(
        landings <= 0 & discards <= 0                    ~ "none",
        landings <= 0                                     ~ "disc_only",
        coverage >= 100 & trips2 > min_trips               ~ "full",
        coverage >= threshold & trips2 >= min_trips        ~ "safe",
        coverage <  threshold & trips2 >= min_trips        ~ "warn_lowcov",
        coverage >= threshold & trips2 <  min_trips        ~ "warn_lowtrip",
        TRUE                                                ~ "warn_red"
      ),
      Status = factor(lk$status[match(cat_key, lk$cat_key)],
                      levels = unique(lk$status)),
      Raising_Decision = factor(lk$decision[match(cat_key, lk$cat_key)],
                                levels = unique(lk$decision))
    )
}
