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

#' Is a threshold argument "on"?
#'
#' `min_trips` / `min_meas` are optional: `NULL` (or `NA`) means the
#' caller has not activated that sampling-effort question at all, so it
#' is dropped from the classification and from every label - not merely
#' given a threshold of zero, which would still count it as an (always
#' passing) active target.
#'
#' @param v A threshold value, or `NULL`/`NA`.
#' @return `TRUE` if `v` is a real, active threshold.
#' @keywords internal
#' @export
thr_active <- function(v) !is.null(v) && !(length(v) == 1 && is.na(v))

#' Sampling-status categories, decisions and colours
#'
#' The single source of truth for how a stratum's sampling is described.
#' Coverage alone says how much of the landings weight carries a discard
#' estimate; it says nothing about how many TRIPS that estimate rests
#' on, and a stratum 100% covered by one unusually large trip is a
#' thinner basis than the same coverage spread across a dozen. Coverage
#' is always part of the scheme; `min_trips` (discards, length/age) and
#' `min_meas` (length/age only) are each independently optional - pass
#' `NULL` to drop that question entirely rather than merely giving it an
#' always-passing threshold. The categories and labels adapt to however
#' many of {coverage, trips, measurements} are actually active.
#'
#' Colours run one direction only: dark green (nothing to fix) -> green
#' -> yellow -> orange -> dark orange as more of the ACTIVE sampling
#' targets are missed, with a single reserved red for the one
#' qualitatively different case - a stratum that carries NO discard
#' estimate (or, for length/age, NO linked distribution) at all, so
#' there is nothing to judge against a threshold. A near-white marks a
#' stratum with no catch to classify. Every reachable colour is reached:
#' the warning shade is a straight count of how many active targets a
#' stratum missed (yellow = 1, orange = 2, dark orange = 3 - only
#' possible when trips AND measurements are both active), the same
#' shade regardless of WHICH targets those were.
#'
#' `min_trips` and the trips comparison are unknown-averse: a stratum
#' with no trip count reported is treated as failing the trips test
#' rather than passing it by default.
#'
#' @param threshold Coverage threshold in percent that separates "enough"
#'   from "too little". Always active.
#' @param what "discard" (default), "length" or "age".
#' @param min_meas Minimum number of measurements for a length/age stratum to
#'   count as well measured, or `NULL` to leave measurements out of the
#'   classification entirely. Ignored for discards.
#' @param min_trips Minimum number of trips for a stratum to count as
#'   adequately covered in EFFORT terms, separately from coverage in
#'   WEIGHT terms, or `NULL` to leave trips out of the classification
#'   entirely. Defaults to 2, matching ICES practice.
#' @return A data frame with columns `cat_key`, `status`, `decision`,
#'   `abbr` and `colour`, one row per REACHABLE category (fewer rows
#'   when `min_trips` / `min_meas` are off), ordered best to worst.
#' @seealso [classify_strata()], [status_palette()]
#' @export
cat_lookup <- function(threshold, what = "discard", min_meas = 0,
                       min_trips = 2) {
  # Shared palette: dark green -> green -> yellow -> orange -> dark
  # orange for 0..3 missed targets, plus one reserved red for "no
  # estimate / distribution at all" and a near-white for "no catch".
  col_full <- "#00441B"; col_safe <- "#41AE76"
  col_w    <- c("#FFD92F", "#F16913", "#7F2704")
  col_none <- "#F0F1F1"; col_red  <- "#D7191C"; col_disc <- "#4292C6"
  borrow <- "BORROW: look for a group to raise with"
  is_bio <- what %in% c("length", "age")

  trip_active <- thr_active(min_trips)
  meas_active <- is_bio && thr_active(min_meas)
  parts <- c("coverage", if (trip_active) "trips",
            if (meas_active) "measurements")
  n_active <- length(parts)

  full_bits <- c("100% covered",
                if (trip_active) sprintf("> %s trips", min_trips),
                if (meas_active) sprintf("%s+ measurements", min_meas))
  safe_bits <- c(sprintf(">= %s%% covered", threshold),
                if (trip_active) sprintf("%s+ trips", min_trips),
                if (meas_active) sprintf("%s+ measurements", min_meas))
  # discard status text reads "A and B"; length/age (up to 3 bits) reads
  # "A, B, C" - matches the wording this already had for the configs
  # that pre-date the optional thresholds.
  join_bits <- function(bits)
    if (length(bits) <= 1) paste(bits, collapse = "")
    else if (is_bio) paste(bits, collapse = ", ")
    else paste(bits, collapse = " and ")
  full_status <- paste0("SAFE: ", join_bits(full_bits))
  safe_status <- paste0("SAFE: ", join_bits(safe_bits))

  full_abbr <- paste(c("100% cov",
                       if (trip_active) sprintf(">%st", min_trips),
                       if (meas_active) sprintf(">=%sm", min_meas)),
                     collapse = ", ")
  safe_abbr <- paste(c(sprintf(">=%s%% cov", threshold),
                       if (trip_active) sprintf(">=%st", min_trips),
                       if (meas_active) sprintf(">=%sm", min_meas)),
                     collapse = ", ")

  # warnN status/abbr: a straight count of how many of the ACTIVE
  # targets were missed, phrased against however many are in play.
  warn_status <- function(n_missed) {
    if (n_missed >= n_active) {
      if (n_active == 1) return(sprintf("WARNING: %s below target", parts))
      joined <- if (n_active == 2) paste(parts, collapse = " and ")
                else paste0(paste(parts[-n_active], collapse = ", "),
                            " and ", parts[n_active])
      if (n_active == 2)
        return(sprintf("WARNING: both %s below target", joined))
      return(sprintf("WARNING: %s all below target", joined))
    }
    sprintf("WARNING: %s of %s below target", c("one", "two")[n_missed],
           paste(parts, collapse = " / "))
  }
  warn_abbr <- function(n_missed)
    if (n_missed >= n_active && n_active > 2)
      sprintf("all %d targets short", n_active)
    else sprintf("%d target%s short", n_missed, if (n_missed == 1) "" else "s")

  warn_keys   <- paste0("warn", seq_len(n_active))
  warn_stat   <- vapply(seq_len(n_active), warn_status, character(1))
  warn_abb    <- vapply(seq_len(n_active), warn_abbr, character(1))
  warn_colour <- col_w[seq_len(n_active)]

  if (is_bio) {
    unit <- if (what == "age") "age" else "length"
    return(data.frame(
      cat_key = c("full", "safe", warn_keys, "no_dist", "none"),
      status = c(full_status, safe_status, warn_stat,
                sprintf("WARNING: no %s distribution linked to this stratum", unit),
                "No catch to classify"),
      decision = c("No raising needed (fully sampled, well measured)",
                   "Raise with own strata",
                   rep(borrow, n_active),
                   borrow, "Excluded: no data"),
      abbr = c(full_abbr, safe_abbr, warn_abb,
              sprintf("no %s dist", unit), "no catch"),
      colour = c(col_full, col_safe, warn_colour, col_red, col_none),
      stringsAsFactors = FALSE))
  }
  # Discards: coverage (weight terms) and, when active, trips (effort
  # terms), plus the reserved red for a stratum with no discard estimate
  # at all.
  data.frame(
    cat_key = c("full", "safe", warn_keys, "no_dist", "disc_only", "none"),
    status = c(full_status, safe_status, warn_stat,
              "WARNING: no discard estimate for this stratum",
              "SAFE: discards only / no landings",
              "No landings / no discards"),
    decision = c("No raising needed (fully sampled)",
                 "Direct raising",
                 rep("POOL: group with adjacent strata", n_active),
                 borrow,
                 "No raising needed (all catch discarded)",
                 "Excluded: no data"),
    abbr = c(full_abbr, safe_abbr, warn_abb,
            "no discard estimate",
            "discards only, no landings",
            "no landings / no discards"),
    colour = c(col_full, col_safe, warn_colour, col_red,
              col_disc, col_none),
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

#' Named short-label vector for the sampling statuses
#'
#' The `status -> abbr` map from [cat_lookup()], for
#' `ggplot2::scale_fill_manual(labels = )` or a base-R legend that needs
#' the compact codes rather than the full sentences.
#'
#' @inheritParams cat_lookup
#' @return A character vector of short codes named by full status label.
#' @seealso [status_palette()], [cat_lookup()]
#' @export
status_abbr <- function(threshold, what = "discard", min_meas = 0,
                        min_trips = 2) {
  lk <- cat_lookup(threshold, what, min_meas, min_trips)
  setNames(lk$abbr, lk$status)
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
#' complete) and `Raising_Decision` (what to do about it). Coverage is
#' always part of the verdict; `min_trips` (discards, length/age) and
#' `min_meas` (length/age only) are each independently optional - pass
#' `NULL` to drop that sampling-effort question from the classification
#' entirely rather than merely giving it an always-passing threshold. The
#' warning shade counts how many of the ACTIVE targets a stratum missed
#' (yellow = 1, orange = 2, dark orange = 3 - reachable only with both
#' trips and measurements active).
#'
#' @param s A stratum table from [build_strata()] (discards) or
#'   [biology_link()] (length/age), carrying at least `coverage`;
#'   length/age classification also reads `n_measurements`. Both carry
#'   `numTrips` when the loaded CEF file reported it; a stratum with no
#'   trip count is treated as failing the trips test, not passing it -
#'   unknown is not the same as adequate. `numTrips` / `n_measurements`
#'   are only read when `min_trips` / `min_meas` are active.
#' @param threshold Coverage threshold in percent.
#' @param what "discard" (default), "length" or "age".
#' @param min_meas Minimum number of measurements for length/age well
#'   measured, or `NULL` to leave measurements out of the classification.
#'   Ignored for discards.
#' @param min_trips Minimum number of trips, or `NULL` to leave trips out
#'   of the classification. Defaults to 2.
#' @return `s` with `cat_key`, `Status` and `Raising_Decision` added.
#' @seealso [build_strata()], [cat_lookup()]
#' @export
classify_strata <- function(s, threshold, what = "discard", min_meas = 0,
                            min_trips = 2) {
  lk <- cat_lookup(threshold, what, min_meas, min_trips)
  trip_active <- thr_active(min_trips)
  if (!"numTrips" %in% names(s)) s$numTrips <- NA_real_
  # trips2: a stratum with no reported trip count is treated as failing
  # the trips test (< min_trips), never as passing it - the same
  # unknown-is-not-adequate stance the rest of the classification takes.
  # Meaningless (and unused) when trips are not active.
  trips2 <- if (trip_active) ifelse(is.na(s$numTrips), min_trips - 1, s$numTrips)
            else rep(NA_real_, nrow(s))
  trip_ok <- if (trip_active) trips2 >= min_trips else rep(TRUE, nrow(s))

  # Length / age: classify on coverage plus whichever of {trips,
  # measurements} are active. The warning shade is just a count of how
  # many of the ACTIVE targets a stratum missed (1 -> yellow, 2 ->
  # orange, 3 -> dark orange, the last only reachable with trips AND
  # measurements both active). Red is reserved for a stratum with no
  # linked distribution at all - nothing to test against a threshold -
  # and near-white for one with no catch.
  if (what %in% c("length", "age")) {
    meas_active <- thr_active(min_meas)
    if (!"n_measurements" %in% names(s)) s$n_measurements <- 0
    # Only strata a real weight column says have nothing in them. A
    # caller that never tracked weight at all (works from coverage
    # directly) keeps the fall-through behaviour rather than being
    # silently reclassified as "no catch" for a column it never had.
    no_catch <- if ("weight" %in% names(s))
      (is.na(s$weight) | s$weight <= 0) else rep(FALSE, nrow(s))
    # "has a linked distribution at all": weight_measured (carried here
    # as landings_with_discards) is > 0 only when at least one catch
    # row's domainBiology matched a distribution row. Absent on a bare
    # coverage-only frame -> taken as present, so those keep the
    # threshold fall-through.
    has_dist <- if ("landings_with_discards" %in% names(s))
      (!is.na(s$landings_with_discards) & s$landings_with_discards > 0)
    else if ("weight_measured" %in% names(s))
      (!is.na(s$weight_measured) & s$weight_measured > 0)
    else rep(TRUE, nrow(s))
    nm      <- ifelse(is.na(s$n_measurements), 0, s$n_measurements)
    cov_ok  <- !is.na(s$coverage) & s$coverage >= threshold
    meas_ok <- if (meas_active) nm >= min_meas else rep(TRUE, nrow(s))
    n_miss  <- (!cov_ok) + (if (trip_active) (!trip_ok) else 0) +
      (if (meas_active) (!meas_ok) else 0)
    is_full <- cov_ok & trip_ok & meas_ok & !is.na(s$coverage) & s$coverage >= 100 &
      (if (trip_active) trips2 > min_trips else TRUE)
    return(s %>%
             mutate(
               n_measurements = nm,
               cat_key = case_when(
                 no_catch    ~ "none",
                 !has_dist   ~ "no_dist",
                 is_full     ~ "full",
                 n_miss == 0 ~ "safe",
                 n_miss == 1 ~ "warn1",
                 n_miss == 2 ~ "warn2",
                 TRUE        ~ "warn3"
               ),
               Status = factor(lk$status[match(cat_key, lk$cat_key)],
                               levels = unique(lk$status)),
               Raising_Decision = factor(lk$decision[match(cat_key, lk$cat_key)],
                                         levels = unique(lk$decision))
             ))
  }

  # Discards: coverage (weight terms) and, when active, trips (effort
  # terms). The warning shade counts missed ACTIVE targets (1 -> yellow,
  # 2 -> orange, the latter only reachable with trips active); red is
  # reserved for a stratum with no discard estimate at all.
  has_disc <- if ("has_disc_data" %in% names(s)) s$has_disc_data
              else !is.na(s$discards)
  cov_ok   <- !is.na(s$coverage) & s$coverage >= threshold
  n_miss   <- (!cov_ok) + (if (trip_active) (!trip_ok) else 0)
  is_full  <- cov_ok & trip_ok & !is.na(s$coverage) & s$coverage >= 100 &
    (if (trip_active) trips2 > min_trips else TRUE)
  s %>%
    mutate(
      cat_key = case_when(
        landings <= 0 & discards <= 0 ~ "none",
        landings <= 0                  ~ "disc_only",
        !has_disc                      ~ "no_dist",
        is_full                        ~ "full",
        n_miss == 0                    ~ "safe",
        n_miss == 1                    ~ "warn1",
        TRUE                           ~ "warn2"
      ),
      Status = factor(lk$status[match(cat_key, lk$cat_key)],
                      levels = unique(lk$status)),
      Raising_Decision = factor(lk$decision[match(cat_key, lk$cat_key)],
                                levels = unique(lk$decision))
    )
}
