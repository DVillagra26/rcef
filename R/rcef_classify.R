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

#' Per-row "warn_..." key from which active thresholds a stratum missed
#'
#' The same key [cat_lookup()] generates for its warning rows - one
#' `"warn_"` + underscore-joined threshold names per SPECIFIC combination
#' of pass/fail, not a generic count. Both must stay in lockstep: this is
#' what [classify_strata()] assigns per row, and `cat_lookup()`'s
#' `combn()` walk is what pre-declares every reachable value of it.
#'
#' @param ... Named logical vectors, same length, one per ACTIVE
#'   threshold, TRUE where that threshold failed for that row - passed
#'   in the canonical `cov`, `trip`, `meas` order. A row with every entry
#'   `FALSE` is not meaningful here (the caller only reaches this for
#'   rows that missed at least one).
#' @return A character vector, one `"warn_<names>"` key per row.
#' @keywords internal
#' @export
combo_key <- function(...) {
  bad <- list(...)
  nm  <- names(bad)
  n   <- length(bad[[1]])
  vapply(seq_len(n), function(i) {
    hit <- nm[vapply(bad, `[[`, FALSE, i)]
    paste0("warn_", paste(hit, collapse = "_"))
  }, character(1))
}

#' Sampling-status categories, decisions and colours
#'
#' @description
#' The single source of truth for how a stratum's sampling is described: the
#' category keys, their wording, the raising decision each implies and the
#' colour drawn for it. [classify_strata()] assigns them to strata and the
#' coverage plots ([coverage-plots]) draw them, so the classification is
#' defined once and never drifts between a table, a figure and the raising
#' algorithm.
#'
#' @details
#' Coverage alone says how much of the landings weight carries a discard
#' estimate; it says nothing about how many trips that estimate rests on, and
#' a stratum 100% covered by one unusually large trip is a thinner basis than
#' the same coverage spread across a dozen. Coverage is always part of the
#' scheme; `min_trips` (discards, length / age) and `min_meas` (length / age
#' only) are each independently optional - pass `NULL` to drop that question
#' entirely rather than merely giving it an always-passing threshold. The
#' categories and labels adapt to however many of coverage, trips and
#' measurements are active.
#'
#' | `cat_key` | Colour | Meaning |
#' |:----------|:-------|:--------|
#' | `full` | dark green | 100% covered, with more than `min_trips` trips and (length / age) at least `min_meas` measurements, for the thresholds that are active |
#' | `safe` | green | every active threshold met |
#' | `warn_cov`, `warn_trip`, `warn_meas`, `warn_cov_trip`, ... | yellow (one missed), orange (two), dark orange (three) | one row per specific combination of missed thresholds; only the combinations of the active thresholds exist |
#' | `no_dist` | red | no discard estimate (or, for length / age, no linked distribution) at all - nothing to judge against a threshold |
#' | `disc_only` | blue | discards only, no landings (discards only) |
#' | `none` | near-white | no catch to classify |
#'
#' The colour is a straight count of how many active targets a stratum
#' missed; every specific combination that reaches a given count still gets
#' its own row and label, so `"< 50% covered, >= 2 trips"` reads differently
#' from `">= 50% covered, < 2 trips"` even though both are yellow. Red is
#' never a threshold miss.
#'
#' The `decision` column says what each category implies for raising: no
#' raising needed (fully sampled, or all catch discarded), raise directly on
#' the stratum's own data, or pool with adjacent strata / borrow from another
#' group.
#'
#' Trips are unknown-averse: a stratum with no reported trip count is treated
#' as failing the trips test, not passing it - unknown is not the same as
#' adequate.
#'
#' @param threshold Coverage threshold in percent that separates "enough"
#'   from "too little". Always active.
#' @param what `"discard"` (default), `"length"` or `"age"`.
#' @param min_meas Minimum number of measurements for a length / age stratum
#'   to count as well measured, or `NULL` to leave measurements out of the
#'   classification entirely. Ignored for discards.
#' @param min_trips Minimum number of trips for a stratum to count as
#'   adequately covered in effort terms (as opposed to weight terms), or
#'   `NULL` to leave trips out of the classification entirely. Defaults to 2,
#'   matching ICES practice.
#' @return A data frame with columns `cat_key`, `status`, `decision`, `abbr`
#'   and `colour`, one row per reachable category (fewer rows when
#'   `min_trips` / `min_meas` are off), ordered best to worst.
#' @seealso [classify_strata()], [status_palette()], [status_abbr()],
#'   [status_legend_plot()]
#' @md
#' @examples
#' # coverage only, then with a trips floor, then with measurements too
#' cat_lookup(50, "discard", min_trips = NULL)$cat_key
#' cat_lookup(50, "discard")$cat_key
#' cat_lookup(50, "length", min_meas = 100)$cat_key
#'
#' # the wording and colour behind each category
#' cat_lookup(50, "discard")[, c("cat_key", "status", "colour")]
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

  # One entry per active threshold, fixed (cov, trip, meas) order. Each
  # carries the phrase used when it PASSES vs FAILS inside a combination
  # row, plus the (stricter, coverage/trips only) phrase for the "full"
  # row specifically - matches classify_strata()'s `is_full` condition.
  defs <- list(cov = list(
    full = "100% covered",              full_abbr = "100% cov",
    pass = sprintf(">= %s%% covered", threshold), pass_abbr = sprintf(">=%s%% cov", threshold),
    fail = sprintf("< %s%% covered", threshold),  fail_abbr = sprintf("<%s%% cov", threshold)))
  if (trip_active) defs$trip <- list(
    full = sprintf("> %s trips", min_trips),  full_abbr = sprintf(">%st", min_trips),
    pass = sprintf(">= %s trips", min_trips), pass_abbr = sprintf(">=%st", min_trips),
    fail = sprintf("< %s trips", min_trips),  fail_abbr = sprintf("<%st", min_trips))
  if (meas_active) defs$meas <- list(
    full = sprintf(">= %s measurements", min_meas), full_abbr = sprintf(">=%sm", min_meas),
    pass = sprintf(">= %s measurements", min_meas), pass_abbr = sprintf(">=%sm", min_meas),
    fail = sprintf("< %s measurements", min_meas),  fail_abbr = sprintf("<%sm", min_meas))
  n_active <- length(defs)

  # length/age (up to 3 bits) reads "A, B, C"; discard (up to 2) reads
  # "A and B" - matches the wording this already had for the configs
  # that pre-date the optional thresholds.
  join_txt  <- function(v) if (length(v) <= 1) paste(v, collapse = "")
                           else if (is_bio) paste(v, collapse = ", ")
                           else paste(v, collapse = " and ")
  join_abbr <- function(v) paste(v, collapse = ", ")

  full_status <- paste0("SAFE: ", join_txt(vapply(defs, `[[`, "", "full")))
  safe_status <- paste0("SAFE: ", join_txt(vapply(defs, `[[`, "", "pass")))
  full_abbr   <- join_abbr(vapply(defs, `[[`, "", "full_abbr"))
  safe_abbr   <- join_abbr(vapply(defs, `[[`, "", "pass_abbr"))

  # Every non-empty subset of the active thresholds is one warning row -
  # colour is the subset's SIZE (how many missed); the text names
  # exactly which ones, with every other active threshold shown passing.
  combos <- unlist(lapply(seq_len(n_active), function(k)
    utils::combn(names(defs), k, simplify = FALSE)), recursive = FALSE)
  warn_row <- function(miss) {
    ph <- vapply(names(defs), function(nm)
      if (nm %in% miss) defs[[nm]]$fail else defs[[nm]]$pass, character(1))
    ab <- vapply(names(defs), function(nm)
      if (nm %in% miss) defs[[nm]]$fail_abbr else defs[[nm]]$pass_abbr, character(1))
    list(cat_key = paste0("warn_", paste(miss, collapse = "_")),
        status  = paste0("WARNING: ", join_txt(ph)),
        abbr    = join_abbr(ab),
        colour  = col_w[[length(miss)]])
  }
  warn_rows   <- lapply(combos, warn_row)
  warn_keys   <- vapply(warn_rows, `[[`, "", "cat_key")
  warn_stat   <- vapply(warn_rows, `[[`, "", "status")
  warn_abb    <- vapply(warn_rows, `[[`, "", "abbr")
  warn_colour <- vapply(warn_rows, `[[`, "", "colour")
  n_warn      <- length(warn_keys)

  if (is_bio) {
    unit <- if (what == "age") "age" else "length"
    return(data.frame(
      cat_key = c("full", "safe", warn_keys, "no_dist", "none"),
      status = c(full_status, safe_status, warn_stat,
                sprintf("WARNING: no %s distribution linked to this stratum", unit),
                "No catch to classify"),
      decision = c("No raising needed (fully sampled, well measured)",
                   "Raise with own strata",
                   rep(borrow, n_warn),
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
                 rep("POOL: group with adjacent strata", n_warn),
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
#' [ggplot2::scale_fill_manual()]. Passing the full vector (with
#' `limits = names(...)`) makes a plot's legend show every possible status,
#' not only those present in the data.
#'
#' @inheritParams cat_lookup
#' @return A character vector of hex colours named by status label.
#' @seealso [cat_lookup()], [classify_strata()], [status_abbr()]
#' @md
#' @examples
#' status_palette(50, "discard", min_trips = NULL)
#' @export
status_palette <- function(threshold, what = "discard", min_meas = 0,
                           min_trips = 2) {
  lk <- cat_lookup(threshold, what, min_meas, min_trips)
  # Map status text directly to colors as named vector
  setNames(lk$colour, lk$status)
}

#' Named short-label vector for the sampling statuses
#'
#' The `status -> abbr` map from [cat_lookup()]: the compact codes (for
#' example `"<50% cov, >=2t"`) rather than the full sentences, for
#' `ggplot2::scale_fill_manual(labels = )` or any legend that has to stay
#' narrow.
#'
#' @inheritParams cat_lookup
#' @return A character vector of short codes named by full status label.
#' @seealso [status_palette()], [cat_lookup()]
#' @md
#' @examples
#' status_abbr(50, "discard")
#' @export
status_abbr <- function(threshold, what = "discard", min_meas = 0,
                        min_trips = 2) {
  lk <- cat_lookup(threshold, what, min_meas, min_trips)
  setNames(lk$abbr, lk$status)
}

#' Build the stratum table from CEF catch data
#'
#' @description
#' Aggregates a long CEF catch table to one row per stratum, in tonnes: the
#' landings, the landings that carry a discard estimate, the discards, the
#' resulting coverage, and the discard rate. This is the raising-side stratum
#' table - what [classify_strata()], the grouping functions and
#' [recommend_raising()] work on.
#'
#' To look at discard rates and sampling coverage - as a table or in a
#' figure - use [build_coverage_strata()] instead: the same numbers under the
#' CAR / CAS column names, with `rate` = discards / covered landings, and no
#' raising vocabulary.
#'
#' @details
#' Landings and discards are summed separately by catch category, so a
#' stratum with landings but no observed discards reads as 0 percent covered
#' rather than going missing. Every catch category other than landings is
#' counted as discards.
#'
#' `discard_rate` uses the stratum's own total landings
#' (`discards / (landings + discards)`); `discard_rate_covered` uses only the
#' covered landings (`discards / (landings_with_discards + discards)`), the
#' definition a donor pool is raised with. At full resolution the two
#' coincide whenever a stratum is either fully covered or not covered at all.
#'
#' | Column | Meaning |
#' |:-------|:--------|
#' | the `dims` columns | The stratum. |
#' | `landings` | All landings, in tonnes. |
#' | `landings_with_discards` | The landings whose `domainCatchDis` is filled: the numerator of coverage. |
#' | `discards` | All non-landing catch, in tonnes. |
#' | `has_disc_data` | Whether the stratum has any discard record at all. |
#' | `coverage` | `landings_with_discards / landings`, in percent. |
#' | `discard_rate` | `discards / (landings + discards)`. |
#' | `discard_rate_covered` | `discards / (landings_with_discards + discards)`. |
#' | `stratum_id` | The `dims` values pasted with `" | "`, in `dims` order: the key the raising conditions are written against. |
#' | `n_measurements` | Distribution records linked to the stratum through `domainBiology`; 0 when `dist` is not given. |
#' | `numTrips` | The largest trip count among the stratum's rows (`NA` when not reported). |
#'
#' @param catch_data A long CEF catch table. Must contain `catchCategory`,
#'   `total`, `domainCatchDis` and every column named in `dims`; a
#'   `variableUnit` column is used for unit conversion when present.
#' @param dims Character vector of the columns that define a stratum (for
#'   example `c("metier4", "Country", "Season")`).
#' @param dist Optional CEF distributions table, used only to fill
#'   `n_measurements`.
#' @return A data frame, one row per stratum, with the columns above.
#' @seealso [build_coverage_strata()] for exploring discard rates and
#'   coverage; [classify_strata()] for the sampling verdict.
#' @md
#' @examples
#' data(anchoveta_catch)
#' st <- build_strata(anchoveta_catch, c("metier4", "Country"))
#' head(st[, c("metier4", "Country", "landings", "discards", "coverage",
#'             "discard_rate")])
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
#' @description
#' Adds the sampling verdict to a stratum table: `cat_key` (a stable key),
#' `Status` (a factor with every level from [cat_lookup()], so legends stay
#' complete) and `Raising_Decision` (what to do about the stratum). This is
#' the raising side's verdict, used to decide which strata raise on their own
#' data and which must borrow. The coverage plots judge cells the same way, at
#' plot time, from the table [build_coverage_strata()] returns.
#'
#' @details
#' Coverage is always part of the verdict; `min_trips` (discards, length /
#' age) and `min_meas` (length / age only) are each independently optional -
#' pass `NULL` to drop that sampling-effort question from the classification
#' entirely rather than merely giving it an always-passing threshold. The
#' warning shade counts how many of the active targets a stratum missed
#' (yellow = 1, orange = 2, dark orange = 3 - reachable only with both trips
#' and measurements active). [cat_lookup()] lists every category.
#'
#' A stratum with no reported trip count fails an active trips test however
#' low `min_trips` is set - unknown is not the same as adequate - and the
#' function warns when that leaves the trips question unable to discriminate
#' at all (every stratum's `numTrips` is `NA`). It likewise warns when, for
#' length / age, a positive `min_meas` can never be met because every
#' stratum's `n_measurements` is 0.
#'
#' @param s A stratum table from [build_strata()] (discards) or from the
#'   biology link (length / age), carrying at least `coverage`. Length / age
#'   classification also reads `n_measurements`. `numTrips` and
#'   `n_measurements` are only read when `min_trips` / `min_meas` are active.
#' @param threshold Coverage threshold, in percent.
#' @param what `"discard"` (default), `"length"` or `"age"`.
#' @param min_meas Minimum number of measurements for a length / age stratum
#'   to count as well measured, or `NULL` to leave measurements out of the
#'   classification. Ignored for discards.
#' @param min_trips Minimum number of trips, or `NULL` to leave trips out of
#'   the classification. Defaults to 2.
#' @return `s` with `cat_key`, `Status` and `Raising_Decision` added.
#' @seealso [build_strata()], [cat_lookup()], [build_coverage_strata()]
#' @md
#' @examples
#' data(anchoveta_catch)
#' st <- classify_strata(build_strata(anchoveta_catch,
#'                                    c("metier4", "Country")),
#'                       threshold = 50)
#' table(st$cat_key)
#' head(st[, c("metier4", "Country", "coverage", "cat_key")])
#'
#' # coverage only: trips dropped from the verdict
#' table(classify_strata(build_strata(anchoveta_catch, "metier4"), 50,
#'                       min_trips = NULL)$cat_key)
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
  # min_trips being LOW does not make the trips test easier to pass when
  # numTrips is unknown everywhere - an unreported trip count fails an
  # active threshold no matter how lenient it is (min_trips = 0 included).
  # A caller who set min_trips expecting it to discriminate, on data that
  # never carries a trip count at all, would otherwise see the exact same
  # result at every threshold and have no way to tell why.
  if (trip_active && nrow(s) && all(is.na(s$numTrips)))
    warning("classify_strata(): min_trips is active but every stratum's ",
            "numTrips is unknown (NA) - none of them can pass the trips ",
            "test, at any min_trips value including 0. Pass min_trips = ",
            "NULL to drop trips from the classification, or check that ",
            "the data actually carries a trip count.", call. = FALSE)

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
    # same nudge as the trips one above: a positive min_meas that no
    # stratum can EVER meet, because n_measurements is 0 everywhere, is
    # far more likely a data gap (no measurement counts came through)
    # than every stratum genuinely having zero measurements.
    if (meas_active && isTRUE(min_meas > 0) && nrow(s) && all(nm == 0))
      warning("classify_strata(): min_meas is active but every stratum's ",
              "n_measurements is 0 - none of them can pass the ",
              "measurements test, at any min_meas value above 0. Pass ",
              "min_meas = NULL to drop measurements from the ",
              "classification, or check that the data actually carries ",
              "measurement counts.", call. = FALSE)
    cov_ok  <- !is.na(s$coverage) & s$coverage >= threshold
    meas_ok <- if (meas_active) nm >= min_meas else rep(TRUE, nrow(s))
    n_miss  <- (!cov_ok) + (if (trip_active) (!trip_ok) else 0) +
      (if (meas_active) (!meas_ok) else 0)
    is_full <- cov_ok & trip_ok & meas_ok & !is.na(s$coverage) & s$coverage >= 100 &
      (if (trip_active) trips2 > min_trips else TRUE)
    # one "warn_cov"/"warn_trip"/"warn_cov_meas"/... key per row, matching
    # exactly the rows cat_lookup() pre-declared via its combn() walk
    combo <- do.call(combo_key, c(list(cov = !cov_ok),
                                  if (trip_active) list(trip = !trip_ok),
                                  if (meas_active) list(meas = !meas_ok)))
    return(s %>%
             mutate(
               n_measurements = nm,
               cat_key = case_when(
                 no_catch    ~ "none",
                 !has_dist   ~ "no_dist",
                 is_full     ~ "full",
                 n_miss == 0 ~ "safe",
                 TRUE        ~ combo
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
  combo <- do.call(combo_key, c(list(cov = !cov_ok),
                                if (trip_active) list(trip = !trip_ok)))
  s %>%
    mutate(
      cat_key = case_when(
        landings <= 0 & discards <= 0 ~ "none",
        landings <= 0                  ~ "disc_only",
        !has_disc                      ~ "no_dist",
        is_full                        ~ "full",
        n_miss == 0                    ~ "safe",
        TRUE                           ~ combo
      ),
      Status = factor(lk$status[match(cat_key, lk$cat_key)],
                      levels = unique(lk$status)),
      Raising_Decision = factor(lk$decision[match(cat_key, lk$cat_key)],
                                levels = unique(lk$decision))
    )
}
