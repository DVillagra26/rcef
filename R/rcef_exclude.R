# =====================================================================
#  rcef_exclude.R   (keeping strata out of a donor pool)
#
#  ORIGIN: this project.
#
#  Three routes to the same place, used by the discard cascade, the
#  discard manual rules, the biology cascade and the biology manual
#  rules alike:
#
#    by name          - pick strata out of a list, expert judgement
#    by metric        - a threshold on coverage, measurements or
#                       discard rate
#    (a catch-all)    - the same two, applied to the last-resort pool
#
#  All of them end as a set of STRATUM KEYS. That matters: a condition
#  passed to catchRaising() or catchAllocationAoL() is evaluated against
#  the CEF rows, which carry no `coverage` column - coverage is a
#  property of a stratum, computed from many rows. So a threshold is
#  resolved here, against the stratum table, and emitted as an explicit
#  list of keys. What the engine receives is always a plain
#  `!paste(...) %in% c(...)`, whatever route produced it.
#
#  Exclusion narrows the DONOR pool only. It never removes a stratum
#  from the set being raised or allocated: a stratum that is too thin to
#  lend its data is exactly the one that most needs to borrow.
# =====================================================================

#' The joined key of every stratum row
#'
#' `" | "` is the separator `build_strata()` uses for `stratum_id`, and
#' it must stay that: `paste(a, " | ", b)` would add its own spaces and
#' never match a stored key.
#'
#' @param strata A stratum table.
#' @param dims The key columns.
#' @return A character vector, one key per row.
#' @export
#' @examples
#' stratum_keys(data.frame(a = c("x", "y"), b = 1:2), c("a", "b"))
stratum_keys <- function(strata, dims) {
  d <- as.data.frame(strata)
  present <- intersect(dims, names(d))
  if (!length(present)) return(rep(NA_character_, nrow(d)))
  do.call(paste, c(lapply(present, function(k) as.character(d[[k]])),
                   sep = " | "))
}

#' What is known about each stratum, for exclusion decisions
#'
#' Aggregated to one row per key, because the exclusion list is per
#' stratum and a table at a finer resolution would offer the same key
#' several times with different numbers.
#'
#' Weight sums; the rates are weighted by it where a weight exists, so a
#' 3 t stratum does not drag the mean of a 3000 t one.
#'
#' @param strata A stratum table.
#' @param dims The key columns.
#' @return A data frame with `key`, and whichever of `weight`,
#'   `coverage`, `discard_rate`, `dl_ratio` (discards / landings) and
#'   `n_measurements` could be computed.
#' @seealso [exclusion_choices()], [excluded_keys()]
#' @export
stratum_metrics <- function(strata, dims) {
  d <- as.data.frame(strata)
  if (!nrow(d)) return(NULL)
  key <- stratum_keys(d, dims)
  if (all(is.na(key))) return(NULL)

  col <- function(nm) if (nm %in% names(d)) as.numeric(d[[nm]]) else NULL
  w <- col("landings")
  if (is.null(w)) w <- col("weight")
  if (is.null(w)) w <- rep(1, nrow(d))
  w[is.na(w)] <- 0

  wmean <- function(x) {
    if (is.null(x)) return(NA_real_)
    ok <- !is.na(x)
    if (!any(ok)) return(NA_real_)
    if (sum(w[ok]) > 0) sum(x[ok] * w[ok]) / sum(w[ok]) else mean(x[ok])
  }

  parts <- split(seq_len(nrow(d)), key)
  cov <- col("coverage"); dr <- col("discard_rate"); nm <- col("n_measurements")
  tr  <- col("numTrips")
  # D/L: discards over landings, the plain ratio some people read more
  # naturally than the discard rate (which divides by landings PLUS
  # discards). A ratio of sums, not a weighted mean of ratios, so a
  # stratum's D/L is its own total discards over its own total landings.
  # Only where both columns exist - a biology link carries no discards.
  dis <- col("discards"); lan <- col("landings")

  out <- data.frame(
    key = names(parts),
    weight = vapply(parts, function(i) sum(w[i], na.rm = TRUE), numeric(1)),
    coverage = vapply(parts, function(i) {
      if (is.null(cov)) NA_real_ else {
        ww <- w[i]; xx <- cov[i]; ok <- !is.na(xx)
        if (!any(ok)) NA_real_
        else if (sum(ww[ok]) > 0) sum(xx[ok] * ww[ok]) / sum(ww[ok])
        else mean(xx[ok]) } }, numeric(1)),
    discard_rate = vapply(parts, function(i) {
      if (is.null(dr)) NA_real_ else {
        ww <- w[i]; xx <- dr[i]; ok <- !is.na(xx)
        if (!any(ok)) NA_real_
        else if (sum(ww[ok]) > 0) sum(xx[ok] * ww[ok]) / sum(ww[ok])
        else mean(xx[ok]) } }, numeric(1)),
    dl_ratio = vapply(parts, function(i) {
      if (is.null(dis) || is.null(lan)) NA_real_ else {
        L <- sum(lan[i], na.rm = TRUE)
        if (L > 0) sum(dis[i], na.rm = TRUE) / L else NA_real_ } }, numeric(1)),
    n_measurements = vapply(parts, function(i)
      if (is.null(nm)) NA_real_ else sum(nm[i], na.rm = TRUE), numeric(1)),
    # Summed across the DIFFERENT strata being pooled here - unlike
    # build_strata()'s max(), which guards against one trip's count
    # being repeated across several RAW ROWS of that same stratum.
    # Different strata are, by definition, different data; pooling two
    # of them combines genuinely separate trips, so their effort adds.
    numTrips = vapply(parts, function(i)
      if (is.null(tr)) NA_real_ else {
        x <- tr[i]; if (!any(!is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
      }, numeric(1)),
    stringsAsFactors = FALSE)
  rownames(out) <- NULL
  out
}

#' Choices for a stratum-exclusion dropdown
#'
#' The label carries the numbers the decision is made on, so the list can
#' be read and acted on without cross-referencing a table beside it.
#' Whichever of coverage, discard rate (DR), discards / landings (D/L),
#' measurements and weight are available appear; the rest are left out rather than shown as blanks.
#'
#' @param strata A stratum table.
#' @param dims The key columns.
#' @param order_by `"weight"` (heaviest first, the default),
#'   `"coverage"` (thinnest first - the ones most likely to be excluded),
#'   `"rate"` (highest discard rate first), `"dl"` (highest
#'   discards / landings first), `"measurements"` or `"trips"`.
#' @param show Which metrics to put in the label. Defaults to all that
#'   are present.
#' @return A named character vector: values are the keys, names are the
#'   labels. Suitable for `selectizeInput(choices = )`.
#' @seealso [excluded_keys()], [exclusion_overview()]
#' @export
exclusion_choices <- function(strata, dims,
                              order_by = c("weight", "coverage", "rate", "dl",
                                           "measurements", "trips"),
                              show = c("coverage", "rate", "dl", "measurements",
                                       "weight", "trips")) {
  order_by <- match.arg(order_by)
  m <- stratum_metrics(strata, dims)
  if (is.null(m) || !nrow(m)) return(character(0))

  has <- function(x) any(!is.na(m[[x]]))
  ord <- switch(order_by,
    weight       = order(-m$weight),
    coverage     = order(m$coverage, na.last = TRUE),
    rate         = order(-m$discard_rate, na.last = TRUE),
    dl           = order(-m$dl_ratio, na.last = TRUE),
    measurements = order(m$n_measurements, na.last = TRUE),
    trips        = order(m$numTrips, na.last = TRUE))
  m <- m[ord, , drop = FALSE]

  num <- function(x, digits = 1, suffix = "")
    ifelse(is.finite(x), paste0(round(x, digits), suffix), "-")

  bits <- list()
  if ("coverage" %in% show && has("coverage"))
    bits$cov <- paste0("cov ", num(m$coverage, 0, "%"))
  if ("rate" %in% show && has("discard_rate"))
    bits$dr <- paste0("DR ", num(m$discard_rate * 100, 1, "%"))
  if ("dl" %in% show && has("dl_ratio"))
    bits$dl <- paste0("D/L ", num(m$dl_ratio * 100, 1, "%"))
  if ("measurements" %in% show && has("n_measurements"))
    bits$ms <- paste0(num(m$n_measurements, 0), " meas")
  if ("trips" %in% show && has("numTrips"))
    bits$tr <- paste0(num(m$numTrips, 0), " trips")
  if ("weight" %in% show)
    bits$w <- paste0(num(m$weight, 1), " t")

  lab <- if (length(bits))
    sprintf("%s   (%s)", m$key,
            do.call(paste, c(bits, sep = ", ")))
  else m$key
  stats::setNames(m$key, lab)
}

#' Which strata are out of the donor pool, and why
#'
#' Every threshold is a MINIMUM to stay in, except the discard-rate
#' window, which is a range. `NA` switches a test off.
#'
#' @param strata A stratum table.
#' @param dims The key columns.
#' @param named Keys picked out of the dropdown by hand.
#' @param cov_min Minimum coverage, percent. Below this the stratum is
#'   too thinly sampled to lend its data.
#' @param meas_min Minimum number of measurements.
#' @param dr_min,dr_max Discard-rate window; outside it the stratum is
#'   excluded.
#' @param wt_min Minimum weight. Chiefly useful to drop the zero-weight
#'   strata, which carry no data to lend.
#' @param m A pre-computed `stratum_metrics(strata, dims)`, for a caller
#'   that already has one and would otherwise pay for it twice. `NULL`
#'   (the default) computes it here, as before.
#' @return A data frame with one row per excluded stratum: `key`, the
#'   metrics, and a `reason` naming every test it failed.
#' @seealso [excluded_keys()]
#' @export
exclusion_overview <- function(strata, dims, named = character(0),
                               cov_min = NA, meas_min = NA,
                               dr_min = NA, dr_max = NA, wt_min = NA,
                               trips_min = NA, m = NULL) {
  # m lets a caller that already has stratum_metrics(strata, dims) - like
  # donor_pool_summary(), which needs its own copy regardless - pass it
  # straight through instead of this walking the strata table a second
  # time. Every existing caller still just gets `strata`/`dims` computed
  # for it, unchanged.
  if (is.null(m)) m <- stratum_metrics(strata, dims)
  if (is.null(m) || !nrow(m)) return(NULL)

  fail <- list(
    named        = m$key %in% named,
    coverage     = !is.na(cov_min)  & !is.na(m$coverage) & m$coverage < cov_min,
    measurements = !is.na(meas_min) & !is.na(m$n_measurements) &
                     m$n_measurements < meas_min,
    trips        = !is.na(trips_min) & !is.na(m$numTrips) &
                     m$numTrips < trips_min,
    rate         = (!is.na(dr_min) & !is.na(m$discard_rate) &
                      m$discard_rate < dr_min) |
                   (!is.na(dr_max) & !is.na(m$discard_rate) &
                      m$discard_rate > dr_max),
    weight       = !is.na(wt_min) & m$weight < wt_min)
  fail <- lapply(fail, function(x) { x[is.na(x)] <- FALSE; x })

  word <- c(named = "named", coverage = "below coverage threshold",
            measurements = "too few measurements",
            trips = "too few trips",
            rate = "outside rate window", weight = "no weight")
  any_fail <- Reduce(`|`, fail)
  if (!any(any_fail)) return(NULL)

  reason <- vapply(which(any_fail), function(i)
    paste(word[names(fail)[vapply(fail, function(f) f[i], logical(1))]],
          collapse = " + "), character(1))

  out <- m[any_fail, , drop = FALSE]
  out$reason <- unname(reason)
  out <- out[order(-out$weight), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' The keys to drop from a donor pool
#'
#' The single answer the app and the emitted script both use, so the
#' table on screen and the condition in the script can never disagree.
#'
#' @inheritParams exclusion_overview
#' @return A character vector of stratum keys; empty if nothing is out.
#' @seealso [exclusion_overview()], [donor_exclusion_expr()]
#' @export
#' @examples
#' s <- data.frame(a = c("x", "y"), landings = c(10, 10),
#'                 coverage = c(80, 10))
#' excluded_keys(s, "a", cov_min = 50)
excluded_keys <- function(strata, dims, named = character(0),
                          cov_min = NA, meas_min = NA,
                          dr_min = NA, dr_max = NA, wt_min = NA) {
  ov <- exclusion_overview(strata, dims, named, cov_min, meas_min,
                           dr_min, dr_max, wt_min)
  if (is.null(ov) || !nrow(ov)) return(character(0))
  ov$key
}

#' A human sentence describing an exclusion setting
#'
#' Used in the note under the control and as a comment in the emitted
#' script, so both say the same thing.
#'
#' @inheritParams exclusion_overview
#' @param n_named How many strata were picked by hand.
#' @return A length-one character vector.
#' @export
#' @examples
#' exclusion_label(cov_min = 40, meas_min = 25, n_named = 3)
exclusion_label <- function(cov_min = NA, meas_min = NA, dr_min = NA,
                            dr_max = NA, wt_min = NA, n_named = 0) {
  bits <- character(0)
  if (!is.na(cov_min))  bits <- c(bits, sprintf("coverage below %s%%", cov_min))
  if (!is.na(meas_min)) bits <- c(bits, sprintf("fewer than %s measurements",
                                                meas_min))
  if (!is.na(dr_min) || !is.na(dr_max))
    bits <- c(bits, sprintf("discard rate outside %s-%s",
                            if (is.na(dr_min)) "0" else dr_min,
                            if (is.na(dr_max)) "1" else dr_max))
  if (!is.na(wt_min))   bits <- c(bits, sprintf("weight below %s t", wt_min))
  if (n_named > 0)      bits <- c(bits, sprintf("%d named stratum/strata",
                                                n_named))
  if (!length(bits)) "nothing excluded" else paste(bits, collapse = "; ")
}

# ---------------------------------------------------------------------
#  Exclusion as a READABLE condition
# ---------------------------------------------------------------------
# The first version resolved every threshold into a list of stratum keys
# and emitted `!paste(...) %in% c(<236 strings>)`. It was correct and it
# was unusable: a reader could not tell a 50% coverage bar from a hand
# picked list, and the line ran to eight thousand characters.
#
# The fix is to put the metrics ON the data instead. add_stratum_metrics()
# joins coverage, discard rate and measurement counts onto the CEF rows,
# keyed by stratum, so a condition can simply say `coverage >= 50`.
# catchRaising() and catchAllocationAoL() both evaluate their conditions
# with filter(!!cond) against the data frame they are given, so any column
# present there is fair game.
#
# Hand-picked strata still have to be named - there is no metric behind
# "the WG decided this one is unreliable" - but that list is short by
# construction, because it is a human decision.

#' Attach stratum-level metrics to a CEF table
#'
#' Adds `coverage`, `discard_rate`, `stratum_weight` and, where the
#' distributions are supplied, `n_measurements` to every row, computed
#' over the stratum that row belongs to. This is what lets a raising or
#' allocation condition read `coverage >= 50` instead of carrying a list
#' of several hundred stratum keys.
#'
#' Call it once, after `derive_fields()` and before building conditions.
#' Existing columns of the same name are overwritten, so re-running it
#' after changing `dims` is safe and is in fact required: the metrics are
#' only meaningful at the resolution they were computed for.
#'
#' @param data A CEF long-format table.
#' @param dims The stratum key columns.
#' @param strata Optional pre-built stratum table from [build_strata()].
#'   Supply it to keep one definition of coverage across the app;
#'   otherwise it is built here.
#' @return `data` with the metric columns added, plus `stratum_id`.
#' @seealso [exclusion_expr()], [build_strata()]
#' @export
add_stratum_metrics <- function(data, dims, strata = NULL, dist = NULL) {
  if (is.null(data) || !nrow(data)) return(data)
  if (is.null(strata)) strata <- build_strata(data, dims, dist = dist)
  key <- stratum_keys(data, dims)
  m <- stratum_metrics(strata, dims)
  if (is.null(m)) return(data)
  i <- match(key, m$key)
  data$stratum_id     <- key
  data$stratum_weight <- m$weight[i]
  data$coverage       <- m$coverage[i]
  data$discard_rate   <- m$discard_rate[i]
  # Always present, 0 where no distributions were supplied. A condition
  # naming n_measurements must find the column even when it is all zero,
  # or it errors the same way a missing coverage column does.
  data$n_measurements <- if ("n_measurements" %in% names(m))
    ifelse(is.na(m$n_measurements[i]), 0, m$n_measurements[i]) else 0
  # Same convention: always present, so a condition naming numTrips does
  # not error on a stratum where no numTrips was ever reported. Unlike
  # n_measurements, 0 here is not "off" - a real 0-trip stratum and an
  # unreported one both read as 0, on the same "unknown is not adequate"
  # basis the classification takes.
  data$numTrips <- if ("numTrips" %in% names(m))
    ifelse(is.na(m$numTrips[i]), 0, m$numTrips[i]) else 0
  data
}

#' A donor-exclusion condition, written the way it was decided
#'
#' Thresholds come out as comparisons on the metric columns, which is
#' short, self-explaining and survives a change in the data. Only
#' hand-picked strata are named, and that list is short because a person
#' typed it.
#'
#' @param cov_min Minimum coverage, percent. `NA` to omit.
#' @param meas_min Minimum measurements. `NA` to omit.
#' @param dr_min,dr_max Discard-rate window. `NA` to omit either bound.
#' @param named Stratum keys picked out by hand.
#' @param dims Key columns, needed only if `named` is non-empty.
#' @return A length-one character vector, or `NULL` if nothing is set.
#' @seealso [add_stratum_metrics()], [excluded_keys()]
#' @export
#' @examples
#' exclusion_expr(cov_min = 50, dr_max = 0.9)
#' exclusion_expr(named = "TBB | 27.4.b", dims = c("metier4", "ICESDivision"))
exclusion_expr <- function(cov_min = NA, meas_min = NA, dr_min = NA,
                           dr_max = NA, named = character(0), dims = NULL,
                           trips_min = NA) {
  bits <- character(0)
  if (!is.na(cov_min))   bits <- c(bits, sprintf("coverage >= %s", cov_min))
  if (!is.na(meas_min))  bits <- c(bits, sprintf("n_measurements >= %s", meas_min))
  if (!is.na(trips_min)) bits <- c(bits, sprintf("numTrips >= %s", trips_min))
  if (!is.na(dr_min))   bits <- c(bits, sprintf("discard_rate >= %s", dr_min))
  if (!is.na(dr_max))   bits <- c(bits, sprintf("discard_rate <= %s", dr_max))
  if (length(named) && !is.null(dims))
    bits <- c(bits, donor_exclusion_expr(named, dims))
  if (!length(bits)) NULL else paste(bits, collapse = " & ")
}

#' Combine a clause with a donor-exclusion condition
#'
#' @param base The clause condition, or `"TRUE"`.
#' @param excl An exclusion condition from [exclusion_expr()], or `NULL`.
#' @return A length-one character vector.
#' @export
#' @examples
#' and_condition('metier4 %in% "TBB"', "coverage >= 50")
and_condition <- function(base, excl) {
  if (is.null(excl) || !nzchar(excl)) return(base)
  if (is.null(base) || !nzchar(base) || identical(base, "TRUE")) return(excl)
  paste0(base, " & ", excl)
}

#' Strata a rule would actually drop, within its own scope
#'
#' Two things the first version got wrong, both of which made the
#' "currently excluded" list unreadable:
#'
#' * It listed every stratum in the stock. If the group only raises TBB,
#'   being told that an OTB stratum is out of its donor pool is noise -
#'   that stratum was never in it.
#' * It listed strata with no landings and strata with zero coverage.
#'   Neither can donate a discard rate under any threshold, so they are
#'   not a decision anyone makes; they are arithmetic.
#'
#' @inheritParams exclusion_overview
#' @param scope Logical vector over `strata`, or `NULL` for all of it.
#'   Pass the group's own raise clause here.
#' @param drop_unusable Remove zero-weight and zero-coverage strata from
#'   the report. They are still excluded by the condition; they are just
#'   not worth listing.
#' @return A data frame as [exclusion_overview()], scoped and pruned.
#' @export
exclusion_report <- function(strata, dims, named = character(0),
                             cov_min = NA, meas_min = NA, dr_min = NA,
                             dr_max = NA, scope = NULL,
                             drop_unusable = TRUE, trips_min = NA) {
  d <- as.data.frame(strata)
  if (!is.null(scope)) {
    scope[is.na(scope)] <- FALSE
    if (length(scope) == 1L) scope <- rep(scope, nrow(d))
    d <- d[scope, , drop = FALSE]
  }
  if (!nrow(d)) return(NULL)
  ov <- exclusion_overview(d, dims, named, cov_min, meas_min, dr_min, dr_max,
                           trips_min = trips_min)
  if (is.null(ov) || !nrow(ov)) return(NULL)
  if (isTRUE(drop_unusable)) {
    keep <- !( (!is.na(ov$weight) & ov$weight <= 0) |
               (!is.na(ov$coverage) & ov$coverage <= 0) )
    ov <- ov[keep, , drop = FALSE]
  }
  if (!nrow(ov)) return(NULL)
  rownames(ov) <- NULL
  ov
}

#' The donor pool's own combined metrics, after every bar and named pick
#'
#' Every bar in an exclusion box is a per-stratum test - a stratum below
#' the coverage floor is dropped whole. What is left after every test is
#' the pool a group would actually borrow from, and that pool can still
#' look thin even when no individual stratum was thin enough to trip a
#' bar: three strata at 20%, 10% and 5% coverage clear a 0% (off) floor
#' individually and still combine to an 12% pool - exactly the "OTB + SDN
#' + SSC together only reach 25%" case a per-stratum bar cannot catch by
#' construction, because it is never asked the question.
#'
#' `warn_cov_min` / `warn_meas_min` are that question, and deliberately
#' separate from `cov_min` / `meas_min` (the per-stratum floor that built
#' the pool in the first place): a pool built with no per-stratum bar at
#' all can still be checked against "is this enough to call adequately
#' sampled", using the same reference the workbench's sampling-status
#' colours use elsewhere.
#'
#' @param strata A stratum table.
#' @param dims The key columns.
#' @param named,cov_min,meas_min,dr_min,dr_max,trips_min The per-stratum
#'   bars, as in [exclusion_overview()] - what built the pool.
#' @param warn_cov_min,warn_meas_min,warn_trips_min The pool-level sanity
#'   thresholds - what decides `ok`. `NA` switches that check off.
#'   Coverage defaults to 50%, the same "adequately sampled" reference
#'   used throughout the workbench; measurements and trips have no such
#'   universal reference (a discard pool has nothing to measure, and
#'   "enough trips" is stock-specific), so both default off.
#' @return A list: `n_strata`, `weight`, `coverage`, `discard_rate`,
#'   `dl_ratio`, `n_measurements`, and `ok` (does the POOL clear the sanity bars).
#'   `NULL` if there is nothing to summarise.
#' @seealso [exclusion_overview()], [stratum_metrics()]
#' @export
donor_pool_summary <- function(strata, dims, named = character(0),
                               cov_min = NA, meas_min = NA,
                               dr_min = NA, dr_max = NA,
                               warn_cov_min = 50, warn_meas_min = NA,
                               trips_min = NA, warn_trips_min = NA) {
  m <- stratum_metrics(strata, dims)
  if (is.null(m) || !nrow(m)) return(NULL)
  excl <- exclusion_overview(strata, dims, named, cov_min, meas_min,
                             dr_min, dr_max, trips_min = trips_min, m = m)
  kept <- if (is.null(excl)) m else m[!m$key %in% excl$key, , drop = FALSE]
  if (!nrow(kept))
    return(list(n_strata = 0L, weight = 0, coverage = NA_real_,
               discard_rate = NA_real_, dl_ratio = NA_real_,
               n_measurements = 0, numTrips = 0, ok = FALSE))

  w <- kept$weight; w[is.na(w)] <- 0
  wmean <- function(x) {
    ok <- !is.na(x)
    if (!any(ok)) return(NA_real_)
    if (sum(w[ok]) > 0) sum(x[ok] * w[ok]) / sum(w[ok]) else mean(x[ok])
  }
  cov <- wmean(kept$coverage)
  dr  <- wmean(kept$discard_rate)
  # pooled D/L: every stratum's weight IS its landings, so weighting the
  # per-stratum ratios by it gives sum(discards) / sum(landings) exactly
  dl  <- if ("dl_ratio" %in% names(kept)) wmean(kept$dl_ratio) else NA_real_
  nmeas <- sum(kept$n_measurements, na.rm = TRUE)
  # Summed, not weight-weighted: numTrips is an EFFORT count (how many
  # trips back the pool), not a rate, so the pool's total is the sum of
  # what each surviving stratum brought - the same convention
  # stratum_metrics() uses when pooling across different strata.
  ntrips <- if (!"numTrips" %in% names(kept) || !any(!is.na(kept$numTrips)))
    NA_real_ else sum(kept$numTrips, na.rm = TRUE)

  ok <- (is.na(warn_cov_min)   || is.na(cov)    || cov    >= warn_cov_min)   &&
        (is.na(warn_meas_min)  || is.na(nmeas)  || nmeas  >= warn_meas_min)  &&
        (is.na(warn_trips_min) || is.na(ntrips) || ntrips >= warn_trips_min)
  list(n_strata = nrow(kept), weight = sum(kept$weight, na.rm = TRUE),
       coverage = cov, discard_rate = dr, dl_ratio = dl,
       n_measurements = nmeas, numTrips = ntrips, ok = ok)
}
