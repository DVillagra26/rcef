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
#' @noRd
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
#' @seealso [exclusion_choices()]
#' @export
#' @noRd
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
#' @seealso [exclusion_overview()]
#' @export
#' @noRd
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
#' @export
#' @noRd
exclusion_overview <- function(strata, dims, named = character(0),
                               cov_min = NA, meas_min = NA,
                               dr_min = NA, dr_max = NA, wt_min = NA,
                               trips_min = NA, m = NULL) {
  # m lets a caller that already has stratum_metrics(strata, dims) pass it
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
#' @noRd
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
#' @seealso [add_stratum_metrics()]
#' @export
#' @examples
#' exclusion_expr(cov_min = 50, dr_max = 0.9)
#' exclusion_expr(named = "TBB | 27.4.b", dims = c("metier4", "ICESDivision"))
#' @noRd
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
#' @noRd
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
