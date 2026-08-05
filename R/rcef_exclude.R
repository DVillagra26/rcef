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
#'   `coverage`, `discard_rate` and `n_measurements` could be computed.
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
    n_measurements = vapply(parts, function(i)
      if (is.null(nm)) NA_real_ else sum(nm[i], na.rm = TRUE), numeric(1)),
    stringsAsFactors = FALSE)
  rownames(out) <- NULL
  out
}

#' Choices for a stratum-exclusion dropdown
#'
#' The label carries the numbers the decision is made on, so the list can
#' be read and acted on without cross-referencing a table beside it.
#' Whichever of coverage, discard rate, measurements and weight are
#' available appear; the rest are left out rather than shown as blanks.
#'
#' @param strata A stratum table.
#' @param dims The key columns.
#' @param order_by `"weight"` (heaviest first, the default),
#'   `"coverage"` (thinnest first - the ones most likely to be excluded),
#'   `"rate"` (highest discard rate first) or `"measurements"`.
#' @param show Which metrics to put in the label. Defaults to all that
#'   are present.
#' @return A named character vector: values are the keys, names are the
#'   labels. Suitable for `selectizeInput(choices = )`.
#' @seealso [excluded_keys()], [exclusion_overview()]
#' @export
exclusion_choices <- function(strata, dims,
                              order_by = c("weight", "coverage", "rate",
                                           "measurements"),
                              show = c("coverage", "rate", "measurements",
                                       "weight")) {
  order_by <- match.arg(order_by)
  m <- stratum_metrics(strata, dims)
  if (is.null(m) || !nrow(m)) return(character(0))

  has <- function(x) any(!is.na(m[[x]]))
  ord <- switch(order_by,
    weight       = order(-m$weight),
    coverage     = order(m$coverage, na.last = TRUE),
    rate         = order(-m$discard_rate, na.last = TRUE),
    measurements = order(m$n_measurements, na.last = TRUE))
  m <- m[ord, , drop = FALSE]

  num <- function(x, digits = 1, suffix = "")
    ifelse(is.finite(x), paste0(round(x, digits), suffix), "-")

  bits <- list()
  if ("coverage" %in% show && has("coverage"))
    bits$cov <- paste0("cov ", num(m$coverage, 0, "%"))
  if ("rate" %in% show && has("discard_rate"))
    bits$dr <- paste0("DR ", num(m$discard_rate * 100, 1, "%"))
  if ("measurements" %in% show && has("n_measurements"))
    bits$ms <- paste0(num(m$n_measurements, 0), " meas")
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
#' @return A data frame with one row per excluded stratum: `key`, the
#'   metrics, and a `reason` naming every test it failed.
#' @seealso [excluded_keys()]
#' @export
exclusion_overview <- function(strata, dims, named = character(0),
                               cov_min = NA, meas_min = NA,
                               dr_min = NA, dr_max = NA, wt_min = NA) {
  m <- stratum_metrics(strata, dims)
  if (is.null(m) || !nrow(m)) return(NULL)

  fail <- list(
    named        = m$key %in% named,
    coverage     = !is.na(cov_min)  & !is.na(m$coverage) & m$coverage < cov_min,
    measurements = !is.na(meas_min) & !is.na(m$n_measurements) &
                     m$n_measurements < meas_min,
    rate         = (!is.na(dr_min) & !is.na(m$discard_rate) &
                      m$discard_rate < dr_min) |
                   (!is.na(dr_max) & !is.na(m$discard_rate) &
                      m$discard_rate > dr_max),
    weight       = !is.na(wt_min) & m$weight < wt_min)
  fail <- lapply(fail, function(x) { x[is.na(x)] <- FALSE; x })

  word <- c(named = "named", coverage = "below coverage threshold",
            measurements = "too few measurements",
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
