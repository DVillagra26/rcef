# =====================================================================
#  rcef_filter.R   (the exploration lens)
#
#  ORIGIN: this project.
#
#  A filter here narrows what a FIGURE shows. It does not narrow what
#  gets raised. Those are different questions and conflating them is how
#  a stock gets raised on a third of its data without anyone noticing:
#  the app keeps `dat()` for the engines and `explore_dat()` for the
#  plots, and this file is the machinery behind the second one.
#
#  The intended loop is: filter to one metier4, read the coverage and
#  discard-rate figures for it alone, learn where that fleet is thin,
#  THEN write that understanding into a group definition - which is
#  applied to the full table.
# =====================================================================

#' The value meaning "do not filter on this column"
#'
#' Held as a constant because it is written into `selectInput()` choices
#' in the app and compared against here; two spellings of it would make
#' a filter silently do nothing.
#'
#' @format A length-one character vector.
#' @export
FILTER_ANY <- "(any)"

#' Values available for filtering, per column
#'
#' Offers only what is actually there: a column absent from the table, or
#' present with a single level, is not a filter anyone can use, and
#' showing it invites a selection that cannot change the figure.
#'
#' @param data A CEF long-format table.
#' @param vars Columns to consider. Defaults to [FILTER_VARS].
#' @param max_levels Columns with more distinct values than this are
#'   dropped. A free-text column with thousands of levels is not a filter,
#'   it is a search box, and rendering it as a dropdown freezes the browser.
#' @return A named list, one element per usable column, each a sorted
#'   character vector of values. Empty list if nothing is usable.
#' @seealso [apply_filters()], [filter_label()]
#' @export
#' @examples
#' d <- data.frame(metier4 = c("TBB", "OTB", "TBB"),
#'                 ICESDivision = c("27.4.b", "27.4.b", "27.4.c"))
#' explore_filter_levels(d, vars = c("metier4", "ICESDivision"))
explore_filter_levels <- function(data, vars = FILTER_VARS, max_levels = 200) {
  if (is.null(data) || !nrow(data)) return(list())
  vars <- intersect(vars, names(data))
  out <- list()
  for (v in vars) {
    lv <- unique(as.character(data[[v]]))
    lv <- sort(lv[!is.na(lv) & nzchar(lv)])
    if (length(lv) > 1 && length(lv) <= max_levels) out[[v]] <- lv
  }
  out
}

#' Drop the no-op entries from a filter list
#'
#' `NULL`, empty, and [FILTER_ANY] all mean "no restriction". Normalising
#' once here means every other function can assume that a name present in
#' the list is a name that actually restricts something.
#'
#' @param filters A named list of character vectors.
#' @return The same list with the inert entries removed.
#' @export
#' @examples
#' clean_filters(list(metier4 = "TBB", ICESDivision = FILTER_ANY, mesh = NULL))
clean_filters <- function(filters) {
  if (!length(filters)) return(list())
  keep <- vapply(filters, function(v) {
    v <- v[!is.na(v)]
    length(v) > 0 && any(nzchar(v)) && !all(v %in% FILTER_ANY)
  }, logical(1))
  lapply(filters[keep], function(v) setdiff(as.character(v), c(NA, "", FILTER_ANY)))
}

#' Is any filter actually restricting the data?
#'
#' @param filters A named list of character vectors.
#' @return `TRUE` if at least one column is restricted.
#' @export
#' @examples
#' filters_active(list(metier4 = FILTER_ANY))
#' filters_active(list(metier4 = "TBB"))
filters_active <- function(filters) length(clean_filters(filters)) > 0

#' Apply an exploration filter to a CEF table
#'
#' Columns are combined with AND, values within a column with OR: the
#' usual reading of "beam trawl, in either of these two divisions".
#'
#' A column named in `filters` but absent from `data` is an error, not a
#' silent pass. The alternative - quietly ignoring it - produces a figure
#' that looks filtered and is not, which is the worst possible outcome
#' for a plot someone is about to base a grouping decision on.
#'
#' @param data A CEF long-format table.
#' @param filters A named list: column name -> values to keep. Entries
#'   equal to [FILTER_ANY], empty, or `NULL` are ignored.
#' @param drop_empty If `TRUE` (default) an empty result is returned as
#'   is; the caller decides how to report it.
#' @return `data`, filtered. Carries an `rcef_filters` attribute holding
#'   the cleaned filter list, so a downstream plot or caption can say what
#'   it is showing without being passed the filters separately.
#' @seealso [explore_filter_levels()], [filter_label()]
#' @export
#' @examples
#' d <- data.frame(metier4 = c("TBB", "OTB", "TBB"), total = 1:3)
#' apply_filters(d, list(metier4 = "TBB"))
apply_filters <- function(data, filters, drop_empty = TRUE) {
  f <- clean_filters(filters)
  if (!length(f)) return(data)
  miss <- setdiff(names(f), names(data))
  if (length(miss))
    stop("apply_filters(): no such column(s) in the data: ",
         paste(miss, collapse = ", "), call. = FALSE)
  keep <- rep(TRUE, nrow(data))
  for (v in names(f)) keep <- keep & as.character(data[[v]]) %in% f[[v]]
  out <- data[keep, , drop = FALSE]
  attr(out, "rcef_filters") <- f
  out
}

#' One-line description of an active filter
#'
#' For plot subtitles, banners and script comments, so the same wording
#' appears in the figure, on screen and in the emitted code.
#'
#' @param filters A named list of character vectors.
#' @param none Text to return when nothing is filtered.
#' @param max_values Values per column beyond which the list is summarised
#'   as a count, to keep a subtitle from wrapping over three lines.
#' @return A length-one character vector.
#' @export
#' @examples
#' filter_label(list(metier4 = "TBB", ICESDivision = c("27.4.b", "27.4.c")))
#' filter_label(list())
filter_label <- function(filters, none = "no filter", max_values = 4) {
  f <- clean_filters(filters)
  if (!length(f)) return(none)
  paste(vapply(names(f), function(v) {
    vals <- f[[v]]
    txt <- if (length(vals) > max_values)
      sprintf("%d values", length(vals)) else paste(vals, collapse = ", ")
    sprintf("%s = %s", v, txt)
  }, character(1)), collapse = "; ")
}

#' How much of the data an exploration filter keeps
#'
#' A filtered figure is only worth reading if enough is left in it. This
#' reports rows and, when the column is present, weight - because a filter
#' can keep 40% of rows and 2% of the landings.
#'
#' @param data The unfiltered table.
#' @param filters A named list of character vectors.
#' @param weight Name of a weight column to total, or `NULL`.
#' @return A list with `n_all`, `n_kept`, `pct_rows` and, if `weight` was
#'   found, `w_all`, `w_kept` and `pct_weight`.
#' @export
#' @examples
#' d <- data.frame(metier4 = c("TBB", "OTB", "TBB"), total = c(1, 100, 1))
#' filter_coverage(d, list(metier4 = "TBB"), weight = "total")
filter_coverage <- function(data, filters, weight = "total") {
  sub <- apply_filters(data, filters)
  out <- list(n_all = nrow(data), n_kept = nrow(sub),
              pct_rows = if (nrow(data)) 100 * nrow(sub) / nrow(data) else NA_real_)
  if (!is.null(weight) && weight %in% names(data)) {
    wa <- sum(as.numeric(data[[weight]]), na.rm = TRUE)
    wk <- sum(as.numeric(sub[[weight]]), na.rm = TRUE)
    out$w_all <- wa; out$w_kept <- wk
    out$pct_weight <- if (wa > 0) 100 * wk / wa else NA_real_
  }
  out
}
