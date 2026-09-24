# =====================================================================
#  rcef_coverage.R   (build_coverage_strata() and what the plots do with it)
#
#  ORIGIN: this project (ILVO / RCEF Workbench). Freely refactorable.
#  No shiny in this file.
# =====================================================================

#' Sampling coverage and discard rate per stratum
#'
#' The table behind the coverage and discard-rate plots, usable on its
#' own: one row per stratum, at whatever aggregation `dims` names, with
#' the weights, the discard rate and how well the stratum is sampled. The
#' plots ([plot_coverage_heat()], [plot_coverage_bubble()],
#' [plot_coverage_bar()], [plot_coverage_map()], [plot_discard_rate()])
#' build exactly this table internally when given raw catch, and take it
#' directly when given the table - so the same numbers can be read as a
#' table and looked at as a figure.
#'
#' Nothing in the table judges the sampling: there is no status, decision
#' or category. Thresholds (`min_cov`, `min_trip`, `min_fish`) are a
#' plotting choice, applied by the coverage plots; the table only reports
#' the numbers they are applied to.
#'
#' The columns follow the final CAR / CAS output tables wherever those
#' have a counterpart, so a stratum here reads the same as its row there.
#' All weights are in tonnes.
#'
#' For `what = "discard"` the table has one row per stratum, the discard
#' estimate on the `Dis` side and the landings that weight it on the
#' `Lan` side:
#' \describe{
#'   \item{`dims`}{the stratum columns, as given.}
#'   \item{`catchCategory`, `variableUnit`}{`"Dis"` and `"t"` - what
#'     `total` is.}
#'   \item{`total`}{discards. `NA` when the stratum has no discard
#'     estimate at all (nobody looked), which is not the same as 0
#'     (looked and found none).}
#'   \item{`weightingCategory`}{`"Lan"`.}
#'   \item{`weightingTotal`}{the landings that carry a discard estimate
#'     (their `domainCatchDis` is filled) - the weight the rate is
#'     weighted by.}
#'   \item{`rate`}{`total / weightingTotal`, the discard rate as CAS
#'     reports it: discards per tonne of covered landings, so it can
#'     exceed 1. `NA` when either part is missing or zero.}
#'   \item{`landings`}{all landings in the stratum, covered or not.}
#'   \item{`coverage`}{`weightingTotal / landings`, in percent.}
#'   \item{`numTrips`}{the trips behind the stratum, `NA` when the data
#'     does not report them. The largest value among the rows of a
#'     stratum (the same trip count repeats across rows of one sample);
#'     summed when strata are pooled into a plot cell.}
#' }
#'
#' For `what = "length"` / `"age"` it has one row per stratum and catch
#' part (`catchCategory`), so `catchCategory = "All"` gives a `Lan` and a
#' `Dis` row for every stratum:
#' \describe{
#'   \item{`dims`, `catchCategory`}{the stratum and the catch part.}
#'   \item{`distributionType`}{`"LengthTotal"` or `"Age"`.}
#'   \item{`variableUnit`, `total`}{`"t"` and the weight of that catch
#'     part.}
#'   \item{`coverage`}{the share of `total`, in percent, whose
#'     `domainBiology` matches a distribution of that type.}
#'   \item{`numMeasurements`}{the measurements behind those
#'     distributions.}
#'   \item{`numTrips`}{as above.}
#' }
#'
#' @param catch CEF catch data (long format) carrying `catchCategory`,
#'   `total`, `domainCatchDis` and every column named in `dims`
#'   ([derive_fields()] adds `Country`, `Season`, `metier4`, `mesh` and
#'   the ICES area levels).
#' @param dims Character vector of the columns that define a stratum. The
#'   aggregation level is whatever you name: `c("metier4", "Country")`
#'   pools everything else, `c("fleetValue", "areaValue", "Season")` is
#'   finer. For length / age, `catchCategory` is never a stratum column
#'   (it is a column of the result, one row per catch part) and is
#'   ignored if named.
#' @param distributions A CEF distributions table - required for
#'   `what = "length"` / `"age"`, ignored for discards.
#' @param what `"discard"` (default), `"length"` or `"age"`. Plural and
#'   mixed case are accepted (`"discards"`, `"Length"`).
#' @param catchCategory For `what = "length"` / `"age"`: `"All"` (default -
#'   landings and discards, one row each), `"Lan"`, `"Dis"` or `"BMS"`.
#'   Not valid for `what = "discard"` (an error).
#' @return A data frame with the columns above, or `NULL` when there is
#'   nothing to describe (no landings / discards, or no catch rows for
#'   that `catchCategory`). Distributions that link to nothing are not
#'   "nothing": every stratum is then reported with coverage 0.
#' @seealso [plot_coverage_heat()] and the other [coverage-plots],
#'   [plot_discard_rate()], [build_strata()] (the raising-side stratum
#'   table)
#' @family coverage plots
#' @examples
#' \dontrun{
#' # discard rate and coverage per metier x country
#' cov <- build_coverage_strata(catch_data, c("metier4", "Country"))
#' cov[order(-cov$rate), ]
#'
#' # the same table, finer or coarser
#' build_coverage_strata(catch_data, c("fleetValue", "areaValue", "Season"))
#' build_coverage_strata(catch_data, "Season")
#'
#' # length coverage of the landings
#' build_coverage_strata(catch_data, c("metier4", "Country"), distributions,
#'                       what = "length", catchCategory = "Lan")
#'
#' # and the plots take either the raw catch or the table
#' plot_coverage_heat(cov, y = "metier4", x = "Country", min_cov = 50)
#' plot_discard_rate(cov, x = "metier4", colour = "Country")
#' }
#' @export
build_coverage_strata <- function(catch, dims, distributions = NULL,
                                  what = "discard", catchCategory = NULL) {
  fn   <- "build_coverage_strata"
  what <- normalize_what(what)
  catch <- as.data.frame(catch)
  dims <- unique(as.character(dims))
  dims <- dims[!is.na(dims) & nzchar(dims)]
  if (!length(dims))
    stop(fn, "(): `dims` names no column", call. = FALSE)
  miss <- setdiff(dims, names(catch))
  if (length(miss))
    stop(fn, "(): column(s) not in `catch`: ", paste(miss, collapse = ", "),
         call. = FALSE)
  # a build_strata() table (already one row per stratum) has none of these
  # and would otherwise come back as "nothing to describe"
  need <- setdiff(c("catchCategory", "total",
                    if (identical(what, "discard")) "domainCatchDis"),
                  names(catch))
  if (length(need))
    stop(fn, "(): `catch` needs the CEF catch columns ",
         paste(need, collapse = ", "), " - pass the catch data itself, not a ",
         "table already aggregated to strata (build_strata() output)",
         call. = FALSE)

  if (identical(what, "discard")) {
    if (!is.null(catchCategory))
      stop(fn, "(): catchCategory does not apply to what = \"discard\" ",
           "(discard coverage is the discard part by definition)",
           call. = FALSE)
    if ("catchCategory" %in% dims)
      stop(fn, "(): catchCategory cannot be a stratum column for discards - ",
           "the landings and discards of a stratum are its two halves",
           call. = FALSE)
    # landings and discards only: BMS is a third catch part with its own
    # raising, not a discard, and build_strata() would otherwise count it
    # as one (it treats every non-landing category as discards)
    d <- catch[catch$catchCategory %in% c("Lan", "Dis"), , drop = FALSE]
    if (!nrow(d)) return(NULL)
    st <- build_strata(d, dims)
    out <- st[, dims, drop = FALSE]
    out$catchCategory     <- "Dis"
    out$variableUnit      <- "t"
    out$total             <- ifelse(st$has_disc_data, st$discards, NA_real_)
    out$weightingCategory <- "Lan"
    out$weightingTotal    <- st$landings_with_discards
    out$rate              <- ifelse(st$has_disc_data &
                                      st$landings_with_discards > 0,
                                    st$discards / st$landings_with_discards,
                                    NA_real_)
    out$landings          <- st$landings
    out$coverage          <- st$coverage
    out$numTrips          <- st$numTrips
    return(out)
  }

  # length / age: the biology link, one catch part at a time so each row's
  # measurement count is its own part's
  cc <- if (is.null(catchCategory)) "All"
        else match.arg(catchCategory, c("Lan", "Dis", "BMS", "All"))
  if (is.null(distributions))
    stop(fn, "(): what = \"", what, "\" needs `distributions` - the coverage ",
         "of a length / age stratum is how much of its weight has a ",
         "distribution linked", call. = FALSE)
  dist_type <- if (identical(what, "age")) "Age" else "LengthTotal"
  # catchCategory is a column of a length / age table (one row per part),
  # so it is never one of the stratum columns; with nothing else named the
  # strata are simply the catch parts
  dims  <- setdiff(dims, "catchCategory")
  key   <- function(x) if (length(dims))
    do.call(paste, c(unname(as.list(x[dims])), sep = "\r")) else rep("", nrow(x))
  parts <- catch_category_parts(cc)
  bl <- bind_rows(lapply(parts, function(p) {
    b <- biology_link(catch, distributions, dims, p, type = dist_type)
    if (is.null(b) || !nrow(b)) return(NULL)
    m <- stratum_measurements(catch, distributions, dims, p, type = dist_type)
    b$numMeasurements <- if (is.null(m)) 0 else
      m$n_measurements[match(key(b), key(m))]
    b$numMeasurements[is.na(b$numMeasurements)] <- 0
    b
  }))
  if (!nrow(bl)) return(NULL)
  out <- bl[, dims, drop = FALSE]
  out$catchCategory    <- bl$catchCategory
  out$distributionType <- dist_type
  out$variableUnit     <- "t"
  out$total            <- bl$weight
  out$coverage         <- bl$coverage
  out$numMeasurements  <- bl$numMeasurements
  out$numTrips         <- bl$numTrips
  out
}

# ---------------------------------------------------------------------
#  What the coverage plots do with the table (internal, one place)
# ---------------------------------------------------------------------

#' Is this a coverage table?
#'
#' A [build_coverage_strata()] table is recognised by its own columns, so
#' raw catch (which can carry a row-level `coverage` after
#' [add_stratum_metrics()]) is never mistaken for one.
#'
#' @param x Anything.
#' @return `"discard"`, `"length"`, `"age"` or `NA` (not a coverage table).
#' @keywords internal
#' @noRd
#' @export
coverage_table_what <- function(x) {
  if (!is.data.frame(x)) return(NA_character_)
  nm <- names(x)
  if (all(c("weightingTotal", "rate", "landings", "coverage") %in% nm))
    return("discard")
  if (all(c("distributionType", "total", "coverage") %in% nm))
    return(if (any(x$distributionType %in% "Age")) "age" else "length")
  NA_character_
}

#' Pool a coverage table to the cells a coverage plot draws
#'
#' Turns raw catch (or a [build_coverage_strata()] table) into one row per
#' plotted cell: builds the table at exactly the plotted columns when
#' given raw catch, and sums a table that is finer than the plot's roles
#' (weights and trips add up; coverage and rate are re-derived from the
#' summed weights, never averaged).
#'
#' @param catch Raw CEF catch, or a [build_coverage_strata()] table.
#' @param distributions Distributions table (raw length / age only).
#' @param keys The plotted columns (y, x / fill, facet, ...).
#' @param what,what_given The `what` argument, and whether the caller
#'   actually typed it (a table brings its own).
#' @param catchCategory The `catchCategory` argument.
#' @param fn The calling function's name, for messages.
#' @return `NULL` (nothing to draw) or `list(cells, what, weight_label)`.
#' @keywords internal
#' @noRd
#' @export
coverage_cells <- function(catch, distributions, keys, what, what_given,
                           catchCategory, fn) {
  keys <- unique(keys[!is.na(keys) & nzchar(keys)])
  tw <- coverage_table_what(catch)
  if (!is.na(tw)) {
    if (what_given && !identical(normalize_what(what), tw))
      stop(fn, "(): `catch` is a ", tw, " coverage table but what = \"",
           what, "\"", call. = FALSE)
    what <- tw
    if (!is.null(distributions))
      warning(fn, "(): `distributions` is only used when building from raw ",
              "catch - ignored for a coverage table", call. = FALSE)
    tbl <- catch
    if (identical(what, "discard")) {
      if (!is.null(catchCategory))
        stop(fn, "(): catchCategory does not apply to what = \"discard\" ",
             "(discard coverage is the discard part by definition)",
             call. = FALSE)
    } else if (!is.null(catchCategory) && !identical(catchCategory, "All")) {
      cc <- match.arg(catchCategory, c("Lan", "Dis", "BMS"))
      tbl <- tbl[tbl$catchCategory %in% cc, , drop = FALSE]
      if (!nrow(tbl))
        stop(fn, "(): the coverage table has no catchCategory \"", cc, "\" rows",
             call. = FALSE)
    }
  } else {
    what <- normalize_what(what)
    tbl <- build_coverage_strata(catch, keys, distributions, what,
                                 catchCategory)
  }
  if (is.null(tbl) || !nrow(tbl)) return(NULL)
  miss <- setdiff(keys, names(tbl))
  if (length(miss))
    stop(fn, "(): column(s) not in the coverage table: ",
         paste(miss, collapse = ", "), " (it has: ",
         paste(setdiff(names(tbl), c("catchCategory", "variableUnit",
                                     "weightingCategory", "distributionType",
                                     "total", "weightingTotal", "rate",
                                     "landings", "coverage", "numTrips",
                                     "numMeasurements")), collapse = ", "),
         ")", call. = FALSE)

  cells <- if (identical(what, "discard")) {
    tbl %>%
      group_by(across(all_of(keys))) %>%
      summarise(total = if (all(is.na(total))) NA_real_
                        else sum(total, na.rm = TRUE),
                weightingTotal = sum(weightingTotal, na.rm = TRUE),
                landings = sum(landings, na.rm = TRUE),
                numTrips = if (all(is.na(numTrips))) NA_real_
                           else sum(numTrips, na.rm = TRUE),
                n_strata = dplyr::n(), .groups = "drop") %>%
      mutate(rate = ifelse(!is.na(total) & weightingTotal > 0,
                           total / weightingTotal, NA_real_),
             coverage = ifelse(landings > 0,
                               weightingTotal / landings * 100, NA_real_))
  } else {
    tbl %>%
      mutate(.covered = ifelse(is.na(coverage), 0, total * coverage / 100)) %>%
      group_by(across(all_of(keys))) %>%
      summarise(total = sum(total, na.rm = TRUE),
                covered = sum(.covered, na.rm = TRUE),
                numMeasurements = sum(numMeasurements, na.rm = TRUE),
                numTrips = if (all(is.na(numTrips))) NA_real_
                           else sum(numTrips, na.rm = TRUE),
                n_strata = dplyr::n(), .groups = "drop") %>%
      mutate(coverage = ifelse(total > 0, covered / total * 100, NA_real_))
  }
  parts <- if (identical(what, "discard")) NULL else unique(tbl$catchCategory)
  wl <- default_weight_label(what, if (length(parts) == 1) parts else "All")
  list(cells = as.data.frame(cells), what = what, weight_label = wl)
}

#' Sampling status of each plotted cell
#'
#' Adds `cat_key` and `Status` to the cells of [coverage_cells()] against
#' the thresholds, using [classify_strata()] - the one classifier, so a
#' cell is judged exactly as the raising side judges a stratum. Only the
#' coverage plots call this: the status is a colour on a figure, not a
#' column of the coverage table.
#'
#' @param cells The `cells` of [coverage_cells()].
#' @param what `"discard"`, `"length"` or `"age"`.
#' @param min_cov,min_trip,min_fish Thresholds; `NULL` drops that
#'   question from the classification.
#' @return `cells` plus `cat_key` and `Status` (and the weight under the
#'   name `landings`, whatever `what` is, which is what the plots label
#'   and size by).
#' @keywords internal
#' @noRd
#' @export
coverage_status <- function(cells, what, min_cov, min_trip = NULL,
                            min_fish = NULL) {
  s <- cells
  if (identical(what, "discard")) {
    s$discards <- ifelse(is.na(cells$total), 0, cells$total)
    s$has_disc_data <- !is.na(cells$total)
    s$landings_with_discards <- cells$weightingTotal
  } else {
    s$weight <- cells$total
    s$landings <- cells$total
    s$landings_with_discards <- cells$covered
    s$discards <- 0
    s$n_measurements <- cells$numMeasurements
  }
  classify_strata(s, min_cov, what, min_fish, min_trip)
}
