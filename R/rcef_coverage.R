# =====================================================================
#  rcef_coverage.R   (build_coverage_strata() and what the plots do with it)
#
#  ORIGIN: this project (ILVO / RCEF Workbench). Freely refactorable.
#  No shiny in this file.
# =====================================================================

#' Sampling coverage and discard rate per stratum
#'
#' @description
#' One row per stratum: what was landed and discarded, the discard rate, and
#' how well the stratum is sampled - a table you can read, sort and filter,
#' at whatever aggregation you choose. Give it the catch and the columns
#' that define a stratum; for length and age, also the distributions.
#'
#' This is the table behind [plot_coverage_heat()], [plot_coverage_bubble()],
#' [plot_coverage_bar()], [plot_coverage_map()] and [plot_DR()].
#' They build it themselves from raw catch and take it directly when you
#' pass it, so every number in a figure can be found in this table, and a
#' table can always be looked at as a figure.
#'
#' @details
#' ## What the table leaves out
#'
#' It reports numbers and does not judge them: there is no status, category
#' or decision column. The thresholds (`min_cov`, `min_trip`, `min_fish`)
#' are a plotting choice; the coverage plots apply them to these numbers as
#' they draw.
#'
#' ## Column names and units
#'
#' Columns follow the final CAR / CAS output tables wherever those have a
#' counterpart (`total`, `weightingTotal`, `rate`, `numTrips`,
#' `catchCategory`, `variableUnit`), so a stratum here reads like its row
#' there. Weights are in tonnes; the CAR / CAS tables themselves keep the
#' CEF's kilograms. Both ways of expressing the discard rate are in the
#' table - `rate` (D/L) and `discardRate` (D/(L+D)) - so either can be read,
#' sorted or plotted.
#'
#' ## Discard tables (`what = "discard"`)
#'
#' One row per stratum, with the discard estimate on the `Dis` side and the
#' landings that weight it on the `Lan` side. Only landings and discards are
#' counted: BMS is a third catch part with its own raising, not a discard.
#'
#' | Column | Meaning |
#' |:-------|:--------|
#' | the `dims` columns | The stratum, as you named it. |
#' | `catchCategory`, `variableUnit` | `"Dis"` and `"t"`: what `total` is. |
#' | `total` | Discards. `NA` when the stratum has no discard estimate at all (nobody looked); `0` means it was looked at and none were found. |
#' | `weightingCategory` | `"Lan"`. |
#' | `weightingTotal` | The landings that carry a discard estimate (their `domainCatchDis` is filled): the weight `rate` is weighted by. |
#' | `rate` | `total / weightingTotal` (D/L): discards per tonne of covered landings, as the CAS output reports it. A ratio, so it can exceed 1. `NA` when `total` is missing or `weightingTotal` is 0. |
#' | `discardRate` | `total / (weightingTotal + total)` (D/(L+D)): the discards as a share of the covered landings plus the discards - the `discard_rate` of [build_strata()]. Between 0 and 1. `NA` when `total` is missing or the denominator is 0. |
#' | `landings` | All landings in the stratum, covered or not. |
#' | `coverage` | `weightingTotal / landings`, in percent; `NA` when there are no landings. |
#' | `numTrips` | The trips behind the stratum; `NA` when the data does not report them. The largest count among the stratum's rows (one trip repeats across the rows of a sample), and summed when the plots pool strata into one cell. |
#'
#' ## Length and age tables (`what = "length"`, `what = "age"`)
#'
#' One row per stratum **and catch part**, so `catchCategory = "All"` (the
#' default) gives a `Lan` and a `Dis` row for every stratum.
#'
#' | Column | Meaning |
#' |:-------|:--------|
#' | the `dims` columns | The stratum, as you named it. |
#' | `catchCategory` | The catch part the row describes: `"Lan"`, `"Dis"` or `"BMS"`. |
#' | `distributionType` | `"LengthTotal"` or `"Age"`. |
#' | `variableUnit` | `"t"`. |
#' | `total` | The weight of that catch part. |
#' | `coverage` | The share of `total`, in percent, whose `domainBiology` matches a distribution of that type and catch part; `NA` when `total` is 0. |
#' | `numMeasurements` | The measurements behind those distributions (0 when there are none). |
#' | `numTrips` | As above. |
#'
#' ## Choosing the aggregation
#'
#' `dims` is the aggregation: name fewer columns for a coarser table (each
#' row pools everything else), more for a finer one. Weights add up, so a
#' table built finer can be pooled to coarser plotted columns later without
#' loss - coverage and rate are re-derived from the summed weights, never
#' averaged. Leave `dims` out and nothing is pooled at all: the table is
#' built at the finest a stratum naturally gets - `fleetValue`, `areaValue`,
#' `vesselFlagCountry`, `seasonValue`, whichever are actually present in
#' `catch` - so a stratum here is as fine as the data itself, and only
#' naming `dims` yourself asks for anything coarser.
#'
#' ## The other derived columns are not lost
#'
#' A column [derive_fields()] added but you left out of `dims` (`mesh` when
#' `dims` is just `"metier4"`, say) still rides along on each row, *when*
#' that stratum pins it to a single value - so the table can be filtered on
#' it afterwards without having named it as part of the aggregation. A
#' stratum coarser than the column - several meshes inside one `metier4` -
#' reports `NA` there instead of an arbitrary pick, since no single value
#' would be true of the whole row.
#'
#' ## Build once, draw many
#'
#' For length and age the expensive step is linking the catch to the
#' distributions. Build the table once and pass it to as many plots as you
#' like, rather than passing raw catch to each.
#'
#' @param catch CEF catch data (long format) carrying `catchCategory`,
#'   `total` and, for discards, `domainCatchDis`, plus every column named in
#'   `dims`. [derive_fields()] adds `metier4`, `mesh` and the ICES area
#'   levels.
#' @param dims Character vector of the columns that define a stratum, for
#'   example `c("metier4", "vesselFlagCountry")`, or the finer
#'   `c("fleetValue", "areaValue", "seasonValue")`. Default `NULL`: nothing
#'   is pooled, and the table is built at the finest a stratum naturally
#'   gets (`fleetValue`, `areaValue`, `vesselFlagCountry`, `seasonValue` -
#'   whichever are present in `catch`). For length and age `catchCategory`
#'   is never a stratum column (it is a column of the result, one row per
#'   catch part) and is ignored if named.
#' @param distributions A CEF distributions table. Required for
#'   `what = "length"` and `what = "age"`, ignored for discards.
#' @param what `"discard"` (default), `"length"` or `"age"`. The plural and
#'   mixed case are accepted (`"discards"`, `"Length"`).
#' @param catchCategory For `what = "length"` / `"age"`: `"All"` (default -
#'   landings and discards, one row each), `"Lan"`, `"Dis"` or `"BMS"`. Not
#'   valid for `what = "discard"` (an error).
#' @return A data frame with the columns described above, or `NULL` when
#'   there is nothing to describe (no landings or discards, or no catch rows
#'   for that `catchCategory`). Distributions that link to nothing are not
#'   "nothing": every stratum is then reported with coverage 0.
#' @seealso [coverage-plots] and [plot_DR()], which draw this
#'   table; [build_strata()], the raising-side stratum table;
#'   [classify_strata()], which judges strata against thresholds.
#' @family coverage plots
#' @md
#' @examples
#' data(anchoveta_catch)
#' data(anchoveta_dist)
#'
#' # discard rate and coverage per metier x country
#' cov <- build_coverage_strata(anchoveta_catch, c("metier4", "vesselFlagCountry"))
#' head(cov[order(-cov$landings), ])
#'
#' # the same table, coarser or finer
#' build_coverage_strata(anchoveta_catch, "seasonValue")
#' nrow(build_coverage_strata(anchoveta_catch,
#'                            c("fleetValue", "areaValue", "seasonValue")))
#'
#' # dims left out: nothing pooled - one row per fleetValue x areaValue x
#' # vesselFlagCountry x seasonValue combination the catch actually has
#' nrow(build_coverage_strata(anchoveta_catch))
#'
#' # length coverage of the landings: weight, share with a distribution,
#' # and the measurements behind it
#' len <- build_coverage_strata(anchoveta_catch, c("metier4", "vesselFlagCountry"),
#'                              anchoveta_dist, what = "length",
#'                              catchCategory = "Lan")
#' head(len[order(-len$total), ])
#'
#' # the plots draw the same table, from the raw catch or from the table
#' plot_coverage_heat(cov, y = "metier4", x = "vesselFlagCountry", min_cov = 50)
#' plot_DR(cov, x = "metier4", colour = "vesselFlagCountry")
#' @export
build_coverage_strata <- function(catch, dims = NULL, distributions = NULL,
                                  what = "discard", catchCategory = NULL) {
  fn   <- "build_coverage_strata"
  what <- normalize_what(what)
  catch <- as.data.frame(catch)
  if (is.null(dims)) {
    # Nothing pooled unless asked for: the finest a stratum naturally gets,
    # not some column the caller never named. Only the ones the data
    # actually carries - a table filtered before derive_fields() may lack
    # one of them.
    dims <- intersect(c("fleetValue", "areaValue", "vesselFlagCountry",
                        "seasonValue"), names(catch))
    if (!length(dims))
      stop(fn, "(): `dims` was not given and none of the default stratum ",
           "columns (fleetValue, areaValue, vesselFlagCountry, seasonValue) ",
           "are in `catch` - name `dims` yourself, or run derive_fields() ",
           "first", call. = FALSE)
  } else {
    dims <- unique(as.character(dims))
    dims <- dims[!is.na(dims) & nzchar(dims)]
    if (!length(dims))
      stop(fn, "(): `dims` names no column", call. = FALSE)
  }
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
    out$discardRate       <- ifelse(st$has_disc_data &
                                      st$landings_with_discards + st$discards > 0,
                                    st$discards / (st$landings_with_discards +
                                                     st$discards),
                                    NA_real_)
    out$landings          <- st$landings
    out$coverage          <- st$coverage
    out$numTrips          <- st$numTrips
    ex <- coverage_extra_cols(d, dims)
    if (!is.null(ex)) out <- left_join(out, ex, by = dims)
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
  ex <- coverage_extra_cols(catch, dims)
  if (!is.null(ex)) out <- left_join(out, ex, by = dims)
  out
}

#' The other `derive_fields()` columns, kept where a stratum pins them
#'
#' The columns a stratum (`dims`) does not already name - `mesh`, `metier5`,
#' `Stratum`, the ICES area levels, and so on ([DIMS_ALL_CANDIDATES], plus
#' `Stratum` itself - the one [derive_fields()] column that constant leaves
#' out, being the finest of all of them rather than a nested level of one) -
#' computed as the single value they share, or `NA` where the stratum spans
#' more than one (so picking one would misrepresent the row). `NULL` when
#' none of those columns are present, or every one of them is already part
#' of `dims`.
#'
#' @param src Catch data to read the extra columns from, carrying `dims`.
#' @param dims The stratum columns already accounted for.
#' @return A data frame of `dims` plus whichever extra columns apply, one
#'   row per stratum - ready to [dplyr::left_join()] onto a strata table by
#'   `dims` - or `NULL`.
#' @seealso [build_coverage_strata()], [DIMS_ALL_CANDIDATES]
#' @keywords internal
#' @noRd
#' @export
coverage_extra_cols <- function(src, dims) {
  # computed here, not as a top-level constant: a top-level
  # c(DIMS_ALL_CANDIDATES, ...) would run at package-load time, when
  # rcef_coverage.R (loaded before rcef_utils.R, alphabetically) cannot
  # yet see DIMS_ALL_CANDIDATES
  cand <- c(DIMS_ALL_CANDIDATES, "Stratum")
  extra <- setdiff(intersect(cand, names(src)), dims)
  if (!length(extra)) return(NULL)
  uniq_or_na <- function(x) {
    u <- unique(x[!is.na(x)])
    if (length(u) == 1) u else NA_character_
  }
  src %>%
    group_by(across(all_of(dims))) %>%
    summarise(across(all_of(extra), ~ uniq_or_na(as.character(.x))),
             .groups = "drop")
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
  # A coverage_extra_cols() column rides along even where no stratum in the
  # table pins it down at all - all `NA`, nothing to plot by. Treated the
  # same as a column that is not there, so asking to plot by one still
  # names it, instead of silently grouping everything into one NA level.
  present <- intersect(keys, names(tbl))
  all_na  <- present[vapply(present, function(k) all(is.na(tbl[[k]])),
                            logical(1))]
  miss <- union(setdiff(keys, names(tbl)), all_na)
  if (length(miss))
    stop(fn, "(): column(s) not in the coverage table: ",
         paste(miss, collapse = ", "), " (it has: ",
         paste(setdiff(names(tbl), c("catchCategory", "variableUnit",
                                     "weightingCategory", "distributionType",
                                     "total", "weightingTotal", "rate",
                                     "discardRate", "landings", "coverage",
                                     "numTrips", "numMeasurements")),
               collapse = ", "),
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
             discardRate = ifelse(!is.na(total) & weightingTotal + total > 0,
                                  total / (weightingTotal + total), NA_real_),
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
