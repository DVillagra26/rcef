# =====================================================================
#  rcef_coverage.R   (build_coverage_strata() and what the plots do with it)
#
#  ORIGIN: this project (ILVO / RCEF Workbench). Freely refactorable.
# =====================================================================

#' Sampling coverage and discard rate per stratum
#'
#' @description
#' One row per stratum: what was landed and discarded, the discard rate, and
#' how well the stratum is sampled - a table you can read, sort and filter,
#' at whatever aggregation you choose. Give it the catch and the columns
#' that define a stratum; for length and age, also the distributions.
#'
#' This is the table behind [plot_coverage_grid()], [plot_coverage_bar()],
#' [plot_coverage_map()] and [plot_DR()].
#' They build it themselves from raw catch and take it directly when you
#' pass it, so every number in a figure can be found in this table, and a
#' table can always be looked at as a figure.
#'
#' @details
#' ## What the table leaves out
#'
#' It reports numbers and does not judge them: there is no status, category
#' or decision column. The thresholds (`thresholds = list(cov = 50, trips = NA,
#' fish = NA)`) are the choice of the functions that judge these numbers:
#' [plot_coverage_grid()] applies them as it draws and [recommend_groups()] to
#' choose the groups. This function takes none.
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
#' | `total` | Discards. `NA` when the stratum has no discard estimate at all (nobody looked), or when a raised estimate has no rate (`NaN`: its raising group had no donor to draw one from); `0` means it was looked at and none were found. |
#' | `weightingCategory` | `"Lan"`. |
#' | `weightingTotal` | The landings that carry a discard estimate: the weight `rate` is weighted by. A landing does when a discard row with a weight has its `domainCatchDis`, in the same country, year, stock, origin and variable type - the way [catchRaising()] pairs them, so a domain whose only discard rows are another origin's (an Official 0 kg beside WGEstimate landings) is no estimate. In a raised catch (see `importedOrRaised` below) all of its landings do, imported or raised. |
#' | `rate` | `total / weightingTotal` (D/L): discards per tonne of covered landings, as the CAS output reports it. A ratio, so it can exceed 1. `NA` when `total` is missing or `weightingTotal` is 0. |
#' | `discardRate` | `total / (weightingTotal + total)` (D/(L+D)): the discards as a share of the covered landings plus the discards - Between 0 and 1. `NA` when `total` is missing or the denominator is 0. |
#' | `landings` | All landings in the stratum, covered or not. |
#' | `coverage` | `weightingTotal / landings`, in percent; `NA` when there are no landings. |
#' | `numTrips` | The trips behind the stratum; `NA` when the data does not report them. The largest count among the stratum's rows (one trip repeats across the rows of a sample), and summed when the plots pool strata into one cell. |
#' | `importedOrRaised` | Only when `catch` is a raised catch (the output of [catchRaising()]): `"raised"` when the stratum's discard rows were added by the raising, `"imported"` when they came with the data. Worked out per stratum, never from the landings rows beside the discards, which are always imported - so it is a column of the result and cannot be one of `dims`. A raised catch is counted on the rows the raising ran on, those of the origin and variable type of the rows it raised. |
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
#' like, rather than passing the catch to each. The same goes for
#' [derive_fields()]: run it once, on the raw catch, and keep building on that
#' catch - the table and the plots never derive anything again.
#'
#' @param catch CEF catch data (long format) carrying `catchCategory`,
#'   `total` and, for discards, `domainCatchDis`, plus every column named in
#'   `dims`. Those can be columns of your own, or the ones [derive_fields()]
#'   adds (`metier4`, `mesh`, `metier4_mesh`, the ICES area levels): run it once
#'   on the raw catch and pass that - the table reads the columns the catch has
#'   and derives nothing itself.
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
#' @seealso [coverage-plots] and [plot_DR()], which draw this table.
#' @family coverage plots
#' @md
#' @examples
#' \dontrun{
#'
#' # discard rate and coverage per metier x country
#' cov <- build_coverage_strata(catch, c("metier4", "vesselFlagCountry"))
#' head(cov[order(-cov$landings), ])
#'
#' # the same table, coarser or finer
#' build_coverage_strata(catch, "seasonValue")
#' nrow(build_coverage_strata(catch,
#'                            c("fleetValue", "areaValue", "seasonValue")))
#'
#' # dims left out: nothing pooled - one row per fleetValue x areaValue x
#' # vesselFlagCountry x seasonValue combination the catch actually has
#' nrow(build_coverage_strata(catch))
#'
#' # length coverage of the landings: weight, share with a distribution,
#' # and the measurements behind it
#' len <- build_coverage_strata(catch, c("metier4", "vesselFlagCountry"),
#'                              distributions, what = "length",
#'                              catchCategory = "Lan")
#' head(len[order(-len$total), ])
#'
#' # the plots draw the same table, from the raw catch or from the table
#' plot_coverage_grid(cov, y = "metier4", x = "vesselFlagCountry",
#'                    thresholds = list(cov = 50))
#' plot_DR(cov, x = "metier4", colour = "vesselFlagCountry")
#' }
#' @export
build_coverage_strata <- function(catch, dims = NULL, distributions = NULL,
                                  what = "discard", catchCategory = NULL) {
  fn   <- "build_coverage_strata"
  what <- coverage_what(what)
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
         derive_hint(miss), call. = FALSE)
  # a table already aggregated to strata has none of these and would
  # otherwise come back as "nothing to describe"
  need <- setdiff(c("catchCategory", "total",
                    if (identical(what, "discard")) "domainCatchDis"),
                  names(catch))
  if (length(need))
    stop(fn, "(): `catch` needs the CEF catch columns ",
         paste(need, collapse = ", "), " - pass the catch data itself, not a ",
         "table already aggregated to strata", call. = FALSE)

  # The other derive_fields() columns, kept where a stratum pins them: the
  # columns `dims` does not already name - mesh, metier5, metier4_mesh,
  # Stratum, the ICES area levels, ... - as the single value the stratum's
  # rows share, or NA where it spans more than one (so picking one would
  # misrepresent the row). NULL when none of them is there or all are in `dims`.
  extras <- function(src) {
    extra <- setdiff(intersect(c("metier4", "metier5", "metier6", "vesselFlagCountry",
                                 "ICESSubArea", "ICESDivision", "ICESSubdivision",
                                 "seasonValue", "FleetType", "target", "mesh",
                                 "metier4_mesh", "Stratum"), names(src)), dims)
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

  if (identical(what, "discard")) {
    if (!is.null(catchCategory))
      stop(fn, "(): catchCategory does not apply to what = \"discard\" ",
           "(discard coverage is the discard part by definition)",
           call. = FALSE)
    if ("catchCategory" %in% dims)
      stop(fn, "(): catchCategory cannot be a stratum column for discards - ",
           "the landings and discards of a stratum are its two halves",
           call. = FALSE)
    if ("importedOrRaised" %in% dims)
      stop(fn, "(): importedOrRaised cannot be a stratum column - whether ",
           "the discards were raised is worked out per stratum, and is a ",
           "column of the result", call. = FALSE)
    # landings and discards only: BMS is a third catch part with its own
    # raising, not a discard
    d <- catch[catch$catchCategory %in% c("Lan", "Dis"), , drop = FALSE]
    has_status <- "importedOrRaised" %in% names(d)
    if (has_status && any(d$importedOrRaised %in% "raised")) {
      # A raised catch: its landings and discards are the rows the raising ran
      # on - the origin and variable type of the rows it raised (WGEstimate and
      # WeightLive by default). The Official 0 kg discards a stratum may carry
      # beside them are not discards that were raised.
      was  <- d$importedOrRaised %in% "raised"
      keep <- rep(TRUE, nrow(d))
      for (col in intersect(c("originType", "variableType"), names(d)))
        keep <- keep & d[[col]] %in% unique(d[[col]][was])
      d <- d[keep, , drop = FALSE]
    }
    if (!nrow(d)) return(NULL)
    # Per stratum, in tonnes: all the landings, the landings that carry a
    # discard estimate (in a raised catch every landing has one, imported or
    # raised, whether or not its domain is filled), the discards, and the trips.
    # A stratum with no discard row at all has no discard estimate - nobody
    # looked - which is not the same as 0. Nor is a raised row that came out of
    # the raising as NaN, a group with no donor to draw a rate from: that
    # stratum has no estimate either, however much else it holds.
    d$.t   <- convert_units(d$total, if ("variableUnit" %in% names(d)) d$variableUnit, "t")
    d$.lan <- d$catchCategory == "Lan"
    # A landing carries a discard estimate the way catchRaising() decides it: a
    # discard row with a weight has the landing's domainCatchDis and is of the
    # same country, year, stock, origin and variable type. A domain that only has
    # discard rows of another origin (the Official 0 kg beside WGEstimate
    # landings) is no estimate: the raising raises that landing.
    has_dom  <- !is.na(d$domainCatchDis) & d$domainCatchDis != ""
    link     <- intersect(c("vesselFlagCountry", "year", "workingGroup", "stock",
                            "speciesCode", "originType", "variableType"), names(d))
    link_key <- do.call(paste, c(unname(as.list(d[c(link, "domainCatchDis")])), sep = "\r"))
    d$.est <- d$.lan & (has_status |
                          (has_dom & link_key %in% link_key[!d$.lan & has_dom & !is.na(d$.t)]))
    has_trips  <- "numTrips" %in% names(d)
    st <- d %>%
      group_by(across(all_of(dims))) %>%
      summarise(landings = sum(.t[.lan], na.rm = TRUE),
                landings_with_discards = sum(.t[.est], na.rm = TRUE),
                discards = sum(.t[!.lan], na.rm = TRUE),
                has_disc_data = any(!.lan),
                no_rate = any(is.nan(.t[!.lan])),
                # a stratum is raised when its discard rows were, never judged
                # from the landings rows next to them (those are always imported)
                raised = has_status && any(importedOrRaised[!.lan] %in% "raised"),
                # max(), not sum(): numTrips is reported per raw CEF row and the
                # same trip count is often repeated across several rows of one
                # sample (by catch category, by length class in the wide files);
                # summing would count one trip several times over. NA where
                # nothing in the stratum reports it: unknown, not zero.
                numTrips = if (has_trips)
                  suppressWarnings(max(numTrips, na.rm = TRUE)) else NA_real_,
                .groups = "drop") %>%
      as.data.frame()
    st$numTrips[!is.finite(st$numTrips)] <- NA_real_
    st$coverage <- ifelse(st$landings > 0,
                          st$landings_with_discards / st$landings * 100, NA_real_)
    looked <- st$has_disc_data & !st$no_rate
    out <- st[, dims, drop = FALSE]
    out$catchCategory     <- "Dis"
    out$variableUnit      <- "t"
    out$total             <- ifelse(looked, st$discards, NA_real_)
    out$weightingCategory <- "Lan"
    out$weightingTotal    <- st$landings_with_discards
    out$rate              <- ifelse(looked & st$landings_with_discards > 0,
                                    st$discards / st$landings_with_discards,
                                    NA_real_)
    out$discardRate       <- ifelse(looked & st$landings_with_discards + st$discards > 0,
                                    st$discards / (st$landings_with_discards +
                                                     st$discards),
                                    NA_real_)
    out$landings          <- st$landings
    out$coverage          <- st$coverage
    out$numTrips          <- st$numTrips
    if (has_status)
      out$importedOrRaised <- ifelse(st$raised, "raised", "imported")
    ex <- extras(d)
    if (!is.null(ex)) out <- left_join(out, ex, by = dims)
    return(out)
  }

  # ---- length / age: how much of each catch part's weight has a distribution ----
  # linked, and how many fish stand behind it; one catch part at a time so each
  # row's measurement count is its own part's
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

  # The catch as it stands - its fields are the ones it has, derived once by the
  # caller or added by hand - and the distributions normalised. A catch row counts
  # as measured only when its domainBiology is filled AND that key appears in the
  # distributions for the same catch category and country: a filled key that
  # matches nothing is a dangling reference, a data problem and not evidence of
  # sampling.
  d  <- catch
  pd <- prep_distributions(distributions, dist_type)
  d$.k <- biology_key(d, pd)
  by <- attr(d$.k, "keys")
  d$.t <- convert_units(d$total, if ("variableUnit" %in% names(d)) d$variableUnit, "t")
  d$.has_key <- if ("domainBiology" %in% names(d))
    !is.na(d$domainBiology) & nzchar(as.character(d$domainBiology)) else FALSE
  d$.measured <- d$.has_key & d$.k %in% (if (is.null(pd)) character(0)
                                           else unique(biology_key(pd, by)))
  # The measurements behind a distribution: numMeasurements repeats across the
  # classes of a domain, so it is taken once per domain (of each country: two
  # countries can use the same domain name, each with its own distribution)
  # rather than summed over every length class.
  n_meas <- NULL
  if (!is.null(pd)) {
    pn <- pd[pd$.var_kind == "numbers", , drop = FALSE]
    pn$.k <- biology_key(pn, by)
    n_meas <- pn %>% group_by(.k) %>%
      summarise(.n = suppressWarnings(max(.n_meas, na.rm = TRUE)), .groups = "drop") %>%
      mutate(.n = ifelse(is.finite(.n), .n, 0))
  }
  bl <- bind_rows(lapply(catch_category_parts(cc), function(p) {
    dp <- d[d$catchCategory %in% p, , drop = FALSE]
    if (!nrow(dp)) return(NULL)
    cells <- dp %>%
      group_by(across(all_of(dims)), catchCategory) %>%
      summarise(weight          = sum(.t, na.rm = TRUE),
                weight_measured = sum(.t[.measured], na.rm = TRUE),
                # max(), not sum(): numTrips is reported per raw CEF row and the
                # same trip count is routinely repeated across several rows of
                # one sample (by length class in the wide files); summing would
                # count one trip several times over.
                numTrips = if ("numTrips" %in% names(dp))
                  suppressWarnings(max(numTrips, na.rm = TRUE)) else NA_real_,
                .groups = "drop") %>%
      mutate(coverage = ifelse(weight > 0, weight_measured / weight * 100,
                               NA_real_),
             numTrips = ifelse(is.finite(numTrips), numTrips, NA_real_)) %>%
      as.data.frame()
    m <- NULL
    if (!is.null(n_meas))
      m <- dp[dp$.has_key, c(dims, ".k"), drop = FALSE] %>% distinct() %>%
        inner_join(n_meas, by = ".k") %>%
        group_by(across(all_of(dims))) %>%
        summarise(.n = sum(.n, na.rm = TRUE), .groups = "drop") %>%
        as.data.frame()
    cells$numMeasurements <- if (is.null(m) || !nrow(m)) 0 else
      m$.n[match(key(cells), key(m))]
    cells$numMeasurements[is.na(cells$numMeasurements)] <- 0
    cells
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
  # the extra columns are read off the same table the strata came from, so a
  # column a csv gave back as a number (ICESSubArea 27.4) meets itself in the
  # join below and not its text
  ex <- extras(d)
  if (!is.null(ex))
    # with no stratum column at all (only the catch parts) every row takes the one value
    out <- if (length(dims)) left_join(out, ex, by = dims)
           else cbind(out, ex[rep(1L, nrow(out)), , drop = FALSE])
  out
}

# ---------------------------------------------------------------------
#  What the coverage plots do with the table (internal, one place)
# ---------------------------------------------------------------------

#' The `thresholds` argument of the functions that judge sampling
#'
#' [recommend_groups()] and [plot_coverage_grid()] take their thresholds the same
#' way: `thresholds = list(cov = 50, trips = NA, fish = NA)`. This reads such a list
#' once, for both: leave out what you do not set (the default stays), `NA` or `NULL`
#' for `trips` and `fish` is "not used", and anything else is an error that names
#' the calling function. Given `what` it also drops `fish` for the discards (no fish
#' are measured for a discard estimate) with a warning, so one list can be given to
#' the discards and to the lengths alike.
#'
#' @param thresholds The list the caller was given.
#' @param fn The calling function's name, for messages.
#' @param what `NULL` (the list is only read), or `"discard"`, `"length"` or `"age"`.
#' @return `list(cov, trips, fish)`: numbers, `NA_real_` where not used.
#' @keywords internal
#' @noRd
#' @export
coverage_thresholds <- function(thresholds, fn, what = NULL) {
  th <- list(cov = 50, trips = NA_real_, fish = NA_real_)
  if (!is.list(thresholds) || (length(thresholds) && (is.null(names(thresholds)) ||
                                                      !all(nzchar(names(thresholds))))))
    stop(fn, "(): `thresholds` is a named list, e.g. list(cov = 50, trips = NA, fish = 100)",
         call. = FALSE)
  unknown <- setdiff(names(thresholds), names(th))
  if (length(unknown))
    stop(fn, "(): `thresholds` has no ", paste0("'", unknown, "'", collapse = ", "),
         ": it takes cov, trips and fish", call. = FALSE)
  for (k in names(thresholds))             # NULL is "not used" (cov: no value, an error below)
    th[[k]] <- if (is.null(thresholds[[k]])) NA_real_ else thresholds[[k]]
  if (!(is.numeric(th$cov) && length(th$cov) == 1 && is.finite(th$cov) &&
        th$cov >= 0 && th$cov <= 100))
    stop(fn, "(): `thresholds$cov` is the coverage a pool has to reach, in percent (0 to ",
         "100); it cannot be empty (the default is 50)", call. = FALSE)
  for (k in c("trips", "fish")) {
    v <- th[[k]]
    if (length(v) != 1 || !(is.na(v) || (is.numeric(v) && v >= 0)))
      stop(fn, "(): `thresholds$", k, "` is a number, or NA when it is not used",
           call. = FALSE)
    th[[k]] <- as.numeric(v)
  }
  if (identical(what, "discard") && !is.na(th$fish)) {
    warning(fn, "(): `thresholds$fish` counts measured fish, which discards have none ",
            "of: it is not used", call. = FALSE)
    th$fish <- NA_real_
  }
  th
}

#' What a `what` argument, or a coverage table, is about
#'
#' One answer to two questions: `"discard"`, `"length"` or `"age"`. Given the
#' `what` a caller typed it settles the spelling - callers reasonably type the
#' plural or a near-synonym (`"discards"`, `"Length"`, `"len"`) - and anything
#' else is a clear error, rather than a value that falls through to the biology
#' pipeline and fails obscurely. Given a data frame it says which kind of
#' [build_coverage_strata()] table it is, recognised by its own columns, so raw
#' catch (even one that carries a row-level `coverage`) is never mistaken for
#' one.
#'
#' @param x A string - discard(s), length(s) / len, age(s) - or a data frame.
#' @return `"discard"`, `"length"` or `"age"`; `NA` for a data frame that is not
#'   a coverage table.
#' @keywords internal
#' @noRd
#' @export
coverage_what <- function(x) {
  if (is.data.frame(x)) {
    nm <- names(x)
    if (all(c("weightingTotal", "rate", "landings", "coverage") %in% nm))
      return("discard")
    if (all(c("distributionType", "total", "coverage") %in% nm))
      return(if (any(x$distributionType %in% "Age")) "age" else "length")
    return(NA_character_)
  }
  out <- switch(tolower(trimws(as.character(x))),
    discard = , discards = "discard",
    length = , lengths = , len = "length",
    age = , ages = "age",
    NA_character_)
  if (is.na(out))
    stop("`what` must be \"discard\", \"length\" or \"age\" (got \"", x,
         "\")", call. = FALSE)
  out
}

#' Pool a coverage table to the cells a coverage plot draws
#'
#' Turns raw catch (or a [build_coverage_strata()] table) into one row per
#' plotted cell: builds the table at exactly the plotted columns when
#' given raw catch, and sums a table that is finer than the plot's roles
#' (weights and trips add up; coverage and rate are re-derived from the
#' summed weights, never averaged). It also words what the cells are about,
#' once, for every coverage plot: the weight they are judged against, the
#' title and the phrase for what "covered" means.
#'
#' @param catch Raw CEF catch, or a [build_coverage_strata()] table.
#' @param distributions Distributions table (raw length / age only).
#' @param keys The plotted columns (y, x / fill, facet, ...).
#' @param what,what_given The `what` argument, and whether the caller
#'   actually typed it (a table brings its own).
#' @param catchCategory The `catchCategory` argument.
#' @param fn The calling function's name, for messages.
#' @return `NULL` (nothing to draw) or `list(cells, what, weight_label, title,
#'   share)`: the weight word (`"landings"`, `"discards"`, `"catch"`, `"BMS
#'   catch"`; always landings for discards, which are judged against them),
#'   the plot title ("Landings - Sampling of Length", "Sampling of
#'   Discards") and the phrase for the covered share ("share of landed weight
#'   with discard estimates").
#' @keywords internal
#' @noRd
#' @export
coverage_cells <- function(catch, distributions, keys, what, what_given,
                           catchCategory, fn) {
  keys <- unique(keys[!is.na(keys) & nzchar(keys)])
  tw <- coverage_what(catch)
  if (!is.na(tw)) {
    if (what_given && !identical(coverage_what(what), tw))
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
    what <- coverage_what(what)
    tbl <- build_coverage_strata(catch, keys, distributions, what,
                                 catchCategory)
  }
  if (is.null(tbl) || !nrow(tbl)) return(NULL)
  # A column that rides along from derive_fields() can be all `NA` where no
  # stratum pins it down - nothing to plot by. Treated the same as a column
  # that is not there, so asking to plot by one still names it, instead of
  # silently grouping everything into one NA level.
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

  # The weight the cells are judged against, in words: discards always against
  # the landings; length and age against the catch part(s) the table holds.
  parts <- if (identical(what, "discard")) NULL else unique(tbl$catchCategory)
  weight_label <- if (identical(what, "discard")) "landings"
    else switch(if (length(parts) == 1) parts else "All",
                Dis = "discards", BMS = "BMS catch", All = "catch", "landings")
  adjective <- switch(weight_label, landings = "landed", discards = "discarded",
                      weight_label)
  list(cells = as.data.frame(cells), what = what, weight_label = weight_label,
       title = if (identical(what, "discard")) "Sampling of Discards"
               else paste0(tools::toTitleCase(weight_label), " - Sampling of ",
                           if (identical(what, "age")) "Age" else "Length"),
       share = sprintf("share of %s weight with %s", adjective,
                       switch(what, length = "length distributions",
                              age = "age distributions", "discard estimates")))
}
