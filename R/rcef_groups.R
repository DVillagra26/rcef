# =====================================================================
#  rcef_groups.R   (recommend_groups(): the groups of the raising and of the allocation, from a hierarchy)
#
#  ORIGIN: this project (ILVO / RCEF Workbench). Freely refactorable.
#  Was, in an earlier form, recommend_raising() / conds_from_auto() of the first
#  versions of the package (they went with the app).
#
#  catchRaising() and catchAllocationAoL() take groups: the strata that are raised
#  (or allocated) together, and the data their discard ratio (or their
#  distribution) is taken from. Written by hand that is two lists of conditions;
#  recommend_groups() lets the data choose them. The user names the dimensions
#  that matter, from the one that matters most to the one that matters least, and
#  the coverage a pool needs; every stratum is raised from the narrowest pool that
#  has it: from itself, then from the pool that is one variable wider, and so on to
#  All by All.
#
#  DEPENDENCIES  Imports: dplyr, rlang. The coverage of every pool is
#  build_coverage_strata() and coverage_cells() of rcef_coverage.R - the table
#  the coverage plots draw - so what the groups rest on is what the plots show.
# =====================================================================

#' Groups for the raising and the allocation, from a hierarchy of variables
#'
#' @description
#' Chooses the groups of [catchRaising()] (discards) or [catchAllocationAoL()]
#' (lengths and ages) from the data, instead of writing them by hand. You name
#' the dimensions the groups are made by, from the one that matters most to the one
#' that matters least - `c("areaValue", "mesh", "vesselFlagCountry",
#' "seasonValue")`; the cluster plot of [build_discard_clusters()] ranks them - and
#' how well a pool has to be sampled. What has no estimate of its own is raised
#' from the narrowest pool that is sampled well enough: first from its own
#' stratum, then from the pool that is one variable wider, and so on up to All by
#' All, so that the variable that matters most is the last one to be pooled
#' across. What comes back holds the two lists of conditions the two functions
#' take, and tables that show how the strata fell into the levels - see
#' [plot_recommended_groups()] and [plot_river()] to draw them.
#'
#' @details
#' ## The levels, from the narrowest to the widest
#'
#' Nothing is pooled until it has to be. Take
#' `c("areaValue", "mesh", "vesselFlagCountry", "seasonValue")` on a catch whose
#' strata are reported as area, season, country and fleet. The levels are
#' numbered from the narrowest, and a stratum is raised from the first of them
#' whose pool reaches the `thresholds`:
#'
#' | Level | The pool a stratum is raised from |
#' |:--|:--|
#' | L1 | **own stratum (raw)**: the strata as they are reported |
#' | L2 | **own stratum (aggregated)**: the reported strata summed into strata of the dimensions |
#' | L3 | pooled across `seasonValue` |
#' | L4 | pooled across `vesselFlagCountry` and `seasonValue` |
#' | L5 | pooled across `mesh`, `vesselFlagCountry` and `seasonValue` |
#' | L6 | **All by All** |
#'
#' 1. **L1, the strata as they are reported.** A landing that has a discard
#'    estimate (a discard row of its domain, as [catchRaising()] pairs them) needs
#'    no raising, and the raising leaves it as it is. Nothing is chosen for it: it
#'    is the part of the stock that stands on its own data. Only the landings
#'    without an estimate are raised, and what `recommend_groups()` chooses is
#'    the data their discard ratio is taken from. A stratum that has nothing left
#'    to raise gets the group `L1`. Printing the result says how much of the landings
#'    that is.
#' 2. **L2, the strata of the dimensions.** The reported strata are summed into
#'    strata made of the variables you named - here the area, the mesh, the
#'    country and the season: what you did not name (the metier, say) is summed
#'    over. Each of them is tested against the `thresholds` *before anything else is
#'    pooled*: a stratum whose landings with an estimate reach them is raised from
#'    itself. If the dimensions are the reported strata already (the area, the
#'    season, the country and the fleet) there is nothing to sum and this level is
#'    skipped: the own stratum is L1, and the numbers that follow move up by one.
#' 3. **L3 and on, across the last variable first.** The strata that fall short
#'    are pooled across the season - the same area, mesh and country in every
#'    season - and tested again; then across the country as well, then across the
#'    mesh. The pool that reaches the thresholds raises them.
#' 4. **The last level, All by All.** Everything pooled across all the variables.
#'    It is the last resort and is always accepted - a warning says when it does
#'    not reach the thresholds - so that nothing is left without a group.
#'
#' ```
#' reported strata: area + season + country + fleet     (landings with an estimate stay as they are)
#'   |  L1  own stratum (raw)
#'   |  summed over what is not named, e.g. the metier
#'   v
#' L2  area + mesh + country + season                   tested first
#'   |  pooled across the season
#'   v
#' L3  area + mesh + country
#'   |  pooled across the country
#'   v
#' L4  area + mesh
#'   |  pooled across the mesh
#'   v
#' L5  area
#'   |  pooled across the area
#'   v
#' L6  All by All                                       the last resort
#' ```
#'
#' So the variable that matters most, the area, is kept the longest and the one
#' that matters least, the season, is pooled across first. The order you write
#' them in is the hierarchy - one variable to a line, the most important first,
#' shows it in a script:
#'
#' ```r
#' hierarchy <- c("areaValue",          # matters most: the last one to be pooled across
#'                "mesh",
#'                "vesselFlagCountry",
#'                "seasonValue")        # matters least: the first one to be pooled across
#' ```
#'
#' Printing the result, and the figures of [plot_recommended_groups()], spell it
#' out as `areaValue > mesh > vesselFlagCountry > seasonValue`.
#'
#' A variable that is not in the hierarchy is not kept apart at any level - from
#' the first step on, the strata are summed over it. If the metier matters for the
#' discards, name it: `c("areaValue", "mesh", "metier4", "vesselFlagCountry")`. A
#' variable and a coarser version of it can both be named, the coarser one first:
#' `c("ICESDivision", "areaValue", "mesh")` pools the strata of a division across
#' their areas only when the pool of the area is not enough.
#'
#' ## The thresholds
#'
#' `thresholds` is a list with up to three values; leave out what you do not
#' use. A pool reaches the thresholds when it reaches every one that is used and
#' has some sampled weight in it (a pool with nothing sampled has no ratio, or
#' distribution, to give).
#'
#' * `cov`, the coverage in percent: the share of the pool's weight that has been
#'   sampled - landings with a discard estimate, as [catchRaising()] counts them
#'   (see `weightingTotal` of [build_coverage_strata()]), or catch with a length
#'   (or age) distribution. It cannot be empty; the default is 50.
#' * `trips`, the number of trips behind the pool (`numTrips`, added up over its
#'   strata). `NA` (default): not used.
#' * `fish`, the number of fish measured behind the pool (`numMeasurements`,
#'   added up over its strata): lengths and ages only. `NA` (default): not used.
#'
#' These are the numbers [plot_coverage_grid()] and the other coverage plots
#' draw, and they are pooled the same way: weights and counts add up, and the
#' coverage is worked out again from the summed weights. A pool takes the rows of
#' every stratum in it as its data - also those of the strata that were raised from
#' their own, which are the best-sampled ones.
#'
#' ## Discards, lengths and ages
#'
#' For discards give the catch as it is (after [derive_fields()]): the coverage is
#' the share of the landings that have a discard estimate, and the groups go to
#' [catchRaising()]. For lengths or ages also give the `distribution_data` and the
#' catch the allocation is run on - the raised catch, the result of
#' [catchRaising()]: the coverage is the share of the weight that has a
#' distribution, and the groups go to [catchAllocationAoL()]. Landings and
#' discards are two catch parts with their own coverage, and the conditions say
#' which (`catchCategory == "Lan"`).
#'
#' `drop_catchCategory` says what the catch part is among the variables. `TRUE`
#' (default): it is the *last* variable to be pooled across - every level above is
#' made within the landings or within the discards, the pool of everything in the
#' catch part ("All by All (per catch part)") is tested with the thresholds, and
#' only the strata that still fall short are pooled across the catch parts as well,
#' in an All by All of landings and discards together. `FALSE`: the catch part is
#' always kept - the landings are never allocated from the discards, nor the other
#' way round - and the last level is All by All within the catch part, accepted
#' whether or not it reaches the thresholds. It does not apply to discards, where
#' the landings are the stratum and the discards its estimate.
#'
#' ## The groups
#'
#' A group is named after its level and a number: `L3_02` is the second pool of
#' level 3, `L1` the strata with nothing to raise, `L6` the pool of All by All
#' (`Lan_L3_02` for the landings, when there are lengths). `strataCond` selects the
#' strata a group raises - every row of them, so the groups share the catch out
#' without overlap - and `matchedDataCond` the rows of the pool its discard ratio
#' (or its distribution) is taken from, which are not only the strata it raises:
#' strata that took a narrower pool give their rows to the wider pools too.
#'
#' @param catch_data The catch, a CEF long-format table that has the `hierarchy`
#'   columns (see [derive_fields()]): for discards the catch as it comes, for
#'   lengths and ages the raised catch. One year and one stock.
#' @param hierarchy The dimensions the groups are made by, from the one that
#'   matters most to the one that matters least:
#'   `c("areaValue", "mesh", "vesselFlagCountry", "seasonValue")` (the area matters
#'   more than the mesh, the mesh more than the country ...). The first is kept the
#'   longest, the last is pooled across first. Columns of `catch_data`; what is not
#'   named is summed over from the start.
#' @param thresholds A list: `cov` (the coverage a pool has to reach, in
#'   percent; default 50, cannot be empty), `trips` (the least number of trips)
#'   and `fish` (the least number of fish measured; lengths and ages), the last
#'   two `NA` (default) when not used.
#' @param distribution_data The CEF distributions, for the groups of the lengths
#'   or the ages. `NULL` (default): the groups of the discards.
#' @param distributionType `"LengthTotal"` (default) or `"Age"`: the distributions
#'   the coverage is judged on. Only with `distribution_data`.
#' @param drop_catchCategory Lengths and ages only: `TRUE` (default) makes the catch
#'   part the last variable to be pooled across, after the thresholds have been
#'   tested; `FALSE` keeps it at every level, the last one included. See
#'   "Discards, lengths and ages".
#' @return A list of class `rcef_groups`. Printing it says how much of the
#'   landings already has a discard estimate (a length or age distribution) and is
#'   kept as it is, and how many strata, groups and landings fall in each level.
#'   It holds
#'   * `strataCond` and `matchedDataCond`: named lists of quosures, one per group,
#'     for the arguments of [catchRaising()] (`condition_raising_st_list`,
#'     `condition_matched_data_list`) or [catchAllocationAoL()]
#'     (`condition_alloc_st_list`, `condition_matched_data_list`);
#'   * `strata`: one row per stratum of the hierarchy (and catch part, for lengths
#'     and ages): the hierarchy columns, `weight_t` (its landings, or its catch, in
#'     tonnes), `covered_t` (the part of it that has an estimate or a distribution
#'     of its own: what is not raised), `raised_t` (the rest), `coverage` (in
#'     percent), `numTrips` (and `numMeasurements`), the `level`, `level_label` and
#'     `group` it falls in - the level its `raised_t` comes from; a stratum with
#'     nothing to raise is level 1 - and what the pool it was given stands on:
#'     `pool_coverage`, `pool_trips` (and `pool_fish`) and `pool_strata`, how many
#'     strata it holds;
#'   * `groups`: one row per group: `group`, `level`, `level_label`, its `pool` in
#'     words, `n_strata`, `weight_t` and `raised_t` of what it raises, and the
#'     `coverage`, `numTrips` (and `numMeasurements`) of its pool;
#'   * `levels`: one row per level (and catch part): `n_groups`, `n_strata` (the
#'     strata that take the level for what they raise; at level 1 those with
#'     nothing to raise), `covered_t` (at level 1: the weight of all the strata
#'     that has an estimate of its own), `raised_t` (what the level's pools raise),
#'     `weight_t` (the two together: the weight that rests on the level) and
#'     `pct_weight`, `pct_raised` and `cum_pct_raised`: the share of the weight,
#'     the share of everything that has to be raised that the level settles, and
#'     the running total of that from the narrowest level;
#'   * `what`, `hierarchy`, `thresholds` and `drop_catchCategory`, as they were used.
#' @seealso [plot_recommended_groups()] and [plot_river()] to look at how the
#'   strata fell into the levels; [build_coverage_strata()] for the numbers behind
#'   them; [build_discard_clusters()] to choose the variables; [catchRaising()] and
#'   [catchAllocationAoL()], which take the groups.
#' @family raising groups
#' @md
#' @examples
#' # a small catch (kg): a landing has a discard estimate when it has a domain
#' # (domainCatchDis) that a discard row carries too
#' catch <- data.frame(
#'   catchCategory  = c("Lan", "Lan", "Lan", "Lan", "Lan", "Lan", "Dis", "Dis"),
#'   areaValue      = c("27.4", "27.4", "27.4", "27.7.d", "27.7.d", "27.7.d", "27.4", "27.7.d"),
#'   mesh           = c("70-99", "70-99", "100-119", "70-99", "70-99", "100-119", "70-99", "70-99"),
#'   total          = c(3000, 1000, 2000, 800, 200, 500, 200, 40),
#'   domainCatchDis = c("d1", NA, NA, "d2", NA, NA, "d1", "d2"),
#'   variableUnit   = "kg", stringsAsFactors = FALSE)
#'
#' # the area matters more than the mesh: the mesh is pooled across first
#' rec <- recommend_groups(catch, c("areaValue", "mesh"), list(cov = 50))
#' rec                       # what needs no raising, and how the strata fell into the levels
#' rec$strata                # 70-99 stands on its own; 100-119 is raised from its area
#' rec$strataCond            # what catchRaising() takes
#' rec$matchedDataCond
#'
#' \dontrun{
#' hierarchy  <- c("areaValue",          # matters most: the last one to be pooled across
#'                 "mesh",
#'                 "vesselFlagCountry",
#'                 "seasonValue")        # matters least: the first one to be pooled across
#' thresholds <- list(cov = 50, trips = NA, fish = NA)
#'
#' # the groups of the discards, and the raising they go to
#' rec <- recommend_groups(catch_data, hierarchy, thresholds)
#' rec                                  # how the strata fell into the levels
#' raised <- catchRaising(catch_data, rec$strataCond, rec$matchedDataCond,
#'                        type = "discards", logFile = tempfile(fileext = ".txt"))
#'
#' # the groups of the lengths of the raised catch: at least 100 fish measured, the
#' # landings never allocated from the discards
#' rec_len <- recommend_groups(raised, hierarchy,
#'                             list(cov = 50, trips = NA, fish = 100),
#'                             distribution_data = distributions_data,
#'                             drop_catchCategory = FALSE)
#' alloc <- catchAllocationAoL(raised, distributions_data,
#'                             rec_len$strataCond, rec_len$matchedDataCond,
#'                             distributionType = "LengthTotal",
#'                             logFile = tempfile(fileext = ".txt"))
#' }
#' @export
recommend_groups <- function(catch_data,
                             hierarchy,
                             thresholds = list(cov = 50, trips = NA, fish = NA),
                             distribution_data = NULL,
                             distributionType = c("LengthTotal", "Age"),
                             drop_catchCategory = TRUE) {
  fn <- "recommend_groups"
  what <- if (is.null(distribution_data)) "discard"
          else if (identical(match.arg(distributionType), "Age")) "age" else "length"
  bio <- !identical(what, "discard")
  catch_data <- as.data.frame(catch_data)
  if (!(is.logical(drop_catchCategory) && length(drop_catchCategory) == 1L && !is.na(drop_catchCategory)))
    stop(fn, "(): `drop_catchCategory` is TRUE or FALSE", call. = FALSE)

  ## ---- what was asked for
  ## c("areaValue", "mesh", "vesselFlagCountry"): the variable that matters most first, the one
  ## that is pooled across first last
  hierarchy <- trimws(as.character(hierarchy))
  if (!length(hierarchy) || anyNA(hierarchy) || !all(nzchar(hierarchy)))
    stop(fn, "(): `hierarchy` names the variables the groups are made by, from the one that ",
         "matters most to the one that matters least: c(\"areaValue\", \"mesh\", \"vesselFlagCountry\")",
         call. = FALSE)
  if (anyDuplicated(hierarchy))
    stop(fn, "(): `hierarchy` names ", paste0("'", unique(hierarchy[duplicated(hierarchy)]), "'", collapse = ", "),
         " more than once", call. = FALSE)
  if ("catchCategory" %in% hierarchy)
    stop(fn, "(): `catchCategory` is not one of the variables of `hierarchy`: the landings and the ",
         "discards of a stratum go together, and for lengths and ages the catch part is the last ",
         "variable to be pooled across - or never, with drop_catchCategory = FALSE", call. = FALSE)
  miss <- setdiff(hierarchy, names(catch_data))
  if (length(miss))
    stop(fn, "(): `catch_data` has no column ", paste0("'", miss, "'", collapse = ", "),
         if (any(grepl("[,;>[:space:]]", miss)))
           " (name the variables one by one: c(\"areaValue\", \"mesh\", \"vesselFlagCountry\"))",
         derive_hint(miss), call. = FALSE)
  for (k in intersect(c("year", "stock"), names(catch_data)))
    if (dplyr::n_distinct(catch_data[[k]]) > 1)
      stop(fn, "(): `catch_data` has more than one ", k, " (",
           paste(sort(unique(catch_data[[k]])), collapse = ", "), "): the groups are ",
           "made within one year and one stock, give them one at a time", call. = FALSE)

  th <- list(cov = 50, trips = NA_real_, fish = NA_real_)
  if (!is.list(thresholds) || (length(thresholds) && is.null(names(thresholds))))
    stop(fn, "(): `thresholds` is a named list, e.g. list(cov = 50, trips = NA, fish = 100)",
         call. = FALSE)
  unknown <- setdiff(names(thresholds), names(th))
  if (length(unknown))
    stop(fn, "(): `thresholds` has no ", paste0("'", unknown, "'", collapse = ", "),
         ": it takes cov, trips and fish", call. = FALSE)
  for (k in names(thresholds)) th[[k]] <- thresholds[[k]]
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

  ## ---- the coverage of every pool: the table of the plots, built once at the
  ## strata of the dimensions and pooled to every level above it
  fine <- build_coverage_strata(catch_data, hierarchy, distribution_data, what)
  if (is.null(fine) || !nrow(fine))
    stop(fn, "(): nothing to make groups of: the catch has no ",
         if (identical(what, "discard")) "landings or discards" else "catch", call. = FALSE)
  cat_key <- if (bio) "catchCategory" else character()
  n <- length(hierarchy)
  join <- function(x) if (length(x) < 2) x
                      else paste(paste(x[-length(x)], collapse = ", "), "and", x[length(x)])

  ## ---- the levels. L1 is the stratum as it is reported (what has an estimate of
  ## its own); then the pools, from the stratum of the dimensions to All by All. The
  ## stratum of the dimensions is the reported one when the dimensions name the whole
  ## reported stratum, and then there is no level of its own for it.
  raw_dims <- intersect(c("fleetValue", "areaValue", "vesselFlagCountry", "seasonValue"),
                        names(catch_data))
  skip2 <- length(raw_dims) > 0 && all(raw_dims %in% hierarchy)
  drop_cat <- bio && drop_catchCategory
  spec <- data.frame(k   = c(n:0, if (drop_cat) 0L),
                     cat = c(rep(bio, n + 1L), if (drop_cat) FALSE))
  spec$level <- seq_len(nrow(spec)) + if (skip2) 0L else 1L
  spec$label <- vapply(seq_len(nrow(spec)), function(i) {
    k <- spec$k[i]
    if (k == n) { if (skip2) "own stratum (raw)" else "own stratum (aggregated)" }
    else if (k == 0L) { if (spec$cat[i]) "All by All (per catch part)" else "All by All" }
    else paste("pooled across", join(hierarchy[(k + 1L):n]))
  }, "")
  if (!bio) spec$label[nrow(spec)] <- "All by All"
  all_levels <- data.frame(level = c(if (!skip2) 1L, spec$level),
                           label = c(if (!skip2) "own stratum (raw)", spec$label),
                           stringsAsFactors = FALSE)
  level_label <- function(l) sprintf("L%d %s", l, all_levels$label[match(l, all_levels$level)])
  keys_of <- function(i) c(hierarchy[seq_len(spec$k[i])], if (spec$cat[i]) "catchCategory")
  last_i <- nrow(spec)

  pools <- lapply(seq_len(nrow(spec)), function(i) {
    keys <- keys_of(i)
    p <- coverage_cells(fine, NULL, keys, what, FALSE, NULL, fn)$cells
    data.frame(p[keys],
               weight_t = if (identical(what, "discard")) p$landings else p$total,
               covered_t = if (identical(what, "discard")) p$weightingTotal else p$covered,
               coverage = p$coverage, numTrips = p$numTrips,
               numMeasurements = if (identical(what, "discard")) rep(NA_real_, nrow(p))
                                 else p$numMeasurements,
               n_strata = p$n_strata, stringsAsFactors = FALSE)
  })
  reached <- function(p) {
    ## a pool with nothing sampled in it has no ratio (or distribution) to give, whatever the threshold
    r <- (p$coverage >= th$cov) %in% TRUE & (p$covered_t > 0) %in% TRUE
    if (!is.na(th$trips)) r <- r & !is.na(p$numTrips) & p$numTrips >= th$trips
    if (!is.na(th$fish)) r <- r & !is.na(p$numMeasurements) & p$numMeasurements >= th$fish
    r
  }
  ## the pool of each stratum at one level: a row of that level's table for each
  ## row of `rows` of the strata
  pool_of <- function(rows, i) {
    keys <- keys_of(i)
    p <- pools[[i]]
    if (!length(keys)) return(p[rep(1L, nrow(rows)), , drop = FALSE])
    dplyr::left_join(rows[keys], p, by = keys)
  }

  ## the strata: one row per combination of the hierarchy (and catch part)
  s <- fine[c(hierarchy, cat_key)]
  zero <- function(v) ifelse(is.na(v), 0, v)
  s$weight_t  <- zero(if (identical(what, "discard")) fine$landings else fine$total)
  s$covered_t <- zero(if (identical(what, "discard")) fine$weightingTotal
                      else fine$total * fine$coverage / 100)
  s$raised_t  <- pmax(s$weight_t - s$covered_t, 0)
  s$coverage <- fine$coverage
  s$numTrips <- fine$numTrips
  s$numMeasurements <- if (identical(what, "discard")) rep(NA_real_, nrow(s)) else fine$numMeasurements
  if (!is.na(th$trips) && all(is.na(s$numTrips)))
    warning(fn, "(): `thresholds$trips` is used but the catch has no numTrips: no pool ",
            "reaches it, so every stratum goes to All by All", call. = FALSE)

  ## ---- the level of each stratum: the narrowest pool that reaches the thresholds,
  ## for the weight that has to be raised; a stratum with nothing to raise is L1
  need  <- s$raised_t > 1e-9
  level <- ifelse(need, NA_integer_, 1L)
  for (i in seq_len(last_i)) {
    open <- is.na(level)
    if (!any(open)) break
    if (i < last_i) {
      keys <- keys_of(i)
      p <- pools[[i]]; p$.ok <- reached(p)
      hit <- if (length(keys)) dplyr::left_join(s[keys], p[c(keys, ".ok")], by = keys)$.ok
             else rep(p$.ok[1], nrow(s))
      level[open & hit %in% TRUE] <- spec$level[i]
    } else level[open] <- spec$level[i]
  }
  last  <- pools[[last_i]]
  short <- !reached(last)
  if (any(short)) {
    cats_in <- if (spec$cat[last_i]) last$catchCategory[short] else NULL
    in_last <- need & level == spec$level[last_i] &
      (if (spec$cat[last_i]) s$catchCategory %in% cats_in else TRUE)
    if (any(in_last))
      warning(fn, "(): the pool of All by All does not reach the thresholds (",
              paste0(if (spec$cat[last_i]) paste0(last$catchCategory[short], ": "),
                     "coverage ", format(round(last$coverage[short], 1)), "%",
                     if (!is.na(th$trips)) paste0(", ", last$numTrips[short], " trips"),
                     if (!is.na(th$fish)) paste0(", ", last$numMeasurements[short], " fish"),
                     collapse = "; "),
              "), and the ", sum(in_last), " strata that fall in it are ",
              if (identical(what, "discard")) "raised" else "allocated", " from it all the same",
              call. = FALSE)
  }

  ## ---- the groups: the distinct pools in use, and one group of the strata with
  ## nothing to raise
  s$level <- level
  s$category <- if (bio) as.character(s$catchCategory) else ""
  s$.key <- do.call(paste, c(unname(as.list(s[hierarchy])), sep = " | "))
  spec_i <- match(level, spec$level)                      # NA: L1 of a stratum with nothing to raise
  kind   <- ifelse(!need, "nothing",
            ifelse(spec_i == last_i, "last", ifelse(spec_i == 1L, "own", "pool")))
  kept   <- ifelse(kind == "nothing", 0L, spec$k[spec_i]) # how many variables its pool keeps
  keep_cat <- ifelse(kind == "nothing", TRUE, spec$cat[spec_i])
  catpart  <- ifelse(keep_cat, s$category, "")
  vals <- lapply(seq_len(n), function(j)
    ifelse(kept >= j, as.character(s[[hierarchy[j]]]), "*"))
  s$.g <- do.call(paste, c(list(catpart, level, kind), vals, sep = "\r"))
  g <- data.frame(.g = s$.g[!duplicated(s$.g)], stringsAsFactors = FALSE)
  at <- match(g$.g, s$.g)
  g$category <- catpart[at]; g$level <- level[at]; g$kind <- kind[at]; g$k <- kept[at]
  gval <- lapply(vals, function(v) v[at])
  g$n_strata <- tabulate(match(s$.g, g$.g), nrow(g))
  g$weight_t <- as.numeric(tapply(s$weight_t, factor(s$.g, g$.g), sum, na.rm = TRUE))
  g$raised_t <- as.numeric(tapply(s$raised_t, factor(s$.g, g$.g), sum, na.rm = TRUE))
  ## the landings before the discards, then the narrowest level first
  cats <- unique(s$category)
  cats <- c(intersect(c("Lan", "Dis"), cats), setdiff(sort(cats), c("Lan", "Dis")))
  ord <- order(match(g$category, c(cats, "")), g$level, g$kind, -g$weight_t)
  g <- g[ord, , drop = FALSE]; gval <- lapply(gval, function(v) v[ord])
  numbered <- g$kind %in% c("own", "pool") & g$k > 0L        # a pool of everything in a catch part is one group
  slot <- paste(g$category, g$level, numbered)
  g$rank <- stats::ave(seq_len(nrow(g)), slot, FUN = seq_along)
  size <- stats::ave(seq_len(nrow(g)), slot, FUN = length)
  g$group <- paste0(ifelse(nzchar(g$category), paste0(g$category, "_"), ""), "L", g$level,
                    ifelse(numbered, sprintf("_%0*d", nchar(size), g$rank), ""))
  s$group <- g$group[match(s$.g, g$.g)]
  s$level_label <- level_label(s$level)

  ## what each stratum's pool stands on (nothing is raised for a stratum with nothing to raise)
  s$pool_coverage <- s$pool_trips <- s$pool_fish <- s$pool_strata <- NA_real_
  for (i in seq_len(last_i)) {
    rows <- which(need & level == spec$level[i])
    if (!length(rows)) next
    p <- pool_of(s[rows, , drop = FALSE], i)
    s$pool_coverage[rows] <- p$coverage; s$pool_trips[rows] <- p$numTrips
    s$pool_fish[rows] <- p$numMeasurements; s$pool_strata[rows] <- p$n_strata
  }

  ## ---- the conditions catchRaising() and catchAllocationAoL() take
  key_call  <- rlang::call2("paste", !!!rlang::syms(hierarchy), sep = " | ")
  pair_call <- rlang::call2("paste", rlang::sym("catchCategory"), !!!rlang::syms(hierarchy), sep = " | ")
  in_keys  <- function(keys) rlang::call2("%in%", key_call, keys)
  cat_call <- function(cat) if (nzchar(cat)) rlang::call2("==", rlang::sym("catchCategory"), cat)
  and_all  <- function(parts) {
    parts <- Filter(Negate(is.null), parts)
    if (!length(parts)) TRUE else Reduce(function(a, b) rlang::call2("&", a, b), parts)
  }
  mk <- function(e) rlang::new_quosure(e, rlang::global_env())
  ## the last group takes what the others do not claim, so that every row of the catch is in a group
  rest <- function(i) {
    if (nzchar(g$category[i]) || !bio) {
      claimed <- unique(s$.key[!(kind == "last") & s$category == g$category[i]])
      cc <- if (nzchar(g$category[i])) cat_call(g$category[i]) else NULL
      and_all(list(cc, if (length(claimed)) rlang::call2("!", in_keys(claimed))))
    } else {
      claimed <- unique(paste(s$category, s$.key, sep = " | ")[kind != "last"])
      and_all(list(rlang::call2("%in%", rlang::sym("catchCategory"), unique(s$category)),
                   if (length(claimed)) rlang::call2("!", rlang::call2("%in%", pair_call, claimed))))
    }
  }
  strataCond <- lapply(seq_len(nrow(g)), function(i)
    mk(if (g$kind[i] == "last") rest(i)
       else and_all(list(cat_call(g$category[i]), in_keys(unique(s$.key[s$.g == g$.g[i]]))))))
  matchedDataCond <- lapply(seq_len(nrow(g)), function(i)
    mk(if (g$kind[i] == "nothing") and_all(list(cat_call(g$category[i]),
                                                in_keys(unique(s$.key[s$.g == g$.g[i]]))))
       else and_all(c(list(cat_call(g$category[i])),
                      lapply(seq_len(g$k[i]), function(j)
                        rlang::call2("%in%", rlang::sym(hierarchy[j]), gval[[j]][i]))))))
  names(strataCond) <- names(matchedDataCond) <- g$group

  ## every row of the catch in exactly one group - the raising leaves out what is in none
  member <- vapply(strataCond, function(q) {
    r <- rep_len(rlang::eval_tidy(q, data = catch_data), nrow(catch_data))
    r[is.na(r)] <- FALSE
    r
  }, logical(nrow(catch_data)))
  member <- matrix(member, nrow = nrow(catch_data))
  handled <- if (bio) catch_data$catchCategory %in% s$category else TRUE
  bad <- handled & rowSums(member) != 1
  if (any(bad))
    stop(fn, "(): the groups do not share the catch out (", sum(bad), " rows are in no group ",
         "or in more than one) - this is a bug, please report it", call. = FALSE)

  ## ---- the tables
  pool_txt <- vapply(seq_len(nrow(g)), function(i)
    if (g$kind[i] == "nothing") "nothing to raise"
    else if (g$k[i] == 0L) "All by All"
    else paste(hierarchy[seq_len(g$k[i])],
               vapply(seq_len(g$k[i]), function(j) gval[[j]][i], ""),
               sep = " = ", collapse = ", "), "")
  first <- match(g$.g, s$.g)
  r1 <- function(x) round(x, 1); r3 <- function(x) round(x, 3)
  groups <- data.frame(group = g$group, category = g$category, level = g$level,
                       level_label = level_label(g$level), pool = pool_txt,
                       n_strata = g$n_strata, weight_t = r3(g$weight_t), raised_t = r3(g$raised_t),
                       coverage = r1(s$pool_coverage[first]), numTrips = s$pool_trips[first],
                       numMeasurements = s$pool_fish[first], stringsAsFactors = FALSE)
  strata <- s[order(match(s$group, g$group), -s$weight_t),
              c(hierarchy, cat_key, "weight_t", "covered_t", "raised_t", "coverage", "numTrips",
                if (bio) "numMeasurements", "level", "level_label",
                "group", "pool_coverage", "pool_trips",
                if (bio) "pool_fish", "pool_strata")]
  for (v in c("weight_t", "covered_t", "raised_t")) strata[[v]] <- r3(strata[[v]])
  strata$coverage <- r1(strata$coverage); strata$pool_coverage <- r1(strata$pool_coverage)
  rownames(strata) <- NULL
  ## one row per level (and catch part): the weight that rests on it - at L1 what has an estimate of
  ## its own, anywhere the weight its pools raise
  levels_tbl <- do.call(rbind, lapply(cats, function(cc)
    do.call(rbind, lapply(seq_len(nrow(all_levels)), function(r) {
      l <- all_levels$level[r]
      i <- s$category == cc & s$level == l
      data.frame(category = cc, level = l, label = all_levels$label[r],
                 n_groups = length(unique(s$group[i])), n_strata = sum(i),
                 covered_t = if (l == 1L) sum(s$covered_t[s$category == cc]) else 0,
                 raised_t = sum(s$raised_t[i]), stringsAsFactors = FALSE)
    }))))
  levels_tbl$weight_t <- levels_tbl$covered_t + levels_tbl$raised_t
  share <- function(v) {
    tot <- stats::ave(levels_tbl[[v]], levels_tbl$category, FUN = sum)
    ifelse(tot > 0, levels_tbl[[v]] / tot * 100, 0)
  }
  levels_tbl$pct_weight <- share("weight_t")
  levels_tbl$pct_raised <- share("raised_t")
  levels_tbl$cum_pct_raised <- stats::ave(levels_tbl$pct_raised, levels_tbl$category, FUN = cumsum)
  for (v in c("covered_t", "raised_t", "weight_t")) levels_tbl[[v]] <- r3(levels_tbl[[v]])
  for (v in c("pct_weight", "pct_raised", "cum_pct_raised")) levels_tbl[[v]] <- r1(levels_tbl[[v]])
  levels_tbl <- levels_tbl[c("category", "level", "label", "n_groups", "n_strata", "covered_t",
                             "raised_t", "weight_t", "pct_weight", "pct_raised", "cum_pct_raised")]
  rownames(levels_tbl) <- NULL
  if (!bio) { levels_tbl$category <- NULL; groups$category <- NULL; groups$numMeasurements <- NULL }

  structure(list(strataCond = strataCond, matchedDataCond = matchedDataCond,
                 strata = strata, groups = groups, levels = levels_tbl,
                 what = what, hierarchy = hierarchy, thresholds = th,
                 drop_catchCategory = drop_cat),
            class = "rcef_groups")
}

#' @rdname recommend_groups
#' @param x The result of `recommend_groups()`.
#' @param ... Not used.
#' @md
#' @export
print.rcef_groups <- function(x, ...) {
  th <- x$thresholds; lv <- x$levels; discard <- identical(x$what, "discard")
  cat(sprintf("Groups for the %s: %d strata in %d groups\n",
              switch(x$what, discard = "raising of the discards",
                     length = "allocation of the lengths", "allocation of the ages"),
              nrow(x$strata), nrow(x$groups)))
  cat("hierarchy:  ", paste(x$hierarchy, collapse = " > "),
      "  (the last is pooled across first",
      if (!discard) paste0("; the catch part ", if (isTRUE(x$drop_catchCategory)) "last of all" else "never"),
      ")\n", sep = "")
  cat("thresholds: coverage >= ", th$cov, "%",
      if (!is.na(th$trips)) paste0(", trips >= ", th$trips),
      if (!is.na(th$fish)) paste0(", fish >= ", th$fish), "\n\n", sep = "")
  ## before any pooling: what has an estimate (or a distribution) already, and is kept as it is
  ## - the rest is what the levels below take
  t1 <- function(v) format(round(v, 1), big.mark = ",", nsmall = 1, trim = TRUE)
  for (cc in if (is.null(lv$category)) "" else unique(lv$category)) {
    d <- if (nzchar(cc)) lv[lv$category == cc, , drop = FALSE] else lv
    all_t <- sum(d$weight_t); todo_t <- sum(d$raised_t)
    if (!(all_t > 0)) next
    cat(sprintf("%s %s t: %s t (%s%%) have %s and are %s; %s t (%s%%) are %s\n",
                if (discard) "Landings" else switch(cc, Lan = "Landings", Dis = "Discards", cc),
                t1(all_t), t1(all_t - todo_t), t1(100 * (all_t - todo_t) / all_t),
                switch(x$what, discard = "a discard estimate", length = "a length distribution",
                       "an age distribution"),
                if (discard) "imported as reported" else "kept as sampled",
                t1(todo_t), t1(100 * todo_t / all_t), if (discard) "raised" else "allocated"))
  }
  cat("\n")
  ## the weight that rests on each level (covered_t and raised_t of the table are what it is made of)
  print(lv[setdiff(names(lv), c("covered_t", "raised_t"))], row.names = FALSE)
  invisible(x)
}
