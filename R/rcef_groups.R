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
#  recommend_groups() lets the data choose them. The user names the variables
#  that matter, from the one that matters most to the one that matters least, and
#  the coverage a pool needs; every stratum is raised from the narrowest pool that
#  has it.
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
#' the variables the groups are made by, from the one that matters most to the
#' one that matters least - the cluster plot of [build_discard_clusters()] ranks
#' them - and how well a pool has to be sampled; every stratum is raised from the
#' narrowest pool that is. What comes back holds the two lists of conditions
#' those functions take, and tables that show how the strata fell into the
#' levels - see [plot_groups()] and [plot_river()] to draw them.
#'
#' @details
#' ## The levels
#'
#' A stratum is one combination of the `hierarchy` variables. With
#' `c("areaValue", "mesh", "vesselFlagCountry", "seasonValue")` the pools a
#' stratum can be raised from, from the narrowest to the widest, are
#'
#' | level | the pool | pooled across |
#' |:--|:--|:--|
#' | 4 | the stratum itself: its area, mesh, country and season | nothing |
#' | 3 | the same area, mesh and country | seasons |
#' | 2 | the same area and mesh | countries and seasons |
#' | 1 | the same area | meshes, countries and seasons |
#' | 0 | everything | all of them |
#'
#' A stratum takes the narrowest level whose pool reaches the `thresholds`, so
#' the last variable of the hierarchy is pooled across first and the first one is
#' kept the longest. Level 0 is the last resort and is always accepted (a warning
#' says when it does not reach the thresholds), so that nothing is left without a
#' group.
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
#' These are the numbers [plot_coverage_heat()] and the other coverage plots
#' draw, and they are pooled the same way: weights and counts add up, and the
#' coverage is worked out again from the summed weights.
#'
#' ## Discards and lengths
#'
#' For discards give the catch as it is (after [derive_fields()]): the coverage is
#' the share of the landings that have a discard estimate, and the groups go to
#' [catchRaising()]. For lengths or ages also give the `distribution_data` and the
#' catch the allocation is run on - the raised catch, the result of
#' [catchRaising()]: the coverage is the share of the weight that has a
#' distribution, and the groups go to [catchAllocationAoL()]. Landings and
#' discards are two catch parts with their own coverage, so they are grouped on
#' their own, and the conditions say which (`catchCategory == "Lan"`).
#'
#' ## The groups
#'
#' A group is named after its level and a number: `L3_02` is the second pool of
#' level 3, `L0` the pool of everything (`Lan_L3_02` for the landings, when
#' there are lengths). `strataCond` selects the strata a group raises - every
#' row of them, so the groups share the catch out without overlap - and
#' `matchedDataCond` the rows of the pool its discard ratio (or its distribution)
#' is taken from, which are not only the strata it raises: strata that took a
#' narrower pool give their rows to the wider pools too.
#'
#' @param catch_data The catch, a CEF long-format table that has the `hierarchy`
#'   columns (see [derive_fields()]): for discards the catch as it comes, for
#'   lengths and ages the raised catch. One year and one stock.
#' @param hierarchy The variables the groups are made by, from the one that
#'   matters most to the one that matters least: the first is kept the longest,
#'   the last is pooled across first. Columns of `catch_data`.
#' @param thresholds A list: `cov` (the coverage a pool has to reach, in
#'   percent; default 50, cannot be empty), `trips` (the least number of trips)
#'   and `fish` (the least number of fish measured; lengths and ages), the last
#'   two `NA` (default) when not used.
#' @param distribution_data The CEF distributions, for the groups of the lengths
#'   or the ages. `NULL` (default): the groups of the discards.
#' @param distributionType `"LengthTotal"` (default) or `"Age"`: the distributions
#'   the coverage is judged on. Only with `distribution_data`.
#' @return A list of class `rcef_groups`, which prints as its table of levels:
#'   * `strataCond` and `matchedDataCond`: named lists of quosures, one per group,
#'     for the arguments of [catchRaising()] (`condition_raising_st_list`,
#'     `condition_matched_data_list`) or [catchAllocationAoL()]
#'     (`condition_alloc_st_list`, `condition_matched_data_list`);
#'   * `strata`: one row per stratum (and catch part, for lengths and ages): its
#'     weight and coverage, the level and group it falls in, and what the pool it
#'     was given has to stand on;
#'   * `groups`: one row per group, its pool and what it holds;
#'   * `levels`: one row per level - how many strata and how much weight each
#'     takes, and how much of what has to be raised it settles;
#'   * `what`, `hierarchy` and `thresholds`, as they were used.
#' @seealso [plot_groups()] and [plot_river()] to look at how the strata fell
#'   into the levels; [build_coverage_strata()] for the numbers behind them;
#'   [build_discard_clusters()] to choose the variables; [catchRaising()] and
#'   [catchAllocationAoL()], which take the groups.
#' @family raising groups
#' @md
#' @examples
#' \dontrun{
#' hierarchy  <- c("areaValue", "mesh", "vesselFlagCountry", "seasonValue")
#' thresholds <- list(cov = 50, trips = NA, fish = NA)
#'
#' # the groups of the discards, and the raising they go to
#' rec <- recommend_groups(catch_data, hierarchy, thresholds)
#' rec                                  # how the strata fell into the levels
#' raised <- catchRaising(catch_data, rec$strataCond, rec$matchedDataCond,
#'                        type = "discards", logFile = tempfile(fileext = ".txt"))
#'
#' # the groups of the lengths of the raised catch: at least 100 fish measured
#' rec_len <- recommend_groups(raised, hierarchy,
#'                             list(cov = 50, trips = NA, fish = 100),
#'                             distribution_data = distributions_data)
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
                             distributionType = c("LengthTotal", "Age")) {
  fn <- "recommend_groups"
  what <- if (is.null(distribution_data)) "discard"
          else if (identical(match.arg(distributionType), "Age")) "age" else "length"
  catch_data <- as.data.frame(catch_data)

  ## ---- what was asked for
  hierarchy <- unique(as.character(hierarchy))
  if (!length(hierarchy) || anyNA(hierarchy) || !all(nzchar(hierarchy)))
    stop(fn, "(): `hierarchy` names the variables the groups are made by, from ",
         "the one that matters most to the one that matters least", call. = FALSE)
  if ("catchCategory" %in% hierarchy)
    stop(fn, "(): `catchCategory` is not a stratum variable: the landings and the ",
         "discards of a stratum go together (lengths and ages are grouped for each ",
         "catch part on its own)", call. = FALSE)
  miss <- setdiff(hierarchy, names(catch_data))
  if (length(miss))
    stop(fn, "(): `catch_data` has no column ", paste0("'", miss, "'", collapse = ", "),
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
  ## finest level and pooled to every level above it
  fine <- build_coverage_strata(catch_data, hierarchy, distribution_data, what)
  if (is.null(fine) || !nrow(fine))
    stop(fn, "(): nothing to make groups of: the catch has no ",
         if (identical(what, "discard")) "landings or discards" else "catch", call. = FALSE)
  cat_key <- if (identical(what, "discard")) character() else "catchCategory"
  n <- length(hierarchy)
  join <- function(x) if (length(x) < 2) x
                      else paste(paste(x[-length(x)], collapse = ", "), "and", x[length(x)])
  pools <- lapply(stats::setNames(n:0, n:0), function(k) {
    keys <- c(hierarchy[seq_len(k)], cat_key)
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
  pool_of <- function(rows, k) {
    keys <- c(hierarchy[seq_len(k)], cat_key)
    p <- pools[[as.character(k)]]
    if (!length(keys)) return(p[rep(1L, nrow(rows)), , drop = FALSE])
    dplyr::left_join(rows[keys], p, by = keys)
  }

  ## the strata: one row per combination of the hierarchy (and catch part)
  s <- fine[c(hierarchy, cat_key)]
  s$weight_t  <- if (identical(what, "discard")) fine$landings else fine$total
  s$covered_t <- if (identical(what, "discard")) fine$weightingTotal
                 else ifelse(is.na(fine$coverage), 0, fine$total * fine$coverage / 100)
  s$coverage <- fine$coverage
  s$numTrips <- fine$numTrips
  s$numMeasurements <- if (identical(what, "discard")) rep(NA_real_, nrow(s)) else fine$numMeasurements
  if (!is.na(th$trips) && all(is.na(s$numTrips)))
    warning(fn, "(): `thresholds$trips` is used but the catch has no numTrips: no pool ",
            "reaches it, so every stratum goes to the pool of everything", call. = FALSE)

  ## ---- the level of each stratum: the narrowest pool that reaches the thresholds
  level <- rep(NA_integer_, nrow(s))
  for (k in n:1) {
    keys <- c(hierarchy[seq_len(k)], cat_key)
    p <- pools[[as.character(k)]]; p$.ok <- reached(p)
    hit <- dplyr::left_join(s[keys], p[c(keys, ".ok")], by = keys)$.ok
    level[is.na(level) & hit %in% TRUE] <- k
  }
  level[is.na(level)] <- 0L
  last  <- pools[["0"]]
  short <- !reached(last)
  if (any(short)) {
    in0 <- level == 0L & (if (length(cat_key)) s$catchCategory %in% last$catchCategory[short] else TRUE)
    if (any(in0))
      warning(fn, "(): the pool of everything does not reach the thresholds (",
              paste0(if (length(cat_key)) paste0(last$catchCategory[short], ": "),
                     "coverage ", format(round(last$coverage[short], 1)), "%",
                     if (!is.na(th$trips)) paste0(", ", last$numTrips[short], " trips"),
                     if (!is.na(th$fish)) paste0(", ", last$numMeasurements[short], " fish"),
                     collapse = "; "),
              "), and the ", sum(in0), " strata that fall in it are ",
              if (identical(what, "discard")) "raised" else "allocated", " from it all the same",
              call. = FALSE)
  }

  ## ---- the groups: the distinct pools in use
  s$level <- level
  s$category <- if (length(cat_key)) as.character(s$catchCategory) else ""
  s$.key <- do.call(paste, c(unname(as.list(s[hierarchy])), sep = " | "))
  s$.g <- do.call(paste, c(list(s$category, level),
                           lapply(seq_len(n), function(j)
                             ifelse(level >= j, as.character(s[[hierarchy[j]]]), "*")),
                           sep = "\r"))
  g <- s[!duplicated(s$.g), c(".g", "category", "level", hierarchy), drop = FALSE]
  g$n_strata <- tabulate(match(s$.g, g$.g), nrow(g))
  g$weight_t <- as.numeric(tapply(s$weight_t, factor(s$.g, g$.g), sum, na.rm = TRUE))
  g <- g[order(g$category, -g$level, -g$weight_t), , drop = FALSE]
  g$rank <- stats::ave(seq_len(nrow(g)), g$category, g$level, FUN = seq_along)
  size <- stats::ave(seq_len(nrow(g)), g$category, g$level, FUN = length)
  g$group <- paste0(ifelse(nzchar(g$category), paste0(g$category, "_"), ""), "L", g$level,
                    ifelse(g$level == 0L, "", sprintf("_%0*d", nchar(size), g$rank)))
  s$group <- g$group[match(s$.g, g$.g)]
  label_of <- function(k)
    if (k == n) "own stratum"
    else if (k == 0L) "everything"
    else paste("pooled across", join(hierarchy[(k + 1L):n]))
  level_label <- function(k) sprintf("L%d %s", k, vapply(k, label_of, ""))
  s$level_label <- level_label(s$level)

  ## what each stratum's pool stands on
  s$pool_coverage <- s$pool_trips <- s$pool_fish <- s$pool_strata <- NA_real_
  for (k in n:0) {
    rows <- which(level == k)
    if (!length(rows)) next
    p <- pool_of(s[rows, , drop = FALSE], k)
    s$pool_coverage[rows] <- p$coverage; s$pool_trips[rows] <- p$numTrips
    s$pool_fish[rows] <- p$numMeasurements; s$pool_strata[rows] <- p$n_strata
  }

  ## ---- the conditions catchRaising() and catchAllocationAoL() take
  key_call <- rlang::call2("paste", !!!rlang::syms(hierarchy), sep = " | ")
  in_keys  <- function(keys) rlang::call2("%in%", key_call, keys)
  cat_call <- function(cat) if (nzchar(cat)) rlang::call2("==", rlang::sym("catchCategory"), cat)
  and_all  <- function(parts) {
    parts <- Filter(Negate(is.null), parts)
    if (!length(parts)) TRUE else Reduce(function(a, b) rlang::call2("&", a, b), parts)
  }
  mk <- function(e) rlang::new_quosure(e, rlang::global_env())
  ## what the groups of a catch part claim by their keys; the pool of everything takes the rest
  claimed <- function(cat) unique(s$.key[s$category == cat & s$level > 0L])
  strataCond <- lapply(seq_len(nrow(g)), function(i) {
    body <- if (g$level[i] > 0L) in_keys(unique(s$.key[s$.g == g$.g[i]]))
            else { others <- claimed(g$category[i])
                   if (length(others)) rlang::call2("!", in_keys(others)) }
    mk(and_all(list(cat_call(g$category[i]), body)))
  })
  matchedDataCond <- lapply(seq_len(nrow(g)), function(i)
    mk(and_all(c(list(cat_call(g$category[i])),
                 lapply(seq_len(g$level[i]), function(j)
                   rlang::call2("%in%", rlang::sym(hierarchy[j]), g[[hierarchy[j]]][i]))))))
  names(strataCond) <- names(matchedDataCond) <- g$group

  ## every row of the catch in exactly one group - the raising leaves out what is in none
  member <- vapply(strataCond, function(q) {
    r <- rep_len(rlang::eval_tidy(q, data = catch_data), nrow(catch_data))
    r[is.na(r)] <- FALSE
    r
  }, logical(nrow(catch_data)))
  member <- matrix(member, nrow = nrow(catch_data))
  handled <- if (length(cat_key)) catch_data$catchCategory %in% g$category else TRUE
  bad <- handled & rowSums(member) != 1
  if (any(bad))
    stop(fn, "(): the groups do not share the catch out (", sum(bad), " rows are in no group ",
         "or in more than one) - this is a bug, please report it", call. = FALSE)

  ## ---- the tables
  pool_txt <- vapply(seq_len(nrow(g)), function(i)
    if (g$level[i] == 0L) "everything"
    else paste(hierarchy[seq_len(g$level[i])],
               vapply(seq_len(g$level[i]), function(j) as.character(g[[hierarchy[j]]][i]), ""),
               sep = " = ", collapse = ", "), "")
  first <- match(g$.g, s$.g)
  r1 <- function(x) round(x, 1); r3 <- function(x) round(x, 3)
  groups <- data.frame(group = g$group, category = g$category, level = g$level,
                       level_label = level_label(g$level), pool = pool_txt,
                       n_strata = g$n_strata, weight_t = r3(g$weight_t),
                       coverage = r1(s$pool_coverage[first]), numTrips = s$pool_trips[first],
                       numMeasurements = s$pool_fish[first], stringsAsFactors = FALSE)
  strata <- s[order(match(s$group, g$group), -s$weight_t),
              c(hierarchy, cat_key, "weight_t", "covered_t", "coverage", "numTrips",
                if (!identical(what, "discard")) "numMeasurements", "level", "level_label",
                "group", "pool_coverage", "pool_trips",
                if (!identical(what, "discard")) "pool_fish", "pool_strata")]
  strata$weight_t <- r3(strata$weight_t); strata$covered_t <- r3(strata$covered_t)
  strata$coverage <- r1(strata$coverage); strata$pool_coverage <- r1(strata$pool_coverage)
  rownames(strata) <- NULL
  ## one row per level (and catch part): what falls in it, and how much of what has to be raised
  ## (the weight with no sample of its own) rests on it
  levels_tbl <- do.call(rbind, lapply(sort(unique(s$category)), function(cc)
    do.call(rbind, lapply(n:0, function(k) {
      i <- s$category == cc & s$level == k
      data.frame(category = cc, level = k, label = label_of(k),
                 n_groups = sum(g$category == cc & g$level == k), n_strata = sum(i),
                 weight_t = sum(s$weight_t[i], na.rm = TRUE),
                 uncovered_t = sum(s$weight_t[i] - s$covered_t[i], na.rm = TRUE),
                 stringsAsFactors = FALSE)
    }))))
  share <- function(v) {
    tot <- stats::ave(levels_tbl[[v]], levels_tbl$category, FUN = sum)
    ifelse(tot > 0, levels_tbl[[v]] / tot * 100, 0)
  }
  levels_tbl$pct_weight <- share("weight_t")
  levels_tbl$pct_uncovered <- share("uncovered_t")
  levels_tbl$cum_pct_uncovered <- stats::ave(levels_tbl$pct_uncovered, levels_tbl$category,
                                             FUN = cumsum)
  for (v in c("weight_t", "uncovered_t")) levels_tbl[[v]] <- r3(levels_tbl[[v]])
  for (v in c("pct_weight", "pct_uncovered", "cum_pct_uncovered")) levels_tbl[[v]] <- r1(levels_tbl[[v]])
  levels_tbl <- levels_tbl[c("category", "level", "label", "n_groups", "n_strata", "weight_t",
                             "pct_weight", "uncovered_t", "pct_uncovered", "cum_pct_uncovered")]
  rownames(levels_tbl) <- NULL
  if (!length(cat_key)) { levels_tbl$category <- NULL; groups$category <- NULL }
  if (identical(what, "discard")) groups$numMeasurements <- NULL

  structure(list(strataCond = strataCond, matchedDataCond = matchedDataCond,
                 strata = strata, groups = groups, levels = levels_tbl,
                 what = what, hierarchy = hierarchy, thresholds = th),
            class = "rcef_groups")
}

#' @export
print.rcef_groups <- function(x, ...) {
  th <- x$thresholds
  cat(sprintf("Groups for the %s: %d strata in %d groups\n",
              switch(x$what, discard = "raising of the discards",
                     length = "allocation of the lengths", "allocation of the ages"),
              nrow(x$strata), nrow(x$groups)))
  cat("hierarchy:  ", paste(x$hierarchy, collapse = " > "),
      "  (the last is pooled across first)\n", sep = "")
  cat("thresholds: coverage >= ", th$cov, "%",
      if (!is.na(th$trips)) paste0(", trips >= ", th$trips),
      if (!is.na(th$fish)) paste0(", fish >= ", th$fish), "\n\n", sep = "")
  print(x$levels, row.names = FALSE)
  invisible(x)
}
