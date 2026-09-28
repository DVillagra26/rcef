# =====================================================================
#  rcef_alloc.R   (run_allocation() and the aggregation wrappers)
#
#  ORIGIN: this project (ILVO / RCEF Workbench). Freely refactorable.
#  Was: R_development/mod_alloc_raising.R
#  No shiny in this file. If it needs `input`, `session` or a tag,
#  it belongs in app/, not here.
# =====================================================================

# ============================================================
#  mod_alloc_raising.R
#  Running the length / age allocation through the ICES
#  catchAllocationAoL(), and comparing what came out.
# ============================================================

# ------------------------------------------------------------
#  Vocabulary bridge
# ------------------------------------------------------------
# The CEF exchange format writes distributionType as "LengthTotal";
# catchAllocationAoL() match.arg()s it against c("Age", "Length") and
# then filters the distribution table on the same value. Passing
# "LengthTotal" fails the match.arg, and passing "Length" silently
# matches no rows. So the value is translated for the call AND the
# column is rewritten to agree with it.
#' @noRd
#' @export
ALLOC_TYPE <- c(LengthTotal = "Length", Length = "Length", Age = "Age")

#' @noRd
#' @export
alloc_type_of <- function(app_type) {
  v <- unname(ALLOC_TYPE[app_type])
  if (is.na(v)) stop("no allocation vocabulary for distributionType '",
                     app_type, "'")
  v
}

# Rewrite distributionType so the function's internal filter finds the
# rows it was handed. Nothing else about the table is touched.
#' @noRd
#' @export
prep_for_alloc <- function(dist, app_type = "LengthTotal") {
  d <- as.data.frame(dist, stringsAsFactors = FALSE)
  target <- alloc_type_of(app_type)
  d <- d[d$distributionType %in% app_type, , drop = FALSE]
  if (nrow(d)) d$distributionType <- target
  d
}

# ------------------------------------------------------------
#  Running the allocation
# ------------------------------------------------------------
# groups: either the app's own group list (a named list, each group with
# $alloc and $matched conditions written as text) or the strataCond /
# matchedDataCond pair that define_raising_groups_auto() / _manual()
# return - the same pair the discard raising consumes. That pair is
# written against stratum_id, so alloc_dims must be the dims it was built
# on (the ids are rebuilt from them here).
# Returns the two tables catchAllocationAoL() produces, plus the
# condition checks, or an object of class "alloc_error" carrying the
# message - the app shows that rather than falling over.
#' @noRd
#' @export
run_allocation <- function(catch_raised, dist, groups,
                           alloc_dims = NULL,
                           app_type = "LengthTotal",
                           originType_catch = "WGEstimate",
                           variableType_catch = "WeightLive",
                           variableType_mean = "WeightLive",
                           weighting = "NumberAtAoL",
                           log_file = NULL, verbose = FALSE) {
  if (!length(groups))
    return(structure(list(message = "No allocation groups defined."),
                     class = "alloc_error"))
  for (f in c("catchAllocationAoL", "check_group_conditions"))
    if (!exists(f, mode = "function"))
      return(structure(list(message = paste0(f, "() is not loaded.")),
                       class = "alloc_error"))

  # Complemented, or catchAllocationAoL() refuses the whole set with
  # "N records assigned to more than one stratum". The app resolves
  # overlaps first-match-wins; the engine does not. The EMITTER has
  # complemented since it was written - this is the same helper - so the
  # script in the R tab was right while the run behind the button was
  # not, which is the worst way round for the two to disagree.
  # The strataCond / matchedDataCond pair is already a genuine partition
  # (conds_from_auto() / conds_from_rules() complement it), so it goes
  # through as it is.
  if (all(c("strataCond", "matchedDataCond") %in% names(groups))) {
    alloc_l <- groups$strataCond
    match_l <- groups$matchedDataCond
  } else {
    cnd <- alloc_conds_from_groups(groups)
    alloc_l <- cnd$allocStrataCond
    match_l <- cnd$bioMatchedCond
  }

  # The conditions are written against the DERIVED stratum fields
  # (metier4, Country, Area1, Season), which a raw CEF catches table does
  # not carry. Deriving here means the function works whether it is
  # handed the raw import or an already-derived table, instead of only
  # working from the one place that happened to derive first.
  catch_raised <- derive_fields(as.data.frame(catch_raised))
  # bioMatchedCond may say `coverage >= 50 & n_measurements >= 100`. Those
  # columns have to be present on the RAISED catch table or the condition
  # errors with "object 'coverage' not found" rather than filtering. The
  # metrics are computed from the distributions where present, so
  # n_measurements reflects what is actually measured; add_stratum_metrics
  # guarantees the column exists (0 where nothing is measured) so a
  # condition naming it never errors. If the dims are unknown we fall back
  # to the catch dims rather than skipping the join, because skipping it
  # is exactly the failure mode.
  jdims <- if (!is.null(alloc_dims) && length(alloc_dims)) alloc_dims
           else intersect(c("catchCategory", "metier4", "Country",
                            "ICESDivision", "Season"), names(catch_raised))
  if (length(jdims))
    catch_raised <- add_stratum_metrics(catch_raised, jdims, dist = dist)

  d <- prep_for_alloc(dist, app_type)
  if (!nrow(d))
    return(structure(list(message = paste0(
      "No rows of distributionType '", app_type, "' in the distributions table.")),
      class = "alloc_error"))

  chk <- tryCatch({
    a <- check_group_conditions(catch_data = catch_raised,
                                condition_list = alloc_l,
                                logFile = log_file, append = TRUE,
                                domain = "domainBiology")
    b <- check_group_conditions(catch_data = catch_raised,
                                condition_list = match_l,
                                conditionType = "matched_data",
                                logFile = log_file, append = TRUE,
                                domain = "domainBiology")
    list(alloc = a, matched = b)
  }, error = function(e) structure(list(message = conditionMessage(e)),
                                   class = "alloc_error"))
  if (inherits(chk, "alloc_error")) return(chk)

  res <- tryCatch(
    catchAllocationAoL(catch_data = catch_raised,
                       distribution_data = d,
                       condition_alloc_st_list = alloc_l,
                       condition_matched_data_list = match_l,
                       originType_catch = originType_catch,
                       variableType_catch = variableType_catch,
                       distributionType = alloc_type_of(app_type),
                       variableType_mean = variableType_mean,
                       weighting = weighting,
                       verbose = verbose,
                       logFile = log_file %||% "Log.txt",
                       assembled_output = TRUE),
    error = function(e) structure(list(message = conditionMessage(e)),
                                  class = "alloc_error"))
  if (inherits(res, "alloc_error")) return(res)

  list(distribution = res$distribution, catch = res$catch, checks = chk,
       type = alloc_type_of(app_type), app_type = app_type)
}

# ------------------------------------------------------------
#  Aggregation wrappers
# ------------------------------------------------------------
# Thin wrappers so the app and the emitted script call the same thing,
# and so a missing function degrades to NULL instead of an error.
#' @noRd
#' @export
agg_numbers <- function(alloc, grouping = c("catchCategory", "sampledOrEstimated"),
                        minAoL = 0, maxAoL = NA, plusGroup = FALSE,
                        unit = NULL, round = 2,
                        originType_catch = "WGEstimate",
                        variableType_catch = "WeightLive") {
  if (is.null(alloc) || inherits(alloc, "alloc_error")) return(NULL)
  if (!exists("catchAtAoLAggregation", mode = "function")) return(NULL)
  tryCatch(as.data.frame(catchAtAoLAggregation(
      distribution_data = alloc$distribution, catch_data = alloc$catch,
      grouping = grouping, originType_catch = originType_catch,
      variableType_catch = variableType_catch,
      distributionType = alloc$type, unit = unit, round = round,
      minAoL = minAoL, maxAoL = maxAoL, plusGroup = plusGroup)),
    error = function(e) NULL)
}

#' @noRd
#' @export
agg_meanweight <- function(alloc, grouping = c("catchCategory", "sampledOrEstimated"),
                           minAoL = 0, maxAoL = NA, plusGroup = FALSE,
                           unit = "g", round = 1,
                           weighting = "NumberAtAoL",
                           originType_catch = "WGEstimate",
                           variableType_catch = "WeightLive") {
  if (is.null(alloc) || inherits(alloc, "alloc_error")) return(NULL)
  if (!exists("meanWoLatAoLAggregation", mode = "function")) return(NULL)
  tryCatch(as.data.frame(meanWoLatAoLAggregation(
      distribution_data = alloc$distribution, catch_data = alloc$catch,
      grouping = grouping, weighting = weighting,
      originType_catch = originType_catch,
      variableType_catch = variableType_catch,
      variableType_dist = "WeightLive", distributionType = alloc$type,
      unit = unit, round = round, minAoL = minAoL, maxAoL = maxAoL,
      plusGroup = plusGroup)),
    error = function(e) NULL)
}

# ------------------------------------------------------------
#  Imported vs allocated
# ------------------------------------------------------------
# The aggregation functions return WIDE tables: one row per grouping
# combination, with the class encoded in the column NAME -
#   N_Length_2900  = numbers at length class 2900
#   W_Age_3        = mean weight at age 3
# so the class has to be read out of the name and pivoted long before
# anything can be plotted.
#' @noRd
#' @export
ALLOC_WIDE_RE <- "^([NW])_(Length|Age)_(.+)$"

#' @noRd
#' @export
alloc_pivot_long <- function(d, kind = c("N", "W"), value_name = "value") {
  kind <- match.arg(kind)
  if (is.null(d) || !nrow(d)) return(NULL)
  cols <- grep(ALLOC_WIDE_RE, names(d), value = TRUE)
  cols <- cols[sub(ALLOC_WIDE_RE, "\\1", cols) == kind]
  if (!length(cols)) return(NULL)
  keys <- setdiff(names(d), grep(ALLOC_WIDE_RE, names(d), value = TRUE))
  long <- d %>%
    select(all_of(c(keys, cols))) %>%
    tidyr::pivot_longer(all_of(cols), names_to = ".col", values_to = value_name) %>%
    mutate(.cls = suppressWarnings(as.numeric(sub(ALLOC_WIDE_RE, "\\3", .col))),
           .aol = sub(ALLOC_WIDE_RE, "\\2", .col)) %>%
    filter(!is.na(.data[[value_name]])) %>%
    select(-.col)
  as.data.frame(long)
}

# The column catchAllocationAoL() adds to say where a row came from.
# "sampled" rows were measured and imported; "estimated" rows are what
# the allocation put there. Older builds used importedOrRaised, so both
# are accepted.
#' @noRd
#' @export
ALLOC_SRC_COLS <- c("sampledOrEstimated", "importedOrRaised")

#' @noRd
#' @export
alloc_src_col <- function(d) {
  hit <- ALLOC_SRC_COLS[ALLOC_SRC_COLS %in% names(d)]
  if (length(hit)) hit[1] else NULL
}

#' @noRd
#' @export
alloc_src_label <- function(v)
  ifelse(tolower(as.character(v)) %in% c("sampled", "imported"),
         "Imported", "Allocated")

# Numbers at age/length, imported vs allocated.
#' @noRd
#' @export
alloc_compare_numbers <- function(alloc, grouping = c("catchCategory")) {
  n <- agg_numbers(alloc, grouping = unique(c(grouping, "sampledOrEstimated")))
  long <- alloc_pivot_long(n, "N", ".val")
  if (is.null(long) || !nrow(long)) return(NULL)
  sc <- alloc_src_col(long)
  long$.src <- if (!is.null(sc)) alloc_src_label(long[[sc]]) else "Allocated"
  # length classes come through on the file's own scale; show them in cm
  if (any(long$.aol == "Length")) long$.cls <- long$.cls / 10
  long[is.finite(long$.cls) & is.finite(long$.val), , drop = FALSE]
}

# Stock weight at age/length = numbers x mean weight, in tonnes.
#' @noRd
#' @export
alloc_stock_weight <- function(alloc, grouping = c("catchCategory")) {
  g <- unique(c(grouping, "sampledOrEstimated"))
  nl <- alloc_pivot_long(agg_numbers(alloc, grouping = g), "N", ".val")
  wl <- alloc_pivot_long(agg_meanweight(alloc, grouping = g, unit = "g"),
                         "W", ".mw")
  if (is.null(nl) || is.null(wl) || !nrow(nl) || !nrow(wl)) return(NULL)
  keys <- intersect(names(nl), names(wl))
  keys <- setdiff(keys, c(".val", ".mw", "unit"))
  j <- merge(nl[, unique(c(keys, ".val")), drop = FALSE],
             wl[, unique(c(keys, ".mw")), drop = FALSE], by = keys)
  if (!nrow(j)) return(NULL)
  # numbers are counts, mean weights are grams: 1e6 g = 1 t
  j$.tonnes <- j$.val * j$.mw / 1e6
  sc <- alloc_src_col(j)
  j$.src <- if (!is.null(sc)) alloc_src_label(j[[sc]]) else "Allocated"
  if (any(j$.aol == "Length")) j$.cls <- j$.cls / 10
  j[is.finite(j$.cls) & is.finite(j$.tonnes), , drop = FALSE]
}

# ------------------------------------------------------------
#  Comparison plots
# ------------------------------------------------------------
#' @noRd
#' @export
plot_alloc_numbers <- function(d, facet_var = NULL, what = "Length",
                               ncol = 2) {
  if (is.null(d) || !nrow(d)) return(NULL)
  has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
  bw <- { u <- sort(unique(d$.cls)); if (length(u) > 1) min(diff(u)) else 1 }
  agg <- d %>% group_by(across(all_of(c(".cls", ".src",
                                        if (has(facet_var)) facet_var)))) %>%
    summarise(v = sum(.val, na.rm = TRUE), .groups = "drop")
  ggplot(agg, aes(x = .cls, y = v, fill = .src)) +
    geom_col(width = bw * 0.95, position = position_stack(reverse = TRUE),
             colour = "grey25", linewidth = 0.18) +
    scale_fill_manual(values = c(Imported = PAL_BASE[1],
                                 Allocated = PAL_BASE[2]), name = NULL) +
    scale_y_continuous(labels = scales::comma) +
    (if (has(facet_var)) facet_wrap(vars(.data[[facet_var]]), ncol = ncol,
                                    scales = "free_y") else NULL) +
    labs(title = sprintf("Numbers at %s: imported vs allocated", tolower(what)),
         subtitle = "Solid = already in the data, second colour = added by the allocation",
         x = if (what == "Age") "Age" else "Length (cm)", y = "Numbers") +
    app_theme()
}

#' @noRd
#' @export
plot_alloc_stockweight <- function(d, facet_var = NULL, what = "Length",
                                   ncol = 2) {
  if (is.null(d) || !nrow(d)) return(NULL)
  has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
  bw <- { u <- sort(unique(d$.cls)); if (length(u) > 1) min(diff(u)) else 1 }
  agg <- d %>% group_by(across(all_of(c(".cls", ".src",
                                        if (has(facet_var)) facet_var)))) %>%
    summarise(t = sum(.tonnes, na.rm = TRUE), .groups = "drop")
  ggplot(agg, aes(x = .cls, y = t, fill = .src)) +
    geom_col(width = bw * 0.95, position = position_stack(reverse = TRUE),
             colour = "grey25", linewidth = 0.18) +
    scale_fill_manual(values = c(Imported = PAL_BASE[1],
                                 Allocated = PAL_BASE[2]), name = NULL) +
    scale_y_continuous(labels = scales::comma) +
    (if (has(facet_var)) facet_wrap(vars(.data[[facet_var]]), ncol = ncol,
                                    scales = "free_y") else NULL) +
    labs(title = sprintf("Stock weight at %s: imported vs allocated",
                         tolower(what)),
         subtitle = "Numbers x mean weight, in tonnes",
         x = if (what == "Age") "Age" else "Length (cm)",
         y = "Tonnes") +
    app_theme()
}

# A short overview table: what the allocation added, per category.
#' @noRd
#' @export
alloc_overview <- function(alloc) {
  n <- alloc_compare_numbers(alloc)
  if (is.null(n) || !nrow(n)) return(NULL)
  keys <- intersect(c("catchCategory", ".src"), names(n))
  out <- n %>% group_by(across(all_of(keys))) %>%
    summarise(classes = dplyr::n_distinct(.cls),
              numbers = sum(.val, na.rm = TRUE), .groups = "drop")
  sw <- alloc_stock_weight(alloc)
  if (!is.null(sw) && nrow(sw)) {
    swk <- intersect(keys, names(sw))
    sw2 <- sw %>% group_by(across(all_of(swk))) %>%
      summarise(tonnes = sum(.tonnes, na.rm = TRUE), .groups = "drop")
    out <- out %>% left_join(sw2, by = swk)
  }
  out %>% rename(`Imported / allocated` = .src) %>%
    mutate(numbers = round(numbers, 0),
           across(any_of("tonnes"), ~round(.x, 3))) %>%
    as.data.frame()
}


# The aggregation and comparison calls that follow an allocation, so the
# script reproduces the tables and plots the app showed.
#' @noRd
#' @export
emit_alloc_agg_code <- function(tag = "Length") {
  c("## aggregated numbers, imported and allocated kept apart",
    "numbers_at <- catchAtAoLAggregation(",
    "    distribution_data = distribution_alloc, catch_data = catch_alloc,",
    "    grouping = c(\"catchCategory\", \"sampledOrEstimated\"),",
    sprintf("    distributionType = \"%s\", round = 0, plusGroup = FALSE)",
            if (tag == "Age") "Age" else "Length"),
    "",
    "## aggregated mean weights, in g and in kg",
    "meanw_at_g <- meanWoLatAoLAggregation(",
    "    distribution_data = distribution_alloc, catch_data = catch_alloc,",
    "    grouping = c(\"catchCategory\", \"sampledOrEstimated\"),",
    sprintf("    distributionType = \"%s\", unit = \"g\", round = 0)",
            if (tag == "Age") "Age" else "Length"),
    "meanw_at_kg <- meanWoLatAoLAggregation(",
    "    distribution_data = distribution_alloc, catch_data = catch_alloc,",
    "    grouping = c(\"catchCategory\", \"sampledOrEstimated\"),",
    sprintf("    distributionType = \"%s\", unit = \"kg\", round = 3)",
            if (tag == "Age") "Age" else "Length"),
    "",
    "## stock weight = numbers x mean weight, in tonnes",
    "alloc_res <- list(distribution = distribution_alloc,",
    sprintf("                  catch = catch_alloc, type = \"%s\")",
            if (tag == "Age") "Age" else "Length"),
    "stock_weight <- alloc_stock_weight(alloc_res)",
    "",
    "## the two comparison plots",
    sprintf("plot_alloc_numbers(alloc_compare_numbers(alloc_res), \"catchCategory\", \"%s\")", tag),
    sprintf("plot_alloc_stockweight(stock_weight, \"catchCategory\", \"%s\")", tag))
}

# ============================================================
#  Comparing allocations, and adopting one
# ============================================================
# The discard section compares raising scenarios by re-running each one
# against the current data; this is the same idea for allocations. Every
# saved allocation is run through catchAllocationAoL(), then the numbers
# and stock weights are put side by side so the choice is made on the
# difference it makes rather than on the rules alone.
#' @noRd
#' @export
run_allocation_set <- function(allocs, catch_raised, dist, app_type,
                               ...) {
  if (!length(allocs)) return(list())
  out <- list()
  for (nm in names(allocs)) {
    # app_type NAMED, not positional. run_allocation()'s 4th argument is
    # alloc_dims, so passing app_type positionally handed it to the wrong
    # parameter - the comparison ran with the wrong app_type and no dims
    # while the final run was correct, which is precisely "compare works
    # but final does not" seen from the other side.
    r <- suppressWarnings(run_allocation(catch_raised, dist,
                                         allocs[[nm]]$groups,
                                         app_type = app_type, ...))
    out[[nm]] <- r
  }
  out
}

# Numbers and stock weight per allocation, long, ready to plot.
#' @noRd
#' @export
compare_allocations <- function(runs, grouping = c("catchCategory")) {
  ok <- names(runs)[!vapply(runs, function(r) inherits(r, "alloc_error"),
                            logical(1))]
  if (!length(ok)) return(NULL)
  rows <- lapply(ok, function(nm) {
    n <- suppressWarnings(alloc_compare_numbers(runs[[nm]], grouping))
    w <- suppressWarnings(alloc_stock_weight(runs[[nm]], grouping))
    if (is.null(n)) return(NULL)
    nn <- n %>% group_by(across(all_of(c(".cls", ".src",
                                         intersect(grouping, names(n)))))) %>%
      summarise(numbers = sum(.val, na.rm = TRUE), .groups = "drop")
    if (!is.null(w) && nrow(w)) {
      ww <- w %>% group_by(across(all_of(c(".cls", ".src",
                                           intersect(grouping, names(w)))))) %>%
        summarise(tonnes = sum(.tonnes, na.rm = TRUE), .groups = "drop")
      nn <- nn %>% left_join(ww, by = intersect(names(nn), names(ww)))
    } else nn$tonnes <- NA_real_

    # Mean weight per class, derived from the two columns above rather
    # than aggregated separately, so it cannot disagree with them:
    # tonnes = numbers * mean weight, so mean weight = tonnes / numbers.
    # A grouping change that moves the numbers moves this with them.
    nn$mean_weight <- ifelse(is.finite(nn$numbers) & nn$numbers > 0 &
                               is.finite(nn$tonnes),
                             nn$tonnes * 1e6 / nn$numbers, NA_real_)
    nn$allocation <- nm
    nn
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (!length(rows)) return(NULL)
  as.data.frame(bind_rows(rows))
}

# A totals table: one row per allocation, so the headline difference is
# readable without squinting at a plot.
#' @noRd
#' @export
comparison_totals <- function(cmp) {
  if (is.null(cmp) || !nrow(cmp)) return(NULL)
  cmp %>%
    group_by(allocation, .src) %>%
    summarise(classes = dplyr::n_distinct(.cls),
              numbers = sum(numbers, na.rm = TRUE),
              tonnes = sum(tonnes, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = .src,
                       values_from = c(numbers, tonnes, classes),
                       values_fill = 0) %>%
    mutate(across(where(is.numeric), ~round(.x, 3))) %>%
    as.data.frame()
}

# The comparison plot: one line per allocation so the shapes can be laid
# over each other, split imported vs allocated.
#' @noRd
#' @export
plot_allocation_comparison <- function(cmp,
                                       value = c("numbers", "tonnes",
                                                 "meanweight"),
                                       facet_var = NULL, what = "Length",
                                       ncol = 2, facet2 = NULL,
                                       free_x = FALSE, free_y = TRUE) {
  value <- match.arg(value)
  if (is.null(cmp) || !nrow(cmp)) return(NULL)
  # A facet naming a column the comparison does not carry used to reach
  # facet_wrap() and error inside ggplot at draw time, which surfaces as
  # an empty panel with no explanation. Unknown facets are dropped.
  has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) &&
    nzchar(v) && v %in% names(cmp)
  if (!has(facet_var)) facet_var <- NULL
  if (!has(facet2))    facet2    <- NULL
  if (!is.null(facet_var) && identical(facet_var, facet2)) facet2 <- NULL
  scl <- facet_scales(free_x, free_y)
  col <- if (value == "meanweight") "mean_weight" else value
  if (!col %in% names(cmp)) return(NULL)
  # Numbers and tonnes are extensive - they add across whatever the
  # grouping collapsed. A mean weight is not: summing it would produce a
  # number with no meaning, so it is re-weighted by the numbers behind it.
  keys <- c(".cls", "allocation", facet_var, facet2)
  d <- if (value == "meanweight")
    cmp %>%
      group_by(across(all_of(keys))) %>%
      summarise(v = {
        w <- .data$numbers; x <- .data$mean_weight
        ok <- is.finite(w) & is.finite(x) & w > 0
        if (any(ok)) sum(x[ok] * w[ok]) / sum(w[ok]) else NA_real_
      }, .groups = "drop") %>%
      filter(is.finite(v))
  else cmp %>%
    group_by(across(all_of(keys))) %>%
    summarise(v = sum(.data[[col]], na.rm = TRUE), .groups = "drop") %>%
    filter(is.finite(v))
  if (!nrow(d)) return(NULL)
  ggplot(d, aes(x = .cls, y = v, colour = allocation, group = allocation)) +
    geom_line(linewidth = 0.7, alpha = 0.9) +
    geom_point(aes(fill = allocation), shape = 21, size = 2, stroke = 0.35,
               colour = "grey20") +
    scale_colour_var("allocation", d$allocation, name = "Allocation") +
    scale_fill_var("allocation", d$allocation, name = "Allocation") +
    scale_y_continuous(labels = scales::comma) +
    # One facet wraps; two go on a grid, rows x columns, because that is
    # the only layout in which both stay readable - wrapping an
    # interaction gives panel strips like "Dis, BE, 1" and no axis for
    # either variable.
    (if (!is.null(facet_var) && !is.null(facet2))
       facet_grid(rows = vars(.data[[facet_var]]),
                  cols = vars(.data[[facet2]]), scales = scl)
     else if (!is.null(facet_var))
       facet_wrap(vars(.data[[facet_var]]), ncol = ncol, scales = scl)
     else NULL) +
    labs(title = sprintf("%s at %s by allocation",
                         switch(value, tonnes = "Stock weight",
                                meanweight = "Mean weight", "Numbers"),
                         tolower(what)),
         subtitle = if (value == "meanweight")
           "Numbers-weighted across the grouping; every allocation re-run against the same data"
         else "Every allocation re-run against the same data",
         x = if (what == "Age") "Age" else "Length (cm)",
         y = switch(value, tonnes = "Tonnes",
                    meanweight = "Mean weight (g)", "Numbers")) +
    app_theme()
}
