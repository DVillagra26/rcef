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
#  Running the allocation
# ------------------------------------------------------------
# distributionType is CEF's own: "LengthTotal" or "Age". It is what
# catchAllocationAoL() matches against and what it filters the
# distributions on, so the table goes in as the file wrote it and the
# aggregations name their columns after it (N_LengthTotal_<class>).
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
  # (metier4, vesselFlagCountry, Area1, seasonValue), which a raw CEF
  # catches table does not carry. Deriving here means the function works
  # whether it is handed the raw import or an already-derived table,
  # instead of only working from the one place that happened to derive
  # first.
  catch_raised <- derive_fields(as.data.frame(catch_raised))
  # Distributions that went through derive_fields(.., what = "distribution")
  # carry the catch's stratum fields as well. The engine joins the two tables
  # by column name and takes those fields from the catch, so a second copy -
  # typed differently, perhaps, by a csv round trip that read "27.4" back as a
  # number - has nothing to add and a chance to make the two fail to combine.
  # They stay out of the call, which therefore returns exactly what it returns
  # without them.
  dist <- as.data.frame(dist, stringsAsFactors = FALSE)
  dist <- dist[, setdiff(names(dist), distribution_fields(dist)), drop = FALSE]
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
           else intersect(c("catchCategory", "metier4", "vesselFlagCountry",
                            "ICESDivision", "seasonValue"), names(catch_raised))
  if (length(jdims))
    catch_raised <- add_stratum_metrics(catch_raised, jdims, dist = dist)

  d <- as.data.frame(dist, stringsAsFactors = FALSE)
  d <- d[d$distributionType %in% app_type, , drop = FALSE]
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
                       distributionType = app_type,
                       variableType_mean = variableType_mean,
                       weighting = weighting,
                       verbose = verbose,
                       logFile = log_file %||% "Log.txt",
                       assembled_output = TRUE),
    error = function(e) structure(list(message = conditionMessage(e)),
                                  class = "alloc_error"))
  if (inherits(res, "alloc_error")) return(res)

  list(distribution = res$distribution, catch = res$catch, checks = chk,
       type = app_type)
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
#   N_LengthTotal_2900  = numbers at length class 2900
#   W_Age_3             = mean weight at age 3
# so the class has to be read out of the name and pivoted long before
# anything can be plotted.
#' @noRd
#' @export
ALLOC_WIDE_RE <- "^([NW])_(LengthTotal|Age)_(.+)$"

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
  if (any(long$.aol == "LengthTotal")) long$.cls <- long$.cls / 10
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
  if (any(j$.aol == "LengthTotal")) j$.cls <- j$.cls / 10
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

# The allocated numbers or mean weights per class, for
# plot_allocated_NaLoA() / plot_allocated_MWaLoA(): the framework's own
# aggregations - catchAtAoLAggregation() for numbers, meanWoLatAoLAggregation()
# for mean weights - run on the allocation result with the grouping columns
# the plot asks for (sampledOrEstimated among them when the plot shows it - as
# a fill, a facet or the shade - and pooled away when it does not), then read
# back long. So a plot shows what the aggregated tables of the allocation say,
# and the classes come out in cm (lengths) or years (ages), numbers in
# thousands and mean weights in g, like every other distribution plot. One row
# per class and per combination of the grouping columns; sampledOrEstimated
# comes back as a factor, sampled first.
#
# The catch is the allocation's own $catch, not the one that went in: a
# stratum with no biological domain gets a domainBiology made up by the
# allocation ("[alloc]_..."), and its estimated distributions carry it, so it
# exists only there. The origin and variable type are the ones the allocation
# ran on (the catch rows it gave a group), which is what the aggregation has
# to filter on or it would join the distributions to rows of another kind too.
# The plus group is the framework's (maxAoL / plusGroup); length bins are not
# in the framework, so they are pooled here, numbers added and mean weights
# averaged weighted by the numbers - the rule the aggregation itself uses.
#' @noRd
#' @export
alloc_class_table <- function(allocated, quantity = c("numbers", "meanweight"),
                              distributionType = "LengthTotal",
                              catchCategory = "All", grouping = character(0),
                              plus_group = NULL, bin_width = NULL,
                              fn = "alloc_class_table(): ") {
  quantity <- match.arg(quantity)
  # a plus group pools ages, length bins pool lengths: each needs its own type
  plus_from <- NULL
  if (!is.null(plus_group)) {
    if (!(is.numeric(plus_group) && length(plus_group) == 1 &&
          is.finite(plus_group) && plus_group >= 0))
      stop(fn, "`plus_group` is the first age of the plus group - a single ",
           "number (plus_group = 7 pools ages 7 and older) - or NULL for ",
           "none", call. = FALSE)
    if (!identical(distributionType, "Age"))
      stop(fn, "plus_group only applies to ages (distributionType = ",
           "\"Age\"): lengths have no plus group", call. = FALSE)
    plus_from <- plus_group
  }
  bin_from <- NULL
  if (!is.null(bin_width)) {
    if (!(is.numeric(bin_width) && length(bin_width) == 1 &&
          is.finite(bin_width) && bin_width > 0))
      stop(fn, "`bin_width` is a single positive number - a bin width in cm ",
           "(bin_width = 5 groups length into 5cm bins) - or NULL for none",
           call. = FALSE)
    if (!identical(distributionType, "LengthTotal"))
      stop(fn, "bin_width only applies to lengths (distributionType = ",
           "\"LengthTotal\"): ages use plus_group instead", call. = FALSE)
    bin_from <- bin_width
  }
  if (inherits(allocated, "alloc_error"))
    stop(fn, "the allocation failed: ", allocated$message, call. = FALSE)
  if (!is.list(allocated) || !is.data.frame(allocated$distribution) ||
      !is.data.frame(allocated$catch))
    stop(fn, "`allocated` is the result of the allocation: the list with ",
         "$distribution and $catch that catchAllocationAoL() returns",
         call. = FALSE)
  dist  <- as.data.frame(allocated$distribution, stringsAsFactors = FALSE)
  catch <- as.data.frame(allocated$catch, stringsAsFactors = FALSE)
  miss <- setdiff(c("sampledOrEstimated", "distributionType", "distributionClass",
                    "variableType", "value", "catchCategory", "domainBiology"),
                  names(dist))
  if (length(miss))
    stop(fn, "`allocated$distribution` has no ",
         paste0("'", miss, "'", collapse = ", "),
         if ("sampledOrEstimated" %in% miss)
           " - the allocation adds sampledOrEstimated, so this is not its result",
         call. = FALSE)
  miss <- setdiff(c("catchCategory", "domainBiology", "originType", "variableType"),
                  names(catch))
  if (length(miss))
    stop(fn, "`allocated$catch` has no ", paste0("'", miss, "'", collapse = ", "),
         call. = FALSE)

  # what the allocation ran on
  used <- if ("allocGroup" %in% names(catch))
    catch[!is.na(catch$allocGroup), , drop = FALSE] else catch[0, , drop = FALSE]
  origin   <- unique(as.character(used$originType))
  variable <- unique(as.character(used$variableType))
  if (!length(origin)) { origin <- "WGEstimate"; variable <- "WeightLive" }
  if (length(origin) > 1L || length(variable) > 1L)
    stop(fn, "the allocation ran on more than one originType / variableType (",
         paste(c(origin, variable), collapse = ", "),
         ") - a plot takes one", call. = FALSE)

  parts <- catch_category_parts(catchCategory)
  have_types <- unique(as.character(dist$distributionType))
  dist  <- dist[dist$catchCategory %in% parts &
                  dist$distributionType %in% distributionType, , drop = FALSE]
  catch <- catch[catch$catchCategory %in% parts, , drop = FALSE]
  if (!nrow(dist)) {
    message(fn, "no ", distributionType, " distributions for ",
            paste(parts, collapse = " + "), " in the allocation result",
            if (!distributionType %in% have_types)
              paste0(" (it has ", paste(have_types, collapse = ", "), ")"))
    return(NULL)
  }
  # a distribution the allocation left as it was is a sampled one
  dist$sampledOrEstimated[is.na(dist$sampledOrEstimated)] <- "sampled"

  grp <- unique(grouping)
  if (!all(grp %in% union(names(catch), names(dist))))
    catch <- tryCatch(derive_fields(catch), error = function(e) catch)
  absent <- setdiff(grp, union(names(catch), names(dist)))
  if (length(absent))
    stop(fn, "the allocation result has no column ",
         paste0("'", absent, "'", collapse = ", "), call. = FALSE)

  # the classes are on the file's own scale; the plots read cm
  to_cls <- 1
  if (!identical(distributionType, "Age")) {
    cu <- if ("distributionUnit" %in% names(dist))
      unique(as.character(dist$distributionUnit)) else "mm"
    cu <- cu[!is.na(cu)]
    if (!length(cu)) cu <- "mm"
    if (length(cu) > 1L)
      stop(fn, "the length classes are in more than one unit (",
           paste(cu, collapse = ", "), ") - convert them first", call. = FALSE)
    to_cls <- unit_factor(cu, LEN_CLASS_TO_MM, "distributionClass") / 10
  }

  plus_on <- !is.null(plus_from)
  max_aol <- if (plus_on) plus_from else NA
  numbers_wide <- function()
    catchAtAoLAggregation(distribution_data = dist, catch_data = catch,
                          grouping = grp, originType_catch = origin,
                          variableType_catch = variable,
                          distributionType = distributionType, unit = "N",
                          round = NULL, minAoL = 0, maxAoL = max_aol,
                          plusGroup = plus_on)
  weights_wide <- function()
    meanWoLatAoLAggregation(distribution_data = dist, catch_data = catch,
                            grouping = grp, weighting = "NumberAtAoL",
                            originType_catch = origin,
                            variableType_catch = variable,
                            variableType_dist = "WeightLive",
                            distributionType = distributionType, unit = "g",
                            round = NULL, minAoL = 0, maxAoL = max_aol,
                            plusGroup = plus_on)
  # wide (one column per class, the class in the name) -> one row per class
  long <- function(w, kind, value_name) {
    pref <- paste0(kind, "_", distributionType, "_")
    cols <- names(w)[startsWith(names(w), pref)]
    if (!length(cols)) return(NULL)
    out <- tidyr::pivot_longer(as.data.frame(w)[c(setdiff(names(w), c(cols, "unit")), cols)],
                               all_of(cols), names_to = ".col",
                               values_to = value_name)
    txt <- substring(out$.col, nchar(pref) + 1L)
    out$.plus  <- endsWith(txt, "+")
    out$.class <- suppressWarnings(as.numeric(sub("+", "", txt, fixed = TRUE))) * to_cls
    out$.col <- NULL
    out[is.finite(out[[value_name]]) & is.finite(out$.class), , drop = FALSE]
  }
  bin_of <- function(cl) floor(round(cl, 6) / bin_from) * bin_from

  if (identical(quantity, "numbers")) {
    d <- long(numbers_wide(), "N", "number")
    if (is.null(d)) return(NULL)
    d$number <- d$number * biology_axis_labels(distributionType, "numbers")$value_scale
    d <- d[d$number > 0, , drop = FALSE]
    if (!is.null(bin_from)) {
      d$.class <- bin_of(d$.class)
      d <- d %>% group_by(across(all_of(c(grp, ".class")))) %>%
        summarise(number = sum(number), .plus = any(.plus), .groups = "drop")
    }
  } else {
    d <- long(weights_wide(), "W", "mean_weight")
    if (is.null(d)) return(NULL)
    if (!is.null(bin_from)) {
      # a bin's mean weight is weighted by the numbers at the classes in it
      dn <- long(numbers_wide(), "N", ".n")
      d <- inner_join(d, dn, by = c(grp, ".class", ".plus"))
      d$.class <- bin_of(d$.class)
      d <- d %>% group_by(across(all_of(c(grp, ".class")))) %>%
        summarise(mean_weight = sum(mean_weight * .n) / sum(.n),
                  .plus = any(.plus), .groups = "drop") %>%
        filter(is.finite(mean_weight))
    }
  }
  if (!nrow(d)) return(NULL)
  d <- as.data.frame(d)
  if ("sampledOrEstimated" %in% names(d)) {
    lv <- unique(as.character(d$sampledOrEstimated))
    d$sampledOrEstimated <- factor(as.character(d$sampledOrEstimated),
                                   levels = c(intersect(c("sampled", "estimated"), lv),
                                              setdiff(lv, c("sampled", "estimated"))))
  }
  # the plus class only exists when some age reached it
  if (plus_on && !any(d$.plus %in% TRUE)) {
    warning(fn, "no age reaches plus_group = ", plus_from,
            ", so there is no plus group", call. = FALSE)
    plus_from <- NULL
  }
  list(data = d, plus_from = plus_from, bin_from = bin_from)
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
            if (tag == "Age") "Age" else "LengthTotal"),
    "",
    "## aggregated mean weights, in g and in kg",
    "meanw_at_g <- meanWoLatAoLAggregation(",
    "    distribution_data = distribution_alloc, catch_data = catch_alloc,",
    "    grouping = c(\"catchCategory\", \"sampledOrEstimated\"),",
    sprintf("    distributionType = \"%s\", unit = \"g\", round = 0)",
            if (tag == "Age") "Age" else "LengthTotal"),
    "meanw_at_kg <- meanWoLatAoLAggregation(",
    "    distribution_data = distribution_alloc, catch_data = catch_alloc,",
    "    grouping = c(\"catchCategory\", \"sampledOrEstimated\"),",
    sprintf("    distributionType = \"%s\", unit = \"kg\", round = 3)",
            if (tag == "Age") "Age" else "LengthTotal"),
    "",
    "## stock weight = numbers x mean weight, in tonnes",
    "alloc_res <- list(distribution = distribution_alloc,",
    sprintf("                  catch = catch_alloc, type = \"%s\")",
            if (tag == "Age") "Age" else "LengthTotal"),
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
