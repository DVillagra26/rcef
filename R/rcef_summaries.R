# =====================================================================
#  rcef_summaries.R   (catch / biology / effort summary tables)
#
#  ORIGIN: this project (ILVO / RCEF Workbench). Freely refactorable.
#  Was: R_development/summaries.R
#  No shiny in this file. If it needs `input`, `session` or a tag,
#  it belongs in app/, not here.
# =====================================================================

# ============================================================
#  summaries.R
#  Overview tables for the three CEF tables and for raised output.
#  All weights in tonnes (CEF WeightLive is kg).
# ============================================================

# ------------------------------------------------------------
#  catches
# ------------------------------------------------------------
#' @noRd
#' @export
catches_summary <- function(catches) {
  if (is.null(catches) || !nrow(catches))
    return(data.frame(item = character(0), value = character(0)))
  u <- if ("variableUnit" %in% names(catches)) catches$variableUnit else NULL
  t_ <- to_tonnes(catches$total, u)
  cc <- catches$catchCategory
  sampled <- !is.na(catches$domainCatchDis) & catches$domainCatchDis != ""
  lan <- cc == "Lan"
  data.frame(
    item = c("Rows", "Stocks", "Years", "Countries", "Fleets (fleetValue)",
             "Areas", "Seasons", "Catch categories",
             "Landings (t)", "Discards (t)",
             "Landings rows linked to a discard estimate",
             "Landings weight with a discard estimate (%)",
             "Rows with domainBiology"),
    value = c(
      format(nrow(catches), big.mark = ","),
      paste(unique(catches$stock), collapse = ", "),
      paste(sort(unique(catches$year)), collapse = ", "),
      length(unique(catches$vesselFlagCountry)),
      length(unique(catches$fleetValue)),
      length(unique(catches$areaValue)),
      paste(sort(unique(catches$seasonValue)), collapse = ", "),
      paste(sort(unique(cc)), collapse = ", "),
      fmt_t3(sum(t_[lan], na.rm = TRUE)),
      fmt_t3(sum(t_[!lan], na.rm = TRUE)),
      sprintf("%s of %s", format(sum(lan & sampled), big.mark = ","),
              format(sum(lan), big.mark = ",")),
      round(sum(t_[lan & sampled], na.rm = TRUE) /
              sum(t_[lan], na.rm = TRUE) * 100, 1),
      if ("domainBiology" %in% names(catches))
        format(sum(!is.na(catches$domainBiology) & catches$domainBiology != ""),
               big.mark = ",") else "0"),
    stringsAsFactors = FALSE)
}

# Catch weight by category, the shape people expect at the top of a report
#' @noRd
#' @export
catches_by_category <- function(catches) {
  u <- if ("variableUnit" %in% names(catches)) catches$variableUnit else NULL
  d <- catches %>%
    mutate(.t = to_tonnes(total, u)) %>%
    group_by(catchCategory) %>%
    summarise(rows = n(), tonnes = sum(.t, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(tonnes))
  bind_rows(d, data.frame(catchCategory = "Total", rows = sum(d$rows),
                          tonnes = sum(d$tonnes))) %>%
    mutate(tonnes = round(tonnes, 3))
}

# ------------------------------------------------------------
#  distributions - biology sampling, seen through domainBiology
# ------------------------------------------------------------
# A catch stratum has biology only when its domainBiology key is filled
# AND that key turns up in the distributions table. distributionType
# says what was measured (LengthTotal / Age), variableType says what the
# value is (Number / WeightLive).
#' @noRd
#' @export
biology_coverage <- function(catches, distributions, dims) {
  miss <- setdiff(dims, names(catches))
  if (length(miss))
    stop("biology_coverage(): run derive_fields() first - missing ",
         paste(miss, collapse = ", "))
  u <- if ("variableUnit" %in% names(catches)) catches$variableUnit else NULL
  keys <- if (!is.null(distributions) && "domainBiology" %in% names(distributions))
    unique(distributions$domainBiology[!is.na(distributions$domainBiology)]) else character(0)

  # a catches table without domainBiology simply has no biology linkage
  db <- if ("domainBiology" %in% names(catches))
    as.character(catches$domainBiology) else rep(NA_character_, nrow(catches))
  catches$.has_key <- !is.na(db) & db != ""
  catches$.in_dist <- catches$.has_key & db %in% keys

  catches %>%
    mutate(.t = to_tonnes(total, u),
           has_key = .has_key, in_dist = .in_dist) %>%
    group_by(across(all_of(dims)), catchCategory) %>%
    summarise(weight = sum(.t, na.rm = TRUE),
              weight_with_biology = sum(.t[in_dist], na.rm = TRUE),
              n_rows = n(), n_with_key = sum(has_key), n_in_dist = sum(in_dist),
              .groups = "drop") %>%
    mutate(biology_coverage = ifelse(weight > 0,
                                     weight_with_biology / weight * 100, NA_real_),
           sampled = n_in_dist > 0) %>%
    as.data.frame()
}

#' @noRd
#' @export
distributions_summary <- function(distributions, catches = NULL) {
  if (is.null(distributions) || !nrow(distributions))
    return(data.frame(item = "Rows", value = "0", stringsAsFactors = FALSE))
  linked <- if (!is.null(catches) && "domainBiology" %in% names(catches))
    sum(unique(distributions$domainBiology) %in%
          unique(catches$domainBiology[!is.na(catches$domainBiology)])) else NA
  data.frame(
    item = c("Rows", "Biology domains", "Domains matched in catches",
             "distributionType", "variableType", "Catch categories",
             "Class range", "Total measurements"),
    value = c(
      format(nrow(distributions), big.mark = ","),
      length(unique(distributions$domainBiology)),
      if (is.na(linked)) "-" else linked,
      paste(sort(unique(distributions$distributionType)), collapse = ", "),
      paste(sort(unique(distributions$variableType)), collapse = ", "),
      paste(sort(unique(distributions$catchCategory)), collapse = ", "),
      if ("distributionClass" %in% names(distributions))
        paste(range(suppressWarnings(as.numeric(distributions$distributionClass)),
                    na.rm = TRUE), collapse = " - ") else "-",
      if ("numMeasurements" %in% names(distributions))
        format(sum(as.numeric(distributions$numMeasurements), na.rm = TRUE),
               big.mark = ",") else "-"),
    stringsAsFactors = FALSE)
}

# What was measured, and for which catch category
#' @noRd
#' @export
distributions_by_type <- function(distributions) {
  distributions %>%
    group_by(distributionType, variableType, catchCategory) %>%
    summarise(rows = n(), domains = n_distinct(domainBiology),
              .groups = "drop") %>%
    arrange(distributionType, variableType, catchCategory)
}

# ------------------------------------------------------------
#  effort
# ------------------------------------------------------------
#' @noRd
#' @export
effort_summary <- function(effort) {
  if (is.null(effort) || !nrow(effort))
    return(data.frame(item = "Rows", value = "0", stringsAsFactors = FALSE))
  data.frame(
    item = c("Rows", "Countries", "Fleets", "Areas", "Seasons",
             "Effort units", "Total effort"),
    value = c(format(nrow(effort), big.mark = ","),
              length(unique(effort$vesselFlagCountry)),
              length(unique(effort$fleetValue)),
              length(unique(effort$areaValue)),
              paste(sort(unique(effort$seasonValue)), collapse = ", "),
              paste(unique(effort$variableType), collapse = ", "),
              format(round(sum(as.numeric(effort$total), na.rm = TRUE), 1),
                     big.mark = ",")),
    stringsAsFactors = FALSE)
}

# ------------------------------------------------------------
#  Raised output
# ------------------------------------------------------------
#' The raised catches handed to a comparison function, with their names
#'
#' [raised_overview_table()] and [plot_comparison()] take their raised catches
#' written `list(a, b)` or, the way it is most naturally typed, `c(a, b)` -
#' which for data frames would otherwise flatten into one list of columns. The
#' call is therefore read before it is evaluated: each element is evaluated on
#' its own in the caller's frame, and one without a name of its own is called
#' what the object is called.
#'
#' @param expr The unevaluated argument, from `substitute()`.
#' @param value A function returning the evaluated argument; only called when
#'   `expr` is not a `c()` / `list()` call, so a `c()` of data frames is never
#'   evaluated as such.
#' @param env The caller's frame, where the elements are evaluated.
#' @param scenarios_names Optional names, one per raised catch, which win over
#'   any other.
#' @param fn The calling function, for error messages.
#' @param min The fewest raised catches the caller accepts.
#' @return A list: `data`, a named list of raised catches, and `single`,
#'   `TRUE` when the argument was one bare data frame.
#' @keywords internal
#' @noRd
#' @export
scenario_list <- function(expr, value, env, scenarios_names = NULL,
                          fn = "plot_comparison()", min = 2L) {
  listing <- is.call(expr) && is.name(expr[[1]]) &&
    as.character(expr[[1]]) %in% c("c", "list")
  single <- FALSE
  if (listing) {
    args <- as.list(expr)[-1]
    vals <- lapply(args, eval, envir = env)
    tags <- names(args)
    if (is.null(tags)) tags <- rep("", length(args))
    nms <- ifelse(nzchar(tags), tags,
                  vapply(args, function(a) rlang::as_label(a), ""))
  } else {
    val <- value()
    if (is.data.frame(val)) {
      vals <- list(val); nms <- rlang::as_label(expr); single <- TRUE
    } else {
      vals <- as.list(val)
      nms <- names(vals)
      if (is.null(nms)) nms <- rep("", length(vals))
      nms[!nzchar(nms)] <- paste("scenario", seq_along(vals))[!nzchar(nms)]
    }
  }
  if (!length(vals))
    stop(fn, ": no raised catches given - pass the output of catchRaising(), ",
         "written list(a, b) or c(a, b)", call. = FALSE)
  if (!is.null(scenarios_names)) {
    if (length(scenarios_names) != length(vals))
      stop(fn, ": `scenarios_names` has ", length(scenarios_names),
           " name(s) for ", length(vals), " raised catch(es)", call. = FALSE)
    nms <- as.character(scenarios_names)
    if (anyDuplicated(nms))
      stop(fn, ": `scenarios_names` must all be different", call. = FALSE)
  }
  nms <- make.unique(nms)
  need <- c("catchCategory", "total", "importedOrRaised")
  for (i in seq_along(vals)) {
    if (!is.data.frame(vals[[i]]))
      stop(fn, ": '", nms[i], "' is not a raised catch (a data frame) - write ",
           "list(a, b) or c(a, b) directly in the call, not a c() of them ",
           "stored beforehand", call. = FALSE)
    miss <- setdiff(need, names(vals[[i]]))
    if (length(miss))
      stop(fn, ": '", nms[i], "' is not a raised catch - it has no ",
           paste0("'", miss, "'", collapse = ", "),
           " column (the output of catchRaising() does)", call. = FALSE)
  }
  if (length(vals) < min)
    stop(fn, ": compares two or more raised catches, got ", length(vals),
         call. = FALSE)
  names(vals) <- nms
  list(data = vals, single = single)
}

#' One column's values across the raised catches, in a sensible order
#'
#' The union of what a column holds in any of the raised catches, as strings:
#' by size for `mesh` and `metier4_mesh` ([mesh_var_levels()]), numerically for
#' a numeric column (`seasonValue`: 2 before 10), in factor-level order for a
#' factor, alphabetically for anything else.
#'
#' @param data A named list of raised catches.
#' @param var The column.
#' @return A character vector.
#' @keywords internal
#' @noRd
#' @export
scenario_levels <- function(data, var) {
  vals <- lapply(data, function(r) r[[var]])
  nat <- mesh_var_levels(var, unlist(lapply(vals, as.character)))
  if (!is.null(nat)) return(nat)
  if (all(vapply(vals, is.numeric, NA)))
    return(as.character(sort(unique(unlist(vals)))))
  if (all(vapply(vals, is.factor, NA)))
    return(unique(unlist(lapply(vals, levels))))
  sort(unique(as.character(unlist(vals))))
}

#' A category column as a factor with the given order
#'
#' Values missing from `levels` are dropped from it; a missing value becomes
#' its own last level, `"NA"`, rather than silently falling out of a plot or a
#' table.
#'
#' @param x The values.
#' @param levels Their order.
#' @return A factor.
#' @keywords internal
#' @noRd
#' @export
level_factor <- function(x, levels) {
  x <- as.character(x)
  if (anyNA(x)) {
    x[is.na(x)] <- "NA"
    levels <- c(levels, "NA")
  }
  factor(x, levels = unique(levels[levels %in% x]))
}

#' Imported against raised catch, by catch category: one raised catch or several
#'
#' @description
#' Each catch category's weight in tonnes - how much of it came with the data
#' (imported) and how much the raising added (raised) - and each part as a
#' share of its category, closed by a Total row. Give it two or more raised
#' catches, say two ways of defining the groups, and they are laid out side by
#' side, scenario by scenario. Give it `by` and every figure is split further
#' by those columns - a season, a metier, an area, the raising group - each
#' split with its own Total.
#'
#' @details
#' The raised catches are written `list(a, b)` or `c(a, b)`; one without a name
#' of its own is called what its object is called, and `scenarios_names`
#' overrides either. A single raised catch needs no list and comes back without
#' the `Scenario` column.
#'
#' Within each scenario and `by` split the rows are catch category, then
#' imported before raised, and the scenarios of one split sit next to each
#' other so the same figure can be read across them. `Percent of category` is
#' the share of a part within its category in that split, so imported and
#' raised add up to 100.
#'
#' A raised row whose group had no donor to draw a rate from comes out of the
#' raising as `NaN`: it raises to nothing and counts as no weight. The
#' `No donor (rows)` column, shown only when there are any, counts them where
#' they fall - by raising group, say - so they are not mistaken for a group
#' that simply raised little.
#'
#' @param raised_catch_scenarios One raised catch (the output of
#'   [catchRaising()]), or two or more written `list(a, b)` or `c(a, b)`.
#' @param scenarios_names Optional names for the scenarios, in the same order.
#'   Default: the names given inside `c()` / `list()`, or else the object
#'   names.
#' @param by Optional columns to split every figure by, in addition to the
#'   catch category and imported / raised: `"seasonValue"`,
#'   `c("metier4", "areaValue")`, `"DrGroup"`. Every raised catch must have
#'   them.
#' @return A tibble with `Scenario` (for several raised catches), `Catch
#'   category`, the `by` columns, `Raised/Imported`, `Caton (t)`, `Percent of
#'   category` and, when any, `No donor (rows)`.
#' @seealso [plot_comparison()] for the same comparison as a figure;
#'   [plot_raised_catches()] for one raised catch.
#' @md
#' @examples
#' \donttest{
#' data(anchoveta_catch)
#' cef  <- subset(anchoveta_catch, year == 2023)
#' dims <- c("metier4", "ICESDivision", "seasonValue")
#' log  <- tempfile(fileext = ".txt")
#' strict  <- define_raising_groups_auto(cef, hierarchy = dims,
#'                                       thresholds = c(80, 60, 40))
#' relaxed <- define_raising_groups_auto(cef, hierarchy = dims,
#'                                       thresholds = c(50, 40, 30))
#' raised_strict  <- run_native_raising(cef, dims, strict,
#'                                      type = "discards", log_file = log)
#' raised_relaxed <- run_native_raising(cef, dims, relaxed,
#'                                      type = "discards", log_file = log)
#'
#' # one raised catch
#' raised_overview_table(raised_strict)
#'
#' # two, side by side, named after the objects ...
#' raised_overview_table(c(raised_strict, raised_relaxed))
#'
#' # ... or after you, split by season and metier
#' raised_overview_table(c(strict = raised_strict, relaxed = raised_relaxed),
#'                       by = c("seasonValue", "metier4"))
#' }
#' @export
raised_overview_table <- function(raised_catch_scenarios, scenarios_names = NULL,
                                  by = NULL) {
  sc <- scenario_list(substitute(raised_catch_scenarios),
                      function() raised_catch_scenarios, parent.frame(),
                      scenarios_names, fn = "raised_overview_table()", min = 1L)
  by <- setdiff(unique(as.character(by[!is.na(by) & nzchar(by)])), "catchCategory")
  for (nm in names(sc$data)) {
    miss <- setdiff(by, names(sc$data[[nm]]))
    if (length(miss))
      stop("raised_overview_table(): '", nm, "' has no column ",
           paste0("'", miss, "'", collapse = ", "), call. = FALSE)
  }

  d <- do.call(rbind, lapply(names(sc$data), function(nm) {
    r <- sc$data[[nm]]
    u <- if ("variableUnit" %in% names(r)) r$variableUnit else NULL
    raised <- r$importedOrRaised %in% "raised"
    out <- data.frame(scenario = rep(nm, nrow(r)),
                      catchCategory = as.character(r$catchCategory),
                      src = ifelse(raised, "Raised", "Imported"),
                      .t = to_tonnes(r$total, u),
                      .no_donor = raised & is.nan(r$total),
                      stringsAsFactors = FALSE)
    for (v in by) out[[v]] <- as.character(r[[v]])
    out
  }))
  d$scenario <- factor(d$scenario, levels = names(sc$data))
  for (v in by) d[[v]] <- level_factor(d[[v]], scenario_levels(sc$data, v))

  cell <- c("scenario", by)
  agg <- d %>%
    group_by(across(all_of(c(cell, "catchCategory", "src")))) %>%
    summarise(tonnes = sum(.t, na.rm = TRUE), no_donor = sum(.no_donor),
              .groups = "drop")
  tot_cat <- agg %>%
    group_by(across(all_of(c(cell, "catchCategory")))) %>%
    summarise(cat_total = sum(tonnes), .groups = "drop")
  out <- agg %>%
    left_join(tot_cat, by = c(cell, "catchCategory")) %>%
    mutate(percent = ifelse(cat_total > 0, round(tonnes / cat_total * 100), NA_real_)) %>%
    select(-cat_total)
  tot <- agg %>%
    group_by(across(all_of(cell))) %>%
    summarise(tonnes = sum(tonnes), no_donor = sum(no_donor), .groups = "drop") %>%
    mutate(catchCategory = "Total", src = "-", percent = 100)
  res <- bind_rows(out, tot) %>%
    arrange(!!!rlang::syms(c(by, "catchCategory", "src", "scenario")))
  for (v in c("scenario", by)) res[[v]] <- as.character(res[[v]])

  res <- res %>%
    select(Scenario = scenario, `Catch category` = catchCategory, all_of(by),
           `Raised/Imported` = src, `Caton (t)` = tonnes,
           `Percent of category` = percent, `No donor (rows)` = no_donor) %>%
    mutate(`Caton (t)` = round(`Caton (t)`, 3))
  if (sc$single) res$Scenario <- NULL
  if (!any(res$`No donor (rows)` > 0)) res$`No donor (rows)` <- NULL
  res
}

#' Raised catch composition: landings and discards, imported and raised
#'
#' A stacked bar of catch weight after raising, at any aggregation: bars are
#' landings/discards (or just the one, via `catchCategory`). What raising added
#' - `importedOrRaised`, imported from the data versus added by raising - is
#' drawn as the shade, as the `fill` or as a `facet`, as you choose.
#'
#' `importedOrRaised` is a variable like any other. By default it is the shade
#' (`alpha = "importedOrRaised"`): solid for what was imported, transparent for
#' what raising added. Name it in `fill` to give the two a colour each (the bars
#' are then no longer coloured landings / discards: say which they are with
#' `catchCategory`, or with `facet = "catchCategory"`), in `facet` to give each
#' its own panel, or in `x` for a bar each; it is then not drawn as a shade as
#' well. `alpha = NULL` draws no shade, and unless it is named elsewhere the
#' imported and the raised weights are added together.
#'
#' @param raised_catch Output of [catchRaising()]: `raised` with
#'   `importedOrRaised` and `DrGroup` columns.
#' @param x Column on the x axis: one bar (or bar segment) per level;
#'   `"importedOrRaised"` included.
#' @param facet Optional facet column; `"importedOrRaised"` included.
#' @param fill Column mapped to the bar fill. `NULL` (default) colours the bars
#'   landings / discards; `"importedOrRaised"` is allowed.
#' @param alpha `"importedOrRaised"` (default) draws the shade: solid for
#'   imported, transparent for raised. `NULL` draws none. Left out when
#'   `importedOrRaised` is the `x`, the `fill` or the `facet`.
#' @param catchCategory Optional filter to just `"Lan"` or `"Dis"` (matched
#'   case-insensitively), so discards are readable on their own when landings
#'   would otherwise dominate the scale. `NULL` (default) keeps both, stacked.
#' @param ncol Facet columns, when `facet` is set.
#' @return A ggplot object.
#' @seealso [plot_raised_DR()] for the discard rate of the same raised catch.
#' @family raising plots
#' @md
#' @export
plot_raised_catches <- function(raised_catch, x = NULL, facet = NULL,
                                fill = NULL, alpha = "importedOrRaised",
                                catchCategory = NULL, ncol = 2) {
  has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
  S <- "importedOrRaised"
  if (!is.null(alpha) && !identical(alpha, S))
    stop("plot_raised_catches(): `alpha` is \"", S, "\" (solid = imported, ",
         "transparent = raised) or NULL for no shade", call. = FALSE)
  miss <- setdiff(c(S, "catchCategory", "total", if (has(x)) x,
                    if (has(facet)) facet, if (has(fill)) fill),
                  names(raised_catch))
  if (length(miss))
    stop("plot_raised_catches(): `raised_catch` has no column ",
         paste0("'", miss, "'", collapse = ", "), call. = FALSE)
  # imported / raised is drawn once: as the shade, unless it is already the
  # x, the fill or the facet
  shown    <- S %in% c(x, fill, facet)
  alpha_on <- !is.null(alpha) && !shown
  u <- if ("variableUnit" %in% names(raised_catch)) raised_catch$variableUnit else NULL
  d <- raised_catch %>%
    mutate(.t = to_tonnes(total, u),
           src = factor(ifelse(importedOrRaised %in% "raised", "Raised", "Imported"),
                        levels = c("Imported", "Raised")),
           importedOrRaised = factor(ifelse(importedOrRaised %in% "raised",
                                            "raised", "imported"),
                                     levels = c("imported", "raised")),
           .cat = ifelse(toupper(catchCategory) %in% c("LAN", "LANDINGS"),
                         "Landings", "Discards"),
           .x = if (has(x)) as.character(.data[[x]]) else "All")
  if (has(catchCategory)) {
    want <- if (toupper(catchCategory) %in% c("LAN", "LANDINGS")) "Landings" else "Discards"
    d <- d[d$.cat == want, , drop = FALSE]
  }
  cc_label <- if (!has(catchCategory)) "Catch"
             else if (toupper(catchCategory) %in% c("LAN", "LANDINGS")) "Landings"
             else "Discards"
  fill_col <- if (has(fill)) fill else ".cat"
  keys <- unique(c(".x", fill_col, if (has(facet)) facet, if (alpha_on) "src"))
  d <- d %>% group_by(across(all_of(keys))) %>%
    summarise(t = sum(.t, na.rm = TRUE), .groups = "drop")
  d <- reorder_mesh(d, c(fill, facet))
  d <- discrete_cols(d, c(fill, facet))

  ae <- aes(x = .x, y = t, fill = .data[[fill_col]])
  if (alpha_on) ae <- modifyList(ae, aes(alpha = src))
  p <- ggplot(d, ae) +
    geom_col(position = position_stack(reverse = TRUE), width = 0.72,
             colour = "grey25", linewidth = 0.25)
  p <- p + if (!has(fill))
    scale_fill_manual(values = c(Landings = "#59A14F", Discards = "#E15759"),
                      name = NULL)
  else if (identical(fill, S))
    scale_fill_manual(values = c(imported = PAL_BASE[1], raised = PAL_BASE[2]),
                      name = S)
  else scale_fill_var(fill, d[[fill]], name = fill)
  if (alpha_on)
    p <- p + scale_alpha_manual(values = c(Imported = 1, Raised = 0.45), name = NULL,
                                guide = guide_legend(override.aes = list(fill = "grey30")))
  p +
    scale_y_continuous(labels = scales::comma) +
    (if (has(facet)) facet_wrap(vars(.data[[facet]]), ncol = ncol,
                                scales = "free_y") else NULL) +
    labs(title = sprintf("Raised %s per stratum", tolower(cc_label)),
         caption = sub_wrap(
           "grouping = no grouping, raw data per stratum.",
           sprintf("bars = %s weight (t), summed per stratum.", tolower(cc_label)),
           if (!has(fill) && !has(catchCategory))
             "color = landings or discards.",
           if (has(fill)) sprintf("color = %s.", fill),
           if (alpha_on)
             "shade = imported from the data, or added by raising.",
           if (!alpha_on && !shown)
             "imported and raised weights added together.",
           width = 110),
         x = if (has(x)) x else NULL, y = sprintf("%s (t)", cc_label)) +
    app_theme() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
