# ============================================================
#  viz_raising.R
#  Raising / allocation diagnostic plots - alluvial flow diagrams and the
#  scenario-comparison plots. Built on ggalluvial and deliberately
#  ggplot-only (a base-R alluvial is impractical), so unlike the
#  exploratory plots in viz_plots.R these carry no `engine` argument.
# ============================================================

#' @keywords internal
#' @name rcef-viz-raising
#' @noRd
#' @import ggplot2
#' @importFrom dplyr %>% arrange group_by mutate summarise
#' @importFrom rlang .data
NULL

# ----------------------------------------------------------
#  Raising diagnostics: river and scenario comparison
# ----------------------------------------------------------

#' River plot: which strata are raised from which group
#'
#' An alluvial plot with one ribbon per stratum, running from the stratum on the
#' left to the donor group on the right, its width the stratum's weight. It
#' answers the question a table cannot: how much of the stock is being raised
#' from each pool, and which strata were swept into the fallback.
#'
#' @param df A stratum table: one row per stratum, with the columns named by
#'   `left_var`, `right_var` and `value_var`. Or the result of
#'   [recommend_groups()]: its strata, which run from the value of `left_var` (any
#'   column of them - a variable of the hierarchy, say) to the level they are
#'   raised at, their weight the width.
#' @param left_var Column on the left axis, usually the stratum label.
#' @param right_var Column on the right axis, usually the donor group (the level,
#'   for the result of [recommend_groups()]).
#' @param value_var Column giving each ribbon its width (the weight, for the result
#'   of [recommend_groups()]).
#' @param palette `"default"`, `"colorblind"` or `"gradual"`: the colours of the
#'   groups.
#' @param title Plot title.
#' @param left_label,right_label Axis labels.
#' @param stratum_axis Label for the stratum axis.
#' @inheritParams saving-plots
#' @return A ggplot object, or `NULL` when there is nothing to draw.
#' @family raising plots
#' @export
plot_river <- function(df, left_var, right_var = "raise_level",
                       value_var = "landings",
                       title = "Which strata are raised from which group",
                       left_label = NULL, right_label = NULL,
                       stratum_axis = "Stratum",
                       palette = c("default", "colorblind", "gradual"), save = NULL) {
  save_on_exit(save, "plot_river")
  palette <- match.arg(palette)
  # the strata of recommend_groups(): each runs from its value of left_var to the level
  # it is raised at, as wide as it is heavy
  fill_var <- "raising group"; weight_label <- "Landings (t)"; to_label <- "Raised from"
  if (inherits(df, "rcef_groups")) {
    verb <- if (identical(df$what, "discard")) "raised" else "allocated"
    if (missing(right_var)) right_var <- "level_label"
    if (missing(value_var)) value_var <- "weight_t"
    if (missing(stratum_axis)) stratum_axis <- left_var
    if (missing(title)) title <- paste("Which", left_var, "is", verb, "at which level")
    if (!identical(df$what, "discard")) weight_label <- "Catch weight (t)"
    to_label <- paste(tools::toTitleCase(verb), "at")
    df <- df$strata
    fill_var <- "raising level"
  }
  # left_label / right_label: optional character vectors (same length as
  # df) giving the text to show on each axis. When supplied they override
  # the raw left_var / right_var values - this is how a stratum is shown
  # as its full Fleet | Country | Area | Season combination, and a donor
  # group as the MEANING of its rule ("metier4 %in% ... & vesselFlagCountry %in% ...")
  # rather than an opaque "G23".
  # stratum_axis: the label under the left column, e.g.
  #   "Stratum (metier4 + Country + Area1 + Season)"
  d <- df
  d$.L <- if (!is.null(left_label)) as.character(left_label)
          else as.character(d[[left_var]])
  d$.R <- if (!is.null(right_label)) as.character(right_label)
          else as.character(d[[right_var]])
  d$.fill <- as.character(d[[right_var]])          # colour still keyed on group id
  d$.w <- d[[value_var]]
  d <- d %>% filter(.w > 0) %>%
    group_by(.L, .R, .fill) %>%
    summarise(w = sum(.w, na.rm = TRUE), .groups = "drop") %>%
    filter(w > 0)
  if (!nrow(d)) return(NULL)

  # wrap long labels so tall boxes can hold the full text
  wrap <- function(x, width = 26)
    vapply(x, function(s) paste(strwrap(s, width = width), collapse = "\n"),
           character(1))
  d$.L <- wrap(d$.L); d$.R <- wrap(d$.R, 30)

  ggplot(d, aes(y = w, axis1 = .L, axis2 = .R)) +
    ggalluvial::geom_alluvium(aes(fill = .fill), width = 0.30, alpha = 0.75,
                              colour = "grey40", linewidth = 0.2) +
    ggalluvial::geom_stratum(width = 0.30, fill = "grey96", colour = "grey45") +
    # The stat object, not the string "stratum": ggplot2 resolves the string
    # from the search path, so it only worked once ggalluvial was attached
    # and failed otherwise with "Can't find stat called stratum".
    geom_text(stat = ggalluvial::StatStratum, aes(label = after_stat(stratum)),
              size = 2.7, lineheight = 0.82) +
    scale_x_discrete(limits = c(stratum_axis, to_label),
                     expand = c(0.16, 0.05)) +
    scale_fill_manual(values = plot_colours(d$.fill, fill_var, palette),
                      name = "Raising group") +
    scale_y_continuous(labels = scales::comma) +
    labs(title = title, y = weight_label, x = NULL) +
    app_theme() +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 9, colour = "grey35"))
}

#' How the strata fall into the levels of the groups
#'
#' @description
#' Draws the result of [recommend_groups()]: how much of the stock is raised (or
#' allocated) at each level of the hierarchy. With `x` it breaks that down by a
#' column of the strata - a variable of the hierarchy, say - so that it shows
#' where the coarse pools are needed: which areas, meshes or countries can stand
#' on their own data and which have to borrow.
#'
#' @details
#' Without `x`, one bar for each level, the narrowest on top: the weight of the
#' strata that take that level. The solid part is weight that has a sample of its
#' own, the transparent hatched part is the weight that has none, and is raised (or
#' allocated) from the pool - so the hatched bars say how much of what has to be
#' estimated rests on each level. The number at the end of a bar is how many strata
#' fall in the level. For lengths and ages the landings and the discards are two
#' panels.
#'
#' With `x`, one bar for each value of `x`, stacked by the level its strata take;
#' `share = TRUE` makes every bar 100 % so that the values can be compared whatever
#' their weight. [plot_river()] draws the same flows as ribbons.
#'
#' A level is a colour, the same in every figure of the groups: the top level (the
#' number of variables in the hierarchy) is the stratum on its own, each level
#' below it pools across one more variable of the hierarchy, from the last one up,
#' and level 0 is everything. A tall bar at the top of the first figure means most
#' of the stock stands on its own data; a tall hatched bar further down means a
#' lot of it is raised from a wide pool.
#'
#' @param rec The result of [recommend_groups()].
#' @param x Column of the strata for the bars, e.g. `"areaValue"`; `NULL`
#'   (default) for one bar for each level. Any column of `rec$strata`: a variable
#'   of the hierarchy, `group`, or `catchCategory` for lengths and ages.
#' @param facet Optional column of `rec$strata` for panels. Only with `x`.
#' @param share With `x`: bars of 100 % instead of tonnes.
#' @param palette `"default"`, `"colorblind"` or `"gradual"`: the colours of the
#'   levels.
#' @inheritParams saving-plots
#' @return A ggplot object.
#' @seealso [recommend_groups()]; [plot_river()] for the flows from a variable to
#'   the levels.
#' @family raising groups
#' @family raising plots
#' @md
#' @examples
#' # the small catch (kg) of ?recommend_groups
#' catch <- data.frame(
#'   catchCategory  = c("Lan", "Lan", "Lan", "Lan", "Lan", "Lan", "Dis", "Dis"),
#'   areaValue      = c("27.4", "27.4", "27.4", "27.7.d", "27.7.d", "27.7.d", "27.4", "27.7.d"),
#'   mesh           = c("70-99", "70-99", "100-119", "70-99", "70-99", "100-119", "70-99", "70-99"),
#'   total          = c(3000, 1000, 2000, 800, 200, 500, 200, 40),
#'   domainCatchDis = c("d1", NA, NA, "d2", NA, NA, "d1", "d2"),
#'   variableUnit   = "kg", stringsAsFactors = FALSE)
#' rec <- recommend_groups(catch, "areaValue > mesh", list(cov = 50))
#' plot_groups(rec)                       # how much is raised at each level
#' plot_groups(rec, x = "areaValue")      # ... and where
#'
#' \dontrun{
#' rec <- recommend_groups(catch_data,
#'                         "areaValue > mesh > vesselFlagCountry > seasonValue")
#' plot_groups(rec)                              # how much is raised at each level
#' plot_groups(rec, x = "areaValue")             # ... and where
#' plot_groups(rec, x = "vesselFlagCountry", share = TRUE)
#' }
#' @export
plot_groups <- function(rec, x = NULL, facet = NULL, share = FALSE,
                        palette = c("default", "colorblind", "gradual"), save = NULL) {
  save_on_exit(save, "plot_groups")
  palette <- match.arg(palette)
  if (!inherits(rec, "rcef_groups"))
    stop("plot_groups(): `rec` is the result of recommend_groups()", call. = FALSE)
  has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
  if (has(facet) && !has(x))
    stop("plot_groups(): `facet` needs `x`", call. = FALSE)
  s <- rec$strata; th <- rec$thresholds; n <- length(rec$hierarchy)
  discard <- identical(rec$what, "discard")
  miss <- setdiff(c(if (has(x)) x, if (has(facet)) facet), names(s))
  if (length(miss))
    stop("plot_groups(): the strata have no column ", paste0("'", miss, "'", collapse = ", "),
         " (they have: ", paste(setdiff(names(s), c("weight_t", "covered_t", "level_label")),
                                collapse = ", "), ")", call. = FALSE)
  weight_word <- if (discard) "landings" else "catch"
  label_of <- function(level, label) sprintf("L%d %s", level, label)
  lv <- unique(rec$levels[c("level", "label")])
  all_levels <- label_of(lv$level, lv$label)[order(-lv$level)]      # the narrowest first, as the table words them
  cols <- plot_colours(all_levels, "raising level", palette)
  caption <- sub_wrap(
    sprintf("grouping = %s (the last is pooled across first).", paste(rec$hierarchy, collapse = " > ")),
    sprintf("thresholds = coverage >= %s%%%s%s.", th$cov,
            if (!is.na(th$trips)) sprintf(", trips >= %s", th$trips) else "",
            if (!is.na(th$fish)) sprintf(", fish >= %s", th$fish) else ""),
    "L<n> = the pool shares the first n variables of the grouping with the stratum (L0: everything).")

  if (!has(x)) {
    d <- rec$levels
    if (is.null(d$category)) d$category <- ""
    d$level_label <- factor(label_of(d$level, d$label), rev(all_levels))
    sampled <- d$weight_t - d$uncovered_t
    long <- rbind(data.frame(d[c("category", "level_label", "n_strata")], part = "Sampled", t = sampled),
                  data.frame(d[c("category", "level_label", "n_strata")],
                             part = if (discard) "Raised" else "Allocated", t = d$uncovered_t))
    long$part <- factor(long$part, c("Sampled", if (discard) "Raised" else "Allocated"))
    ends <- stats::aggregate(t ~ category + level_label + n_strata, data = long, FUN = sum)
    ends$label <- sprintf("%d %s", ends$n_strata, ifelse(ends$n_strata == 1, "stratum", "strata"))
    return(ggplot(long, aes(x = t, y = level_label, fill = level_label, alpha = part)) +
      geom_col_hatch(orientation = "y", position = position_stack(reverse = TRUE),
                     colour = OUTLINE, linewidth = OUTLINE_LW) +
      geom_text(data = ends, aes(x = t, y = level_label, label = label), inherit.aes = FALSE,
                hjust = -0.1, size = 3) +
      scale_fill_manual(values = cols, guide = "none") +
      scale_alpha_manual(values = stats::setNames(c(1, 0.45), levels(long$part)), name = NULL,
                         guide = guide_legend(override.aes = list(fill = "grey30"))) +
      scale_x_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.18))) +
      scale_y_discrete(drop = FALSE) +
      facet_layer(if (length(unique(long$category)) > 1) "category", ncol = 1, scales = "free_x") +
      labs(title = sprintf("Where the %s %s %s from", weight_word, if (discard) "are" else "is",
                           if (discard) "raised" else "allocated"),
           x = sprintf("%s weight (t)", tools::toTitleCase(weight_word)), y = NULL,
           caption = sub_wrap(
             sprintf("bars = %s weight (t) of the strata that take the level; transparent = the part of it with no sample of its own, %s from the pool.",
                     weight_word, if (discard) "raised" else "allocated"), caption)) +
      app_theme() + theme(legend.position = "top"))
  }

  d <- s
  d$.x <- as.character(d[[x]])
  d$.level <- factor(d$level_label, all_levels)
  if (has(facet)) d$.f <- as.character(d[[facet]])
  d <- stats::aggregate(d$weight_t, d[c(".x", ".level", if (has(facet)) ".f")], sum)
  names(d)[ncol(d)] <- "t"
  ord <- names(sort(tapply(d$t, d$.x, sum), decreasing = TRUE))
  d$.x <- factor(d$.x, ord)
  ggplot(d, aes(x = .x, y = t, fill = .level)) +
    geom_col(position = if (share) "fill" else "stack", colour = OUTLINE, linewidth = OUTLINE_LW,
             width = 0.8) +
    scale_fill_manual(values = cols, breaks = levels(droplevels(d$.level)), drop = TRUE, name = "Level") +
    scale_y_continuous(labels = if (share) scales::percent else scales::comma,
                       expand = expansion(mult = c(0, 0.05))) +
    facet_layer(if (has(facet)) ".f", ncol = 3, scales = "free_x") +
    labs(title = sprintf("Level of the groups, by %s", x), x = NULL,
         y = if (share) sprintf("Share of the %s weight", weight_word)
             else sprintf("%s weight (t)", tools::toTitleCase(weight_word)),
         caption = sub_wrap(
           sprintf("bars = %s weight (t) of the strata of each %s, coloured by the level they take.",
                   weight_word, x), caption)) +
    app_theme() + theme(legend.position = "right",
                        axis.text.x = element_text(angle = if (length(ord) > 6) 45 else 0,
                                                   hjust = if (length(ord) > 6) 1 else 0.5))
}

#' Raised catch of several scenarios, side by side
#'
#' @description
#' [plot_catches()] for two or more raised catches at once - two ways of
#' defining the groups, say. For each level of `x` (and each panel of `facet`)
#' one bar per scenario: its colour says which scenario, its mark whether the
#' weight came with the data (imported, solid) or was added by the raising
#' (raised, transparent and hatched). The bars are catch weight, or only the landings or
#' only the discards with `catchCategory`.
#'
#' @details
#' Only the raised catches are needed: the plot sums each of them itself, at
#' whatever `x` and `facet` name, so changing either never needs a table built
#' first. They are written `list(a, b)` or `c(a, b)`; a scenario without a
#' name of its own is called what its object is called, and `scenarios_names`
#' overrides either. Every raised catch must carry the columns `x` and `facet`
#' name.
#'
#' Catch is every catch category: landings, discards and anything else the
#' raised catch carries (BMS), which counts toward the catch but is neither
#' landings nor discards. [raised_overview_table()] gives the same comparison
#' as a table, category by category.
#'
#' @param raised_catch_scenarios Two or more raised catches (the output of
#'   [catchRaising()]), written `list(a, b)` or `c(a, b)`.
#' @param scenarios_names Optional names for the scenarios, in the same order.
#'   Default: the names given inside `c()` / `list()`, or else the object
#'   names.
#' @param x Column on the x axis: one group of bars per level. `NULL`
#'   (default): one group for everything.
#' @param facet Optional facet column.
#' @param catchCategory Optional filter to just `"Lan"` or `"Dis"` (matched
#'   case-insensitively), so discards are readable on their own when landings
#'   would otherwise dominate the scale. `NULL` (default) keeps every
#'   category, summed.
#' @param ncol Facet columns, when `facet` is set.
#' @param palette `"default"`, `"colorblind"` or `"gradual"`: the scenario
#'   colours.
#' @inheritParams saving-plots
#' @return A ggplot object, or `NULL` when there is nothing to draw.
#' @seealso [raised_overview_table()] for the same comparison as a table;
#'   [plot_catches()] for one raised catch.
#' @family raising plots
#' @md
#' @examples
#' \dontrun{
#' cef <- subset(catch, year == 2023)
#' log <- tempfile(fileext = ".txt")
#'
#' # the groups, and the data each takes its discard ratio from: its own, or everything
#' strataCond <- list(TBB  = rlang::quo(metier4 == "TBB"),
#'                    REST = rlang::quo(metier4 != "TBB"))
#' own <- strataCond
#' all <- list(TBB = rlang::quo(TRUE), REST = rlang::quo(TRUE))
#' raised_own <- catchRaising(cef, strataCond, own, logFile = log)
#' raised_all <- catchRaising(cef, strataCond, all, logFile = log)
#'
#' # the scenarios are named after the objects ...
#' plot_compare_raised(c(raised_own, raised_all), x = "metier4")
#'
#' # ... or after you; discards only, one panel per quarter
#' plot_compare_raised(c(own = raised_own, everything = raised_all),
#'                     x = "metier4", facet = "seasonValue", catchCategory = "Dis")
#' }
#' @export
plot_compare_raised <- function(raised_catch_scenarios, scenarios_names = NULL,
                            x = NULL, facet = NULL, catchCategory = NULL,
                            ncol = 2,
                            palette = c("default", "colorblind", "gradual"), save = NULL) {
  save_on_exit(save, "plot_compare_raised")
  palette <- match.arg(palette)
  has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
  sc <- scenario_list(substitute(raised_catch_scenarios),
                      function() raised_catch_scenarios, parent.frame(),
                      scenarios_names, fn = "plot_compare_raised()", min = 2L)$data
  keys <- c(if (has(x)) x, if (has(facet)) facet)
  for (nm in names(sc)) {
    miss <- setdiff(keys, names(sc[[nm]]))
    if (length(miss))
      stop("plot_compare_raised(): '", nm, "' has no column ",
           paste0("'", miss, "'", collapse = ", "), call. = FALSE)
  }
  want <- NULL
  if (has(catchCategory))
    want <- switch(toupper(catchCategory),
                   LAN = , LANDINGS = "Landings",
                   DIS = , DISCARDS = "Discards",
                   stop("plot_compare_raised(): `catchCategory` must be \"Lan\" ",
                        "or \"Dis\"", call. = FALSE))
  cc_label <- if (is.null(want)) "Catch" else want

  # one row per raised-catch row, in tonnes, tagged with its scenario and
  # whether it was imported or raised - the same reading as plot_catches()
  d <- do.call(rbind, lapply(names(sc), function(nm) {
    r <- sc[[nm]]
    u <- if ("variableUnit" %in% names(r)) r$variableUnit else NULL
    out <- data.frame(
      scenario = rep(nm, nrow(r)),
      .t = convert_units(r$total, u, "t"),
      src = ifelse(r$importedOrRaised %in% "raised", "Raised", "Imported"),
      .cat = ifelse(toupper(r$catchCategory) %in% c("LAN", "LANDINGS"), "Landings",
             ifelse(toupper(r$catchCategory) %in% c("DIS", "DISCARDS"),
                    "Discards", "Other")),
      .x = if (has(x)) as.character(r[[x]]) else rep("All", nrow(r)),
      stringsAsFactors = FALSE)
    if (has(facet)) out$.f <- as.character(r[[facet]])
    out
  }))
  if (!is.null(want)) d <- d[d$.cat == want, , drop = FALSE]
  if (!nrow(d)) return(NULL)
  d$.x <- scenario_factor(d$.x, sc, if (has(x)) x)
  if (has(facet)) d$.f <- scenario_factor(d$.f, sc, facet)
  d$scenario <- factor(d$scenario, levels = names(sc))
  d$src <- factor(d$src, levels = c("Imported", "Raised"))
  d <- d %>%
    group_by(across(all_of(c(".x", "scenario", "src",
                             if (has(facet)) ".f")))) %>%
    summarise(t = sum(.t, na.rm = TRUE), .groups = "drop")

  # each level of x holds one slot per scenario, side by side; imported and
  # raised stack inside a scenario's slot
  xl <- levels(d$.x)
  n  <- nlevels(d$scenario)
  w  <- 0.84 / n
  d$.xn <- as.integer(d$.x) + (as.integer(d$scenario) - (n + 1) / 2) * w

  ggplot(d, aes(x = .xn, y = t, fill = scenario, alpha = src)) +
    geom_col_hatch(width = w * 0.9, position = position_stack(reverse = TRUE),
                   colour = OUTLINE, linewidth = 0.25) +
    scale_x_continuous(breaks = seq_along(xl), labels = xl,
                       expand = expansion(add = 0.55)) +
    scale_fill_manual(values = plot_colours(levels(d$scenario), "scenario", palette),
                      name = "Scenario", na.value = "grey85") +
    scale_alpha_manual(values = c(Imported = 1, Raised = 0.45), name = NULL,
                       guide = guide_legend(override.aes = list(fill = "grey30"))) +
    scale_y_continuous(labels = scales::comma) +
    facet_layer(if (has(facet)) ".f", ncol = ncol, scales = "free_y") +
    labs(title = sprintf("Raised %s per stratum, by scenario", tolower(cc_label)),
         caption = sub_wrap(
           "grouping = no grouping, raw data per stratum.",
           sprintf("bars = %s weight (t), summed per stratum.", tolower(cc_label)),
           "color = discard raising/allocation scenario.",
           "solid = imported from the data, hatched = added by raising.",
           width = 110),
         x = if (has(x)) x else NULL, y = sprintf("%s (t)", cc_label)) +
    app_theme() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
