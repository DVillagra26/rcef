# ============================================================
#  viz_raising.R
#  Raising / allocation diagnostic plots - alluvial flow diagrams and the
#  scenario-comparison line plot. Built on ggalluvial and deliberately
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
#' @param df A recommendation table from [recommend_raising()] or a tagged
#'   table from [assign_rule_groups()].
#' @param left_var Column on the left axis, usually the stratum label.
#' @param right_var Column on the right axis, usually the donor group.
#' @param value_var Column giving each ribbon its width.
#' @param pal Named colours for the groups, e.g. from `group_palette()`.
#' @param title Plot title.
#' @param left_label,right_label Axis labels.
#' @param stratum_axis Label for the stratum axis.
#' @return A ggplot object, or `NULL` when there is nothing to draw.
#' @seealso [plot_river_level()], [recommend_raising()]
#' @family raising plots
#' @export
plot_river <- function(df, left_var, right_var = "raise_level",
                       value_var = "landings", pal = NULL,
                       title = "Which strata are raised from which group",
                       left_label = NULL, right_label = NULL,
                       stratum_axis = "Stratum") {
  # left_label / right_label: optional character vectors (same length as
  # df) giving the text to show on each axis. When supplied they override
  # the raw left_var / right_var values - this is how a stratum is shown
  # as its full Fleet | Country | Area | Season combination, and a donor
  # group as the MEANING of its rule ("metier4 %in% ... & Country %in% ...")
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
  if (is.null(pal)) pal <- group_palette(unique(d$.fill))

  # wrap long labels so tall boxes can hold the full text
  wrap <- function(x, width = 26)
    vapply(x, function(s) paste(strwrap(s, width = width), collapse = "\n"),
           character(1))
  d$.L <- wrap(d$.L); d$.R <- wrap(d$.R, 30)

  ggplot(d, aes(y = w, axis1 = .L, axis2 = .R)) +
    ggalluvial::geom_alluvium(aes(fill = .fill), width = 0.30, alpha = 0.75,
                              colour = "grey40", linewidth = 0.2) +
    ggalluvial::geom_stratum(width = 0.30, fill = "grey96", colour = "grey45") +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)),
              size = 2.7, lineheight = 0.82) +
    scale_x_discrete(limits = c(stratum_axis, "Raised from"),
                     expand = c(0.16, 0.05)) +
    scale_fill_manual(values = pal, name = "Raising group") +
    scale_y_continuous(labels = scales::comma) +
    labs(title = title, y = "Landings (t)", x = NULL) +
    app_theme() +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 9, colour = "grey35"))
}

#' River plot for a single cascade step
#'
#' [plot_river()] restricted to the strata that left the cascade at one step, so
#' a single rung of the ladder can be read on its own instead of competing with
#' every other rung for space.
#'
#' @param rec_g A recommendation table.
#' @param step_id The cascade step to show.
#' @param dims The stratum dimensions.
#' @param pal Named colours for the groups.
#' @param value_var Column giving each ribbon its width.
#' @return A ggplot object, or `NULL`.
#' @seealso [plot_river()], [decision_tree2()]
#' @family raising plots
#' @export
plot_river_level <- function(rec_g, step_id, dims, pal,
                             value_var = "landings") {
  d <- rec_g[rec_g$raise_step == step_id, , drop = FALSE]
  if (!nrow(d) || sum(d[[value_var]], na.rm = TRUE) <= 0) return(NULL)
  # a title that says what the reader is looking at: the strata resolved
  # at this step on the left, the donor pool they were raised from on the
  # right, ribbon width = landings
  lvl <- unique(d$raise_level)
  ttl <- if (length(lvl) == 1)
    sprintf("Step: %s -- strata raised here, and the data raising them", lvl)
  else "Strata raised at this step, and the data raising them"
  plot_river(d, left_var = NULL, right_var = "group_id",
             value_var = value_var, pal = pal, title = ttl,
             left_label = stratum_label(d, dims),
             right_label = rule_label(d),
             stratum_axis = stratum_axis_label(dims))
}

#' Compare scenarios side by side
#'
#' Draws the raised result of several scenarios against each other, so the
#' effect of a different threshold or hierarchy is visible per stratum rather
#' than only in the totals.
#'
#' @param cmp Output of [compare_scenarios()].
#' @param x_var Column on the x axis.
#' @param facet_var Optional facet column.
#' @param ncol Facet columns.
#' @return A ggplot object, or `NULL`.
#' @seealso [compare_scenarios()], [comparison_table()]
#' @family raising plots
#' @export
plot_comparison <- function(cmp, x_var, facet_var = NULL, ncol = 2) {
  if (!nrow(cmp)) return(NULL)
  has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)

  d <- cmp %>%
    mutate(Imported = observed_discards,
           Raised   = pmax(estimated_discards - observed_discards, 0)) %>%
    tidyr::pivot_longer(c(Imported, Raised),
                        names_to = "component", values_to = "value") %>%
    mutate(component = factor(component, levels = c("Imported", "Raised")))

  xl <- sort(unique(as.character(d[[x_var]])))
  sc <- sort(unique(d$scenario))
  n  <- length(sc)
  w  <- 0.84 / n                       # slot width per scenario
  d$.xi <- match(as.character(d[[x_var]]), xl)
  d$.si <- match(d$scenario, sc)
  d$.x  <- d$.xi + (d$.si - (n + 1) / 2) * w

  ggplot(d, aes(x = .x, y = value, fill = scenario, alpha = component)) +
    geom_col(width = w * 0.9, position = position_stack(reverse = TRUE),
             colour = OUTLINE, linewidth = 0.25) +
    scale_x_continuous(breaks = seq_along(xl), labels = xl,
                       expand = expansion(add = 0.55)) +
    scale_fill_var("scenario", d$scenario, name = "Allocation") +
    scale_alpha_manual(values = c(Imported = 1, Raised = 0.45), name = NULL,
                       guide = guide_legend(
                         override.aes = list(fill = "grey30"))) +
    scale_y_continuous(labels = scales::comma) +
    (if (has(facet_var)) facet_wrap(vars(.data[[facet_var]]), ncol = ncol,
                                    scales = "free_y") else NULL) +
    labs(title = "Discards by raising scenario",
         subtitle = paste("One bar per scenario. Solid = imported from the",
                          "data, translucent = added by raising."),
         x = x_var, y = "Discards (t)", fill = "Scenario") +
    app_theme() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
