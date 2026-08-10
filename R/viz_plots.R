# =====================================================================
#  viz_plots.R   (plot_coverage(), plot_biology_coverage(), plot_discard_rate(), plot_biology_distribution())
#
#  ORIGIN: this project (ILVO / RCEF Workbench). Freely refactorable.
#  Was: R_development/0_Functions_plots.R
#  No shiny in this file. If it needs `input`, `session` or a tag,
#  it belongs in app/, not here.
# =====================================================================

# ============================================================
#  0_Functions_plots.R
#  Exploratory plots for the RCEF / discard-raising workflow.
# ------------------------------------------------------------
#  Package-ready plotting module. It is meant to live in the package R/
#  folder and be usable on its own in RStudio, exactly as it is used inside
#  the Shiny workbench. Nothing here reads app state: every function takes a
#  prepared data frame and returns a ggplot object (or NULL when there is
#  nothing to draw), so the same call works at the console and in the app.
#
#  PUBLIC API (four dispatchers, one per exploratory question):
#    plot_coverage()             heatmap / bubbles / bars - how well a set of
#                                strata is sampled (discards).
#    plot_biology_coverage()     the same three views for length or age
#                                sampling (shares the engine; wording and the
#                                status scheme differ).
#    plot_discard_rate()         raw per-stratum points, or a boxplot of the
#                                spread over a grouping.
#    plot_biology_distribution() numbers at length/age (histogram or smoothed
#                                density) and mean weight (line + points).
#
#  Each dispatcher wraps small, single-purpose builder functions
#  (plot_heatmap(), plot_bubbles(), plot_coverage_bars(), plot_dr_raw(),
#  plot_dr_box(), plot_length_hist(), plot_length_ridges(),
#  plot_length_meanweight()), which stay exported so a caller who wants
#  exactly one view can reach it directly.
#
#  PACKAGE DEPENDENCIES
#    Imports: ggplot2, dplyr, tidyr, ggrepel, scales, rlang, stats, tools
#    Provided by the rest of the package (not redefined here, so nothing is
#    duplicated): classify_strata(), cat_lookup(), status_palette() for the
#    stratum classification and status colours; app_theme() for the shared
#    look; var_palette(), scale_fill_var(), scale_colour_var(), PAL_BASE for
#    the per-level colour scheme.
# ============================================================

#' @keywords internal
#' @name rcef-plots-imports
#' @noRd
#' @import ggplot2
#' @importFrom dplyr %>% across all_of arrange bind_rows case_when desc filter
#'   first group_by group_split left_join mutate row_number select summarise
#'   ungroup
#' @importFrom rlang .data
NULL

# ----------------------------------------------------------
#  Shared internal helpers
# ----------------------------------------------------------

#' Facet layer for the coverage and discard-rate plots
#'
#' Builds a single-variable [ggplot2::facet_wrap()], or `NULL` when no facet
#' variable is given, so the caller can add it unconditionally.
#'
#' @param facet_var Column to facet on, or `NULL`/`""` for none.
#' @param free_y Let each panel choose its own y range.
#' @param ncol Number of facet columns.
#' @return A ggplot facet layer, or `NULL`.
#' @keywords internal
#' @noRd
#' @export
maybe_facet <- function(facet_var, free_y = TRUE, ncol = 2) {
  if (is.null(facet_var) || !nzchar(facet_var)) return(NULL)
  facet_wrap(vars(.data[[facet_var]]),
             scales = if (free_y) "free_y" else "fixed", ncol = ncol)
}

#' One- or two-variable facetting for the distribution plots
#'
#' Facets on one variable ([ggplot2::facet_wrap()]), two variables
#' ([ggplot2::facet_grid()], rows x cols) or neither, from up to two column
#' names, so the distribution builders need not repeat the branch.
#'
#' @param f1,f2 Column names for the two facet dimensions; either may be
#'   `NULL`/`""`.
#' @param ncol Facet columns when only one variable is used.
#' @param scales Facet scales: "fixed", "free_x", "free_y" or "free".
#' @return A ggplot facet layer, or `NULL`.
#' @keywords internal
#' @noRd
#' @export
facet_two <- function(f1, f2, ncol = 2, scales = "fixed") {
  has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
  if (has(f1) && has(f2))
    facet_grid(rows = vars(.data[[f1]]), cols = vars(.data[[f2]]),
               scales = scales)
  else if (has(f1)) facet_wrap(vars(.data[[f1]]), ncol = ncol, scales = scales)
  else if (has(f2)) facet_wrap(vars(.data[[f2]]), ncol = ncol, scales = scales)
  else NULL
}

#' Keep only strata that carry a discard estimate
#'
#' A discard rate is meaningful only where discards were observed.
#' `build_strata()` reports a rate of 0 for unsampled strata, which reads as
#' "looked and found none" when it means "nobody looked", so those rows are
#' dropped before any rate plot. The test is a non-missing rate and, when
#' present, `landings_with_discards > 0` (or `coverage > 0`).
#'
#' @param s A stratum table.
#' @return `s` with unsampled strata removed.
#' @keywords internal
#' @noRd
#' @export
sampled_for_discards <- function(s) {
  keep <- !is.na(s$discard_rate)
  if ("landings_with_discards" %in% names(s))
    keep <- keep & !is.na(s$landings_with_discards) & s$landings_with_discards > 0
  else if ("coverage" %in% names(s))
    keep <- keep & !is.na(s$coverage) & s$coverage > 0
  s[keep, , drop = FALSE]
}

#' Collapse strata to one row per plotted cell
#'
#' A heatmap or bubble cell must hold a single value, but the stratum table
#' is finer than the plotted roles, so several rows can land on one cell.
#' This sums their weights, re-derives coverage and the discard rate from the
#' summed weights, and re-runs `classify_strata()` so the cell's status
#' matches its pooled numbers. The measurement count is summed too, so
#' length/age cells can still be flagged as thinly measured after collapsing.
#'
#' @param sc A classified stratum table.
#' @param keys Columns that define a cell (y, x and facet).
#' @param threshold Coverage threshold, passed to `classify_strata()`.
#' @param what "discard", "length" or "age".
#' @param min_meas Measurement threshold for length/age classification.
#' @return One row per cell, re-classified.
#' @keywords internal
#' @noRd
#' @export
collapse_cells <- function(sc, keys, threshold, what = "discard", min_meas = 0) {
  keys <- keys[!is.na(keys) & nzchar(keys)]
  if (!length(keys)) return(sc)
  # carry a measurement count through the collapse (length/age) so the
  # re-classification can still flag thinly measured cells; measurements
  # sum across the fine strata that merge into each cell
  if (!"n_measurements" %in% names(sc)) sc$n_measurements <- 0
  sc %>%
    group_by(across(all_of(keys))) %>%
    summarise(landings = sum(landings, na.rm = TRUE),
              landings_with_discards = sum(landings_with_discards, na.rm = TRUE),
              discards = sum(discards, na.rm = TRUE),
              n_measurements = sum(n_measurements, na.rm = TRUE),
              n_strata = dplyr::n(), .groups = "drop") %>%
    mutate(coverage = ifelse(landings > 0,
                             landings_with_discards / landings * 100, NA_real_),
           discard_rate = ifelse(landings_with_discards + discards > 0,
                                 discards / (landings_with_discards + discards),
                                 NA_real_)) %>%
    classify_strata(threshold, what, min_meas) %>%
    as.data.frame()
}

#' Title-case word for a coverage plot title
#'
#' @param what "discard", "length" or "age".
#' @return "Discard", "Length" or "Age".
#' @keywords internal
#' @noRd
#' @export
cov_title_what <- function(what)
  switch(what, length = "Length", age = "Age", "Discard")

#' Lower-case word for "with / without X sampling"
#'
#' @param what "discard", "length" or "age".
#' @return "discard", "length" or "age".
#' @keywords internal
#' @noRd
#' @export
samp_word <- function(what)
  switch(what, length = "length", age = "age", "discard")

# ----------------------------------------------------------
#  Coverage builders
# ----------------------------------------------------------

#' Standalone legend for the sampling statuses
#'
#' A minimal plot whose only purpose is its legend. It is built from
#' [cat_lookup()] rather than from the data, so it carries a filled key for
#' EVERY possible status, including those absent from the plot being shown.
#' Drawing the legend from this plot (rather than from the coverage plot) is
#' what makes the full set of categories visible with their colours: a
#' discrete scale can list an unused level, but it has no data row from which
#' to draw that level's swatch.
#'
#' The coverage builders attach the arguments needed to rebuild this legend as
#' a `"status_legend"` attribute, so a caller that shows the legend in its own
#' panel can construct it on demand without carrying a second plot around.
#'
#' @inheritParams cat_lookup
#' @param key Glyph the status keys are drawn with: `"tile"` for the heatmap,
#'   `"point"` for the bubble plot. The legend should look like the thing it
#'   describes, so a heatmap gets square swatches and a bubble plot circles.
#' @param size Optional spec for a second, independent SIZE key, as attached
#'   by [plot_bubbles()]: a list with `breaks`, `labels`, `limits`, `range`
#'   and `name`. Built with the SAME limits and range as the plot it came
#'   from, so a key circle is drawn at exactly the diameter that value has in
#'   the plot and can be read off it.
#' @return A ggplot object; only its legend is meant to be used.
#' @seealso [plot_coverage()], [cat_lookup()]
#' @family coverage plots
#' @export
status_legend_plot <- function(threshold, what = "discard", min_meas = 0,
                               key = c("point", "tile"), size = NULL) {
  key <- match.arg(key)
  lk <- cat_lookup(threshold, what, min_meas)
  d <- data.frame(.x = seq_len(nrow(lk)), .y = 1,
                  Status = factor(lk$status, levels = lk$status))
  p <- ggplot(d, aes(x = .data$.x, y = .data$.y, fill = .data$Status)) +
    # the key glyph comes from the geom, so the legend matches the plot:
    # a rect for the heatmap, a filled circle for the bubbles
    (if (identical(key, "tile")) geom_tile(colour = OUTLINE, alpha = 0.9)
     else geom_point(shape = 21, colour = OUTLINE)) +
    scale_fill_manual(values = setNames(lk$colour, lk$status),
                      limits = lk$status, drop = FALSE,
                      name = "Sampling status")
  # A bubble plot carries two messages at once - status in the colour,
  # weight in the diameter - and the second one is unreadable without a
  # scale bar. This adds a throwaway layer whose only purpose is to make
  # ggplot draw a size guide next to the status guide.
  if (!is.null(size) && length(size$breaks)) {
    ds <- data.frame(.x = seq_along(size$breaks), .y = 0,
                     .sz = as.numeric(size$breaks))
    p <- p +
      geom_point(data = ds, inherit.aes = FALSE,
                 aes(x = .data$.x, y = .data$.y, size = .data$.sz),
                 shape = 21, fill = "grey75", colour = OUTLINE, stroke = 0.7) +
      scale_size_continuous(
        range  = if (length(size$range) == 2) size$range else c(4, 14),
        limits = size$limits, breaks = as.numeric(size$breaks),
        labels = if (length(size$labels)) size$labels else ggplot2::waiver(),
        name   = if (length(size$name)) size$name else "Weight (t)")
  }
  p + app_theme()
}

#' Coverage heatmap
#'
#' One tile per cell, filled by sampling status and labelled with the cell's
#' weight. Cells are collapsed to a single value first (see
#' [collapse_cells()]). A lower-level builder behind [plot_coverage()].
#'
#' @param sc Classified stratum table (output of `classify_strata()`).
#' @param y_var Column on the y axis.
#' @param x_var Column on the x axis; `NULL` collapses to a single column.
#' @param facet_var Optional facet column.
#' @param threshold Coverage threshold driving the status colours.
#' @param what "discard", "length" or "age".
#' @param weight_label Word for the weight shown in the tile labels.
#' @param min_meas Measurement threshold, for length/age status.
#' @return A ggplot object, or `NULL` when there is nothing to draw.
#' @seealso [plot_coverage()]
#' @family coverage plots
#' @export
plot_heatmap <- function(sc, y_var, x_var = NULL, facet_var = NULL,
                         threshold = 50, what = "discard",
                         weight_label = "landings", min_meas = 0) {
  d <- sc %>% filter(cat_key != "none")
  if (!nrow(d)) return(NULL)
  if (is.null(x_var) || !nzchar(x_var)) { d$.x <- "All"; x_var <- ".x" }
  d <- collapse_cells(d, c(y_var, x_var, facet_var), threshold, what, min_meas)
  fmt <- function(v) ifelse(is.na(v) | v <= 0, "",
                     ifelse(v > 99, sprintf("%.0f", v),
                     ifelse(v >= 10, sprintf("%.1f", v), sprintf("%.2f", v))))
  p <- ggplot(d, aes(x = factor(.data[[x_var]]), y = .data[[y_var]], fill = Status)) +
    geom_tile(colour = OUTLINE, alpha = 0.9) +
    geom_text(aes(label = fmt(landings)), size = 2.6, fontface = "bold",
              colour = OUTLINE) +
    scale_fill_manual(values = status_palette(threshold, what, min_meas),
                      limits = names(status_palette(threshold, what, min_meas)),
                      drop = FALSE, name = "Sampling status") +
    maybe_facet(facet_var) +
    labs(title = sprintf("%s sampling coverage vs threshold",
                         cov_title_what(what)),
         subtitle = if (any(d$n_strata > 1, na.rm = TRUE))
           sprintf("Each cell aggregates every stratum at this grouping (up to %d per cell)",
                   max(d$n_strata, na.rm = TRUE)) else NULL,
         caption = sprintf("Labels = tonnes of %s | Threshold = %s%%",
                           weight_label, threshold),
         x = if (x_var == ".x") NULL else x_var, y = y_var) +
    app_theme() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  # lets a caller rebuild the complete-status legend (see status_legend_plot).
  # key = "tile": the legend swatches should be squares, like the cells.
  attr(p, "status_legend") <- list(threshold = threshold, what = what,
                                   min_meas = min_meas, key = "tile")
  p
}

#' Coverage bubble plot
#'
#' One bubble per cell, sized by weight and filled by sampling status, with
#' the coverage percent labelled on partly-covered cells. A lower-level
#' builder behind [plot_coverage()].
#'
#' Two guides come out of this: the status colours and, through the
#' `"status_legend"` attribute, a size key giving the weight each diameter
#' stands for. For length and age, only cells that are actually actionable
#' warnings are labelled - the "no lengths/ages sampled" category is left
#' bare, since its numbers are always 0% / n=0 and labelling it drowns the
#' borderline cells in text.
#'
#' @inheritParams plot_heatmap
#' @return A ggplot object, or `NULL`.
#' @seealso [plot_coverage()]
#' @family coverage plots
#' @export
plot_bubbles <- function(sc, y_var, x_var = NULL, facet_var = NULL,
                         threshold = 50, what = "discard",
                         weight_label = "landings", min_meas = 0) {
  d <- sc %>% filter(cat_key != "none")
  if (!nrow(d)) return(NULL)
  if (is.null(x_var) || !nzchar(x_var)) { d$.x <- "All"; x_var <- ".x" }
  # one bubble per cell, or several sit on the same point with their
  # labels overlapping
  d <- collapse_cells(d, c(y_var, x_var, facet_var), threshold, what, min_meas)
  smallest <- suppressWarnings(min(d$landings[d$landings > 0], na.rm = TRUE))
  if (!is.finite(smallest)) smallest <- 1
  d$plot_size <- ifelse(d$cat_key == "disc_only", smallest, d$landings)
  if (what %in% c("length", "age")) {
    # say WHY a bubble is a warning: the coverage when it is short of the
    # coverage threshold, the measurement count when it is short of the
    # measurement threshold, both when it misses both
    cov_bit <- ifelse(!is.na(d$coverage) & d$coverage < threshold,
                      paste0(round(d$coverage, 1), "%"), "")
    mea_bit <- ifelse(!is.na(d$n_measurements) & d$n_measurements < min_meas,
                      paste0("n=", format(round(d$n_measurements),
                                          big.mark = ",", trim = TRUE)), "")
    # Label only the cells a reader has to ACT on. Two exclusions:
    #   - the SAFE categories, which need no explanation;
    #   - "no lengths/ages sampled" (cat_key "nosamp"), whose numbers are
    #     always 0% and n=0 and so say nothing the red colour has not
    #     already said. On a stock where most cells are unsampled that one
    #     category supplies most of the labels, and the crowd of "0%"
    #     buries the handful of borderline cells that actually matter.
    lab_me <- d$cat_key %in% c("full_thin", "safe_thin", "low_ok", "low_thin")
    d$lab <- ifelse(lab_me,
                    paste0(cov_bit,
                           ifelse(nzchar(cov_bit) & nzchar(mea_bit), "\n", ""),
                           mea_bit),
                    "")
  } else {
    d$lab <- with(d, case_when(cat_key == "disc_only" ~ "100%\nDisc",
                               cat_key == "warn" ~ paste0(round(coverage, 1), "%"),
                               TRUE ~ ""))
  }
  # A bubble plot says two things at once: status in the colour, weight in
  # the diameter. The second is guesswork without a scale bar, so the
  # breaks are pinned here (smallest / median / largest actually drawn) and
  # handed to the plot AND to the pulled-out legend, which rebuilds the key
  # with the same limits and range so a key circle is literally the size
  # that value has in the panel.
  sz_vals <- d$plot_size[is.finite(d$plot_size) & d$plot_size > 0]
  SZ <- list(range = c(4, 14), name = sprintf("%s (t)",
                                              tools::toTitleCase(weight_label)))
  if (length(sz_vals)) {
    SZ$limits <- range(sz_vals)
    SZ$breaks <- unique(c(SZ$limits[1], stats::median(sz_vals), SZ$limits[2]))
    SZ$labels <- format(signif(SZ$breaks, 3), big.mark = ",", trim = TRUE,
                        scientific = FALSE)
  } else {
    SZ$limits <- NULL; SZ$breaks <- ggplot2::waiver(); SZ$labels <- ggplot2::waiver()
  }
  p <- ggplot(d, aes(x = factor(.data[[x_var]]), y = .data[[y_var]])) +
    geom_point(aes(size = plot_size, fill = Status), shape = 21,
               colour = OUTLINE, stroke = 0.8) +
    geom_point(data = filter(d, cat_key == "disc_only"),
               shape = 4, colour = OUTLINE, size = 2, stroke = 1) +
    ggrepel::geom_text_repel(aes(label = lab), size = 2.8, fontface = "bold",
                             colour = "red3", box.padding = 0.6,
                             min.segment.length = 0, segment.size = 0.2,
                             segment.colour = "grey40", max.overlaps = Inf) +
    scale_size_continuous(range = SZ$range, limits = SZ$limits,
                          breaks = SZ$breaks, labels = SZ$labels,
                          name = SZ$name) +
    scale_fill_manual(values = status_palette(threshold, what, min_meas),
                      limits = names(status_palette(threshold, what, min_meas)),
                      drop = FALSE, name = "Sampling status",
                      guide = guide_legend(override.aes = list(size = 5))) +
    scale_x_discrete(expand = expansion(0.2)) +
    maybe_facet(facet_var) +
    labs(title = sprintf("%s coverage vs %s", cov_title_what(what), weight_label),
         subtitle = if (any(d$n_strata > 1, na.rm = TRUE))
           sprintf("Each bubble aggregates every stratum at this grouping (up to %d per cell)",
                   max(d$n_strata, na.rm = TRUE)) else NULL,
         caption = if (what == "discard")
           "Cross (X) = discards present with zero official landings"
         else sprintf(paste("Labels = coverage %% below the %s%% threshold",
                            "and/or n measurements below %s |",
                            "unsampled cells are left unlabelled"),
                      threshold, min_meas),
         x = if (x_var == ".x") NULL else x_var, y = y_var) +
    app_theme() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  # key = "point": circles in the legend, like the bubbles in the panel.
  # size = SZ: a second key giving the weight each diameter stands for.
  attr(p, "status_legend") <- list(
    threshold = threshold, what = what, min_meas = min_meas, key = "point",
    size = if (length(sz_vals)) SZ else NULL)
  p
}

#' Sampled-vs-unsampled weight bars
#'
#' For each cell, splits the weight into the part that carries a
#' distribution / discard estimate and the part that does not, drawing the
#' two as separate bars grouped into "with" and "without" sampling sections.
#' Because the split is by weight, the sampled fraction is identical whatever
#' the grouping variable. A lower-level builder behind [plot_coverage()].
#'
#' @param sc Classified stratum table.
#' @param y_var Column on the y axis.
#' @param fill_var Column mapped to bar fill; `NULL` uses `y_var`.
#' @param facet_var Optional facet column.
#' @param ncol Facet columns.
#' @param what "discard", "length" or "age".
#' @param weight_label Word for the weight on the x axis.
#' @return A ggplot object, or `NULL`.
#' @seealso [plot_coverage()]
#' @family coverage plots
#' @export
plot_coverage_bars <- function(sc, y_var, fill_var = NULL, facet_var = NULL,
                               ncol = 2, what = "discard",
                               weight_label = "landings") {
  d <- sc %>% filter(cat_key != "none", landings > 0)
  if (!nrow(d)) return(NULL)
  has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)

  d$.lab   <- as.character(d[[y_var]])
  d$.facet <- if (has(facet_var)) as.character(d[[facet_var]]) else ""
  d$.fill  <- if (has(fill_var))  as.character(d[[fill_var]])  else d$.lab

  wi <- paste("With", samp_word(what), "sampling")
  wo <- paste("Without", samp_word(what), "sampling")

  # Split each stratum's weight into the part that carries a <what>
  # estimate (landings_with_discards) and the part that does not
  # (landings - landings_with_discards), then sum per cell and draw the
  # two as separate bars. A partly-sampled stratum (e.g. TBB, sampled at
  # some sublevels only) therefore shows BOTH a sampled and an unsampled
  # bar, instead of the whole thing counting as sampled. Because the two
  # parts always add back to the same weights, the sampled fraction
  # sum(landings_with_discards) / sum(landings) is identical whatever the
  # grouping variable.
  d <- d %>%
    group_by(.facet, .lab, .fill) %>%
    summarise(.samp   = sum(landings_with_discards, na.rm = TRUE),
              .unsamp = sum(landings, na.rm = TRUE) -
                        sum(landings_with_discards, na.rm = TRUE),
              .groups = "drop") %>%
    tidyr::pivot_longer(c(.samp, .unsamp), names_to = ".part",
                        values_to = "landings") %>%
    filter(landings > 1e-9) %>%
    mutate(sampled = factor(ifelse(.part == ".samp", wi, wo),
                            levels = c(wi, wo))) %>%
    arrange(.facet, desc(sampled), landings) %>%
    mutate(.key = factor(
      paste0(.lab, "\u001F", .fill, "\u001F", sampled, "\u001F", .facet),
      levels = unique(paste0(.lab, "\u001F", .fill, "\u001F", sampled,
                             "\u001F", .facet)))) %>%
    group_by(.facet) %>%
    mutate(y_num = row_number(), facet_total = sum(landings)) %>% ungroup()

  x_edge <- max(d$landings, na.rm = TRUE) * 1.05
  pct <- sum(d$landings[d$sampled == wi]) / sum(d$landings) * 100
  br <- d %>% group_by(.facet, sampled) %>%
    summarise(y_min = min(y_num) - 0.5, y_max = max(y_num) + 0.5,
              y_mid = (min(y_num) + max(y_num)) / 2,
              pctg = sum(landings) / first(facet_total) * 100, .groups = "drop") %>%
    mutate(x_pos = x_edge)
  both <- intersect(d$.facet[d$sampled == wo], d$.facet[d$sampled == wi])
  sep <- if (length(both)) {
    d %>% filter(sampled == wo, .facet %in% both) %>%
      group_by(.facet) %>% summarise(sep_y = max(y_num) + 0.5, .groups = "drop")
  } else data.frame(.facet = character(0), sep_y = numeric(0))

  ggplot(d, aes(x = landings, y = .key, fill = .fill)) +
    geom_col(width = 0.72, colour = OUTLINE) +
    geom_segment(data = br, aes(x = x_pos, xend = x_pos, y = y_min, yend = y_max),
                 inherit.aes = FALSE, linewidth = 0.6, colour = "grey30") +
    geom_text(data = br, aes(x = x_pos * 1.02, y = y_mid,
                             label = paste0(sampled, "\n(", round(pctg, 0), "%)")),
              inherit.aes = FALSE, hjust = 0, size = 3, fontface = "bold",
              colour = OUTLINE) +
    geom_segment(data = sep, aes(x = 0, xend = x_edge, y = sep_y, yend = sep_y),
                 inherit.aes = FALSE, linetype = "dashed", colour = "grey50") +
    scale_y_discrete(labels = setNames(d$.lab, as.character(d$.key))) +
    scale_x_continuous(labels = scales::comma,
                       expand = expansion(mult = c(0, 0.34))) +
    scale_fill_var(if (has(fill_var)) fill_var else y_var, d$.fill,
                   name = if (has(fill_var)) fill_var else y_var) +
    (if (has(facet_var)) facet_wrap(vars(.facet), scales = "free_y", ncol = ncol)
     else NULL) +
    labs(title = sprintf("%s with vs without %s sampling",
                         tools::toTitleCase(weight_label), samp_word(what)),
         subtitle = sprintf("%s%% of %s weight sampled for %s",
                            round(pct, 0), weight_label, samp_word(what)),
         x = sprintf("%s (t)", tools::toTitleCase(weight_label)), y = NULL,
         fill = if (has(fill_var)) fill_var else y_var) +
    coord_cartesian(clip = "off") + app_theme() +
    theme(plot.margin = margin(10, 115, 10, 10))
}

# ----------------------------------------------------------
#  Coverage dispatchers
# ----------------------------------------------------------

#' Sampling-coverage plot (heatmap, bubbles or bars)
#'
#' One view of how well a set of strata is sampled, from a classified
#' stratum table. The same engine serves discard coverage and, through
#' `what`, length/age coverage, so a biology-coverage panel reads exactly
#' like a discard one.
#'
#' @param strata Classified stratum table: the output of `classify_strata()`,
#'   carrying at least `landings`, `landings_with_discards`, `discards`,
#'   `coverage`, `cat_key` and `Status`.
#' @param type "heatmap", "bubbles" or "bars".
#' @param y Column on the y axis (rows).
#' @param fill Second dimension: the x axis for heatmap/bubbles, the bar fill
#'   for bars. `NULL` collapses it away.
#' @param facet Optional facet column.
#' @param threshold Coverage threshold in percent; drives the status colours.
#' @param what "discard" (default), "length" or "age". Changes titles and,
#'   for biology, the status scheme; never the computation.
#' @param weight_label Word for the weight shown, e.g. "landings" or "catch".
#' @param ncol Facet columns (bars only).
#' @param min_meas Measurement threshold for length/age status.
#' @return A ggplot object, or `NULL`.
#' @seealso [plot_biology_coverage()], [plot_discard_rate()], [plot_heatmap()],
#'   [plot_bubbles()], [plot_coverage_bars()]
#' @family coverage plots
#' @examples
#' \dontrun{
#' sc <- classify_strata(build_strata(catch, c("metier4", "Season")), 50)
#' plot_coverage(sc, "heatmap", y = "metier4", fill = "Season")
#' plot_coverage(sc, "bars", y = "metier4")
#' }
#' @export
plot_coverage <- function(strata,
                          type = c("heatmap", "bubbles", "bars"),
                          y, fill = NULL, facet = NULL,
                          threshold = 50, what = "discard",
                          weight_label = "landings", ncol = 2, min_meas = 0) {
  type <- match.arg(type)
  switch(type,
    heatmap = plot_heatmap(strata, y, fill, facet, threshold, what,
                           weight_label, min_meas),
    bubbles = plot_bubbles(strata, y, fill, facet, threshold, what,
                           weight_label, min_meas),
    bars    = plot_coverage_bars(strata, y, fill, facet, ncol, what, weight_label))
}

#' Length- or age-sampling coverage plot
#'
#' Thin wrapper over [plot_coverage()] for biology sampling: it shares the
#' identical heatmap/bubbles/bars engine, so a length- or age-coverage panel
#' looks like a discard-coverage one, with only the wording and the status
#' scheme changed. Here "covered" means a length/age distribution exists for
#' the stratum, and the status also reflects the number of fish measured.
#'
#' @inheritParams plot_coverage
#' @param what "length" or "age".
#' @param weight_label Defaults to "catch".
#' @return A ggplot object, or `NULL`.
#' @seealso [plot_coverage()]
#' @family coverage plots
#' @export
plot_biology_coverage <- function(strata,
                                  type = c("heatmap", "bubbles", "bars"),
                                  y, fill = NULL, facet = NULL,
                                  threshold = 50, what = "length",
                                  weight_label = "catch", ncol = 2, min_meas = 0) {
  plot_coverage(strata, type = type, y = y, fill = fill, facet = facet,
                threshold = threshold, what = what,
                weight_label = weight_label, ncol = ncol, min_meas = min_meas)
}

# ----------------------------------------------------------
#  Discard-rate builders
# ----------------------------------------------------------

#' Discard rate, one point per stratum
#'
#' The discard rate of each sampled stratum as a point, optionally coloured
#' and shaped by grouping variables. A lower-level builder behind
#' [plot_discard_rate()].
#'
#' @param s Stratum table with `discard_rate` and the grouping columns.
#' @param x_var Column on the x axis.
#' @param colour_var Column mapped to point fill.
#' @param shape_var Column mapped to point shape.
#' @param facet_var Optional facet column.
#' @param jitter Jitter the points horizontally.
#' @return A ggplot object, or `NULL` when no stratum has discard data.
#' @seealso [plot_discard_rate()]
#' @family discard-rate plots
#' @export
plot_dr_raw <- function(s, x_var, colour_var = NULL, shape_var = NULL,
                        facet_var = NULL, jitter = TRUE) {
  d <- sampled_for_discards(s)
  if (!nrow(d)) return(NULL)
  has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
  pos <- if (jitter) position_jitter(width = 0.18, height = 0, seed = 1) else "identity"

  ae <- aes(x = .data[[x_var]], y = discard_rate * 100)
  if (has(colour_var)) ae <- modifyList(ae, aes(fill = .data[[colour_var]]))
  if (has(shape_var))  ae <- modifyList(ae, aes(shape = .data[[shape_var]]))

  # only shapes 21-25 take a fill, so pin shape 21 when nothing maps to shape
  p <- ggplot(d, ae) +
    (if (has(shape_var))
       geom_point(size = 3, alpha = 0.85, colour = OUTLINE, stroke = 0.5, position = pos)
     else
       geom_point(size = 3, alpha = 0.85, colour = OUTLINE, stroke = 0.5,
                  shape = 21, position = pos))
  if (has(shape_var))
    p <- p + scale_shape_manual(values = c(21, 22, 23, 24, 25, 8, 3, 4))
  # the fill legend keys inherit the default shape (19), which takes no
  # fill, so the swatches come out empty unless forced to a filled shape
  if (has(colour_var))
    p <- p + guides(fill = guide_legend(
      override.aes = list(shape = 21, size = 3.5, alpha = 1)))
  if (has(shape_var))
    p <- p + guides(shape = guide_legend(
      override.aes = list(fill = "grey45", size = 3.5, alpha = 1)))
  # one colour per level of this variable, shared with every other plot
  if (has(colour_var))
    p <- p + scale_fill_var(colour_var, s[[colour_var]], name = colour_var)

  p +
    scale_y_continuous(labels = function(z) paste0(z, "%")) +
    maybe_facet(facet_var, free_y = FALSE) +
    labs(title = "Discard rate by stratum (raw, no aggregation)",
         subtitle = "One point per stratum that has discard data",
         y = "Discard rate", x = x_var,
         fill = if (has(colour_var)) colour_var else NULL,
         shape = if (has(shape_var)) shape_var else NULL) +
    app_theme() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

#' Discard rate as a boxplot over a grouping
#'
#' Summarises the spread of the discard rate across the underlying strata as
#' a boxplot, optionally overlaying the individual strata and a
#' catch-weighted pooled rate. Runs on the full-resolution table so each box
#' shows real spread. A lower-level builder behind [plot_discard_rate()].
#'
#' @param s_full Full-resolution stratum table.
#' @param x_var Column on the x axis.
#' @param fill_var Column mapped to box fill.
#' @param facet_var Optional facet column.
#' @param show_points Overlay the individual strata.
#' @param show_pooled Add the catch-weighted pooled rate as a red diamond.
#' @param ncol Facet columns.
#' @return A ggplot object, or `NULL`.
#' @seealso [plot_discard_rate()]
#' @family discard-rate plots
#' @export
plot_dr_box <- function(s_full, x_var, fill_var = NULL, facet_var = NULL,
                        show_points = TRUE, show_pooled = TRUE, ncol = 2) {
  d <- sampled_for_discards(s_full)
  if (!nrow(d)) return(NULL)
  has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
  dodge <- position_dodge(width = 0.78, preserve = "single")

  p <- ggplot(d, aes(x = factor(.data[[x_var]]), y = discard_rate * 100))
  if (has(fill_var)) {
    p <- p + geom_boxplot(aes(fill = factor(.data[[fill_var]])),
                          outlier.shape = NA, alpha = 0.5, colour = OUTLINE,
                          position = dodge, width = 0.7)
    if (show_points)
      p <- p + geom_point(aes(fill = factor(.data[[fill_var]])), shape = 21,
                          size = 1.9, alpha = 0.85, colour = OUTLINE,
                          position = position_jitterdodge(
                            jitter.width = 0.16, dodge.width = 0.78, seed = 1))
  } else {
    p <- p + geom_boxplot(outlier.shape = NA, alpha = 0.5, fill = "grey80",
                          colour = OUTLINE, width = 0.6)
    if (show_points)
      p <- p + geom_point(size = 1.9, alpha = 0.75, colour = OUTLINE,
                          position = position_jitter(width = 0.14, seed = 1))
  }

  if (show_pooled) {
    keys <- c(x_var, fill_var, facet_var)
    keys <- keys[!is.null(keys) & !is.na(keys) & nzchar(keys)]
    pooled <- d %>% group_by(across(all_of(keys))) %>%
      summarise(pooled = sum(discards, na.rm = TRUE) /
                  (sum(landings_with_discards, na.rm = TRUE) +
                     sum(discards, na.rm = TRUE)) * 100, .groups = "drop")
    p <- p + geom_point(
      data = pooled,
      mapping = if (has(fill_var))
        aes(x = factor(.data[[x_var]]), y = pooled, group = factor(.data[[fill_var]]))
      else aes(x = factor(.data[[x_var]]), y = pooled),
      shape = 23, size = 3.4, fill = "red2", colour = OUTLINE,
      position = if (has(fill_var)) dodge else "identity",
      inherit.aes = FALSE)
  }

  if (has(fill_var))
    p <- p + scale_fill_var(fill_var, d[[fill_var]], name = fill_var)

  p + scale_y_continuous(labels = function(z) paste0(z, "%")) +
    maybe_facet(facet_var, free_y = FALSE, ncol = ncol) +
    labs(title = paste("Discard rate by", x_var,
                       if (has(fill_var)) paste("and", fill_var) else ""),
         subtitle = paste("Box + points = individual strata at full resolution;",
                          "red diamond = catch-weighted pooled rate"),
         y = "Discard rate", x = x_var, fill = fill_var) +
    app_theme() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# ----------------------------------------------------------
#  Discard-rate dispatcher
# ----------------------------------------------------------

#' Discard-rate plot (raw strata or boxplot)
#'
#' Two views of the discard rate. "raw" draws one point per sampled stratum;
#' "box" summarises the spread over a grouping. Both drop strata with no
#' discard estimate (see [sampled_for_discards()]): a rate is a property of
#' sampled strata only.
#'
#' @param strata Stratum table with `discard_rate`, `discards`,
#'   `landings_with_discards` and the grouping columns. For "box", pass the
#'   full-resolution table so each box shows real spread.
#' @param type "raw" or "box".
#' @param x Column on the x axis.
#' @param colour (raw) column mapped to point fill.
#' @param shape (raw) column mapped to point shape.
#' @param facet Optional facet column.
#' @param jitter (raw) jitter the points horizontally.
#' @param fill (box) column mapped to box fill.
#' @param show_points (box) overlay the individual strata.
#' @param show_pooled (box) add the catch-weighted pooled rate.
#' @param ncol Facet columns.
#' @return A ggplot object, or `NULL`.
#' @seealso [plot_dr_raw()], [plot_dr_box()]
#' @family discard-rate plots
#' @examples
#' \dontrun{
#' plot_discard_rate(strata, "raw", x = "Season", colour = "metier4")
#' plot_discard_rate(strata, "box", x = "metier4", fill = "Country")
#' }
#' @export
plot_discard_rate <- function(strata,
                              type = c("raw", "box"),
                              x, colour = NULL, shape = NULL, facet = NULL,
                              jitter = TRUE, fill = NULL,
                              show_points = TRUE, show_pooled = TRUE, ncol = 2) {
  type <- match.arg(type)
  if (type == "box")
    plot_dr_box(strata, x, fill, facet, show_points, show_pooled, ncol)
  else
    plot_dr_raw(strata, x, colour, shape, facet, jitter)
}

# ----------------------------------------------------------
#  Distribution builders
# ----------------------------------------------------------

#' Numbers-at-class histogram
#'
#' Bars of numbers over the length or age classes, stacked or dodged on an
#' optional fill, with up to two facet variables. A lower-level builder
#' behind [plot_biology_distribution()].
#'
#' @param d Numbers-at-class table (`length_numbers()` output): columns
#'   `.class`, `number`, plus any grouping columns.
#' @param fill_var Column mapped to bar fill.
#' @param facet1,facet2 Up to two facet columns.
#' @param stacked Stack the bars on `fill_var` rather than dodge.
#' @param ncol Facet columns.
#' @param scales Facet scales: "fixed", "free_x", "free_y" or "free".
#' @param class_label X-axis label, e.g. "Length (cm)" or "Age (years)".
#' @return A ggplot object, or `NULL`.
#' @seealso [plot_biology_distribution()]
#' @family distribution plots
#' @export
plot_length_hist <- function(d, fill_var = NULL, facet1 = NULL, facet2 = NULL,
                             stacked = TRUE, ncol = 2, scales = "fixed",
                             class_label = "Length (cm)") {
  if (is.null(d) || !nrow(d)) return(NULL)
  has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
  bw <- { u <- sort(unique(d$.class)); if (length(u) > 1) min(diff(u)) else 1 }

  p <- ggplot(d, aes(x = .class, y = number))
  p <- p + if (has(fill_var))
    geom_col(aes(fill = factor(.data[[fill_var]])), width = bw * 0.95,
             position = if (stacked) "stack" else position_dodge(width = bw),
             colour = OUTLINE, linewidth = OUTLINE_LW)
  else geom_col(width = bw * 0.95, fill = PAL_BASE[1], colour = OUTLINE,
                linewidth = OUTLINE_LW)

  if (has(fill_var))
    p <- p + scale_fill_var(fill_var, d[[fill_var]], name = fill_var)

  p +
    scale_y_continuous(labels = scales::comma) +
    facet_two(facet1, facet2, ncol = ncol, scales = scales) +
    labs(title = sprintf("Numbers at %s", tolower(sub(" .*", "", class_label))),
         subtitle = if (has(fill_var)) paste0("Stacked by ", fill_var) else NULL,
         x = class_label, y = "Numbers", fill = fill_var) +
    app_theme()
}

#' Numbers-at-class smoothed density (ridgeline)
#'
#' One area-normalised, weight-smoothed density curve per group, offset
#' vertically so many groups stay readable. Each curve integrates to one, so
#' groups of very different size are compared on shape alone. A lower-level
#' builder behind [plot_biology_distribution()].
#'
#' @param d Numbers-at-class table: columns `.class`, `number`, plus grouping
#'   columns.
#' @param group_var Column that defines the ridge rows; `NULL` uses one row.
#' @param facet1,facet2 Up to two facet columns.
#' @param overlap How far adjacent curves may overlap.
#' @param smooth Bandwidth multiplier: down for detail, up for a smoother
#'   shape.
#' @param ncol Facet columns.
#' @param scales Facet scales.
#' @param class_label X-axis label.
#' @return A ggplot object, or `NULL`.
#' @seealso [plot_biology_distribution()]
#' @family distribution plots
#' @export
plot_length_ridges <- function(d, group_var, facet1 = NULL, facet2 = NULL,
                               overlap = 0.75, smooth = 1, ncol = 2,
                               scales = "fixed",
                               class_label = "Length (cm)") {
  if (is.null(d) || !nrow(d)) return(NULL)
  has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
  if (!has(group_var)) { d$.g <- "All"; group_var <- ".g" }
  d$.g <- as.character(d[[group_var]])

  facets <- c(if (has(facet1)) facet1, if (has(facet2)) facet2)
  key <- c(".g", facets)

  # A kernel density rather than a bare normalised histogram: the counts
  # at each class are used as weights, so a class where 500 fish were
  # measured pulls the curve harder than one with 5. `smooth` multiplies
  # the bandwidth - turn it down for a well-sampled stratum to keep the
  # detail, up for a sparse or noisy one so the shape is still readable
  # instead of a comb of spikes.
  dens_one <- function(g) {
    x <- g$.class; w <- g$number
    ok <- is.finite(x) & is.finite(w) & w > 0
    x <- x[ok]; w <- w[ok]
    if (length(x) < 2 || sum(w) <= 0) return(NULL)
    if (length(unique(x)) < 2) return(NULL)
    p <- w / sum(w)
    # stats::density picks its bandwidth from the unweighted points, which
    # ignores exactly the information that matters here, so it is computed
    # from the WEIGHTED spread instead (Silverman's rule on the weighted
    # sd). `smooth` then scales it.
    mu <- sum(p * x)
    sdw <- sqrt(sum(p * (x - mu)^2))
    if (!is.finite(sdw) || sdw <= 0) sdw <- diff(range(x)) / 6
    n_eff <- 1 / sum(p^2)                    # Kish effective sample size
    bw <- 1.06 * sdw * n_eff^(-1 / 5) * smooth
    if (!is.finite(bw) || bw <= 0) return(NULL)
    dn <- tryCatch(stats::density(x, weights = p, bw = bw, n = 256,
                                  from = min(x) - 2 * bw,
                                  to = max(x) + 2 * bw),
                   error = function(e) NULL)
    if (is.null(dn)) return(NULL)
    data.frame(.class = dn$x, .dens = dn$y)
  }

  parts <- d %>% group_by(across(all_of(key))) %>% group_split()
  built <- lapply(parts, function(g) {
    dd <- dens_one(g)
    if (is.null(dd)) return(NULL)
    for (k in key) dd[[k]] <- g[[k]][1]
    dd })
  built <- built[!vapply(built, is.null, logical(1))]
  if (!length(built)) return(NULL)
  d <- bind_rows(built)
  if (!nrow(d) || max(d$.dens, na.rm = TRUE) <= 0) return(NULL)

  # vertical offset: one row per group, curves scaled to overlap a little
  lev <- sort(unique(d$.g))
  d$.row <- match(d$.g, lev)
  scale_h <- (1 + overlap) / max(d$.dens, na.rm = TRUE)
  d$.y <- d$.row + d$.dens * scale_h

  ggplot(d, aes(x = .class, group = .g)) +
    geom_ribbon(aes(ymin = .row, ymax = .y, fill = .g), alpha = 0.72,
                colour = NA) +
    geom_line(aes(y = .y, colour = .g), linewidth = 0.5) +
    geom_segment(aes(x = min(.class, na.rm = TRUE),
                     xend = max(.class, na.rm = TRUE),
                     y = .row, yend = .row), colour = "grey65",
                 linewidth = 0.25) +
    scale_y_continuous(breaks = seq_along(lev), labels = lev,
                       expand = expansion(mult = c(0.02, 0.12))) +
    scale_fill_var(group_var, d$.g, name = group_var) +
    scale_colour_var(group_var, d$.g, name = group_var) +
    facet_two(facet1, facet2, ncol = ncol, scales = scales) +
    labs(title = "Frequency, as a smoothed density",
         subtitle = sprintf(paste("Each curve integrates to one, so strata of",
                                  "very different size are compared on shape",
                                  "alone (smoothing x%.2f)"), smooth),
         x = class_label, y = NULL) +
    app_theme() +
    theme(legend.position = "none",
          panel.grid.major.y = element_blank())
}

#' Mean-weight-at-class line
#'
#' Mean weight at length or age as a line with points. A +/- 1 sd ribbon
#' shows the spread hidden by grouping; setting `shape_var` instead draws
#' each stratum separately and drops the ribbon. A lower-level builder behind
#' [plot_biology_distribution()].
#'
#' @param d Mean-weight table (`length_meanweight()` output): columns
#'   `.class`, `mean_weight`, `sd_weight`, plus grouping columns.
#' @param colour_var Column mapped to line colour.
#' @param facet1,facet2 Up to two facet columns.
#' @param shape_var Column drawn as separate point shapes; opens the averaged
#'   group back up and drops the sd ribbon.
#' @param show_points Draw the points.
#' @param show_spread Draw the sd ribbon (when strata are averaged).
#' @param ncol Facet columns.
#' @param scales Facet scales.
#' @param class_label X-axis label.
#' @return A ggplot object, or `NULL`.
#' @seealso [plot_biology_distribution()]
#' @family distribution plots
#' @export
plot_length_meanweight <- function(d, colour_var = NULL, facet1 = NULL,
                                   facet2 = NULL, shape_var = NULL,
                                   show_points = TRUE, show_spread = TRUE,
                                   ncol = 2, scales = "fixed",
                                   class_label = "Length (cm)") {
  if (is.null(d) || !nrow(d)) return(NULL)
  has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)

  grp <- if (has(colour_var)) rlang::sym(colour_var) else NULL
  p <- if (has(colour_var))
    ggplot(d, aes(x = .class, y = mean_weight,
                  colour = factor(!!grp), group = factor(!!grp)))
  else ggplot(d, aes(x = .class, y = mean_weight, group = 1))

  # the ribbon only means something when strata were actually collapsed
  if (show_spread && !has(shape_var) && "sd_weight" %in% names(d) &&
      any(!is.na(d$sd_weight))) {
    p <- p + if (has(colour_var))
      geom_ribbon(aes(ymin = pmax(0, mean_weight - sd_weight),
                      ymax = mean_weight + sd_weight,
                      fill = factor(!!grp)),
                  alpha = 0.18, colour = NA)
    else geom_ribbon(aes(ymin = pmax(0, mean_weight - sd_weight),
                         ymax = mean_weight + sd_weight),
                     alpha = 0.18, fill = PAL_BASE[1], colour = NA)
  }

  p <- p + geom_line(linewidth = 0.65, alpha = 0.9)
  if (show_points) {
    p <- p + if (has(shape_var))
      geom_point(aes(shape = factor(.data[[shape_var]])), size = 2.3,
                 stroke = 0.5, fill = "white")
    else if (has(colour_var))
      geom_point(aes(fill = factor(!!grp)), shape = 21, size = 2.2,
                 colour = OUTLINE, stroke = 0.4)
    else geom_point(shape = 21, size = 2.2, fill = PAL_BASE[1],
                    colour = OUTLINE, stroke = 0.4)
    if (has(shape_var))
      p <- p + scale_shape_manual(values = c(16, 17, 15, 18, 4, 3, 8, 7),
                                  name = shape_var)
  }

  if (has(colour_var))
    p <- p + scale_colour_var(colour_var, d[[colour_var]], name = colour_var) +
             scale_fill_var(colour_var, d[[colour_var]], name = colour_var)

  sub <- if (has(shape_var))
    paste0("Each stratum shown separately by ", shape_var)
  else if (show_spread)
    "Line = number-weighted mean, band = +/- 1 sd across the grouped strata"
  else NULL

  p +
    scale_y_continuous(labels = scales::comma) +
    facet_two(facet1, facet2, ncol = ncol, scales = scales) +
    labs(title = sprintf("Mean weight at %s",
                         tolower(sub(" .*", "", class_label))), subtitle = sub,
         x = class_label, y = "Mean weight (g)",
         colour = colour_var, fill = colour_var, shape = shape_var) +
    app_theme()
}

# ----------------------------------------------------------
#  Distribution dispatcher
# ----------------------------------------------------------

#' Numbers- or mean-weight-at-length/age distribution plot
#'
#' One entry point for the biology distribution views. `quantity =
#' "numbers"` draws numbers at class as a histogram (`type = "histogram"`) or
#' a smoothed density (`type = "density"`); `quantity = "meanweight"` draws
#' mean weight at class as a line (`type = "line"`).
#'
#' @param data For "numbers", a `length_numbers()` table (`.class`, `number`,
#'   grouping columns); for "meanweight", a `length_meanweight()` table
#'   (`.class`, `mean_weight`, `sd_weight`, ...).
#' @param quantity "numbers" or "meanweight".
#' @param type "histogram" or "density" for numbers; "line" for meanweight.
#' @param fill (numbers) column mapped to bar fill / density group.
#' @param colour (meanweight) column mapped to line colour.
#' @param facet1,facet2 Up to two facet columns.
#' @param shape (meanweight) column drawn as separate point shapes.
#' @param stacked (histogram) stack the bars rather than dodge.
#' @param smooth (density) bandwidth multiplier.
#' @param show_points,show_spread (meanweight) draw the points / sd ribbon.
#' @param ncol Facet columns.
#' @param scales Facet scales: "fixed", "free_x", "free_y" or "free".
#' @param class_label X-axis label, e.g. "Length (cm)" or "Age (years)".
#' @return A ggplot object, or `NULL`.
#' @seealso [plot_length_hist()], [plot_length_ridges()],
#'   [plot_length_meanweight()]
#' @family distribution plots
#' @examples
#' \dontrun{
#' plot_biology_distribution(nums, "numbers", "histogram", fill = "Country")
#' plot_biology_distribution(mw, "meanweight", "line", colour = "metier4")
#' }
#' @export
plot_biology_distribution <- function(data,
                                      quantity = c("numbers", "meanweight"),
                                      type = c("histogram", "density", "line"),
                                      fill = NULL, colour = NULL,
                                      facet1 = NULL, facet2 = NULL, shape = NULL,
                                      stacked = TRUE, smooth = 1,
                                      show_points = TRUE, show_spread = TRUE,
                                      ncol = 2, scales = "fixed",
                                      class_label = "Length (cm)") {
  quantity <- match.arg(quantity)
  type <- match.arg(type)
  if (quantity == "numbers") {
    if (type == "density")
      plot_length_ridges(data, fill, facet1, facet2, smooth = smooth,
                         ncol = ncol, scales = scales, class_label = class_label)
    else
      plot_length_hist(data, fill, facet1, facet2, stacked = stacked,
                       ncol = ncol, scales = scales, class_label = class_label)
  } else {
    plot_length_meanweight(data, colour, facet1, facet2, shape,
                           show_points = show_points, show_spread = show_spread,
                           ncol = ncol, scales = scales, class_label = class_label)
  }
}

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

# ----------------------------------------------------------
#  Mean weight, drawn as a spread rather than a point
# ----------------------------------------------------------

#' Mean weight at length or age, as boxes
#'
#' The companion to [plot_length_meanweight()], and a separate plot
#' rather than an option on it, because the two answer different
#' questions and should not silently swap places. The line plot says
#' "what is the mean weight at this class"; this says "how much did the
#' strata that were averaged into it disagree".
#'
#' A box is drawn wherever a class and group has more than one
#' contributing stratum. Where it has exactly one there is no spread to
#' show, so a point is drawn instead - which is the honest rendering and
#' also what happens naturally once the facets are fine enough to leave
#' one stratum per cell.
#'
#' `shape_var` does not replace the box, it decorates it: the box still
#' shows the spread, and the dots on top are shaped by the variable so
#' the season, or country, behind each value is readable.
#'
#' @param d A [length_meanweight_raw()] table.
#' @param colour_var Column mapped to box fill.
#' @param facet1,facet2 Up to two facet columns.
#' @param shape_var Column mapped to point shape, drawn over the boxes.
#' @param show_points Draw the underlying values over the boxes.
#' @param ncol Facet columns.
#' @param scales Facet scales, as `facet_wrap()` takes.
#' @param class_label X axis label.
#' @return A ggplot, or `NULL` if there is nothing to draw.
#' @seealso [plot_length_meanweight()], [length_meanweight_raw()]
#' @export
plot_length_meanweight_box <- function(d, colour_var = NULL, facet1 = NULL,
                                       facet2 = NULL, shape_var = NULL,
                                       show_points = TRUE, ncol = 2,
                                       scales = "fixed",
                                       class_label = "Length (cm)") {
  if (is.null(d) || !nrow(d)) return(NULL)
  has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)

  d <- d[is.finite(d$mean_weight), , drop = FALSE]
  if (!nrow(d)) return(NULL)
  if (!"n_group" %in% names(d)) d$n_group <- 2L

  # A class axis has to be discrete for boxes to sit side by side, but
  # keeping it numeric underneath would put every box at its own x with
  # no dodging. Grouping on the class value gives one box per class.
  d$.cls <- factor(d$.class, levels = sort(unique(d$.class)))

  grp <- if (has(colour_var)) rlang::sym(colour_var) else NULL
  p <- if (has(colour_var))
    ggplot(d, aes(x = .cls, y = mean_weight, fill = factor(!!grp)))
  else ggplot(d, aes(x = .cls, y = mean_weight))

  many <- d[d$n_group > 1, , drop = FALSE]
  one  <- d[d$n_group <= 1, , drop = FALSE]

  if (nrow(many))
    p <- p + geom_boxplot(data = many, outlier.shape = NA, alpha = 0.55,
                          linewidth = 0.35, colour = OUTLINE,
                          position = position_dodge(width = 0.8))
  if (nrow(one))
    p <- p + if (has(colour_var))
      geom_point(data = one, shape = 21, size = 2.1, colour = OUTLINE,
                 stroke = 0.4, position = position_dodge(width = 0.8))
    else geom_point(data = one, shape = 21, size = 2.1, fill = PAL_BASE[1],
                    colour = OUTLINE, stroke = 0.4)

  if (show_points && nrow(many)) {
    p <- p + if (has(shape_var))
      geom_point(data = many, aes(shape = factor(.data[[shape_var]])),
                 size = 1.8, alpha = 0.9,
                 position = position_dodge(width = 0.8))
    else geom_point(data = many, size = 1.3, alpha = 0.55,
                    position = position_dodge(width = 0.8))
    if (has(shape_var))
      p <- p + scale_shape_manual(values = c(16, 17, 15, 18, 4, 3, 8, 7),
                                  name = shape_var)
  }

  if (has(colour_var))
    p <- p + scale_fill_var(colour_var, d[[colour_var]], name = colour_var)

  n_box <- nrow(many); n_pt <- nrow(one)
  sub <- if (n_box == 0)
    "One stratum per class here, so every value is a point - nothing is being averaged."
  else if (n_pt > 0)
    sprintf("Box = the strata averaged into one mean (%d values); a point is a class with only one stratum.",
            n_box)
  else "Box = the strata averaged into one mean at that class."

  p +
    scale_y_continuous(labels = scales::comma) +
    facet_two(facet1, facet2, ncol = ncol, scales = scales) +
    labs(title = sprintf("Spread of mean weight at %s",
                         tolower(sub(" .*", "", class_label))),
         subtitle = sub, x = class_label, y = "Mean weight (g)",
         fill = colour_var, shape = shape_var) +
    app_theme() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 7))
}
