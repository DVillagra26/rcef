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
collapse_cells <- function(sc, keys, threshold, what = "discard", min_meas = 0,
                           min_trips = 2) {
  keys <- keys[!is.na(keys) & nzchar(keys)]
  if (!length(keys)) return(sc)
  # carry a measurement count through the collapse (length/age) so the
  # re-classification can still flag thinly measured cells; measurements
  # sum across the fine strata that merge into each cell
  if (!"n_measurements" %in% names(sc)) sc$n_measurements <- 0
  # Same for trips: summed, not maxed - a cell that merges several
  # strata genuinely combines several DIFFERENT samples' trips, the
  # same convention stratum_metrics() uses when pooling across strata
  # rather than within one. Without this, every collapsed cell lost its
  # trip count entirely (summarise() only keeps what it is told to),
  # which reclassified every merged cell as "unknown trips" regardless
  # of the real data - the heatmap's colouring would have been wrong
  # for any cell with more than one stratum in it.
  if (!"numTrips" %in% names(sc)) sc$numTrips <- NA_real_
  sc %>%
    group_by(across(all_of(keys))) %>%
    summarise(landings = sum(landings, na.rm = TRUE),
              landings_with_discards = sum(landings_with_discards, na.rm = TRUE),
              discards = sum(discards, na.rm = TRUE),
              n_measurements = sum(n_measurements, na.rm = TRUE),
              numTrips = if (!any(!is.na(numTrips))) NA_real_
                        else sum(numTrips, na.rm = TRUE),
              n_strata = dplyr::n(), .groups = "drop") %>%
    mutate(coverage = ifelse(landings > 0,
                             landings_with_discards / landings * 100, NA_real_),
           discard_rate = ifelse(landings_with_discards + discards > 0,
                                 discards / (landings_with_discards + discards),
                                 NA_real_)) %>%
    classify_strata(threshold, what, min_meas, min_trips) %>%
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
                               key = c("point", "tile"), size = NULL,
                               min_trips = 2) {
  key <- match.arg(key)
  lk <- cat_lookup(threshold, what, min_meas, min_trips)
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
                         weight_label = "landings", min_meas = 0,
                         min_trips = 2) {
  d <- sc %>% filter(cat_key != "none")
  if (!nrow(d)) return(NULL)
  if (is.null(x_var) || !nzchar(x_var)) { d$.x <- "All"; x_var <- ".x" }
  d <- collapse_cells(d, c(y_var, x_var, facet_var), threshold, what, min_meas,
                      min_trips)
  fmt <- function(v) ifelse(is.na(v) | v <= 0, "",
                     ifelse(v > 99, sprintf("%.0f", v),
                     ifelse(v >= 10, sprintf("%.1f", v), sprintf("%.2f", v))))
  p <- ggplot(d, aes(x = factor(.data[[x_var]]), y = .data[[y_var]], fill = Status)) +
    geom_tile(colour = OUTLINE, alpha = 0.9) +
    geom_text(aes(label = fmt(landings)), size = 2.6, fontface = "bold",
              colour = OUTLINE) +
    scale_fill_manual(values = status_palette(threshold, what, min_meas, min_trips),
                      limits = names(status_palette(threshold, what, min_meas,
                                                    min_trips)),
                      labels = status_abbr(threshold, what, min_meas, min_trips),
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
                                   min_meas = min_meas, min_trips = min_trips,
                                   key = "tile")
  p
}

#' Coverage heatmap, drawn with base R graphics
#'
#' Same data and the same colour-by-status scheme as [plot_heatmap()],
#' drawn with `graphics::image()`/`graphics::rect()` instead of ggplot2.
#' Called through `plot_coverage(..., engine = "base")`, not directly.
#'
#' @inheritParams plot_heatmap
#' @return `NULL`, invisibly - drawn on the current graphics device.
#' @seealso [plot_heatmap()], [plot_coverage()]
#' @keywords internal
#' @export
plot_heatmap_base <- function(sc, y_var, x_var = NULL, facet_var = NULL,
                              threshold = 50, what = "discard",
                              weight_label = "landings", min_meas = 0,
                              min_trips = 2, palette = c("default", "colorblind"),
                              angle = 45, n_per_screen = NULL) {
  palette <- match.arg(palette)
  d <- sc %>% filter(cat_key != "none")
  if (!nrow(d)) { message("Nothing to plot."); return(invisible(NULL)) }
  if (is.null(x_var) || !nzchar(x_var)) { d$.x <- "All"; x_var <- ".x" }
  d <- collapse_cells(d, c(y_var, x_var, facet_var), threshold, what,
                      min_meas, min_trips)
  pal <- status_palette(threshold, what, min_meas, min_trips)
  # Every possible status, in the SAME order cat_lookup() defines them
  # (safest first) - a legend built only from the statuses actually
  # PRESENT in this figure was missing whichever ones this particular
  # cut of the data did not happen to hit, which is exactly backwards:
  # the reader needs the full scale to judge where what they ARE
  # seeing sits, especially when the worst (or best) category is
  # exactly the one absent here.
  lk_all <- cat_lookup(threshold, what, min_meas, min_trips)

  draw_one <- function(dd, main) {
    ys <- sort(unique(as.character(dd[[y_var]])))
    xs <- sort(unique(as.character(dd[[x_var]])))
    m_col <- matrix(NA_character_, length(ys), length(xs),
                    dimnames = list(ys, xs))
    m_lab <- matrix(NA_character_, length(ys), length(xs),
                    dimnames = list(ys, xs))
    for (i in seq_len(nrow(dd))) {
      yi <- as.character(dd[[y_var]][i]); xi <- as.character(dd[[x_var]][i])
      m_col[yi, xi] <- pal[as.character(dd$Status[i])] %||% "#CCCCCC"
      m_lab[yi, xi] <- if (is.na(dd$landings[i]) || dd$landings[i] <= 0) ""
                        else if (dd$landings[i] > 99) sprintf("%.0f", dd$landings[i])
                        else if (dd$landings[i] >= 10) sprintf("%.1f", dd$landings[i])
                        else sprintf("%.2f", dd$landings[i])
    }
    graphics::plot(NA, xlim = c(0, length(xs)), ylim = c(0, length(ys)),
                   xaxs = "i", yaxs = "i", axes = FALSE, xlab = "",
                   ylab = "", main = main)
    for (r in seq_along(ys)) for (cc in seq_along(xs)) {
      col <- m_col[r, cc]
      if (is.na(col)) next
      # Black outline (OUTLINE), not white - a tile's own edge should
      # read clearly against its neighbours regardless of how close
      # two statuses' fill colours happen to be.
      graphics::rect(cc - 1, r - 1, cc, r, col = col, border = OUTLINE, lwd = 0.6)
      lab <- m_lab[r, cc]
      if (nzchar(lab))
        graphics::text(cc - 0.5, r - 0.5, lab, cex = 0.7, font = 2,
                       col = OUTLINE)
    }
    graphics::box()
    axis_x_cat(seq_along(xs) - 0.5, xs, angle = angle, cex = 0.8)
    graphics::axis(2, at = seq_along(ys) - 0.5, labels = ys, las = 1,
                   cex.axis = 0.8, tick = TRUE)
  }

  facets <- if (!is.null(facet_var) && facet_var %in% names(d))
    sort(unique(as.character(d[[facet_var]]))) else NA
  # One left margin for every panel, sized to the WIDEST y-axis label
  # anywhere in the figure - not recomputed per panel, or a panel whose
  # own labels happen to be short gets a narrow margin and the next
  # panel's labels bleed into it.
  lmar <- max(6, max(nchar(as.character(d[[y_var]]))) * 0.42)
  # Bottom margin sized the same way for the (possibly 45-degree)
  # x labels, from the widest one anywhere in the figure.
  bmar <- cat_axis_mar(unique(as.character(d[[x_var]])), 0.8, angle)
  groups <- split_facets_to_screens(facets, n_per_screen)
  main_txt <- sprintf("%s sampling coverage vs threshold", cov_title_what(what))
  # Captured once, before the loop: dev.cur() changes the moment the
  # first dev.new() runs, so re-checking it every iteration would only
  # ever open a second screen and then stop.
  had_device <- grDevices::dev.cur() > 1
  for (grp in groups) {
    op <- graphics::par(no.readonly = TRUE); on.exit(graphics::par(op), add = TRUE)
    if (length(groups) > 1 && !had_device) grDevices::dev.new()
    if (!identical(grp, "__nofacet__") && length(grp) > 1) {
      nc <- ceiling(sqrt(length(grp))); nr <- ceiling(length(grp) / nc)
      graphics::par(mfrow = c(nr, nc), mar = c(min(bmar, 7), lmar, 2, 1),
                    oma = c(0, 0, 3, 13))
      for (fv in grp)
        draw_one(d[as.character(d[[facet_var]]) == fv, , drop = FALSE], fv)
      graphics::mtext(main_txt, outer = TRUE, side = 3, cex = 1.05, font = 2, line = 1)
      if (!is.null(facet_var))
        graphics::mtext(paste(facet_var, "=", paste(grp, collapse = ", ")),
                        outer = TRUE, side = 3, cex = 0.8, line = -0.2, col = "grey30")
    } else if (!identical(grp, "__nofacet__") && length(grp) == 1) {
      graphics::par(mar = c(bmar, lmar, 3, 13))
      draw_one(d[as.character(d[[facet_var]]) == grp, , drop = FALSE],
               paste0(main_txt, " (", facet_var, " = ", grp, ")"))
    } else {
      graphics::par(mar = c(bmar, lmar, 3, 13))
      draw_one(d, main_txt)
    }
    legend_status_base(lk_all)
    graphics::par(op)
  }
  invisible(NULL)
}

#' Draw a status legend in the current figure's right margin
#'
#' Shared by the base-R heatmap and bubble plots: every possible status
#' from [cat_lookup()], not just the ones present in this particular
#' figure - drawn in the outer margin so it does not have to compete
#' for space with the plotting area itself. Needs `oma`/`mar` room
#' already reserved on the right (both callers set one).
#'
#' @param lk_all A [cat_lookup()] data frame.
#' @return `NULL`, invisibly.
#' @keywords internal
#' @export
legend_status_base <- function(lk_all) {
  graphics::par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),
               new = TRUE)
  graphics::plot.new()
  # The compact codes from cat_lookup()$abbr (cov / trip / meas vs their
  # thresholds) rather than the full sentences - short enough to read at
  # a legible size instead of being shrunk to fit.
  labs <- if ("abbr" %in% names(lk_all)) lk_all$abbr else lk_all$status
  graphics::legend("right", legend = labs, fill = lk_all$colour, cex = 0.8,
                   bty = "o", bg = "white", box.col = "grey80", title = "Status",
                   xpd = NA, inset = c(0.001, 0), y.intersp = 1.1)
  invisible(NULL)
}

#' Split a vector of facet values into groups, N per screen
#'
#' Used by every base-R plot's faceting: `NULL` (the default) keeps
#' everything on one screen, matching every plotting call before this
#' existed. A number splits the facets into that many per group, each
#' group meant for its OWN figure/device - the caller loops over the
#' groups and draws one figure per group.
#'
#' @param facets Character vector of facet values, or `NA` (no facet).
#' @param n_per_screen `NULL` (one screen) or a positive integer.
#' @return A list of character vectors (each a group of facet values),
#'   or `list("__nofacet__")` when `facets` is `NA`.
#' @keywords internal
#' @export
split_facets_to_screens <- function(facets, n_per_screen) {
  if (length(facets) == 1 && is.na(facets)) return(list("__nofacet__"))
  if (is.null(n_per_screen) || n_per_screen >= length(facets)) return(list(facets))
  split(facets, ceiling(seq_along(facets) / n_per_screen))
}

#' Light panel guide lines for the base-R plots
#'
#' The reading aid ggplot2's `panel.grid` gives for free: a faint grey
#' line at each supplied x and y position, spanning the current plot
#' region. Call it right after the plot region is set up and before the
#' data marks, so the marks sit on top of the lines, not under them.
#'
#' @param v,h x / y positions for the vertical / horizontal lines.
#'   Default: the axis tick positions. `NULL` or all-`NA` skips that set.
#' @param col,lwd Line colour and width.
#' @return `NULL`, invisibly.
#' @keywords internal
#' @export
grid_base <- function(v = graphics::axTicks(1), h = graphics::axTicks(2),
                      col = "grey92", lwd = 0.6) {
  u <- graphics::par("usr")
  v <- v[is.finite(v)]; h <- h[is.finite(h)]
  if (length(v)) graphics::segments(v, u[3], v, u[4], col = col, lwd = lwd)
  if (length(h)) graphics::segments(u[1], h, u[2], h, col = col, lwd = lwd)
  invisible(NULL)
}

#' Bottom-margin lines a rotated categorical x axis needs
#'
#' `graphics::axis()` only draws tick labels parallel or perpendicular to
#' the axis - there is no 45-degree setting - so [axis_x_cat()] draws them
#' by hand with `text(srt = )`, and the caller has to reserve
#' `par("mar")[1]` for them up front. This returns that reservation, in
#' margin lines, from the longest label, its `cex` and the angle.
#'
#' @param labels The category labels.
#' @param cex Their character expansion.
#' @param angle Rotation in degrees (0, 45 or 90).
#' @return A number of margin lines (never below 4).
#' @keywords internal
#' @export
cat_axis_mar <- function(labels, cex = 0.8, angle = 45) {
  n <- suppressWarnings(max(nchar(as.character(labels))))
  if (!is.finite(n)) n <- 0
  frac <- if (angle >= 90) 1 else if (angle >= 45) sin(angle * pi / 180) else 0.25
  max(4, ceiling(n * cex * 0.62 * frac) + 2.5)
}

#' Categorical x axis, optionally at 45 degrees
#'
#' Ticks plus labels on side 1. `angle` 0 or 90 defers to
#' `graphics::axis()`; any other angle (45 by default) is drawn with
#' `text(srt = )` just below the axis, right-justified onto the tick, and
#' spills into the bottom margin [cat_axis_mar()] was asked to reserve
#' (so `text()` needs `xpd = NA`, which it sets).
#'
#' @param at,labels Tick positions and their labels.
#' @param angle Rotation in degrees.
#' @param cex Character expansion.
#' @param col Label colour.
#' @return `NULL`, invisibly.
#' @keywords internal
#' @export
axis_x_cat <- function(at, labels, angle = 45, cex = 0.8, col = "grey15") {
  graphics::axis(1, at = at, labels = FALSE, tick = TRUE)
  if (angle %in% c(0, 90)) {
    graphics::axis(1, at = at, labels = labels, tick = FALSE, line = -0.4,
                   las = if (angle == 90) 2 else 1, cex.axis = cex)
    return(invisible(NULL))
  }
  u <- graphics::par("usr")
  graphics::text(x = at, y = u[3] - (u[4] - u[3]) * 0.018, labels = labels,
                 srt = angle, adj = c(1, 1), xpd = NA, cex = cex, col = col)
  invisible(NULL)
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
                         weight_label = "landings", min_meas = 0,
                         min_trips = 2) {
  d <- sc %>% filter(cat_key != "none")
  if (!nrow(d)) return(NULL)
  if (is.null(x_var) || !nzchar(x_var)) { d$.x <- "All"; x_var <- ".x" }
  # one bubble per cell, or several sit on the same point with their
  # labels overlapping
  d <- collapse_cells(d, c(y_var, x_var, facet_var), threshold, what, min_meas,
                      min_trips)
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
    trp_bit <- if ("numTrips" %in% names(d))
      ifelse(!is.na(d$numTrips) & d$numTrips < min_trips,
             paste0(round(d$numTrips), " trip",
                    ifelse(round(d$numTrips) == 1, "", "s")), "") else ""
    # Label only the cells a reader has to ACT on. Two exclusions:
    #   - the SAFE categories, which need no explanation;
    #   - "coverage, trips and measurements all fail" (cat_key "red"),
    #     whose numbers are usually 0% and n=0 and so say nothing the red
    #     colour has not already said. On a stock where most cells are
    #     unsampled that one category supplies most of the labels, and
    #     the crowd of "0%" buries the handful of borderline cells that
    #     actually matter.
    lab_me <- d$cat_key %in% c("yellowgreen", "yellow", "orangeyellow",
                               "orange_lowtrip", "orange_lowcov",
                               "orange_lowcovtrip")
    d$lab <- ifelse(lab_me,
                    paste0(cov_bit,
                           ifelse(nzchar(cov_bit) & nzchar(mea_bit), "\n", ""),
                           mea_bit,
                           ifelse(nzchar(trp_bit) &
                                    (nzchar(cov_bit) | nzchar(mea_bit)),
                                  "\n", ""),
                           trp_bit),
                    "")
  } else {
    trp_bit <- if ("numTrips" %in% names(d))
      ifelse(!is.na(d$numTrips) & d$numTrips < min_trips,
             paste0(round(d$numTrips), " trip",
                    ifelse(round(d$numTrips) == 1, "", "s")), "") else ""
    d$lab <- with(d, case_when(
      cat_key == "disc_only" ~ "100%\nDisc",
      cat_key %in% c("warn_lowcov", "warn_lowtrip") ~
        paste0(round(coverage, 1), "%",
               ifelse(nzchar(trp_bit), paste0("\n", trp_bit), "")),
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
    scale_fill_manual(values = status_palette(threshold, what, min_meas, min_trips),
                      limits = names(status_palette(threshold, what, min_meas,
                                                    min_trips)),
                      labels = status_abbr(threshold, what, min_meas, min_trips),
                      drop = FALSE, name = "Sampling status",
                      guide = guide_legend(override.aes = list(size = 5))) +
    scale_x_discrete(expand = expansion(0.2)) +
    maybe_facet(facet_var) +
    labs(title = sprintf("%s coverage vs %s", cov_title_what(what), weight_label),
         subtitle = if (any(d$n_strata > 1, na.rm = TRUE))
           sprintf("Each bubble aggregates every stratum at this grouping (up to %d per cell)",
                   max(d$n_strata, na.rm = TRUE)) else NULL,
         x = if (x_var == ".x") NULL else x_var, y = y_var) +
    app_theme() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  # key = "point": circles in the legend, like the bubbles in the panel.
  # size = SZ: a second key giving the weight each diameter stands for.
  attr(p, "status_legend") <- list(
    threshold = threshold, what = what, min_meas = min_meas,
    min_trips = min_trips, key = "point",
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

  gmax   <- max(d$landings, na.rm = TRUE)
  x_edge <- gmax * 1.05
  pct <- sum(d$landings[d$sampled == wi]) / sum(d$landings) * 100
  br <- d %>% group_by(.facet, sampled) %>%
    summarise(y_min = min(y_num) - 0.5, y_max = max(y_num) + 0.5,
              y_mid = (min(y_num) + max(y_num)) / 2,
              pctg = sum(landings) / first(facet_total) * 100, .groups = "drop") %>%
    mutate(x_pos = x_edge,
           disp = ifelse(sampled == wi, "Sampled", "Not sampled"))
  both <- intersect(d$.facet[d$sampled == wo], d$.facet[d$sampled == wi])
  sep <- if (length(both)) {
    d %>% filter(sampled == wo, .facet %in% both) %>%
      group_by(.facet) %>% summarise(sep_y = max(y_num) + 0.5, .groups = "drop")
  } else data.frame(.facet = character(0), sep_y = numeric(0))

  # Cumulative % of landings walking DOWN from the top row (largest
  # sampled stratum) to the bottom - a Lorenz-style staircase read
  # against the secondary top axis. Where it crosses the sampled /
  # not-sampled block boundary is the sampled fraction. Per facet, its
  # own total, mapped onto the shared weight x by the global max.
  stair <- d %>% arrange(.facet, desc(y_num)) %>%
    group_by(.facet) %>%
    mutate(cum = cumsum(landings) / first(facet_total) * 100,
           x_map = cum / 100 * gmax) %>%
    ungroup() %>% select(.facet, y_num, x_map)
  stair <- bind_rows(
    d %>% group_by(.facet) %>%
      summarise(y_num = max(y_num) + 0.5, x_map = 0, .groups = "drop"),
    stair) %>% arrange(.facet, desc(y_num))

  ggplot(d, aes(x = landings, y = .key, fill = .fill)) +
    geom_vline(xintercept = c(0.9, 0.95, 1) * gmax, colour = "grey85",
               linewidth = 0.3) +
    geom_col(width = 0.72, colour = OUTLINE) +
    geom_step(data = stair, aes(x = x_map, y = y_num), direction = "vh",
              inherit.aes = FALSE, colour = "grey20", linewidth = 0.5) +
    geom_segment(data = br, aes(x = x_pos, xend = x_pos, y = y_min, yend = y_max),
                 inherit.aes = FALSE, linewidth = 0.6, colour = "grey30") +
    geom_text(data = br, aes(x = x_pos * 1.02, y = y_mid,
                             label = paste0(disp, "\n(", round(pctg, 0), "%)")),
              inherit.aes = FALSE, hjust = 0, size = 3, fontface = "bold",
              colour = OUTLINE) +
    geom_segment(data = sep, aes(x = 0, xend = x_edge, y = sep_y, yend = sep_y),
                 inherit.aes = FALSE, linetype = "dashed", colour = "grey50") +
    scale_y_discrete(labels = setNames(d$.lab, as.character(d$.key))) +
    scale_x_continuous(labels = scales::comma,
                       expand = expansion(mult = c(0, 0.34)),
                       sec.axis = sec_axis(~ . / gmax * 100,
                                           name = "Cumulative % of landings",
                                           breaks = c(0, 25, 50, 75, 100))) +
    scale_fill_var(if (has(fill_var)) fill_var else y_var, d$.fill,
                   name = if (has(fill_var)) fill_var else y_var) +
    (if (has(facet_var)) facet_wrap(vars(.facet), scales = "free_y", ncol = ncol)
     else NULL) +
    labs(title = sprintf("%s sampled vs not sampled for %s",
                         tools::toTitleCase(weight_label), samp_word(what)),
         subtitle = sprintf("%s%% of %s weight sampled for %s; staircase = cumulative %% of landings",
                            round(pct, 0), weight_label, samp_word(what)),
         x = sprintf("%s (t)", tools::toTitleCase(weight_label)), y = NULL,
         fill = if (has(fill_var)) fill_var else y_var) +
    coord_cartesian(clip = "off") + app_theme() +
    theme(plot.margin = margin(10, 120, 10, 10))
}

#' Sampled-vs-unsampled weight bars, drawn with base R graphics
#'
#' Same underlying split as [plot_coverage_bars()] - each stratum's
#' weight divided into the part that carries a sampling estimate and
#' the part that does not - drawn as a stacked horizontal bar per
#' `y_var` category with `graphics::barplot()` instead of ggplot2.
#' Bars are grouped by `fill_var` within each `y_var` category when
#' `fill_var` is given, rather than the ggplot version's sorted
#' two-block layout with separator lines and percentage callouts -
#' the same two numbers (sampled weight, unsampled weight) are all
#' still there, shown a plainer way. Called through
#' `plot_coverage(..., engine = "base")`, not directly.
#'
#' @inheritParams plot_coverage_bars
#' @return `NULL`, invisibly - drawn on the current graphics device.
#' @seealso [plot_coverage_bars()], [plot_coverage()]
#' @keywords internal
#' @export
plot_coverage_bars_base <- function(sc, y_var, fill_var = NULL,
                                    facet_var = NULL, what = "discard",
                                    weight_label = "landings",
                                    palette = c("default", "colorblind"),
                                    n_per_screen = NULL,
                                    show_fill_in_labels = FALSE) {
  palette <- match.arg(palette)
  d <- sc %>% filter(cat_key != "none", landings > 0)
  if (!nrow(d)) { message("Nothing to plot."); return(invisible(NULL)) }
  has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
  d$.lab   <- as.character(d[[y_var]])
  d$.facet <- if (has(facet_var)) as.character(d[[facet_var]]) else ""
  d$.fill  <- if (has(fill_var))  as.character(d[[fill_var]])  else d$.lab

  wi <- paste("With", samp_word(what), "sampling")
  wo <- paste("Without", samp_word(what), "sampling")
  # Coloured by the FILL variable (Country, say), not by sampled status -
  # matching what the ggplot engine actually draws. The with/without
  # split is shown by splitting each (y, fill) pair into two ROWS, one
  # per group, sorted so every "with sampling" row comes before every
  # "without sampling" row within a facet - the same two-block layout
  # plot_coverage_bars() draws, just without its exact separator line
  # and percentage callouts.
  agg <- d %>%
    group_by(.facet, .lab, .fill) %>%
    summarise(samp = sum(landings_with_discards, na.rm = TRUE),
             unsamp = sum(landings, na.rm = TRUE) -
                      sum(landings_with_discards, na.rm = TRUE),
             .groups = "drop") %>%
    tidyr::pivot_longer(c(samp, unsamp), names_to = ".part",
                        values_to = "landings") %>%
    filter(landings > 1e-9) %>%
    mutate(sampled = factor(ifelse(.part == "samp", wi, wo), levels = c(wi, wo))) %>%
    arrange(.facet, sampled, dplyr::desc(landings))

  pal <- var_palette(if (has(fill_var)) fill_var else y_var, agg$.fill, palette)

  draw_one <- function(dd, main, subtitle, compact = FALSE) {
    key <- if (has(fill_var) && isTRUE(show_fill_in_labels))
      paste(dd$.lab, dd$.fill, sep = " / ") else dd$.lab
    # barplot() draws bottom-to-top, so the row vector is reversed here -
    # otherwise the first row (the largest "with sampling" bar) would be
    # drawn at the BOTTOM of the figure, the opposite of every other
    # plot in this package.
    ord   <- rev(seq_len(nrow(dd)))
    total <- sum(dd$landings)
    dmax  <- max(dd$landings)
    # Left margin sized to the y labels actually present, not a generous
    # constant. Right margin sized to the "With / Without ... sampling"
    # callouts that sit past the bars - the ggplot version reserves
    # plot.margin on the right for exactly the same text. In `compact`
    # mode (a par(mfrow=) facet grid, where each panel is a fraction of
    # the device) the callouts and legend are dropped, so the right
    # margin shrinks back to a hairline - a label-sized margin on a
    # quarter-width panel is what "figure margins too large" is.
    lmar <- max(4, max(nchar(key)) * 0.46)
    rmar <- if (compact) 2 else 11
    if (compact) lmar <- min(lmar, 8)
    tmar <- if (compact) 2.5 else 7
    op <- graphics::par(mar = c(4, lmar, tmar, rmar))
    on.exit(graphics::par(op), add = TRUE)
    # The plot region runs well past the longest bar: the cumulative-%
    # staircase tops out at 100% == max(landings), the block callouts
    # sit past that, and the whole right third is read against the
    # secondary top axis, so it is deliberately generous here.
    dmax <- max(dd$landings)
    xmax <- dmax * 1.30
    mx   <- function(p) p / 100 * dmax          # % of landings -> x
    bp <- graphics::barplot(dd$landings[ord], horiz = TRUE, las = 1,
                            names.arg = key[ord], cex.names = 0.7,
                            col = pal[dd$.fill[ord]], border = OUTLINE,
                            xlim = c(0, xmax), space = 0.15,
                            xlab = sprintf("%s (t)", tools::toTitleCase(weight_label)),
                            main = "")
    pitch <- if (nrow(bp) > 1) bp[2, 1] - bp[1, 1] else 1.2
    y0 <- bp[1, 1] - pitch / 2
    y1 <- bp[nrow(bp), 1] + pitch / 2
    # Guide lines behind the bars: x ticks full height, one per row full
    # width, plus faint 90 / 95 / 100 % markers for the staircase.
    xticks <- graphics::axTicks(1)
    graphics::segments(xticks, y0, xticks, y1, col = "grey88", lwd = 0.7)
    graphics::segments(0, bp, xmax, bp, col = "grey92", lwd = 0.5)
    if (!compact)
      graphics::segments(mx(c(90, 95, 100)), y0, mx(c(90, 95, 100)), y1,
                         col = "grey85", lwd = 0.6)
    graphics::barplot(dd$landings[ord], horiz = TRUE, las = 1, add = TRUE,
                      names.arg = NA, axes = FALSE, space = 0.15,
                      col = pal[dd$.fill[ord]], border = OUTLINE)
    graphics::box()
    graphics::axis(1, at = xticks, labels = FALSE, tick = TRUE)
    # Y-axis tick marks (barplot draws the names but no ticks).
    graphics::axis(2, at = bp, labels = FALSE, tick = TRUE)

    grp    <- as.character(dd$sampled[ord])
    blocks <- rle(grp)
    ends   <- cumsum(blocks$lengths)
    starts <- ends - blocks$lengths + 1L
    # Horizontal dashed separator between the sampled and not-sampled
    # blocks - the piece the base port was missing (ggplot keeps its
    # `sep` segment).
    if (length(blocks$lengths) > 1) {
      yb <- mean(c(bp[ends[1], 1], bp[ends[1] + 1L, 1]))
      graphics::segments(0, yb, xmax, yb, lty = 2, col = "grey50")
    }
    # Cumulative % of landings walking down from the top row, as a step
    # line read against the secondary top axis (0-100 %). Its crossing
    # of the block separator is the sampled fraction.
    if (!compact) {
      cumv <- cumsum(dd$landings) / total * 100     # dd row 1 = top bar
      y_by_row <- rev(bp[, 1])                      # dd row i -> this y
      graphics::lines(c(0, mx(cumv)), c(y1, y_by_row), type = "s",
                      col = "grey20", lwd = 0.9)
      graphics::axis(3, at = mx(c(0, 25, 50, 75, 100)),
                     labels = c("0", "25", "50", "75", "100"), cex.axis = 0.72,
                     mgp = c(3, 0.5, 0))
      graphics::mtext("Cumulative % of landings", side = 3, line = 1.8,
                      cex = 0.72, col = "grey30")
    }
    # Dashed vertical line where the bars' own weight ends and the
    # callout column begins.
    lx <- dmax * 1.06
    graphics::segments(lx, y0, lx, y1, lty = 2, col = "grey40")
    # "Sampled (NN%)" / "Not sampled (NN%)" beside that line, one per
    # contiguous block. Label read from the data (rle over the draw
    # order), not assumed to be sampled-on-top.
    if (!compact) {
      for (i in seq_along(blocks$values)) {
        lvl  <- blocks$values[i]
        disp <- if (identical(lvl, wo)) "Not sampled" else "Sampled"
        ymid <- mean(bp[starts[i]:ends[i], 1])
        pctb <- sum(dd$landings[dd$sampled == lvl]) / total * 100
        graphics::text(lx + dmax * 0.04, ymid,
                       paste0(disp, "\n(", round(pctb), "%)"),
                       adj = c(0, 0.5), cex = 0.72, font = 2, col = "grey30",
                       xpd = NA)
      }
    }
    graphics::mtext(main, side = 3, line = if (compact) 0.5 else 5.1,
                    cex = if (compact) 0.9 else 1.05, font = 2, adj = 0)
    if (!compact)
      graphics::mtext(subtitle, side = 3, line = 3.9, cex = 0.8, adj = 0,
                      col = "grey30")
    if (has(fill_var) && !compact) {
      lv <- sort(unique(dd$.fill))
      # Legend anchored to the bottom-right MARGIN corner, outside the
      # bars - the way the ggplot version puts its fill guide off to the
      # side. An in-panel "topright" (or any x inside the data) lands on
      # top of the bars once the panel is this densely packed.
      graphics::legend(x = xmax + dmax * 0.06, y = y0 + pitch,
                       xjust = 0, yjust = 0, legend = lv, fill = pal[lv],
                       cex = 0.6, bty = "n", xpd = NA,
                       ncol = min(2, ceiling(length(lv) / 8)))
    }
  }

  facets <- if (has(facet_var)) sort(unique(agg$.facet)) else NA
  pct <- sum(agg$landings[agg$sampled == wi]) / sum(agg$landings) * 100
  # Title / subtitle split to match the ggplot version's exactly: a
  # plain title, and a subtitle line stating the % and what it means -
  # not both crammed into one line.
  title_txt <- sprintf("%s sampled vs not sampled for %s",
                       tools::toTitleCase(weight_label), samp_word(what))
  subtitle_txt <- sprintf(
    "%s%% of %s weight sampled for %s; staircase = cumulative %% of landings",
    round(pct, 0), weight_label, samp_word(what))
  groups <- split_facets_to_screens(facets, n_per_screen)
  # Captured once, before the loop: dev.cur() itself changes the moment
  # the first dev.new() runs, so re-checking it each iteration would
  # only ever open a second screen and then stop, thinking a device
  # was already open (it was, of course - the one just opened).
  had_device <- grDevices::dev.cur() > 1
  for (grp in groups) {
    op <- graphics::par(no.readonly = TRUE); on.exit(graphics::par(op), add = TRUE)
    # A new screen per group when nothing was already open - a caller
    # writing to their own png()/pdf() keeps writing to that one
    # device, since dev.new() there would just add an unwanted extra
    # page. Without an explicit device, `n_per_screen` genuinely
    # produces several SEPARATE figures in the plot viewer, not several
    # panels squeezed onto one.
    if (length(groups) > 1 && !had_device) grDevices::dev.new()
    if (!identical(grp, "__nofacet__") && length(grp) > 1) {
      nc <- ceiling(sqrt(length(grp))); nr <- ceiling(length(grp) / nc)
      graphics::par(mfrow = c(nr, nc), oma = c(0, 0, 2, 0))
      for (fv in grp)
        draw_one(agg[agg$.facet == fv, , drop = FALSE], fv, "", compact = TRUE)
      if (!is.null(facet_var))
        graphics::mtext(paste0(title_txt, "  (", facet_var, " = ",
                               paste(grp, collapse = ", "), ")"),
                        outer = TRUE, cex = 0.85, font = 2, line = 0.3)
    } else if (!identical(grp, "__nofacet__") && length(grp) == 1) {
      draw_one(agg[agg$.facet == grp, , drop = FALSE],
               paste0(title_txt, " (", facet_var, " = ", grp, ")"), subtitle_txt)
    } else {
      draw_one(agg, title_txt, subtitle_txt)
    }
    graphics::par(op)
  }
  invisible(NULL)
}

#' Coverage bubble plot, drawn with base R graphics
#'
#' Same data and colour-by-status scheme as [plot_bubbles()] - status in
#' the fill colour, weight in the circle's area - drawn with
#' `graphics::symbols(circles = )` instead of ggplot2's size-mapped
#' points. `disc_only` cells still get a cross mark; warning-status
#' cells still get a coverage/measurement label, placed just above the
#' bubble rather than steered clear of neighbours the way
#' `ggrepel::geom_text_repel()` does in the ggplot version - the one
#' piece of the original this port does not reproduce, since base R has
#' no built-in overlap-avoidance layout. Called through
#' `plot_coverage(..., engine = "base")`, not directly.
#'
#' @inheritParams plot_bubbles
#' @return `NULL`, invisibly - drawn on the current graphics device.
#' @seealso [plot_bubbles()], [plot_coverage()]
#' @keywords internal
#' @export
plot_bubbles_base <- function(sc, y_var, x_var = NULL, facet_var = NULL,
                              threshold = 50, what = "discard",
                              weight_label = "landings", min_meas = 0,
                              min_trips = 2, palette = c("default", "colorblind"),
                              angle = 45, n_per_screen = NULL) {
  palette <- match.arg(palette)
  d <- sc %>% filter(cat_key != "none")
  if (!nrow(d)) { message("Nothing to plot."); return(invisible(NULL)) }
  if (is.null(x_var) || !nzchar(x_var)) { d$.x <- "All"; x_var <- ".x" }
  d <- collapse_cells(d, c(y_var, x_var, facet_var), threshold, what,
                      min_meas, min_trips)
  smallest <- suppressWarnings(min(d$landings[d$landings > 0], na.rm = TRUE))
  if (!is.finite(smallest)) smallest <- 1
  d$plot_size <- ifelse(d$cat_key == "disc_only", smallest, d$landings)
  pal <- status_palette(threshold, what, min_meas, min_trips)

  trp_bit <- if ("numTrips" %in% names(d))
    ifelse(!is.na(d$numTrips) & d$numTrips < min_trips,
           paste0(round(d$numTrips), " trip",
                  ifelse(round(d$numTrips) == 1, "", "s")), "") else ""
  if (what %in% c("length", "age")) {
    cov_bit <- ifelse(!is.na(d$coverage) & d$coverage < threshold,
                      paste0(round(d$coverage, 1), "%"), "")
    mea_bit <- ifelse(!is.na(d$n_measurements) & d$n_measurements < min_meas,
                      paste0("n=", format(round(d$n_measurements),
                                          big.mark = ",", trim = TRUE)), "")
    lab_me <- d$cat_key %in% c("yellowgreen", "yellow", "orangeyellow",
                               "orange_lowtrip", "orange_lowcov",
                               "orange_lowcovtrip")
    d$lab <- ifelse(lab_me,
                    paste0(cov_bit, ifelse(nzchar(cov_bit) & nzchar(mea_bit),
                                           " ", ""), mea_bit,
                           ifelse(nzchar(trp_bit) &
                                    (nzchar(cov_bit) | nzchar(mea_bit)),
                                  "\n", ""), trp_bit), "")
  } else {
    d$lab <- dplyr::case_when(
      d$cat_key == "disc_only" ~ "100% Disc",
      d$cat_key %in% c("warn_lowcov", "warn_lowtrip") ~
        paste0(round(d$coverage, 1), "%",
               ifelse(nzchar(trp_bit), paste0("\n", trp_bit), "")),
      TRUE ~ "")
  }

  sz_vals <- d$plot_size[is.finite(d$plot_size) & d$plot_size > 0]
  facets <- if (!is.null(facet_var) && facet_var %in% names(d))
    sort(unique(as.character(d[[facet_var]]))) else NA
  # symbols(circles=) takes a RADIUS in the axis's own USER units here
  # (xlim/ylim run 1 unit per category), converted through par("pin")
  # (the panel's plotting region, in inches) and par("usr") so the
  # LARGEST circle never exceeds a fixed fraction of one grid cell -
  # ggplot's point-size legend is independent of how many rows or
  # panels share the figure, but a physical-inch radius here is not:
  # unless it is worked out from the panel's own dimensions AFTER
  # par(mfrow=) is set, the same "big enough for one panel" radius
  # buries 26 fleet categories crammed into a five-panel facet grid
  # under solid, overlapping colour.
  rad_of <- function(v, cell_rad) {
    if (!length(sz_vals) || diff(range(sz_vals)) < 1e-9)
      return(rep(cell_rad * 0.75, length(v)))
    cell_rad * (0.28 + 0.72 * (v - min(sz_vals)) / diff(range(sz_vals)))
  }

  draw_one <- function(dd, main) {
    xs <- sort(unique(as.character(dd[[x_var]])))
    ys <- sort(unique(as.character(dd[[y_var]])))
    dd$.xi <- match(as.character(dd[[x_var]]), xs)
    dd$.yi <- match(as.character(dd[[y_var]]), ys)
    fill <- pal[as.character(dd$Status)] %||% "#CCCCCC"
    graphics::plot(NA, xlim = c(0.4, length(xs) + 0.6),
                   ylim = c(0.4, length(ys) + 0.6), axes = FALSE,
                   xlab = "", ylab = "", main = main)
    grid_base(v = seq_along(xs), h = seq_along(ys))
    # One grid cell is 1 x-unit by 1 y-unit here; symbols(inches=FALSE)
    # takes its radius in THOSE user units directly, so a cell-relative
    # radius holds regardless of how many rows/columns, panels, or what
    # physical size the device ends up being - the actual source of the
    # earlier overlap, not the facet count on its own.
    cell_rad <- 0.42
    graphics::symbols(dd$.xi, dd$.yi, circles = rad_of(dd$plot_size, cell_rad),
                      inches = FALSE, bg = fill, fg = OUTLINE, add = TRUE)
    xm <- dd$cat_key == "disc_only"
    if (any(xm))
      graphics::points(dd$.xi[xm], dd$.yi[xm], pch = 4, col = OUTLINE,
                       cex = 0.9, lwd = 1.2)
    haslab <- nzchar(dd$lab)
    if (any(haslab)) {
      # A label placed just above the bubble's own radius still sits ON
      # the bubble for a large one - the radius that pushes the label
      # clear of a SMALL bubble is nowhere near enough for a big one. A
      # fixed nudge instead (independent of the bubble's own size), with
      # a thin leader line back to the bubble it belongs to - the same
      # idea ggrepel uses for the ggplot version, without the full
      # collision-avoidance layout base R has no equivalent for.
      lx <- dd$.xi[haslab] + 0.28
      ly <- dd$.yi[haslab] + rad_of(dd$plot_size[haslab], cell_rad) + 0.3
      graphics::segments(dd$.xi[haslab], dd$.yi[haslab] +
                           rad_of(dd$plot_size[haslab], cell_rad),
                         lx, ly - 0.06, col = "grey40", lwd = 0.5)
      graphics::text(lx, ly, dd$lab[haslab], cex = 0.55, font = 2,
                     col = "red3", adj = c(0.5, 0))
    }
    axis_x_cat(seq_along(xs), xs, angle = angle, cex = 0.8)
    graphics::axis(2, at = seq_along(ys), labels = ys, las = 1, cex.axis = 0.8,
                   tick = TRUE)
    graphics::box()
  }

  lmar <- max(6, max(nchar(as.character(d[[y_var]]))) * 0.42)
  bmar <- cat_axis_mar(unique(as.character(d[[x_var]])), 0.8, angle)
  main_txt <- sprintf("%s coverage vs %s", cov_title_what(what), weight_label)
  lk_all <- cat_lookup(threshold, what, min_meas, min_trips)
  draw_legends <- function() {
    legend_status_base(lk_all)
    if (length(sz_vals) && diff(range(sz_vals)) > 1e-9) {
      ref <- signif(c(min(sz_vals), stats::median(sz_vals), max(sz_vals)), 2)
      graphics::par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),
                   new = TRUE)
      graphics::plot.new()
      graphics::legend("bottomright", title = sprintf("%s (t)", weight_label),
                       legend = format(ref, big.mark = ","),
                       pch = 21, pt.cex = c(0.9, 1.4, 2), pt.bg = "grey70",
                       col = OUTLINE, cex = 0.6, bty = "o", bg = "white",
                       box.col = "grey80", xpd = NA, inset = c(0.001, 0))
    }
  }

  groups <- split_facets_to_screens(facets, n_per_screen)
  had_device <- grDevices::dev.cur() > 1
  for (grp in groups) {
    op <- graphics::par(no.readonly = TRUE); on.exit(graphics::par(op), add = TRUE)
    if (length(groups) > 1 && !had_device) grDevices::dev.new()
    if (!identical(grp, "__nofacet__") && length(grp) > 1) {
      nc <- ceiling(sqrt(length(grp))); nr <- ceiling(length(grp) / nc)
      graphics::par(mfrow = c(nr, nc), mar = c(min(bmar, 7), lmar, 2, 3),
                    oma = c(0, 0, 3, 13))
      for (fv in grp)
        draw_one(d[as.character(d[[facet_var]]) == fv, , drop = FALSE], fv)
      graphics::mtext(main_txt, outer = TRUE, side = 3, cex = 1.05, font = 2, line = 1)
      if (!is.null(facet_var))
        graphics::mtext(paste(facet_var, "=", paste(grp, collapse = ", ")),
                        outer = TRUE, side = 3, cex = 0.8, line = -0.2, col = "grey30")
    } else if (!identical(grp, "__nofacet__") && length(grp) == 1) {
      graphics::par(mar = c(bmar, lmar, 4, 13))
      draw_one(d[as.character(d[[facet_var]]) == grp, , drop = FALSE],
               paste0(main_txt, " (", facet_var, " = ", grp, ")"))
    } else {
      graphics::par(mar = c(bmar, lmar, 4, 13))
      draw_one(d, "")
      graphics::mtext(main_txt, side = 3, line = 1.2, cex = 1.05, font = 2)
    }
    draw_legends()
    graphics::par(op)
  }
  invisible(NULL)
}

# ----------------------------------------------------------
#  Data-availability scatter (before any raising)
# ----------------------------------------------------------

#' Axis label for a coverage-scatter metric
#'
#' Maps the `y` / `size` column name used by [plot_coverage_scatter()] to
#' a readable axis / legend label; `what` tunes the coverage wording.
#'
#' @param metric Column name ("coverage", "n_measurements", "numTrips",
#'   "discard_rate", or anything else - returned unchanged).
#' @param what "discard", "length" or "age".
#' @return A single string.
#' @keywords internal
#' @export
scatter_axis_label <- function(metric, what = "discard") {
  cov_txt <- if (what %in% c("length", "age"))
    sprintf("%% of catch with a %s", if (what == "age") "age" else "length")
  else "% of landings sampled"
  switch(metric,
         coverage       = cov_txt,
         n_measurements = "Fish measured (n)",
         numTrips       = "Sampled trips (n)",
         discard_rate   = "Discard rate",
         metric)
}

#' Data-availability scatter: sampling vs weight per stratum
#'
#' One point per stratum: its landed/caught weight on a log x axis, a
#' chosen sampling metric on y (coverage %, measurements, trips, ...),
#' the point sized by a second metric (trips by default, measurements
#' for biology), coloured by sampling status. A dashed line marks the
#' relevant threshold. Only the heaviest not-yet-safe strata are
#' labelled, so the picture stays readable with many strata - these are
#' the ones worth grouping or chasing a sample for.
#'
#' It answers "where is the catch, and where is the sampling" in one
#' view, from the data alone - before any donor pool or raising is
#' decided. A lower-level builder behind [plot_coverage()]
#' (`type = "scatter"`); call it directly for full control of `x` / `y`
#' / `size`.
#'
#' @param sc Classified stratum table (or anything with the `x`/`y`
#'   columns and, optionally, `Status`).
#' @param x,y,size Column names: weight on x (log), the sampling metric
#'   on y, a second metric for point size. Defaults `"landings"`,
#'   `"coverage"`, `"numTrips"`.
#' @param facet_var Optional facet column.
#' @param threshold,min_meas,min_trips Bars the status colours and the
#'   dashed reference line are drawn against.
#' @param what "discard", "length" or "age" - only tunes wording.
#' @param weight_label Word for the weight on x.
#' @param label_n How many strata to label (heaviest non-safe first).
#' @param palette Passed through for signature symmetry; status colours
#'   are fixed.
#' @return A ggplot object, or `NULL` when there is nothing to draw.
#' @seealso [plot_coverage()]
#' @family coverage plots
#' @export
plot_coverage_scatter <- function(sc, x = "landings", y = "coverage",
                                  size = "numTrips", facet_var = NULL,
                                  threshold = 50, what = "discard",
                                  weight_label = "landings", min_meas = 0,
                                  min_trips = 2, label_n = 8,
                                  palette = c("default", "colorblind")) {
  palette <- match.arg(palette)
  has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
  miss <- setdiff(c(x, y), names(sc))
  if (length(miss))
    stop("plot_coverage_scatter(): column(s) not in the table: ",
         paste(miss, collapse = ", "))
  d <- sc %>% filter(is.finite(.data[[x]]), .data[[x]] > 0, is.finite(.data[[y]]))
  if (!nrow(d)) return(NULL)
  d$.x <- d[[x]]; d$.y <- d[[y]]
  d$.sz <- if (size %in% names(d)) suppressWarnings(as.numeric(d[[size]])) else NA_real_
  if (all(is.na(d$.sz))) d$.sz <- 1
  # label with just the first key (fleet / metier), not the full
  # "fleet | country | season" id - it has to sit next to a point.
  d$.lab <- if ("stratum_id" %in% names(d))
    sub(" \\| .*$", "", as.character(d$stratum_id)) else as.character(seq_len(nrow(d)))
  fillable <- "Status" %in% names(d)

  lab_d <- d
  if (fillable) lab_d <- lab_d %>% filter(!grepl("^SAFE", Status))
  # one label per fleet/metier name (its heaviest stratum), then the
  # `label_n` heaviest of those - so a dozen near-identical strata do
  # not all get the same tag stacked on each other.
  lab_d <- lab_d %>% arrange(desc(.x)) %>% filter(!duplicated(.lab)) %>%
    slice_max(.x, n = label_n, with_ties = FALSE)

  ylab  <- scatter_axis_label(y, what)
  szlab <- scatter_axis_label(size, what)
  p <- ggplot(d, aes(.x, .y))
  if (identical(y, "coverage"))
    p <- p + geom_hline(yintercept = threshold, linetype = "dashed",
                        colour = "grey50")
  if (identical(y, "n_measurements") && min_meas > 0)
    p <- p + geom_hline(yintercept = min_meas, linetype = "dashed",
                        colour = "grey50")
  p <- p +
    (if (fillable)
       geom_point(aes(fill = Status, size = .sz), shape = 21, colour = OUTLINE,
                  alpha = 0.9, stroke = 0.6)
     else geom_point(aes(size = .sz), shape = 21, fill = PAL_BASE[1],
                     colour = OUTLINE, alpha = 0.9)) +
    ggrepel::geom_text_repel(data = lab_d, aes(label = .lab), size = 2.5,
                             colour = "grey20", box.padding = 0.5,
                             min.segment.length = 0, segment.size = 0.2,
                             segment.colour = "grey55", max.overlaps = Inf) +
    scale_x_log10(labels = scales::comma) +
    scale_size_area(name = szlab, max_size = 9) +
    maybe_facet(facet_var) +
    labs(title = sprintf("Sampling vs %s weight per stratum",
                         tools::toTitleCase(weight_label)),
         subtitle = sprintf("x = %s (log) | y = %s | size = %s | %d labelled",
                            weight_label, ylab, szlab, nrow(lab_d)),
         x = sprintf("%s (t, log scale)", tools::toTitleCase(weight_label)),
         y = ylab) +
    app_theme()
  if (fillable)
    p <- p + scale_fill_manual(
      values = status_palette(threshold, what, min_meas, min_trips),
      limits = names(status_palette(threshold, what, min_meas, min_trips)),
      labels = status_abbr(threshold, what, min_meas, min_trips),
      drop = FALSE, name = "Sampling status",
      guide = guide_legend(override.aes = list(size = 4)))
  p
}

#' Data-availability scatter, drawn with base R graphics
#'
#' Base-R port of [plot_coverage_scatter()] - same log-x weight axis,
#' status colours, threshold line and sparse labelling, drawn with
#' `graphics::plot()`/`points()`. Called through
#' `plot_coverage(..., type = "scatter", engine = "base")`.
#'
#' @inheritParams plot_coverage_scatter
#' @param angle,n_per_screen As in the other base-R coverage plots.
#' @return `NULL`, invisibly - drawn on the current graphics device.
#' @seealso [plot_coverage_scatter()], [plot_coverage()]
#' @keywords internal
#' @export
plot_coverage_scatter_base <- function(sc, x = "landings", y = "coverage",
                                       size = "numTrips", facet_var = NULL,
                                       threshold = 50, what = "discard",
                                       weight_label = "landings", min_meas = 0,
                                       min_trips = 2, label_n = 8,
                                       palette = c("default", "colorblind"),
                                       angle = 45, n_per_screen = NULL) {
  palette <- match.arg(palette)
  has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
  miss <- setdiff(c(x, y), names(sc))
  if (length(miss))
    stop("plot_coverage_scatter_base(): column(s) not in the table: ",
         paste(miss, collapse = ", "))
  d <- sc[is.finite(sc[[x]]) & sc[[x]] > 0 & is.finite(sc[[y]]), , drop = FALSE]
  if (!nrow(d)) { message("Nothing to plot."); return(invisible(NULL)) }
  d$.x <- d[[x]]; d$.y <- d[[y]]
  d$.sz <- if (size %in% names(d)) suppressWarnings(as.numeric(d[[size]])) else NA_real_
  d$.lab <- if ("stratum_id" %in% names(d))
    sub(" \\| .*$", "", as.character(d$stratum_id)) else as.character(seq_len(nrow(d)))
  d$.facet <- if (has(facet_var)) as.character(d[[facet_var]]) else NA
  has_stat <- "Status" %in% names(d)
  pal <- if (has_stat) status_palette(threshold, what, min_meas, min_trips) else NULL
  lk  <- if (has_stat) cat_lookup(threshold, what, min_meas, min_trips) else NULL
  ylab <- scatter_axis_label(y, what)

  rad <- function(v) {
    v[!is.finite(v) | v <= 0] <- NA
    if (all(is.na(v))) return(rep(1.1, length(v)))
    r <- sqrt(v); r <- r / max(r, na.rm = TRUE)
    r[is.na(r)] <- min(r, na.rm = TRUE) * 0.4
    0.7 + r * 2.0
  }
  draw_one <- function(dd, main) {
    yl <- range(c(if (identical(y, "coverage")) c(0, 100), dd$.y), na.rm = TRUE)
    graphics::plot(dd$.x, dd$.y, log = "x", type = "n", axes = FALSE,
                   xlab = "", ylab = ylab, main = main, ylim = yl)
    grid_base()
    if (identical(y, "coverage"))
      graphics::abline(h = threshold, lty = 2, col = "grey50")
    if (identical(y, "n_measurements") && min_meas > 0)
      graphics::abline(h = min_meas, lty = 2, col = "grey50")
    cols <- if (has_stat) pal[as.character(dd$Status)] else rep(PAL_BASE[1], nrow(dd))
    cols[is.na(cols)] <- "#CCCCCC"
    graphics::points(dd$.x, dd$.y, pch = 21, bg = cols, col = OUTLINE,
                     cex = rad(dd$.sz), lwd = 0.7)
    lab <- dd
    if (has_stat) lab <- lab[!grepl("^SAFE", lab$Status), , drop = FALSE]
    lab <- lab[order(-lab$.x), , drop = FALSE]
    lab <- utils::head(lab[!duplicated(lab$.lab), , drop = FALSE], min(label_n, 6L))
    if (nrow(lab)) {
      ymid <- mean(range(dd$.y, na.rm = TRUE))
      graphics::text(lab$.x, lab$.y, lab$.lab,
                     pos = ifelse(lab$.y > ymid, 1, 3), offset = 0.6,
                     cex = 0.5, col = "grey20", xpd = NA)
    }
    graphics::box(); graphics::axis(1); graphics::axis(2, las = 1)
  }
  draw_legend <- function() {
    if (!has_stat) return(invisible(NULL))
    graphics::par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),
                 new = TRUE)
    graphics::plot.new()
    graphics::legend("right", legend = lk$abbr, fill = lk$colour, cex = 0.8,
                     bty = "o", bg = "white", box.col = "grey80",
                     title = "Status", xpd = NA, y.intersp = 1.1,
                     inset = c(0.001, 0))
  }
  facets <- if (has(facet_var)) sort(unique(d$.facet)) else NA
  main_txt <- sprintf("Sampling vs %s weight per stratum",
                      tools::toTitleCase(weight_label))
  xlab <- sprintf("%s (t, log scale)", tools::toTitleCase(weight_label))
  groups <- split_facets_to_screens(facets, n_per_screen)
  had_device <- grDevices::dev.cur() > 1
  for (grp in groups) {
    op <- graphics::par(no.readonly = TRUE); on.exit(graphics::par(op), add = TRUE)
    if (length(groups) > 1 && !had_device) grDevices::dev.new()
    rmar <- if (has_stat) 12 else 3
    if (!identical(grp, "__nofacet__") && length(grp) > 1) {
      nc <- ceiling(sqrt(length(grp))); nr <- ceiling(length(grp) / nc)
      graphics::par(mfrow = c(nr, nc), mar = c(4, 5, 3, 1), oma = c(2, 0, 3, rmar))
      for (fv in grp) draw_one(d[d$.facet == fv, , drop = FALSE], fv)
      graphics::mtext(main_txt, outer = TRUE, side = 3, cex = 1.05, font = 2, line = 1)
      graphics::mtext(xlab, outer = TRUE, side = 1, line = 0.4, cex = 0.85)
    } else if (!identical(grp, "__nofacet__") && length(grp) == 1) {
      graphics::par(mar = c(5, 5, 4, rmar))
      draw_one(d[d$.facet == grp, , drop = FALSE], paste0(main_txt, " (", grp, ")"))
      graphics::mtext(xlab, side = 1, line = 3, cex = 0.85)
    } else {
      graphics::par(mar = c(5, 5, 4, rmar))
      draw_one(d, main_txt)
      graphics::mtext(xlab, side = 1, line = 3, cex = 0.85)
    }
    draw_legend()
    graphics::par(op)
  }
  invisible(NULL)
}

# ----------------------------------------------------------
#  Coverage map (ICES-area pies)
# ----------------------------------------------------------

#' Approximate centroids of the common NE Atlantic ICES areas
#'
#' `lon` / `lat` for the ICES divisions and subdivisions that a stock
#' assessment in this region normally uses - enough to place one bubble
#' per area on a map. Codes are stored WITHOUT the leading `"27."`;
#' [ices_area_centroid()] accepts either form.
#'
#' @format A data frame with columns `area`, `lon`, `lat`.
#' @seealso [ices_area_centroid()], [plot_coverage_map()]
#' @keywords internal
#' @export
ICES_AREA_CENTROID <- data.frame(
  area = c("1", "2.a", "2.b",
           "3.a", "3.a.20", "3.a.21",
           "3.b.23", "3.c.22", "3.d.24", "3.d.25", "3.d.26", "3.d.27",
           "3.d.28", "3.d.29", "3.d.30", "3.d.31", "3.d.32",
           "4", "4.a", "4.b", "4.c",
           "5.a", "5.b",
           "6", "6.a", "6.b",
           "7", "7.a", "7.b", "7.c", "7.d", "7.e", "7.f", "7.g", "7.h",
           "7.j", "7.k",
           "8", "8.a", "8.b", "8.c", "8.d", "8.e",
           "9", "9.a", "9.b",
           "10.a", "12", "14.a", "14.b"),
  lon = c(20.0, 5.0, 0.0,
          10.5, 10.2, 11.6,
          10.7, 12.5, 12.0, 16.5, 19.0, 18.5,
          20.0, 20.5, 20.0, 23.0, 25.0,
          2.5, 1.0, 2.5, 3.2,
          -19.0, -7.0,
          -8.5, -8.0, -13.0,
          -8.0, -5.3, -10.5, -14.0, 0.5, -4.0, -5.2, -7.7, -7.7,
          -11.0, -14.0,
          -5.0, -3.0, -2.5, -4.2, -6.0, -10.0,
          -10.0, -9.5, -11.0,
          -28.0, -10.0, -35.0, -25.0),
  lat = c(73.0, 68.0, 64.0,
          57.5, 58.2, 56.9,
          55.2, 55.5, 55.0, 55.6, 55.6, 58.0,
          57.5, 59.5, 62.0, 64.5, 60.0,
          56.5, 58.6, 55.6, 53.6,
          64.0, 62.0,
          57.5, 57.5, 57.5,
          51.0, 53.6, 53.6, 52.0, 50.2, 49.8, 51.3, 51.0, 49.6,
          50.6, 49.6,
          45.5, 46.4, 45.0, 44.0, 45.6, 45.5,
          39.0, 40.0, 38.0,
          46.0, 74.0, 60.0, 62.0),
  stringsAsFactors = FALSE)

#' Look up ICES-area centroids
#'
#' @param codes ICES area codes, with or without the `"27."` prefix.
#'   Anything past a recognised division/subdivision is ignored.
#' @return A data frame `area`, `lon`, `lat` aligned to `codes`; `lon` /
#'   `lat` are `NA` for a code with no centroid (a warning names them).
#' @seealso [ICES_AREA_CENTROID], [plot_coverage_map()]
#' @keywords internal
#' @export
ices_area_centroid <- function(codes) {
  key <- sub("^27\\.", "", trimws(as.character(codes)))
  m <- ICES_AREA_CENTROID[match(key, ICES_AREA_CENTROID$area), c("lon", "lat")]
  bad <- unique(key[is.na(m$lon) & nzchar(key)])
  if (length(bad))
    warning("no centroid for ICES area(s): ", paste(bad, collapse = ", "),
            " - dropped from the map", call. = FALSE)
  data.frame(area = as.character(codes), lon = m$lon, lat = m$lat,
             stringsAsFactors = FALSE)
}

#' Coverage map: one pie per ICES area (optionally per group level)
#'
#' A map of the fishing area with one small pie per ICES division: pie
#' radius is the area's landed/caught weight, and the pie is split into
#' the sampled and the not-sampled share of that weight. With
#' `group_var` (e.g. `"Season"`, `"Country"`, `"metier4"`) each area
#' gets a labelled box holding a tidy grid of pies, one per group level;
#' overlapping boxes are nudged apart and a thin leader keeps each box
#' tied to its area's true centroid.
#'
#' Auto-builds from raw catch data (and `distributions` for a biology
#' `what`) exactly like [plot_coverage()]; an already-classified table is
#' used as-is. Needs the suggested `maps` package.
#'
#' @param sc Classified stratum table, or raw catch data.
#' @param area_var Column holding the ICES area code. Default
#'   `"areaValue"`.
#' @param group_var Optional column - one boxed grid of pies per area,
#'   one pie per level.
#' @param weight_var,covered_var The total weight and its sampled part
#'   (`"landings"` / `"landings_with_discards"` by default).
#' @param what,threshold,min_meas,min_trips,dims,distributions As in
#'   [plot_coverage()] - only used when `sc` is raw and has to be built.
#' @param weight_label Word for the weight (title only).
#' @param bubble_scale Multiplies every pie radius (and, when grouped,
#'   the box grid cell size).
#' @param engine `"base"` (default) or `"ggplot"`.
#' @param palette Accepted for signature symmetry with the other
#'   coverage plots; not used here.
#' @param verbose Print a one-line summary before drawing.
#' @return `NULL` invisibly (`engine = "base"`) or a ggplot object.
#' @seealso [plot_coverage()], [ices_area_centroid()]
#' @family coverage plots
#' @export
plot_coverage_map <- function(sc, area_var = "areaValue", group_var = NULL,
                              weight_var = "landings",
                              covered_var = "landings_with_discards",
                              what = "discard", threshold = NULL, min_meas = 0,
                              min_trips = NULL, dims = NULL, distributions = NULL,
                              weight_label = "landings", bubble_scale = 1,
                              engine = c("base", "ggplot"),
                              palette = c("default", "colorblind"),
                              verbose = FALSE) {
  engine  <- match.arg(engine)
  palette <- match.arg(palette)
  has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
  if (!requireNamespace("maps", quietly = TRUE))
    stop("plot_coverage_map() needs the 'maps' package: install.packages(\"maps\")")
  thr <- if (is.null(threshold) || is.na(threshold)) 50 else threshold
  mtr <- if (is.null(min_trips)  || is.na(min_trips))  2 else min_trips
  if (is.null(dims))
    dims <- unique(c(area_var, if (has(group_var)) group_var))
  sc <- resolve_coverage_strata(sc, dims, NULL, NULL, NULL, thr, what, min_meas,
                                mtr, distributions)
  for (v in c(area_var, weight_var, covered_var))
    if (!v %in% names(sc)) stop("plot_coverage_map(): no column '", v, "'")

  keys <- c(area_var, if (has(group_var)) group_var)
  agg <- sc %>% group_by(across(all_of(keys))) %>%
    summarise(w   = sum(.data[[weight_var]],  na.rm = TRUE),
              cov = sum(.data[[covered_var]], na.rm = TRUE), .groups = "drop") %>%
    filter(w > 0) %>% as.data.frame()
  cen <- ices_area_centroid(agg[[area_var]])
  agg$lon <- cen$lon; agg$lat <- cen$lat
  agg <- agg[!is.na(agg$lon), , drop = FALSE]
  if (!nrow(agg)) { message("plot_coverage_map(): no mappable areas."); return(invisible(NULL)) }
  agg$frac <- pmin(1, pmax(0, agg$cov / agg$w))

  lev <- NULL; ngrp <- 1L
  if (has(group_var)) {
    lev  <- sort(unique(as.character(agg[[group_var]])))
    ngrp <- length(lev)
  }
  # Single-pie radius (no group): sqrt(weight) to `max_rad`, small floor.
  max_rad <- bubble_scale * 0.95
  min_rad <- max_rad * 0.28
  boxed <- has(group_var) && ngrp > 1
  fills <- c(sampled = "#41AE76", unsampled = "grey80")
  gmax  <- max(agg$w, na.rm = TRUE)
  # Without a group it is one pie at the centroid. With a group the
  # area's pies go in a tidy grid inside a labelled box: one cell per
  # level, left to right then top to bottom. Pie radius is sqrt(weight)
  # scaled so the busiest (area x level) cell anywhere fills its cell.
  ac <- ices_area_centroid(unique(agg[[area_var]]))
  ac <- ac[!is.na(ac$lon), , drop = FALSE]
  cs  <- 0.46 * bubble_scale                 # half a grid cell, lat-deg
  nc  <- if (boxed) ceiling(sqrt(ngrp)) else 1L
  nr  <- if (boxed) ceiling(ngrp / nc) else 1L
  bhw <- nc * cs + 0.16                      # box half-width  (lat-deg)
  bhh <- nr * cs + 0.16                      # box half-height (lat-deg)

  boxes <- NULL
  if (boxed) {
    # Nudge overlapping boxes apart, then place the pies relative to the
    # nudged box centre (a thin leader keeps each box tied to its area).
    ac$bx <- ac$lon; ac$by <- ac$lat
    hwl <- bhw / cos(ac$lat * pi / 180); hhl <- rep(bhh, nrow(ac))
    for (t in seq_len(80)) {
      moved <- FALSE
      gap <- 0.25
      if (nrow(ac) > 1) for (i in 1:(nrow(ac) - 1)) for (j in (i + 1):nrow(ac)) {
        ox <- (hwl[i] + hwl[j] + gap) - abs(ac$bx[i] - ac$bx[j])
        oy <- (hhl[i] + hhl[j] + gap) - abs(ac$by[i] - ac$by[j])
        if (ox > 0 && oy > 0) {
          moved <- TRUE
          if (ox <= oy) {
            s <- sign(ac$bx[i] - ac$bx[j]); if (s == 0) s <- 1
            ac$bx[i] <- ac$bx[i] + s * ox / 2; ac$bx[j] <- ac$bx[j] - s * ox / 2
          } else {
            s <- sign(ac$by[i] - ac$by[j]); if (s == 0) s <- 1
            ac$by[i] <- ac$by[i] + s * oy / 2; ac$by[j] <- ac$by[j] - s * oy / 2
          }
        }
      }
      if (!moved) break
    }
    boxes <- data.frame(area = ac$area,
                        x0 = ac$bx - hwl, x1 = ac$bx + hwl,
                        y0 = ac$by - bhh, y1 = ac$by + bhh,
                        cx0 = ac$lon, cy0 = ac$lat, bx = ac$bx, by = ac$by,
                        lx = ac$bx - hwl + 0.1, ly = ac$by + bhh - 0.16,
                        stringsAsFactors = FALSE)
  }

  agg$k   <- if (boxed) match(as.character(agg[[group_var]]), lev) else 1L
  col_i   <- ((agg$k - 1) %% nc) + 1
  row_i   <- ((agg$k - 1) %/% nc) + 1
  bx <- if (boxed) boxes$bx[match(as.character(agg[[area_var]]), boxes$area)] else agg$lon
  by <- if (boxed) boxes$by[match(as.character(agg[[area_var]]), boxes$area)] else agg$lat
  agg$cx  <- if (boxed) bx + (col_i - (nc + 1) / 2) * 2 * cs / cos(by * pi / 180) else bx
  agg$cy  <- if (boxed) by + ((nr + 1) / 2 - row_i) * 2 * cs else by
  agg$rad <- if (boxed) pmax(cs * 0.22, cs * 0.92 * sqrt(agg$w / gmax))
             else       pmax(min_rad, max_rad * sqrt(agg$w / gmax))
  agg$glab <- if (boxed) as.character(agg[[group_var]]) else NA_character_

  pad  <- function(z, p) c(min(z, na.rm = TRUE) - p, max(z, na.rm = TRUE) + p)
  allx <- c(agg$cx, if (boxed) c(boxes$x0, boxes$x1) else agg$lon)
  ally <- c(agg$cy, if (boxed) c(boxes$y0, boxes$y1 + 0.5) else agg$lat)
  xlim <- pad(allx, 2.5); ylim <- pad(ally, 1.8)
  ttl  <- sprintf("%s coverage by %s%s", cov_title_what(what), area_var,
                  if (has(group_var)) paste0(" and ", group_var) else "")

  if (isTRUE(verbose))
    cat(sprintf("-- plot_coverage_map(): %s | area = %s%s | %d pies over %d areas | %s | engine = %s\n",
                cov_title_what(what), area_var,
                if (has(group_var)) paste0(" x ", group_var) else "",
                nrow(agg), nrow(ac), if (boxed) sprintf("%dx%d boxes", nc, nr) else "single pies",
                engine))

  slice_xy <- function(cx, cy, r, a0, a1) {
    asp <- 1 / cos(cy * pi / 180)
    th  <- seq(a0, a1, length.out = 48)
    list(x = cx + c(0, r * asp * cos(th)), y = cy + c(0, r * sin(th)))
  }

  if (identical(engine, "base")) {
    op <- graphics::par(mar = c(2, 2, 3, 2)); on.exit(graphics::par(op), add = TRUE)
    suppressWarnings(maps::map("world", xlim = xlim, ylim = ylim, fill = TRUE,
                               col = "grey93", border = "grey75"))
    graphics::box(); graphics::title(ttl)
    if (boxed) {
      graphics::segments(boxes$cx0, boxes$cy0, boxes$bx, boxes$by,
                         col = "grey60", lwd = 0.6)
      graphics::points(boxes$cx0, boxes$cy0, pch = 20, cex = 0.7, col = "grey45")
      for (b in seq_len(nrow(boxes))) {
        graphics::rect(boxes$x0[b], boxes$y0[b], boxes$x1[b], boxes$y1[b],
                       col = grDevices::adjustcolor("white", 0.78),
                       border = "grey55", lwd = 0.8)
        graphics::text(boxes$lx[b], boxes$ly[b], boxes$area[b], cex = 0.66,
                       font = 2, col = "grey20", adj = c(0, 0.5))
      }
    } else
      graphics::text(ac$lon, ac$lat, ac$area, cex = 0.62, font = 2, col = "grey20")
    for (i in seq_len(nrow(agg))) {
      f <- agg$frac[i] * 2 * pi
      s1 <- slice_xy(agg$cx[i], agg$cy[i], agg$rad[i], pi / 2, pi / 2 - f)
      s2 <- slice_xy(agg$cx[i], agg$cy[i], agg$rad[i], pi / 2 - f, pi / 2 - 2 * pi)
      graphics::polygon(s1$x, s1$y, col = fills["sampled"],   border = OUTLINE, lwd = 0.6)
      graphics::polygon(s2$x, s2$y, col = fills["unsampled"], border = OUTLINE, lwd = 0.6)
      if (boxed)
        graphics::text(agg$cx[i], agg$cy[i] - cs * 0.98, agg$glab[i], cex = 0.5,
                       col = "grey25")
    }
    graphics::legend("bottomleft", bg = "white", box.col = "grey80", fill = fills,
                     legend = c("sampled", "not sampled"), cex = 0.85)
    return(invisible(NULL))
  }

  polys <- do.call(rbind, lapply(seq_len(nrow(agg)), function(i) {
    f <- agg$frac[i] * 2 * pi
    s1 <- slice_xy(agg$cx[i], agg$cy[i], agg$rad[i], pi / 2, pi / 2 - f)
    s2 <- slice_xy(agg$cx[i], agg$cy[i], agg$rad[i], pi / 2 - f, pi / 2 - 2 * pi)
    rbind(data.frame(x = s1$x, y = s1$y, pid = i, slice = "sampled"),
          data.frame(x = s2$x, y = s2$y, pid = i, slice = "unsampled"))
  }))
  p <- ggplot() +
    borders("world", xlim = xlim, ylim = ylim, fill = "grey93", colour = "grey75")
  if (boxed)
    p <- p +
      geom_segment(data = boxes, aes(x = cx0, y = cy0, xend = bx, yend = by),
                   colour = "grey60", linewidth = 0.3, inherit.aes = FALSE) +
      geom_point(data = boxes, aes(cx0, cy0), colour = "grey45", size = 0.9,
                 inherit.aes = FALSE) +
      geom_rect(data = boxes,
                aes(xmin = x0, xmax = x1, ymin = y0, ymax = y1),
                fill = "white", alpha = 0.78, colour = "grey55",
                linewidth = 0.3, inherit.aes = FALSE)
  p <- p +
    geom_polygon(data = polys,
                 aes(x, y, group = interaction(pid, slice), fill = slice),
                 colour = OUTLINE, linewidth = 0.2) +
    scale_fill_manual(values = fills, name = NULL,
                      labels = c(sampled = "sampled", unsampled = "not sampled")) +
    coord_quickmap(xlim = xlim, ylim = ylim) +
    labs(title = ttl, x = NULL, y = NULL) + app_theme()
  if (boxed)
    p <- p +
      geom_text(data = boxes, aes(lx, ly, label = area), size = 2.3, hjust = 0,
                fontface = 2, colour = "grey20", inherit.aes = FALSE) +
      geom_text(data = agg, aes(cx, cy - cs * 0.98, label = glab), size = 1.9,
                colour = "grey25", inherit.aes = FALSE)
  else
    p <- p + geom_text(data = ac, aes(lon, lat, label = area), size = 2.3,
                       fontface = 2, colour = "grey20")
  p
}



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
#' Discard- or biology-sampling coverage plot
#'
#' Draws a coverage figure directly from raw catch data, or from an
#' already-built and classified strata table - whichever is handed in.
#'
#' `strata` can be either:
#'   - a data frame already run through [build_strata()] and
#'     [classify_strata()] (the original calling convention, still fully
#'     supported - every existing caller keeps working unchanged), or
#'   - raw catch data (whatever [build_strata()] itself expects), in
#'     which case `plot_coverage()` builds and classifies it internally,
#'     using `dims` (or, if `dims` is not given, the union of `y`,
#'     `fill` and `facet`) and the `threshold` / `min_meas` / `min_trips`
#'     already being passed in for the plot.
#'
#' The second form is the point: previously a caller always had to write
#' `build_strata()` and `classify_strata()` themselves before calling a
#' plot function, passing the SAME threshold and resolution to both
#' steps by hand and risking the two disagreeing. Now one call is
#' enough, and the strata table matches the figure by construction.
#'
#' `threshold` and `min_trips` are used differently by the three types.
#' "heatmap" and "bubbles" colour every cell by its sampling STATUS,
#' which is a verdict against these two bars - they are genuinely
#' required, and default to 50 / 2 (the same defaults `classify_strata()`
#' itself uses) whenever left `NULL`. "bars" shows the SAME split
#' (sampled vs unsampled weight) regardless of what either bar is set
#' to - the only thing that changes which rows are drawn at all is
#' whether a stratum has any landings or discards, which neither
#' threshold nor min_trips affects. Passing either in for `type =
#' "bars"` is therefore accepted but ignored - both are forced to `NA`
#' internally, and a `verbose` run says so, rather than silently
#' pretending the value you set was used for something.
#'
#' @param strata Raw catch data, or an already-classified strata table
#'   (see Details).
#' @param dims Columns to build the strata on, when `strata` is raw
#'   data. Defaults to the union of `y`, `fill` and `facet` - enough for
#'   the figure itself, though a caller building strata for reuse
#'   elsewhere may want to pass a wider set explicitly.
#' @param type "heatmap", "bubbles" or "bars".
#' @param y,fill,facet Column names for the plot's roles. `y` is
#'   required; `fill`/`facet` may be `NULL`.
#' @param threshold Coverage threshold (%), used to classify raw data
#'   into a sampling status. Ignored for `type = "bars"` (see Details).
#'   Defaults to 50 when `NULL` and the type needs it.
#' @param what "discard" (default), "length" or "age". For "length" /
#'   "age", either pass an already-classified biology table as `strata`,
#'   or pass raw catch data together with `distributions` and the whole
#'   biology pipeline (`biology_link()`, `stratum_measurements()`,
#'   `add_sufficiency()`, `classify_strata()`) runs internally.
#' @param distributions A distributions table, needed only to auto-build
#'   a `what = "length"`/`"age"` figure from raw catch data. `"age"`
#'   reads the `Age` rows, otherwise `LengthTotal`.
#' @param weight_label Axis/legend label for the weight measure.
#' @param ncol Facet columns, for the "bars" type.
#' @param min_meas,min_trips Sufficiency floors used to classify raw
#'   data. `min_trips` is ignored for `type = "bars"` (see Details);
#'   defaults to 2 when `NULL` and the type needs it. `min_meas`
#'   defaults to 0 (no measurement floor) regardless of type.
#' @param engine "ggplot" (default, unchanged) or "base" - draws with
#'   base R graphics instead of ggplot2. Same data, same colour-by-status
#'   scheme, same faceting, all three types ("heatmap", "bubbles",
#'   "bars"); a lighter-weight rendering for a script that would rather
#'   not depend on ggplot2, at the cost of some of the ggplot version's
#'   finer annotation (exact separator lines, precise percentage
#'   callouts, ggrepel's automatic label-overlap avoidance on the
#'   bubble plot - labels there are placed just above each bubble
#'   instead).
#' @param verbose If `TRUE`, prints a short summary to the console
#'   before drawing: the plot type and engine, which columns feed y /
#'   fill / facet, whether a threshold applies and what it resolved to,
#'   and how many strata rows are being drawn. Meant for exploring at
#'   the console, not for a script that already knows what it asked for
#'   - default `FALSE`.
#' @return A ggplot object (`engine = "ggplot"`), or `NULL` invisibly
#'   after drawing to the current device (`engine = "base"`).
#' @seealso [build_strata()], [classify_strata()], [plot_biology_coverage()]
#' @family coverage plots
#' @export
plot_coverage <- function(strata,
                          type = c("heatmap", "bubbles", "bars", "scatter"),
                          y = NULL, fill = NULL, facet = NULL,
                          threshold = NULL, what = "discard",
                          weight_label = "landings", ncol = 2, min_meas = 0,
                          min_trips = NULL, dims = NULL, distributions = NULL,
                          scatter_y = "coverage", scatter_size = "numTrips",
                          label_n = 8,
                          engine = c("ggplot", "base"), verbose = FALSE,
                          palette = c("default", "colorblind"), angle = 45,
                          n_per_screen = NULL, show_fill_in_labels = FALSE) {
  type    <- match.arg(type)
  engine  <- match.arg(engine)
  palette <- match.arg(palette)
  if (!identical(type, "scatter") &&
        (is.null(y) || !nzchar(y)))
    stop("`y` is required for type = \"", type, "\"")
  # scatter has numeric axes, not a category on y - infer a stratum
  # resolution from fill/facet (or fall back to fleet x country) so the
  # raw-catch auto-build still works without a `y`.
  if (identical(type, "scatter") && is.null(dims))
    dims <- unique(c("fleetValue", "Country",
                     if (!is.null(fill)  && nzchar(fill))  fill,
                     if (!is.null(facet) && nzchar(facet)) facet))
  # "bars" never uses either bar: the ggplot version filters on
  # cat_key != "none" only, which landings/discards alone decide, and
  # colours by the fill variable, not by status - a threshold or a
  # trips floor genuinely changes nothing it draws. Forced to NA (not
  # just left at whatever default) so a verbose run reports the truth,
  # and a caller cannot be misled into thinking a value they set was
  # doing something.
  if (identical(type, "bars")) {
    threshold_used <- NA_real_; min_trips_used <- NA_real_
  } else {
    threshold_used <- if (is.null(threshold) || is.na(threshold)) 50 else threshold
    min_trips_used <- if (is.null(min_trips) || is.na(min_trips)) 2 else min_trips
  }
  # classify_strata() itself still needs SOME numeric threshold/min_trips
  # to run, even when building for "bars" - an internal placeholder,
  # standing in for the fact that no rung of the resulting table is
  # actually influenced by it (see above). Never surfaced to the caller
  # or the verbose summary as if it meant something.
  classify_threshold <- if (is.na(threshold_used)) 50 else threshold_used
  classify_min_trips <- if (is.na(min_trips_used)) 2 else min_trips_used

  strata <- resolve_coverage_strata(strata, dims, y, fill, facet,
                                    classify_threshold, what, min_meas,
                                    classify_min_trips, distributions)

  if (isTRUE(verbose)) coverage_verbose_summary(
    strata, type, engine, y, fill, facet, threshold_used, what,
    weight_label, min_meas, min_trips_used)

  if (identical(engine, "base")) {
    return(invisible(switch(type,
      heatmap = plot_heatmap_base(strata, y, fill, facet, threshold_used, what,
                                  weight_label, min_meas, min_trips_used,
                                  palette = palette, angle = angle,
                                  n_per_screen = n_per_screen),
      bars    = plot_coverage_bars_base(strata, y, fill, facet, what,
                                        weight_label, palette = palette,
                                        n_per_screen = n_per_screen,
                                        show_fill_in_labels = show_fill_in_labels),
      bubbles = plot_bubbles_base(strata, y, fill, facet, threshold_used, what,
                                  weight_label, min_meas, min_trips_used,
                                  palette = palette, angle = angle,
                                  n_per_screen = n_per_screen),
      scatter = plot_coverage_scatter_base(strata, y = scatter_y,
                                  size = scatter_size, facet_var = facet,
                                  threshold = threshold_used, what = what,
                                  weight_label = weight_label, min_meas = min_meas,
                                  min_trips = min_trips_used, label_n = label_n,
                                  palette = palette, angle = angle,
                                  n_per_screen = n_per_screen))))
  }
  switch(type,
    heatmap = plot_heatmap(strata, y, fill, facet, threshold_used, what,
                           weight_label, min_meas, min_trips_used),
    bubbles = plot_bubbles(strata, y, fill, facet, threshold_used, what,
                           weight_label, min_meas, min_trips_used),
    bars    = plot_coverage_bars(strata, y, fill, facet, ncol, what, weight_label),
    scatter = plot_coverage_scatter(strata, y = scatter_y, size = scatter_size,
                           facet_var = facet, threshold = threshold_used,
                           what = what, weight_label = weight_label,
                           min_meas = min_meas, min_trips = min_trips_used,
                           label_n = label_n, palette = palette))
}

#' Build and classify strata only if `strata` looks like raw catch data
#'
#' The detection is simple and deliberate: a classified strata table
#' always carries `cat_key` ([classify_strata()] adds it unconditionally),
#' raw catch data never does. If `strata` already has it, it is returned
#' completely unchanged - this function does not re-classify, re-filter,
#' or otherwise second-guess a table a caller already built, which
#' matters for every existing call site in the app that passes a strata
#' table already carrying exclusions, a custom donor window, or a
#' resolution wider than this one figure's own axes.
#'
#' @param strata Raw catch data, or an already-classified strata table.
#' @param dims Columns to build on; defaults to the union of `y`,
#'   `fill`, `facet` when `NULL`.
#' @param y,fill,facet,threshold,what,min_meas,min_trips As in
#'   [plot_coverage()].
#' @param distributions A distributions table - required to auto-build a
#'   `what = "length"`/`"age"` table from raw catch data; ignored when
#'   `strata` is already classified or `what = "discard"`.
#' @return A classified strata table.
#' @keywords internal
#' @export
resolve_coverage_strata <- function(strata, dims, y, fill, facet,
                                    threshold, what, min_meas, min_trips,
                                    distributions = NULL) {
  if (!is.null(strata) && "cat_key" %in% names(strata)) return(strata)
  has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
  if (is.null(dims))
    dims <- unique(c(y, if (has(fill)) fill, if (has(facet)) facet))
  if (identical(what, "discard"))
    return(classify_strata(build_strata(strata, dims), threshold, what,
                           min_meas, min_trips))
  # length / age: the full biology pipeline, which needs the
  # distributions table (build_strata() alone cannot do it).
  if (is.null(distributions))
    stop('for what = "', what, '", pass either an already-classified ',
         'length/age table as `strata`, or raw catch data together with ',
         '`distributions` - build_strata() alone does not carry what a ',
         "biology stratum needs (biology_link(), stratum_measurements(), ",
         "add_sufficiency()).")
  dist_type <- if (identical(what, "age")) "Age" else "LengthTotal"
  bl  <- biology_link(strata, distributions, dims, type = dist_type)
  ms  <- stratum_measurements(strata, distributions, dims, type = dist_type)
  bl  <- add_sufficiency(bl, ms, threshold, min_meas)
  bio <- bl %>% mutate(landings = weight,
                       landings_with_discards = weight_measured,
                       discards = 0, discard_rate = NA_real_)
  classify_strata(bio, threshold, what, min_meas, min_trips)
}

#' Print a plain-language summary of a coverage plot before drawing it
#'
#' What `plot_coverage(..., verbose = TRUE)` prints: the type and
#' engine, which columns are on which role, whether a threshold applies
#' here and what it resolved to, and how many strata rows are about to
#' be drawn. Meant to answer "what exactly am I looking at" at the
#' console without having to read the call back to yourself - not a
#' log, and not shown unless asked for.
#'
#' @param strata The (already classified) strata table about to be
#'   plotted.
#' @param type,engine,y,fill,facet,threshold,what,weight_label,min_meas,min_trips
#'   As resolved inside [plot_coverage()] - `threshold`/`min_trips` are
#'   already `NA` here when `type = "bars"`, not the raw arguments the
#'   caller passed.
#' @return `NULL`, invisibly - printed as a side effect.
#' @keywords internal
#' @export
coverage_verbose_summary <- function(strata, type, engine, y, fill, facet,
                                     threshold, what, weight_label,
                                     min_meas, min_trips) {
  has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
  line <- function(...) cat(..., "\n", sep = "")
  line("-- plot_coverage(): ", cov_title_what(what), " coverage, type = \"",
       type, "\", engine = \"", engine, "\" --")
  line("  y      : ", if (has(y)) y else if (identical(type, "scatter"))
                        "(numeric - see the scatter y/size metrics)" else "(none)")
  line("  fill   : ", if (has(fill))  fill  else "(none)")
  line("  facet  : ", if (has(facet)) facet else "(none)")
  line("  weight : ", weight_label)
  if (identical(type, "bars")) {
    line("  threshold / min_trips: not used by type = \"bars\" - it shows ",
         "sampled vs unsampled weight, which neither one affects. Any ",
         "value passed in was ignored.")
  } else {
    line("  threshold  : ", threshold, "%")
    line("  min_trips  : ", min_trips)
  }
  if (!is.na(min_meas) && min_meas > 0)
    line("  min_meas   : ", min_meas)
  n <- if (is.null(strata)) 0L else nrow(strata)
  line("  strata rows being drawn: ", n)
  if (n > 0 && "Status" %in% names(strata)) {
    tb <- sort(table(strata$Status), decreasing = TRUE)
    line("  status breakdown:")
    for (i in seq_along(tb))
      line("    ", tb[i], "  ", names(tb)[i])
  }
  invisible(NULL)
}

#' Length- or age-sampling coverage plot (back-compat wrapper)
#'
#' Kept for existing call sites. [plot_coverage()] now takes `distributions`
#' and a biology `what` directly, so
#' `plot_coverage(catch, distributions = dist, what = "length", ...)` is
#' the same call in one place. This wrapper just forwards, with
#' `weight_label` defaulting to "catch".
#'
#' @inheritParams plot_coverage
#' @param what "length" or "age".
#' @return A ggplot object, or `NULL` (base engine draws and returns `NULL`).
#' @seealso [plot_coverage()], [biology_link()]
#' @family coverage plots
#' @export
plot_biology_coverage <- function(strata, distributions = NULL,
                                  type = c("heatmap", "bubbles", "bars"),
                                  y, fill = NULL, facet = NULL,
                                  threshold = NULL, what = "length",
                                  weight_label = "catch", ncol = 2, min_meas = 0,
                                  min_trips = NULL, dims = NULL,
                                  engine = c("ggplot", "base"), verbose = FALSE) {
  plot_coverage(strata, type = match.arg(type), y = y, fill = fill,
                facet = facet, threshold = threshold, what = what,
                weight_label = weight_label, ncol = ncol, min_meas = min_meas,
                min_trips = min_trips, dims = dims, distributions = distributions,
                engine = match.arg(engine), verbose = verbose)
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
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.text = element_text(size = 11),
          legend.key.size = unit(1.1, "lines"))
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

  # Y always from 0 to at least 100%, expanding only if some stratum's
  # rate is genuinely higher - so a set of low rates is not magnified
  # into looking spread out.
  y_top <- max(100, max(d$discard_rate * 100, na.rm = TRUE))
  p + scale_y_continuous(labels = function(z) paste0(z, "%"),
                         limits = c(0, y_top),
                         expand = expansion(mult = c(0, 0.03))) +
    maybe_facet(facet_var, free_y = FALSE, ncol = ncol) +
    labs(title = paste("Discard rate by", x_var,
                       if (has(fill_var)) paste("and", fill_var) else ""),
         subtitle = paste("Box + points = individual strata at full resolution;",
                          "red diamond = catch-weighted pooled rate"),
         y = "Discard rate", x = x_var, fill = fill_var) +
    app_theme() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.text = element_text(size = 11),
          legend.key.size = unit(1.1, "lines"))
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
#' @param dims Columns to build the strata on, when `strata` is raw
#'   catch data. Defaults to the union of `x`, `colour`/`fill` and
#'   `facet` - the same convenience [plot_coverage()] offers, so a
#'   discard-rate figure needs no `build_strata()` call of its own
#'   either.
#' @return A ggplot object, or `NULL`.
#' @seealso [plot_dr_raw()], [plot_dr_box()]
#' @family discard-rate plots
#' @examples
#' \dontrun{
#' plot_discard_rate(catch_data, "raw", x = "Season", colour = "metier4")
#' plot_discard_rate(catch_data, "box", x = "metier4", fill = "Country")
#' }
#' @param engine "ggplot" (default) or "base" - draws with base R
#'   graphics instead of ggplot2. Same data, same per-level colours
#'   (via [var_palette()], the same assignment the ggplot engine uses),
#'   same faceting.
#' @param verbose If `TRUE`, prints a short console summary before
#'   drawing - see [plot_coverage()]'s `verbose` for the same idea
#'   applied here.
#' @export
plot_discard_rate <- function(strata,
                              type = c("raw", "box"),
                              x, colour = NULL, shape = NULL, facet = NULL,
                              jitter = TRUE, fill = NULL,
                              show_points = TRUE, show_pooled = TRUE, ncol = 2,
                              dims = NULL,
                              engine = c("ggplot", "base"), verbose = FALSE,
                              palette = c("default", "colorblind"), angle = 45,
                              n_per_screen = NULL) {
  type    <- match.arg(type)
  engine  <- match.arg(engine)
  palette <- match.arg(palette)
  # Raw catch data never carries discard_rate - build_strata() is what
  # adds it. A caller passing an already-built table (has the column)
  # is used exactly as before; nothing here changes for it.
  if (!"discard_rate" %in% names(strata)) {
    if (is.null(dims)) {
      has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
      group_var <- if (identical(type, "box")) fill else colour
      dims <- unique(c(x, if (has(group_var)) group_var, if (has(facet)) facet))
    }
    strata <- build_strata(strata, dims)
  }
  if (isTRUE(verbose)) {
    cat("-- plot_discard_rate(): type = \"", type, "\", engine = \"", engine,
        "\" --\n", sep = "")
    cat("  x      : ", x, "\n", sep = "")
    if (identical(type, "raw")) {
      cat("  colour : ", if (!is.null(colour)) colour else "(none)", "\n", sep = "")
      cat("  shape  : ", if (!is.null(shape))  shape  else "(none)", "\n", sep = "")
    } else {
      cat("  fill   : ", if (!is.null(fill)) fill else "(none)", "\n", sep = "")
      cat("  show_points/show_pooled: ", show_points, " / ", show_pooled, "\n", sep = "")
    }
    cat("  facet  : ", if (!is.null(facet)) facet else "(none)", "\n", sep = "")
    d <- sampled_for_discards(strata)
    cat("  strata rows with a discard rate: ", nrow(d), "\n", sep = "")
  }
  if (identical(engine, "base")) {
    return(invisible(if (identical(type, "box"))
      plot_dr_box_base(strata, x, fill, facet, show_points, show_pooled,
                       palette = palette, angle = angle,
                       n_per_screen = n_per_screen)
    else
      plot_dr_raw_base(strata, x, colour, shape, facet, jitter,
                       palette = palette, angle = angle,
                       n_per_screen = n_per_screen)))
  }
  if (type == "box")
    plot_dr_box(strata, x, fill, facet, show_points, show_pooled, ncol)
  else
    plot_dr_raw(strata, x, colour, shape, facet, jitter)
}

#' Discard rate by stratum, drawn with base R graphics
#'
#' Same data as [plot_dr_raw()] - one jittered point per stratum with a
#' discard rate, coloured by `colour_var` via [var_palette()] (the same
#' per-level colours the ggplot engine uses) - drawn with
#' `graphics::plot()`/`points()` instead of ggplot2. `shape_var` maps to
#' point shape via `pch`, the base-R equivalent of ggplot's shape scale.
#' Called through `plot_discard_rate(..., engine = "base")`, not directly.
#'
#' @inheritParams plot_dr_raw
#' @return `NULL`, invisibly - drawn on the current graphics device.
#' @seealso [plot_dr_raw()], [plot_discard_rate()]
#' @keywords internal
#' @export
plot_dr_raw_base <- function(s, x_var, colour_var = NULL, shape_var = NULL,
                             facet_var = NULL, jitter = TRUE,
                             palette = c("default", "colorblind"), angle = 45,
                             n_per_screen = NULL) {
  palette <- match.arg(palette)
  d <- sampled_for_discards(s)
  if (!nrow(d)) { message("Nothing to plot."); return(invisible(NULL)) }
  has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
  xs <- sort(unique(as.character(d[[x_var]])))
  d$.xi <- match(as.character(d[[x_var]]), xs)
  if (isTRUE(jitter)) {
    set.seed(1)
    d$.xi <- d$.xi + stats::runif(nrow(d), -0.18, 0.18)
  }
  d$.rate <- d$discard_rate * 100
  # Colour and shape attached as columns of d itself, not looked up
  # separately by row position or name - a lookup keyed on rownames()
  # silently returns NA (an invisible point: pch = NA) the moment a
  # facet subset's rownames do not still match the un-subset table's,
  # which is exactly what happened here: only one point ever rendered,
  # for a coincidental row where the mismatched lookup still lined up.
  # A column travels with its row through any subsetting automatically.
  pal <- if (has(colour_var)) var_palette(colour_var, d[[colour_var]], palette) else NULL
  d$.ptcol <- if (has(colour_var)) pal[as.character(d[[colour_var]])] else PAL_BASE[1]
  pch_lv <- c(21, 22, 23, 24, 25, 8, 3, 4)
  d$.ptpch <- if (has(shape_var)) {
    lv <- sort(unique(as.character(d[[shape_var]])))
    pch_lv[match(as.character(d[[shape_var]]), lv)]
  } else 21

  draw_one <- function(dd, main) {
    graphics::plot(dd$.xi, dd$.rate, xlim = c(0.5, length(xs) + 0.5),
                   axes = FALSE, xlab = "", ylab = "Discard rate", type = "n",
                   main = main)
    grid_base(v = seq_along(xs), h = graphics::axTicks(2))
    graphics::points(dd$.xi, dd$.rate, pch = dd$.ptpch, bg = dd$.ptcol,
                     col = OUTLINE, cex = 1.3)
    graphics::box()
    axis_x_cat(seq_along(xs), xs, angle = angle, cex = 0.7)
    graphics::axis(2, at = graphics::axTicks(2),
                   labels = paste0(graphics::axTicks(2), "%"), las = 1, cex.axis = 0.8)
  }

  facets <- if (has(facet_var)) sort(unique(as.character(d[[facet_var]]))) else NA
  main_txt <- "Discard rate by stratum (raw, no aggregation)"
  # Bottom margin sized to the (45-degree) x labels, so the shared
  # x-axis title placed below them has somewhere to sit that is not on
  # top of the labels - the collision this fixes.
  bmar <- cat_axis_mar(xs, 0.7, angle)
  # The legend, once, whether faceted or not - it used to be drawn only
  # for the single-panel case, so a faceted figure had no key linking
  # colour to level at all.
  draw_legend <- function() {
    if (!has(colour_var)) return(invisible(NULL))
    lv <- sort(unique(as.character(d[[colour_var]])))
    graphics::par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),
                 new = TRUE)
    graphics::plot.new()
    graphics::legend("right", title = colour_var, legend = lv,
                     pt.bg = pal[lv], pch = 21, pt.cex = 1.4, cex = 0.9,
                     bty = "o", bg = "white", box.col = "grey80", xpd = NA,
                     y.intersp = 1.15, inset = c(0.001, 0))
  }

  groups <- split_facets_to_screens(facets, n_per_screen)
  had_device <- grDevices::dev.cur() > 1
  for (grp in groups) {
    op <- graphics::par(no.readonly = TRUE); on.exit(graphics::par(op), add = TRUE)
    if (length(groups) > 1 && !had_device) grDevices::dev.new()
    rmar <- if (has(colour_var)) 9 else 2
    if (!identical(grp, "__nofacet__") && length(grp) > 1) {
      nc <- ceiling(sqrt(length(grp))); nr <- ceiling(length(grp) / nc)
      graphics::par(mfrow = c(nr, nc), mar = c(min(bmar, 8), 4, 2, rmar),
                    oma = c(2, 0, 3, 1.5))
      for (fv in grp) draw_one(d[as.character(d[[facet_var]]) == fv, , drop = FALSE], fv)
      graphics::mtext(main_txt, outer = TRUE, side = 3, cex = 1.05, font = 2, line = 1)
      graphics::mtext(paste(facet_var, "=", paste(grp, collapse = ", ")),
                      outer = TRUE, side = 3, cex = 0.8, line = -0.2, col = "grey30")
      # One shared x-axis title for the whole figure, in the OUTER
      # margin - not per-panel, which is exactly what collided with the
      # rotated tick labels once several small panels made each one's
      # own margin too short relative to the label text underneath it.
      graphics::mtext(x_var, outer = TRUE, side = 1, cex = 0.8, line = 0.3)
    } else if (!identical(grp, "__nofacet__") && length(grp) == 1) {
      graphics::par(mar = c(bmar, 4, 4, rmar))
      draw_one(d[as.character(d[[facet_var]]) == grp, , drop = FALSE],
               paste0(main_txt, " (", facet_var, " = ", grp, ")"))
      graphics::mtext(x_var, side = 1, line = bmar - 1.4, cex = 0.85)
    } else {
      graphics::par(mar = c(bmar, 4, 4, rmar))
      draw_one(d, main_txt)
      graphics::mtext(x_var, side = 1, line = bmar - 1.4, cex = 0.85)
    }
    draw_legend()
    graphics::par(op)
  }
  invisible(NULL)
}

#' Discard rate as a boxplot, drawn with base R graphics
#'
#' Same summary as [plot_dr_box()] - a boxplot of discard rate per
#' `x_var` category, with the individual strata and a catch-weighted
#' pooled rate optionally overlaid - drawn with `graphics::boxplot()`
#' instead of ggplot2. Called through
#' `plot_discard_rate(..., engine = "base")`, not directly.
#'
#' @inheritParams plot_dr_box
#' @return `NULL`, invisibly - drawn on the current graphics device.
#' @seealso [plot_dr_box()], [plot_discard_rate()]
#' @keywords internal
#' @export
plot_dr_box_base <- function(s_full, x_var, fill_var = NULL, facet_var = NULL,
                             show_points = TRUE, show_pooled = TRUE,
                             palette = c("default", "colorblind"), angle = 45,
                             n_per_screen = NULL) {
  palette <- match.arg(palette)
  d <- sampled_for_discards(s_full)
  if (!nrow(d)) { message("Nothing to plot."); return(invisible(NULL)) }
  has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
  d$.rate <- d$discard_rate * 100
  pal <- if (has(fill_var)) var_palette(fill_var, d[[fill_var]], palette) else NULL
  bmar   <- cat_axis_mar(unique(as.character(d[[x_var]])), 0.8, angle)
  # A facet grid packs the panels tight; the x labels shrink there so a
  # margin that actually holds them stays a sane fraction of each panel.
  bmar_f <- cat_axis_mar(unique(as.character(d[[x_var]])), 0.55, angle)

  draw_one <- function(dd, main, cex_x = 0.8) {
    xs <- sort(unique(as.character(dd[[x_var]])))
    bxcol <- if (has(fill_var)) pal[as.character(dd[[fill_var]])] else PAL_BASE[1]
    bxdf <- data.frame(.rate = dd$.rate,
                       .data_x = factor(as.character(dd[[x_var]]), levels = xs))
    # An empty frame first, then the guide lines, then the boxes on top
    # (boxplot(add = TRUE)) - the same "grid behind the marks" order the
    # other base-R plots use, which boxplot() drawing its own frame does
    # not otherwise allow.
    # Y always spans 0 to at least 100% - a boxplot of rates that all
    # sit near a few percent otherwise zooms in and reads as if the
    # spread were large.
    ymax <- max(100, suppressWarnings(max(dd$.rate, na.rm = TRUE)), na.rm = TRUE)
    graphics::plot(NA, xlim = c(0.5, length(xs) + 0.5), ylim = c(0, ymax),
                   axes = FALSE, xlab = "", ylab = "Discard rate (%)", main = main)
    grid_base(v = seq_along(xs), h = graphics::axTicks(2))
    graphics::boxplot(.rate ~ .data_x, data = bxdf, add = TRUE, axes = FALSE,
                      col = if (has(fill_var)) "white" else PAL_BASE[1],
                      border = OUTLINE, outline = FALSE)
    graphics::box()
    axis_x_cat(seq_along(xs), xs, angle = angle, cex = cex_x)
    graphics::axis(2, at = graphics::axTicks(2), las = 1, cex.axis = 0.8)
    if (isTRUE(show_points)) {
      xi <- match(as.character(dd[[x_var]]), xs)
      xi <- xi + stats::runif(length(xi), -0.15, 0.15)
      graphics::points(xi, dd$.rate, pch = 21, bg = bxcol, col = OUTLINE,
                       cex = 1, lwd = 0.6)
    }
    if (isTRUE(show_pooled) && "landings_with_discards" %in% names(dd)) {
      pooled <- dd %>% group_by(across(all_of(x_var))) %>%
        summarise(w = sum(landings_with_discards, na.rm = TRUE),
                 r = sum(discards, na.rm = TRUE) /
                   pmax(sum(landings_with_discards, na.rm = TRUE) +
                          sum(discards, na.rm = TRUE), 1e-9) * 100,
                 .groups = "drop")
      pi <- match(as.character(pooled[[x_var]]), xs)
      graphics::points(pi, pooled$r, pch = 18, col = "red3", cex = 1.6)
    }
  }

  draw_legend <- function() {
    if (!has(fill_var)) return(invisible(NULL))
    lv <- sort(unique(as.character(d[[fill_var]])))
    graphics::par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),
                 new = TRUE)
    graphics::plot.new()
    graphics::legend("right", title = fill_var, legend = lv,
                     pt.bg = pal[lv], pch = 21, pt.cex = 1.4, cex = 0.9,
                     bty = "o", bg = "white", box.col = "grey80", xpd = NA,
                     y.intersp = 1.15, inset = c(0.001, 0))
  }

  facets <- if (has(facet_var)) sort(unique(as.character(d[[facet_var]]))) else NA
  groups <- split_facets_to_screens(facets, n_per_screen)
  had_device <- grDevices::dev.cur() > 1
  for (grp in groups) {
    op <- graphics::par(no.readonly = TRUE); on.exit(graphics::par(op), add = TRUE)
    if (length(groups) > 1 && !had_device) grDevices::dev.new()
    rmar <- if (has(fill_var)) 9 else 2
    if (!identical(grp, "__nofacet__") && length(grp) > 1) {
      nc <- ceiling(sqrt(length(grp))); nr <- ceiling(length(grp) / nc)
      graphics::par(mfrow = c(nr, nc), mar = c(bmar_f, 4, 2, rmar),
                    oma = c(2, 0, 3, 0))
      for (fv in grp)
        draw_one(d[as.character(d[[facet_var]]) == fv, , drop = FALSE], fv, cex_x = 0.55)
      graphics::mtext("Discard rate by stratum", outer = TRUE, side = 3,
                      cex = 1.05, font = 2, line = 1)
      graphics::mtext(paste(facet_var, "=", paste(grp, collapse = ", ")),
                      outer = TRUE, side = 3, cex = 0.8, line = -0.2, col = "grey30")
      graphics::mtext(x_var, outer = TRUE, side = 1, cex = 0.8, line = 0.3)
    } else if (!identical(grp, "__nofacet__") && length(grp) == 1) {
      graphics::par(mar = c(bmar, 4, 3, rmar))
      draw_one(d[as.character(d[[facet_var]]) == grp, , drop = FALSE],
               paste0("Discard rate by stratum (", facet_var, " = ", grp, ")"))
      graphics::mtext(x_var, side = 1, line = bmar - 1.4, cex = 0.85)
    } else {
      graphics::par(mar = c(bmar, 4, 3, rmar))
      draw_one(d, "Discard rate by stratum")
      graphics::mtext(x_var, side = 1, line = bmar - 1.4, cex = 0.85)
    }
    draw_legend()
    graphics::par(op)
  }
  invisible(NULL)
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
                             class_label = "Length (cm)",
                             value_label = "Numbers") {
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
         x = class_label, y = value_label, fill = fill_var) +
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

#' Axis labels and presentation scale for a biology distribution plot
#'
#' [prep_distributions()] already normalises the length class to cm and
#' the value to base N / g (mm and kg get converted on the way in). On
#' top of that, numbers are presented in thousands (N3). This returns
#' the axis labels that match, and the numbers scale factor.
#'
#' @param distributionType "LengthTotal" (default) or "Age".
#' @param quantity "numbers" (default) or "meanweight".
#' @return A list: `class_label`, `value_label`, `value_scale`.
#' @seealso [prep_distributions()], [plot_biology_distribution()]
#' @keywords internal
#' @export
biology_axis_labels <- function(distributionType = "LengthTotal",
                                quantity = "numbers") {
  is_age <- identical(distributionType, "Age")
  list(class_label = if (is_age) "Age (years)" else "Length (cm)",
       value_label = if (identical(quantity, "numbers"))
         "Numbers (thousands)" else "Mean weight (g)",
       value_scale = if (identical(quantity, "numbers")) 1e-3 else 1)
}

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
#' @param engine "ggplot" (default) or "base" - draws with base R
#'   graphics instead of ggplot2. Same data, same per-level colours (via
#'   [var_palette()]), same faceting (up to two facet columns, combined
#'   into one grid of panels rather than ggplot's nested rows/columns).
#' @param verbose If `TRUE`, prints a short console summary before
#'   drawing - see [plot_coverage()]'s `verbose` for the same idea
#'   applied here.
#' @export
plot_biology_distribution <- function(data, distributions = NULL,
                                      variableType = NULL,
                                      distributionType = NULL,
                                      quantity = c("numbers", "meanweight"),
                                      type = c("histogram", "density", "line"),
                                      fill = NULL, colour = NULL,
                                      facet1 = NULL, facet2 = NULL, shape = NULL,
                                      dims = NULL, catch_category = "Lan",
                                      stacked = TRUE, smooth = 1,
                                      show_points = TRUE, show_spread = TRUE,
                                      ncol = 2, scales = "fixed",
                                      class_label = NULL, value_label = NULL,
                                      engine = c("ggplot", "base"),
                                      verbose = FALSE,
                                      palette = c("default", "colorblind"),
                                      angle = 45, n_per_screen = NULL) {
  type <- match.arg(type)
  engine <- match.arg(engine)
  palette <- match.arg(palette)
  # variableType, when given, decides the quantity: Number -> numbers,
  # WeightLive -> meanweight. Otherwise fall back to `quantity`.
  quantity <- if (!is.null(variableType))
    (if (identical(variableType, "WeightLive")) "meanweight" else "numbers")
  else match.arg(quantity)
  distributionType <- distributionType %||%
    (if (identical(quantity, "meanweight")) "LengthTotal" else "LengthTotal")

  # Direct-call form: raw catch data + a distributions table. Link and
  # summarise here so a caller need not run link_dist_to_strata() /
  # length_numbers() / length_meanweight() by hand. `data` that is
  # already a prepared numbers / mean-weight table (no `distributions`)
  # takes the original path unchanged.
  lab <- biology_axis_labels(distributionType, quantity)
  if (!is.null(distributions)) {
    has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
    grp <- unique(c(if (has(fill)) fill, if (has(colour)) colour,
                    if (has(facet1)) facet1, if (has(facet2)) facet2,
                    if (has(shape)) shape))
    if (is.null(dims))
      dims <- unique(c("catchCategory", "fleetValue", "Country", "Season", grp))
    dims <- intersect(dims, names(as.data.frame(data)))
    linked <- link_dist_to_strata(data, distributions, dims, catch_category,
                                  type = distributionType)
    data <- if (identical(quantity, "numbers"))
      length_numbers(linked, grp) else length_meanweight(linked, grp)
    # Numbers presented in thousands (N3); length already in cm, age as
    # is - prep_distributions() did the unit normalisation, this is the
    # presentation scale on top.
    if (!is.null(data) && nrow(data) && identical(quantity, "numbers"))
      data$number <- data$number * lab$value_scale
  }
  class_label <- class_label %||% lab$class_label
  value_label <- value_label %||% lab$value_label

  if (isTRUE(verbose)) {
    cat("-- plot_biology_distribution(): quantity = \"", quantity,
        "\", type = \"", type, "\", engine = \"", engine, "\" --\n", sep = "")
    cat("  class_label : ", class_label, "\n", sep = "")
    if (identical(quantity, "numbers"))
      cat("  fill  : ", if (!is.null(fill)) fill else "(none)", "\n", sep = "")
    else {
      cat("  colour: ", if (!is.null(colour)) colour else "(none)", "\n", sep = "")
      cat("  shape : ", if (!is.null(shape))  shape  else "(none)", "\n", sep = "")
    }
    cat("  facet1 / facet2: ", if (!is.null(facet1)) facet1 else "(none)", " / ",
        if (!is.null(facet2)) facet2 else "(none)", "\n", sep = "")
    cat("  rows: ", if (is.null(data)) 0L else nrow(data), "\n", sep = "")
  }
  if (identical(engine, "base")) {
    return(invisible(if (quantity == "numbers") {
      if (type == "density")
        plot_length_ridges_base(data, fill, facet1, facet2, smooth = smooth,
                                class_label = class_label, palette = palette,
                                angle = angle, n_per_screen = n_per_screen)
      else
        plot_length_hist_base(data, fill, facet1, facet2, stacked = stacked,
                              class_label = class_label, value_label = value_label,
                              palette = palette, angle = angle,
                              n_per_screen = n_per_screen)
    } else {
      plot_length_meanweight_base(data, colour, facet1, facet2, shape,
                                  show_points = show_points,
                                  show_spread = show_spread,
                                  class_label = class_label, palette = palette,
                                  angle = angle, n_per_screen = n_per_screen)
    }))
  }
  if (quantity == "numbers") {
    if (type == "density")
      plot_length_ridges(data, fill, facet1, facet2, smooth = smooth,
                         ncol = ncol, scales = scales, class_label = class_label)
    else
      plot_length_hist(data, fill, facet1, facet2, stacked = stacked,
                       ncol = ncol, scales = scales, class_label = class_label,
                       value_label = value_label)
  } else {
    plot_length_meanweight(data, colour, facet1, facet2, shape,
                           show_points = show_points, show_spread = show_spread,
                           ncol = ncol, scales = scales, class_label = class_label)
  }
}

#' Facet key combining up to two facet columns into one grid of panels
#'
#' The base-R distribution plots use `par(mfrow = )` for faceting, one
#' grid of panels rather than ggplot's nested rows/columns - so two
#' facet columns are combined into a single composite label ("A | B")
#' here, one panel per combination, rather than drawn as a true 2D grid
#' the way [facet_two()] lays them out for ggplot.
#'
#' @param d A data frame.
#' @param facet1,facet2 Optional facet column names.
#' @return Character vector, one label per row of `d` (or `NA` if
#'   neither facet is given).
#' @keywords internal
#' @export
facet_key_base <- function(d, facet1 = NULL, facet2 = NULL) {
  has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
  parts <- list()
  if (has(facet1)) parts$f1 <- as.character(d[[facet1]])
  if (has(facet2)) parts$f2 <- as.character(d[[facet2]])
  if (!length(parts)) return(rep(NA_character_, nrow(d)))
  do.call(paste, c(parts, sep = " | "))
}

#' Numbers-at-class histogram, drawn with base R graphics
#'
#' Same data as [plot_length_hist()] - bars of numbers over the length
#' or age classes, coloured by `fill_var` via [var_palette()] (the same
#' per-level colours the ggplot engine uses), stacked or side-by-side -
#' drawn with `graphics::barplot()` instead of ggplot2. Called through
#' `plot_biology_distribution(..., engine = "base")`, not directly.
#'
#' @inheritParams plot_length_hist
#' @return `NULL`, invisibly - drawn on the current graphics device.
#' @seealso [plot_length_hist()], [plot_biology_distribution()]
#' @keywords internal
#' @export
plot_length_hist_base <- function(d, fill_var = NULL, facet1 = NULL,
                                  facet2 = NULL, stacked = TRUE,
                                  class_label = "Length (cm)",
                                  value_label = "Numbers",
                                  palette = c("default", "colorblind"),
                                  angle = 45, n_per_screen = NULL) {
  palette <- match.arg(palette)
  if (is.null(d) || !nrow(d)) { message("Nothing to plot."); return(invisible(NULL)) }
  has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
  d$.fk <- facet_key_base(d, facet1, facet2)
  pal <- if (has(fill_var)) var_palette(fill_var, d[[fill_var]], palette) else NULL

  # Guide lines behind the bars, matching the other base-R plots and
  # ggplot's panel.grid: horizontal at the y ticks, vertical at pretty
  # class values mapped onto barplot()'s own bar-index x scale (that
  # scale is 1-per-bar, not the class value, so a plain abline(v = )
  # cannot be used - the class positions are interpolated onto the bar
  # centres instead).
  guides <- function(bpc, classes) {
    graphics::abline(h = graphics::axTicks(2), col = "grey92", lwd = 0.6)
    cl <- suppressWarnings(as.numeric(as.character(classes)))
    if (!all(is.finite(cl)) || length(cl) < 3) return(invisible(NULL))
    tk <- pretty(cl, 6); tk <- tk[tk >= min(cl) & tk <= max(cl)]
    if (length(tk))
      graphics::abline(v = stats::approx(cl, bpc, xout = tk, rule = 2)$y,
                       col = "grey92", lwd = 0.6)
    invisible(NULL)
  }

  draw_one <- function(dd, main) {
    classes <- sort(unique(dd$.class))
    ax_las <- if (angle >= 45 || length(classes) > 20) 2 else 1
    if (has(fill_var)) {
      lv <- sort(unique(as.character(dd[[fill_var]])))
      m <- matrix(0, length(lv), length(classes), dimnames = list(lv, classes))
      for (i in seq_len(nrow(dd)))
        m[as.character(dd[[fill_var]][i]), as.character(dd$.class[i])] <-
          m[as.character(dd[[fill_var]][i]), as.character(dd$.class[i])] + dd$number[i]
      sp <- if (stacked) 0.1 else c(0, 1.5)
      bp <- graphics::barplot(m, col = pal[lv], border = OUTLINE, beside = !stacked,
                        space = sp, xlab = class_label, ylab = value_label,
                        main = main, cex.names = 0.7, las = ax_las)
      guides(if (is.matrix(bp)) colMeans(bp) else as.numeric(bp), classes)
      graphics::barplot(m, col = pal[lv], border = OUTLINE, beside = !stacked,
                        space = sp, add = TRUE, axes = FALSE, axisnames = FALSE)
      graphics::legend("topright", legend = lv, fill = pal[lv], cex = 0.68,
                       bty = "o", bg = "white", box.col = "grey80")
    } else {
      v <- stats::setNames(rep(0, length(classes)), classes)
      agg <- stats::aggregate(number ~ .class, dd, sum)
      v[as.character(agg$.class)] <- agg$number
      bp <- graphics::barplot(v, col = PAL_BASE[1], border = OUTLINE, space = 0.1,
                        xlab = class_label, ylab = value_label, main = main,
                        cex.names = 0.7, las = ax_las)
      guides(as.numeric(bp), classes)
      graphics::barplot(v, col = PAL_BASE[1], border = OUTLINE, space = 0.1,
                        add = TRUE, axes = FALSE, axisnames = FALSE)
    }
    graphics::box()
  }

  facets <- if (!all(is.na(d$.fk))) sort(unique(d$.fk)) else NA
  main_txt <- sprintf("Numbers at %s", tolower(sub(" .*", "", class_label)))
  groups <- split_facets_to_screens(facets, n_per_screen)
  had_device <- grDevices::dev.cur() > 1
  for (grp in groups) {
    op <- graphics::par(no.readonly = TRUE); on.exit(graphics::par(op), add = TRUE)
    if (length(groups) > 1 && !had_device) grDevices::dev.new()
    if (!identical(grp, "__nofacet__") && length(grp) > 1) {
      nc <- ceiling(sqrt(length(grp))); nr <- ceiling(length(grp) / nc)
      graphics::par(mfrow = c(nr, nc), mar = c(4, 4, 2, 1), oma = c(0, 0, 3, 0))
      for (fv in grp) draw_one(d[d$.fk == fv, , drop = FALSE], fv)
      graphics::mtext(main_txt, outer = TRUE, side = 3, cex = 1.05, font = 2, line = 1)
      graphics::mtext(paste0(facet1 %||% "", if (!is.null(facet2)) paste0(", ", facet2),
                            " = ", paste(grp, collapse = ", ")),
                      outer = TRUE, side = 3, cex = 0.8, line = -0.2, col = "grey30")
    } else if (!identical(grp, "__nofacet__") && length(grp) == 1) {
      graphics::par(mar = c(4, 4, 3, 2))
      draw_one(d[d$.fk == grp, , drop = FALSE], paste0(main_txt, " (", grp, ")"))
    } else {
      graphics::par(mar = c(4, 4, 3, 2))
      draw_one(d, main_txt)
    }
    graphics::par(op)
  }
  invisible(NULL)
}

#' Numbers-at-class smoothed density, drawn with base R graphics
#'
#' Same weighted kernel density as [plot_length_ridges()] - one curve
#' per `group_var` level, each integrating to one so groups of very
#' different size compare on shape alone - drawn as filled, vertically
#' offset ribbons with `graphics::polygon()` instead of ggplot2's
#' `geom_ribbon()`. Called through
#' `plot_biology_distribution(..., engine = "base")`, not directly.
#'
#' @inheritParams plot_length_ridges
#' @return `NULL`, invisibly - drawn on the current graphics device.
#' @seealso [plot_length_ridges()], [plot_biology_distribution()]
#' @keywords internal
#' @export
plot_length_ridges_base <- function(d, group_var = NULL, facet1 = NULL,
                                    facet2 = NULL, overlap = 0.75,
                                    smooth = 1, class_label = "Length (cm)",
                                    palette = c("default", "colorblind"),
                                    angle = 45, n_per_screen = NULL) {
  palette <- match.arg(palette)
  if (is.null(d) || !nrow(d)) { message("Nothing to plot."); return(invisible(NULL)) }
  has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
  if (!has(group_var)) { d$.g <- "All"; group_var <- ".g" }
  d$.g <- as.character(d[[group_var]])
  d$.fk <- facet_key_base(d, facet1, facet2)

  # Same weighted-density logic as plot_length_ridges() - see there for
  # why the bandwidth comes from the weighted spread rather than
  # stats::density()'s own unweighted default.
  dens_one <- function(g) {
    x <- g$.class; w <- g$number
    keep <- is.finite(x) & is.finite(w) & w > 0
    x <- x[keep]; w <- w[keep]
    if (length(x) < 2 || sum(w) <= 0 || length(unique(x)) < 2) return(NULL)
    p <- w / sum(w)
    mu <- sum(p * x); sdw <- sqrt(sum(p * (x - mu)^2))
    if (!is.finite(sdw) || sdw <= 0) sdw <- diff(range(x)) / 6
    n_eff <- 1 / sum(p^2)
    bw <- 1.06 * sdw * n_eff^(-1 / 5) * smooth
    if (!is.finite(bw) || bw <= 0) return(NULL)
    dn <- tryCatch(stats::density(x, weights = p, bw = bw, n = 256,
                                  from = min(x) - 2 * bw, to = max(x) + 2 * bw),
                   error = function(e) NULL)
    if (is.null(dn)) return(NULL)
    data.frame(.class = dn$x, .dens = dn$y)
  }
  pal <- var_palette(group_var, d$.g, palette)

  draw_one <- function(dd, main) {
    lev <- sort(unique(dd$.g))
    curves <- lapply(lev, function(g) dens_one(dd[dd$.g == g, , drop = FALSE]))
    names(curves) <- lev
    curves <- Filter(Negate(is.null), curves)
    if (!length(curves)) { message("Nothing to plot for ", main); return(invisible(NULL)) }
    maxd <- max(vapply(curves, function(x) max(x$.dens), numeric(1)))
    scale_h <- (1 + overlap) / maxd
    xr <- range(unlist(lapply(curves, function(x) x$.class)))
    # ylim's upper bound was exactly length(lev) + 1.3, which clipped
    # the TALLEST curve's peak whenever that curve belonged to the row
    # closest to the top (its own peak plus 1+overlap could exceed that
    # fixed margin) - widened to fit the actual tallest peak instead of
    # a constant that assumed every curve's own height stayed small.
    graphics::plot(NA, xlim = xr, ylim = c(0.5, length(lev) + max(1.3, 1 + overlap + 0.3)),
                   axes = FALSE, xlab = class_label, ylab = "", main = main)
    grid_base(v = graphics::axTicks(1), h = seq_along(lev))
    for (i in seq_along(lev)) {
      g <- lev[i]; cv <- curves[[g]]
      if (is.null(cv)) next
      row <- i
      # Black outline (OUTLINE), not coloured to match the fill - the
      # same "make the shape's edge read clearly" fix the heatmap tiles
      # got, here applied to the density ribbon.
      graphics::polygon(c(cv$.class, rev(cv$.class)),
                        c(row + cv$.dens * scale_h, rep(row, nrow(cv))),
                        col = grDevices::adjustcolor(pal[g], alpha.f = 0.72),
                        border = OUTLINE, lwd = 0.6)
      graphics::segments(min(cv$.class), row, max(cv$.class), row,
                         col = "grey65", lwd = 0.6)
    }
    graphics::box()
    graphics::axis(1, cex.axis = 0.8)
    graphics::axis(2, at = seq_along(lev), labels = lev, las = 1, cex.axis = 0.8)
  }

  facets <- if (!all(is.na(d$.fk))) sort(unique(d$.fk)) else NA
  main_txt <- sprintf("Frequency, as a smoothed density (x%.2f smoothing)", smooth)
  # Left margin sized to the widest group label, the same way the
  # heatmap and bubble plots do it - a fixed 6 clipped the long
  # fleet-level names on the y axis.
  lmar <- max(6, max(nchar(unique(d$.g))) * 0.45)
  groups <- split_facets_to_screens(facets, n_per_screen)
  had_device <- grDevices::dev.cur() > 1
  for (grp in groups) {
    op <- graphics::par(no.readonly = TRUE); on.exit(graphics::par(op), add = TRUE)
    if (length(groups) > 1 && !had_device) grDevices::dev.new()
    if (!identical(grp, "__nofacet__") && length(grp) > 1) {
      nc <- ceiling(sqrt(length(grp))); nr <- ceiling(length(grp) / nc)
      graphics::par(mfrow = c(nr, nc), mar = c(4, lmar, 2, 1), oma = c(0, 0, 3, 0))
      for (fv in grp) draw_one(d[d$.fk == fv, , drop = FALSE], fv)
      graphics::mtext(main_txt, outer = TRUE, side = 3, cex = 1.05, font = 2, line = 1)
    } else if (!identical(grp, "__nofacet__") && length(grp) == 1) {
      graphics::par(mar = c(4, lmar, 3, 2))
      draw_one(d[d$.fk == grp, , drop = FALSE], paste0(main_txt, " (", grp, ")"))
    } else {
      graphics::par(mar = c(4, lmar, 3, 2))
      draw_one(d, main_txt)
    }
    graphics::par(op)
  }
  invisible(NULL)
}

#' Mean-weight-at-class line, drawn with base R graphics
#'
#' Same data as [plot_length_meanweight()] - mean weight per class as a
#' line with points, coloured by `colour_var` via [var_palette()], with
#' an optional +/- 1 sd ribbon - drawn with `graphics::plot()`/`lines()`
#' instead of ggplot2. Called through
#' `plot_biology_distribution(..., engine = "base")`, not directly.
#'
#' @inheritParams plot_length_meanweight
#' @return `NULL`, invisibly - drawn on the current graphics device.
#' @seealso [plot_length_meanweight()], [plot_biology_distribution()]
#' @keywords internal
#' @export
plot_length_meanweight_base <- function(d, colour_var = NULL, facet1 = NULL,
                                        facet2 = NULL, shape_var = NULL,
                                        show_points = TRUE, show_spread = TRUE,
                                        class_label = "Length (cm)",
                                        palette = c("default", "colorblind"),
                                        angle = 45, n_per_screen = NULL) {
  palette <- match.arg(palette)
  if (is.null(d) || !nrow(d)) { message("Nothing to plot."); return(invisible(NULL)) }
  has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
  d$.fk <- facet_key_base(d, facet1, facet2)
  grp_var <- if (has(colour_var)) colour_var else if (has(shape_var)) shape_var else NULL
  pal <- if (has(colour_var)) var_palette(colour_var, d[[colour_var]], palette) else NULL
  pch_lv <- c(16, 17, 15, 18, 4, 3, 8, 7)

  draw_one <- function(dd, main) {
    ylim <- range(c(dd$mean_weight,
                    if (show_spread && "sd_weight" %in% names(dd))
                      dd$mean_weight + dd$sd_weight else NULL,
                    if (show_spread && "sd_weight" %in% names(dd))
                      pmax(0, dd$mean_weight - dd$sd_weight) else NULL),
                  na.rm = TRUE)
    graphics::plot(NA, xlim = range(dd$.class, na.rm = TRUE), ylim = ylim,
                   xlab = class_label, ylab = "Mean weight (g)", main = main,
                   axes = FALSE)
    grid_base()
    graphics::box(); graphics::axis(1, cex.axis = 0.8); graphics::axis(2, cex.axis = 0.8)
    grp_lv <- if (!is.null(grp_var)) sort(unique(as.character(dd[[grp_var]]))) else "All"
    for (g in grp_lv) {
      gd <- if (!is.null(grp_var)) dd[as.character(dd[[grp_var]]) == g, , drop = FALSE] else dd
      gd <- gd[order(gd$.class), ]
      if (!nrow(gd)) next
      col <- if (has(colour_var)) pal[g] else PAL_BASE[1]
      if (isTRUE(show_spread) && !has(shape_var) && "sd_weight" %in% names(gd) &&
            any(!is.na(gd$sd_weight))) {
        # A polygon whose vertex list includes NA coordinates does not
        # skip them the way lines() does - it drew a straight connecting
        # segment across the gap instead, from whatever valid point came
        # before the NA run to whatever valid point came after, which is
        # exactly the wide diagonal wedge this fixes: a stratum missing
        # sd_weight for a few classes in the middle of its range had its
        # ribbon jump straight across that gap rather than break there.
        # One polygon per CONTIGUOUS run of non-NA points instead, so a
        # gap breaks the ribbon rather than bridging it.
        ok_sd <- !is.na(gd$sd_weight)
        run <- cumsum(c(TRUE, diff(ok_sd) != 0))
        for (r in unique(run[ok_sd])) {
          seg <- gd[run == r & ok_sd, , drop = FALSE]
          if (nrow(seg) < 2) next
          graphics::polygon(c(seg$.class, rev(seg$.class)),
                            c(seg$mean_weight + seg$sd_weight,
                              rev(pmax(0, seg$mean_weight - seg$sd_weight))),
                            col = grDevices::adjustcolor(col, alpha.f = 0.18),
                            border = NA)
        }
      }
      graphics::lines(gd$.class, gd$mean_weight, col = col, lwd = 1.8)
      if (isTRUE(show_points)) {
        pch <- if (has(shape_var)) pch_lv[match(g, grp_lv)] else 21
        graphics::points(gd$.class, gd$mean_weight, pch = pch, bg = col,
                         col = if (pch %in% 21:25) OUTLINE else col, cex = 1.1)
      }
    }
    if (!is.null(grp_var) && length(grp_lv) > 1) {
      pch_leg <- if (has(shape_var)) pch_lv[seq_along(grp_lv)] else rep(21, length(grp_lv))
      graphics::legend("topright", title = grp_var, legend = grp_lv,
                       col = if (has(colour_var)) pal[grp_lv] else PAL_BASE[1],
                       pt.bg = if (has(colour_var)) pal[grp_lv] else PAL_BASE[1],
                       pch = pch_leg, lwd = 1.8, cex = 0.68, bty = "o",
                       bg = "white", box.col = "grey80")
    }
  }

  facets <- if (!all(is.na(d$.fk))) sort(unique(d$.fk)) else NA
  main_txt <- sprintf("Mean weight at %s", tolower(sub(" .*", "", class_label)))
  groups <- split_facets_to_screens(facets, n_per_screen)
  had_device <- grDevices::dev.cur() > 1
  for (grp in groups) {
    op <- graphics::par(no.readonly = TRUE); on.exit(graphics::par(op), add = TRUE)
    if (length(groups) > 1 && !had_device) grDevices::dev.new()
    if (!identical(grp, "__nofacet__") && length(grp) > 1) {
      nc <- ceiling(sqrt(length(grp))); nr <- ceiling(length(grp) / nc)
      graphics::par(mfrow = c(nr, nc), mar = c(4, 4, 2, 1), oma = c(0, 0, 3, 0))
      for (fv in grp) draw_one(d[d$.fk == fv, , drop = FALSE], fv)
      graphics::mtext(main_txt, outer = TRUE, side = 3, cex = 1.05, font = 2, line = 1)
    } else if (!identical(grp, "__nofacet__") && length(grp) == 1) {
      graphics::par(mar = c(4, 4, 3, 2))
      draw_one(d[d$.fk == grp, , drop = FALSE], paste0(main_txt, " (", grp, ")"))
    } else {
      graphics::par(mar = c(4, 4, 3, 2))
      draw_one(d, main_txt)
    }
    graphics::par(op)
  }
  invisible(NULL)
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
