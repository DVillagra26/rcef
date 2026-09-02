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
                    oma = c(0, 0, 3, 8))
      for (fv in grp)
        draw_one(d[as.character(d[[facet_var]]) == fv, , drop = FALSE], fv)
      graphics::mtext(main_txt, outer = TRUE, side = 3, cex = 1.05, font = 2, line = 1)
      if (!is.null(facet_var))
        graphics::mtext(paste(facet_var, "=", paste(grp, collapse = ", ")),
                        outer = TRUE, side = 3, cex = 0.8, line = -0.2, col = "grey30")
    } else if (!identical(grp, "__nofacet__") && length(grp) == 1) {
      graphics::par(mar = c(bmar, lmar, 3, 8))
      draw_one(d[as.character(d[[facet_var]]) == grp, , drop = FALSE],
               paste0(main_txt, " (", facet_var, " = ", grp, ")"))
    } else {
      graphics::par(mar = c(bmar, lmar, 3, 8))
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
  wrap <- function(s, w = 26) {
    words <- strsplit(s, " ")[[1]]; out <- character(0); cur <- ""
    for (w2 in words) {
      cand <- if (nzchar(cur)) paste(cur, w2) else w2
      if (nchar(cand) > w) { out <- c(out, cur); cur <- w2 } else cur <- cand
    }
    c(out, cur)
  }
  labs <- vapply(lk_all$status, function(s) wrap(s)[1], character(1))
  graphics::legend("right", legend = labs, fill = lk_all$colour, cex = 0.55,
                   bty = "o", bg = "white", box.col = "grey80", title = "Status",
                   xpd = NA, inset = c(0.001, 0))
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
                           mea_bit),
                    "")
  } else {
    d$lab <- with(d, case_when(
      cat_key == "disc_only" ~ "100%\nDisc",
      cat_key %in% c("warn_lowcov", "warn_lowtrip") ~
        paste0(round(coverage, 1), "%"),
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
    rmar <- if (compact) 2 else
      max(9, max(nchar(as.character(dd$sampled))) * 0.62 + 3)
    if (compact) lmar <- min(lmar, 8)
    op <- graphics::par(mar = c(4, lmar, 5.5, rmar))
    on.exit(graphics::par(op), add = TRUE)
    # xlim stops just past the longest bar and the separator line, not
    # far enough to drown the chart in whitespace. The callout text
    # lives in the right MARGIN (xpd = NA below), not the plot region,
    # so the region itself is not widened to hold it.
    xmax <- max(dd$landings) * 1.12
    bp <- graphics::barplot(dd$landings[ord], horiz = TRUE, las = 1,
                            names.arg = key[ord], cex.names = 0.7,
                            col = pal[dd$.fill[ord]], border = OUTLINE,
                            xlim = c(0, xmax), space = 0.15,
                            xlab = sprintf("%s (t)", tools::toTitleCase(weight_label)),
                            main = "")
    # Panel top and bottom, half a bar-pitch beyond the end bars - the
    # separator and the vertical guide lines are clamped to this so they
    # do not spill into the title or the x-axis label the way the raw
    # device usr range lets them.
    pitch <- if (nrow(bp) > 1) bp[2, 1] - bp[1, 1] else 1.2
    y0 <- bp[1, 1] - pitch / 2
    y1 <- bp[nrow(bp), 1] + pitch / 2
    # Grey guide lines spanning the whole panel - the reading aid
    # ggplot's panel.grid gives for free: a vertical line at every x
    # tick across the full height, one faint horizontal line per bar
    # row across the full width. Drawn first, then the bars re-drawn on
    # top so the lines sit behind them.
    xticks <- graphics::axTicks(1)
    graphics::segments(xticks, y0, xticks, y1, col = "grey88", lwd = 0.7)
    graphics::segments(0, bp, xmax, bp, col = "grey92", lwd = 0.5)
    graphics::barplot(dd$landings[ord], horiz = TRUE, las = 1, add = TRUE,
                      names.arg = NA, axes = FALSE, space = 0.15,
                      col = pal[dd$.fill[ord]], border = OUTLINE)
    # box(): the same panel border every ggplot figure in this package
    # carries by default.
    graphics::box()
    graphics::axis(1, at = xticks, labels = FALSE, tick = TRUE)
    # Dashed vertical separator, full height, marking where the bars end
    # and the sampled / unsampled callouts begin - the one piece of the
    # ggplot layout this port keeps verbatim.
    lx <- dmax * 1.06
    graphics::segments(lx, y0, lx, y1, lty = 2, col = "grey40")
    # "With ... sampling (NN%)" / "Without ... sampling (NN%)" beside the
    # separator, one per contiguous block, horizontal, in the right
    # margin - matching the ggplot version's callouts including the
    # per-block share of the panel total. The label for each block is
    # read straight off the data (rle(grp)$values) at that block's own
    # rows, NOT assumed to be "with" on top: barplot()'s bottom-to-top
    # order combined with the size sort means either block can end up
    # physically uppermost, and hardcoding that mapping is what an
    # earlier pass got backwards. Skipped in compact (facet-grid) mode,
    # which has no room for it - as the pre-rewrite faceted path also
    # showed no per-block figure.
    if (!compact) {
      grp    <- as.character(dd$sampled[ord])
      blocks <- rle(grp)
      ends   <- cumsum(blocks$lengths)
      starts <- ends - blocks$lengths + 1L
      for (i in seq_along(blocks$values)) {
        lvl  <- blocks$values[i]
        ymid <- mean(bp[starts[i]:ends[i], 1])
        pctb <- sum(dd$landings[dd$sampled == lvl]) / total * 100
        graphics::text(lx + dmax * 0.04, ymid,
                       paste0(lvl, "\n(", round(pctb), "%)"),
                       adj = c(0, 0.5), cex = 0.7, font = 2, col = "grey30",
                       xpd = NA)
      }
    }
    # Title and subtitle left-margined (adj = 0), matching the ggplot
    # version - base R centres the title by default, ggplot does not.
    graphics::mtext(main, side = 3, line = 3.6, cex = 1.05, font = 2, adj = 0)
    graphics::mtext(subtitle, side = 3, line = 2.3, cex = 0.8, adj = 0, col = "grey30")
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
  title_txt <- sprintf("%s with vs without %s sampling",
                       tools::toTitleCase(weight_label), samp_word(what))
  subtitle_txt <- sprintf("%s%% of %s weight sampled for %s", round(pct, 0),
                          weight_label, samp_word(what))
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
                                           " ", ""), mea_bit), "")
  } else {
    d$lab <- dplyr::case_when(
      d$cat_key == "disc_only" ~ "100% Disc",
      d$cat_key %in% c("warn_lowcov", "warn_lowtrip") ~
        paste0(round(d$coverage, 1), "%"),
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
      graphics::par(mfrow = c(nr, nc), mar = c(min(bmar, 7), lmar, 2, 8),
                    oma = c(0, 0, 3, 0))
      for (fv in grp)
        draw_one(d[as.character(d[[facet_var]]) == fv, , drop = FALSE], fv)
      graphics::mtext(main_txt, outer = TRUE, side = 3, cex = 1.05, font = 2, line = 1)
      if (!is.null(facet_var))
        graphics::mtext(paste(facet_var, "=", paste(grp, collapse = ", ")),
                        outer = TRUE, side = 3, cex = 0.8, line = -0.2, col = "grey30")
    } else if (!identical(grp, "__nofacet__") && length(grp) == 1) {
      graphics::par(mar = c(bmar, lmar, 4, 8))
      draw_one(d[as.character(d[[facet_var]]) == grp, , drop = FALSE],
               paste0(main_txt, " (", facet_var, " = ", grp, ")"))
    } else {
      graphics::par(mar = c(bmar, lmar, 4, 8))
      draw_one(d, "")
      graphics::mtext(main_txt, side = 3, line = 2.3, cex = 1.05, font = 2)
      graphics::mtext(
        if (what == "discard")
          "Cross (X) = discards present with zero official landings"
        else sprintf("Labels = coverage below %s%% and/or n below %s",
                    threshold, min_meas),
        side = 3, line = 0.8, cex = 0.72, col = "grey30")
    }
    draw_legends()
    graphics::par(op)
  }
  invisible(NULL)
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
#' @param what "discard" (default), "length" or "age".
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
                          type = c("heatmap", "bubbles", "bars"),
                          y, fill = NULL, facet = NULL,
                          threshold = NULL, what = "discard",
                          weight_label = "landings", ncol = 2, min_meas = 0,
                          min_trips = NULL, dims = NULL,
                          engine = c("ggplot", "base"), verbose = FALSE,
                          palette = c("default", "colorblind"), angle = 45,
                          n_per_screen = NULL, show_fill_in_labels = FALSE) {
  type    <- match.arg(type)
  engine  <- match.arg(engine)
  palette <- match.arg(palette)
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
                                    classify_min_trips)

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
                                  n_per_screen = n_per_screen))))
  }
  switch(type,
    heatmap = plot_heatmap(strata, y, fill, facet, threshold_used, what,
                           weight_label, min_meas, min_trips_used),
    bubbles = plot_bubbles(strata, y, fill, facet, threshold_used, what,
                           weight_label, min_meas, min_trips_used),
    bars    = plot_coverage_bars(strata, y, fill, facet, ncol, what, weight_label))
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
#' @return A classified strata table.
#' @keywords internal
#' @export
resolve_coverage_strata <- function(strata, dims, y, fill, facet,
                                    threshold, what, min_meas, min_trips) {
  if (!is.null(strata) && "cat_key" %in% names(strata)) return(strata)
  if (!identical(what, "discard"))
    stop('"strata" must already be a classified length/age table for ',
         'what = "', what, '" - build_strata() alone does not carry the ',
         "distributions a biology stratum needs (biology_link(), ",
         "stratum_measurements(), add_sufficiency()). Auto-building from ",
         'raw catch data is only supported for what = "discard".')
  if (is.null(dims)) {
    has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
    dims <- unique(c(y, if (has(fill)) fill, if (has(facet)) facet))
  }
  classify_strata(build_strata(strata, dims), threshold, what,
                  min_meas, min_trips)
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
  line("  y      : ", y)
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

#' Length- or age-sampling coverage plot
#'
#' Thin wrapper over [plot_coverage()] for biology sampling: it shares the
#' identical heatmap/bubbles/bars engine, so a length- or age-coverage panel
#' looks like a discard-coverage one, with only the wording and the status
#' scheme changed. Here "covered" means a length/age distribution exists for
#' the stratum, and the status also reflects the number of fish measured.
#'
#' `strata` can be either:
#'   - a data frame already run through the full biology pipeline
#'     ([biology_link()], [stratum_measurements()], [add_sufficiency()],
#'     [classify_strata()]) - the original calling convention, still
#'     fully supported - or
#'   - raw catch data, with `distributions` also supplied, in which
#'     case the whole pipeline runs internally: `dims` (or the union of
#'     `y`, `fill`, `facet` when `dims` is `NULL`), `what` (translated
#'     to `distributionType`: "length" -> "LengthTotal", "age" ->
#'     "Age"), and `threshold`/`min_meas`/`min_trips` (defaulting the
#'     same way [plot_coverage()] does) drive every step. One call
#'     instead of four, matching the discard side's own convenience.
#'
#' @inheritParams plot_coverage
#' @param distributions The distributions table, required only when
#'   `strata` is raw catch data rather than an already-built table.
#' @param what "length" or "age".
#' @param weight_label Defaults to "catch".
#' @return A ggplot object, or `NULL`.
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
  type   <- match.arg(type)
  engine <- match.arg(engine)
  # Raw catch data never carries cat_key - classify_strata() is what
  # adds it, at the end of a pipeline build_strata() alone cannot do
  # for biology (it needs the distributions table too). A caller
  # passing an already-classified table (has cat_key) is used exactly
  # as before, distributions or not.
  if (!is.null(distributions) && !("cat_key" %in% names(strata))) {
    has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
    if (is.null(dims))
      dims <- unique(c(y, if (has(fill)) fill, if (has(facet)) facet))
    dist_type <- if (identical(what, "age")) "Age" else "LengthTotal"
    thr    <- if (is.null(threshold) || is.na(threshold)) 50 else threshold
    mtrips <- if (is.null(min_trips) || is.na(min_trips)) 2 else min_trips
    bl <- biology_link(strata, distributions, dims, type = dist_type)
    ms <- stratum_measurements(strata, distributions, dims, type = dist_type)
    bl <- add_sufficiency(bl, ms, thr, min_meas)
    bio <- bl %>% mutate(landings = weight, landings_with_discards = weight_measured,
                         discards = 0, discard_rate = NA_real_)
    strata <- classify_strata(bio, thr, what, min_meas, mtrips)
  }
  plot_coverage(strata, type = type, y = y, fill = fill, facet = facet,
                threshold = threshold, what = what,
                weight_label = weight_label, ncol = ncol, min_meas = min_meas,
                min_trips = min_trips, dims = dims,
                engine = engine, verbose = verbose)
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
                     pt.bg = pal[lv], pch = 21, cex = 0.62, bty = "o",
                     bg = "white", box.col = "grey80", xpd = NA,
                     inset = c(0.001, 0))
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
    graphics::plot(NA, xlim = c(0.5, length(xs) + 0.5),
                   ylim = range(c(0, dd$.rate), na.rm = TRUE), axes = FALSE,
                   xlab = "", ylab = "Discard rate (%)", main = main)
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
                     pt.bg = pal[lv], pch = 21, cex = 0.62, bty = "o",
                     bg = "white", box.col = "grey80", xpd = NA,
                     inset = c(0.001, 0))
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
#' @param engine "ggplot" (default) or "base" - draws with base R
#'   graphics instead of ggplot2. Same data, same per-level colours (via
#'   [var_palette()]), same faceting (up to two facet columns, combined
#'   into one grid of panels rather than ggplot's nested rows/columns).
#' @param verbose If `TRUE`, prints a short console summary before
#'   drawing - see [plot_coverage()]'s `verbose` for the same idea
#'   applied here.
#' @export
plot_biology_distribution <- function(data,
                                      quantity = c("numbers", "meanweight"),
                                      type = c("histogram", "density", "line"),
                                      fill = NULL, colour = NULL,
                                      facet1 = NULL, facet2 = NULL, shape = NULL,
                                      stacked = TRUE, smooth = 1,
                                      show_points = TRUE, show_spread = TRUE,
                                      ncol = 2, scales = "fixed",
                                      class_label = "Length (cm)",
                                      engine = c("ggplot", "base"),
                                      verbose = FALSE,
                                      palette = c("default", "colorblind"),
                                      angle = 45, n_per_screen = NULL) {
  quantity <- match.arg(quantity)
  type <- match.arg(type)
  engine <- match.arg(engine)
  palette <- match.arg(palette)
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
                              class_label = class_label, palette = palette,
                              angle = angle, n_per_screen = n_per_screen)
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
                       ncol = ncol, scales = scales, class_label = class_label)
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
                        space = sp, xlab = class_label, ylab = "Numbers",
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
                        xlab = class_label, ylab = "Numbers", main = main,
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
