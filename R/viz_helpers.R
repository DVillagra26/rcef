# ============================================================
#  viz_helpers.R
#  Shared plumbing for R/viz_plots.R: faceting, base-R axis / legend
#  drawing, strata building, arg validation and the ICES-area centroid
#  table. The plot functions themselves are in viz_plots.R - this file is
#  the machinery they lean on.
# ============================================================

#' @keywords internal
#' @name rcef-viz-helpers
#' @noRd
#' @import ggplot2
#' @importFrom dplyr %>% across all_of arrange bind_rows case_when desc filter
#'   first group_by left_join mutate row_number select summarise ungroup
#' @importFrom rlang .data
NULL

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
  # A merged cell "has a discard estimate" if ANY of the strata folded
  # into it does - so a cell where no component stratum was ever sampled
  # for discards still classifies as "no estimate at all" (red), not as
  # a threshold miss.
  if (!"has_disc_data" %in% names(sc)) sc$has_disc_data <- !is.na(sc$discards)
  sc %>%
    group_by(across(all_of(keys))) %>%
    summarise(landings = sum(landings, na.rm = TRUE),
              landings_with_discards = sum(landings_with_discards, na.rm = TRUE),
              discards = sum(discards, na.rm = TRUE),
              has_disc_data = any(has_disc_data),
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

#' Canonicalise a `what` argument
#'
#' The coverage plots take `what = "discard"` / `"length"` / `"age"`.
#' Callers reasonably type the plural or a near-synonym; this maps those
#' to the canonical singular and gives a clear error for anything else,
#' rather than letting an unrecognised value fall through to the biology
#' pipeline and fail obscurely.
#'
#' @param what A string: discard(s), length(s)/len, or age(s).
#' @return `"discard"`, `"length"` or `"age"`.
#' @keywords internal
#' @export
normalize_what <- function(what) {
  w <- tolower(trimws(as.character(what)))
  out <- switch(w,
    discard = , discards = "discard",
    length = , lengths = , len = "length",
    age = , ages = "age",
    NA_character_)
  if (is.na(out))
    stop("`what` must be \"discard\", \"length\" or \"age\" (got \"", what,
         "\")", call. = FALSE)
  out
}

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
#'   by [plot_coverage_bubble()]: a list with `breaks`, `labels`, `limits`, `range`
#'   and `name`. Built with the SAME limits and range as the plot it came
#'   from, so a key circle is drawn at exactly the diameter that value has in
#'   the plot and can be read off it.
#' @return A ggplot object; only its legend is meant to be used.
#' @seealso [coverage-plots], [cat_lookup()]
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

#' An invisible layer that completes a coverage plot's status legend
#'
#' A discrete scale can LIST an unused level (with `drop = FALSE` +
#' `limits`), but a geom has no data row from which to draw that level's
#' key swatch, so absent statuses show as a label with an empty box. This
#' adds one zero-size mark per status - drawn from a copy of the first
#' data row so every mapped column is present, positioned on top of a
#' real cell and sized to nothing so it never shows in the panel - which
#' gives the guide a row for every status and fills every swatch.
#'
#' @param d The data frame the plot is built from (its first row is
#'   copied, so the layer inherits the same x / y / facet columns).
#' @param all_status Every status label, in order - `names(status_palette())`.
#' @param key `"tile"` for the heatmap, `"point"` for the bubble grid.
#' @return A ggplot2 layer.
#' @seealso [status_legend_plot()], [status_palette()]
#' @keywords internal
#' @export
status_key_layer <- function(d, all_status, key = c("tile", "point")) {
  key <- match.arg(key)
  dummy <- d[rep(1L, length(all_status)), , drop = FALSE]
  dummy$Status <- factor(all_status, levels = all_status)
  if ("landings" %in% names(dummy)) dummy$landings <- NA_real_
  if ("plot_size" %in% names(dummy)) dummy$plot_size <- NA_real_
  if (identical(key, "tile"))
    ggplot2::geom_tile(data = dummy, ggplot2::aes(fill = .data$Status),
                       width = 0, height = 0, na.rm = TRUE, show.legend = TRUE)
  else
    ggplot2::geom_point(data = dummy, ggplot2::aes(fill = .data$Status),
                        size = 0, shape = 21, stroke = 0, na.rm = TRUE,
                        show.legend = TRUE)
}

#' Draw a status legend in the current figure's right margin
#'
#' Shared by the base-R heatmap and bubble plots: every possible status
#' from [cat_lookup()], not just the ones present in this particular
#' figure - drawn as an overlay in the bottom-right corner of the figure,
#' on top of the panels, with a semi-opaque background so the marks behind
#' it stay faintly visible.
#'
#' @param lk_all A [cat_lookup()] data frame.
#' @return `NULL`, invisibly.
#' @keywords internal
#' @export
legend_status_base <- function(lk_all) {
  # The compact codes from cat_lookup()$abbr (cov / trip / meas vs their
  # thresholds) rather than the full sentences - short enough to read at
  # a legible size instead of being shrunk to fit.
  labs <- if ("abbr" %in% names(lk_all)) lk_all$abbr else lk_all$status
  legend_overlay("bottomright", legend = labs, fill = lk_all$colour,
                 cex = 0.95, bty = "o",
                 bg = grDevices::adjustcolor("white", 0.9),
                 box.col = "grey65", title = "Status",
                 inset = c(0.02, 0.02), y.intersp = 1.15)
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

#' Facet grid dimensions, capped at two columns
#'
#' Every base-R plot lays its facets out with `par(mfrow = )`. A square-ish
#' grid (`ceiling(sqrt(n))` columns) makes each panel small and its y-axis
#' labels unreadable once there are more than a few facets. This keeps the
#' grid to at most `max_col` columns - two by default - so the panels stay
#' wide and the figure simply grows downward.
#'
#' @param n Number of facets.
#' @param max_col Maximum columns (default 2).
#' @return `c(nrow, ncol)`.
#' @keywords internal
#' @export
facet_grid_dims <- function(n, max_col = 2L) {
  nc <- max(1L, min(as.integer(max_col), as.integer(n)))
  c(ceiling(n / nc), nc)
}

#' Categorical y axis with every label shown
#'
#' `graphics::axis()` silently drops tick labels that would overlap, so a
#' y axis with many categories loses rows and there is no telling which bar
#' or tile is which. This draws the ticks with `axis()` but every label
#' with `graphics::mtext()` (which never thins), shrinking the text just
#' enough to fit the panel height when the categories are dense - down to
#' `min_cex`, past which it stops and the panel is simply too short for
#' that many rows.
#'
#' @param at,labels Tick positions and their labels.
#' @param cex Target character expansion; the drawn size may be smaller.
#' @param col Label colour.
#' @param min_cex Floor on the shrink.
#' @param line Margin line the labels sit on.
#' @return `NULL`, invisibly.
#' @keywords internal
#' @export
axis_y_cat <- function(at, labels, cex = 0.8, col = "grey15",
                       min_cex = 0.45, line = 0.5) {
  graphics::axis(2, at = at, labels = FALSE, tick = TRUE)
  labels <- as.character(labels)
  n <- length(labels)
  if (!n) return(invisible(NULL))
  usr_h <- diff(graphics::par("usr")[3:4])
  need  <- n * graphics::strheight("Mg", units = "user", cex = cex) * 1.28
  use   <- if (is.finite(need) && need > usr_h)
    max(min_cex, cex * usr_h / need) else cex
  graphics::mtext(labels, side = 2, at = at, line = line, las = 1,
                  cex = use, col = col, adj = 1)
  invisible(NULL)
}

#' Draw a legend as an overlay in a figure corner
#'
#' Switches to a whole-figure coordinate system and drops a legend into
#' `pos` (`"bottomright"` for these plots), on top of the panels instead of
#' in a wide reserved outer margin. `...` goes straight to
#' [graphics::legend()]; callers pass their own `bg` / `box.col` / `cex` so
#' the box stays semi-opaque and the marks behind it stay faintly visible.
#'
#' @param pos A [graphics::legend()] keyword position.
#' @param ... Passed to [graphics::legend()].
#' @return `NULL`, invisibly.
#' @keywords internal
#' @export
legend_overlay <- function(pos, ...) {
  graphics::par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),
                new = TRUE)
  graphics::plot.new()
  graphics::legend(pos, ..., xpd = NA)
  invisible(NULL)
}

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
           "6", "6.a", "6.b", "6.b.1", "6.b.2",
           "7", "7.a", "7.b", "7.c", "7.d", "7.e", "7.f", "7.g", "7.h",
           "7.j", "7.k",
           "8", "8.a", "8.b", "8.c", "8.d", "8.e",
           "9", "9.a", "9.b",
           "10.a", "12", "14.a", "14.b",
           # FAO Major Fishing Area 87 (Pacific, Southeast) - so the
           # bundled anchoveta example data can be mapped too
           "87.1.1", "87.1.2", "87.2.4", "87.2.5",
           "87.3.1", "87.3.2", "87.3.3", "87.3.4"),
  lon = c(20.0, 5.0, 0.0,
          10.5, 10.2, 11.6,
          10.7, 12.5, 12.0, 16.5, 19.0, 18.5,
          20.0, 20.5, 20.0, 23.0, 25.0,
          2.5, 1.0, 2.5, 3.2,
          -19.0, -7.0,
          -8.5, -8.0, -13.0, -14.5, -11.0,
          -8.0, -5.3, -10.5, -14.0, 0.5, -4.0, -5.2, -7.7, -7.7,
          -11.0, -14.0,
          -5.0, -3.0, -2.5, -4.2, -6.0, -10.0,
          -10.0, -9.5, -11.0,
          -28.0, -10.0, -35.0, -25.0,
          -82.0, -80.0, -78.0, -73.0,
          -71.0, -73.0, -74.0, -75.0),
  lat = c(73.0, 68.0, 64.0,
          57.5, 58.2, 56.9,
          55.2, 55.5, 55.0, 55.6, 55.6, 58.0,
          57.5, 59.5, 62.0, 64.5, 60.0,
          56.5, 58.6, 55.6, 53.6,
          64.0, 62.0,
          57.5, 57.5, 57.5, 57.5, 56.8,
          51.0, 53.6, 53.6, 52.0, 50.2, 49.8, 51.3, 51.0, 49.6,
          50.6, 49.6,
          45.5, 46.4, 45.0, 44.0, 45.6, 45.5,
          39.0, 40.0, 38.0,
          46.0, 74.0, 60.0, 62.0,
          -6.0, -8.0, -13.0, -17.0,
          -22.0, -30.0, -37.0, -43.0),
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

#' Default weight label for a coverage / distribution plot
#'
#' @param what `"discard"`, `"length"` or `"age"`.
#' @param catchCategory `"Lan"`, `"Dis"`, `"BMS"` or `NULL`.
#' @return `"landings"`, `"discards"` or `"BMS catch"`.
#' @keywords internal
#' @export
default_weight_label <- function(what, catchCategory = NULL) {
  if (identical(what, "discard")) return("landings")
  switch(catchCategory %||% "Lan",
         Dis = "discards", BMS = "BMS catch", All = "catch", "landings")
}

#' Expand a `catchCategory` argument to the catch parts it selects
#'
#' `"All"` means the landings and the discards together; every other
#' value is itself. Used so a plot can title itself "catch" while
#' filtering the distribution / catch rows to both parts.
#'
#' @param cc `"Lan"`, `"Dis"`, `"BMS"`, `"All"` or `NULL`.
#' @return A character vector of `catchCategory` values to keep.
#' @keywords internal
#' @export
catch_category_parts <- function(cc) {
  if (is.null(cc)) return("Lan")
  if (identical(cc, "All")) c("Lan", "Dis") else cc
}

#' Build and classify the strata a coverage plot draws
#'
#' The coverage plots take raw CEF catch (plus `distributions` for
#' length / age) and this turns it into the classified stratum table they
#' render: [build_strata()] + [classify_strata()] for discards, or the
#' full biology pipeline ([biology_link()], [stratum_measurements()],
#' [add_sufficiency()], [classify_strata()]) for length / age.
#'
#' A table that already carries `cat_key` (already through
#' [classify_strata()]) is passed straight through - a narrow convenience
#' for a caller that must pre-aggregate; the documented input is raw
#' catch.
#'
#' @param catch Raw CEF catch data (or an already-classified table).
#' @param dims Columns to build on; defaults to the union of `y`,
#'   `fill`, `facet` when `NULL`.
#' @param y,fill,facet,min_cov,what,min_fish,min_trip As in the coverage
#'   plots.
#' @param distributions A distributions table - required for
#'   `what = "length"`/`"age"`.
#' @param catch_category For `what = "length"`/`"age"`: which catch part
#'   the distribution is linked to - `"Lan"` (default), `"Dis"` or
#'   `"BMS"`.
#' @return A classified strata table.
#' @keywords internal
#' @export
build_coverage_strata <- function(catch, dims, y, fill, facet,
                                  min_cov, what, min_fish, min_trip,
                                  distributions = NULL,
                                  catch_category = NULL) {
  strata <- catch
  if (!is.null(strata) && "cat_key" %in% names(strata)) return(strata)
  has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
  if (is.null(dims))
    dims <- unique(c(y, if (has(fill)) fill, if (has(facet)) facet))
  if (identical(what, "discard"))
    return(classify_strata(build_strata(strata, dims), min_cov, what,
                           min_fish, min_trip))
  # length / age: the full biology pipeline, which needs the
  # distributions table (build_strata() alone cannot do it).
  if (is.null(distributions))
    stop('for what = "', what, '", pass either an already-classified ',
         'length/age table as `strata`, or raw catch data together with ',
         '`distributions` - build_strata() alone does not carry what a ',
         "biology stratum needs (biology_link(), stratum_measurements(), ",
         "add_sufficiency()).")
  dist_type <- if (identical(what, "age")) "Age" else "LengthTotal"
  cc <- catch_category %||% "Lan"
  bl  <- biology_link(strata, distributions, dims, cc, type = dist_type)
  ms  <- stratum_measurements(strata, distributions, dims, cc, type = dist_type)
  bl  <- add_sufficiency(bl, ms, min_cov, min_fish)
  bio <- bl %>% mutate(landings = weight,
                       landings_with_discards = weight_measured,
                       discards = 0, discard_rate = NA_real_)
  classify_strata(bio, min_cov, what, min_fish, min_trip)
}

#' Warn about arguments that do not fit the requested plot
#'
#' The plotting dispatchers accept a wide, shared set of arguments, only
#' some of which apply to any one `engine` / `quantity`. This turns a
#' list of "this argument was set but does nothing here" checks into
#' `warning()`s, so a caller who sets, say, `n_per_screen =` on the
#' ggplot engine hears about it instead of quietly getting something
#' else.
#'
#' Each check is a `list(bad = <logical>, msg = <character>)`; only the
#' ones whose `bad` is `TRUE` are reported. A dispatcher builds the list
#' so that `bad` is `TRUE` only when the argument is at a NON-default
#' value and does not apply - leaving an inapplicable argument alone
#' never warns.
#'
#' @param fn The calling function's name, for the message prefix.
#' @param checks A list of `list(bad, msg)` entries.
#' @return The character vector of emitted messages, invisibly.
#' @keywords internal
#' @export
plot_arg_report <- function(fn, checks) {
  bad <- Filter(function(ch) isTRUE(ch$bad), checks)
  msgs <- vapply(bad, function(ch) as.character(ch$msg), character(1))
  for (m in msgs) warning(fn, "(): ", m, call. = FALSE)
  invisible(msgs)
}

#' Print a plain-language summary of a coverage plot before drawing it
#'
#' What `plot_coverage_heat(..., verbose = TRUE)` (and the bubble / bar
#' siblings) prints: the view and engine, which columns are on which
#' role, the threshold / min_trips in force (or, for bars, that they do
#' not apply), the catch part for a length / age figure, and how many
#' strata rows are about to be drawn.
#'
#' @param strata The already-classified strata table about to be plotted.
#' @param view `"heat"`, `"bubble"` or `"bar"`.
#' @param engine,y,x,facet,min_cov,what,weight_label,min_fish,min_trip
#'   As resolved inside the coverage plot functions ([plot_coverage_heat()]
#'   and siblings).
#' @param catchCategory `"Lan"` / `"Dis"` / `"BMS"` or `NULL`.
#' @param palette,angle,n_per_screen Rendering options, echoed back.
#' @return `NULL`, invisibly - printed as a side effect.
#' @keywords internal
#' @export
coverage_verbose_summary <- function(strata, view, engine, y, x, facet,
                                     min_cov, what, weight_label,
                                     min_fish, min_trip, catchCategory = NULL,
                                     palette = "default", angle = 45,
                                     n_per_screen = NULL) {
  has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
  line <- function(...) cat(..., "\n", sep = "")
  line("-- plot_coverage_", view, "(): ", cov_title_what(what),
       " coverage, engine = \"", engine, "\" --")
  line("  y      : ", if (has(y)) y else "(none)")
  line("  ", if (identical(view, "bar")) "fill   : " else "x      : ",
       if (has(x)) x else "(none)")
  line("  facet  : ", if (has(facet)) facet else "(none)")
  line("  weight : ", weight_label)
  if (!is.null(catchCategory))
    line("  catchCategory : ", catchCategory)
  if (identical(view, "bar")) {
    line("  min_cov / min_trip: not used by a bar chart - it shows ",
         "sampled vs unsampled weight, which neither affects.")
  } else {
    line("  min_cov    : ", min_cov, "%")
    line("  min_trip   : ", min_trip)
  }
  if (!is.na(min_fish) && min_fish > 0)
    line("  min_fish   : ", min_fish)
  line("  render : engine = ", engine, ", palette = ", palette,
       ", angle = ", angle,
       if (!is.null(n_per_screen)) paste0(", n_per_screen = ", n_per_screen)
       else "")
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
#' @seealso [prep_distributions()], [plot_numbers_at_length_age()]
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

#' Link + summarise a biology distribution for the plot functions
#'
#' The shared front half of [plot_numbers_at_length_age()] and
#' [plot_meanweight_at_length_age()]: when `distributions` is given, run
#' [link_dist_to_strata()] then [length_numbers()] / [length_meanweight()]
#' (numbers are scaled to thousands, N3); otherwise `data` is already a
#' summarised table and is used unchanged. Returns the axis labels that
#' match.
#'
#' @param data A summarised numbers / mean-weight table, or raw CEF catch.
#' @param distributions A CEF distributions table, or `NULL`.
#' @param distributionType `"LengthTotal"` or `"Age"`.
#' @param catchCategory `"Lan"`, `"Dis"` or `"BMS"`.
#' @param quantity `"numbers"` or `"meanweight"`.
#' @param grp Grouping columns for the summary.
#' @param dims Stratum dimensions for the link; a default is used when `NULL`.
#' @return `list(data, class_label, value_label)`.
#' @keywords internal
#' @export
biology_dist_prep <- function(data, distributions, distributionType,
                              catchCategory, quantity, grp, dims) {
  lab <- biology_axis_labels(distributionType, quantity)
  if (!is.null(distributions)) {
    if (is.null(dims))
      dims <- unique(c("catchCategory", "fleetValue", "Country", "Season", grp))
    dims <- intersect(dims, names(as.data.frame(data)))
    linked <- link_dist_to_strata(data, distributions, dims, catchCategory,
                                  type = distributionType)
    data <- if (identical(quantity, "numbers"))
      length_numbers(linked, grp) else length_meanweight(linked, grp)
    if (!is.null(data) && nrow(data) && identical(quantity, "numbers"))
      data$number <- data$number * lab$value_scale
  }
  list(data = data, class_label = lab$class_label, value_label = lab$value_label)
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
