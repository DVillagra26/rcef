# ============================================================
#  viz_helpers.R
#  Shared plumbing for R/viz_plots.R: faceting layers, the status legend,
#  weight/catch-part wording, the biology distribution front half and the
#  ICES-area centroid table. The plot functions themselves are in
#  viz_plots.R, the coverage table they draw from in rcef_coverage.R -
#  this file is the small machinery they lean on. ggplot only.
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
#' @param drop_none Drop the "none" (no weight at all) category, which
#'   the coverage plots never draw. Default `TRUE`.
#' @family coverage plots
#' @export
status_legend_plot <- function(threshold, what = "discard", min_meas = 0,
                               key = c("point", "tile"), size = NULL,
                               min_trips = 2, drop_none = TRUE) {
  key <- match.arg(key)
  lk <- cat_lookup(threshold, what, min_meas, min_trips)
  # A stratum with no weight draws no cell on the coverage plots, so its
  # "none" category has no swatch to explain - dropped, matching the
  # plots' own legends, unless a caller asks for the full table.
  if (isTRUE(drop_none)) lk <- lk[lk$cat_key != "none", , drop = FALSE]
  d <- data.frame(.x = seq_len(nrow(lk)), .y = 1,
                  Status = factor(lk$status, levels = lk$status))
  p <- ggplot(d, aes(x = .data$.x, y = .data$.y, fill = .data$Status)) +
    # the key glyph comes from the geom, so the legend matches the plot:
    # a rect for the heatmap, a filled circle for the bubbles
    (if (identical(key, "tile")) geom_tile(colour = OUTLINE, alpha = 0.9)
     else geom_point(shape = 21, colour = OUTLINE)) +
    scale_fill_manual(values = setNames(lk$colour, lk$status),
                      limits = lk$status,
                      # the compact codes, like the plots' own legends - a
                      # full sentence per combination is too wide once
                      # trips and measurements are both active
                      labels = setNames(lk$abbr, lk$status),
                      drop = FALSE, name = "Sampling status")
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
