# =====================================================================
#  viz_plots.R   (coverage / discard-rate / biology-distribution plots)
#
#  ORIGIN: this project (ILVO / RCEF Workbench). Freely refactorable.
#  Was: R_development/0_Functions_plots.R
#  No shiny in this file. If it needs `input`, `session` or a tag,
#  it belongs in app/, not here.
# =====================================================================

# ============================================================
#  Exploratory plots for the RCEF / discard-raising workflow.
# ------------------------------------------------------------
#  ggplot2 only. Nothing here reads app state: every function takes a
#  prepared data frame and returns a ggplot object (or NULL when there
#  is nothing to draw), so the same call works at the console and in
#  the app.
#
#  PUBLIC API (one function per view, one exploratory question each):
#    plot_coverage_heat/_bubble/_bar/_map  how well a set of strata is
#                                sampled - `what` switches discard / length /
#                                age, `catchCategory` the landings / discards
#                                part for length & age.
#    plot_discard_rate()         per-stratum discard-rate points.
#    plot_numbers_at_length_age()   numbers at length/age (histogram).
#    plot_meanweight_at_length_age() mean weight at length/age (line + points).
#
#  The coverage and discard-rate plots draw the table that
#  build_coverage_strata() (R/rcef_coverage.R) returns. Given raw CEF
#  catch (and `distributions` for length / age) they build that table at
#  exactly the columns they plot; given the table itself they use it -
#  so the numbers behind any of these figures can be read as a table too.
#  The sampling STATUS (the colour of a heatmap cell or a bubble) is
#  worked out here, at plot time, from the thresholds - it is not a
#  column of the table.
#
#  The shared plumbing (facet layers, status legend, catch-part wording,
#  the biology distribution front half) lives in R/viz_helpers.R; the
#  raising flow diagrams (plot_river(), plot_river_level(),
#  plot_comparison()) live in R/viz_raising.R.
#
#  PACKAGE DEPENDENCIES
#    Imports: ggplot2, dplyr, tidyr, ggrepel, scales, rlang, stats, tools
#    Provided by the rest of the package (not redefined here, so nothing is
#    duplicated): build_coverage_strata() and the coverage_cells() /
#    coverage_status() that pool and judge it, cat_lookup() /
#    status_palette() for the status colours; app_theme() for the shared
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
#  Coverage map (ICES-area pies)
# ----------------------------------------------------------

#' Coverage map: one pie per ICES area
#'
#' @description
#' A map of the fishing area with one small pie per ICES area: the pie's
#' radius is the area's landed (or caught) weight, and the pie is split into
#' the sampled and the not-sampled share of that weight. With `group` (for
#' example `"Season"`, `"Country"` or `"metier4"`) each area gets a labelled
#' box holding a tidy grid of pies, one per group level; overlapping boxes
#' are nudged apart and a thin leader keeps each box tied to its area's true
#' position.
#'
#' @details
#' "Sampled" means what it means in the other coverage plots (see
#' [coverage-plots]): the weight carries a discard estimate (`what =
#' "discard"`) or a linked length / age distribution. Like them, the map takes
#' raw catch (plus `distributions` for length and age) or a
#' [build_coverage_strata()] table that has the `area` column (and the
#' `group` column) among its `dims`.
#'
#' Areas are placed at approximate centroids ([ices_area_centroid()]); an area
#' with no known centroid is warned about by name and left off the map. The
#' map needs the suggested `maps` package.
#'
#' @param catch Raw CEF catch data, or a [build_coverage_strata()] table
#'   (its own `what` is then used).
#' @param distributions A CEF distributions table - needed for
#'   `what = "length"` / `"age"` with raw catch, ignored otherwise.
#' @param area Column holding the ICES area code. Default `"areaValue"`.
#' @param group Optional column: one boxed grid of pies per area, one pie per
#'   level. A `group` with a single level warns and draws one pie per area.
#' @param what,catchCategory As in [coverage-plots]. The weight word in the
#'   title comes from `what` and `catchCategory`.
#' @param scale Multiplies every pie radius (and, when grouped, the size of
#'   the box's grid cell).
#' @return A ggplot object, or `NULL` (with a message) when nothing can be
#'   mapped.
#' @seealso [coverage-plots] for the heatmap, bubbles and bars;
#'   [build_coverage_strata()] for the table behind the pies.
#' @family coverage plots
#' @md
#' @examples
#' if (requireNamespace("maps", quietly = TRUE)) {
#'   data(anchoveta_catch)
#'   plot_coverage_map(anchoveta_catch)
#'   plot_coverage_map(anchoveta_catch, group = "Season")
#' }
#' @export
plot_coverage_map <- function(catch, distributions = NULL, area = "areaValue",
                              group = NULL, what = "discard",
                              catchCategory = NULL, scale = 1) {
  fn <- "plot_coverage_map"
  area_var <- area; group_var <- group; bubble_scale <- scale
  has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
  if (!requireNamespace("maps", quietly = TRUE))
    stop("plot_coverage_map() needs the 'maps' package: install.packages(\"maps\")")
  keys <- c(area_var, if (has(group_var)) group_var)
  cv <- coverage_cells(catch, distributions, keys, what, !missing(what),
                       catchCategory, fn)
  if (is.null(cv)) {
    message("plot_coverage_map(): nothing to map.")
    return(invisible(NULL))
  }
  what <- cv$what; weight_label <- cv$weight_label
  sc <- cv$cells
  # per (area x group) cell: the weight, and the part of it that is sampled
  agg <- data.frame(sc[keys],
                    w   = if (identical(what, "discard")) sc$landings else sc$total,
                    cov = if (identical(what, "discard")) sc$weightingTotal else sc$covered,
                    stringsAsFactors = FALSE)
  agg <- agg[!is.na(agg$w) & agg$w > 0, , drop = FALSE]
  cen <- ices_area_centroid(agg[[area_var]])
  agg$lon <- cen$lon; agg$lat <- cen$lat
  agg <- agg[!is.na(agg$lon), , drop = FALSE]
  if (!nrow(agg)) { message("plot_coverage_map(): no mappable areas."); return(invisible(NULL)) }
  agg$frac <- pmin(1, pmax(0, agg$cov / agg$w))

  lev <- NULL; ngrp <- 1L
  if (has(group_var)) {
    lev  <- sort(unique(as.character(agg[[group_var]])))
    ngrp <- length(lev)
    if (ngrp <= 1)
      warning("plot_coverage_map(): group_var = \"", group_var,
              "\" has a single level in the data; drawing one pie per area",
              call. = FALSE)
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
  ttl  <- sprintf("%s coverage by %s%s%s", cov_title_what(what), area_var,
                  if (has(group_var)) paste0(" and ", group_var) else "",
                  if (identical(what, "discard")) ""
                  else paste0(" (", weight_label, ")"))

  slice_xy <- function(cx, cy, r, a0, a1) {
    asp <- 1 / cos(cy * pi / 180)
    th  <- seq(a0, a1, length.out = 48)
    list(x = cx + c(0, r * asp * cos(th)), y = cy + c(0, r * sin(th)))
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


#' Coverage: which strata are sampled well enough
#'
#' @description
#' Four views of how well a set of strata is sampled: a heatmap
#' (`plot_coverage_heat()`), a weighted bubble grid (`plot_coverage_bubble()`)
#' and a sampled-versus-unsampled weight bar chart (`plot_coverage_bar()`),
#' all documented here, plus a map ([plot_coverage_map()]) on its own page.
#'
#' All take raw CEF catch (and `distributions` for length and age) or the
#' table [build_coverage_strata()] returns from it. Given raw catch they build
#' that table at exactly the plotted columns; given the table they pool it to
#' them. So the numbers behind a figure can always be read as a table, at the
#' same aggregation.
#'
#' @details
#' ## What "sampled" means
#'
#' `what` chooses the question:
#'
#' * `"discard"` (default): does the stratum have a discard estimate? Its
#'   landings carry a `domainCatchDis`.
#' * `"length"` / `"age"`: is a length / age distribution linked to the
#'   stratum's catch? Needs `distributions`. `catchCategory` chooses the catch
#'   part - `"Lan"`, `"Dis"`, `"BMS"`, or `"All"` (the default: landings and
#'   discards together, titled "catch").
#'
#' The plural and mixed case are accepted (`"discards"`, `"Length"`). A
#' coverage table brings its own `what` and catch parts, so with one neither
#' needs to be given; a `what` that contradicts the table is an error.
#'
#' ## The status scale (heatmap and bubbles)
#'
#' The colour of a cell is its sampling status against the thresholds:
#'
#' | Colour | Status |
#' |:-------|:-------|
#' | dark green | 100% covered (and, when trips are active, more than `min_trip` trips) |
#' | green | covered at least `min_cov`, and passing every active threshold |
#' | yellow | one active threshold missed |
#' | orange | two active thresholds missed |
#' | dark orange | three missed: only for length / age, with both `min_trip` and `min_fish` active |
#' | red | no discard estimate / no linked distribution at all - never a threshold miss |
#' | blue | discards only, no landings (discard coverage) |
#'
#' A cell with no weight at all draws nothing. The legend always lists every
#' reachable status, including those no cell falls in, so the same figure
#' drawn on two subsets has the same key.
#'
#' Coverage (`min_cov`) is always on. `min_trip` (discard, length, age) and
#' `min_fish` (length, age) both default to `NULL` and are each optional:
#' `NULL` drops that question from the classification and the legend, a
#' number turns it on. The label says which threshold(s) a cell missed, so
#' `"< 50% covered"` and `">= 50% covered, < 2 trips"` are both yellow but
#' read differently.
#'
#' `min_trip = 0` is not the same as `NULL`: it is an active (if lenient)
#' threshold, and a stratum with no reported trip count fails an active trips
#' test however low it is set - unknown is not treated as adequate. A fully
#' covered stratum with no trip count reads "safe" at `min_trip = NULL` but
#' stays a warning at `min_trip = 0` (see [classify_strata()]).
#'
#' ## Bars and palettes
#'
#' The bar chart shows sampled versus unsampled weight, which no threshold
#' changes, so it takes none: passing `min_cov` or `min_trip` is an error.
#' `palette` recolours the bars; the status colours are a fixed scale, so
#' `palette = "colorblind"` on the heatmap or bubbles warns.
#'
#' ## Argument order
#'
#' What data (`catch`, `distributions`, `what`, `catchCategory`), then which
#' columns are the strata (`y`, `x` or `fill`, `facet`), then the thresholds
#' (`min_cov`, `min_trip`, `min_fish`), then `palette`.
#'
#' @param catch Raw CEF catch data, or a table from
#'   [build_coverage_strata()].
#' @param distributions A CEF distributions table - needed for
#'   `what = "length"` / `"age"` with raw catch, ignored otherwise.
#' @param what `"discard"` (default), `"length"` or `"age"`. Taken from the
#'   table when `catch` is a coverage table.
#' @param catchCategory For `what = "length"` / `"age"`: `"All"` (default),
#'   `"Lan"`, `"Dis"` or `"BMS"`. Sets which catch part the distribution is
#'   linked to and the weight word in the plot title. With a coverage table
#'   it selects among the parts the table has. Not valid for
#'   `what = "discard"` (an error).
#' @param y Row category: a column of the catch (or of the table). Required.
#' @param x Column category, the second axis of the heatmap / bubble grid.
#'   `NULL` collapses to a single column.
#' @param fill Bar fill category (bars only). `NULL` uses `y`.
#' @param facet Optional facet column.
#' @param min_cov Minimum coverage, in percent (heatmap and bubbles).
#'   Always active: coverage is never dropped from the classification.
#' @param min_trip Minimum sampling trips (heatmap and bubbles), or `NULL`
#'   (the default) to leave trips out of the classification altogether.
#'   `0` is not the same as `NULL`: it still checks trips, and a stratum with
#'   no reported trip count still fails that check.
#' @param min_fish Minimum measurements for `what = "length"` / `"age"`, or
#'   `NULL` (the default) to leave measurements out of the classification.
#' @param palette `"default"` or `"colorblind"`. Recolours the bar fill; on
#'   the heatmap and bubbles the status colours are fixed, and asking for
#'   `"colorblind"` warns.
#' @return A ggplot object, or `NULL` when there is nothing to draw. The
#'   heatmap and bubbles also carry a `"status_legend"` attribute: the
#'   specification [status_legend_plot()] rebuilds a complete legend from.
#' @seealso [build_coverage_strata()] for the table, [plot_coverage_map()],
#'   [plot_discard_rate()]; [classify_strata()] and [cat_lookup()] for the
#'   classification behind the colours.
#' @name coverage-plots
#' @family coverage plots
#' @md
#' @examples
#' data(anchoveta_catch)
#' data(anchoveta_dist)
#'
#' # discard coverage: which metier x country strata carry a discard estimate?
#' plot_coverage_heat(anchoveta_catch, y = "metier4", x = "Country")
#' plot_coverage_bubble(anchoveta_catch, y = "metier4", x = "Country",
#'                      min_cov = 50, min_trip = 2)
#' plot_coverage_bar(anchoveta_catch, y = "metier4", fill = "Country")
#'
#' # length coverage of the landings, with a floor on the measurements
#' plot_coverage_heat(anchoveta_catch, anchoveta_dist, y = "metier4",
#'                    x = "Country", what = "length", catchCategory = "Lan",
#'                    min_cov = 50, min_fish = 100)
#'
#' # the same figure from the table behind it
#' cov <- build_coverage_strata(anchoveta_catch, c("metier4", "Country"))
#' plot_coverage_heat(cov, y = "metier4", x = "Country", min_cov = 50)
NULL


#' @rdname coverage-plots
#' @export
plot_coverage_heat <- function(catch, distributions = NULL, y, x = NULL,
                               facet = NULL, what = "discard",
                               catchCategory = NULL,
                               min_cov = 50, min_trip = NULL, min_fish = NULL,
                               palette = c("default", "colorblind")) {
  fn <- "plot_coverage_heat"
  palette <- match.arg(palette)
  if (is.null(y) || !nzchar(y)) stop(fn, "(): `y` is required", call. = FALSE)
  if (identical(palette, "colorblind"))
    warning(fn, "(): palette does not recolour a status heatmap / bubble grid ",
            "(the cells use a fixed semantic scale)", call. = FALSE)

  cv <- coverage_cells(catch, distributions, c(y, x, facet), what,
                       !missing(what), catchCategory, fn)
  if (is.null(cv)) return(NULL)
  what <- cv$what; weight_label <- cv$weight_label
  y_var <- y; x_var <- x; facet_var <- facet; threshold <- min_cov
  min_meas <- min_fish; min_trips <- min_trip

  # A cell with no landed/caught weight at all draws no tile - it has
  # nothing to judge, so it stays a blank hole rather than a "no
  # landings" placeholder.
  d <- coverage_status(cv$cells, what, threshold, min_trips, min_meas)
  d <- d[d$cat_key != "none", , drop = FALSE]
  if (!nrow(d)) return(NULL)
  if (is.null(x_var) || !nzchar(x_var)) { d$.x <- "All"; x_var <- ".x" }
  fmt <- function(v) ifelse(is.na(v) | v <= 0, "",
                     ifelse(v > 99, sprintf("%.0f", v),
                     ifelse(v >= 10, sprintf("%.1f", v), sprintf("%.2f", v))))
  # "none" never draws a tile, so it is dropped from the legend too.
  lk_no_none <- cat_lookup(threshold, what, min_meas, min_trips)
  stat_all <- lk_no_none$status[lk_no_none$cat_key != "none"]
  d$Status <- factor(as.character(d$Status), levels = stat_all)
  p <- ggplot(d, aes(x = factor(.data[[x_var]]), y = .data[[y_var]], fill = Status)) +
    geom_tile(colour = OUTLINE, alpha = 0.9) +
    geom_text(aes(label = fmt(landings)), size = 2.6, fontface = "bold",
              colour = OUTLINE) +
    # a zero-size mark per status, so the legend carries a filled key for
    # categories that no cell in this particular figure lands in - added
    # last so the real tiles stay layer 1
    status_key_layer(d, stat_all, "tile") +
    scale_fill_manual(values = status_palette(threshold, what, min_meas, min_trips),
                      limits = stat_all,
                      labels = status_abbr(threshold, what, min_meas, min_trips),
                      drop = FALSE, name = "Sampling status") +
    maybe_facet(facet_var) +
    labs(title = sprintf("%s sampling coverage vs threshold%s",
                         cov_title_what(what),
                         if (identical(what, "discard")) ""
                         else paste0(" (", weight_label, ")")),
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


#' @rdname coverage-plots
#' @export
plot_coverage_bubble <- function(catch, distributions = NULL, y, x = NULL,
                                 facet = NULL, what = "discard",
                                 catchCategory = NULL,
                                 min_cov = 50, min_trip = NULL, min_fish = NULL,
                                 palette = c("default", "colorblind")) {
  fn <- "plot_coverage_bubble"
  palette <- match.arg(palette)
  if (is.null(y) || !nzchar(y)) stop(fn, "(): `y` is required", call. = FALSE)
  if (identical(palette, "colorblind"))
    warning(fn, "(): palette does not recolour a status heatmap / bubble grid ",
            "(the cells use a fixed semantic scale)", call. = FALSE)

  cv <- coverage_cells(catch, distributions, c(y, x, facet), what,
                       !missing(what), catchCategory, fn)
  if (is.null(cv)) return(NULL)
  what <- cv$what; weight_label <- cv$weight_label
  y_var <- y; x_var <- x; facet_var <- facet; threshold <- min_cov
  min_meas <- min_fish; min_trips <- min_trip

  # A cell with no landed/caught weight at all draws no bubble - it has
  # nothing to judge, so it stays a blank hole rather than a "no
  # landings" placeholder. One bubble per cell: the cells are already
  # pooled to the plotted columns, so no two bubbles share a point.
  d <- coverage_status(cv$cells, what, threshold, min_trips, min_meas)
  d <- d[d$cat_key != "none", , drop = FALSE]
  if (!nrow(d)) return(NULL)
  if (is.null(x_var) || !nzchar(x_var)) { d$.x <- "All"; x_var <- ".x" }
  smallest <- suppressWarnings(min(d$landings[d$landings > 0], na.rm = TRUE))
  if (!is.finite(smallest)) smallest <- 1
  d$plot_size <- ifelse(d$cat_key == "disc_only", smallest, d$landings)
  if (what %in% c("length", "age")) {
    # say WHY a bubble is a warning: the coverage when it is short of the
    # coverage threshold, the measurement count when it is short of the
    # measurement threshold, both when it misses both
    cov_bit <- ifelse(!is.na(d$coverage) & d$coverage < threshold,
                      paste0(round(d$coverage, 1), "%"), "")
    mea_bit <- if (thr_active(min_meas))
      ifelse(!is.na(d$n_measurements) & d$n_measurements < min_meas,
            paste0("n=", format(round(d$n_measurements),
                                big.mark = ",", trim = TRUE)), "") else ""
    trp_bit <- if (thr_active(min_trips) && "numTrips" %in% names(d))
      ifelse(!is.na(d$numTrips) & d$numTrips < min_trips,
             paste0(round(d$numTrips), " trip",
                    ifelse(round(d$numTrips) == 1, "", "s")), "") else ""
    # Label only the cells a reader has to ACT on: the warning shades
    # (one, two or three targets short), with their coverage / trip /
    # measurement shortfalls spelled out. The SAFE cells need no
    # explanation; the "no distribution linked" cells (red) have no
    # numbers to show.
    lab_me <- grepl("^warn_", d$cat_key)
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
    trp_bit <- if (thr_active(min_trips) && "numTrips" %in% names(d))
      ifelse(!is.na(d$numTrips) & d$numTrips < min_trips,
             paste0(round(d$numTrips), " trip",
                    ifelse(round(d$numTrips) == 1, "", "s")), "") else ""
    d$lab <- with(d, case_when(
      cat_key == "disc_only" ~ "100%\nDisc",
      grepl("^warn_", cat_key) ~
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
  # "none" never draws a bubble, so it is dropped from the legend too.
  lk_no_none <- cat_lookup(threshold, what, min_meas, min_trips)
  stat_all <- lk_no_none$status[lk_no_none$cat_key != "none"]
  d$Status <- factor(as.character(d$Status), levels = stat_all)
  p <- ggplot(d, aes(x = factor(.data[[x_var]]), y = .data[[y_var]])) +
    geom_point(aes(size = plot_size, fill = Status), shape = 21,
               colour = OUTLINE, stroke = 0.8) +
    geom_point(data = filter(d, cat_key == "disc_only"),
               shape = 4, colour = OUTLINE, size = 2, stroke = 1) +
    ggrepel::geom_text_repel(aes(label = lab), size = 2.8, fontface = "bold",
                             colour = "red3", box.padding = 0.6,
                             min.segment.length = 0, segment.size = 0.2,
                             segment.colour = "grey40", max.overlaps = Inf) +
    # a zero-size mark per status, so the legend carries a filled key for
    # categories no bubble in this particular figure lands in - added
    # last so the real points stay layer 1
    status_key_layer(d, stat_all, "point") +
    scale_size_continuous(range = SZ$range, limits = SZ$limits,
                          breaks = SZ$breaks, labels = SZ$labels,
                          name = SZ$name) +
    scale_fill_manual(values = status_palette(threshold, what, min_meas, min_trips),
                      limits = stat_all,
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


#' @rdname coverage-plots
#' @export
plot_coverage_bar <- function(catch, distributions = NULL, y, fill = NULL,
                              facet = NULL, what = "discard",
                              catchCategory = NULL,
                              palette = c("default", "colorblind")) {
  fn <- "plot_coverage_bar"
  palette <- match.arg(palette)
  if (is.null(y) || !nzchar(y)) stop(fn, "(): `y` is required", call. = FALSE)

  # No thresholds here: a bar chart shows sampled vs not-sampled WEIGHT,
  # and the shortfall a threshold would miss never reaches its colouring
  # (that is the `fill` variable, not a status) - so nothing is judged.
  cv <- coverage_cells(catch, distributions, c(y, fill, facet), what,
                       !missing(what), catchCategory, fn)
  if (is.null(cv)) return(NULL)
  what <- cv$what; weight_label <- cv$weight_label
  y_var <- y; fill_var <- fill; facet_var <- facet

  # the weight of a cell, and the part of it that carries an estimate /
  # distribution: what the two bars per cell are made of
  d <- cv$cells
  if (!identical(what, "discard")) d$landings <- d$total
  d$landings_with_discards <- if (identical(what, "discard")) d$weightingTotal
                              else d$covered
  d <- d[!is.na(d$landings) & d$landings > 0, , drop = FALSE]
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
                   name = if (has(fill_var)) fill_var else y_var,
                   palette = palette) +
    (if (has(facet_var)) facet_wrap(vars(.facet), scales = "free_y", ncol = 2)
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

# ----------------------------------------------------------
#  Discard rate
# ----------------------------------------------------------

#' Discard rate per stratum
#'
#' @description
#' One point per stratum, its discard rate on the y axis and its landings as
#' the point size. Change `x`, `colour`, `shape` or `facet` and the strata -
#' and the points - are rebuilt at that resolution: a point is never an
#' average of finer strata.
#'
#' @details
#' ## What a point is
#'
#' A stratum is one row of the discard coverage table
#' ([build_coverage_strata()]). Given raw catch, the table is built at
#' exactly the plotted columns (`x` + `colour` + `shape` + `facet`); given a
#' discard coverage table, its rows are drawn as they are, so a table built
#' finer than the plotted columns shows one point per finer stratum. Strata
#' with no discard estimate have no rate and are not drawn: a rate is a
#' property of sampled strata only.
#'
#' ## The rate
#'
#' `rate` is `total / weightingTotal`: the stratum's discards over the
#' landings that carry a discard estimate, in percent - the same number the
#' CAS output reports. It is a ratio, so it can exceed 100%. It is not the
#' `discard_rate` of [build_strata()] (discards / (landings + discards)).
#'
#' ## The y axis
#'
#' The axis is pseudo-log: linear near zero (zero stays zero) and
#' logarithmic above about 5%. A few tiny strata (a few kilograms of covered
#' landings) give ratios of many hundreds of percent that would flatten
#' everything else on a plain axis. Add `+ ggplot2::scale_y_continuous()` to
#' the returned plot to replace it with an ordinary one.
#'
#' @param catch Raw CEF catch data, or a discard coverage table from
#'   [build_coverage_strata()].
#' @param x Column on the x axis: one point per level (times `colour`,
#'   `shape` and `facet`). Required.
#' @param colour Column mapped to the point fill.
#' @param shape Column mapped to the point shape.
#' @param facet Optional facet column.
#' @param jitter Jitter the points horizontally. Cosmetic only - one point is
#'   still one stratum.
#' @param palette `"default"` or `"colorblind"`: the point colours (see
#'   [var_palette()]).
#' @return A ggplot object, or `NULL` when no stratum has a discard rate.
#' @seealso [build_coverage_strata()] for the table behind the points;
#'   [coverage-plots] for how well those strata are sampled.
#' @family discard-rate plots
#' @md
#' @examples
#' data(anchoveta_catch)
#'
#' plot_discard_rate(anchoveta_catch, x = "metier4", colour = "Country")
#' plot_discard_rate(anchoveta_catch, x = "Country", colour = "metier4",
#'                   facet = "Season")
#'
#' # the table behind the points
#' cov <- build_coverage_strata(anchoveta_catch, c("metier4", "Country"))
#' head(cov[!is.na(cov$rate), c("metier4", "Country", "total", "rate")])
#' @export
plot_discard_rate <- function(catch, x, colour = NULL, shape = NULL,
                              facet = NULL, jitter = TRUE,
                              palette = c("default", "colorblind")) {
  palette <- match.arg(palette)
  has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
  if (!has(x)) stop("plot_discard_rate(): `x` is required", call. = FALSE)
  x_var <- x; colour_var <- colour; shape_var <- shape; facet_var <- facet
  keys <- unique(c(x, if (has(colour)) colour, if (has(shape)) shape,
                   if (has(facet)) facet))

  tw <- coverage_table_what(catch)
  if (is.na(tw)) {
    d <- build_coverage_strata(catch, keys)
  } else if (identical(tw, "discard")) {
    d <- catch
  } else {
    stop("plot_discard_rate(): `catch` is a ", tw, " coverage table - a ",
         "discard rate needs raw catch or a discard coverage table",
         call. = FALSE)
  }
  if (is.null(d)) return(NULL)
  miss <- setdiff(keys, names(d))
  if (length(miss))
    stop("plot_discard_rate(): column(s) not in the coverage table: ",
         paste(miss, collapse = ", "), call. = FALSE)

  # rate is NA for a stratum with no discard estimate (nobody looked -
  # not the same as looked and found none), so those simply are not drawn
  d <- d[!is.na(d$rate), , drop = FALSE]
  if (!nrow(d)) return(NULL)
  # A point is one stratum - sizing it by that stratum's landings turns
  # "how much of the catch is this dot" from a question the reader has to
  # go look up into something the plot itself answers. Off when landings
  # is degenerate (every value 0 or NA).
  has_landings <- any(is.finite(d$landings) & d$landings > 0)

  pos <- if (jitter) position_jitter(width = 0.18, height = 0, seed = 1) else "identity"
  ae <- aes(x = .data[[x_var]], y = rate * 100)
  if (has(colour_var)) ae <- modifyList(ae, aes(fill = .data[[colour_var]]))
  if (has(shape_var))  ae <- modifyList(ae, aes(shape = .data[[shape_var]]))
  if (has_landings)    ae <- modifyList(ae, aes(size = landings))

  # only shapes 21-25 take a fill, so pin shape 21 when nothing maps to shape.
  # size is either mapped (added to ae above, scaled below) or fixed at the
  # old constant - never both, which is what a geom_point(size = ...) call
  # alongside an aes(size = ...) mapping would otherwise silently conflict on.
  pt_args <- list(alpha = 0.85, colour = OUTLINE, stroke = 0.5, position = pos)
  if (!has(shape_var))  pt_args$shape <- 21
  if (!has_landings)    pt_args$size  <- 3
  p <- ggplot(d, ae) + do.call(geom_point, pt_args)
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
  if (has(colour_var))
    p <- p + scale_fill_var(colour_var, d[[colour_var]], name = colour_var,
                            palette = palette)
  # area-, not radius-, proportional - the honest way to size a point by
  # a weight, and the same convention the coverage bubble plots already use.
  if (has_landings)
    p <- p + scale_size_area(max_size = 8, labels = scales::comma,
                             name = "Landings (t)")

  # A rate is a ratio, so it can exceed 100% - and a few tiny strata (a
  # few kg of covered landings) give ratios of many hundreds of percent
  # that on a plain axis flatten everything else against the floor.
  # Pseudo-log: linear near zero (zero stays zero), logarithmic above
  # ~5%, so the bulk and the extremes are both readable.
  p +
    scale_y_continuous(
      transform = scales::transform_pseudo_log(sigma = 5),
      breaks = c(0, 5, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000),
      labels = function(z) paste0(round(z), "%")) +
    maybe_facet(facet_var, free_y = FALSE, ncol = 2) +
    labs(title = "Discard rate - one point per stratum",
         subtitle = paste0("One point per x / colour / shape / facet stratum ",
                           "with a discard estimate; rate = discards / landings",
                           if (has_landings) ", point size = landings" else ""),
         y = "Discard rate", x = x_var,
         fill = if (has(colour_var)) colour_var else NULL,
         shape = if (has(shape_var)) shape_var else NULL) +
    app_theme() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.text = element_text(size = 11),
          legend.key.size = unit(1.1, "lines"))
}


# ----------------------------------------------------------
#  Length / age distribution plots
# ----------------------------------------------------------

#' Numbers at length or age
#'
#' @description
#' The catch numbers spread over the length (or age) classes, as a stacked or
#' dodged histogram. Numbers are in thousands, length in cm and age in years.
#'
#' @details
#' `data` is either raw CEF catch together with `distributions` - linked
#' through `domainBiology` and summed inside, with `catchCategory` choosing
#' the landings, the discards, BMS or the catch as a whole - or a table
#' already summarised by `length_numbers()`. When `distributions` is given the
#' catch part goes in the plot title.
#'
#' `fill` colours (and stacks) the bars by a column of the catch. `facet`
#' takes one column (a wrapped set of panels) or two (`c("row", "col")`, a
#' grid).
#'
#' @param data Raw CEF catch data, or a `length_numbers()` table.
#' @param distributions A CEF distributions table. Needed with raw catch;
#'   leave it out when `data` is already a `length_numbers()` table.
#' @param distributionType `"LengthTotal"` (default) or `"Age"`.
#' @param catchCategory `"All"` (default: landings and discards together,
#'   titled "catch"), `"Lan"`, `"Dis"` or `"BMS"`. Only used with
#'   `distributions`.
#' @param fill Column mapped to the bar fill.
#' @param facet Up to two facet columns: `"col"` or `c("row", "col")`.
#' @param stacked Stack the bars on `fill` (default) rather than dodge them.
#' @param scales Facet scales: `"fixed"` (default), `"free_x"`, `"free_y"` or
#'   `"free"`.
#' @param palette `"default"` or `"colorblind"`.
#' @return A ggplot object, or `NULL` when there is nothing to draw.
#' @seealso [plot_meanweight_at_length_age()] for the mean weight in the same
#'   classes; [coverage-plots] for whether the strata have a distribution at
#'   all.
#' @family distribution plots
#' @md
#' @examples
#' data(anchoveta_catch)
#' data(anchoveta_dist)
#'
#' # numbers at length of the landings, stacked by country
#' plot_numbers_at_length_age(anchoveta_catch, anchoveta_dist,
#'                            catchCategory = "Lan", fill = "Country")
#'
#' # numbers at age, one panel per quarter, dodged
#' plot_numbers_at_length_age(anchoveta_catch, anchoveta_dist,
#'                            distributionType = "Age", fill = "Country",
#'                            facet = "Season", stacked = FALSE)
#' @export
plot_numbers_at_length_age <- function(data, distributions = NULL,
                                       distributionType = c("LengthTotal", "Age"),
                                       catchCategory = "All",
                                       fill = NULL, facet = NULL,
                                       stacked = TRUE, scales = "fixed",
                                       palette = c("default", "colorblind")) {
  typed_dt <- !missing(distributionType); typed_cc <- !missing(catchCategory)
  dims <- NULL
  distributionType <- match.arg(distributionType)
  palette <- match.arg(palette)
  catchCategory <- match.arg(catchCategory, c("Lan", "Dis", "BMS", "All"))
  if (length(facet) > 2)
    stop("plot_numbers_at_length_age(): `facet` takes at most two columns",
         call. = FALSE)
  facet1 <- if (length(facet) >= 1) facet[1] else NULL
  facet2 <- if (length(facet) >= 2) facet[2] else NULL
  has_ <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
  if (is.null(distributions) && (typed_dt || typed_cc))
    warning("plot_numbers_at_length_age(): distributionType / catchCategory ",
            "only matter when `distributions` is supplied (building from ",
            "raw catch)", call. = FALSE)
  grp <- unique(c(if (has_(fill)) fill, if (has_(facet1)) facet1,
                  if (has_(facet2)) facet2))
  pr <- biology_dist_prep(data, distributions, distributionType,
                          catch_category_parts(catchCategory),
                          "numbers", grp, dims)
  data <- pr$data
  cl <- pr$class_label
  vl <- pr$value_label
  cc_word   <- switch(catchCategory, Lan = "landings", Dis = "discards",
                      BMS = "BMS catch", All = "catch")
  cc_suffix <- if (!is.null(distributions)) paste0(" (", cc_word, ")") else ""
  d <- data; fill_var <- fill; group_var <- fill
  class_label <- cl; value_label <- vl

  p <- {
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
        p <- p + scale_fill_var(fill_var, d[[fill_var]], name = fill_var,
                                palette = palette)

      p +
        scale_y_continuous(labels = scales::comma) +
        facet_two(facet1, facet2, ncol = 2, scales = scales) +
        labs(title = paste0(sprintf("Numbers at %s",
                                    tolower(sub(" .*", "", class_label))), cc_suffix),
             subtitle = if (has(fill_var)) paste0("Stacked by ", fill_var) else NULL,
             x = class_label, y = value_label, fill = fill_var) +
        app_theme()
  }
  p
}



#' Mean weight at length or age
#'
#' @description
#' The number-weighted mean weight in each length (or age) class, as a line
#' with points. Weight is in grams, length in cm and age in years. When the
#' strata are averaged (no `shape`) and a spread is available, a +/- 1 sd
#' band is drawn as well.
#'
#' @details
#' `data` is either raw CEF catch together with `distributions` - linked
#' through `domainBiology` and summarised inside, with `catchCategory`
#' choosing the landings, the discards, BMS or the catch as a whole - or a
#' table already summarised by `length_meanweight()`. When `distributions` is
#' given the catch part goes in the plot title.
#'
#' A mean weight is not additive, so strata that are pooled are averaged
#' weighted by the numbers measured at that class: a stratum that measured
#' five fish cannot outweigh one that measured five hundred. The band is the
#' number-weighted standard deviation across the strata pooled into each
#' point; a point built from a single stratum has none.
#'
#' `colour` groups the lines. `shape` opens the averaged group back up - one
#' series per level, each drawn separately - and drops the band. `facet`
#' takes one column (a wrapped set of panels) or two (`c("row", "col")`, a
#' grid). Live weight and a line are the only options, so there is no
#' `variableType` or `type` argument.
#'
#' @param data Raw CEF catch data, or a `length_meanweight()` table.
#' @param distributions A CEF distributions table. Needed with raw catch;
#'   leave it out when `data` is already a `length_meanweight()` table.
#' @param distributionType `"LengthTotal"` (default) or `"Age"`.
#' @param catchCategory `"All"` (default: landings and discards together,
#'   titled "catch"), `"Lan"`, `"Dis"` or `"BMS"`. Only used with
#'   `distributions`.
#' @param colour Column mapped to the line colour.
#' @param facet Up to two facet columns: `"col"` or `c("row", "col")`.
#' @param shape Column drawn as separate series with their own point shapes;
#'   opens the averaged group back up and drops the band.
#' @param scales Facet scales: `"fixed"` (default), `"free_x"`, `"free_y"` or
#'   `"free"`.
#' @param palette `"default"` or `"colorblind"`.
#' @return A ggplot object, or `NULL` when there is nothing to draw.
#' @seealso [plot_numbers_at_length_age()] for the numbers in the same
#'   classes; [coverage-plots] for whether the strata have a distribution at
#'   all.
#' @family distribution plots
#' @md
#' @examples
#' data(anchoveta_catch)
#' data(anchoveta_dist)
#'
#' # mean weight at length of the landings, one line per country
#' plot_meanweight_at_length_age(anchoveta_catch, anchoveta_dist,
#'                               catchCategory = "Lan", colour = "Country")
#'
#' # at age, one panel per quarter
#' plot_meanweight_at_length_age(anchoveta_catch, anchoveta_dist,
#'                               distributionType = "Age",
#'                               colour = "Country", facet = "Season")
#' @export
plot_meanweight_at_length_age <- function(data, distributions = NULL,
                                          distributionType = c("LengthTotal", "Age"),
                                          catchCategory = "All",
                                          colour = NULL, facet = NULL, shape = NULL,
                                          scales = "fixed",
                                          palette = c("default", "colorblind")) {
  typed_dt <- !missing(distributionType); typed_cc <- !missing(catchCategory)
  dims <- NULL
  distributionType <- match.arg(distributionType)
  palette <- match.arg(palette)
  catchCategory <- match.arg(catchCategory, c("Lan", "Dis", "BMS", "All"))
  if (length(facet) > 2)
    stop("plot_meanweight_at_length_age(): `facet` takes at most two columns",
         call. = FALSE)
  facet1 <- if (length(facet) >= 1) facet[1] else NULL
  facet2 <- if (length(facet) >= 2) facet[2] else NULL
  has_ <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
  if (is.null(distributions) && (typed_dt || typed_cc))
    warning("plot_meanweight_at_length_age(): distributionType / catchCategory ",
            "only matter when `distributions` is supplied (building from ",
            "raw catch)", call. = FALSE)
  grp <- unique(c(if (has_(colour)) colour, if (has_(facet1)) facet1,
                  if (has_(facet2)) facet2, if (has_(shape)) shape))
  pr <- biology_dist_prep(data, distributions, distributionType,
                          catch_category_parts(catchCategory),
                          "meanweight", grp, dims)
  data <- pr$data
  cl <- pr$class_label
  cc_word   <- switch(catchCategory, Lan = "landings", Dis = "discards",
                      BMS = "BMS catch", All = "catch")
  cc_suffix <- if (!is.null(distributions)) paste0(" (", cc_word, ")") else ""
  d <- data; colour_var <- colour; shape_var <- shape
  class_label <- cl

  p <- {
      if (is.null(d) || !nrow(d)) return(NULL)
      has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
      # the band only means something when strata were actually collapsed
      band <- !has(shape_var) && "sd_weight" %in% names(d) &&
        any(!is.na(d$sd_weight))

      grp <- if (has(colour_var)) rlang::sym(colour_var) else NULL
      p <- if (has(colour_var))
        ggplot(d, aes(x = .class, y = mean_weight,
                      colour = factor(!!grp), group = factor(!!grp)))
      else ggplot(d, aes(x = .class, y = mean_weight, group = 1))

      if (band) {
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

      if (has(colour_var)) {
        p <- p + scale_colour_var(colour_var, d[[colour_var]],
                                  name = colour_var, palette = palette)
        # fill is only mapped by the band and the colour-fill points, and
        # neither is drawn once `shape` opens the strata up
        if (!has(shape_var))
          p <- p + scale_fill_var(colour_var, d[[colour_var]],
                                  name = colour_var, palette = palette)
      }

      sub <- if (has(shape_var))
        paste0("Each stratum shown separately by ", shape_var)
      else if (band)
        "Line = number-weighted mean, band = +/- 1 sd across the grouped strata"
      else NULL

      p +
        scale_y_continuous(labels = scales::comma) +
        facet_two(facet1, facet2, ncol = 2, scales = scales) +
        labs(title = paste0(sprintf("Mean weight at %s",
                             tolower(sub(" .*", "", class_label))), cc_suffix), subtitle = sub,
             x = class_label, y = "Mean weight (g)",
             colour = colour_var,
             fill = if (has(shape_var)) NULL else colour_var,
             shape = shape_var) +
        app_theme()
  }
  p
}
