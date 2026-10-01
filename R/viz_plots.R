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
#    plot_DR()                   per-stratum discard-rate points.
#    plot_NaLoA()                numbers at length/age (histogram).
#    plot_MWaLoA()               mean weight at length/age (line + points).
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
    lev  <- if (identical(group_var, "mesh")) mesh_levels(agg[[group_var]])
            else sort(unique(as.character(agg[[group_var]])))
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
  ttl  <- cov_title(what, weight_label,
                    by = c(area_var, if (has(group_var)) group_var))
  # the area level itself is never "grouped over" - it is the unit each pie
  # is drawn at - so it goes in the sentence's own vars, group first
  grp_keys <- c(if (has(group_var)) group_var, area_var)
  cap  <- sub_wrap(
    strata_sentence(grp_keys, catch),
    sprintf("area = %s.", area_var),
    sprintf("bubble size = %s weight (t).", weight_label),
    sprintf("coverage slice (green) = %s.", share_phrase(what, weight_label)),
    width = 110)

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
    labs(title = ttl, caption = cap, x = NULL, y = NULL) +
    app_theme()
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
#' ## What the title and caption say
#'
#' The title names the weight a figure is judged against first, then what is
#' sampled: "Landings - Sampling of Length", "Discards - Sampling of Age".
#' The weight word comes from `catchCategory` (landed, discarded, BMS, or the
#' catch as a whole). Discard coverage drops it entirely - "Sampling of
#' Discards" - since it is always judged against the landings, so naming
#' that weight would only repeat itself.
#'
#' The caption's first line says what a stratum is here: the plotted
#' columns, grouped over every other stratum column ("grouping = metier4 x
#' Country x Season, grouped over finer fleet levels (fleetValue) and area
#' (areaValue)"), so the same figure drawn at other columns has other strata
#' behind it. The rest of the caption spells out how the numbers on the
#' figure are worked out - the value in a tile, what a bubble's size or a
#' bar means - leaving out anything a legend already shows. On the heatmap
#' and bubble grid the active thresholds move into their own boxed guide,
#' bottom right, under the in-panel status legend - a detail section of it,
#' not a caption about the plot itself.
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
#'   An error with `what = "discard"` (no fish are measured for a discard
#'   estimate).
#' @param palette `"default"` or `"colorblind"`. Recolours the bar fill; on
#'   the heatmap and bubbles the status colours are fixed, and asking for
#'   `"colorblind"` warns.
#' @return A ggplot object, or `NULL` when there is nothing to draw. The
#'   heatmap and bubbles also carry a `"status_legend"` attribute: the
#'   specification [status_legend_plot()] rebuilds a complete legend from.
#' @seealso [build_coverage_strata()] for the table, [plot_coverage_map()],
#'   [plot_DR()]; [classify_strata()] and [cat_lookup()] for the
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
  if (identical(what, "discard") && thr_active(min_fish))
    stop(fn, "(): min_fish does not apply to what = \"discard\" (no fish ",
         "are measured for a discard estimate - min_fish is for length / ",
         "age)", call. = FALSE)
  y_var <- y; x_var <- x; facet_var <- facet; threshold <- min_cov
  min_meas <- min_fish; min_trips <- min_trip

  # A cell with no landed/caught weight at all draws no tile - it has
  # nothing to judge, so it stays a blank hole rather than a "no
  # landings" placeholder.
  d <- coverage_status(cv$cells, what, threshold, min_trips, min_meas)
  d <- d[d$cat_key != "none", , drop = FALSE]
  if (!nrow(d)) return(NULL)
  d <- reorder_mesh(d, c(y_var, x_var, facet_var))
  if (is.null(x_var) || !nzchar(x_var)) { d$.x <- "All"; x_var <- ".x" }
  fmt <- function(v) ifelse(is.na(v) | v <= 0, "",
                     ifelse(v > 99, sprintf("%.0f", v),
                     ifelse(v >= 10, sprintf("%.1f", v), sprintf("%.2f", v))))
  # the shortfall a warning cell missed, same helper the bubble grid uses -
  # one line under the tile number, so nudge the number up to make room
  d$lab <- shortfall_label(d, sep = ", ")
  d$.num_vjust <- ifelse(nzchar(d$lab), -0.2, 0.5)
  # "none" never draws a tile, so it is dropped from the legend too.
  lk_no_none <- cat_lookup(threshold, what, min_meas, min_trips)
  stat_all <- lk_no_none$status[lk_no_none$cat_key != "none"]
  d$Status <- factor(as.character(d$Status), levels = stat_all)
  p <- ggplot(d, aes(x = factor(.data[[x_var]]), y = .data[[y_var]], fill = Status)) +
    geom_tile(colour = OUTLINE, alpha = 0.9) +
    geom_text(aes(label = fmt(landings), vjust = .data$.num_vjust), size = 2.6,
              fontface = "bold", colour = OUTLINE) +
    geom_text(aes(label = lab), vjust = 1.3, size = 2.2, fontface = "bold",
              colour = "red3") +
    # a zero-size mark per status, so the legend carries a filled key for
    # categories that no cell in this particular figure lands in - added
    # last so the real tiles stay layer 1
    status_key_layer(d, stat_all, "tile") +
    scale_fill_manual(values = status_palette(threshold, what, min_meas, min_trips),
                      limits = stat_all,
                      labels = status_abbr(threshold, what, min_meas, min_trips),
                      drop = FALSE, name = "Sampling status") +
    maybe_facet(facet_var) +
    labs(title = cov_title(what, weight_label),
         caption = sub_wrap(strata_sentence(c(y, x, facet), catch),
                            sprintf("tile number = %s weight (t).", weight_label),
                            width = 110),
         x = if (x_var == ".x") NULL else x_var, y = y_var) +
    app_theme() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    thr_guide(thr_lines(threshold, min_trips, min_meas, what, weight_label))
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
  if (identical(what, "discard") && thr_active(min_fish))
    stop(fn, "(): min_fish does not apply to what = \"discard\" (no fish ",
         "are measured for a discard estimate - min_fish is for length / ",
         "age)", call. = FALSE)
  y_var <- y; x_var <- x; facet_var <- facet; threshold <- min_cov
  min_meas <- min_fish; min_trips <- min_trip

  # A cell with no landed/caught weight at all draws no bubble - it has
  # nothing to judge, so it stays a blank hole rather than a "no
  # landings" placeholder. One bubble per cell: the cells are already
  # pooled to the plotted columns, so no two bubbles share a point.
  d <- coverage_status(cv$cells, what, threshold, min_trips, min_meas)
  d <- d[d$cat_key != "none", , drop = FALSE]
  if (!nrow(d)) return(NULL)
  d <- reorder_mesh(d, c(y_var, x_var, facet_var))
  if (is.null(x_var) || !nzchar(x_var)) { d$.x <- "All"; x_var <- ".x" }
  smallest <- suppressWarnings(min(d$landings[d$landings > 0], na.rm = TRUE))
  if (!is.finite(smallest)) smallest <- 1
  d$plot_size <- ifelse(d$cat_key == "disc_only", smallest, d$landings)
  # the shortfall a warning cell missed, same helper the heatmap uses;
  # "100%\nDisc" is discard-only and has no threshold to miss
  d$lab <- ifelse(d$cat_key == "disc_only", "100%\nDisc", shortfall_label(d, "\n"))
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
    labs(title = cov_title(what, weight_label),
         caption = sub_wrap(strata_sentence(c(y, x, facet), catch),
                            sprintf("bubble size = %s weight (t).", weight_label),
                            width = 110),
         x = if (x_var == ".x") NULL else x_var, y = y_var) +
    app_theme() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    thr_guide(thr_lines(threshold, min_trips, min_meas, what, weight_label))
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
  # .fill / .facet are built as plain character above (the row order itself
  # is by landings, not by category - see below), so mesh's size order has
  # to be reapplied here rather than carried through from d$mesh directly
  if (identical(if (has(fill_var)) fill_var else y_var, "mesh"))
    d$.fill <- factor(d$.fill, levels = mesh_levels(d$.fill))
  if (identical(facet_var, "mesh"))
    d$.facet <- factor(d$.facet, levels = mesh_levels(d$.facet))

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
  # the sampled share is annotated on the plot itself (the bracket labels
  # below, per facet), so it is not repeated in words here
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
                                           name = sprintf("Cumulative %% of %s weight",
                                                          weight_label),
                                           breaks = c(0, 25, 50, 75, 100))) +
    scale_fill_var(if (has(fill_var)) fill_var else y_var, d$.fill,
                   name = if (has(fill_var)) fill_var else y_var,
                   palette = palette) +
    (if (has(facet_var)) facet_wrap(vars(.facet), scales = "free_y", ncol = 2)
     else NULL) +
    labs(title = cov_title(what, weight_label),
         caption = sub_wrap(
           strata_sentence(c(y, fill, facet), catch),
           sprintf("staircase = cumulative %% of %s weight (top axis).", weight_label),
           sprintf("bar = %s weight (bottom axis).", weight_label),
           width = 110),
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
#' the point size - the finest a stratum gets, never an average of finer ones.
#' A thin line behind the points marks the mean rate of each `x` level,
#' weighted by landings, and an optional `reference` draws a dashed line and
#' names the strata above it.
#'
#' @details
#' ## What a point is
#'
#' A stratum is one row of the discard coverage table
#' ([build_coverage_strata()]). Given a discard coverage table, its rows are
#' drawn exactly as they are - one point per row. Given raw catch, the table
#' is built at the finest resolution the catch carries (`fleetValue`,
#' `areaValue`, `Country`, `Season`, whichever are present) *together with*
#' `x` / `colour` / `shape` / `facet`, so a point is always an individual,
#' un-pooled stratum: several points can share one `x` level. `strata_vars`
#' (the columns behind a point's label, see below) is exactly this set.
#' Strata with no discard estimate have no rate and are not drawn: a rate is
#' a property of sampled strata only.
#'
#' ## The rate
#'
#' `rate` chooses what is plotted, and both are columns of the coverage table:
#'
#' * `"ratio"` (the default, the "discard ratio") is `rate` = `total /
#'   weightingTotal`: the stratum's discards D over the landings L that carry
#'   a discard estimate - the same number the CAS output reports. It is a
#'   ratio, so it can exceed 100%.
#' * `"rate"` (the "discard rate") is `discardRate` = `total / (weightingTotal
#'   + total)`: the discards as a share of the catch - those landings plus the
#'   discards, the `discard_rate` of [build_strata()]. It stays between 0 and
#'   100%.
#'
#' ## The weighted mean
#'
#' The line on each `x` level (in each panel, when faceted) is the mean of
#' the plotted rates of the strata at that level, weighted by the landings
#' that carry a discard estimate (`weightingTotal`, the CAS weight): a big
#' stratum counts for more than a small one. For `"ratio"` it equals the rate
#' of the `x` level built directly, `build_coverage_strata(catch, x)`. It sits
#' behind the points, thin and muted, so it reads as a reference, not the
#' headline.
#'
#' ## The reference line
#'
#' `reference` is a rate on the same scale as the axis, as a fraction (`0.3` is
#' 30%; with `rate = "rate"` it must lie between 0 and 1). A dashed line marks
#' it, and the strata above it are labelled with their identity - the finest
#' column of each stratum family present (so a coarser alias already
#' determined by a finer one you also plotted, `metier4` alongside
#' `fleetValue` say, is not repeated), bare values joined by `"|"`
#' (`"PS_SPF_0_0_0|87.1.2|Chile|1"`) - a point is an individual stratum, so
#' its label identifies it fully, not just by the columns you happened to
#' plot.
#'
#' ## The y axis
#'
#' For `"ratio"` the axis is pseudo-log: linear near zero (zero stays zero)
#' and logarithmic above about 5%. A few tiny strata (a few kilograms of
#' covered landings) give ratios of many hundreds of percent that would
#' flatten everything else on a plain axis. Add `+
#' ggplot2::scale_y_continuous()` to the returned plot to replace it with an
#' ordinary one. `"rate"` is bounded, so its axis is an ordinary 0-100% one.
#'
#' @param catch Raw CEF catch data, or a discard coverage table from
#'   [build_coverage_strata()].
#' @param x Column on the x axis: one point per level (times `colour`,
#'   `shape` and `facet`). Required.
#' @param colour Column mapped to the point fill.
#' @param shape Column mapped to the point shape.
#' @param facet Optional facet column.
#' @param rate `"ratio"` (default, discards / landings) or `"rate"` (discards
#'   / catch): which discard rate is the y variable. See *The rate*.
#' @param reference Optional single rate, as a fraction (`0.3` = 30%), on the
#'   scale of `rate`. Draws a dashed line and labels the strata above it.
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
#' plot_DR(anchoveta_catch, x = "metier4", colour = "Country")
#' plot_DR(anchoveta_catch, x = "Country", colour = "metier4",
#'        facet = "Season")
#'
#' # discards as a share of the catch, with a reference line: the strata
#' # above 20% are named, in full
#' plot_DR(anchoveta_catch, x = "metier4", colour = "Country",
#'        rate = "rate", reference = 0.2)
#'
#' # the table behind the points
#' cov <- build_coverage_strata(anchoveta_catch, c("metier4", "Country"))
#' head(cov[!is.na(cov$rate), c("metier4", "Country", "total", "rate",
#'                              "discardRate")])
#' @export
plot_DR <- function(catch, x, colour = NULL, shape = NULL,
                    facet = NULL, rate = c("ratio", "rate"),
                    reference = NULL, jitter = TRUE,
                    palette = c("default", "colorblind")) {
  palette <- match.arg(palette)
  rate_type <- match.arg(rate)
  if (!is.null(reference) && !(is.numeric(reference) && length(reference) == 1 &&
                               is.finite(reference) && reference >= 0))
    stop("plot_DR(): `reference` must be a single number, a rate ",
         "as a fraction (0.3 = 30%)", call. = FALSE)
  # the discard rate cannot exceed 100%, so 30 meant as a percent would draw
  # a line off the plot and label nothing - say so instead of drawing nothing
  if (!is.null(reference) && identical(rate_type, "rate") && reference > 1)
    stop("plot_DR(): with rate = \"rate\" the reference line is a ",
         "fraction between 0 and 1 (0.3 = 30%), not a percent", call. = FALSE)
  has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
  if (!has(x)) stop("plot_DR(): `x` is required", call. = FALSE)
  x_var <- x; colour_var <- colour; shape_var <- shape; facet_var <- facet
  keys <- unique(c(x, if (has(colour)) colour, if (has(shape)) shape,
                   if (has(facet)) facet))

  tw <- coverage_table_what(catch)
  if (is.na(tw)) {
    # Raw catch: build at the finest the catch carries, together with
    # whatever is plotted - not pooled to just the plotted columns, or a
    # point (and its label) could be many different areas and seasons
    # wearing one name. A minimal catch with none of the four keeps the old
    # pooled-to-plotted-columns behaviour (nothing finer to build at).
    base_dims <- intersect(c("fleetValue", "areaValue", "Country", "Season"),
                           names(as.data.frame(catch)))
    d <- build_coverage_strata(catch, dims = unique(c(base_dims, keys)))
  } else if (identical(tw, "discard")) {
    d <- catch
  } else {
    stop("plot_DR(): `catch` is a ", tw, " coverage table - a ",
         "discard rate needs raw catch or a discard coverage table",
         call. = FALSE)
  }
  if (is.null(d)) return(NULL)
  miss <- setdiff(keys, names(d))
  if (length(miss))
    stop("plot_DR(): column(s) not in the coverage table: ",
         paste(miss, collapse = ", "), call. = FALSE)

  # rate is NA for a stratum with no discard estimate (nobody looked -
  # not the same as looked and found none), so those simply are not drawn -
  # whichever rate is plotted, so the two show the same strata
  d <- d[!is.na(d$rate), , drop = FALSE]
  if (!nrow(d)) return(NULL)
  d <- reorder_mesh(d, c(x_var, colour_var, shape_var, facet_var))
  # The y variable, in percent. Both rates are columns of the coverage
  # table; one built before `discardRate` existed gets it worked out here
  # from the two weights it does carry.
  dr <- if (identical(rate_type, "ratio")) d$rate
        else if ("discardRate" %in% names(d)) d$discardRate
        else d$total / (d$weightingTotal + d$total)
  d$.y <- dr * 100
  # A point is one stratum - sizing it by that stratum's landings turns
  # "how much of the catch is this dot" from a question the reader has to
  # go look up into something the plot itself answers. Off when landings
  # is degenerate (every value 0 or NA).
  has_landings <- any(is.finite(d$landings) & d$landings > 0)
  # Floored at 15% of the largest landings in THIS plot (not a fixed
  # tonnage, so it adapts to the data): area scaling alone shrinks a tiny
  # stratum's bubble to a sliver too thin to read its colour.
  if (has_landings)
    d$landings <- pmax(d$landings, max(d$landings, na.rm = TRUE) * 0.15)

  # What a point is: every stratum column the table carries - the table's
  # own dims, drawn as they are. This is the finest available (see above),
  # so it is also what a label needs to identify the point in full.
  strata_vars <- setdiff(names(d), c("catchCategory", "variableUnit",
                                     "weightingCategory", "total",
                                     "weightingTotal", "rate",
                                     "discardRate", "landings", "coverage",
                                     "numTrips", ".y"))
  # The mean of each x level (per panel), weighted by the landings behind
  # each rate - for the ratio exactly the rate of that level built directly.
  wkeys <- unique(c(x_var, if (has(facet_var)) facet_var))
  wm <- d %>%
    group_by(across(all_of(wkeys))) %>%
    summarise(.y = stats::weighted.mean(.y, weightingTotal), .groups = "drop") %>%
    filter(is.finite(.y))
  # Labels for the strata above the reference: bare values, only the finest
  # column of each family (a coarser alias already determined by a finer one
  # you also plotted - metier4 alongside fleetValue, say - says nothing extra
  # and is dropped) so the point can be told apart from another that only
  # shares some of its columns. Blank for every other point, so the label
  # layer has exactly the rows of the point layer and the same jitter puts
  # each label on its point.
  lab_vars <- finest_per_family(strata_vars)
  ref_y <- if (is.null(reference)) NULL else reference * 100
  lab_all <- do.call(paste, c(lapply(lab_vars, function(v) d[[v]]), sep = "|"))
  d$.lab <- if (is.null(ref_y)) "" else ifelse(d$.y > ref_y, lab_all, "")

  pos <- if (jitter) position_jitter(width = 0.18, height = 0, seed = 1) else "identity"
  ae <- aes(x = .data[[x_var]], y = .y)
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
  p <- ggplot(d, ae)
  # the reference goes first, so the points sit on top of the line
  if (!is.null(ref_y))
    p <- p + geom_hline(yintercept = ref_y, linetype = "dashed",
                        colour = "red3", linewidth = 0.5) +
      annotate("text", x = -Inf, y = ref_y, hjust = -0.05, vjust = -0.5,
               size = 3, colour = "red3",
               label = sprintf("reference %s%%", format(round(reference * 100, 1),
                                                        trim = TRUE)))
  # the weighted mean sits behind the strata: a thin, muted line - no
  # whiskers, since geom_errorbar's top and bottom cap coincide at
  # ymin = ymax - and no marker at its centre, so it reads as a backdrop
  # the points sit on, not a headline of its own.
  p <- p +
    geom_errorbar(data = wm, aes(x = .data[[x_var]], ymin = .y, ymax = .y),
                 inherit.aes = FALSE, width = 0.6, colour = "grey45",
                 linewidth = 0.4) +
    do.call(geom_point, pt_args)
  if (!is.null(ref_y))
    p <- p + ggrepel::geom_text_repel(
      data = d, aes(x = .data[[x_var]], y = .y, label = .lab),
      inherit.aes = FALSE, position = pos, size = 2.6, fontface = "bold",
      colour = "red3", box.padding = 0.4, min.segment.length = 0,
      segment.size = 0.2, segment.colour = "grey40", max.overlaps = Inf)
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

  # the ratio can exceed 100% - and a few tiny strata (a few kg of covered
  # landings) give ratios of many hundreds of percent that on a plain axis
  # flatten everything else against the floor. Pseudo-log: linear near zero
  # (zero stays zero), logarithmic above ~5%, so the bulk and the extremes
  # are both readable. The rate stays within 0-100%, so it gets an ordinary
  # axis.
  p <- p +
    (if (identical(rate_type, "ratio"))
       scale_y_continuous(
         transform = scales::transform_pseudo_log(sigma = 5),
         breaks = c(0, 5, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000),
         labels = function(z) paste0(round(z), "%"))
     else
       scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20),
                          labels = function(z) paste0(round(z), "%"))) +
    maybe_facet(facet_var, free_y = FALSE, ncol = 2) +
    labs(title = sprintf("Discard %s per stratum", rate_type),
         caption = sub_wrap(
           strata_sentence(strata_vars, catch),
           if (identical(rate_type, "ratio")) "Discard ratio = discard/landings weight."
           else "Discard rate = discard/catch weight.",
           sprintf("bubbles = estimated discard %s.", rate_type),
           if (has_landings) "bubble size = landings weight (t).",
           sprintf("horizontal line = landings weighted average per %s.", x_var),
           if (!is.null(ref_y))
             "dashed line = reference arbitrary threshold in the same units as y axis.",
           if (!is.null(ref_y))
             sprintf("labels = strata ID exceeding reference line (%s).",
                     paste(lab_vars, collapse = "|")),
           width = 110),
         y = sprintf("Discard %s (%s)", rate_type,
                    if (identical(rate_type, "ratio")) "D/L" else "D/(L+D)"),
         x = x_var,
         fill = if (has(colour_var)) colour_var else NULL,
         shape = if (has(shape_var)) shape_var else NULL) +
    app_theme() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.text = element_text(size = 11),
          legend.key.size = unit(1.1, "lines"))
  p
}

#' Discard rate after raising: the raised catch, and the donor strata behind it
#'
#' @description
#' [plot_DR()] for after raising: one point per stratum from the
#' raised catch, shaded by how much of its discard weight was raised rather
#' than sampled (as in [plot_raised_catches()]'s imported/raised shading, but
#' as a share rather than a strict either/or - a point can blend both when the
#' chosen `x`/`colour`/`facet` groups several finer strata together), plus -
#' underneath, muted - one point per donor stratum actually behind each
#' group's rate. Put the raising group on `x` (`"DrGroup"`) and a group whose
#' donor cloud is tight is a real group; one whose cloud is scattered is a
#' guess dressed up as a group, no matter how plausible its own raised rate
#' looks on top.
#'
#' @details
#' ## Why two layers, not one
#'
#' A raised stratum's own `DrGroup` names only the group *that stratum*
#' belongs to - never every group whose rate it helped compute. A REST-style
#' group that draws on strata far outside its own (see the donor-pool section
#' of the raising vignettes) looks deceptively tight if only `raised_catch` is
#' plotted: its own few strata may agree, while the much wider pool that
#' actually set its rate does not. Re-testing `matched_data_cond` against
#' `catch_data` - done here internally, so it re-runs correctly whatever `x`,
#' `colour` or `facet` is set to - is the only way to see that wider pool.
#'
#' ## The rate, and the reference line
#'
#' Both work exactly as in [plot_DR()]: `rate` chooses `"ratio"`
#' (discards / landings, the default) or `"rate"` (discards / catch), and
#' `reference` draws a dashed line at a fixed threshold. Unlike
#' [plot_DR()], there is no weighted-mean line - the raised points
#' themselves are the reference here - and no per-point labels.
#'
#' @param raised_catch Output of [catchRaising()].
#' @param catch_data The original catch [derive_fields()] was run on - what
#'   `matched_data_cond` is re-tested against for the donor layer.
#' @param matched_data_cond Named list of donor-pool quosures, one per raising
#'   group (as passed to [catchRaising()]'s
#'   `condition_matched_data_list`).
#' @param x Column on the x axis: one point per level (times `colour`,
#'   `shape` and `facet`). Can be `"DrGroup"`. Required.
#' @param colour Column mapped to the point fill (raised layer only).
#' @param shape Column mapped to the point shape (raised layer only).
#' @param facet Optional facet column.
#' @param rate `"ratio"` (default, discards / landings) or `"rate"` (discards
#'   / catch): which discard rate is the y variable. See
#'   [plot_DR()].
#' @param reference Optional single rate, as a fraction (`0.3` = 30%), on the
#'   scale of `rate`. Draws a dashed line.
#' @param jitter Jitter the points horizontally. Cosmetic only.
#' @param palette `"default"` or `"colorblind"`: the raised-layer point
#'   colours (see [var_palette()]).
#' @return A ggplot object, or `NULL` when no stratum has a discard rate.
#' @seealso [plot_DR()] for the pre-raising counterpart;
#'   [plot_raised_catches()] for the raised weight itself.
#' @family discard-rate plots
#' @family raising plots
#' @md
#' @export
plot_raised_DR <- function(raised_catch, catch_data, matched_data_cond, x,
                           colour = NULL, shape = NULL, facet = NULL,
                           rate = c("ratio", "rate"), reference = NULL,
                           jitter = TRUE, palette = c("default", "colorblind")) {
  palette <- match.arg(palette)
  rate_type <- match.arg(rate)
  if (!is.null(reference) && !(is.numeric(reference) && length(reference) == 1 &&
                               is.finite(reference) && reference >= 0))
    stop("plot_raised_DR(): `reference` must be a single number, a rate ",
         "as a fraction (0.3 = 30%)", call. = FALSE)
  if (!is.null(reference) && identical(rate_type, "rate") && reference > 1)
    stop("plot_raised_DR(): with rate = \"rate\" the reference line is a ",
         "fraction between 0 and 1 (0.3 = 30%), not a percent", call. = FALSE)
  has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
  if (!has(x)) stop("plot_raised_DR(): `x` is required", call. = FALSE)
  if (!is.list(matched_data_cond) || is.null(names(matched_data_cond)) ||
      any(!nzchar(names(matched_data_cond))))
    stop("plot_raised_DR(): `matched_data_cond` must be a named list of ",
         "quosures, one per raising group", call. = FALSE)
  x_var <- x; colour_var <- colour; shape_var <- shape; facet_var <- facet
  struct_keys <- unique(c(x_var, if (has(facet_var)) facet_var))
  keys <- unique(c(struct_keys, if (has(colour_var)) colour_var,
                   if (has(shape_var)) shape_var))

  # y for a coverage-strata table, whichever rate is asked for - shared by
  # both layers so a point means the same thing in each
  y_of <- function(d) if (identical(rate_type, "ratio")) d$rate
                      else if ("discardRate" %in% names(d)) d$discardRate
                      else d$total / (d$weightingTotal + d$total)

  # ---- raised layer: raised_catch, at the finest dims it carries ----------
  # Not build_coverage_strata(): its rate needs a Dis row's domainCatchDis to
  # link to the Lan row it belongs to, and a raised Dis row's domain key is a
  # synthetic one nothing else shares (RDBESStockCoord's own convention, and
  # rcef's native engine follows the same one) - so a raised stratum's rate
  # never links and always comes back NA. Landings, discards and the raised
  # share are instead summed directly off the rows in each group, which needs
  # no linking and treats a sampled stratum and a raised one the same way.
  base_dims <- intersect(c("fleetValue", "areaValue", "Country", "Season"),
                        names(raised_catch))
  rc <- raised_catch[raised_catch$originType %in% "WGEstimate" &
                       raised_catch$variableType %in% "WeightLive", , drop = FALSE]
  rd_dims <- unique(c(base_dims, keys))
  rd <- rc %>%
    group_by(across(all_of(rd_dims))) %>%
    summarise(landings = sum(total[catchCategory %in% "Lan"], na.rm = TRUE),
              .discards = sum(total[catchCategory %in% "Dis"], na.rm = TRUE),
              .raised = sum(total[catchCategory %in% "Dis" &
                                    importedOrRaised %in% "raised"], na.rm = TRUE),
              .groups = "drop")
  rd <- rd[rd$landings > 0, , drop = FALSE]
  if (!nrow(rd)) return(NULL)
  rd <- reorder_mesh(rd, c(x_var, colour_var, shape_var, facet_var))
  rd$.raised_share <- ifelse(rd$.discards > 0, rd$.raised / rd$.discards, 0)
  rd$.y <- (if (identical(rate_type, "ratio")) rd$.discards / rd$landings
           else rd$.discards / (rd$landings + rd$.discards)) * 100
  rd$landings <- rd$landings / 1000   # kg -> tonnes, for bubble size and labels only
  has_landings <- any(is.finite(rd$landings) & rd$landings > 0)
  # Floored at 15% of the largest landings in THIS plot, as in plot_DR():
  # area scaling alone shrinks a tiny stratum's bubble to a sliver too thin
  # to read its colour.
  if (has_landings)
    rd$landings <- pmax(rd$landings, max(rd$landings, na.rm = TRUE) * 0.15)

  # ---- donor layer: matched_data_cond re-tested against catch_data --------
  # "DrGroup" names the group *this loop* is testing donors for - it is not a
  # real catch_data column (that only exists after raising), so it is never
  # asked of build_coverage_strata() here; every other key is a real column.
  donor_dims <- unique(c(intersect(c("fleetValue", "areaValue", "Country",
                                     "Season", "domainCatchDis"),
                                   names(catch_data)),
                        setdiff(struct_keys, "DrGroup")))
  donor <- bind_rows(lapply(names(matched_data_cond), function(g) {
    dd <- catch_data %>%
      filter(!!matched_data_cond[[g]], !is.na(domainCatchDis),
             originType == "WGEstimate", variableType == "WeightLive") %>%
      build_coverage_strata(dims = donor_dims)
    if (!is.null(dd)) mutate(dd, DrGroup = g, .before = 1)
  }))
  has_donor <- !is.null(donor) && nrow(donor) > 0
  if (has_donor) {
    donor$.y <- y_of(donor) * 100
    donor <- donor[!is.na(donor$.y), , drop = FALSE]
    has_donor <- nrow(donor) > 0
  }
  if (has_donor) donor <- reorder_mesh(donor, c(x_var, facet_var))

  pos <- if (jitter) position_jitter(width = 0.18, height = 0, seed = 1) else "identity"
  ref_y <- if (is.null(reference)) NULL else reference * 100

  p <- ggplot(rd, aes(x = .data[[x_var]], y = .y))
  if (!is.null(ref_y))
    p <- p + geom_hline(yintercept = ref_y, linetype = "dashed",
                        colour = "red3", linewidth = 0.5)
  if (has_donor)
    p <- p + geom_point(data = donor, aes(x = .data[[x_var]], y = .y),
                        inherit.aes = FALSE, colour = "grey55", alpha = 0.5,
                        size = 1.8, position = pos)

  ae <- aes(x = .data[[x_var]], y = .y, alpha = .raised_share)
  if (has(colour_var)) ae <- modifyList(ae, aes(fill = .data[[colour_var]]))
  if (has(shape_var))  ae <- modifyList(ae, aes(shape = .data[[shape_var]]))
  if (has_landings)    ae <- modifyList(ae, aes(size = landings))
  pt_args <- list(data = rd, mapping = ae, colour = OUTLINE, stroke = 0.5,
                  position = pos)
  if (!has(shape_var))  pt_args$shape <- 21
  if (!has_landings)    pt_args$size  <- 3
  p <- p + do.call(geom_point, pt_args) +
    scale_alpha_continuous(range = c(1, 0.3), limits = c(0, 1),
                           labels = scales::percent, name = "Share raised",
                           guide = guide_legend(override.aes = list(fill = "grey30")))

  if (has(shape_var))
    p <- p + scale_shape_manual(values = c(21, 22, 23, 24, 25, 8, 3, 4))
  if (has(colour_var))
    p <- p + guides(fill = guide_legend(
      override.aes = list(shape = 21, size = 3.5, alpha = 1)))
  if (has(shape_var))
    p <- p + guides(shape = guide_legend(
      override.aes = list(fill = "grey45", size = 3.5, alpha = 1)))
  if (has(colour_var))
    p <- p + scale_fill_var(colour_var, rd[[colour_var]], name = colour_var,
                            palette = palette)
  if (has_landings)
    p <- p + scale_size_area(max_size = 8, labels = scales::comma,
                             name = "Landings (t)")

  p <- p +
    (if (identical(rate_type, "ratio"))
       scale_y_continuous(
         transform = scales::transform_pseudo_log(sigma = 5),
         breaks = c(0, 5, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000),
         labels = function(z) paste0(round(z), "%"))
     else
       scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20),
                          labels = function(z) paste0(round(z), "%"))) +
    maybe_facet(facet_var, free_y = FALSE, ncol = 2) +
    labs(title = sprintf("Discard %s per stratum, raised", rate_type),
         caption = sub_wrap(
           "grouping = no grouping, raw data per strata including imported and raised.",
           if (identical(rate_type, "ratio")) "Discard ratio = discard/landings weight."
           else "Discard rate = discard/catch weight.",
           sprintf("bubbles = estimated discard %s, blending imported and raised.", rate_type),
           if (has_landings) "bubble size = landings weight (t).",
           "shade = share of this point's discards that was raised rather than sampled.",
           if (has_donor)
             "small grey points = individual donor strata behind each group's rate.",
           if (!is.null(ref_y))
             "dashed line = reference arbitrary threshold in the same units as y axis.",
           width = 110),
         y = sprintf("Discard %s (%s)", rate_type,
                    if (identical(rate_type, "ratio")) "D/L" else "D/(L+D)"),
         x = x_var,
         fill = if (has(colour_var)) colour_var else NULL,
         shape = if (has(shape_var)) shape_var else NULL) +
    app_theme() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.text = element_text(size = 11),
          legend.key.size = unit(1.1, "lines"))
  p
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
#' through `domainBiology` and the country and summed inside, with
#' `catchCategory` choosing the landings, the discards, BMS or the catch as a
#' whole - or a table already summarised by `length_numbers()`. When
#' `distributions` is given the catch part goes in the plot title. A
#' distributions row that repeats another exactly is dropped, with a warning:
#' a repeat is not a second sample.
#'
#' `fill` colours (and stacks) the bars by a column of the catch. `facet`
#' takes one column (a wrapped set of panels) or two (`c("row", "col")`, a
#' grid).
#'
#' `plus_group` (ages) and `bin_width` (lengths) pool the classes exactly as
#' in [plot_MWaLoA()] - see that function's *The plus group* and *Length
#' bins* - except numbers simply add rather than being averaged, so there is
#' no per-distribution weighting step to speak of.
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
#' @param plus_group The first age of the plus group: `7` pools ages 7 and
#'   older into one class, shown as `7+`. `NULL` (default): no plus group.
#'   Ages only.
#' @param bin_width Width, in cm, of the length bins every class is pooled
#'   into. `NULL` (default): no binning. Lengths only.
#' @param scales Facet scales: `"fixed"` (default), `"free_x"`, `"free_y"` or
#'   `"free"`.
#' @param palette `"default"` or `"colorblind"`.
#' @return A ggplot object, or `NULL` when there is nothing to draw.
#' @seealso [plot_MWaLoA()] for the mean weight in the same classes;
#'   [coverage-plots] for whether the strata have a distribution at all.
#' @family distribution plots
#' @md
#' @examples
#' data(anchoveta_catch)
#' data(anchoveta_dist)
#'
#' # numbers at length of the landings, stacked by country
#' plot_NaLoA(anchoveta_catch, anchoveta_dist,
#'           catchCategory = "Lan", fill = "Country")
#'
#' # numbers at age, one panel per quarter, dodged
#' plot_NaLoA(anchoveta_catch, anchoveta_dist,
#'           distributionType = "Age", fill = "Country",
#'           facet = "Season", stacked = FALSE)
#'
#' # every age from 3 upward pooled into "3+"
#' plot_NaLoA(anchoveta_catch, anchoveta_dist,
#'           distributionType = "Age", fill = "Country", plus_group = 3)
#'
#' # length classes pooled into 2cm bins
#' plot_NaLoA(anchoveta_catch, anchoveta_dist, fill = "Country", bin_width = 2)
#' @export
plot_NaLoA <- function(data, distributions = NULL,
                       distributionType = c("LengthTotal", "Age"),
                       catchCategory = "All",
                       fill = NULL, facet = NULL,
                       stacked = TRUE,
                       plus_group = NULL, bin_width = NULL,
                       scales = "fixed",
                       palette = c("default", "colorblind")) {
  fn <- "plot_NaLoA(): "
  typed_dt <- !missing(distributionType); typed_cc <- !missing(catchCategory)
  dims <- NULL
  distributionType <- match.arg(distributionType)
  palette <- match.arg(palette)
  catchCategory <- match.arg(catchCategory, c("Lan", "Dis", "BMS", "All"))
  if (length(facet) > 2)
    stop(fn, "`facet` takes at most two columns", call. = FALSE)
  facet1 <- if (length(facet) >= 1) facet[1] else NULL
  facet2 <- if (length(facet) >= 2) facet[2] else NULL
  has_ <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
  if (is.null(distributions) && (typed_dt || typed_cc))
    warning(fn, "distributionType / catchCategory ",
            "only matter when `distributions` is supplied (building from ",
            "raw catch)", call. = FALSE)
  # A plus group (ages) and length bins (lengths) - see plot_MWaLoA()'s own
  # versions of these two, which this mirrors exactly.
  plus_from <- NULL
  if (!is.null(plus_group)) {
    if (!(is.numeric(plus_group) && length(plus_group) == 1 &&
          is.finite(plus_group) && plus_group >= 0))
      stop(fn, "`plus_group` is the first age of the plus group - a single ",
           "number (plus_group = 7 pools ages 7 and older) - or NULL for ",
           "none", call. = FALSE)
    if (is.null(distributions))
      warning(fn, "plus_group only applies when `distributions` is supplied ",
              "(a summarised table cannot be pooled again)", call. = FALSE)
    else if (!identical(distributionType, "Age"))
      stop(fn, "plus_group only applies to ages (distributionType = ",
           "\"Age\"): lengths have no plus group", call. = FALSE)
    else plus_from <- plus_group
  }
  bin_from <- NULL
  if (!is.null(bin_width)) {
    if (!(is.numeric(bin_width) && length(bin_width) == 1 &&
          is.finite(bin_width) && bin_width > 0))
      stop(fn, "`bin_width` is a single positive number - a bin width in cm ",
           "(bin_width = 5 groups length into 5cm bins) - or NULL for none",
           call. = FALSE)
    if (is.null(distributions))
      warning(fn, "bin_width only applies when `distributions` is supplied ",
              "(a summarised table cannot be pooled again)", call. = FALSE)
    else if (!identical(distributionType, "LengthTotal"))
      stop(fn, "bin_width only applies to lengths (distributionType = ",
           "\"LengthTotal\"): ages use plus_group instead", call. = FALSE)
    else bin_from <- bin_width
  }
  grp <- unique(c(if (has_(fill)) fill, if (has_(facet1)) facet1,
                  if (has_(facet2)) facet2))
  catch0 <- if (!is.null(distributions)) data   # `data` is summarised below
  pr <- biology_dist_prep(data, distributions, distributionType,
                          catch_category_parts(catchCategory),
                          "numbers", grp, dims, plus_from = plus_from,
                          bin_from = bin_from)
  data <- pr$data
  cl <- pr$class_label
  vl <- pr$value_label
  cc_word  <- switch(catchCategory, Lan = "Landings", Dis = "Discards",
                     BMS = "BMS Catch", All = "Catch")
  cc_title <- if (!is.null(distributions)) paste0(cc_word, " - ") else ""
  d <- data; fill_var <- fill; group_var <- fill
  class_label <- cl; value_label <- vl
  # A summary built from a link that pool_plus_group() / pool_length_bins()
  # pooled says so in its `plus` / `bin_width` column (how the app draws it).
  if (is.null(plus_from) && !is.null(d) && "plus" %in% names(d) &&
      any(d$plus %in% TRUE)) {
    pc <- unique(d$.class[d$plus %in% TRUE])
    if (length(pc) == 1) plus_from <- pc
  }
  if (is.null(bin_from) && !is.null(d) && "bin_width" %in% names(d) &&
      any(is.finite(d$bin_width))) {
    bwv <- unique(d$bin_width[is.finite(d$bin_width)])
    if (length(bwv) == 1) bin_from <- bwv
  }

  p <- {
      if (is.null(d) || !nrow(d)) return(NULL)
      d <- reorder_mesh(d, c(fill_var, facet1, facet2))
      has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
      plus_on <- !is.null(plus_from) && plus_from %in% d$.class
      if (!is.null(plus_from) && !plus_on)
        warning(fn, "no age reaches plus_group = ", plus_from,
                ", so there is no plus group", call. = FALSE)
      bin_on <- !is.null(bin_from)
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

      # what a bar is: the numbers of every sampled stratum that shares the
      # plotted columns, summed - everything else is grouped into it
      cc_lead <- if (is.null(distributions)) ""
                 else switch(catchCategory, Lan = "Landings ", Dis = "Discards ",
                            BMS = "BMS catch ", All = "Catch ")
      cls_word <- if (identical(distributionType, "Age")) "NaA" else "NaL"
      plus_txt <- if (plus_on)
        sprintf("%s+ = ages %s+ pooled.", format(plus_from), format(plus_from))
      bin_txt <- if (bin_on)
        sprintf("length classes pooled into %scm bins.",
                format(bin_from, trim = TRUE))
      cap <- sub_wrap(
        if (is.null(distributions)) NULL else strata_sentence(grp, catch0),
        sprintf("bars = %s%s summed over strata.", cc_lead, cls_word),
        if (has(fill_var))
          sprintf("%s by %s.", if (stacked) "stacked" else "dodged", fill_var),
        plus_txt, bin_txt,
        width = 110)
      p <- p + class_axis_scale(d$.class, plus_from = if (plus_on) plus_from,
                                bin_from = if (bin_on) bin_from)
      p +
        scale_y_continuous(labels = scales::comma) +
        facet_two(facet1, facet2, ncol = 2, scales = scales) +
        labs(title = paste0(cc_title, sprintf("Numbers at %s",
                                              sub(" .*", "", class_label))),
             caption = cap,
             x = class_label, y = value_label, fill = fill_var) +
        app_theme() + theme(plot.title = element_text(size = 11))
  }
  p
}



#' Mean weight at length or age
#'
#' @description
#' The number-weighted mean weight in each length (or age) class, as a line
#' with points. Weight is in grams, length in cm and age in years. When the
#' strata are averaged (no `shape`) and a spread is available, a +/- 1 sd
#' band is drawn as well. Two options help to judge how far to trust the
#' curve: a plus group pools the oldest ages into one class, and
#' semi-transparent bars behind the lines show how many fish stand behind
#' each point.
#'
#' @details
#' `data` is either raw CEF catch together with `distributions` - linked
#' through `domainBiology` and the country and summarised inside, with
#' `catchCategory` choosing the landings, the discards, BMS or the catch as a
#' whole - or a table already summarised by `length_meanweight()`. When
#' `distributions` is given the catch part goes in the plot title. A
#' distributions row that repeats another exactly is dropped, with a warning:
#' a repeat is not a second sample.
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
#' `variableType` or `type` argument. The legend sits inside the panel like
#' the other plots', except with `show_samples` or `plus_group`, where it moves
#' outside so it cannot cover the right-hand end of the curve.
#'
#' ## The plus group
#'
#' `plus_group = 7` pools every age from 7 upward into one class, labelled
#' `7+` on the axis and in the caption. Inside each
#' distribution the numbers of those ages are added and their mean weights
#' averaged, weighted by those numbers; the pooled classes are then averaged
#' over strata like any other point, so the band is still a spread across
#' strata, not across ages. It shows what a plus group does to the curve: the
#' ragged tail of old ages, each on a handful of fish, becomes one point
#' with many. It applies to ages only, and needs raw catch with
#' `distributions` - a table that is already summarised cannot be pooled
#' again. A table built from a link that `pool_plus_group()` pooled carries a
#' `plus` column instead, and the plot labels the class from it: that is how
#' the app draws a plus group.
#'
#' ## Length bins
#'
#' `bin_width = 5` is the length counterpart: every length class is grouped
#' into 5cm bins (`10`-`14` becomes one class labelled `10-15`), pooled the
#' same way a plus group is - numbers added, mean weights averaged weighted
#' by those numbers, inside each distribution first, then strata averaged as
#' usual. It applies to lengths only (`distributionType = "LengthTotal"`),
#' and needs raw catch with `distributions`, same restriction and the same
#' `bin_width` column fallback as the plus group.
#'
#' ## The fish behind each point
#'
#' `show_samples = TRUE` draws, behind the lines, semi-transparent bars of the
#' number of fish measured at each class, read on a second axis on the right.
#' A mean weight that rests on a few fish is uncertain however smooth the
#' line looks; a short bar under a point says so. The bars are stacked by
#' `colour`, one stack per class in each panel, and follow the plus group when
#' there is one.
#'
#' The distributions record `numMeasurements` once for a whole distribution,
#' not per class, so the count at a class is that number shared out over the
#' distribution's classes in proportion to its numbers there. This is exact
#' for a length distribution raised from a length sample by one factor, and an
#' estimate for ages read off an age-length key. A distribution counts once
#' however many strata it is linked to, and one that reports no count
#' (`numMeasurements` 0) adds none - a point with no bar is a point whose
#' sample size is unknown, not zero, and the caption says so. Needs raw catch
#' with `distributions`, or a `length_meanweight()` table (which carries the
#' count as `n_fish`).
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
#' @param plus_group The first age of the plus group: `7` pools ages 7 and
#'   older into one class, shown as `7+`. `NULL` (default): no plus group.
#'   Ages only. See *The plus group*.
#' @param bin_width Width, in cm, of the length bins every class is pooled
#'   into. `NULL` (default): no binning. Lengths only. See *Length bins*.
#' @param show_samples `TRUE` draws bars of the fish measured at each class
#'   behind the lines, on a second axis. Default `FALSE`. See *The fish behind
#'   each point*.
#' @param scales Facet scales: `"fixed"` (default), `"free_x"`, `"free_y"` or
#'   `"free"`.
#' @param palette `"default"` or `"colorblind"`.
#' @return A ggplot object, or `NULL` when there is nothing to draw.
#' @seealso [plot_NaLoA()] for the numbers in the same classes;
#'   [coverage-plots] for whether the strata have a distribution at all.
#' @family distribution plots
#' @md
#' @examples
#' data(anchoveta_catch)
#' data(anchoveta_dist)
#'
#' # mean weight at length of the landings, one line per country
#' plot_MWaLoA(anchoveta_catch, anchoveta_dist,
#'            catchCategory = "Lan", colour = "Country")
#'
#' # at age, one panel per quarter
#' plot_MWaLoA(anchoveta_catch, anchoveta_dist,
#'            distributionType = "Age",
#'            colour = "Country", facet = "Season")
#'
#' # what a plus group does: every age from 3 upward pooled into "3+"
#' plot_MWaLoA(anchoveta_catch, anchoveta_dist,
#'            distributionType = "Age", colour = "Country",
#'            plus_group = 3)
#'
#' # length classes pooled into 2cm bins
#' plot_MWaLoA(anchoveta_catch, anchoveta_dist, colour = "Country",
#'            bin_width = 2)
#'
#' # the fish measured at each age, as bars behind the lines
#' plot_MWaLoA(anchoveta_catch, anchoveta_dist,
#'            distributionType = "Age", colour = "Country",
#'            show_samples = TRUE)
#' @export
plot_MWaLoA <- function(data, distributions = NULL,
                        distributionType = c("LengthTotal", "Age"),
                        catchCategory = "All",
                        colour = NULL, facet = NULL, shape = NULL,
                        plus_group = NULL,
                        bin_width = NULL,
                        show_samples = FALSE,
                        scales = "fixed",
                        palette = c("default", "colorblind")) {
  fn <- "plot_MWaLoA(): "
  typed_dt <- !missing(distributionType); typed_cc <- !missing(catchCategory)
  dims <- NULL
  distributionType <- match.arg(distributionType)
  palette <- match.arg(palette)
  catchCategory <- match.arg(catchCategory, c("Lan", "Dis", "BMS", "All"))
  if (length(facet) > 2)
    stop(fn, "`facet` takes at most two columns", call. = FALSE)
  facet1 <- if (length(facet) >= 1) facet[1] else NULL
  facet2 <- if (length(facet) >= 2) facet[2] else NULL
  has_ <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
  flag <- function(v) is.logical(v) && length(v) == 1 && !is.na(v)
  if (!flag(show_samples))
    stop(fn, "`show_samples` is TRUE or FALSE", call. = FALSE)
  if (is.null(distributions) && (typed_dt || typed_cc))
    warning(fn, "distributionType / catchCategory only matter when ",
            "`distributions` is supplied (building from raw catch)",
            call. = FALSE)
  # The plus group pools ages, and pooling needs the distributions
  # themselves: a table that is already summarised cannot be pooled again.
  plus_from <- NULL
  if (!is.null(plus_group)) {
    if (!(is.numeric(plus_group) && length(plus_group) == 1 &&
          is.finite(plus_group) && plus_group >= 0))
      stop(fn, "`plus_group` is the first age of the plus group - a single ",
           "number (plus_group = 7 pools ages 7 and older) - or NULL for ",
           "none", call. = FALSE)
    if (is.null(distributions))
      warning(fn, "plus_group only applies when `distributions` is supplied ",
              "(a summarised table cannot be pooled again)", call. = FALSE)
    else if (!identical(distributionType, "Age"))
      stop(fn, "plus_group only applies to ages (distributionType = ",
           "\"Age\"): lengths have no plus group", call. = FALSE)
    else plus_from <- plus_group
  }
  # Length classes pooled into fixed-width bins - the length counterpart of
  # a plus group, so the same rules apply: needs raw catch with
  # distributions, and only for LengthTotal (ages take a plus group instead).
  bin_from <- NULL
  if (!is.null(bin_width)) {
    if (!(is.numeric(bin_width) && length(bin_width) == 1 &&
          is.finite(bin_width) && bin_width > 0))
      stop(fn, "`bin_width` is a single positive number - a bin width in cm ",
           "(bin_width = 5 groups length into 5cm bins) - or NULL for none",
           call. = FALSE)
    if (is.null(distributions))
      warning(fn, "bin_width only applies when `distributions` is supplied ",
              "(a summarised table cannot be pooled again)", call. = FALSE)
    else if (!identical(distributionType, "LengthTotal"))
      stop(fn, "bin_width only applies to lengths (distributionType = ",
           "\"LengthTotal\"): ages use plus_group instead", call. = FALSE)
    else bin_from <- bin_width
  }
  grp <- unique(c(if (has_(colour)) colour, if (has_(facet1)) facet1,
                  if (has_(facet2)) facet2, if (has_(shape)) shape))
  grp_vars <- grp
  catch0 <- if (!is.null(distributions)) data   # `data` is summarised below
  pr <- biology_dist_prep(data, distributions, distributionType,
                          catch_category_parts(catchCategory),
                          "meanweight", grp, dims, plus_from = plus_from,
                          bin_from = bin_from)
  data <- pr$data
  cl <- pr$class_label
  cc_word  <- switch(catchCategory, Lan = "Landings", Dis = "Discards",
                     BMS = "BMS Catch", All = "Catch")
  cc_title <- if (!is.null(distributions)) paste0(cc_word, " - ") else ""
  d <- data; colour_var <- colour; shape_var <- shape
  class_label <- cl
  cls <- tolower(sub(" .*", "", class_label))   # "age" or "length"
  # A summary built from a link that pool_plus_group() / pool_length_bins()
  # pooled says so in its `plus` / `bin_width` column, so the class is
  # labelled without being told (which is how the app draws it).
  if (is.null(plus_from) && !is.null(d) && "plus" %in% names(d) &&
      any(d$plus %in% TRUE)) {
    pc <- unique(d$.class[d$plus %in% TRUE])
    if (length(pc) == 1) plus_from <- pc
  }
  if (is.null(bin_from) && !is.null(d) && "bin_width" %in% names(d) &&
      any(is.finite(d$bin_width))) {
    bwv <- unique(d$bin_width[is.finite(d$bin_width)])
    if (length(bwv) == 1) bin_from <- bwv
  }

  p <- {
      if (is.null(d) || !nrow(d)) return(NULL)
      d <- reorder_mesh(d, c(colour_var, shape_var, facet1, facet2))
      has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
      # the plus class only exists when some age reached it
      plus_on <- !is.null(plus_from) && plus_from %in% d$.class
      if (!is.null(plus_from) && !plus_on)
        warning(fn, "no age reaches plus_group = ", plus_from,
                ", so there is no plus group", call. = FALSE)
      bin_on <- !is.null(bin_from)
      # the fish behind each point, when the data can say
      n_ok <- "n_fish" %in% names(d) && any(is.finite(d$n_fish) & d$n_fish > 0)
      bars <- show_samples && n_ok
      if (show_samples && !n_ok)
        warning(fn, "show_samples has nothing to draw: no fish counts ",
                "(the distributions carry no numMeasurements, or the table ",
                "has no n_fish column)", call. = FALSE)
      # the band only means something when strata were actually collapsed
      band <- !has(shape_var) && "sd_weight" %in% names(d) &&
        any(!is.na(d$sd_weight))

      grp <- if (has(colour_var)) rlang::sym(colour_var) else NULL
      p <- if (has(colour_var))
        ggplot(d, aes(x = .class, y = mean_weight,
                      colour = factor(!!grp), group = factor(!!grp)))
      else ggplot(d, aes(x = .class, y = mean_weight, group = 1))

      # Bars first, so they sit behind everything. They live on the weight
      # axis, scaled so the tallest stack reaches 75% of the tallest line -
      # tall enough that a small sample still shows, not just the big ones -
      # and a second axis on the right reads them back as fish.
      facet_on <- has(facet1) || has(facet2)
      kbar <- NULL
      if (bars) {
        top <- max(c(d$mean_weight, if (band) d$mean_weight + d$sd_weight),
                   na.rm = TRUE)
        if (!is.finite(top) || top <= 0) top <- 1
        tot <- d %>%
          group_by(across(all_of(c(".class", facet1, facet2)))) %>%
          summarise(n = sum(n_fish, na.rm = TRUE), .groups = "drop")
        # the global kbar is what the right axis itself is drawn to (one
        # secondary axis cannot vary per panel), so it stays exact for
        # whichever facet has the most fish measured
        kbar <- 0.75 * top / max(tot$n)
        bw <- { u <- sort(unique(d$.class)); if (length(u) > 1) min(diff(u)) else 1 }
        bd <- d[is.finite(d$n_fish), , drop = FALSE]
        if (facet_on) {
          # each facet's own max reaches 75% of the panel height too, not
          # just the biggest facet's - so a low-sample panel still shows a
          # bar. Boosted beyond what the (necessarily shared) right axis
          # reads for every facet but the biggest - see the caption note.
          fmax <- tot %>% group_by(across(all_of(c(facet1, facet2)))) %>%
            summarise(fmax = max(n), .groups = "drop")
          bd <- bd %>%
            left_join(fmax, by = intersect(c(facet1, facet2), names(fmax))) %>%
            mutate(.kbar = 0.75 * top / fmax)
        } else bd$.kbar <- kbar
        p <- p + if (has(colour_var))
          geom_col(data = bd, aes(y = n_fish * .kbar, fill = factor(!!grp)),
                   width = bw * 0.9, alpha = 0.25, colour = OUTLINE,
                   linewidth = OUTLINE_LW)
        else geom_col(data = bd, aes(y = n_fish * .kbar), width = bw * 0.9,
                      alpha = 0.25, fill = PAL_BASE[1], colour = OUTLINE,
                      linewidth = OUTLINE_LW)
      }

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
        # fill is mapped by the band, the colour-fill points and the bars;
        # once `shape` opens the strata up only the bars are left
        if (!has(shape_var) || bars)
          p <- p + scale_fill_var(colour_var, d[[colour_var]],
                                  name = colour_var, palette = palette)
      }

      # what a point is: the mean weight of every sampled stratum that
      # shares the plotted columns, weighted by numbers - everything else is
      # grouped into it, and the ribbon is the spread across exactly those
      # strata. Kept to a short, formula-like line each - the grouping
      # sentence already says what is shown and grouped, so this is only
      # how the marks are built.
      cc_lead <- if (is.null(distributions)) ""
                 else switch(catchCategory, Lan = "Landings ", Dis = "Discards ",
                            BMS = "BMS catch ", All = "Catch ")
      mw_word <- if (identical(cls, "age")) "MWaA" else "MWaL"
      n_word  <- if (identical(cls, "age")) "NaA" else "NaL"
      dots_txt <- sprintf("dots and lines = %s%s weight averaged by %s%s.",
                          cc_lead, mw_word, n_word, if (bars) " (left axis)" else "")
      ribbon_txt <- if (has(shape_var))
        sprintf("each %s drawn separately (no ribbon).", shape_var)
      else if (band) "ribbon = +/- 1sd across grouped strata."
      plus_txt <- if (plus_on)
        sprintf("%s+ = ages %s+ pooled (per-distribution mean).",
                format(plus_from), format(plus_from))
      bin_txt <- if (bin_on)
        sprintf("length classes pooled into %scm bins (per-distribution mean).",
                format(bin_from, trim = TRUE))
      # a point with no bar is not "no fish": its distributions report no count
      no_count <- bars && any(is.na(d$n_fish) | d$n_fish == 0)
      bars_txt <- if (bars)
        sprintf("bars = fish measured (right axis)%s.",
                if (no_count) "; no bar = count unknown" else "")
      scaled_txt <- if (bars && facet_on)
        "bars scaled per panel (right axis exact for the panel with the most fish)."
      cap <- sub_wrap(
        if (is.null(distributions)) NULL else strata_sentence(grp_vars, catch0),
        dots_txt, ribbon_txt, plus_txt, bin_txt, bars_txt, scaled_txt, width = 110)

      # the bars read back as fish on the right; they start on the panel floor
      p <- p + (if (bars)
        scale_y_continuous(labels = scales::comma,
                           expand = expansion(mult = c(0, 0.05)),
                           sec.axis = sec_axis(~ . / kbar, name = "Fish measured (n)",
                                               labels = scales::comma))
        else scale_y_continuous(labels = scales::comma))
      # the pooled class is named for what it holds: "7+", not "10-15"
      p <- p + class_axis_scale(d$.class, plus_from = if (plus_on) plus_from,
                                bin_from = if (bin_on) bin_from)

      p <- p +
        facet_two(facet1, facet2, ncol = 2, scales = scales) +
        labs(title = paste0(cc_title, sprintf("Mean Weight at %s",
                                              sub(" .*", "", class_label))),
             caption = cap,
             x = class_label, y = "Mean weight (g)",
             colour = colour_var,
             fill = if (has(shape_var) && !bars) NULL else colour_var,
             shape = shape_var) +
        app_theme() + theme(plot.title = element_text(size = 11))
      # The theme keeps the legend inside the panel, bottom right. With bars
      # that is where the second axis is, and with a plus group it is the
      # right-hand end of the curve the eye is on - so it moves outside.
      if (bars || plus_on)
        p <- p + theme(legend.position = "right",
                       legend.justification = "center")
      p
  }
  p
}
