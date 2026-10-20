# =====================================================================
#  viz_plots.R   (coverage / discard-rate / biology-distribution plots)
#
#  ORIGIN: this project (ILVO / RCEF Workbench). Freely refactorable.
#  Was: R_development/0_Functions_plots.R
# =====================================================================

# ============================================================
#  Exploratory plots for the RCEF / discard-raising workflow.
# ------------------------------------------------------------
#  ggplot2 only. Every function takes a prepared data frame and returns
#  a ggplot object (or NULL when there is nothing to draw).
#
#  PUBLIC API (one function per view, one exploratory question each):
#    plot_coverage_heat/_bubble/_bar/_map  how well a set of strata is
#                                sampled - `what` switches discard / length /
#                                age, `catchCategory` the landings / discards
#                                part for length & age.
#    plot_DR()                   per-stratum discard-rate points.
#    plot_catches()              catch weight (stacked bars).
#    plot_NaLoA()                numbers at length/age (histogram).
#    plot_MWaLoA()               mean weight at length/age (line + points).
#
#  The last four look at the data before a step and after it with the same
#  arguments: the raising adds importedOrRaised to the catch and the
#  allocation adds sampledOrEstimated to the distributions, and that
#  variable - the mark, by default: a cross, a dotted line, a hatched bar - and
#  the title say which it is (status_marks() in R/viz_helpers.R).
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
#  The shared plumbing (facet layer, category columns, grouping sentence,
#  catch-part wording, the class axis of the distribution plots) lives in
#  R/viz_helpers.R, the data half of the distribution plots in R/rcef_length.R; the
#  raising flow diagram (plot_river()) and the comparison of raised catches
#  (plot_compare_raised()) live in R/viz_raising.R.
#
#  PACKAGE DEPENDENCIES
#    Imports: ggplot2, dplyr, tidyr, ggrepel, scales, rlang, stats, tools
#    Provided by the rest of the package (not redefined here, so nothing is
#    duplicated): build_coverage_strata() and the coverage_cells() that
#    pools it; app_theme() for the shared look; plot_colours() for every
#    colour.
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
#' example `"seasonValue"`, `"vesselFlagCountry"` or `"metier4"`) each area gets a labelled
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
#' ## Where a pie is placed
#'
#' Areas are placed at approximate centroids of the ICES divisions and
#' subdivisions. A coarser `area` column (`"ICESSubArea"`, `"ICESDivision"`)
#' is placed where its data is: at the mean of the centroids of the
#' `areaValue`s that have data in it. The pie of subarea 27.7 sits around 7.d
#' and 7.e when nothing comes from 7.a or 7.f, not in the middle of the whole
#' of 7, and 27.3 sits between 27.3.a.20 and 27.3.a.21 when those are the only
#' two areas with data. Only a level with no `areaValue` behind it - a table
#' aggregated to the subarea alone, say - takes the centroid of the area
#' itself, and a subarea or division the list leaves out (3, 3.d, 5 ...) has
#' one too, the mean of the listed areas inside it. An area with no centroid at
#' all is warned about by name and left off the map. The map needs the
#' suggested `maps` package.
#'
#' @param catch CEF catch data (with `areaValue` and the `area` / `group`
#'   columns on it), or a [build_coverage_strata()] table (its own `what` is
#'   then used).
#' @param distributions A CEF distributions table - needed for
#'   `what = "length"` / `"age"` with raw catch, ignored otherwise.
#' @param area Column holding the ICES area code: `"areaValue"` (default), or a
#'   coarser level such as `"ICESSubArea"` or `"ICESDivision"`, which needs
#'   `areaValue` in `catch` to be placed where the data is (see *Where a pie is
#'   placed*).
#' @param group Optional column: one boxed grid of pies per area, one pie per
#'   level. A `group` with a single level warns and draws one pie per area.
#' @param what,catchCategory As in [coverage-plots]. The weight word in the
#'   title comes from `what` and `catchCategory`.
#' @param scale Multiplies every pie radius (and, when grouped, the size of
#'   the box's grid cell).
#' @param palette `"default"`, `"colorblind"` or `"gradual"`: the colour of the
#'   sampled slice.
#' @inheritParams saving-plots
#' @return A ggplot object, or `NULL` (with a message) when nothing can be
#'   mapped.
#' @seealso [coverage-plots] for the heatmap, bubbles and bars;
#'   [build_coverage_strata()] for the table behind the pies.
#' @family coverage plots
#' @md
#' @examples
#' \dontrun{
#' if (requireNamespace("maps", quietly = TRUE)) {
#'   plot_coverage_map(catch)
#'   plot_coverage_map(catch, group = "seasonValue")
#'
#'   # one pie per subarea, placed at the mean of the areaValue centroids with data
#'   plot_coverage_map(catch, area = "ICESSubArea", group = "fleetGroup")
#' }
#' }
#' @export
plot_coverage_map <- function(catch, distributions = NULL, area = "areaValue",
                              group = NULL, what = "discard",
                              catchCategory = NULL, scale = 1,
                              palette = c("default", "colorblind", "gradual"),
                              save = NULL) {
  save_on_exit(save, "plot_coverage_map")
  fn <- "plot_coverage_map"
  palette <- match.arg(palette)
  area_var <- area; group_var <- group; bubble_scale <- scale
  has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
  if (!requireNamespace("maps", quietly = TRUE))
    stop("plot_coverage_map() needs the 'maps' package: install.packages(\"maps\")")
  # Approximate centroids (decimal degrees) of the ICES divisions and subdivisions a
  # stock assessment in the NE Atlantic normally uses - enough to place one pie per
  # area. Codes are looked up with or without the leading "27."; one with no
  # centroid is named in a warning (when `warn`) and left off the map.
  centroid <- function(codes, warn = TRUE) {
    tab <- data.frame(
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
               "10.a", "12", "14.a", "14.b"),
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
              -28.0, -10.0, -35.0, -25.0),
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
              46.0, 74.0, 60.0, 62.0),
      stringsAsFactors = FALSE)
    # A coarser level the list leaves out (3, 3.d, 5, 14 ...) sits at the mean of the
    # finest listed areas inside it - so a subarea or division with no listed
    # centroid of its own still has one.
    up <- unique(unlist(lapply(strsplit(tab$area, ".", fixed = TRUE), function(p)
      vapply(seq_len(length(p) - 1L), function(k) paste(p[seq_len(k)], collapse = "."), ""))))
    leaf <- tab[!tab$area %in% up, , drop = FALSE]
    for (a in setdiff(up, tab$area)) {
      k <- startsWith(leaf$area, paste0(a, "."))
      tab <- rbind(tab, data.frame(area = a, lon = mean(leaf$lon[k]), lat = mean(leaf$lat[k]),
                                   stringsAsFactors = FALSE))
    }
    key <- sub("^27\\.", "", trimws(as.character(codes)))
    m <- tab[match(key, tab$area), c("lon", "lat")]
    bad <- unique(key[is.na(m$lon) & nzchar(key)])
    if (warn && length(bad))
      warning("no centroid for ICES area(s): ", paste(bad, collapse = ", "),
              " - dropped from the map", call. = FALSE)
    data.frame(area = as.character(codes), lon = m$lon, lat = m$lat,
               stringsAsFactors = FALSE)
  }
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
  # Where a pie goes. A coarse area column (ICESSubArea, ICESDivision ...) says little
  # about where its data is: the pie of 27.7 belongs around 7.d and 7.e when nothing
  # comes from 7.a or 7.f, not in the middle of the whole of 7. So a level is placed
  # at the mean of the centroids of the areaValues that have data in it; only a level
  # with no areaValue behind it takes the centroid listed for the area itself.
  ac <- data.frame(area = unique(as.character(agg[[area_var]])),
                   lon = NA_real_, lat = NA_real_, stringsAsFactors = FALSE)
  src <- as.data.frame(catch)
  averaged <- FALSE
  if (!identical(area_var, "areaValue") && all(c(area_var, "areaValue") %in% names(src))) {
    # the rows with data: the weight the map is about (landings for discards, the
    # catch part for length and age) above zero - the same strata that get a pie
    parts <- catch_category_parts(if (is.null(catchCategory)) "All" else catchCategory)
    wt <- if (identical(what, "discard")) {
            if (is.na(coverage_table_what(src))) ifelse(src$catchCategory %in% "Lan", src$total, 0)
            else src$landings
          } else ifelse(src$catchCategory %in% parts, src$total, 0)
    has_data <- !is.na(wt) & wt > 0
    fine <- unique(data.frame(level = as.character(src[[area_var]][has_data]),
                              fine  = as.character(src$areaValue[has_data]),
                              stringsAsFactors = FALSE))
    fc <- centroid(unique(fine$fine), warn = FALSE)
    for (i in seq_len(nrow(ac))) {
      f <- fc[match(fine$fine[fine$level == ac$area[i]], fc$area), , drop = FALSE]
      if (any(!is.na(f$lon))) {
        ac$lon[i] <- mean(f$lon, na.rm = TRUE); ac$lat[i] <- mean(f$lat, na.rm = TRUE)
        averaged <- TRUE
      }
    }
  }
  own <- is.na(ac$lon)
  if (any(own)) {
    c0 <- centroid(ac$area[own])
    ac$lon[own] <- c0$lon; ac$lat[own] <- c0$lat
  }
  ac <- ac[!is.na(ac$lon), , drop = FALSE]
  agg <- agg[as.character(agg[[area_var]]) %in% ac$area, , drop = FALSE]
  if (!nrow(agg)) { message("plot_coverage_map(): no mappable areas."); return(invisible(NULL)) }
  agg$lon <- ac$lon[match(as.character(agg[[area_var]]), ac$area)]
  agg$lat <- ac$lat[match(as.character(agg[[area_var]]), ac$area)]
  agg$frac <- pmin(1, pmax(0, agg$cov / agg$w))

  lev <- NULL; ngrp <- 1L
  if (has(group_var)) {
    lev  <- mesh_var_levels(group_var, agg[[group_var]])
    if (is.null(lev)) lev <- sort(unique(as.character(agg[[group_var]])))
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
  fills <- c(sampled = plot_colours("safe", palette = palette, type = "status")[[1]],
             unsampled = "grey80")
  gmax  <- max(agg$w, na.rm = TRUE)
  # Without a group it is one pie at the centroid. With a group the
  # area's pies go in a tidy grid inside a labelled box: one cell per
  # level, left to right then top to bottom. Pie radius is sqrt(weight)
  # scaled so the busiest (area x level) cell anywhere fills its cell.
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
  ttl  <- paste0(cv$title, " by ",
                 paste(c(area_var, if (has(group_var)) group_var), collapse = " and "))
  # the area level itself is never "grouped over" - it is the unit each pie
  # is drawn at - so it goes in the sentence's own vars, group first
  grp_keys <- c(if (has(group_var)) group_var, area_var)
  cap  <- sub_wrap(
    strata_sentence(grp_keys, catch),
    sprintf("area = %s.", area_var),
    if (averaged) sprintf("pie position = mean of the areaValue centroids with data in each %s.",
                          area_var),
    sprintf("bubble size = %s weight (t).", weight_label),
    sprintf("coverage slice = %s.", cv$share),
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
#' All take the CEF catch (and `distributions` for length and age) or the
#' table [build_coverage_strata()] returns from it. Given the catch they build
#' that table at exactly the plotted columns; given the table they pool it to
#' them. So the numbers behind a figure can always be read as a table, at the
#' same aggregation. The plotted columns are read off the catch as it is -
#' [derive_fields()] run once on the raw data, and any column of your own - and
#' nothing is derived again.
#'
#' @details
#' ## What "sampled" means
#'
#' `what` chooses the question:
#'
#' * `"discard"` (default): does the stratum have a discard estimate? A landing
#'   has one when a discard row with a weight carries its `domainCatchDis`
#'   (same country, origin and variable type), as [catchRaising()] pairs them.
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
#' | Colour (default palette) | Status |
#' |:-------|:-------|
#' | dark green | 100% covered (and, when trips are active, more than `min_trip` trips) |
#' | green | covered at least `min_cov`, and passing every active threshold |
#' | yellow | one active threshold missed |
#' | orange | two active thresholds missed |
#' | dark orange | three missed: only for length / age, with both `min_trip` and `min_fish` active |
#' | red | no discard estimate / no linked distribution at all - never a threshold miss |
#' | blue | discards only, no landings (discard coverage) |
#'
#' The other palettes keep the same scale, from best to worst sampled.
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
#' stays a warning at `min_trip = 0`.
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
#' vesselFlagCountry x seasonValue, grouped over finer fleet levels
#' (fleetValue) and area (areaValue)"), so the same figure drawn at other
#' columns has other strata
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
#' `palette` recolours the bars, and the status scale of the heatmap and the
#' bubbles.
#'
#' ## Argument order
#'
#' What data (`catch`, `distributions`, `what`, `catchCategory`), then which
#' columns are the strata (`y`, `x` or `fill`, `facet`), then the thresholds
#' (`min_cov`, `min_trip`, `min_fish`), then `palette`.
#'
#' @param catch CEF catch data carrying the columns you plot by ([derive_fields()]
#'   output, or columns of your own), or a table from [build_coverage_strata()].
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
#' @param palette `"default"`, `"colorblind"` or `"gradual"`. Recolours the bar
#'   fill, or the status colours of the heatmap and the bubbles.
#' @inheritParams saving-plots
#' @return A ggplot object, or `NULL` when there is nothing to draw.
#' @seealso [build_coverage_strata()] for the table, [plot_coverage_map()],
#'   [plot_DR()].
#' @name coverage-plots
#' @family coverage plots
#' @md
#' @examples
#' \dontrun{
#'
#' # discard coverage: which metier x country strata carry a discard estimate?
#' plot_coverage_heat(catch, y = "metier4", x = "vesselFlagCountry")
#' plot_coverage_bubble(catch, y = "metier4", x = "vesselFlagCountry",
#'                      min_cov = 50, min_trip = 2)
#' plot_coverage_bar(catch, y = "metier4", fill = "vesselFlagCountry")
#'
#' # length coverage of the landings, with a floor on the measurements
#' plot_coverage_heat(catch, distributions, y = "metier4",
#'                    x = "vesselFlagCountry", what = "length", catchCategory = "Lan",
#'                    min_cov = 50, min_fish = 100)
#'
#' # the same figure from the table behind it
#' cov <- build_coverage_strata(catch, c("metier4", "vesselFlagCountry"))
#' plot_coverage_heat(cov, y = "metier4", x = "vesselFlagCountry", min_cov = 50)
#' }
NULL


#' @rdname coverage-plots
#' @export
plot_coverage_heat <- function(catch, distributions = NULL, y, x = NULL,
                               facet = NULL, what = "discard",
                               catchCategory = NULL,
                               min_cov = 50, min_trip = NULL, min_fish = NULL,
                               palette = c("default", "colorblind",
                                          "gradual"), save = NULL) {
  save_on_exit(save, "plot_coverage_heat")
  coverage_grid("heat", catch, distributions, y, x, facet, what, !missing(what),
                catchCategory, min_cov, min_trip, min_fish, match.arg(palette))
}


#' @rdname coverage-plots
#' @export
plot_coverage_bubble <- function(catch, distributions = NULL, y, x = NULL,
                                 facet = NULL, what = "discard",
                                 catchCategory = NULL,
                                 min_cov = 50, min_trip = NULL, min_fish = NULL,
                                 palette = c("default", "colorblind",
                                            "gradual"), save = NULL) {
  save_on_exit(save, "plot_coverage_bubble")
  coverage_grid("bubble", catch, distributions, y, x, facet, what, !missing(what),
                catchCategory, min_cov, min_trip, min_fish, match.arg(palette))
}


#' The grid behind the heatmap and the bubble plot
#'
#' The two plots are one figure drawn with two marks: the same cells, the same
#' status, the same legend, the same thresholds box and shortfall labels. Only
#' the mark differs - a tile carrying the weight as a number, or a bubble sized
#' by it - so the body is written once.
#'
#' @param kind `"heat"` or `"bubble"`.
#' @param what_given Whether the caller typed `what` (a coverage table brings
#'   its own).
#' @inheritParams coverage-plots
#' @return A ggplot object, or `NULL` when there is nothing to draw.
#' @keywords internal
#' @noRd
#' @export
coverage_grid <- function(kind, catch, distributions, y, x, facet, what,
                          what_given, catchCategory, min_cov, min_trip,
                          min_fish, palette) {
  fn <- paste0("plot_coverage_", kind)
  heat <- identical(kind, "heat")
  y_var <- y
  if (is.null(y) || !nzchar(y)) stop(fn, "(): `y` is required", call. = FALSE)
  cv <- coverage_cells(catch, distributions, c(y, x, facet), what, what_given,
                       catchCategory, fn)
  if (is.null(cv)) return(NULL)
  what <- cv$what
  is_bio <- !identical(what, "discard")
  on <- function(v) !is.null(v) && !(length(v) == 1 && is.na(v))
  if (!is_bio && on(min_fish))
    stop(fn, "(): min_fish does not apply to what = \"discard\" (no fish ",
         "are measured for a discard estimate - min_fish is for length / ",
         "age)", call. = FALSE)
  trip_on <- on(min_trip)
  meas_on <- is_bio && on(min_fish)

  # ---- the sampling status a cell can have, best to worst ---------------------------
  # The colour of a cell is its status against the thresholds, worked out here, at
  # plot time: the status is a colour on a figure, not a column of the coverage
  # table (the table reports numbers; the thresholds are a plotting choice).
  # Coverage is always part of the verdict. min_trip (discards, length / age) and
  # min_fish (length / age) are each independently optional - NULL drops that
  # question entirely rather than giving it an always-passing threshold. A cell is:
  #   full       100% covered, with more than min_trip trips and at least min_fish
  #              measurements, for the thresholds that are active
  #   safe       every active threshold met
  #   warn_...   one status per specific combination of missed thresholds (warn_cov,
  #              warn_trip, warn_cov_trip, ...); only the combinations of the active
  #              thresholds exist, and the colour counts how many were missed
  #   no_dist    no discard estimate (or no linked length / age distribution) at all:
  #              nothing to judge against a threshold
  #   disc_only  discards only, no landings (discards only)
  #   none       no catch: the cell has nothing to judge, so it draws nothing
  # A cell with no reported trip count fails an active trips test however low
  # min_trip is set - unknown is not the same as adequate.
  #
  # One entry per ACTIVE threshold, in the fixed (cov, trip, meas) order, with the
  # short phrase it shows when it passes and when it fails (and in the "full" row).
  defs <- list(cov = c(full = "100% cov", pass = sprintf(">=%s%% cov", min_cov),
                       fail = sprintf("<%s%% cov", min_cov)))
  if (trip_on) defs$trip <- c(full = sprintf(">%st", min_trip),
                              pass = sprintf(">=%st", min_trip),
                              fail = sprintf("<%st", min_trip))
  if (meas_on) defs$meas <- c(full = sprintf(">=%sm", min_fish),
                              pass = sprintf(">=%sm", min_fish),
                              fail = sprintf("<%sm", min_fish))
  combos <- unlist(lapply(seq_along(defs), function(k)
    utils::combn(names(defs), k, simplify = FALSE)), recursive = FALSE)
  says <- function(ms) paste(vapply(names(defs), function(nm)
    defs[[nm]][[if (nm %in% ms) "fail" else "pass"]], ""), collapse = ", ")
  key <- data.frame(
    cat_key = c("full", "safe",
                paste0("warn_", vapply(combos, paste, "", collapse = "_")),
                "no_dist", if (!is_bio) "disc_only"),
    abbr = c(paste(vapply(defs, `[[`, "", "full"), collapse = ", "),
             paste(vapply(defs, `[[`, "", "pass"), collapse = ", "),
             vapply(combos, says, ""),
             if (is_bio) sprintf("no %s dist", what) else "no discard estimate",
             if (!is_bio) "discards only, no landings"),
    stringsAsFactors = FALSE)
  key$colour <- unname(plot_colours(key$cat_key, palette = palette,
                                    type = "status"))

  # ---- the verdict on each cell ------------------------------------------------------
  d <- cv$cells
  if (is.null(d$numTrips)) d$numTrips <- NA_real_
  if (is_bio) {
    d$landings <- d$total                    # the weight the plots label and size by
    d$n_measurements <- ifelse(is.na(d$numMeasurements), 0, d$numMeasurements)
    no_catch  <- is.na(d$total) | d$total <= 0
    disc_only <- rep(FALSE, nrow(d))
    has_est   <- !is.na(d$covered) & d$covered > 0
  } else {
    d$n_measurements <- NA_real_
    no_catch  <- d$landings <= 0 & (is.na(d$total) | d$total <= 0)
    disc_only <- d$landings <= 0 & !no_catch
    has_est   <- !is.na(d$total)
  }
  if (trip_on && nrow(d) && all(is.na(d$numTrips)))
    warning(fn, "(): min_trip is active but every stratum's numTrips is ",
            "unknown (NA) - none of them can pass the trips test, at any ",
            "min_trip value including 0. Pass min_trip = NULL to drop trips ",
            "from the classification, or check that the data actually ",
            "carries a trip count.", call. = FALSE)
  if (meas_on && isTRUE(min_fish > 0) && nrow(d) && all(d$n_measurements == 0))
    warning(fn, "(): min_fish is active but every stratum's numMeasurements ",
            "is 0 - none of them can pass the measurements test, at any ",
            "min_fish value above 0. Pass min_fish = NULL to drop ",
            "measurements from the classification, or check that the data ",
            "actually carries measurement counts.", call. = FALSE)
  cov_ok  <- !is.na(d$coverage) & d$coverage >= min_cov
  # a cell with no reported trip count is taken to fail the trips test
  trips   <- if (trip_on) ifelse(is.na(d$numTrips), min_trip - 1, d$numTrips)
  trip_ok <- if (trip_on) trips >= min_trip else rep(TRUE, nrow(d))
  meas_ok <- if (meas_on) d$n_measurements >= min_fish else rep(TRUE, nrow(d))
  n_miss  <- (!cov_ok) + (!trip_ok) + (!meas_ok)
  is_full <- cov_ok & trip_ok & meas_ok & d$coverage >= 100 &
    (if (trip_on) trips > min_trip else TRUE)
  d$cat_key <- dplyr::case_when(
    no_catch    ~ "none",
    disc_only   ~ "disc_only",
    !has_est    ~ "no_dist",
    is_full     ~ "full",
    n_miss == 0 ~ "safe",
    TRUE        ~ paste0("warn", ifelse(!cov_ok, "_cov", ""),
                         ifelse(!trip_ok, "_trip", ""),
                         ifelse(!meas_ok, "_meas", "")))
  # a cell with no weight at all has nothing to judge: it draws nothing
  d <- d[d$cat_key != "none", , drop = FALSE]
  if (!nrow(d)) return(NULL)
  d$Status <- factor(d$cat_key, levels = key$cat_key)

  # the text of the thresholds box: what each active threshold means
  behind <- switch(what, length = "length distributions", age = "age distributions",
                   "discard estimates")
  thr_lines <- c(sprintf("coverage >= %s%%: %s.", min_cov, cv$share),
                 if (trip_on)
                   sprintf("trips >= %s: sampled trips behind the %s, summed across strata.",
                           min_trip, behind),
                 if (meas_on)
                   sprintf("measured fish >= %s: fish measured for %s estimates, summed across strata.",
                           format(min_fish, big.mark = ",", trim = TRUE),
                           if (identical(what, "age")) "NaA/MWaA" else "NaL/MWaL"),
                 "labels: each unmet threshold is shown as a label.")
  d <- category_cols(d, c(y, x, facet))
  x_var <- x
  if (is.null(x_var) || !nzchar(x_var)) { d$.x <- "All"; x_var <- ".x" }

  # What a warning cell missed, read off its status key (so the label always
  # agrees with the colour): blank for every cell that is not a warning. The
  # heatmap puts it on one line under the tile number, the bubble grid on
  # repelled lines of its own.
  miss <- function(t) grepl(paste0("^warn_.*", t), d$cat_key)
  s_cov  <- ifelse(miss("cov"), paste0(round(d$coverage, 1), "%"), "")
  s_meas <- ifelse(miss("meas"), paste0("n=", format(round(d$n_measurements),
                                                     big.mark = ",", trim = TRUE)), "")
  s_trip <- ifelse(miss("trip"),
                   ifelse(is.na(d$numTrips), "trips n/a",
                          paste0(round(d$numTrips), " trip",
                                 ifelse(round(d$numTrips) == 1, "", "s"))), "")
  d$lab <- vapply(seq_len(nrow(d)), function(i) {
    b <- c(s_cov[i], s_meas[i], s_trip[i])
    paste(b[nzchar(b)], collapse = if (heat) ", " else "\n")
  }, character(1))

  # One zero-size mark per status, so the legend carries a filled key for the
  # statuses no cell of this particular figure lands in - added last so the
  # real marks stay layer 1. A discrete scale can list an unused level (drop =
  # FALSE + limits), but a geom has no data row to draw its swatch from.
  dummy <- d[rep(1L, nrow(key)), , drop = FALSE]
  dummy$Status <- factor(key$cat_key, levels = key$cat_key)
  dummy$landings <- NA_real_
  if (!heat) dummy$plot_size <- NA_real_
  key_layer <- if (heat)
    geom_tile(data = dummy, aes(fill = .data$Status), width = 0, height = 0,
              na.rm = TRUE, show.legend = TRUE)
  else geom_point(data = dummy, aes(fill = .data$Status), size = 0, shape = 21,
                  stroke = 0, na.rm = TRUE, show.legend = TRUE)
  fill_scale <- do.call(scale_fill_manual, c(
    list(values = setNames(key$colour, key$cat_key), limits = key$cat_key,
         labels = setNames(key$abbr, key$cat_key), drop = FALSE,
         name = "Sampling status"),
    if (!heat) list(guide = guide_legend(override.aes = list(size = 5)))))

  if (heat) {
    fmt <- function(v) ifelse(is.na(v) | v <= 0, "",
                       ifelse(v > 99, sprintf("%.0f", v),
                       ifelse(v >= 10, sprintf("%.1f", v), sprintf("%.2f", v))))
    # the shortfall is one line under the tile number: nudge the number up to
    # make room for it
    d$.num_vjust <- ifelse(nzchar(d$lab), -0.2, 0.5)
    p <- ggplot(d, aes(x = factor(.data[[x_var]]), y = .data[[y_var]], fill = Status)) +
      geom_tile(colour = OUTLINE, alpha = 0.9) +
      geom_text(aes(label = fmt(landings), vjust = .data$.num_vjust), size = 2.6,
                fontface = "bold", colour = OUTLINE) +
      geom_text(aes(label = lab), vjust = 1.3, size = 2.2, fontface = "bold",
                colour = "red3")
    mark_txt <- sprintf("tile number = %s weight (t).", cv$weight_label)
  } else {
    # one bubble per cell: the cells are already pooled to the plotted columns,
    # so no two bubbles share a point. A discards-only cell has no landings to
    # size it by, so it is drawn as small as the smallest bubble and crossed.
    smallest <- suppressWarnings(min(d$landings[d$landings > 0], na.rm = TRUE))
    if (!is.finite(smallest)) smallest <- 1
    d$plot_size <- ifelse(d$cat_key == "disc_only", smallest, d$landings)
    d$lab[d$cat_key == "disc_only"] <- "100%\nDisc"
    # A bubble plot says two things at once: status in the colour, weight in the
    # diameter. The second is guesswork without a scale bar, so the breaks are
    # pinned here (smallest / median / largest actually drawn): a key circle is
    # literally the size that value has in the panel.
    sz_vals <- d$plot_size[is.finite(d$plot_size) & d$plot_size > 0]
    sz <- list(limits = NULL, breaks = ggplot2::waiver(), labels = ggplot2::waiver())
    if (length(sz_vals)) {
      sz$limits <- range(sz_vals)
      sz$breaks <- unique(c(sz$limits[1], stats::median(sz_vals), sz$limits[2]))
      sz$labels <- format(signif(sz$breaks, 3), big.mark = ",", trim = TRUE,
                          scientific = FALSE)
    }
    p <- ggplot(d, aes(x = factor(.data[[x_var]]), y = .data[[y_var]])) +
      geom_point(aes(size = plot_size, fill = Status), shape = 21,
                 colour = OUTLINE, stroke = 0.8) +
      geom_point(data = filter(d, cat_key == "disc_only"),
                 shape = 4, colour = OUTLINE, size = 2, stroke = 1) +
      ggrepel::geom_text_repel(aes(label = lab), size = 2.8, fontface = "bold",
                               colour = "red3", box.padding = 0.6,
                               min.segment.length = 0, segment.size = 0.2,
                               segment.colour = "grey40", max.overlaps = Inf)
    mark_txt <- sprintf("bubble size = %s weight (t).", cv$weight_label)
  }
  p <- p + key_layer
  if (!heat)
    p <- p +
      scale_size_continuous(range = c(4, 14), limits = sz$limits,
                            breaks = sz$breaks, labels = sz$labels,
                            name = sprintf("%s (t)", tools::toTitleCase(cv$weight_label))) +
      scale_x_discrete(expand = expansion(0.2))

  # The thresholds, in a boxed guide of their own under the panel, right-
  # justified under the in-panel status legend: a detail section of it, not a
  # caption about the plot. Added after app_theme(), whose complete theme resets
  # the legend justification.
  thr_box <- grid::textGrob(paste(thr_lines, collapse = "\n"), x = 0, y = 1,
                            hjust = 0, vjust = 1,
                            gp = grid::gpar(fontsize = 8, col = "grey25",
                                            lineheight = 1.1))
  p + fill_scale +
    facet_layer(facet, scales = "free_y") +
    labs(title = cv$title,
         caption = sub_wrap(strata_sentence(c(y, x, facet), catch), mark_txt,
                            width = 110),
         x = if (x_var == ".x") NULL else x_var, y = y_var) +
    app_theme() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(thresholds = guide_custom(
      thr_box, title = "Thresholds", position = "bottom", order = 99,
      theme = theme(legend.title = element_text(face = "bold", size = 8)))) +
    theme(legend.justification.bottom = "right")
}


#' @rdname coverage-plots
#' @export
plot_coverage_bar <- function(catch, distributions = NULL, y, fill = NULL,
                              facet = NULL, what = "discard",
                              catchCategory = NULL,
                              palette = c("default", "colorblind",
                                         "gradual"), save = NULL) {
  save_on_exit(save, "plot_coverage_bar")
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
  # is by landings, not by category - see below), so the size order of mesh
  # (and metier4_mesh) has to be reapplied here rather than carried through
  # from the column itself
  fill_lev <- mesh_var_levels(if (has(fill_var)) fill_var else y_var, d$.fill)
  if (!is.null(fill_lev)) d$.fill <- factor(d$.fill, levels = fill_lev)
  facet_lev <- mesh_var_levels(facet_var, d$.facet)
  if (!is.null(facet_lev)) d$.facet <- factor(d$.facet, levels = facet_lev)

  wi <- paste("With", what, "sampling")
  wo <- paste("Without", what, "sampling")

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
    scale_fill_manual(values = plot_colours(d$.fill, if (has(fill_var)) fill_var else y_var,
                                            palette),
                      name = if (has(fill_var)) fill_var else y_var,
                      na.value = "grey85") +
    facet_layer(if (has(facet_var)) ".facet", scales = "free_y") +
    labs(title = cv$title,
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
#  Discard rate and catch weight, before and after the raising
# ----------------------------------------------------------

#' Discard rate per stratum, before or after the raising
#'
#' @description
#' One point per stratum, its discard rate on the y axis and its landings as
#' the point size - the finest a stratum gets, never an average of finer ones.
#' Given raw catch it shows the rates as they were reported, with a thin line
#' behind the points marking the mean rate of each `x` level, weighted by
#' landings, and an optional `reference` that draws a dashed line and names the
#' strata above it. Given the raised catch (the output of [catchRaising()]) it
#' shows what the raising did: a stratum whose discards came with the data
#' (*imported*) is drawn as a circle, one whose discards the raising added
#' (*raised*) as a cross. The arguments are the same either way, and
#' so is the plot; the title says which it is.
#'
#' @details
#' ## What a point is
#'
#' A stratum is one row of the discard coverage table
#' ([build_coverage_strata()]). Given a discard coverage table, its rows are
#' drawn exactly as they are - one point per row. Given catch, raw or raised,
#' the table is built at the finest resolution the catch carries (`fleetValue`,
#' `areaValue`, `vesselFlagCountry`, `seasonValue`, whichever are present)
#' *together with* `x` / `colour` / `shape` / `facet`, so a point is always an
#' individual, un-pooled stratum: several points can share one `x` level.
#' `strata_vars` (the columns behind a point's label, see below) is exactly this
#' set. Strata with no discard estimate have no rate and are not drawn: a rate
#' is a property of sampled strata only.
#'
#' ## Before and after the raising
#'
#' The raising adds `importedOrRaised` to the catch, and with it the plot knows
#' what the raising did. `importedOrRaised` is a variable like any other. By
#' default it is the mark (`alpha = "importedOrRaised"`): imported strata are
#' circles, raised ones crosses (in the colour a circle would be filled with).
#' When `shape` is already a column of its own the symbol is taken, and raised
#' strata are transparent circles instead. Name it in `x`, `colour`, `shape` or
#' `facet` to draw it there instead - a colour each, a panel each, two columns
#' of points - and it is then not drawn as a mark as well. `alpha = NULL` draws
#' no mark, and unless it is named elsewhere imported and raised strata are
#' drawn alike. Whichever way, it is worked out per stratum (a stratum is raised
#' when its discard rows were), never from the landings rows next to them, which
#' are always imported; a stratum that holds both kinds of discard row is raised.
#'
#' The title says what the strata drawn are: *imported + raised* when the
#' raising added some of them, *only imported* when it did not - a raw catch,
#' or a raised catch in which nothing needed raising - and then there is no
#' mark, because there is nothing to tell apart.
#'
#' Put the raising group on `x` (`"DrGroup"`): a group's crosses all
#' sit on one line, the rate it was raised at, and its circles show how
#' far the strata it was built from scatter around that line - a group whose
#' circles are spread far from it is a guess dressed up as a group. Only
#' the strata of the raised catch are drawn, so the donors of a group wider
#' than its own strata (a REST group raised from every gear, say) appear under
#' the groups they themselves belong to.
#'
#' A raised stratum whose group had no donor to draw a rate from comes out of
#' the raising as `NaN`: it has no rate to plot and is left out, with a message
#' saying how many.
#'
#' Once strata are raised the crosses are themselves the reference,
#' so the weighted-mean line, the reference line's label and the per-point
#' labels, which help to read the rates as they were reported, are left out;
#' the dashed `reference` line stays.
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
#'   discards. It stays between 0 and
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
#' headline. Before the raising only.
#'
#' ## The reference line
#'
#' `reference` is a rate on the same scale as the axis, as a fraction (`0.3` is
#' 30%; with `rate = "rate"` it must lie between 0 and 1). A dashed line marks
#' it and, before the raising, the strata above it are labelled with their
#' identity - the finest column of each stratum family present (so a coarser
#' alias already determined by a finer one you also plotted, `metier4` alongside
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
#' @param catch Raw CEF catch data, the raised catch ([catchRaising()]), or a
#'   discard coverage table from [build_coverage_strata()].
#' @param x Column on the x axis: one point per level (times `colour`,
#'   `shape` and `facet`). Can be `"DrGroup"` or `"importedOrRaised"`.
#'   Required.
#' @param colour Column mapped to the point fill; `"importedOrRaised"`
#'   included.
#' @param shape Column mapped to the point shape; `"importedOrRaised"`
#'   included.
#' @param facet Optional facet column; `"importedOrRaised"` included.
#' @param alpha `"importedOrRaised"` (default) marks what the raising added:
#'   circles for imported strata, crosses for raised ones (transparent circles
#'   when `shape` is a column). `NULL` draws no mark. Left out when
#'   `importedOrRaised` is the `x`, `colour`, `shape` or `facet`, and when
#'   nothing was raised.
#' @param rate `"ratio"` (default, discards / landings) or `"rate"` (discards
#'   / catch): which discard rate is the y variable. See *The rate*.
#' @param reference Optional single rate, as a fraction (`0.3` = 30%), on the
#'   scale of `rate`. Draws a dashed line and, before the raising, labels the
#'   strata above it.
#' @param jitter Jitter the points horizontally. Cosmetic only - one point is
#'   still one stratum.
#' @param palette `"default"`, `"colorblind"` or `"gradual"`: the point
#'   colours.
#' @inheritParams saving-plots
#' @return A ggplot object, or `NULL` when no stratum has a discard rate.
#' @seealso [build_coverage_strata()] for the table behind the points;
#'   [coverage-plots] for how well those strata are sampled; [plot_catches()]
#'   for the weight itself, before or after the raising.
#' @family discard-rate plots
#' @md
#' @examples
#' \dontrun{
#'
#' plot_DR(catch, x = "metier4", colour = "vesselFlagCountry")
#' plot_DR(catch, x = "vesselFlagCountry", colour = "metier4",
#'        facet = "seasonValue")
#'
#' # discards as a share of the catch, with a reference line: the strata
#' # above 20% are named, in full
#' plot_DR(catch, x = "metier4", colour = "vesselFlagCountry",
#'        rate = "rate", reference = 0.2)
#'
#' # the same plot of the raised catch: imported are circles, raised crosses
#' raised <- catchRaising(catch, strataCond, logFile = tempfile(fileext = ".txt"))
#' plot_DR(raised, x = "DrGroup", colour = "metier4")
#'
#' # imported against raised as the panels instead of the crosses
#' plot_DR(raised, x = "metier4", colour = "vesselFlagCountry",
#'        facet = "importedOrRaised")
#'
#' # the table behind the points
#' cov <- build_coverage_strata(catch, c("metier4", "vesselFlagCountry"))
#' head(cov[!is.na(cov$rate), c("metier4", "vesselFlagCountry", "total", "rate",
#'                              "discardRate")])
#' }
#' @export
plot_DR <- function(catch, x, colour = NULL, shape = NULL, facet = NULL,
                    alpha = "importedOrRaised", rate = c("ratio", "rate"),
                    reference = NULL, jitter = TRUE,
                    palette = c("default", "colorblind", "gradual"), save = NULL) {
  save_on_exit(save, "plot_DR")
  palette <- match.arg(palette)
  rate_type <- match.arg(rate)
  S <- "importedOrRaised"
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
    # Raw or raised catch: build at the finest the catch carries, together with
    # whatever is plotted - not pooled to just the plotted columns, or a
    # point (and its label) could be many different areas and seasons
    # wearing one name. A minimal catch with none of the four keeps the old
    # pooled-to-plotted-columns behaviour (nothing finer to build at).
    # importedOrRaised is no stratum column: it is the table's own, per stratum.
    base_dims <- intersect(c("fleetValue", "areaValue", "vesselFlagCountry", "seasonValue"),
                           names(as.data.frame(catch)))
    d <- build_coverage_strata(catch, dims = unique(c(base_dims, setdiff(keys, S))))
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
         paste(miss, collapse = ", "),
         if (S %in% miss) " - importedOrRaised comes with the output of catchRaising()",
         call. = FALSE)

  # rate is NA for a stratum with no discard estimate (nobody looked -
  # not the same as looked and found none), so those simply are not drawn -
  # whichever rate is plotted, so the two show the same strata. A raised
  # stratum is another matter: it was meant to have one, and its group had no
  # donor to draw it from.
  n_none <- if (S %in% names(d)) sum(d[[S]] %in% "raised" & is.na(d$total)) else 0L
  if (n_none)
    message("plot_DR(): ", n_none, " raised ",
            if (n_none == 1) "stratum has" else "strata have",
            " no rate (their group had no donor to draw one from) and ",
            if (n_none == 1) "is" else "are", " left out.")
  d <- d[!is.na(d$rate), , drop = FALSE]
  if (!nrow(d)) return(NULL)
  # what is drawn - the strata that have a rate - is what the title speaks of
  present <- if (S %in% names(d)) unique(as.character(d[[S]]))
  st <- status_marks(S, alpha, keys, present, "plot_DR(): ",
                     override = list(fill = "grey30", shape = 21, size = 3.5))
  raised <- "raised" %in% present
  d <- category_cols(d, keys)
  if (S %in% names(d)) d$.status <- st$status(d[[S]])
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
  # Every bubble is shifted up by 10% of the largest landings in THIS plot
  # (not a fixed tonnage, so it adapts to the data): area scaling alone
  # shrinks a tiny stratum's bubble to a sliver too thin to read its
  # colour. An ADDITIVE shift, not a floor that clamps every small value to
  # the same number - that gave every small stratum the exact same bubble,
  # no contrast between them at all. Shifting instead keeps every stratum's
  # bubble a little bigger than the last, all the way down.
  if (has_landings)
    d$landings <- d$landings + max(d$landings, na.rm = TRUE) * 0.1
  # Raised points go down first and the big bubbles before the small ones, so
  # an imported point is never buried under a bigger bubble.
  if (".status" %in% names(d))
    d <- d[order(d$.status == "Imported", -d$landings), , drop = FALSE]

  # What a point is: every stratum column the table carries - the table's
  # own dims, drawn as they are. This is the finest available (see above),
  # so it is also what a label needs to identify the point in full.
  # "Stratum" is excluded outright: it is fleetValue/vesselFlagCountry/
  # areaValue/seasonValue already joined into one string, a composite of
  # four OTHER columns rather than a nested level of any one of them, so
  # strata_families() has no family to ever drop it from - it would just
  # repeat the same four values again, pasted together. A
  # derived column that rides along on the table but is NOT reliably defined here (NA on
  # some point) is also excluded - it is not part of what THIS table's
  # strata are, just a bonus that happened to ride along, and a label or a
  # grouping sentence showing "NA" for it would say nothing true.
  strata_vars <- setdiff(names(d), c("catchCategory", "variableUnit",
                                     "weightingCategory", "total",
                                     "weightingTotal", "rate",
                                     "discardRate", "landings", "coverage",
                                     "numTrips", ".y", "Stratum", S, ".status"))
  strata_vars <- strata_vars[!vapply(strata_vars,
                                     function(v) anyNA(d[[v]]), logical(1))]
  # The rates as they were reported get a backdrop: the mean of each x level
  # (per panel), weighted by the landings behind each rate - for the ratio
  # exactly the rate of that level built directly - and, over a reference, the
  # strata named. Once strata are raised the crosses are the
  # reference, and these only crowd them.
  backdrop <- !raised
  wm <- NULL
  if (backdrop) {
    wkeys <- unique(c(x_var, if (has(facet_var)) facet_var))
    wm <- d %>%
      group_by(across(all_of(wkeys))) %>%
      summarise(.y = stats::weighted.mean(.y, weightingTotal), .groups = "drop") %>%
      filter(is.finite(.y))
  }
  # Labels for the strata above the reference: bare values, only the finest
  # column of each family (a coarser alias already determined by a finer one
  # you also plotted - metier4 alongside fleetValue, say - says nothing extra
  # and is dropped) so the point can be told apart from another that only
  # shares some of its columns. Blank for every other point, so the label
  # layer has exactly the rows of the point layer and the same jitter puts
  # each label on its point.
  coarser <- unlist(lapply(strata_families(), function(m) {
    s <- intersect(strata_vars, names(m))
    if (length(s) > 1) s[m[s] > min(m[s])]
  }))
  lab_vars <- setdiff(strata_vars, coarser)
  ref_y <- if (is.null(reference)) NULL else reference * 100
  lab_all <- do.call(paste, c(lapply(lab_vars, function(v) d[[v]]), sep = "|"))
  d$.lab <- if (is.null(ref_y) || !backdrop) "" else ifelse(d$.y > ref_y, lab_all, "")

  # What marks a raised stratum: a cross. It has no fill, so it takes the colour a
  # circle is filled with (see cross_marks()). Not when `shape` is already a column:
  # then the marks are the transparency of the points.
  cross <- st$on && !has(shape_var)
  if (cross) d <- cross_marks(d, if (has(colour_var)) colour_var, 1.2, 0.5)
  pos <- if (jitter) position_jitter(width = 0.18, height = 0, seed = 1) else "identity"
  ae <- aes(x = .data[[x_var]], y = .y)
  if (has(colour_var)) ae <- modifyList(ae, aes(fill = .data[[colour_var]]))
  if (has(shape_var))  ae <- modifyList(ae, aes(shape = .data[[shape_var]]))
  if (has_landings)    ae <- modifyList(ae, aes(size = landings))
  if (cross)           ae <- modifyList(ae, aes(shape = .status, colour = .edge, stroke = .stroke))
  else if (st$on)      ae <- modifyList(ae, aes(alpha = .status))

  # only shapes 21-25 take a fill, so pin shape 21 when nothing maps to shape.
  # size is either mapped (added to ae above, scaled below) or fixed at the
  # old constant - never both, which is what a geom_point(size = ...) call
  # alongside an aes(size = ...) mapping would otherwise silently conflict on.
  # The same for alpha: mapped to imported / raised, or fixed. With crosses the
  # shape, the colour and the stroke are all mapped.
  pt_args <- list(position = pos)
  if (!cross)           pt_args <- c(pt_args, list(colour = OUTLINE, stroke = 0.5))
  if (!has(shape_var) && !cross) pt_args$shape <- 21
  if (!has_landings)    pt_args$size  <- 3
  if (!st$on || cross)  pt_args$alpha <- 0.85
  p <- ggplot(d, ae)
  # the reference goes first, so the points sit on top of the line
  if (!is.null(ref_y))
    p <- p + geom_hline(yintercept = ref_y, linetype = "dashed",
                        colour = "red3", linewidth = 0.5)
  if (!is.null(ref_y) && backdrop)
    p <- p + annotate("text", x = -Inf, y = ref_y, hjust = -0.05, vjust = -0.5,
                      size = 3, colour = "red3",
                      label = sprintf("reference %s%%", format(round(reference * 100, 1),
                                                               trim = TRUE)))
  # the weighted mean sits behind the strata: a thin, muted line - no
  # whiskers, since geom_errorbar's top and bottom cap coincide at
  # ymin = ymax - and no marker at its centre, so it reads as a backdrop
  # the points sit on, not a headline of its own.
  if (backdrop)
    p <- p + geom_errorbar(data = wm, aes(x = .data[[x_var]], ymin = .y, ymax = .y),
                           inherit.aes = FALSE, width = 0.6, colour = "grey45",
                           linewidth = 0.4)
  p <- p + do.call(geom_point, pt_args)
  if (!is.null(ref_y) && backdrop)
    p <- p + ggrepel::geom_text_repel(
      data = d, aes(x = .data[[x_var]], y = .y, label = .lab),
      inherit.aes = FALSE, position = pos, size = 2.6, fontface = "bold",
      colour = "red3", box.padding = 0.4, min.segment.length = 0,
      segment.size = 0.2, segment.colour = "grey40", max.overlaps = Inf)
  if (cross) {
    p <- p + st$shape +
      scale_colour_manual(values = c(if (has(colour_var)) plot_colours(d[[colour_var]], colour_var, palette),
                                     .outline = OUTLINE, .cross = plot_colours(palette = palette)),
                          na.value = "grey60", guide = "none") +
      scale_continuous_identity("stroke", guide = "none") +
      guides(shape = guide_legend(override.aes = list(
        fill = "grey30", colour = "grey30", size = 3.5, stroke = c(0.5, 1.2))))
    if (has_landings)
      p <- p + guides(size = guide_legend(override.aes = list(
        shape = 21, colour = OUTLINE, fill = "grey85")))
  } else if (st$on) p <- p + st$scale
  if (has(shape_var))
    p <- p + scale_shape_manual(values = c(21, 22, 23, 24, 25, 1, 2, 0))
  # the fill legend keys inherit the default shape (19), which takes no
  # fill, so the swatches come out empty unless forced to a filled shape
  if (has(colour_var))
    p <- p + guides(fill = guide_legend(override.aes = c(
      list(shape = 21, size = 3.5, alpha = 1), if (cross) list(colour = OUTLINE))))
  if (has(shape_var))
    p <- p + guides(shape = guide_legend(
      override.aes = list(fill = "grey45", size = 3.5, alpha = 1)))
  if (has(colour_var))
    p <- p + scale_fill_manual(values = plot_colours(d[[colour_var]], colour_var, palette),
                               name = colour_var, na.value = "grey85")
  # area-, not radius-, proportional - the honest way to size a point by
  # a weight, and the same convention the coverage bubble plots already use.
  if (has_landings)
    p <- p + scale_size_area(max_size = 10, labels = scales::comma,
                             name = "Landings (t)")

  # the ratio can exceed 100% - and a few tiny strata (a few kg of covered
  # landings) give ratios of many hundreds of percent that on a plain axis
  # flatten everything else against the floor. Pseudo-log: linear near zero
  # (zero stays zero), logarithmic above ~5%, so the bulk and the extremes
  # are both readable. The rate stays within 0-100%, so it gets an ordinary
  # axis.
  grouping <- strata_sentence(strata_vars, catch)
  if (raised)
    grouping <- sub("as in CEF (CN) data", "per strata including imported and raised",
                    grouping, fixed = TRUE)
  p <- p +
    (if (identical(rate_type, "ratio"))
       scale_y_continuous(
         transform = scales::transform_pseudo_log(sigma = 5),
         breaks = c(0, 5, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000),
         labels = function(z) paste0(round(z), "%"))
     else
       scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20),
                          labels = function(z) paste0(round(z), "%"))) +
    facet_layer(facet_var) +
    labs(title = sprintf("Discard %s per stratum, %s", rate_type, st$word),
         caption = sub_wrap(
           grouping,
           if (identical(rate_type, "ratio")) "Discard ratio = discard/landings weight."
           else "Discard rate = discard/catch weight.",
           sprintf("bubbles = estimated discard %s%s.", rate_type,
                   if (raised) ", imported and raised" else ""),
           if (has_landings) "bubble size = landings weight (t).",
           if (st$on) {
             if (cross) "circles = imported from the data, crosses = added by raising."
             else "solid = imported from the data, transparent = added by raising."
           },
           if (raised && !st$on && !st$shown) "imported and raised strata are drawn alike.",
           if (backdrop)
             sprintf("horizontal line = landings weighted average per %s.", x_var),
           if (!is.null(ref_y))
             "dashed line = reference arbitrary threshold in the same units as y axis.",
           if (!is.null(ref_y) && backdrop)
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

#' Catch weight per stratum, before or after the raising
#'
#' @description
#' A stacked bar of catch weight at any aggregation: bars are landings,
#' discards (and BMS), or just the one, via `catchCategory`. Given the raised
#' catch (the output of [catchRaising()]) it shows what the raising added -
#' `importedOrRaised`, imported from the data versus added by the raising - as
#' the mark, as the `fill` or as a `facet`, as you choose. Given raw catch
#' it is the weight as it was reported. The arguments are the same either way,
#' and so is the plot; the title says which it is.
#'
#' @details
#' `importedOrRaised` is a variable like any other. By default it is the mark
#' (`alpha = "importedOrRaised"`): solid for what was imported, transparent and
#' hatched for what raising added. Name it in `fill` to give the two a colour
#' each (the bars
#' are then no longer coloured landings / discards: say which they are with
#' `catchCategory`, or with `facet = "catchCategory"`), in `facet` to give each
#' its own panel, or in `x` for a bar each; it is then not drawn as a mark as
#' well. `alpha = NULL` draws no mark, and unless it is named elsewhere the
#' imported and the raised weights are added together.
#'
#' The title says what the rows drawn are: *imported + raised* when the raising
#' added some of them, *only imported* when it did not - a raw catch, a raised
#' catch in which nothing needed raising, or the landings of a raised catch,
#' which are always imported - and then there is no mark, because there is
#' nothing to tell apart.
#'
#' @param catch CEF catch data, or the raised catch ([catchRaising()]):
#'   `catchCategory` and `total`, and for the mark `importedOrRaised`.
#' @param x Column on the x axis: one bar (or bar segment) per level;
#'   `"importedOrRaised"` included.
#' @param facet Optional facet column; `"importedOrRaised"` included.
#' @param fill Column mapped to the bar fill. `NULL` (default) colours the bars
#'   by catch category - landings, discards, BMS; `"importedOrRaised"` is
#'   allowed.
#' @param alpha `"importedOrRaised"` (default) marks what the raising added:
#'   solid bars for imported weight, transparent and hatched ones for raised.
#'   `NULL` draws no mark. Left out when `importedOrRaised` is the `x`, the
#'   `fill` or the `facet`, and when nothing was raised.
#' @param catchCategory Optional filter to just `"Lan"`, `"Dis"` or `"BMS"`
#'   (matched case-insensitively), so discards are readable on their own when
#'   landings would otherwise dominate the scale. `NULL` (default) keeps all
#'   the catch parts, stacked.
#' @param ncol Facet columns, when `facet` is set.
#' @param palette `"default"`, `"colorblind"` or `"gradual"`: the bar colours.
#' @inheritParams saving-plots
#' @return A ggplot object.
#' @seealso [plot_DR()] for the discard rate of the same catch, before or after
#'   the raising; [raised_overview_table()] for the weights as a table.
#' @family raising plots
#' @md
#' @examples
#' \dontrun{
#'
#' # the catch as reported, by metier
#' plot_catches(catch, x = "metier4")
#'
#' # the same of the raised catch: solid = imported, hatched = raised
#' raised <- catchRaising(catch, strataCond, logFile = tempfile(fileext = ".txt"))
#' plot_catches(raised, x = "metier4", facet = "vesselFlagCountry")
#'
#' # only the discards, imported and raised side by side
#' plot_catches(raised, x = "metier4", catchCategory = "Dis",
#'              fill = "importedOrRaised")
#' }
#' @export
plot_catches <- function(catch, x = NULL, facet = NULL, fill = NULL,
                         alpha = "importedOrRaised", catchCategory = NULL,
                         ncol = 2,
                         palette = c("default", "colorblind", "gradual"),
                         save = NULL) {
  save_on_exit(save, "plot_catches")
  palette <- match.arg(palette)
  has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
  S <- "importedOrRaised"
  miss <- setdiff(c("catchCategory", "total", if (has(x)) x,
                    if (has(facet)) facet, if (has(fill)) fill),
                  names(catch))
  if (length(miss))
    stop("plot_catches(): `catch` has no column ",
         paste0("'", miss, "'", collapse = ", "),
         if (S %in% miss) " - importedOrRaised comes with the output of catchRaising()",
         derive_hint(miss), call. = FALSE)
  # the catch parts, by the words the plot uses
  part_of <- function(v) {
    m <- unname(c(LAN = "Landings", LANDINGS = "Landings", DIS = "Discards",
                  DISCARDS = "Discards", BMS = "BMS")[toupper(as.character(v))])
    ifelse(is.na(m), as.character(v), m)
  }
  d <- as.data.frame(catch)
  d$.t   <- convert_units(d$total, if ("variableUnit" %in% names(d)) d$variableUnit, "t")
  d$.cat <- part_of(d$catchCategory)
  cc_label <- "Catch"
  if (has(catchCategory)) {
    cc_label <- part_of(catchCategory)
    if (!cc_label %in% c("Landings", "Discards", "BMS"))
      stop("plot_catches(): `catchCategory` is \"Lan\", \"Dis\" or \"BMS\" ",
           "(or NULL for all of them)", call. = FALSE)
    d <- d[d$.cat == cc_label, , drop = FALSE]
    if (identical(cc_label, "BMS")) cc_label <- "BMS catch"
  }
  # what is drawn - the rows left after the catch part - is what the title
  # speaks of: the landings of a raised catch were all imported
  present <- if (S %in% names(d)) unique(as.character(d[[S]]))
  st <- status_marks(S, alpha, c(x, fill, facet), present, "plot_catches(): ")
  raised <- "raised" %in% present
  # what the bars are coloured by when nothing else says: the catch parts there are
  parts <- c(Landings = "landings", Discards = "discards", BMS = "BMS")
  parts <- parts[names(parts) %in% d$.cat]
  d <- category_cols(d, c(x, fill, facet))
  d$.x <- if (has(x)) d[[x]] else "All"
  if (st$on) d$.status <- st$status(d[[S]])
  fill_col <- if (has(fill)) fill else ".cat"
  keys <- unique(c(".x", fill_col, if (has(facet)) facet, if (st$on) ".status"))
  d <- d %>% group_by(across(all_of(keys))) %>%
    summarise(t = sum(.t, na.rm = TRUE), .groups = "drop")

  ae <- aes(x = .x, y = t, fill = .data[[fill_col]])
  if (st$on) ae <- modifyList(ae, aes(alpha = .status))
  p <- ggplot(d, ae) +
    geom_col_hatch(position = position_stack(reverse = TRUE), width = 0.72,
                   colour = "grey25", linewidth = 0.25)
  p <- p + if (!has(fill)) {
    # every catch part, so landings keep their colour when only they are drawn
    cols <- c(plot_colours(c("Dis", "Lan"), "catchCategory", palette),
              plot_colours("BMS", "catchCategory", palette))
    scale_fill_manual(values = setNames(cols, c("Discards", "Landings", "BMS")),
                      name = NULL)
  } else scale_fill_manual(values = plot_colours(d[[fill]], fill, palette), name = fill,
                           na.value = "grey85")
  if (st$on) p <- p + st$scale
  p +
    scale_y_continuous(labels = scales::comma) +
    facet_layer(facet, ncol = ncol, scales = "free_y") +
    labs(title = sprintf("%s per stratum, %s", cc_label, st$word),
         caption = sub_wrap(
           strata_sentence(setdiff(c(x, fill, facet), S), catch),
           sprintf("bars = %s weight (t), summed over strata.", tolower(cc_label)),
           if (!has(fill) && length(parts) > 1)
             sprintf("color = %s or %s.", paste(utils::head(parts, -1), collapse = ", "),
                     utils::tail(parts, 1)),
           if (has(fill)) sprintf("color = %s.", fill),
           if (st$on)
             "solid = imported from the data, hatched = added by raising.",
           if (raised && !st$on && !st$shown)
             "imported and raised weights added together.",
           width = 110),
         x = if (has(x)) x else NULL, y = sprintf("%s (t)", cc_label)) +
    app_theme() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# ----------------------------------------------------------
#  Length / age distribution plots, before and after the allocation
# ----------------------------------------------------------

#' Numbers at length or age, before or after the allocation
#'
#' @description
#' The catch numbers spread over the length (or age) classes, as a stacked or
#' dodged histogram. Numbers are in thousands, length in cm and age in years.
#' Given the result of the allocation ([catchAllocationAoL()]) it shows what
#' the allocation did: `sampledOrEstimated` says whether a number was sampled
#' or estimated for a stratum nobody sampled, and it is drawn as the mark
#' (solid bars = sampled, transparent and hatched bars = estimated), as the
#' `fill` or as a `facet`, as you choose. The arguments are the same before and
#' after, and so is the plot; the title says which it is.
#'
#' @details
#' `data` is the catch and `distributions` the CEF distributions: they are
#' linked through `domainBiology` and the country and summed inside, with
#' `catchCategory` choosing the landings, the discards, BMS or the catch as a
#' whole. The catch part goes in the plot title. A
#' distributions row that repeats another exactly is dropped, with a warning:
#' a repeat is not a second sample.
#'
#' ## After the allocation
#'
#' Give both tables of the allocation's result: its `$catch` as `data` and its
#' `$distribution` as `distributions`, `plot_NaLoA(alloc$catch,
#' alloc$distribution)`. The distributions it estimated for a stratum with no
#' biological domain carry a domain the allocation made up, and only its own
#' `$catch` knows it - the catch that went into the allocation cannot find
#' them, which the plot warns about.
#'
#' The allocation adds `sampledOrEstimated` to the distributions, and from then
#' on it is a variable like any other. By default it is the mark
#' (`alpha = "sampledOrEstimated"`): solid bars where the numbers were sampled,
#' transparent and hatched ones where the allocation estimated them. Name it in
#' `fill` to give the two a colour each, or in `facet` to give each its own
#' panel; it is then not drawn as a mark as well. `alpha = NULL` draws no mark,
#' and unless it is also the fill or a facet the sampled and the estimated
#' numbers are added together - the numbers the allocation finally reports.
#'
#' The title says what the distributions drawn are: *sampled + estimated* when
#' the allocation estimated some of them, *only sampled* when it did not -
#' distributions as they were sampled, or an allocation that estimated none of
#' the ones drawn - and then there is no mark, because there is nothing to
#' tell apart.
#'
#' Like the allocation itself, the numbers count a distribution once for every
#' stratum of the catch that names its `domainBiology`, so they assume the
#' catch has one row per domain, catch category and country - which CEF files
#' built from InterCatch have. Where there are several, the numbers come out
#' inflated, though the shapes still read.
#'
#' ## Fill, facets and classes
#'
#' `fill` colours (and stacks) the bars by a column of the catch. `facet`
#' takes one column (a wrapped set of panels) or two (`c("row", "col")`, a
#' grid). `plus_group` (ages) and `bin_width` (lengths) pool the classes exactly
#' as in [plot_MWaLoA()] - see that function's *The plus group* and *Length
#' bins* - except numbers simply add rather than being averaged, so there is
#' no per-distribution weighting step to speak of.
#'
#' @param data CEF catch data carrying the columns you group by
#'   ([derive_fields()] output, or your own); after the allocation, its `$catch`.
#' @param distributions A CEF distributions table, linked to the catch through
#'   `domainBiology`; after the allocation, its `$distribution`.
#' @param distributionType `"LengthTotal"` (default) or `"Age"`.
#' @param catchCategory `"All"` (default: landings and discards together,
#'   titled "catch"), `"Lan"`, `"Dis"` or `"BMS"`. Only used with
#'   `distributions`.
#' @param fill Column mapped to the bar fill; `"sampledOrEstimated"` included.
#' @param facet Up to two facet columns: `"col"` or `c("row", "col")`;
#'   `"sampledOrEstimated"` included.
#' @param alpha `"sampledOrEstimated"` (default) marks what the allocation
#'   estimated: solid bars for sampled numbers, transparent and hatched ones for
#'   estimated. `NULL` draws no mark. Left out when `sampledOrEstimated` is the
#'   `fill` or a `facet`, and when nothing was estimated.
#' @param stacked Stack the bars on `fill` (default) rather than dodge them.
#' @param plus_group The first age of the plus group: `7` pools ages 7 and
#'   older into one class, shown as `7+`. `NULL` (default): no plus group.
#'   Ages only.
#' @param bin_width Width, in cm, of the length bins every class is pooled
#'   into. `NULL` (default): no binning. Lengths only.
#' @param scales Facet scales: `"fixed"` (default), `"free_x"`, `"free_y"` or
#'   `"free"`.
#' @param palette `"default"`, `"colorblind"` or `"gradual"`.
#' @inheritParams saving-plots
#' @return A ggplot object, or `NULL` when there is nothing to draw.
#' @seealso [plot_MWaLoA()] for the mean weight in the same classes;
#'   [coverage-plots] for whether the strata have a distribution at all;
#'   [catchAtAoLAggregation()] for the same numbers as a table;
#'   [plot_catches()] for the same sampled / raised contrast on the weights.
#' @family distribution plots
#' @md
#' @examples
#' \dontrun{
#'
#' # numbers at length of the landings, stacked by country
#' plot_NaLoA(catch, distributions,
#'           catchCategory = "Lan", fill = "vesselFlagCountry")
#'
#' # numbers at age, one panel per quarter, dodged
#' plot_NaLoA(catch, distributions,
#'           distributionType = "Age", fill = "vesselFlagCountry",
#'           facet = "seasonValue", stacked = FALSE)
#'
#' # every age from 3 upward pooled into "3+"
#' plot_NaLoA(catch, distributions,
#'           distributionType = "Age", fill = "vesselFlagCountry", plus_group = 3)
#'
#' # length classes pooled into 2cm bins
#' plot_NaLoA(catch, distributions, fill = "vesselFlagCountry", bin_width = 2)
#'
#' # after the allocation: both tables of its result. Landings by country,
#' # solid = sampled, hatched = estimated
#' alloc <- catchAllocationAoL(raised, distributions,
#'                             condition_alloc_st_list = list(ALL = rlang::quo(TRUE)),
#'                             distributionType = "LengthTotal", verbose = FALSE,
#'                             logFile = tempfile(fileext = ".txt"))
#' plot_NaLoA(alloc$catch, alloc$distribution,
#'           catchCategory = "Lan", fill = "vesselFlagCountry")
#'
#' # sampled and estimated as the fill instead, and the countries as panels
#' plot_NaLoA(alloc$catch, alloc$distribution, catchCategory = "Lan",
#'           fill = "sampledOrEstimated", facet = "vesselFlagCountry")
#' }
#' @export
plot_NaLoA <- function(data, distributions = NULL,
                       distributionType = c("LengthTotal", "Age"),
                       catchCategory = "All",
                       fill = NULL, facet = NULL,
                       alpha = "sampledOrEstimated",
                       stacked = TRUE,
                       plus_group = NULL, bin_width = NULL,
                       scales = "fixed",
                       palette = c("default", "colorblind", "gradual"), save = NULL) {
  save_on_exit(save, "plot_NaLoA")
  fn <- "plot_NaLoA(): "
  typed_dt <- !missing(distributionType); typed_cc <- !missing(catchCategory)
  distributionType <- match.arg(distributionType)
  palette <- match.arg(palette)
  catchCategory <- match.arg(catchCategory, c("Lan", "Dis", "BMS", "All"))
  if (length(facet) > 2)
    stop(fn, "`facet` takes at most two columns", call. = FALSE)
  facet1 <- if (length(facet) >= 1) facet[1] else NULL
  facet2 <- if (length(facet) >= 2) facet[2] else NULL
  has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
  S <- "sampledOrEstimated"
  if (is.null(distributions) && (typed_dt || typed_cc))
    warning(fn, "distributionType / catchCategory ",
            "only matter when `distributions` is supplied (building from ",
            "raw catch)", call. = FALSE)
  # sampled / estimated is grouped by whenever the distributions carry it and a
  # shade is asked for (or it is a fill or a facet): the table then says which
  # numbers were which, and the plot decides what to do with that
  grp <- unique(c(if (has(fill)) fill, if (has(facet1)) facet1,
                  if (has(facet2)) facet2,
                  if (!is.null(alpha) && S %in% names(distributions)) S))
  catch0 <- if (!is.null(distributions)) data
  pr <- biology_dist_prep(data, distributions, distributionType, catchCategory,
                          "numbers", grp, plus_group, bin_width, fn)
  d <- pr$data
  if (is.null(d) || !nrow(d)) return(NULL)
  plus_from <- pr$plus_from; bin_from <- pr$bin_from
  st <- status_marks(S, alpha, c(fill, facet1, facet2), pr$status, fn)
  estimated <- "estimated" %in% pr$status
  if (S %in% names(d)) {
    if (st$on) d$.status <- st$status(d[[S]])
    else if (!st$shown) d[[S]] <- NULL     # nothing estimated: one value, nothing to tell apart
  }
  d <- category_cols(d, c(fill, facet1, facet2))
  plus_on <- !is.null(plus_from) && plus_from %in% d$.class
  if (!is.null(plus_from) && !plus_on)
    warning(fn, "no age reaches plus_group = ", plus_from,
            ", so there is no plus group", call. = FALSE)
  bin_on <- !is.null(bin_from)
  bw <- { u <- sort(unique(d$.class)); if (length(u) > 1) min(diff(u)) else 1 }
  pos <- if (!stacked) position_dodge(width = bw)
         else if (st$on) position_stack(reverse = TRUE) else "stack"

  ae <- aes(x = .class, y = number)
  if (st$on) ae <- modifyList(ae, aes(alpha = .status))
  p <- ggplot(d, ae)
  p <- p + if (has(fill))
    geom_col_hatch(aes(fill = factor(.data[[fill]])), width = bw * 0.95, position = pos,
                   colour = OUTLINE, linewidth = OUTLINE_LW)
  else geom_col_hatch(width = bw * 0.95, position = pos, fill = plot_colours(palette = palette),
                      colour = OUTLINE, linewidth = OUTLINE_LW)
  if (has(fill))
    p <- p + scale_fill_manual(values = plot_colours(d[[fill]], fill, palette),
                               name = fill, na.value = "grey85")
  if (st$on) p <- p + st$scale

  # what a bar is: the numbers of every sampled stratum that shares the
  # plotted columns, summed - everything else is grouped into it
  with_dist <- !is.null(distributions)
  cc_word <- switch(catchCategory, Lan = "Landings", Dis = "Discards",
                    BMS = "BMS Catch", All = "Catch")
  cc_lead <- if (!with_dist) ""
             else switch(catchCategory, Lan = "Landings ", Dis = "Discards ",
                         BMS = "BMS catch ", All = "Catch ")
  cls_word <- if (identical(distributionType, "Age")) "NaA" else "NaL"
  cap <- sub_wrap(
    if (with_dist) strata_sentence(setdiff(grp, S), catch0),
    sprintf("bars = %s%s summed over strata.", cc_lead, cls_word),
    if (has(fill))
      sprintf("%s by %s.", if (stacked) "stacked" else "dodged", fill),
    if (st$on) "solid = sampled, hatched = estimated by the allocation.",
    if (estimated && !st$on && !st$shown)
      "sampled and estimated numbers added together.",
    if (plus_on) sprintf("%s+ = ages %s+ pooled.", format(plus_from), format(plus_from)),
    if (bin_on) sprintf("length classes pooled into %scm bins.",
                        format(bin_from, trim = TRUE)),
    width = 110)
  p <- p +
    class_axis_scale(d$.class, plus_from = if (plus_on) plus_from,
                     bin_from = if (bin_on) bin_from) +
    scale_y_continuous(labels = scales::comma) +
    facet_layer(facet1, facet2, 2, scales) +
    labs(title = paste0(if (with_dist) paste0(cc_word, " - "),
                        sprintf("Numbers at %s", sub(" .*", "", pr$class_label)),
                        if (with_dist) paste0(", ", st$word)),
         caption = cap, x = pr$class_label, y = pr$value_label, fill = fill) +
    app_theme() + theme(plot.title = element_text(size = 11))
  # after an allocation the legends - the shade, the status, the fill - are
  # tall: outside the panel, where they cannot sit on the bars
  if (estimated)
    p <- p + theme(legend.position = "right", legend.justification = "center")
  p
}



#' Mean weight at length or age, before or after the allocation
#'
#' @description
#' The number-weighted mean weight in each length (or age) class, as a line
#' with points. Weight is in grams, length in cm and age in years. When the
#' strata are averaged (no `shape`) and a spread is available, a +/- 1 sd
#' band is drawn as well. Two options help to judge how far to trust the
#' curve: a plus group pools the oldest ages into one class, and
#' semi-transparent bars behind the lines show how many fish stand behind
#' each point. Given the result of the allocation ([catchAllocationAoL()]) it
#' shows what the allocation did: `sampledOrEstimated` says whether a mean
#' weight was sampled or estimated for a stratum nobody sampled, and it is
#' drawn as the mark (a solid line with circles = sampled, a dotted line with
#' crosses = estimated), as the `colour` or as a `facet`, as you choose. The
#' arguments are the same before and after, and so is the plot; the title says
#' which it is.
#'
#' @details
#' `data` is the catch and `distributions` the CEF distributions: they are
#' linked through `domainBiology` and the country and summarised inside, with
#' `catchCategory` choosing the landings, the discards, BMS or the catch as a
#' whole. The catch part goes in the plot title. A
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
#' the other plots', except with `show_samples`, `plus_group` or the mark,
#' where it moves outside so it cannot cover the right-hand end of the curve.
#'
#' ## After the allocation
#'
#' As in [plot_NaLoA()]: give both tables of the allocation's result, its
#' `$catch` as `data` and its `$distribution` as `distributions`. The
#' allocation adds `sampledOrEstimated` to the distributions, and it is a
#' variable like any other: by default the mark (`alpha =
#' "sampledOrEstimated"`), or named in `colour` (a colour each) or in `facet`
#' (a panel each), where it is not drawn as a mark as well. With the mark,
#' every combination of the plotted columns is one line per sampled /
#' estimated, so the sampled curve (solid, with circles) and the curve the
#' allocation estimated for the unsampled strata (dotted, with crosses) sit side
#' by side. When `shape` is already a column of its own the crosses are not
#' available, and the estimated points are transparent instead. `alpha = NULL`
#' draws no mark, and unless it is also the colour or a facet the sampled and
#' the estimated weights are averaged together, weighted by their numbers.
#'
#' The title says what the distributions drawn are: *sampled + estimated* when
#' the allocation estimated some of them, *only sampled* when it did not, and
#' then there is no mark, because there is nothing to tell apart. A
#' distribution the allocation estimated has no measured fish to count, so it
#' has no bar with `show_samples`.
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
#' with many. It applies to ages only.
#'
#' ## Length bins
#'
#' `bin_width = 5` is the length counterpart: every length class is grouped
#' into 5cm bins (`10`-`14` becomes one class labelled `10-15`), pooled the
#' same way a plus group is - numbers added, mean weights averaged weighted
#' by those numbers, inside each distribution first, then strata averaged as
#' usual. It applies to lengths only (`distributionType = "LengthTotal"`).
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
#' sample size is unknown, not zero, and the caption says so.
#'
#' @param data CEF catch data carrying the columns you group by
#'   ([derive_fields()] output, or your own); after the allocation, its `$catch`.
#' @param distributions A CEF distributions table, linked to the catch through
#'   `domainBiology`; after the allocation, its `$distribution`.
#' @param distributionType `"LengthTotal"` (default) or `"Age"`.
#' @param catchCategory `"All"` (default: landings and discards together,
#'   titled "catch"), `"Lan"`, `"Dis"` or `"BMS"`. Only used with
#'   `distributions`.
#' @param colour Column mapped to the line colour; `"sampledOrEstimated"`
#'   included.
#' @param facet Up to two facet columns: `"col"` or `c("row", "col")`;
#'   `"sampledOrEstimated"` included.
#' @param shape Column drawn as separate series with their own point shapes;
#'   opens the averaged group back up and drops the band.
#' @param alpha `"sampledOrEstimated"` (default) marks what the allocation
#'   estimated: a solid line with circles for sampled mean weights, a dotted line
#'   with crosses for estimated ones (transparent circles when `shape` is a
#'   column). `NULL` draws no mark. Left out when `sampledOrEstimated` is the
#'   `colour`, the `shape` or a `facet`, and when nothing was estimated.
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
#' @param palette `"default"`, `"colorblind"` or `"gradual"`.
#' @inheritParams saving-plots
#' @return A ggplot object, or `NULL` when there is nothing to draw.
#' @seealso [plot_NaLoA()] for the numbers in the same classes;
#'   [coverage-plots] for whether the strata have a distribution at all;
#'   [meanWoLatAoLAggregation()] for the same mean weights as a table.
#' @family distribution plots
#' @md
#' @examples
#' \dontrun{
#'
#' # mean weight at length of the landings, one line per country
#' plot_MWaLoA(catch, distributions,
#'            catchCategory = "Lan", colour = "vesselFlagCountry")
#'
#' # at age, one panel per quarter
#' plot_MWaLoA(catch, distributions,
#'            distributionType = "Age",
#'            colour = "vesselFlagCountry", facet = "seasonValue")
#'
#' # what a plus group does: every age from 3 upward pooled into "3+"
#' plot_MWaLoA(catch, distributions,
#'            distributionType = "Age", colour = "vesselFlagCountry",
#'            plus_group = 3)
#'
#' # length classes pooled into 2cm bins
#' plot_MWaLoA(catch, distributions, colour = "vesselFlagCountry",
#'            bin_width = 2)
#'
#' # the fish measured at each age, as bars behind the lines
#' plot_MWaLoA(catch, distributions,
#'            distributionType = "Age", colour = "vesselFlagCountry",
#'            show_samples = TRUE)
#'
#' # after the allocation: both tables of its result. Landings by country,
#' # solid line = sampled, dotted line with crosses = estimated
#' plot_MWaLoA(alloc$catch, alloc$distribution,
#'            catchCategory = "Lan", colour = "vesselFlagCountry")
#'
#' # sampled and estimated as the colour instead
#' plot_MWaLoA(alloc$catch, alloc$distribution, catchCategory = "Lan",
#'            colour = "sampledOrEstimated")
#' }
#' @export
plot_MWaLoA <- function(data, distributions = NULL,
                        distributionType = c("LengthTotal", "Age"),
                        catchCategory = "All",
                        colour = NULL, facet = NULL, shape = NULL,
                        alpha = "sampledOrEstimated",
                        plus_group = NULL,
                        bin_width = NULL,
                        show_samples = FALSE,
                        scales = "fixed",
                        palette = c("default", "colorblind", "gradual"), save = NULL) {
  save_on_exit(save, "plot_MWaLoA")
  fn <- "plot_MWaLoA(): "
  typed_dt <- !missing(distributionType); typed_cc <- !missing(catchCategory)
  distributionType <- match.arg(distributionType)
  palette <- match.arg(palette)
  catchCategory <- match.arg(catchCategory, c("Lan", "Dis", "BMS", "All"))
  if (length(facet) > 2)
    stop(fn, "`facet` takes at most two columns", call. = FALSE)
  facet1 <- if (length(facet) >= 1) facet[1] else NULL
  facet2 <- if (length(facet) >= 2) facet[2] else NULL
  has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
  flag <- function(v) is.logical(v) && length(v) == 1 && !is.na(v)
  S <- "sampledOrEstimated"
  if (!flag(show_samples))
    stop(fn, "`show_samples` is TRUE or FALSE", call. = FALSE)
  if (is.null(distributions) && (typed_dt || typed_cc))
    warning(fn, "distributionType / catchCategory only matter when ",
            "`distributions` is supplied (building from raw catch)",
            call. = FALSE)
  # sampled / estimated is grouped by whenever the distributions carry it and a
  # shade is asked for (or it is a colour, shape or facet), as in plot_NaLoA()
  grp <- unique(c(if (has(colour)) colour, if (has(facet1)) facet1,
                  if (has(facet2)) facet2, if (has(shape)) shape,
                  if (!is.null(alpha) && S %in% names(distributions)) S))
  catch0 <- if (!is.null(distributions)) data
  pr <- biology_dist_prep(data, distributions, distributionType, catchCategory,
                          "meanweight", grp, plus_group, bin_width, fn)
  d <- pr$data
  if (is.null(d) || !nrow(d)) return(NULL)
  plus_from <- pr$plus_from; bin_from <- pr$bin_from
  st <- status_marks(S, alpha, c(colour, facet1, facet2, shape), pr$status, fn,
                     override = list(colour = "grey30", fill = "grey30"))
  estimated <- "estimated" %in% pr$status
  if (S %in% names(d)) {
    if (st$on) d$.status <- st$status(d[[S]])
    else if (!st$shown) d[[S]] <- NULL     # nothing estimated: one value, nothing to tell apart
  }
  d <- category_cols(d, c(colour, shape, facet1, facet2))
  # one line per colour level, per shape level and per sampled / estimated when
  # that is the shade: the series each is a line of its own
  line_by <- c(if (has(colour)) colour, if (has(shape)) shape, if (st$on) ".status")
  d$.line <- if (length(line_by)) interaction(d[line_by], drop = TRUE) else 1L
  # What marks an estimated mean weight: a dotted line, and crosses for its points
  # (which have no fill, so they take the colour a circle is filled with: see
  # cross_marks()). Not when `shape` is already a column: then the points are
  # transparent instead.
  cross <- st$on && !has(shape)
  if (cross) d <- cross_marks(d, if (has(colour)) colour, 1, 0.4)
  with_dist <- !is.null(distributions)
  cc_word <- switch(catchCategory, Lan = "Landings", Dis = "Discards",
                    BMS = "BMS Catch", All = "Catch")
  cls <- tolower(sub(" .*", "", pr$class_label))   # "age" or "length"

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
  band <- !has(shape) && "sd_weight" %in% names(d) && any(!is.na(d$sd_weight))

  p <- ggplot(d, if (has(colour))
                   aes(x = .class, y = mean_weight, group = .line,
                       colour = factor(.data[[colour]]))
                 else aes(x = .class, y = mean_weight, group = .line))

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
    p <- p + if (has(colour))
      geom_col(data = bd, aes(y = n_fish * .kbar, fill = factor(.data[[colour]])),
               width = bw * 0.9, alpha = 0.25, colour = OUTLINE,
               linewidth = OUTLINE_LW)
    else geom_col(data = bd, aes(y = n_fish * .kbar), width = bw * 0.9,
                  alpha = 0.25, fill = plot_colours(palette = palette),
                  colour = OUTLINE, linewidth = OUTLINE_LW)
  }

  # a point made of one stratum has no spread: no ribbon there, and no warning
  # about the rows it skips (na.rm)
  if (band) {
    p <- p + if (has(colour))
      geom_ribbon(aes(ymin = pmax(0, mean_weight - sd_weight),
                      ymax = mean_weight + sd_weight,
                      fill = factor(.data[[colour]])),
                  alpha = 0.18, colour = NA, na.rm = TRUE)
    else geom_ribbon(aes(ymin = pmax(0, mean_weight - sd_weight),
                         ymax = mean_weight + sd_weight),
                     alpha = 0.18, fill = plot_colours(palette = palette), colour = NA,
                     na.rm = TRUE)
  }

  # the line is dotted where the status marks it; the points are crosses, or - when
  # `shape` is a column of its own - transparent
  line_args <- list(linewidth = 0.65, alpha = 0.9)
  if (st$on) line_args$mapping <- aes(linetype = .status)
  p <- p + do.call(geom_line, line_args)
  shade <- if (st$on && !cross) aes(alpha = .status) else aes()
  p <- p + if (cross) {
    if (has(colour))
      geom_point(aes(fill = factor(.data[[colour]]), shape = .status, colour = .edge,
                     stroke = .stroke), size = 2.2)
    else geom_point(aes(shape = .status, colour = .edge, stroke = .stroke), size = 2.2,
                    fill = plot_colours(palette = palette))
  } else if (has(shape))
    geom_point(modifyList(aes(shape = factor(.data[[shape]])), shade), size = 2.3,
               stroke = 0.5, fill = "white")
  else if (has(colour))
    geom_point(modifyList(aes(fill = factor(.data[[colour]])), shade), shape = 21,
               size = 2.2, colour = OUTLINE, stroke = 0.4)
  else geom_point(shade, shape = 21, size = 2.2, fill = plot_colours(palette = palette),
                  colour = OUTLINE, stroke = 0.4)
  if (has(shape))
    p <- p + scale_shape_manual(values = c(16, 17, 15, 18, 1, 2, 0, 5),
                                name = shape)

  if (has(colour)) {
    cols <- plot_colours(d[[colour]], colour, palette)
    # the crosses' own outline colour is a value of the scale too, kept out of the legend
    p <- p + scale_colour_manual(values = c(cols, if (cross) c(.outline = OUTLINE)),
                                 breaks = if (cross) names(cols) else waiver(),
                                 name = colour, na.value = "grey70")
    # fill is mapped by the band, the colour-fill points and the bars;
    # once `shape` opens the strata up only the bars are left
    if (!has(shape) || bars)
      p <- p + scale_fill_manual(values = cols, name = colour, na.value = "grey85")
  } else if (cross)
    p <- p + scale_colour_manual(values = c(.outline = OUTLINE, .cross = plot_colours(palette = palette)),
                                 guide = "none")
  if (st$on) p <- p + st$line
  if (cross)
    p <- p + st$shape + scale_continuous_identity("stroke", guide = "none") +
      guides(shape = guide_legend(override.aes = list(
        fill = "grey30", colour = "grey30", size = 2.4, stroke = c(0.4, 1))))
  else if (st$on) p <- p + st$scale

  # what a point is: the mean weight of every sampled stratum that
  # shares the plotted columns, weighted by numbers - everything else is
  # grouped into it, and the ribbon is the spread across exactly those
  # strata. Kept to a short, formula-like line each - the grouping
  # sentence already says what is shown and grouped, so this is only
  # how the marks are built.
  cc_lead <- if (!with_dist) ""
             else switch(catchCategory, Lan = "Landings ", Dis = "Discards ",
                         BMS = "BMS catch ", All = "Catch ")
  mw_word <- if (identical(cls, "age")) "MWaA" else "MWaL"
  n_word  <- if (identical(cls, "age")) "NaA" else "NaL"
  dots_txt <- sprintf("dots and lines = %s%s weight averaged by %s%s.",
                      cc_lead, mw_word, n_word, if (bars) " (left axis)" else "")
  ribbon_txt <- if (has(shape))
    sprintf("each %s drawn separately (no ribbon).", shape)
  else if (band) "ribbon = +/- 1sd across grouped strata."
  shade_txt <- if (st$on) {
    if (cross) "solid line and dots = sampled, dotted line and crosses = estimated by the allocation."
    else "solid line = sampled, dotted line and transparent dots = estimated by the allocation."
  } else if (estimated && !st$shown)
    "sampled and estimated weights averaged together (weighted by numbers)."
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
    if (with_dist) strata_sentence(setdiff(grp, S), catch0),
    dots_txt, ribbon_txt, shade_txt, plus_txt, bin_txt, bars_txt, scaled_txt,
    width = 110)

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
    facet_layer(facet1, facet2, 2, scales) +
    labs(title = paste0(if (with_dist) paste0(cc_word, " - "),
                        sprintf("Mean Weight at %s", sub(" .*", "", pr$class_label)),
                        if (with_dist) paste0(", ", st$word)),
         caption = cap,
         x = pr$class_label, y = "Mean weight (g)",
         colour = colour,
         fill = if (has(shape) && !bars) NULL else colour,
         shape = shape) +
    app_theme() + theme(plot.title = element_text(size = 11))
  # The theme keeps the legend inside the panel, bottom right. With bars
  # that is where the second axis is, with a plus group it is the right-hand
  # end of the curve the eye is on, and after an allocation the legends - the
  # shade, the status, the colour - are tall - so it moves outside.
  if (bars || plus_on || estimated)
    p <- p + theme(legend.position = "right",
                   legend.justification = "center")
  p
}
