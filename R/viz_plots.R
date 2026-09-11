# =====================================================================
#  viz_plots.R   (coverage / discard-rate / biology-distribution plots)
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
#  PUBLIC API (one function per view, one exploratory question each):
#    plot_coverage_heat/_bubble/_bar/_map  how well a set of strata is
#                                sampled - `what` switches discard / length /
#                                age, `catchCategory` the landings / discards
#                                part for length & age.
#    plot_discard_rate()         raw per-stratum discard-rate points.
#    plot_numbers_at_length_age()   numbers at length/age (histogram or
#                                smoothed density).
#    plot_meanweight_at_length_age() mean weight at length/age (line + points).
#
#  Each is a single self-contained function that does both drawing
#  engines internally: `engine = "ggplot"` (default) or `engine = "base"`.
#  Every function takes raw CEF catch (and `distributions` for length /
#  age) and builds + classifies the strata itself - there is no
#  separate builder or dispatcher to step through, and no
#  already-classified-table argument. The shared plumbing (faceting,
#  base-R axis / legend drawing, strata building, arg validation) lives
#  in R/viz_helpers.R; the ggplot-only raising flow diagrams
#  (plot_river(), plot_river_level(), plot_comparison()) live in
#  R/viz_raising.R.
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








# ----------------------------------------------------------
#  Coverage map (ICES-area pies)
# ----------------------------------------------------------



#' Coverage map: one pie per ICES area (optionally per group level)
#'
#' A map of the fishing area with one small pie per ICES division: pie
#' radius is the area's landed/caught weight, and the pie is split into
#' the sampled and the not-sampled share of that weight. With `group`
#' (e.g. `"Season"`, `"Country"`, `"metier4"`) each area gets a labelled
#' box holding a tidy grid of pies, one per group level; overlapping
#' boxes are nudged apart and a thin leader keeps each box tied to its
#' area's true centroid.
#'
#' Auto-builds from raw catch data (and `distributions` + `catchCategory`
#' for a biology `what`) exactly like [coverage-plots]. Needs the
#' suggested `maps` package.
#'
#' @param catch Raw CEF catch data.
#' @param area Column holding the ICES area code. Default `"areaValue"`.
#' @param group Optional column - one boxed grid of pies per area, one
#'   pie per level.
#' @param what,catchCategory,min_cov,min_trip,min_fish,distributions
#'   As in [coverage-plots]. The weight word in the title is derived
#'   from `what` and `catchCategory`.
#' @param scale Multiplies every pie radius (and, when grouped, the box
#'   grid cell size).
#' @param engine `"base"` (default) or `"ggplot"`.
#' @return `NULL` invisibly (`engine = "base"`) or a ggplot object.
#' @seealso [coverage-plots], [ices_area_centroid()]
#' @family coverage plots
#' @export
plot_coverage_map <- function(catch, distributions = NULL, area = "areaValue",
                              group = NULL, what = "discard",
                              catchCategory = NULL,
                              min_cov = 50, min_trip = 2, min_fish = NULL,
                              scale = 1,
                              engine = c("base", "ggplot")) {
  engine <- match.arg(engine)
  what   <- normalize_what(what)
  strata <- catch
  area_var <- area; group_var <- group; bubble_scale <- scale
  weight_var <- "landings"; covered_var <- "landings_with_discards"
  has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
  if (!requireNamespace("maps", quietly = TRUE))
    stop("plot_coverage_map() needs the 'maps' package: install.packages(\"maps\")")
  if (identical(what, "discard")) {
    if (!is.null(catchCategory))
      stop("plot_coverage_map(): catchCategory does not apply to ",
           "what = \"discard\"", call. = FALSE)
    cc <- NULL
  } else {
    cc <- if (is.null(catchCategory)) "All"
          else match.arg(catchCategory, c("Lan", "Dis", "BMS", "All"))
  }
  weight_label <- default_weight_label(what, cc)
  dims <- unique(c(area_var, if (has(group_var)) group_var))
  sc <- build_coverage_strata(strata, dims, NULL, NULL, NULL, min_cov, what,
                                min_fish, min_trip, distributions,
                                    catch_category_parts(cc))
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
    graphics::legend("bottomright", bg = grDevices::adjustcolor("white", 0.88),
                     box.col = "grey65", fill = fills,
                     legend = c("sampled", "not sampled"), cex = 0.95,
                     inset = c(0.02, 0.02))
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



#' Coverage: which strata are sampled well enough
#'
#' Three views of how well a set of strata is sampled - a heatmap
#' ([plot_coverage_heat()]), a weighted bubble grid
#' ([plot_coverage_bubble()]) and a sampled-vs-unsampled weight bar chart
#' ([plot_coverage_bar()]) - plus a map ([plot_coverage_map()]). All four
#' take raw CEF catch data (plus `distributions` for length / age) and
#' build + classify the strata internally.
#'
#' `what` selects what "sampled" means: `"discard"` (a discard estimate
#' exists), `"length"` or `"age"` (a length / age distribution exists).
#' For `"length"` / `"age"`, pass `distributions` and, with
#' `catchCategory`, choose whether to look at the sampling of the landings
#' (`"Lan"`, the default), the discards (`"Dis"`) or BMS (`"BMS"`).
#'
#' The sampling-status colours adapt to which thresholds are active.
#' Coverage (`min_cov`) is always on; `min_trip` (discard, length, age)
#' and `min_fish` (length, age) are each independently optional - `NULL`
#' drops that question from the classification and the legend entirely,
#' a number turns it on. The warning shade is a straight count of how
#' many of the ACTIVE thresholds a stratum missed (yellow = 1, orange =
#' 2, dark orange = 3 - only reachable with trips AND measurements both
#' on); red is reserved for a stratum with no discard estimate / no
#' linked distribution at all, never for a threshold miss.
#'
#' The arguments run in the order you usually think about them: what data
#' (`catch`, `distributions`, `what`, `catchCategory`), then which columns
#' are the strata (`y`, `x` / `fill`, `facet`), then the pass / fail
#' thresholds (`min_cov`, `min_trip`, `min_fish`), then `n_per_screen`,
#' and finally `palette` and `engine` last.
#'
#' @param catch Raw CEF catch data.
#' @param distributions A CEF distributions table - needed for
#'   `what = "length"`/`"age"`, ignored otherwise.
#' @param what `"discard"` (default), `"length"` or `"age"`. Plural and
#'   mixed case are accepted (`"discards"`, `"Length"`, ...).
#' @param catchCategory For `what = "length"`/`"age"`: `"All"` (default -
#'   landings and discards together, titled "catch"), `"Lan"`, `"Dis"` or
#'   `"BMS"`. Sets which catch part the distribution is linked to and the
#'   weight word in the plot title. Not valid for `what = "discard"`
#'   (an error).
#' @param y Row category. Required.
#' @param x Column category - the second axis of the heatmap / bubble
#'   grid. `NULL` collapses to a single column.
#' @param fill Bar fill category (bars only). `NULL` uses `y`.
#' @param facet Optional facet column.
#' @param min_cov Minimum coverage, percent (heat / bubble only). Always
#'   active - coverage is never dropped from the classification.
#' @param min_trip Minimum sampling trips (heat / bubble only), or
#'   `NULL` to leave trips out of the classification altogether -
#'   categories and legend both drop to a coverage-only scheme.
#'   Defaults to 2 (active).
#' @param min_fish Minimum measurements for `what = "length"`/`"age"`,
#'   or `NULL` (the default) to leave measurements out of the
#'   classification.
#' @param n_per_screen Base engine: split a faceted figure across this
#'   many devices.
#' @param palette `"default"` or `"colorblind"` - recolours the bar fill.
#'   The heatmap / bubble status colours are a fixed semantic scale;
#'   asking for `"colorblind"` there warns.
#' @param engine `"ggplot"` (default) or `"base"`.
#' @return A ggplot object (`engine = "ggplot"`) or `NULL` invisibly
#'   (`engine = "base"`).
#' @seealso [classify_strata()], [plot_discard_rate()],
#'   [plot_numbers_at_length_age()]
#' @name coverage-plots
#' @family coverage plots
#' @examples
#' \dontrun{
#' plot_coverage_heat(catch, y = "metier4", x = "Season")
#' plot_coverage_bar(catch, y = "metier4", fill = "Season")
#' plot_coverage_heat(catch, dist, y = "fleetValue", what = "length",
#'                    catchCategory = "Dis", min_cov = 50, min_fish = 100)
#' }
NULL


#' @rdname coverage-plots
#' @export
plot_coverage_heat <- function(catch, distributions = NULL, y, x = NULL,
                               facet = NULL, what = "discard",
                               catchCategory = NULL,
                               min_cov = 50, min_trip = 2, min_fish = NULL,
                               n_per_screen = NULL,
                               palette = c("default", "colorblind"),
                               engine = c("ggplot", "base")) {
  view <- "heat"; fn <- "plot_coverage_heat"
  engine <- match.arg(engine); palette <- match.arg(palette)
  strata <- catch
  angle <- 45
  what <- normalize_what(what)
  if (is.null(y) || !nzchar(y)) stop(fn, "(): `y` is required", call. = FALSE)

  user_cc <- !is.null(catchCategory)
  if (identical(what, "discard")) {
    if (user_cc)
      stop(fn, "(): catchCategory does not apply to what = \"discard\" ",
           "(discard coverage is the discard part by definition)",
           call. = FALSE)
    cc <- NULL
  } else {
    cc <- if (user_cc) match.arg(catchCategory, c("Lan", "Dis", "BMS", "All")) else "All"
  }

  plot_arg_report(fn, list(
    list(bad = identical(palette, "colorblind") &&
           view %in% c("heat", "bubble"),
         msg = "palette does not recolour a status heatmap / bubble grid (the cells use a fixed semantic scale)"),
    list(bad = identical(engine, "ggplot") && !is.null(n_per_screen),
         msg = "n_per_screen is a base-engine option; the ggplot engine draws one figure")))

  wl   <- default_weight_label(what, cc)
  xcol <- x

  strata <- build_coverage_strata(strata, NULL, y, xcol, facet, min_cov,
                                    what, min_fish, min_trip, distributions,
                                    catch_category_parts(cc))

  sc <- strata; y_var <- y; x_var <- x; fill_var <- NULL
  facet_var <- facet; threshold <- min_cov; weight_label <- wl
  min_meas <- min_fish; min_trips <- min_trip

  ## ============ base engine ==========================================
  if (identical(engine, "base")) {
    # keep cat_key == "none" (a stratum that landed nothing) so its cell
    # is drawn in the "no landings / no discards" colour rather than
    # left as a blank hole - collapse_cells() re-classifies below and a
    # cell that mixes a real stratum with an empty one stays real
    d <- sc
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
      axis_y_cat(seq_along(ys) - 0.5, ys, cex = 0.8)
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
    main_txt <- sprintf("%s sampling coverage vs threshold%s", cov_title_what(what),
                        if (identical(what, "discard")) ""
                        else paste0(" (", weight_label, ")"))
    # Captured once, before the loop: dev.cur() changes the moment the
    # first dev.new() runs, so re-checking it every iteration would only
    # ever open a second screen and then stop.
    had_device <- grDevices::dev.cur() > 1
    for (grp in groups) {
      op <- graphics::par(no.readonly = TRUE); on.exit(graphics::par(op), add = TRUE)
      if (length(groups) > 1 && !had_device) grDevices::dev.new()
      if (!identical(grp, "__nofacet__") && length(grp) > 1) {
        gd <- facet_grid_dims(length(grp)); nr <- gd[1L]; nc <- gd[2L]
        graphics::par(mfrow = c(nr, nc), mar = c(min(bmar, 7), lmar, 2, 1),
                      oma = c(0, 0, 3, 1))
        for (fv in grp)
          draw_one(d[as.character(d[[facet_var]]) == fv, , drop = FALSE], fv)
        graphics::mtext(main_txt, outer = TRUE, side = 3, cex = 1.05, font = 2, line = 1)
        if (!is.null(facet_var))
          graphics::mtext(paste(facet_var, "=", paste(grp, collapse = ", ")),
                          outer = TRUE, side = 3, cex = 0.8, line = -0.2, col = "grey30")
      } else if (!identical(grp, "__nofacet__") && length(grp) == 1) {
        graphics::par(mar = c(bmar, lmar, 3, 1.5))
        draw_one(d[as.character(d[[facet_var]]) == grp, , drop = FALSE],
                 paste0(main_txt, " (", facet_var, " = ", grp, ")"))
      } else {
        graphics::par(mar = c(bmar, lmar, 3, 1.5))
        draw_one(d, main_txt)
      }
      legend_status_base(lk_all)
      graphics::par(op)
    }
    invisible(NULL)
    return(invisible(NULL))
  }

  ## ============ ggplot engine ========================================
  # keep cat_key == "none" (a stratum that landed nothing) so its cell
  # shows in the "no landings / no discards" colour, not as a blank hole
  d <- sc
  if (!nrow(d)) return(NULL)
  if (is.null(x_var) || !nzchar(x_var)) { d$.x <- "All"; x_var <- ".x" }
  d <- collapse_cells(d, c(y_var, x_var, facet_var), threshold, what, min_meas,
                      min_trips)
  fmt <- function(v) ifelse(is.na(v) | v <= 0, "",
                     ifelse(v > 99, sprintf("%.0f", v),
                     ifelse(v >= 10, sprintf("%.1f", v), sprintf("%.2f", v))))
  stat_all <- names(status_palette(threshold, what, min_meas, min_trips))
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
                                 min_cov = 50, min_trip = 2, min_fish = NULL,
                                 n_per_screen = NULL,
                                 palette = c("default", "colorblind"),
                                 engine = c("ggplot", "base")) {
  view <- "bubble"; fn <- "plot_coverage_bubble"
  engine <- match.arg(engine); palette <- match.arg(palette)
  strata <- catch
  angle <- 45
  what <- normalize_what(what)
  if (is.null(y) || !nzchar(y)) stop(fn, "(): `y` is required", call. = FALSE)

  user_cc <- !is.null(catchCategory)
  if (identical(what, "discard")) {
    if (user_cc)
      stop(fn, "(): catchCategory does not apply to what = \"discard\" ",
           "(discard coverage is the discard part by definition)",
           call. = FALSE)
    cc <- NULL
  } else {
    cc <- if (user_cc) match.arg(catchCategory, c("Lan", "Dis", "BMS", "All")) else "All"
  }

  plot_arg_report(fn, list(
    list(bad = identical(palette, "colorblind") &&
           view %in% c("heat", "bubble"),
         msg = "palette does not recolour a status heatmap / bubble grid (the cells use a fixed semantic scale)"),
    list(bad = identical(engine, "ggplot") && !is.null(n_per_screen),
         msg = "n_per_screen is a base-engine option; the ggplot engine draws one figure")))

  wl   <- default_weight_label(what, cc)
  xcol <- x

  strata <- build_coverage_strata(strata, NULL, y, xcol, facet, min_cov,
                                    what, min_fish, min_trip, distributions,
                                    catch_category_parts(cc))

  sc <- strata; y_var <- y; x_var <- x; fill_var <- NULL
  facet_var <- facet; threshold <- min_cov; weight_label <- wl
  min_meas <- min_fish; min_trips <- min_trip

  ## ============ base engine ==========================================
  if (identical(engine, "base")) {
    # keep cat_key == "none" (a stratum that landed nothing) so its cell
    # is drawn in the "no landings / no discards" colour rather than
    # left as a blank hole - collapse_cells() re-classifies below and a
    # cell that mixes a real stratum with an empty one stays real
    d <- sc
    if (!nrow(d)) { message("Nothing to plot."); return(invisible(NULL)) }
    if (is.null(x_var) || !nzchar(x_var)) { d$.x <- "All"; x_var <- ".x" }
    d <- collapse_cells(d, c(y_var, x_var, facet_var), threshold, what,
                        min_meas, min_trips)
    smallest <- suppressWarnings(min(d$landings[d$landings > 0], na.rm = TRUE))
    if (!is.finite(smallest)) smallest <- 1
    d$plot_size <- ifelse(d$cat_key == "disc_only", smallest, d$landings)
    pal <- status_palette(threshold, what, min_meas, min_trips)

    trp_bit <- if (thr_active(min_trips) && "numTrips" %in% names(d))
      ifelse(!is.na(d$numTrips) & d$numTrips < min_trips,
             paste0(round(d$numTrips), " trip",
                    ifelse(round(d$numTrips) == 1, "", "s")), "") else ""
    if (what %in% c("length", "age")) {
      cov_bit <- ifelse(!is.na(d$coverage) & d$coverage < threshold,
                        paste0(round(d$coverage, 1), "%"), "")
      mea_bit <- if (thr_active(min_meas))
        ifelse(!is.na(d$n_measurements) & d$n_measurements < min_meas,
              paste0("n=", format(round(d$n_measurements),
                                  big.mark = ",", trim = TRUE)), "") else ""
      lab_me <- grepl("^warn_", d$cat_key)
      d$lab <- ifelse(lab_me,
                      paste0(cov_bit, ifelse(nzchar(cov_bit) & nzchar(mea_bit),
                                             " ", ""), mea_bit,
                             ifelse(nzchar(trp_bit) &
                                      (nzchar(cov_bit) | nzchar(mea_bit)),
                                    "\n", ""), trp_bit), "")
    } else {
      d$lab <- dplyr::case_when(
        d$cat_key == "disc_only" ~ "100% Disc",
        grepl("^warn_", d$cat_key) ~
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
      axis_y_cat(seq_along(ys), ys, cex = 0.8)
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
        graphics::legend("topright", title = sprintf("%s (t)", weight_label),
                         legend = format(ref, big.mark = ","),
                         pch = 21, pt.cex = c(0.9, 1.4, 2), pt.bg = "grey70",
                         col = OUTLINE, cex = 0.8, bty = "o",
                         bg = grDevices::adjustcolor("white", 0.88),
                         box.col = "grey65", xpd = NA, inset = c(0.02, 0.02))
      }
    }

    groups <- split_facets_to_screens(facets, n_per_screen)
    had_device <- grDevices::dev.cur() > 1
    for (grp in groups) {
      op <- graphics::par(no.readonly = TRUE); on.exit(graphics::par(op), add = TRUE)
      if (length(groups) > 1 && !had_device) grDevices::dev.new()
      if (!identical(grp, "__nofacet__") && length(grp) > 1) {
        gd <- facet_grid_dims(length(grp)); nr <- gd[1L]; nc <- gd[2L]
        graphics::par(mfrow = c(nr, nc), mar = c(min(bmar, 7), lmar, 2, 1.5),
                      oma = c(0, 0, 3, 1))
        for (fv in grp)
          draw_one(d[as.character(d[[facet_var]]) == fv, , drop = FALSE], fv)
        graphics::mtext(main_txt, outer = TRUE, side = 3, cex = 1.05, font = 2, line = 1)
        if (!is.null(facet_var))
          graphics::mtext(paste(facet_var, "=", paste(grp, collapse = ", ")),
                          outer = TRUE, side = 3, cex = 0.8, line = -0.2, col = "grey30")
      } else if (!identical(grp, "__nofacet__") && length(grp) == 1) {
        graphics::par(mar = c(bmar, lmar, 4, 1.5))
        draw_one(d[as.character(d[[facet_var]]) == grp, , drop = FALSE],
                 paste0(main_txt, " (", facet_var, " = ", grp, ")"))
      } else {
        graphics::par(mar = c(bmar, lmar, 4, 1.5))
        draw_one(d, "")
        graphics::mtext(main_txt, side = 3, line = 1.2, cex = 1.05, font = 2)
      }
      draw_legends()
      graphics::par(op)
    }
    invisible(NULL)
    return(invisible(NULL))
  }

  ## ============ ggplot engine ========================================
  # keep cat_key == "none" (a stratum that landed nothing) so its cell
  # shows in the "no landings / no discards" colour, not as a blank hole
  d <- sc
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
  stat_all <- names(status_palette(threshold, what, min_meas, min_trips))
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
                              n_per_screen = NULL,
                              palette = c("default", "colorblind"),
                              engine = c("ggplot", "base")) {
  view <- "bar"; fn <- "plot_coverage_bar"
  engine <- match.arg(engine); palette <- match.arg(palette)
  strata <- catch
  x <- NULL; min_cov <- 50; min_trip <- 2; min_fish <- 0; angle <- 45
  what <- normalize_what(what)
  if (is.null(y) || !nzchar(y)) stop(fn, "(): `y` is required", call. = FALSE)

  user_cc <- !is.null(catchCategory)
  if (identical(what, "discard")) {
    if (user_cc)
      stop(fn, "(): catchCategory does not apply to what = \"discard\" ",
           "(discard coverage is the discard part by definition)",
           call. = FALSE)
    cc <- NULL
  } else {
    cc <- if (user_cc) match.arg(catchCategory, c("Lan", "Dis", "BMS", "All")) else "All"
  }

  plot_arg_report(fn, list(
    list(bad = identical(engine, "ggplot") && !is.null(n_per_screen),
         msg = "n_per_screen is a base-engine option; the ggplot engine draws one figure")))

  wl   <- default_weight_label(what, cc)
  xcol <- fill

  strata <- build_coverage_strata(strata, NULL, y, xcol, facet, min_cov,
                                    what, min_fish, min_trip, distributions,
                                    catch_category_parts(cc))

  sc <- strata; y_var <- y; x_var <- x; fill_var <- fill
  facet_var <- facet; threshold <- min_cov; weight_label <- wl
  min_meas <- min_fish; min_trips <- min_trip

  ## ============ base engine ==========================================
  if (identical(engine, "base")) {
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
    # this function's ggplot branch draws, just without its exact
    # separator line and percentage callouts.
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
      key <- dd$.lab
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
                              names.arg = NA, axisnames = FALSE,
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
      # Y axis: ticks plus EVERY row label (barplot's own names.arg is
      # thinned once the rows are dense - axis_y_cat() shrinks the text to
      # fit instead of dropping labels).
      axis_y_cat(bp[, 1], key[ord], cex = 0.7)

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
      if (has(fill_var)) {
        lv <- sort(unique(dd$.fill))
        # Legend inside the panel, bottom-right corner, on a semi-opaque
        # box - the right margin is reserved for the "Sampled / Not sampled
        # (NN%)" callouts, not the fill key. Drawn on EVERY panel, faceted
        # or not, so a facet grid does not lose the key; smaller when the
        # panel is a fraction of the device.
        graphics::legend("bottomright", legend = lv, fill = pal[lv],
                         cex = if (compact) 0.6 else 0.8, bty = "o",
                         bg = grDevices::adjustcolor("white", 0.85),
                         box.col = "grey65", xpd = NA, inset = c(0.02, 0.02),
                         ncol = min(2, ceiling(length(lv) /
                                               (if (compact) 6 else 10))))
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
        gd <- facet_grid_dims(length(grp)); nr <- gd[1L]; nc <- gd[2L]
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
    return(invisible(NULL))
  }

  ## ============ ggplot engine ========================================
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
#  Discard rate (both engines in one function)
# ----------------------------------------------------------

#' Discard rate per stratum
#'
#' One point per stratum, its discard rate on the y axis - raw, with no
#' aggregation or averaging. A stratum here is one `x` + `colour` +
#' `shape` + `facet` cell: each point is that cell's own discards over
#' its own landings plus discards (`discards / (landings + discards)`,
#' from [build_strata()]), not the mean of any finer strata. Change `x`
#' (or `colour` / `shape` / `facet`) and the strata - and the points -
#' are rebuilt at that resolution.
#'
#' Takes raw CEF catch (it builds the strata with [build_strata()]) or an
#' already-built stratum table. Strata with no discard estimate are
#' dropped - a rate is a property of sampled strata only (see
#' [sampled_for_discards()]).
#'
#' One function, two engines: `engine = "ggplot"` (default) draws it with
#' ggplot2; `engine = "base"` draws the same figure with base graphics.
#'
#' @param catch Raw CEF catch data, or a stratum table with `discard_rate`.
#'   When raw, the strata are built on `x` + `colour` + `shape` + `facet`,
#'   and each point is one such stratum - not an average over finer ones.
#' @param x Column on the x axis. One point per level (times `colour` /
#'   `shape` / `facet`).
#' @param colour Column mapped to point fill.
#' @param shape Column mapped to point shape.
#' @param facet Optional facet column.
#' @param jitter Jitter the points horizontally (cosmetic only - one
#'   point is still one stratum).
#' @param n_per_screen Base engine: split a faceted figure across this
#'   many devices.
#' @param palette `"default"` or `"colorblind"` (point colours, via
#'   [var_palette()]).
#' @param engine `"ggplot"` (default) or `"base"`.
#' @return A ggplot object (`engine = "ggplot"`), or `NULL` invisibly
#'   after drawing to the current device (`engine = "base"`).
#' @seealso [sampled_for_discards()], [build_strata()]
#' @family discard-rate plots
#' @examples
#' \dontrun{
#' plot_discard_rate(catch_data, x = "Season", colour = "metier4")
#' plot_discard_rate(catch_data, x = "metier4", engine = "base")
#' }
#' @export
plot_discard_rate <- function(catch, x, colour = NULL, shape = NULL,
                              facet = NULL, jitter = TRUE,
                              n_per_screen = NULL,
                              palette = c("default", "colorblind"),
                              engine = c("ggplot", "base")) {
  engine  <- match.arg(engine)
  palette <- match.arg(palette)
  angle   <- 45
  dims    <- NULL
  has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
  plot_arg_report("plot_discard_rate", list(
    list(bad = identical(engine, "ggplot") && !is.null(n_per_screen),
         msg = "n_per_screen is a base-engine option")))
  # Raw catch never carries discard_rate - build_strata() adds it. A
  # caller passing an already-built table (has the column) is used as is.
  if (!"discard_rate" %in% names(catch)) {
    if (is.null(dims))
      dims <- unique(c(x, if (has(colour)) colour, if (has(shape)) shape,
                       if (has(facet)) facet))
    catch <- build_strata(catch, dims)
  }
  d <- sampled_for_discards(catch)
  x_var <- x; colour_var <- colour; shape_var <- shape; facet_var <- facet

  ## ============ base engine ============================================
  if (identical(engine, "base")) {
    if (!nrow(d)) { message("Nothing to plot."); return(invisible(NULL)) }
    xs <- sort(unique(as.character(d[[x_var]])))
    d$.xi <- match(as.character(d[[x_var]]), xs)
    if (isTRUE(jitter)) {
      set.seed(1)
      d$.xi <- d$.xi + stats::runif(nrow(d), -0.18, 0.18)
    }
    d$.rate <- d$discard_rate * 100
    # Colour and shape attached as columns of d itself, not looked up
    # separately by row position or name - a lookup keyed on rownames()
    # silently returns NA (an invisible point) the moment a facet
    # subset's rownames no longer match the un-subset table's.
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
    main_txt <- "Discard rate - one point per stratum (raw, no averaging)"
    bmar <- cat_axis_mar(xs, 0.7, angle)
    draw_legend <- function() {
      if (!has(colour_var)) return(invisible(NULL))
      lv <- sort(unique(as.character(d[[colour_var]])))
      legend_overlay("bottomright", title = colour_var, legend = lv,
                     pt.bg = pal[lv], pch = 21, pt.cex = 1.5, cex = 0.95,
                     bty = "o", bg = grDevices::adjustcolor("white", 0.9),
                     box.col = "grey65", y.intersp = 1.15, inset = c(0.02, 0.02))
    }

    groups <- split_facets_to_screens(facets, n_per_screen)
    had_device <- grDevices::dev.cur() > 1
    for (grp in groups) {
      op <- graphics::par(no.readonly = TRUE); on.exit(graphics::par(op), add = TRUE)
      if (length(groups) > 1 && !had_device) grDevices::dev.new()
      rmar <- 2
      if (!identical(grp, "__nofacet__") && length(grp) > 1) {
        gd <- facet_grid_dims(length(grp)); nr <- gd[1L]; nc <- gd[2L]
        graphics::par(mfrow = c(nr, nc), mar = c(min(bmar, 8), 4, 2, rmar),
                      oma = c(2, 0, 3, 1.5))
        for (fv in grp) draw_one(d[as.character(d[[facet_var]]) == fv, , drop = FALSE], fv)
        graphics::mtext(main_txt, outer = TRUE, side = 3, cex = 1.05, font = 2, line = 1)
        graphics::mtext(paste(facet_var, "=", paste(grp, collapse = ", ")),
                        outer = TRUE, side = 3, cex = 0.8, line = -0.2, col = "grey30")
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
    return(invisible(NULL))
  }

  ## ============ ggplot engine =========================================
  if (!nrow(d)) return(NULL)
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
  if (has(colour_var))
    p <- p + scale_fill_var(colour_var, d[[colour_var]], name = colour_var,
                            palette = palette)

  p <- p +
    scale_y_continuous(labels = function(z) paste0(z, "%")) +
    maybe_facet(facet_var, free_y = FALSE, ncol = 2) +
    labs(title = "Discard rate - one point per stratum (raw, no averaging)",
         subtitle = "One point per x / colour / shape / facet stratum with discard data",
         y = "Discard rate", x = x_var,
         fill = if (has(colour_var)) colour_var else NULL,
         shape = if (has(shape_var)) shape_var else NULL) +
    app_theme() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.text = element_text(size = 11),
          legend.key.size = unit(1.1, "lines"))
  p
}


# ----------------------------------------------------------
#  Distribution builders
# ----------------------------------------------------------

# ----------------------------------------------------------
#  Length / age distribution plots (both engines in one function)
# ----------------------------------------------------------

#' Numbers at length or age
#'
#' The catch numbers spread over the length (or age) classes, as a
#' stacked (or dodged) histogram. One function, two engines
#' (`engine = "ggplot"` / `"base"`).
#'
#' `data` is a [length_numbers()] table, or raw CEF catch with
#' `distributions` (then the link + summary run internally, with
#' `catchCategory` choosing the landings / discards / BMS part, or
#' `"All"` for the landings and discards together).
#' Numbers come out in thousands (N3), length in cm, age in years.
#'
#' @param data A [length_numbers()] table, or raw CEF catch data.
#' @param distributions A CEF distributions table - needed only with raw
#'   catch.
#' @param distributionType `"LengthTotal"` (default) or `"Age"`.
#' @param catchCategory `"All"` (default - landings + discards, titled
#'   "catch"), `"Lan"`, `"Dis"` or `"BMS"`. When `distributions` is
#'   given, the resolved word goes in the plot title.
#' @param fill Column mapped to bar fill.
#' @param facet Up to two facet columns (`"col"` or `c("row", "col")`).
#' @param stacked Stack the bars on `fill` rather than dodge.
#' @param scales Facet scales (ggplot): `"fixed"`, `"free_x"`,
#'   `"free_y"` or `"free"`.
#' @param n_per_screen Base engine: split a faceted figure across devices.
#' @param palette `"default"` or `"colorblind"`.
#' @param engine `"ggplot"` (default) or `"base"`.
#' @return A ggplot object, or `NULL` invisibly (base engine).
#' @seealso [plot_meanweight_at_length_age()], [length_numbers()]
#' @family distribution plots
#' @examples
#' \dontrun{
#' plot_numbers_at_length_age(catch, distributions, distributionType = "Age",
#'                            catchCategory = "Dis", fill = "Country")
#' }
#' @export
plot_numbers_at_length_age <- function(data, distributions = NULL,
                                       distributionType = c("LengthTotal", "Age"),
                                       catchCategory = "All",
                                       fill = NULL, facet = NULL,
                                       stacked = TRUE, scales = "fixed",
                                       n_per_screen = NULL,
                                       palette = c("default", "colorblind"),
                                       engine = c("ggplot", "base")) {
  typed_dt <- !missing(distributionType); typed_cc <- !missing(catchCategory)
  angle <- 45; dims <- NULL
  distributionType <- match.arg(distributionType)
  engine  <- match.arg(engine)
  palette <- match.arg(palette)
  catchCategory <- match.arg(catchCategory, c("Lan", "Dis", "BMS", "All"))
  if (length(facet) > 2)
    stop("plot_numbers_at_length_age(): `facet` takes at most two columns",
         call. = FALSE)
  facet1 <- if (length(facet) >= 1) facet[1] else NULL
  facet2 <- if (length(facet) >= 2) facet[2] else NULL
  has_ <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
  plot_arg_report("plot_numbers_at_length_age", list(
    list(bad = is.null(distributions) && (typed_dt || typed_cc),
         msg = "distributionType / catchCategory only matter when `distributions` is supplied (building from raw catch)"),
    list(bad = identical(engine, "ggplot") && !is.null(n_per_screen),
         msg = "n_per_screen is a base-engine option"),
    list(bad = identical(engine, "base") && !identical(scales, "fixed"),
         msg = "scales is a ggplot facet option")))
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

  ## ============ base engine ==========================================
  if (identical(engine, "base")) {
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
            graphics::legend("bottomright", legend = lv, fill = pal[lv], cex = 0.9,
                             bty = "o", bg = grDevices::adjustcolor("white", 0.88),
                             box.col = "grey65", inset = c(0.02, 0.02))
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
        main_txt <- paste0(sprintf("Numbers at %s",
                                   tolower(sub(" .*", "", class_label))), cc_suffix)
        groups <- split_facets_to_screens(facets, n_per_screen)
        had_device <- grDevices::dev.cur() > 1
        for (grp in groups) {
          op <- graphics::par(no.readonly = TRUE); on.exit(graphics::par(op), add = TRUE)
          if (length(groups) > 1 && !had_device) grDevices::dev.new()
          if (!identical(grp, "__nofacet__") && length(grp) > 1) {
            gd <- facet_grid_dims(length(grp)); nr <- gd[1L]; nc <- gd[2L]
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
    return(invisible(NULL))
  }

  ## ============ ggplot engine ========================================
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
#' The number-weighted mean weight in each length (or age) class, as a
#' line with points. When the strata are averaged (no `shape`) and a
#' spread is available, a +/- 1 sd band is drawn too. Live weight and a
#' line are the only options, so there is no `variableType` or `type`
#' argument. One function, two engines.
#'
#' `data` is a [length_meanweight()] table, or raw CEF catch with
#' `distributions` (then the link + summary run internally,
#' `catchCategory` choosing the part - `"All"` combines landings and
#' discards). Weight in grams, length in cm,
#' age in years.
#'
#' @param data A [length_meanweight()] table, or raw CEF catch data.
#' @param distributions A CEF distributions table - needed only with raw
#'   catch.
#' @param distributionType `"LengthTotal"` (default) or `"Age"`.
#' @param catchCategory `"All"` (default - landings + discards, titled
#'   "catch"), `"Lan"`, `"Dis"` or `"BMS"`. When `distributions` is
#'   given, the resolved word goes in the plot title.
#' @param colour Column mapped to line colour.
#' @param facet Up to two facet columns (`"col"` or `c("row", "col")`).
#' @param shape Column drawn as separate point shapes; opens the averaged
#'   group back up and drops the band.
#' @param scales Facet scales (ggplot).
#' @param n_per_screen Base engine: split a faceted figure across devices.
#' @param palette `"default"` or `"colorblind"`.
#' @param engine `"ggplot"` (default) or `"base"`.
#' @return A ggplot object, or `NULL` invisibly (base engine).
#' @seealso [plot_numbers_at_length_age()], [length_meanweight()]
#' @family distribution plots
#' @examples
#' \dontrun{
#' plot_meanweight_at_length_age(catch, distributions, distributionType = "Age",
#'                               colour = "metier4")
#' }
#' @export
plot_meanweight_at_length_age <- function(data, distributions = NULL,
                                          distributionType = c("LengthTotal", "Age"),
                                          catchCategory = "All",
                                          colour = NULL, facet = NULL, shape = NULL,
                                          scales = "fixed",
                                          n_per_screen = NULL,
                                          palette = c("default", "colorblind"),
                                          engine = c("ggplot", "base")) {
  typed_dt <- !missing(distributionType); typed_cc <- !missing(catchCategory)
  angle <- 45; dims <- NULL
  distributionType <- match.arg(distributionType)
  engine  <- match.arg(engine)
  palette <- match.arg(palette)
  catchCategory <- match.arg(catchCategory, c("Lan", "Dis", "BMS", "All"))
  if (length(facet) > 2)
    stop("plot_meanweight_at_length_age(): `facet` takes at most two columns",
         call. = FALSE)
  facet1 <- if (length(facet) >= 1) facet[1] else NULL
  facet2 <- if (length(facet) >= 2) facet[2] else NULL
  has_ <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
  plot_arg_report("plot_meanweight_at_length_age", list(
    list(bad = is.null(distributions) && (typed_dt || typed_cc),
         msg = "distributionType / catchCategory only matter when `distributions` is supplied (building from raw catch)"),
    list(bad = identical(engine, "ggplot") && !is.null(n_per_screen),
         msg = "n_per_screen is a base-engine option"),
    list(bad = identical(engine, "base") && !identical(scales, "fixed"),
         msg = "scales is a ggplot facet option")))
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

  ## ============ base engine ==========================================
  if (identical(engine, "base")) {
      if (is.null(d) || !nrow(d)) { message("Nothing to plot."); return(invisible(NULL)) }
      has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
      d$.fk <- facet_key_base(d, facet1, facet2)
      grp_var <- if (has(colour_var)) colour_var else if (has(shape_var)) shape_var else NULL
      pal <- if (has(colour_var)) var_palette(colour_var, d[[colour_var]], palette) else NULL
      pch_lv <- c(16, 17, 15, 18, 4, 3, 8, 7)

      band <- !has(shape_var)      # the +/- 1 sd band is for averaged strata only
      draw_one <- function(dd, main) {
        ylim <- range(c(dd$mean_weight,
                        if (band && "sd_weight" %in% names(dd))
                          dd$mean_weight + dd$sd_weight else NULL,
                        if (band && "sd_weight" %in% names(dd))
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
          if (band && "sd_weight" %in% names(gd) &&
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
          pch <- if (has(shape_var)) pch_lv[match(g, grp_lv)] else 21
          graphics::points(gd$.class, gd$mean_weight, pch = pch, bg = col,
                           col = if (pch %in% 21:25) OUTLINE else col, cex = 1.1)
        }
        if (!is.null(grp_var) && length(grp_lv) > 1) {
          pch_leg <- if (has(shape_var)) pch_lv[seq_along(grp_lv)] else rep(21, length(grp_lv))
          graphics::legend("bottomright", title = grp_var, legend = grp_lv,
                           col = if (has(colour_var)) pal[grp_lv] else PAL_BASE[1],
                           pt.bg = if (has(colour_var)) pal[grp_lv] else PAL_BASE[1],
                           pch = pch_leg, lwd = 1.8, cex = 0.9, bty = "o",
                           bg = grDevices::adjustcolor("white", 0.88),
                           box.col = "grey65", inset = c(0.02, 0.02))
        }
      }

      facets <- if (!all(is.na(d$.fk))) sort(unique(d$.fk)) else NA
      main_txt <- paste0(sprintf("Mean weight at %s",
                                 tolower(sub(" .*", "", class_label))), cc_suffix)
      groups <- split_facets_to_screens(facets, n_per_screen)
      had_device <- grDevices::dev.cur() > 1
      for (grp in groups) {
        op <- graphics::par(no.readonly = TRUE); on.exit(graphics::par(op), add = TRUE)
        if (length(groups) > 1 && !had_device) grDevices::dev.new()
        if (!identical(grp, "__nofacet__") && length(grp) > 1) {
          gd <- facet_grid_dims(length(grp)); nr <- gd[1L]; nc <- gd[2L]
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
    return(invisible(NULL))
  }

  ## ============ ggplot engine ========================================
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
