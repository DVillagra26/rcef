# ============================================================
#  viz_helpers.R
#  Shared plumbing for R/viz_plots.R: mesh order and category columns, the
#  imported / raised and sampled / estimated marks (a cross, a dotted line, a
#  hatched bar), the facet layer, the
#  grouping sentence, the catch-part words and the class axis of the
#  distribution plots. The plot functions themselves are in
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

#' The natural level order of a column that carries mesh sizes
#'
#' `mesh` values are strings like `"16-31"`, `"<16"`, `">=120"`, `"0"`,
#' `"all"` - sorted as plain text, `"100-119"` comes before `"16-31"` and
#' `"all"` comes before `"90-99"`. For `mesh` this sorts by each range's own
#' lower bound (the upper bound only breaks a tie, e.g. `"90-99"` before
#' `"90-119"`): `"<N"` sits just below `N`, `">=N"` just above any closed
#' range that also starts at `N`, and `"all"` / `"All"` sort last. A value
#' matching none of those shapes sorts last too, rather than erroring - an
#' unrecognised mesh string should not stop a plot from drawing.
#'
#' `metier4_mesh` values are a gear and a mesh range joined by an underscore
#' (`"TBB_70-99"`, `"OTB_>=120"`), or a bare gear when the fleet code had no mesh
#' part. Sorted as plain text they interleave the sizes the same way raw `mesh`
#' does (`"TBB_100-119"` before `"TBB_70-99"`), so each gear is kept together,
#' in alphabetical order, with its meshes in the size order above - a bare gear
#' after all of its own meshes.
#'
#' @param var A column name (or `NULL`).
#' @param x The column's values.
#' @return The distinct, non-`NA` values of `x`, in natural order; `NULL` when
#'   `var` is neither `mesh` nor `metier4_mesh`.
#' @keywords internal
#' @noRd
#' @export
mesh_var_levels <- function(var, x) {
  if (!(identical(var, "mesh") || identical(var, "metier4_mesh"))) return(NULL)
  u <- unique(as.character(x))
  u <- u[!is.na(u)]
  if (!length(u)) return(u)
  by_size <- function(m) {                 # the distinct mesh strings, smallest first
    m <- unique(m)
    lo <- rep(Inf, length(m)); hi <- rep(Inf, length(m))
    rng <- regmatches(m, regexec("^\\s*([0-9.]+)\\s*-\\s*([0-9.]+)\\s*$", m))
    lt  <- regmatches(m, regexec("^\\s*<\\s*([0-9.]+)\\s*$", m))
    ge  <- regmatches(m, regexec("^\\s*>=\\s*([0-9.]+)\\s*$", m))
    num <- regmatches(m, regexec("^\\s*([0-9.]+)\\s*$", m))
    for (i in seq_along(m)) {
      if (tolower(trimws(m[i])) == "all") next
      if (length(rng[[i]]) == 3) {
        lo[i] <- as.numeric(rng[[i]][2]); hi[i] <- as.numeric(rng[[i]][3])
      } else if (length(lt[[i]]) == 2) {
        lo[i] <- as.numeric(lt[[i]][2]) - 0.1; hi[i] <- lo[i]
      } else if (length(ge[[i]]) == 2) {
        lo[i] <- as.numeric(ge[[i]][2]) + 0.1; hi[i] <- Inf
      } else if (length(num[[i]]) == 2) {
        lo[i] <- as.numeric(num[[i]][2]); hi[i] <- lo[i]
      }
    }
    m[order(lo, hi, m)]
  }
  if (identical(var, "mesh")) return(by_size(u))
  # metier4 is the first fleet part and never holds an underscore, so the first
  # underscore is the seam between gear and mesh
  gear <- sub("_.*$", "", u)
  mesh <- ifelse(grepl("_", u, fixed = TRUE), sub("^[^_]*_", "", u), "")
  # the mesh part of a bare gear is "": rank it after every real mesh, "all"
  # included, instead of wherever "" happens to sort alphabetically
  u[order(gear, match(mesh, c(setdiff(by_size(mesh), ""), "")), u)]
}

#' Make the columns a plot uses as categories fit to be categories
#'
#' Every column a plot is about to use as a category - an axis, a fill, a
#' colour, a shape, a facet - is a category, whatever type the file stored it
#' as, and has an order of its own. `mesh` and `metier4_mesh` are releveled into
#' size order ([mesh_var_levels()]), so they read the same wherever they turn
#' up. `seasonValue` is an integer quarter code in the CEF data: mapped straight
#' onto a fill, a shape or an axis it is read as continuous (a fill or shape
#' scale then errors outright, and an axis grows breaks at 1.5 and 2.5), so a
#' numeric column becomes a factor in numeric order, `2` before `10`. Done once,
#' on the plot's own data, before any aesthetic is built from it; a no-op for a
#' column that is already character or factor, or that `d` does not carry.
#' The two status columns the raising and the allocation add
#' ([status_levels()]) are factors in the order they read, `imported` before
#' `raised` and `sampled` before `estimated`, so a legend, a panel order or a
#' colour never depends on the alphabet.
#'
#' @param d A data frame.
#' @param vars Column names the plot is about to use (`x`, `y`, `fill`,
#'   `colour`, `shape`, `facet`, ... - `NULL` entries are fine).
#' @return `d`, with those columns as factors where they need to be.
#' @keywords internal
#' @noRd
#' @export
category_cols <- function(d, vars) {
  vars <- vars[!is.na(vars) & nzchar(vars)]
  for (v in intersect(vars, names(d))) {
    lev <- mesh_var_levels(v, d[[v]])
    sl  <- status_levels(v)
    if (!is.null(sl)) {
      u <- unique(as.character(d[[v]])); u <- u[!is.na(u)]
      lev <- c(intersect(sl, u), setdiff(u, sl))
    }
    if (!is.null(lev)) d[[v]] <- factor(d[[v]], levels = lev)
    else if (is.numeric(d[[v]])) d[[v]] <- factor(d[[v]])
  }
  d
}

#' The values of a status column, in the order they read
#'
#' The raising adds `importedOrRaised` to the catch and the allocation adds
#' `sampledOrEstimated` to the distributions: what was in the data, and what
#' the step added to it.
#'
#' @param var A column name.
#' @return `c("imported", "raised")` for `importedOrRaised`,
#'   `c("sampled", "estimated")` for `sampledOrEstimated`, `NULL` for any other
#'   column.
#' @keywords internal
#' @noRd
#' @export
status_levels <- function(var) switch(var,
  importedOrRaised   = c("imported", "raised"),
  sampledOrEstimated = c("sampled", "estimated"),
  NULL)

#' Whether, and how, a plot marks what the raising or the allocation added
#'
#' One rule for the plots that look at a catch or at distributions before or
#' after a step - [plot_catches()], [plot_DR()], [plot_NaLoA()], [plot_MWaLoA()]
#' and [plot_compare_raised()]. The status column (`S`) is a variable like any
#' other: by default it is the mark, and what the mark is depends on what is
#' drawn. What was in the data is drawn plain; what the step added is a cross
#' where the data are points, a dotted line where they are lines, and a
#' transparent, hatched bar where they are bars. Named in `x`, `fill`, `colour`,
#' `shape` or `facet` it is drawn there instead, and then not as a mark as well;
#' `alpha = NULL` draws no mark. A mark only means something when the step did
#' add something, so with nothing raised (or estimated) there is none. The title
#' says what the data holds, precisely: *imported + raised* or *only imported*,
#' *sampled + estimated* or *only sampled*.
#'
#' @param S `"importedOrRaised"` or `"sampledOrEstimated"`.
#' @param alpha The plot's `alpha` argument: `S` or `NULL`.
#' @param named The columns the plot already maps to `x`, `fill`, `colour`,
#'   `shape` and `facet`.
#' @param present The values of the status column in the data that is drawn
#'   (`NULL` or empty when the data has no such column: it is the data as it
#'   came, imported, or sampled).
#' @param fn The calling function's name, for the error (with its colon).
#' @param override What the alpha scale's legend keys are drawn with (a fill, a
#'   colour ...), so they show on the white key background.
#' @return A list: `shown` (it is mapped elsewhere), `on` (it is the mark:
#'   something was raised or estimated), `word` (what the title says the data
#'   is: `"imported + raised"`, `"only imported"`, `"only raised"`, `"sampled +
#'   estimated"`, `"only sampled"` or `"only estimated"`), `status` (a function
#'   turning the column's values into the factor the mark is mapped to:
#'   `Imported` / `Raised`, `Sampled` / `Estimated`) and the scales to add when
#'   `on`: `scale` (transparency, 1 and 0.45 - the bars', and the points' when
#'   `shape` is taken), `shape` (circle and cross) and `line` (solid and dotted).
#' @keywords internal
#' @noRd
#' @export
status_marks <- function(S, alpha, named, present, fn, override = list(fill = "grey30")) {
  lv <- status_levels(S)
  if (!is.null(alpha) && !identical(alpha, S))
    stop(fn, "`alpha` is \"", S, "\" (to mark what was ", lv[2],
         ") or NULL for no mark", call. = FALSE)
  shown <- S %in% named
  lab <- paste0(toupper(substring(lv, 1, 1)), substring(lv, 2))
  have <- lv[lv %in% present]
  if (!length(have)) have <- lv[1]
  list(shown = shown,
       on = !is.null(alpha) && !shown && lv[2] %in% have,
       word = if (length(have) == 2) paste(have, collapse = " + ")
              else paste("only", have),
       status = function(x) factor(lab[match(as.character(x), lv)], levels = lab),
       scale = scale_alpha_manual(values = setNames(c(1, 0.45), lab), name = NULL,
                                  guide = guide_legend(override.aes = override)),
       shape = scale_shape_manual(values = setNames(c(21, 4), lab), name = NULL),
       line  = scale_linetype_manual(values = setNames(c("solid", "dotted"), lab),
                                     name = NULL))
}

#' The colour of the points of a plot that marks its raised values with crosses
#'
#' A circle is drawn with a fill and a dark outline, a cross has no fill and
#' takes its colour from the `colour` aesthetic. So a plot that draws the
#' status as circles and crosses gives each point a `.edge`: the dark outline
#' for a circle and, for a cross, the level of the category the circle would
#' have been filled with (or one fixed colour when there is none). And a
#' `.stroke`, which lets a cross be drawn heavier than the outline of a circle.
#'
#' @param d The plot's data, with `.status` (`Imported` / `Raised`, or `Sampled` /
#'   `Estimated`) in it.
#' @param category The column the circles are filled by, or `NULL`.
#' @param cross_stroke,circle_stroke The line weight of a cross and of a
#'   circle's outline.
#' @return `d` with `.edge` and `.stroke`.
#' @keywords internal
#' @noRd
#' @export
cross_marks <- function(d, category, cross_stroke, circle_stroke) {
  marked <- d$.status %in% c("Raised", "Estimated")
  d$.edge   <- ifelse(marked, if (is.null(category)) ".cross" else as.character(d[[category]]),
                      ".outline")
  d$.stroke <- ifelse(marked, cross_stroke, circle_stroke)
  d
}

#' Stripes inside bars: how a hatched bar is drawn
#'
#' A bar that is transparent is hatched as well, so what the raising or the
#' allocation added can be told from what was in the data even where
#' transparency is hard to see. The stripes run at 45 degrees and are a fixed
#' distance apart on the page - not in data units - so they look the same on
#' every bar and in every panel, and they continue across neighbouring bars. A
#' grob, because the distance is in millimetres and the size of the panel is
#' only known when it is drawn.
#'
#' @param x A `hatch_grob`: its `rects` (the bars' `xmin`, `xmax`, `ymin`,
#'   `ymax` in the panel's npc units) and the `gap` between stripes, in mm.
#' @return The grob with its stripes.
#' @keywords internal
#' @noRd
#' @exportS3Method grid::makeContent
makeContent.hatch_grob <- function(x) {
  W <- grid::convertWidth(grid::unit(1, "npc"), "mm", valueOnly = TRUE)
  H <- grid::convertHeight(grid::unit(1, "npc"), "mm", valueOnly = TRUE)
  r <- x$rects
  step <- x$gap * sqrt(2)               # between two stripes, measured along x - y
  seg <- lapply(seq_len(nrow(r)), function(i) {
    x0 <- r$xmin[i] * W; x1 <- r$xmax[i] * W
    y0 <- r$ymin[i] * H; y1 <- r$ymax[i] * H
    if (!all(is.finite(c(x0, x1, y0, y1))) || x1 <= x0 || y1 <= y0) return(NULL)
    k <- seq.int(ceiling((x0 - y1) / step), floor((x1 - y0) / step))
    cc <- k * step                       # the stripe x - y = cc, cut to the bar
    xa <- pmax(x0, y0 + cc); xb <- pmin(x1, y1 + cc)
    keep <- xb > xa
    cbind(x0 = xa[keep], y0 = (xa - cc)[keep], x1 = xb[keep], y1 = (xb - cc)[keep])
  })
  seg <- do.call(rbind, seg)
  grid::setChildren(x, if (is.null(seg) || !nrow(seg)) grid::gList()
    else grid::gList(grid::segmentsGrob(
      grid::unit(seg[, "x0"], "mm"), grid::unit(seg[, "y0"], "mm"),
      grid::unit(seg[, "x1"], "mm"), grid::unit(seg[, "y1"], "mm"))))
}

#' A bar layer that hatches its transparent bars
#'
#' [ggplot2::geom_col()] whose bars with an `alpha` below 1 are also hatched
#' (see [makeContent.hatch_grob()]), in the colour of their outline - and so are
#' their legend keys. The plots that mark what a step added map `alpha` to the
#' status, so the hatch follows the status without a second aesthetic. The
#' geom is made here, per call, from the `GeomCol` of the ggplot2 in use.
#'
#' @param mapping,data,position,show.legend,inherit.aes,... As in
#'   [ggplot2::geom_col()].
#' @param hatch_gap The distance between two stripes, in mm.
#' @keywords internal
#' @noRd
#' @export
geom_col_hatch <- function(mapping = NULL, data = NULL, position = "stack", ...,
                           hatch_gap = 1.8, show.legend = NA, inherit.aes = TRUE) {
  geom <- ggproto("GeomColHatch", GeomCol,
    draw_panel = function(self, data, panel_params, coord, lineend = "butt",
                          linejoin = "mitre", hatch_gap = 1.8) {
      bars <- ggproto_parent(GeomCol, self)$draw_panel(data, panel_params, coord,
                                                       lineend = lineend, linejoin = linejoin)
      hit <- which(!is.na(data$alpha) & data$alpha < 1)
      if (!length(hit) || !coord$is_linear()) return(bars)
      cd <- coord$transform(data[hit, , drop = FALSE], panel_params)
      col <- cd$colour[!is.na(cd$colour)][1]
      lw  <- cd$linewidth[!is.na(cd$linewidth)][1]
      grid::grobTree(bars, grid::gTree(
        rects = cd[c("xmin", "xmax", "ymin", "ymax")], gap = hatch_gap,
        gp = grid::gpar(col = if (is.na(col)) "grey25" else col,
                        lwd = (if (is.na(lw)) 0.25 else lw) * .pt, lineend = "butt"),
        cl = "hatch_grob"))
    },
    draw_key = function(data, params, size) {
      box <- draw_key_polygon(data, params, size)
      if (is.na(data$alpha %||% NA) || data$alpha >= 1) return(box)
      c0 <- seq(-0.75, 0.75, by = 0.25)     # the stripes y = x + c0 across the square key
      grid::grobTree(box, grid::segmentsGrob(
        x0 = ifelse(c0 >= 0, 0, -c0), y0 = ifelse(c0 >= 0, c0, 0),
        x1 = ifelse(c0 >= 0, 1 - c0, 1), y1 = ifelse(c0 >= 0, 1, 1 + c0),
        gp = grid::gpar(col = data$colour %||% "grey25", lwd = 0.7)))
    })
  layer(geom = geom, stat = "identity", data = data, mapping = mapping,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(hatch_gap = hatch_gap, na.rm = FALSE, ...))
}

#' Facet layer for the plots
#'
#' One variable ([ggplot2::facet_wrap()]), two ([ggplot2::facet_grid()], rows x
#' cols) or none (`NULL`), from up to two column names, so a plot can add it
#' unconditionally.
#'
#' @param f1,f2 Column names for the two facet dimensions; either may be
#'   `NULL`/`""`.
#' @param ncol Facet columns when only one variable is used.
#' @param scales Facet scales: "fixed", "free_x", "free_y" or "free".
#' @return A ggplot facet layer, or `NULL`.
#' @keywords internal
#' @noRd
#' @export
facet_layer <- function(f1 = NULL, f2 = NULL, ncol = 2, scales = "fixed") {
  has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
  if (has(f1) && has(f2))
    facet_grid(rows = vars(.data[[f1]]), cols = vars(.data[[f2]]),
               scales = scales)
  else if (has(f1)) facet_wrap(vars(.data[[f1]]), ncol = ncol, scales = scales)
  else if (has(f2)) facet_wrap(vars(.data[[f2]]), ncol = ncol, scales = scales)
  else NULL
}

#' The four stratum families and their column levels, finest first
#'
#' Shared by [strata_sentence()] and [plot_DR()]: fleet
#' (`fleetValue` > `metier6` > `metier5`/`metier4_mesh` >
#' `metier4`/`gear`/`target`/`mesh` > `FleetType`), area (`areaValue` >
#' `ICESSubdivision` > `ICESDivision` > `ICESSubArea`), country and season
#' (`vesselFlagCountry`, `seasonValue` - a single level each, unlike
#' fleet/area). `metier4_mesh` (gear and mesh) sits at level 3 beside
#' `metier5` (gear and target): `metier6` determines both and both determine
#' `metier4`, but neither determines the other - which is why a tie keeps
#' both.
#'
#' @return A named list of named integer vectors (column name -> level,
#'   1 = finest).
#' @keywords internal
#' @noRd
#' @export
strata_families <- function() list(
  fleet   = c(fleetValue = 1, metier6 = 2, metier5 = 3, metier4_mesh = 3,
              metier4 = 4, gear = 4, target = 4, mesh = 4, FleetType = 5),
  area    = c(areaValue = 1, ICESSubdivision = 2, ICESDivision = 3,
              ICESSubArea = 4),
  country = c(vesselFlagCountry = 1),
  season  = c(seasonValue = 1))

#' The caption sentence naming the strata and what they group over
#'
#' A stratum is fleet x area x country x season. A figure that shows only
#' some of those - metier4 by vesselFlagCountry, say - groups everything
#' else into each mark: every area and season, and every fleet finer than
#' metier4. This names what is shown and what is grouped over, so a caption
#' can say it.
#'
#' The variables are grouped into the four families [strata_families()]
#' defines. A family with no variable shown is grouped over entirely; one
#' shown at a coarse level is grouped over the finer levels the data has. A
#' family with only one member (country, season) is never parenthesised -
#' there is no coarser/finer level to disambiguate, unlike fleet/area.
#' `Stratum`, and the domain columns `domainCatchDis` / `domainBiology`,
#' stand for all four at their finest (a domain names its fleet, area,
#' country and season), so nothing is grouped over.
#'
#' @param vars The columns the figure shows or splits by.
#' @param data The data the figure is made from. Raw catch says which stratum
#'   columns exist, so only variables it really has are named; a coverage
#'   table no longer has them and `NULL` has nothing to say, so both assume
#'   the finest of each family.
#' @return e.g. `"grouping = metier4 x vesselFlagCountry x seasonValue,
#'   grouped over finer fleet levels (fleetValue) and area (areaValue)."`;
#'   `"grouping = no grouping, raw data as in CEF (CN) data."` when nothing
#'   is grouped over; `""` when `vars` names no column.
#' @keywords internal
#' @noRd
#' @export
strata_sentence <- function(vars, data = NULL) {
  vars <- unique(as.character(vars[!is.na(vars) & nzchar(vars)]))
  if (!length(vars)) return("")
  have <- if (is.data.frame(data) && is.na(coverage_table_what(data))) names(data)
  fam <- strata_families()
  canonical <- c(fleet = "fleetValue", area = "areaValue",
                 country = "vesselFlagCountry", season = "seasonValue")
  grouped <- character(0)
  if (!any(c("Stratum", "domainCatchDis", "domainBiology") %in% vars)) {
    for (f in names(fam)) {
      m <- fam[[f]]
      present <- if (is.null(have)) canonical[[f]] else intersect(names(m), have)
      if (!length(present)) next
      finest <- present[which.min(m[present])]
      shown <- intersect(vars, names(m))
      one_level <- all(m == m[[1]])
      if (!length(shown)) {
        grouped <- c(grouped, if (one_level) f else sprintf("%s (%s)", f, finest))
      } else if (m[[finest]] < min(m[shown])) {
        grouped <- c(grouped, sprintf("finer %s levels (%s)", f, finest))
      }
    }
  }
  if (!length(grouped))
    return("grouping = no grouping, raw data as in CEF (CN) data.")
  # the Oxford comma the sentence's own wording asks for ("fleet, area, and season")
  n <- length(grouped)
  paste0("grouping = ", paste(vars, collapse = " x "), ", grouped over ",
         if (n <= 2) paste(grouped, collapse = " and ")
         else paste0(paste(grouped[-n], collapse = ", "), ", and ", grouped[n]), ".")
}

#' Wrap subtitle text to a plot's width
#'
#' Each argument is wrapped on its own and they are joined by newlines, so a
#' subtitle can be a few short paragraphs. ggplot2 does not wrap for you.
#'
#' @param ... Character strings; empty ones are dropped.
#' @param width Characters per line.
#' @keywords internal
#' @noRd
#' @export
sub_wrap <- function(..., width = 96) {
  parts <- Filter(function(s) length(s) == 1 && !is.na(s) && nzchar(s),
                  list(...))
  paste(vapply(parts, function(s) paste(strwrap(s, width = width),
                                        collapse = "\n"), ""),
        collapse = "\n")
}

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
#' @noRd
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
#' @noRd
catch_category_parts <- function(cc) {
  if (is.null(cc)) return("Lan")
  if (identical(cc, "All")) c("Lan", "Dis") else cc
}


#' Continuous x scale for a pooled length/age class axis
#'
#' The plus class reads "N+"; a length axis binned with `bin_width`
#' reads every break "lo-hi". Shared by [plot_NaLoA()] and [plot_MWaLoA()];
#' at most one of `plus_from` / `bin_from` is ever set (ages take a plus
#' group, lengths take bins). Returns `NULL` (a no-op added to a plot) when
#' neither applies.
#'
#' @param classes The `.class` values actually present.
#' @param plus_from Optional: the plus class.
#' @param bin_from Optional: the bin width.
#' @return A `ggplot2::scale_x_continuous()`, or `NULL`.
#' @keywords internal
#' @noRd
#' @export
class_axis_scale <- function(classes, plus_from = NULL, bin_from = NULL) {
  classes <- sort(unique(classes))
  if (!is.null(plus_from) && plus_from %in% classes) {
    pf <- plus_from
    return(scale_x_continuous(
      breaks = function(lim) {
        lo <- ceiling(lim[1]); hi <- floor(lim[2])
        b <- if (hi - lo <= 15) seq(lo, hi) else scales::extended_breaks()(lim)
        sort(unique(c(b[b < pf], pf)))
      },
      labels = function(b) ifelse(b == pf, paste0(format(pf, trim = TRUE), "+"),
                                  format(b, trim = TRUE))))
  }
  if (!is.null(bin_from))
    return(scale_x_continuous(
      breaks = classes,
      labels = paste0(format(classes, trim = TRUE), "-",
                      format(classes + bin_from, trim = TRUE))))
  NULL
}