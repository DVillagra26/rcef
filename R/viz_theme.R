# =====================================================================
#  viz_theme.R   (app_theme(), stable colour per level)
#
#  ORIGIN: this project (ILVO / RCEF Workbench). Freely refactorable.
#  Was: R_development/0_Functions_theme.R
#  No shiny in this file. If it needs `input`, `session` or a tag,
#  it belongs in app/, not here.
# =====================================================================

# ============================================================
#  0_Functions_theme.R
#  The shared look: one theme, and stable colours per level.
# ------------------------------------------------------------
#  Package-ready plotting-support module. It is ggplot2 only (no shiny), and
#  0_Functions_plots.R depends on it, so the plots can be built at the console
#  exactly as they are in the app.
#
#  WHY IT EXISTS
#  A level must keep its colour everywhere it appears: if "TBB" is orange in
#  the coverage heatmap it must be orange in the discard-rate plot and in the
#  river plot too, or the reader compares the wrong things. var_palette()
#  therefore remembers the colour it assigned to each level of each variable
#  in a private environment, and hands the same one back next time.
#
#  DEPENDENCIES  Imports: ggplot2, scales.
# ============================================================

# ----------------------------------------------------------
#  Per-level colours, stable across plots
# ----------------------------------------------------------

#' Base qualitative palette
#'
#' The colours levels are drawn from, in order.
#'
#' Chosen by farthest-point search in CIE Lab, so consecutive entries are as
#' far apart as the space allows and the palette degrades gracefully as more
#' levels appear. The previous palette held up to about eight levels and then
#' collapsed: `#76B7B2` and `#86BCB6` sat 4.1 dE apart, which is not a
#' difference anyone can see, and with a dozen countries on a plot two of
#' them were simply the same colour. This one holds 36.8 dE at sixteen levels
#' and 26.8 at twenty-eight.
#'
#' Lightness is capped below white and above black, and chroma below
#' fluorescent, so the colours stay readable on a white panel with black
#' outlines and do not look like a warning label in a working-group report.
#'
#' A caveat worth stating: this is optimised for normal colour vision.
#' Optimising simultaneously for deuteranopia and protanopia costs about half
#' the separation and pushes the palette into blues, greens and browns, which
#' is a poor trade when the reported problem is two colours being
#' indistinguishable to everyone. Where a figure must survive colour-blind
#' readers, prefer a facet or a shape over a colour.
#'
#' @keywords internal
#' @export
PAL_BASE <- c(
  "#4980B6", "#FFB649", "#00DB92", "#B60049", "#FF92FF", "#495B12",
  "#EDB6B6", "#5B37A4", "#00DBED", "#A4C849", "#924912", "#80495B",
  "#92B692", "#005B5B", "#FF6D5B", "#A41280", "#9292FF", "#128012",
  "#FF80A4", "#C8B6ED", "#B6925B", "#A49200", "#00926D", "#5B4980",
  "#92B6C8", "#005BB6", "#B66DB6", "#FFA480")

#' The outline every filled mark is drawn with
#'
#' One value, so a bar, a box, a bubble and a tile all carry the same edge.
#' Before this the plots used grey15, grey20 and grey25 interchangeably,
#' picked whenever each was written, which made the same data look subtly
#' heavier in one panel than another. Black also holds the fill apart from
#' the white panel at any lightness, which a grey outline stops doing once
#' the fill itself is dark.
#'
#' @export
OUTLINE <- "black"

#' Line weight for those outlines
#'
#' @export
OUTLINE_LW <- 0.25

#' Cache of colours already handed out, per variable
#'
#' A private environment, so a level keeps its colour for the whole session.
#'
#' @keywords internal
#' @noRd
.LEVEL_COLOURS <- new.env(parent = emptyenv())

#' Stable colours for the levels of a variable
#'
#' Returns a named colour vector for `values`, reusing the colours already
#' assigned to levels of `var` and extending the palette for any new level.
#' Because the assignment is remembered, a level keeps its colour across every
#' plot in the session.
#'
#' @param var Name of the variable being coloured.
#' @param values The levels to colour.
#' @return A character vector of colours, named by level.
#' @seealso [scale_fill_var()], [scale_colour_var()], [reset_palettes()]
#' @export
#' Colourblind-safe categorical palette (Okabe-Ito)
#'
#' The standard 8-colour palette recommended for red-green and
#' blue-yellow colour vision deficiencies, distinct even in greyscale.
#' An alternative to [PAL_BASE] for [var_palette()]'s `palette =
#' "colorblind"`.
#'
#' @keywords internal
#' @export
PAL_CB <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
           "#D55E00", "#CC79A7", "#000000")

var_palette <- function(var, values, palette = c("default", "colorblind")) {
  palette <- match.arg(palette)
  pal_src <- if (identical(palette, "colorblind")) PAL_CB else PAL_BASE
  # cached per (variable, palette) - not just per variable, or asking
  # for "colorblind" after "default" already ran for this variable in
  # this session returns the stale default colours instead of actually
  # switching
  cache_key <- paste0(var, "\r", palette)
  vals <- sort(unique(as.character(values[!is.na(values)])))
  if (!length(vals)) return(character(0))
  cur <- if (exists(cache_key, envir = .LEVEL_COLOURS))
    get(cache_key, envir = .LEVEL_COLOURS) else character(0)
  new <- setdiff(vals, names(cur))
  if (length(new)) {
    idx <- (length(cur) + seq_along(new) - 1L) %% length(pal_src) + 1L
    cur <- c(cur, setNames(pal_src[idx], new))
    assign(cache_key, cur, envir = .LEVEL_COLOURS)
  }
  cur
}

#' Assign colours to every level of a variable up front
#'
#' [var_palette()] hands colours out on demand, which keeps a level's colour
#' stable but makes the ASSIGNMENT depend on which plot was drawn first: open
#' the discard tab before the length tab and a country can end up a different
#' colour than the other way round. Nothing is wrong on screen - the colour
#' is still consistent across that session - but two runs of the same
#' analysis produce differently coloured figures, which is not a property a
#' report should have.
#'
#' Seeding from the full data at load time removes the ordering entirely:
#' every level present anywhere is coloured at once, in sorted order, so the
#' assignment depends only on the data. Levels that turn up later - a
#' derived column, a value only one plot computes - are still appended by
#' [var_palette()] and keep their colour from then on.
#'
#' Seeding REPLACES the variable\'s assignment rather than extending it, so
#' it must run when the data changes and not in the middle of a session.
#'
#' @param data A data frame.
#' @param vars The columns to seed; those absent are skipped.
#' @return Called for its side effect; invisibly the variables seeded.
#' @seealso [var_palette()], [reset_palettes()]
#' @export
seed_palettes <- function(data, vars = PALETTE_VARS) {
  if (is.null(data) || !nrow(data)) return(invisible(character(0)))
  done <- character(0)
  for (v in intersect(vars, names(data))) {
    vals <- sort(unique(as.character(data[[v]][!is.na(data[[v]])])))
    if (!length(vals)) next
    # ASSIGN, do not top up. Calling var_palette() here would keep whatever
    # colours a plot had already claimed and only fill in the rest, which
    # leaves the assignment depending on which tab was opened first - the
    # exact thing seeding exists to remove. Clearing the variable's entry
    # first makes the result a function of the data alone.
    idx <- (seq_along(vals) - 1L) %% length(PAL_BASE) + 1L
    assign(v, setNames(PAL_BASE[idx], vals), envir = .LEVEL_COLOURS)
    done <- c(done, v)
  }
  invisible(done)
}

#' Variables whose colours should be seeded from the whole data set
#'
#' The keys that appear as a fill or colour in more than one plot, and so
#' have to look the same in all of them.
#'
#' @export
PALETTE_VARS <- c("Country", "vesselFlagCountry", "Season", "seasonValue",
                  "metier4", "metier5", "metier6", "gear", "target", "mesh",
                  "FleetType", "catchCategory",
                  "ICESSubArea", "ICESDivision", "ICESSubdivision", "areaValue")

#' Forget every colour assignment
#'
#' Clears the cache, so the next plot starts the palette again. Useful between
#' unrelated stocks or in a test.
#'
#' @return Called for its side effect.
#' @seealso [var_palette()]
#' @export
reset_palettes <- function() rm(list = ls(.LEVEL_COLOURS), envir = .LEVEL_COLOURS)

#' Fill scale using the stable per-level colours
#'
#' @param var Name of the variable being coloured.
#' @param values The levels present.
#' @param name Legend title; defaults to `var`.
#' @param palette "default" or "colorblind" - passed to [var_palette()].
#' @param ... Passed to [ggplot2::scale_fill_manual()].
#' @return A ggplot fill scale.
#' @seealso [var_palette()]
#' @export
scale_fill_var <- function(var, values, name = var,
                           palette = c("default", "colorblind"), ...)
  scale_fill_manual(values = var_palette(var, values, match.arg(palette)),
                    name = name, na.value = "grey85", ...)

#' Colour scale using the stable per-level colours
#'
#' The `colour` counterpart of [scale_fill_var()], so a level drawn as a line
#' matches the same level drawn as a bar.
#'
#' @inheritParams scale_fill_var
#' @return A ggplot colour scale.
#' @seealso [var_palette()], [scale_fill_var()]
#' @export
scale_colour_var <- function(var, values, name = var,
                             palette = c("default", "colorblind"), ...)
  scale_colour_manual(values = var_palette(var, values, match.arg(palette)),
                      name = name, na.value = "grey70", ...)

#' Distinct colours for a set of group ids
#'
#' Group ids (`G1`, `G2`, ...) are arbitrary labels rather than data levels, so
#' they get their own palette rather than competing for the level colours.
#'
#' @param ids The group ids.
#' @return A character vector of colours, named by id.
#' @export
group_palette <- function(ids) {
  ids <- unique(ids[!is.na(ids)])
  n <- length(ids)
  if (!n) return(character(0))
  base <- PAL_BASE
  cols <- if (n <= length(base)) base[seq_len(n)]
          else grDevices::hcl.colors(n, "Dark 3")
  # the fallback pool is always grey
  cols[ids %in% c("ALL", "Catch All by All", "no raising needed", "(none)")] <- "#BFBFBF"
  setNames(cols, ids)
}

# ----------------------------------------------------------
#  The theme
# ----------------------------------------------------------

#' The shared plot theme
#'
#' One look for every plot in the workflow: bold title, muted grid, compact
#' legend text. Applied last so a builder can still override single elements.
#'
#' @param base Base font size in points.
#' @return A ggplot theme object.
#' @export
app_theme <- function(base = 11) {
  theme_bw(base_size = base) +
    theme(
      plot.title       = element_text(face = "bold", size = base + 2,
                                      margin = margin(b = 3)),
      plot.subtitle    = element_text(colour = "grey35", size = base - 1,
                                      margin = margin(b = 8)),
      plot.caption     = element_text(colour = "grey45", size = base - 3,
                                      hjust = 0),
      panel.border     = element_rect(colour = "grey20", fill = NA,
                                      linewidth = 0.7),
      panel.grid.major = element_line(colour = "grey92", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      axis.ticks       = element_line(colour = "grey35", linewidth = 0.35),
      axis.title       = element_text(size = base - 1, colour = "grey25"),
      # facet labels: no grey box, just a rule under the label
      strip.background = element_blank(),
      strip.text       = element_text(face = "bold", size = base - 1,
                                      colour = "grey15",
                                      margin = margin(t = 3, b = 4)),
      panel.spacing    = unit(0.6, "lines"),
      # Legend: a touch bigger than before, and sat inside the panel in
      # the bottom-right corner on a semi-opaque box rather than taking a
      # column of its own down the right-hand side. A builder that needs
      # it elsewhere (or gone) still overrides this - app_theme() is
      # applied before those local theme() calls.
      legend.key           = element_blank(),
      legend.key.size      = unit(1.1, "lines"),
      legend.title         = element_text(face = "bold", size = base - 1),
      legend.text          = element_text(size = base - 1),
      legend.background    = element_rect(fill = scales::alpha("white", 0.85),
                                          colour = "grey70", linewidth = 0.3),
      legend.margin        = margin(3, 5, 3, 5),
      legend.position      = c(0.995, 0.005),
      legend.justification = c(1, 0))
}
