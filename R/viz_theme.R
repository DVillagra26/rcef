# =====================================================================
#  viz_theme.R   (plot_colours(), app_theme())
#
#  ORIGIN: this project (ILVO / RCEF Workbench). Freely refactorable.
#  Was: R_development/0_Functions_theme.R
# =====================================================================

# ============================================================
#  The shared look: one theme, one function for every colour.
# ------------------------------------------------------------
#  Plotting-support module. It is ggplot2 only, and viz_plots.R depends on it.
#
#  WHY IT EXISTS
#  A level must keep its colour everywhere it appears: if "TBB" is orange in
#  the coverage bars it must be orange in the discard-rate plot and in the
#  river plot too, or the reader compares the wrong things. plot_colours()
#  therefore remembers the colour it assigned to each level of each variable
#  in a private environment, and hands the same one back next time. Every
#  plot takes its colours from it, and every plot has the same `palette`
#  argument, so one choice recolours a whole report.
#
#  DEPENDENCIES  Imports: ggplot2, scales.
# ============================================================

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
#' @noRd
OUTLINE <- "black"

#' Line weight for those outlines
#'
#' @export
#' @noRd
OUTLINE_LW <- 0.25

#' Cache of colours already handed out, per variable and palette
#'
#' A private environment, so a level keeps its colour for the whole session.
#'
#' @keywords internal
#' @noRd
.LEVEL_COLOURS <- new.env(parent = emptyenv())

#' The colours of a plot: stable per level, or per sampling status
#'
#' @description
#' The one place every rcef plot takes its colours from. Three palettes, the
#' same `palette` argument on every plot:
#'
#' * `"default"`: 28 colours picked by a farthest-point search in CIE Lab, so
#'   consecutive entries are as far apart as the space allows and the palette
#'   degrades gracefully as more levels appear (16 levels still sit 36.8 dE
#'   apart, 28 sit 26.8). Lightness is capped below white and above black,
#'   and chroma below fluorescent, so the colours stay readable on a white
#'   panel with black outlines. It is optimised for normal colour vision:
#'   optimising for deuteranopia and protanopia as well costs about half the
#'   separation, which is why the next palette exists.
#' * `"colorblind"`: the 8-colour Okabe-Ito palette, distinct for red-green and
#'   blue-yellow colour vision deficiencies and in greyscale.
#' * `"gradual"`: ColorBrewer's 12-colour "Paired" palette, six hues each a
#'   light and a dark step next to each other. Its steps read as a progression,
#'   so it suits a variable whose levels have a real order (mesh size bins,
#'   length or age classes grouped coarser): the colours follow the order of
#'   a factor's levels.
#'
#' With `type = "levels"` (the default) it colours the levels of a variable.
#' The assignment is remembered per variable and palette, so a level keeps its
#' colour across every plot of the session; levels beyond the palette's own
#' colours take further, distinct ones. With `type = "status"` it colours the
#' sampling statuses of a coverage plot (`full`, `safe`, `warn_*`, `no_dist`,
#' `disc_only`, `none`): a fixed scale per palette, best sampled to worst, the
#' colour of a warning telling how many thresholds were missed.
#'
#' @param values The levels (or, for `type = "status"`, the status keys) to
#'   colour. `NULL` returns the palette's first colour, for a plot with
#'   nothing to tell apart.
#' @param var Name of the variable being coloured; levels keep their colour
#'   per variable. `NULL`: nothing is remembered.
#' @param palette `"default"`, `"colorblind"` or `"gradual"`.
#' @param type `"levels"` (default) or `"status"`.
#' @return A character vector of colours, named by level (a single unnamed
#'   colour when `values` is `NULL`).
#' @keywords internal
#' @export
#' @noRd
plot_colours <- function(values = NULL, var = NULL,
                         palette = c("default", "colorblind", "gradual"),
                         type = c("levels", "status")) {
  palette <- match.arg(palette)
  type <- match.arg(type)
  if (identical(type, "status")) {
    # full / safe, then yellow -> orange -> dark orange for 1, 2, 3 missed
    # thresholds, one reserved colour for "nothing to judge" (no estimate or
    # no distribution), one for "discards only", near-white for "no catch"
    ramp <- switch(palette,
      colorblind = c(full = "#0072B2", safe = "#56B4E9", w1 = "#F0E442",
                     w2 = "#E69F00", w3 = "#D55E00", no_dist = "#CC79A7",
                     disc_only = "#009E73", none = "#F0F1F1"),
      gradual    = c(full = "#440154", safe = "#3B528B", w1 = "#21908C",
                     w2 = "#5DC863", w3 = "#FDE725", no_dist = "#D7191C",
                     disc_only = "#7F7F7F", none = "#F0F1F1"),
      c(full = "#00441B", safe = "#41AE76", w1 = "#FFD92F", w2 = "#F16913",
        w3 = "#7F2704", no_dist = "#D7191C", disc_only = "#4292C6",
        none = "#F0F1F1"))
    role <- ifelse(grepl("^warn_", values),
                   paste0("w", lengths(strsplit(sub("^warn_", "", values), "_"))),
                   values)
    return(setNames(unname(ramp[role]), values))
  }
  pal <- switch(palette,
    colorblind = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
                   "#D55E00", "#CC79A7", "#000000"),
    gradual    = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99",
                   "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A",
                   "#FFFF99", "#B15928"),
    c("#4980B6", "#FFB649", "#00DB92", "#B60049", "#FF92FF", "#495B12",
      "#EDB6B6", "#5B37A4", "#00DBED", "#A4C849", "#924912", "#80495B",
      "#92B692", "#005B5B", "#FF6D5B", "#A41280", "#9292FF", "#128012",
      "#FF80A4", "#C8B6ED", "#B6925B", "#A49200", "#00926D", "#5B4980",
      "#92B6C8", "#005BB6", "#B66DB6", "#FFA480"))
  if (is.null(values)) return(pal[[1]])
  vals <- if (is.factor(values)) levels(droplevels(values))
          else sort(unique(as.character(values[!is.na(values)])))
  if (!length(vals)) return(character(0))
  # beyond the palette's own colours, a fixed run of further distinct ones
  # rather than starting over (two levels in one colour read as one level)
  src <- c(pal, grDevices::hcl.colors(60, "Dark 3"))
  # cached per (variable, palette) - not just per variable, or asking for
  # "colorblind" after "default" already ran for this variable in this
  # session would return the stale default colours instead of switching
  key <- if (!is.null(var)) paste0(var, "\r", palette)
  cur <- if (!is.null(key) && exists(key, envir = .LEVEL_COLOURS, inherits = FALSE))
    get(key, envir = .LEVEL_COLOURS) else character(0)
  new <- setdiff(vals, names(cur))
  if (length(new)) {
    idx <- (length(cur) + seq_along(new) - 1L) %% length(src) + 1L
    cur <- c(cur, setNames(src[idx], new))
    if (!is.null(key)) assign(key, cur, envir = .LEVEL_COLOURS)
  }
  cur[vals]
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
#' @noRd
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
