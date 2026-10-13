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

#' Natural size order for `mesh` range strings
#'
#' `mesh` values are strings like `"16-31"`, `"<16"`, `">=120"`, `"0"`,
#' `"all"` - sorted as plain text, `"100-119"` comes before `"16-31"` and
#' `"all"` comes before `"90-99"`. This sorts by each range's own lower
#' bound (the upper bound only breaks a tie, e.g. `"90-99"` before
#' `"90-119"`): `"<N"` sits just below `N`, `">=N"` just above any closed
#' range that also starts at `N`, and `"all"` / `"All"` sort last. A value
#' matching none of those shapes sorts last too, rather than erroring - an
#' unrecognised mesh string should not stop a plot from drawing.
#'
#' @param x A character (or factor) vector of mesh values.
#' @return The distinct, non-`NA` values of `x`, in natural size order.
#' @seealso [reorder_mesh()], which applies this to a plot's own data.
#' @keywords internal
#' @noRd
#' @export
mesh_levels <- function(x) {
  u <- unique(as.character(x))
  u <- u[!is.na(u)]
  if (!length(u)) return(u)
  is_all <- tolower(trimws(u)) == "all"
  lo <- rep(Inf, length(u)); hi <- rep(Inf, length(u))
  rng <- regmatches(u, regexec("^\\s*([0-9.]+)\\s*-\\s*([0-9.]+)\\s*$", u))
  lt  <- regmatches(u, regexec("^\\s*<\\s*([0-9.]+)\\s*$", u))
  ge  <- regmatches(u, regexec("^\\s*>=\\s*([0-9.]+)\\s*$", u))
  num <- regmatches(u, regexec("^\\s*([0-9.]+)\\s*$", u))
  for (i in seq_along(u)) {
    if (is_all[i]) next
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
  u[order(lo, hi, u)]
}

#' Natural order for `metier4_mesh` values: gear first, then mesh size
#'
#' `metier4_mesh` values are a gear and a mesh range joined by an underscore
#' (`"TBB_70-99"`, `"OTB_>=120"`), or a bare gear when the fleet code had no
#' mesh part. Sorted as plain text they interleave the sizes the same way raw
#' `mesh` does (`"TBB_100-119"` before `"TBB_70-99"`), so this keeps each
#' gear together, in alphabetical order, and puts its meshes in the size
#' order [mesh_levels()] gives - a bare gear after all of its own meshes.
#'
#' @param x A character (or factor) vector of `metier4_mesh` values.
#' @return The distinct, non-`NA` values of `x`, gear by gear in natural
#'   mesh order.
#' @seealso [mesh_levels()], [reorder_mesh()]
#' @keywords internal
#' @noRd
#' @export
metier4_mesh_levels <- function(x) {
  u <- unique(as.character(x))
  u <- u[!is.na(u)]
  if (!length(u)) return(u)
  # metier4 is the first fleet part and never holds an underscore, so the
  # first underscore is the seam between gear and mesh
  gear <- sub("_.*$", "", u)
  mesh <- ifelse(grepl("_", u, fixed = TRUE), sub("^[^_]*_", "", u), "")
  # the mesh part of a bare gear is "": rank it after every real mesh, "all"
  # included, instead of wherever "" happens to sort alphabetically
  rank <- match(mesh, c(setdiff(mesh_levels(mesh), ""), ""))
  u[order(gear, rank, u)]
}

#' The natural level order for a column that carries mesh sizes
#'
#' One place that knows which columns have a size order - `mesh` and
#' `metier4_mesh` - so the plots do not each repeat the test.
#'
#' @param var A column name (or `NULL`).
#' @param x The column's values.
#' @return The levels in natural order, or `NULL` when `var` is neither.
#' @seealso [mesh_levels()], [metier4_mesh_levels()]
#' @keywords internal
#' @noRd
#' @export
mesh_var_levels <- function(var, x) {
  if (identical(var, "mesh")) mesh_levels(x)
  else if (identical(var, "metier4_mesh")) metier4_mesh_levels(x)
  else NULL
}

#' Relevel a plot's mesh columns into size order, if it carries them
#'
#' Every rcef plot that can take `mesh` or `metier4_mesh` on an axis, a fill,
#' a colour or a facet calls this once its data frame is final, so they read
#' in the same size order ([mesh_levels()], [metier4_mesh_levels()]) wherever
#' they turn up - a no-op unless one of `vars` is literally one of those two
#' names and `d` actually carries that column.
#'
#' @param d A data frame.
#' @param vars Column names the plot is about to use (`x`, `y`, `fill`,
#'   `colour`, `shape`, `facet`, ... - `NULL` entries are fine).
#' @return `d`, with `d$mesh` and `d$metier4_mesh` releveled when named in
#'   `vars`.
#' @seealso [mesh_levels()], [metier4_mesh_levels()]
#' @keywords internal
#' @noRd
#' @export
reorder_mesh <- function(d, vars) {
  vars <- vars[!is.na(vars) & nzchar(vars)]
  for (v in intersect(c("mesh", "metier4_mesh"), intersect(vars, names(d))))
    d[[v]] <- factor(d[[v]], levels = mesh_var_levels(v, d[[v]]))
  d
}

#' Make a plot's numeric category columns discrete
#'
#' `seasonValue` is an integer quarter code in the CEF data, not a character
#' column. Mapped straight onto a fill, a shape or an axis it is read as a
#' continuous variable: a fill or shape scale then errors outright, and an
#' axis grows breaks at 1.5 and 2.5. Every column a plot is about to use as
#' a category is a category, whatever type the file stored it as, so this
#' turns the numeric ones into factors - in numeric order, `2` before `10` -
#' once, on the plot's own data, before any aesthetic is built from it. A
#' no-op for a column that is already character or factor.
#'
#' @param d A data frame.
#' @param vars Column names the plot is about to use as categories (`x`,
#'   `y`, `fill`, `colour`, `shape`, `facet`, ... - `NULL` entries and names
#'   `d` does not carry are fine).
#' @return `d`, with each numeric column named in `vars` as a factor.
#' @seealso [reorder_mesh()]
#' @keywords internal
#' @noRd
#' @export
discrete_cols <- function(d, vars) {
  vars <- vars[!is.na(vars) & nzchar(vars)]
  for (v in intersect(vars, names(d)))
    if (is.numeric(d[[v]])) d[[v]] <- factor(d[[v]])
  d
}

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

#' The weight a plot is about, in words
#'
#' @param weight_label `"landings"`, `"discards"`, `"catch"` or `"BMS catch"`
#'   (what [default_weight_label()] returns).
#' @return A list: `adj` (`"landed"`, `"discarded"`, `"catch"`, `"BMS
#'   catch"`) and `noun` (`"landed weight"`, ...).
#' @keywords internal
#' @noRd
#' @export
weight_words <- function(weight_label) {
  adj <- switch(weight_label, landings = "landed", discards = "discarded",
                weight_label)
  list(adj = adj, noun = paste(adj, "weight"))
}

#' Title of a coverage plot: which weight, sampling what
#'
#' "Landings - Sampling of Length", "Catch - Sampling of Age by areaValue",
#' "Sampling of Discards": the weight a figure is judged over first (dropped
#' entirely for `what = "discard"`, where it is always the landings, so
#' naming it would only repeat itself), then what is being sampled.
#'
#' @param what `"discard"`, `"length"` or `"age"`.
#' @param weight_label As for [weight_words()].
#' @param by Optional columns the figure is split by, named at the end.
#' @keywords internal
#' @noRd
#' @export
cov_title <- function(what, weight_label, by = NULL) {
  by_txt <- if (length(by)) paste0(" by ", paste(by, collapse = " and ")) else ""
  if (identical(what, "discard")) return(paste0("Sampling of Discards", by_txt))
  paste0(tools::toTitleCase(weight_label), " - Sampling of ",
         cov_title_what(what), by_txt)
}

#' The four stratum families and their column levels, finest first
#'
#' Shared by [strata_sentence()] and [finest_per_family()]: fleet
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

#' Join strings with a trailing "and", Oxford comma for three or more
#'
#' Distinct from [and_list()] (used elsewhere for the cascade step
#' descriptions), which does not use an Oxford comma - the grouping
#' sentence's own wording asks for one ("fleet, area, country, and season").
#'
#' @param x A character vector.
#' @return `"a"`, `"a and b"` or `"a, b, and c"`.
#' @keywords internal
#' @noRd
#' @export
oxford_list <- function(x) {
  n <- length(x)
  if (n <= 2) paste(x, collapse = " and ")
  else paste0(paste(x[-n], collapse = ", "), ", and ", x[n])
}

#' Drop a column when a strictly finer member of its family is also present
#'
#' Used where several columns of the same family - `metier4` alongside
#' `fleetValue`, say - would otherwise repeat the same information: `metier4`
#' is a coarser grouping `fleetValue` always determines, so naming both says
#' nothing that `fleetValue` alone does not already say. Columns outside
#' [strata_families()] pass through unchanged.
#'
#' @param vars A character vector of column names.
#' @return `vars`, minus any column strictly coarser than another member of
#'   the same family that is also present. Ties (two columns at the same
#'   level) are both kept.
#' @keywords internal
#' @noRd
#' @export
finest_per_family <- function(vars) {
  vars <- unique(as.character(vars))
  drop <- unlist(lapply(strata_families(), function(m) {
    s <- intersect(vars, names(m))
    if (length(s) > 1) s[m[s] > min(m[s])]
  }))
  setdiff(vars, drop)
}

#' What "% covered" means for a coverage plot, as a short phrase
#'
#' @param what `"discard"`, `"length"` or `"age"`.
#' @param weight_label As for [weight_words()].
#' @return e.g. `"share of landed weight with discard estimates"`.
#' @keywords internal
#' @noRd
#' @export
share_phrase <- function(what, weight_label)
  sprintf("share of %s with %s", weight_words(weight_label)$noun,
          switch(what, length = "length distributions", age = "age distributions",
                 "discard estimates"))

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
  paste0("grouping = ", paste(vars, collapse = " x "), ", grouped over ",
         oxford_list(grouped), ".")
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

#' Threshold-box text lines for a coverage heatmap or bubble grid
#'
#' One line per active threshold, each stating what it means as well as its
#' value, plus a closing line about the per-cell shortfall labels. Meant for
#' [thr_guide()], not the ordinary caption - see [coverage-plots].
#'
#' @param threshold Minimum coverage, in percent.
#' @param min_trips,min_meas Minimum trips / measured fish, or `NULL` when
#'   that question is not part of the classification.
#' @param what `"discard"`, `"length"` or `"age"`.
#' @param weight_label As for [weight_words()].
#' @return A character vector, one threshold definition per element.
#' @keywords internal
#' @noRd
#' @export
thr_lines <- function(threshold, min_trips = NULL, min_meas = NULL,
                      what = "discard", weight_label = "landings") {
  behind <- switch(what, length = "length distributions", age = "age distributions",
                   "discard estimates")
  c(sprintf("coverage >= %s%%: %s.", threshold, share_phrase(what, weight_label)),
    if (thr_active(min_trips))
      sprintf("trips >= %s: sampled trips behind the %s, summed across strata.",
              min_trips, behind),
    if (thr_active(min_meas) && what %in% c("length", "age"))
      sprintf("measured fish >= %s: fish measured for %s estimates, summed across strata.",
              format(min_meas, big.mark = ",", trim = TRUE),
              if (identical(what, "age")) "NaA/MWaA" else "NaL/MWaL"),
    "labels: each unmet threshold is shown as a label.")
}

#' A boxed "Thresholds" guide, positioned under the panel like a legend
#'
#' @description
#' Draws [thr_lines()]' text as its own guide, via [ggplot2::guide_custom()],
#' so it keeps the plot a plain ggplot object (no composite/patchwork return)
#' while reading as a bordered box below the panel, right-justified under
#' where [app_theme()] already puts the in-panel status legend - a detail
#' section of it, not a caption about the plot itself.
#'
#' @param lines Character vector, one fact per line - typically [thr_lines()].
#' @param size Text size, in points.
#' @return A list of two ggplot layers (`guides()` + `theme()`) to add with
#'   `+`, after [app_theme()] (a complete theme resets `legend.justification`).
#' @seealso [thr_lines()]
#' @keywords internal
#' @noRd
#' @export
thr_guide <- function(lines, size = 8) {
  tg <- grid::textGrob(paste(lines, collapse = "\n"), x = 0, y = 1,
                       hjust = 0, vjust = 1,
                       gp = grid::gpar(fontsize = size, col = "grey25",
                                       lineheight = 1.1))
  list(guides(thresholds = guide_custom(
         tg, title = "Thresholds", position = "bottom", order = 99,
         theme = theme(legend.title = element_text(face = "bold", size = size)))),
       theme(legend.justification.bottom = "right"))
}

#' Per-cell "which threshold did this miss" label for a coverage plot
#'
#' Reads the shortfall directly off [classify_strata()]'s own `cat_key`
#' (`warn_cov` / `warn_trip` / `warn_meas`, singly or underscore-joined when
#' several are missed) rather than re-deriving it from the raw values, so the
#' label always agrees with the colour. Blank for every row that is not a
#' warning category.
#'
#' @param d A classified coverage data frame (as `plot_coverage_heat()` /
#'   `plot_coverage_bubble()` build it): needs `cat_key`, `coverage`, and
#'   (when active) `n_measurements` / `numTrips`.
#' @param sep Joins a cell's shortfalls when it misses more than one
#'   threshold - `"\n"` for the bubble grid (repelled text), `", "` for the
#'   heatmap (one line under the tile number).
#' @return A character vector, aligned to `d`, `""` for a non-warning row.
#' @keywords internal
#' @noRd
#' @export
shortfall_label <- function(d, sep = "\n") {
  k <- as.character(d$cat_key); n <- nrow(d)
  miss <- function(t) grepl(paste0("^warn_.*", t), k)
  cov <- ifelse(miss("cov"), paste0(round(d$coverage, 1), "%"), "")
  mea <- if ("n_measurements" %in% names(d))
    ifelse(miss("meas"), paste0("n=", format(round(d$n_measurements),
                                             big.mark = ",", trim = TRUE)), "")
  else rep("", n)
  trp <- if (!("numTrips" %in% names(d))) ifelse(miss("trip"), "trips n/a", "")
         else ifelse(miss("trip"),
                    ifelse(is.na(d$numTrips), "trips n/a",
                           paste0(round(d$numTrips), " trip",
                                  ifelse(round(d$numTrips) == 1, "", "s"))), "")
  vapply(seq_len(n), function(i) {
    b <- c(cov[i], mea[i], trp[i])
    paste(b[nzchar(b)], collapse = sep)
  }, character(1))
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

#' Standalone legend for the sampling statuses
#'
#' @description
#' A minimal plot whose only purpose is its legend: every status a coverage
#' plot can show, with its colour and compact label, whether or not the
#' figure in front of you happens to contain it.
#'
#' @details
#' The legend is built from [cat_lookup()] rather than from data, so it
#' carries a filled key for every possible status, including those absent
#' from the plot being shown. Drawing the legend from this plot (rather than
#' from the coverage plot) is what makes the full scale visible: a discrete
#' scale can list an unused level, but there is no data row to draw that
#' level's swatch from.
#'
#' [plot_coverage_heat()] and [plot_coverage_bubble()] attach the arguments
#' needed to rebuild their legend as a `"status_legend"` attribute, so a
#' caller that shows the legend in its own panel (the app does) can build it
#' on demand with `do.call(status_legend_plot, attr(p, "status_legend"))`.
#'
#' @inheritParams cat_lookup
#' @param key Glyph the status keys are drawn with: `"tile"` for the heatmap,
#'   `"point"` for the bubble plot. The legend should look like the thing it
#'   describes, so a heatmap gets square swatches and a bubble plot circles.
#' @param size Optional specification of a second, independent SIZE key, as
#'   attached by [plot_coverage_bubble()]: a list with `breaks`, `labels`,
#'   `limits`, `range` and `name`. It is built with the same limits and range
#'   as the plot it came from, so a key circle is drawn at exactly the
#'   diameter that value has in the plot and can be read off it.
#' @param drop_none Drop the "none" (no weight at all) category, which the
#'   coverage plots never draw. Default `TRUE`.
#' @return A ggplot object; only its legend is meant to be used.
#' @seealso [coverage-plots], which draw the statuses; [cat_lookup()], which
#'   defines them.
#' @family coverage plots
#' @md
#' @examples
#' \dontrun{
#' # every status for a 50% coverage bar with no trip or measurement floor
#' status_legend_plot(50, "discard", min_trips = NULL)
#'
#' # the legend of a figure that is on screen
#' p <- plot_coverage_heat(catch, y = "metier4", x = "vesselFlagCountry",
#'                         min_cov = 50, min_trip = 2)
#' do.call(status_legend_plot, attr(p, "status_legend"))
#' }
#' @export
#' @noRd
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
#' @noRd
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

#' @rdname ices_area_centroid
#' @format `ICES_AREA_CENTROID` is a data frame with one row per area and the
#'   columns `area` (the code, without the leading `"27."`), `lon` and `lat`
#'   (decimal degrees).
#' @keywords internal
#' @md
#' @export
#' @noRd
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

#' Approximate centroids of ICES areas
#'
#' @description
#' `ICES_AREA_CENTROID` is a table of approximate `lon` / `lat` centroids for
#' the ICES divisions and subdivisions a stock assessment in the NE Atlantic
#' normally uses - enough to place one pie or bubble per area on a map. Codes
#' are stored without the leading `"27."`.
#'
#' `ices_area_centroid()` looks codes up in that table, with or without the
#' prefix.
#'
#' @param codes ICES area codes, with or without the `"27."` prefix.
#'   Anything past a recognised division or subdivision is ignored.
#' @return `ices_area_centroid()` returns a data frame with `area` (the codes
#'   as given), `lon` and `lat`, aligned to `codes`. `lon` and `lat` are `NA`
#'   for a code with no centroid, and a warning names those codes.
#' @seealso [plot_coverage_map()], which uses it.
#' @keywords internal
#' @md
#' @examples
#' ices_area_centroid(c("27.4.b", "7.d", "27.3.a.20"))
#' head(ICES_AREA_CENTROID)
#' @export
#' @noRd
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
#' The word a plot title uses for the weight it describes.
#'
#' @param what `"discard"`, `"length"` or `"age"`.
#' @param catchCategory `"Lan"`, `"Dis"`, `"BMS"`, `"All"` or `NULL`
#'   (treated as `"Lan"`). Ignored for `what = "discard"`.
#' @return `"landings"` (always, for discards), `"discards"`, `"BMS catch"`
#'   or `"catch"` (landings and discards together).
#' @keywords internal
#' @examples
#' default_weight_label("discard")
#' default_weight_label("length", "Dis")
#' default_weight_label("age", "All")
#' @export
#' @noRd
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
#' @noRd
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
#' @seealso [prep_distributions()], [plot_NaLoA()]
#' @keywords internal
#' @export
#' @noRd
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
#' The shared front half of [plot_NaLoA()] and [plot_MWaLoA()]: when
#' `distributions` is given, run
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
#' @param plus_from Optional: the class (an age) from which everything is
#'   pooled into one plus class (`pool_plus_group()`).
#' @param bin_from Optional: a bin width (for lengths) to pool every class
#'   into (`pool_length_bins()`).
#' @return `list(data, class_label, value_label)`.
#' @keywords internal
#' @export
#' @noRd
biology_dist_prep <- function(data, distributions, distributionType,
                              catchCategory, quantity, grp, dims,
                              plus_from = NULL, bin_from = NULL) {
  lab <- biology_axis_labels(distributionType, quantity)
  if (!is.null(distributions)) {
    if (is.null(dims))
      dims <- unique(c("catchCategory", "fleetValue", "vesselFlagCountry",
                       "seasonValue", grp))
    dims <- intersect(dims, names(as.data.frame(data)))
    linked <- link_dist_to_strata(data, distributions, dims, catchCategory,
                                  type = distributionType)
    if (!is.null(plus_from)) linked <- pool_plus_group(linked, plus_from, dims)
    if (!is.null(bin_from)) linked <- pool_length_bins(linked, bin_from, dims)
    data <- if (identical(quantity, "numbers"))
      length_numbers(linked, grp) else length_meanweight(linked, grp)
    if (!is.null(data) && nrow(data) && identical(quantity, "numbers"))
      data$number <- data$number * lab$value_scale
  }
  list(data = data, class_label = lab$class_label, value_label = lab$value_label)
}

#' Continuous x scale for a pooled length/age class axis
#'
#' The plus class reads "N+"; a length axis binned by [pool_length_bins()]
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