## One palette for every plot: `palette = "default"`, `"colorblind"` or `"gradual"`.
##
##  1. Every plot function takes the same `palette` argument, "default" first.
##  2. Every plot draws only colours of the palette it was asked for - the
##     bars, points, lines, ribbons, tiles, bubbles, pies, rivers and
##     clusters alike - and a palette changes them.
##  3. A level keeps its colour across plots (and across calls): the same
##     country is the same colour in the bars, the discard-rate points and the
##     mean-weight lines.
##  4. "gradual" follows the order of a variable's levels (mesh sizes), so the
##     colours read as a progression.
##  5. More levels than the palette has colours do not start over: no two
##     levels share a colour.
##  6. The sampling status of the heatmap and the bubbles follows the palette
##     too, from best sampled to worst.

source("dev/tests/fixture.R")
FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}
err <- function(expr) tryCatch({ force(expr); "no error" },
                               error = function(e) conditionMessage(e))
quiet <- function(expr) {
  out <- NULL
  invisible(utils::capture.output(out <- suppressMessages(suppressWarnings(expr))))
  out
}
suppressPackageStartupMessages(library(ggplot2))

## the three palettes, as documented: 28 well-separated colours, Okabe-Ito, and
## ColorBrewer's "Paired" - and the status scale of each, best to worst
PAL <- list(
  default = c("#4980B6", "#FFB649", "#00DB92", "#B60049", "#FF92FF", "#495B12",
              "#EDB6B6", "#5B37A4", "#00DBED", "#A4C849", "#924912", "#80495B",
              "#92B692", "#005B5B", "#FF6D5B", "#A41280", "#9292FF", "#128012",
              "#FF80A4", "#C8B6ED", "#B6925B", "#A49200", "#00926D", "#5B4980",
              "#92B6C8", "#005BB6", "#B66DB6", "#FFA480"),
  colorblind = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
                 "#D55E00", "#CC79A7", "#000000"),
  gradual = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C",
              "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928"))
STATUS <- list(
  default = c("#00441B", "#41AE76", "#FFD92F", "#F16913", "#7F2704", "#D7191C",
              "#4292C6", "#F0F1F1"),
  colorblind = c("#0072B2", "#56B4E9", "#F0E442", "#E69F00", "#D55E00", "#CC79A7",
                 "#009E73", "#F0F1F1"),
  gradual = c("#440154", "#3B528B", "#21908C", "#5DC863", "#FDE725", "#D7191C",
              "#7F7F7F", "#F0F1F1"))
ok(all(vapply(c(PAL, STATUS), function(p) all(grepl("^#[0-9A-Fa-f]{6}$", p)) && !anyDuplicated(p), NA)),
   "the palettes in this test are valid hex colours, none repeated within a palette")

## ---- the plots to draw ---------------------------------------------------------
cd <- fx_catch(); dd <- fx_dist()
c23 <- subset(cd, year == 2023); d23 <- subset(dd, year == 2023)
lf <- tempfile(fileext = ".txt")
raised  <- quiet(fx_raise(c23, fx_groups("own"),   type = "discards", log_file = lf))
raised2 <- quiet(fx_raise(c23, fx_groups("mixed"), type = "discards", log_file = lf))
alloc   <- quiet(fx_allocate(raised, d23, "LengthTotal", log_file = lf))
dc      <- quiet(build_discard_clusters(c23, dims = c("metier4", "ICESDivision", "seasonValue"),
                                        k = 3, seed = 7))
plots <- list(
  plot_coverage_grid     = function(pal) plot_coverage_grid(cd, y = "metier4", x = "vesselFlagCountry", min_trip = 2, palette = pal),
  plot_coverage_grid_bubble = function(pal) plot_coverage_grid(type = "bubble", cd, y = "metier4", x = "vesselFlagCountry", min_trip = 2, palette = pal),
  plot_coverage_bar      = function(pal) plot_coverage_bar(cd, y = "metier4", fill = "vesselFlagCountry", palette = pal),
  plot_DR                = function(pal) plot_DR(cd, x = "metier4", colour = "vesselFlagCountry", palette = pal),
  plot_NaLoA             = function(pal) plot_NaLoA(cd, dd, fill = "vesselFlagCountry", palette = pal),
  plot_NaLoA_one_colour  = function(pal) plot_NaLoA(cd, dd, palette = pal),
  plot_MWaLoA            = function(pal) plot_MWaLoA(cd, dd, colour = "vesselFlagCountry", palette = pal),
  plot_MWaLoA_one_colour = function(pal) plot_MWaLoA(cd, dd, palette = pal),
  plot_catches           = function(pal) plot_catches(cd, x = "metier4", palette = pal),
  plot_catches_raised    = function(pal) plot_catches(raised, x = "metier4", palette = pal),
  plot_catches_by        = function(pal) plot_catches(raised, x = "metier4", fill = "vesselFlagCountry", palette = pal),
  plot_DR_raised         = function(pal) plot_DR(raised, x = "metier4", colour = "vesselFlagCountry", palette = pal),
  plot_compare_raised    = function(pal) plot_compare_raised(c(raised, raised2), x = "metier4", palette = pal),
  plot_NaLoA_allocated   = function(pal) plot_NaLoA(alloc$catch, alloc$distribution, catchCategory = "Lan", fill = "vesselFlagCountry", palette = pal),
  plot_MWaLoA_allocated  = function(pal) plot_MWaLoA(alloc$catch, alloc$distribution, catchCategory = "Lan", colour = "vesselFlagCountry", palette = pal),
  plot_river             = function(pal) suppressWarnings(plot_river(raised, left_var = "metier4", right_var = "DrGroup", value_var = "total", palette = pal)),
  plot_discard_clusters  = function(pal) plot_discard_clusters(dc, palette = pal),
  plot_discard_cluster_variables = function(pal) plot_discard_cluster_variables(dc, palette = pal))
if (requireNamespace("maps", quietly = TRUE))
  plots$plot_coverage_map <- function(pal) suppressWarnings(plot_coverage_map(cd, palette = pal))

## ---- 1. the same argument on every plot -----------------------------------------
exported <- c("plot_coverage_map", "plot_coverage_grid", "plot_coverage_bar",
              "plot_DR", "plot_NaLoA", "plot_MWaLoA", "plot_catches",
              "plot_compare_raised", "plot_river",
              "plot_discard_clusters", "plot_discard_cluster_variables")
for (f in exported)
  ok(identical(eval(formals(get(f))$palette), c("default", "colorblind", "gradual")),
     sprintf("%s(): palette = c(\"default\", \"colorblind\", \"gradual\")", f))
ok(!"pal" %in% names(formals(plot_river)), "plot_river(): `pal` is gone - it is `palette` like everywhere else")
ok(grepl("should be one of", err(plot_coverage_bar(cd, y = "metier4", palette = "nope"))) &&
     grepl("should be one of", err(plot_river(raised, left_var = "metier4", right_var = "DrGroup",
                                              value_var = "total", palette = "nope"))),
   "an unknown palette is rejected, by every plot alike")

## ---- 2. every plot draws only its palette ---------------------------------------
## the hex colours a figure actually draws (names like "black" or "red3" are the
## outline, the labels and the reference lines, not the palette)
drawn <- function(p) {
  b <- suppressWarnings(ggplot_build(p))$data     # the river warns, when it is built, that group and stratum names coincide
  v <- unlist(lapply(b, function(l) c(if ("fill" %in% names(l)) as.character(l$fill),
                                      if ("colour" %in% names(l)) as.character(l$colour))))
  unique(toupper(v[!is.na(v) & grepl("^#[0-9A-Fa-f]{6}$", v)]))
}
for (nm in names(plots)) {
  sets <- lapply(c("default", "colorblind", "gradual"), function(pal) drawn(plots[[nm]](pal)))
  names(sets) <- c("default", "colorblind", "gradual")
  # the status plots use the status scale, the map only its sampled slice
  allowed <- if (grepl("^plot_coverage_grid", nm)) STATUS
             else if (nm == "plot_coverage_map") lapply(STATUS, function(s) s[2])
             else PAL
  inside <- vapply(names(sets), function(pal)
    length(sets[[pal]]) > 0 && all(sets[[pal]] %in% toupper(allowed[[pal]])), NA)
  ok(all(inside), sprintf("%s(): draws only colours of the palette it was asked for", nm))
  ok(!identical(sets$default, sets$colorblind) && !identical(sets$default, sets$gradual) &&
       !identical(sets$colorblind, sets$gradual),
     sprintf("%s(): and each palette draws it differently", nm))
}

## ---- 3. a level keeps its colour -------------------------------------------------
level_colours <- function(p, aes) { s <- p$scales$get_scales(aes); s$palette(1) }
bar <- level_colours(plots$plot_coverage_bar("default"), "fill")
dr  <- level_colours(plots$plot_DR("default"), "fill")
mw  <- level_colours(plots$plot_MWaLoA("default"), "colour")
na  <- level_colours(plots$plot_NaLoA("default"), "fill")
common <- Reduce(intersect, list(names(bar), names(dr), names(mw), names(na)))
ok(length(common) > 3 &&
     identical(bar[common], dr[common]) && identical(bar[common], mw[common]) &&
     identical(bar[common], na[common]),
   "the same country is the same colour in the bars, the discard-rate points and the length plots")
bar_cb <- level_colours(plots$plot_coverage_bar("colorblind"), "fill")
ok(!identical(unname(bar[common]), unname(bar_cb[common])) &&
     identical(bar_cb[common], level_colours(plots$plot_DR("colorblind"), "fill")[common]),
   "... and under each palette it keeps its own colour for that palette")
ok(identical(level_colours(plots$plot_coverage_bar("default"), "fill"), bar),
   "drawing the same plot again gives the same colours")
## the imported / raised and sampled / estimated pairs are colours of the palette too
rc <- level_colours(plot_DR(raised, x = "metier4", colour = "importedOrRaised"), "fill")
ok(identical(unname(rc), PAL$default[1:2]) && identical(names(rc), c("imported", "raised")),
   "imported / raised: the first two colours, imported first")
al <- level_colours(plot_NaLoA(alloc$catch, alloc$distribution, catchCategory = "Lan", fill = "sampledOrEstimated"), "fill")
ok(identical(unname(al), PAL$default[1:2]) && identical(names(al), c("sampled", "estimated")),
   "sampled / estimated: the first two colours, sampled first")
rc_all <- level_colours(plot_catches(raised, x = "metier4"), "fill")
rc_lan <- level_colours(plot_catches(raised, x = "metier4", catchCategory = "Lan"), "fill")
ok(identical(unname(rc_all[c("Discards", "Landings")]), PAL$default[1:2]) &&
     identical(names(rc_all), c("Discards", "Landings", "BMS")) &&
     identical(unname(rc_all[["BMS"]]), PAL$default[3]) &&
     identical(rc_lan[["Landings"]], rc_all[["Landings"]]),
   "discards and landings are the first two colours, BMS the third, and landings keep theirs when only they are drawn")

## ---- 4. "gradual" follows the order of the levels --------------------------------
mesh_vals <- c("0", "16-31", "32-69", "70-99", "90-99", "100-119", ">=120")
m23 <- c23
m23$mesh <- mesh_vals[(as.integer(factor(m23$fleetValue)) - 1) %% length(mesh_vals) + 1]
pg <- level_colours(plot_coverage_bar(m23, y = "metier4", fill = "mesh", palette = "gradual"), "fill")
ok(length(pg) >= 3 && identical(names(pg), mesh_vals[mesh_vals %in% names(pg)]) &&
     identical(unname(pg), PAL$gradual[seq_along(pg)]),
   "mesh sizes in size order get the gradual colours in order - not in alphabetical order")
ok(all(vapply(c("default", "colorblind", "gradual"), function(p)
       drawn_ok <- inherits(try(ggplot_build(plot_coverage_bar(m23, y = "metier4", fill = "mesh", palette = p)),
                                silent = TRUE), "ggplot_built"), NA)),
   "mesh draws with every palette")
## plain text levels are sorted, as they always were
letters_fill <- level_colours(plot_coverage_bar(transform(c23, grp = c("c", "a", "b")[1 + seq_len(nrow(c23)) %% 3]),
                                                y = "metier4", fill = "grp", palette = "gradual"), "fill")
ok(identical(names(letters_fill), c("a", "b", "c")) && identical(unname(letters_fill), PAL$gradual[1:3]),
   "text levels are coloured in alphabetical order")

## ---- 5. more levels than colours --------------------------------------------------
many <- c23
many$lvl <- sprintf("L%02d", seq_len(nrow(many)) %% 40)
pm <- level_colours(plot_coverage_bar(many, y = "metier4", fill = "lvl", palette = "colorblind"), "fill")
ok(length(pm) == 40 && !anyDuplicated(pm) && identical(unname(pm[1:8]), PAL$colorblind),
   "40 levels on the 8-colour palette: the first eight are the palette, the rest are further colours, none repeated")
pd <- level_colours(plot_coverage_bar(many, y = "metier4", fill = "lvl"), "fill")
ok(length(pd) == 40 && !anyDuplicated(pd) && identical(unname(pd[1:28]), PAL$default),
   "40 levels on the default palette: its 28 colours, then further ones")

## ---- 6. the status scale ----------------------------------------------------------
fills <- function(p) ggplot_build(p)$data[[length(p$layers)]]$fill     # the legend's own marks
key_of <- function(pal) fills(plots$plot_coverage_grid(pal))
ok(identical(unname(key_of("default")[1:2]), STATUS$default[1:2]) &&
     identical(unname(key_of("colorblind")[1:2]), STATUS$colorblind[1:2]) &&
     identical(unname(key_of("gradual")[1:2]), STATUS$gradual[1:2]),
   "the status legend starts with the palette's full and safe colours")
h_cb <- plots$plot_coverage_grid("colorblind")
ok(!any(drawn(h_cb) %in% toupper(STATUS$default)),
   "a colourblind heatmap draws none of the default status colours")
ok(identical(plot_coverage_grid(cd, y = "metier4", palette = "colorblind")$data$cat_key,
             plot_coverage_grid(cd, y = "metier4")$data$cat_key),
   "the palette recolours the status; it never changes which status a cell has")
ok(identical(tryCatch({ plot_coverage_grid(cd, y = "metier4", palette = "colorblind"); "silent" },
                      warning = function(w) "warned"), "silent"),
   "a palette on the status plots is honoured silently (it used to warn that it did nothing)")

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
