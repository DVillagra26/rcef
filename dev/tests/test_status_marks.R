## How the plots mark what the raising or the allocation added: a cross for a point, a
## dotted line for a line, a transparent and hatched bar for a bar. The same marks in every
## plot that has a status (plot_catches(), plot_DR(), plot_NaLoA(), plot_MWaLoA(),
## plot_compare_raised()); the symbol of a point is a cross only while `shape` is free.
##
## What is pinned: the layers carry the marks, the stripes of a hatched bar really are in the
## drawn plot - 45 degrees, a fixed distance apart, inside the bars that are transparent and
## only there - the legend keys too, and the plots that name the status elsewhere or draw
## none have no mark.

source("dev/tests/fixture.R")
FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}
quiet <- function(expr) {
  out <- NULL
  invisible(utils::capture.output(out <- suppressWarnings(suppressMessages(expr))))
  out
}
suppressPackageStartupMessages(library(ggplot2))
lf <- tempfile(fileext = ".txt")

## ---- the fixture: a raised catch and an allocation of it ---------------------------
c23 <- subset(fx_catch(), year == 2023)
d23 <- as.data.frame(subset(fx_dist(), year == 2023))
raised <- as.data.frame(quiet(fx_raise(c23, fx_groups("own"), type = "discards", log_file = lf)))
ckey <- paste(raised$vesselFlagCountry, raised$catchCategory, raised$domainBiology)
akey <- paste(raised$vesselFlagCountry, raised$seasonValue, raised$areaValue,
              raised$fisheriesManagementUnit, raised$fleetValue, raised$catchCategory)
n_ck <- ave(seq_along(ckey), ckey, FUN = length)
n_ak <- ave(seq_along(akey), akey, FUN = length)
catch1 <- raised[ifelse(is.na(raised$domainBiology), n_ak == 1L, n_ck == 1L), ]
dist1  <- d23[paste(d23$vesselFlagCountry, d23$catchCategory, d23$domainBiology) %in%
                paste(catch1$vesselFlagCountry, catch1$catchCategory, catch1$domainBiology), ]
al <- quiet(fx_allocate(catch1, dist1, "LengthTotal", log_file = lf))
NaLoA  <- function(...) quiet(plot_NaLoA(al$catch, al$distribution, ...))
MWaLoA <- function(...) quiet(plot_MWaLoA(al$catch, al$distribution, ...))
ok(any(raised$importedOrRaised == "raised") && any(al$distribution$sampledOrEstimated == "estimated"),
   "fixture: something was raised, and something estimated")

## ---- looking into the drawn plot ----------------------------------------------------
# every grob of a class in the drawn plot (the panels are trees of grobs)
grobs_of <- function(p, cls) {
  out <- list()
  walk <- function(x) {
    if (inherits(x, cls)) out[[length(out) + 1L]] <<- x
    for (k in c(if (inherits(x, "gtable")) x$grobs, if (inherits(x, "gTree")) unclass(x$children))) walk(k)
  }
  walk(ggplot2::ggplotGrob(p))
  out
}
hatch_of <- function(p) grobs_of(p, "hatch_grob")
# the stripes the grob has when its panel is 100 x 60 mm, and the bars they are in, in mm
stripes <- function(hg) {
  grDevices::pdf(NULL); on.exit(grDevices::dev.off())
  grid::pushViewport(grid::viewport(width = grid::unit(100, "mm"), height = grid::unit(60, "mm")))
  out <- grid::makeContent(hg)
  grid::popViewport()
  s <- out$children[[1]]
  list(x0 = grid::convertX(s$x0, "mm", valueOnly = TRUE), y0 = grid::convertY(s$y0, "mm", valueOnly = TRUE),
       x1 = grid::convertX(s$x1, "mm", valueOnly = TRUE), y1 = grid::convertY(s$y1, "mm", valueOnly = TRUE),
       rects = data.frame(xmin = hg$rects$xmin * 100, xmax = hg$rects$xmax * 100,
                          ymin = hg$rects$ymin * 60, ymax = hg$rects$ymax * 60))
}
near <- function(a, b) isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = 1e-9))

## ---- bars: transparent and hatched --------------------------------------------------
pc <- quiet(plot_catches(raised, x = "metier4"))
ok(inherits(pc$layers[[1]]$geom, "GeomColHatch") && !is.null(pc$scales$get_scales("alpha")),
   "plot_catches(): the bars are a hatching layer, and imported / raised is their alpha")
hg <- hatch_of(pc)
n_raised_bars <- sum(round(ggplot_build(pc)$data[[1]]$alpha, 2) < 1)
ok(length(hg) == 1 && n_raised_bars > 0 && nrow(hg[[1]]$rects) == n_raised_bars,
   "plot_catches(): the drawn plot holds one hatch, over exactly the bars that are transparent")
sg <- stripes(hg[[1]])
ok(length(sg$x0) > 20 && all(sg$x1 > sg$x0) && near(sg$y1 - sg$y0, sg$x1 - sg$x0),
   "the stripes run at 45 degrees")
step <- 1.8 * sqrt(2)
ok(near((sg$x0 - sg$y0) / step, round((sg$x0 - sg$y0) / step)) &&
     near((sg$x1 - sg$y1) / step, round((sg$x1 - sg$y1) / step)),
   "... a fixed distance apart (1.8 mm), on one lattice - so they run on from bar to bar")
inside <- vapply(seq_along(sg$x0), function(i)
  any(sg$x0[i] >= sg$rects$xmin - 1e-6 & sg$x1[i] <= sg$rects$xmax + 1e-6 &
      pmin(sg$y0[i], sg$y1[i]) >= sg$rects$ymin - 1e-6 & pmax(sg$y0[i], sg$y1[i]) <= sg$rects$ymax + 1e-6), NA)
ok(all(inside), "... and every stripe lies inside a transparent bar")
# a bar of area A is covered with stripes about A / gap long: more stripes in a bigger bar
big <- which.max((sg$rects$xmax - sg$rects$xmin) * (sg$rects$ymax - sg$rects$ymin))
in_bar <- function(i) sg$x0 >= sg$rects$xmin[i] - 1e-6 & sg$x1 <= sg$rects$xmax[i] + 1e-6 &
  pmin(sg$y0, sg$y1) >= sg$rects$ymin[i] - 1e-6 & pmax(sg$y0, sg$y1) <= sg$rects$ymax[i] + 1e-6
len_in <- function(i) sum(sqrt(2) * (sg$x1 - sg$x0)[in_bar(i)])
area_i <- function(i) (sg$rects$xmax[i] - sg$rects$xmin[i]) * (sg$rects$ymax[i] - sg$rects$ymin[i])
ok(abs(len_in(big) / (area_i(big) / 1.8) - 1) < 0.15,
   "a bar is hatched at 1.8 mm: the stripes in it are as long as its area divided by the distance")

# the legend keys: a plain one for imported, a hatched one for raised
geom <- pc$layers[[1]]$geom
key <- function(a) geom$draw_key(data.frame(alpha = a, fill = "grey30", colour = "black", linewidth = 0.25,
                                            linetype = 1), list(), 5)
ok(inherits(key(1), "rect") && !inherits(key(1), "gTree") && inherits(key(0.45), "gTree") &&
     length(key(0.45)$children) == 2,
   "the legend: the key of an imported bar is a box, the key of a raised one is a box with stripes")

# nothing to tell apart, or told apart elsewhere: no hatch
no_hatch <- function(p) length(hatch_of(p)) == 0
ok(no_hatch(quiet(plot_catches(c23, x = "metier4"))) &&
     no_hatch(quiet(plot_catches(raised, x = "metier4", alpha = NULL))) &&
     no_hatch(quiet(plot_catches(raised, x = "metier4", fill = "importedOrRaised"))) &&
     no_hatch(quiet(plot_catches(raised, x = "metier4", facet = "importedOrRaised"))),
   "plot_catches(): no stripes for a raw catch, with alpha = NULL, or with importedOrRaised as the fill or the facet")
pf <- quiet(plot_catches(raised, x = "metier4", facet = "vesselFlagCountry", catchCategory = "Dis"))
ok(length(hatch_of(pf)) == length(unique(pf$data$vesselFlagCountry[pf$data$.status == "Raised"])),
   "plot_catches(): a panel that holds a raised bar has its stripes, a panel with none has not")

## numbers at length / age and the comparison of raised catches: the same bars
pn <- NaLoA(catchCategory = "Lan", fill = "vesselFlagCountry")
ok(inherits(pn$layers[[1]]$geom, "GeomColHatch") && length(hatch_of(pn)) == 1 &&
     nrow(hatch_of(pn)[[1]]$rects) == sum(round(ggplot_build(pn)$data[[1]]$alpha, 2) < 1),
   "plot_NaLoA(): the estimated bars are hatched")
pnd <- NaLoA(catchCategory = "Dis", fill = "vesselFlagCountry", stacked = FALSE, facet = "seasonValue")
ok(length(hatch_of(pnd)) >= 1 && no_hatch(NaLoA(catchCategory = "Lan", fill = "sampledOrEstimated")) &&
     no_hatch(NaLoA(catchCategory = "Lan", alpha = NULL)),
   "plot_NaLoA(): dodged and in panels too; none when the status is the fill, or alpha = NULL")
r_all <- as.data.frame(quiet(fx_raise(c23, fx_groups("all"), type = "discards", log_file = lf)))
pcmp <- quiet(plot_compare_raised(c(own = raised, everything = r_all), x = "metier4", catchCategory = "Dis"))
ok(inherits(pcmp$layers[[1]]$geom, "GeomColHatch") && length(hatch_of(pcmp)) == 1 &&
     grepl("solid = imported from the data, hatched = added by raising", pcmp$labels$caption, fixed = TRUE),
   "plot_compare_raised(): the raised part of each scenario is hatched")

## ---- points: a cross while the symbol is free ----------------------------------------------
pd <- quiet(plot_DR(raised, x = "metier4", colour = "vesselFlagCountry"))
b <- ggplot_build(pd)$data[[1]]
ok(setequal(unique(b$shape), c(21, 4)) && identical(b$shape == 4, pd$data$.status == "Raised"),
   "plot_DR(): imported strata are circles, raised ones crosses")
# a point in a facet per season, the status as colour: no cross at all, as before
pdc <- quiet(plot_DR(raised, x = "metier4", colour = "importedOrRaised"))
ok(all(ggplot_build(pdc)$data[[1]]$shape == 21),
   "plot_DR(): importedOrRaised as the colour: every point a circle, the colour says which")
# shape is a column of its own: the symbol is taken, the raised points are transparent instead
pds <- quiet(plot_DR(raised, x = "metier4", colour = "vesselFlagCountry", shape = "seasonValue"))
bs <- ggplot_build(pds)$data[[1]]
ok(!any(bs$shape %in% c(3, 4, 7, 8)) && setequal(round(unique(bs$alpha), 2), c(1, 0.45)) &&
     grepl("solid = imported from the data, transparent = added by raising", pds$labels$caption, fixed = TRUE),
   "plot_DR(): with shape a column the raised points are transparent, the caption says so - and no user symbol is a cross")
# the symbols a column can take never include a cross: a cross always means raised or estimated
ok(!any(c(3, 4, 7, 8) %in% pds$scales$get_scales("shape")$palette(8)),
   "plot_DR(): the symbols for a shape column hold no cross")

## ---- lines: dotted, with crosses ---------------------------------------------------------
pw <- MWaLoA(catchCategory = "Lan", colour = "vesselFlagCountry")
lay <- function(p, g) which(vapply(p$layers, function(l) inherits(l$geom, g), NA))
bl <- ggplot_build(pw)$data
ln <- bl[[lay(pw, "GeomLine")]]; pt <- bl[[lay(pw, "GeomPoint")]]
ok(setequal(ln$linetype, c("solid", "dotted")) && setequal(unique(pt$shape), c(21, 4)) &&
     all(pt$colour[pt$shape == 21] == OUTLINE) && all(pt$colour[pt$shape == 4] != OUTLINE) && !anyNA(pt$colour),
   "plot_MWaLoA(): sampled is a solid line with circles, estimated a dotted line with crosses in the colour of the line")
ok(all(pt$colour[pt$shape == 4] %in% ln$colour),
   "plot_MWaLoA(): a cross has the colour of its own line")
pw0 <- MWaLoA(catchCategory = "Lan")
b0 <- ggplot_build(pw0)$data
ok(setequal(b0[[lay(pw0, "GeomLine")]]$linetype, c("solid", "dotted")) &&
     setequal(unique(b0[[lay(pw0, "GeomPoint")]]$shape), c(21, 4)),
   "plot_MWaLoA(): the same without a colour")
# shape a column: dotted line, and the estimated points transparent
pws <- MWaLoA(catchCategory = "Lan", colour = "vesselFlagCountry", shape = "seasonValue")
bws <- ggplot_build(pws)$data
ok(setequal(bws[[lay(pws, "GeomLine")]]$linetype, c("solid", "dotted")) &&
     setequal(round(unique(bws[[lay(pws, "GeomPoint")]]$alpha), 2), c(1, 0.45)) &&
     !any(bws[[lay(pws, "GeomPoint")]]$shape %in% c(3, 4, 7, 8)),
   "plot_MWaLoA(): with shape a column the line is dotted and the estimated points transparent - none of the symbols a cross")
# the status as the colour or a facet, or alpha = NULL: one line type, no cross
pn1 <- MWaLoA(catchCategory = "Lan", colour = "sampledOrEstimated")
pn2 <- MWaLoA(catchCategory = "Lan", alpha = NULL)
solid <- function(x) all(x %in% c(1, "1", "solid"))          # the default line type, or the solid one
ok(solid(ggplot_build(pn1)$data[[lay(pn1, "GeomLine")]]$linetype) &&
     solid(ggplot_build(pn2)$data[[lay(pn2, "GeomLine")]]$linetype) &&
     all(ggplot_build(pn2)$data[[lay(pn2, "GeomPoint")]]$shape == 21),
   "plot_MWaLoA(): the status as the colour, or alpha = NULL: solid lines, circles")

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
