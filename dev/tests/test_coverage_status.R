## The coverage plots look at a raised catch, or at distributions the allocation estimated: the bar chart draws what the
## step added transparent (hatched), as plot_catches() and plot_NaLoA() do, and every coverage plot is titled with what
## its data holds - imported + raised or only imported, sampled + estimated or only sampled.
##
## What is pinned: the table has the weights the raising added (totalRaised, weightingTotalRaised - landings that
## carry a raised estimate -, coveredRaised) and the weight covered by an estimated distribution (coveredEstimated), and
## only when the data says so; coverage_cells() splits every cell into what was in the data and what the step added when
## it is given `alpha`, and the two add up to the cell as it was; `alpha` chooses what is marked (the allocation's step
## when both are in the data, one of them by name, none with NULL); the bar chart hatches the added part of its bars;
## the grid and the map draw their cells as they were, with the title; and a raw catch gives the plots as they were.
##
## Uses the synthetic test fixture (see dev/testdata/README.md), so this runs anywhere.

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
err <- function(expr) tryCatch({ expr; "" }, error = function(e) conditionMessage(e))
near <- function(a, b) isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = 1e-9))
suppressPackageStartupMessages(library(ggplot2))
lf <- tempfile(fileext = ".txt")
lay <- function(p, cls) which(vapply(p$layers, function(l) inherits(l$geom, cls), NA))[1]
built <- function(p, cls) ggplot_build(p)$data[[lay(p, cls)]]
tonnes <- function(d) convert_units(d$total, if ("variableUnit" %in% names(d)) d$variableUnit, "t")

## the fixture: a raised catch, and an allocation of it (lengths)
c23 <- subset(fx_catch(), year == 2023)
d23 <- as.data.frame(subset(fx_dist(), year == 2023))
raised <- as.data.frame(quiet(fx_raise(c23, fx_groups("own"), type = "discards", log_file = lf)))
al <- quiet(fx_allocation())$len
ok(any(raised$importedOrRaised == "raised") && any(al$distribution$sampledOrEstimated == "estimated"),
   "fixture: something was raised, and something estimated")

## ==== the table ================================================================================
tab <- build_coverage_strata(raised, c("metier4", "vesselFlagCountry"))
ok(all(c("totalRaised", "weightingTotalRaised") %in% names(tab)) &&
     !any(c("totalRaised", "weightingTotalRaised") %in% names(build_coverage_strata(c23, c("metier4", "vesselFlagCountry")))),
   "build_coverage_strata(): a raised catch's discard table has totalRaised and weightingTotalRaised, a raw catch's has not")
rd  <- raised$catchCategory == "Dis" & raised$importedOrRaised == "raised"
lan <- raised$catchCategory == "Lan"
k   <- function(x) paste(x$vesselFlagCountry, x$year, x$workingGroup, x$stock, x$speciesCode, x$originType,
                         x$variableType, x$domainCatchDis)
ok(near(sum(tab$totalRaised, na.rm = TRUE), sum(tonnes(raised)[rd], na.rm = TRUE)),
   "build_coverage_strata(): totalRaised is the weight of the discards the raising made")
ok(near(sum(tab$weightingTotalRaised), sum(tonnes(raised)[lan & k(raised) %in% k(raised[rd, ])], na.rm = TRUE)) &&
     all(tab$weightingTotalRaised <= tab$weightingTotal + 1e-9) && sum(tab$weightingTotalRaised) > 0,
   "build_coverage_strata(): weightingTotalRaised is the landings that carry a raised discard estimate - a part of weightingTotal")

len <- build_coverage_strata(raised, "metier4", d23, what = "length")
ok(all(c("totalRaised", "coveredRaised") %in% names(len)) && !"coveredEstimated" %in% names(len) &&
     near(sum(len$totalRaised), sum(tonnes(raised)[rd], na.rm = TRUE)) &&
     all(len$totalRaised[len$catchCategory == "Lan"] == 0) && all(len$coveredRaised == 0),
   "build_coverage_strata(): a length table has totalRaised (the raised rows, all of them discards) and coveredRaised - none of it covered - and no coveredEstimated without the allocation")
lal <- build_coverage_strata(al$catch, "metier4", al$distribution, what = "length")
ok("coveredEstimated" %in% names(lal) && sum(lal$coveredEstimated) > 0 &&
     all(lal$coveredEstimated <= lal$total * lal$coverage / 100 + 1e-9),
   "build_coverage_strata(): with the allocation's distributions, coveredEstimated is the weight covered by an estimated one - a part of the covered weight")

## ==== coverage_cells(): the cells and what a step added to them =================================
both <- c("importedOrRaised", "sampledOrEstimated")
cc <- function(catch, dist, keys, what, cat, alpha = NULL) coverage_cells(catch, dist, keys, what, TRUE, cat, "t", alpha)
plain <- cc(raised, d23, "metier4", "length", "Dis")
split <- cc(raised, d23, "metier4", "length", "Dis", both)
pool  <- function(a, b, cols) {          # the status rows of a cell, summed, against the cell (no estimate = 0)
  a[cols] <- lapply(a[cols], function(v) ifelse(is.na(v), 0, v))
  b[cols] <- lapply(b[cols], function(v) ifelse(is.na(v), 0, v))
  s <- aggregate(a[cols], a["metier4"], sum)
  m <- merge(s, b[c("metier4", cols)], by = "metier4", suffixes = c(".s", ".p"))
  nrow(m) == nrow(b) && all(vapply(cols, function(v) near(m[[paste0(v, ".s")]], m[[paste0(v, ".p")]]), NA))
}
ok(is.null(plain$status) && identical(split$status, "importedOrRaised") && nrow(split$cells) > nrow(plain$cells) &&
     setequal(split$cells$importedOrRaised, c("imported", "raised")),
   "coverage_cells(): with `alpha` a raised catch's cells come in two rows, imported and raised; without, one")
ok(pool(split$cells, plain$cells, c("total", "covered")),
   "coverage_cells(): ... and the two rows of a cell add up to the cell as it was")
r2 <- split$cells[split$cells$importedOrRaised == "raised", ]
ok(all(r2$covered == 0) && all(is.na(r2$numTrips)) && all(r2$numMeasurements == 0),
   "coverage_cells(): what the raising added has no distribution, no trips and no fish behind it")
ok(identical(plain$title, "Discards - Sampling of Length, imported + raised") && identical(split$title, plain$title),
   "coverage_cells(): the title says what the cells hold, marked or not")
lanc <- cc(raised, d23, "metier4", "length", "Lan", both)
ok(is.null(lanc$status) && identical(lanc$title, "Landings - Sampling of Length, only imported") &&
     nrow(lanc$cells) == nrow(cc(raised, d23, "metier4", "length", "Lan")$cells),
   "coverage_cells(): the landings of a raised catch are all imported: no split, the title says 'only imported'")
rawc <- cc(c23, d23, "metier4", "length", "Dis", both)
ok(is.null(rawc$status) && identical(rawc$title, "Discards - Sampling of Length"),
   "coverage_cells(): a raw catch has no status: no split, the title as it was")
dsc  <- cc(raised, NULL, "metier4", "discard", NULL)
dss  <- cc(raised, NULL, "metier4", "discard", NULL, both)
ok(identical(dss$status, "importedOrRaised") && identical(dss$title, "Sampling of Discards, imported + raised") &&
     pool(dss$cells, dsc$cells, c("total", "weightingTotal", "landings")) &&
     all(dss$cells$coverage[dss$cells$importedOrRaised == "raised"] %in% c(NA, 100)),
   "coverage_cells(): the discard coverage splits the landings by whose estimate they carry - and a raised landing is covered")

## the allocation: what is estimated is marked; the raising when asked
ca  <- cc(al$catch, al$distribution, "metier4", "length", NULL, both)
cr  <- cc(al$catch, al$distribution, "metier4", "length", NULL, "importedOrRaised")
ok(identical(ca$status, "sampledOrEstimated") && endsWith(ca$title, "sampled + estimated") &&
     setequal(ca$cells$sampledOrEstimated, c("sampled", "estimated")),
   "coverage_cells(): with the allocation's distributions the estimated weight is the one marked (the later step), and the title says so")
ok(identical(cr$status, "importedOrRaised") &&
     is.null(cc(raised, d23, "metier4", "length", "Dis", "sampledOrEstimated")$status),
   "coverage_cells(): `alpha` names the status to mark; one that is not in the data marks nothing")
ca1 <- ca$cells[ca$cells$sampledOrEstimated == "estimated", ]
ok(all(near(ca1$covered, ca1$total)) && all(ca1$numMeasurements == 0),
   "coverage_cells(): what is estimated is covered by definition and has no fish measured")
ok(grepl("alpha", err(cc(raised, d23, "metier4", "length", "Dis", "colour")), fixed = TRUE) &&
     is.null(cc(raised, d23, "metier4", "length", "Dis", NULL)$status),
   "coverage_cells(): `alpha` is the two status columns or NULL, nothing else")
ok(is.null(cc(raised, d23, c("metier4", "importedOrRaised"), "length", "Dis", both)$status),
   "coverage_cells(): a status that is a plotted column is shown there, not marked as well")

## ==== the bar chart: hatched where a step added ==================================================
pb <- plot_coverage_bar(raised, d23, y = "metier4", what = "length", catchCategory = "Dis")
bb <- built(pb, "GeomColHatch")
ok(inherits(pb$layers[[lay(pb, "GeomColHatch")]]$geom, "GeomColHatch") && !is.null(pb$scales$get_scales("alpha")) &&
     identical(pb$labels$title, "Discards - Sampling of Length, imported + raised") &&
     grepl("solid = imported from the data, hatched = added by raising", pb$labels$caption, fixed = TRUE),
   "plot_coverage_bar(): the raised part of a bar is hatched, and the title and the caption say so")
ok(near(sum(bb$xmax - bb$xmin), sum(plain$cells$total[plain$cells$total > 0])) &&
     near(sum((bb$xmax - bb$xmin)[round(bb$alpha, 2) < 1]), sum(len$totalRaised[len$catchCategory == "Dis"])),
   "plot_coverage_bar(): the bars add up to the weight of the cells, and the hatched part to the weight the raising added")
pbn <- plot_coverage_bar(raised, d23, y = "metier4", what = "length", catchCategory = "Dis", alpha = NULL)
ok(!inherits(pbn$layers[[lay(pbn, "GeomCol")]]$geom, "GeomColHatch") && is.null(pbn$scales$get_scales("alpha")) &&
     identical(pbn$labels$title, pb$labels$title) &&
     near(sum(built(pbn, "GeomCol")$xmax - built(pbn, "GeomCol")$xmin), sum(bb$xmax - bb$xmin)),
   "plot_coverage_bar(): alpha = NULL draws the same bars with no mark")
pbr <- plot_coverage_bar(c23, d23, y = "metier4", what = "length", catchCategory = "Dis")
ok(is.null(pbr$scales$get_scales("alpha")) && identical(pbr$labels$title, "Discards - Sampling of Length") &&
     !any(vapply(pbr$layers, function(l) inherits(l$geom, "GeomColHatch"), NA)),
   "plot_coverage_bar(): a raw catch is drawn as it was")
pbl <- plot_coverage_bar(raised, d23, y = "metier4", what = "length", catchCategory = "Lan")
ok(is.null(pbl$scales$get_scales("alpha")) && identical(pbl$labels$title, "Landings - Sampling of Length, only imported"),
   "plot_coverage_bar(): the landings of a raised catch have nothing to mark")
pbs <- plot_coverage_bar(raised, d23, y = "importedOrRaised", what = "length", catchCategory = "Dis")
ok(is.null(pbs$scales$get_scales("alpha")) && !any(vapply(pbs$layers, function(l) inherits(l$geom, "GeomColHatch"), NA)),
   "plot_coverage_bar(): importedOrRaised as the row variable is drawn there, not marked as well")
pbd <- plot_coverage_bar(raised, y = "metier4", fill = "vesselFlagCountry")
bd  <- built(pbd, "GeomColHatch")
ok(identical(pbd$labels$title, "Sampling of Discards, imported + raised") &&
     near(sum(bd$xmax - bd$xmin), sum(dsc$cells$landings[dsc$cells$landings > 0])) &&
     near(sum((bd$xmax - bd$xmin)[round(bd$alpha, 2) < 1]), sum(tab$weightingTotalRaised)),
   "plot_coverage_bar(): the discard coverage of a raised catch hatches the landings that carry a raised estimate")
pba <- plot_coverage_bar(al$catch, al$distribution, y = "metier4", what = "length")
ok(grepl("solid = sampled, hatched = estimated by the allocation", pba$labels$caption, fixed = TRUE) &&
     endsWith(pba$labels$title, "sampled + estimated"),
   "plot_coverage_bar(): the allocation's estimated weight is hatched, the caption says it is the allocation")
# every bar of the chart stays one bar: the segments of a bar share a row
ok(length(unique(pb$data$.key)) == length(unique(plot_coverage_bar(raised, d23, y = "metier4", what = "length",
                                                                   catchCategory = "Dis", alpha = NULL)$data$.key)),
   "plot_coverage_bar(): the segments of a bar share its row")

## ==== the grid and the map: the title says what the data holds, the cells are as they were ==========
## (the marks are the bar chart's: a cell of a grid or a pie cannot be split into what was in the data and what was added)
cc2   <- cc(raised, d23, c("metier4", "vesselFlagCountry"), "length", "Dis")
pg <- plot_coverage_grid(raised, d23, y = "metier4", x = "vesselFlagCountry", what = "length", catchCategory = "Dis")
tg <- built(pg, "GeomTile")
tg <- tg[tg$xmax - tg$xmin > 0, ]                       # the zero-size tiles are the legend's
ok(identical(pg$labels$title, "Discards - Sampling of Length, imported + raised") &&
     is.null(pg$scales$get_scales("alpha")) && all(round(tg$xmax - tg$xmin, 6) == 1) &&
     nrow(tg) == sum(cc2$cells$total > 0),
   "plot_coverage_grid(): the title says the catch is imported + raised, and a cell is one tile - no transparency")
pgb <- plot_coverage_grid(raised, d23, y = "metier4", x = "vesselFlagCountry", what = "length", catchCategory = "Dis", type = "bubble")
ok(identical(pgb$labels$title, pg$labels$title) && is.null(pgb$scales$get_scales("alpha")) &&
     nrow(built(pgb, "GeomPoint")[is.finite(built(pgb, "GeomPoint")$size) & built(pgb, "GeomPoint")$size > 0, ]) >= sum(cc2$cells$total > 0),
   "plot_coverage_grid(type = 'bubble'): the same title, a bubble for a cell")
ok(identical(plot_coverage_grid(raised, y = "metier4", x = "vesselFlagCountry")$labels$title, "Sampling of Discards, imported + raised") &&
     identical(plot_coverage_grid(raised, d23, y = "metier4", x = "vesselFlagCountry", what = "length", catchCategory = "Lan")$labels$title,
               "Landings - Sampling of Length, only imported") &&
     identical(plot_coverage_grid(c23, d23, y = "metier4", x = "vesselFlagCountry", what = "length", catchCategory = "Dis")$labels$title,
               "Discards - Sampling of Length"),
   "plot_coverage_grid(): the discard coverage of a raised catch is titled so, its landings 'only imported', a raw catch as it was")
tga <- plot_coverage_grid(al$catch, al$distribution, y = "metier4", what = "length")$labels$title
ok(startsWith(tga, "Catch - Sampling of Length, ") && endsWith(tga, "sampled + estimated"),
   "plot_coverage_grid(): after the allocation the title says the distributions are sampled + estimated")
ok(grepl("imported + raised", tga, fixed = TRUE) == ("importedOrRaised" %in% names(al$catch)),
   "plot_coverage_grid(): ... and the catch imported + raised when it carries that, and only then")
ok(grepl("unused argument", err(plot_coverage_grid(raised, d23, y = "metier4", what = "length", alpha = NULL)), fixed = TRUE) &&
     grepl("unused argument", err(plot_coverage_map(raised, d23, what = "length", alpha = NULL)), fixed = TRUE),
   "plot_coverage_grid() and plot_coverage_map() have no `alpha`: the bar chart is the one that marks")
if (requireNamespace("maps", quietly = TRUE)) {
  pm <- plot_coverage_map(raised, d23, area = "ICESDivision", what = "length", catchCategory = "Dis")
  pmr <- plot_coverage_map(c23, d23, area = "ICESDivision", what = "length", catchCategory = "Dis")
  ok(grepl("Sampling of Length, imported + raised by ICESDivision", pm$labels$title, fixed = TRUE) &&
       is.null(pm$scales$get_scales("alpha")) &&
       identical(pmr$labels$title, "Discards - Sampling of Length by ICESDivision") &&
       length(unique(ggplot_build(pm)$data[[tail(which(vapply(pm$layers, function(l) inherits(l$geom, "GeomPolygon"), NA)), 1)]]$group)) <=
         2 * sum(cc(raised, d23, "ICESDivision", "length", "Dis")$cells$total > 0),
     "plot_coverage_map(): the title says the catch is imported + raised, a pie is a sampled and a not-sampled slice; a raw catch as it was")
}

## ==== the groups are made from the table as before ===============================================
rg <- try(quiet(recommend_groups(c23, c("metier4", "vesselFlagCountry"))), silent = TRUE)
rg_len <- try(quiet(recommend_groups(c23, c("metier4", "vesselFlagCountry"), distribution_data = d23)), silent = TRUE)
ok(inherits(rg, "rcef_groups") && inherits(rg_len, "rcef_groups"),
   "recommend_groups(): the groups are made as before, from the table with its new columns")

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
