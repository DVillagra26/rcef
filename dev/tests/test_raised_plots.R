## plot_catches() and plot_DR() - the same plots before and after the raising.
##
## There is one function for the catch weight and one for the discard rate; given
## the raised catch they show what the raising did (imported solid, raised
## transparent), given a raw catch the data as it was reported. The title says
## what the data holds: "imported + raised", or "only imported" when nothing was raised. Every
## number a plot draws is a column of build_coverage_strata() of that catch.
##
## Uses the synthetic test fixture and fx_raise() (as test_workflow_example.R
## does) rather than a local-machine-only CEF file, so this runs anywhere.

source("dev/tests/fixture.R")
FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}
quiet <- function(expr) {
  out <- NULL
  invisible(utils::capture.output(out <- suppressMessages(expr)))
  out
}
# the value of expr, with the messages it gave
heard <- function(expr) {
  msgs <- character()
  val <- withCallingHandlers(expr,
    message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") })
  list(value = val, messages = msgs)
}
err <- function(expr) tryCatch({ expr; "" }, error = function(e) conditionMessage(e))
suppressPackageStartupMessages(library(ggplot2))
lf <- tempfile(fileext = ".txt")

drawable <- function(p)
  !is.null(p) && inherits(p, "ggplot") &&
  !inherits(try(ggplot_build(p), silent = TRUE), "try-error")
has_geom <- function(p, cls)
  any(vapply(p$layers, function(l) inherits(l$geom, cls), logical(1)))
near <- function(a, b) isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = 1e-9))
shaded <- function(p) {      # the status is drawn as a mark: bars by transparency, points as a cross
  s <- p$scales$get_scales("shape")
  !is.null(p$scales$get_scales("alpha")) || (!is.null(s) && identical(as.numeric(unname(s$palette(2))), c(21, 4)))
}
n_panels <- function(p) nrow(ggplot_build(p)$layout$layout)

test_catch <- fx_catch()
c23 <- subset(test_catch, year == 2023)
raised <- quiet(fx_raise(c23, fx_groups("own"), type = "discards", log_file = lf))
S <- "importedOrRaised"

## a raised catch in which nothing needed raising: every row imported
untouched <- raised[raised$importedOrRaised == "imported", ]

## ==== plot_catches() ===========================================================
p1 <- plot_catches(catch = raised, x = "metier4")
ok(drawable(p1), "plot_catches(): draws")
ok(identical(p1$labels$title, "Catch per stratum, imported + raised"),
   "plot_catches(): a raised catch is titled 'imported + raised'")
ok(shaded(p1) && identical(levels(p1$data$.status), c("Imported", "Raised")),
   "plot_catches(): ... and imported / raised is the shade by default")
ok(grepl("^grouping = metier4, grouped over", p1$labels$caption),
   "plot_catches(): the caption names what the bars pool, as the other plots do")

p1_dis <- plot_catches(raised, x = "metier4", catchCategory = "Dis")
ok(drawable(p1_dis) && identical(sort(unique(p1_dis$data$.cat)), "Discards") &&
     identical(p1_dis$labels$title, "Discards per stratum, imported + raised") &&
     identical(p1_dis$labels$y, "Discards (t)"),
   "plot_catches(): catchCategory = 'Dis' keeps only the discards and says so, in the title and the axis")

## raw catch: the data as it was reported
p_raw <- plot_catches(c23, x = "metier4")
ok(drawable(p_raw) && identical(p_raw$labels$title, "Catch per stratum, only imported") &&
     !shaded(p_raw) && !".status" %in% names(p_raw$data),
   "plot_catches(): a raw catch is titled 'only imported' and has no shade")
tonnes <- function(d) sum(d$total) / 1000
ok(near(sum(p_raw$data$t), tonnes(c23)),
   "plot_catches(): ... and draws the weight of the catch, in tonnes")
p_none <- plot_catches(untouched, x = "metier4")
ok(identical(p_none$labels$title, "Catch per stratum, only imported") && !shaded(p_none),
   "plot_catches(): a raised catch in which nothing was raised is titled and drawn as imported")
ok(!grepl("shade", p_none$labels$caption) && !grepl("added together", p_none$labels$caption),
   "plot_catches(): ... and the caption does not talk about a shade it does not draw")

## the title says what is DRAWN: the landings of a raised catch were all imported, so they read as such
p_lan <- plot_catches(raised, x = "metier4", catchCategory = "Lan")
ok(identical(p_lan$labels$title, "Landings per stratum, only imported") && !shaded(p_lan),
   "plot_catches(): the landings of a raised catch are titled 'only imported' and have no shade - nothing was raised there")
p_only <- plot_catches(raised[raised$importedOrRaised == "raised", ], x = "metier4")
ok(identical(p_only$labels$title, "Catch per stratum, only raised") && shaded(p_only),
   "plot_catches(): a catch of raised rows alone is titled 'only raised' (and drawn transparent)")

## the catch parts: landings, discards and BMS each their own
bms <- c23[c23$catchCategory == "Lan", ][1:3, ]; bms$catchCategory <- "BMS"
p_b <- plot_catches(rbind(c23, bms), x = "metier4")
ok(setequal(unique(p_b$data$.cat), c("Landings", "Discards", "BMS")) &&
     length(p_b$scales$get_scales("fill")$palette(3)) == 3 &&
     grepl("landings, discards or BMS", p_b$labels$caption, fixed = TRUE),
   "plot_catches(): BMS is a catch part of its own, not discards")
ok(identical(plot_catches(rbind(c23, bms), x = "metier4", catchCategory = "BMS")$labels$title,
             "BMS catch per stratum, only imported"),
   "plot_catches(): catchCategory = 'BMS' works")
ok(grepl("Lan.*Dis.*BMS", err(plot_catches(c23, catchCategory = "xyz"))),
   "plot_catches(): a catch part that is none of the three is refused")

## ---- importedOrRaised: the shade by default, or the fill, the facet, the x ----
## It is a variable like any other: drawn once - as the shade unless it is named
## in fill / facet / x - and alpha = NULL draws no shade.
by_src <- tapply(p1$data$t, p1$data$.status, sum)
ok(inherits(plot_catches(raised, "metier4", "vesselFlagCountry")$facet, "FacetWrap"),
   "plot_catches(): x and facet are still the first two arguments after the catch")
pcf <- plot_catches(raised, x = "metier4", fill = S)
ok(drawable(pcf) && !shaded(pcf) && is.factor(pcf$data[[S]]) &&
     identical(levels(pcf$data[[S]]), c("imported", "raised")) &&
     near(tapply(pcf$data$t, pcf$data[[S]], sum), by_src),
   "plot_catches(): fill = importedOrRaised - a colour each, no shade, the same weights")
ok(grepl("color = importedOrRaised", pcf$labels$caption, fixed = TRUE) &&
     !grepl("hatched", pcf$labels$caption, fixed = TRUE),
   "plot_catches(): ... and the caption says what the colour is")
pcx <- plot_catches(raised, x = "metier4", facet = S)
ok(drawable(pcx) && !shaded(pcx) && inherits(pcx$facet, "FacetWrap") && n_panels(pcx) == 2 &&
     near(tapply(pcx$data$t, pcx$data[[S]], sum), by_src),
   "plot_catches(): facet = importedOrRaised - a panel each, no shade, the same weights")
pcb <- plot_catches(raised, x = S)
ok(drawable(pcb) && !shaded(pcb) && setequal(as.character(unique(pcb$data$.x)), c("imported", "raised")) &&
     near(tapply(pcb$data$t, pcb$data$.x, sum), by_src),
   "plot_catches(): x = importedOrRaised - a bar each, no shade, the same weights")
pcn <- plot_catches(raised, x = "metier4", alpha = NULL)
ok(drawable(pcn) && !shaded(pcn) && near(sum(pcn$data$t), sum(by_src)) &&
     grepl("added together", pcn$labels$caption, fixed = TRUE) &&
     identical(pcn$labels$title, "Catch per stratum, imported + raised"),
   "plot_catches(): alpha = NULL - no shade, imported and raised added together, still titled raised")
pcm <- plot_catches(raised, x = "metier4", fill = "vesselFlagCountry", catchCategory = "Dis")
ok(drawable(pcm) && shaded(pcm) && near(sum(pcm$data$t), sum(p1_dis$data$t)) &&
     identical(pcm$scales$get_scales("fill")$name, "vesselFlagCountry"),
   "plot_catches(): another fill column keeps the shade, and the weights")
ok(grepl("`alpha` is \"importedOrRaised\"", err(plot_catches(raised, alpha = "metier4")), fixed = TRUE) &&
     grepl("no column 'nope'", err(plot_catches(raised, fill = "nope")), fixed = TRUE) &&
     grepl("catchRaising", err(plot_catches(c23, fill = S)), fixed = TRUE),
   "plot_catches(): a wrong alpha, a column the catch lacks, importedOrRaised asked of a raw catch: refused, and named")

## ==== plot_DR() on the raised catch ===============================================
ok(inherits(try(plot_DR(raised), silent = TRUE), "try-error"),
   "plot_DR(): `x` is required")
ok(grepl("nope", err(plot_DR(raised, x = "nope")), fixed = TRUE),
   "plot_DR(): a column the catch lacks is refused, and named")
ok(grepl("catchRaising", err(plot_DR(c23, x = S)), fixed = TRUE),
   "plot_DR(): importedOrRaised asked of a catch that was never raised: refused, and says where it comes from")
ok(inherits(try(plot_DR(raised_catch = raised, x = "metier4"), silent = TRUE), "try-error"),
   "plot_DR(): the catch is `catch` - there is no raised_catch argument")

p2 <- plot_DR(catch = raised, x = "metier4")
ok(drawable(p2), "plot_DR(): draws the raised catch")
ok(identical(p2$labels$title, "Discard ratio per stratum, imported + raised"),
   "plot_DR(): default rate is ratio, and a raised catch is titled 'imported + raised'")
ok(grepl("raw data per strata including imported and raised", p2$labels$caption, fixed = TRUE),
   "plot_DR(): the caption says what a point is")
ok(!has_geom(p2, "GeomErrorbar"),
   "plot_DR(): no weighted-mean line once strata are raised - the crosses are the reference")
ok(!has_geom(p2, "GeomHline"),
   "plot_DR(): no reference line when `reference` is not set")
ok(sum(vapply(p2$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1))) == 1,
   "plot_DR(): one point layer, the strata of the raised catch - no donor layer")
ok(min(p2$data$landings) >= max(p2$data$landings) * (0.1 / 1.1) - 1e-9 &&
     length(unique(p2$data$landings)) > 1,
   "plot_DR(): bubble size is shifted up (additively - strata with different landings still differ)")

p2_rate <- plot_DR(raised, x = "metier4", rate = "rate", reference = 0.2)
ok(identical(p2_rate$labels$title, "Discard rate per stratum, imported + raised") &&
     has_geom(p2_rate, "GeomHline") &&
     !has_geom(p2_rate, "GeomText") && !any(grepl("labels =", p2_rate$labels$caption)),
   "plot_DR(): rate = \"rate\" is named in the title; the reference line draws, its labels do not (raised)")

## raw strata, imported circles and raised crosses: a two-level status, not a share
ok(is.factor(p2$data$.status) && identical(levels(p2$data$.status), c("Imported", "Raised")) &&
     !anyNA(p2$data$.status) && any(p2$data$.status == "Imported") && any(p2$data$.status == "Raised"),
   "plot_DR(): every point is Imported or Raised (this example has both)")
ok(!".raised_share" %in% names(p2$data) && !grepl("share", p2$labels$caption, ignore.case = TRUE) &&
     is.null(p2$scales$get_scales("shape")$name),
   "plot_DR(): no 'share raised' - not in the data, the caption or a legend title")
b2 <- ggplot_build(p2)$data[[1]]
ok(setequal(unique(b2$shape), c(21, 4)) && identical(b2$shape == 4, p2$data$.status == "Raised") &&
     all(round(b2$alpha, 2) == 0.85),
   "plot_DR(): imported strata are circles (shape 21), raised ones crosses (shape 4), alike in transparency")
ok(all(b2$colour[b2$shape == 21] == OUTLINE) && all(b2$colour[b2$shape == 4] != OUTLINE) &&
     !anyNA(b2$colour) && all(b2$stroke[b2$shape == 4] > b2$stroke[b2$shape == 21][1]),
   "plot_DR(): a circle keeps the dark outline, a cross is drawn in the colour of its category and heavier")
ok(identical(unname(p2$scales$get_scales("shape")$palette(2)), c(21, 4)) &&
     grepl("circles = imported from the data, crosses = added by raising", p2$labels$caption, fixed = TRUE),
   "plot_DR(): the legend and the caption say circle = imported, cross = raised")
ok(max(which(p2$data$.status == "Raised")) < min(which(p2$data$.status == "Imported")),
   "plot_DR(): raised points are drawn first, so no imported point is buried under one")

# one point per raw stratum (fleet x area x country x season with landings)
wg <- raised$originType %in% "WGEstimate" & raised$variableType %in% "WeightLive"
skey <- function(d) paste(d$fleetValue, d$areaValue, d$vesselFlagCountry, d$seasonValue)
lan <- raised[wg & raised$catchCategory == "Lan", ]
dis <- raised[wg & raised$catchCategory == "Dis", ]
has_rate <- sum(tapply(lan$total, skey(lan), sum) > 0)
ok(nrow(p2$data) <= has_rate && nrow(p2$data) > 0.5 * has_rate,
   "plot_DR(): one point per raw stratum of the raised catch (less the raised ones with no donor)")
ok(anyDuplicated(skey(p2$data)) == 0, "plot_DR(): ... and never two for one stratum")

# a point's status and ratio are those of its own rows
raised_keys <- unique(skey(dis)[dis$importedOrRaised == "raised"])
ok(identical(p2$data$.status == "Raised", skey(p2$data) %in% raised_keys),
   "plot_DR(): a stratum is Raised exactly when its discard rows were raised")
i <- which(p2$data$.status == "Imported" & p2$data$total > 0)[1]
k <- skey(p2$data)[i]
y_ref <- sum(dis$total[skey(dis) == k]) / sum(lan$total[skey(lan) == k]) * 100
ok(isTRUE(all.equal(p2$data$.y[i], y_ref)),
   "plot_DR(): a sampled stratum's point is its own discards / landings")
j <- which(p2$data$.status == "Raised" & p2$data$total > 0)[1]
k2 <- skey(p2$data)[j]
y_ref2 <- sum(dis$total[skey(dis) == k2]) / sum(lan$total[skey(lan) == k2]) * 100
ok(isTRUE(all.equal(p2$data$.y[j], y_ref2)),
   "plot_DR(): a raised stratum's point is its raised discards / landings")

# the Official 0 kg discards a stratum may carry beside the raised ones are not discards
extra <- dis[dis$importedOrRaised == "imported", ][1:5, ]
extra$originType <- "Official"; extra$total <- 1e6
ok(near(plot_DR(rbind(raised, extra), x = "metier4")$data$.y, p2$data$.y) &&
     near(build_coverage_strata(rbind(raised, extra))$total, build_coverage_strata(raised)$total),
   "plot_DR(): rows of another origin than the ones raised are not counted")

# colour, shape and facet each take a column of the raised catch
p3 <- plot_DR(raised, x = "DrGroup", colour = "metier4",
              shape = "seasonValue", facet = "ICESDivision")
ok(drawable(p3), "plot_DR(): x = DrGroup with colour, shape and facet")
ok(drawable(plot_DR(raised, x = "DrGroup", jitter = FALSE)), "plot_DR(): jitter = FALSE")

## a raised stratum whose group had no donor comes out of the raising as NaN:
## no rate to draw, so it is left out - and the reader is told
r_nan <- raised
k_nan <- skey(p2$data)[j]
hit <- which(wg & r_nan$catchCategory == "Dis" & skey(r_nan) == k_nan)
r_nan$total[hit] <- NaN
h <- heard(plot_DR(r_nan, x = "metier4"))
ok(drawable(h$value) && nrow(h$value$data) == nrow(p2$data) - 1 && !any(skey(h$value$data) == k_nan),
   "plot_DR(): a raised stratum with no rate (NaN) is left out, not drawn at 0")
ok(any(grepl("no rate", h$messages)), "plot_DR(): and says so")
ok(is.na(build_coverage_strata(r_nan)$total[skey(build_coverage_strata(r_nan)) == k_nan]),
   "build_coverage_strata(): a stratum whose discards are NaN has no discard estimate")

## ---- importedOrRaised in plot_DR: the shade by default, or the colour, shape, facet, x ----
ok(shaded(p2), "plot_DR(): by default imported / raised is the shade")
same_strata <- function(p) nrow(p$data) == nrow(p2$data) && near(sort(p$data$.y), sort(p2$data$.y))
pdc <- plot_DR(raised, x = "metier4", colour = S)
ok(drawable(pdc) && !shaded(pdc) && is.factor(pdc$data[[S]]) &&
     identical(levels(pdc$data[[S]]), c("imported", "raised")) && same_strata(pdc) &&
     identical(pdc$data[[S]] == "raised", pdc$data$.status == "Raised"),
   "plot_DR(): colour = importedOrRaised - a colour each, no shade, the same strata and rates")
pds <- plot_DR(raised, x = "metier4", shape = S)
pdf_ <- plot_DR(raised, x = "metier4", facet = S)
pdx <- plot_DR(raised, x = S)
ok(drawable(pds) && !shaded(pds) && same_strata(pds) &&
     drawable(pdf_) && !shaded(pdf_) && same_strata(pdf_) && n_panels(pdf_) == 2 &&
     drawable(pdx) && !shaded(pdx) && setequal(as.character(pdx$data[[S]]), c("imported", "raised")),
   "plot_DR(): shape, facet and x = importedOrRaised work the same way")
pdn <- plot_DR(raised, x = "metier4", alpha = NULL)
ok(drawable(pdn) && !shaded(pdn) && same_strata(pdn) &&
     grepl("drawn alike", pdn$labels$caption, fixed = TRUE) &&
     !grepl("transparent", pdn$labels$caption, fixed = TRUE) &&
     identical(pdn$labels$title, "Discard ratio per stratum, imported + raised"),
   "plot_DR(): alpha = NULL - no shade, imported and raised drawn alike, still titled raised")
ok(grepl("`alpha` is \"importedOrRaised\"", err(plot_DR(raised, x = "metier4", alpha = "metier4")), fixed = TRUE),
   "plot_DR(): an alpha that is not importedOrRaised is refused, with the two values it takes")

## ==== plot_DR() before the raising: the same function, the data as reported ============
p_pre <- plot_DR(c23, x = "metier4", colour = "vesselFlagCountry", reference = 0.2)
ok(identical(p_pre$labels$title, "Discard ratio per stratum, only imported") &&
     !shaded(p_pre) && !".status" %in% names(p_pre$data),
   "plot_DR(): a raw catch is titled 'only imported' and has no shade")
ok(has_geom(p_pre, "GeomErrorbar") && has_geom(p_pre, "GeomHline") && has_geom(p_pre, "GeomText") &&
     grepl("horizontal line = landings weighted average", p_pre$labels$caption, fixed = TRUE) &&
     grepl("labels = strata ID exceeding reference line", p_pre$labels$caption, fixed = TRUE),
   "plot_DR(): ... with the weighted-mean line, the reference line and the labels of the strata above it")
ok(grepl("as in CEF (CN) data", p_pre$labels$caption, fixed = TRUE),
   "plot_DR(): ... and the caption says the strata are the data as in CEF")
p_none_dr <- plot_DR(untouched, x = "metier4")
ok(identical(p_none_dr$labels$title, "Discard ratio per stratum, only imported") &&
     !shaded(p_none_dr) && has_geom(p_none_dr, "GeomErrorbar"),
   "plot_DR(): a raised catch in which nothing was raised is treated as imported: titled so, no shade, the backdrop")
# the numbers are the coverage table's
cov <- build_coverage_strata(raised, dims = c("fleetValue", "areaValue", "vesselFlagCountry", "seasonValue", "metier4"))
ok(S %in% names(cov) && setequal(cov[[S]], c("imported", "raised")) &&
     all(cov$coverage[!is.na(cov$coverage)] == 100),
   "build_coverage_strata(): a raised catch's table says which strata were raised, and every landing counts")
ok(grepl("importedOrRaised", err(build_coverage_strata(raised, dims = c("metier4", S))), fixed = TRUE),
   "build_coverage_strata(): importedOrRaised is no stratum column")

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
