## plot_raised_catches() (renamed from plot_raised_composition()) and the new
## plot_raised_DR() - the after-raising counterparts of plot_DR().
##
## Uses the bundled anchoveta example and run_native_raising() (as
## test_anchoveta_example.R does) rather than a local-machine-only CEF file,
## so this runs anywhere. plot_raised_DR() takes the raised catch and nothing
## else: one point per raw stratum, solid when its discards were imported and
## transparent when the raising added them.

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
suppressPackageStartupMessages(library(ggplot2))
lf <- tempfile(fileext = ".txt")

drawable <- function(p)
  !is.null(p) && inherits(p, "ggplot") &&
  !inherits(try(ggplot_build(p), silent = TRUE), "try-error")
has_geom <- function(p, cls)
  any(vapply(p$layers, function(l) inherits(l$geom, cls), logical(1)))

data(anchoveta_catch)
c23 <- subset(anchoveta_catch, year == 2023)
dims <- c("metier4", "ICESDivision", "seasonValue")
grp <- quiet(define_raising_groups_auto(c23, hierarchy = dims, thresholds = c(50, 40, 30)))
raised <- quiet(run_native_raising(c23, dims, grp, type = "discards", log_file = lf))

## ---- plot_raised_catches(): the rename ----------------------------------
p1 <- plot_raised_catches(raised_catch = raised, x = "metier4")
ok(drawable(p1), "plot_raised_catches(): draws with the renamed args")
ok(grepl("^Raised catch per stratum$", p1$labels$title),
   "plot_raised_catches(): unfiltered title says 'Raised catch'")

p1_dis <- plot_raised_catches(raised_catch = raised, x = "metier4",
                              catchCategory = "Dis")
ok(drawable(p1_dis), "plot_raised_catches(): catchCategory = 'Dis' draws")
ok(identical(sort(unique(p1_dis$data$.cat)), "Discards"),
   "plot_raised_catches(): catchCategory = 'Dis' keeps only discards")
ok(grepl("^Raised discards per stratum$", p1_dis$labels$title),
   "plot_raised_catches(): title follows the catchCategory filter")

## ---- plot_raised_DR(): required args and validation ----------------------
ok(inherits(try(plot_raised_DR(raised), silent = TRUE), "try-error"),
   "plot_raised_DR(): `x` is required")
ok(inherits(try(plot_raised_DR(raised, x = "nope"), silent = TRUE), "try-error"),
   "plot_raised_DR(): a column the raised catch lacks is refused")
ok(inherits(try(plot_raised_DR(c23, x = "metier4"), silent = TRUE), "try-error"),
   "plot_raised_DR(): a catch that was never raised (no importedOrRaised) is refused")
ok(inherits(try(plot_raised_DR(raised_catch = raised, catch_data = c23, x = "metier4"),
                silent = TRUE), "try-error"),
   "plot_raised_DR(): the raised catch is all it takes - catch_data is not an argument")

## ---- plot_raised_DR(): the plot itself -----------------------------------
p2 <- plot_raised_DR(raised_catch = raised, x = "metier4")
ok(drawable(p2), "plot_raised_DR(): draws")
ok(identical(p2$labels$title, "Discard ratio per stratum, raised"),
   "plot_raised_DR(): default rate is ratio, named in the title")
ok(!has_geom(p2, "GeomErrorbar"),
   "plot_raised_DR(): no weighted-mean line (unlike plot_DR())")
ok(!has_geom(p2, "GeomHline"),
   "plot_raised_DR(): no reference line when `reference` is not set")
ok(sum(vapply(p2$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1))) == 1,
   "plot_raised_DR(): one point layer, the strata of the raised catch - no donor layer")
ok(min(p2$data$landings) >= max(p2$data$landings) * (0.1 / 1.1) - 1e-9,
   "plot_raised_DR(): bubble size is shifted up, same as plot_DR()")
ok(length(unique(p2$data$landings)) > 1,
   "plot_raised_DR(): the shift is additive - strata with different landings still differ")

p2_rate <- plot_raised_DR(raised_catch = raised, x = "metier4",
                          rate = "rate", reference = 0.2)
ok(identical(p2_rate$labels$title, "Discard rate per stratum, raised"),
   "plot_raised_DR(): rate = \"rate\" is named in the title")
ok(has_geom(p2_rate, "GeomHline"),
   "plot_raised_DR(): the reference line draws when `reference` is set")

## ---- plot_raised_DR(): raw strata, imported solid and raised transparent --
## every point is one stratum and is either imported or raised: a two-level
## status, not a share between 0 and 1 (so no "share raised" anywhere)
ok(is.factor(p2$data$.status) && identical(levels(p2$data$.status), c("Imported", "Raised")) &&
     !anyNA(p2$data$.status),
   "plot_raised_DR(): every point is Imported or Raised")
ok(any(p2$data$.status == "Imported") && any(p2$data$.status == "Raised"),
   "plot_raised_DR(): this example both samples some strata and raises others")
ok(!".raised_share" %in% names(p2$data) &&
     !grepl("share", p2$labels$caption, ignore.case = TRUE) &&
     is.null(p2$scales$get_scales("alpha")$name),
   "plot_raised_DR(): no 'share raised' - not in the data, the caption or a legend title")
b2 <- ggplot_build(p2)$data[[1]]
ok(setequal(round(unique(b2$alpha), 2), c(1, 0.45)),
   "plot_raised_DR(): imported points are solid (alpha 1), raised ones transparent (0.45)")
ok(max(which(p2$data$.status == "Raised")) < min(which(p2$data$.status == "Imported")),
   "plot_raised_DR(): raised points are drawn first, so no solid point is buried under one")

# one point per raw stratum (fleet x area x country x season with landings)
wg <- raised$originType %in% "WGEstimate" & raised$variableType %in% "WeightLive"
skey <- function(d) paste(d$fleetValue, d$areaValue, d$vesselFlagCountry, d$seasonValue)
lan <- raised[wg & raised$catchCategory == "Lan", ]
n_ref <- sum(tapply(lan$total, skey(lan), sum) > 0)
ok(nrow(p2$data) == n_ref,
   "plot_raised_DR(): one point per raw stratum of the raised catch")

# a point's status and ratio are those of its own rows
dis <- raised[wg & raised$catchCategory == "Dis", ]
raised_keys <- unique(skey(dis)[dis$importedOrRaised == "raised"])
ok(identical(p2$data$.status == "Raised", skey(p2$data) %in% raised_keys),
   "plot_raised_DR(): a stratum is Raised exactly when its discard rows were raised")
i <- which(p2$data$.status == "Imported" & p2$data$.discards > 0)[1]
k <- skey(p2$data)[i]
y_ref <- sum(dis$total[skey(dis) == k]) / sum(lan$total[skey(lan) == k]) * 100
ok(isTRUE(all.equal(p2$data$.y[i], y_ref)),
   "plot_raised_DR(): a sampled stratum's point is its own discards / landings")
j <- which(p2$data$.status == "Raised" & p2$data$.discards > 0)[1]
k2 <- skey(p2$data)[j]
y_ref2 <- sum(dis$total[skey(dis) == k2]) / sum(lan$total[skey(lan) == k2]) * 100
ok(isTRUE(all.equal(p2$data$.y[j], y_ref2)),
   "plot_raised_DR(): a raised stratum's point is its raised discards / landings")

# colour, shape and facet each take a column of the raised catch
p3 <- plot_raised_DR(raised, x = "DrGroup", colour = "metier4",
                     shape = "seasonValue", facet = "ICESDivision")
ok(drawable(p3), "plot_raised_DR(): x = DrGroup with colour, shape and facet")
p3b <- plot_raised_DR(raised, x = "DrGroup", jitter = FALSE)
ok(drawable(p3b), "plot_raised_DR(): jitter = FALSE")

## a raised stratum whose group had no donor comes out of the raising as NaN:
## no rate to draw, so it is left out - and the reader is told
r_nan <- raised
j <- which(p2$data$.status == "Raised" & p2$data$.discards > 0)[1]
k_nan <- skey(p2$data)[j]
hit <- which(wg & r_nan$catchCategory == "Dis" & skey(r_nan) == k_nan)
r_nan$total[hit] <- NaN
msgs <- character(0)
p_nan <- withCallingHandlers(plot_raised_DR(r_nan, x = "metier4"),
                             message = function(m) {
                               msgs <<- c(msgs, conditionMessage(m))
                               invokeRestart("muffleMessage")
                             })
ok(drawable(p_nan) && nrow(p_nan$data) == nrow(p2$data) - 1 &&
     !any(skey(p_nan$data) == k_nan),
   "plot_raised_DR(): a raised stratum with no rate (NaN) is left out, not drawn at 0")
ok(any(grepl("no rate", msgs)),
   "plot_raised_DR(): and says so")

## ---- importedOrRaised: the shade by default, or the fill, the facet, the x ----
## It is a variable like any other: drawn once - as the shade unless it is named
## in fill / colour / shape / facet / x - and alpha = NULL draws no shade.
S <- "importedOrRaised"
near <- function(a, b) isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = 1e-9))
shaded <- function(p) !is.null(p$scales$get_scales("alpha"))
n_panels <- function(p) nrow(ggplot_build(p)$layout$layout)

# plot_raised_catches()
pc0 <- plot_raised_catches(raised, x = "metier4")
by_src <- tapply(pc0$data$t, pc0$data$src, sum)
ok(shaded(pc0), "plot_raised_catches(): by default imported / raised is the shade")
ok(inherits(plot_raised_catches(raised, "metier4", "vesselFlagCountry")$facet, "FacetWrap"),
   "plot_raised_catches(): x and facet are still the first two arguments (the app calls it that way)")
pcf <- plot_raised_catches(raised, x = "metier4", fill = S)
ok(drawable(pcf) && !shaded(pcf) && is.factor(pcf$data[[S]]) &&
     identical(levels(pcf$data[[S]]), c("imported", "raised")) &&
     near(tapply(pcf$data$t, pcf$data[[S]], sum), by_src),
   "plot_raised_catches(): fill = importedOrRaised - a colour each, no shade, the same weights")
ok(grepl("color = importedOrRaised", pcf$labels$caption, fixed = TRUE) &&
     !grepl("shade =", pcf$labels$caption, fixed = TRUE),
   "plot_raised_catches(): ... and the caption says what the colour is")
pcx <- plot_raised_catches(raised, x = "metier4", facet = S)
ok(drawable(pcx) && !shaded(pcx) && inherits(pcx$facet, "FacetWrap") && n_panels(pcx) == 2 &&
     near(tapply(pcx$data$t, pcx$data[[S]], sum), by_src),
   "plot_raised_catches(): facet = importedOrRaised - a panel each, no shade, the same weights")
pcb <- plot_raised_catches(raised, x = S)
ok(drawable(pcb) && !shaded(pcb) && setequal(unique(pcb$data$.x), c("imported", "raised")) &&
     near(tapply(pcb$data$t, pcb$data$.x, sum), by_src),
   "plot_raised_catches(): x = importedOrRaised - a bar each, no shade, the same weights")
pcn <- plot_raised_catches(raised, x = "metier4", alpha = NULL)
ok(drawable(pcn) && !shaded(pcn) && near(sum(pcn$data$t), sum(by_src)) &&
     grepl("added together", pcn$labels$caption, fixed = TRUE),
   "plot_raised_catches(): alpha = NULL - no shade, imported and raised added together")
pcm <- plot_raised_catches(raised, x = "metier4", fill = "vesselFlagCountry", catchCategory = "Dis")
ok(drawable(pcm) && shaded(pcm) && near(sum(pcm$data$t), sum(p1_dis$data$t)) &&
     identical(pcm$scales$get_scales("fill")$name, "vesselFlagCountry"),
   "plot_raised_catches(): another fill column keeps the shade, and the weights")
ok(inherits(try(plot_raised_catches(raised, alpha = "metier4"), silent = TRUE), "try-error") &&
     inherits(try(plot_raised_catches(raised, fill = "nope"), silent = TRUE), "try-error"),
   "plot_raised_catches(): an alpha that is not importedOrRaised, or a column it lacks, is refused")

# plot_raised_DR()
pd0 <- plot_raised_DR(raised, x = "metier4")
ok(shaded(pd0), "plot_raised_DR(): by default imported / raised is the shade")
same_strata <- function(p) nrow(p$data) == nrow(pd0$data) && near(sort(p$data$.y), sort(pd0$data$.y))
pdc <- plot_raised_DR(raised, x = "metier4", colour = S)
ok(drawable(pdc) && !shaded(pdc) && is.factor(pdc$data[[S]]) &&
     identical(levels(pdc$data[[S]]), c("imported", "raised")) && same_strata(pdc) &&
     identical(pdc$data[[S]] == "raised", pdc$data$.status == "Raised"),
   "plot_raised_DR(): colour = importedOrRaised - a colour each, no shade, the same strata and rates")
pds <- plot_raised_DR(raised, x = "metier4", shape = S)
pdf_ <- plot_raised_DR(raised, x = "metier4", facet = S)
pdx <- plot_raised_DR(raised, x = S)
ok(drawable(pds) && !shaded(pds) && same_strata(pds) &&
     drawable(pdf_) && !shaded(pdf_) && same_strata(pdf_) && n_panels(pdf_) == 2 &&
     drawable(pdx) && !shaded(pdx) && setequal(as.character(pdx$data[[S]]), c("imported", "raised")),
   "plot_raised_DR(): shape, facet and x = importedOrRaised work the same way")
pdn <- plot_raised_DR(raised, x = "metier4", alpha = NULL)
ok(drawable(pdn) && !shaded(pdn) && same_strata(pdn) &&
     grepl("drawn alike", pdn$labels$caption, fixed = TRUE) &&
     !grepl("transparent", pdn$labels$caption, fixed = TRUE),
   "plot_raised_DR(): alpha = NULL - no shade, imported and raised drawn alike")
ok(inherits(try(plot_raised_DR(raised, x = "metier4", alpha = "metier4"), silent = TRUE), "try-error"),
   "plot_raised_DR(): an alpha that is not importedOrRaised is refused")

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
