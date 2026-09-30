## plot_raised_catches() (renamed from plot_raised_composition()) and the new
## plot_raised_DR() - the after-raising counterparts of plot_discard_rate().
##
## Uses the bundled anchoveta example and run_native_raising() (as
## test_anchoveta_example.R does) rather than a local-machine-only CEF file,
## so this runs anywhere. `matched_data_cond` is hand-written here, the same
## shape the vignettes write by hand for the vendored catchRaising() path -
## plot_raised_DR() only needs a named list of quosures re-testable against
## `catch_data`, not any particular engine's own grouping representation.

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
dims <- c("metier4", "ICESDivision", "Season")
grp <- quiet(define_raising_groups_auto(c23, hierarchy = dims, thresholds = c(50, 40, 30)))
raised <- quiet(run_native_raising(c23, dims, grp, type = "discards", log_file = lf))

matched_data_cond <- list(
  PS   = rlang::quo(metier4 == "PS"),
  MPT  = rlang::quo(metier4 == "MPT"),
  REST = rlang::quo(TRUE))

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
ok(inherits(try(plot_raised_DR(raised, c23, matched_data_cond), silent = TRUE),
           "try-error"),
   "plot_raised_DR(): `x` is required")
ok(inherits(try(plot_raised_DR(raised, c23, list(1, 2), x = "metier4"),
               silent = TRUE), "try-error"),
   "plot_raised_DR(): an unnamed matched_data_cond is rejected")

## ---- plot_raised_DR(): the plot itself -----------------------------------
p2 <- plot_raised_DR(raised_catch = raised, catch_data = c23,
                     matched_data_cond = matched_data_cond, x = "metier4")
ok(drawable(p2), "plot_raised_DR(): draws")
ok(identical(p2$labels$title, "Discard ratio per stratum, raised"),
   "plot_raised_DR(): default rate is ratio, named in the title")
ok(!has_geom(p2, "GeomErrorbar"),
   "plot_raised_DR(): no weighted-mean line (unlike plot_discard_rate())")
ok(!any(vapply(p2$layers, function(l)
    inherits(l$geom, "GeomHline"), logical(1))),
   "plot_raised_DR(): no reference line when `reference` is not set")
ok(sum(vapply(p2$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1))) == 2,
   "plot_raised_DR(): two point layers - the raised catch and the donor strata")

p2_rate <- plot_raised_DR(raised_catch = raised, catch_data = c23,
                          matched_data_cond = matched_data_cond,
                          x = "metier4", rate = "rate", reference = 0.2)
ok(identical(p2_rate$labels$title, "Discard rate per stratum, raised"),
   "plot_raised_DR(): rate = \"rate\" is named in the title")
ok(has_geom(p2_rate, "GeomHline"),
   "plot_raised_DR(): the reference line draws when `reference` is set")

## the raised layer's own share-raised is a real fraction, and both ends of
## it actually occur (this example both raises some strata and samples
## others directly - see test_anchoveta_example.R)
rs <- p2$data$.raised_share
ok(is.numeric(rs) && all(rs >= 0 & rs <= 1, na.rm = TRUE),
   "plot_raised_DR(): raised-share is a fraction between 0 and 1")
ok(any(rs > 0) && any(rs == 0),
   "plot_raised_DR(): some strata are fully raised, some fully sampled")

## the donor layer (drawn first, so the raised points sit on top of it) is
## its own geom_point with its own data - build_coverage_strata() output
## re-tested from matched_data_cond, not the raised layer's own rows
donor_layer <- p2$layers[[1]]$data
ok(is.data.frame(donor_layer) && nrow(donor_layer) > 0 &&
     "DrGroup" %in% names(donor_layer),
   "plot_raised_DR(): the donor layer is its own non-empty, DrGroup-tagged data")

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
