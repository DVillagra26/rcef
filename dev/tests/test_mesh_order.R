## mesh_var_levels() / category_cols() - natural size order for the `mesh` column
## ("0", "<16", "16-31", ..., ">=220", "all"/"All" last), wired into every
## plot that can take `mesh` as an axis, fill, colour, shape, group or facet,
## so it never falls back to plain alphabetical order ("100-119" before
## "16-31") wherever it turns up.
##
## Uses the synthetic test fixture (as the other recent plot tests do,
## for portability), with its own `mesh` column overwritten by a synthetic
## set of values chosen so natural and alphabetical order disagree -
## "100-119" sorts before "16-31" and "90-99" alphabetically, but after both
## naturally; that disagreement is what each check below actually proves.

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
suppressPackageStartupMessages(library(ggplot2))
drawable <- function(p)
  !is.null(p) && inherits(p, "ggplot") &&
  !inherits(try(ggplot_build(p), silent = TRUE), "try-error")

test_catch <- fx_catch()
test_dist <- fx_dist()
c23 <- subset(test_catch, year == 2023)
mesh_vals <- c("0", "16-31", "32-69", "70-99", "90-99", "100-119", ">=120")
# Keyed off fleetValue (shared by a stratum's Lan and Dis rows alike), not
# raw row position - a mesh value has to agree within a stratum or discard
# coverage linking (build_coverage_strata(), by domainCatchDis) silently
# finds no match and every rate comes back NA.
mesh_of <- function(fleet)
  mesh_vals[(as.integer(factor(fleet)) - 1) %% length(mesh_vals) + 1]
c23$mesh <- mesh_of(c23$fleetValue)
natural <- mesh_var_levels("mesh", c23$mesh)

## ---- mesh_var_levels(): the ordering rule itself ------------------------------
# mesh_vals is already in natural order by construction, so intersecting it
# with whatever actually turned up (not every one of the 7 values is
# guaranteed a fleetValue to land on) gives the same order a correct
# mesh_var_levels() call must reproduce.
ok(length(natural) >= 4,
  "mesh_var_levels(): the synthetic fixture produced enough distinct mesh values")
ok(identical(natural, intersect(mesh_vals, natural)),
  "mesh_var_levels(): the synthetic fixture sorts by size, not alphabetically")
ok(!identical(natural, sort(mesh_vals)),
  "mesh_var_levels(): natural order actually differs from plain sort() here")

## ---- plot_coverage_bar(): fill = \"mesh\" (the original request) ----------
p1 <- plot_coverage_bar(c23, y = "metier4", fill = "mesh")
ok(drawable(p1), "plot_coverage_bar(): draws with fill = mesh")
ok(identical(levels(p1$data$.fill), natural),
  "plot_coverage_bar(): fill = mesh orders the legend by size")

## ---- plot_coverage_bar(): facet = \"mesh\" ---------------------------------
p1f <- plot_coverage_bar(c23, y = "metier4", facet = "mesh")
ok(drawable(p1f), "plot_coverage_bar(): draws with facet = mesh")
ok(identical(levels(p1f$data$.facet), natural),
  "plot_coverage_bar(): facet = mesh orders the panels by size")

## ---- plot_coverage_grid(): y = \"mesh\" ------------------------------------
p2 <- plot_coverage_grid(c23, y = "mesh", x = "metier4")
ok(drawable(p2), "plot_coverage_grid(): draws with y = mesh")
ok(identical(levels(p2$data$mesh), natural),
  "plot_coverage_grid(): y = mesh orders the rows by size")

## ---- plot_coverage_grid(): x = \"mesh\" -----------------------------------
p3 <- plot_coverage_grid(type = "bubble", c23, y = "metier4", x = "mesh")
ok(drawable(p3), "plot_coverage_grid(): draws with x = mesh")
ok(identical(levels(p3$data$mesh), natural),
  "plot_coverage_grid(): x = mesh orders the columns by size")

## ---- plot_coverage_map(): group = \"mesh\" ----------------------------------
p4 <- quiet(plot_coverage_map(c23, group = "mesh"))
ok(drawable(p4), "plot_coverage_map(): draws with group = mesh")
glab_layer <- p4$layers[[which(vapply(p4$layers,
  function(l) is.data.frame(l$data) && all(c("k", "glab") %in% names(l$data)),
  logical(1)))[1]]]$data
present <- mesh_var_levels("mesh", unique(glab_layer$glab))
ok(identical(glab_layer$k[match(present, glab_layer$glab)], seq_along(present)),
  "plot_coverage_map(): group = mesh places the grid cells in size order")

## ---- plot_DR(): x = \"mesh\" -------------------------------------------------
p5 <- plot_DR(c23, x = "mesh")
ok(drawable(p5), "plot_DR(): draws with x = mesh")
ok(identical(levels(p5$data$mesh), natural),
  "plot_DR(): x = mesh orders the x axis by size")

## ---- plot_DR(), of a raised catch: x = \"mesh\" ---------------------------------
raised <- quiet(fx_raise(c23, fx_groups("own"), type = "discards",
                         log_file = tempfile(fileext = ".txt")))
# per (fleet, area) rather than per metier4: three metiers give three meshes
# whose natural and alphabetical order agree, which would prove nothing
raised$mesh <- mesh_of(paste(raised$fleetValue, raised$areaValue))
p6 <- plot_DR(catch = raised, x = "mesh")
ok(drawable(p6), "plot_DR(): a raised catch draws with x = mesh")
ok(length(levels(p6$data$mesh)) > 3 &&
     identical(levels(p6$data$mesh), mesh_var_levels("mesh", p6$data$mesh)) &&
     !identical(levels(p6$data$mesh), sort(levels(p6$data$mesh))),
  "plot_DR(): a raised catch, x = mesh, orders the points by size, not alphabetically")

## ---- plot_NaLoA(): fill = \"mesh\" -------------------------------------------
p7 <- plot_NaLoA(c23, test_dist, fill = "mesh")
ok(drawable(p7), "plot_NaLoA(): draws with fill = mesh")
ok(identical(levels(p7$data$mesh), mesh_var_levels("mesh", p7$data$mesh)),
  "plot_NaLoA(): fill = mesh orders the legend by size")

## ---- plot_MWaLoA(): colour = \"mesh\" -----------------------------------------
p8 <- plot_MWaLoA(c23, test_dist, colour = "mesh")
ok(drawable(p8), "plot_MWaLoA(): draws with colour = mesh")
ok(identical(levels(p8$data$mesh), mesh_var_levels("mesh", p8$data$mesh)),
  "plot_MWaLoA(): colour = mesh orders the legend by size")

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
