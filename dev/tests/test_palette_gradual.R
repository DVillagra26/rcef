## PAL_GRADUAL / palette = "gradual" - ColorBrewer's 12-colour "Paired"
## palette, an alternative to PAL_BASE/PAL_CB for a variable whose levels
## already have a natural order (mesh size bins, in particular - see
## test_mesh_order.R) and where that order is worth keeping visible in the
## colour, rather than just telling levels apart.

FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}
err <- function(expr) tryCatch({ force(expr); "no error" },
                               error = function(e) conditionMessage(e))
warn <- function(expr) tryCatch({ force(expr); "no warning" },
                                warning = function(w) conditionMessage(w))
suppressPackageStartupMessages(library(ggplot2))
drawable <- function(p)
  !is.null(p) && inherits(p, "ggplot") &&
  !inherits(try(ggplot_build(p), silent = TRUE), "try-error")
fill_colours <- function(p) {
  bd <- ggplot_build(p)$data
  unique(unlist(lapply(bd, function(l)
    if ("fill" %in% names(l)) as.character(l$fill))))
}

## ---- PAL_GRADUAL itself ----------------------------------------------
ok(length(PAL_GRADUAL) == 12, "PAL_GRADUAL: 12 colours")
ok(all(grepl("^#[0-9A-Fa-f]{6}$", PAL_GRADUAL)),
  "PAL_GRADUAL: every entry is a valid hex colour")
ok(!anyDuplicated(PAL_GRADUAL), "PAL_GRADUAL: no repeated colour")

## ---- var_palette(): picks PAL_GRADUAL, in order -----------------------
reset_palettes()
pg <- var_palette("x", c("c", "a", "b"), palette = "gradual")
ok(identical(unname(pg), PAL_GRADUAL[1:3]),
  "var_palette(): palette = \"gradual\" draws from PAL_GRADUAL, in order")
ok(identical(names(pg), c("a", "b", "c")),
  "var_palette(): levels are still sorted, same as \"default\"/\"colorblind\"")
ok(!identical(err(var_palette("x", "a", palette = "nope")), "no error"),
  "var_palette(): an unknown palette is still rejected")

## ---- scale_fill_var() / scale_colour_var() -----------------------------
reset_palettes()
d_sc <- data.frame(g = c("a", "b", "a"), v = c(1, 2, 3))
p_sf <- ggplot(d_sc, aes(g, v, fill = g)) + geom_col() +
  scale_fill_var("g", d_sc$g, palette = "gradual")
ok(drawable(p_sf), "scale_fill_var(): palette = \"gradual\" builds a working scale")
ok(all(fill_colours(p_sf) %in% PAL_GRADUAL),
  "scale_fill_var(): the scale's colours come from PAL_GRADUAL")
p_sc <- ggplot(d_sc, aes(g, v, colour = g)) + geom_point() +
  scale_colour_var("g", d_sc$g, palette = "gradual")
ok(drawable(p_sc), "scale_colour_var(): palette = \"gradual\" builds a working scale")

## ---- a real plot: plot_coverage_bar(fill = mesh, palette = "gradual") --
data(anchoveta_catch)
c23 <- subset(anchoveta_catch, year == 2023)
mesh_vals <- c("0", "16-31", "32-69", "70-99", "90-99", "100-119", ">=120")
c23$mesh <- mesh_vals[(as.integer(factor(c23$fleetValue)) - 1) %%
                        length(mesh_vals) + 1]
reset_palettes()
pb <- plot_coverage_bar(c23, y = "metier4", fill = "mesh", palette = "gradual")
ok(drawable(pb), "plot_coverage_bar(): draws with palette = \"gradual\"")
ok(all(fill_colours(pb) %in% PAL_GRADUAL),
  "plot_coverage_bar(): every bar colour comes from PAL_GRADUAL")

## ---- heat/bubble: a fixed semantic scale, so any non-default palette ---
## warns exactly as "colorblind" already did - not just that one value
reset_palettes()
w <- warn(plot_coverage_heat(c23, y = "metier4", x = "mesh",
                             palette = "gradual"))
ok(grepl("does not recolour", w),
  "plot_coverage_heat(): palette = \"gradual\" warns, same as \"colorblind\"")

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
