## The `save` argument every plot function takes (R/viz_save.R).
##
## What is checked, and how: the files are really written and read back - the size of a png
## from its header, the format of the others from their first bytes - because "a file
## appeared" says nothing about the pixels or the format the caller asked for.
##
##  1. A folder, a file name, and a list of options; the default size; the size units.
##  2. The automatic file name: the title, plus a code that tells plots with one title apart
##     and gives the same plot the same name again.
##  3. Formats, the background, a name with no extension.
##  4. What it refuses, before anything is drawn or written.
##  5. A plot that was not drawn (nothing to draw, or an error on the way) is not written.
##  6. Every plot function takes it, and writes.

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
err <- function(expr) tryCatch({ suppressMessages(expr); "" }, error = function(e) conditionMessage(e))
heard <- function(expr) {                       # the messages an expression gave, and its value
  msgs <- character()
  val <- withCallingHandlers(expr, message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") })
  list(value = val, messages = msgs)
}
suppressPackageStartupMessages(library(ggplot2))

png_size <- function(f) {                       # width and height from the IHDR chunk
  b <- readBin(f, "raw", 32)
  c(w = sum(as.integer(b[17:20]) * 256^(3:0)), h = sum(as.integer(b[21:24]) * 256^(3:0)))
}
png_colour_type <- function(f) as.integer(readBin(f, "raw", 32)[26])     # 2 = RGB, 6 = RGB with alpha
magic <- function(f, n) readBin(f, "raw", n)
slug <- function(t) gsub("^-+|-+$", "", gsub("[^A-Za-z0-9]+", "-", tolower(t)))
n_files <- function(d) if (dir.exists(d)) length(list.files(d)) else 0L

root <- file.path(tempdir(), "save_plots_test"); unlink(root, recursive = TRUE)
fresh <- function(name) { d <- file.path(root, name); unlink(d, recursive = TRUE); d }
lf <- tempfile(fileext = ".txt")

test_catch <- fx_catch(); test_dist <- fx_dist()
c23 <- subset(test_catch, year == 2023)
d23 <- subset(test_dist, year == 2023)

## ---- 1. a folder, a file, a list ------------------------------------------------------
p0 <- plot_DR(c23, x = "metier4")
title <- p0$labels$title

d1 <- fresh("none")
r <- plot_DR(c23, x = "metier4", save = NULL)
ok(inherits(r, "ggplot") && !dir.exists(d1) && identical(r$labels$title, title),
   "save = NULL: the plot comes back and nothing is written")
r <- plot_DR(c23, x = "metier4", save = FALSE)
ok(inherits(r, "ggplot"), "save = FALSE is the same as NULL")

d2 <- fresh("folder")
h <- heard(plot_DR(c23, x = "metier4", save = d2))
f2 <- list.files(d2)
ok(length(f2) == 1L && grepl(paste0("^", slug(title), "_[0-9a-f]{6}\\.png$"), f2),
   "a folder: one png, named after the title and a six-character code")
ok(inherits(h$value, "ggplot") && identical(h$value$labels$title, title),
   "... and the plot still comes back, so it prints and can be added to")
ok(any(grepl("^plot_DR\\(\\): saved .*png$", trimws(h$messages))), "... and it says what it wrote")
ok(identical(unname(png_size(file.path(d2, f2))), c(3000, 2100)),
   "the default is 10 x 7 inches at 300 dpi: 3000 x 2100 pixels")
ok(png_colour_type(file.path(d2, f2)) == 2L, "... on a white background (no alpha channel)")

quiet(plot_DR(c23, x = "metier4", save = d2))
ok(identical(list.files(d2), f2), "the same plot again writes the same file, over the old one")
quiet(plot_DR(c23, x = "metier4", colour = "vesselFlagCountry", save = d2))
ok(n_files(d2) == 2L,
   "a plot with the same title but another colour gets a file of its own")
quiet(plot_DR(c23, x = "metier4", colour = "vesselFlagCountry", facet = "seasonValue", save = d2))
ok(n_files(d2) == 3L, "... and so does another facet")

d3 <- fresh("file")
f3 <- file.path(d3, "sub", "rates.png")
quiet(plot_DR(c23, x = "metier4", save = f3))
ok(file.exists(f3) && n_files(file.path(d3, "sub")) == 1L,
   "a file name: that file, in a folder created for it")

f4 <- file.path(fresh("opts"), "small.png")
quiet(plot_DR(c23, x = "metier4", save = list(path = f4, width = 6, height = 4, dpi = 150)))
ok(identical(unname(png_size(f4)), c(900, 600)), "list(width = 6, height = 4, dpi = 150): 900 x 600 pixels")

f5 <- file.path(fresh("px"), "wide.png")
quiet(plot_DR(c23, x = "metier4", save = list(path = dirname(f5), filename = "wide.png",
                                              width = 1600, height = 900, units = "px")))
ok(identical(unname(png_size(f5)), c(1600, 900)), "units = \"px\": exactly the pixels asked for")

f6 <- file.path(fresh("cm"), "cm.png")
quiet(plot_DR(c23, x = "metier4", save = list(path = f6, width = 10, height = 5, units = "cm", dpi = 254)))
ok(identical(unname(png_size(f6)), c(1000, 500)), "units = \"cm\": 10 x 5 cm at 254 dpi is 1000 x 500 pixels")
f7 <- file.path(fresh("mm"), "mm.png")
quiet(plot_DR(c23, x = "metier4", save = list(path = f7, width = 100, height = 50, units = "mm", dpi = 254)))
ok(identical(unname(png_size(f7)), c(1000, 500)), "units = \"mm\": 100 x 50 mm at 254 dpi is 1000 x 500 pixels")

d8 <- fresh("dotted.v2")
quiet(plot_DR(c23, x = "metier4", save = list(path = d8, filename = "rates.png", width = 4, height = 3, dpi = 50)))
ok(file.exists(file.path(d8, "rates.png")),
   "a folder with a dot in its name goes in as list(path = , filename = )")

## ---- 3. formats, background, no extension --------------------------------------------
d9 <- fresh("formats"); small <- list(width = 4, height = 3, dpi = 50)
fmt <- function(file, ...) { quiet(plot_DR(c23, x = "metier4", save = c(list(path = file.path(d9, file)), small, list(...)))); file.path(d9, file) }
ok(identical(as.character(magic(fmt("a.jpg"), 2)), c("ff", "d8")), ".jpg is a jpeg")
ok(identical(rawToChar(magic(fmt("a.pdf"), 4)), "%PDF"), ".pdf is a pdf")
ok(all(file.exists(fmt("a.tiff"), fmt("a.bmp"), fmt("a.svg"))), ".tiff, .bmp and .svg are written too")
ok(identical(unname(png_size(fmt("a.png"))), c(200, 150)), "... and .png at 4 x 3 inches and 50 dpi is 200 x 150")
ok(png_colour_type(fmt("t.png", bg = "transparent")) == 6L, "bg = \"transparent\" gives a png with an alpha channel")
quiet(plot_DR(c23, x = "metier4", save = c(list(path = d9, filename = "plain"), small)))
ok(file.exists(file.path(d9, "plain.png")), "a file name with no extension is a png")
quiet(plot_DR(c23, x = "metier4", save = c(list(path = d9, filename = "plain2", device = "jpeg"), small)))
ok(file.exists(file.path(d9, "plain2.jpeg")), "... or the device's, when device is given")

## ---- 4. what it refuses ---------------------------------------------------------------
d10 <- fresh("refused")
refuse <- function(save) err(plot_DR(c23, x = "metier4", save = save))
ok(grepl("needs somewhere to write", refuse(TRUE)), "save = TRUE: asks for a folder or a file name")
ok(grepl("folder or a file name, or a named list", refuse(5)), "save = 5: not a place")
ok(grepl("has no option 'foo'", refuse(list(foo = 1))), "an option it does not have is named")
ok(grepl("'units'", refuse(list(path = d10, units = "yd"))), "units other than in, cm, mm, px")
ok(grepl("'dpi'", refuse(list(path = d10, dpi = -1))) && grepl("'dpi'", refuse(list(path = d10, dpi = "huge"))),
   "a dpi that is not a positive number, \"screen\", \"print\" or \"retina\"")
ok(grepl("'width'", refuse(list(path = d10, width = 0))), "a width that is not a positive number")
ok(grepl("'device'", refuse(list(path = d10, device = "gif"))), "a device it does not write")
ok(grepl("not a format it writes", refuse(file.path(d10, "x.gif"))), "a file name with a format it does not write")
ok(grepl("not a format it writes", refuse("report.v2")) && grepl("list\\(path = <folder>", refuse("report.v2")),
   "a folder with a dot in its name reads as a file - and the message says how to give it")
ok(grepl("^plot_DR\\(\\)", refuse(TRUE)), "the message names the function that was called")
ok(n_files(d10) == 0L, "nothing is written when the options are refused")

## ---- 5. not drawn, not written ---------------------------------------------------------
d11 <- fresh("notdrawn")
ok(is.null(suppressMessages(plot_DR(c23[0, ], x = "metier4", save = d11))) && n_files(d11) == 0L,
   "nothing to draw: NULL back, no file")
ok(is.null(suppressMessages(plot_coverage_grid(c23[0, ], y = "metier4", x = "vesselFlagCountry", save = d11))) &&
     n_files(d11) == 0L, "... for the coverage plots too")
ok(nzchar(err(plot_DR(c23, x = "nope", save = d11))) && n_files(d11) == 0L,
   "an error on the way: the error comes through and no file is written")

## ---- 6. every plot function takes it, and writes ---------------------------------------
raised  <- quiet(fx_raise(c23, fx_groups("own"),   type = "discards", log_file = lf))
raised2 <- quiet(fx_raise(c23, fx_groups("mixed"), type = "discards", log_file = lf))
alloc <- quiet(catchAllocationAoL(raised, d23, condition_alloc_st_list = list(ALL = rlang::quo(TRUE)),
                                  distributionType = "LengthTotal", verbose = FALSE, logFile = lf))
dc  <- quiet(build_discard_clusters(c23, dims = c("metier4", "ICESDivision", "seasonValue"), k = 3, seed = 7))

plots <- list(
  plot_coverage_grid      = function(s) plot_coverage_grid(c23, y = "metier4", x = "vesselFlagCountry", save = s),
  plot_coverage_bar       = function(s) plot_coverage_bar(c23, y = "metier4", fill = "vesselFlagCountry", save = s),
  plot_DR                 = function(s) plot_DR(c23, x = "metier4", save = s),
  plot_NaLoA              = function(s) plot_NaLoA(c23, d23, fill = "vesselFlagCountry", save = s),
  plot_MWaLoA             = function(s) plot_MWaLoA(c23, d23, colour = "vesselFlagCountry", save = s),
  plot_catches            = function(s) plot_catches(raised, x = "metier4", save = s),
  plot_compare_raised     = function(s) plot_compare_raised(c(raised, raised2), x = "metier4", save = s),
  plot_river              = function(s) plot_river(raised, left_var = "metier4", right_var = "DrGroup", value_var = "total", save = s),
  plot_discard_clusters   = function(s) plot_discard_clusters(dc, save = s),
  plot_discard_cluster_variables = function(s) plot_discard_cluster_variables(dc, save = s))
if (requireNamespace("maps", quietly = TRUE))
  plots$plot_coverage_map <- function(s) plot_coverage_map(c23, save = s)

for (nm in names(plots)) {
  ok("save" %in% names(formals(get(nm))) && identical(formals(get(nm))$save, NULL),
     sprintf("%s(): has a `save` argument, NULL by default", nm))
  d <- fresh(nm)
  p <- suppressWarnings(quiet(plots[[nm]](list(path = d, width = 4, height = 3, dpi = 50))))
  ok(inherits(p, "ggplot") && n_files(d) == 1L && identical(unname(png_size(file.path(d, list.files(d)))), c(200, 150)),
     sprintf("%s(): writes one 200 x 150 png and still returns the plot", nm))
}

## the same functions on a raised catch and on an allocation write a file just the same
variants <- list(
  plot_coverage_grid_bubble = function(s) plot_coverage_grid(c23, y = "metier4", x = "vesselFlagCountry",
                                                             type = "bubble", save = s),
  plot_DR_raised        = function(s) plot_DR(raised, x = "metier4", save = s),
  plot_NaLoA_allocated  = function(s) plot_NaLoA(alloc$catch, alloc$distribution, catchCategory = "Lan",
                                                 fill = "vesselFlagCountry", save = s),
  plot_MWaLoA_allocated = function(s) plot_MWaLoA(alloc$catch, alloc$distribution, catchCategory = "Lan",
                                                  colour = "vesselFlagCountry", save = s))
for (nm in names(variants)) {
  d <- fresh(nm)
  p <- suppressWarnings(quiet(variants[[nm]](list(path = d, width = 4, height = 3, dpi = 50))))
  ok(inherits(p, "ggplot") && n_files(d) == 1L && identical(unname(png_size(file.path(d, list.files(d)))), c(200, 150)),
     sprintf("%s(): writes one 200 x 150 png and still returns the plot", nm))
}

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
