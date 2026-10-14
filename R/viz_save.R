# =====================================================================
#  viz_save.R   (the `save` argument of the plot functions)
#
#  ORIGIN: this project. ggplot2 only, no shiny.
#
#  Every user-facing plot function takes `save = NULL`. The one line
#  save_on_exit(save, "plot_x") at the top of its body validates it and,
#  when a place was given, registers the write on the function's way OUT -
#  where the plot being returned is known, whichever of the function's
#  several return points it leaves by. A plot that was not drawn (NULL, or
#  an error on the way) is not written.
# =====================================================================

#' Saving a plot as it is drawn
#'
#' @description
#' Every plot function takes a `save` argument. Left as `NULL` the plot is only
#' returned. Give it a place to write to and the plot is written to a file as
#' well, with the size and resolution you ask for, and returned all the same -
#' so it still prints and can be added to.
#'
#' @details
#' `save` is one of:
#'
#' * a **folder**, `save = "report/figures"`: the plot is written there, named
#'   after its title plus a short code that tells apart plots with the same
#'   title (a different `fill` or `facet`, say). Drawing the same plot again
#'   writes the same file;
#' * a **file name** with an extension, `save = "report/figures/rates.png"`:
#'   that file. The extension (`png`, `pdf`, `svg`, `jpeg`, `tiff`, `bmp`)
#'   chooses the format;
#' * a **list** of options, named as in [ggplot2::ggsave()]: `path` (a folder or
#'   a file name, as above), `filename`, `width`, `height`, `units`, `dpi`,
#'   `device` and `bg`.
#'
#' Unless you say otherwise the plot is 10 x 7 inches at 300 dpi on a white
#' background, as a png. `units` is `"in"`, `"cm"`, `"mm"` or `"px"`: with
#' `"px"`, `width` and `height` are the pixels of the image itself, whatever the
#' `dpi`. The folder is created when it does not exist, and a file that is
#' already there is overwritten. A folder whose name contains a dot is read as a
#' file name: give it as `list(path = "report.v2", filename = "rates.png")`.
#'
#' @param save Where to write the plot as well, or `NULL` (default) to only
#'   return it: a folder, a file name, or a list of options - see
#'   [saving-plots].
#' @seealso [ggplot2::ggsave()], which does the writing.
#' @name saving-plots
#' @md
#' @examples
#' \dontrun{
#' fig <- file.path(tempdir(), "figures")
#'
#' # into a folder, named after the plot
#' plot_DR(catch, x = "metier4", save = fig)
#'
#' # to a file, 6 x 4 inches at 150 dpi
#' plot_DR(catch, x = "metier4",
#'         save = list(path = file.path(fig, "rates.png"),
#'                     width = 6, height = 4, dpi = 150))
#'
#' # exactly 1600 x 900 pixels
#' plot_DR(catch, x = "metier4",
#'         save = list(path = fig, filename = "rates_wide.png",
#'                     width = 1600, height = 900, units = "px"))
#'
#' list.files(fig)
#' }
NULL

# Validates a plot function's `save` argument and, when there is something to
# write, registers the write to run as the calling function exits (see the
# header): the writer is defined here and handed to the exit expression as the
# function itself. Returns the normalised options, invisibly, or NULL.
#' @noRd
#' @export
save_on_exit <- function(save, fn = "plot", frame = parent.frame()) {
  if (is.null(save) || identical(save, FALSE)) return(invisible(NULL))
  bad <- function(...) stop(fn, "(): `save` ", ..., call. = FALSE)
  opts <- c("path", "filename", "width", "height", "units", "dpi", "device", "bg")
  exts <- c("png", "pdf", "svg", "jpeg", "jpg", "tiff", "tif", "bmp")
  if (identical(save, TRUE))
    bad("needs somewhere to write to: a folder or a file name, as in ",
        "save = \"figures\"")
  if (is.character(save) && length(save) == 1L && !is.na(save) && nzchar(save))
    save <- list(path = save)
  if (!is.list(save) || (length(save) && (is.null(names(save)) || !all(nzchar(names(save))))))
    bad("is a folder or a file name, or a named list of options (",
        paste(opts, collapse = ", "), ")")
  unknown <- setdiff(names(save), opts)
  if (length(unknown))
    bad("has no option ", paste0("'", unknown, "'", collapse = ", "),
        " - it takes ", paste(opts, collapse = ", "))
  one_string <- function(v) is.character(v) && length(v) == 1L && !is.na(v) && nzchar(v)
  one_number <- function(v) is.numeric(v) && length(v) == 1L && is.finite(v) && v > 0
  for (nm in c("path", "filename", "device", "bg"))
    if (!is.null(save[[nm]]) && !one_string(save[[nm]]))
      bad("option '", nm, "' is a single string")
  for (nm in c("width", "height"))
    if (!is.null(save[[nm]]) && !one_number(save[[nm]]))
      bad("option '", nm, "' is a single positive number")
  if (!is.null(save$device) && !save$device %in% exts)
    bad("option 'device' is one of ", paste(exts, collapse = ", "))
  units <- if (is.null(save$units)) "in" else save$units
  if (!(one_string(units) && units %in% c("in", "cm", "mm", "px")))
    bad("option 'units' is \"in\", \"cm\", \"mm\" or \"px\"")
  dpi <- if (is.null(save$dpi)) 300 else save$dpi
  if (!(one_number(dpi) || (one_string(dpi) && dpi %in% c("screen", "print", "retina"))))
    bad("option 'dpi' is a positive number, or \"screen\", \"print\" or \"retina\"")

  # `path` is a folder - or, with an extension and no `filename`, the file itself
  path <- if (is.null(save$path)) "." else path.expand(save$path)
  file <- save$filename
  ext  <- tolower(tools::file_ext(path))
  if (is.null(file) && nzchar(ext)) {
    if (!ext %in% exts)
      bad("file name ends in '.", ext, "', which is not a format it writes (",
          paste(exts, collapse = ", "), "); for a folder with a dot in its name ",
          "use list(path = <folder>, filename = <file>)")
    file <- basename(path); path <- dirname(path)
  }
  if (!is.null(file)) {
    fext <- tolower(tools::file_ext(file))
    if (nzchar(fext) && !fext %in% exts)
      bad("file name ends in '.", fext, "', which is not a format it writes (",
          paste(exts, collapse = ", "), ")")
    # a name with no extension gets the device's, png unless said otherwise
    if (!nzchar(fext))
      file <- paste0(file, ".", if (is.null(save$device)) "png" else save$device)
  }
  spec <- list(dir = path, file = file,
               width = if (is.null(save$width)) 10 else save$width,
               height = if (is.null(save$height)) 7 else save$height,
               units = units, dpi = dpi, device = save$device,
               bg = if (is.null(save$bg)) "white" else save$bg)
  # Writes the plot as normalised above. Called from the calling function's exit,
  # so it must never fail a function that returned nothing: a plot that was not
  # drawn is skipped.
  write_plot <- function(p, spec, fn = "plot") {
    if (is.null(spec) || !inherits(p, "ggplot")) return(invisible(p))
    file <- spec$file
    if (is.null(file)) {
      # named after the title; the code is made from what the plot says and
      # from what it draws (the built layers: positions, colours, sizes), so two
      # plots with one title - another fill, another facet - get two files, and
      # the same plot gets the same one again
      lab  <- function(x) if (is.null(x)) "" else paste(unlist(x), collapse = " ")
      built <- tryCatch(ggplot2::ggplot_build(p)$data, error = function(e) list())
      lay  <- vapply(built, function(d) {
        num <- sum(unlist(lapply(d, function(v) if (is.numeric(v)) sum(v, na.rm = TRUE) else 0)))
        txt <- sort(unique(unlist(lapply(d, function(v)
          if (is.character(v) || is.factor(v)) as.character(v) else NULL))))
        paste(nrow(d), paste(names(d), collapse = ","), signif(num, 10),
              paste(txt, collapse = ";"), sep = "/")
      }, "")
      desc <- paste(c(lab(p$labels), lay), collapse = "|")
      a <- 1; s <- 0
      for (v in utf8ToInt(desc)) { a <- (a + v) %% 65521; s <- (s + a) %% 65521 }
      stem <- gsub("^-+|-+$", "", gsub("[^A-Za-z0-9]+", "-", tolower(lab(p$labels$title))))
      if (!nzchar(stem)) stem <- gsub("_", "-", fn)
      file <- sprintf("%s_%06x.%s", stem, (s * 65521 + a) %% 16777216,
                      if (is.null(spec$device)) "png" else spec$device)
    }
    dir.create(spec$dir, recursive = TRUE, showWarnings = FALSE)
    out <- file.path(spec$dir, file)
    ggplot2::ggsave(out, plot = p, device = spec$device, width = spec$width,
                    height = spec$height, units = spec$units, dpi = spec$dpi,
                    bg = spec$bg)
    message(fn, "(): saved ", out)
    invisible(p)
  }
  do.call(on.exit,
          list(bquote({ .rcef_plot <- returnValue(); .(write_plot)(.rcef_plot, .(spec), .(fn)) }),
               TRUE),
          envir = frame)
  invisible(spec)
}
