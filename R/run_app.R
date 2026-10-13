# =====================================================================
#  run_app.R   (the one entry point into the shiny front end)
#
#  ORIGIN: this project.
#  The application itself is NOT package code: it lives in inst/app/ and
#  is never checked by R CMD check. shiny and DT are Suggests, so a
#  headless TAF or Quarto script that only wants catchRaising() does not
#  drag a web framework in.
# =====================================================================

#' Packages the shiny front end needs on top of the package Imports
#' @format A character vector.
#' @export
#' @noRd
RCEF_APP_PKGS <- c("shiny", "DT")

#' Run the RCEF Workbench application
#'
#' @param ... Passed to \code{shiny::runApp()} - e.g. \code{port},
#'   \code{host}, \code{launch.browser}.
#' @param app_dir Folder holding \code{app.R}. Defaults to the copy
#'   installed with the package. Point it at \code{"inst/app"} to run the
#'   working tree.
#' @return Called for its side effect; does not return until the app stops.
#' @export
#' @examples
#' \dontrun{
#' run_app()
#' run_app(port = 4321, launch.browser = FALSE)
#' }
#' @noRd
run_app <- function(..., app_dir = system.file("app", package = "rcef")) {
  miss <- RCEF_APP_PKGS[!vapply(RCEF_APP_PKGS, requireNamespace,
                                logical(1), quietly = TRUE)]
  if (length(miss))
    stop("run_app() needs: ", paste(miss, collapse = ", "),
         "\n  install.packages(c(",
         paste(sprintf('"%s"', miss), collapse = ", "), "))",
         call. = FALSE)
  if (!nzchar(app_dir) || !file.exists(file.path(app_dir, "app.R")))
    stop("No app.R under '", app_dir, "'. Reinstall the package, or pass ",
         "app_dir = \"inst/app\" to run the working tree.", call. = FALSE)
  shiny::runApp(app_dir, ...)
}
