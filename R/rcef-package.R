# ============================================================
#  rcef-package.R
#  Package-level documentation, the app launcher, and the imports that
#  make the rest of R/ work without attaching anything.
# ============================================================

#' rcef: discard raising and length/age allocation over CEF data
#'
#' Two halves that are deliberately separable.
#'
#' The **data-processing half** - everything in `R/` - is plain R. It builds
#' strata from a CEF catch table, classifies their sampling status, defines
#' raising groups automatically or by hand, calls the ICES raising and
#' allocation routines, and writes the whole analysis out as a runnable
#' script set. None of it needs Shiny, and all of it can be scripted from a
#' plain R session.
#'
#' The **workbench** is a Shiny app that drives those functions
#' interactively. It ships inside the package and is started with
#' [run_app()]. It is a client of the package, not the other way round: if
#' the app were deleted tomorrow the analysis would still run.
#'
#' That separation is the point. An assessment that can only be reproduced
#' by clicking through an app is not reproducible, so the app's job is to
#' help you decide, and the package's job is to do the work and write down
#' what was done.
#'
#' @section External dependencies:
#' The ICES routines `catchRaising()`, `catchAllocationAoL()`,
#' `catchAtAoLAggregation()`, `funIntercatchCEF()` and `funICoutCEF()` are
#' not on CRAN and are not bundled here. Point [set_external_dir()] at your
#' copy of the ICES scripts before running anything that raises or
#' allocates. Everything else - stratification, classification, grouping,
#' plotting, script emission - works without them.
#'
#' @keywords internal
"_PACKAGE"

## The imports below replace the library() block the script version used.
## Attaching packages from inside a package alters the user's search path
## as a side effect of loading; @importFrom is namespaced and cannot
## collide. Only operators and functions used unqualified across many files
## are imported by name - everything else is called as pkg::fn().
#' @importFrom dplyr across arrange bind_rows case_when count desc distinct
#' @importFrom dplyr filter first full_join group_by group_split inner_join
#' @importFrom dplyr left_join mutate n n_distinct rename row_number select
#' @importFrom dplyr summarise ungroup vars all_of any_of pull last if_else
#' @importFrom dplyr anti_join slice bind_cols tibble
#' @importFrom tidyr pivot_longer pivot_wider complete nesting replace_na
#' @importFrom rlang .data sym syms := %||% enquo quo quo_text eval_tidy
#' @importFrom purrr map map_dfr map_chr map_dbl map_lgl pmap keep discard
#' @importFrom ggplot2 aes after_stat coord_cartesian element_blank
#' @importFrom ggplot2 element_line element_rect element_text expansion
#' @importFrom ggplot2 facet_grid facet_wrap geom_boxplot geom_col geom_line
#' @importFrom ggplot2 geom_point geom_ribbon geom_segment geom_text geom_tile
#' @importFrom ggplot2 ggplot guide_legend guides labs margin position_dodge
#' @importFrom ggplot2 position_jitter position_jitterdodge position_stack
#' @importFrom ggplot2 scale_alpha_manual scale_colour_manual
#' @importFrom ggplot2 scale_fill_manual scale_shape_manual
#' @importFrom ggplot2 scale_size_continuous scale_x_continuous
#' @importFrom ggplot2 scale_x_discrete scale_y_continuous scale_y_discrete
#' @importFrom ggplot2 theme theme_bw ggsave waiver
#' @importFrom stats setNames median quantile sd weighted.mean complete.cases
#' @importFrom stats dnorm density approx
#' @importFrom utils head tail write.csv read.csv zip modifyList
#' @importFrom grDevices col2rgb rgb colorRampPalette png dev.off
#' @importFrom tidyselect where
NULL

# ----------------------------------------------------------
#  Where the ICES scripts live
# ----------------------------------------------------------

#' Pipe operator
#'
#' Re-exported from magrittr so package code and user code use the same one.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value.
#' @param rhs A function call.
#' @return The result of calling `rhs` on `lhs`.
NULL

# The ICES routines are sourced at run time from a folder the user points
# at, so R CMD check cannot see them and reports each as an undefined
# global. Declaring them here says "these are resolved elsewhere, on
# purpose" rather than silencing the check wholesale.
utils::globalVariables(c(
  "catchRaising", "catchAllocationAoL", "catchAtAoLAggregation",
  "meanWoLatAoLAggregation", "funIntercatchCEF", "funICoutCEF",
  "getQuovisData", "getPolarisData"))

.rcef_env <- new.env(parent = emptyenv())

#' Point the package at your copy of the ICES routines
#'
#' `catchRaising()` and friends are distributed as loose scripts, not as a
#' package, so their location differs on every machine. Set it once per
#' session, or once per project in `.Rprofile`.
#'
#' The alternative - bundling a copy - was rejected on purpose: it would
#' pin this package to one revision of code that ICES revises on its own
#' schedule, and a stock raised with a stale copy would be wrong in a way
#' nothing here could detect.
#'
#' @param path Folder holding the ICES `.R` files.
#' @return The path, invisibly.
#' @seealso [external_dir()], [load_external()]
#' @export
#' @examples
#' \dontrun{
#' set_external_dir("~/ICES/scripts")
#' load_external()
#' }
set_external_dir <- function(path) {
  if (!dir.exists(path)) stop("no such folder: ", path)
  assign("external_dir", normalizePath(path), envir = .rcef_env)
  invisible(path)
}

#' The folder the ICES routines are read from
#'
#' Falls back to the `rcef.external_dir` option, so a project can set it in
#' `.Rprofile` and never call [set_external_dir()].
#'
#' @return A path, or `NULL` when none has been set.
#' @seealso [set_external_dir()]
#' @export
external_dir <- function() {
  if (exists("external_dir", envir = .rcef_env, inherits = FALSE))
    return(get("external_dir", envir = .rcef_env))
  getOption("rcef.external_dir", default = NULL)
}

#' Source the ICES routines into the search path
#'
#' @param dir Folder to read from; defaults to [external_dir()].
#' @param required Names of functions that must exist afterwards.
#' @return The functions found, invisibly.
#' @export
load_external <- function(dir = external_dir(),
                          required = c("catchRaising", "catchAllocationAoL")) {
  if (is.null(dir))
    stop("No ICES script folder set. Call set_external_dir(), or set ",
         "options(rcef.external_dir = ...) in your .Rprofile.")
  fs <- list.files(dir, pattern = "\\.R$", full.names = TRUE)
  if (!length(fs)) stop("no .R files in ", dir)
  for (f in fs) try(sys.source(f, envir = globalenv()), silent = TRUE)
  missing <- required[!vapply(required, exists, logical(1))]
  if (length(missing))
    warning("sourced ", length(fs), " files from ", dir,
            " but these are still missing: ", paste(missing, collapse = ", "))
  invisible(setdiff(required, missing))
}

# ----------------------------------------------------------
#  The workbench
# ----------------------------------------------------------

#' Launch the RCEF Workbench
#'
#' Starts the Shiny app bundled in `inst/app`. Shiny and DT are Suggests
#' rather than Imports: the analysis functions do not need them, and a
#' colleague who only wants to script the raising should not have to
#' install a web framework to do it.
#'
#' @param external_dir Folder holding the ICES routines. Defaults to
#'   [external_dir()].
#' @param ... Passed to [shiny::runApp()], e.g. `launch.browser`, `port`.
#' @return Does not return; runs until the app is stopped.
#' @export
#' @examples
#' \dontrun{
#' rcef::run_app()
#' rcef::run_app(external_dir = "~/ICES/scripts", launch.browser = TRUE)
#' }
run_app <- function(external_dir = rcef::external_dir(), ...) {
  for (p in c("shiny", "DT")) {
    if (!requireNamespace(p, quietly = TRUE))
      stop("run_app() needs the '", p, "' package.\n",
           "  install.packages(c(\"shiny\", \"DT\"))")
  }
  if (!is.null(external_dir)) set_external_dir(external_dir)
  app <- system.file("app", package = "rcef")
  if (!nzchar(app))
    stop("the bundled app is missing - reinstall rcef")
  shiny::runApp(app, ...)
}
