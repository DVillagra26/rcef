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
#' @section The ICES routines:
#' `catchRaising()`, `catchAllocationAoL()`, `catchAtAoLAggregation()`,
#' `funIntercatchCEF()` and `funICoutCEF()` are **bundled**, so the package
#' works out of the box with nothing else to install or configure.
#'
#' They are a SNAPSHOT. ICES revises these scripts on its own schedule, and
#' a stock raised with a stale copy would be wrong in a way nothing here
#' could detect. [ices_routine_status()] says what you have;
#' [use_external_ices()] points at a newer copy on disk and says loudly
#' which routines it replaced.
#'
#' They are also not this package's work. See `inst/scripts/README_ices_source.txt`
#' and the `Authors@R` `ctb` entry; check the licence before redistributing.
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
#' @importFrom tibble tibble as_tibble tribble
NULL

# ----------------------------------------------------------
#  Legacy accessors
# ----------------------------------------------------------
# The routines used to be sourced from a folder, so the app and older
# scripts call set_external_dir() before anything else. They are bundled
# now, so these do nothing but record a path and forward to the override.
# Kept rather than removed: silently deleting a function that appears in
# somebody's saved script buys nothing, and a deprecation message tells
# them what to use instead.

.rcef_env <- new.env(parent = emptyenv())

#' Point the package at a folder of ICES scripts (superseded)
#'
#' The ICES routines are bundled now, so this is no longer needed. Kept so
#' existing scripts keep running.
#'
#' @param path Folder holding the ICES `.R` files.
#' @return The path, invisibly.
#' @seealso [use_external_ices()], [ices_routine_status()]
#' @export
set_external_dir <- function(path) {
  if (!dir.exists(path)) stop("no such folder: ", path)
  assign("external_dir", normalizePath(path), envir = .rcef_env)
  invisible(path)
}

#' The override folder, if one was set (superseded)
#'
#' @return A path, or `NULL`.
#' @seealso [use_external_ices()]
#' @export
external_dir <- function() {
  if (exists("external_dir", envir = .rcef_env, inherits = FALSE))
    return(get("external_dir", envir = .rcef_env))
  getOption("rcef.external_dir", default = NULL)
}

#' Source ICES routines from disk (superseded)
#'
#' Forwards to [use_external_ices()]. With no folder set it now does
#' nothing and says so, rather than failing: the routines are already here.
#'
#' @param dir Folder to read from; defaults to [external_dir()].
#' @param required Ignored; kept for call compatibility.
#' @return The status table from [ices_routine_status()], invisibly.
#' @seealso [use_external_ices()], [ices_routine_status()]
#' @export
load_external <- function(dir = external_dir(), required = NULL) {
  if (is.null(dir)) {
    message("The ICES routines ship with rcef - nothing to load. ",
            "Use ices_routine_status() to see what is available, or ",
            "use_external_ices(dir) to override with a newer copy.")
    return(invisible(ices_routine_status()))
  }
  use_external_ices(dir)
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
#' @param external_dir Optional folder of ICES scripts to override the
#'   bundled ones with. `NULL` (the default) uses what ships with rcef.
#' @param ... Passed to [shiny::runApp()], e.g. `launch.browser`, `port`.
#' @return Does not return; runs until the app is stopped.
#' @export
#' @examples
#' \dontrun{
#' rcef::run_app()
#' rcef::run_app(external_dir = "~/ICES/scripts", launch.browser = TRUE)
#' }
run_app <- function(external_dir = NULL, ...) {
  for (p in c("shiny", "DT")) {
    if (!requireNamespace(p, quietly = TRUE))
      stop("run_app() needs the '", p, "' package.\n",
           "  install.packages(c(\"shiny\", \"DT\"))")
  }
  # only ever an override now; the bundled routines are already loaded
  if (!is.null(external_dir)) use_external_ices(external_dir)
  app <- system.file("app", package = "rcef")
  if (!nzchar(app))
    stop("the bundled app is missing - reinstall rcef")
  shiny::runApp(app, ...)
}
