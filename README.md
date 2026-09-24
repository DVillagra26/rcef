# rcef — RCEF Workbench

ICES discard raising and length/age catch allocation over CEF (RDBES)
long-format data. **One package, one repo**: the analysis functions, the
vendored RDBESStockCoord engines, and the shiny application that drives
them.

## Install and run

```r
# Restart R first (Session > Restart R) if rcef is already loaded: a loaded
# package cannot be replaced and the install stops with "cannot remove
# earlier installation".
install.packages("remotes")                                    # once
remotes::install_github("DVillagra26/rcef", dependencies = TRUE)

rcef::run_app()
```

`dependencies = TRUE` also installs what is only suggested: `shiny` and `DT`
for the app, `icesVocab` and `icesSD` for InterCatch input. The plots need
ggplot2 >= 3.5.0 and scales >= 1.3.0; the installer updates them if yours
are older.

## Look at the data first

Discard rates and sampling coverage are one table, and the plots draw that
table, so every number in a figure can be read off it:

```r
library(rcef)
data(anchoveta_catch); data(anchoveta_dist)     # the bundled example stock

# discard rate and coverage per stratum, at the aggregation you choose
tab <- build_coverage_strata(anchoveta_catch, c("metier4", "Country"))
tab

# the same numbers as figures (raw catch or the table both work)
plot_coverage_heat(tab, y = "metier4", x = "Country", min_cov = 50)
plot_discard_rate(tab, x = "metier4")

# length and age: add the distributions
build_coverage_strata(anchoveta_catch, c("metier4", "Country"),
                      distributions = anchoveta_dist, what = "length")
```

`?build_coverage_strata` lists every column, `?"coverage-plots"` every plot
argument, and `vignette("coverage-plots", package = "rcef")` draws them all.

## Raise without the app

Headless — the whole point of the split:

```r
library(rcef)
cef <- derive_fields(read.csv("catches_CEF_v17.1.csv"))
cef <- subset(cef, stock == "sol.27.4" & year == 2023)   # one stock and year at a time

dims   <- c("metier4", "ICESDivision", "Season")          # what a stratum is
st     <- classify_strata(build_strata(cef, dims), threshold = 50)  # covered enough?
groups <- define_raising_groups_auto(cef, hierarchy = dims,         # who borrows from whom
                                     thresholds = c(50, 40, 30))
raised <- run_native_raising(cef, dims, groups, type = "discards")
```

No shiny is loaded. Raising uses the discard estimates whose `originType`
matches the `originType` argument (`"WGEstimate"` by default). The bundled
example data is labelled `"Estimate"`, so it is for exploring and plotting,
not for a raising run.

## Layout

```
R/                     THE PACKAGE
  aaa-package.R          package doc, imports, globals
  RDBESStockCoord_*.R    vendored ICES WG code — do not refactor
  rcef_*.R               our analysis logic
  viz_*.R                our theme and plots (ggplot2 only)
  run_app.R              the single entry point into the app
  rcef_status.R          replaces the old external_loader.R
inst/app/              THE APPLICATION — shipped, never R CMD check'ed
  app.R, app_ui_helpers.R, app_mod_intercatch_ui.R, app_state.R
data/                  anchoveta_catch, anchoveta_dist — the example data set
dev/                   tests, tooling, deploy (.Rbuildignore'd)
```

**MANIFEST.md** maps every file to where it came from. **DROPPED.md**
lists what was removed and why.

`vignette("rcef-workflow", package = "rcef")` walks one stock from input
files to numbers and mean weights at length and age, with a table of what
to put in each argument.

## The one rule

> If a function needs `input`, `session`, or returns a shiny tag, it lives
> in `inst/app/`. If it does arithmetic on catch data, it lives in `R/`.

There is deliberately no shiny call anywhere under `R/` except the
Suggests-guarded `shiny::runApp()` inside `run_app()`.

## Developing

```r
source("dev/setup.R")        # pkgload::load_all(".") — no install needed
shiny::runApp("inst/app")    # the app against the working tree
devtools::document()         # regenerate NAMESPACE and man/ after any change
```

```sh
bash dev/check_all.sh        # everything below, in one go

Rscript dev/check_syntax.R   # parse() every file; must print DONE, 0 failures
Rscript dev/check_exports.R  # must print NOT EXPORTED : 0
Rscript dev/check_app.R      # sources the app + renders the UI (needs install)
Rscript dev/check_server.R   # RUNS the server function (needs install)
Rscript dev/check_sections.R # the two manual builders must present the same steps
R CMD build .                # catches DESCRIPTION errors in 2s — run before every push
```

`NAMESPACE` and `man/` are **generated** and committed. Every top-level
object carries `@export` — the functions **and the constants**; the
constants are the ones easily forgotten, and a missing one fails at
runtime with `object 'X' not found`, not at build time. `document()` is
safe to run. Objects that also carry `@noRd` export correctly but have no
man page yet; the functions still to document are listed in
`dev/DOC_BACKLOG.txt`, and replacing a `@noRd` with a real roxygen block
is the unit of work. `dev/check_exports.R` prints the current counts and
fails if a new function is ever added without a tag.

Documentation blocks use `@md` (the package keeps roxygen's Markdown off
globally, so each block opts in), which gives backticks, `[fn()]` links and
pipe tables. Link only to functions that have a man page; write the others
in code font.

## Deploying to shinyapps.io

`dev/deploy_shinyapps.R`. The catch worth knowing before you hit it:
shinyapps.io resolves dependencies from CRAN, and `rcef` is not there. So
install it **from GitHub** locally first — `remotes::install_github()`
writes `RemoteType`/`RemoteUrl` into the installed DESCRIPTION and
`rsconnect` reads them. Install with `devtools::install()` or
`R CMD INSTALL` instead and the deploy fails with *"Unable to determine
package source"*.

## Vendored code

`R/RDBESStockCoord_*.R` is not ours. Function names, argument lists,
defaults and return shapes are upstream's and must not be changed here —
a local rename would silently fork us from the working group. Local edits
are packaging-only and every one carries a `## [pkg]` marker:

```sh
grep -rn '## \[pkg\]' R/     # a complete diff against upstream
```
