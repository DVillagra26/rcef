# rcef — RCEF Workbench

ICES discard raising and length/age catch allocation over CEF (RDBES)
long-format data. **One package, one repo**: the analysis functions, the
vendored RDBESStockCoord engines, and the shiny application that drives
them.

## Install and run

```r
remotes::install_github("DVillagra26/rcef")
rcef::run_app()
```

Headless — the whole point of the split:

```r
library(rcef)
cef  <- derive_fields(readr::read_csv("catches_CEF_v17.1.csv"))
st   <- classify_strata(build_strata(cef, c("metier4", "ICESDivision")))
grp  <- define_raising_groups_auto(st)
out  <- catchRaising(cef, grp$strataCond, grp$matchedDataCond)
```

No shiny is loaded. `shiny` and `DT` are **Suggests**, pulled in only by
`run_app()`.

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
inst/extdata/          CSV fixtures (was data/)
dev/                   tests, tooling, deploy (.Rbuildignore'd)
```

**MANIFEST.md** maps every file to where it came from. **DROPPED.md**
lists what was removed and why.

## The one rule

> If a function needs `input`, `session`, or returns a shiny tag, it lives
> in `inst/app/`. If it does arithmetic on catch data, it lives in `R/`.

There is deliberately no shiny call anywhere under `R/` except the
Suggests-guarded `shiny::runApp()` inside `run_app()`.

## Developing

```r
source("dev/setup.R")        # pkgload::load_all(".") — no install needed
shiny::runApp("inst/app")    # the app against the working tree
devtools::document()         # regenerate NAMESPACE and man/
```

```sh
Rscript dev/check_syntax.R   # parse() every file; must print DONE, 0 failures
```

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
