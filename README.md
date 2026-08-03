# rcef

Discard raising and length/age catch allocation over CEF (RDBES)
long-format data, following the ICES workflows.

The package is two halves, and the split is deliberate.

The **analysis functions** are plain R. They build strata from a CEF catch
table, classify sampling status, define raising groups automatically or by
hand, drive the ICES raising and allocation routines, and write the whole
analysis out as a runnable script set. None of it needs Shiny.

The **workbench** is a Shiny app that drives those functions
interactively. It ships inside the package and is a client of it, not the
other way round. An assessment that can only be reproduced by clicking
through an app is not reproducible, so the app's job is to help you decide
and the package's job is to do the work and write down what was done.

## Installation

```r
# install.packages("remotes")
remotes::install_github("USERNAME/rcef")

# with the workbench
remotes::install_github("USERNAME/rcef", dependencies = TRUE)
```

`shiny` and `DT` are Suggests, not Imports: a colleague who only wants to
script the raising should not have to install a web framework to do it.
`dependencies = TRUE` pulls them in; the default does not.

## The ICES routines

`catchRaising()`, `catchAllocationAoL()`, `catchAtAoLAggregation()`,
`funIntercatchCEF()` and `funICoutCEF()` are distributed by ICES as loose
scripts, not as a package, and are **not bundled here**. Bundling a copy
would pin this package to one revision of code ICES revises on its own
schedule, and a stock raised with a stale copy would be wrong in a way
nothing here could detect.

Point the package at your copy:

```r
library(rcef)
set_external_dir("~/ICES/scripts")
load_external()
```

Or once per project, in `.Rprofile`:

```r
options(rcef.external_dir = "~/ICES/scripts")
```

Everything that does not raise or allocate — stratification,
classification, grouping, plotting, script emission — works without them.

## The workbench

```r
rcef::run_app()
```

## Scripting it instead

```r
library(rcef)

catch  <- read.csv("CEF_catches.csv")
catch  <- derive_fields(catch)          # metier4/5/6, ICES area levels, Season

dims   <- c("metier4", "Country", "ICESDivision", "Season")
strata <- classify_strata(build_strata(catch, dims), threshold = 50)

# how much of the stock carries a discard estimate, and where the gaps are
plot_coverage(strata, type = "heatmap", y = "metier4", fill = "Season",
              threshold = 50)

# automatic grouping: a cascade down fleet -> area -> season
rec   <- recommend_raising(strata, hierarchy = c("metier4","ICESDivision","Season"),
                           thresholds = c(50, 50, 50))
conds <- conds_from_auto(rec)

raised <- catchRaising(catch, conds$strataCond, conds$matchedDataCond)
```

## Writing the analysis out

The workbench and the package both emit the session as an ICES TAF script
set — seven files wired together by what they read and write on disk, so a
stage can be re-run on its own:

```
data.R                            read and wrangle; nothing is decided
model_explore_discards.R          coverage and discard rates
model_allocate_raise_discards.R   group definition + catchRaising()
model_explore_length_age.R        length/age sampling coverage
model_allocate_length_age.R       group definition + catchAllocationAoL()
output.R                          the result figures
report.R                          left for the working group
```

```r
bundle <- taf_bundle(ctx, kept_plots)
taf_write(bundle, "analysis/")
```

`model_allocate_length_age.R` reads `model/catch_data_raised.rds`, not the
unraised table. Numbers at length and age are raised to the total catch,
which only exists once the discards have been raised — so the dependency
is a file the script cannot run without, rather than a comment asking you
to remember.

## Development

```r
devtools::load_all()      # develop against the source
devtools::document()      # regenerate NAMESPACE and man/ from roxygen
devtools::check()         # R CMD check
```

## Licence

MIT.
