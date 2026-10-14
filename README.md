# rcef — RCEF Workbench

ICES discard raising and length/age catch allocation over CEF (RDBES)
long-format data: the analysis and plotting functions, and the vendored
RDBESStockCoord engines they drive.

## Install

```r
# Restart R first (Session > Restart R) if rcef is already loaded: a loaded
# package cannot be replaced and the install stops with "cannot remove
# earlier installation".
install.packages("remotes")                                    # once
remotes::install_github("DVillagra26/rcef", dependencies = TRUE)
```

`dependencies = TRUE` also installs what is only suggested: `icesVocab` and
`icesSD` for InterCatch input. The plots need ggplot2 >= 3.5.0 and scales
>= 1.3.0; the installer updates them if yours are older.

## Look at the data first

Discard rates and sampling coverage are one table, and the plots draw that
table, so every number in a figure can be read off it. `derive_fields()` runs
once, on the raw data; the table and the plots read the columns the catch has
and derive nothing, so a column of your own - a fleet grouping, say - works
beside the derived ones:

```r
library(rcef)
catch         <- derive_fields(read.csv("catches_CEF_v17.1.csv"))      # your CEF catch
distributions <- derive_fields(catch, read.csv("distributions_CEF_v17.1.csv"),
                               what = "distribution")                    # and its distributions

# discard rate and coverage per stratum, at the aggregation you choose
tab <- build_coverage_strata(catch, c("metier4", "vesselFlagCountry"))
tab

# the same numbers as figures (raw catch or the table both work)
plot_coverage_heat(tab, y = "metier4", x = "vesselFlagCountry", min_cov = 50)
plot_DR(tab, x = "metier4", colour = "vesselFlagCountry")

# the discard rate (discards as a share of the catch) instead of the ratio,
# and a dashed reference line that names the strata above 30%
plot_DR(tab, x = "metier4", colour = "vesselFlagCountry",
       rate = "rate", reference = 0.3)

# length and age: add the distributions
build_coverage_strata(catch, c("metier4", "vesselFlagCountry"),
                      distributions = distributions, what = "length")

# mean weight at age, the ages from 3 upward pooled into a "3+" plus group, and
# the fish measured at each age as bars behind the lines
plot_MWaLoA(catch, distributions,
           distributionType = "Age", colour = "vesselFlagCountry",
           plus_group = 3, show_samples = TRUE)
```

Every title says what is sampled and over which weight ("Discard sampling of
landed weight"), and every caption says how the numbers on the figure are
worked out and which columns each mark is grouped over. Every plot takes the
same `palette` argument - `"default"`, `"colorblind"` or `"gradual"` (for
levels with an order, mesh sizes say) - and a level keeps its colour from
plot to plot:

```r
plot_coverage_heat(tab, y = "metier4", x = "vesselFlagCountry", palette = "colorblind")
plot_NaLoA(catch, distributions, fill = "mesh", palette = "gradual")
```

`?build_coverage_strata` lists every column, `?"coverage-plots"` every plot
argument, and `vignette("part1-explore-sampling-coverage", package = "rcef")`
draws them all.

## Raise and allocate

The groups are yours to define: conditions for the strata that are raised together,
and for the data each group takes its discard ratio from.
`check_group_conditions()` tells you if a stratum sits in two groups.

```r
library(rcef)
catch <- derive_fields(read.csv("catches_CEF_v17.1.csv"))
cef <- subset(catch, year == 2023)               # one stock and year at a time

strataCond <- list(TBB  = rlang::quo(metier4 == "TBB"),
                   REST = rlang::quo(metier4 != "TBB"))
matchedDataCond <- list(TBB  = rlang::quo(metier4 == "TBB"),
                        REST = rlang::quo(TRUE))        # the rest draws on every metier

check_group_conditions(cef, strataCond, logFile = NULL)
raised <- catchRaising(cef, strataCond, matchedDataCond, type = "discards")

table(raised$catchCategory, raised$importedOrRaised)   # what was imported, what was raised
```

To look at what the raising did, use the plots you used before it: the catch
weight and the discard rate of the raised catch take the same arguments as
those of the raw catch. What was imported is drawn plain - a circle, a solid
bar - and what the raising added is marked: a cross, a transparent and hatched
bar. The title says what the catch holds: *imported + raised*
(or *only imported*, when nothing was raised):

```r
plot_catches(raised, x = "metier4", facet = "vesselFlagCountry")
plot_DR(raised, x = "DrGroup", colour = "metier4")     # one line per group: the rate it was raised at
```

Raised it more than one way? Hand the raised catches to
`raised_overview_table()` and `plot_compare_raised()` - two or more, written
`c(a, b)` - and they are compared scenario by scenario. A scenario is named
after its object unless you pass `scenarios_names`, and `by` aggregates the
table at any other level:

```r
all <- list(TBB = rlang::quo(TRUE), REST = rlang::quo(TRUE))        # both draw on everything
raised2 <- catchRaising(cef, strataCond, all, type = "discards")

raised_overview_table(c(raised, raised2), by = "seasonValue")   # imported vs raised
plot_compare_raised(c(raised, raised2), x = "metier4", catchCategory = "Dis")
```

Lengths (or ages, the same way) are allocated from the raised catch, and added up into
catch-at-length and mean-weight tables:

```r
distributions <- derive_fields(catch, read.csv("distributions_CEF_v17.1.csv"),
                               what = "distribution")
alloc <- catchAllocationAoL(raised, subset(distributions, year == 2023),
                            condition_alloc_st_list = list(ALL = rlang::quo(TRUE)),
                            distributionType = "LengthTotal")

catchAtAoLAggregation(alloc$distribution, alloc$catch, grouping = "catchCategory",
                      originType_catch = "WGEstimate", variableType_catch = "WeightLive",
                      distributionType = "LengthTotal")
```

And the allocation is looked at with the length and age plots, given both tables of
its result - solid is what was sampled, hatched bars and dotted lines with crosses
what the allocation estimated, and the title says what the distributions hold:
*sampled + estimated* (or *only sampled*, as before the allocation):

```r
plot_NaLoA(alloc$catch, alloc$distribution, catchCategory = "Lan", fill = "vesselFlagCountry")
plot_MWaLoA(alloc$catch, alloc$distribution, catchCategory = "Lan", colour = "sampledOrEstimated")
```

Raising uses the discard estimates whose `originType` matches the `originType`
argument (`"WGEstimate"` by default, or `"Official"`), so that is the value the
rows you want raised must carry.

## Layout

```
R/                     THE PACKAGE
  aaa-package.R          package doc, imports, globals
  RDBESStockCoord_*.R    vendored ICES WG code — do not refactor
  rcef_*.R               our analysis logic
  viz_*.R                our theme and plots (ggplot2 only)
dev/                   tests and tooling (.Rbuildignore'd)
```

**MANIFEST.md** maps every file to where it came from. **DROPPED.md**
lists what was removed and why, the Shiny application included: it was taken out
of the working tree to work on the package alone, and is kept whole under the git tag
`app-archive`.

Three vignettes, all on real CEF data read from `inst/extdata/data_CEF_format`
(git-ignored, so they only build on a machine that has the files - see
*Developing*), in the order to read them (R requires a vignette's file name to
start with a letter, so the number is in the title and after `part`):

| vignette | what it does |
|---|---|
| `part1-explore-sampling-coverage` | 1. explores the data: sampling coverage, discard rates, and the lengths, ages and weights of the sampled strata - every plot family, on brill (discards, lengths) and saithe (ages) |
| `part2-discard-raising` | 2. discard raising with groups defined by hand, the CC, CAR and CAS tables, and two ways of raising compared |
| `part3-length-age-allocation` | 3. the allocation of lengths from the raised catch, what it did to the numbers and mean weights, and the catch-at-length tables |

Open one with `vignette("part1-explore-sampling-coverage", package = "rcef")`,
or run its chunks one by one in RStudio.

## Developing

```r
source("dev/setup.R")        # pkgload::load_all(".") — no install needed
devtools::document()         # regenerate NAMESPACE and man/ after any change
```

```sh
bash dev/check_all.sh        # everything below, plus every suite in dev/tests (needs install)

Rscript dev/check_ascii.R    # R/, vignettes and man/ are 7-bit ASCII
Rscript dev/check_syntax.R   # parse() every file; must print DONE, 0 failures
Rscript dev/check_exports.R  # must print NOT EXPORTED : 0
R CMD build --no-build-vignettes .   # catches DESCRIPTION errors in 2s — run before every push
R CMD build .                        # also runs the three vignettes (~1 min); they read the
                                     # local CEF csv files and stop with an error without them
```

`NAMESPACE` and `man/` are **generated** and committed. Every top-level
object carries `@export` — the functions **and the constants**; the
constants are the ones easily forgotten, and a missing one fails at
runtime with `object 'X' not found`, not at build time. `document()` is
safe to run. Only the functions a user calls directly have a help page
(the table in `?rcef` lists them); every other object carries `@noRd`, which
still exports it but writes no page, since it is plumbing between the
documented ones. `dev/check_exports.R` prints the current counts and fails
if a new function is ever added without a tag.

Documentation blocks use `@md` (the package keeps roxygen's Markdown off
globally, so each block opts in), which gives backticks, `[fn()]` links and
pipe tables. Link only to functions that have a man page; write the others
in code font.

## Vendored code

`R/RDBESStockCoord_*.R` is not ours. Function names, argument lists,
defaults and return shapes are upstream's and must not be changed here —
a local rename would silently fork us from the working group. Local edits
are packaging-only and every one carries a `## [pkg]` marker:

```sh
grep -rn '## \[pkg\]' R/     # a complete diff against upstream
```
