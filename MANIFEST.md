# MANIFEST — where every file came from

Provenance is encoded in the **file name prefix**, so it is visible in
`ls`, in the GitHub file tree, and in every `git log --stat` line.

| prefix | origin | may we refactor? |
|---|---|---|
| `RDBESStockCoord_*` | ICES working-group code, **vendored** | **No.** Fix upstream, re-vendor. Local edits limited to packaging, marked `## [pkg]`. |
| `rcef_*` | this project — analysis / data logic | Yes |
| `viz_*` | this project — theme and plots (ggplot2 only) | Yes |
| `aaa-package.R` | this project — package doc, imports, globals | Yes |

---

## R/ — the package

### Vendored (RDBESStockCoord)

| new file | was | functions |
|---|---|---|
| `RDBESStockCoord_utils.R` | `R/0_Functions_other.R` | `check_group_conditions`, `convert_field`, `unit2ratio` |
| `RDBESStockCoord_raising.R` | `R/0_Functions_discards_raising.R` | `catchRaising`, `grp_catch_raising`, `grp_catch_raising_condition` |
| `RDBESStockCoord_allocation.R` | `R/0_Functions_age-length_alloc.R` | `catchAllocationAoL`, `catchAtAoLAggregation`, `meanWoLatAoLAggregation`, `grp_AoL_alloc_N`, `grp_AoL_alloc_WL`, `grp_AoL_alloc_condition` |
| `RDBESStockCoord_intercatch_in.R` | `R/funIntercatchCEF.R` | `funIntercatchCEF` |
| `RDBESStockCoord_intercatch_out.R` | `R/funICoutCEF.R` | `funICoutCEF` |
| `RDBESStockCoord_relation.R` | `R/funMakeRelation.R` | `funMakeRelation` |

**Packaging-only edits applied (all marked `## [pkg]`, all behaviour-preserving):**

1. In-function `library()` / `require()` removed — attaching a package from
   inside a function is an R CMD check failure and a side effect on the
   user's search path. Dependencies now live in `DESCRIPTION` / `NAMESPACE`.
   (6 call sites: `rlang` ×5, `dplyr` ×3 across three files.)
2. `funMakeRelation()`: unqualified `getCodeTypeRelation`, `setDT`, `dcast`
   → `icesVocab::` / `data.table::`. `icesVocab`/`icesSD` moved to
   `Suggests` with an explicit `requireNamespace()` guard that errors with
   the package name instead of failing obscurely.
3. `funMakeRelation()` still does `assign("stock_relation", .GlobalEnv)` —
   upstream behaviour, unchanged — but now **also** returns the object
   invisibly, so new callers need not read `.GlobalEnv`. Additive only.
4. CRLF → LF.
5. Provenance banner + `@family RDBESStockCoord` on each file.

Three later edits to the vendored files, also marked `## [pkg]`: `distributionType`
takes CEF's own `"LengthTotal"` where upstream says `"Length"`,
`meanWoLatAoLAggregation(weighting = "CATON")` weights by the catch row's total weight
(it looked for a `total` column the function had already dropped), and
`grp_AoL_alloc_N()` / `grp_AoL_alloc_WL()` skip the estimation for a group whose catch rows
all have a sampled distribution (upstream stops there: `1:n()` of no rows) - a stratum that is
sampled well enough is a group of its own in `recommend_groups()`. All three belong upstream.

The roxygen text of the vendored files has additions of ours (no marker: it is not
code): the help pages of `catchRaising()`, `catchAllocationAoL()` and the two
aggregations, and the arguments of `funICoutCEF()`, `funIntercatchCEF()` and
`check_group_conditions()` that upstream leaves undocumented.

**Not changed:** every function name, every argument name, every default,
every return shape, every algorithm.

### Ours

| file | was | what |
|---|---|---|
| `aaa-package.R` | *(new)* | package doc, `@import`, `globals` |
| `rcef_utils.R` | `R_development/0_Functions_utils.R` | `derive_fields()` (the nested fleet and area levels, on the catch or on its distributions), `convert_units()` (the one unit conversion: weights to tonnes, numbers to N, mean weights to g, length classes to mm) |
| `rcef_coverage.R` | *(new)* | `build_coverage_strata()`: the discard-rate / sampling-coverage table (CAR / CAS-style columns, no status) the coverage and discard-rate plots draw, usable on its own - it also holds the link between the catch and the distributions that gives length and age coverage; `coverage_cells()`, which pools that table to the cells a coverage plot draws and words what the figure is about |
| `rcef_groups.R` | *(new; the first versions of the package had `recommend_raising()` for this)* | `recommend_groups()`: chooses the groups of `catchRaising()` and `catchAllocationAoL()` from a hierarchy of variables (from the one that matters most to the one that matters least) and the coverage a pool needs - every stratum takes the narrowest pool that reaches it, on the pooled coverage of `build_coverage_strata()` and `coverage_cells()` - and returns the two lists of conditions (`strataCond`, `matchedDataCond`) with the tables of how the strata fell into the levels (`print.rcef_groups()` prints them) |
| `rcef_length.R` | `R_development/mod_length.R` | the distributions: `prep_distributions()` (one unit, one class scale), the match between catch and distributions through `domainBiology` and the country, `biology_dist_prep()` (the link, the plus-group and length-bin pooling, numbers and mean weight at length / age, before the allocation and after it: the data half of `plot_NaLoA()` and `plot_MWaLoA()`) |
| `rcef_summaries.R` | `R_development/summaries.R` | `raised_overview_table()` and the reading of `c(a, b)` raised catches |
| `rcef_scf.R` | *(new)* | the stock coordination format (SCF v4): `catchCombined()` (CC) and `distributionsCombined()` (DC) - `scf_header()` checks the header (working group year, version, calendar year / start date of a model year) before anything is added up, `scf_sampling()` takes the sampling behind each row (`PSU`, `numPSUs`, `numTrips`, `numMeasurements`) from the data's columns, `scf_input()` / `scf_table()` check what goes in and put every field of the format in its order - and the adding up they are made of - `catchAtAoL()` and `meanWoLatAoL()`, which `catchAtAoLAggregation()` and `meanWoLatAoLAggregation()` (vendored; marked `## [pkg]`) spread over one column per class (`spread_AoL()`); `catch_domain()` gives a catch row with no `domainBiology` the `[alloc]_<season>_<area>_<unit>_<fleet>_<category>` the allocation gives its estimated strata, so the raised catch and the allocation's catch find the distributions alike, and `link_check()` warns about a catch weight with no distribution and a distribution with fish and no catch weight |
| `viz_theme.R` | `R_development/0_Functions_theme.R` | `plot_colours()` (every colour of every plot: three palettes, a level keeps its colour, one status scale), `app_theme()` |
| `viz_plots.R` | `R_development/0_Functions_plots.R` | the public plot functions (ggplot only), the catch and the distributions each looked at before a step and after it by the same function (`plot_catches()`, `plot_DR()`, `plot_NaLoA()`, `plot_MWaLoA()`); `coverage_grid()`, the one body behind the heatmap and the bubble grid, including the sampling status of a cell |
| `viz_helpers.R` | *(split from the plots)* | mesh size order, category columns, the imported / raised and sampled / estimated marks (a cross, a dotted line, a hatched bar), the facet layer, the grouping sentence of a caption, catch-part wording, the class axis of the distribution plots |
| `viz_raising.R` | *(split from the plots)* | `plot_river()` (also for the result of `recommend_groups()`), `plot_groups()` (how the strata fell into the levels of the groups), `plot_compare_raised()` |
| `viz_clusters.R` | *(new)* | `build_discard_clusters()` and its two plots |
| `viz_save.R` | *(new)* | the `save` argument every plot takes |

---

## The Shiny application — put apart

`inst/app/`, `R/run_app.R` and the functions only the app used are not in the
working tree. They are whole in the git tag `app-archive`; `DROPPED.md` says how
to bring them back and lists what went.

## dev/ — not shipped (`.Rbuildignore`)

`dev/tests/` (the suites), `dev/testdata/` (the synthetic CEF fixture they share),
`dev/check_ascii.R`, `dev/check_syntax.R`, `dev/check_exports.R`, `dev/check_all.sh`.

## data/

None: the package ships no data. The tests run off the synthetic CEF fixture in
`dev/testdata/` (`dev/tests/fixture.R` loads it); it carries
`originType = "WGEstimate"` on every row - the value `catchRaising()` and
`catchAllocationAoL()` select on - so it runs the whole workflow (coverage
tables and plots, discard raising, length and age allocation, aggregation), one
year at a time. `dev/tests/test_workflow_example.R` guards that.

## inst/extdata/ (local only)

`inst/extdata/` is git-ignored: it holds real fleet data on this machine
only, and the three vignettes read it:

| file | what |
|---|---|
| `data_CEF_format/bll/{2024,2025}/bll_{2024,2025}_{catches,distributions}_CEF_v17.1.csv` | brill `bll.27.3a47de` 2024 and 2025 - discards and lengths, no ages. From the git history (removed in `4c1028c`). |
| `data_CEF_format/pok/pok_2022_{catches,distributions}_CEF_v17.1.csv` | saithe `pok.27.3a46` 2022 - with ages. Converted from `data_overviews_format/pok/2022` with `funICoutCEF(..., output_format = "to_file", file_prefix = "pok_2022_")`; the `pok_2022_*` csv files in the history were byte-copies of the bll ones. |
| `data_overviews_format/{bll/2025,pok/2022}/` | the InterCatch overview files the CEF csv files came from. |

The vignettes read the csv files directly, with no existence checks: they
stop with an error where the files are missing, so `R CMD build` needs
`--no-build-vignettes` there, and the CI workflow skips vignettes for the
same reason.

| vignette | reads |
|---|---|
| `part1-explore-sampling-coverage` | bll and pok, catches + distributions |
| `part2-discard-raising` | bll 2024 catches |
| `part3-length-age-allocation` | bll 2025 catches and distributions |

The file names start with `part`, not with the number: R drops a vignette
whose file name does not start with a letter from the built package
("excluding invalid files"). The numbers are in the titles.

## Repo scaffolding

`DESCRIPTION`, `NAMESPACE` (generated), `.Rbuildignore`, `.gitignore`,
`rcef.Rproj`, `LICENSE` (**placeholder — settle this with the WG before the
repo goes public**), `.github/workflows/R-CMD-check.yaml`, `dev/setup.R`,
`dev/README.md`.