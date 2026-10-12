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
| `RDBESStockCoord_utils.R` | `R/0_Functions_other.R` | `check_group_conditions`, `convert_field`, `unit2ratio`, `fieldsToStrata` |
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

**Not changed:** every function name, every argument name, every default,
every return shape, every algorithm.

### Ours

| new file | was | what |
|---|---|---|
| `aaa-package.R` | *(new)* | package doc, `@import`, `globalVariables()` |
| `rcef_utils.R` | `R_development/0_Functions_utils.R` | vocabulary, `derive_fields()`, units, labellers |
| `rcef_classify.R` | `R_development/0_Functions_classify.R` | `build_strata()`, `classify_strata()`, `cat_lookup()`, `status_palette()` |
| `rcef_coverage.R` | *(new)* | `build_coverage_strata()`: the discard-rate / sampling-coverage table (CAR / CAS-style columns, no status) the coverage and discard-rate plots draw, usable on its own |
| `rcef_groups.R` | `R_development/0_Functions_groups.R` | auto cascade + manual rules → `strata_groups` → raising conditions |
| `rcef_raising.R` | `R_development/0_Functions_raising.R` | `apply_raising()`, scenarios, comparison |
| `rcef_raise_native.R` | `R_development/mod_native_raising.R` | CEF prepare/check + `run_native_raising()` |
| `rcef_length.R` | `R_development/mod_length.R` | distributions prep, biology link, allocation steps/cascades |
| `rcef_alloc.R` | `R_development/mod_alloc_raising.R` | `run_allocation()` + aggregation wrappers + `alloc_class_table()` (what the allocated plots draw) |
| `rcef_intercatch.R` | `R_development/mod_intercatch.R` **minus `ic_ui()`** | IC staging, mapping, year detection, code emission |
| `rcef_summaries.R` | `R_development/summaries.R` | catch / biology / effort summary tables |
| `rcef_session.R` | `R_development/session_log.R` **minus `state_capture()`** | session log + code fragment builders |
| `rcef_emit.R` | `R_development/0_Functions_emit.R` | reproducible single-script emitter |
| `rcef_taf.R` | `R_development/0_Functions_taf.R` | the same session as an ICES TAF script set |
| `viz_theme.R` | `R_development/0_Functions_theme.R` | `app_theme()`, stable colour per level |
| `viz_plots.R` | `R_development/0_Functions_plots.R` | the public plot functions (ggplot only) |
| `viz_helpers.R` | *(split from the plots)* | facet layers, status legend, catch-part wording, plot title / subtitle / caption wording (what is sampled, over which weight, what a stratum pools over), ICES-area centroids |

---

## app/ — NOT part of the package

| file | was | why it is here |
|---|---|---|
| `app.R` | `R_development/app.R` | the application |
| `app_ui_helpers.R` | `R_development/app_ui_helpers.R` | icons, panel heights, legend panel — shiny only |
| `app_mod_intercatch_ui.R` | split out of `mod_intercatch.R` | `ic_ui()` — `actionButton`, `selectInput` |
| `app_state.R` | split out of `session_log.R` | `state_capture()` — reads `reactiveValuesToList(input)` |

Two files were **split**, not moved. `mod_intercatch.R` and
`session_log.R` each held one shiny-coupled function among otherwise pure
ones; keeping them whole would have dragged shiny into `Imports`.

## dev/ — not shipped (`.Rbuildignore`)

`dev/tests/` (the 11 suites + `run_tests.R`), `dev/testdata/`,
`dev/check_syntax.R`, `dev/load_all_sourced.R`.

## data/

`anchoveta_catch` and `anchoveta_dist` - a fully synthetic
Southeast-Pacific anchoveta CEF pair, lazy-loaded with the package
(`data(anchoveta_catch)`). The only example data now shipped: the real
bll / pok CEF files and the IC exchange / overview fixtures that used to
live in `inst/extdata/` were removed from the repository - the tests and
the app checks run off `anchoveta_*`.

The example carries `originType = "WGEstimate"` on every row - the value
`catchRaising()`, `catchAllocationAoL()` and the app's picker select on -
so it runs the whole workflow (coverage tables and plots, discard raising,
length and age allocation, aggregation), one year at a time.
`dev/tests/test_anchoveta_example.R` guards that, headless.

## inst/extdata/ (local only)

`inst/extdata/` is git-ignored: it holds real fleet data on this machine
only, and the four vignettes read it:

| file | what |
|---|---|
| `data_CEF_format/bll/bll_2025_{catches,distributions}_CEF_v17.1.csv` | brill `bll.27.3a47de` 2025 - discards and lengths, no ages. From the git history (removed in `4c1028c`). |
| `data_CEF_format/pok/pok_2022_{catches,distributions}_CEF_v17.1.csv` | saithe `pok.27.3a46` 2022 - with ages. Converted from `data_overviews_format/pok/2022` with `funICoutCEF(..., output_format = "to_file", file_prefix = "pok_2022_")`; the `pok_2022_*` csv files in the history were byte-copies of the bll ones. |
| `data_overviews_format/{bll/2025,pok/2022}/` | the InterCatch overview files the CEF csv files came from. |

The vignettes read the csv files directly, with no existence checks: they
stop with an error where the files are missing, so `R CMD build` needs
`--no-build-vignettes` there, and the CI workflow skips vignettes for the
same reason.

| vignette | reads |
|---|---|
| `part1-explore-sampling-coverage` | bll and pok, catches + distributions |
| `part2a-discard-raising-manual-groups` | bll catches |
| `part2b-discard-raising-hierarchical-groups` | bll catches |
| `part3-compare-allocation-schemes` | bll catches |

The file names start with `part`, not with the number: R drops a vignette
whose file name does not start with a letter from the built package
("excluding invalid files"). The numbers are in the titles.

---

# Stage 2 — the app became part of the package

| new | was | why |
|---|---|---|
| `inst/app/app.R` | `app/app.R` | `inst/` ships with the install, so `run_app()` finds it after `install_github()`. Never seen by `R CMD check`. |
| `inst/app/app_ui_helpers.R` | `app/` | |
| `inst/app/app_mod_intercatch_ui.R` | `app/` | |
| `inst/app/app_state.R` | `app/` | |
| `R/run_app.R` | *(new)* | `run_app()` — the only supported entry point. Guards `shiny`/`DT` (Suggests) with a named error. |
| `R/rcef_status.R` | `R_development/external_loader.R` | `rcef_sources()` / `rcef_status()`. Nothing is sourced any more, but the **return shape is identical**, so the Availability panel and `taf_ctx` needed no edit — only `EXT <- rcef_sources(); EXT_STATUS <- rcef_status(EXT)`. |

**`app.R` head rewritten.** Dropped: `APP_DIR` auto-detection over
`0_Functions_utils.R`, ten `source()` calls into the data modules,
`external_loader.R`, `EXT_DIR`. Kept: a three-line `source()` of the
shiny-only helpers, because those are not package code. Everything below
the head is byte-identical.

**Two emitters had to change**, because they wrote `source()` calls
naming files that no longer exist:

| function | was | now |
|---|---|---|
| `code_setup()` | 5 `source(file.path(scriptPath, ...))` lines | `library(rcef)`; `external_dir` now optional and recorded as a comment |
| `taf_sources()` | 2 `for`-loops sourcing 11 files | `library(rcef)`; `external_dir` ignored, argument kept so callers do not break |

This is the substantive behaviour change of stage 2: **emitted scripts and
TAF sets now reproduce via the installed package**, not via a folder of
loose files. That is what makes a script generated on your machine runnable
on a colleague's.

## Repo scaffolding added

`DESCRIPTION` (shiny/DT → Suggests; URL/BugReports), `NAMESPACE`
(placeholder until `document()`), `.Rbuildignore`, `.gitignore`,
`rcef.Rproj`, `LICENSE` (**placeholder — settle this with the WG before the
repo goes public**), `.github/workflows/R-CMD-check.yaml`,
`dev/setup.R`, `dev/deploy_shinyapps.R`, `dev/README.md`.
