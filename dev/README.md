# dev/ — not shipped

`.Rbuildignore`d, so nothing here is installed or checked.

| file | what |
|---|---|
| `setup.R` | `pkgload::load_all(".")` — develop without installing |
| `check_ascii.R` | every text file must be 7-bit ASCII. Non-ASCII (em-dashes, arrows, box-drawing) breaks `R CMD build` on a non-UTF-8 Windows locale with an opaque error |
| `check_syntax.R` | `parse()` every `.R` file; must print `DONE ... 0 failure(s)` |
| `check_exports.R` | every top-level object — functions **and constants** — must be exported; must print `NOT EXPORTED : 0` |
| `check_app.R` | sources `inst/app/app.R` against the installed package and renders the UI |
| `check_server.R` | **runs the server function** in a test session. `check_app.R` only builds tags, so it cannot see a helper defined after the module server that calls it |
| | Also: filter Apply round-trips every plot panel; per-tab level of detail is independent; the code/status outputs render without error (the class of bug the `mg_excl` typo was) |
| `check_sections.R` | the discard and biology manual builders must present the same 8 numbered steps |
| `check_parity.R` | lists the panels and controls of the two manual builders side by side, so a drift is visible rather than found by a user. Informational — it reports, it does not fail |
| `deploy_shinyapps.R` | shinyapps.io, with the GitHub-remote caveat spelled out |
| `check_all.sh` | every check above, with real exit-code handling |
| `tests/` | `test_cc_thresholds.R` (Lan/Dis split, zero-weight strata), `test_exclusion.R` (donor exclusion), `test_workflow_example.R` (the test fixture through raising, allocation and aggregation, with only rcef attached) |
| `tests/legacy/` | the 10 remaining pre-package suites. **All currently error** on paths that no longer exist; quarantined so they cannot hide a real failure. See the README there. |
| `testdata/` | `cef_fixture.rds` — the synthetic CEF data the tests share, read through `tests/fixture.R` (see its README) |

## Running the tests

They predate the package and still `source()` the old file names. Load the
package first and they will find the functions on the search path:

```r
pkgload::load_all(".")
for (f in list.files("dev/tests", "^test_.*\\.R$", full.names = TRUE))
  source(f)
```

Each must print its own `DONE` marker. An R error halts a script without
printing FAIL, so check for `DONE`, not for a zero fail count.

Porting them to `testthat` under `tests/testthat/` is the next job; that is
what turns `R CMD check` into a real gate.
