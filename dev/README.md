# dev/ — not shipped

`.Rbuildignore`d, so nothing here is installed or checked.

| file | what |
|---|---|
| `setup.R` | `pkgload::load_all(".")` — develop without installing |
| `check_syntax.R` | `parse()` every `.R` file; must print `DONE ... 0 failure(s)` |
| `check_exports.R` | every function must carry `@export`; must print `NOT EXPORTED : 0` |
| `DOC_BACKLOG.txt` | the 94 functions exported with `@noRd` — no man page yet |
| `deploy_shinyapps.R` | shinyapps.io, with the GitHub-remote caveat spelled out |
| `tests/` | the 11 suites + `run_tests.R` |
| `testdata/` | `make_test_data_anchoveta.R` — synthetic CEF fixtures |

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
