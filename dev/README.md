# dev/ — not shipped

`.Rbuildignore`d, so nothing here is installed or checked.

| file | what |
|---|---|
| `setup.R` | `pkgload::load_all(".")` — develop without installing |
| `check_ascii.R` | every text file under `R/`, `vignettes/` and `man/` must be 7-bit ASCII. Non-ASCII (em-dashes, arrows, box-drawing) breaks `R CMD build` on a non-UTF-8 Windows locale with an opaque error |
| `check_syntax.R` | `parse()` every `.R` file; must print `DONE ... 0 failure(s)` |
| `check_exports.R` | every top-level object — functions **and constants** — must be exported; must print `NOT EXPORTED : 0` |
| `check_all.sh` | the three checks above, then every suite in `tests/`, with real exit-code handling |
| `tests/` | the suites, `test_*.R`, and `fixture.R`, the helpers they share: the synthetic CEF data (`fx_catch()`, `fx_dist()`), three ways of grouping the fixture written out as conditions (`fx_groups()`), and the engines called the way a TAF script calls them (`fx_raise()`, `fx_allocate()`, `fx_numbers()`, `fx_meanweight()`). `test_workflow_example.R` pushes the fixture through raising, allocation and aggregation with only rcef attached |
| `testdata/` | `cef_fixture.rds` — the synthetic CEF data the tests share, read through `tests/fixture.R` (see its README) |

The Shiny application, its checks and its tests are not here: see `DROPPED.md`.

## Running the tests

`bash dev/check_all.sh` runs everything against the installed package. One
suite on its own, from the repo root:

```sh
Rscript -e "suppressPackageStartupMessages(library(rcef)); source('dev/tests/test_workflow_example.R')"
```

Each suite prints its own `DONE  N failure(s)` marker. An R error halts a script
without printing FAIL, so check for `DONE`, not for a zero fail count.

Porting them to `testthat` under `tests/testthat/` is the next job; that is
what turns `R CMD check` into a real gate.
