# Legacy suites — currently broken, kept for porting

These predate the package. They `source("R_development/0_Functions_utils.R")`
and `readLines("app.R")`, paths that no longer exist, so **every one of
them errors immediately**.

They are here rather than in `dev/tests/` so that `dev/check_all.sh` does
not run them: a suite that always fails is noise, and noise is how a real
failure gets scrolled past. They are not deleted, because the assertions
inside them are worth keeping — several cover TAF emission and app wiring
that nothing else does.

Porting one means: replace the `source()` block with
`library(rcef)`, replace `readLines("app.R")` with
`readLines(system.file("app/app.R", package = "rcef"))`, and move it up
to `dev/tests/`.

**`test_wiring.R` has been ported** and now lives in `dev/tests/`. It was
worth it: mutating one `output[[ns("ov")]]` to a typo makes it fail with
the slot named, which is exactly what it exists for.

`test_app_server.R` HAS NOW BEEN MINED: its one still-valid assertion -
it asserts on `Area1`, a dimension removed when the area levels became
nested, and on a `test_census.csv` fixture that no longer ships. Its
behavioural assertions have largely been absorbed into
`dev/check_server.R`, which runs the server against a real CEF fixture
from `inst/extdata`. the per-tab level-of-detail independence check now lives in
`dev/check_server.R`, run against a real fixture. The rest of the file
(Area1, test_census.csv) is dead and can be deleted whenever.

`test_taf.R` cannot be fully ported: it asserted on `taf_parts_of()`
and `TAF_STAGE`, a stage-grouped download UI ("data" / "model" /
"output" / "report" tabs, each holding several files) that was
abandoned for the current one-tab-per-file design (`taf_ui(part)`,
`TAF_FILES`, `TAF_LABEL`, `TAF_BLURB` - all still live). Both were
confirmed to have zero callers anywhere in `R/` or `inst/app/app.R` and
were removed as dead code. The remaining assertions in `test_taf.R` -
`taf_bundle()` producing all seven parts, `taf_write()` laying down
seven files, `taf_zip()`, the concatenated listing - are still valid
and worth porting; skip lines 77-82 (the `TAF_FILES`/`TAF_STAGE`/
`taf_parts_of` block) when doing it.

`taf_header()` was found MISSING entirely during this pass - called
seven times across `rcef_taf.R`, defined nowhere. `taf_bundle()` wraps
every part in `tryCatch`, so the failure was silent: every downloaded
script read "This script could not be generated from the current
session" instead of real content, and nothing in `check_all.sh` caught
it because nothing there calls the TAF emitters. `roxygenise()`'s
"exported but not in namespace" warning is what surfaced it. This is
the strongest argument for porting `test_taf.R` fully rather than
leaving it archived: a suite that actually calls `taf_bundle()` and
inspects the output would have caught this the moment it was
introduced.
