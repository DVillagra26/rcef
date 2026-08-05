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

`test_app_server.R` is the next one, but it needs more than a path fix -
it asserts on `Area1`, a dimension removed when the area levels became
nested, and on a `test_census.csv` fixture that no longer ships. Its
behavioural assertions have largely been absorbed into
`dev/check_server.R`, which runs the server against a real CEF fixture
from `inst/extdata`. What is left worth rescuing is the per-tab
level-of-detail independence check.
