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
to `dev/tests/`. `test_wiring.R` and `test_app_server.R` are the two
worth doing first.
