## The synthetic CEF data the tests share: a catch table and its distributions.
##
## Invented numbers, neutral labels: flags BE / DE / DK / ES / FR / IE / NL, ICES areas
## 27.3.a to 27.7.f, stock "syn.27.347", years 2020-2023, a landings / discards / BMS split
## with uneven sampling (so there is something to raise), length and age distributions.
## Nothing in it is real. See dev/testdata/README.md.
##
## From the repo root:   source("dev/tests/fixture.R")   then   fx_catch()   fx_dist()

.fx <- local({
  path <- c("dev/testdata/cef_fixture.rds", "../testdata/cef_fixture.rds", "testdata/cef_fixture.rds")
  path <- path[file.exists(path)]
  if (!length(path)) stop("cef_fixture.rds not found: run the tests from the repo root")
  path <- normalizePath(path[1], winslash = "/")   # absolute, so a test that changes directory still finds it
  cached <- NULL
  function() { if (is.null(cached)) cached <<- readRDS(path); cached }
})
fx_catch <- function() .fx()$catch
fx_dist  <- function() .fx()$dist
