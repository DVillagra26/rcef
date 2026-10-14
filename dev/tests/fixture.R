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

## Raising groups written by hand, as in the TAF scripts: the strata that are raised together (`strataCond`) and the
## data each group takes its discard ratio from (`matchedDataCond`), here on the metier. Three ways to raise the same
## catch, so there is something to compare:
##   "own"   one group per metier, each raised from its own discard estimates;
##   "mixed" PS on its own, LHP and MPT together;
##   "all"   one group, everything raised from everything.
fx_groups <- function(scheme = c("own", "mixed", "all")) {
  q <- rlang::quo
  switch(match.arg(scheme),
    own   = { g <- list(LHP = q(metier4 == "LHP"), MPT = q(metier4 == "MPT"), PS = q(metier4 == "PS"))
              list(strataCond = g, matchedDataCond = g) },
    mixed = { g <- list(PS = q(metier4 == "PS"), OTHER = q(metier4 != "PS"))
              list(strataCond = g, matchedDataCond = g) },
    all   = list(strataCond = list(ALL = q(TRUE)), matchedDataCond = list(ALL = q(TRUE))))
}

## The raised catch the way a TAF script gets it: catchRaising() on the catch. `groups` is what fx_groups() returns, or
## any list(strataCond =, matchedDataCond =).
fx_raise <- function(catch, groups, type = "discards", log_file = NULL, ...)
  catchRaising(catch_data = catch,
               condition_raising_st_list = groups$strataCond,
               condition_matched_data_list = groups$matchedDataCond,
               type = type, logFile = log_file, ...)

## Lengths or ages allocated the way model_length_allocation.R does it: catchAllocationAoL() on the raised catch and
## the distributions, one group that takes everything. Returns the engine's list (distribution, catch) and `type`.
fx_allocate <- function(raised, dist, type = "LengthTotal", log_file = NULL, ...) {
  dist <- as.data.frame(dist, stringsAsFactors = FALSE)
  res <- catchAllocationAoL(catch_data = as.data.frame(raised),
                            distribution_data = dist[dist$distributionType %in% type, , drop = FALSE],
                            condition_alloc_st_list = list(ALL = rlang::quo(TRUE)),
                            condition_matched_data_list = list(ALL = rlang::quo(TRUE)),
                            distributionType = type, verbose = FALSE, logFile = log_file,
                            assembled_output = TRUE, ...)
  c(res[c("distribution", "catch")], list(type = type))
}

## The two aggregations of an allocation, as catchAtAoLAggregation() / meanWoLatAoLAggregation() give them
## (wide: one column per class, N_<type>_<class> / W_<type>_<class>), no plus group unless asked for.
fx_numbers <- function(al, grouping = c("catchCategory", "sampledOrEstimated"), round = 2, plusGroup = FALSE, ...)
  as.data.frame(catchAtAoLAggregation(distribution_data = al$distribution, catch_data = al$catch,
                                      grouping = grouping, originType_catch = "WGEstimate",
                                      variableType_catch = "WeightLive", distributionType = al$type,
                                      round = round, plusGroup = plusGroup, ...))
fx_meanweight <- function(al, grouping = c("catchCategory", "sampledOrEstimated"), unit = "g", round = 1,
                          plusGroup = FALSE, ...)
  as.data.frame(meanWoLatAoLAggregation(distribution_data = al$distribution, catch_data = al$catch,
                                        grouping = grouping, weighting = "NumberAtAoL",
                                        originType_catch = "WGEstimate", variableType_catch = "WeightLive",
                                        variableType_dist = "WeightLive", distributionType = al$type,
                                        unit = unit, round = round, plusGroup = plusGroup, ...))

## A wide aggregation read long: one row per class, the class (`.cls`) and the type (`.aol`) read from the column name.
fx_pivot_long <- function(d, kind = c("N", "W"), value_name = "value") {
  kind <- match.arg(kind)
  re <- "^([NW])_(LengthTotal|Age)_(.+)$"
  cols <- grep(re, names(d), value = TRUE)
  cols <- cols[sub(re, "\\1", cols) == kind]
  keys <- setdiff(names(d), grep(re, names(d), value = TRUE))
  long <- tidyr::pivot_longer(d[c(keys, cols)], dplyr::all_of(cols), names_to = ".col", values_to = value_name)
  long$.cls <- suppressWarnings(as.numeric(sub(re, "\\3", long$.col)))
  long$.aol <- sub(re, "\\2", long$.col)
  long <- long[!is.na(long[[value_name]]), setdiff(names(long), ".col")]
  as.data.frame(long)
}

## One year's raised catch and its allocation, lengths and ages, the way the TAF scripts get them: fx_raise() with one group per
## metier, then fx_allocate(). The engine and its aggregations count a distribution once for every catch row that names its
## domainBiology, so the catch handed to the allocation has ONE row per domain (a domain of its own for every catch row, per
## country and catch category; the raised discards have none, and the allocation makes one up for each stratum): the CEF files built
## from InterCatch are like that, the fixture as it is bundled is not. Returns raised (the raising's output), len and age (the two
## allocations: distribution, catch, type).
fx_allocation <- function(yr = 2023, log_file = tempfile(fileext = ".txt")) {
  quiet <- function(expr) { out <- NULL; invisible(utils::capture.output(out <- suppressWarnings(suppressMessages(expr)))); out }
  cy <- fx_catch(); cy <- cy[cy$year == yr, ]
  dy <- as.data.frame(fx_dist()); dy <- dy[dy$year == yr, ]
  raised <- as.data.frame(quiet(fx_raise(cy, fx_groups("own"), type = "discards", log_file = log_file)))
  ckey <- paste(raised$vesselFlagCountry, raised$catchCategory, raised$domainBiology)
  akey <- paste(raised$vesselFlagCountry, raised$seasonValue, raised$areaValue, raised$fisheriesManagementUnit,
                raised$fleetValue, raised$catchCategory)
  n_ck <- ave(seq_along(ckey), ckey, FUN = length)
  n_ak <- ave(seq_along(akey), akey, FUN = length)
  catch1 <- raised[ifelse(is.na(raised$domainBiology), n_ak == 1L, n_ck == 1L), ]
  dist1  <- dy[paste(dy$vesselFlagCountry, dy$catchCategory, dy$domainBiology) %in%
                 paste(catch1$vesselFlagCountry, catch1$catchCategory, catch1$domainBiology), ]
  list(raised = raised,
       len = quiet(fx_allocate(catch1, dist1, "LengthTotal", log_file = log_file)),
       age = quiet(fx_allocate(catch1, dist1, "Age", log_file = log_file)))
}
