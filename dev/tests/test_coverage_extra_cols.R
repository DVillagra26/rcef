## build_coverage_strata() used to report only its `dims` columns, dropping
## every other derive_fields() column (mesh, metier5, metier6, target, the
## ICES area levels, ...) even when a stratum pinned one to a
## single value - so a caller could never filter the table on a column
## they had not also named as part of the aggregation. The extra-column rule
## now carries those along, NA where the stratum genuinely spans more than
## one value.

source("dev/tests/fixture.R")
FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}

test_catch <- fx_catch()
test_dist <- fx_dist()
c23 <- subset(test_catch, year == 2023)

## ---- dims left out: the finest stratum, everything is pinned -----------
cov0 <- build_coverage_strata(c23)
ok(all(c("mesh", "metier5", "metier6", "target", "ICESSubArea", "Stratum") %in%
      names(cov0)),
  "build_coverage_strata(): the derived columns ride along, dims left out")
ok(!any(is.na(cov0$mesh)),
  "build_coverage_strata(): at the finest resolution, mesh is never NA")
ok(!any(is.na(cov0$Stratum)) && !anyDuplicated(cov0$Stratum),
  "build_coverage_strata(): Stratum rides along too, one distinct value per row")

## ---- a coarser dims: pinned where the stratum agrees, NA where it doesn't
cov1 <- build_coverage_strata(c23, c("metier4", "vesselFlagCountry"))
ok(all(c("mesh", "metier5", "metier6", "target", "ICESSubArea", "seasonValue")
      %in% names(cov1)),
  "build_coverage_strata(): the derived columns ride along, dims = metier4 x vesselFlagCountry")
lhp <- cov1[cov1$metier4 == "LHP", , drop = FALSE]
ok(nrow(lhp) > 0 && length(unique(lhp$mesh)) == 1 && !is.na(lhp$mesh[1]),
  "build_coverage_strata(): mesh is pinned where metier4 alone determines it")
ps <- cov1[cov1$metier4 == "PS", , drop = FALSE]
ok(nrow(ps) > 0 && all(is.na(ps$mesh)),
  "build_coverage_strata(): mesh is NA where metier4 spans more than one mesh")
ok(all(is.na(cov1$seasonValue)),
  "build_coverage_strata(): seasonValue is NA throughout - metier4 x vesselFlagCountry never pins it")
ok("Stratum" %in% names(cov1) && all(is.na(cov1$Stratum)),
  "build_coverage_strata(): Stratum is NA too - the finest of all of them")

## ---- filtering on a column that was never named in dims -----------------
mesh0 <- subset(cov1, mesh == "0")
ok(nrow(mesh0) > 0 && nrow(mesh0) < nrow(cov1),
  "build_coverage_strata(): the kept column actually filters the table")

## ---- a column already in dims is not duplicated or re-added -------------
cov2 <- build_coverage_strata(c23, c("metier4", "mesh"))
ok(sum(names(cov2) == "mesh") == 1,
  "build_coverage_strata(): a column already in dims is not duplicated")

## ---- length / age branch gets the same treatment -------------------------
covL <- build_coverage_strata(c23, c("metier4", "vesselFlagCountry"), test_dist,
                              what = "length", catchCategory = "Lan")
ok(all(c("mesh", "metier5") %in% names(covL)),
  "build_coverage_strata(): length/age branch also keeps the derived columns")

## ---- nothing extra when nothing applies -----------------------------------
value_cols <- c("catchCategory", "variableUnit", "total", "weightingCategory", "weightingTotal",
                "rate", "discardRate", "landings", "coverage", "numTrips")
all_cand <- intersect(c("metier4", "metier5", "metier6", "vesselFlagCountry", "ICESSubArea", "ICESDivision",
                        "ICESSubdivision", "seasonValue", "FleetType", "target", "mesh", "metier4_mesh",
                        "Stratum"), names(c23))
ok(identical(names(build_coverage_strata(c23, all_cand)), c(all_cand, value_cols)),
  "build_coverage_strata(): no extra column once dims already names every candidate")
mini <- data.frame(catchCategory = c("Lan", "Dis"), total = c(100, 10), domainCatchDis = "a", x = "k",
                   stringsAsFactors = FALSE)
ok(identical(names(build_coverage_strata(mini, "x")), c("x", value_cols)),
  "build_coverage_strata(): no extra column when the catch carries none of the candidate columns")

## ---- an all-NA extra column does not fake out coverage_cells() ----------
## A column that rides along but is NA on every row of THIS table (its dims
## never pin it down) is not something a plot can actually group by - it
## has to still read as "not in the coverage table", the same error as a
## column genuinely absent, not be silently accepted and grouped into one
## useless NA level.
err <- function(expr) tryCatch({ force(expr); "no error" },
                               error = function(e) conditionMessage(e))
tab_no_season <- build_coverage_strata(c23, "metier4")
ok("seasonValue" %in% names(tab_no_season) && all(is.na(tab_no_season$seasonValue)),
  "fixture: seasonValue rides along on this table, but is NA throughout")
ok(grepl("not in the coverage table: seasonValue", err(
     plot_coverage_heat(tab_no_season, y = "metier4", x = "seasonValue"))),
  "plot_coverage_heat(): an all-NA extra column is still named as unavailable")

## ---- plot_DR() does not let Stratum (or an NA extra) into a label -------
## Stratum is fleetValue/vesselFlagCountry/areaValue/seasonValue already
## joined into one string - a composite of four other columns, not a
## nested level of any one of them, so the label columns of plot_DR() never drop it
## the way it drops metier6/metier5/metier4 once fleetValue is present. It
## has to be kept out of a point's label by name, or a label repeats the
## same four values again, pasted together, under a different format.
suppressPackageStartupMessages(library(ggplot2))
p_dr <- plot_DR(c23, x = "metier4", colour = "vesselFlagCountry", reference = 0)
labs <- p_dr$data$.lab[nzchar(p_dr$data$.lab)]
ok(length(labs) > 0, "fixture: plot_DR(reference = 0) labels most strata")
n_parts <- lengths(strsplit(labs, "|", fixed = TRUE))
ok(all(n_parts == 4),
  "plot_DR(): a label is exactly fleetValue|areaValue|vesselFlagCountry|seasonValue - Stratum adds no fifth part")

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)