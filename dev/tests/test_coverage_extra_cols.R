## build_coverage_strata() used to report only its `dims` columns, dropping
## every other derive_fields() column (mesh, metier5, metier6, target, the
## ICES area levels, Season, ...) even when a stratum pinned one to a
## single value - so a caller could never filter the table on a column
## they had not also named as part of the aggregation. coverage_extra_cols()
## now carries those along, NA where the stratum genuinely spans more than
## one value.

FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}

data(anchoveta_catch)
data(anchoveta_dist)
c23 <- subset(anchoveta_catch, year == 2023)

## ---- dims left out: the finest stratum, everything is pinned -----------
cov0 <- build_coverage_strata(c23)
ok(all(c("mesh", "metier5", "metier6", "target", "ICESSubArea") %in%
      names(cov0)),
  "build_coverage_strata(): the derived columns ride along, dims left out")
ok(!any(is.na(cov0$mesh)),
  "build_coverage_strata(): at the finest resolution, mesh is never NA")

## ---- a coarser dims: pinned where the stratum agrees, NA where it doesn't
cov1 <- build_coverage_strata(c23, c("metier4", "Country"))
ok(all(c("mesh", "metier5", "metier6", "target", "ICESSubArea", "Season")
      %in% names(cov1)),
  "build_coverage_strata(): the derived columns ride along, dims = metier4 x Country")
lhp <- cov1[cov1$metier4 == "LHP", , drop = FALSE]
ok(nrow(lhp) > 0 && length(unique(lhp$mesh)) == 1 && !is.na(lhp$mesh[1]),
  "build_coverage_strata(): mesh is pinned where metier4 alone determines it")
ps <- cov1[cov1$metier4 == "PS", , drop = FALSE]
ok(nrow(ps) > 0 && all(is.na(ps$mesh)),
  "build_coverage_strata(): mesh is NA where metier4 spans more than one mesh")
ok(all(is.na(cov1$Season)),
  "build_coverage_strata(): Season is NA throughout - metier4 x Country never pins it")

## ---- filtering on a column that was never named in dims -----------------
mesh0 <- subset(cov1, mesh == "0")
ok(nrow(mesh0) > 0 && nrow(mesh0) < nrow(cov1),
  "build_coverage_strata(): the kept column actually filters the table")

## ---- a column already in dims is not duplicated or re-added -------------
cov2 <- build_coverage_strata(c23, c("metier4", "mesh"))
ok(sum(names(cov2) == "mesh") == 1,
  "build_coverage_strata(): a column already in dims is not duplicated")

## ---- length / age branch gets the same treatment -------------------------
covL <- build_coverage_strata(c23, c("metier4", "Country"), anchoveta_dist,
                              what = "length", catchCategory = "Lan")
ok(all(c("mesh", "metier5") %in% names(covL)),
  "build_coverage_strata(): length/age branch also keeps the derived columns")

## ---- coverage_extra_cols() directly: NULL when nothing applies -----------
all_cand <- intersect(DIMS_ALL_CANDIDATES, names(c23))
ok(is.null(coverage_extra_cols(c23, all_cand)),
  "coverage_extra_cols(): NULL once dims already covers every candidate")
ok(is.null(coverage_extra_cols(data.frame(x = 1), "x")),
  "coverage_extra_cols(): NULL when none of the candidate columns are present")

## ---- an all-NA extra column does not fake out coverage_cells() ----------
## A column that rides along but is NA on every row of THIS table (its dims
## never pin it down) is not something a plot can actually group by - it
## has to still read as "not in the coverage table", the same error as a
## column genuinely absent, not be silently accepted and grouped into one
## useless NA level.
err <- function(expr) tryCatch({ force(expr); "no error" },
                               error = function(e) conditionMessage(e))
tab_no_season <- build_coverage_strata(c23, "metier4")
ok("Season" %in% names(tab_no_season) && all(is.na(tab_no_season$Season)),
  "fixture: Season rides along on this table, but is NA throughout")
ok(grepl("not in the coverage table: Season", err(
     plot_coverage_heat(tab_no_season, y = "metier4", x = "Season"))),
  "plot_coverage_heat(): an all-NA extra column is still named as unavailable")

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
