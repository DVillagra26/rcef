## derive_fields() once, then build on it. The coverage table and the plots read the
## columns the catch has and derive nothing themselves, so a column the user regrouped
## or added is used as it stands, and a derived column the catch lacks is an error that
## says where it comes from. derive_fields() itself keeps the columns it finds, so
## calling it again - or deriving the distributions from an edited catch - never undoes
## an edit.

source("dev/tests/fixture.R")
FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}
quiet <- function(expr) suppressWarnings(suppressMessages(expr))
err <- function(expr) tryCatch({ quiet(expr); "" }, error = function(e) conditionMessage(e))
drawable <- function(p) !is.null(p) && inherits(p, "ggplot") &&
  !inherits(try(ggplot2::ggplot_build(p), silent = TRUE), "try-error")

derived <- c("metier4", "metier5", "metier6", "target", "mesh", "metier4_mesh",
             "ICESSubArea", "ICESDivision", "ICESSubdivision", "Stratum")
cd0 <- subset(fx_catch(), year == 2023)
cd0 <- cd0[, setdiff(names(cd0), derived)]                    # the catch as the CEF file has it
dd0 <- subset(fx_dist(), year == 2023)
dd0 <- dd0[, setdiff(names(dd0), c(derived, "fleetValue", "areaValue", "seasonValue"))]
ok(!any(derived %in% names(cd0)) && nrow(cd0) > 0 && nrow(dd0) > 0,
   "fixture: a raw catch and raw distributions, none of the derived columns on them")
cd <- derive_fields(cd0)
dd <- quiet(derive_fields(cd, dd0, what = "distribution"))

## ---- derive_fields() keeps what the catch already has ----------------------------
ok(all(derived %in% names(cd)), "derive_fields() adds the derived columns")
ed <- cd
ed$metier4 <- paste0("G_", ed$metier4)
ed$mesh    <- "m"
ok(identical(derive_fields(ed), ed),
   "deriving again leaves the columns it finds as they are: the regrouped metier4 and mesh stay")
x3 <- derive_fields(ed[setdiff(names(ed), "metier4_mesh")])
ok(identical(x3$metier4_mesh, paste(ed$metier4, "m", sep = "_")),
   "a column that is missing is derived from the columns that are there (the regrouped ones)")
x4 <- derive_fields(ed[setdiff(names(ed), "metier4")])
ok(identical(x4$metier4, cd$metier4) && identical(x4$mesh, ed$mesh),
   "drop a column first to have it derived afresh: the others stay")
de <- quiet(derive_fields(ed, dd0, what = "distribution"))
lk <- !is.na(de$metier4)
ok(any(lk) && all(startsWith(de$metier4[lk], "G_")) && all(de$mesh[!is.na(de$mesh)] == "m"),
   "the distributions look up the catch as it is: they carry the regrouped metier4, not a fresh one")

## ---- the table and the plots use the catch as it stands --------------------------
reg <- cd
reg$metier4 <- ifelse(reg$metier4 == "PS", "NET", "OTHER")
for (what in c("length", "age")) {
  t <- quiet(build_coverage_strata(reg, "metier4", dd, what = what))
  ok(!is.null(t) && setequal(unique(t$metier4), c("NET", "OTHER")),
     sprintf("what = \"%s\": the strata are the regrouped metier4 - nothing was derived afresh", what))
}
tl <- quiet(build_coverage_strata(reg, "metier4", dd, what = "length", catchCategory = "Lan"))
is_net <- reg$metier4 == "NET" & reg$catchCategory == "Lan"
ok(isTRUE(all.equal(tl$total[tl$metier4 == "NET"],
                    sum(convert_units(reg$total[is_net], reg$variableUnit[is_net], "t"), na.rm = TRUE))),
   "... and their weight is that of the regrouped rows")
ok(setequal(unique(quiet(plot_NaLoA(reg, dd, fill = "metier4"))$data$metier4), c("NET", "OTHER")) &&
     setequal(unique(quiet(plot_MWaLoA(reg, dd, colour = "metier4"))$data$metier4), c("NET", "OTHER")),
   "plot_NaLoA() and plot_MWaLoA() group by the regrouped metier4")

## a column of the user's own, not one derive_fields() knows
cust <- cd
cust$fleetGroup <- ifelse(cust$metier4 == "PS", "G1_PS", "G2_REST")
tc <- quiet(build_coverage_strata(cust, "fleetGroup", dd, what = "length"))
ok(!is.null(tc) && setequal(unique(tc$fleetGroup), c("G1_PS", "G2_REST")),
   "build_coverage_strata(): a column of your own is a stratum column like any other")
ok(drawable(quiet(plot_coverage_bar(cust, dd, y = "fleetGroup", what = "length", catchCategory = "Lan"))) &&
     drawable(quiet(plot_coverage_grid(cust, dd, y = "fleetGroup", x = "vesselFlagCountry", what = "age"))) &&
     drawable(quiet(plot_NaLoA(cust, dd, fill = "fleetGroup"))) &&
     drawable(quiet(plot_MWaLoA(cust, dd, colour = "fleetGroup"))) &&
     drawable(quiet(plot_catches(cust, x = "fleetGroup"))) &&
     drawable(quiet(plot_DR(cust, x = "fleetGroup"))),
   "the coverage plots, plot_NaLoA(), plot_MWaLoA(), plot_catches() and plot_DR() take it as an axis, fill or colour")

## ---- a derived column the catch lacks: the error says where it comes from --------
says <- function(msg, col) nzchar(msg) && grepl(col, msg, fixed = TRUE) && grepl("derive_fields()", msg, fixed = TRUE)
ok(says(err(build_coverage_strata(cd0, "metier4", dd0, what = "length")), "metier4") &&
     says(err(build_coverage_strata(cd0, "metier4")), "metier4") &&
     says(err(plot_coverage_bar(cd0, dd0, y = "metier4", what = "length")), "metier4") &&
     says(err(plot_NaLoA(cd0, dd0, fill = "metier4")), "metier4") &&
     says(err(plot_MWaLoA(cd0, dd0, colour = "metier4")), "metier4") &&
     says(err(plot_catches(cd0, x = "metier4")), "metier4") &&
     says(err(plot_DR(cd0, x = "metier4")), "metier4"),
   "metier4 missing: every table and plot names the column and derive_fields()")
ok(!grepl("derive_fields", err(build_coverage_strata(cd0, "nonsense")), fixed = TRUE) &&
     grepl("nonsense", err(build_coverage_strata(cd0, "nonsense")), fixed = TRUE),
   "a column derive_fields() does not add is just reported as missing")

## ---- nothing derives behind your back --------------------------------------------
no_derive <- function(f) !"derive_fields" %in% all.names(body(f))     # a call, not the word in a message
ok(all(vapply(list(build_coverage_strata, biology_dist_prep, coverage_cells, plot_DR, plot_catches,
                   plot_NaLoA, plot_MWaLoA, plot_coverage_grid,
                   plot_coverage_bar, plot_coverage_map), no_derive, NA)),
   "no coverage table or plot calls derive_fields()")

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
