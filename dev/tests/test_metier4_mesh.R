## metier4_mesh - the gear and the mesh joined ("TBB_70-99"), a derive_fields()
## column. A mesh size only means something within its gear, so this is the
## one label to put on a single axis, fill or filter instead of two.
##
## What is pinned here: the value itself (including the fleet codes that have
## no mesh part), that every list that has to know about a derived column
## does, that the emitted script derives it the same way, and that every plot
## that orders `mesh` by size orders `metier4_mesh` the same way - gear by
## gear, each gear's meshes small to large - instead of falling back to the
## alphabetical order that puts "TBB_100-119" before "TBB_70-99".

source("dev/tests/fixture.R")
FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}
quiet <- function(expr) {
  out <- NULL
  invisible(utils::capture.output(out <- suppressMessages(expr)))
  out
}
suppressPackageStartupMessages(library(ggplot2))
drawable <- function(p)
  !is.null(p) && inherits(p, "ggplot") &&
  !inherits(try(ggplot_build(p), silent = TRUE), "try-error")

## ---- derive_fields(): the value ----------------------------------------
fleet <- c("TBB_DEF_70-99_0_0_all",   # the example
           "OTB_DEF_>=120_0_0",
           "GNS_DEF_100-119_0_0_all",
           "DRB_MOL_0_0_0_all",       # a mesh of "0" is still a mesh part
           "GNS_DEF_all_0_0_all",
           "OTB_DEF",                 # no mesh part: just the gear
           "MIS_MIS",
           "All",
           "TBB_DEF__0_0")            # an empty mesh part is no mesh either
x <- data.frame(fleetValue = fleet, areaValue = "27.4.b",
                vesselFlagCountry = "NL", seasonValue = 1L,
                stringsAsFactors = FALSE)
d <- derive_fields(x)
ok(identical(d$metier4_mesh,
             c("TBB_70-99", "OTB_>=120", "GNS_100-119", "DRB_0", "GNS_all",
               "OTB", "MIS", "All", "TBB")),
   "derive_fields(): metier4_mesh is gear_mesh, e.g. TBB_70-99")
ok(!anyNA(d$metier4_mesh) && !any(grepl("_$|_NA", d$metier4_mesh)),
   "derive_fields(): a fleet code with no mesh part is its gear - never NA, never TBB_NA")
has_mesh <- !is.na(d$mesh) & nzchar(d$mesh)
ok(identical(d$metier4_mesh[has_mesh],
             paste(d$metier4[has_mesh], d$mesh[has_mesh], sep = "_")),
   "derive_fields(): where there is a mesh, metier4_mesh is exactly metier4 + '_' + mesh")
ok(identical(derive_fields(d), d),
   "derive_fields(): calling it again changes nothing")

## ---- the bundled example already carries it ----------------------------
test_catch <- fx_catch(); test_dist <- fx_dist()
cc <- test_catch
ok("metier4_mesh" %in% names(cc) &&
     identical(cc$metier4_mesh, derive_fields(cc)$metier4_mesh),
   "test_catch: metier4_mesh is there and is what derive_fields() gives")
ok(identical(sort(unique(cc$metier4_mesh)), c("LHP_0", "MPT_0", "PS_0", "PS_16-31")),
   "test_catch: LHP_0 / MPT_0 / PS_0 / PS_16-31")

## ---- every list that has to know about a derived column ----------------
ok("metier4_mesh" %in% FILTER_VARS && "metier4_mesh" %in% DIMS_ALL_CANDIDATES &&
     "metier4_mesh" %in% PALETTE_VARS,
   "metier4_mesh is in FILTER_VARS, DIMS_ALL_CANDIDATES and PALETTE_VARS")
gv <- grouping_vars(cc)
ok("metier4_mesh" %in% gv && names(gv)[gv == "metier4_mesh"] == "metier4_mesh (gear_mesh)",
   "grouping_vars(): offered, labelled 'metier4_mesh (gear_mesh)'")
ok(which(gv == "metier5") < which(gv == "metier4_mesh") &&
     which(gv == "metier4_mesh") < which(gv == "metier4"),
   "grouping_vars(): sits between metier5 and metier4, with the other fleet levels")
fl <- explore_filter_levels(cc)
ok(identical(fl$metier4_mesh, c("LHP_0", "MPT_0", "PS_0", "PS_16-31")),
   "explore_filter_levels(): the filter lens offers it, with its levels")
ok(identical(unique(apply_filters(cc, list(metier4_mesh = "PS_16-31"))$metier4_mesh),
             "PS_16-31"),
   "apply_filters(): filtering on it keeps just that gear and mesh")
# Deliberately NOT a cascade key: the cascade's Fleet family is a strict
# chain (fleetValue > metier6 > metier5 > metier4) whose widening warnings
# assume every level determines the next, and gear_mesh is not on that
# chain - metier5 (gear_target) is its equal, neither determines the other.
ok(!"metier4_mesh" %in% unlist(key_vocab_grouped(cc)),
   "key_vocab_grouped(): not a cascade key (the Fleet chain is strictly nested)")

fam <- strata_families()$fleet
ok(fam[["metier4_mesh"]] == fam[["metier5"]] &&
     fam[["metier6"]] < fam[["metier4_mesh"]] && fam[["metier4_mesh"]] < fam[["metier4"]],
   "strata_families(): level 3 beside metier5, between metier6 and metier4")
ok(identical(finest_per_family(c("metier5", "metier4_mesh")), c("metier5", "metier4_mesh")),
   "finest_per_family(): metier5 and metier4_mesh each say something the other does not - both kept")
ok(identical(finest_per_family(c("metier4", "metier4_mesh")), "metier4_mesh") &&
     identical(finest_per_family(c("mesh", "metier4_mesh")), "metier4_mesh"),
   "finest_per_family(): metier4_mesh makes metier4 and mesh redundant")
ok(identical(finest_per_family(c("metier6", "metier4_mesh")), "metier6"),
   "finest_per_family(): metier6 makes metier4_mesh redundant")
s <- strata_sentence(c("metier4_mesh", "vesselFlagCountry"), cc)
ok(grepl("finer fleet levels (fleetValue)", s, fixed = TRUE) &&
     !grepl("over fleet (", s, fixed = TRUE),
   "strata_sentence(): a plot by metier4_mesh groups over the FINER fleet levels, not the whole fleet")

## ---- the coverage table keeps it -----------------------------------------
c23 <- subset(cc, year == 2023)
cv <- build_coverage_strata(c23)
ref <- unique(c23[c("fleetValue", "metier4_mesh")])
ok("metier4_mesh" %in% names(cv) &&
     identical(cv$metier4_mesh, ref$metier4_mesh[match(cv$fleetValue, ref$fleetValue)]),
   "build_coverage_strata(): the default (finest) table carries it, right for every stratum")
cvm <- build_coverage_strata(c23, c("metier4", "mesh"))
ok(!anyNA(cvm$metier4_mesh) &&
     identical(cvm$metier4_mesh, paste(cvm$metier4, cvm$mesh, sep = "_")),
   "build_coverage_strata(): a metier4 x mesh table carries it - the pair pins it")
cvg <- build_coverage_strata(c23, "metier4")
ok(all(is.na(cvg$metier4_mesh[cvg$metier4 == "PS"])) &&
     identical(cvg$metier4_mesh[cvg$metier4 == "LHP"], "LHP_0"),
   "build_coverage_strata(): a metier4-only table has it only where one mesh is all there is")

## ---- the emitted script derives it the same way ---------------------------
code <- c(emit_area_helper(), emit_derive_code())
ok(any(grepl("metier4_mesh", code, fixed = TRUE)),
   "emit_derive_code(): writes the metier4_mesh line")
env <- new.env()
env$catch_data <- x
env$mutate <- dplyr::mutate
env$`%>%` <- dplyr::`%>%`
eval(parse(text = code), envir = env)
ok(identical(env$catch_data$metier4_mesh, d$metier4_mesh),
   "emit_derive_code(): the script it writes gives the same metier4_mesh as derive_fields()")
ok(any(grepl("metier4_mesh", taf_derive(), fixed = TRUE)),
   "taf_derive(): the script header lists it among the derived columns")

## ---- the size order: gear by gear, each gear's meshes small to large ------
lv <- metier4_mesh_levels(c("TBB_100-119", "TBB_70-99", "OTB_>=120", "OTB_16-31",
                            "OTB_<16", "TBB_all", "TBB", "GNS_0", "OTB_90-99", NA,
                            "OTB_16-31"))
ok(identical(lv, c("GNS_0", "OTB_<16", "OTB_16-31", "OTB_90-99", "OTB_>=120",
                   "TBB_70-99", "TBB_100-119", "TBB_all", "TBB")),
   "metier4_mesh_levels(): gear alphabetical; meshes by size; 'all' then a bare gear last; NA and repeats dropped")
ok(identical(metier4_mesh_levels(factor(c("B_2-3", "A_10-19", "A_2-3"))),
             c("A_2-3", "A_10-19", "B_2-3")),
   "metier4_mesh_levels(): a factor is read by its labels, 10-19 after 2-3")
ok(identical(metier4_mesh_levels(character(0)), character(0)) &&
     identical(metier4_mesh_levels(NA_character_), character(0)),
   "metier4_mesh_levels(): nothing to order is not an error")
ok(identical(mesh_var_levels("mesh", c("100-119", "16-31")), c("16-31", "100-119")) &&
     identical(mesh_var_levels("metier4_mesh", c("B_2", "A_2")), c("A_2", "B_2")) &&
     is.null(mesh_var_levels("metier4", "TBB")) && is.null(mesh_var_levels(NULL, "x")),
   "mesh_var_levels(): knows mesh and metier4_mesh, and says NULL for anything else")
rd <- data.frame(mesh = c("100-119", "16-31"), metier4_mesh = c("TBB_100-119", "TBB_16-31"),
                 stringsAsFactors = FALSE)
r1 <- reorder_mesh(rd, "metier4_mesh")
ok(identical(levels(r1$metier4_mesh), c("TBB_16-31", "TBB_100-119")) && is.character(r1$mesh),
   "reorder_mesh(): releveling metier4_mesh leaves a mesh column it was not asked about alone")
r2 <- reorder_mesh(rd, c("mesh", "metier4_mesh"))
ok(identical(levels(r2$mesh), c("16-31", "100-119")) &&
     identical(levels(r2$metier4_mesh), c("TBB_16-31", "TBB_100-119")),
   "reorder_mesh(): both named columns are releveled")

## ---- the plots ---------------------------------------------------------------
# A synthetic mesh per (fleet, area), chosen so natural and alphabetical order
# disagree - "PS_100-119" sorts before "PS_16-31" and "PS_90-99" as text. Keyed
# off fleetValue + areaValue, which a stratum's Lan and Dis rows share, so the
# value agrees within a stratum and discard coverage linking still matches.
# It is written into fleetValue's mesh part and re-derived, not just pasted
# onto the mesh column: the length and age plots re-run derive_fields() on the
# catch, which would put the real mesh values straight back.
mesh_vals <- c("0", "16-31", "32-69", "70-99", "90-99", "100-119", ">=120")
key <- paste(c23$fleetValue, c23$areaValue)
new_mesh <- mesh_vals[(as.integer(factor(key)) - 1) %% length(mesh_vals) + 1]
c23$fleetValue <- vapply(seq_len(nrow(c23)), function(i) {
  z <- strsplit(c23$fleetValue[i], "_", fixed = TRUE)[[1]]
  z[3] <- new_mesh[i]
  paste(z, collapse = "_")
}, "")
c23 <- derive_fields(c23)
natural <- metier4_mesh_levels(c23$metier4_mesh)
# more than three levels, or "in order" could be alphabetical order by luck
in_order <- function(lv) length(lv) > 3 && identical(lv, natural[natural %in% lv])
ok(length(natural) >= 6 && !identical(natural, sort(unique(c23$metier4_mesh))),
   "fixture: enough gear/mesh pairs, and the size order really differs from alphabetical")

p1 <- plot_coverage_bar(c23, y = "vesselFlagCountry", fill = "metier4_mesh")
ok(drawable(p1) && length(levels(p1$data$.fill)) > 3 && in_order(levels(p1$data$.fill)),
   "plot_coverage_bar(): fill = metier4_mesh orders the legend gear by gear, by size")
p1b <- plot_coverage_bar(c23, y = "metier4_mesh")
ok(drawable(p1b) && in_order(levels(p1b$data$.fill)),
   "plot_coverage_bar(): metier4_mesh on y (it is also the default fill) orders the legend")
p1f <- plot_coverage_bar(c23, y = "vesselFlagCountry", facet = "metier4_mesh")
ok(drawable(p1f) && in_order(levels(p1f$data$.facet)),
   "plot_coverage_bar(): facet = metier4_mesh orders the panels")
p2 <- plot_coverage_heat(c23, y = "metier4_mesh", x = "vesselFlagCountry")
ok(drawable(p2) && in_order(levels(p2$data$metier4_mesh)),
   "plot_coverage_heat(): y = metier4_mesh orders the rows")
p3 <- plot_coverage_bubble(c23, y = "vesselFlagCountry", x = "metier4_mesh")
ok(drawable(p3) && in_order(levels(p3$data$metier4_mesh)),
   "plot_coverage_bubble(): x = metier4_mesh orders the columns")
p4 <- quiet(plot_coverage_map(c23, group = "metier4_mesh"))
ok(drawable(p4), "plot_coverage_map(): draws with group = metier4_mesh")
glab <- p4$layers[[which(vapply(p4$layers,
  function(l) is.data.frame(l$data) && all(c("k", "glab") %in% names(l$data)),
  logical(1)))[1]]]$data
present <- metier4_mesh_levels(unique(glab$glab))
ok(identical(glab$k[match(present, glab$glab)], seq_along(present)),
   "plot_coverage_map(): group = metier4_mesh places the grid cells in size order")
p5 <- plot_DR(c23, x = "metier4_mesh")
ok(drawable(p5) && in_order(levels(p5$data$metier4_mesh)),
   "plot_DR(): x = metier4_mesh orders the x axis")
p6 <- plot_NaLoA(c23, test_dist, fill = "metier4_mesh")
ok(drawable(p6) && length(levels(p6$data$metier4_mesh)) > 3 &&
     in_order(levels(p6$data$metier4_mesh)),
   "plot_NaLoA(): fill = metier4_mesh orders the legend")
p7 <- plot_MWaLoA(c23, test_dist, colour = "metier4_mesh")
ok(drawable(p7) && length(levels(p7$data$metier4_mesh)) > 3 &&
     in_order(levels(p7$data$metier4_mesh)),
   "plot_MWaLoA(): colour = metier4_mesh orders the legend")

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)