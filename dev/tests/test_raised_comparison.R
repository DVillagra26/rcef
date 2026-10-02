## plot_comparison() and raised_overview_table() on two or more raised catches.
##
## Both take the raised catches themselves - nothing else - written list(a, b)
## or c(a, b), name the scenarios after the objects unless told otherwise, and
## sum each raised catch at whatever x / facet / by the caller names. The
## stratum-level pair the application uses (compare_scenarios() +
## plot_scenario_comparison()) is the old plot_comparison(), renamed; its
## drawing is pinned at the end.
##
## Uses the bundled anchoveta example and run_native_raising(), as the other
## after-raising tests do, so it runs anywhere.

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
fails <- function(expr, pattern = "")
  inherits(tr <- try(expr, silent = TRUE), "try-error") &&
    grepl(pattern, conditionMessage(attr(tr, "condition")), fixed = TRUE)
suppressPackageStartupMessages(library(ggplot2))
drawable <- function(p)
  !is.null(p) && inherits(p, "ggplot") &&
  !inherits(try(ggplot_build(p), silent = TRUE), "try-error")

data(anchoveta_catch)
c23  <- subset(anchoveta_catch, year == 2023)
dims <- c("metier4", "ICESDivision", "seasonValue")
lf   <- tempfile(fileext = ".txt")
raise_with <- function(th) {
  grp <- quiet(define_raising_groups_auto(c23, hierarchy = dims, thresholds = th))
  quiet(run_native_raising(c23, dims, grp, type = "discards", log_file = lf))
}
raised_strict  <- raise_with(c(95, 90, 85))
raised_relaxed <- raise_with(c(10, 10, 10))
raised_third   <- raise_with(c(60, 50, 40))
dis_t <- function(r) sum(to_tonnes(r$total, r$variableUnit)[r$catchCategory == "Dis"],
                         na.rm = TRUE)
ok(abs(dis_t(raised_strict) - dis_t(raised_relaxed)) > 1e-6,
   "fixture: the two ways of raising give different discards")

## ---- the scenarios: how they are read and named --------------------------
p <- plot_comparison(c(raised_strict, raised_relaxed), x = "metier4")
ok(drawable(p), "plot_comparison(): draws with c(a, b)")
ok(identical(levels(p$data$scenario), c("raised_strict", "raised_relaxed")),
   "c(a, b): the scenarios are named after the objects, in the order given")
p <- plot_comparison(list(raised_strict, raised_relaxed), x = "metier4")
ok(identical(levels(p$data$scenario), c("raised_strict", "raised_relaxed")),
   "list(a, b): the same")
p <- plot_comparison(c(usual = raised_strict, `usual with 3a` = raised_relaxed),
                     x = "metier4")
ok(identical(levels(p$data$scenario), c("usual", "usual with 3a")),
   "names given inside c() / list() are used")
p <- plot_comparison(c(usual = raised_strict, raised_relaxed), x = "metier4")
ok(identical(levels(p$data$scenario), c("usual", "raised_relaxed")),
   "one name given, one left to the object")
p <- plot_comparison(c(raised_strict, raised_relaxed),
                     scenarios_names = c("Usual method", "Usual method with 3.a.21"),
                     x = "metier4")
ok(identical(levels(p$data$scenario), c("Usual method", "Usual method with 3.a.21")),
   "scenarios_names sets them, whatever the objects are called")
both <- list(first = raised_strict, second = raised_relaxed)
p <- plot_comparison(both, x = "metier4")
ok(identical(levels(p$data$scenario), c("first", "second")),
   "a list built beforehand keeps its own names")
p <- plot_comparison(unname(both), x = "metier4")
ok(identical(levels(p$data$scenario), c("scenario 1", "scenario 2")),
   "an unnamed list built beforehand gets scenario 1, scenario 2")
p3 <- plot_comparison(c(raised_strict, raised_relaxed, raised_third), x = "metier4")
ok(drawable(p3) && nlevels(p3$data$scenario) == 3,
   "three scenarios draw")

ok(fails(plot_comparison(raised_strict, x = "metier4"), "two or more"),
   "a single raised catch is refused: there is nothing to compare")
ok(fails(plot_comparison(c(raised_strict, raised_relaxed),
                         scenarios_names = "only one", x = "metier4"),
         "`scenarios_names` has 1"),
   "scenarios_names of the wrong length is refused")
ok(fails(plot_comparison(c(raised_strict, raised_relaxed),
                         scenarios_names = c("same", "same"), x = "metier4"),
         "must all be different"),
   "duplicate scenarios_names are refused")
stored <- c(raised_strict, raised_relaxed)
ok(fails(plot_comparison(stored, x = "metier4"), "not a raised catch"),
   "a c() of data frames stored beforehand (a flat list of columns) is refused, and says why")
ok(fails(plot_comparison(c(raised_strict, c23), x = "metier4"), "'c23' is not a raised catch"),
   "a catch that was never raised is refused by name")
ok(fails(plot_comparison(c(raised_strict, raised_relaxed), x = "nope"), "has no column 'nope'"),
   "an x column the raised catches lack is refused by name")
broken <- raised_relaxed; broken$metier4 <- NULL
ok(fails(plot_comparison(c(raised_strict, broken), x = "metier4"), "'broken' has no column"),
   "and it names the raised catch that lacks it")
ok(fails(plot_comparison(c(raised_strict, raised_relaxed), x = "metier4", catchCategory = "BMS"),
         "`catchCategory` must be"),
   "an unknown catchCategory is refused")

## ---- plot_comparison(): what is drawn ------------------------------------
p <- plot_comparison(c(raised_strict, raised_relaxed), x = "metier4")
ok(identical(p$labels$title, "Raised catch per stratum, by scenario") &&
     identical(p$labels$y, "Catch (t)"),
   "catch weight by default: title and y axis say Catch")
ok(grepl("color = discard raising/allocation scenario", p$labels$caption, fixed = TRUE) &&
     grepl("shade = imported", p$labels$caption, fixed = TRUE),
   "the caption says what colour and shade are")
ok(identical(levels(p$data$src), c("Imported", "Raised")) &&
     all(p$data$src %in% c("Imported", "Raised")),
   "every bar part is Imported or Raised")
ok(setequal(round(unique(ggplot_build(p)$data[[1]]$alpha), 2), c(1, 0.45)),
   "imported is solid (alpha 1), raised transparent (0.45)")
ok(all(p$data$t >= 0) && nrow(p$data) == nrow(unique(p$data[c(".x", "scenario", "src")])),
   "one bar part per x, scenario and imported / raised")
# the two plots agree: a scenario's bars add up to what plot_raised_catches() draws
rc <- function(r) sum(plot_raised_catches(r, x = "metier4")$data$t)
ok(isTRUE(all.equal(sum(p$data$t[p$data$scenario == "raised_strict"]), rc(raised_strict))) &&
     isTRUE(all.equal(sum(p$data$t[p$data$scenario == "raised_relaxed"]), rc(raised_relaxed))),
   "a scenario's bars add up to the weight plot_raised_catches() draws for it")

pd <- plot_comparison(c(raised_strict, raised_relaxed), x = "metier4", catchCategory = "Dis")
pl <- plot_comparison(c(raised_strict, raised_relaxed), x = "metier4", catchCategory = "lan")
ok(identical(pd$labels$title, "Raised discards per stratum, by scenario") &&
     identical(pd$labels$y, "Discards (t)") &&
     identical(pl$labels$y, "Landings (t)"),
   "catchCategory = Dis / lan: the title and y axis follow")
ok(isTRUE(all.equal(sum(pd$data$t[pd$data$scenario == "raised_strict"]), dis_t(raised_strict))),
   "discards only: the bars are the discards")
ok(all(pl$data$src == "Imported"),
   "landings only: every landing is imported")
# BMS (the example has some) is part of the catch, not of the discards
t_of <- function(r, cc) sum(to_tonnes(r$total, r$variableUnit)[r$catchCategory %in% cc],
                            na.rm = TRUE)
ok(t_of(raised_strict, "BMS") > 0 &&
     isTRUE(all.equal(sum(p$data$t[p$data$scenario == "raised_strict"]),
                      t_of(raised_strict, c("Lan", "Dis", "BMS")))) &&
     isTRUE(all.equal(sum(pl$data$t[pl$data$scenario == "raised_strict"]),
                      t_of(raised_strict, "Lan"))),
   "the catch is every category (BMS included); landings are only the landings")

pf <- plot_comparison(c(raised_strict, raised_relaxed), x = "metier4", facet = "ICESDivision")
ok(drawable(pf) && ".f" %in% names(pf$data) && nlevels(pf$data$.f) > 1,
   "facet = ICESDivision: one panel per level")
pn <- plot_comparison(c(raised_strict, raised_relaxed))
ok(drawable(pn) && identical(levels(pn$data$.x), "All"),
   "no x: one group of bars for everything")
ok(drawable(plot_comparison(c(raised_strict, raised_relaxed), x = "metier4",
                            palette = "colorblind")),
   "palette = colorblind draws")

# seasonValue is an integer: its levels read 1, 2, 3, 4 - and 2 before 10
ps <- plot_comparison(c(raised_strict, raised_relaxed), x = "seasonValue")
ok(drawable(ps) && identical(levels(ps$data$.x), as.character(sort(unique(c23$seasonValue)))),
   "an integer x (seasonValue) is a discrete axis in numeric order")
bump_a <- raised_strict;  bump_a$seasonValue <- ifelse(bump_a$seasonValue == 4, 10L, bump_a$seasonValue)
bump_b <- raised_relaxed; bump_b$seasonValue <- ifelse(bump_b$seasonValue == 4, 10L, bump_b$seasonValue)
pb <- plot_comparison(c(bump_a, bump_b), x = "seasonValue")
ok(tail(levels(pb$data$.x), 1) == "10" && which(levels(pb$data$.x) == "2") <
     which(levels(pb$data$.x) == "10"),
   "numeric order, not text order: 2 comes before 10")

# mesh in size order, as everywhere else
mesh_vals <- c("0", "16-31", "32-69", "70-99", "90-99", "100-119", ">=120")
with_mesh <- function(r) {
  r$mesh <- mesh_vals[(as.integer(factor(paste(r$fleetValue, r$areaValue))) - 1) %%
                        length(mesh_vals) + 1]
  r
}
pm <- plot_comparison(c(with_mesh(raised_strict), with_mesh(raised_relaxed)), x = "mesh")
ok(length(levels(pm$data$.x)) > 3 &&
     identical(levels(pm$data$.x), mesh_levels(levels(pm$data$.x))),
   "x = mesh: the groups run by mesh size")

## ---- raised_overview_table(): one raised catch is the old table -----------
t1 <- raised_overview_table(raised_strict)
ok(identical(names(t1), c("Catch category", "Raised/Imported", "Caton (t)",
                          "Percent of category")),
   "one raised catch: the four columns it always had, no Scenario column")
ok(identical(t1$`Catch category`[nrow(t1)], "Total") && t1$`Percent of category`[nrow(t1)] == 100,
   "and a Total row at the end")
tt <- to_tonnes(raised_strict$total, raised_strict$variableUnit)
ok(isTRUE(all.equal(t1$`Caton (t)`[t1$`Catch category` == "Total"], round(sum(tt, na.rm = TRUE), 3))),
   "the Total is the whole catch in tonnes")
ok(all(t1$`Percent of category`[t1$`Catch category` == "Lan"] == 100),
   "landings are all imported: 100%")
dd <- t1[t1$`Catch category` == "Dis", ]
ok(abs(sum(dd$`Percent of category`) - 100) <= 1,
   "imported and raised discards add up to 100%")
ok(identical(raised_overview_table(raised = raised_strict), t1),
   "the old raised = argument still finds its place")

## ---- raised_overview_table(): several raised catches ------------------------
t2 <- raised_overview_table(c(raised_strict, raised_relaxed))
ok(identical(names(t2)[1:2], c("Scenario", "Catch category")) &&
     setequal(unique(t2$Scenario), c("raised_strict", "raised_relaxed")),
   "several raised catches: a Scenario column, named after the objects")
ok(identical(unique(t2$Scenario), c("raised_strict", "raised_relaxed")),
   "in the order given")
ok(all(diff(match(t2$Scenario[t2$`Catch category` == "Dis" & t2$`Raised/Imported` == "Imported"],
                  c("raised_strict", "raised_relaxed"))) != 0),
   "the scenarios of one figure sit next to each other, so it reads across")
ok(isTRUE(all.equal(
     t2$`Caton (t)`[t2$Scenario == "raised_strict" & t2$`Catch category` == "Total"],
     t1$`Caton (t)`[t1$`Catch category` == "Total"])),
   "each scenario's rows are what that raised catch gives alone")
t2n <- raised_overview_table(c(raised_strict, raised_relaxed),
                             scenarios_names = c("A", "B"))
ok(setequal(unique(t2n$Scenario), c("A", "B")), "scenarios_names applies to the table too")
ok(fails(raised_overview_table(c(raised_strict, raised_relaxed), scenarios_names = "A"),
         "`scenarios_names` has 1"),
   "and is checked there too")

## ---- raised_overview_table(by = ...): the variables to aggregate at -------
tb <- raised_overview_table(c(raised_strict, raised_relaxed), by = "seasonValue")
ok(identical(names(tb)[1:4], c("Scenario", "Catch category", "seasonValue", "Raised/Imported")),
   "by = seasonValue: the column sits between the category and imported / raised")
seasons <- as.character(sort(unique(c23$seasonValue)))
ok(identical(unique(tb$seasonValue), seasons),
   "and runs in numeric order")
# every figure is split by season: the seasons' Totals add up to the whole
tot_all <- t2$`Caton (t)`[t2$`Catch category` == "Total"]
names(tot_all) <- t2$Scenario[t2$`Catch category` == "Total"]
tot_by <- tapply(tb$`Caton (t)`[tb$`Catch category` == "Total"],
                 tb$Scenario[tb$`Catch category` == "Total"], sum)
ok(isTRUE(all.equal(as.numeric(tot_by[names(tot_all)]), as.numeric(tot_all), tolerance = 1e-3)),
   "the Totals of the splits add up to the unsplit Total")
cells <- tb[tb$Scenario == "raised_strict" & tb$seasonValue == seasons[1] &
              tb$`Catch category` == "Dis", ]
ok(abs(sum(cells$`Percent of category`) - 100) <= 1,
   "within a split, imported and raised discards add up to 100%")
tb2 <- raised_overview_table(c(raised_strict, raised_relaxed), by = c("metier4", "seasonValue"))
ok(identical(names(tb2)[3:4], c("metier4", "seasonValue")) &&
     nrow(unique(tb2[c("metier4", "seasonValue")])) > length(seasons),
   "by = c(metier4, seasonValue): both columns, every combination that exists")
ok(fails(raised_overview_table(c(raised_strict, raised_relaxed), by = "nope"), "has no column 'nope'"),
   "a by column the raised catches lack is refused by name")
ok(identical(raised_overview_table(c(raised_strict, raised_relaxed), by = "catchCategory"), t2),
   "by = catchCategory changes nothing: it is already the split")

# mesh in size order here too
tm <- raised_overview_table(c(with_mesh(raised_strict), with_mesh(raised_relaxed)), by = "mesh")
ok(identical(unique(tm$mesh), mesh_levels(unique(tm$mesh))) && length(unique(tm$mesh)) > 3,
   "by = mesh: the splits run by mesh size")
tnum <- raised_overview_table(c(bump_a, bump_b), by = "seasonValue")
ok(tail(unique(tnum$seasonValue), 1) == "10",
   "by a numeric column: 2 before 10, not after")

## ---- rows that were raised to nothing -------------------------------------
ok(!"No donor (rows)" %in% names(t2),
   "no No donor column when every raised row has a rate")
nan_a <- raised_strict
hit <- which(nan_a$importedOrRaised == "raised" & nan_a$catchCategory == "Dis")[1:2]
nan_a$total[hit] <- NaN
tn <- raised_overview_table(c(nan_a, raised_relaxed))
ok("No donor (rows)" %in% names(tn) &&
     sum(tn$`No donor (rows)`[tn$Scenario == "nan_a" & tn$`Raised/Imported` == "Raised"]) == 2 &&
     sum(tn$`No donor (rows)`[tn$Scenario == "raised_relaxed"]) == 0,
   "raised rows with a NaN total are counted, in the scenario that has them")
tg <- raised_overview_table(c(nan_a, raised_relaxed), by = "DrGroup")
grp_hit <- unique(nan_a$DrGroup[hit])
ok(all(tg$`No donor (rows)`[tg$Scenario == "nan_a" & tg$DrGroup %in% grp_hit &
                              tg$`Raised/Imported` == "Raised"] > 0) &&
     sum(tg$`No donor (rows)`[tg$Scenario == "nan_a" & tg$`Raised/Imported` == "Raised"]) == 2,
   "by = DrGroup says which groups had no donor")
pn2 <- plot_comparison(c(nan_a, raised_relaxed), x = "metier4")
ok(drawable(pn2) && all(is.finite(pn2$data$t)),
   "plot_comparison(): NaN rows count as no weight and do not break it")

## ---- the stratum-level pair the application uses ---------------------------
sca <- scenario_auto("A", c("metier4", "vesselFlagCountry"),
                     c("metier4", "vesselFlagCountry"), c(50, 50), self_threshold = 50)
scb <- scenario_auto("B", c("metier4", "vesselFlagCountry"), "metier4", 50,
                     self_threshold = 50)
runs <- quiet(lapply(list(A = sca, B = scb), run_scenario, catch_data = c23))
cmp <- compare_scenarios(runs, c("metier4", "vesselFlagCountry"))
ok(drawable(plot_scenario_comparison(cmp, "metier4")) &&
     drawable(plot_scenario_comparison(cmp, "vesselFlagCountry", "metier4")),
   "plot_scenario_comparison(): the stratum-level plot the app uses still draws")
ok(is.data.frame(comparison_table(cmp, c("metier4", "vesselFlagCountry"))),
   "and comparison_table() still reads compare_scenarios()")

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
