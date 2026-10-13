## plot_allocated_NaLoA() / plot_allocated_MWaLoA(): numbers and mean weight at
## length or age from the RESULT of the allocation, the shade always saying
## sampled (solid) or estimated by the allocation (transparent).
##
## They are built on the framework's own aggregations - catchAtAoLAggregation()
## and meanWoLatAoLAggregation() - run on the allocation result. What is pinned
## here: the numbers and mean weights ARE those aggregations (units, classes in
## cm, thousands), the sampled part is what plot_NaLoA() draws from the same
## data, the estimated distributions of strata with no domain are found (they
## exist only in the result's own $catch), the shade is mapped and means what
## it says, and the catch category, facets, plus group and length bins.
##
## The fixture is the synthetic test fixture with ONE catch row per domainBiology:
## the framework's aggregation counts a distribution once per catch row that
## names its domain (as the allocation itself does), which is right for CEF
## files built from InterCatch and not for the example as it is bundled.

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
# the value of expr, with the messages and warnings it gave
heard <- function(expr) {
  msgs <- character(); warns <- character()
  val <- withCallingHandlers(expr,
    message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") },
    warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") })
  list(value = val, messages = msgs, warnings = warns)
}
err <- function(expr) tryCatch({ expr; "" }, error = function(e) conditionMessage(e))
suppressPackageStartupMessages(library(ggplot2))
lf <- tempfile(fileext = ".txt")

## ---- the fixture ---------------------------------------------------------------
test_catch <- fx_catch(); test_dist <- fx_dist()
c23 <- subset(test_catch, year == 2023)
d23 <- as.data.frame(subset(test_dist, year == 2023))
hier <- c("metier4", "ICESDivision", "seasonValue")
grp <- quiet(define_raising_groups_auto(c23, hierarchy = hier, thresholds = c(50, 40, 30)))
raised <- as.data.frame(quiet(run_native_raising(c23, hier, grp, type = "discards",
                                                 log_file = lf)))
# a domainBiology of its own for every catch row (per country and catch category);
# the raised discards have none, and get "[alloc]_season_area_unit_fleet_category"
# from the allocation, so those must be of their own as well
ckey <- paste(raised$vesselFlagCountry, raised$catchCategory, raised$domainBiology)
akey <- paste(raised$vesselFlagCountry, raised$seasonValue, raised$areaValue,
              raised$fisheriesManagementUnit, raised$fleetValue, raised$catchCategory)
n_ck <- ave(seq_along(ckey), ckey, FUN = length)
n_ak <- ave(seq_along(akey), akey, FUN = length)
catch1 <- raised[ifelse(is.na(raised$domainBiology), n_ak == 1L, n_ck == 1L), ]
dist1  <- d23[paste(d23$vesselFlagCountry, d23$catchCategory, d23$domainBiology) %in%
                paste(catch1$vesselFlagCountry, catch1$catchCategory, catch1$domainBiology), ]
galloc <- list(ALL = list(alloc = "TRUE", matched = "TRUE"))
al <- quiet(run_allocation(catch1, dist1, galloc, app_type = "LengthTotal",
                           verbose = FALSE, log_file = lf))
aa <- quiet(run_allocation(catch1, dist1, galloc, app_type = "Age",
                           verbose = FALSE, log_file = lf))
D <- al$distribution
ok(!inherits(al, "alloc_error") && !inherits(aa, "alloc_error"),
   "fixture: both allocations ran")
ok(all(c("sampled", "estimated") %in% D$sampledOrEstimated[D$catchCategory == "Lan"]) &&
     all(c("sampled", "estimated") %in% D$sampledOrEstimated[D$catchCategory == "Dis"]),
   "fixture: landings and discards both have sampled and estimated distributions")
est_dom <- unique(as.character(D$domainBiology[D$sampledOrEstimated == "estimated"]))
ok(any(startsWith(est_dom, "[alloc]")) && !all(est_dom %in% catch1$domainBiology) &&
     all(est_dom %in% al$catch$domainBiology),
   "fixture: some estimated distributions carry a domain only the result's own $catch knows")
raw_k <- function(cc, st) sum(D$value[D$catchCategory == cc & D$sampledOrEstimated == st &
                                      D$variableType == "Number"]) * 1e-3
tot <- function(p, st) sum(p$data$number[p$data$.status == st])
near <- function(a, b) isTRUE(all.equal(a, b, tolerance = 1e-9))

## ---- why the result's own catch: the input catch cannot find the estimated ones ----
lin_in  <- link_dist_to_strata(catch1, D, "vesselFlagCountry", catch_category = "Dis",
                               type = "LengthTotal")
lin_res <- link_dist_to_strata(al$catch, D, "vesselFlagCountry", catch_category = "Dis",
                               type = "LengthTotal")
n_est_in  <- sum(lin_in$sampledOrEstimated == "estimated")
n_est_res <- sum(lin_res$sampledOrEstimated == "estimated")
ok(n_est_res > 5 * n_est_in && !any(startsWith(as.character(lin_in$domainBiology), "[alloc]")),
   "linked to the catch that went in, the estimated discards are mostly lost; to the result's $catch they are all there")

## ---- numbers: the framework's aggregation -----------------------------------------
pl <- plot_allocated_NaLoA(al, catchCategory = "Lan", fill = "vesselFlagCountry")
pd <- plot_allocated_NaLoA(al, catchCategory = "Dis", fill = "vesselFlagCountry")
ok(inherits(pl, "ggplot") && inherits(pd, "ggplot"), "plot_allocated_NaLoA(): draws")
ok(near(tot(pl, "Sampled"), raw_k("Lan", "sampled")) &&
     near(tot(pl, "Estimated"), raw_k("Lan", "estimated")) &&
     near(tot(pd, "Sampled"), raw_k("Dis", "sampled")) &&
     near(tot(pd, "Estimated"), raw_k("Dis", "estimated")),
   "numbers: sampled and estimated, landings and discards, are the allocation's own numbers (in thousands)")
ag <- agg_numbers(al, grouping = c("catchCategory", "sampledOrEstimated"))
ag_lan <- ag[ag$catchCategory == "Lan" & ag$sampledOrEstimated == "sampled", ]
ok(near(sum(unlist(ag_lan[grep("^N_", names(ag_lan))])) * 1e-3, tot(pl, "Sampled")),
   "numbers: the same as agg_numbers() - catchAtAoLAggregation() - of that result")
raw_cls <- range(as.numeric(D$distributionClass[D$distributionType == "LengthTotal"])) / 10
ok(near(range(pl$data$.class), raw_cls),
   "classes: the file's mm come out in cm")
p_n0 <- plot_NaLoA(catch1, dist1, catchCategory = "Lan", fill = "vesselFlagCountry")
s <- pl$data[pl$data$.status == "Sampled", ]
m <- merge(s[c("vesselFlagCountry", ".class", "number")],
           p_n0$data[c("vesselFlagCountry", ".class", "number")],
           by = c("vesselFlagCountry", ".class"), all = TRUE)
ok(nrow(m) == nrow(s) && !anyNA(m) && near(m$number.x, m$number.y),
   "the sampled part is exactly what plot_NaLoA() draws from the same catch and distributions")

## ---- the shade -------------------------------------------------------------------
ok(identical(levels(pl$data$.status), c("Sampled", "Estimated")) &&
     identical(rlang::as_label(pl$mapping$alpha), ".status"),
   "the shade is mapped to sampled / estimated, sampled first")
a_sc <- pl$scales$get_scales("alpha")
ok(!is.null(a_sc) && near(unname(a_sc$palette(2)), c(1, 0.45)),
   "sampled is solid (1) and estimated transparent (0.45) - as in the raised plots")
b <- ggplot_build(pl)$data[[1]]
ok(all(sort(unique(round(b$alpha, 2))) == c(0.45, 1)),
   "both shades are really drawn")
ok(grepl("sampled (solid), or estimated by the allocation (transparent)", pl$labels$caption, fixed = TRUE) &&
     grepl("after the allocation", pl$labels$title, fixed = TRUE),
   "the caption says what the shade means, the title says it is after the allocation")
ok(identical(pl$theme$legend.position, "right"),
   "the legends sit outside the panel (they are tall and would cover the bars)")

## ---- catch category, facets, fill ---------------------------------------------------
pa <- plot_allocated_NaLoA(al, catchCategory = "All")
ok(near(tot(pa, "Sampled"), raw_k("Lan", "sampled") + raw_k("Dis", "sampled")) &&
     near(tot(pa, "Estimated"), raw_k("Lan", "estimated") + raw_k("Dis", "estimated")) &&
     grepl("^Catch - ", pa$labels$title),
   "catchCategory = 'All' adds landings and discards, and is titled catch")
ok(grepl("^Landings - ", pl$labels$title) && grepl("^Discards - ", pd$labels$title) &&
     grepl("Landings NaL", pl$labels$caption, fixed = TRUE),
   "landings and discards say so, in the title and the caption")
ok(inherits(plot_allocated_NaLoA(al, fill = "metier4", facet = "vesselFlagCountry")$facet, "FacetWrap") &&
     inherits(plot_allocated_NaLoA(al, fill = "metier4",
                                   facet = c("vesselFlagCountry", "seasonValue"))$facet, "FacetGrid"),
   "one facet column wraps, two make a grid")
ps <- plot_allocated_NaLoA(al, fill = "seasonValue", facet = "seasonValue")
ok(inherits(ps, "ggplot") && is.factor(ps$data$seasonValue) &&
     !inherits(try(ggplot_build(ps), silent = TRUE), "try-error"),
   "an integer column (seasonValue) is a category: fill and facet work")
ok(inherits(plot_allocated_NaLoA(al, fill = "metier4", stacked = FALSE), "ggplot") &&
     grepl("dodged by metier4", plot_allocated_NaLoA(al, fill = "metier4", stacked = FALSE)$labels$caption,
           fixed = TRUE),
   "stacked = FALSE dodges, and the caption says so")
ok(!inherits(try(ggplot_build(plot_allocated_NaLoA(al)), silent = TRUE), "try-error"),
   "no fill and no facet: one colour, the shade alone")
# a column the catch does not carry is derived, as everywhere else
al_nm <- al; al_nm$catch <- al$catch[setdiff(names(al$catch), "mesh")]
ok(identical(plot_allocated_NaLoA(al_nm, fill = "mesh")$data,
             plot_allocated_NaLoA(al, fill = "mesh")$data),
   "a derived column the result's catch lacks (mesh) is derived on the way")

## ---- what the allocation ran on ----------------------------------------------------------
# rows of another origin are not what was allocated: they must not double the numbers
off <- al$catch[!is.na(al$catch$allocGroup), ]
off$originType <- "Official"; off$allocGroup <- NA
al_off <- al; al_off$catch <- rbind(al$catch, off)
ok(identical(plot_allocated_NaLoA(al_off, catchCategory = "Lan", fill = "vesselFlagCountry")$data, pl$data),
   "catch rows of another origin than the allocation ran on are left out")
# a distribution the allocation left as it was is a sampled one
al_na <- al
al_na$distribution$sampledOrEstimated[al$distribution$sampledOrEstimated == "sampled"] <- NA
ok(identical(plot_allocated_NaLoA(al_na, catchCategory = "Lan", fill = "vesselFlagCountry")$data, pl$data),
   "a distribution with no sampledOrEstimated is a sampled one")
# the engine's own list, without the checks run_allocation() adds
al_eng <- list(distribution = al$distribution, catch = al$catch)
ok(identical(plot_allocated_NaLoA(al_eng, catchCategory = "Lan", fill = "vesselFlagCountry")$data, pl$data),
   "the list catchAllocationAoL() returns works the same")

## ---- mean weight: the framework's aggregation --------------------------------------------------
pw <- plot_allocated_MWaLoA(al, catchCategory = "Lan", colour = "vesselFlagCountry")
ok(inherits(pw, "ggplot"), "plot_allocated_MWaLoA(): draws")
lanD <- D[D$catchCategory == "Lan", ]; lanC <- al$catch[al$catch$catchCategory == "Lan", ]
w_fw <- meanWoLatAoLAggregation(distribution_data = lanD, catch_data = lanC,
                                grouping = c("sampledOrEstimated", "vesselFlagCountry"),
                                weighting = "NumberAtAoL", originType_catch = "WGEstimate",
                                variableType_catch = "WeightLive", variableType_dist = "WeightLive",
                                distributionType = "LengthTotal", unit = "g", round = NULL,
                                plusGroup = FALSE)
w_long <- alloc_pivot_long(w_fw, "W", ".mw")
w_long$.status <- ifelse(w_long$sampledOrEstimated == "estimated", "Estimated", "Sampled")
w_long$.class <- w_long$.cls / 10
mm <- merge(w_long[c("vesselFlagCountry", ".status", ".class", ".mw")],
            transform(pw$data, .status = as.character(.status))[c("vesselFlagCountry", ".status", ".class", "mean_weight")],
            by = c("vesselFlagCountry", ".status", ".class"), all = TRUE)
ok(nrow(mm) == nrow(pw$data) && !anyNA(mm) && near(mm$.mw, mm$mean_weight),
   "mean weights: those of meanWoLatAoLAggregation(), in g, at classes in cm")
ok(identical(rlang::as_label(pw$mapping$alpha), ".status") &&
     near(unname(pw$scales$get_scales("alpha")$palette(2)), c(1, 0.45)) &&
     grepl("sampled (solid), or estimated by the allocation (transparent)", pw$labels$caption, fixed = TRUE),
   "mean weight: the same shade, and the caption says so")
ok(length(unique(ggplot_build(pw)$data[[1]]$group)) ==
     nrow(unique(pw$data[c("vesselFlagCountry", ".status")])),
   "one line per colour level and per sampled / estimated")
pw0 <- plot_allocated_MWaLoA(al)
ok(!inherits(try(ggplot_build(pw0), silent = TRUE), "try-error") &&
     identical(rlang::as_label(pw0$mapping$alpha), ".status") &&
     !inherits(try(ggplot_build(plot_allocated_MWaLoA(al, colour = "metier4",
                                                      facet = c("vesselFlagCountry", "seasonValue"))),
                   silent = TRUE), "try-error"),
   "no colour (the shade still tells sampled from estimated), and a grid of facets, draw")

## ---- ages: the plus group -------------------------------------------------------------------------
na0 <- plot_allocated_NaLoA(aa, distributionType = "Age", catchCategory = "Lan")
na3 <- plot_allocated_NaLoA(aa, distributionType = "Age", catchCategory = "Lan", plus_group = 3)
ok(near(sum(na3$data$number), sum(na0$data$number)) && max(na3$data$.class) == 3 &&
     any(na3$data$.plus) && max(na0$data$.class) > 3 &&
     grepl("3+ = ages 3+ pooled", na3$labels$caption, fixed = TRUE),
   "plus_group = 3: ages 3 and older are one class, nothing lost, and the caption says it")
ok(identical(na0$labels$x, "Age (years)") && identical(pl$labels$x, "Length (cm)") &&
     identical(pl$labels$y, "Numbers (thousands)") && identical(pw$labels$y, "Mean weight (g)"),
   "ages in years, lengths in cm, numbers in thousands, mean weight in g")
nw0 <- plot_allocated_MWaLoA(aa, distributionType = "Age", catchCategory = "Lan")
nw3 <- plot_allocated_MWaLoA(aa, distributionType = "Age", catchCategory = "Lan", plus_group = 3)
wt <- function(nn, ww) { j <- merge(nn$data, ww$data, by = c(".class", ".status")); sum(j$number * j$mean_weight) }
ok(near(wt(na3, nw3), wt(na0, nw0)),
   "plus_group on the mean weight: the pooled class is weighted by its numbers (numbers x weight is kept)")
w <- heard(plot_allocated_NaLoA(aa, distributionType = "Age", plus_group = 99))
ok(inherits(w$value, "ggplot") && length(w$warnings) == 1L && grepl("no age reaches plus_group = 99", w$warnings),
   "a plus group nothing reaches is said, and not drawn")

## ---- lengths: the bins -----------------------------------------------------------------------------------
nb <- plot_allocated_NaLoA(al, catchCategory = "Lan", bin_width = 2)
ok(near(sum(nb$data$number), sum(plot_allocated_NaLoA(al, catchCategory = "Lan")$data$number)) &&
     all(nb$data$.class %% 2 == 0) && nrow(nb$data) < nrow(plot_allocated_NaLoA(al, catchCategory = "Lan")$data) &&
     grepl("pooled into 2cm bins", nb$labels$caption, fixed = TRUE),
   "bin_width = 2: classes are pooled into 2cm bins, nothing lost, and the caption says it")
wb <- plot_allocated_MWaLoA(al, catchCategory = "Lan", bin_width = 2)
ok(near(wt(nb, wb), wt(plot_allocated_NaLoA(al, catchCategory = "Lan"),
                       plot_allocated_MWaLoA(al, catchCategory = "Lan"))),
   "bin_width on the mean weight: each bin is weighted by its numbers (numbers x weight is kept)")

## ---- the mistakes it names ------------------------------------------------------------------------------------
m <- err(plot_allocated_NaLoA(raised))
ok(grepl("$distribution and $catch", m, fixed = TRUE),
   "a plain catch table: asked for the allocation result")
al_ns <- al; al_ns$distribution <- D[setdiff(names(D), "sampledOrEstimated")]
ok(grepl("no 'sampledOrEstimated'", err(plot_allocated_NaLoA(al_ns)), fixed = TRUE),
   "a result with no sampledOrEstimated: named")
ok(grepl("the allocation failed: boom",
         err(plot_allocated_NaLoA(structure(list(message = "boom"), class = "alloc_error"))), fixed = TRUE),
   "an alloc_error: its message")
ok(grepl("at most two", err(plot_allocated_NaLoA(al, facet = c("a", "b", "c"))), fixed = TRUE) &&
     grepl("at most two", err(plot_allocated_MWaLoA(al, facet = c("a", "b", "c"))), fixed = TRUE),
   "three facet columns: refused")
ok(grepl("no column 'nope'", err(plot_allocated_NaLoA(al, fill = "nope")), fixed = TRUE) &&
     grepl("no column 'nope'", err(plot_allocated_MWaLoA(al, colour = "nope")), fixed = TRUE),
   "a column the result does not carry: named")
ok(grepl("only applies to ages", err(plot_allocated_NaLoA(al, plus_group = 3)), fixed = TRUE) &&
     grepl("only applies to lengths", err(plot_allocated_MWaLoA(aa, distributionType = "Age", bin_width = 2)),
           fixed = TRUE),
   "a plus group on lengths and bins on ages: refused")
ok(grepl("single number", err(plot_allocated_NaLoA(aa, distributionType = "Age", plus_group = "x")), fixed = TRUE) &&
     grepl("single positive number", err(plot_allocated_NaLoA(al, bin_width = -1)), fixed = TRUE),
   "a plus group or bin width that is not a number: refused")
n <- heard(plot_allocated_NaLoA(al, distributionType = "Age"))
ok(is.null(n$value) && any(grepl("(it has LengthTotal)", n$messages, fixed = TRUE)),
   "a type the result does not hold: nothing to draw, and it says what the result has")
ok(is.null(suppressMessages(plot_allocated_MWaLoA(al, catchCategory = "BMS", distributionType = "Age"))),
   "nothing to draw is NULL for the mean weight too")

## ---- sampledOrEstimated: the shade by default, or the fill, the colour, the facet --------
## A variable like any other: drawn once - as the shade unless it is named in
## fill / colour / facet - and alpha = NULL draws no shade (the numbers are then
## added together).
S <- "sampledOrEstimated"
shaded <- function(p) !is.null(p$scales$get_scales("alpha"))
n_panels <- function(p) nrow(ggplot_build(p)$layout$layout)
by_lvl <- function(p) tapply(p$data$number, p$data[[S]], sum)
raw_both <- c(sampled = raw_k("Lan", "sampled"), estimated = raw_k("Lan", "estimated"))

nf <- plot_allocated_NaLoA(al, catchCategory = "Lan", fill = S)
ok(!inherits(try(ggplot_build(nf), silent = TRUE), "try-error") && !shaded(nf) &&
     identical(levels(nf$data[[S]]), c("sampled", "estimated")) && !".status" %in% names(nf$data) &&
     near(as.numeric(by_lvl(nf)[c("sampled", "estimated")]), as.numeric(raw_both)),
   "numbers: fill = sampledOrEstimated - a colour each, no shade, the same numbers")
ok(identical(nf$labels$fill, S) && grepl("stacked by sampledOrEstimated", nf$labels$caption, fixed = TRUE) &&
     !grepl("shade =", nf$labels$caption, fixed = TRUE) &&
     identical(nf$scales$get_scales("fill")$palette(2)[["sampled"]], PAL_BASE[[1]]) &&
     identical(nf$scales$get_scales("fill")$palette(2)[["estimated"]], PAL_BASE[[2]]),
   "numbers: ... and the caption and the two colours say so (sampled first)")
nfa <- plot_allocated_NaLoA(al, catchCategory = "Lan", fill = "vesselFlagCountry", facet = S)
ok(!shaded(nfa) && inherits(nfa$facet, "FacetWrap") && n_panels(nfa) == 2 &&
     near(as.numeric(by_lvl(nfa)[c("sampled", "estimated")]), as.numeric(raw_both)),
   "numbers: facet = sampledOrEstimated - a panel each, no shade, the same numbers")
ok(!shaded(plot_allocated_NaLoA(al, catchCategory = "Lan", fill = S, alpha = "sampledOrEstimated")),
   "numbers: asking for the shade AND naming it in fill draws it once, as the fill")
nn <- plot_allocated_NaLoA(al, catchCategory = "Lan", fill = "vesselFlagCountry", alpha = NULL)
ok(!shaded(nn) && !S %in% names(nn$data) && !".status" %in% names(nn$data) &&
     near(sum(nn$data$number), sum(raw_both)) &&
     grepl("sampled and estimated numbers added together", nn$labels$caption, fixed = TRUE),
   "numbers: alpha = NULL - no shade, sampled and estimated added together")
ok(grepl("`alpha` is \"sampledOrEstimated\"", err(plot_allocated_NaLoA(al, alpha = "mesh")), fixed = TRUE) &&
     grepl("`alpha` is \"sampledOrEstimated\"", err(plot_allocated_MWaLoA(al, alpha = "mesh")), fixed = TRUE),
   "an alpha that is not sampledOrEstimated: refused, with the two values it takes")

wsh <- plot_allocated_MWaLoA(al, catchCategory = "Lan")                       # the shade
wcl <- plot_allocated_MWaLoA(al, catchCategory = "Lan", colour = S)
mw_a <- wsh$data; mw_a$k <- tolower(as.character(mw_a$.status))
mw_b <- wcl$data;  mw_b$k <- as.character(mw_b[[S]])
mw_cmp <- merge(mw_a[c("k", ".class", "mean_weight")], mw_b[c("k", ".class", "mean_weight")],
                by = c("k", ".class"))
ok(!inherits(try(ggplot_build(wcl), silent = TRUE), "try-error") && !shaded(wcl) &&
     identical(levels(wcl$data[[S]]), c("sampled", "estimated")) &&
     nrow(mw_cmp) == nrow(wsh$data) && near(mw_cmp$mean_weight.x, mw_cmp$mean_weight.y) &&
     length(unique(ggplot_build(wcl)$data[[1]]$group)) == 2,
   "mean weight: colour = sampledOrEstimated - a line and colour each, no shade, the same mean weights")
wfa <- plot_allocated_MWaLoA(al, catchCategory = "Lan", colour = "vesselFlagCountry", facet = S)
ok(!shaded(wfa) && inherits(wfa$facet, "FacetWrap") && n_panels(wfa) == 2,
   "mean weight: facet = sampledOrEstimated - a panel each, no shade")
wn <- plot_allocated_MWaLoA(al, catchCategory = "Lan", alpha = NULL)
w_fw0 <- meanWoLatAoLAggregation(distribution_data = lanD, catch_data = lanC, grouping = "catchCategory",
                                 weighting = "NumberAtAoL", originType_catch = "WGEstimate",
                                 variableType_catch = "WeightLive", variableType_dist = "WeightLive",
                                 distributionType = "LengthTotal", unit = "g", round = NULL,
                                 plusGroup = FALSE)
w_l0 <- alloc_pivot_long(w_fw0, "W", ".mw")
w_m <- merge(data.frame(.class = w_l0$.cls / 10, fw = w_l0$.mw), wn$data[c(".class", "mean_weight")], by = ".class")
ok(!shaded(wn) && !S %in% names(wn$data) && nrow(w_m) == nrow(wn$data) && near(w_m$fw, w_m$mean_weight) &&
     grepl("sampled and estimated weights averaged together", wn$labels$caption, fixed = TRUE),
   "mean weight: alpha = NULL - one curve, the number-weighted mean of sampled and estimated together")

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)