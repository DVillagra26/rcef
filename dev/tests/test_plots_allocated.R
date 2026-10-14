## plot_NaLoA() / plot_MWaLoA() on the RESULT of the allocation: numbers and mean
## weight at length or age, the mark saying sampled (solid bars; a solid line with
## circles) or estimated by the allocation (hatched bars; a dotted line with crosses).
##
## They are the same functions as before the allocation, given the result's two
## tables - its $catch as `data` and its $distribution as `distributions` - and
## nothing special is run on them: the plots are built from the catch and the
## distributions the way they are for raw data. What is pinned here: the numbers
## and mean weights ARE those of the framework's aggregations,
## catchAtAoLAggregation() and meanWoLatAoLAggregation(), run on the same result
## (units, classes in cm, thousands), the sampled part is what the plain plot
## draws from the same data, the estimated distributions of strata with no domain
## are found (they exist only in the result's own $catch, and the plot warns when
## it is given the catch that went in), the mark is mapped and means what it
## says, the title says "sampled + estimated" - or "only sampled" when nothing
## was estimated - and the catch category, facets, plus group and length bins.
##
## The fixture is the synthetic test fixture with ONE catch row per domainBiology:
## the allocation and its aggregations count a distribution once per catch row
## that names its domain, which is right for CEF files built from InterCatch and
## not for the example as it is bundled.

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
raised <- as.data.frame(quiet(fx_raise(c23, fx_groups("own"), type = "discards", log_file = lf)))
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
al <- quiet(fx_allocate(catch1, dist1, "LengthTotal", log_file = lf))
aa <- quiet(fx_allocate(catch1, dist1, "Age", log_file = lf))
D <- al$distribution
ok(nrow(al$distribution) > 0 && nrow(aa$distribution) > 0,
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
NaLoA <- function(a, ...) plot_NaLoA(a$catch, a$distribution, ...)
MWaLoA <- function(a, ...) plot_MWaLoA(a$catch, a$distribution, ...)

## ---- why the result's own catch: the input catch cannot find the estimated ones ----
h_in <- heard(plot_NaLoA(catch1, D, catchCategory = "Dis"))
est_in  <- tot(h_in$value, "Estimated")
est_res <- tot(NaLoA(al, catchCategory = "Dis"), "Estimated")
ok(near(est_res, raw_k("Dis", "estimated")) && est_in < 0.5 * est_res,
   "given the catch that went in, the estimated discards are mostly lost; with the result's $catch they are all there")
ok(length(h_in$warnings) == 1L && grepl("not the allocation's own $catch", h_in$warnings, fixed = TRUE),
   "... and the plot says so, naming the catch to give")
ok(length(heard(NaLoA(al, catchCategory = "Dis"))$warnings) == 0L,
   "... and says nothing when given the result's own")
# a filtered result's catch is still its own
sub_catch <- al$catch[al$catch$metier4 == "OTB", ]
ok(length(heard(plot_NaLoA(sub_catch, D, catchCategory = "Lan"))$warnings) == 0L,
   "a result's catch cut down to some strata is still its own: no warning")

## ---- numbers: the framework's aggregation -----------------------------------------
pl <- NaLoA(al, catchCategory = "Lan", fill = "vesselFlagCountry")
pd <- NaLoA(al, catchCategory = "Dis", fill = "vesselFlagCountry")
ok(inherits(pl, "ggplot") && inherits(pd, "ggplot"), "plot_NaLoA(): draws the allocation")
ok(near(tot(pl, "Sampled"), raw_k("Lan", "sampled")) &&
     near(tot(pl, "Estimated"), raw_k("Lan", "estimated")) &&
     near(tot(pd, "Sampled"), raw_k("Dis", "sampled")) &&
     near(tot(pd, "Estimated"), raw_k("Dis", "estimated")),
   "numbers: sampled and estimated, landings and discards, are the allocation's own numbers (in thousands)")
ag <- fx_numbers(al, grouping = c("catchCategory", "sampledOrEstimated"))
ag_lan <- ag[ag$catchCategory == "Lan" & ag$sampledOrEstimated == "sampled", ]
ok(near(sum(unlist(ag_lan[grep("^N_", names(ag_lan))])) * 1e-3, tot(pl, "Sampled")),
   "numbers: the same as catchAtAoLAggregation() of that result")
raw_cls <- range(as.numeric(D$distributionClass[D$distributionType == "LengthTotal"])) / 10
ok(near(range(pl$data$.class), raw_cls),
   "classes: the file's mm come out in cm")
p_n0 <- plot_NaLoA(catch1, dist1, catchCategory = "Lan", fill = "vesselFlagCountry")
s <- pl$data[pl$data$.status == "Sampled", ]
m <- merge(s[c("vesselFlagCountry", ".class", "number")],
           p_n0$data[c("vesselFlagCountry", ".class", "number")],
           by = c("vesselFlagCountry", ".class"), all = TRUE)
ok(nrow(m) == nrow(s) && !anyNA(m) && near(m$number.x, m$number.y),
   "the sampled part is exactly what plot_NaLoA() draws from the catch and distributions before the allocation")

## ---- the shade and the title ------------------------------------------------------
ok(identical(levels(pl$data$.status), c("Sampled", "Estimated")) &&
     identical(rlang::as_label(pl$mapping$alpha), ".status"),
   "the shade is mapped to sampled / estimated, sampled first")
a_sc <- pl$scales$get_scales("alpha")
ok(!is.null(a_sc) && near(unname(a_sc$palette(2)), c(1, 0.45)),
   "sampled is solid (1) and estimated transparent (0.45) - as in the raised plots")
b <- ggplot_build(pl)$data[[1]]
ok(all(sort(unique(round(b$alpha, 2))) == c(0.45, 1)),
   "both shades are really drawn")
ok(grepl("solid = sampled, hatched = estimated by the allocation", pl$labels$caption, fixed = TRUE) &&
     endsWith(pl$labels$title, ", sampled + estimated"),
   "the caption says what the mark means, the title says the numbers are sampled + estimated")
ok(inherits(pl$layers[[1]]$geom, "GeomColHatch"),
   "the bars are a hatching layer: the estimated ones are drawn with stripes")
ok(identical(pl$theme$legend.position, "right"),
   "the legends sit outside the panel (they are tall and would cover the bars)")
ok(identical(NaLoA(al, catchCategory = "Lan", fill = "sampledOrEstimated")$theme$legend.position, "right") &&
     identical(MWaLoA(al, catchCategory = "Lan", colour = "vesselFlagCountry", facet = "sampledOrEstimated")$theme$legend.position, "right") &&
     !identical(plot_NaLoA(catch1, dist1, fill = "vesselFlagCountry")$theme$legend.position, "right"),
   "... also when sampled / estimated is the fill or a facet, and only after an allocation: before it the legend stays inside")
# before the allocation nothing is estimated: the title says so, and there is no shade
ok(endsWith(p_n0$labels$title, ", only sampled") && is.null(p_n0$scales$get_scales("alpha")) &&
     !".status" %in% names(p_n0$data) && !"sampledOrEstimated" %in% names(p_n0$data),
   "the plot before the allocation is titled 'only sampled' and has no shade")
# ... and so does a result in which none of what is drawn was estimated
al_s <- list(catch = al$catch, distribution = D[D$sampledOrEstimated == "sampled", ])
p_s <- NaLoA(al_s, catchCategory = "Lan", fill = "vesselFlagCountry")
ok(endsWith(p_s$labels$title, ", only sampled") && is.null(p_s$scales$get_scales("alpha")) &&
     !"sampledOrEstimated" %in% names(p_s$data),
   "an allocation in which nothing drawn was estimated reads as only sampled, with no shade")
# with the shade switched off the numbers are the allocation's, so the title still says allocated
p_off <- NaLoA(al, catchCategory = "Lan", alpha = NULL)
ok(endsWith(p_off$labels$title, ", sampled + estimated") && is.null(p_off$scales$get_scales("alpha")),
   "alpha = NULL: no shade, and still titled sampled + estimated")
# only estimated rows (the distributions of strata nobody sampled)
al_e <- list(catch = al$catch, distribution = D[D$sampledOrEstimated == "estimated", ])
p_e <- NaLoA(al_e, catchCategory = "Lan")
ok(endsWith(p_e$labels$title, ", only estimated") && !is.null(p_e$scales$get_scales("alpha")),
   "distributions that are all estimated are titled 'only estimated' (and drawn transparent)")

## ---- catch category, facets, fill ---------------------------------------------------
pa <- NaLoA(al, catchCategory = "All")
ok(near(tot(pa, "Sampled"), raw_k("Lan", "sampled") + raw_k("Dis", "sampled")) &&
     near(tot(pa, "Estimated"), raw_k("Lan", "estimated") + raw_k("Dis", "estimated")) &&
     grepl("^Catch - ", pa$labels$title),
   "catchCategory = 'All' adds landings and discards, and is titled catch")
ok(grepl("^Landings - ", pl$labels$title) && grepl("^Discards - ", pd$labels$title) &&
     grepl("Landings NaL", pl$labels$caption, fixed = TRUE),
   "landings and discards say so, in the title and the caption")
ok(inherits(NaLoA(al, fill = "metier4", facet = "vesselFlagCountry")$facet, "FacetWrap") &&
     inherits(NaLoA(al, fill = "metier4",
                    facet = c("vesselFlagCountry", "seasonValue"))$facet, "FacetGrid"),
   "one facet column wraps, two make a grid")
ps <- NaLoA(al, fill = "seasonValue", facet = "seasonValue")
ok(inherits(ps, "ggplot") && is.factor(ps$data$seasonValue) &&
     !inherits(try(ggplot_build(ps), silent = TRUE), "try-error"),
   "an integer column (seasonValue) is a category: fill and facet work")
ok(inherits(NaLoA(al, fill = "metier4", stacked = FALSE), "ggplot") &&
     grepl("dodged by metier4", NaLoA(al, fill = "metier4", stacked = FALSE)$labels$caption,
           fixed = TRUE),
   "stacked = FALSE dodges, and the caption says so")
ok(!inherits(try(ggplot_build(NaLoA(al)), silent = TRUE), "try-error"),
   "no fill and no facet: one colour, the shade alone")
# a column the catch does not carry is not derived on the way: the plot reads the
# catch as it is, and the error says where the column comes from
al_nm <- al; al_nm$catch <- al$catch[setdiff(names(al$catch), "mesh")]
e_nm <- err(NaLoA(al_nm, fill = "mesh"))
ok(grepl("no column 'mesh'", e_nm, fixed = TRUE) && grepl("derive_fields() adds mesh", e_nm, fixed = TRUE),
   "a derived column the result's catch lacks (mesh) is an error that names derive_fields()")

## ---- what the allocation ran on ----------------------------------------------------------
# rows of another origin are not what was allocated: they must not double the numbers
off <- al$catch[!is.na(al$catch$allocGroup), ]
off$originType <- "Official"; off$allocGroup <- NA
al_off <- al; al_off$catch <- rbind(al$catch, off)
ok(identical(NaLoA(al_off, catchCategory = "Lan", fill = "vesselFlagCountry")$data, pl$data),
   "catch rows of another origin than the allocation ran on do not double the numbers")
# a distribution the allocation left as it was is a sampled one
al_na <- al
al_na$distribution$sampledOrEstimated[al$distribution$sampledOrEstimated == "sampled"] <- NA
ok(identical(NaLoA(al_na, catchCategory = "Lan", fill = "vesselFlagCountry")$data, pl$data),
   "a distribution with no sampledOrEstimated is a sampled one")
# the engine's own list: nothing but $distribution and $catch
al_eng <- list(distribution = al$distribution, catch = al$catch)
ok(identical(NaLoA(al_eng, catchCategory = "Lan", fill = "vesselFlagCountry")$data, pl$data),
   "the list catchAllocationAoL() returns works the same")

## ---- mean weight: the framework's aggregation --------------------------------------------------
pw <- MWaLoA(al, catchCategory = "Lan", colour = "vesselFlagCountry")
ok(inherits(pw, "ggplot"), "plot_MWaLoA(): draws the allocation")
lanD <- D[D$catchCategory == "Lan", ]; lanC <- al$catch[al$catch$catchCategory == "Lan", ]
w_fw <- meanWoLatAoLAggregation(distribution_data = lanD, catch_data = lanC,
                                grouping = c("sampledOrEstimated", "vesselFlagCountry"),
                                weighting = "NumberAtAoL", originType_catch = "WGEstimate",
                                variableType_catch = "WeightLive", variableType_dist = "WeightLive",
                                distributionType = "LengthTotal", unit = "g", round = NULL,
                                plusGroup = FALSE)
w_long <- fx_pivot_long(as.data.frame(w_fw), "W", ".mw")
w_long$.status <- ifelse(w_long$sampledOrEstimated == "estimated", "Estimated", "Sampled")
w_long$.class <- w_long$.cls / 10
mm <- merge(w_long[c("vesselFlagCountry", ".status", ".class", ".mw")],
            transform(pw$data, .status = as.character(.status))[c("vesselFlagCountry", ".status", ".class", "mean_weight")],
            by = c("vesselFlagCountry", ".status", ".class"), all = TRUE)
ok(nrow(mm) == nrow(pw$data) && !anyNA(mm) && near(mm$.mw, mm$mean_weight),
   "mean weights: those of meanWoLatAoLAggregation(), in g, at classes in cm")
mapped_to_status <- function(p, aes)
  sum(vapply(p$layers, function(l) identical(rlang::as_label(l$mapping[[aes]]), ".status"), logical(1)))
ok(mapped_to_status(pw, "linetype") == 1 && mapped_to_status(pw, "shape") == 1 &&
     mapped_to_status(pw, "alpha") == 0,
   "mean weight: sampled / estimated is the line type of the lines and the symbol of the points")
ok(identical(unname(pw$scales$get_scales("linetype")$palette(2)), c("solid", "dotted")) &&
     near(unname(pw$scales$get_scales("shape")$palette(2)), c(21, 4)) &&
     grepl("solid line and dots = sampled, dotted line and crosses = estimated by the allocation",
           pw$labels$caption, fixed = TRUE) &&
     endsWith(pw$labels$title, "Mean Weight at Length, sampled + estimated"),
   "mean weight: sampled is a solid line with circles, estimated a dotted line with crosses; the caption says so, and the title says allocated")
ok(length(unique(ggplot_build(pw)$data[[1]]$group)) ==
     nrow(unique(pw$data[c("vesselFlagCountry", ".status")])),
   "one line per colour level and per sampled / estimated")
# a class with no fish has no mean weight: every point of the mean weight is a class that has numbers
na_k <- NaLoA(al, catchCategory = "Lan", fill = "vesselFlagCountry")$data
na_k$k <- paste(na_k$vesselFlagCountry, na_k$.status, na_k$.class)
pw$data$k <- paste(pw$data$vesselFlagCountry, pw$data$.status, pw$data$.class)
ok(all(pw$data$k %in% na_k$k),
   "a class with no fish (zero numbers) has no mean weight point - the allocation writes a weight at every class")
pw0 <- MWaLoA(al)
ok(!inherits(try(ggplot_build(pw0), silent = TRUE), "try-error") &&
     !is.null(pw0$scales$get_scales("linetype")) &&
     !inherits(try(ggplot_build(MWaLoA(al, colour = "metier4",
                                       facet = c("vesselFlagCountry", "seasonValue"))),
                   silent = TRUE), "try-error"),
   "no colour (the shade still tells sampled from estimated), and a grid of facets, draw")
# the band and the sample-size bars work on an allocation too; an estimated distribution has no fish to count
pw_b <- heard(MWaLoA(al, catchCategory = "Lan", colour = "vesselFlagCountry", show_samples = TRUE))
ok(inherits(pw_b$value, "ggplot") && !inherits(try(ggplot_build(pw_b$value), silent = TRUE), "try-error") &&
     any(vapply(pw_b$value$layers, function(l) inherits(l$geom, "GeomCol"), logical(1))) &&
     any(vapply(pw$layers, function(l) inherits(l$geom, "GeomRibbon"), logical(1))),
   "show_samples draws bars and the +/- 1 sd ribbon is there, as before the allocation")

## ---- ages: the plus group -------------------------------------------------------------------------
na0 <- NaLoA(aa, distributionType = "Age", catchCategory = "Lan")
na3 <- NaLoA(aa, distributionType = "Age", catchCategory = "Lan", plus_group = 3)
ok(near(sum(na3$data$number), sum(na0$data$number)) && max(na3$data$.class) == 3 &&
     any(na3$data$plus) && max(na0$data$.class) > 3 &&
     grepl("3+ = ages 3+ pooled", na3$labels$caption, fixed = TRUE),
   "plus_group = 3: ages 3 and older are one class, nothing lost, and the caption says it")
ok(identical(na0$labels$x, "Age (years)") && identical(pl$labels$x, "Length (cm)") &&
     identical(pl$labels$y, "Numbers (thousands)") && identical(pw$labels$y, "Mean weight (g)"),
   "ages in years, lengths in cm, numbers in thousands, mean weight in g")
nw0 <- MWaLoA(aa, distributionType = "Age", catchCategory = "Lan")
nw3 <- MWaLoA(aa, distributionType = "Age", catchCategory = "Lan", plus_group = 3)
wt <- function(nn, ww) { j <- merge(nn$data, ww$data, by = c(".class", ".status")); sum(j$number * j$mean_weight) }
ok(near(wt(na3, nw3), wt(na0, nw0)),
   "plus_group on the mean weight: the pooled class is weighted by its numbers (numbers x weight is kept)")
w <- heard(NaLoA(aa, distributionType = "Age", plus_group = 99))
ok(inherits(w$value, "ggplot") && length(w$warnings) == 1L && grepl("no age reaches plus_group = 99", w$warnings),
   "a plus group nothing reaches is said, and not drawn")

## ---- lengths: the bins -----------------------------------------------------------------------------------
nb <- NaLoA(al, catchCategory = "Lan", bin_width = 2)
ok(near(sum(nb$data$number), sum(NaLoA(al, catchCategory = "Lan")$data$number)) &&
     all(nb$data$.class %% 2 == 0) && nrow(nb$data) < nrow(NaLoA(al, catchCategory = "Lan")$data) &&
     grepl("pooled into 2cm bins", nb$labels$caption, fixed = TRUE),
   "bin_width = 2: classes are pooled into 2cm bins, nothing lost, and the caption says it")
wb <- MWaLoA(al, catchCategory = "Lan", bin_width = 2)
ok(near(wt(nb, wb), wt(NaLoA(al, catchCategory = "Lan"), MWaLoA(al, catchCategory = "Lan"))),
   "bin_width on the mean weight: each bin is weighted by its numbers (numbers x weight is kept)")

## ---- the mistakes it names ------------------------------------------------------------------------------------
m <- err(plot_NaLoA(al, al$distribution))
ok(grepl("`data` is the catch table", m, fixed = TRUE) &&
     grepl("its $catch as `data` and its $distribution as `distributions`", m, fixed = TRUE),
   "the allocation result handed over whole: asked for its two tables")
al_ns <- al; al_ns$distribution <- D[setdiff(names(D), "sampledOrEstimated")]
ok(grepl("no column 'sampledOrEstimated'", err(NaLoA(al_ns, fill = "sampledOrEstimated")), fixed = TRUE),
   "sampledOrEstimated asked for, of distributions that have none: named")
p_ns <- NaLoA(al_ns)
ok(inherits(p_ns, "ggplot") && endsWith(p_ns$labels$title, ", only sampled"),
   "distributions with no sampledOrEstimated are plain distributions: titled as sampled, no shade")
ok(grepl("at most two", err(NaLoA(al, facet = c("a", "b", "c"))), fixed = TRUE) &&
     grepl("at most two", err(MWaLoA(al, facet = c("a", "b", "c"))), fixed = TRUE),
   "three facet columns: refused")
ok(grepl("no column 'nope'", err(NaLoA(al, fill = "nope")), fixed = TRUE) &&
     grepl("no column 'nope'", err(MWaLoA(al, colour = "nope")), fixed = TRUE),
   "a column neither the catch nor the distributions carry: named")
ok(grepl("only applies to ages", err(NaLoA(al, plus_group = 3)), fixed = TRUE) &&
     grepl("only applies to lengths", err(MWaLoA(aa, distributionType = "Age", bin_width = 2)),
           fixed = TRUE),
   "a plus group on lengths and bins on ages: refused")
ok(grepl("single number", err(NaLoA(aa, distributionType = "Age", plus_group = "x")), fixed = TRUE) &&
     grepl("single positive number", err(NaLoA(al, bin_width = -1)), fixed = TRUE),
   "a plus group or bin width that is not a number: refused")
n <- heard(NaLoA(al, distributionType = "Age"))
ok(is.null(n$value) && any(grepl("(it has LengthTotal)", n$messages, fixed = TRUE)),
   "a type the result does not hold: nothing to draw, and it says what the result has")
ok(is.null(suppressMessages(MWaLoA(al, catchCategory = "BMS", distributionType = "Age"))),
   "nothing to draw is NULL for the mean weight too")

## ---- sampledOrEstimated: the shade by default, or the fill, the colour, the facet --------
## A variable like any other: drawn once - as the shade unless it is named in
## fill / colour / facet - and alpha = NULL draws no shade (the numbers are then
## added together).
S <- "sampledOrEstimated"
shaded <- function(p) !is.null(p$scales$get_scales("alpha")) || !is.null(p$scales$get_scales("linetype"))
n_panels <- function(p) nrow(ggplot_build(p)$layout$layout)
by_lvl <- function(p) tapply(p$data$number, p$data[[S]], sum)
raw_both <- c(sampled = raw_k("Lan", "sampled"), estimated = raw_k("Lan", "estimated"))

nf <- NaLoA(al, catchCategory = "Lan", fill = S)
ok(!inherits(try(ggplot_build(nf), silent = TRUE), "try-error") && !shaded(nf) &&
     identical(levels(nf$data[[S]]), c("sampled", "estimated")) && !".status" %in% names(nf$data) &&
     near(as.numeric(by_lvl(nf)[c("sampled", "estimated")]), as.numeric(raw_both)),
   "numbers: fill = sampledOrEstimated - a colour each, no shade, the same numbers")
ok(identical(nf$labels$fill, S) && grepl("stacked by sampledOrEstimated", nf$labels$caption, fixed = TRUE) &&
     !grepl("hatched", nf$labels$caption, fixed = TRUE) &&
     identical(nf$scales$get_scales("fill")$palette(2)[["sampled"]], "#4980B6") &&
     identical(nf$scales$get_scales("fill")$palette(2)[["estimated"]], "#FFB649"),
   "numbers: ... and the caption and the two colours say so (sampled first)")
nfa <- NaLoA(al, catchCategory = "Lan", fill = "vesselFlagCountry", facet = S)
ok(!shaded(nfa) && inherits(nfa$facet, "FacetWrap") && n_panels(nfa) == 2 &&
     near(as.numeric(by_lvl(nfa)[c("sampled", "estimated")]), as.numeric(raw_both)),
   "numbers: facet = sampledOrEstimated - a panel each, no shade, the same numbers")
ok(!shaded(NaLoA(al, catchCategory = "Lan", fill = S, alpha = "sampledOrEstimated")),
   "numbers: asking for the shade AND naming it in fill draws it once, as the fill")
nn <- NaLoA(al, catchCategory = "Lan", fill = "vesselFlagCountry", alpha = NULL)
ok(!shaded(nn) && !S %in% names(nn$data) && !".status" %in% names(nn$data) &&
     near(sum(nn$data$number), sum(raw_both)) &&
     grepl("sampled and estimated numbers added together", nn$labels$caption, fixed = TRUE),
   "numbers: alpha = NULL - no shade, sampled and estimated added together")
ok(grepl("`alpha` is \"sampledOrEstimated\"", err(NaLoA(al, alpha = "mesh")), fixed = TRUE) &&
     grepl("`alpha` is \"sampledOrEstimated\"", err(MWaLoA(al, alpha = "mesh")), fixed = TRUE),
   "an alpha that is not sampledOrEstimated: refused, with the two values it takes")

wsh <- MWaLoA(al, catchCategory = "Lan")                       # the shade
wcl <- MWaLoA(al, catchCategory = "Lan", colour = S)
mw_a <- wsh$data; mw_a$k <- tolower(as.character(mw_a$.status))
mw_b <- wcl$data;  mw_b$k <- as.character(mw_b[[S]])
mw_cmp <- merge(mw_a[c("k", ".class", "mean_weight")], mw_b[c("k", ".class", "mean_weight")],
                by = c("k", ".class"))
ok(!inherits(try(ggplot_build(wcl), silent = TRUE), "try-error") && !shaded(wcl) &&
     identical(levels(wcl$data[[S]]), c("sampled", "estimated")) &&
     nrow(mw_cmp) == nrow(wsh$data) && near(mw_cmp$mean_weight.x, mw_cmp$mean_weight.y) &&
     length(unique(ggplot_build(wcl)$data[[1]]$group)) == 2,
   "mean weight: colour = sampledOrEstimated - a line and colour each, no shade, the same mean weights")
wfa <- MWaLoA(al, catchCategory = "Lan", colour = "vesselFlagCountry", facet = S)
ok(!shaded(wfa) && inherits(wfa$facet, "FacetWrap") && n_panels(wfa) == 2,
   "mean weight: facet = sampledOrEstimated - a panel each, no shade")
wn <- MWaLoA(al, catchCategory = "Lan", alpha = NULL)
w_fw0 <- meanWoLatAoLAggregation(distribution_data = lanD, catch_data = lanC, grouping = "catchCategory",
                                 weighting = "NumberAtAoL", originType_catch = "WGEstimate",
                                 variableType_catch = "WeightLive", variableType_dist = "WeightLive",
                                 distributionType = "LengthTotal", unit = "g", round = NULL,
                                 plusGroup = FALSE)
w_l0 <- fx_pivot_long(as.data.frame(w_fw0), "W", ".mw")
w_m <- merge(data.frame(.class = w_l0$.cls / 10, fw = w_l0$.mw), wn$data[c(".class", "mean_weight")], by = ".class")
ok(!shaded(wn) && !S %in% names(wn$data) && nrow(w_m) == nrow(wn$data) && near(w_m$fw, w_m$mean_weight) &&
     grepl("sampled and estimated weights averaged together", wn$labels$caption, fixed = TRUE) &&
     endsWith(wn$labels$title, ", sampled + estimated"),
   "mean weight: alpha = NULL - one curve, the number-weighted mean of sampled and estimated together")

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
