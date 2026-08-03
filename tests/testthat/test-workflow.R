# The end-to-end path, run against synthetic data. These are the checks
# that would have caught the transition from script to package breaking
# something: a function that only worked because library(dplyr) was called
# somewhere, or an import that was never declared.

mk_catch <- function(n = 60) {
  set.seed(1)
  d <- data.frame(
    vesselFlagCountry = rep(c("Belgium", "France", "Netherlands"), each = n / 3),
    fleetValue  = rep(c("OTB_DEF_70-99", "TBB_DEF_70-99"), n / 2),
    areaValue   = rep(c("27.4.b", "27.4.c", "27.7.d"), n / 3),
    seasonValue = rep(c("1", "2"), n / 2),
    catchCategory = rep(c("Lan", "Dis"), each = 2, length.out = n),
    total = round(runif(n, 500, 90000)), variableUnit = "kg",
    domainCatchDis = NA_character_, stringsAsFactors = FALSE)
  d$domainCatchDis[d$catchCategory == "Lan"][1:12] <- "dis_a"
  d
}
DIMS <- c("metier4", "Country", "ICESDivision", "Season")

test_that("derive_fields splits the CEF codes", {
  d <- derive_fields(mk_catch())
  expect_true(all(c("metier4", "metier6", "Country", "Season",
                    "ICESSubArea", "ICESDivision") %in% names(d)))
  expect_equal(sort(unique(d$metier4)), c("OTB", "TBB"))
  # a code already coarser than the level asked for repeats rather than
  # becoming NA - 27.4 IS its own division as far as the data goes
  expect_false(any(is.na(d$ICESDivision)))
})

test_that("strata are built and classified", {
  s <- build_strata(derive_fields(mk_catch()), DIMS)
  expect_gt(nrow(s), 0)
  expect_true(all(c("landings", "landings_with_discards", "discards") %in% names(s)))
  sc <- classify_strata(s, 50)
  expect_true("Status" %in% names(sc))
  # counted, not named: the factor carries a fixed level set, so checking a
  # level EXISTS passes even when every stratum sits in one of them
  expect_gte(sum(table(sc$Status) > 0), 2)
})

test_that("manual rules partition the strata", {
  sc <- classify_strata(build_strata(derive_fields(mk_catch()), DIMS), 50)
  rules <- list(
    new_rule("G1", "otter", list(list(var = "metier4", values = "OTB",
                                      negate = FALSE))),
    new_rule("G2", "catch-all", list()))
  eff <- rule_effective_sets(sc, rules)
  m <- do.call(cbind, eff)
  expect_true(all(rowSums(m) == 1))          # never twice, never missed
  expect_lt(sum(eff$G2), nrow(sc))           # the catch-all takes the REST

  # and the emitted conditions select exactly the same strata
  cd <- conds_from_rules(rules)
  ev <- lapply(cd$strataCond, function(q) rlang::eval_tidy(q, data = sc))
  expect_equal(unname(vapply(ev, sum, integer(1))),
               unname(vapply(eff, sum, integer(1))))
})

test_that("the TAF bundle is seven scripts that parse", {
  b <- taf_bundle(list(load_method = "cef"), list())
  expect_equal(length(b), 7L)
  expect_equal(names(b), names(TAF_FILES))
  for (nm in names(b))
    expect_silent(parse(text = b[[nm]]))
  # the dependency that stops ages being allocated over unraised landings
  expect_true(any(grepl('readRDS("model/catch_data_raised.rds")',
                        b$model_allocate_length_age, fixed = TRUE)))
  expect_false(any(grepl('readRDS("data/catch_data.rds")',
                         b$model_allocate_length_age, fixed = TRUE)))
})

test_that("plots build without shiny attached", {
  sc <- classify_strata(build_strata(derive_fields(mk_catch()), DIMS), 50)
  p <- plot_coverage(sc, type = "heatmap", y = "metier4", fill = "Season",
                     threshold = 50)
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("the palette separates its colours", {
  # the reported failure was two countries rendering the same colour; the
  # invariant worth holding is a measured separation, not an eyeball
  tolab <- function(x) t(apply(t(grDevices::col2rgb(x)) / 255, 1,
                               function(v) grDevices::convertColor(matrix(v, 1),
                                                                   "sRGB", "Lab")))
  d <- as.matrix(dist(tolab(PAL_BASE[1:16]))); diag(d) <- Inf
  expect_gt(min(d), 15)
})
