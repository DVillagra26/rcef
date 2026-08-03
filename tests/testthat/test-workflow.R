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

test_that("the ICES routines are bundled and callable", {
  # the whole point of folding them in: no folder to configure, nothing to
  # source, and the status table says so
  st <- ices_routine_status()
  expect_true(all(st$available))
  expect_true(all(st$source == "rcef"))
  for (f in c("catchRaising", "catchAllocationAoL", "funIntercatchCEF",
              "check_group_conditions"))
    expect_true(is.function(get(f, envir = asNamespace("rcef"))))
  expect_true(all(c("catchRaising", "catchAllocationAoL") %in%
                    getNamespaceExports("rcef")))
})

test_that("external_status reports capability from the bundled code", {
  s <- external_status()
  expect_true(s$raising)
  expect_true(s$catch_alloc)
  expect_true(s$ic_exchange)
})

test_that("loading rcef attaches nothing", {
  # The library() block that used to sit at the top of fn_utils.R would
  # have failed this: attaching packages as a side effect of loading is
  # exactly what turning the scripts into a package had to stop.
  #
  # Two things on the search path are NOT the package's doing and are
  # excluded by name rather than by loosening the assertion:
  #   testthat        - attached by the runner
  #   devtools_shims  - scaffolding devtools inserts under load_all() and
  #                     test_local(). Absent under R CMD check. An earlier
  #                     version of this test failed on it and looked like a
  #                     leak in rcef, which cost a bisect to disprove.
  runner <- c("package:testthat", "devtools_shims", "package:devtools",
              "package:pkgload")
  extra <- setdiff(search(), c(".GlobalEnv", "package:rcef", "package:stats",
    "package:graphics", "package:grDevices", "package:utils",
    "package:datasets", "package:methods", "Autoloads", "package:base",
    runner))
  # naming what leaked matters more than the count
  expect_true(length(extra) == 0,
              info = paste("unexpectedly attached:", paste(extra, collapse = ", ")))
})

test_that("the library()-inside-body behaviour is pinned, not forgotten", {
  # Several bundled ICES functions call library() inside their own bodies.
  # That runs at CALL time, so loading rcef stays clean - but calling
  # catchRaising() will attach dplyr to the user's search path, and that is
  # documented in README.md and in inst/scripts/README_ices_source.txt.
  #
  # This pins the behaviour by inspecting the function bodies rather than
  # by calling them, so the test does not pollute its own session. If a
  # future ICES revision removes those calls, this fails and the notes can
  # come out - which is the point: a documented caveat that silently stops
  # being true is just a wrong document.
  has_attach <- function(fn) {
    f <- get0(fn, envir = asNamespace("rcef"))
    if (!is.function(f)) return(NA)
    any(grepl("\\b(library|require)\\(", deparse(body(f))))
  }
  offenders <- c("catchRaising", "catchAllocationAoL", "check_group_conditions",
                 "funMakeRelation")
  hits <- vapply(offenders, has_attach, logical(1))
  expect_true(any(hits, na.rm = TRUE),
              info = paste("if this now fails, upstream has cleaned these up:",
                           paste(offenders, collapse = ", ")))
})

test_that("no name is defined twice in the package namespace", {
  # Folding the ICES scripts in produced two collisions - `%||%` and
  # load_external() - and both were found only because something else
  # broke. Two definitions of one name in a namespace is a bug waiting to
  # be blamed on something unrelated, so it is checked directly.
  #
  # Run against the development R/ when it is reachable; the installed
  # package does not ship its sources, so under R CMD check this skips
  # rather than pretending to pass.
  dev <- NULL
  for (p in c("R", file.path("..", "..", "R"), file.path("..", "..", "..", "R")))
    if (dir.exists(p) && length(list.files(p, pattern = "\\.R$"))) { dev <- p; break }
  skip_if(is.null(dev), "development R/ not reachable from the test directory")

  defs <- list()
  for (f in list.files(dev, pattern = "\\.R$", full.names = TRUE))
    for (e in as.list(parse(f)))
      if (is.call(e) && length(e) >= 3 &&
          identical(as.character(e[[1]]), "<-") && is.name(e[[2]]))
        defs[[length(defs) + 1]] <- c(nm = as.character(e[[2]]), f = basename(f))
  nm <- vapply(defs, `[[`, character(1), "nm")
  fl <- vapply(defs, `[[`, character(1), "f")
  dup <- unique(nm[duplicated(nm)])
  expect_length(dup, 0)
  if (length(dup))
    cat("\nduplicated:\n", paste(sprintf("  %s -> %s", dup,
        vapply(dup, function(d) paste(unique(fl[nm == d]), collapse = ", "),
               character(1))), collapse = "\n"), "\n")
})
