## build_discard_clusters() / plot_discard_clusters() / plot_discard_cluster_variables()
##
## Uses the synthetic test fixture so this runs anywhere (no local-only
## CEF file needed).

source("dev/tests/fixture.R")
FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}
suppressPackageStartupMessages(library(ggplot2))

drawable <- function(p)
  !is.null(p) && inherits(p, "ggplot") &&
  !inherits(try(ggplot_build(p), silent = TRUE), "try-error")

test_catch <- fx_catch()
c23 <- subset(test_catch, year == 2023)
dims <- c("metier4", "ICESDivision", "seasonValue")

## ---- reproducible, and does not touch the caller's random state ---------
seed_before <- if (exists(".Random.seed", envir = .GlobalEnv))
  .Random.seed else NULL
dc1 <- build_discard_clusters(c23, dims = dims, k = 3, seed = 7)
seed_after <- if (exists(".Random.seed", envir = .GlobalEnv))
  .Random.seed else NULL
ok(identical(seed_before, seed_after),
   "build_discard_clusters(): the caller's random state is unchanged")

dc2 <- build_discard_clusters(c23, dims = dims, k = 3, seed = 7)
ok(identical(as.character(dc1$strata$cluster), as.character(dc2$strata$cluster)),
   "build_discard_clusters(): the same seed clusters the same way twice")

## ---- the table --------------------------------------------------------
ok(is.list(dc1) && all(c("strata", "importance", "eig") %in% names(dc1)),
   "build_discard_clusters(): returns strata/importance/eig")
ok(nrow(dc1$strata) > 0 && all(c("Dim1", "Dim2", "cluster") %in% names(dc1$strata)),
   "build_discard_clusters(): one row per sampled stratum, with its MCA coords")
ok(nlevels(dc1$strata$cluster) <= 3 && nlevels(dc1$strata$cluster) >= 1,
   "build_discard_clusters(): at most k cluster levels")

## cluster levels are ordered low to high by their own mean discard rate
means_in_level_order <- tapply(dc1$strata$discardRate, dc1$strata$cluster, mean)
ok(!is.unsorted(means_in_level_order),
   "build_discard_clusters(): cluster levels read low to high by mean discard rate")

## ---- variable importance -----------------------------------------------
ok(identical(sort(dc1$importance$variable), sort(dims)),
   "build_discard_clusters(): one importance row per dims column")
ok(all(dc1$importance$cramers_v >= 0 & dc1$importance$cramers_v <= 1),
   "build_discard_clusters(): Cramer's V is between 0 and 1")
ok(!is.unsorted(rev(dc1$importance$cramers_v)),
   "build_discard_clusters(): importance is sorted strongest first")

## ---- NULL-safety --------------------------------------------------------
ok(is.null(build_discard_clusters(c23[0, ], dims = dims)),
   "build_discard_clusters(): no rows gives NULL rather than an error")

## ---- the plots ------------------------------------------------------------
p1 <- plot_discard_clusters(dc1)
ok(drawable(p1), "plot_discard_clusters(): draws")
ok(identical(p1$labels$title, "Which strata share a discard rate"),
   "plot_discard_clusters(): title")

p2 <- plot_discard_cluster_variables(dc1)
ok(drawable(p2), "plot_discard_cluster_variables(): draws")
ok(identical(p2$labels$title, "Which variable best predicts the discard-rate cluster"),
   "plot_discard_cluster_variables(): title")
ok(nrow(p2$data) == length(dims),
   "plot_discard_cluster_variables(): one bar per grouping variable")

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)