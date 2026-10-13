# =====================================================================
#  viz_clusters.R   (which strata share a discard rate, and why)
#
#  ORIGIN: this project (ILVO / RCEF Workbench). Freely refactorable.
#  No shiny in this file. If it needs `input`, `session` or a tag,
#  it belongs in app/, not here.
# =====================================================================

# ------------------------------------------------------------
#  Discard-rate clusters: build the table, then two plots of it
# ------------------------------------------------------------
# Most of the columns a discard rate could depend on - metier4, mesh,
# vesselFlagCountry, seasonValue, areaValue - are categorical, so an
# ordinary PCA (which needs numeric input) does not apply. MCA (Multiple
# Correspondence Analysis) is the categorical equivalent: it places every
# stratum on a map
# from how its categories co-occur, without needing the discard rate itself
# to build that map. Clustering is a separate, second step, done directly on
# the discard rate (not on the MCA coordinates) - the map is only where the
# resulting clusters are then shown, so a reader can see whether a cluster
# occupies its own part of "variable space" or is scattered through it.

#' Evaluate `expr` under a fixed seed, without disturbing the caller's own
#' random state (MCA and k-means both draw on it)
#' @keywords internal
#' @noRd
.with_seed <- function(seed, expr) {
  had <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old <- if (had) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit(if (had) assign(".Random.seed", old, envir = .GlobalEnv)
          else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
            rm(".Random.seed", envir = .GlobalEnv))
  set.seed(seed)
  expr
}

#' Cramer's V, the association between two categorical variables
#'
#' 0 when `x` and `y` are independent, 1 when one fully determines the other.
#' @keywords internal
#' @noRd
.cramers_v <- function(x, y) {
  tab <- table(x, y)
  chi <- suppressWarnings(stats::chisq.test(tab, correct = FALSE)$statistic)
  sqrt(as.numeric(chi) / (sum(tab) * (min(dim(tab)) - 1)))
}

#' Which strata share a discard rate, and which variables predict that
#'
#' Groups sampled strata into `k` clusters of similar discard rate (k-means),
#' places every stratum on a map of `dims` built by MCA (Multiple
#' Correspondence Analysis - the categorical equivalent of PCA, since `dims`
#' is ordinarily categorical: metier4, mesh, vesselFlagCountry...), and scores how
#' strongly each of `dims` lines up with the resulting clusters. The table
#' this returns is what [plot_discard_clusters()] and
#' [plot_discard_cluster_variables()] draw; nothing here needs a distribution or a
#' raised catch.
#'
#' @details
#' The two steps are independent on purpose. Clustering runs on the discard
#' rate alone (`stats::kmeans()`), not on the MCA coordinates, so a cluster
#' always means "similar discard rate", never "similar categories that
#' happen to also have a similar rate". The MCA map is only where those
#' clusters are then shown, so [plot_discard_clusters()] can answer "does a
#' cluster occupy its own part of variable space, or is it scattered
#' through it" - the map's two dimensions rarely capture more than a fifth
#' of the total structure (`eig`), so treat it as a sketch, not the full
#' picture.
#'
#' Clustering uses `discardRate` (bounded 0-100%) rather than `rate`, which
#' can run past 100% for a stratum with very little covered landings and
#' would otherwise dominate the clustering on its own.
#'
#' @param catch Raw CEF catch data.
#' @param dims Categorical columns to place on the map and to score for
#'   importance - typically fleet/area/country/season columns
#'   (`derive_fields()` output), e.g. `c("metier4", "mesh", "vesselFlagCountry")`.
#' @param k Number of discard-rate clusters.
#' @param seed Seed for the (otherwise global-state-free) k-means run, so
#'   the same data and `k` always cluster the same way.
#' @return A list, or `NULL` when no stratum has a discard rate:
#'   * `strata` - one row per sampled stratum: `dims`, `rate`, `discardRate`,
#'     `landings`, `cluster` (low to high mean rate) and `Dim1`/`Dim2` (its
#'     MCA coordinates).
#'   * `importance` - one row per `dims` column, its Cramer's V against
#'     `cluster`, sorted strongest first.
#'   * `eig` - percent of variance the MCA's first dimensions explain.
#' @seealso [build_coverage_strata()] for the table this clusters;
#'   [plot_discard_clusters()], [plot_discard_cluster_variables()].
#' @md
#' @export
build_discard_clusters <- function(catch, dims, k = 3, seed = 1) {
  cov <- build_coverage_strata(catch, dims = dims)
  if (is.null(cov)) return(NULL)
  cov <- cov[!is.na(cov$rate), , drop = FALSE]
  if (!nrow(cov)) return(NULL)

  mca_df <- as.data.frame(lapply(cov[dims], function(v) factor(as.character(v))))
  mca_df$.y <- cov$discardRate * 100
  fit <- .with_seed(seed, {
    res_mca <- FactoMineR::MCA(mca_df, quanti.sup = ncol(mca_df), graph = FALSE)
    km <- stats::kmeans(cov$discardRate, centers = k, nstart = 25)
    list(mca = res_mca, km = km)
  })
  res_mca <- fit$mca; km <- fit$km

  ord <- order(tapply(cov$discardRate, km$cluster, mean))
  lab <- setNames(sprintf("%d (%.0f%% avg)", seq_len(k),
                          tapply(cov$discardRate, km$cluster, mean)[ord] * 100),
                  ord)
  cov$cluster <- factor(lab[as.character(km$cluster)], levels = lab[as.character(ord)])
  cov$Dim1 <- res_mca$ind$coord[, 1]
  cov$Dim2 <- res_mca$ind$coord[, 2]

  importance <- data.frame(
    variable = dims,
    cramers_v = vapply(dims, function(v) .cramers_v(cov[[v]], cov$cluster), numeric(1)),
    stringsAsFactors = FALSE)
  importance <- importance[order(-importance$cramers_v), ]

  list(strata = cov, importance = importance, eig = res_mca$eig[, 2],
       dims = dims, k = k)
}

#' Discard-rate clusters, shown on the map of `dims`
#'
#' Every sampled stratum, coloured by its discard-rate cluster
#' ([build_discard_clusters()]), on the MCA map of `dims` - an ellipse per
#' cluster so a reader can see at a glance whether it occupies its own part
#' of the map or overlaps the rest.
#'
#' @param x Output of [build_discard_clusters()].
#' @param palette `"default"` or `"colorblind"` -
#'   unlike most of this package's colours, a cluster's own colour is not
#'   kept stable across plots: which cluster is "1" depends on this run's
#'   data and `k` alone.
#' @inheritParams saving-plots
#' @return A ggplot object.
#' @seealso [build_discard_clusters()], [plot_discard_cluster_variables()].
#' @md
#' @export
plot_discard_clusters <- function(x, palette = c("default", "colorblind"), save = NULL) {
  save_on_exit(save, "plot_discard_clusters")
  palette <- match.arg(palette)
  d <- x$strata
  pal <- group_palette(levels(d$cluster))
  has_landings <- "landings" %in% names(d) &&
    any(is.finite(d$landings) & d$landings > 0)

  p <- ggplot(d, aes(x = Dim1, y = Dim2, fill = cluster)) +
    stat_ellipse(geom = "polygon", colour = NA, alpha = 0.15, level = 0.7)
  pt_args <- list(shape = 21, colour = OUTLINE, stroke = 0.5, alpha = 0.85)
  ae <- aes(fill = cluster)
  if (has_landings) ae <- modifyList(ae, aes(size = landings)) else pt_args$size <- 3
  p <- p + do.call(geom_point, c(list(mapping = ae), pt_args))
  if (has_landings)
    p <- p + scale_size_area(max_size = 7, labels = scales::comma, name = "Landings (t)")
  p +
    scale_fill_manual(values = pal, name = "Discard-rate\ncluster") +
    labs(title = "Which strata share a discard rate",
         caption = sub_wrap(
           sprintf("grouping = %s, clustered into %d groups by discard rate.",
                   paste(x$dims, collapse = " x "), x$k),
           "map = MCA of the grouping columns (categories that co-occur sit close together).",
           sprintf("axes explain %s%% of the total structure between them - a sketch, not the full picture.",
                   format(round(sum(x$eig[1:2]), 1), trim = TRUE)),
           "ellipse = 70% of each cluster's own strata.",
           width = 110),
         x = sprintf("Dim1 (%s%%)", format(round(x$eig[1], 1), trim = TRUE)),
         y = sprintf("Dim2 (%s%%)", format(round(x$eig[2], 1), trim = TRUE))) +
    app_theme()
}

#' Which variable best predicts the discard-rate cluster
#'
#' Cramer's V of each grouping column against the cluster it helped
#' describe ([build_discard_clusters()]) - 0 means unrelated to the
#' clustering, 1 means it alone determines it.
#'
#' @param x Output of [build_discard_clusters()].
#' @inheritParams saving-plots
#' @return A ggplot object.
#' @seealso [build_discard_clusters()], [plot_discard_clusters()].
#' @md
#' @export
plot_discard_cluster_variables <- function(x, save = NULL) {
  save_on_exit(save, "plot_discard_cluster_variables")
  imp <- x$importance
  imp$variable <- factor(imp$variable, levels = rev(imp$variable))
  ggplot(imp, aes(x = variable, y = cramers_v)) +
    geom_col(fill = "#2a78d6", width = 0.6) +
    coord_flip() +
    scale_y_continuous(limits = c(0, 1)) +
    labs(title = "Which variable best predicts the discard-rate cluster",
         caption = sub_wrap(
           sprintf("grouping = %s, clustered into %d groups by discard rate.",
                   paste(x$dims, collapse = " x "), x$k),
           "bars = Cramer's V against the cluster (0 = unrelated, 1 = fully determines it).",
           width = 110),
         x = NULL, y = "Cramer's V") +
    app_theme()
}
