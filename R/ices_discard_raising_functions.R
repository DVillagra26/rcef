# ============================================================================
# discard_raising_functions.R
# Coverage diagnosis + hierarchical raising ladder + rule-based raising.
# Sourced by discard_raising_app.Rmd (also usable standalone in a script).
# ============================================================================

# PACKAGE NOTE (added when these scripts were folded into rcef)
# The original standalone version attached its dependencies here with
# library(). A package must not: attaching changes the user's search path
# as a side effect of loading. The same packages are now declared in
# DESCRIPTION under Imports and reached through the namespace, which is
# what makes load order stop mattering.
#
# Original block, for reference:
#   suppressPackageStartupMessages({
#     library(dplyr); library(tidyr); library(stringr)
#     library(rlang); library(ggplot2); library(scales)
#   })


# `%||%` was defined here in the standalone script. rcef defines its own in
# fn_utils.R, and two definitions of one operator in a single namespace is
# a collision waiting to be blamed on something else, so this copy is gone.
# The definitions were equivalent; if that ever stops being true, the
# package's is the one that wins and this comment is where to look.

# ---- Standardise catch category -------------------------------------------
std_catch_cat <- function(x) {
  x <- as.character(x)
  case_when(
    x %in% c("Lan", "Landings", "L") ~ "Landings",
    x %in% c("Dis", "Discards", "D", "RegDis", "Regulated Discard",
             "Logbook Registered Discard", "BMS", "BMS landing") ~ "Discards",
    TRUE ~ x
  )
}

# ---- Harmonise column names across the two common schemas -----------------
normalise_catch_names <- function(df) {
  aliases <- list(
    areaValue   = c("areaValue", "area"),
    seasonValue = c("seasonValue", "quarter", "season"),
    fleetValue  = c("fleetValue", "fleet"),
    numTrips    = c("numTrips", "num_trips"),
    numPSUs     = c("numPSUs", "num_PSUs")
  )
  for (canon in names(aliases)) {
    if (!canon %in% names(df)) {
      alt <- intersect(aliases[[canon]], names(df))
      if (length(alt)) names(df)[names(df) == alt[1]] <- canon
    }
  }
  df
}

# ---- Parse metier levels out of fleetValue --------------------------------
add_fleet_levels <- function(df, fleet_col = "fleetValue") {
  fv   <- as.character(df[[fleet_col]])
  toks <- strsplit(fv, "_", fixed = TRUE)
  g  <- vapply(toks, function(x) if (length(x) >= 1) x[[1]] else NA_character_, character(1))
  t2 <- vapply(toks, function(x) if (length(x) >= 2) x[[2]] else NA_character_, character(1))
  m  <- vapply(toks, function(x) if (length(x) >= 3) x[[3]] else NA_character_, character(1))
  mutate(df,
    Metier_L4 = g,
    Metier_L5 = if_else(is.na(t2), g, paste(g, t2, sep = "_")),
    Metier_L6 = sub("_all$", "", fv),
    mesh_size = m
  )
}

# ---- One-shot prep: names, fleet levels, convenience vars, types ----------
prep_census <- function(df, effort_col = NULL) {
  df <- df %>% normalise_catch_names() %>% add_fleet_levels()
  df %>% mutate(
    total       = suppressWarnings(as.numeric(total)),
    numTrips    = if ("numTrips" %in% names(.)) suppressWarnings(as.numeric(numTrips)) else NA_real_,
    cat_std     = std_catch_cat(catchCategory),
    is_sampled  = !is.na(domainCatchDis) & as.character(domainCatchDis) != "",
    Country     = vesselFlagCountry,
    Season      = seasonValue,
    Area1       = str_replace(as.character(areaValue), "^27\\.([0-9]+).*$", "\\1"),
    raise_effort = if (!is.null(effort_col) && effort_col %in% names(.))
      suppressWarnings(as.numeric(.data[[effort_col]])) else NA_real_
  )
}

# ---- Stratum summary at arbitrary keys, splitting Lan / Dis correctly ------
summarise_strata <- function(df, keys) {
  if (!"cat_std" %in% names(df)) stop("Run prep_census() first.")
  lan <- df %>% filter(cat_std == "Landings") %>%
    group_by(across(all_of(keys))) %>%
    summarise(
      landings         = sum(total, na.rm = TRUE),
      covered_landings = sum(total * is_sampled, na.rm = TRUE),
      effort           = sum(raise_effort, na.rm = TRUE),
      .groups = "drop"
    )
  dis <- df %>% filter(cat_std == "Discards") %>%
    group_by(across(all_of(keys))) %>%
    summarise(
      discards = sum(total, na.rm = TRUE),
      n_trips  = sum(numTrips, na.rm = TRUE),
      .groups = "drop"
    )
  full_join(lan, dis, by = keys) %>%
    mutate(across(c(landings, covered_landings, discards, n_trips),
                  ~ coalesce(.x, 0))) %>%
    mutate(
      has_sample   = discards > 0 | covered_landings > 0 | n_trips > 0,
      coverage_pct = if_else(landings > 0, 100 * covered_landings / landings, NA_real_),
      dr_raise     = if_else(landings > 0, discards / landings, NA_real_)
    )
}

# ============================================================================
# 1. COVERAGE DIAGNOSIS  - one stock, flexible x / y / facet across the 4 dims
# ============================================================================
diagnose_coverage <- function(
    census, stock,
    fleet_level   = "Metier_L4",
    x_var         = "seasonValue",
    y_var         = fleet_level,
    facet_var     = "areaValue",
    coverage_threshold = 30
) {
  thr_lab <- paste0(coverage_threshold, "%")
  roles   <- c(x = x_var, y = y_var, facet = facet_var)
  roles   <- roles[!is.na(roles) & roles != ""]
  keys    <- unique(unname(roles))

  d <- census %>% filter(.data$stock == !!stock)
  if (!nrow(d)) stop("No rows for stock '", stock, "'.")

  tab <- summarise_strata(d, c("stock", "year", keys)) %>%
    mutate(
      Status = case_when(
        landings > 0 & !has_sample                                   ~ "No discard data",
        landings > 0 & coverage_pct >  coverage_threshold            ~ paste0("SAFE > ", thr_lab),
        landings > 0 & coverage_pct <= coverage_threshold & coverage_pct > 0
                                                                     ~ paste0("WARNING < ", thr_lab),
        landings > 0                                                 ~ "No discard data",
        has_sample                                                   ~ "Discard only: no landings",
        TRUE                                                         ~ "No activity"
      ),
      Raising_Decision = case_when(
        !is.na(coverage_pct) & coverage_pct >= coverage_threshold ~ "Direct (self)",
        !is.na(coverage_pct) & coverage_pct >  0                  ~ "Pool with neighbours",
        TRUE                                                      ~ "Borrow from similar strata"
      )
    )

  status_colors <- c(
    "No discard data"           = "cornflowerblue",
    "No activity"               = "grey85",
    "Discard only: no landings" = "#ffff99"
  )
  status_colors[paste0("SAFE > ", thr_lab)]    <- "#2ca25f"
  status_colors[paste0("WARNING < ", thr_lab)] <- "darkorange"

  y_ord <- tab %>% group_by(.data[[y_var]]) %>%
    summarise(t = sum(landings, na.rm = TRUE), .groups = "drop") %>%
    arrange(t) %>% pull(1)
  tab[[y_var]] <- factor(tab[[y_var]], levels = y_ord)

  base_theme <- theme_bw(base_size = 12) +
    theme(axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          strip.text  = element_text(face = "bold", size = 9),
          strip.background = element_rect(fill = "grey92", colour = NA),
          legend.position = "right",
          panel.spacing = unit(0.3, "lines"))

  add_facet <- function(p) {
    if (is.null(facet_var) || is.na(facet_var) || facet_var == "") return(p)
    p + facet_grid(rows = vars(.data[[facet_var]]), scales = "free_y", space = "free_y")
  }

  heatmap_plot <- ggplot(tab, aes(x = factor(.data[[x_var]]),
                                  y = .data[[y_var]], fill = Status)) +
    geom_tile(colour = "black", alpha = 0.85) +
    geom_text(aes(label = ifelse(landings > 99, round(landings, 0),
                          ifelse(landings >= 10, round(landings, 1), round(landings, 2)))),
              size = 2.4, fontface = "bold") +
    scale_fill_manual(values = status_colors, name = "Sampling status") +
    labs(title = paste0("Discard-sampling coverage - ", stock),
         subtitle = paste0("labels = landings weight | fleet level = ", fleet_level,
                           " | threshold = ", thr_lab),
         x = x_var, y = y_var)
  heatmap_plot <- add_facet(heatmap_plot) + base_theme

  bubble_data <- tab %>% mutate(
    plot_size  = ifelse(landings == 0 & discards > 0, NA, landings),
    label_text = case_when(
      landings == 0 & discards > 0            ~ "100%\nDisc",
      is.na(coverage_pct) | coverage_pct == 0 ~ "",
      coverage_pct > coverage_threshold       ~ "",
      TRUE                                    ~ paste0(round(coverage_pct, 0), "%"))
  )
  bubble_plot <- ggplot(bubble_data, aes(x = factor(.data[[x_var]]), y = .data[[y_var]])) +
    geom_point(aes(size = plot_size, fill = Status), shape = 21, colour = "black", stroke = .6) +
    geom_point(data = filter(bubble_data, landings == 0 & discards > 0),
               shape = 4, size = 2, stroke = 1) +
    geom_text(aes(label = label_text), size = 2.4, fontface = "bold", colour = "grey20") +
    scale_size_continuous(range = c(4, 13), name = "Landings") +
    scale_fill_manual(values = status_colors, name = "Sampling status",
                      guide = guide_legend(override.aes = list(size = 6))) +
    labs(title = paste0("Coverage vs landings - ", stock),
         subtitle = "X = discards present with zero landings", x = x_var, y = y_var)
  bubble_plot <- add_facet(bubble_plot) + base_theme

  gap_table <- tab %>%
    filter(!Status %in% paste0("SAFE > ", thr_lab)) %>% as.data.frame()

  list(diagnosis = as.data.frame(tab), heatmap = heatmap_plot,
       bubbles = bubble_plot, gap_table = gap_table)
}

# ============================================================================
# 2. HIERARCHICAL RAISING LADDER  - fine -> coarse, per-rung availability gate
# ============================================================================
raise_discards_ladder <- function(
    census, stock, ladder,
    target_keys        = ladder[[1]],
    coverage_threshold = 50,
    avail_thresholds   = NULL,
    avail_metric       = c("discards", "landings", "n_trips"),
    outlier_dr         = NULL,
    raising_var        = "landings"
) {
  avail_metric <- match.arg(avail_metric)
  if (is.null(avail_thresholds)) avail_thresholds <- rep(0, length(ladder))
  stopifnot(length(avail_thresholds) == length(ladder))

  d <- census %>% filter(.data$stock == !!stock)
  strata <- summarise_strata(d, c("year", target_keys)) %>%
    mutate(
      raise_w   = if (raising_var == "effort") effort else landings,
      avail_val = switch(avail_metric,
                         discards = discards, landings = landings, n_trips = n_trips),
      role = case_when(
        landings > 0 & coverage_pct >= coverage_threshold ~ "self",
        landings > 0                                      ~ "target",
        TRUE                                              ~ "no_landings"),
      dr_used = NA_real_, est_discards = NA_real_, used_level = NA_integer_,
      used_keys = NA_character_, n_donors = NA_integer_, donor_avail = NA_real_)

  strata <- strata %>% mutate(
    est_discards = if_else(role == "self", discards, est_discards),
    dr_used      = if_else(role == "self", dr_raise, dr_used),
    used_level   = if_else(role == "self", 0L, used_level),
    used_keys    = if_else(role == "self", "self", used_keys))

  donors_all <- strata %>% filter(role == "self")
  if (!is.null(outlier_dr))
    donors_all <- donors_all %>% filter(dr_raise * 100 <= outlier_dr)

  targets <- strata %>% filter(role == "target")
  grid    <- vector("list", 0)
  trace   <- vector("list", 0)

  lvl_lab <- vapply(ladder, function(k)
    paste(intersect(k, target_keys), collapse = " + "), character(1))

  for (i in seq_len(nrow(targets))) {
    tg <- targets[i, ]
    chosen <- NA_integer_
    for (L in seq_along(ladder)) {
      kl   <- intersect(ladder[[L]], target_keys)
      pool <- donors_all
      for (k in kl)
        pool <- pool[!is.na(pool[[k]]) & pool[[k]] == tg[[k]], , drop = FALSE]

      avail <- if (nrow(pool)) sum(pool$avail_val, na.rm = TRUE) else 0
      w_sum <- if (nrow(pool)) sum(pool$raise_w,  na.rm = TRUE) else 0
      meets <- nrow(pool) > 0 && w_sum > 0 && avail >= avail_thresholds[L]

      key_vals <- lapply(as.list(tg[target_keys]), as.character)
      grid[[length(grid) + 1]] <- tibble(
        target_id = i, level = L, level_lab = lvl_lab[L],
        avail = avail, threshold = avail_thresholds[L],
        n_donors = nrow(pool), meets = meets, !!!key_vals)

      if (meets && is.na(chosen)) {
        chosen <- L
        rate   <- sum(pool$discards, na.rm = TRUE) / w_sum
        targets$dr_used[i]      <- rate
        targets$est_discards[i] <- tg$raise_w * rate
        targets$used_level[i]   <- L
        targets$used_keys[i]    <- lvl_lab[L]
        targets$n_donors[i]     <- nrow(pool)
        targets$donor_avail[i]  <- avail
        trace[[length(trace) + 1]] <- pool %>%
          mutate(target_id = i, level = L, level_lab = lvl_lab[L])
      }
    }
    if (is.na(chosen)) targets$used_keys[i] <- "UNASSIGNED"
  }

  grid_df <- bind_rows(grid) %>% group_by(target_id) %>%
    mutate(chosen = level == min(level[meets], Inf)) %>% ungroup()
  strata_out <- bind_rows(strata %>% filter(role != "target"), targets)

  list(
    strata      = as.data.frame(strata_out),
    assignments = targets %>%
      select(all_of(target_keys), landings, coverage_pct, raise_w,
             used_level, used_keys, n_donors, donor_avail, dr_used, est_discards) %>%
      as.data.frame(),
    donor_trace = if (length(trace)) as.data.frame(bind_rows(trace)) else data.frame(),
    ladder_grid = as.data.frame(grid_df),
    plot        = plot_ladder(grid_df, target_keys, avail_metric, stock)
  )
}

plot_ladder <- function(grid_df, target_keys, avail_metric, stock) {
  if (!nrow(grid_df))
    return(ggplot() + annotate("text", 1, 1, label = "No strata to raise") + theme_void())
  lab <- do.call(paste, c(lapply(target_keys, function(k) grid_df[[k]]), sep = " | "))
  grid_df$target_label <- lab
  metier_facet <- "Metier_L4" %in% target_keys

  ord <- grid_df %>% distinct(target_id, target_label) %>% arrange(target_label)
  grid_df$target_label <- factor(grid_df$target_label, levels = rev(ord$target_label))
  grid_df$level_lab    <- factor(grid_df$level_lab,
                                 levels = unique(grid_df$level_lab[order(grid_df$level)]))

  p <- ggplot(grid_df, aes(level_lab, target_label, fill = avail)) +
    geom_tile(colour = "grey85") +
    geom_tile(data = filter(grid_df, chosen), fill = NA, colour = "black", linewidth = 1.1) +
    geom_text(aes(label = round(avail, 1)), size = 2.4) +
    scale_fill_gradient(low = "#fff7ec", high = "#2ca25f",
                        name = paste0("donor ", avail_metric)) +
    labs(title = paste0("Discard-raising ladder - ", stock),
         subtitle = "black outline = rung used (first to clear its threshold)",
         x = "Ladder rung (fine \u2192 coarse)", y = "Under-sampled stratum") +
    theme_bw(base_size = 11) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
          axis.text.y = element_text(size = 7))
  if (metier_facet)
    p <- p + facet_grid(rows = vars(.data[["Metier_L4"]]), scales = "free_y", space = "free_y")
  p
}

# ============================================================================
# 3. RULE-BASED RAISING  - explicit target/donor conditions (fine control)
# ============================================================================
raise_discards_rules <- function(
    census, stock, rules,
    coverage_threshold = 50, outlier_dr = NULL, raising_var = "landings",
    target_keys = c("vesselFlagCountry", "fleetValue", "areaValue", "seasonValue")
) {
  d <- census %>% filter(.data$stock == !!stock)
  strata <- summarise_strata(
    d, c("year", union(target_keys,
                       c("Metier_L4","Metier_L5","Metier_L6","Country","Season","Area1")))) %>%
    mutate(raise_w   = if (raising_var == "effort") effort else landings,
           is_donor  = landings > 0 & coverage_pct >= coverage_threshold,
           is_target = landings > 0 & (is.na(coverage_pct) | coverage_pct < coverage_threshold))

  out <- lapply(names(rules), function(g) {
    r <- rules[[g]]
    donors  <- strata %>% filter(is_donor,  !!r$donor)
    if (!is.null(outlier_dr)) donors <- donors %>% filter(dr_raise * 100 <= outlier_dr)
    targets <- strata %>% filter(is_target, !!r$target)
    w_sum <- sum(donors$raise_w, na.rm = TRUE)
    rate  <- if (w_sum > 0) sum(donors$discards, na.rm = TRUE) / w_sum else NA_real_
    targets %>% transmute(rule = g, across(all_of(target_keys)),
                          landings, raise_w, pooled_rate = rate,
                          n_donors = nrow(donors),
                          est_discards = raise_w * rate)
  })
  assignments <- bind_rows(out) %>% as.data.frame()
  list(assignments = assignments,
       summary = assignments %>% group_by(rule) %>%
         summarise(n_targets = n(), pooled_rate = first(pooled_rate),
                   est_discards = sum(est_discards, na.rm = TRUE), .groups = "drop") %>%
         as.data.frame())
}
