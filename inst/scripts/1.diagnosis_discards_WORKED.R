# ============================================================
# diagnosis_discards()
# ============================================================
# Discard-sampling diagnostic for RCEF / discard-raising workflows.
# ONE stock, ONE year at a time (the function stops if more than one stock
# survives the filter, so there is no stock facet any more).
#
# ------------------------------------------------------------
# GROUPING VARIABLES -> PLOT ROLES (positional)
# ------------------------------------------------------------
#     grouping_vars[1]  ->  Y axis
#     grouping_vars[2]  ->  X axis
#     grouping_vars[3]  ->  FACET
#
#   1 or 2 variables are allowed: the missing roles are simply not used
#   (with 1 var there is no X axis and no facet; with 2 there is no facet).
#
#   "vesselFlagCountry" is ALWAYS forced into the facet role, wherever you
#   put it, because it must not sit on an axis. Put it third to keep the
#   call self-documenting; if you don't, the function moves it and says so.
#
#   Examples
#     c("fleetValue", "seasonValue", "areaValue")          y=fleet x=season facet=area
#     c("fleetValue", "seasonValue", "vesselFlagCountry")  y=fleet x=season facet=country
#     c("fleetValue", "areaValue")                         y=fleet x=area   no facet
#     c("fleetValue", "vesselFlagCountry")                 y=fleet no x     facet=country
#     c("fleetValue")                                      y=fleet no x     no facet
#
# ------------------------------------------------------------
# CATEGORIES (one lookup drives Status, Raising_Decision AND colours)
# ------------------------------------------------------------
#   full       100% of landings sampled     dark green    no raising needed
#   safe       coverage > threshold         medium green  direct raising
#   warn       0 < coverage <= threshold    yellow        pool
#   no_disc    landings, no discard data    orange        borrow
#   disc_only  discards, no landings        blue          all catch discarded
#   none       no landings AND no discards  grey          kept in $diagnosis,
#                                                         dropped from all plots
#
# Arguments
#   census         data frame in CEF / RDBES long format
#   stock_id       single stock code
#   grouping_vars  1-3 columns, roles as above
#   threshold      single numeric coverage threshold, e.g. 30 (= 30%)
#   fleet_palette  optional named colours for the bar plot fill
#                  (names = levels of grouping_vars[1]); auto if NULL
#
# Returns list(diagnosis, heatmap, bubbles, coverage_bars, discard_rate,
#              gap_table, palette)
# ============================================================

packs <- c("dplyr", "ggplot2", "ggrepel", "scales", "rlang")
invisible(lapply(packs, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}))

diagnosis_discards <- function(
    census,
    stock_id      = "bll.27.3a47de",
    grouping_vars = c("fleetValue", "seasonValue", "areaValue"),
    threshold     = 30,
    fleet_palette = NULL
) {

  stopifnot(length(grouping_vars) >= 1, length(grouping_vars) <= 3,
            length(stock_id) == 1, is.numeric(threshold))

  # ------------------------------------------------------------
  # 0. ROLES
  # ------------------------------------------------------------
  CTRY <- "vesselFlagCountry"
  if (CTRY %in% grouping_vars) {
    others <- setdiff(grouping_vars, CTRY)          # setdiff keeps input order
    if (length(others) < 1)
      stop("grouping_vars needs at least one variable besides ", CTRY, ".")
    if (grouping_vars[length(grouping_vars)] != CTRY)
      message(CTRY, " moved to the facet role (it must not sit on an axis).")
    y_var     <- others[1]
    x_var     <- if (length(others) >= 2) others[2] else NULL
    facet_var <- CTRY
  } else {
    y_var     <- grouping_vars[1]
    x_var     <- if (length(grouping_vars) >= 2) grouping_vars[2] else NULL
    facet_var <- if (length(grouping_vars) == 3) grouping_vars[3] else NULL
  }
  grouping_syms <- syms(grouping_vars)

  # ------------------------------------------------------------
  # 1. CATEGORY LOOKUP  (labels + colours defined exactly once)
  # ------------------------------------------------------------
  cat_lookup <- data.frame(
    cat_key = c("full", "safe", "warn", "no_disc", "disc_only", "none"),
    status = c(
      "SAFE: 100% sampled",
      paste0("SAFE: > ", threshold, "% coverage"),
      paste0("WARNING: < ", threshold, "% coverage"),
      "WARNING: no discard data",
      "SAFE: discards only / no landings",
      "No landings / no discards"
    ),
    decision = c(
      "\u2705 No raising needed (fully sampled)",
      "\u2705 Direct raising",
      "\u26A0\uFE0F POOL: group with adjacent strata",
      "\u274C BORROW: find similar strata",
      "\u2705 No raising needed (all catch discarded)",
      "\u2013 Excluded: no data"
    ),
    colour = c(
      "#00441B",  # dark green   - fully sampled
      "#41AE76",  # medium green - above threshold
      "#FFD92F",  # yellow       - below threshold but sampled
      "#E66101",  # orange       - no discard data
      "#4292C6",  # blue         - discards only
      "#F0F1F1"   # grey         - nothing (dropped from plots)
    ),
    stringsAsFactors = FALSE
  )
  pal_status   <- setNames(cat_lookup$colour, cat_lookup$status)
  pal_decision <- setNames(cat_lookup$colour, cat_lookup$decision)
  pal_cat      <- setNames(cat_lookup$colour, cat_lookup$cat_key)

  thr_pct <- paste0(threshold, "%")

  # small helpers
  fmt_t <- function(x) ifelse(is.na(x), "",
                              ifelse(x > 99,  sprintf("%.0f", x),
                                     ifelse(x >= 10, sprintf("%.1f", x), sprintf("%.2f", x))))
  mk_label <- function(df, vars) {
    if (length(vars) == 0) return(rep("All", nrow(df)))
    do.call(paste, c(lapply(vars, function(v) as.character(df[[v]])), sep = " | "))
  }

  # ------------------------------------------------------------
  # 2. AGGREGATE
  # ------------------------------------------------------------
  landings_df <- census %>%
    filter(catchCategory == "Lan") %>%      # <- without this, discard rows
    group_by(stock, year, !!!grouping_syms) %>%   #    get summed into landings
    summarise(
      landings = sum(total, na.rm = TRUE),
      landings_with_discards = sum(
        total[!is.na(domainCatchDis) & domainCatchDis != ""], na.rm = TRUE),
      .groups = "drop"
    )

  discards_df <- census %>%
    filter(catchCategory != "Lan") %>%
    group_by(stock, year, !!!grouping_syms) %>%
    summarise(discards = sum(as.numeric(total), na.rm = TRUE), .groups = "drop")

  # ------------------------------------------------------------
  # 3. JOIN + CLASSIFY
  # ------------------------------------------------------------
  setup_analysis <- landings_df %>%
    full_join(discards_df, by = c("stock", "year", grouping_vars)) %>%
    filter(stock %in% stock_id) %>%
    mutate(
      coverage_discards = ifelse(is.na(landings) | landings <= 0, NA_real_,
                                 landings_with_discards / landings * 100),
      cat_key = case_when(
        (is.na(landings)  | landings  <= 0) &
          (is.na(discards) | discards <= 0)              ~ "none",
        is.na(landings) | landings <= 0                  ~ "disc_only",
        is.na(discards) | is.na(coverage_discards) |
          coverage_discards <= 0                         ~ "no_disc",
        coverage_discards >= 100                         ~ "full",
        coverage_discards >  threshold                   ~ "safe",
        TRUE                                             ~ "warn"
      ),
      Status = factor(cat_lookup$status[match(cat_key, cat_lookup$cat_key)],
                      levels = cat_lookup$status),
      Raising_Decision = factor(
        cat_lookup$decision[match(cat_key, cat_lookup$cat_key)],
        levels = cat_lookup$decision),
      # rate only defined where the sampled fraction is trustworthy
      Discard_Rate = case_when(
        cat_key == "full" ~ discards / (landings + discards),
        cat_key == "safe" ~ discards / (landings_with_discards + discards),
        TRUE              ~ NA_real_
      )
    ) %>%
    as.data.frame()

  if (nrow(setup_analysis) == 0)
    stop("No rows left after filtering for stock '", stock_id, "'.")
  if (length(unique(setup_analysis$stock)) > 1)
    stop("More than one stock present; this function handles one stock at a time.")
  if (length(unique(setup_analysis$year)) > 1)
    warning("More than one year present; results are pooled across years.")

  # ------------------------------------------------------------
  # 4. PLOT DATA + SHARED LAYERS
  # ------------------------------------------------------------
  plot_data <- filter(setup_analysis, cat_key != "none")

  # dummy x so the grid plots still work with a single grouping variable
  gx <- x_var
  if (is.null(gx)) { plot_data$.xdummy <- "All"; gx <- ".xdummy" }
  x_lab <- if (gx == ".xdummy") NULL else gx

  facet_layer <- if (!is.null(facet_var))
    facet_wrap(vars(.data[[facet_var]]), scales = "free_y") else NULL

  subtitle_txt <- paste0(unique(setup_analysis$stock), " | ",
                         paste(unique(setup_analysis$year), collapse = ", "))

  plot_theme <- theme_bw(base_size = 10) +
    theme(
      plot.title       = element_text(face = "bold", size = 13),
      plot.subtitle    = element_text(color = "grey40", margin = margin(b = 8)),
      plot.caption     = element_text(color = "grey50", size = 8, hjust = 0),
      axis.text.x      = element_text(size = 7),
      axis.text.y      = element_text(size = 8),
      strip.text       = element_text(face = "bold", size = 9),
      strip.background = element_rect(fill = "grey92", color = NA),
      panel.grid.minor = element_blank(),
      panel.spacing    = unit(0.3, "lines"),
      legend.title     = element_text(face = "bold", size = 9),
      legend.text      = element_text(size = 8)
    )

  # ------------------------------------------------------------
  # 5. HEATMAP
  # ------------------------------------------------------------
  heatmap_plot <- ggplot(plot_data,
                         aes(x = factor(.data[[gx]]), y = .data[[y_var]],
                             fill = Status)) +
    geom_tile(color = "black", alpha = 0.9) +
    geom_text(aes(label = fmt_t(landings)), size = 2.5, fontface = "bold",
              colour = "grey15") +
    scale_fill_manual(values = pal_status, drop = TRUE, name = "Sampling status") +
    facet_layer +
    labs(title = "Discard raising setup: coverage vs threshold",
         subtitle = subtitle_txt,
         caption = paste("Labels = tonnes of landings | Threshold =", thr_pct),
         x = x_lab, y = y_var) +
    plot_theme

  # ------------------------------------------------------------
  # 6. BUBBLE PLOT
  # ------------------------------------------------------------
  bubble_data <- plot_data %>%
    mutate(
      # disc_only has no landings: show it as the smallest bubble, not a gap
      plot_size  = ifelse(cat_key == "disc_only",
                          suppressWarnings(min(landings[landings > 0], na.rm = TRUE)),
                          landings),
      plot_size  = ifelse(is.finite(plot_size), plot_size, 1),
      label_text = case_when(
        cat_key == "disc_only" ~ "100%\nDisc",
        cat_key == "warn"      ~ paste0(round(coverage_discards, 1), "%"),
        TRUE                   ~ ""
      )
    )

  bubble_plot <- ggplot(bubble_data,
                        aes(x = factor(.data[[gx]]), y = .data[[y_var]])) +
    geom_point(aes(size = plot_size, fill = Status),
               shape = 21, color = "black", stroke = 0.8) +
    geom_point(data = filter(bubble_data, cat_key == "disc_only"),
               shape = 4, color = "black", size = 2, stroke = 1) +
    scale_size_continuous(range = c(5, 15), name = "Landings (t)") +
    scale_fill_manual(values = pal_status, drop = TRUE, name = "Sampling status",
                      guide = guide_legend(override.aes = list(size = 6))) +
    scale_x_discrete(expand = expansion(0.2)) +
    geom_text_repel(aes(label = label_text), size = 2.8, fontface = "bold",
                    colour = "red3", force = 2, box.padding = 0.6,
                    point.padding = 0.4, min.segment.length = 0,
                    segment.size = 0.2, segment.color = "grey40",
                    lineheight = 0.8, max.overlaps = Inf) +
    facet_layer +
    labs(title = "Discard raising diagnostic: coverage vs landings",
         subtitle = paste0(subtitle_txt,
                           " | bubble = landings (t), label = coverage %"),
         caption = "Cross (X) = discards present with zero official landings",
         x = x_lab, y = y_var) +
    plot_theme

  # ------------------------------------------------------------
  # 7. SAMPLED vs UNSAMPLED LANDINGS
  # ------------------------------------------------------------
  bar_data <- plot_data %>%
    filter(!is.na(landings), landings > 0) %>%
    mutate(
      sampled = factor(cat_key %in% c("full", "safe", "warn"),
                       levels = c(TRUE, FALSE),
                       labels = c("With discard sampling",
                                  "Without discard sampling")),
      .facet  = if (is.null(facet_var)) "" else as.character(.data[[facet_var]]),
      .lab    = mk_label(., c(y_var, x_var))
    ) %>%
    arrange(.facet, desc(sampled), landings) %>%
    mutate(.key = factor(paste0(.lab, "\u001F", .facet),
                         levels = unique(paste0(.lab, "\u001F", .facet)))) %>%
    group_by(.facet) %>%
    mutate(y_num = row_number(), facet_total = sum(landings)) %>%
    ungroup()

  key_labels  <- setNames(bar_data$.lab, as.character(bar_data$.key))
  x_edge      <- max(bar_data$landings, na.rm = TRUE) * 1.05
  pct_sampled <- with(bar_data,
                      sum(landings[sampled == "With discard sampling"]) /
                        sum(landings) * 100)

  bracket_df <- bar_data %>%
    group_by(.facet, sampled) %>%
    summarise(y_min = min(y_num) - 0.5, y_max = max(y_num) + 0.5,
              y_mid = (min(y_num) + max(y_num)) / 2,
              pct   = sum(landings) / first(facet_total) * 100,
              .groups = "drop") %>%
    mutate(x_pos = x_edge)

  separator_df <- bar_data %>%
    filter(sampled == "Without discard sampling") %>%
    group_by(.facet) %>%
    summarise(sep_y = max(y_num) + 0.5, .groups = "drop") %>%
    filter(.facet %in% bar_data$.facet[bar_data$sampled == "With discard sampling"])

  fleet_levels <- sort(unique(as.character(bar_data[[y_var]])))
  if (is.null(fleet_palette))
    fleet_palette <- setNames(scales::hue_pal()(length(fleet_levels)), fleet_levels)

  coverage_bars <- ggplot(bar_data,
                          aes(x = landings, y = .key, fill = .data[[y_var]])) +
    geom_col(width = 0.7, color = "black") +
    geom_segment(data = bracket_df,
                 aes(x = x_pos, xend = x_pos, y = y_min, yend = y_max),
                 inherit.aes = FALSE, linewidth = 0.6, colour = "grey30") +
    geom_segment(data = bracket_df,
                 aes(x = x_pos, xend = x_pos * 1.02, y = y_mid, yend = y_mid),
                 inherit.aes = FALSE, linewidth = 0.4, colour = "grey30") +
    geom_text(data = bracket_df,
              aes(x = x_pos * 1.03, y = y_mid,
                  label = paste0(sampled, "\n(", round(pct, 0), "%)")),
              inherit.aes = FALSE, hjust = 0, size = 3, fontface = "bold",
              colour = "grey20") +
    geom_segment(data = separator_df,
                 aes(x = 0, xend = x_edge, y = sep_y, yend = sep_y),
                 inherit.aes = FALSE, linetype = "dashed", colour = "grey50",
                 linewidth = 0.4) +
    scale_y_discrete(labels = key_labels) +
    scale_fill_manual(values = fleet_palette, drop = FALSE, name = y_var) +
    scale_x_continuous(labels = scales::comma,
                       expand = expansion(mult = c(0, 0.30))) +
    (if (!is.null(facet_var))
      facet_wrap(vars(.facet), scales = "free_y", ncol = 1) else NULL) +
    labs(title = "Landings with vs without discard sampling",
         subtitle = paste0(subtitle_txt, " | ", round(pct_sampled, 0),
                           "% of landings weight sampled for discards"),
         x = "Landings (t)", y = NULL) +
    coord_cartesian(clip = "off") +
    plot_theme +
    theme(plot.margin = margin(t = 10, r = 100, b = 10, l = 10))

  # ------------------------------------------------------------
  # 8. DISCARD RATE (SAFE strata only: fully sampled + above threshold)
  # ------------------------------------------------------------
  rate_data <- plot_data %>%
    filter(!is.na(Discard_Rate)) %>%
    mutate(
      .facet   = if (is.null(facet_var)) "" else as.character(.data[[facet_var]]),
      .lab     = mk_label(., c(y_var, x_var)),
      rate_pct = Discard_Rate * 100
    ) %>%
    arrange(.facet, rate_pct) %>%
    mutate(.key = factor(paste0(.lab, "\u001F", .facet),
                         levels = unique(paste0(.lab, "\u001F", .facet))))

  discard_rate_plot <- if (nrow(rate_data) == 0) {
    message("No strata qualify for the discard-rate plot (none above threshold).")
    NULL
  } else {
    ggplot(rate_data, aes(x = rate_pct, y = .key, fill = Status)) +
      geom_col(width = 0.7, colour = "black") +
      geom_text(aes(label = paste0(round(rate_pct, 1), "%")),
                hjust = -0.15, size = 2.8, fontface = "bold", colour = "grey20") +
      scale_y_discrete(labels = setNames(rate_data$.lab,
                                         as.character(rate_data$.key))) +
      scale_fill_manual(values = pal_status, drop = TRUE,
                        name = "Sampling status") +
      scale_x_continuous(labels = function(x) paste0(x, "%"),
                         expand = expansion(mult = c(0, 0.18))) +
      (if (!is.null(facet_var))
        facet_wrap(vars(.facet), scales = "free_y", ncol = 1) else NULL) +
      labs(title = "Discard rate in adequately sampled strata",
           subtitle = paste0(subtitle_txt,
                             " | only strata fully sampled or above ", thr_pct),
           caption = paste0("Fully sampled: discards / (landings + discards). ",
                            "Above threshold: discards / (sampled landings + discards)."),
           x = "Discard rate", y = NULL) +
      plot_theme
  }

  # ------------------------------------------------------------
  # 9. GAP TABLE + RETURN
  # ------------------------------------------------------------
  gap_table <- setup_analysis %>%
    filter(!cat_key %in% c("full", "safe", "none")) %>%
    arrange(desc(landings)) %>%
    as.data.frame()

  list(
    diagnosis     = setup_analysis,
    heatmap       = heatmap_plot,
    bubbles       = bubble_plot,
    coverage_bars = coverage_bars,
    discard_rate  = discard_rate_plot,
    gap_table     = gap_table,
    palette       = list(status = pal_status, decision = pal_decision,
                         cat = pal_cat, lookup = cat_lookup)
  )
}

# ------------------------------------------------------------------
# EXAMPLES
# ------------------------------------------------------------------
setup <- diagnosis_discards(census, "bll.27.3a47de",
           grouping_vars = c("fleetValue", "seasonValue", "areaValue"),
           threshold = 50)
# setup$heatmap; setup$bubbles; setup$coverage_bars; setup$discard_rate
# setup$gap_table
#
# # country as facet (third position)
# diagnosis_discards(census, "bll.27.3a47de",
#   grouping_vars = c("fleetValue", "seasonValue", "vesselFlagCountry"),
#   threshold = 30)
# # coarser: no facet
# diagnosis_discards(census, "bll.27.3a47de",
#   grouping_vars = c("fleetValue", "areaValue"), threshold = 30)
# # single dimension
# diagnosis_discards(census, "bll.27.3a47de",
#   grouping_vars = c("fleetValue"), threshold = 30)
#
# # reuse the same colours anywhere else:
# ggplot(...) + scale_fill_manual(values = setup$palette$decision)
