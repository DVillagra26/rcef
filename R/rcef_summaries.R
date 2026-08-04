# =====================================================================
#  rcef_summaries.R   (catch / biology / effort summary tables)
#
#  ORIGIN: this project (ILVO / RCEF Workbench). Freely refactorable.
#  Was: R_development/summaries.R
#  No shiny in this file. If it needs `input`, `session` or a tag,
#  it belongs in app/, not here.
# =====================================================================

# ============================================================
#  summaries.R
#  Overview tables for the three CEF tables and for raised output.
#  All weights in tonnes (CEF WeightLive is kg).
# ============================================================

# ------------------------------------------------------------
#  catches
# ------------------------------------------------------------
#' @noRd
#' @export
catches_summary <- function(catches) {
  if (is.null(catches) || !nrow(catches))
    return(data.frame(item = character(0), value = character(0)))
  u <- if ("variableUnit" %in% names(catches)) catches$variableUnit else NULL
  t_ <- to_tonnes(catches$total, u)
  cc <- catches$catchCategory
  sampled <- !is.na(catches$domainCatchDis) & catches$domainCatchDis != ""
  lan <- cc == "Lan"
  data.frame(
    item = c("Rows", "Stocks", "Years", "Countries", "Fleets (fleetValue)",
             "Areas", "Seasons", "Catch categories",
             "Landings (t)", "Discards (t)",
             "Landings rows linked to a discard estimate",
             "Landings weight with a discard estimate (%)",
             "Rows with domainBiology"),
    value = c(
      format(nrow(catches), big.mark = ","),
      paste(unique(catches$stock), collapse = ", "),
      paste(sort(unique(catches$year)), collapse = ", "),
      length(unique(catches$vesselFlagCountry)),
      length(unique(catches$fleetValue)),
      length(unique(catches$areaValue)),
      paste(sort(unique(catches$seasonValue)), collapse = ", "),
      paste(sort(unique(cc)), collapse = ", "),
      fmt_t3(sum(t_[lan], na.rm = TRUE)),
      fmt_t3(sum(t_[!lan], na.rm = TRUE)),
      sprintf("%s of %s", format(sum(lan & sampled), big.mark = ","),
              format(sum(lan), big.mark = ",")),
      round(sum(t_[lan & sampled], na.rm = TRUE) /
              sum(t_[lan], na.rm = TRUE) * 100, 1),
      if ("domainBiology" %in% names(catches))
        format(sum(!is.na(catches$domainBiology) & catches$domainBiology != ""),
               big.mark = ",") else "0"),
    stringsAsFactors = FALSE)
}

# Catch weight by category, the shape people expect at the top of a report
#' @noRd
#' @export
catches_by_category <- function(catches) {
  u <- if ("variableUnit" %in% names(catches)) catches$variableUnit else NULL
  d <- catches %>%
    mutate(.t = to_tonnes(total, u)) %>%
    group_by(catchCategory) %>%
    summarise(rows = n(), tonnes = sum(.t, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(tonnes))
  bind_rows(d, data.frame(catchCategory = "Total", rows = sum(d$rows),
                          tonnes = sum(d$tonnes))) %>%
    mutate(tonnes = round(tonnes, 3))
}

# ------------------------------------------------------------
#  distributions - biology sampling, seen through domainBiology
# ------------------------------------------------------------
# A catch stratum has biology only when its domainBiology key is filled
# AND that key turns up in the distributions table. distributionType
# says what was measured (LengthTotal / Age), variableType says what the
# value is (Number / WeightLive).
#' @noRd
#' @export
biology_coverage <- function(catches, distributions, dims) {
  miss <- setdiff(dims, names(catches))
  if (length(miss))
    stop("biology_coverage(): run derive_fields() first - missing ",
         paste(miss, collapse = ", "))
  u <- if ("variableUnit" %in% names(catches)) catches$variableUnit else NULL
  keys <- if (!is.null(distributions) && "domainBiology" %in% names(distributions))
    unique(distributions$domainBiology[!is.na(distributions$domainBiology)]) else character(0)

  # a catches table without domainBiology simply has no biology linkage
  db <- if ("domainBiology" %in% names(catches))
    as.character(catches$domainBiology) else rep(NA_character_, nrow(catches))
  catches$.has_key <- !is.na(db) & db != ""
  catches$.in_dist <- catches$.has_key & db %in% keys

  catches %>%
    mutate(.t = to_tonnes(total, u),
           has_key = .has_key, in_dist = .in_dist) %>%
    group_by(across(all_of(dims)), catchCategory) %>%
    summarise(weight = sum(.t, na.rm = TRUE),
              weight_with_biology = sum(.t[in_dist], na.rm = TRUE),
              n_rows = n(), n_with_key = sum(has_key), n_in_dist = sum(in_dist),
              .groups = "drop") %>%
    mutate(biology_coverage = ifelse(weight > 0,
                                     weight_with_biology / weight * 100, NA_real_),
           sampled = n_in_dist > 0) %>%
    as.data.frame()
}

#' @noRd
#' @export
distributions_summary <- function(distributions, catches = NULL) {
  if (is.null(distributions) || !nrow(distributions))
    return(data.frame(item = "Rows", value = "0", stringsAsFactors = FALSE))
  linked <- if (!is.null(catches) && "domainBiology" %in% names(catches))
    sum(unique(distributions$domainBiology) %in%
          unique(catches$domainBiology[!is.na(catches$domainBiology)])) else NA
  data.frame(
    item = c("Rows", "Biology domains", "Domains matched in catches",
             "distributionType", "variableType", "Catch categories",
             "Class range", "Total measurements"),
    value = c(
      format(nrow(distributions), big.mark = ","),
      length(unique(distributions$domainBiology)),
      if (is.na(linked)) "-" else linked,
      paste(sort(unique(distributions$distributionType)), collapse = ", "),
      paste(sort(unique(distributions$variableType)), collapse = ", "),
      paste(sort(unique(distributions$catchCategory)), collapse = ", "),
      if ("distributionClass" %in% names(distributions))
        paste(range(suppressWarnings(as.numeric(distributions$distributionClass)),
                    na.rm = TRUE), collapse = " - ") else "-",
      if ("numMeasurements" %in% names(distributions))
        format(sum(as.numeric(distributions$numMeasurements), na.rm = TRUE),
               big.mark = ",") else "-"),
    stringsAsFactors = FALSE)
}

# What was measured, and for which catch category
#' @noRd
#' @export
distributions_by_type <- function(distributions) {
  distributions %>%
    group_by(distributionType, variableType, catchCategory) %>%
    summarise(rows = n(), domains = n_distinct(domainBiology),
              .groups = "drop") %>%
    arrange(distributionType, variableType, catchCategory)
}

# ------------------------------------------------------------
#  effort
# ------------------------------------------------------------
#' @noRd
#' @export
effort_summary <- function(effort) {
  if (is.null(effort) || !nrow(effort))
    return(data.frame(item = "Rows", value = "0", stringsAsFactors = FALSE))
  data.frame(
    item = c("Rows", "Countries", "Fleets", "Areas", "Seasons",
             "Effort units", "Total effort"),
    value = c(format(nrow(effort), big.mark = ","),
              length(unique(effort$vesselFlagCountry)),
              length(unique(effort$fleetValue)),
              length(unique(effort$areaValue)),
              paste(sort(unique(effort$seasonValue)), collapse = ", "),
              paste(unique(effort$variableType), collapse = ", "),
              format(round(sum(as.numeric(effort$total), na.rm = TRUE), 1),
                     big.mark = ",")),
    stringsAsFactors = FALSE)
}

# ------------------------------------------------------------
#  Raised output
# ------------------------------------------------------------
# Catch category x imported/raised, with the share within each category.
#' @noRd
#' @export
raised_overview_table <- function(raised) {
  u <- if ("variableUnit" %in% names(raised)) raised$variableUnit else NULL
  d <- raised %>%
    mutate(.t = to_tonnes(total, u),
           src = ifelse(importedOrRaised %in% "raised", "Raised", "Imported")) %>%
    group_by(catchCategory, src) %>%
    summarise(tonnes = sum(.t, na.rm = TRUE), .groups = "drop")
  tot_cat <- d %>% group_by(catchCategory) %>%
    summarise(cat_total = sum(tonnes), .groups = "drop")
  out <- d %>% left_join(tot_cat, by = "catchCategory") %>%
    mutate(percent = ifelse(cat_total > 0, round(tonnes / cat_total * 100), NA_real_)) %>%
    select(`Catch category` = catchCategory, `Raised/Imported` = src,
           `Caton (t)` = tonnes, `Percent of category` = percent) %>%
    arrange(`Catch category`, `Raised/Imported`)
  bind_rows(out,
            data.frame(`Catch category` = "Total", `Raised/Imported` = "-",
                       `Caton (t)` = sum(out$`Caton (t)`),
                       `Percent of category` = 100, check.names = FALSE)) %>%
    mutate(`Caton (t)` = round(`Caton (t)`, 3))
}

# Country x area style cross-tab: one block of Landings / Discards / BMS
# per level of col_var, plus a Total block and a Total row.
#' @noRd
#' @export
raised_crosstab <- function(raised, row_var = "vesselFlagCountry",
                            col_var = "areaValue") {
  u <- if ("variableUnit" %in% names(raised)) raised$variableUnit else NULL
  cat_of <- function(x) case_when(
    toupper(x) %in% c("LAN", "LANDINGS") ~ "Landings",
    toupper(x) %in% c("BMS", "BMS LANDING") ~ "BMS",
    TRUE ~ "Discards")
  d <- raised %>%
    mutate(.t = to_tonnes(total, u), .cat = cat_of(catchCategory),
           .r = as.character(.data[[row_var]]),
           .c = as.character(.data[[col_var]])) %>%
    group_by(.r, .c, .cat) %>%
    summarise(t = sum(.t, na.rm = TRUE), .groups = "drop")
  tot <- d %>% group_by(.r, .cat) %>%
    summarise(t = sum(t), .groups = "drop") %>% mutate(.c = "Total")
  all <- bind_rows(d, tot) %>%
    mutate(.cat = factor(.cat, levels = c("Landings", "Discards", "BMS")))
  wide <- all %>%
    tidyr::pivot_wider(names_from = c(.c, .cat), values_from = t,
                       values_fill = 0, names_sep = " | ") %>%
    arrange(.r)
  grand <- wide %>% summarise(across(where(is.numeric), sum)) %>% mutate(.r = "Total")
  bind_rows(wide, grand) %>%
    rename(!!row_var := .r) %>%
    mutate(across(where(is.numeric), ~round(.x, 3)))
}

# Landings and discards split into imported and raised
#' @noRd
#' @export
plot_raised_composition <- function(raised, x_var = NULL, facet_var = NULL,
                                    ncol = 2) {
  u <- if ("variableUnit" %in% names(raised)) raised$variableUnit else NULL
  has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
  d <- raised %>%
    mutate(.t = to_tonnes(total, u),
           src = factor(ifelse(importedOrRaised %in% "raised", "Raised", "Imported"),
                        levels = c("Imported", "Raised")),
           .cat = ifelse(toupper(catchCategory) %in% c("LAN", "LANDINGS"),
                         "Landings", "Discards"),
           .x = if (has(x_var)) as.character(.data[[x_var]]) else "All")
  keys <- c(".x", ".cat", "src", if (has(facet_var)) facet_var)
  d <- d %>% group_by(across(all_of(keys))) %>%
    summarise(t = sum(.t, na.rm = TRUE), .groups = "drop")

  ggplot(d, aes(x = .x, y = t, fill = .cat, alpha = src)) +
    geom_col(position = position_stack(reverse = TRUE), width = 0.72,
             colour = "grey25", linewidth = 0.25) +
    scale_fill_manual(values = c(Landings = "#59A14F", Discards = "#E15759"),
                      name = NULL) +
    scale_alpha_manual(values = c(Imported = 1, Raised = 0.45), name = NULL,
                       guide = guide_legend(override.aes = list(fill = "grey30"))) +
    scale_y_continuous(labels = scales::comma) +
    (if (has(facet_var)) facet_wrap(vars(.data[[facet_var]]), ncol = ncol,
                                    scales = "free_y") else NULL) +
    labs(title = "Catch after raising",
         subtitle = "Solid = imported from the data, translucent = added by raising",
         x = if (has(x_var)) x_var else NULL, y = "Tonnes") +
    app_theme() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
