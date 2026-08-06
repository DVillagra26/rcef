# =====================================================================
#  rcef_session.R   (session log + code fragment builders)
#
#  ORIGIN: this project. Was: R_development/session_log.R
#  state_capture() was SPLIT OUT to app/app_state.R: it reads shiny
#  reactiveValues and is the only shiny-coupled function in the file.
# =====================================================================

# ============================================================
#  session_log.R
#  Two parallel records of everything the user does:
#
#   1. an R script, appended to as the session goes, meant to be read
#      and re-run outside the app;
#   2. a JSON state file, meant to be read back BY the app to refill
#      every control.
#
#  They serve different masters. The script is for a human and a
#  reviewer; the JSON is for the app. Trying to make one do both jobs
#  is why "replay the log" usually ends up brittle.
# ============================================================

# Blocks are written in this order regardless of when they were set,
# so the emitted script always reads top to bottom in a sensible order.
#' @noRd
#' @export
LOG_ORDER <- c("header", "setup", "intercatch", "load_cef", "derive",
               "strata", "explore", "auto_raising", "manual_groups",
               "native_raising", "scenarios", "session")

#' @noRd
#' @export
log_new <- function() list(blocks = list(), stamp = list())

# Setting the same id twice REPLACES the block: the script reflects the
# current state of the app, not a transcript of every fumble.
#' @noRd
#' @export
log_set <- function(lg, id, lines) {
  if (is.null(lines) || !length(lines)) { lg$blocks[[id]] <- NULL; return(lg) }
  lg$blocks[[id]] <- lines
  lg$stamp[[id]] <- format(Sys.time(), "%H:%M:%S")
  lg
}

#' @noRd
#' @export
log_render <- function(lg) {
  ids <- c(intersect(LOG_ORDER, names(lg$blocks)),
           setdiff(names(lg$blocks), LOG_ORDER))
  if (!length(ids)) return("## nothing recorded yet")
  unlist(lapply(ids, function(i) c(lg$blocks[[i]], "")), use.names = FALSE)
}

#' @noRd
#' @export
log_write <- function(lg, path) {
  d <- dirname(path)
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  writeLines(log_render(lg), path)
  invisible(path)
}

# ------------------------------------------------------------
#  State: save / restore the app itself
# ------------------------------------------------------------
# Buttons carry a click count, not a setting; replaying them would fire
# the action again on load, so they are dropped.
#' @noRd
#' @export
STATE_SKIP <- paste0("^(save_|add_|del_|move_|apply_all|mirror|to_strata|",
                     "to_matched|clear_|run_|rule_move|up_sc|dl_|file$)")

#' @noRd
#' @export
state_to_json <- function(st) jsonlite::toJSON(st, auto_unbox = TRUE, pretty = TRUE,
                                               null = "null")
#' @noRd
#' @export
state_from_json <- function(txt)
  jsonlite::fromJSON(paste(txt, collapse = "\n"),
                     simplifyDataFrame = FALSE, simplifyVector = TRUE)

# Which updater an input needs. Explicit rules first, then patterns;
# anything unrecognised is skipped rather than guessed at, because a
# wrong updater silently corrupts the control.
#' @noRd
#' @export
state_updater <- function(id, value) {
  if (grepl("^(thr_|self_thr$|ar_dr$|rule_dr$|catchall_dr$)", id)) return("slider")
  if (grepl(paste0("^(self_on|dr_jitter|box_points|box_pooled|clause_neg|",
                   "log_auto|ic_keep_temp|ic_incl_bms|ic_patch_psu|",
                   "nr_assembled)$"), id)) return("check")
  if (grepl(paste0("^(hier_mode|clause_src|ic_source|ic_input_mode|",
                   "ic_rel_mode|nr_engine|nr_source)$"), id)) return("radio")
  if (grepl("^(threshold|ic_rel_year)$", id)) return("numeric")
  if (grepl(paste0("(_fleet$|_area$|_x$|_y$|_fill$|_facet$|_shape$|",
                   "^hier_[0-9]$|^sep$|^stock$|^year$|^cmp_|^river_left$|",
                   "^clause_var$|^ic_metier6$|^nr_(type|origin|vartype)$)"), id))
    return("select")
  if (grepl(paste0("^(rule_id|rule_label|auto_name|man_name|log_dir|log_name|",
                   "ic_path|ic_years|ic_out_path|ic_prefix|ic_suffix|",
                   "nr_log)$"), id)) return("text")
  if (grepl("^(clause_vals|code_parts|ic_output_format)$", id)) return("selectize")
  NA_character_
}

#' @noRd
#' @export
state_apply <- function(session, st) {
  applied <- skipped <- character(0)
  for (id in names(st$inputs)) {
    v <- st$inputs[[id]]
    u <- state_updater(id, v)
    if (is.na(u)) { skipped <- c(skipped, id); next }
    ok <- tryCatch({
      switch(u,
        slider    = updateSliderInput(session, id, value = v),
        check     = updateCheckboxInput(session, id, value = as.logical(v)),
        radio     = updateRadioButtons(session, id, selected = v),
        numeric   = updateNumericInput(session, id, value = as.numeric(v)),
        select    = updateSelectInput(session, id, selected = v),
        selectize = updateSelectizeInput(session, id, selected = v),
        text      = updateTextInput(session, id, value = as.character(v)))
      TRUE }, error = function(e) FALSE)
    if (isTRUE(ok)) applied <- c(applied, id) else skipped <- c(skipped, id)
  }
  list(applied = applied, skipped = skipped)
}

# ------------------------------------------------------------
#  Code fragments contributed by each section
# ------------------------------------------------------------
#' @noRd
#' @export
code_header <- function(script_name) c(
  "## ===========================================================",
  "## Discard / sampling raising workflow",
  sprintf("## Generated by the RCEF workbench, %s",
          format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  sprintf("## Script: %s", script_name),
  "## ===========================================================",
  "library(dplyr); library(tidyr); library(ggplot2); library(rlang)",
  "library(data.table); library(readr)")

#' The setup block of the emitted script
#'
#' @param external_dir Optional; if given, recorded as a comment only.
#'   The RDBESStockCoord scripts are vendored, so nothing is sourced.
#' @return A character vector of R code.
#' @export
code_setup <- function(external_dir = NULL) c(
  "## ---- Workflow functions ----------------------------------",
  "## Everything below - the workbench helpers AND the RDBESStockCoord",
  "## engines catchRaising() / catchAllocationAoL() - ships in one package.",
  "library(rcef)",
  if (!is.null(external_dir) && nzchar(external_dir))
    c("",
      "## A local copy of the RDBESStockCoord scripts was present when this",
      "## script was generated. It is NOT sourced: the package vendors them.",
      sprintf('## (was: %s)', external_dir)) else NULL)

#' @noRd
#' @export
code_session <- function(st_path) c(
  "## ---- Session state ---------------------------------------",
  sprintf("## App state saved to: %s", st_path),
  "## Load it back in the workbench to refill every control.")

# ============================================================
#  Code emission: rules, not frozen groupings
# ============================================================
# The automatic allocation must be reproducible as a RULE. Emitting the
# 66 stratum_id lists this year's data happened to produce would be
# wrong next year: the same script must be re-runnable on new data and
# yield next year's grouping from the same policy. So the script records
# the hierarchy, the thresholds and the donor window, and calls the same
# functions the app calls.
#
# Manual allocations are the opposite case: there the grouping IS the
# decision, so the explicit strataCond / matchedDataCond lists are what
# gets written.
#' @noRd
#' @export
code_auto_rule <- function(name, dims, hierarchy, thresholds, self_threshold,
                           mode, dr_min = NA, dr_max = NA) {
  c(sprintf("## ---- Allocation '%s' (automatic rule) --------------------", name),
    "## Re-running this on another year reapplies the same policy and",
    "## produces that year's grouping. Nothing is hard-coded.",
    sprintf('alloc_%s <- list(', name),
    sprintf('  name          = "%s",', name),
    sprintf("  dims          = %s,", fmt_vals(dims)),
    sprintf("  hierarchy     = %s,", fmt_vals(hierarchy)),
    sprintf("  thresholds    = c(%s),", paste(thresholds, collapse = ", ")),
    sprintf("  self_threshold = %s,",
            if (is.na(self_threshold)) "NA" else self_threshold),
    sprintf('  mode          = "%s",', mode),
    sprintf("  donor_dr_min  = %s,", if (is.na(dr_min)) "NA" else dr_min),
    sprintf("  donor_dr_max  = %s)", if (is.na(dr_max)) "NA" else dr_max),
    "",
    sprintf("strata_%s <- build_strata(catch_data, alloc_%s$dims) %%>%%", name, name),
    sprintf("    flag_donor_exclusion(alloc_%s$donor_dr_min, alloc_%s$donor_dr_max)",
            name, name),
    "",
    sprintf("rec_%s <- recommend_raising(strata_%s,", name, name),
    sprintf("    hierarchy      = alloc_%s$hierarchy,", name),
    sprintf("    thresholds     = alloc_%s$thresholds,", name),
    sprintf("    self_threshold = alloc_%s$self_threshold,", name),
    sprintf("    mode           = alloc_%s$mode,", name),
    sprintf("    dims           = alloc_%s$dims)", name),
    "",
    sprintf("## the two condition lists fall out of the rule:"),
    sprintf("conds_%s <- conds_from_auto(rec_%s)", name, name),
    "")
}

#' @noRd
#' @export
code_manual_rule <- function(name, dims, rules, sets = list()) {
  cd <- conds_from_rules(rules, sets, dims)
  q <- function(l) vapply(seq_along(l), function(i)
    sprintf("    %s = quo(%s)%s", names(l)[i],
            wrap_lines(rlang::quo_text(l[[i]]), width = 64),
            if (i < length(l)) "," else ""), character(1))
  needs_metrics <- any(vapply(rules, function(r)
    any(!is.na(c(r$cov_min %||% NA, r$meas_min %||% NA,
                 r$dr_min %||% NA, r$dr_max %||% NA))), logical(1)))
  # Show the SIMPLE clauses - each group's own condition, the catch-all
  # as TRUE - and complement them at runtime. First-match-wins means the
  # "& !(everything above)" is redundant to a reader; complement_conds()
  # supplies it for the engine, which does need the partition.
  qs <- function(l) vapply(seq_along(l), function(i)
    sprintf("    %s = quo(%s)%s", names(l)[i],
            wrap_lines(rlang::quo_text(l[[i]]), width = 64),
            if (i < length(l)) "," else ""), character(1))
  c(sprintf("## ---- Allocation '%s' (manual groups) ---------------------", name),
    "## Groups are tested top to bottom, first match wins. The strata",
    "## clauses are written in that plain form; complement_conds() turns",
    "## them into the partition catchRaising() needs (it sums the",
    "## per-clause results, so overlapping clauses would double-count).",
    sprintf("dims_%s <- %s", name, fmt_vals(dims)),
    if (needs_metrics) c("",
      "## A donor condition below is written as arithmetic on stratum",
      "## metrics (coverage, discard_rate, n_measurements). Those are",
      "## properties of a STRATUM, so a CEF table has none of them - this",
      "## joins them onto the rows before the conditions are evaluated.",
      sprintf("catch_data <- add_stratum_metrics(catch_data, dims_%s)", name)),
    "",
    sprintf("strataClauses_%s <- list(", name), qs(cd$strataCond_simple), ")",
    "",
    sprintf("conds_%s <- list(", name),
    sprintf("  strataCond = complement_conds(strataClauses_%s),", name),
    "  matchedDataCond = list(", q(cd$matchedDataCond), "  ))",
    "")
}

# Raising is always catchRaising(); the built-in shortcut is gone.
#' @noRd
#' @export
code_raise <- function(name, dims, type = "discards", originType = "WGEstimate",
                       variableType = "WeightLive", log_file = "Log.txt") {
  c(sprintf("## ---- Raise with allocation '%s' --------------------------", name),
    sprintf("cd_%s <- cef_prepare(catch_data, %s)", name, fmt_vals(dims)),
    sprintf("raised_%s <- catchRaising(", name),
    sprintf("    catch_data                  = cd_%s,", name),
    sprintf("    condition_raising_st_list   = conds_%s$strataCond,", name),
    sprintf("    condition_matched_data_list = conds_%s$matchedDataCond,", name),
    sprintf('    type = "%s", originType = "%s", variableType = "%s",',
            type, originType, variableType),
    sprintf('    logFile = "%s", assembled_output = TRUE)', log_file),
    "")
}

#' @noRd
#' @export
code_compare <- function(names_vec) {
  if (length(names_vec) < 2) return(character(0))
  c("## ---- Compare allocations ---------------------------------",
    sprintf("comparison <- bind_rows(%s)",
            paste(sprintf('raised_%s %%>%% mutate(allocation = "%s")',
                          names_vec, names_vec), collapse = ",\n                        ")),
    "",
    "comparison %>%",
    '    filter(catchCategory == "Dis") %>%',
    "    group_by(allocation, importedOrRaised) %>%",
    "    summarise(tonnes = sum(total, na.rm = TRUE) / 1000, .groups = \"drop\") %>%",
    "    tidyr::pivot_wider(names_from = importedOrRaised, values_from = tonnes)",
    "")
}

#' @noRd
#' @export
code_final <- function(name) c(
  "## ===========================================================",
  sprintf("## FINAL: allocation '%s' was the one adopted.", name),
  "## The comparisons above are kept for the record; this is the",
  "## result that feeds the assessment.",
  "## ===========================================================",
  sprintf("catch_data_raised <- raised_%s", name),
  "")

# Plot code, so the script reproduces what was on screen
#' @noRd
#' @export
code_plots <- function(active) {
  if (!length(active)) return(character(0))
  c("## ---- Plots shown in the session --------------------------",
    "## Only the configuration that was active is written out.",
    unlist(active, use.names = FALSE), "")
}
