# =====================================================================
#  rcef_raising.R   (apply_raising(), scenarios, scenario comparison)
#
#  ORIGIN: this project (ILVO / RCEF Workbench). Freely refactorable.
#  Was: R_development/0_Functions_raising.R
#  No shiny in this file. If it needs `input`, `session` or a tag,
#  it belongs in app/, not here.
# =====================================================================

# ============================================================
#  0_Functions_raising.R
#  Apply the raising, and compare scenarios.
# ------------------------------------------------------------
#  Package-ready data module. It sits at the end of the chain:
#
#    build_strata()  ->  classify_strata()  ->  define_raising_groups_*()
#                                                      |
#                              apply_raising() / run_scenario()  <- here
#
#  apply_raising() is the app's OWN arithmetic, used for the interactive
#  preview: it takes a stratum table that already carries a donor condition
#  and raises each stratum on its donor pool. It exists alongside, not instead
#  of, catchRaising(): the ICES function is what produces the numbers that are
#  submitted, and run_native_raising() drives it. Keeping both lets the app
#  show a result instantly and then reconcile it against the reference
#  implementation.
#
#  A SCENARIO is a whole configuration saved under a name - the dimensions, the
#  method and its settings - so two ways of raising the same stock can be run
#  and compared side by side, and so next year's assessment can reload the
#  policy rather than replaying this year's clicks.
#
#  DEPENDENCIES  Imports: dplyr, jsonlite (namespace only).
#  Input: a classified stratum table (0_Functions_classify.R) with the group
#  columns added (0_Functions_groups.R).
# ============================================================

# ----------------------------------------------------------
#  Applying the raising
# ----------------------------------------------------------

#' Raise each stratum on its donor pool
#'
#' Computes the raised discards for every stratum: the discard rate of the
#' stratum's donor pool applied to the weight that carries no discard estimate,
#' added to the discards already observed. Strata that need no raising keep
#' their own numbers.
#'
#' This is the app's own arithmetic, for the interactive preview. The figures
#' that get submitted come from catchRaising() via run_native_raising(); having
#' both means the preview can be checked against the reference implementation
#' rather than trusted.
#'
#' @param strata A stratum table carrying a donor condition column, i.e. the
#'   output of [recommend_raising()] or [assign_rule_groups()].
#' @param donor_key Column naming each stratum's donor pool.
#' @param donor_members Optional pre-computed pool membership; recomputed from
#'   `donor_key` when `NULL`.
#' @return `strata` with the pooled rate, the raised discards and the raised
#'   totals added.
#' @seealso [run_native_raising()], [run_scenario()]
#' @export
apply_raising <- function(strata, donor_key = "raise_condition",
                          donor_members = NULL) {
  s <- strata
  s$.pool <- as.character(s[[donor_key]])

  dok <- donor_ok_of(s)
  pool_rate <- function(idx) {
    idx <- idx[dok[idx]]                      # excluded strata never donate
    if (!length(idx)) return(NA_real_)
    lw <- sum(s$landings_with_discards[idx], na.rm = TRUE)
    dd <- sum(s$discards[idx], na.rm = TRUE)
    if (lw + dd <= 0) return(NA_real_)
    dd / (lw + dd)
  }

  if (is.null(donor_members)) {
    # pool = all strata sharing the same donor_key value
    rates <- vapply(unique(s$.pool), function(p) {
      if (identical(p, "TRUE")) pool_rate(seq_len(nrow(s)))
      else pool_rate(which(s$.pool == p))
    }, numeric(1))
    s$donor_rate <- unname(rates[match(s$.pool, names(rates))])
  } else {
    # manual: pool = every stratum that donates to this stratum's group
    grp <- as.character(s$toBeRaisedGroup)
    s$donor_rate <- vapply(seq_len(nrow(s)), function(i) {
      g <- grp[i]
      if (is.na(g)) return(NA_real_)
      idx <- which(vapply(donor_members, function(v) g %in% v, logical(1)))
      pool_rate(idx)
    }, numeric(1))
  }

  s %>% mutate(
    unsampled_landings = pmax(landings - landings_with_discards, 0),
    # discards already observed, plus an estimate for the unsampled part
    raised_discards = ifelse(
      is.na(donor_rate) | donor_rate >= 1, NA_real_,
      discards + unsampled_landings * donor_rate / (1 - donor_rate)),
    total_discards_est = ifelse(is.na(raised_discards), discards, raised_discards)
  ) %>% select(-.pool)
}

# ----------------------------------------------------------
#  Scenarios: a whole configuration under a name
# ----------------------------------------------------------

#' Describe an automatic-method scenario
#'
#' @param name Scenario name.
#' @param dims The stratum dimensions.
#' @param hierarchy Key columns, finest first.
#' @param thresholds One threshold, or one per level.
#' @param self_threshold Own-stratum threshold, or `NA`.
#' @param mode Retained for older saved scenarios.
#' @param dr_min,dr_max Donor discard-rate window.
#' @return A scenario list, ready for [run_scenario()] or [scenarios_to_json()].
#' @seealso [scenario_manual()], [run_scenario()]
#' @export
scenario_auto <- function(name, dims, hierarchy, thresholds,
                          self_threshold = NA, mode = "nested",
                          dr_min = NA, dr_max = NA) {
  list(name = name, method = "auto", dims = dims, hierarchy = hierarchy,
       thresholds = thresholds, self_threshold = self_threshold, mode = mode,
       dr_min = dr_min, dr_max = dr_max,
       saved = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
}

#' Describe a manual-method scenario
#'
#' @param name Scenario name.
#' @param dims The stratum dimensions.
#' @param rules The rule list.
#' @param sets Named value sets referenced by the rules.
#' @return A scenario list.
#' @seealso [scenario_auto()], [run_scenario()]
#' @export
scenario_manual <- function(name, dims, rules, sets) {
  list(name = name, method = "manual", dims = dims, rules = rules, sets = sets,
       saved = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
}

#' Serialise scenarios to JSON
#'
#' @param sc A list of scenarios.
#' @return A JSON string.
#' @seealso [scenarios_from_json()]
#' @export
scenarios_to_json <- function(sc) jsonlite::toJSON(sc, auto_unbox = TRUE, pretty = TRUE)

#' Read scenarios back from JSON
#'
#' @param txt A JSON string written by [scenarios_to_json()].
#' @return A list of scenarios.
#' @seealso [scenarios_to_json()]
#' @export
scenarios_from_json <- function(txt) {
  x <- jsonlite::fromJSON(txt, simplifyDataFrame = FALSE, simplifyVector = TRUE)
  if (!is.null(x$name)) list(x) else x
}

#' Run one scenario end to end
#'
#' Derives the fields, builds and classifies the strata, defines the groups by
#' whichever method the scenario names, and applies the raising - the whole
#' chain from a catch table to a raised result, driven by a saved
#' configuration.
#'
#' @param sc A scenario from [scenario_auto()] or [scenario_manual()].
#' @param catch_data A CEF catch table.
#' @return The raised stratum table.
#' @seealso [scenario_auto()], [compare_scenarios()]
#' @examples
#' \dontrun{
#' sc  <- scenario_auto("base", dims, c("metier4", "Country"), 50,
#'                      "metier4", "ICESDivision")
#' out <- run_scenario(sc, catch)
#' }
#' @export
run_scenario <- function(sc, catch_data) {
  s <- build_strata(catch_data, sc$dims)
  if (sc$method == "auto") {
    # auto keeps one global window; manual carries it per group instead
    s <- flag_donor_exclusion(s, sc$dr_min %||% NA, sc$dr_max %||% NA)
    r <- recommend_raising(s, sc$hierarchy,
                           combine_thresholds(sc$self_threshold %||% NA,
                                              sc$thresholds, sc$hierarchy),
                           mode = sc$mode %||% "nested", dims = sc$dims)
    apply_raising(r, "raise_condition")
  } else {
    a <- assign_rule_groups(s, sc$rules, sc$sets)
    apply_raising(a, "toBeRaisedGroup", donor_members = a$usedToRaiseGroup)
  }
}

# ----------------------------------------------------------
#  Comparing scenarios
# ----------------------------------------------------------

#' Columns common to several scenario runs
#'
#' @param runs A named list of scenario results.
#' @return The intersection of their column names.
#' @keywords internal
#' @noRd
#' @export
common_keys <- function(runs) Reduce(intersect, lapply(runs, names))

#' Put several scenario runs side by side
#'
#' Joins the runs on the stratum keys so the same stratum's raised discards can
#' be read across scenarios, which is what makes the effect of a threshold or a
#' hierarchy visible.
#'
#' @param runs A named list of results from [run_scenario()].
#' @param dims The stratum dimensions to join on.
#' @return A long data frame with one row per stratum and scenario.
#' @seealso [comparison_table()], [run_scenario()]
#' @export
compare_scenarios <- function(runs, dims) {
  purrr::imap_dfr(runs, function(r, nm) {
    r %>% group_by(across(all_of(dims))) %>%
      summarise(landings = sum(landings, na.rm = TRUE),
                observed_discards = sum(discards, na.rm = TRUE),
                estimated_discards = sum(total_discards_est, na.rm = TRUE),
                .groups = "drop") %>%
      mutate(scenario = nm)
  })
}

#' Wide comparison table, one row per stratum
#'
#' @param cmp Output of [compare_scenarios()].
#' @param keys The stratum key columns.
#' @return A wide data frame, one column per scenario.
#' @seealso [compare_scenarios()]
#' @export
comparison_table <- function(cmp, keys) {
  wide <- cmp %>% select(all_of(keys), scenario, estimated_discards) %>%
    tidyr::pivot_wider(names_from = scenario, values_from = estimated_discards)
  imported <- cmp %>% group_by(across(all_of(keys))) %>%
    summarise(Imported = first(observed_discards), .groups = "drop")
  imported %>% left_join(wide, by = keys) %>%
    mutate(across(where(is.numeric), ~round(.x, 1)))
}

# ----------------------------------------------------------
#  Overviews: what happened, in one table
# ----------------------------------------------------------

