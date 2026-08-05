# =====================================================================
#  rcef_groups.R   (auto cascade + manual rules -> strata_groups -> raising conditions)
#
#  ORIGIN: this project (ILVO / RCEF Workbench). Freely refactorable.
#  Was: R_development/0_Functions_groups.R
#  No shiny in this file. If it needs `input`, `session` or a tag,
#  it belongs in app/, not here.
# =====================================================================

# ============================================================
#  0_Functions_groups.R
#  Define the raising / allocation groups for a stock.
# ------------------------------------------------------------
#  Package-ready data module. Once the strata are classified by sampling
#  (0_Functions_classify.R), each stratum that cannot stand on its own must
#  be told WHERE to borrow a discard rate or a length/age distribution from.
#  Grouping strata with their donors is what these functions do, by either of
#  two methods that produce the SAME output shape:
#
#    AUTOMATIC  a fine-to-coarse cascade. Each stratum is raised on the
#               finest pool in a key hierarchy that clears a coverage
#               threshold; anything left over falls back to ALL.
#                 recommend_raising() -> per-stratum recommendation
#                 raising_groups()    -> the distinct groups
#                 conds_from_auto()   -> conditions for raising
#               or, in one call, define_raising_groups_auto().
#
#    MANUAL     an ordered list of rules, first match wins. Each rule has a
#               raise side (which strata) and a source side (which strata may
#               donate, optionally narrowed by a discard-rate window or named
#               exclusions).
#                 new_rule()          -> build a rule
#                 assign_rule_groups()-> tag strata with their groups
#                 conds_from_rules()  -> conditions for raising
#               or, in one call, define_raising_groups_manual().
#
#  OUTPUT (both methods)
#    A list with `strataCond` and `matchedDataCond`: named lists of quosures,
#    one per group, giving the strata to raise and the data to raise them
#    with. This is exactly the shape catchRaising() (discards) and
#    catchAllocationAoL() (lengths and ages) consume, so the app -- or a user
#    at the console -- can go straight from groups to a raised catch.
#
#  DEPENDENCIES
#    Imports: dplyr, rlang, purrr.
#    Input is a classified stratum table from 0_Functions_classify.R.
# ============================================================

#' @keywords internal
#' @name rcef-groups-imports
#' @noRd
#' @importFrom dplyr %>% across all_of arrange count desc group_by mutate n
#'   row_number select summarise
#' @importFrom rlang %||%
NULL

# ----------------------------------------------------------
#  Condition primitives (internal)
# ----------------------------------------------------------

#' Quote a value set for an emitted `%in%` clause
#' @param v A vector of values.
#' @return A string: `"x"` for one value, `c("a", "b")` for several.
#' @keywords internal
#' @noRd
#' @export
fmt_vals <- function(v) {
  v <- unique(as.character(v))
  if (length(v) == 1) paste0('"', v, '"')
  else paste0('c(', paste0('"', v, '"', collapse = ", "), ')')
}

#' Render one clause (a dimension restricted to a value set) as text
#' @param cl A clause: list(var, values or set, negate).
#' @return A string such as `metier4 %in% c("OTB", "TBB")`.
#' @keywords internal
#' @noRd
#' @export
clause_string <- function(cl) {
  rhs <- if (!is.null(cl$set) && nzchar(cl$set)) cl$set else fmt_vals(cl$values)
  core <- sprintf("%s %%in%% %s", cl$var, rhs)
  if (isTRUE(cl$negate)) paste0("!(", core, ")") else core
}

#' Join clauses into one condition string with `&`
#' @param clauses A list of clauses.
#' @return A condition string, or "TRUE" when there are no clauses.
#' @keywords internal
#' @noRd
#' @export
cond_string <- function(clauses) {
  if (!length(clauses)) return("TRUE")
  paste(vapply(clauses, clause_string, character(1)), collapse = " & ")
}

#' Evaluate clauses against a data frame
#' @param df A data frame.
#' @param clauses A list of clauses.
#' @param sets Named lists of value sets referenced by clauses.
#' @return A logical vector, one element per row of `df`.
#' @keywords internal
#' @noRd
#' @export
eval_clauses <- function(df, clauses, sets = list()) {
  if (!length(clauses)) return(rep(TRUE, nrow(df)))
  Reduce(`&`, lapply(clauses, function(cl) {
    vals <- if (!is.null(cl$set) && nzchar(cl$set)) sets[[cl$set]] else cl$values
    hit <- as.character(df[[cl$var]]) %in% as.character(vals)
    if (isTRUE(cl$negate)) !hit else hit
  }))
}

#' Condition text that selects exactly one stratum row
#' @param row A one-row data frame.
#' @param keys The key columns to pin.
#' @return A condition string pinning each key to the row's value.
#' @keywords internal
#' @noRd
#' @export
row_condition <- function(row, keys) {
  paste(vapply(keys, function(k)
    sprintf('%s %%in%% %s', k, fmt_vals(row[[k]])), character(1)),
    collapse = " & ")
}

# ----------------------------------------------------------
#  Donor exclusion
# ----------------------------------------------------------

#' Flag strata that may not donate, by discard-rate window
#'
#' Adds a logical `donor_ok` column: a stratum whose own discard rate falls
#' outside `[dr_min, dr_max]` is kept in the diagnosis but never contributes
#' to a donor pool. Use it to stop a freak stratum dragging a pooled rate
#' around. Strata with no rate are left eligible.
#'
#' @param s A stratum table.
#' @param dr_min,dr_max Rate window bounds (fractions, e.g. 0.05); `NA` = open.
#' @param cov_min Minimum coverage, percent, to remain an eligible donor.
#'   A stratum that is itself barely sampled lends a rate resting on very
#'   little. `NA` = off.
#' @param meas_min Minimum number of measurements to remain eligible.
#'   `NA` = off.
#' @param excluded Stratum keys picked out by hand, as produced by
#'   [excluded_keys()].
#' @param dims Key columns, needed to match `excluded` against the rows.
#' @return `s` with a `donor_ok` logical column.
#' @seealso [recommend_raising()]
#' @export
flag_donor_exclusion <- function(s, dr_min = NA, dr_max = NA,
                                 cov_min = NA, meas_min = NA,
                                 excluded = character(0), dims = NULL) {
  ok <- rep(TRUE, nrow(s))
  if (!is.na(dr_min)) ok <- ok & (is.na(s$discard_rate) | s$discard_rate >= dr_min)
  if (!is.na(dr_max)) ok <- ok & (is.na(s$discard_rate) | s$discard_rate <= dr_max)

  # A stratum that is itself barely sampled is a bad donor: raising a
  # neighbour on it lends the neighbour a discard rate that rests on very
  # little. NA coverage is left in rather than dropped - unknown is not
  # the same as low, and dropping it silently would shrink the pool in a
  # way nothing on screen explains.
  if (!is.na(cov_min) && "coverage" %in% names(s))
    ok <- ok & (is.na(s$coverage) | s$coverage >= cov_min)
  if (!is.na(meas_min) && "n_measurements" %in% names(s))
    ok <- ok & (is.na(s$n_measurements) | s$n_measurements >= meas_min)

  if (length(excluded) && !is.null(dims))
    ok <- ok & !(stratum_keys(s, dims) %in% excluded)

  s$donor_ok <- ok
  s
}

#' Read the `donor_ok` flag, defaulting to all-eligible
#' @param s A stratum table.
#' @return A logical vector; all `TRUE` when `donor_ok` is absent.
#' @keywords internal
#' @noRd
#' @export
donor_ok_of <- function(s)
  if ("donor_ok" %in% names(s)) s$donor_ok else rep(TRUE, nrow(s))

#' Emitted clause that drops named strata from a donor pool
#' @param excluded Character vector of joined stratum keys to drop.
#' @param dims The key columns that make up a stratum key.
#' @return A negated `%in%` expression string, or `NULL`.
#' @keywords internal
#' @noRd
#' @export
donor_exclusion_expr <- function(excluded, dims) {
  if (!length(excluded)) return(NULL)
  present <- dims[nzchar(dims)]
  # sep = " | " is required: paste(a, " | ", b) would insert its own
  # spaces around the separator and never match the stored key
  key <- paste(sprintf("as.character(%s)", present), collapse = ", ")
  sprintf('!paste(%s, sep = " | ") %%in%% c(%s)', key,
          paste0('"', excluded, '"', collapse = ", "))
}

#' Drop named strata from a table, for a donor-pool preview
#'
#' Removes the rows whose joined key (`dims` pasted with " | ") is in
#' `excluded`. Used to preview a donor pool after named exclusions.
#'
#' @param strata A stratum table.
#' @param excluded Character vector of joined stratum keys to drop.
#' @param dims The key columns that make up a stratum key.
#' @return `strata` without the excluded rows.
#' @seealso [flag_donor_exclusion()]
#' @export
apply_donor_exclusion <- function(strata, excluded, dims) {
  if (!length(excluded) || is.null(strata) || !nrow(strata)) return(strata)
  present <- intersect(dims, names(strata))
  if (!length(present)) return(strata)
  key <- do.call(paste, c(lapply(present, function(k)
    as.character(strata[[k]])), sep = " | "))
  strata[!key %in% excluded, , drop = FALSE]
}

# ----------------------------------------------------------
#  Automatic method: fine-to-coarse cascade
# ----------------------------------------------------------

#' Build the fine-to-coarse cascade of donor pools
#'
#' Turns a key hierarchy and its thresholds into the ordered list of steps
#' the automatic method walks, from the stratum itself out to ALL. Each step
#' drops the last key, widening the pool by one dimension, and carries the
#' coverage threshold it must clear and a plain-language note of what it
#' pools. Built once and shared by [recommend_raising()] and
#' [decision_tree()] so the explanation can never drift from the algorithm.
#'
#' @param hierarchy Key columns, finest first (e.g. `c("metier4",
#'   "Country", "Season")`).
#' @param thresholds One threshold, or one per hierarchy level.
#' @param self_threshold Threshold for the own-stratum step; `NA` to omit it.
#' @param mode Retained for older saved scenarios; the cascade always widens
#'   one key at a time.
#' @param dims The full stratum dimensions, for the "pooled across ..." note;
#'   defaults to `hierarchy`.
#' @return A list of steps, each with `keys`, `threshold`, `label`, `note`
#'   and `id`.
#' @seealso [recommend_raising()], [decision_tree()]
#' @export
raising_steps <- function(hierarchy, thresholds, self_threshold = NA,
                          mode = "nested", dims = NULL) {
  mode <- "nested"
  if (length(thresholds) == 1) thresholds <- rep(thresholds, length(hierarchy))
  stopifnot(length(thresholds) == length(hierarchy))
  if (is.null(dims)) dims <- hierarchy

  and_list <- function(v) {
    if (length(v) == 1) return(v)
    paste(paste(v[-length(v)], collapse = ", "), "and", v[length(v)])
  }
  describe <- function(keys) {
    dropped <- setdiff(dims, keys)
    paste0("same ", and_list(keys),
           if (length(dropped))
             paste0(" - pooled across all ", and_list(dropped)) else "")
  }

  steps <- list()
  if (!is.na(self_threshold))
    steps[[1]] <- list(keys = NULL, self = TRUE, threshold = self_threshold,
                       label = "own stratum",
                       note = "the stratum has enough of its own sampling")

  key_sets <- lapply(rev(seq_along(hierarchy)),
                     function(k) hierarchy[seq_len(k)])
  thr_of <- function(ks) thresholds[length(ks)]

  for (ks in key_sets) {
    # grouping by every dimension is the same thing as the stratum itself,
    # so skip it when the own-stratum step already covered that
    if (!is.na(self_threshold) && setequal(ks, dims)) next
    steps[[length(steps) + 1]] <- list(
      keys = ks, self = FALSE, threshold = thr_of(ks),
      label = paste(ks, collapse = " + "), note = describe(ks))
  }
  for (i in seq_along(steps)) steps[[i]]$id <- as.character(i)
  steps
}

#' Recommend a donor pool for every stratum (automatic method)
#'
#' Walks the cascade from [raising_steps()] fine to coarse and assigns each
#' stratum to the FIRST pool that clears its threshold: its own data if
#' covered enough, else the narrowest wider pool that qualifies, else ALL.
#' Fully sampled strata (coverage >= 100) need no raising and are labelled so.
#' Excluded donors (see [flag_donor_exclusion()]) never enter a pool.
#'
#' @param strata A classified stratum table with `coverage`, `landings` and
#'   `landings_with_discards`, plus the hierarchy columns.
#' @param hierarchy Key columns, finest first.
#' @param thresholds One threshold, or one per hierarchy level.
#' @param self_threshold Threshold for raising on the stratum's own data;
#'   `NA` folds it into the first cascade step.
#' @param mode Retained for older saved scenarios.
#' @param all_label Label for the final fallback pool.
#' @param dims The full stratum dimensions; defaults to `hierarchy`.
#' @return `strata` with `raise_step`, `raise_level`, `raise_condition`
#'   (the donor-selecting expression), `raise_coverage` and
#'   `raise_n_strata` added.
#' @seealso [raising_groups()], [conds_from_auto()], [decision_tree()]
#' @export
recommend_raising <- function(strata, hierarchy, thresholds,
                              self_threshold = NA, mode = "nested",
                              all_label = "ALL", dims = NULL) {
  stopifnot(length(hierarchy) >= 1, all(hierarchy %in% names(strata)))
  steps <- raising_steps(hierarchy, thresholds, self_threshold, mode, dims)

  n <- nrow(strata); dok <- donor_ok_of(strata)
  cond <- rep(NA_character_, n); lvl <- rep(NA_character_, n)
  cov  <- rep(NA_real_, n);      nstr <- rep(NA_integer_, n)
  stp  <- rep(NA_character_, n)          # which step resolved each stratum

  full <- !is.na(strata$coverage) & strata$coverage >= 100
  if (any(full)) {
    cond[full] <- "(none needed)"; lvl[full] <- "no raising needed"
    cov[full] <- 100; nstr[full] <- 1L; stp[full] <- "0"
  }

  for (st in steps) {
    todo <- is.na(cond)
    if (!any(todo)) break

    if (isTRUE(st$self)) {
      take <- todo & !is.na(strata$coverage) &
        strata$coverage >= st$threshold & dok
      if (any(take)) {
        keys <- if (is.null(dims)) hierarchy else dims
        cond[take] <- vapply(which(take), function(j)
          row_condition(strata[j, , drop = FALSE], keys), character(1))
        lvl[take] <- "own stratum"; stp[take] <- st$id
        cov[take] <- strata$coverage[take]; nstr[take] <- 1L
      }
      next
    }

    keys <- st$keys
    g <- strata[dok, , drop = FALSE] %>%
      group_by(across(all_of(keys))) %>%
      summarise(g_lan = sum(landings, na.rm = TRUE),
                g_lwd = sum(landings_with_discards, na.rm = TRUE),
                g_n = n(), .groups = "drop") %>%
      mutate(g_cov = ifelse(g_lan > 0, g_lwd / g_lan * 100, NA_real_))
    kf <- function(d) do.call(paste, c(lapply(keys, function(k)
      as.character(d[[k]])), sep = "\r"))
    idx  <- match(kf(strata), kf(g))
    qual <- !is.na(idx) & !is.na(g$g_cov[idx]) & g$g_cov[idx] >= st$threshold
    take <- todo & qual
    if (any(take)) {
      cond[take] <- vapply(which(take), function(j)
        row_condition(strata[j, , drop = FALSE], keys), character(1))
      lvl[take] <- st$label; stp[take] <- st$id
      cov[take] <- g$g_cov[idx[take]]; nstr[take] <- g$g_n[idx[take]]
    }
  }

  fb <- is.na(cond)
  if (any(fb)) {
    cond[fb] <- "TRUE"; lvl[fb] <- all_label; stp[fb] <- "fallback"
    tl <- sum(strata$landings[dok], na.rm = TRUE)
    cov[fb] <- if (tl > 0)
      sum(strata$landings_with_discards[dok], na.rm = TRUE) / tl * 100 else NA
    nstr[fb] <- sum(dok)
  }

  strata %>% mutate(raise_step = stp, raise_level = lvl, raise_condition = cond,
                    raise_coverage = round(cov, 1), raise_n_strata = nstr)
}

#' Step-by-step table of the automatic cascade
#'
#' Summarises a recommendation by cascade step: the pool, its threshold, what
#' it pools, and how many strata and how much landings left the cascade
#' there. Counted on `raise_step`, so two steps that share a label are never
#' conflated.
#'
#' @param rec Output of [recommend_raising()].
#' @param hierarchy,thresholds,self_threshold,mode,dims As passed to
#'   [recommend_raising()].
#' @return A data frame, one row per step (including step 0 and the fallback).
#' @seealso [recommend_raising()], [check_partition()]
#' @export
decision_tree <- function(rec, hierarchy, thresholds, self_threshold,
                          mode = "nested", dims = NULL) {
  steps <- raising_steps(hierarchy, thresholds, self_threshold, mode, dims)
  ids  <- c("0", vapply(steps, `[[`, character(1), "id"), "fallback")
  labs <- c("no raising needed", vapply(steps, `[[`, character(1), "label"), "ALL")
  thr  <- c(NA, vapply(steps, `[[`, numeric(1), "threshold"), NA)
  note <- c("already 100% sampled, nothing to raise",
            vapply(steps, `[[`, character(1), "note"),
            paste0("every stratum in the data, pooled across all ",
                   paste(dims %||% hierarchy, collapse = ", ")))
  cnt <- vapply(ids, function(i) sum(rec$raise_step == i, na.rm = TRUE), integer(1))
  lan <- vapply(ids, function(i)
    sum(rec$landings[rec$raise_step == i], na.rm = TRUE), numeric(1))
  data.frame(step = ifelse(ids == "fallback", "Fallback",
                           ifelse(ids == "0", "0", ids)),
             pool = labs, threshold = thr, what_it_pools = note,
             n_strata = unname(cnt), landings = round(unname(lan), 3),
             pct_landings = round(unname(lan) / sum(rec$landings, na.rm = TRUE) * 100, 1),
             stringsAsFactors = FALSE)
}

#' Check the cascade partitions the data exactly
#'
#' Confirms every stratum leaves the cascade at exactly one step and the
#' landings add back to the total -- nothing dropped or double-counted.
#'
#' @param rec Output of [recommend_raising()].
#' @param tr Output of [decision_tree()].
#' @return A list of checks: `n_ok`, `l_ok`, `unassigned`, and the counts and
#'   totals compared.
#' @seealso [decision_tree()]
#' @export
check_partition <- function(rec, tr) {
  tot_n <- nrow(rec); tot_l <- sum(rec$landings, na.rm = TRUE)
  list(n_ok = sum(tr$n_strata) == tot_n,
       l_ok = isTRUE(all.equal(sum(tr$landings), round(tot_l, 3), tolerance = 1e-6)),
       unassigned = sum(is.na(rec$raise_step)),
       n_steps = sum(tr$n_strata), n_total = tot_n,
       l_steps = round(sum(tr$landings), 3), l_total = round(tot_l, 3))
}

#' Collapse the per-stratum recommendation into a group list
#'
#' Reduces a recommendation to its distinct groups: one row per unique
#' (raise level, donor condition), numbered `G1`, `G2`, ... These groups are
#' what [conds_from_auto()] turns into raising conditions.
#'
#' @param rec Output of [recommend_raising()].
#' @return A data frame with `group_id`, `raise_level`, `raise_condition` and
#'   `n_strata`.
#' @seealso [conds_from_auto()]
#' @export
raising_groups <- function(rec) {
  rec %>%
    count(raise_level, raise_condition, name = "n_strata") %>%
    arrange(raise_condition == "TRUE", raise_level, raise_condition) %>%
    mutate(group_id = paste0("G", row_number())) %>%
    select(group_id, raise_level, raise_condition, n_strata)
}

# ----------------------------------------------------------
#  Manual method: ordered rules, first match wins
# ----------------------------------------------------------

#' Construct one manual grouping rule
#'
#' A rule is one group with a raise side (clauses selecting the strata to be
#' raised) and a source side (clauses selecting the strata allowed to donate,
#' defaulting to the raise side). The source side may be narrowed by a
#' discard-rate window and by named stratum exclusions. Rules are applied in
#' order, first match wins, so a final clause-less rule is the catch-all.
#'
#' @param id Group id.
#' @param label Human-readable group label.
#' @param raise Clauses selecting the strata to raise.
#' @param source Clauses selecting the donors; defaults to `raise`.
#' @param dr_min,dr_max Discard-rate window on the source side; `NA` = open.
#' @param excluded Joined stratum keys removed from this group's donor pool.
#' @return A rule (a list) for [assign_rule_groups()] and
#'   [conds_from_rules()].
#' @seealso [assign_rule_groups()], [conds_from_rules()]
#' @export
new_rule <- function(id, label = "", raise = list(), source = NULL,
                     dr_min = NA, dr_max = NA, excluded = character(0)) {
  list(id = id, label = label, raise = raise,
       source = if (is.null(source)) raise else source,
       dr_min = dr_min, dr_max = dr_max,
       # named strata dropped from THIS group's donor pool
       excluded = excluded)
}

#' The discard-rate window as a printable clause
#' @param r A rule.
#' @return A string such as `discard_rate >= 0.05`, or "".
#' @keywords internal
#' @noRd
#' @export
dr_clause_string <- function(r) {
  bits <- c(if (!is.na(r$dr_min %||% NA)) sprintf("discard_rate >= %s", r$dr_min),
            if (!is.na(r$dr_max %||% NA)) sprintf("discard_rate <= %s", r$dr_max))
  if (!length(bits)) "" else paste(bits, collapse = " & ")
}

#' Full donor condition text for a rule (clauses + rate + exclusions)
#' @param r A rule.
#' @param dims Stratum key columns, for named exclusions.
#' @return The `matchedDataCond` string for the rule.
#' @keywords internal
#' @noRd
#' @export
source_cond_string <- function(r, dims = NULL) {
  base <- cond_string(r$source)
  dr <- dr_clause_string(r)
  # a named-stratum exclusion is part of the donor clause too, so the
  # printed condition and the emitted script both carry it
  ex <- if (length(r$excluded %||% character(0)) && !is.null(dims))
    donor_exclusion_expr(r$excluded, dims) else NULL
  parts <- c(if (base != "TRUE") base, if (nzchar(dr)) dr, ex)
  if (!length(parts)) "TRUE" else paste(parts, collapse = " & ")
}

#' Rows eligible to donate to a rule
#' @param df A stratum table.
#' @param r A rule.
#' @param sets Named value sets referenced by clauses.
#' @param dims Stratum key columns, for named exclusions.
#' @return A logical vector over `df`: clause match, inside the rate window,
#'   and not named for exclusion.
#' @keywords internal
#' @noRd
#' @export
source_match <- function(df, r, sets = list(), dims = NULL) {
  m <- eval_clauses(df, r$source, sets)
  # drop any stratum the user named for exclusion from this donor pool
  ex <- r$excluded %||% character(0)
  if (length(ex) && !is.null(dims)) {
    present <- intersect(dims, names(df))
    if (length(present)) {
      key <- do.call(paste, c(lapply(present, function(k)
        as.character(df[[k]])), sep = " | "))
      m <- m & !(key %in% ex)
    }
  }
  if (!is.na(r$dr_min %||% NA))
    m <- m & !is.na(df$discard_rate) & df$discard_rate >= r$dr_min
  if (!is.na(r$dr_max %||% NA))
    m <- m & !is.na(df$discard_rate) & df$discard_rate <= r$dr_max
  m
}

#' First-match-wins raise-group id per stratum
#' @param df A stratum table.
#' @param rules A list of rules.
#' @param sets Named value sets.
#' @return A character vector: the id of the first rule whose raise side
#'   matches each row.
#' @keywords internal
#' @noRd
#' @export
apply_rules_first <- function(df, rules, sets = list()) {
  out <- rep(NA_character_, nrow(df))
  for (r in rules) {
    m <- eval_clauses(df, r$raise, sets)
    out[is.na(out) & m] <- r$id
  }
  out
}

#' Every donor-group id a stratum contributes to
#' @param df A stratum table.
#' @param rules A list of rules.
#' @param sets Named value sets.
#' @return A list-column of character vectors, one per row.
#' @keywords internal
#' @noRd
#' @export
apply_rules_all <- function(df, rules, sets = list()) {
  hits <- lapply(rules, function(r) source_match(df, r, sets))
  ids  <- vapply(rules, `[[`, character(1), "id")
  lapply(seq_len(nrow(df)), function(i) {
    k <- vapply(hits, `[`, logical(1), i)
    if (any(k)) ids[k] else character(0)
  })
}

#' Tag strata with their raise and donor groups (manual method)
#'
#' Adds the group membership a set of rules implies: `toBeRaisedGroup` (the
#' one group that raises each stratum, first match wins) and `usedToRaiseGroup`
#' (every group the stratum donates to, since donor pools may overlap).
#'
#' @param strata A classified stratum table.
#' @param rules A list of rules from [new_rule()].
#' @param sets Named value sets referenced by clauses.
#' @return `strata` with `toBeRaisedGroup`, `usedToRaiseGroup`,
#'   `usedToRaiseGroups_txt` and `n_donates_to` added.
#' @seealso [new_rule()], [conds_from_rules()], [rule_group_summary()]
#' @export
assign_rule_groups <- function(strata, rules, sets = list()) {
  strata$toBeRaisedGroup <- apply_rules_first(strata, rules, sets)
  used <- apply_rules_all(strata, rules, sets)
  strata$usedToRaiseGroup <- used
  strata$usedToRaiseGroups_txt <- vapply(used, paste, character(1), collapse = ", ")
  strata$n_donates_to <- lengths(used)
  strata
}

#' The strata each manual rule ACTUALLY raises
#'
#' A rule's raise clause says what it *claims*; what it raises is that minus
#' everything the rules above it already took. The distinction only becomes
#' visible on the catch-all - a rule with no clause claims every stratum in
#' the stock, and read literally would raise the ones earlier rules had
#' already raised, a second time.
#'
#' [conds_from_rules()] has always emitted the complements, so what
#' `catchRaising()` receives is correct. The reporting did not, which is
#' worse than it sounds: the table said the catch-all covered 340 strata and
#' 12,000 t when it covered 41 and 900, so the one number a user would check
#' the partition against was the one number that could not fail.
#'
#' Returns first-match-wins membership, computed the same way
#' [apply_rules_first()] computes it, so the table, the decision boxes, the
#' rivers and the emitted conditions cannot disagree.
#'
#' @param strata A classified stratum table.
#' @param rules A list of rules from [new_rule()].
#' @param sets Named value sets referenced by clauses.
#' @return A list of logical vectors over `strata`, one per rule, disjoint
#'   by construction.
#' @seealso [conds_from_rules()], [rule_group_summary()]
#' @export
rule_effective_sets <- function(strata, rules, sets = list()) {
  n <- if (is.null(strata)) 0L else nrow(strata)
  taken <- rep(FALSE, n)
  out <- vector("list", length(rules))
  for (i in seq_along(rules)) {
    claim <- if (n) eval_clauses(strata, rules[[i]]$raise, sets) else logical(0)
    mine <- claim & !taken
    out[[i]] <- mine
    taken <- taken | mine
  }
  names(out) <- vapply(rules, `[[`, character(1), "id")
  attr(out, "unassigned") <- !taken
  out
}

#' Per-group summary of a manual rule set
#'
#' One row per rule: the strata it raises and their landings, and its donor
#' pool's size, coverage and discard rate. Donor pools may overlap, so a
#' stratum can appear in several rows -- that is intended.
#'
#' @param strata A classified stratum table.
#' @param rules A list of rules from [new_rule()].
#' @param sets Named value sets referenced by clauses.
#' @return A data frame with the raise/source condition text and the pool
#'   statistics for each group.
#' @seealso [assign_rule_groups()]
#' @export
rule_group_summary <- function(strata, rules, sets = list()) {
  # first-match-wins, so the counts match what catchRaising() will do
  eff <- rule_effective_sets(strata, rules, sets)
  purrr::map_dfr(seq_along(rules), function(i) {
    r <- rules[[i]]
    src <- strata[source_match(strata, r, sets), , drop = FALSE]
    tgt <- strata[eff[[i]], , drop = FALSE]
    # what the rule asked for, before the rules above it took their share.
    # Shown beside the effective count so a catch-all reads as "claims
    # everything, raises what is left" rather than as a contradiction.
    claimed <- sum(eval_clauses(strata, r$raise, sets))
    data.frame(
      group_id        = r$id,
      label           = r$label,
      strataCond      = cond_string(r$raise),
      matchedDataCond = source_cond_string(r),
      n_strata_claimed = claimed,
      n_strata_raised = nrow(tgt),
      landings_raised = round(sum(tgt$landings, na.rm = TRUE), 1),
      n_strata_source = nrow(src),
      source_coverage = round(ifelse(sum(src$landings, na.rm = TRUE) > 0,
                                     sum(src$landings_with_discards, na.rm = TRUE) /
                                       sum(src$landings, na.rm = TRUE) * 100, NA), 1),
      source_DR = round(ifelse(sum(src$landings_with_discards, na.rm = TRUE) +
                                 sum(src$discards, na.rm = TRUE) > 0,
                               sum(src$discards, na.rm = TRUE) /
                                 (sum(src$landings_with_discards, na.rm = TRUE) +
                                    sum(src$discards, na.rm = TRUE)), NA), 3),
      stringsAsFactors = FALSE)
  })
}

# ----------------------------------------------------------
#  Conditions for raising (catchRaising / catchAllocationAoL)
# ----------------------------------------------------------

#' Raising conditions from an automatic recommendation
#'
#' Turns the recommendation into the condition lists catchRaising() and
#' catchAllocationAoL() expect: one quosure per group for the strata to raise
#' (`strataCond`) and one for the data to raise them with (`matchedDataCond`).
#' A fully sampled group points its donor condition at its own strata, so the
#' ratio table stays computable.
#'
#' @param rec Output of [recommend_raising()].
#' @return A list with `strataCond`, `matchedDataCond` (named lists of
#'   quosures) and `groups` (the [raising_groups()] table).
#' @seealso [recommend_raising()], [define_raising_groups_auto()]
#' @export
conds_from_auto <- function(rec) {
  g <- raising_groups(rec)
  sid <- split(rec$stratum_id, match(rec$raise_condition, g$raise_condition))
  strata <- lapply(seq_len(nrow(g)), function(i)
    rlang::new_quosure(rlang::parse_expr(
      sprintf("stratum_id %%in%% %s", fmt_vals(sid[[as.character(i)]]))),
      env = globalenv()))
  matched <- lapply(seq_len(nrow(g)), function(i) {
    cn <- g$raise_condition[i]
    # A fully sampled group needs no raising, but pointing its donor
    # condition at FALSE would leave catchRaising with an empty ratio
    # table. Point it at its own strata instead: harmless, and the ratio
    # stays computable.
    txt <- if (cn == "(none needed)")
      sprintf("stratum_id %%in%% %s", fmt_vals(sid[[as.character(i)]])) else cn
    rlang::new_quosure(rlang::parse_expr(txt), env = globalenv())
  })
  names(strata) <- names(matched) <- g$group_id
  list(strataCond = strata, matchedDataCond = matched, groups = g)
}

#' Raising conditions from a manual rule set
#'
#' Turns the rules into the condition lists catchRaising() and
#' catchAllocationAoL() expect. First-match-wins is implicit in the app, but
#' those functions treat the strata conditions as a partition, so the
#' complements are made explicit here (each rule minus everything above it).
#'
#' @param rules A list of rules from [new_rule()].
#' @param sets Named value sets referenced by clauses.
#' @return A list with `strataCond` and `matchedDataCond`, named lists of
#'   quosures, one per rule.
#' @seealso [new_rule()], [define_raising_groups_manual()]
#' @export
conds_from_rules <- function(rules, sets = list()) {
  ids <- vapply(rules, `[[`, character(1), "id")
  mk <- function(txt) rlang::new_quosure(rlang::parse_expr(txt), env = globalenv())
  # first-match-wins is implicit in the app, but catchRaising treats the
  # strata conditions as a partition, so the complements are made explicit
  seen <- character(0)
  strata <- vector("list", length(rules))
  for (i in seq_along(rules)) {
    this <- cond_string(rules[[i]]$raise)
    txt <- if (!length(seen)) this
           else if (this == "TRUE") paste0("!(", paste(seen, collapse = " | "), ")")
           else paste0("(", this, ") & !(", paste(seen, collapse = " | "), ")")
    strata[[i]] <- mk(txt)
    seen <- c(seen, paste0("(", this, ")"))
  }
  matched <- lapply(rules, function(r) mk(source_cond_string(r)))
  names(strata) <- names(matched) <- ids
  list(strataCond = strata, matchedDataCond = matched)
}

# ----------------------------------------------------------
#  Convenience entry points (auto and manual, one call each)
# ----------------------------------------------------------

#' Define automatic raising groups in one call
#'
#' Convenience wrapper for the automatic method: recommend a donor pool for
#' every stratum and return the raising-ready conditions in one step. The
#' `strataCond` / `matchedDataCond` in the result go straight into
#' catchRaising() (discards) or catchAllocationAoL() (lengths and ages).
#'
#' @param strata A classified stratum table from [classify_strata()].
#' @param hierarchy Key columns, finest first.
#' @param thresholds One threshold, or one per hierarchy level.
#' @param self_threshold Own-stratum threshold; `NA` to fold into step 1.
#' @param dims The full stratum dimensions; defaults to `hierarchy`.
#' @param all_label Label for the fallback pool.
#' @return A list with `recommendation`, `groups`, `strataCond` and
#'   `matchedDataCond`.
#' @seealso [recommend_raising()], [conds_from_auto()]
#' @examples
#' \dontrun{
#' sc <- classify_strata(build_strata(catch, dims), 50)
#' g  <- define_raising_groups_auto(sc, c("metier4", "Country"), 50)
#' # g$strataCond / g$matchedDataCond -> catchRaising()
#' }
#' @export
define_raising_groups_auto <- function(strata, hierarchy, thresholds,
                                       self_threshold = NA, dims = NULL,
                                       all_label = "ALL") {
  rec   <- recommend_raising(strata, hierarchy, thresholds,
                             self_threshold = self_threshold,
                             all_label = all_label, dims = dims)
  conds <- conds_from_auto(rec)
  list(recommendation  = rec,
       groups          = conds$groups,
       strataCond      = conds$strataCond,
       matchedDataCond = conds$matchedDataCond)
}

#' Define manual raising groups in one call
#'
#' Convenience wrapper for the manual method: tag the strata with their
#' groups, summarise the rule set, and return the raising-ready conditions in
#' one step. The `strataCond` / `matchedDataCond` in the result go straight
#' into catchRaising() or catchAllocationAoL().
#'
#' @param strata A classified stratum table from [classify_strata()].
#' @param rules A list of rules from [new_rule()].
#' @param sets Named value sets referenced by clauses.
#' @return A list with `strata` (tagged), `summary`, `strataCond` and
#'   `matchedDataCond`.
#' @seealso [assign_rule_groups()], [conds_from_rules()]
#' @examples
#' \dontrun{
#' rules <- list(new_rule("G1", raise = list(list(var = "metier4",
#'                                                 values = "TBB"))),
#'               new_rule("G2"))                 # catch-all
#' g <- define_raising_groups_manual(sc, rules)
#' }
#' @export
define_raising_groups_manual <- function(strata, rules, sets = list()) {
  assigned <- assign_rule_groups(strata, rules, sets)
  conds    <- conds_from_rules(rules, sets)
  list(strata          = assigned,
       summary         = rule_group_summary(strata, rules, sets),
       strataCond      = conds$strataCond,
       matchedDataCond = conds$matchedDataCond)
}

# ----------------------------------------------------------
#  Reporting on the cascade and the donor pools
# ----------------------------------------------------------

#' Cascade step table with running totals
#'
#' [decision_tree()] plus the share of landings each step resolved and the
#' running totals, so it is obvious how much of the stock is settled by the
#' time the cascade reaches the fallback.
#'
#' @param rec Output of [recommend_raising()].
#' @param hierarchy,thresholds,self_threshold,mode,dims As passed to
#'   [recommend_raising()].
#' @return A data frame, one row per step, with cumulative columns.
#' @seealso [decision_tree()], [check_partition()]
#' @export
decision_tree2 <- function(rec, hierarchy, thresholds, self_threshold,
                           mode = "nested", dims = NULL) {
  tr <- decision_tree(rec, hierarchy, thresholds, self_threshold, mode, dims)
  tot_n <- sum(tr$n_strata); tot_l <- sum(tr$landings)
  # NOTE: ifelse() with a scalar test returns a length-1 answer, which
  # then recycles the first row over every row. Use plain if/else here.
  pc <- function(x) if (tot_l > 0) round(x / tot_l * 100, 1) else rep(0, length(x))
  tr$pct_strata       <- round(tr$n_strata / tot_n * 100, 1)
  tr$pct_landings     <- pc(tr$landings)
  tr$cum_pct_strata   <- round(cumsum(tr$n_strata) / tot_n * 100, 1)
  tr$cum_pct_landings <- pc(cumsum(tr$landings))
  tr
}

#' Everything currently kept out of the donor pools
#'
#' Lists the strata excluded from donating and WHY, from both routes at once:
#' the discard-rate window and the named-stratum list. Reading them in one place
#' is the point - a stratum can vanish from a pool for either reason, and
#' otherwise it is not obvious which.
#'
#' @param strata A stratum table.
#' @param dims The stratum key columns.
#' @param excluded Joined stratum keys excluded by name.
#' @param dr_min,dr_max The discard-rate window.
#' @return A data frame of excluded strata with a `reason` column, or `NULL`
#'   when nothing is excluded.
#' @seealso [flag_donor_exclusion()], [apply_donor_exclusion()]
#' @export
excluded_overview <- function(strata, dims, excluded = character(0),
                              dr_min = NA, dr_max = NA) {
  if (is.null(strata) || !nrow(strata)) return(NULL)
  d <- as.data.frame(strata)
  present <- intersect(dims, names(d))
  if (!length(present)) return(NULL)
  key <- do.call(paste, c(lapply(present, function(k) as.character(d[[k]])),
                          sep = " | "))
  rate <- if ("discard_rate" %in% names(d)) d$discard_rate else NA_real_
  by_name <- key %in% excluded
  by_rate <- rep(FALSE, nrow(d))
  if (!is.na(dr_min)) by_rate <- by_rate | (!is.na(rate) & rate < dr_min)
  if (!is.na(dr_max)) by_rate <- by_rate | (!is.na(rate) & rate > dr_max)
  sel <- by_name | by_rate
  if (!any(sel)) return(NULL)
  out <- data.frame(
    stratum = key[sel],
    discard_rate = round(rate[sel], 4),
    landings = if ("landings" %in% names(d)) round(d$landings[sel], 3) else NA_real_,
    reason = ifelse(by_name[sel] & by_rate[sel], "named + rate window",
             ifelse(by_name[sel], "named", "outside rate window")),
    stringsAsFactors = FALSE)
  out[order(-out$landings), , drop = FALSE]
}

# ----------------------------------------------------------
#  Manual rules: the same decision view as the cascade
# ----------------------------------------------------------

#' Rule-by-rule decision table for a manual rule set
#'
#' The manual tab reported one flat table and left the reader to work out
#' whether the rules partitioned the stock. The hierarchical tab has had a
#' rule-by-rule breakdown all along, and the question is the same on both:
#' what does this rule take, how much of the stock is that, and how much is
#' still unresolved after it. So it is answered the same way.
#'
#' `n_strata` and `landings` are the EFFECTIVE figures - what the rule takes
#' after the rules above it have taken theirs - because that is what
#' `catchRaising()` will do. `n_claimed` is what the rule's clause would
#' select on its own; the two differ exactly where rules overlap, and the
#' gap is the thing worth seeing. On a catch-all the gap is the whole point:
#' it claims everything and raises what is left.
#'
#' A final `(unassigned)` row appears only when the rules do not cover the
#' stock. It should normally be absent, and it is a warning when it is not:
#' those strata reach `catchRaising()` in no group at all.
#'
#' @param strata A classified stratum table.
#' @param rules A list of rules from [new_rule()].
#' @param sets Named value sets referenced by clauses.
#' @return A data frame with one row per rule: `rule`, `label`, `raises`,
#'   `donors`, `n_claimed`, `n_strata`, `landings`, `pct_strata`,
#'   `pct_landings`, `cum_pct_strata`, `cum_pct_landings`, `n_donors`,
#'   `donor_coverage`, `donor_DR`.
#' @seealso [rule_effective_sets()], [decision_tree2()], [rule_group_summary()]
#' @export
rule_decision_table <- function(strata, rules, sets = list()) {
  if (is.null(strata) || !nrow(strata) || !length(rules))
    return(NULL)
  eff <- rule_effective_sets(strata, rules, sets)
  tot_n <- nrow(strata)
  tot_l <- sum(strata$landings, na.rm = TRUE)
  pc <- function(x) if (tot_l > 0) round(x / tot_l * 100, 1) else rep(0, length(x))

  rows <- lapply(seq_along(rules), function(i) {
    r <- rules[[i]]
    tgt <- strata[eff[[i]], , drop = FALSE]
    src <- strata[source_match(strata, r, sets), , drop = FALSE]
    sl  <- sum(src$landings_with_discards, na.rm = TRUE)
    sd_ <- sum(src$discards, na.rm = TRUE)
    data.frame(
      rule = r$id,
      label = r$label %||% r$id,
      raises = cond_string(r$raise),
      donors = source_cond_string(r),
      n_claimed = sum(eval_clauses(strata, r$raise, sets)),
      n_strata = nrow(tgt),
      landings = sum(tgt$landings, na.rm = TRUE),
      n_donors = nrow(src),
      donor_coverage = round(if (sum(src$landings, na.rm = TRUE) > 0)
        sl / sum(src$landings, na.rm = TRUE) * 100 else NA_real_, 1),
      donor_DR = round(if (sl + sd_ > 0) sd_ / (sl + sd_) else NA_real_, 3),
      stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)

  un <- attr(eff, "unassigned")
  if (any(un)) {
    rest <- strata[un, , drop = FALSE]
    out <- rbind(out, data.frame(
      rule = "(unassigned)", label = "Not covered by any rule",
      raises = "-", donors = "-",
      n_claimed = NA_integer_, n_strata = nrow(rest),
      landings = sum(rest$landings, na.rm = TRUE),
      n_donors = NA_integer_, donor_coverage = NA_real_, donor_DR = NA_real_,
      stringsAsFactors = FALSE))
  }

  out$pct_strata       <- round(out$n_strata / tot_n * 100, 1)
  out$pct_landings     <- pc(out$landings)
  out$cum_pct_strata   <- round(cumsum(out$n_strata) / tot_n * 100, 1)
  out$cum_pct_landings <- pc(cumsum(out$landings))
  out$landings         <- round(out$landings, 3)
  out
}

#' Tag every stratum with the rule that actually raises it
#'
#' The shape [plot_river_level()] wants: one row per stratum carrying
#' `raise_step` (the rule id), `group_id` (the same, for colour) and
#' `raise_condition` / `raise_level` so [rule_label()] can describe it.
#' Building it here rather than in the app keeps the rivers, the decision
#' table and the emitted conditions reading from one definition of which
#' rule owns which stratum.
#'
#' @param strata A classified stratum table.
#' @param rules A list of rules from [new_rule()].
#' @param sets Named value sets referenced by clauses.
#' @return `strata` with `raise_step`, `group_id`, `raise_condition` and
#'   `raise_level` columns; unassigned strata get `NA`.
#' @seealso [rule_decision_table()], [plot_river_level()]
#' @export
rule_river_data <- function(strata, rules, sets = list()) {
  if (is.null(strata) || !nrow(strata)) return(strata)
  eff <- rule_effective_sets(strata, rules, sets)
  step <- rep(NA_character_, nrow(strata))
  cond <- rep(NA_character_, nrow(strata))
  for (i in seq_along(rules)) {
    step[eff[[i]]] <- rules[[i]]$id
    cond[eff[[i]]] <- source_cond_string(rules[[i]])
  }
  strata$raise_step <- step
  strata$group_id <- step
  strata$raise_condition <- cond
  # rule_label() switches on this; "manual rule" keeps it out of the
  # special-case branches meant for the cascade's own-stratum and
  # no-raising-needed levels
  strata$raise_level <- ifelse(is.na(step), NA_character_, "manual rule")
  strata
}

# ----------------------------------------------------------
#  Manual discard rules: what is claimed, and what is not
# ----------------------------------------------------------
# The biology manual builder has had these two tables since it was
# written; the discard manual builder never did. Same question, same
# shape, so the two panels read the same way: one row per rule in the
# order it is tested, the running total, and the remainder on the last
# row rather than left to be inferred from a gap.

#' What each manual discard rule claims, and what nothing has claimed
#'
#' First-match-wins, so a rule receives its clause minus everything above
#' it. The cumulative columns stop before the last row: `STILL MISSING`
#' is the remainder, not another step, and running it into the total
#' would read as though the gap had been closed.
#'
#' @param strata A stratum table.
#' @param rules A list of rules, in evaluation order.
#' @param sets Named value sets.
#' @param weight_col Column to total. Defaults to `landings`.
#' @return A data frame, one row per rule plus a `STILL MISSING` row.
#' @seealso [rules_unassigned()], [assign_rule_groups()]
#' @export
rules_progress <- function(strata, rules, sets = list(),
                           weight_col = "landings") {
  if (is.null(strata) || !nrow(strata) || !length(rules)) return(NULL)
  gid <- apply_rules_first(as.data.frame(strata), rules, sets)
  w <- if (weight_col %in% names(strata)) as.numeric(strata[[weight_col]])
       else rep(1, nrow(strata))
  w[is.na(w)] <- 0
  tot_w <- sum(w); tot_n <- nrow(strata)

  rows <- lapply(seq_along(rules), function(i) {
    r <- rules[[i]]
    sel <- which(gid == r$id)
    data.frame(order = i, group = r$id, label = r$label %||% "",
               condition = cond_string(r$raise),
               n_strata = length(sel),
               weight = sum(w[sel], na.rm = TRUE),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  miss <- which(is.na(gid))
  out <- rbind(out, data.frame(order = NA_integer_, group = "STILL MISSING",
    label = "", condition = "-", n_strata = length(miss),
    weight = sum(w[miss], na.rm = TRUE), stringsAsFactors = FALSE))

  out$pct_strata <- round(out$n_strata / tot_n * 100, 1)
  out$pct_weight <- if (tot_w > 0) round(out$weight / tot_w * 100, 1) else NA_real_
  claimed <- out$group != "STILL MISSING"
  out$cum_pct_strata <- NA_real_; out$cum_pct_weight <- NA_real_
  out$cum_pct_strata[claimed] <-
    round(cumsum(out$n_strata[claimed]) / tot_n * 100, 1)
  if (tot_w > 0) out$cum_pct_weight[claimed] <-
    round(cumsum(out$weight[claimed]) / tot_w * 100, 1)
  out$weight <- round(out$weight, 3)
  out
}

#' The strata no manual discard rule has claimed
#'
#' Heaviest first, so the biggest hole is on top and the next rule to
#' write is obvious.
#'
#' @inheritParams rules_progress
#' @param dims Key columns to show.
#' @return A data frame of unclaimed strata, or `NULL` if none.
#' @seealso [rules_progress()]
#' @export
rules_unassigned <- function(strata, rules, sets = list(), dims = NULL,
                             weight_col = "landings") {
  if (is.null(strata) || !nrow(strata)) return(NULL)
  d <- as.data.frame(strata)
  gid <- if (length(rules)) apply_rules_first(d, rules, sets)
         else rep(NA_character_, nrow(d))
  un <- d[is.na(gid), , drop = FALSE]
  if (!nrow(un)) return(NULL)
  keep <- intersect(c(dims, weight_col, "coverage", "discard_rate"), names(un))
  out <- un[, keep, drop = FALSE]
  if (weight_col %in% names(out))
    out <- out[order(-out[[weight_col]]), , drop = FALSE]
  num <- vapply(out, is.numeric, logical(1))
  out[num] <- lapply(out[num], function(x) round(x, 3))
  rownames(out) <- NULL
  out
}
