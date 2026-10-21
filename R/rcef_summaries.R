# =====================================================================
#  rcef_summaries.R   (raised catches: the overview table and the reading of c(a, b))
#
#  ORIGIN: this project (ILVO / RCEF Workbench). Freely refactorable.
#  Was: R_development/summaries.R
# =====================================================================

# ============================================================
#  Overview tables and plots for raised output.
#  All weights in tonnes (CEF WeightLive is kg).
# ============================================================
#' The raised catches handed to a comparison function, with their names
#'
#' [raised_overview_table()] and [plot_compare_raised()] take their raised catches
#' written `list(a, b)` or, the way it is most naturally typed, `c(a, b)` -
#' which for data frames would otherwise flatten into one list of columns. The
#' call is therefore read before it is evaluated: each element is evaluated on
#' its own in the caller's frame, and one without a name of its own is called
#' what the object is called.
#'
#' @param expr The unevaluated argument, from `substitute()`.
#' @param value A function returning the evaluated argument; only called when
#'   `expr` is not a `c()` / `list()` call, so a `c()` of data frames is never
#'   evaluated as such.
#' @param env The caller's frame, where the elements are evaluated.
#' @param scenarios_names Optional names, one per raised catch, which win over
#'   any other.
#' @param fn The calling function, for error messages.
#' @param min The fewest raised catches the caller accepts.
#' @return A list: `data`, a named list of raised catches; `single`, `TRUE`
#'   when the argument was one bare data frame; and `factor`, a function
#'   `factor(x, var = NULL)` that turns the values `x` of one category column
#'   of the raised catches into a factor in a sensible order. The levels are the
#'   union of what the column `var` holds in any of the raised catches, as
#'   strings: by size for `mesh` and `metier4_mesh` ([mesh_var_levels()]),
#'   numerically for a numeric column (`seasonValue`: 2 before 10), in
#'   factor-level order for a factor, alphabetically for anything else. Values
#'   missing from them are dropped from the levels; a missing value becomes its
#'   own last level, `"NA"`, rather than silently falling out of a plot or a
#'   table. `var = NULL` is no column: one level, `"All"`.
#' @keywords internal
#' @noRd
#' @export
scenario_list <- function(expr, value, env, scenarios_names = NULL,
                          fn = "plot_compare_raised()", min = 2L) {
  listing <- is.call(expr) && is.name(expr[[1]]) &&
    as.character(expr[[1]]) %in% c("c", "list")
  single <- FALSE
  if (listing) {
    args <- as.list(expr)[-1]
    vals <- lapply(args, eval, envir = env)
    tags <- names(args)
    if (is.null(tags)) tags <- rep("", length(args))
    nms <- ifelse(nzchar(tags), tags,
                  vapply(args, function(a) rlang::as_label(a), ""))
  } else {
    val <- value()
    if (is.data.frame(val)) {
      vals <- list(val); nms <- rlang::as_label(expr); single <- TRUE
    } else {
      vals <- as.list(val)
      nms <- names(vals)
      if (is.null(nms)) nms <- rep("", length(vals))
      nms[!nzchar(nms)] <- paste("scenario", seq_along(vals))[!nzchar(nms)]
    }
  }
  if (!length(vals))
    stop(fn, ": no raised catches given - pass the output of catchRaising(), ",
         "written list(a, b) or c(a, b)", call. = FALSE)
  if (!is.null(scenarios_names)) {
    if (length(scenarios_names) != length(vals))
      stop(fn, ": `scenarios_names` has ", length(scenarios_names),
           " name(s) for ", length(vals), " raised catch(es)", call. = FALSE)
    nms <- as.character(scenarios_names)
    if (anyDuplicated(nms))
      stop(fn, ": `scenarios_names` must all be different", call. = FALSE)
  }
  nms <- make.unique(nms)
  need <- c("catchCategory", "total", "importedOrRaised")
  for (i in seq_along(vals)) {
    if (!is.data.frame(vals[[i]]))
      stop(fn, ": '", nms[i], "' is not a raised catch (a data frame) - write ",
           "list(a, b) or c(a, b) directly in the call, not a c() of them ",
           "stored beforehand", call. = FALSE)
    miss <- setdiff(need, names(vals[[i]]))
    if (length(miss))
      stop(fn, ": '", nms[i], "' is not a raised catch - it has no ",
           paste0("'", miss, "'", collapse = ", "),
           " column (the output of catchRaising() does)", call. = FALSE)
  }
  if (length(vals) < min)
    stop(fn, ": compares two or more raised catches, got ", length(vals),
         call. = FALSE)
  names(vals) <- nms
  list(data = vals, single = single,
       factor = function(x, var = NULL) {
         levels <- "All"
         if (!is.null(var)) {
           cols <- lapply(vals, function(r) r[[var]])
           levels <- mesh_var_levels(var, unlist(lapply(cols, as.character)))
           if (is.null(levels))
             levels <- if (all(vapply(cols, is.numeric, NA)))
                         as.character(sort(unique(unlist(cols))))
                       else if (all(vapply(cols, is.factor, NA)))
                         unique(unlist(lapply(cols, levels)))
                       else sort(unique(as.character(unlist(cols))))
         }
         x <- as.character(x)
         if (anyNA(x)) {
           x[is.na(x)] <- "NA"
           levels <- c(levels, "NA")
         }
         factor(x, levels = unique(levels[levels %in% x]))
       })
}

#' Imported against raised catch, by catch category: one raised catch or several
#'
#' @description
#' Each catch category's weight in tonnes - how much of it came with the data
#' (imported) and how much the raising added (raised) - and each part as a
#' share of its category, closed by a Total row. Give it two or more raised
#' catches, say two ways of defining the groups, and they are laid out side by
#' side, scenario by scenario. Give it `by` and every figure is split further
#' by those columns - a season, a metier, an area, the raising group - each
#' split with its own Total.
#'
#' @details
#' The raised catches are written `list(a, b)` or `c(a, b)`; one without a name
#' of its own is called what its object is called, and `scenarios_names`
#' overrides either. A single raised catch needs no list and comes back without
#' the `Scenario` column.
#'
#' Within each scenario and `by` split the rows are catch category, then
#' imported before raised, and the scenarios of one split sit next to each
#' other so the same figure can be read across them. `Percent of category` is
#' the share of a part within its category in that split, so imported and
#' raised add up to 100.
#'
#' A raised row whose group had no donor to draw a rate from comes out of the
#' raising as `NaN`: it raises to nothing and counts as no weight. The
#' `No donor (rows)` column, shown only when there are any, counts them where
#' they fall - by raising group, say - so they are not mistaken for a group
#' that simply raised little.
#'
#' @param raised_catch_scenarios One raised catch (the output of
#'   [catchRaising()]), or two or more written `list(a, b)` or `c(a, b)`.
#' @param scenarios_names Optional names for the scenarios, in the same order.
#'   Default: the names given inside `c()` / `list()`, or else the object
#'   names.
#' @param by Optional columns to split every figure by, in addition to the
#'   catch category and imported / raised: `"seasonValue"`,
#'   `c("metier4", "areaValue")`, `"DrGroup"`. Every raised catch must have
#'   them.
#' @return A tibble with `Scenario` (for several raised catches), `Catch
#'   category`, the `by` columns, `Raised/Imported`, `Caton (t)`, `Percent of
#'   category` and, when any, `No donor (rows)`.
#' @seealso [plot_compare_raised()] for the same comparison as a figure;
#'   [plot_catches()] for one raised catch.
#' @md
#' @examples
#' \dontrun{
#' cef <- subset(catch, year == 2023)
#' log <- tempfile(fileext = ".txt")
#'
#' # the groups, and the data each takes its discard ratio from: its own, or everything
#' strataCond <- list(TBB  = rlang::quo(metier4 == "TBB"),
#'                    REST = rlang::quo(metier4 != "TBB"))
#' own <- strataCond
#' all <- list(TBB = rlang::quo(TRUE), REST = rlang::quo(TRUE))
#' raised_own <- catchRaising(cef, strataCond, own, logFile = log)
#' raised_all <- catchRaising(cef, strataCond, all, logFile = log)
#'
#' # one raised catch
#' raised_overview_table(raised_own)
#'
#' # two, side by side, named after the objects ...
#' raised_overview_table(c(raised_own, raised_all))
#'
#' # ... or after you, split by season and metier
#' raised_overview_table(c(own = raised_own, everything = raised_all),
#'                       by = c("seasonValue", "metier4"))
#' }
#' @export
raised_overview_table <- function(raised_catch_scenarios, scenarios_names = NULL,
                                  by = NULL) {
  sc <- scenario_list(substitute(raised_catch_scenarios),
                      function() raised_catch_scenarios, parent.frame(),
                      scenarios_names, fn = "raised_overview_table()", min = 1L)
  by <- setdiff(unique(as.character(by[!is.na(by) & nzchar(by)])), "catchCategory")
  for (nm in names(sc$data)) {
    miss <- setdiff(by, names(sc$data[[nm]]))
    if (length(miss))
      stop("raised_overview_table(): '", nm, "' has no column ",
           paste0("'", miss, "'", collapse = ", "), call. = FALSE)
  }

  d <- do.call(rbind, lapply(names(sc$data), function(nm) {
    r <- sc$data[[nm]]
    u <- if ("variableUnit" %in% names(r)) r$variableUnit else NULL
    raised <- r$importedOrRaised %in% "raised"
    out <- data.frame(scenario = rep(nm, nrow(r)),
                      catchCategory = as.character(r$catchCategory),
                      src = ifelse(raised, "Raised", "Imported"),
                      .t = convert_units(r$total, u, "t"),
                      .no_donor = raised & is.nan(r$total),
                      stringsAsFactors = FALSE)
    for (v in by) out[[v]] <- as.character(r[[v]])
    out
  }))
  d$scenario <- factor(d$scenario, levels = names(sc$data))
  for (v in by) d[[v]] <- sc$factor(d[[v]], v)

  cell <- c("scenario", by)
  agg <- d %>%
    group_by(across(all_of(c(cell, "catchCategory", "src")))) %>%
    summarise(tonnes = sum(.t, na.rm = TRUE), no_donor = sum(.no_donor),
              .groups = "drop")
  tot_cat <- agg %>%
    group_by(across(all_of(c(cell, "catchCategory")))) %>%
    summarise(cat_total = sum(tonnes), .groups = "drop")
  out <- agg %>%
    left_join(tot_cat, by = c(cell, "catchCategory")) %>%
    mutate(percent = ifelse(cat_total > 0, round(tonnes / cat_total * 100), NA_real_)) %>%
    select(-cat_total)
  tot <- agg %>%
    group_by(across(all_of(cell))) %>%
    summarise(tonnes = sum(tonnes), no_donor = sum(no_donor), .groups = "drop") %>%
    mutate(catchCategory = "Total", src = "-", percent = 100)
  res <- bind_rows(out, tot) %>%
    arrange(!!!rlang::syms(c(by, "catchCategory", "src", "scenario")))
  for (v in c("scenario", by)) res[[v]] <- as.character(res[[v]])

  res <- res %>%
    select(Scenario = scenario, `Catch category` = catchCategory, all_of(by),
           `Raised/Imported` = src, `Caton (t)` = tonnes,
           `Percent of category` = percent, `No donor (rows)` = no_donor) %>%
    mutate(`Caton (t)` = round(`Caton (t)`, 3))
  if (sc$single) res$Scenario <- NULL
  if (!any(res$`No donor (rows)` > 0)) res$`No donor (rows)` <- NULL
  res
}

