# ============================================================
#  mod_length.R
#  Length (and age) sampling: link the catch table to the
#  distributions table through domainBiology, then explore what
#  was measured.
#
#  The two variables in the distributions table are NOT the same
#  kind of quantity and must never be mixed:
#
#    variableType = "Number"     valueType = "Total"  -> numbers at length
#    variableType = "WeightLive" valueType = "Mean"   -> MEAN weight at length
#
#  Numbers add up. Mean weights do not: pooling two strata means
#  taking a mean weighted by their numbers, which is what
#  length_meanweight() does. A plain average would silently
#  over-weight a stratum that measured five fish.
# ============================================================

# ------------------------------------------------------------
#  Units
# ------------------------------------------------------------
# The exchange format lets each row carry its own unit, so a file can
# mix mm and cm, or g and kg. Everything is converted once, on the way
# in, to mm / N / g. Anything unrecognised is left alone and reported,
# rather than being silently treated as the base unit.
LEN_CLASS_TO_MM <- c(mm = 1, cm = 10, dm = 100, m = 1000)
NUMBER_TO_N     <- c(N = 1, n = 1, thousands = 1000, kN = 1000,
                     millions = 1e6, MN = 1e6,
                     NE3 = 1e3, NE6 = 1e6, NE9 = 1e9)
WEIGHT_TO_G     <- c(g = 1, kg = 1000, t = 1e6, mg = 0.001)

unit_factor <- function(units, table_, what) {
  u <- as.character(units)
  f <- unname(table_[u])
  bad <- unique(u[is.na(f) & !is.na(u)])
  if (length(bad))
    warning(sprintf("%s: unrecognised unit(s) %s - left unconverted",
                    what, paste(bad, collapse = ", ")), call. = FALSE)
  f[is.na(f)] <- 1
  f
}

# Normalise a distributions table. Everything is converted once, on the
# way in, to the PRESENTATION units the app uses everywhere: length class
# in cm and mean weight in g. A file may mix mm and cm, or g and kg, and
# a stratum measured in mm must never end up on the same axis as one in
# cm - so this is not offered as a choice, it is enforced here.
# .class / .value_std are the only fields the rest of the app reads.
prep_distributions <- function(dist, type = "LengthTotal") {
  if (is.null(dist) || !nrow(dist)) return(NULL)
  d <- as.data.frame(dist, stringsAsFactors = FALSE)
  need <- c("domainBiology", "distributionType", "distributionClass",
            "variableType", "value")
  miss <- setdiff(need, names(d))
  if (length(miss))
    stop("distributions table is missing: ", paste(miss, collapse = ", "))

  d <- d[d$distributionType %in% type, , drop = FALSE]
  if (!nrow(d)) return(NULL)

  cls_u <- if ("distributionUnit" %in% names(d)) d$distributionUnit else "mm"
  var_u <- if ("variableUnit" %in% names(d)) d$variableUnit else NA
  # LengthTotal: convert whatever unit the row carries to mm, then show
  # in cm. Age has no unit to convert - an age is an age - so the class
  # is taken as it stands. Both land in .class, which is the only column
  # the rest of the module reads.
  raw <- suppressWarnings(as.numeric(d$distributionClass))
  if (identical(type, "Age")) {
    d$.class_mm <- NA_real_
    d$.class <- raw
  } else {
    d$.class_mm <- raw * unit_factor(cls_u, LEN_CLASS_TO_MM, "distributionClass")
    d$.class <- d$.class_mm / 10
  }
  d$.dist_type <- type[1]

  v <- suppressWarnings(as.numeric(d$value))
  is_n <- d$variableType %in% "Number"
  d$.value_std <- NA_real_
  d$.value_std[is_n]  <- v[is_n]  * unit_factor(var_u[is_n], NUMBER_TO_N, "Number")
  d$.value_std[!is_n] <- v[!is_n] * unit_factor(var_u[!is_n], WEIGHT_TO_G, "WeightLive")

  # what the row actually is, so callers never have to re-derive it
  d$.var_kind <- ifelse(is_n, "numbers", "meanweight")
  d$.n_meas <- if ("numMeasurements" %in% names(d))
    suppressWarnings(as.numeric(d$numMeasurements)) else NA_real_
  d
}

# Which units the file arrived in, so the screen can say what was
# converted rather than silently changing the numbers.
distribution_units <- function(dist, type = "LengthTotal") {
  if (is.null(dist) || !nrow(dist)) return(NULL)
  d <- as.data.frame(dist)
  d <- d[d$distributionType %in% type, , drop = FALSE]
  if (!nrow(d)) return(NULL)
  cu <- sort(unique(as.character(d$distributionUnit)))
  wu <- sort(unique(as.character(d$variableUnit[d$variableType == "WeightLive"])))
  nu <- sort(unique(as.character(d$variableUnit[d$variableType == "Number"])))
  list(class = cu, weight = wu, number = nu,
       mixed = length(cu) > 1 || length(wu) > 1 || length(nu) > 1)
}

# ------------------------------------------------------------
#  Linking catches to biology
# ------------------------------------------------------------
# A catch stratum counts as measured only when its domainBiology is
# filled AND that key actually appears in the distributions table for
# the same catch category. A filled key that matches nothing is a
# dangling reference, and is reported separately - it is a data problem,
# not evidence of sampling.
biology_link <- function(catches, dist, dims, catch_category = NULL,
                         type = "LengthTotal") {
  d <- derive_fields(as.data.frame(catches))
  if (!is.null(catch_category) && length(catch_category) &&
      any(nzchar(catch_category)))
    d <- d[d$catchCategory %in% catch_category, , drop = FALSE]
  if (!nrow(d)) return(NULL)

  u <- if ("variableUnit" %in% names(d)) d$variableUnit else NULL
  d$.t <- to_tonnes(d$total, u)

  db <- if ("domainBiology" %in% names(d)) as.character(d$domainBiology)
        else rep(NA_character_, nrow(d))
  d$.has_key <- !is.na(db) & nzchar(db)

  pd <- prep_distributions(dist, type)
  keys <- if (is.null(pd)) character(0)
          else unique(paste(pd$domainBiology, pd$catchCategory))
  d$.measured <- d$.has_key &
    paste(db, d$catchCategory) %in% keys
  d$.dangling <- d$.has_key & !d$.measured

  d %>%
    group_by(across(all_of(dims)), catchCategory) %>%
    summarise(weight            = sum(.t, na.rm = TRUE),
              weight_measured   = sum(.t[.measured], na.rm = TRUE),
              weight_unmeasured = sum(.t[!.measured], na.rm = TRUE),
              n_rows            = n(),
              n_measured        = sum(.measured),
              n_dangling        = sum(.dangling),
              .groups = "drop") %>%
    mutate(coverage = ifelse(weight > 0, weight_measured / weight * 100,
                             NA_real_),
           sampled  = n_measured > 0) %>%
    as.data.frame()
}

# Distributions whose domainBiology has NO matching catch row: a length or
# age distribution was submitted for a stratum that carries no catch with
# that domainBiology. That means the catch and biology files disagree, so
# it is flagged rather than silently ignored. Returns a data frame of the
# orphan domainBiology / catchCategory keys with the number of distribution
# rows each covers, or NULL when every distribution matches a catch.
dist_without_catch <- function(catches, dist, catch_category = NULL,
                               type = "LengthTotal") {
  pd <- prep_distributions(dist, type)
  if (is.null(pd) || !nrow(pd)) return(NULL)
  if (!is.null(catch_category) && length(catch_category) &&
      any(nzchar(catch_category)))
    pd <- pd[pd$catchCategory %in% catch_category, , drop = FALSE]
  if (!nrow(pd)) return(NULL)
  d <- derive_fields(as.data.frame(catches))
  db <- if ("domainBiology" %in% names(d)) as.character(d$domainBiology)
        else character(0)
  keep <- nzchar(db) & !is.na(db)
  catch_keys <- unique(paste(db[keep], d$catchCategory[keep]))
  pd$.key <- paste(pd$domainBiology, pd$catchCategory)
  orphan <- pd[!pd$.key %in% catch_keys, , drop = FALSE]
  if (!nrow(orphan)) return(NULL)
  orphan %>%
    group_by(domainBiology, catchCategory) %>%
    summarise(n_rows = dplyr::n(), .groups = "drop") %>%
    as.data.frame()
}

# The stratum keys carried by the catch table, attached to every
# distribution row through domainBiology. This is what lets a length
# distribution be plotted by fleet, country, area or season.
link_dist_to_strata <- function(catches, dist, dims, catch_category = NULL,
                                type = "LengthTotal") {
  pd <- prep_distributions(dist, type)
  if (is.null(pd)) return(NULL)
  d <- derive_fields(as.data.frame(catches))
  if (!"domainBiology" %in% names(d)) return(NULL)
  if (!is.null(catch_category) && length(catch_category) &&
      any(nzchar(catch_category))) {
    d  <- d[d$catchCategory %in% catch_category, , drop = FALSE]
    pd <- pd[pd$catchCategory %in% catch_category, , drop = FALSE]
  }
  if (!nrow(d) || !nrow(pd)) return(NULL)

  # one row per domain x category, carrying the stratum keys. A domain
  # can be shared by more than one country, so the keys are taken from
  # the catch rows and de-duplicated rather than assumed unique.
  keymap <- d %>%
    filter(!is.na(domainBiology), nzchar(as.character(domainBiology))) %>%
    select(domainBiology, catchCategory, all_of(dims)) %>%
    distinct()
  if (!nrow(keymap)) return(NULL)

  pd %>%
    inner_join(keymap, by = c("domainBiology", "catchCategory"),
               relationship = "many-to-many")
}

# ------------------------------------------------------------
#  The two quantities
# ------------------------------------------------------------
# Numbers at length: additive, so summing over whatever is not plotted
# is correct.
length_numbers <- function(linked, group_vars = character(0)) {
  if (is.null(linked) || !nrow(linked)) return(NULL)
  d <- linked[linked$.var_kind == "numbers", , drop = FALSE]
  if (!nrow(d)) return(NULL)
  keys <- unique(c(".class", group_vars))
  d %>% group_by(across(all_of(keys))) %>%
    summarise(number = sum(.value_std, na.rm = TRUE),
              n_meas = sum(.n_meas, na.rm = TRUE), .groups = "drop") %>%
    filter(number > 0) %>% as.data.frame()
}

# Mean weight at length: NOT additive. Pooling strata takes a mean
# weighted by the numbers measured at that length, so a stratum that
# measured five fish cannot outweigh one that measured five hundred.
# Falls back to numMeasurements, then to an unweighted mean, when no
# numbers are available.
#
# Because there is always some grouping, the spread across whatever was
# collapsed is carried too: sd_weight is the number-weighted standard
# deviation of the strata that went into each point, and n_strata says
# how many there were. A point built from one stratum has no spread, and
# gets NA rather than a misleading zero.
length_meanweight <- function(linked, group_vars = character(0)) {
  if (is.null(linked) || !nrow(linked)) return(NULL)
  mw <- linked[linked$.var_kind == "meanweight", , drop = FALSE]
  if (!nrow(mw)) return(NULL)
  nb <- linked[linked$.var_kind == "numbers", , drop = FALSE]

  join_keys <- intersect(c("domainBiology", "catchCategory", ".class",
                           "vesselFlagCountry"), names(mw))
  if (nrow(nb)) {
    nb2 <- nb %>% select(all_of(join_keys), .w_num = .value_std) %>%
      group_by(across(all_of(join_keys))) %>%
      summarise(.w_num = sum(.w_num, na.rm = TRUE), .groups = "drop")
    mw <- mw %>% left_join(nb2, by = join_keys)
  } else mw$.w_num <- NA_real_

  mw$.w <- ifelse(!is.na(mw$.w_num) & mw$.w_num > 0, mw$.w_num,
                  ifelse(!is.na(mw$.n_meas) & mw$.n_meas > 0, mw$.n_meas, 1))

  keys <- unique(c(".class", group_vars))
  mw %>% group_by(across(all_of(keys))) %>%
    summarise(mean_weight = sum(.value_std * .w, na.rm = TRUE) /
                            sum(.w, na.rm = TRUE),
              # number-weighted sd across the strata collapsed into this point
              sd_weight = {
                m <- sum(.value_std * .w, na.rm = TRUE) / sum(.w, na.rm = TRUE)
                sw <- sum(.w, na.rm = TRUE)
                v <- sum(.w * (.value_std - m)^2, na.rm = TRUE) / sw
                if (dplyr::n() > 1 && is.finite(v)) sqrt(v) else NA_real_
              },
              n_strata = dplyr::n(),
              n_meas = sum(.n_meas, na.rm = TRUE),
              weight_basis = sum(.w, na.rm = TRUE), .groups = "drop") %>%
    filter(is.finite(mean_weight)) %>% as.data.frame()
}




# Does the length scale agree with the weights?
#
# Fish weight goes roughly as length^3, so W = a * L_cm^b with b near 3
# and a of the order 0.01 g/cm^3 for most species. Fitting that on the
# log scale gives a quick check that the class column really is on the
# scale its unit claims: if every length is ten times too big, the fitted
# a comes out a thousand times too small, which is easy to see and hard
# to get by accident.
#
# This never rewrites the data - a unit column is the user's to fix. It
# reports what the numbers imply so the mismatch is not silently plotted.
# Only meaningful for lengths: an age has no weight-length relationship
# to check against.
length_scale_check <- function(linked) {
  if (is.null(linked) || !nrow(linked)) return(NULL)
  if (identical(linked$.dist_type[1], "Age")) return(NULL)
  mw <- linked[linked$.var_kind == "meanweight" &
               is.finite(linked$.class) & linked$.class > 0 &
               is.finite(linked$.value_std) & linked$.value_std > 0, ,
               drop = FALSE]
  if (nrow(mw) < 5) return(NULL)
  fit <- tryCatch(stats::lm(log(.value_std) ~ log(.class), data = mw),
                  error = function(e) NULL)
  if (is.null(fit)) return(NULL)
  b <- unname(stats::coef(fit)[2]); a <- exp(unname(stats::coef(fit)[1]))
  # a is in g per cm^b; for real fish it sits around 0.003 - 0.05
  implied <- if (!is.finite(a) || !is.finite(b)) NA_real_
             else (0.01 / a)^(1 / b)
  list(a = a, b = b,
       plausible = is.finite(a) && a > 0.0005 && a < 0.2 && b > 2.2 && b < 3.8,
       implied_factor = implied,
       median_cm = stats::median(mw$.class, na.rm = TRUE))
}





# A short summary of what the biology link found, for the screen.
length_link_summary <- function(bl, linked, type = "LengthTotal") {
  cls_word <- if (identical(type, "Age")) "age" else "length"
  cls_unit <- if (identical(type, "Age")) "years" else "cm"
  if (is.null(bl) || !nrow(bl))
    return(data.frame(item = "Catch rows", value = "0",
                      stringsAsFactors = FALSE))
  tot <- sum(bl$weight, na.rm = TRUE)
  meas <- sum(bl$weight_measured, na.rm = TRUE)
  cls <- if (!is.null(linked) && nrow(linked))
    range(linked$.class, na.rm = TRUE) else c(NA, NA)
  data.frame(
    item = c("Strata", paste("Strata with", cls_word, "data"),
             "Catch weight (t)",
             paste("Weight with", cls_word, "data (t)"), "Coverage (%)",
             paste(tools::toTitleCase(cls_word), "classes present"),
             paste0("Class range (", cls_unit, ")"),
             "Domains with a key but no distribution"),
    value = c(
      format(nrow(bl), big.mark = ","),
      format(sum(bl$sampled), big.mark = ","),
      fmt_t3(tot), fmt_t3(meas),
      if (tot > 0) round(meas / tot * 100, 1) else NA,
      if (!is.null(linked) && nrow(linked))
        length(unique(linked$.class)) else 0,
      if (all(is.finite(cls))) paste(cls, collapse = " - ") else "-",
      format(sum(bl$n_dangling, na.rm = TRUE), big.mark = ",")),
    stringsAsFactors = FALSE)
}

# ============================================================
#  Allocation groups for length / age
# ============================================================
# The cascade is the same idea as the discard one, with catchCategory
# sitting above every other key.
#
# keep_cc = TRUE  (the default): the ladder never drops the category, so
#   the widest pool a landings stratum can reach is "all other landings".
#   Landings are never allocated from discard samples.
# keep_cc = FALSE: once the other keys are exhausted the category is
#   dropped too, and whatever is still unresolved pools landings and
#   discards together.
alloc_steps <- function(keys, thresholds, keep_cc = TRUE) {
  keys <- keys[nzchar(keys)]
  thr <- function(i) if (length(thresholds) >= i) thresholds[i] else NA_real_
  out <- list()

  add <- function(id, label, drop, drop_cc, threshold)
    out[[length(out) + 1]] <<- list(id = id, label = label, drop = drop,
                                    drop_cc = drop_cc, threshold = threshold)

  # step 1 is always the stratum's own sampled data
  add("own", "own stratum", character(0), FALSE, thr(1))

  # --- pass 1: stay inside the catch category, widen one key at a time
  for (i in seq_along(keys)) {
    dropped <- rev(keys)[seq_len(i)]
    kept <- setdiff(keys, dropped)
    add(paste0("w", i),
        if (length(kept)) paste(c("catchCategory", kept), collapse = " + ")
        else "catchCategory (all other data in the same category)",
        dropped, FALSE, thr(i + 1))
  }

  # --- pass 2: only when dropping the category is allowed.
  #
  # Joining landings and discards is a big step, so it does NOT go
  # straight to pooling everything. The ladder restarts at the FINEST
  # precision with the categories joined - same fleet, country, season
  # and area, now drawing on both - and only widens from there, one key
  # at a time, exactly as pass 1 did. Pooling everything is the last
  # resort, not the first thing tried after the category is released.
  if (!keep_cc) {
    for (j in seq_along(c(0, keys)) - 1) {   # j = 0 .. length(keys)
      dropped <- if (j == 0) character(0) else rev(keys)[seq_len(j)]
      kept <- setdiff(keys, dropped)
      add(paste0("x", j),
          if (length(kept))
            paste0(paste(kept, collapse = " + "), "  (categories joined)")
          else "ALL data, both categories pooled",
          dropped, TRUE,
          # same key precision as pass 1, so the same threshold applies
          thr(j + 1))
    }
  }
  out
}

# Which pass a step belongs to, for the table and the chain.
alloc_step_scope <- function(step)
  if (isTRUE(step$drop_cc)) "categories joined" else "within category"

# The condition text for one group, as it will appear in the emitted
# allocStrataCond / bioMatchedCond lists.
alloc_cond_string <- function(vals, keep_cc = TRUE) {
  parts <- character(0)
  if (!is.null(vals$catchCategory) && length(vals$catchCategory))
    parts <- c(parts, if (length(vals$catchCategory) == 1)
      sprintf('catchCategory == "%s"', vals$catchCategory)
      else sprintf('catchCategory %%in%% c(%s)',
                   paste0('"', vals$catchCategory, '"', collapse = ", ")))
  for (k in setdiff(names(vals), "catchCategory")) {
    v <- vals[[k]]
    if (!length(v)) next
    neg <- isTRUE(attr(v, "negate"))
    v <- as.character(v)
    parts <- c(parts,
      if (length(v) == 1)
        sprintf('%s %s "%s"', k, if (neg) "!=" else "==", v)
      else sprintf('%s%s %%in%% c(%s)', if (neg) "!" else "", k,
                   paste0('"', v, '"', collapse = ", ")))
  }
  if (!length(parts)) "TRUE" else paste(parts, collapse = " & ")
}

# The same clause as an evaluable quosure, so the app can assign strata
# with it and the emitted script can run the identical expression.
alloc_quo <- function(vals) {
  txt <- alloc_cond_string(vals)
  rlang::new_quosure(str2lang(txt), env = rlang::global_env())
}

# Emit the two lists the allocation takes. Names follow the ICES script:
# allocStrataCond for the strata being allocated, bioMatchedCond for the
# sampled data each is matched to.
emit_alloc_conditions <- function(groups) {
  fmt <- function(nm, field) {
    if (!length(groups)) return(sprintf("%s <- list()", nm))
    body <- vapply(names(groups), function(g)
      sprintf("     %s = quo(%s)", g, groups[[g]][[field]]), character(1))
    paste0(nm, " <-\n    list(", sub("^     ", "", paste(body, collapse = ",\n")), ")")
  }
  c(fmt("allocStrataCond", "alloc"), "", fmt("bioMatchedCond", "matched"))
}

# The R the app would run to allocate lengths or ages, ready to paste
# into a script. Mirrors the ICES workflow exactly.
#' Emit the code that DEFINES the length/age allocation groups
#'
#' The allocation block on its own shows only the resolved conditions, which
#' says what the groups are but not how they were arrived at. This emits the
#' grouping step itself: classify the biology strata by coverage and
#' measurement count, then either walk the cascade (hierarchical) or apply the
#' saved rules (manual) to reach the same groups. Run before
#' `emit_alloc_code()`, it makes the length/age grouping reproducible outside
#' the app.
#'
#' @param spec The grouping spec stored with the allocation: `keys`,
#'   `thresholds`, `min_meas`, `keep_cc`, `cols`, `bms` and `dims`. `NULL`
#'   emits only a note (an allocation saved before specs were recorded).
#' @param method "hierarchical" or "manual".
#' @param tag "Length" or "Age", for the comments.
#' @return A character vector of R code lines.
#' @seealso `emit_alloc_code()`
#' @export
emit_alloc_grouping_code <- function(spec, method = "hierarchical",
                                     tag = "Length") {
  fmt <- function(v) {
    if (!length(v)) return("character(0)")
    if (is.numeric(v)) {
      if (length(v) == 1) return(as.character(v))
      return(sprintf("c(%s)", paste(v, collapse = ", ")))
    }
    if (length(v) == 1) return(sprintf('"%s"', v))
    sprintf("c(%s)", paste0('"', v, '"', collapse = ", "))
  }
  head <- sprintf("## --- how the %s groups were defined (%s) ---", tag, method)
  if (is.null(spec))
    return(c(head,
             "## (this allocation was saved without a grouping spec; only the",
             "##  resolved conditions below are available)"))

  common <- c(
    head,
    "## 1. the biology link: catch strata with the weight that carries a",
    "##    distribution, and how many fish were measured",
    sprintf("alloc_cols <- %s", fmt(spec$cols)),
    sprintf("bio_link <- biology_link(catch_data_raised, distributions, %s)",
            fmt(if (identical(tag, "Age")) "Age" else "LengthTotal")),
    "## 2. classify it: coverage AND number of measurements decide the status",
    sprintf("bio_link <- classify_strata(bio_link, threshold = %s, what = %s, min_meas = %s)",
            fmt(spec$thresholds[1]),
            fmt(if (identical(tag, "Age")) "age" else "length"),
            fmt(spec$min_meas[1])))

  if (identical(method, "manual"))
    return(c(common,
      "## 3. the groups come from the rules built by hand; their strata and",
      "##    matched-data conditions are the ones listed below"))

  c(common,
    "## 3. the cascade: each rung widens the donor pool by dropping a key,",
    "##    and a stratum leaves at the first rung it clears",
    sprintf("alloc_keys <- %s", fmt(spec$keys)),
    sprintf("alloc_thresholds <- %s", fmt(spec$thresholds)),
    sprintf("steps <- alloc_steps(alloc_keys, alloc_thresholds, keep_cc = %s)",
            if (isTRUE(spec$keep_cc)) "TRUE" else "FALSE"),
    sprintf("assigned <- assign_alloc_cascade(bio_link, alloc_cols, steps, bms = %s)",
            if (isTRUE(spec$bms)) "TRUE" else "FALSE"),
    "## 4. collapse the per-stratum assignment into the allocation groups",
    "alloc_groups <- cascade_to_groups(assigned, alloc_cols)")
}

emit_alloc_code <- function(groups, distributionType = "LengthTotal",
                            originType = "WGEstimate",
                            variableType = "WeightLive",
                            variableType_mean = "WeightLive") {
  c(emit_alloc_conditions(groups), "",
    "## check the conditions resolve before allocating",
    "cond_bio_test <- check_group_conditions(",
    "    catch_data = catch_data_raised, condition_list = allocStrataCond,",
    "    logFile = NULL, append = TRUE, domain = \"domainBiology\")",
    "cond_bio_test2 <- check_group_conditions(",
    "    catch_data = catch_data_raised, condition_list = bioMatchedCond,",
    "    conditionType = \"matched_data\", logFile = NULL, append = TRUE,",
    "    domain = \"domainBiology\")",
    "",
    "## allocate numbers and mean weights",
    "alloc <- catchAllocationAoL(",
    "    catch_data = catch_data_raised,",
    "    distribution_data = distributions,",
    "    condition_alloc_st_list = allocStrataCond,",
    "    condition_matched_data_list = bioMatchedCond,",
    sprintf("    originType_catch = \"%s\",", originType),
    sprintf("    variableType_catch = \"%s\",", variableType),
    sprintf("    distributionType = \"%s\",", distributionType),
    sprintf("    variableType_mean = \"%s\",", variableType_mean),
    "    logFile = \"Log.txt\", assembled_output = TRUE)",
    "distribution_alloc <- alloc$distribution",
    "catch_alloc <- alloc$catch")
}


# ============================================================
#  Allocation groups: assignment and overview
# ============================================================
# BMS is a landing in the exchange format but is biologically discard-
# like, so by default a clause naming "Dis" also picks up "BMS". Turning
# the option off treats them as the separate categories they formally
# are.
expand_cc <- function(vals, bms_as_dis = TRUE) {
  if (!length(vals)) return(vals)
  if (bms_as_dis && any(vals %in% "Dis") && !any(vals %in% "BMS"))
    c(vals, "BMS") else vals
}

# Assign each stratum to the first group whose allocStrataCond matches,
# exactly like the discard rules: order is the tie-breaker, so moving a
# group up or down changes what it catches.
assign_alloc_groups <- function(strata, groups, bms_as_dis = TRUE) {
  if (is.null(strata) || !nrow(strata)) return(NULL)
  strata$alloc_group <- NA_character_
  strata$alloc_rule  <- NA_character_
  if (!length(groups)) return(strata)
  for (g in names(groups)) {
    cond <- groups[[g]]$alloc_expr
    if (is.null(cond)) next
    hit <- tryCatch(rlang::eval_tidy(cond, data = strata),
                    error = function(e) rep(FALSE, nrow(strata)))
    hit[is.na(hit)] <- FALSE
    take <- hit & is.na(strata$alloc_group)
    strata$alloc_group[take] <- g
    strata$alloc_rule[take]  <- groups[[g]]$alloc
  }
  strata
}

# One row per group: what it allocates and what it is matched to.
alloc_group_summary <- function(assigned, groups) {
  if (is.null(assigned) || !nrow(assigned)) return(NULL)
  base <- assigned %>%
    filter(!is.na(alloc_group)) %>%
    group_by(group = alloc_group) %>%
    summarise(strata = dplyr::n(),
              weight = sum(weight, na.rm = TRUE),
              measured = sum(weight_measured, na.rm = TRUE), .groups = "drop") %>%
    mutate(coverage = ifelse(weight > 0, round(measured / weight * 100, 1), NA))
  base$allocStrataCond <- vapply(base$group,
    function(g) groups[[g]]$alloc %||% "", character(1))
  base$bioMatchedCond <- vapply(base$group,
    function(g) groups[[g]]$matched %||% groups[[g]]$alloc %||% "", character(1))
  un <- sum(is.na(assigned$alloc_group))
  if (un > 0)
    base <- bind_rows(base, data.frame(
      group = "(unassigned)", strata = un,
      weight = sum(assigned$weight[is.na(assigned$alloc_group)], na.rm = TRUE),
      measured = NA_real_, coverage = NA_real_,
      allocStrataCond = "-", bioMatchedCond = "-", stringsAsFactors = FALSE))
  base %>% mutate(weight = round(weight, 3), measured = round(measured, 3))
}

# The river for the allocation: stratum on the left, the MEANING of the
# rule that allocates it on the right - same reading as the discard one.
plot_alloc_river <- function(assigned, dims, groups, pal = NULL) {
  if (is.null(assigned) || !nrow(assigned)) return(NULL)
  d <- assigned[!is.na(assigned$alloc_group), , drop = FALSE]
  if (!nrow(d)) return(NULL)
  d$landings <- d$weight
  right <- vapply(seq_len(nrow(d)), function(i) {
    g <- d$alloc_group[i]
    m <- groups[[g]]$matched %||% groups[[g]]$alloc
    paste0(m, "  [", g, "]")
  }, character(1))
  plot_river(d, left_var = NULL, right_var = "alloc_group",
             value_var = "landings", pal = pal,
             title = "Strata and the sampled data they are allocated from",
             left_label = stratum_label(d, dims),
             right_label = right,
             stratum_axis = stratum_axis_label(dims))
}

# Walk the cascade for real: for each stratum, try each step in turn and
# take the first whose pool carries enough sampled weight.
#
# The pool at a step is every stratum sharing the kept keys - and the
# catch category too, unless the step joined them. "Enough" is the
# step's threshold read against the pool's sampled fraction, so a step
# that would borrow from barely-sampled neighbours is skipped rather
# than used.
assign_alloc_cascade <- function(bl, keys, steps, bms_as_dis = TRUE) {
  if (is.null(bl) || !nrow(bl)) return(NULL)
  d <- as.data.frame(bl)
  cc <- if ("catchCategory" %in% names(d)) as.character(d$catchCategory)
        else rep("Lan", nrow(d))
  # with BMS folded into discards the two share a pool
  cc_pool <- if (bms_as_dis) ifelse(cc %in% c("Dis", "BMS"), "Dis", cc) else cc

  d$alloc_step  <- NA_integer_
  d$alloc_level <- NA_character_
  d$alloc_pool  <- NA_character_
  d$alloc_group <- NA_character_

  pool_id <- function(step, i) {
    kept <- setdiff(keys, step$drop)
    kept <- intersect(kept, names(d))
    parts <- if (length(kept))
      do.call(paste, c(lapply(kept, function(k) as.character(d[[k]])), sep = "|"))
      else rep("ALL", nrow(d))
    if (isTRUE(step$drop_cc)) parts else paste(cc_pool, parts, sep = "||")
  }

  for (si in seq_along(steps)) {
    st <- steps[[si]]
    todo <- is.na(d$alloc_step)
    if (!any(todo)) break
    pid <- pool_id(st, si)
    # step 1 is the stratum on its own data
    if (identical(st$id, "own")) pid <- paste0("own::", seq_len(nrow(d)))
    agg <- tapply(seq_len(nrow(d)), pid, function(ix)
      c(w = sum(d$weight[ix], na.rm = TRUE),
        m = sum(d$weight_measured[ix], na.rm = TRUE)))
    cov <- vapply(agg, function(z)
      if (is.na(z["w"]) || z["w"] <= 0) NA_real_ else z["m"] / z["w"] * 100,
      numeric(1))
    thr <- if (is.na(st$threshold)) -Inf else st$threshold
    ok <- todo & !is.na(cov[pid]) & cov[pid] >= thr
    d$alloc_step[ok]  <- si
    d$alloc_level[ok] <- st$label
    d$alloc_pool[ok]  <- pid[ok]
    d$alloc_group[ok] <- paste0("S", si)
  }
  # anything still unresolved fell off the end of the ladder
  d$alloc_level[is.na(d$alloc_step)] <- "not allocated"
  d$alloc_group[is.na(d$alloc_step)] <- "none"
  d
}

# What each rung of the cascade actually resolved.
alloc_cascade_table <- function(assigned, steps) {
  if (is.null(assigned) || !nrow(assigned)) return(NULL)
  tot_w <- sum(assigned$weight, na.rm = TRUE)
  tot_n <- nrow(assigned)
  rows <- lapply(seq_along(steps), function(i) {
    sel <- which(assigned$alloc_step == i)
    data.frame(step = i,
               scope = alloc_step_scope(steps[[i]]),
               pool = steps[[i]]$label,
               threshold = steps[[i]]$threshold,
               n_strata = length(sel),
               weight = sum(assigned$weight[sel], na.rm = TRUE),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  un <- which(is.na(assigned$alloc_step))
  if (length(un))
    out <- rbind(out, data.frame(step = NA_integer_, scope = "-",
      pool = "not allocated", threshold = NA_real_, n_strata = length(un),
      weight = sum(assigned$weight[un], na.rm = TRUE),
      stringsAsFactors = FALSE))
  out$pct_strata <- round(out$n_strata / tot_n * 100, 1)
  out$pct_weight <- round(out$weight / tot_w * 100, 1)
  out$cum_pct_weight <- round(cumsum(out$weight) / tot_w * 100, 1)
  out$weight <- round(out$weight, 3)
  out
}

# Turn a resolved cascade into the same group structure the manual
# builder produces, so both feed catchAllocationAoL() identically.
#
# The condition has to reproduce the assignment EXACTLY. Listing the
# values each rung saw and combining them with & is not enough: the
# cross-product of those value sets matches strata the rung never
# resolved, and catchAllocationAoL() then rejects the lot with
# "records assigned to more than one stratum". So each group is written
# as an OR over its actual key tuples, with any key that is constant
# across the group factored out in front to keep it readable.
cascade_to_groups <- function(assigned, keys) {
  if (is.null(assigned) || !nrow(assigned)) return(list())
  d <- assigned[!is.na(assigned$alloc_step), , drop = FALSE]
  if (!nrow(d)) return(list())
  kk <- c(if ("catchCategory" %in% names(d)) "catchCategory",
          intersect(keys, names(d)))
  q1 <- function(k, v) sprintf('%s == "%s"', k, v)

  gs <- list()
  for (g in unique(d$alloc_group)) {
    sel <- unique(d[d$alloc_group == g, kk, drop = FALSE])
    # keys with a single value across the whole group go out front
    const <- kk[vapply(kk, function(k)
      length(unique(as.character(sel[[k]]))) == 1, logical(1))]
    vary <- setdiff(kk, const)
    front <- vapply(const, function(k) q1(k, as.character(sel[[k]][1])),
                    character(1))
    txt <- if (!length(vary)) paste(front, collapse = " & ")
    else {
      terms <- vapply(seq_len(nrow(sel)), function(i)
        paste0("(", paste(vapply(vary, function(k)
          q1(k, as.character(sel[[k]][i])), character(1)),
          collapse = " & "), ")"), character(1))
      body <- paste(unique(terms), collapse = " | ")
      if (length(front))
        paste0(paste(front, collapse = " & "), " & (", body, ")")
      else body
    }
    gs[[g]] <- list(label = d$alloc_level[d$alloc_group == g][1],
                    alloc = txt, matched = txt,
                    alloc_expr = rlang::new_quosure(str2lang(txt),
                                                    rlang::global_env()),
                    matched_expr = rlang::new_quosure(str2lang(txt),
                                                      rlang::global_env()))
  }
  gs
}

# ============================================================
#  Sufficiency: coverage AND number of measurements
# ============================================================
# Coverage answers "how much of the weight has a distribution attached".
# It says nothing about how many fish were actually measured, and a
# stratum can be 100% covered by a domainBiology built on eight fish.
# numMeasurements is carried per domain in the distributions table, so it
# is summed back onto the stratum and used as a second, independent
# threshold. A stratum is a usable donor only if it clears both.
stratum_measurements <- function(catches, dist, dims,
                                 catch_category = NULL,
                                 type = "LengthTotal") {
  lk <- link_dist_to_strata(catches, dist, dims, catch_category, type)
  if (is.null(lk) || !nrow(lk)) return(NULL)
  # numMeasurements repeats across the classes of a domain, so it is
  # taken once per domain rather than summed over every length class
  per_domain <- lk %>%
    filter(.var_kind == "numbers") %>%
    group_by(across(all_of(c(dims, "domainBiology")))) %>%
    summarise(n_meas = suppressWarnings(max(.n_meas, na.rm = TRUE)),
              .groups = "drop") %>%
    mutate(n_meas = ifelse(is.finite(n_meas), n_meas, 0))
  per_domain %>%
    group_by(across(all_of(dims))) %>%
    summarise(n_measurements = sum(n_meas, na.rm = TRUE),
              n_domains = dplyr::n(), .groups = "drop") %>%
    as.data.frame()
}

# Attach the measurement counts to a biology link and flag both tests.
add_sufficiency <- function(bl, meas, min_coverage = 50, min_measurements = 0) {
  if (is.null(bl) || !nrow(bl)) return(bl)
  if (!is.null(meas)) {
    keys <- intersect(names(bl), names(meas))
    keys <- setdiff(keys, c("n_measurements", "n_domains"))
    bl <- bl %>% left_join(meas, by = keys)
  }
  if (!"n_measurements" %in% names(bl)) bl$n_measurements <- NA_real_
  bl$n_measurements[is.na(bl$n_measurements)] <- 0
  bl$ok_coverage <- !is.na(bl$coverage) & bl$coverage >= min_coverage
  bl$ok_measurements <- bl$n_measurements >= min_measurements
  bl$sufficient <- bl$ok_coverage & bl$ok_measurements
  # why a stratum failed, in words, so the screen can say it
  bl$shortfall <- ifelse(
    bl$sufficient, "",
    ifelse(!bl$ok_coverage & !bl$ok_measurements, "coverage and measurements",
    ifelse(!bl$ok_coverage, "coverage", "measurements")))
  bl
}

# A one-line warning about thinly measured strata, for the screen.
measurement_warning <- function(bl, min_measurements) {
  if (is.null(bl) || !nrow(bl) || min_measurements <= 0) return(NULL)
  thin <- bl[bl$n_measurements > 0 & bl$n_measurements < min_measurements, ,
             drop = FALSE]
  if (!nrow(thin)) return(NULL)
  sprintf(paste("%s stratum/strata carry a distribution built on fewer than",
                "%s measurements (fewest: %s). They are marked as insufficient",
                "donors."),
          nrow(thin), min_measurements, min(thin$n_measurements, na.rm = TRUE))
}

# ============================================================
#  Manual allocation: what is assigned, and what is still missing
# ============================================================
# Exact-match field access. `$` partial-matches on lists, so on a group
# holding both `alloc` and `alloc_expr` a lookup of `$matched` returns
# `matched_expr` - a quosure where a string was wanted. `[[` matches
# exactly, which is what is needed here.
fld <- function(x, nm, default = NULL) {
  v <- x[[nm]]
  if (is.null(v)) default else v
}

# Building groups by hand, the question that matters after every rule is
# "what have I NOT covered yet". This mirrors the hierarchical cascade
# table: one row per rule in the order it is tested, the strata and
# weight it claims, and the running total - so the gap closing (or not)
# is visible as the rules go in, rather than only at the end.
manual_progress <- function(assigned, groups, weight_col = "weight") {
  if (is.null(assigned) || !nrow(assigned)) return(NULL)
  w <- assigned[[weight_col]]
  tot_w <- sum(w, na.rm = TRUE); tot_n <- nrow(assigned)
  gid <- assigned$alloc_group
  rows <- lapply(seq_along(groups), function(i) {
    g <- names(groups)[i]
    sel <- which(gid == g)
    data.frame(order = i, group = g,
               label = fld(groups[[g]], "label", ""),
               condition = fld(groups[[g]], "alloc", ""),
               n_strata = length(sel),
               weight = sum(w[sel], na.rm = TRUE),
               stringsAsFactors = FALSE)
  })
  out <- if (length(rows)) do.call(rbind, rows) else
    data.frame(order = integer(0), group = character(0), label = character(0),
               condition = character(0), n_strata = integer(0),
               weight = numeric(0), stringsAsFactors = FALSE)
  miss <- which(is.na(gid))
  out <- rbind(out, data.frame(order = NA_integer_, group = "STILL MISSING",
    label = "", condition = "-", n_strata = length(miss),
    weight = sum(w[miss], na.rm = TRUE), stringsAsFactors = FALSE))
  out$pct_strata <- round(out$n_strata / tot_n * 100, 1)
  out$pct_weight <- round(out$weight / tot_w * 100, 1)
  # the cumulative columns stop before the MISSING row: that row is the
  # remainder, not another step
  assigned_rows <- out$group != "STILL MISSING"
  out$cum_pct_strata <- NA_real_; out$cum_pct_weight <- NA_real_
  out$cum_pct_strata[assigned_rows] <-
    round(cumsum(out$n_strata[assigned_rows]) / tot_n * 100, 1)
  out$cum_pct_weight[assigned_rows] <-
    round(cumsum(out$weight[assigned_rows]) / tot_w * 100, 1)
  out$weight <- round(out$weight, 3)
  out
}

# The strata not yet claimed by any rule, listed so they can be read and
# a rule written for them. Sorted by weight: the biggest hole first.
manual_missing <- function(assigned, dims, weight_col = "weight") {
  if (is.null(assigned) || !nrow(assigned)) return(NULL)
  miss <- assigned[is.na(assigned$alloc_group), , drop = FALSE]
  if (!nrow(miss)) return(NULL)
  keep <- intersect(c(dims, weight_col, "coverage", "n_measurements"),
                    names(miss))
  out <- miss[, keep, drop = FALSE]
  out <- out[order(-out[[weight_col]]), , drop = FALSE]
  num <- vapply(out, is.numeric, logical(1))
  out[num] <- lapply(out[num], function(x) round(x, 3))
  out
}

# A richer per-group summary, in the shape the discard section uses:
# what each group RAISES on the left, what it is SOURCED from on the
# right, plus how much is still unassigned.
alloc_group_table <- function(assigned, groups, dims,
                              weight_col = "weight") {
  if (is.null(assigned) || !nrow(assigned) || !length(groups)) return(NULL)
  w <- assigned[[weight_col]]
  rows <- lapply(seq_along(groups), function(i) {
    g <- names(groups)[i]
    sel <- assigned$alloc_group == g & !is.na(assigned$alloc_group)
    # the donor side is evaluated independently: a group may be sourced
    # from strata it does not itself raise
    src <- tryCatch(rlang::eval_tidy(fld(groups[[g]], "matched_expr",
                                         groups[[g]][["alloc_expr"]]),
                                     data = assigned),
                    error = function(e) rep(FALSE, nrow(assigned)))
    src[is.na(src)] <- FALSE
    data.frame(
      # NOTE [[ ]] not $ : `$` partial-matches on lists, so `$matched`
      # would silently return `matched_expr` - a quosure - and land a
      # language object in a data.frame column.
      group_id = g, label = fld(groups[[g]], "label", ""),
      allocStrataCond = fld(groups[[g]], "alloc", ""),
      bioMatchedCond = fld(groups[[g]], "matched", fld(groups[[g]], "alloc", "")),
      n_strata_raised = sum(sel),
      weight_raised = round(sum(w[sel], na.rm = TRUE), 3),
      n_strata_source = sum(src),
      source_coverage = if (sum(src))
        round(sum(assigned$weight_measured[src], na.rm = TRUE) /
              sum(assigned$weight[src], na.rm = TRUE) * 100, 1) else NA_real_,
      source_measurements = if (sum(src) && "n_measurements" %in% names(assigned))
        sum(assigned$n_measurements[src], na.rm = TRUE) else NA_real_,
      stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  miss_n <- sum(is.na(assigned$alloc_group))
  out$still_missing <- c(rep(NA_integer_, nrow(out) - 1), miss_n)
  out
}

# ============================================================
#  Thumbnail preview of a stratum, for a hover tooltip
# ============================================================
# Excluding a stratum from a donor pool is a decision made on a name in
# a dropdown. This renders that stratum's own length frequency and mean
# weight as a very small inline SVG, so the shape being removed can be
# seen before removing it - a stratum with a bimodal or truncated length
# distribution is exactly the one worth excluding, and that is invisible
# from the label alone.
#
# Deliberately crude: two sparklines on a shared x range, no axes, no
# legend. It is a glance, not a plot.
stratum_sparkline <- function(linked, dims, key, width = 220, height = 96) {
  if (is.null(linked) || !nrow(linked)) return(NULL)
  present <- intersect(dims, names(linked))
  if (!length(present)) return(NULL)
  k <- do.call(paste, c(lapply(present, function(c_)
    as.character(linked[[c_]])), sep = " | "))
  d <- linked[k == key, , drop = FALSE]
  if (!nrow(d)) return(NULL)

  nb <- length_numbers(d, character(0))
  mw <- length_meanweight(d, character(0))
  if (is.null(nb) || !nrow(nb)) return(NULL)

  pad <- 6; w <- width - 2 * pad; h <- height - 2 * pad
  xr <- range(nb$.class_cm, na.rm = TRUE)
  if (!all(is.finite(xr)) || diff(xr) <= 0) xr <- xr + c(-1, 1)
  sx <- function(x) pad + (x - xr[1]) / diff(xr) * w

  # top half: numbers at length, as an area
  nr <- range(c(0, nb$number), na.rm = TRUE)
  syn <- function(y) pad + h * 0.45 - (y - nr[1]) / max(diff(nr), 1e-9) * (h * 0.42)
  poly <- paste(sprintf("%.1f,%.1f", sx(nb$.class_cm), syn(nb$number)),
                collapse = " ")
  area <- sprintf('<polygon points="%.1f,%.1f %s %.1f,%.1f" fill="%s" fill-opacity="0.55" stroke="%s" stroke-width="1"/>',
                  sx(xr[1]), syn(0), poly, sx(xr[2]), syn(0),
                  PAL_BASE[1], PAL_BASE[1])

  # bottom half: mean weight at length, as a line
  line <- ""
  if (!is.null(mw) && nrow(mw) > 1) {
    wr <- range(mw$mean_weight, na.rm = TRUE)
    syw <- function(y) pad + h - (y - wr[1]) / max(diff(wr), 1e-9) * (h * 0.42)
    line <- sprintf('<polyline points="%s" fill="none" stroke="%s" stroke-width="1.4"/>',
                    paste(sprintf("%.1f,%.1f", sx(mw$.class_cm),
                                  syw(mw$mean_weight)), collapse = " "),
                    PAL_BASE[3])
  }

  lab <- function(x, y, t, col) sprintf(
    '<text x="%.0f" y="%.0f" font-size="8" fill="%s" font-family="sans-serif">%s</text>',
    x, y, col, t)

  paste0('<svg width="', width, '" height="', height,
         '" xmlns="http://www.w3.org/2000/svg">',
         '<rect width="100%" height="100%" fill="#fff" stroke="#cfd8e0"/>',
         area, line,
         lab(pad, pad + 8, "numbers", PAL_BASE[1]),
         lab(pad, pad + h * 0.62, "mean weight", PAL_BASE[3]),
         lab(pad, height - 2, sprintf("%.0f-%.0f cm", xr[1], xr[2]), "#666"),
         '</svg>')
}

# Build the whole lookup once: key -> svg. Cheap enough for the number
# of strata in play, and it means the tooltip is instant on hover.
stratum_sparkline_map <- function(linked, dims, keys) {
  if (is.null(linked) || !length(keys)) return(list())
  out <- lapply(keys, function(k)
    tryCatch(stratum_sparkline(linked, dims, k), error = function(e) NULL))
  names(out) <- keys
  out[!vapply(out, is.null, logical(1))]
}
