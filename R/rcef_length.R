# =====================================================================
#  rcef_length.R   (distributions prep, biology link, allocation steps and cascades)
#
#  ORIGIN: this project (ILVO / RCEF Workbench). Freely refactorable.
#  Was: R_development/mod_length.R
#  No shiny in this file. If it needs `input`, `session` or a tag,
#  it belongs in app/, not here.
# =====================================================================

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
#' @noRd
#' @export
LEN_CLASS_TO_MM <- c(mm = 1, cm = 10, dm = 100, m = 1000)
#' @noRd
#' @export
NUMBER_TO_N     <- c(N = 1, n = 1, thousands = 1000, kN = 1000,
                     millions = 1e6, MN = 1e6,
                     NE3 = 1e3, NE6 = 1e6, NE9 = 1e9)
#' @noRd
#' @export
WEIGHT_TO_G     <- c(g = 1, kg = 1000, t = 1e6, mg = 0.001)

#' @noRd
#' @export
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
#' @noRd
#' @export
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
#' @noRd
#' @export
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
#' @noRd
#' @export
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
#' @noRd
#' @export
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
#' @noRd
#' @export
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
#' @noRd
#' @export
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
#' @noRd
#' @export
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

#' The values behind each mean-weight point, un-collapsed
#'
#' [length_meanweight()] answers "what is the mean weight at this class"
#' by collapsing every stratum that shares the grouping into one number.
#' That number is what should be used, but it hides how much the strata
#' it came from disagreed - and a mean of four seasons that range from
#' 180 g to 340 g is a different object from a mean of four that all sit
#' at 250 g.
#'
#' This returns the values BEFORE the collapse, one row per contributing
#' stratum, so a box can be drawn where a single dot would otherwise sit.
#' The `+/- 1 sd` band on the line plot answers the same question with a
#' summary statistic; a box answers it with the values, which for four or
#' five strata is the more honest picture.
#'
#' @param linked A linked biology table, as [length_meanweight()] takes.
#' @param group_vars Grouping columns. Everything NOT named here is what
#'   the box spreads over.
#' @return A data frame with `.class`, the grouping columns,
#'   `mean_weight` (the stratum's own value), `.w` (its weight in the
#'   mean) and `n_group`, the number of values sharing that class and
#'   group. `NULL` if there is nothing to draw.
#' @seealso [length_meanweight()], [plot_length_meanweight_box()]
#' @export
length_meanweight_raw <- function(linked, group_vars = character(0)) {
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
  keep <- intersect(c(keys, "catchCategory", DIMS_ALL_CANDIDATES), names(mw))
  out <- mw[, unique(c(keep, ".value_std", ".w", ".n_meas")), drop = FALSE]
  names(out)[names(out) == ".value_std"] <- "mean_weight"
  out <- out[is.finite(out$mean_weight), , drop = FALSE]
  if (!nrow(out)) return(NULL)
  # how many values share each box, so the plot can say where a "box" is
  # really one point and stop pretending to show a spread
  k <- do.call(paste, c(lapply(keys, function(v) as.character(out[[v]])),
                        sep = "\r"))
  out$n_group <- as.integer(table(k)[k])
  as.data.frame(out)
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
#' @noRd
#' @export
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
#' @noRd
#' @export
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
# ------------------------------------------------------------
#  Landings and discards, held to different standards
# ------------------------------------------------------------
# Discards are sampled less than landings almost everywhere, so a single
# coverage threshold either lets landings resolve on too little or forces
# discards down to the bottom of the ladder. Every threshold in the
# cascade is therefore a PAIR.
#
# A spec is accepted in three shapes, so nothing that used to pass a
# plain vector breaks:
#   50                  -> both categories, every rung
#   c(50, 40, 30)       -> both categories, per rung
#   list(Lan = c(50, 40), Dis = c(30, 20))
#
# BMS follows discards, matching bms_as_dis elsewhere.

#' Which of the two brackets a catch category falls in
#' @param cc Character vector of catch categories.
#' @param bms_as_dis Fold BMS in with discards.
#' @return `"Lan"` or `"Dis"`, one per element.
#' @export
#' @examples
#' cc_bracket(c("Lan", "Dis", "BMS"))
cc_bracket <- function(cc, bms_as_dis = TRUE) {
  cc <- as.character(cc)
  dis <- if (bms_as_dis) c("Dis", "BMS") else "Dis"
  ifelse(cc %in% dis, "Dis", "Lan")
}

#' Normalise a threshold spec to a Landings / Discards pair
#'
#' @param x A number, a numeric vector, or a list with `Lan` and `Dis`.
#' @param default Value used where the spec runs short.
#' @return A list with `Lan` and `Dis`, both numeric vectors.
#' @seealso [cc_step_value()]
#' @export
#' @examples
#' as_cc_spec(50)
#' as_cc_spec(list(Lan = c(50, 40), Dis = c(30, 20)))
as_cc_spec <- function(x, default = NA_real_) {
  if (is.null(x)) return(list(Lan = default, Dis = default))
  if (is.list(x)) {
    lan <- if (!is.null(x$Lan)) as.numeric(x$Lan) else default
    dis <- if (!is.null(x$Dis)) as.numeric(x$Dis) else lan
    return(list(Lan = lan, Dis = dis))
  }
  x <- as.numeric(x)
  list(Lan = x, Dis = x)
}

#' The Landings / Discards pair for one rung of a cascade
#'
#' @param spec A spec, as accepted by [as_cc_spec()].
#' @param i Rung number, 1-based.
#' @param default Value used where the spec runs short.
#' @return A named length-two numeric: `Lan` and `Dis`.
#' @export
#' @examples
#' cc_step_value(list(Lan = c(50, 40), Dis = c(30, 20)), 2)
cc_step_value <- function(spec, i, default = NA_real_) {
  sp <- as_cc_spec(spec, default)
  pick <- function(v) if (length(v) >= i) v[i] else
    if (length(v)) v[length(v)] else default
  c(Lan = pick(sp$Lan), Dis = pick(sp$Dis))
}

#' A threshold spec as R source
#'
#' @param x A number, numeric vector, or `Lan`/`Dis` list.
#' @return A length-one character vector of R code.
#' @export
#' @examples
#' fmt_cc_spec(list(Lan = c(50, 40), Dis = c(30, 20)))
fmt_cc_spec <- function(x) {
  num <- function(v) if (length(v) == 1) format(v) else
    paste0("c(", paste(format(v), collapse = ", "), ")")
  if (is.list(x)) {
    sp <- as_cc_spec(x)
    sprintf("list(Lan = %s, Dis = %s)", num(sp$Lan), num(sp$Dis))
  } else num(as.numeric(x))
}

#' Coerce a value to a Landings / Discards pair
#' @param x A number or a named c(Lan=, Dis=) vector.
#' @return A c(Lan=, Dis=) numeric.
#' @noRd
#' @export
as_cc_pair <- function(x) {
  if (is.null(x)) return(c(Lan = NA_real_, Dis = NA_real_))
  if (!is.null(names(x)) && all(c("Lan","Dis") %in% names(x)))
    return(c(Lan = as.numeric(x[["Lan"]]), Dis = as.numeric(x[["Dis"]])))
  v <- as.numeric(x)[1]; c(Lan = v, Dis = v)
}

#' @noRd
#' @export
#' @noRd
#' @export
alloc_steps <- function(keys, thresholds, keep_cc = TRUE, min_meas = 0,
                        donor_cov_min = NA, donor_meas_min = NA,
                        donor_excluded = character(0),
                        own_threshold = NULL, own_min_meas = NULL) {
  keys <- keys[nzchar(keys)]
  thr <- function(i) cc_step_value(thresholds, i, NA_real_)
  mm  <- function(i) cc_step_value(min_meas, i, 0)
  out <- list()

  # `drop_n` counts how many keys are dropped from the coarse end;
  # `drop` keeps the names for the label. The count is what the cascade
  # uses, because alloc_steps() is called with the ROLE names the user
  # picked ("Fleet", "Area") while assign_alloc_cascade() is called with
  # the COLUMN names those map to ("metier4", "ICESDivision"). Matching
  # by name therefore dropped nothing whenever the two differed, and
  # every rung silently stayed at full precision - which is why a
  # no-threshold last rung could still leave strata unresolved.
  add <- function(id, label, drop, drop_cc, threshold,
                  minmeas = c(Lan = 0, Dis = 0), drop_all = FALSE)
    out[[length(out) + 1]] <<- list(id = id, label = label, drop = drop,
                                    drop_n = length(drop), drop_all = drop_all,
                                    drop_cc = drop_cc, threshold = threshold,
                                    min_meas = minmeas,
                                    donor_cov_min = donor_cov_min,
                                    donor_meas_min = donor_meas_min,
                                    donor_excluded = donor_excluded)

  # A threshold spec may carry its own-stratum bar separately under $own,
  # with the per-key vector under $Lan / $Dis. When it does, that is the
  # authoritative own bar: the caller measured which box drives the own
  # rung so the cascade never has to guess.
  if (is.list(thresholds) && !is.null(thresholds$own) && is.null(own_threshold))
    own_threshold <- thresholds$own
  if (is.list(min_meas) && !is.null(min_meas$own) && is.null(own_min_meas))
    own_min_meas <- min_meas$own

  n <- length(keys)

  # The threshold and the measurement bar belong to the FINEST KEY STILL
  # IN THE POOL at a rung, not to the rung's position in the ladder. This
  # was the bug: rung w1 drops the coarsest key (Area) and keeps
  # metier4..Season, so its threshold is Season's - thr(n) - but the old
  # code read thr(2), which is the SECOND key's. The mapping ran backwards,
  # so the Area box drove the catchCategory rung and vice versa. `tk()`
  # and `tm()` take a KEY INDEX and are the single place that mapping lives.
  tk <- function(ki) cc_step_value(thresholds, ki, NA_real_)
  tm <- function(ki) cc_step_value(min_meas,   ki, 0)

  # step 1 is always the stratum's own sampled data. Its bar is the
  # explicit own bar when the caller supplied one, else the finest key's.
  own_thr <- if (!is.null(own_threshold)) as_cc_pair(own_threshold) else tk(n)
  own_mm  <- if (!is.null(own_min_meas))  as_cc_pair(own_min_meas)  else tm(n)
  add("own", "own stratum", character(0), FALSE, own_thr, own_mm)

  # --- pass 1: stay inside the catch category, widen one key at a time
  for (i in seq_along(keys)) {
    dropped <- rev(keys)[seq_len(i)]        # drop the i coarsest keys
    kept <- setdiff(keys, dropped)
    # finest key still pooled is keys[n-i]; when nothing is left the pool
    # is the whole category, which takes the coarsest bar, key 1
    ki <- if (length(kept)) n - i else 1L
    add(paste0("w", i),
        if (length(kept)) paste(c("catchCategory", kept), collapse = " + ")
        else "catchCategory (all other data in the same category)",
        dropped, FALSE, tk(ki), tm(ki))
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
      ki <- if (length(kept)) n - j else 1L
      add(paste0("x", j),
          if (length(kept))
            paste0(paste(kept, collapse = " + "), "  (categories joined)")
          else "ALL data, both categories pooled",
          dropped, TRUE,
          # Same finest-key rule as pass 1. The bracket is read from the
          # stratum BEING allocated, not from the pool: a discard stratum
          # keeps its looser standard even when the pool now holds landings.
          tk(ki), tm(ki))
    }
  }

  # --- last resort: every rung above has a bar, and a stratum that
  # clears none of them ends the ladder unallocated. That is the right
  # answer for a stratum nobody has any data for; it is the wrong answer
  # when data exists and is simply thinner than the bar. The catch is
  # that "unallocated" is not a neutral outcome downstream - the weight
  # never reaches the numbers at length, so the totals silently stop
  # adding up.
  #
  # So the ladder ends with one rung that has NO bar at all: pool
  # everything available and use it. Two versions, depending on whether
  # the category may be dropped:
  #
  #   keep_cc = TRUE   pool everything IN THE SAME CATEGORY. Discards are
  #                    allocated from all discard data there is, landings
  #                    from all landings data. The two never mix, which is
  #                    the whole point of keeping the category.
  #   keep_cc = FALSE  pool everything, both categories. Only reached when
  #                    the category-joined rungs above have also failed.
  #
  # It is reached ONLY by strata the bars did not resolve, so nothing that
  # met a real threshold is affected.
  # drop_all, not `keys`: this rung pools EVERYTHING, and saying so as a
  # flag cannot be defeated by a naming mismatch the way a list of names
  # can. It is the biology equivalent of the discard catch-all - every
  # stratum left over is allocated, from every distribution there is.
  if (isTRUE(keep_cc))
    add("last", "ALL data in the same catchCategory  (no threshold)",
        keys, FALSE, c(Lan = -Inf, Dis = -Inf), c(Lan = 0, Dis = 0),
        drop_all = TRUE)
  else
    add("last", "ALL data, both categories pooled  (no threshold)",
        keys, TRUE, c(Lan = -Inf, Dis = -Inf), c(Lan = 0, Dis = 0),
        drop_all = TRUE)

  # carry the key set so alloc_cascade_table() can name what each rung
  # groups by without the caller having to pass the keys a second time
  attr(out, "keys") <- keys
  out
}

# Which pass a step belongs to, for the table and the chain.
#' @noRd
#' @export
alloc_step_scope <- function(step) {
  if (identical(step$id, "last")) "last resort - no threshold"
  else if (isTRUE(step$drop_cc)) "categories joined"
  else "within category"
}

# The condition text for one group, as it will appear in the emitted
# allocStrataCond / bioMatchedCond lists.
#' @noRd
#' @export
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
#' @noRd
#' @export
alloc_quo <- function(vals) {
  txt <- alloc_cond_string(vals)
  rlang::new_quosure(str2lang(txt), env = rlang::global_env())
}

# Emit the two lists the allocation takes. Names follow the ICES script:
# allocStrataCond for the strata being allocated, bioMatchedCond for the
# sampled data each is matched to.
#' @noRd
#' @export
emit_alloc_conditions <- function(groups) {
  if (!length(groups))
    return(c("allocStrataCond <- list()", "", "bioMatchedCond <- list()"))
  cs <- alloc_conds_from_groups(groups)
  one <- function(nm, txts) {
    body <- vapply(names(txts), function(g) sprintf("     %s = quo(%s)", g,
                                                    txts[[g]]), character(1))
    paste0(nm, " <-\n    list(", sub("^     ", "",
                                     paste(body, collapse = ",\n")), ")")
  }
  matched_txt <- vapply(names(groups), function(g)
    fld(groups[[g]], "matched", fld(groups[[g]], "alloc", "TRUE")),
    character(1))
  needs_metrics <- any(grepl("coverage|discard_rate|n_measurements",
                             matched_txt))
  c(if (needs_metrics) c(
      "## A bioMatchedCond below compares stratum metrics, which a CEF",
      "## table does not carry - coverage is a property of a stratum, not",
      "## of a row. Join them on once, and the conditions read as the",
      "## thresholds they are rather than as a list of stratum keys.",
      "catch_raised <- add_stratum_metrics(catch_raised, dims_biology)",
      ""),
    "## Groups are tested top to bottom, first match wins - so the strata",
    "## clauses are written plainly (a catch-all is just quo(TRUE)).",
    "## complement_conds() turns them into the partition",
    "## catchAllocationAoL() needs; without it a catch-all would match",
    "## records an earlier group already took and the run is rejected.",
    one("allocStrataClauses", cs$allocStrataSimple),
    "allocStrataCond <- complement_conds(allocStrataClauses)", "",
    "## bioMatchedCond is NOT a partition - donor pools may overlap, and",
    "## a well-sampled stratum usually feeds several groups.",
    one("bioMatchedCond",
        vapply(names(groups), function(g)
          fld(groups[[g]], "matched", fld(groups[[g]], "alloc", "TRUE")),
          character(1))))
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
#' @seealso [emit_alloc_code()]
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
            fmt(cc_step_value(spec$thresholds, 1)["Lan"]),
            fmt(if (identical(tag, "Age")) "age" else "length"),
            fmt(cc_step_value(spec$min_meas, 1)["Lan"])))

  if (identical(method, "manual"))
    return(c(common,
      "## 3. the groups come from the rules built by hand; their strata and",
      "##    matched-data conditions are the ones listed below"))

  c(common,
    "## 3. the cascade: each rung widens the donor pool by dropping a key,",
    "##    and a stratum leaves at the first rung it clears",
    sprintf("alloc_keys <- %s", fmt(spec$keys)),
    "## Landings and discards carry their own thresholds: discards are",
    "## sampled less, so holding both to one bar either lets landings",
    "## resolve on too little or pushes discards to the bottom rung.",
    sprintf("alloc_thresholds <- %s", fmt_cc_spec(spec$thresholds)),
    sprintf("alloc_min_meas   <- %s", fmt_cc_spec(spec$min_meas)),
    "## Donor bars are a SEPARATE question from the rung thresholds: the",
    "## thresholds decide which strata a rung resolves, these decide which",
    "## strata may lend a distribution. A stratum failing them is still",
    "## allocated - it just does not donate.",
    sprintf("alloc_donor_cov  <- %s",
            if (is.null(spec$donor_cov_min) || is.na(spec$donor_cov_min))
              "NA" else format(spec$donor_cov_min)),
    sprintf("alloc_donor_meas <- %s",
            if (is.null(spec$donor_meas_min) || is.na(spec$donor_meas_min))
              "NA" else format(spec$donor_meas_min)),
    sprintf(paste("steps <- alloc_steps(alloc_keys, alloc_thresholds,",
                  "keep_cc = %s, min_meas = alloc_min_meas,",
                  "donor_cov_min = alloc_donor_cov,",
                  "donor_meas_min = alloc_donor_meas)"),
            if (isTRUE(spec$keep_cc)) "TRUE" else "FALSE"),
    sprintf("assigned <- assign_alloc_cascade(bio_link, alloc_cols, steps, bms = %s)",
            if (isTRUE(spec$bms)) "TRUE" else "FALSE"),
    "## 4. collapse the per-stratum assignment into the allocation groups",
    "alloc_groups <- cascade_to_groups(assigned, alloc_cols)")
}

#' @noRd
#' @export
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
#' @noRd
#' @export
expand_cc <- function(vals, bms_as_dis = TRUE) {
  if (!length(vals)) return(vals)
  if (bms_as_dis && any(vals %in% "Dis") && !any(vals %in% "BMS"))
    c(vals, "BMS") else vals
}

# Assign each stratum to the first group whose allocStrataCond matches,
# exactly like the discard rules: order is the tie-breaker, so moving a
# group up or down changes what it catches.
#' @noRd
#' @export
assign_alloc_groups <- function(strata, groups, bms_as_dis = TRUE) {
  if (is.null(strata) || !nrow(strata)) return(NULL)
  strata$alloc_group <- NA_character_
  strata$alloc_rule  <- NA_character_
  if (!length(groups)) return(strata)
  # A clause-less group evaluates to the literal TRUE, which comes back
  # length 1, not length nrow(). `hit & is.na(...)` recycled it silently,
  # but `which(hit)` did not: the catch-all's donor pool came out as a
  # single stratum. Normalised once here so both loops are safe.
  eval_hit <- function(cond) {
    h <- tryCatch(rlang::eval_tidy(cond, data = strata),
                  error = function(e) rep(FALSE, nrow(strata)))
    if (length(h) == 1L) h <- rep(h, nrow(strata))
    h[is.na(h)] <- FALSE
    h
  }

  for (g in names(groups)) {
    cond <- groups[[g]]$alloc_expr
    if (is.null(cond)) next
    hit <- eval_hit(cond)
    take <- hit & is.na(strata$alloc_group)
    strata$alloc_group[take] <- g
    strata$alloc_rule[take]  <- groups[[g]]$alloc
  }

  # Donor membership, the other half of the picture. Allocation is
  # first-match-wins so a stratum is allocated by exactly one group;
  # donor pools are the opposite and may overlap freely, so this is a
  # list column, not a single id. The discard side has carried the same
  # pair since it was written - assign_rule_groups() calls them
  # toBeRaisedGroup and usedToRaiseGroup - and without it there is no way
  # to answer "which groups is this stratum lending its distribution to".
  used <- vector("list", nrow(strata))
  for (i in seq_len(nrow(strata))) used[[i]] <- character(0)
  for (g in names(groups)) {
    cond <- groups[[g]]$matched_expr %||% groups[[g]]$alloc_expr
    if (is.null(cond)) next
    hit <- eval_hit(cond)
    for (i in which(hit)) used[[i]] <- c(used[[i]], g)
  }
  strata$usedToAllocGroup <- used
  strata$usedToAllocGroups_txt <- vapply(used, paste, character(1),
                                         collapse = ", ")
  strata$n_donates_to <- lengths(used)
  strata
}

#' Allocated against donated, one row per allocation group
#'
#' The counterpart of [manual_overview()] on the discard side. Two
#' numbers that are easy to conflate and mean different things: how many
#' strata a group ALLOCATES (exactly one group per stratum) and how many
#' sit in the pool it allocates FROM (a stratum can lend to several).
#'
#' @param assigned A table tagged by [assign_alloc_groups()].
#' @param groups The allocation group list.
#' @return A data frame, one row per group, plus `(unassigned)` if any.
#' @seealso [assign_alloc_groups()], [alloc_group_summary()]
#' @export
alloc_overview <- function(assigned, groups) {
  if (is.null(assigned) || !nrow(assigned) || !length(groups)) return(NULL)
  ids <- names(groups)
  w <- if ("weight" %in% names(assigned)) assigned$weight else rep(0, nrow(assigned))
  used <- if ("usedToAllocGroup" %in% names(assigned)) assigned$usedToAllocGroup
          else rep(list(character(0)), nrow(assigned))

  out <- data.frame(
    group = ids,
    label = vapply(ids, function(g) fld(groups[[g]], "label", ""), character(1)),
    n_strata_allocated = vapply(ids, function(g)
      sum(assigned$alloc_group %in% g), integer(1)),
    weight_allocated = vapply(ids, function(g)
      sum(w[assigned$alloc_group %in% g], na.rm = TRUE), numeric(1)),
    n_strata_in_pool = vapply(ids, function(g)
      sum(vapply(used, function(v) g %in% v, logical(1))), integer(1)),
    stringsAsFactors = FALSE)

  un <- sum(is.na(assigned$alloc_group))
  if (un > 0)
    out <- rbind(out, data.frame(
      group = "(unassigned)", label = "",
      n_strata_allocated = un,
      weight_allocated = sum(w[is.na(assigned$alloc_group)], na.rm = TRUE),
      n_strata_in_pool = NA_integer_, stringsAsFactors = FALSE))
  out$weight_allocated <- round(out$weight_allocated, 3)
  rownames(out) <- NULL
  out
}

# One row per group: what it allocates and what it is matched to.
#' @noRd
#' @export
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
#' @noRd
#' @export
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
#' @noRd
#' @export
assign_alloc_cascade <- function(bl, keys, steps, bms_as_dis = TRUE) {
  step_num <- function(x) {
    if (is.null(x) || !length(x) || all(is.na(x))) NA_real_ else as.numeric(x)[1]
  }
  if (is.null(bl) || !nrow(bl)) return(NULL)
  d <- as.data.frame(bl)
  cc <- if ("catchCategory" %in% names(d)) as.character(d$catchCategory)
        else rep("Lan", nrow(d))
  # with BMS folded into discards the two share a pool
  cc_pool <- if (bms_as_dis) ifelse(cc %in% c("Dis", "BMS"), "Dis", cc) else cc

  brk <- cc_bracket(cc, bms_as_dis)
  has_meas <- "n_measurements" %in% names(d)

  d$alloc_step  <- NA_integer_
  d$alloc_level <- NA_character_
  d$alloc_pool  <- NA_character_
  d$alloc_group <- NA_character_
  d$alloc_bracket <- brk
  d$alloc_donor_pool <- NA_character_

  # A stratum with no catch has nothing to allocate, so it is taken out
  # BEFORE the ladder runs rather than after. Left in, it would either sit
  # in the unresolved list for ever - it can never clear a coverage test,
  # the denominator is zero - or, worse, get quietly assigned a donor pool
  # at some rung and appear in the groups as though it needed one.
  d$alloc_empty <- is.na(d$weight) | d$weight <= 0
  d$alloc_level[d$alloc_empty] <- "no catch to allocate"
  d$alloc_group[d$alloc_empty] <- "none"

  pool_id <- function(step, i) {
    # Positional, so a role name that differs from its column name cannot
    # quietly leave a key in the pool.
    kept <- if (isTRUE(step$drop_all)) character(0)
            else if (!is.null(step$drop_n)) {
              n <- length(keys) - step$drop_n
              if (n <= 0) character(0) else keys[seq_len(n)]
            } else setdiff(keys, step$drop)
    kept <- intersect(kept, names(d))
    parts <- if (length(kept))
      do.call(paste, c(lapply(kept, function(k) as.character(d[[k]])), sep = "|"))
      else rep("ALL", nrow(d))
    if (isTRUE(step$drop_cc)) parts else paste(cc_pool, parts, sep = "||")
  }

  for (si in seq_along(steps)) {
    st <- steps[[si]]
    todo <- is.na(d$alloc_step) & !d$alloc_empty
    if (!any(todo)) break
    pid <- pool_id(st, si)
    # step 1 is the stratum on its own data
    if (identical(st$id, "own")) pid <- paste0("own::", seq_len(nrow(d)))
    # Which strata may DONATE. This is a different question from which
    # strata may be allocated, and conflating the two is what made a thin
    # stratum unallocatable rather than merely a bad donor. A stratum
    # failing these bars is dropped from the pool sums; it still gets a
    # distribution allocated to it from whatever is left.
    donor <- rep(TRUE, nrow(d))
    dcov <- step_num(st$donor_cov_min)
    dmes <- step_num(st$donor_meas_min)
    if (!is.na(dcov) && "coverage" %in% names(d))
      donor <- donor & (!is.na(d$coverage) & d$coverage >= dcov)
    else if (!is.na(dcov))
      donor <- donor & (!is.na(d$weight) & d$weight > 0 &
                        d$weight_measured / d$weight * 100 >= dcov)
    if (!is.na(dmes) && has_meas)
      donor <- donor & (!is.na(d$n_measurements) & d$n_measurements >= dmes)
    # and strata named by hand, for the cases no bar expresses - the same
    # "use everything except these" the discard catch-all offers
    dex <- st$donor_excluded %||% character(0)
    if (length(dex)) donor <- donor & !(stratum_keys(d, keys) %in% dex)

    agg <- tapply(seq_len(nrow(d)), pid, function(ix) {
      jx <- ix[donor[ix]]
      c(w = sum(d$weight[jx], na.rm = TRUE),
        m = sum(d$weight_measured[jx], na.rm = TRUE),
        n = if (has_meas) sum(d$n_measurements[jx], na.rm = TRUE) else NA_real_)
    })
    cov <- vapply(agg, function(z)
      if (is.na(z["w"]) || z["w"] <= 0) NA_real_ else z["m"] / z["w"] * 100,
      numeric(1))
    nms <- vapply(agg, function(z) unname(z["n"]), numeric(1))

    # the threshold is a property of the stratum being resolved, so it is
    # read per row from its own bracket
    thr_pair <- st$threshold
    if (is.null(thr_pair)) thr_pair <- c(Lan = NA_real_, Dis = NA_real_)
    thr_row <- unname(thr_pair[brk]); thr_row[is.na(thr_row)] <- -Inf

    mm_pair <- st$min_meas
    if (is.null(mm_pair)) mm_pair <- c(Lan = 0, Dis = 0)
    mm_row <- unname(mm_pair[brk]); mm_row[is.na(mm_row)] <- 0

    ok <- todo & !is.na(cov[pid]) & cov[pid] >= thr_row
    # A pool can clear the coverage bar on a handful of measured fish.
    # Where the counts are available the second bar is applied too; where
    # they are not, the test is skipped rather than silently failed.
    if (has_meas) ok <- ok & !is.na(nms[pid]) & nms[pid] >= mm_row

    # The last-resort rung has no threshold, but "no threshold" is not
    # "no data": a pool with weight and nothing measured in it would pass
    # a -Inf coverage bar and then allocate from a distribution that does
    # not exist. It still has to contain something measured.
    if (identical(st$id, "last")) {
      meas <- vapply(agg, function(z) unname(z["m"]), numeric(1))
      ok <- ok & !is.na(meas[pid]) & meas[pid] > 0
      d$alloc_donor_pool[ok] <- pid[ok]
    }
    d$alloc_step[ok]  <- si
    d$alloc_level[ok] <- st$label
    d$alloc_pool[ok]  <- pid[ok]
    d$alloc_group[ok] <- paste0("S", si)
  }
  # Anything still unresolved fell off the end of the ladder - but WHY
  # matters, and the two reasons need different fixes. Reporting both as
  # "not allocated" sends a user to loosen thresholds that are not the
  # problem.
  un <- is.na(d$alloc_step) & !d$alloc_empty
  last <- steps[[length(steps)]]
  bars_set <- !is.na(step_num(last$donor_cov_min)) ||
              !is.na(step_num(last$donor_meas_min)) ||
              length(last$donor_excluded %||% character(0)) > 0
  d$alloc_level[un] <- "not allocated"
  d$alloc_group[un] <- "none"
  d$alloc_reason <- NA_character_
  if (any(un)) {
    # Was there anything measured in the category at all, ignoring the
    # donor bars? If yes, the bars emptied the pool; if no, there is
    # simply no data and no threshold can conjure any.
    tot_m <- tapply(d$weight_measured, cc_pool, sum, na.rm = TRUE)
    has_any <- !is.na(tot_m[cc_pool]) & tot_m[cc_pool] > 0
    d$alloc_reason[un & !has_any] <-
      "no measured data anywhere in this catch category"
    d$alloc_reason[un & has_any] <- if (bars_set)
      "the donor-pool bars left no eligible donor - loosen them, not the rung thresholds"
      else "no donor pool with measured data"
  }
  d
}

# What each rung of the cascade actually resolved.
#' @noRd
#' @export
#' A one-line summary of what a rung groups by
#'
#' The readable form of the `pool` column: the keys still distinguished
#' at that rung. The own rung is the stratum itself; the last resort is
#' everything pooled.
#'
#' @param step One step from [alloc_steps()].
#' @return A length-one character vector.
#' @noRd
#' @export
alloc_step_grouping <- function(step, allkeys = NULL) {
  if (identical(step$id, "own")) return("the stratum itself")
  if (identical(step$id, "last") || isTRUE(step$drop_all))
    return("everything pooled")
  kept <- if (!is.null(allkeys)) setdiff(allkeys, step$drop) else NULL
  if (length(kept)) paste(kept, collapse = " + ")
  else "all data in the category"
}

# What each rung of the cascade actually resolved.
#' @noRd
#' @export
alloc_cascade_table <- function(assigned, steps, keys = NULL) {
  if (is.null(assigned) || !nrow(assigned)) return(NULL)
  # the full key set, so the grouping column can name what each rung keeps
  allkeys <- keys %||% attr(steps, "keys")
  tot_w <- sum(assigned$weight, na.rm = TRUE)
  tot_n <- nrow(assigned)
  rows <- lapply(seq_along(steps), function(i) {
    sel <- which(assigned$alloc_step == i)
    thp <- steps[[i]]$threshold
    mmp <- steps[[i]]$min_meas
    dcv <- steps[[i]]$donor_cov_min
    dms <- steps[[i]]$donor_meas_min
    st <- steps[[i]]
    grouping <- alloc_step_grouping(st, allkeys)
    data.frame(step = i,
               scope = alloc_step_scope(st),
               pool = st$label,
               grouping = grouping,
               threshold = unname(thp["Lan"]),
               threshold_Dis = unname(thp["Dis"]),
               min_meas = unname(if (is.null(mmp)) NA_real_ else mmp["Lan"]),
               min_meas_Dis = unname(if (is.null(mmp)) NA_real_ else mmp["Dis"]),
               donor_cov_min = if (is.null(dcv)) NA_real_ else as.numeric(dcv)[1],
               donor_meas_min = if (is.null(dms)) NA_real_ else as.numeric(dms)[1],
               n_strata = length(sel),
               weight = sum(assigned$weight[sel], na.rm = TRUE),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  empty <- if ("alloc_empty" %in% names(assigned)) which(assigned$alloc_empty)
           else integer(0)
  un <- setdiff(which(is.na(assigned$alloc_step)), empty)
  if (length(un))
    out <- rbind(out, data.frame(step = NA_integer_, scope = "-",
      pool = "not allocated", grouping = "-", threshold = NA_real_, threshold_Dis = NA_real_,
      min_meas = NA_real_, min_meas_Dis = NA_real_, donor_cov_min = NA_real_, donor_meas_min = NA_real_, n_strata = length(un),
      weight = sum(assigned$weight[un], na.rm = TRUE),
      stringsAsFactors = FALSE))
  if (length(empty))
    out <- rbind(out, data.frame(step = NA_integer_, scope = "-",
      pool = "no catch to allocate (weight 0)", grouping = "-", threshold = NA_real_,
      threshold_Dis = NA_real_, min_meas = NA_real_, min_meas_Dis = NA_real_,
      donor_cov_min = NA_real_, donor_meas_min = NA_real_, n_strata = length(empty),
      weight = sum(assigned$weight[empty], na.rm = TRUE),
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
#' @noRd
#' @export
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
#' @noRd
#' @export
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
#' @noRd
#' @export
add_sufficiency <- function(bl, meas, min_coverage = 50, min_measurements = 0) {
  if (is.null(bl) || !nrow(bl)) return(bl)
  if (!is.null(meas)) {
    keys <- intersect(names(bl), names(meas))
    keys <- setdiff(keys, c("n_measurements", "n_domains"))
    bl <- bl %>% left_join(meas, by = keys)
  }
  if (!"n_measurements" %in% names(bl)) bl$n_measurements <- NA_real_
  bl$n_measurements[is.na(bl$n_measurements)] <- 0
  # thresholds may be a single number or a Lan / Dis pair
  if (!"coverage" %in% names(bl)) bl$coverage <- NA_real_
  brk <- if ("catchCategory" %in% names(bl))
    cc_bracket(bl$catchCategory) else rep("Lan", nrow(bl))
  cvp <- cc_step_value(min_coverage, 1, 0)
  mmp <- cc_step_value(min_measurements, 1, 0)
  bl$ok_coverage <- !is.na(bl$coverage) & bl$coverage >= unname(cvp[brk])
  bl$ok_measurements <- bl$n_measurements >= unname(mmp[brk])
  bl$sufficient <- bl$ok_coverage & bl$ok_measurements
  # why a stratum failed, in words, so the screen can say it
  bl$shortfall <- ifelse(
    bl$sufficient, "",
    ifelse(!bl$ok_coverage & !bl$ok_measurements, "coverage and measurements",
    ifelse(!bl$ok_coverage, "coverage", "measurements")))
  bl
}

# A one-line warning about thinly measured strata, for the screen.
#' @noRd
#' @export
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
#' @noRd
#' @export
fld <- function(x, nm, default = NULL) {
  v <- x[[nm]]
  if (is.null(v)) default else v
}

# Building groups by hand, the question that matters after every rule is
# "what have I NOT covered yet". This mirrors the hierarchical cascade
# table: one row per rule in the order it is tested, the strata and
# weight it claims, and the running total - so the gap closing (or not)
# is visible as the rules go in, rather than only at the end.
#' @noRd
#' @export
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
#' @noRd
#' @export
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
#' @noRd
#' @export
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
#' @noRd
#' @export
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
#' @noRd
#' @export
stratum_sparkline_map <- function(linked, dims, keys) {
  if (is.null(linked) || !length(keys)) return(list())
  out <- lapply(keys, function(k)
    tryCatch(stratum_sparkline(linked, dims, k), error = function(e) NULL))
  names(out) <- keys
  out[!vapply(out, is.null, logical(1))]
}

# ------------------------------------------------------------
#  Excluding strata from a biology donor pool
# ------------------------------------------------------------

#' A donor condition with named strata removed
#'
#' The allocation groups hold their conditions as text and as a quosure,
#' and the two must stay identical or the table on screen stops matching
#' the script. Both are built here, from one string.
#'
#' The exclusion is a plain `!paste(...) %in% c(...)`, whatever produced
#' the key list - a coverage bar, a measurement bar, or a hand-picked
#' selection. `catchAllocationAoL()` evaluates the condition against CEF
#' rows, which carry no coverage column, so a threshold has to arrive
#' here already resolved into keys.
#'
#' @param clauses The clause list, as [alloc_cond_string()] takes.
#' @param excluded Stratum keys to drop, from [excluded_keys()].
#' @param dims The key columns those keys were built from.
#' @return A list with `text` and `quo`.
#' @seealso [excluded_keys()], [donor_exclusion_expr()]
#' @export
#' @examples
#' alloc_matched_cond(list(catchCategory = "Dis"), "TBB | 27.4.b",
#'                    c("metier4", "ICESDivision"))
alloc_matched_cond <- function(clauses, excluded = character(0), dims = NULL,
                               cov_min = NA, meas_min = NA) {
  base <- alloc_cond_string(clauses)
  ex <- exclusion_expr(cov_min = cov_min, meas_min = meas_min,
                       named = excluded, dims = dims)
  txt <- if (is.null(ex)) base
         else if (identical(base, "TRUE")) ex
         else paste0("(", base, ") & ", ex)
  list(text = txt, quo = rlang::new_quosure(str2lang(txt),
                                            env = rlang::global_env()))
}

#' Allocation conditions as a genuine partition
#'
#' `catchAllocationAoL()` rejects a run where any record matches more than
#' one allocStrataCond - "N records assigned to more than one stratum".
#' The app has always been first-match-wins on screen, so a catch-all
#' written as `quo(TRUE)` looked right and then failed at the engine,
#' because `TRUE` matches everything including what earlier groups took.
#'
#' The complements are made explicit here, exactly as [conds_from_rules()]
#' has always done on the discard side. bioMatchedCond is left alone:
#' donor pools may overlap and usually should.
#'
#' @param groups The allocation group list, in evaluation order.
#' @return A list with `allocStrataCond` and `bioMatchedCond`.
#' @seealso [conds_from_rules()], [emit_alloc_conditions()]
#' @export
alloc_conds_from_groups <- function(groups) {
  if (!length(groups)) return(list(allocStrataCond = list(),
                                   bioMatchedCond = list()))
  mk <- function(txt) rlang::new_quosure(rlang::parse_expr(txt),
                                         env = rlang::global_env())
  seen <- character(0)
  alloc <- vector("list", length(groups))
  txts  <- character(length(groups))
  for (i in seq_along(groups)) {
    this <- fld(groups[[i]], "alloc", "TRUE")
    if (!nzchar(this)) this <- "TRUE"
    txt <- if (!length(seen)) this
           else if (identical(this, "TRUE"))
             paste0("!(", paste(seen, collapse = " | "), ")")
           else paste0("(", this, ") & !(", paste(seen, collapse = " | "), ")")
    txts[i]   <- txt
    alloc[[i]] <- mk(txt)
    seen <- c(seen, paste0("(", this, ")"))
  }
  matched <- lapply(groups, function(g)
    mk(fld(g, "matched", fld(g, "alloc", "TRUE"))))
  simple <- stats::setNames(
    vapply(groups, function(g) { t <- fld(g, "alloc", "TRUE"); if (nzchar(t)) t else "TRUE" },
           character(1)), names(groups))
  names(alloc) <- names(matched) <- names(groups)
  list(allocStrataCond = alloc, bioMatchedCond = matched,
       allocStrataText = stats::setNames(txts, names(groups)),
       allocStrataSimple = simple)
}
