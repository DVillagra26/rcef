# =====================================================================
#  rcef_length.R   (the distributions: normalising, matching to the catch, numbers and mean weight at length / age)
#
#  ORIGIN: this project (ILVO / RCEF Workbench). Freely refactorable.
#  Was: R_development/mod_length.R
# =====================================================================

# ============================================================
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
#  biology_dist_prep() does. A plain average would silently
#  over-weight a stratum that measured five fish.
# ============================================================

# Normalise a distributions table. Everything is converted once, on the
# way in, to the PRESENTATION units used everywhere: length class
# in cm and mean weight in g. A file may mix mm and cm, or g and kg, and
# a stratum measured in mm must never end up on the same axis as one in
# cm - so this is not offered as a choice, it is enforced here.
# .class / .value_std are the only fields the rest of the package reads.
#
# A record repeated exactly is not a second sample: summed, it doubles the
# numbers, over-weights the mean weight and doubles the fish counted. The
# repeats are dropped, and said so, because the file is what needs fixing.
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
    d$.class_mm <- convert_units(raw, cls_u, "mm", "distributionClass")
    d$.class <- d$.class_mm / 10
  }
  d$.dist_type <- type[1]

  v <- suppressWarnings(as.numeric(d$value))
  is_n <- d$variableType %in% "Number"
  d$.value_std <- NA_real_
  d$.value_std[is_n]  <- convert_units(v[is_n], var_u[is_n], "N", "Number")
  d$.value_std[!is_n] <- convert_units(v[!is_n], var_u[!is_n], "g", "WeightLive")

  # what the row actually is, so callers never have to re-derive it
  d$.var_kind <- ifelse(is_n, "numbers", "meanweight")
  d$.n_meas <- if ("numMeasurements" %in% names(d))
    suppressWarnings(as.numeric(d$numMeasurements)) else NA_real_
  # a distribution the allocation left as it was is a sampled one
  if ("sampledOrEstimated" %in% names(d))
    d$sampledOrEstimated[is.na(d$sampledOrEstimated)] <- "sampled"

  # data.table's duplicated(): the data.frame method pastes every row together and
  # took two seconds on a 100 000-row allocation result - on every plot
  dup <- duplicated(data.table::as.data.table(d))
  if (any(dup)) {
    warning(sprintf(paste("%s distribution row(s) repeat another row exactly and",
                          "were dropped, so that nothing is counted twice - check",
                          "the distributions file"),
                    format(sum(dup), big.mark = ",")), call. = FALSE)
    d <- d[!dup, , drop = FALSE]
  }
  d
}

# One string per row for some columns, to test membership with. `keys` is the
# columns (a character vector), or the table to be matched with - a data frame,
# or NULL for none - and then it is the columns a catch and its distributions are
# matched on, which the result also carries as attr(, "keys").
#
# A distribution belongs to the country that submitted it. A domainBiology
# carries no country (the same "IC_1_27.4_TBB_DEF_70-99_0_0_all" names a
# stratum in Germany and in the Netherlands), so catch and distributions
# are matched on the domain, the catch category AND the country - as the
# RDBESStockCoord engines do: the country only when both tables carry it.
#' @noRd
#' @export
biology_key <- function(x, keys) {
  matched <- !is.character(keys)
  if (matched)
    keys <- c("domainBiology", "catchCategory",
              if ("vesselFlagCountry" %in% intersect(names(x), names(keys)))
                "vesselFlagCountry")
  k <- if (length(keys))
    do.call(paste, c(lapply(keys, function(k) as.character(x[[k]])), sep = "\r"))
  else rep("", nrow(x))
  if (matched) attr(k, "keys") <- keys
  k
}

#' The numbers or the mean weight at each length / age class, for the plots
#'
#' @description
#' The data half of [plot_NaLoA()] and [plot_MWaLoA()], before the allocation
#' and after it: when `distributions` is given, each distribution row is given
#' the stratum columns of the catch it belongs to (through `domainBiology`, the
#' catch category and the country), the classes are pooled into a plus group or
#' into bins when asked for, and the rows are summed (numbers) or averaged
#' (mean weight) over whatever is not plotted; otherwise `data` is already a
#' summarised table and is used unchanged. Numbers come back in thousands; the
#' axis labels that match come with them.
#'
#' After the allocation the distributions carry `sampledOrEstimated`, and it is
#' a grouping column like any other: asked for in `grp` it splits the rows into
#' the sampled and the estimated ones, left out of `grp` it pools them. The
#' estimated distributions of strata with no biological domain carry a domain
#' the allocation made up, which only the allocation's own `$catch` holds, so
#' that is the catch to give.
#'
#' @details
#' Numbers add up, so summing over whatever is not plotted is correct. A mean
#' weight is not additive: pooling strata takes a mean weighted by the numbers
#' measured at that class, so a stratum that measured five fish cannot
#' outweigh one that measured five hundred (a distribution that reports no
#' numbers at all falls back to `numMeasurements`, then to an unweighted mean;
#' a class with no fish in it has no weight, so no point). Because there is
#' always some grouping, the spread across
#' whatever was collapsed is carried too: `sd_weight` is the number-weighted
#' standard deviation of the strata that went into each point and `n_strata`
#' says how many there were; a point built from one stratum has no spread and
#' gets `NA` rather than a misleading zero.
#'
#' `n_fish` is how many fish stand behind each point. `numMeasurements` is one
#' count for a whole distribution (it repeats on every class row), so it is
#' shared out over the classes in proportion to the numbers at each: exact for
#' a distribution raised from a length sample by a single factor, an estimate
#' for ages read off an age-length key. A distribution counts once however many
#' stratum keys it is linked to; one that reports no count contributes none.
#'
#' Ages from `plus_group` upward are pooled into one plus class, and length
#' classes into fixed-width bins, distribution by distribution (each domain of
#' each country, in each stratum key it is linked to): the numbers are added
#' up and the mean weights averaged weighted by those numbers - the rule used
#' to pool strata, applied first inside each distribution, so the spread across
#' strata that is plotted later is still a spread across strata and not across
#' ages. The pooled class is labelled with the first age (`from`) or with the
#' lower edge of its bin (`bin_width = 5` turns class 12 into 10).
#'
#' @param data A summarised numbers / mean-weight table, or CEF catch with the
#'   columns you group by already on it - [derive_fields()] run once, or your own
#'   (after the allocation: the `$catch` of its result).
#' @param distributions A CEF distributions table (after the allocation: the
#'   `$distribution` of its result), or `NULL`.
#' @param distributionType `"LengthTotal"` or `"Age"`.
#' @param catchCategory `"Lan"`, `"Dis"`, `"BMS"` or `"All"`.
#' @param quantity `"numbers"` or `"meanweight"`.
#' @param grp Grouping columns for the summary: of the catch or of the
#'   distributions (`sampledOrEstimated`).
#' @param plus_group The first age pooled into the plus class (ages only), or
#'   `NULL`; `bin_width` the width, in cm, of the bins the length classes are
#'   pooled into (lengths only), or `NULL`. As in [plot_NaLoA()]. Given
#'   without `distributions` (a summarised table cannot be pooled again) they
#'   only warn.
#' @param bin_width See `plus_group`.
#' @param fn The calling function's name, for messages (with its colon).
#' @return `list(data, class_label, value_label, plus_from, bin_from,
#'   status)`; `status` is which of `sampled` / `estimated` the distributions
#'   drawn were (empty when the distributions carry no `sampledOrEstimated`).
#'   `data` is `NULL` when there is nothing to draw.
#' @keywords internal
#' @noRd
#' @export
biology_dist_prep <- function(data, distributions, distributionType,
                              catchCategory, quantity, grp,
                              plus_group = NULL, bin_width = NULL, fn = "") {
  # ---- what is asked for: a plus group of ages, bins of lengths ------------------
  has_dist <- !is.null(distributions)
  plus_from <- NULL
  if (!is.null(plus_group)) {
    if (!(is.numeric(plus_group) && length(plus_group) == 1 &&
          is.finite(plus_group) && plus_group >= 0))
      stop(fn, "`plus_group` is the first age of the plus group - a single ",
           "number (plus_group = 7 pools ages 7 and older) - or NULL for ",
           "none", call. = FALSE)
    if (!has_dist)
      warning(fn, "plus_group only applies when `distributions` is supplied ",
              "(a summarised table cannot be pooled again)", call. = FALSE)
    else if (!identical(distributionType, "Age"))
      stop(fn, "plus_group only applies to ages (distributionType = ",
           "\"Age\"): lengths have no plus group", call. = FALSE)
    else plus_from <- plus_group
  }
  bin_from <- NULL
  if (!is.null(bin_width)) {
    if (!(is.numeric(bin_width) && length(bin_width) == 1 &&
          is.finite(bin_width) && bin_width > 0))
      stop(fn, "`bin_width` is a single positive number - a bin width in cm ",
           "(bin_width = 5 groups length into 5cm bins) - or NULL for none",
           call. = FALSE)
    if (!has_dist)
      warning(fn, "bin_width only applies when `distributions` is supplied ",
              "(a summarised table cannot be pooled again)", call. = FALSE)
    else if (!identical(distributionType, "LengthTotal"))
      stop(fn, "bin_width only applies to lengths (distributionType = ",
           "\"LengthTotal\"): ages use plus_group instead", call. = FALSE)
    else bin_from <- bin_width
  }
  # prep_distributions() already normalised the length class to cm and the
  # value to N / g; numbers are presented in thousands on top of that
  lab <- list(class_label = if (identical(distributionType, "Age")) "Age (years)"
                            else "Length (cm)",
              value_label = if (identical(quantity, "numbers")) "Numbers (thousands)"
                            else "Mean weight (g)",
              value_scale = if (identical(quantity, "numbers")) 1e-3 else 1)
  S <- "sampledOrEstimated"
  result <- function(out, status)
    list(data = out, class_label = lab$class_label, value_label = lab$value_label,
         plus_from = plus_from, bin_from = bin_from, status = status)
  if (!has_dist)
    return(result(data, if (S %in% names(data)) unique(as.character(data[[S]]))
                        else character(0)))
  if (!is.data.frame(data))
    stop(fn, "`data` is the catch table",
         if (is.list(data) && all(c("catch", "distribution") %in% names(data)))
           paste0(" - not the result of the allocation itself: give its two ",
                  "tables, its $catch as `data` and its $distribution as ",
                  "`distributions`"), call. = FALSE)

  # ---- each distribution row, with the stratum columns of its catch -------------
  parts <- catch_category_parts(catchCategory)
  d  <- as.data.frame(data)       # the catch as it stands: its own fields, derived once
  pd <- prep_distributions(distributions, distributionType)
  if (!is.null(pd)) pd <- pd[pd$catchCategory %in% parts, , drop = FALSE]
  if (is.null(pd) || !nrow(pd)) {
    have <- unique(as.character(distributions$distributionType))
    message(fn, "no ", distributionType, " distributions for ",
            paste(parts, collapse = " + "),
            if (!distributionType %in% have)
              paste0(" (it has ", paste(have, collapse = ", "), ")"))
    return(result(NULL, character(0)))
  }
  absent <- setdiff(grp, union(names(d), names(pd)))
  if (length(absent))
    stop(fn, "the catch and the distributions have no column ",
         paste0("'", absent, "'", collapse = ", "), derive_hint(absent), call. = FALSE)
  estimated <- S %in% names(pd) && any(pd[[S]] %in% "estimated")
  # the estimated distributions of a stratum with no biological domain carry a
  # domain the allocation made up, which only its own catch has: the catch that
  # went into the allocation cannot find them, and the numbers would silently
  # lack them
  if (estimated && !"allocGroup" %in% names(d))
    warning(fn, "the distributions hold estimates, but `data` is not the ",
            "allocation's own $catch (it has no allocGroup): the estimated ",
            "distributions of strata with no biological domain are found only ",
            "there, so most of them are missing here", call. = FALSE)
  dims <- intersect(unique(c("catchCategory", "fleetValue", "vesselFlagCountry",
                             "seasonValue", grp)), names(d))
  linked <- NULL
  if ("domainBiology" %in% names(d)) {
    d  <- d[d$catchCategory %in% parts, , drop = FALSE]
    if (nrow(d)) {
      # one row per domain x category x country, carrying the stratum keys. A
      # domain can be used by several strata of one country (a distribution
      # covers every catch row that names it), so the keys are taken from the
      # catch rows and de-duplicated rather than assumed unique. Without the
      # country in the match, a domain that two countries both use would hand
      # each country the other's distributions too.
      by <- attr(biology_key(d, pd), "keys")
      if ("vesselFlagCountry" %in% by) {
        d$vesselFlagCountry  <- as.character(d$vesselFlagCountry)
        pd$vesselFlagCountry <- as.character(pd$vesselFlagCountry)
      }
      keymap <- d %>%
        filter(!is.na(domainBiology), nzchar(as.character(domainBiology))) %>%
        select(all_of(by), all_of(dims)) %>%
        distinct()
      if (nrow(keymap)) {
        # Distributions that went through derive_fields(.., what = "distribution")
        # carry these columns already. The ones taken from the catch here are
        # the ones used, so the distributions' copies go rather than being
        # joined to a .x / .y pair that no `dims` name would find.
        pd <- pd[, setdiff(names(pd), setdiff(dims, by)), drop = FALSE]
        linked <- inner_join(pd, keymap, by = by, relationship = "many-to-many")
      }
    }
  }

  # ---- pooled: a plus group (ages) or bins (lengths) ------------------------------
  # Both pool the rows `hit` into one class each (`new_class`), distribution
  # instance by distribution instance. The pooled rows are marked - `.plus`,
  # or the bin width itself in `.bin_width` - so the summary says so too.
  pool_to <- function(lk, hit, new_class, mark, mark_value, mark_rest) {
    hit <- hit & is.finite(lk$.class)
    if (!any(hit)) return(lk)
    id  <- c(intersect(c("domainBiology", "catchCategory", "vesselFlagCountry"),
                       names(lk)), intersect(unique(c(dims, grp)), names(lk)))
    gid <- function(x) paste(biology_key(x, id), new_class(x$.class), sep = "\r")
    n <- lk[hit & lk$.var_kind == "numbers", , drop = FALSE]
    w <- lk[hit & lk$.var_kind == "meanweight", , drop = FALSE]
    # numbers add up
    gn <- gid(n)
    pn <- n[!duplicated(gn), , drop = FALSE]
    pn$.value_std <- unname(tapply(n$.value_std, gn, sum, na.rm = TRUE)[gid(pn)])
    # a mean weight is weighted by the numbers at its own original class; an
    # instance with no numbers to weigh by is averaged plainly
    gw <- gid(w)
    wt <- n$.value_std[match(paste(gw, w$.class), paste(gn, n$.class))]
    wt[!is.finite(wt) | wt < 0 | !is.finite(w$.value_std)] <- 0
    none <- unname(tapply(wt, gw, sum)[gw]) <= 0
    wt[none & is.finite(w$.value_std)] <- 1
    pw <- w[!duplicated(gw), , drop = FALSE]
    pw$.value_std <- unname((tapply(wt * ifelse(wt > 0, w$.value_std, 0), gw, sum) /
                               tapply(wt, gw, sum))[gid(pw)])
    pooled <- rbind(pn, pw)
    pooled$.class <- new_class(pooled$.class)
    if ("distributionClass" %in% names(pooled))
      pooled$distributionClass <- if (is.character(lk$distributionClass))
        as.character(pooled$.class) else pooled$.class
    rest <- lk[!hit, , drop = FALSE]
    if (is.null(rest[[mark]])) rest[[mark]] <- rep(mark_rest, nrow(rest))
    pooled[[mark]] <- mark_value
    rbind(rest, pooled)
  }
  if (!is.null(linked) && !is.null(plus_from))
    linked <- pool_to(linked, linked$.class >= plus_from, function(cl) rep(plus_from, length(cl)),
                      ".plus", TRUE, FALSE)
  if (!is.null(linked) && !is.null(bin_from))
    linked <- pool_to(linked, rep(TRUE, nrow(linked)),
                      function(cl) floor(cl / bin_from) * bin_from,
                      ".bin_width", bin_from, NA_real_)

  # ---- summed (numbers) or averaged (mean weight) over what is not plotted ----------
  out <- NULL
  keys <- unique(c(".class", grp))
  if (!is.null(linked) && nrow(linked)) {
    has_plus <- ".plus" %in% names(linked)
    has_bin  <- ".bin_width" %in% names(linked)
    if (identical(quantity, "numbers")) {
      nb <- linked[linked$.var_kind == "numbers", , drop = FALSE]
      if (nrow(nb))
        out <- nb %>% group_by(across(all_of(keys))) %>%
          summarise(number = sum(.value_std, na.rm = TRUE),
                    n_meas = sum(.n_meas, na.rm = TRUE),
                    plus = if (has_plus) any(.plus %in% TRUE) else NA,
                    bin_width = if (has_bin) .bin_width[1] else NA_real_,
                    .groups = "drop") %>%
          filter(number > 0) %>% as.data.frame()
    } else {
      mw <- linked[linked$.var_kind == "meanweight", , drop = FALSE]
      nb <- linked[linked$.var_kind == "numbers", , drop = FALSE]
      if (nrow(mw)) {
        join_keys <- intersect(c("domainBiology", "catchCategory", ".class",
                                 "vesselFlagCountry"), names(mw))
        inst <- setdiff(join_keys, ".class")
        if (nrow(nb)) {
          nb2 <- nb %>% select(all_of(join_keys), .w_num = .value_std) %>%
            group_by(across(all_of(join_keys))) %>%
            summarise(.w_num = sum(.w_num, na.rm = TRUE), .groups = "drop") %>%
            # the share of a distribution's numbers that sits at each class
            group_by(across(all_of(inst))) %>%
            mutate(.share = if (sum(.w_num) > 0) .w_num / sum(.w_num) else NA_real_) %>%
            ungroup()
          mw <- mw %>% left_join(nb2, by = join_keys)
        } else {
          mw$.w_num <- NA_real_
          mw$.share <- NA_real_
        }
        # The numbers at the class weigh the mean weight there. A distribution
        # that reports no numbers at all falls back on numMeasurements, then on
        # an unweighted mean; one that has numbers, but none at this class, has
        # no fish here - no weight, and no point (the allocation writes a mean
        # weight at every class of an estimated distribution, fish or not).
        mw$.w <- if (nrow(nb)) {
          has_nb <- biology_key(mw, inst) %in% biology_key(nb, inst)
          ifelse(has_nb, ifelse(is.na(mw$.w_num), 0, mw$.w_num),
                 ifelse(!is.na(mw$.n_meas) & mw$.n_meas > 0, mw$.n_meas, 1))
        } else ifelse(!is.na(mw$.n_meas) & mw$.n_meas > 0, mw$.n_meas, 1)
        mw <- mw[mw$.w > 0, , drop = FALSE]
        mw$.n_fish <- mw$.n_meas * mw$.share
        mw$.inst   <- biology_key(mw, inst)
        out <- mw %>% group_by(across(all_of(keys))) %>%
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
                    n_fish = if (all(is.na(.n_fish))) NA_real_
                             else sum(.n_fish[!duplicated(.inst)], na.rm = TRUE),
                    weight_basis = sum(.w, na.rm = TRUE),
                    plus = if (has_plus) any(.plus %in% TRUE) else NA,
                    bin_width = if (has_bin) .bin_width[1] else NA_real_,
                    .groups = "drop") %>%
          filter(is.finite(mean_weight)) %>% as.data.frame()
      }
    }
    if (!is.null(out)) {
      if (!has_plus) out$plus <- NULL
      if (!has_bin) out$bin_width <- NULL
    }
  }
  if (!is.null(out) && nrow(out) && identical(quantity, "numbers"))
    out$number <- out$number * lab$value_scale
  result(out, if (!is.null(linked) && S %in% names(linked))
                unique(as.character(linked[[S]])) else character(0))
}
