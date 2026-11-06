# =====================================================================
#  rcef_multi_area.R   (flag_multi_area(), split_multi_area(), and the
#                       check that the groups of a raising keep a domain whole)
#
#  ORIGIN: this project. Written here, free to refactor.
# =====================================================================

# ============================================================
#  Records that name several areas, fleets or seasons.
# ------------------------------------------------------------
#  The CEF lets a discard record list several codes in one field,
#  ';'-separated: one estimate for a set of areas whose landings are
#  reported one by one (areaValue "27.4.b;27.4.c;27.7.d;27.7.e"), or for
#  two fleets. Such a record is relevant for each of the codes and one
#  amount belongs to all of them together. Left as it is it is a cell of
#  its own with no landings, while the landings it goes with show no
#  discards; counted in every code it would be counted several times; and
#  a raising whose groups tell the areas apart takes its landings for
#  landings without an estimate and raises them again.
#
#    flag_multi_area()    finds them, and says what the split would do
#    split_multi_area()   one record for each code, the amount shared out
#                         by the landings of the record's domain, the
#                         distributions with it
#    .cut_domains()       what check_group_conditions() asks before a
#                         raising: is every domain with an estimate in ONE
#                         group?
#
#  DEPENDENCIES  Imports: dplyr.
# ============================================================

# the fields of the CEF that can list several codes, and what makes two records of one domain the same domain
.LIST_FIELDS  <- c("areaValue", "fleetValue", "seasonValue")
.DOMAIN_KEYS  <- c("vesselFlagCountry", "year", "workingGroup", "stock", "speciesCode", "originType", "variableType")

# The arguments the two functions share, checked once.
.multi_area_args <- function(x, by, weight, fn) {
  miss <- setdiff(c("catchCategory", "total"), names(x))
  if (length(miss))
    stop(fn, "(): `catch_data` needs the CEF catch columns ", paste(miss, collapse = ", "), call. = FALSE)
  by <- intersect(match.arg(by, .LIST_FIELDS, several.ok = TRUE), names(x))
  if (!length(by))
    stop(fn, "(): `catch_data` has none of the fields to look at (", paste(.LIST_FIELDS, collapse = ", "), ")", call. = FALSE)
  if (!(is.character(weight) && length(weight) == 1L && !is.na(weight)))
    stop(fn, "(): `weight` is \"landings\", \"equal\" or the name of a numeric column of `catch_data`", call. = FALSE)
  if (!weight %in% c("landings", "equal")) {
    if (!weight %in% names(x))
      stop(fn, "(): `weight` = \"", weight, "\" is neither \"landings\", \"equal\" nor a column of `catch_data`", call. = FALSE)
    if (!is.numeric(x[[weight]]))
      stop(fn, "(): the column `weight` = \"", weight, "\" is not numeric", call. = FALSE)
  }
  list(by = by, weight = weight)
}

#' Find the records that name several areas, fleets or seasons
#'
#' @description
#' The CEF lets a record list several codes in one field, `;`-separated: a
#' discard record can cover a set of areas whose landings are reported one by one
#' (`areaValue` `27.4.b;27.4.c;27.7.d;27.7.e`), or a set of fleets
#' (`GNS_DEF_all_0_0_all;GTR_DEF_all_0_0_all`). Such a record is relevant for each
#' of them, and its one amount belongs to all of them together. Left as it is, it
#' is a stratum of its own with no landings, while the landings it goes with show
#' no discards; and groups that tell the areas apart cut the domain that links it to
#' its landings - the raising would raise those landings again and count the
#' discards twice, so [catchRaising()] stops there. This function finds these
#' records and says what [split_multi_area()] would do with each of them, without
#' changing anything.
#'
#' @details
#' A record is flagged when one of the fields in `by` holds more than one code.
#' For each one the message and the table give its amount, the landings of its
#' domain (`domainCatchDis`, or `domainCatchBMS` for a BMS record) that fall in
#' each of the codes it lists - the weight its amount would be shared out by -, the
#' landings of the domain that fall outside the list, and, when `distribution_data`
#' is given, how many rows of the distributions belong to it. A record can be
#' split when it is not a landings record, it has a domain and the weights are
#' not all zero; when the weights are all zero the split is into equal parts, and
#' it says so.
#'
#' @param catch_data A CEF catch table.
#' @param distribution_data A CEF distributions table, optional: the rows of it
#'   that belong to a flagged record (through `domainBiology`, the country, the
#'   catch category ...) are counted.
#' @param by The fields to look at: any of `"areaValue"`, `"fleetValue"` and
#'   `"seasonValue"` that the catch has. Default: all of them.
#' @param weight What the amount of a record is shared out by, between the codes
#'   it lists. `"landings"` (default): the landings of the record's domain in each
#'   code, in tonnes. `"equal"`: the same share for every code. Or the name of a
#'   numeric column of `catch_data` - effort that was added to the landings
#'   records, say - summed over the landings records of the domain in each code.
#'   (A column named `landings` or `equal` cannot be used.)
#' @return A data frame with one row per flagged record - `row` (its row in
#'   `catch_data`), the country and year, the catch category, the fields it lists,
#'   `total` and `variableUnit`, its `domain`, the number of codes it lists
#'   (`cells`), the landings of the domain in them (`landings_rows`,
#'   `landings_t`) and outside them (`landings_outside_t`), the weight of each
#'   code (`weights`), `distribution_rows` (with `distribution_data`), whether it
#'   can be split (`split`) and a `note` where it cannot or is split into equal
#'   parts. No rows when no record lists several codes. The table of the codes
#'   and their shares is attached as `attr(, "cells")`: [split_multi_area()]
#'   reads it.
#' @seealso [split_multi_area()] to split them, [derive_fields()], which gives such
#'   a record the set of its codes at every level, [build_coverage_strata()].
#' @examples
#' \dontrun{
#'
#' # which records name several areas, and how would their amount be shared?
#' flag_multi_area(catch, distributions)
#'
#' # the same, by an effort that was joined to the landings records
#' flag_multi_area(catch, weight = "effort")
#' }
#' @md
#' @export
flag_multi_area <- function(catch_data, distribution_data = NULL,
                            by = c("areaValue", "fleetValue", "seasonValue"),
                            weight = "landings") {
  fn <- "flag_multi_area"
  x  <- as.data.frame(catch_data)
  a  <- .multi_area_args(x, by, weight, fn)
  by <- a$by; weight <- a$weight
  pcs  <- lapply(stats::setNames(by, by), function(f) .field_pieces(x[[f]]))
  n_pc <- matrix(unlist(lapply(pcs, lengths)), ncol = length(by), dimnames = list(NULL, by))
  listed <- which(rowSums(n_pc > 1L) > 0L)

  if (!is.null(distribution_data)) {
    dd   <- as.data.frame(distribution_data)
    link <- intersect(c("vesselFlagCountry", "year", "workingGroup", "stock", "speciesCode", "catchCategory", "domainBiology"),
                      intersect(names(x), names(dd)))
    k_d  <- biology_key(dd, link)
  }
  lan   <- which(x$catchCategory %in% "Lan")
  lan_t <- convert_units(x$total[lan], if ("variableUnit" %in% names(x)) x$variableUnit[lan], "t")
  lan_w <- switch(weight, landings = lan_t, equal = NULL, x[[weight]][lan])
  keys  <- intersect(.DOMAIN_KEYS, names(x))

  recs <- cells <- vector("list", length(listed))
  for (j in seq_along(listed)) {
    r    <- listed[j]
    ld   <- by[n_pc[r, ] > 1L]                                           # the fields this record lists
    vals <- lapply(stats::setNames(by, by), function(f) if (f %in% ld) pcs[[f]][[r]] else as.character(x[[f]][r]))
    cl   <- expand.grid(vals, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)   # one row per code (or combination)
    cl$label <- do.call(paste, c(unname(as.list(cl[ld])), sep = "|"))
    dcol <- if (x$catchCategory[r] %in% "BMS") "domainCatchBMS" else "domainCatchDis"
    dom  <- if (dcol %in% names(x)) as.character(x[[dcol]][r]) else NA_character_
    note <- ""; can <- TRUE
    w <- cl_t <- rep(0, nrow(cl)); n_lan <- 0L; out_t <- 0
    if (x$catchCategory[r] %in% "Lan") {
      note <- "a landings record: nothing to weight it by"; can <- FALSE
    } else if (is.na(dom)) {
      note <- "no domain: nothing links it to landings"; can <- FALSE
    } else {
      same <- as.character(x[[dcol]][lan]) %in% dom
      for (k in keys) same <- same & (x[[k]][lan] %in% x[[k]][r])
      L <- lan[same]; n_lan <- length(L)
      inside <- lapply(seq_len(nrow(cl)), function(i) {
        ok <- rep(TRUE, n_lan)
        for (f in ld) ok <- ok & (as.character(x[[f]][L]) %in% cl[[f]][i])
        ok
      })
      cl_t  <- vapply(inside, function(ok) sum(lan_t[same][ok], na.rm = TRUE), 0)
      out_t <- sum(lan_t[same][!Reduce(`|`, inside, rep(FALSE, n_lan))], na.rm = TRUE)
      w     <- if (identical(weight, "equal")) rep(1, nrow(cl)) else vapply(inside, function(ok) sum(lan_w[same][ok], na.rm = TRUE), 0)
      if (any(w < 0))
        stop(fn, "(): the weights of the record in row ", r, " are not all positive - a share cannot be negative", call. = FALSE)
      if (identical(weight, "equal")) note <- "equal parts"
      else if (sum(w) <= 0) note <- paste0("equal parts: the weight (", weight, ") is zero in every code it lists")
    }
    cl$weight_ <- w; cl$landings_t <- cl_t
    cl$share <- if (!can) NA_real_ else if (sum(w) > 0) w / sum(w) else rep(1 / nrow(cl), nrow(cl))
    cl$row <- r
    cells[[j]] <- cl[, c("row", by, "label", "landings_t", "weight_", "share"), drop = FALSE]
    names(cells[[j]])[names(cells[[j]]) == "weight_"] <- "weight"
    recs[[j]] <- data.frame(
      row = r,
      x[r, intersect(c("vesselFlagCountry", "year", "catchCategory", by), names(x)), drop = FALSE],
      listed = paste(ld, collapse = "+"), total = x$total[r],
      variableUnit = if ("variableUnit" %in% names(x)) as.character(x$variableUnit[r]) else NA_character_,
      domain = dom, cells = nrow(cl), landings_rows = n_lan, landings_t = sum(cl_t), landings_outside_t = out_t,
      weights = paste(sprintf("%s %s", cl$label, if (identical(weight, "landings")) sprintf("%.1f t", w) else vapply(w, format, "", digits = 3)), collapse = " | "),
      distribution_rows = if (!is.null(distribution_data)) sum(k_d == biology_key(x[r, , drop = FALSE], link)) else NA_integer_,
      split = can, note = note, stringsAsFactors = FALSE, check.names = FALSE)
  }
  out <- if (length(recs)) dplyr::bind_rows(recs) else
    data.frame(row = integer(0), listed = character(0), split = logical(0), stringsAsFactors = FALSE)
  if (is.null(distribution_data) && "distribution_rows" %in% names(out)) out$distribution_rows <- NULL
  rownames(out) <- NULL
  attr(out, "cells") <- if (length(cells)) dplyr::bind_rows(cells) else data.frame()

  if (!nrow(out)) {
    message(fn, "(): no record names several ", paste0(sub("Value$", "", by), "s", collapse = ", "), " in its ", paste(by, collapse = ", "), ".")
    return(out)
  }
  show <- utils::head(seq_len(nrow(out)), 8L)
  lines <- unlist(lapply(show, function(i) {
    o <- out[i, ]
    c(sprintf("  %s %s | %s %s | %s", if ("vesselFlagCountry" %in% names(o)) o$vesselFlagCountry else "", o$catchCategory, o$listed,
              paste(vapply(strsplit(o$listed, "+", fixed = TRUE)[[1]], function(f) as.character(o[[f]]), ""), collapse = " | "),
              paste(format(o$total, digits = 4), if (is.na(o$variableUnit)) "" else o$variableUnit)),
      if (!is.na(o$domain)) sprintf("      domain %s: %d landings records, %.1f t in the codes it lists, %.1f t outside", o$domain, o$landings_rows, o$landings_t, o$landings_outside_t),
      sprintf("      %s weight by code: %s", weight, o$weights),
      if (isTRUE(!is.na(o$distribution_rows))) sprintf("      distribution: %d rows", o$distribution_rows),
      sprintf("      -> %s", if (!o$split) paste0("cannot be split: ", o$note) else if (nzchar(o$note)) paste0("split into ", o$note) else "can be split"))
  }))
  message(fn, "(): ", nrow(out), " of ", nrow(x), " catch records name several codes in one field:\n", paste(lines, collapse = "\n"),
          if (nrow(out) > length(show)) paste0("\n  ... and ", nrow(out) - length(show), " more (all in the table)"),
          "\nsplit_multi_area() cuts each into one record for every code, the amount shared out by the weight above.")
  out
}

#' Split the records that name several areas, fleets or seasons
#'
#' @description
#' Cuts each record that lists several codes in one field (`areaValue`
#' `27.4.b;27.4.c;27.7.d;27.7.e`, see [flag_multi_area()] for which) into one
#' record for every code, and shares its amount out between them. Run it once, on
#' the raw data, before the coverage by area and before the raising: afterwards every
#' stratum has one area, one fleet and one season, and nothing downstream needs to
#' know that a record ever listed several - the strata add up to what the record
#' said, once.
#'
#' @details
#' ## The amount
#'
#' The `total` of each part is the record's `total` times the part's share, and the
#' shares add up to 1. The share of a code is the `weight` of that code over the
#' weight of all the codes the record lists - by default the landings of the
#' record's domain in it (a discard rate is a rate on landings, so each part keeps
#' the rate of the domain). A code with no weight gets no part. When no code has
#' any weight the record is split into equal parts, with a warning. A landings
#' record, and a record with no domain, cannot be weighted: they are left as they
#' are, with a warning. `variance` is the variance of the whole estimate and is
#' `NA` on the parts.
#'
#' ## The domains
#'
#' The raising takes one discard record per domain, so each part gets a domain of
#' its own - `<domainCatchDis> [<code>]` - and so do the landings records of the
#' domain that fall in that code. A domain that is not re-keyed would give the
#' parts the same key, and the raising would divide its discard ratio between them.
#' The landings of the domain that fall outside the codes the record lists keep
#' the old key and have no discard estimate any more: [flag_multi_area()] shows how
#' much there is.
#'
#' ## The distributions
#'
#' With `distribution_data`, the distribution of each split record is copied for
#' every part under the part's `domainBiology` (`<domainBiology> [<code>]`, on the
#' part and on its copy). The rows that are totals (`valueType` `"Total"`, or, when
#' the table has none, the `"Number"` rows) are shared out by the same shares, so
#' the fish add up; the mean weights stay as they are. A distribution that other
#' catch records use as well is not split - the parts keep sharing it - with a
#' warning. A distributions table that carries the fields of [derive_fields()]
#' (`derive_fields(catch, distributions, what = "distribution")`) is derived again
#' from the split catch.
#'
#' ## The sample
#'
#' What stands behind a record - `numTrips` and `numPSUs` of the catch, `numTrips`,
#' `numPSUs` and `numMeasurements` of the distributions - belongs to the record,
#' not to each of its codes: repeated in every part, a stratum pooled from the parts
#' would count the trips and fish several times. With `counts = "share"` (default)
#' they are shared out like the amount, so they add up to what the record had;
#' with `counts = "repeat"` every part keeps them whole.
#'
#' ## What else changes
#'
#' The part is its record with one code in the field(s) that were split; the fields
#' of [derive_fields()] that depend on them (`ICESSubArea`, `ICESDivision`,
#' `ICESSubdivision` for the area; `metier4`, `metier5`, `target`, `mesh`,
#' `metier4_mesh` for the fleet; `Stratum` for any) are derived again from it, and
#' the others are as they were. Two columns are added, on the parts: `splitFrom`
#' (the field(s) and the value(s) of the record they come from) and `splitShare`.
#' The parts are in the place of the record. Called again, on a table with
#' nothing left to split, it returns it as it is.
#'
#' @inheritParams flag_multi_area
#' @param distribution_data A CEF distributions table, optional: the distribution
#'   of each record that is split is split with it, see the section on the
#'   distributions. Give it in the same call - the parts and their distributions
#'   get the same keys.
#' @param by The fields to split on: any of `"areaValue"`, `"fleetValue"` and
#'   `"seasonValue"` that the catch has. Default: all of them - a record that lists
#'   several in more than one is split into every combination. A record that
#'   lists only in a field that is not in `by` is left as it is.
#' @param counts `"share"` (default) or `"repeat"`: what happens to the counts of
#'   the sample behind a record, see the section on the sample.
#' @return A list: `catch`, the catch with each splittable record replaced by its
#'   parts and the landings of its domains re-keyed, and `distribution`, the
#'   distributions with the same done (`NULL` when none were given). Both come
#'   back as the class they went in.
#' @seealso [flag_multi_area()] to see what it would do, [derive_fields()],
#'   [build_coverage_strata()], [catchRaising()].
#' @examples
#' \dontrun{
#'
#' catch <- derive_fields(catch)
#' flag_multi_area(catch, distributions)               # what is there, and how it would be shared
#' s <- split_multi_area(catch, distributions)         # by the landings of the domain
#' s2 <- split_multi_area(catch, distributions, weight = "equal")
#' catch         <- s$catch
#' distributions <- s$distribution
#' }
#' @md
#' @export
split_multi_area <- function(catch_data, distribution_data = NULL,
                             by = c("areaValue", "fleetValue", "seasonValue"),
                             weight = "landings", counts = c("share", "repeat")) {
  fn     <- "split_multi_area"
  counts <- match.arg(counts)
  x      <- as.data.frame(catch_data)
  a      <- .multi_area_args(x, by, weight, fn)
  fl     <- suppressMessages(flag_multi_area(x, distribution_data, by = a$by, weight = a$weight))
  tibble_in <- c(catch = inherits(catch_data, "tbl_df"), dist = inherits(distribution_data, "tbl_df"))
  if (!nrow(fl)) {
    message(fn, "(): no record names several ", paste0(sub("Value$", "", a$by), "s", collapse = ", "), " - nothing to split.")
    return(list(catch = catch_data, distribution = distribution_data))
  }
  for (i in which(!fl$split))
    warning(fn, "(): the record in row ", fl$row[i], " (", paste(fl$vesselFlagCountry[i], fl$catchCategory[i], fl$listed[i]), ") is left as it is: ",
            fl$note[i], call. = FALSE)
  for (i in which(fl$split & grepl("^equal parts: the weight", fl$note)))
    warning(fn, "(): the record in row ", fl$row[i], " is split into equal parts - ", sub("^equal parts: ", "", fl$note[i]), call. = FALSE)
  todo  <- fl[fl$split, , drop = FALSE]
  if (!nrow(todo)) return(list(catch = catch_data, distribution = distribution_data))
  cells <- attr(fl, "cells")
  keys  <- intersect(.DOMAIN_KEYS, names(x))
  if (!"splitFrom" %in% names(x))  x$splitFrom  <- NA_character_
  if (!"splitShare" %in% names(x)) x$splitShare <- NA_real_
  y <- x; y$.pos <- seq_len(nrow(y))
  dd <- if (is.null(distribution_data)) NULL else as.data.frame(distribution_data)
  if (!is.null(dd)) {
    link <- intersect(c("vesselFlagCountry", "year", "workingGroup", "stock", "speciesCode", "catchCategory", "domainBiology"),
                      intersect(names(x), names(dd)))
    k_x <- biology_key(x, link); k_d <- biology_key(dd, link)
  }
  share_cols <- function(z, cols, s) {
    if (identical(counts, "share")) for (col in intersect(cols, names(z))) if (is.numeric(z[[col]])) z[[col]] <- z[[col]] * s
    z
  }
  parts <- dparts <- vector("list", nrow(todo)); drop_d <- integer(0)
  for (i in seq_len(nrow(todo))) {
    r    <- todo$row[i]
    ld   <- strsplit(todo$listed[i], "+", fixed = TRUE)[[1]]
    cl   <- cells[cells$row == r & cells$share > 0, , drop = FALSE]
    dcol <- if (x$catchCategory[r] %in% "BMS") "domainCatchBMS" else "domainCatchDis"
    dom  <- as.character(x[[dcol]][r])
    new_key <- paste0(dom, " [", cl$label, "]")
    ## the landings of the domain follow their code: same domain, new key
    lan <- which(x$catchCategory %in% "Lan" & as.character(x[[dcol]]) %in% dom)
    for (k in keys) lan <- lan[x[[k]][lan] %in% x[[k]][r]]
    for (c in seq_len(nrow(cl))) {
      inc <- lan
      for (f in ld) inc <- inc[as.character(x[[f]][inc]) %in% cl[[f]][c]]
      y[[dcol]][inc] <- new_key[c]
    }
    ## the record: one row for every code
    nr <- x[rep(r, nrow(cl)), , drop = FALSE]
    for (f in ld) nr[[f]] <- cl[[f]]
    nr$total <- x$total[r] * cl$share
    if ("variance" %in% names(nr)) nr$variance <- NA_real_
    nr <- share_cols(nr, c("numTrips", "numPSUs"), cl$share)
    nr[[dcol]] <- new_key
    nr$splitFrom  <- paste0(paste(ld, collapse = "+"), ": ", paste(vapply(ld, function(f) as.character(x[[f]][r]), ""), collapse = " | "))
    nr$splitShare <- cl$share
    ## the derived fields that depend on what was split
    if ("fleetValue" %in% ld && "metier6" %in% names(nr)) {          # the CEF's own metier6 is cut as long as the others are
      n6 <- lengths(strsplit(x$metier6[!is.na(x$metier6) & !grepl(";", x$metier6, fixed = TRUE)], "_", fixed = TRUE))
      nr$metier6 <- if (length(n6)) {
        n6 <- as.integer(names(which.max(table(n6))))
        vapply(strsplit(nr$fleetValue, "_", fixed = TRUE), function(z) paste(z[seq_len(min(n6, length(z)))], collapse = "_"), "")
      } else NA_character_
    }
    der <- intersect(c(if ("areaValue" %in% ld) c("ICESSubArea", "ICESDivision", "ICESSubdivision"),
                       if ("fleetValue" %in% ld) c("metier4", "metier5", "target", "mesh", "metier4_mesh"), "Stratum"), names(nr))
    if (length(der)) nr[der] <- derive_fields(nr[, setdiff(names(nr), der), drop = FALSE])[der]
    nr$.pos <- r + seq_len(nrow(cl)) / (nrow(cl) + 1)
    ## the distribution of the record: one for every code, the totals shared out, the means as they were
    if (!is.null(dd) && !is.na(x$domainBiology[r])) {
      rows <- which(k_d == k_x[r])
      if (length(rows) && sum(k_x == k_x[r]) > 1L) {
        warning(fn, "(): the distribution of the record in row ", r, " is used by other catch records as well: it is not split, the parts share it",
                call. = FALSE)
      } else if (length(rows)) {
        nr$domainBiology <- paste0(x$domainBiology[r], " [", cl$label, "]")
        add <- if ("valueType" %in% names(dd)) dd$valueType[rows] %in% "Total" else dd$variableType[rows] %in% "Number"
        dparts[[i]] <- dplyr::bind_rows(lapply(seq_len(nrow(cl)), function(c) {
          z <- dd[rows, , drop = FALSE]
          z$domainBiology <- nr$domainBiology[c]
          z$value[add] <- z$value[add] * cl$share[c]
          z <- share_cols(z, c("numTrips", "numPSUs", "numMeasurements"), cl$share[c])
          z$.pos <- rows + c / (nrow(cl) + 1)
          z
        }))
        drop_d <- c(drop_d, rows)
      }
    }
    parts[[i]] <- nr
  }
  y   <- y[-todo$row, , drop = FALSE]
  nrs <- dplyr::bind_rows(parts)
  out <- dplyr::bind_rows(y, nrs)
  out <- out[order(out$.pos), , drop = FALSE]
  out$.pos <- NULL
  rownames(out) <- NULL
  dout <- distribution_data
  if (!is.null(dd)) {
    d2 <- dd; d2$.pos <- seq_len(nrow(d2))
    if (length(drop_d)) d2 <- d2[-drop_d, , drop = FALSE]
    d2 <- dplyr::bind_rows(d2, dplyr::bind_rows(dparts))
    d2 <- d2[order(d2$.pos), , drop = FALSE]
    d2$.pos <- NULL
    rownames(d2) <- NULL
    ## a distributions table that carries the fields of derive_fields() is derived again from the split catch
    proto <- data.frame(fleetValue = "A_B_C", areaValue = "27.4", vesselFlagCountry = "X", seasonValue = 1L, stringsAsFactors = FALSE)
    added <- setdiff(names(derive_fields(proto)), intersect("vesselFlagCountry", names(dd)))
    if (all(c("areaValue", "fleetValue", "seasonValue") %in% names(dd)))    # (what it says about shared distributions, it has said)
      d2 <- suppressMessages(derive_fields(out, d2[, setdiff(names(d2), added), drop = FALSE], what = "distribution"))
    dout <- d2
  }
  if (tibble_in[["catch"]]) out <- dplyr::as_tibble(out)
  if (!is.null(dout) && tibble_in[["dist"]]) dout <- dplyr::as_tibble(dout)
  message(fn, "(): ", nrow(todo), " record(s) split into ", nrow(nrs), " (", length(unique(todo$domain)), " domain(s) re-keyed; ",
          "weight: ", a$weight, "; the amount before and after: ",
          format(sum(todo$total, na.rm = TRUE), digits = 6), " / ", format(sum(nrs$total, na.rm = TRUE), digits = 6), ")")
  list(catch = out, distribution = dout)
}

# Do the groups of a raising keep every landing with the estimate of its domain? The raising takes the estimates from the rows of
# the raising stratum, and a landing has one when a discard row with a weight carries its domain (the same country, year, stock,
# origin, variable type and domainCatchDis) IN ITS OWN stratum. A landing whose domain has an estimate in another stratum, or in
# none, looks like a landing without one: it is raised again, and the estimate is counted twice - or, in no stratum, left out. A
# domain whose estimates and landings are in several strata but each landing in a stratum with an estimate of its own (one
# estimate for each season, say) is fine. A record that lists several areas is the usual cause of the other kind (groups that tell
# the areas apart cut the domain), split_multi_area() the cure. `cond_st` is the list of the groups' conditions as logical
# vectors, as check_group_conditions() has them. Gives the text of the error, NULL when there is none.
.cut_domains <- function(catch_data, cond_st, domain, originType = "unspecified", variableType = "unspecified") {
  est <- switch(domain, domainCatchDis = "Dis", domainCatchBMS = "BMS", return(NULL))
  x   <- as.data.frame(catch_data)
  if (!all(c("catchCategory", "total", domain) %in% names(x)) || !length(cond_st)) return(NULL)
  nm  <- names(cond_st); if (is.null(nm)) nm <- paste0("G", seq_along(cond_st))
  grp <- rep("(no group)", nrow(x))
  for (g in rev(seq_along(cond_st))) grp[which(cond_st[[g]])] <- nm[g]     # in two groups is the other error: the first wins here
  use <- !is.na(x[[domain]]) & x$catchCategory %in% c("Lan", est)
  if (!originType %in% "unspecified" && "originType" %in% names(x))     use <- use & x$originType %in% originType
  if (!variableType %in% "unspecified" && "variableType" %in% names(x)) use <- use & x$variableType %in% variableType
  keys <- intersect(.DOMAIN_KEYS, names(x))
  d <- data.frame(x[use, intersect(c("vesselFlagCountry", "year"), names(x)), drop = FALSE], domain = as.character(x[[domain]][use]),
                  cat = x$catchCategory[use], group = grp[use],
                  t = convert_units(x$total[use], if ("variableUnit" %in% names(x)) x$variableUnit[use], "t"),
                  has = x$catchCategory[use] %in% est & !is.na(x$total[use]), stringsAsFactors = FALSE)
  d$key <- do.call(paste, c(unname(as.list(x[use, c(keys, domain), drop = FALSE])), sep = "\r"))
  has_est  <- tapply(d$has, d$key, any)                                       # the domains with an estimate
  est_here <- paste(d$key, d$group)[d$has & d$group != "(no group)"]          # ... and the strata that hold one
  stranded <- d$cat == "Lan" & d$group != "(no group)" & has_est[d$key] & !paste(d$key, d$group) %in% est_here
  cut <- unique(d$key[stranded])                                              # landings in a stratum with no estimate of their domain
  if (!length(cut)) return(NULL)
  where <- function(rows) paste(sort(unique(d$group[rows])), collapse = " | ")
  lines <- vapply(cut, function(k) {
    i <- match(k, d$key)
    sprintf("%s: %s %.3g t in %s, landings %.3g t in %s (no estimate there)", paste(c(if ("vesselFlagCountry" %in% names(d)) d$vesselFlagCountry[i], d$domain[i]), collapse = " | "),
            if (est == "Dis") "discards" else "BMS", sum(d$t[d$key == k & d$cat == est]), where(d$key == k & d$cat == est),
            sum(d$t[d$key == k & stranded]), where(d$key == k & stranded))
  }, "")
  paste0(length(cut), " of ", sum(has_est), " domains with a ", if (est == "Dis") "discard" else "BMS", " estimate are cut by the groups - some of their landings are in a ",
         "raising stratum that has no estimate of the domain, so they would be raised again and the estimate counted twice (or left out): ",
         paste(utils::head(lines, 5), collapse = "; "), if (length(lines) > 5) paste0("; and ", length(lines) - 5, " more"),
         ". Keep a domain's landings with its estimate; a record that lists several areas is split with split_multi_area() first.")
}
