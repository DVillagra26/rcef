# =====================================================================
#  rcef_utils.R   (derive_fields(), unit conversion)
#
#  ORIGIN: this project (ILVO / RCEF Workbench). Freely refactorable.
#  Was: R_development/0_Functions_utils.R
# =====================================================================

# ============================================================
#  Field derivation and unit handling.
# ------------------------------------------------------------
#  The base of the stack: everything else (the coverage table, the plots,
#  the allocated plots) assumes these. It holds no state and draws no plots.
#
#  WHAT IS HERE
#    derive_fields()      split the CEF fleet and area codes into the nested
#                         levels an assessment actually groups by;
#    convert_units()      one unit conversion for the whole package: weights
#                         to tonnes, numbers to N, mean weights to g, length
#                         classes to mm.
#
#  DEPENDENCIES  Imports: dplyr, rlang.
# ============================================================

# ----------------------------------------------------------
#  Deriving the nested stratum fields
# ----------------------------------------------------------

#' Derive the grouping columns from a CEF catch table - or from its distributions
#'
#' Adds the columns an assessment groups by, none of which the CEF format
#' carries directly: the nested metier levels (`metier4` to `metier6`),
#' `target` and `mesh` from `fleetValue`; `metier4_mesh`, the gear and the
#' mesh joined (`"TBB_70-99"`; just the gear when the fleet code has no mesh
#' part); the three ICES area levels from `areaValue`; and `Stratum`, the four
#' native keys joined. Country and season are not among them -
#' `vesselFlagCountry` and `seasonValue` already name them, so a `Country` /
#' `Season` alias would only be a second name for the same column.
#'
#' Run it once, on the raw CEF data, and build on the result. The coverage
#' table and the plots read the columns your catch has and derive nothing
#' themselves, so a column you add (a fleet grouping of your own, say) or
#' regroup (your own `metier4`) is used as it stands. A column the catch
#' already has is left as it is - an edit of yours, or a column from an earlier
#' call - so calling it again, or deriving the distributions from a catch you
#' have edited, never undoes the edit; drop a column first to have it derived
#' afresh.
#'
#' An empty field of a CEF csv arrives as `""` in a text column when the file is
#' read with `read.csv()` or `read.taf()` (and as `NA` when it is read with
#' readr). Empty text is missing in the CEF format, so it comes back as `NA` - on
#' the catch and, with `what = "distribution"`, on the distributions - and the
#' raising, the allocation and the link between catch and distributions all see
#' the same missing id. Left as `""`, every catch row with no `domainBiology`
#' would share the domain `""` and be taken for one distribution.
#'
#' An ICES area code is a nested path (`27.4.b.1`), so it is split into
#' subarea, division and subdivision. A code already coarser than the level
#' asked for REPEATS rather than becoming `NA`: `27.4` is its own division and
#' subdivision as far as the data goes, so a stratum reported at that
#' resolution is never silently dropped from a finer grouping.
#'
#' With `what = "distribution"` the same fields come back on the distributions
#' table instead: `derive_fields(catch_data, distribution_data, what =
#' "distribution")`. `derive_fields(catch_data)` is the catch, as it always was.
#'
#' @details
#' A distribution carries none of the stratum columns - the fleet, the area
#' and the season belong to the catch - so deriving its fields needs both
#' tables. Each distribution row is linked to the catch rows it describes
#' through `domainBiology`, together with the catch category and every other
#' column both tables carry among the ones the allocation itself matches on
#' (`vesselFlagCountry`, `year`, `workingGroup`, `stock`, `speciesCode`: a
#' `domainBiology` names no country, so two countries can use the same one).
#' The row takes the `fleetValue`, `areaValue` and `seasonValue` of those
#' catch rows and every field derived from them. The derivation is the one
#' described above, run once on the catch: the distributions only look its
#' results up, so the two tables can never disagree about what a field means.
#'
#' A `domainBiology` can be used by catch rows of several strata (a
#' distribution covers every catch row that names it). The fields those rows
#' share are kept and the ones they differ on are `NA`, with a message, rather
#' than one of them being picked at random. A distribution row with no catch
#' row behind it gets `NA` throughout, with a warning: the two files disagree.
#' The rows of `distribution_data` come back as they went in (empty text apart), same
#' number and same order; columns of the same name already on it (an earlier
#' call, say) are replaced.
#'
#' @param catch_data A CEF catch table (data frame).
#' @param distribution_data A CEF distributions table. Only with
#'   `what = "distribution"`, which needs it.
#' @param what `"catch"` (default): `catch_data` with the derived columns.
#'   `"distribution"`: `distribution_data` with them.
#' @return With `what = "catch"`, `catch_data` with the derived columns added.
#'   With `what = "distribution"`, `distribution_data` with `fleetValue`,
#'   `areaValue`, `seasonValue` (and `vesselFlagCountry`, when the
#'   distributions have none of their own) and the derived columns added.
#' @seealso [build_coverage_strata()]
#' @examples
#' \dontrun{
#'
#' catch <- derive_fields(catch)
#' head(catch[, c("fleetValue", "metier4", "metier4_mesh", "ICESDivision")], 3)
#'
#' # the distributions get the same fields, read off the catch on domainBiology
#' dist <- derive_fields(catch, distributions, what = "distribution")
#' head(dist[, c("domainBiology", "fleetValue", "metier4_mesh", "ICESDivision")], 3)
#' }
#' @export
derive_fields <- function(catch_data, distribution_data = NULL,
                          what = c("catch", "distribution")) {
  what <- match.arg(what)
  stopifnot(is.data.frame(catch_data))
  if (identical(what, "catch") && !is.null(distribution_data))
    stop("derive_fields(): `distribution_data` is only read with what = ",
         "\"distribution\", which hands the distributions back with the ",
         "derived fields", call. = FALSE)
  if (identical(what, "distribution")) {
    if (!is.data.frame(distribution_data))
      stop("derive_fields(): what = \"distribution\" needs `distribution_data`, ",
           "the distributions table", call. = FALSE)
    link_on <- c("domainBiology", "catchCategory")
    tables  <- list(catch_data = catch_data, distribution_data = distribution_data)
    for (nm in names(tables)) {
      miss <- setdiff(link_on, names(tables[[nm]]))
      if (length(miss))
        stop("derive_fields(): `", nm, "` has no ",
             paste0("'", miss, "'", collapse = " or "),
             " column, which the two tables are linked on", call. = FALSE)
    }
  }
  need <- c("fleetValue", "areaValue", "vesselFlagCountry", "seasonValue")
  miss <- setdiff(need, names(catch_data))
  if (length(miss)) {
    if ("distributionType" %in% names(catch_data))
      stop("derive_fields(): `catch_data` is a distributions table - it has no ",
           paste(miss, collapse = ", "), ", which belong to the catch. To ",
           "derive the distributions give both: derive_fields(catch_data, ",
           "distribution_data, what = \"distribution\")", call. = FALSE)
    stop("Missing expected column(s): ", paste(miss, collapse = ", "))
  }

  # An empty field of a csv read with read.csv() or read.taf() is "" in a text column, not NA,
  # while the raising, the allocation and catch_domain() tell a missing id by NA: every catch row
  # with no domainBiology would share the domain "" and be taken for one distribution. Empty
  # text is missing in the CEF format, so it becomes NA, on both tables, here.
  na_text <- function(x) {
    x[] <- lapply(x, function(v) if (is.character(v)) replace(v, which(v == ""), NA_character_) else v)
    x
  }
  catch_data <- na_text(catch_data)
  if (identical(what, "distribution")) distribution_data <- na_text(distribution_data)

  # A column the catch already has is kept as it is - the caller's own regrouping, or a
  # column from an earlier call - and the codes are split only when some column still
  # needs them, so deriving a catch that is already derived costs nothing.
  d <- catch_data
  put <- function(col, value) if (!col %in% names(d)) d[[col]] <<- value
  tok <- area <- NULL
  fleet_tok <- function() {
    if (is.null(tok)) tok <<- strsplit(as.character(catch_data$fleetValue), "_", fixed = TRUE)
    tok
  }
  area_tok <- function() {
    if (is.null(area)) area <<- strsplit(as.character(catch_data$areaValue), ".", fixed = TRUE)
    area
  }
  tok_at <- function(i) vapply(fleet_tok(), function(z)
    if (length(z) >= i) z[i] else NA_character_, character(1))
  tok_join <- function(n) vapply(fleet_tok(), function(z)
    paste(z[seq_len(min(n, length(z)))], collapse = "_"), character(1))
  area_join <- function(n) vapply(area_tok(), function(z) {
    if (!length(z) || all(is.na(z))) return(NA_character_)
    paste(z[seq_len(min(n, length(z)))], collapse = ".")
  }, character(1))

  put("metier4", tok_join(1))                # GNS
  put("metier5", tok_join(2))                # GNS_DEF
  put("metier6", tok_join(3))                # GNS_DEF_120-219
  put("target",  tok_at(2))
  put("mesh",    tok_at(3))
  # Gear and mesh together, e.g. TBB_70-99. A mesh size only means
  # something within its gear, so this is the level a fleet is usually
  # read at - and as one label it can sit on a single axis, fill or
  # filter. A fleet code with no mesh part ("All", "OTB_DEF") is just
  # its gear, the way metier5 / metier6 stop at the last part there
  # is, rather than becoming "TBB_NA" or dropping out as NA.
  put("metier4_mesh", ifelse(is.na(d$mesh) | !nzchar(as.character(d$mesh)), d$metier4,
                             paste(d$metier4, d$mesh, sep = "_")))
  put("ICESSubArea",     area_join(2))       # 27.X
  put("ICESDivision",    area_join(3))       # 27.X.X
  put("ICESSubdivision", area_join(4))       # 27.X.X.X
  # The stratum AS UPLOADED: the four native CEF keys, joined. This
  # is the finest grouping the data itself defines, and it is the
  # only one that needs no interpretation - every other column here
  # is something rcef derived by splitting or mapping a native one.
  put("Stratum", paste(d$vesselFlagCountry, d$fleetValue, d$areaValue,
                       d$seasonValue, sep = " | "))
  d <- na_text(d)   # a code split on an empty part ("TBB_DEF__0_0") gives "" for it: missing as well
  if (identical(what, "catch")) return(d)

  # ---- what = "distribution": the same fields, read off the catch ------------
  # Derived once, above, on the table that has the columns; each distribution
  # row then takes the values of the catch rows its key points at. The key is
  # what catchAllocationAoL() joins the two tables on (whichever of those
  # columns both carry), so a distribution gets the stratum the allocation
  # will pair it with.
  link_cols <- c("vesselFlagCountry", "year", "workingGroup", "stock",
                 "speciesCode", "catchCategory", "domainBiology")
  by <- link_cols[link_cols %in% names(d) & link_cols %in% names(distribution_data)]
  # everything derive_fields() puts on a catch - the stratum keys and what is
  # derived from them - less the country, where the distributions have their
  # own. Asked of derive_fields() itself, so a field added above is carried
  # here without a second edit.
  proto <- data.frame(fleetValue = "A_B_C", areaValue = "27.4",
                      vesselFlagCountry = "X", seasonValue = 1L,
                      stringsAsFactors = FALSE)
  carry <- setdiff(names(derive_fields(proto)),
                   intersect("vesselFlagCountry", names(distribution_data)))

  has_domain <- !is.na(d$domainBiology) & nzchar(as.character(d$domainBiology))
  d   <- d[has_domain, , drop = FALSE]
  k_c <- biology_key(d, by)
  k_d <- biology_key(distribution_data, by)
  # one value per key, or NA when the catch rows of the key disagree on it
  one <- function(v) {
    u <- unique(v[!is.na(v)])
    if (length(u) == 1L) u else v[NA_integer_]
  }
  lk <- d %>%
    mutate(.key = k_c, .stratum = Stratum, .domain = as.character(domainBiology)) %>%
    group_by(.key) %>%
    summarise(.n_strata = dplyr::n_distinct(.stratum),
              .domain   = dplyr::first(.domain),
              across(all_of(carry), one),
              .groups = "drop")
  i <- match(k_d, lk$.key)

  shared <- lk$.domain[lk$.n_strata > 1L & lk$.key %in% k_d]
  if (length(shared))
    message("derive_fields(): ", length(shared), " distribution key(s) are used ",
            "by catch rows of more than one stratum (e.g. ",
            paste(utils::head(unique(shared), 3), collapse = ", "),
            "); the fields those rows differ on are NA for them")
  if (anyNA(i))
    warning("derive_fields(): ", sum(is.na(i)), " of ", length(i),
            " distribution row(s) have no catch row with the same ",
            paste(by, collapse = " + "), " - their derived fields are NA",
            call. = FALSE)

  out <- distribution_data
  for (col in carry) out[[col]] <- lk[[col]][i]
  out
}

#' What to add to the error of a column the catch lacks
#'
#' The coverage table and the plots derive nothing themselves - they read the
#' columns the catch has - so a column named as a stratum, a fill or a facet
#' that [derive_fields()] would have added, and the catch does not carry, is a
#' missing column like any other. This is the sentence that tells where it
#' comes from.
#'
#' @param cols The names of the missing columns.
#' @return `""` when none of them is a column [derive_fields()] adds, otherwise
#'   a sentence fragment, starting with " - ", to put after the error.
#' @keywords internal
#' @noRd
#' @export
derive_hint <- function(cols) {
  # asked of derive_fields() itself, so a column added there is known here without
  # a second edit
  proto <- data.frame(fleetValue = "A_B_C", areaValue = "27.4", vesselFlagCountry = "X",
                      seasonValue = 1L, stringsAsFactors = FALSE)
  hit <- intersect(cols, setdiff(names(derive_fields(proto)), names(proto)))
  if (!length(hit)) return("")
  paste0(" - derive_fields() adds ", paste(hit, collapse = ", "),
         ": run it once on the raw catch and use that")
}

# ----------------------------------------------------------
#  Units: one conversion for the whole workflow
# ----------------------------------------------------------

#' Convert values to the unit the package works in
#'
#' @description
#' Every weight is carried and shown in tonnes, while CEF `WeightLive` arrives
#' in kilograms; distributions come with a unit of their own on every row, so a
#' file can mix mm and cm, g and kg, or numbers and thousands. This converts
#' once, on the way in, using the row's own unit:
#'
#' * `to = "t"`: a catch weight (`kg`, `g`, `t`) to tonnes. A missing or
#'   unrecognised unit is taken as kilograms - the CEF default - without a
#'   word.
#' * `to = "g"`: a mean weight (`g`, `kg`, `t`, `mg`) to grams.
#' * `to = "N"`: a number (`N`, `thousands`, `millions`, `NE3`, ...) to numbers.
#' * `to = "mm"`: a length class (`mm`, `cm`, `dm`, `m`) to millimetres.
#'
#' For the last three a missing unit is the target unit already, and an
#' unrecognised one is left unconverted with a warning rather than silently
#' treated as the target unit.
#'
#' @param x Numeric values.
#' @param unit Character vector of units, recycled over `x`; `NULL` for none.
#' @param to `"t"`, `"g"`, `"N"` or `"mm"`.
#' @param what What is being converted, for the warning (default `to`).
#' @return Numeric values in the target unit.
#' @keywords internal
#' @export
#' @noRd
convert_units <- function(x, unit = NULL, to = c("t", "g", "N", "mm"), what = to) {
  to <- match.arg(to)
  table_ <- switch(to,
    t  = c(kg = 1e-3, g = 1e-6, t = 1, tonnes = 1, KG = 1e-3, G = 1e-6),
    g  = c(g = 1, kg = 1000, t = 1e6, mg = 0.001),
    N  = c(N = 1, n = 1, thousands = 1000, kN = 1000, millions = 1e6, MN = 1e6,
           NE3 = 1e3, NE6 = 1e6, NE9 = 1e9),
    mm = c(mm = 1, cm = 10, dm = 100, m = 1000))
  u <- if (is.null(unit)) rep(NA_character_, length(x)) else as.character(unit)
  f <- unname(table_[u])
  if (identical(to, "t")) {
    f[is.na(f)] <- 1e-3                      # the CEF default is kg
  } else {
    bad <- unique(u[is.na(f) & !is.na(u)])
    if (length(bad))
      warning(sprintf("%s: unrecognised unit(s) %s - left unconverted",
                      what, paste(bad, collapse = ", ")), call. = FALSE)
    f[is.na(f)] <- 1
  }
  as.numeric(x) * f
}
