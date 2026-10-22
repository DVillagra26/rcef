# =====================================================================
#  rcef_scf.R   (the Stock Coordination Format: Catch Combined, Distributions Combined
#                and the Catch Allocation Results and Settings)
#
#  ORIGIN: this project (ILVO / RCEF Workbench). Freely refactorable.
#
#  The end of the chain: what catchRaising() and catchAllocationAoL() made is
#  added up into the tables of the stock coordination format (SCF v4), CC and
#  DC, and written down stratum by stratum in the allocation setup, CAR and CAS.
#
#    catchCombined()          the CC: the catch added up by year, stock, catch
#                             category and whatever else the SCF reports by;
#    distributionsCombined()  the DC: the numbers and the mean weight at length
#                             or age of the same, in the same long format;
#    catchAllocation()        the CAR and the CAS: every stratum of the raised
#                             catch, imported or raised, with the group it was
#                             raised for or fed, and the donors behind each
#                             group's discard ratio.
#
#  The adding up of the distributions is catchAtAoL() and meanWoLatAoL(). They
#  do what catchAtAoLAggregation() and meanWoLatAoLAggregation() did in one
#  piece - link the distributions to the catch, convert, pool the classes, add up
#  or average - and stop where the table is still one row per class. The DC is
#  made from that, and the two aggregations of RDBESStockCoord spread it over
#  the columns (spread_AoL()), so the three can never disagree.
#
#  DEPENDENCIES  Imports: dplyr, tidyr, rlang. convert_field() (units) is
#  RDBESStockCoord's.
# =====================================================================

# ---------------------------------------------------------------------
#  The adding up, one row per group and class
# ---------------------------------------------------------------------

#' Numbers at length or age, added up: one row per group and class
#'
#' The joining, the converting, the pooling of classes and the adding up of
#' [catchAtAoLAggregation()], up to the point where the table is one row per
#' group and class. That table is what [distributionsCombined()] reports, and
#' what [catchAtAoLAggregation()] spreads over one column per class. The
#' arguments are those of [catchAtAoLAggregation()].
#'
#' @inheritParams catchAtAoLAggregation
#' @param check Warn about the catch with no distribution and the distributions
#'   with no catch ([distributionsCombined()] asks it of the numbers only, not
#'   twice).
#' @return A tibble with the `grouping` columns, `distributionType`,
#'   `distributionClass` (the class; the lowest class of the plus group for the
#'   classes pooled into it), `plusClass` (that lowest class when a plus group
#'   was asked for, `NA` otherwise), `variableUnit`, `value` (the numbers) and
#'   the sampling behind each row - `PSU`, `numPSUs`, `numTrips` and
#'   `numMeasurements` (see `scf_sampling()`).
#' @keywords internal
#' @noRd
#' @export
catchAtAoL <- function(distribution_data,
                       catch_data,
                       grouping = c("catchCategory", "attributeType",
                                    "attributeValue"),
                       originType_catch = c("WGEstimate", "Official"),
                       variableType_catch = c("WeightLive", "Number"),
                       distributionType = "Age",
                       unit = NULL,
                       round = NULL,
                       minAoL = 0, maxAoL = NA,
                       plusGroup = TRUE,
                       check = TRUE)
{
    keys <- c("vesselFlagCountry", "year", "workingGroup", "stock", "speciesCode", "catchCategory")
    catch_sel <- catch_data %>%
        filter(originType %in% originType_catch,
               variableType %in% variableType_catch)
    catch_sel$.dom <- catch_domain(catch_sel)
    if (check)
        link_check(distribution_data, catch_sel, distributionType)

    ## Conversion (only if relevant unit):
    if (! is.null(unit))
    {
        distribution_data <- distribution_data %>%
            convert_field(valueField = "value",
                          to = unit)
    }

    tmp <- distribution_data %>%
        inner_join(catch_sel %>%
                   select(all_of(keys), .dom, any_of(grouping)) %>%
                   distinct(),
                   by = c(keys, domainBiology = ".dom"),
                   suffix = c(".c", "")) %>%
        filter(variableType %in% c("Number"),
               distributionType %in% !!distributionType)

    classes <- aol_classes(tmp, minAoL, maxAoL, plusGroup)

    tmp <- tmp %>%
        filter(between(distributionClass, minAoL, classes$last)) %>%
        mutate(distributionClass = pmin(distributionClass, classes$from))
    cell <- c(grouping, "variableUnit", "distributionClass")

    tmp %>%
        dplyr::group_by(across(all_of(cell))) %>%
        summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
        left_join(scf_sampling(tmp, cell, "DC"), by = cell) %>%
        aol_finish(grouping, distributionType, classes, round)
}

#' Mean weight at length or age, averaged: one row per group and class
#'
#' The joining, the converting, the pooling of classes and the averaging of
#' [meanWoLatAoLAggregation()], up to the point where the table is one row per
#' group and class. The arguments are those of [meanWoLatAoLAggregation()].
#'
#' @inheritParams meanWoLatAoLAggregation
#' @return As [catchAtAoL()], with the mean weights in `value`.
#' @keywords internal
#' @noRd
#' @export
meanWoLatAoL <- function(distribution_data,
                         catch_data,
                         grouping = c("catchCategory", "attributeType",
                                      "attributeValue"),
                         weighting = c("NumberAtAoL", "CATON"),
                         originType_catch = c("WGEstimate", "Official"),
                         variableType_catch = c("WeightLive", "Number"),
                         variableType_dist = "WeightLive",
                         distributionType = "Age",
                         unit = NULL,
                         round = NULL,
                         minAoL = 0, maxAoL = NA,
                         plusGroup = TRUE,
                         check = TRUE)
{
    weighting <- match.arg(weighting, c("NumberAtAoL", "CATON"), several.ok = FALSE)

    variableType_dist <- match.arg(variableType_dist,
                                   c("WeightLive"), several.ok = FALSE)

    keys <- c("vesselFlagCountry", "year", "workingGroup", "stock", "speciesCode", "catchCategory")
    catch_sel <- catch_data %>%
        filter(originType %in% originType_catch,
               variableType %in% variableType_catch) %>%
        mutate(CATON = if (weighting == "CATON") total else NA_real_)
    catch_sel$.dom <- catch_domain(catch_sel)
    if (check)
        link_check(distribution_data, catch_sel, distributionType)

    ## Conversion (only if relevant unit):
    if (! is.null(unit))
    {
        distribution_data <- distribution_data %>%
            convert_field(valueField = "value",
                          to = unit)
    }

    tmp <- distribution_data %>%
        inner_join(catch_sel %>%
                   select(all_of(keys), .dom, CATON, any_of(grouping)) %>%
                   distinct(),
                   by = c(keys, domainBiology = ".dom"),
                   suffix = c(".c", "")) %>%
        filter(variableType %in% variableType_dist,
               distributionType %in% !!distributionType) %>%
        ## Add sampled and estimated catch numbers:
        left_join(distribution_data %>%
                  filter(variableType %in% "Number",
                         distributionType %in% !!distributionType) %>%
                  select(vesselFlagCountry, year, workingGroup,
                         stock, speciesCode, catchCategory, domainBiology,
                         attributeType, attributeValue, distributionType, distributionUnit, distributionClass,
                         NaAoL = value),
                  by = join_by(vesselFlagCountry, year, workingGroup,
                               stock, speciesCode, catchCategory, domainBiology,
                               attributeType, attributeValue, distributionType, distributionUnit, distributionClass),
                  suffix = c(".c", ""))

    classes <- aol_classes(tmp, minAoL, maxAoL, plusGroup)

    ## Field used for weighting, based on user choice:
    wgField <- switch(weighting, NumberAtAoL = "NaAoL", CATON = "CATON")

    tmp <- tmp %>%
        filter(between(distributionClass, minAoL, classes$last),
               ! is.na(!!sym(wgField))) %>% # Does not handle NAs in weights.
        mutate(distributionClass = pmin(distributionClass, classes$from))
    cell <- c(grouping, "variableUnit", "distributionClass")

    tmp %>%
        dplyr::group_by(across(all_of(cell))) %>%
        summarize(value = weighted.mean(x = value,
                                        w = !!sym(wgField),
                                        na.rm = TRUE),
                  .groups = "drop") %>%
        left_join(scf_sampling(tmp, cell, "DC"), by = cell) %>%
        aol_finish(grouping, distributionType, classes, round)
}

# What a distribution and a catch row are linked by: the domainBiology. The strata an allocation
# estimated a distribution for have none in the raised catch: the allocation made one up from the
# stratum, for the distribution and for the `$catch` it returns - "[alloc]_<seasonValue>_<areaValue>_
# <fisheriesManagementUnit>_<fleetValue>_<catchCategory>", with the catch category because an
# allocation is made for each. The same recipe gives that domain to a catch row that has none, so a
# stratum - raised or not, sampled or allocated - finds its distribution, whichever of the raised
# catch and the catch of the allocation result is handed in.
#' @noRd
#' @export
catch_domain <- function(catch_data)
{
    own <- as.character(catch_data$domainBiology)
    stratum <- c("seasonValue", "areaValue", "fisheriesManagementUnit", "fleetValue", "catchCategory")
    if (! all(stratum %in% names(catch_data)))
        return(own)
    coalesce(own, paste("[alloc]", catch_data$seasonValue, catch_data$areaValue,
                        catch_data$fisheriesManagementUnit, catch_data$fleetValue,
                        catch_data$catchCategory, sep = "_"))
}

# What does not go together, said: a catch (not 0, not NaN) that has no distribution of the type
# asked for, and a distribution with fish that has no catch (not 0, not NaN) to go with.
# `catch_sel` is the catch of the origin and the variable asked for, with its domain in `.dom`.
#' @noRd
#' @export
link_check <- function(distribution_data, catch_sel, type)
{
    keys <- c("vesselFlagCountry", "year", "workingGroup", "stock", "speciesCode", "catchCategory")
    dist_all <- distribution_data %>%
        filter(distributionType %in% !!type) %>%
        distinct(across(all_of(c(keys, "domainBiology"))))
    dist_fish <- distribution_data %>%
        filter(distributionType %in% !!type, variableType %in% "Number", value > 0) %>%
        distinct(across(all_of(c(keys, "domainBiology"))))
    catch_w <- catch_sel %>%
        filter(is.finite(total), total > 0) %>%
        dplyr::group_by(across(all_of(c(keys, ".dom")))) %>%
        summarize(total = max(total), .groups = "drop")
    some <- function(d, dom)
        paste(utils::head(paste(d$vesselFlagCountry, d[[dom]], sep = " | "), 3), collapse = "; ")

    no_dist <- anti_join(catch_w, dist_all, by = c(keys, ".dom" = "domainBiology"))
    if (nrow(no_dist))
        warning(nrow(no_dist), " of the ", nrow(catch_w), " catch strata with a weight have no ",
                paste(type, collapse = "/"), " distribution to go with: ",
                signif(sum(no_dist$total), 4), " ", catch_sel$variableUnit[1],
                " of the catch is not in the numbers (", some(no_dist, ".dom"), ")",
                call. = FALSE)

    no_catch <- anti_join(dist_fish, catch_w, by = c(keys, "domainBiology" = ".dom"))
    if (nrow(no_catch))
        warning(nrow(no_catch), " of the ", nrow(dist_fish), " distributions with fish have no ",
                "catch weight to go with and are left out (", some(no_catch, "domainBiology"), ")",
                call. = FALSE)
}

# Where the classes of a table of distributions stop, and where its plus group
# starts: `last` is the highest class kept, `from` the lowest class of the plus
# group - or the highest class, which is then a class like the others - and
# `plus` whether there is a plus group (`plusGroup` and a `maxAoL` to start
# it at). The two adding-up functions share it.
#' @noRd
#' @export
aol_classes <- function(tmp, minAoL, maxAoL, plusGroup)
{
    top <- max(tmp$distributionClass, na.rm = TRUE)
    list(from = if (is.na(maxAoL)) top else maxAoL,
         last = ifelse(plusGroup || is.na(maxAoL), top, maxAoL),
         plus = isTRUE(plusGroup) && ! is.na(maxAoL))
}

# The end of the two adding-up functions: rounding, the type of distribution and
# the plus group named, the columns in their order - the sampling behind each row
# (scf_sampling()) after them.
#' @noRd
#' @export
aol_finish <- function(res, grouping, distributionType, classes, round)
{
    if (!is.null(round) && is.numeric(round))
        res <- res %>% mutate(value = round(value, digits = round))

    res %>%
        mutate(distributionType = !!distributionType,
               plusClass = if (classes$plus) classes$from else NA_real_) %>%
        select(all_of(grouping), distributionType, distributionClass, plusClass,
               variableUnit, value,
               any_of(c("PSU", "numPSUs", "numTrips", "numMeasurements")))
}

# The table of catchAtAoL() / meanWoLatAoL() spread over one column per class,
# named <prefix><class> - with a "+" on the plus group - in the order of the
# classes: what catchAtAoLAggregation() and meanWoLatAoLAggregation() return.
# `fill` is the value a class a group has no numbers in gets, or NA to leave it.
#' @noRd
#' @export
spread_AoL <- function(long, grouping, prefix, fill = NA)
{
    res <- long %>%
        mutate(grp = ifelse(! is.na(plusClass) & distributionClass == plusClass,
                            paste0(distributionClass, "+"),
                            as.character(distributionClass)),
               grp = factor(grp,
                            levels = unique(grp)[order(as.numeric(sub("+",
                                                                      "",
                                                                      unique(grp),
                                                                      fixed = TRUE)))])) %>%
        select(all_of(grouping), unit = variableUnit, grp, value) %>%
        arrange(grp) %>%
        pivot_wider(names_from = "grp", names_prefix = prefix,
                    values_from = "value")

    if (! is.na(fill))
        res <- res %>%
            mutate(across(starts_with(prefix), ~replace_na(.x, fill)))

    res
}


# ---------------------------------------------------------------------
#  The two tables of the SCF
# ---------------------------------------------------------------------

# What the two builders check on the way in. `grouping` is what the year, the
# stock and the catch category are always reported by, the season, the area and
# the fleet when asked for - a value with its type - and, for the distributions,
# the sex; the catch has to have the columns to add up and to report by; and the
# default `originType_catch` takes both origins, so a stratum that has rows of
# both counts twice: said, when the default is in use and the catch has both.
# Returns the `grouping` with the keys and the types that go with it.
#' @noRd
#' @export
scf_input <- function(catch_data, grouping, record, originType_catch, default_origin, fn)
{
    keys  <- c("year", "workingGroup", "stock", "catchCategory")
    extra <- c("seasonType", "seasonValue", "areaType", "areaValue", "fleetValue",
               if (identical(record, "DC")) c("attributeType", "attributeValue"))
    bad <- setdiff(grouping, c(keys, extra))
    if (length(bad))
        stop(fn, "(): `grouping` takes the columns the SCF reports by (",
             paste(extra, collapse = ", "), ", on top of the year, the stock and ",
             "the catch category), not ", paste0("'", bad, "'", collapse = ", "),
             call. = FALSE)
    g <- union(keys, grouping)
    if ("seasonValue" %in% g) g <- union(g, "seasonType")
    if ("areaValue" %in% g) g <- union(g, "areaType")
    if ("attributeValue" %in% g) g <- union(g, "attributeType")

    miss <- setdiff(c("originType", "variableType", "total",
                      if (identical(record, "CC")) "variableUnit"
                      else c("domainBiology", "vesselFlagCountry", "speciesCode"),
                      setdiff(g, c("attributeType", "attributeValue"))),
                    names(catch_data))
    if (length(miss))
        stop(fn, "(): `catch_data` has no column ",
             paste0("'", miss, "'", collapse = ", "), call. = FALSE)

    if (default_origin && length(intersect(unique(catch_data$originType), originType_catch)) > 1)
        warning(fn, "(): `originType_catch` takes both ",
                paste0("'", originType_catch, "'", collapse = " and "),
                " (the default), so a stratum that has rows of both is counted twice - ",
                "pass originType_catch = \"WGEstimate\" for the working group's own ",
                "figures", call. = FALSE)
    g
}

# The sampling behind the rows of a table, as the CEF has it: the type of the primary
# sampling unit (`PSU`) and the counts of what was sampled - `numPSUs`, `numTrips` and,
# for the distributions, `numMeasurements` - one row for each cell made of the columns
# `by`. The counts of the rows of a cell add up, and are `NA` when none of the rows
# reports one: nothing was counted, which is not 0. The `PSU` of a cell are those of
# the rows that report a number of PSUs - the format wants a number with every type -
# as a ";" separated list when there are several, since the samples of one estimate
# can have different ones. A column the table lacks is `NA`. `record` is "CC", whose
# rows are strata, each with a sample of its own, or "DC", whose rows are the classes
# of distributions: the CEF repeats the counts of a distribution on each of its
# classes (and on each sex) and a plus group puts several classes in one cell, so a
# distribution - the country, the species and the domain - is counted once in a cell,
# count by count: a count it repeats is one, a count that differs from class to class
# adds up.
#' @noRd
#' @export
scf_sampling <- function(d, by, record)
{
    psu <- if ("PSU" %in% names(d)) as.character(d$PSU) else rep(NA_character_, nrow(d))
    psu[psu %in% ""] <- NA_character_
    d$PSU <- psu
    for (f in c("numPSUs", "numTrips", "numMeasurements"))
        d[[f]] <- if (f %in% names(d)) suppressWarnings(as.numeric(d[[f]]))
                  else rep(NA_real_, nrow(d))
    ## what a row is a sample of: itself in the CC, its distribution in the DC
    id <- intersect(c("vesselFlagCountry", "speciesCode", "domainBiology"), names(d))
    d$.sample <- if (identical(record, "DC") && length(id))
                     do.call(paste, c(unname(as.list(d[id])), sep = "\r"))
                 else seq_len(nrow(d))

    types_of <- function(psu, n)
    {
        types <- as.character(unlist(strsplit(psu[! is.na(psu) & ! is.na(n)], ";",
                                              fixed = TRUE)))
        types <- sort(unique(types[nzchar(types)]))
        if (length(types)) paste(types, collapse = ";") else NA_character_
    }
    count_of <- function(n, sample)
        if (all(is.na(n))) NA_integer_
        else as.integer(round(sum(n[! duplicated(data.frame(sample, n))], na.rm = TRUE)))

    res <- d %>%
        dplyr::group_by(across(all_of(by))) %>%
        summarize(PSU = types_of(PSU, numPSUs),
                  numPSUs = count_of(numPSUs, .sample),
                  numTrips = count_of(numTrips, .sample),
                  numMeasurements = count_of(numMeasurements, .sample),
                  .groups = "drop")
    if (identical(record, "DC")) res else res %>% select(-numMeasurements)
}

# The header of the file: what the SCF has once for the whole run and the data does
# not have - the year of the working group, the version, the calendar year (or the
# date a model year starts on). Checked first, before anything is added up, and
# returned as the SCF has it, to be written on every row.
#' @noRd
#' @export
scf_header <- function(workingGroupYear, version, versionDescription, calendarYear,
                       startDateYear, fn)
{
    if (missing(workingGroupYear) ||
        ! (is.numeric(workingGroupYear) && length(workingGroupYear) == 1 &&
           is.finite(workingGroupYear) && workingGroupYear == round(workingGroupYear) &&
           workingGroupYear >= 1000 && workingGroupYear <= 9999))
        stop(fn, "(): `workingGroupYear` is the year of the working group the data are ",
             "processed for (the 2026 of a WGNSSK 2026), as a four-digit year",
             call. = FALSE)
    if (! (is.numeric(version) && length(version) == 1 && is.finite(version) &&
           version >= 0 && version == round(version)))
        stop(fn, "(): `version` is a whole number", call. = FALSE)
    if (! (length(versionDescription) == 1 &&
           (is.na(versionDescription) || is.character(versionDescription))))
        stop(fn, "(): `versionDescription` is a single string, or NA",
             call. = FALSE)
    if (! (is.character(calendarYear) && length(calendarYear) == 1 &&
           toupper(calendarYear) %in% c("Y", "N")))
        stop(fn, "(): `calendarYear` is \"Y\" (the years are calendar years) or ",
             "\"N\" (a model year that starts on another date)", call. = FALSE)
    calendarYear <- toupper(calendarYear)
    if (identical(calendarYear, "N"))
    {
        if (! (is.character(startDateYear) && length(startDateYear) == 1 &&
               grepl("^(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])$", startDateYear)))
            stop(fn, "(): with calendarYear = \"N\" `startDateYear` is the start ",
                 "of the model year, as \"MM-DD\" (\"07-01\")", call. = FALSE)
    }else{
        if (! (length(startDateYear) == 1 && is.na(startDateYear)))
            warning(fn, "(): `startDateYear` is left blank with calendarYear = ",
                    "\"Y\"; the one given is not used", call. = FALSE)
        startDateYear <- NA_character_
    }

    list(workingGroupYear = as.integer(workingGroupYear),
         version = as.integer(version),
         versionDescription = as.character(versionDescription),
         calendarYear = calendarYear,
         startDateYear = as.character(startDateYear))
}

# The table as the SCF has it: every field there is, in the order of the format -
# the ones the data has filled, each of its type, and the others `NA` - the header
# of the file (scf_header()) on every row, the codes checked against the ones the
# format accepts (a code of another kind is a warning, not an error: the table is
# still the table, and the vocabularies grow), and sorted. `record` is "CC", "DC"
# or, for the allocation setup, "CAR" and "CAS".
#' @noRd
#' @export
scf_table <- function(d, record, header, fn)
{
    d <- d %>%
        mutate(recordType = record, !!!header)
    alloc <- record %in% c("CAR", "CAS")

    ## The converted InterCatch files name the country (Belgium, UK(Scotland)); the
    ## SCF wants its ISO 3166 code (BE, GB-SCT): the names that have a code get it, the
    ## others are warned about below.
    if (alloc && "vesselFlagCountry" %in% names(d))
    {
        iso <- c(Belgium = "BE", Bulgaria = "BG", Canada = "CA", Croatia = "HR",
                 Denmark = "DK", Estonia = "EE", "Faroe Islands" = "FO", Finland = "FI",
                 France = "FR", Germany = "DE", Greece = "GR", Greenland = "GL",
                 Iceland = "IS", Ireland = "IE", Italy = "IT", Latvia = "LV",
                 Lithuania = "LT", Netherlands = "NL", Norway = "NO", Poland = "PL",
                 Portugal = "PT", Romania = "RO", Russia = "RU", Spain = "ES",
                 Sweden = "SE", "Isle of Man" = "IM", Jersey = "JE", Guernsey = "GG",
                 "UK (England)" = "GB-ENG", "UK(England)" = "GB-ENG",
                 "UK (Scotland)" = "GB-SCT", "UK(Scotland)" = "GB-SCT",
                 "UK (Wales)" = "GB-WLS", "UK(Wales)" = "GB-WLS",
                 "UK (Northern Ireland)" = "GB-NIR", "UK(Northern Ireland)" = "GB-NIR")
        named <- d$vesselFlagCountry %in% names(iso)
        d$vesselFlagCountry[named] <- unname(iso[d$vesselFlagCountry[named]])
    }

    ## A working group does not process data that come after it:
    late <- unique(d$year[! is.na(d$year) & d$year > header$workingGroupYear])
    if (length(late))
        warning(fn, "(): the data of ", paste(sort(late), collapse = ", "),
                " come after the working group year ", header$workingGroupYear,
                " - `workingGroupYear` is the year of the working group the data are ",
                "processed for", call. = FALSE)

    ## Every field there is, in the order of the format, each of its type:
    head <- c("recordType", "workingGroupYear", "version", "versionDescription",
              "calendarYear", "startDateYear", "year", "workingGroup", "stock",
              "catchCategory", "seasonType", "seasonValue", "areaType", "areaValue",
              "fleetValue")
    stratum <- c("vesselFlagCountry", "calendarYear", "startDateYear", "year",
                 "workingGroup", "stock", "speciesCode", "catchCategory", "seasonType",
                 "seasonValue", "areaType", "areaValue", "fisheriesManagementUnit",
                 "fleetType", "fleetValue")
    fields <- switch(record,
                     CC = c(head, "variableType", "variableUnit", "total", "variance",
                            "PSU", "numPSUs", "numTrips"),
                     DC = c(head, "distributionType", "distributionUnit",
                            "distributionClass", "ageGroupPlus", "attributeType",
                            "attributeValue", "variableType", "variableUnit",
                            "valueType", "value", "variance", "PSU", "numPSUs",
                            "numTrips", "numMeasurements"),
                     CAR = c("recordType", "workingGroupYear", "version",
                             "versionDescription", "catchAllocationSchemeName",
                             "catchAllocationGroupNameTarget",
                             "catchAllocationGroupNameSource", "imported", stratum,
                             "variableType", "variableUnit", "total", "variance",
                             "PSU", "numPSUs", "numTrips", "comment"),
                     CAS = c("recordType", "workingGroupYear", "version",
                             "versionDescription", "catchAllocationSchemeName",
                             "catchAllocationGroupName", stratum, "rate",
                             "averagingMethod", "weightingCategory",
                             "weightingParameter", "weightingTotal", "comment"))
    integer_fields <- c("workingGroupYear", "version", "year", "seasonValue",
                        "speciesCode", "distributionClass", "ageGroupPlus", "numPSUs",
                        "numTrips", "numMeasurements")
    numeric_fields <- c("total", "value", "variance", "rate", "weightingTotal")
    for (f in setdiff(fields, names(d)))
        d[[f]] <- if (f %in% integer_fields) NA_integer_
                  else if (f %in% numeric_fields) NA_real_
                  else NA_character_
    d <- d %>% select(all_of(fields))
    for (f in intersect(fields, integer_fields))
        if (! is.integer(d[[f]]))
        {
            v <- suppressWarnings(as.integer(d[[f]]))
            if (identical(is.na(v), is.na(d[[f]])) && all(v == d[[f]], na.rm = TRUE))
                d[[f]] <- v
        }
    ## (a field a file left empty is read as logical, or as "")
    for (f in setdiff(fields, c(integer_fields, numeric_fields)))
    {
        d[[f]] <- as.character(d[[f]])
        d[[f]][which(d[[f]] == "")] <- NA_character_
    }

    ## The codes the format accepts:
    check <- function(field, accepted, sep = NULL)
    {
        bad <- unique(d[[field]])
        bad <- as.character(bad[! is.na(bad)])
        if (! is.null(sep))
            bad <- unique(unlist(strsplit(bad, sep, fixed = TRUE)))
        bad <- setdiff(bad, accepted)
        if (length(bad))
            warning(fn, "(): ", field, " ", paste0("'", bad, "'", collapse = ", "),
                    " is not a code the SCF accepts (", paste(accepted, collapse = ", "),
                    ")", call. = FALSE)
    }
    check("catchCategory", switch(record, CAR = c("BMS", "Dis", "DisBMS", "Lan"),
                                  CAS = c("BMS", "Dis", "DisBMS"),
                                  c("BMS", "Dis", "DisBMS", "Lan", "Catch")))
    check("seasonType", if (alloc) c("Year", "Quarter", "Month")
                        else c("Quarter", "Month", "Half-year"))
    check("areaType", if (alloc) "ICESArea" else c("ICESArea", "ICESAreaNonFAO"))
    check("PSU", c("Location", "LandingEvent", "FishingTrip", "FishingOperation",
                   "TemporalEvent", "FishingVessel", "OnshoreEvent"), sep = ";")
    if (alloc)
    {
        check("imported", c("Y", "N"))
        check("fleetType", c("Fleet", "Metier6", "Metier5", "Metier4"))
        check("averagingMethod", c("Weighted", "Equal"))
        check("weightingCategory", c("BMS", "Dis", "Lan", "DisBMS"))
        check("weightingParameter", c("WeightLive", "Number"))
        notiso <- unique(d$vesselFlagCountry[! is.na(d$vesselFlagCountry) &
                                             ! grepl("^[A-Z]{2}(-[A-Z]{3})?$", d$vesselFlagCountry)])
        if (length(notiso))
            warning(fn, "(): vesselFlagCountry ", paste0("'", notiso, "'", collapse = ", "),
                    " is not an ISO 3166 code the SCF accepts (BE, DK, GB-SCT ...)",
                    call. = FALSE)
    }
    weight_units <- if (record %in% c("CC", "CAR")) c("t", "kg") else c("g", "kg")
    if ("variableUnit" %in% fields)
    {
        unit_ok <- ifelse(d$variableType %in% "WeightLive", d$variableUnit %in% weight_units,
                          ifelse(d$variableType %in% "Number",
                                 d$variableUnit %in% c("N", "NE3"), TRUE))
        if (any(! unit_ok))
            warning(fn, "(): variableUnit ",
                    paste0("'", unique(d$variableUnit[! unit_ok]), "'", collapse = ", "),
                    " does not go with its variableType (WeightLive: ",
                    paste(weight_units, collapse = ", "), "; Number: N, NE3) - give `unit`",
                    call. = FALSE)
    }
    if (identical(record, "DC"))
    {
        check("distributionType",
              c("Age", "WidthCarapace", "LengthCarapace", "LengthMantle",
                "LengthMaximumShell", "LengthPreAnal", "LengthPreCaudal",
                "LengthStandard", "LengthTotal"))
        check("distributionUnit", c("y", "wr", "mm"))
        check("attributeType", "Sex")
        check("attributeValue", c("F", "M"))
    }

    ## Sorted by the stratum, then - for the distributions - by type, sex and variable,
    ## the classes in their order within each. The allocation setup is sorted by the
    ## country and the stratum, the landings and the discards of a stratum one after the
    ## other (the settings by the group first, in the order the groups were given):
    place <- c("seasonType", "seasonValue", "areaType", "areaValue", "fleetValue")
    order_by <- switch(record,
                       CAR = c("vesselFlagCountry", "year", "workingGroup", "stock", place,
                               "catchCategory", "variableType"),
                       CAS = c(".group", "vesselFlagCountry", "year", "workingGroup", "stock", place),
                       c("year", "workingGroup", "stock", "catchCategory", place,
                         if (identical(record, "DC"))
                             c("distributionType", "attributeType", "attributeValue",
                               "variableType", "distributionClass")
                         else "variableType"))
    if (identical(record, "CAS"))
        d$.group <- match(d$catchAllocationGroupName, unique(d$catchAllocationGroupName))
    d %>%
        arrange(across(all_of(order_by))) %>%
        select(any_of(fields))
}

#' Catch Combined (CC): the catch of a stock, added up, in the format of the stock coordination
#'
#' @description
#' The catch of a stock, the way the stock coordination format (SCF v4) wants it:
#' one row per year, working group, stock and catch category - and per
#' variable and unit - with the catch added up. Give it the result of
#' [catchRaising()]; every column the format has and the catch does not fill is
#' there too, `NA`, and every column the format does not have is gone.
#'
#' @details
#' The rows are added up by `year`, `workingGroup`, `stock`, the variables in
#' `grouping` and the catch's `variableType` and `variableUnit`. `grouping`
#' always holds the catch category, and takes the other columns of the SCF
#' that a stock can be split by - `seasonValue`, `areaValue` and `fleetValue`;
#' a season or an area is reported with its type. Usually the catch is not
#' split, which is the default; ask for more when the stock assessment is. To
#' report the catch by fleets of your own, put their names in `fleetValue`
#' first.
#'
#' Take one `originType_catch`: the default takes the catch rows of both origins,
#' and a stratum that has rows of both is counted twice (a warning says so).
#' `"WGEstimate"` is the working group's own catch - the raised discards among
#' it. A catch of `NaN` (a stratum the raising had no rate for) adds up as
#' nothing. The rows are added up whatever their `domainBiology`: the raised
#' catch, and the `catch` of the allocation made from it (which has the domain the
#' allocation made up for the strata it estimated), give the same CC.
#'
#' The distributions of the same stock go with it: use the same `grouping` in
#' [distributionsCombined()], since each distribution needs the catch it
#' belongs to in the CC.
#'
#' ## The header
#'
#' `workingGroupYear` is the year of the working group the data are processed
#' for - the 2026 of a WGNSSK 2026 - and has no default, since it is not in the
#' data. `version` and `versionDescription` name the run, so more than one can
#' be kept (a benchmark, a sensitivity test). `calendarYear` is `"Y"` when `year`
#' is the calendar year, which it is for most stocks, and `"N"` when it is a
#' model year that starts on another date, `startDateYear` (`"07-01"`), the same
#' for every row of the stock. They are written on every row; every other field
#' comes from the catch.
#'
#' ## The sampling
#'
#' `PSU`, `numPSUs` and `numTrips` say how much sampling stands behind the
#' total. They are taken from the columns of the catch with the same names, as
#' the CEF has them: the counts of the rows added up are summed, and are `NA`
#' when none of the rows reports one (nothing was counted, which is not 0), and
#' `PSU`, the type of the sampling unit (`"FishingTrip"`), is that of the rows
#' that report a number of them - a `;` separated list when there are several
#' types. A row the raising only estimated (`importedOrRaised` is `"raised"`)
#' has no sample of its own, its rate was borrowed from a group, so whatever
#' count it carries is left out: the format asks for the strata that actually
#' contributed to the estimate. `variance` is left empty.
#'
#' @param catch_data The catch to add up: the result of [catchRaising()], or a
#'   CEF catch table.
#' @param grouping The variables to add up by, on top of the year, the working
#'   group and the stock: `"catchCategory"` (default) and, if wanted,
#'   `"seasonValue"`, `"areaValue"`, `"fleetValue"` (with `seasonType` and
#'   `areaType`, which go with them).
#' @param originType_catch `"WGEstimate"` or `"Official"`: the origin of the
#'   catch rows to add up. Pass one.
#' @param variableType_catch `"WeightLive"` or `"Number"`: the variable of the
#'   catch rows to add up; both are reported, each in its own rows.
#' @param unit The unit to convert the catch to before adding it up: `"t"` or
#'   `"kg"` for weights, `"N"` or `"NE3"` for numbers. `NULL` (default) leaves
#'   them as they are.
#' @param round Digits to round the totals to; `NULL` (default) does not round.
#' @param workingGroupYear The year of the working group the data are processed
#'   for, as a four-digit year (`2026`). Required.
#' @param version The version of the file, a whole number (default `1`).
#' @param versionDescription What the version is, a string, or `NA` (default).
#' @param calendarYear `"Y"` (default) or `"N"`.
#' @param startDateYear With `calendarYear = "N"`, the start of the model year
#'   as `"MM-DD"`; left out otherwise.
#' @return A tibble with the CC fields of the SCF: `recordType`,
#'   `workingGroupYear`, `version`, `versionDescription`, `calendarYear`,
#'   `startDateYear`, `year`, `workingGroup`, `stock`, `catchCategory`,
#'   `seasonType`, `seasonValue`, `areaType`, `areaValue`, `fleetValue`,
#'   `variableType`, `variableUnit`, `total`, `variance`, `PSU`, `numPSUs` and
#'   `numTrips`. A code the SCF does not accept is warned about.
#' @seealso [distributionsCombined()] for the distributions that go with it;
#'   [catchRaising()] for the catch it is made of.
#' @family stock coordination format
#' @md
#' @examples
#' \dontrun{
#' raised <- catchRaising(catch, strataCond, matchedDataCond,
#'                        logFile = tempfile(fileext = ".txt"))
#'
#' # the catch of the stock by catch category, in tonnes
#' catchCombined(raised, originType_catch = "WGEstimate",
#'               variableType_catch = "WeightLive", unit = "t",
#'               workingGroupYear = 2026,
#'               version = 1, versionDescription = "WGNSSK 2026, first run")
#'
#' # by quarter as well
#' catchCombined(raised, grouping = c("catchCategory", "seasonValue"),
#'               originType_catch = "WGEstimate", unit = "t",
#'               workingGroupYear = 2026)
#'
#' # a model year that starts on 1 July
#' catchCombined(raised, originType_catch = "WGEstimate", unit = "t",
#'               workingGroupYear = 2026,
#'               calendarYear = "N", startDateYear = "07-01")
#' }
#' @export
catchCombined <- function(catch_data,
                          grouping = "catchCategory",
                          originType_catch = c("WGEstimate", "Official"),
                          variableType_catch = c("WeightLive", "Number"),
                          unit = NULL,
                          round = NULL,
                          workingGroupYear,
                          version = 1,
                          versionDescription = NA_character_,
                          calendarYear = "Y",
                          startDateYear = NA_character_)
{
    fn <- "catchCombined"
    header <- scf_header(workingGroupYear, version, versionDescription,
                         calendarYear, startDateYear, fn)
    grouping <- scf_input(catch_data, grouping, "CC", originType_catch,
                          missing(originType_catch), fn)

    res <- catch_data %>%
        filter(originType %in% originType_catch,
               variableType %in% variableType_catch)
    if (nrow(res) == 0)
        stop(fn, "(): no catch rows with originType ",
             paste0("'", originType_catch, "'", collapse = " or "),
             " and variableType ",
             paste0("'", variableType_catch, "'", collapse = " or "),
             call. = FALSE)

    if (! is.null(unit))
        res <- res %>%
            convert_field(valueField = "total", to = unit)

    ## A row the raising only estimated has no sample of its own - its rate was
    ## borrowed from a group - so the counts it carries (those of the landings it was
    ## made from) are not the discards': they are left out.
    if ("importedOrRaised" %in% names(res))
        res <- res %>%
            mutate(across(any_of(c("numPSUs", "numTrips")),
                          ~ ifelse(importedOrRaised %in% "raised", NA, .x)))

    cell <- c(grouping, "variableType", "variableUnit")
    res <- res %>%
        dplyr::group_by(across(all_of(cell))) %>%
        summarize(total = sum(total, na.rm = TRUE), .groups = "drop") %>%
        left_join(scf_sampling(res, cell, "CC"), by = cell)

    if (!is.null(round) && is.numeric(round))
        res <- res %>% mutate(total = round(total, digits = round))

    scf_table(res, "CC", header, fn)
}

#' Distributions Combined (DC): the numbers and mean weights at length or age, in the format of the stock coordination
#'
#' @description
#' The length or age distributions of a stock, the way the stock coordination
#' format (SCF v4) wants them: the catch's numbers at length (or age) and the
#' mean weight at length (or age), one row per class, added up by year, working
#' group, stock and catch category - and whatever else `grouping` adds. Give it
#' the two tables of [catchAllocationAoL()]; every column the format has and the
#' distributions do not fill is there too, `NA`.
#'
#' @details
#' This is what [catchAtAoLAggregation()] (the numbers) and
#' [meanWoLatAoLAggregation()] (the mean weights) add up - they are given the
#' same arguments here, and add up the same way - in the long format of the
#' SCF instead of one column per class: a `Number` row (`valueType` `Total`) and
#' a `WeightLive` row (`valueType` `Mean`) for each class. A class no fish
#' fall in is left out, since it has no mean weight.
#'
#' The distributions are linked to the catch through `domainBiology`, the catch
#' category and the country, and the `grouping` is read off the catch they are
#' linked to. A sampled distribution has the domain of its catch. The strata an
#' allocation estimated a distribution for have none in the raised catch, and the
#' allocation made one up for the distribution and for the `catch` it returns,
#' `"[alloc]_<season>_<area>_<unit>_<fleet>_<category>"`; a catch row with no
#' domain is given the same, so every stratum - raised or not, sampled or
#' allocated - finds its distribution whether the raised catch the allocation was
#' run on or the `catch` of its result is handed in
#' (`distributionsCombined(alloc$distribution, alloc$catch, ...)`). It has to be
#' that raised catch: another raising has other strata and weights. A catch weight
#' with no distribution, or a distribution with fish and no catch weight, is
#' warned about. Take one `originType_catch` and one `variableType_catch` too, as
#' in [catchAtAoLAggregation()].
#'
#' The `grouping` is the one of [catchCombined()] (and of the same file: a
#' distribution needs a catch of the same year, stock, category, season, area and
#' fleet in the CC), with the sex of the distribution on top -
#' `attributeType` and `attributeValue`, the default - which is `NA` when the
#' distributions do not have one. The header - `workingGroupYear` and the rest -
#' is the one of [catchCombined()] too, and so is the sampling's reading.
#'
#' ## The sampling
#'
#' `PSU`, `numPSUs`, `numTrips` and `numMeasurements` say how much sampling
#' stands behind each class. They are taken from the columns of the
#' distributions with the same names, as the CEF has them. A row of the table is
#' made of the distributions of the catch it is added up from: each counts once,
#' however many classes it has (the CEF repeats its counts on every class, and a
#' plus group puts several of them in one row), and the counts of the
#' distributions are summed. They are `NA` when none of the distributions reports
#' one - the allocation leaves them empty for the distributions it estimated,
#' which were borrowed: the format asks for the strata that actually contributed
#' - and `PSU`, the type of the sampling unit, is that of the distributions that
#' report a number of them, a `;` separated list when there are several types.
#' `numMeasurements` is the count of the whole distribution, which is all the CEF
#' has, though the format asks for the measurements at the class: do not sum it
#' over the classes. `variance` is left empty.
#'
#' ## Classes
#'
#' Lengths are reported in mm, in the class the distributions give. `minAoL` drops
#' the classes below it; `maxAoL` and `plusGroup` make a plus group of the ages
#' from `maxAoL` up, written `ageGroupPlus` on every row of the distribution. The
#' SCF has no plus group for lengths: with them `maxAoL` only cuts the classes
#' off (`plusGroup = FALSE`). `distributionType` can be several (`c("Age",
#' "LengthTotal")`), and `minAoL`, `maxAoL` and `plusGroup` then one value for
#' each, named: `maxAoL = c(Age = 10)`.
#'
#' @inheritParams catchAtAoLAggregation
#' @inheritParams catchCombined
#' @param grouping The variables to add up by, on top of the year, the working
#'   group, the stock and the catch category (always reported by): the sex of
#'   the distribution - `"attributeType"` and `"attributeValue"`, the default -
#'   and, as in [catchCombined()], `"seasonValue"`, `"areaValue"` and
#'   `"fleetValue"` (with `seasonType` and `areaType`, which go with them).
#' @param round Digits to round the numbers and the mean weights to; `NULL`
#'   (default) does not round.
#' @param weighting How the mean weights are weighted: `"NumberAtAoL"` (default),
#'   by the numbers at length or age, or `"CATON"`, by the weight of the catch
#'   row a distribution belongs to. See [meanWoLatAoLAggregation()].
#' @param unit The units to convert to before adding up: the numbers in `"N"` or
#'   `"NE3"` (thousands), the mean weights in `"g"` or `"kg"`. Default
#'   `c("NE3", "g")`; `NULL` leaves them as they are.
#' @param distributionType `"Age"` (default), `"LengthTotal"` or both.
#' @param minAoL,maxAoL,plusGroup As in [catchAtAoLAggregation()]; named by
#'   distribution type when there are several.
#' @return A tibble with the DC fields of the SCF: `recordType`,
#'   `workingGroupYear`, `version`, `versionDescription`, `calendarYear`,
#'   `startDateYear`, `year`, `workingGroup`, `stock`, `catchCategory`,
#'   `seasonType`, `seasonValue`, `areaType`, `areaValue`, `fleetValue`,
#'   `distributionType`, `distributionUnit`, `distributionClass`,
#'   `ageGroupPlus`, `attributeType`, `attributeValue`, `variableType`,
#'   `variableUnit`, `valueType`, `value`, `variance`, `PSU`, `numPSUs`,
#'   `numTrips` and `numMeasurements`.
#' @seealso [catchCombined()] for the catch that goes with it;
#'   [catchAllocationAoL()] for the allocation it is made of;
#'   [catchAtAoLAggregation()] and [meanWoLatAoLAggregation()] for the same
#'   numbers and mean weights with one column per class.
#' @family stock coordination format
#' @md
#' @examples
#' \dontrun{
#' alloc <- catchAllocationAoL(raised, distributions,
#'                             condition_alloc_st_list = list(ALL = rlang::quo(TRUE)),
#'                             distributionType = "LengthTotal",
#'                             logFile = tempfile(fileext = ".txt"))
#'
#' # numbers in thousands and mean weights in g, by catch category
#' distributionsCombined(alloc$distribution, alloc$catch,
#'                       originType_catch = "WGEstimate",
#'                       variableType_catch = "WeightLive",
#'                       distributionType = "LengthTotal",
#'                       workingGroupYear = 2026,
#'                       version = 1, versionDescription = "WGNSSK 2026, first run")
#'
#' # ages, with every age from 10 up in a plus group, and the lengths as they are
#' distributionsCombined(alloc$distribution, alloc$catch,
#'                       originType_catch = "WGEstimate",
#'                       variableType_catch = "WeightLive",
#'                       distributionType = c("Age", "LengthTotal"),
#'                       maxAoL = c(Age = 10), plusGroup = c(Age = TRUE, LengthTotal = FALSE),
#'                       workingGroupYear = 2026)
#' }
#' @export
distributionsCombined <- function(distribution_data,
                                  catch_data,
                                  grouping = c("catchCategory", "attributeType",
                                               "attributeValue"),
                                  weighting = c("NumberAtAoL", "CATON"),
                                  originType_catch = c("WGEstimate", "Official"),
                                  variableType_catch = c("WeightLive", "Number"),
                                  distributionType = "Age",
                                  unit = c("NE3", "g"),
                                  round = NULL,
                                  minAoL = 0, maxAoL = NA,
                                  plusGroup = TRUE,
                                  workingGroupYear,
                                  version = 1,
                                  versionDescription = NA_character_,
                                  calendarYear = "Y",
                                  startDateYear = NA_character_)
{
    fn <- "distributionsCombined"
    header <- scf_header(workingGroupYear, version, versionDescription,
                         calendarYear, startDateYear, fn)
    weighting <- match.arg(weighting, c("NumberAtAoL", "CATON"), several.ok = FALSE)
    grouping <- scf_input(catch_data, grouping, "DC", originType_catch,
                          missing(originType_catch), fn)
    miss <- setdiff(c("domainBiology", "vesselFlagCountry", "speciesCode",
                      "distributionType", "distributionUnit", "distributionClass",
                      "variableType", "variableUnit", "value",
                      intersect(grouping, c("attributeType", "attributeValue"))),
                    names(distribution_data))
    if (length(miss))
        stop(fn, "(): `distribution_data` has no column ",
             paste0("'", miss, "'", collapse = ", "), call. = FALSE)

    ## minAoL, maxAoL and plusGroup: one value for every type, or one for each, named
    types <- unique(distributionType)
    per_type <- function(x, type, default, what)
    {
        if (is.null(names(x)))
        {
            if (length(x) != 1)
                stop(fn, "(): `", what, "` is one value, or one per distribution type, ",
                     "named: ", what, " = c(", types[1], " = ...)", call. = FALSE)
            return(x)
        }
        if (type %in% names(x)) x[[type]] else default
    }

    one <- function(type)
    {
        min_c  <- per_type(minAoL, type, 0, "minAoL")
        max_c  <- per_type(maxAoL, type, NA, "maxAoL")
        plus_c <- per_type(plusGroup, type, TRUE, "plusGroup")
        if (! identical(type, "Age") && ! is.na(max_c) && isTRUE(plus_c))
            stop(fn, "(): the SCF has no plus group for lengths: with ", type,
                 " `maxAoL` cuts the classes off, with plusGroup = FALSE",
                 call. = FALSE)

        d <- distribution_data %>%
            filter(distributionType %in% type)
        if (! identical(type, "Age"))
        {
            ## The classes of a length are in mm, as the SCF has them:
            r <- unit2ratio(from = unique(d$distributionUnit), to = "mm")
            if (any(! r$valid & ! r$from %in% "mm"))
                warning(fn, "(): the ", type, " classes are in ",
                        paste(r$from[! r$valid & ! r$from %in% "mm"], collapse = ", "),
                        ", which is not converted to mm: reported as they are",
                        call. = FALSE)
            d <- d %>%
                mutate(distributionClass = distributionClass *
                           r$ratio[match(distributionUnit, r$from)],
                       distributionUnit = "mm")
        }
        unit_c <- unique(d$distributionUnit[! is.na(d$distributionUnit)])
        if (length(unit_c) > 1)
            warning(fn, "(): the ", type, " distributions have several ",
                    "distributionUnit (", paste(unit_c, collapse = ", "), "): ",
                    unit_c[1], " is reported", call. = FALSE)

        num <- catchAtAoL(distribution_data = d, catch_data = catch_data,
                          grouping = grouping, originType_catch = originType_catch,
                          variableType_catch = variableType_catch,
                          distributionType = type, unit = unit, round = round,
                          minAoL = min_c, maxAoL = max_c, plusGroup = plus_c) %>%
            mutate(variableType = "Number", valueType = "Total")
        wgt <- meanWoLatAoL(distribution_data = d, catch_data = catch_data,
                            grouping = grouping, weighting = weighting,
                            originType_catch = originType_catch,
                            variableType_catch = variableType_catch,
                            distributionType = type, unit = unit, round = round,
                            minAoL = min_c, maxAoL = max_c, plusGroup = plus_c,
                            check = FALSE) %>%
            mutate(variableType = "WeightLive", valueType = "Mean")

        ## A class no fish fall in has no mean weight, and is not reported:
        bind_rows(num %>% filter(value > 0),
                  wgt %>% filter(is.finite(value))) %>%
            mutate(distributionUnit = if (length(unit_c)) unit_c[1] else NA_character_,
                   ageGroupPlus = if (identical(type, "Age")) plusClass else NA_real_) %>%
            select(-plusClass)
    }

    res <- bind_rows(lapply(types, one))
    if (nrow(res) == 0)
        stop(fn, "(): no distributions are linked to the catch rows asked for",
             call. = FALSE)

    scf_table(res, "DC", header, fn)
}

#' Catch Allocation (CAR and CAS): how the discards were raised, in the format of the stock coordination
#'
#' @description
#' The raised catch written down stratum by stratum, the way the allocation setup of the
#' stock coordination format wants it: the **Catch Allocation Results (CAR)**, one row for
#' every stratum of the catch - imported or raised - with the group it was raised for or
#' fed, and the **Catch Allocation Settings (CAS)**, one row for every donor stratum of
#' every group, with the discard ratio it gave and the weight it counted for. Give it what
#' [catchRaising()] was given and made: the raised catch and the conditions of the groups.
#'
#' @details
#' ## The CAR
#'
#' Every stratum of the raised catch is a row: the landings, the discards that were
#' reported and the discards the raising made, whichever of the origin and the variable
#' the raising was run for. `imported` says which: `"N"` for the discards the raising
#' made (`importedOrRaised == "raised"`), `"Y"` for all the others. The two names of the
#' group say how the strata took part in the raising:
#'
#' * `catchAllocationGroupNameTarget`: the group the stratum was **raised for**, on the
#'   discards the raising made (`imported = "N"`) and on those only. It is the group of
#'   `condition_raising_st_list` the stratum falls in.
#' * `catchAllocationGroupNameSource`: the group(s) the stratum **feeds**, on the donor
#'   strata (`imported = "Y"`) and on those only - the landings and the discards of the
#'   strata whose discard ratio went into a group's. A stratum that feeds several groups
#'   has them all, separated by `"; "`. A group's donors are the strata of its
#'   `condition_matched_data_list` condition that have a discard estimate (and landings),
#'   and the other rows of the same domain, as [catchRaising()] picks them.
#' * `catchAllocationSchemeName` is written on the raised discards only.
#'
#' A group that raised nothing (it had no landings without an estimate, or no donor to
#' take a ratio from) is neither a target nor a source: it is not in the tables. A raised
#' discard with no total - the ratio of a group with no donor is `NaN` - is not a result
#' and is left out, with a warning.
#'
#' The other fields are the catch's: `total` is added up for the strata that are one row
#' of the CAR (the format has no `metier6`, so a stratum the CEF splits further is added
#' up), and so are the counts of the sampling - `PSU`, `numPSUs` and `numTrips`, `NA` for
#' the raised discards, which have no sample of their own - as in [catchCombined()], whose
#' totals the CAR adds up to. The format asks for them on every imported discard (and BMS)
#' stratum, and a stratum without them is counted in a warning. `variance` is left empty.
#' `vesselFlagCountry` is the ISO 3166
#' code the format wants: a country the converted InterCatch files name (`"Belgium"`,
#' `"UK(Scotland)"`) is written as its code (`BE`, `GB-SCT`), and one with no code is
#' warned about and kept.
#'
#' ## The CAS
#'
#' One row for each donor discard stratum of each group: `rate`, the discards over the
#' landings of the donor (the discard ratio, which can exceed 1), and `weightingTotal`,
#' the landings that weight it. This is how [catchRaising()] combines them: the ratio of a
#' group is the mean of the donors' ratios weighted by their landings -
#' `averagingMethod` `"Weighted"`, `weightingCategory` `"Lan"` and `weightingParameter`
#' the variable of the raising (`"WeightLive"`) - and so `sum(rate * weightingTotal) /
#' sum(weightingTotal)` of a group is the ratio the raised discards of the group were made
#' with. That is checked: a group whose donors do not give the ratio its raised discards
#' have is warned about - the conditions are not those of the raising, or the donors are
#' not all in the catch given. The CAS strata are rows of the CAR (`imported = "Y"`,
#' `Dis`, with the group in its source), by the country, year, season, area and fleet.
#'
#' ## The header
#'
#' `workingGroupYear`, `version`, `versionDescription`, `calendarYear` and
#' `startDateYear` are those of [catchCombined()] - give them the same values - and are
#' written on every row. `catchAllocationSchemeName` is the name of the scheme the discards
#' were allocated by; it is the `versionDescription` when not given.
#'
#' @param catch_data The raised catch: the result of [catchRaising()], with its
#'   `importedOrRaised`, `DrGroup` and the catch's own columns.
#' @param condition_raising_st_list,condition_matched_data_list The conditions the catch was
#'   raised with: the strata each group raises and the data each takes its discard ratio
#'   from, as in [catchRaising()] (the same names and order; quosures or text, not logical
#'   vectors, which refer to the rows of the catch before the raising). The matched data
#'   default to the strata. A scheme of [recommend_groups()] has both in `strataCond` and
#'   `matchedDataCond`. A condition that uses an object of the session (a list of strata,
#'   say) rather than columns of the catch is evaluated where it was made: unquote the
#'   object into it - `rlang::quo(Stratum %in% !!high)` - to keep the condition for another
#'   session (`saveRDS()`).
#' @param originType,variableType The origin and the variable of the rows the catch was
#'   raised for, as in [catchRaising()]: `"WGEstimate"` and `"WeightLive"` by default.
#'   The rows of other origins and variables are not part of the tables.
#' @param catchAllocationSchemeName The name of the scheme the discards were allocated by,
#'   a string; default the `versionDescription`.
#' @param unit The unit to convert the weights to before the tables are made: `"t"` or
#'   `"kg"` (for weights; the raising works in kg). `NULL` (default) leaves them as they
#'   are.
#' @param round Digits to round `total` and `weightingTotal` to; `NULL` (default) does not
#'   round. The rates are not rounded.
#' @param workingGroupYear,version,versionDescription,calendarYear,startDateYear The header
#'   of [catchCombined()]; `workingGroupYear` has no default.
#' @return A list of two tibbles, with the fields of the SCF in their order:
#'   `CAR` (`recordType`, `workingGroupYear`, `version`, `versionDescription`,
#'   `catchAllocationSchemeName`, `catchAllocationGroupNameTarget`,
#'   `catchAllocationGroupNameSource`, `imported`, `vesselFlagCountry`, `calendarYear`,
#'   `startDateYear`, `year`, `workingGroup`, `stock`, `speciesCode`, `catchCategory`,
#'   `seasonType`, `seasonValue`, `areaType`, `areaValue`, `fisheriesManagementUnit`,
#'   `fleetType`, `fleetValue`, `variableType`, `variableUnit`, `total`, `variance`,
#'   `PSU`, `numPSUs`, `numTrips`, `comment`) and `CAS` (`recordType`,
#'   `workingGroupYear`, `version`, `versionDescription`, `catchAllocationSchemeName`,
#'   `catchAllocationGroupName`, `vesselFlagCountry`, `calendarYear`, `startDateYear`,
#'   `year`, `workingGroup`, `stock`, `speciesCode`, `catchCategory`, `seasonType`,
#'   `seasonValue`, `areaType`, `areaValue`, `fisheriesManagementUnit`, `fleetType`,
#'   `fleetValue`, `rate`, `averagingMethod`, `weightingCategory`, `weightingParameter`,
#'   `weightingTotal`, `comment`). A code the SCF does not accept is warned about.
#' @seealso [catchRaising()] for the raising, [recommend_groups()] for groups to give it,
#'   [catchCombined()] for the catch added up.
#' @family stock coordination format
#' @md
#' @examples
#' \dontrun{
#' strataCond <- list(TBB = rlang::quo(metier4 == "TBB"), REST = rlang::quo(metier4 != "TBB"))
#' raised <- catchRaising(catch, strataCond, logFile = tempfile(fileext = ".txt"))
#'
#' alloc <- catchAllocation(raised, strataCond,
#'                          workingGroupYear = 2026, version = 1,
#'                          versionDescription = "WGNSSK 2026, first run")
#' alloc$CAR
#' alloc$CAS
#'
#' # the discards of every group can be read back from its donors
#' with(alloc$CAS[alloc$CAS$catchAllocationGroupName == "TBB", ],
#'      sum(rate * weightingTotal) / sum(weightingTotal))
#' }
#' @export
catchAllocation <- function(catch_data,
                            condition_raising_st_list,
                            condition_matched_data_list = condition_raising_st_list,
                            originType = c("WGEstimate", "Official"),
                            variableType = c("WeightLive", "Number"),
                            catchAllocationSchemeName = NULL,
                            unit = NULL,
                            round = NULL,
                            workingGroupYear,
                            version = 1,
                            versionDescription = NA_character_,
                            calendarYear = "Y",
                            startDateYear = NA_character_)
{
    fn <- "catchAllocation"
    header <- scf_header(workingGroupYear, version, versionDescription,
                         calendarYear, startDateYear, fn)
    originType <- match.arg(originType, c("WGEstimate", "Official"), several.ok = FALSE)
    variableType <- match.arg(variableType, c("WeightLive", "Number"), several.ok = FALSE)
    scheme <- if (is.null(catchAllocationSchemeName)) header$versionDescription
              else catchAllocationSchemeName
    if (! (is.character(scheme) && length(scheme) == 1 && ! is.na(scheme) && nzchar(scheme)))
        stop(fn, "(): `catchAllocationSchemeName` is the name of the scheme the discards ",
             "were allocated by, which the SCF asks for: give it, or a `versionDescription` ",
             "to take it from", call. = FALSE)

    ## ---- the catch: the rows the raising was run for
    d <- as.data.frame(catch_data, stringsAsFactors = FALSE)
    stratum <- c("vesselFlagCountry", "year", "workingGroup", "stock", "speciesCode",
                 "catchCategory", "seasonType", "seasonValue", "areaType", "areaValue",
                 "fisheriesManagementUnit", "fleetType", "fleetValue")
    miss <- setdiff(c(setdiff(stratum, "fisheriesManagementUnit"), "originType",
                      "variableType", "variableUnit", "total", "domainCatchDis",
                      "importedOrRaised"), names(d))
    if (length(miss))
        stop(fn, "(): `catch_data` has no column ", paste0("'", miss, "'", collapse = ", "),
             " - it is the result of catchRaising()", call. = FALSE)
    for (f in setdiff(c("fisheriesManagementUnit", "PSU", "numPSUs", "numTrips", "comment"),
                      names(d)))
        d[[f]] <- NA
    w <- d[d$originType %in% originType & d$variableType %in% variableType, , drop = FALSE]
    if (nrow(w) == 0)
        stop(fn, "(): no catch rows with originType '", originType, "' and variableType '",
             variableType, "'", call. = FALSE)
    if (! is.null(unit))
        w <- convert_field(w, valueField = "total", to = unit)
    rownames(w) <- NULL
    n <- nrow(w)

    ## ---- the groups: the conditions of the raising, a group's name for both
    as_quo <- function(x, what)
    {
        if (is.character(x)) x <- eval(rlang::parse_expr(paste0("rlang::quo(", x, ")")))
        if (! rlang::is_quosure(x))
            stop(fn, "(): `", what, "` are conditions as catchRaising() takes them, ",
                 "quosures (rlang::quo(metier4 == \"TBB\")) or text; a logical vector refers ",
                 "to the rows of the catch before the raising", call. = FALSE)
        x
    }
    if (length(condition_raising_st_list) != length(condition_matched_data_list))
        stop(fn, "(): `condition_raising_st_list` and `condition_matched_data_list` are the ",
             "conditions of the same groups: they have to be as long", call. = FALSE)
    strata  <- lapply(condition_raising_st_list, as_quo, what = "condition_raising_st_list")
    matched <- lapply(condition_matched_data_list, as_quo, what = "condition_matched_data_list")
    if (is.null(names(strata))) names(strata) <- paste0("G", seq_along(strata))
    names(matched) <- names(strata)
    rows_of <- function(q)
    {
        r <- rep_len(rlang::eval_tidy(q, data = w), n)
        r[is.na(r)] <- FALSE
        r
    }

    ## the group each stratum is raised for: the group whose strata it falls in
    in_strata <- vapply(strata, rows_of, logical(n))
    in_strata <- matrix(in_strata, nrow = n)
    twice <- rowSums(in_strata) > 1 & w$catchCategory %in% c("Lan", "Dis")
    if (any(twice))
        stop(fn, "(): ", sum(twice), " rows fall in the strata of more than ",
             "one group - the strata of `condition_raising_st_list` do not overlap, as ",
             "catchRaising() asks", call. = FALSE)
    group_of <- ifelse(rowSums(in_strata) == 1, names(strata)[max.col(in_strata * 1, "first")],
                       NA_character_)
    raised <- w$importedOrRaised %in% "raised"
    if (any(raised & is.na(group_of)))
        stop(fn, "(): ", sum(raised & is.na(group_of)), " raised rows fall in none of the ",
             "strata of `condition_raising_st_list`: those are not the groups the catch was ",
             "raised with", call. = FALSE)
    if ("DrGroup" %in% names(w))
    {
        other <- ! is.na(w$DrGroup) & (is.na(group_of) | w$DrGroup != group_of)
        if (any(other))
            warning(fn, "(): ", sum(other), " rows were raised in another group than ",
                    "`condition_raising_st_list` puts them in (the DrGroup of the raising): ",
                    "the conditions are not those of the raising", call. = FALSE)
    }

    ## a raised discard with no total (NaN: its group had no donor) is not a result
    keep <- is.finite(w$total)
    if (any(! keep))
        warning(fn, "(): ", sum(! keep), " rows have no total and are not reported (",
                paste0(names(table(paste(w$catchCategory, w$importedOrRaised, group_of)[! keep])),
                       collapse = "; "),
                "): a group with no donor has no discard ratio to raise by", call. = FALSE)

    ## ---- the donors of each group that raised something: the strata of its matched
    ## data with a discard estimate, as catchRaising() has them - the rows the condition
    ## selects, and the others of their domains - and the landings of each domain
    imp <- ! raised
    key <- c("vesselFlagCountry", "year", "workingGroup", "stock", "speciesCode",
             "domainCatchDis", "originType", "variableType")
    dom <- ifelse(is.na(w$domainCatchDis), NA_character_,
                  do.call(paste, c(unname(as.list(w[key])), sep = "\r")))
    ## the landings a raised discard was made from: those of the stratum it shares with them
    skey <- do.call(paste, c(unname(as.list(w[setdiff(stratum, "catchCategory")])), sep = "\r"))
    lan_of <- tapply(w$total[imp & w$catchCategory %in% "Lan" & keep],
                     skey[imp & w$catchCategory %in% "Lan" & keep], sum)
    groups <- names(strata)[names(strata) %in% unique(group_of[raised & keep])]
    source_of <- vector("list", n)
    donors <- list()
    for (g in groups)
    {
        m <- imp & rows_of(matched[[g]])
        m <- (m | (imp & dom %in% unique(dom[m & ! is.na(dom)]))) & ! is.na(dom)
        lan_g <- tapply(w$total[m & w$catchCategory %in% "Lan" & keep],
                        dom[m & w$catchCategory %in% "Lan" & keep], sum)
        dis_g <- which(m & w$catchCategory %in% "Dis" & keep &
                       dom %in% names(lan_g)[lan_g > 0])
        if (! length(dis_g)) next
        feeds <- which(m & w$catchCategory %in% "Lan" & keep & dom %in% dom[dis_g])
        for (r in c(dis_g, feeds)) source_of[[r]] <- c(source_of[[r]], g)
        donors[[g]] <- data.frame(group = g, row = dis_g, dis = w$total[dis_g],
                                  lan = as.numeric(lan_g[dom[dis_g]]), stringsAsFactors = FALSE)
    }
    donors <- if (length(donors)) dplyr::bind_rows(donors)
              else data.frame(group = character(), row = integer(), dis = numeric(),
                              lan = numeric(), stringsAsFactors = FALSE)

    ## the ratio the donors give is the one the raised discards were made with
    for (g in groups)
    {
        r <- which(raised & keep & group_of %in% g & w$catchCategory %in% "Dis")
        base <- as.numeric(lan_of[skey[r]])
        dg <- donors[donors$group == g, , drop = FALSE]
        if (! length(r) || anyNA(base) || ! nrow(dg)) next
        used <- sum(w$total[r]) / sum(base)
        given <- sum(dg$dis) / sum(dg$lan)
        if (is.finite(used) && is.finite(given) && abs(used - given) > 1e-6 * max(1, abs(given)))
            warning(fn, "(): the donors of group '", g, "' give a discard ratio of ",
                    signif(given, 6), ", the raised discards of the group were made with ",
                    signif(used, 6), " - the conditions are not those of the raising, or ",
                    "the catch does not hold all its donors", call. = FALSE)
    }

    ## ---- the CAR: every stratum, imported or raised, with the groups
    labels <- vapply(source_of, function(g)
        if (is.null(g)) NA_character_
        else paste(unique(g)[order(match(unique(g), names(strata)))], collapse = "; "), "")
    car <- w[keep, , drop = FALSE]
    car$imported <- ifelse(raised[keep], "N", "Y")
    car$catchAllocationGroupNameTarget <- ifelse(raised[keep], group_of[keep], NA_character_)
    car$catchAllocationGroupNameSource <- ifelse(raised[keep], NA_character_, labels[keep])
    car$catchAllocationSchemeName <- ifelse(raised[keep], scheme, NA_character_)
    ## a raised discard has no sample of its own: its ratio was borrowed
    for (f in c("numPSUs", "numTrips")) car[[f]][raised[keep]] <- NA
    cell <- c(stratum, "variableType", "variableUnit", "imported", "catchAllocationSchemeName",
              "catchAllocationGroupNameTarget", "catchAllocationGroupNameSource")
    car <- car %>%
        group_by(across(all_of(cell))) %>%
        summarise(total = sum(total),
                  comment = {said <- unique(as.character(comment[! is.na(comment) & nzchar(as.character(comment))]))
                             if (length(said)) paste(said, collapse = "; ") else NA_character_},
                  .groups = "drop") %>%
        left_join(scf_sampling(car, cell, "CC"), by = cell)
    if (anyDuplicated(car[stratum]))
        warning(fn, "(): ", sum(duplicated(car[stratum])),
                " strata are in the CAR more than once, with another group or another ",
                "origin in each of their rows", call. = FALSE)
    ## the format asks for the sampling of every discard (and BMS) stratum that was imported
    asked <- car$imported == "Y" & ! car$catchCategory %in% "Lan"
    lacking <- asked & (is.na(car$PSU) | is.na(car$numPSUs) | is.na(car$numTrips))
    if (any(lacking))
        warning(fn, "(): ", sum(lacking), " of the ", sum(asked), " imported discard (and BMS) ",
                "strata have no PSU, numPSUs or numTrips, which the format asks for",
                call. = FALSE)

    ## ---- the CAS: the donor discard strata of each group
    param <- variableType
    cas <- cbind(donors, w[donors$row, stratum, drop = FALSE]) %>%
        group_by(across(all_of(c("group", stratum)))) %>%
        summarise(dis = sum(dis), weightingTotal = sum(lan), .groups = "drop") %>%
        mutate(catchAllocationGroupName = group,
               catchAllocationSchemeName = scheme,
               rate = dis / weightingTotal,
               averagingMethod = "Weighted",
               weightingCategory = "Lan",
               weightingParameter = param) %>%
        select(-group, -dis)

    if (! is.null(round) && is.numeric(round))
    {
        car <- car %>% mutate(total = round(total, digits = round))
        cas <- cas %>% mutate(weightingTotal = round(weightingTotal, digits = round))
    }
    tables <- list(CAR = scf_table(car, "CAR", header, fn),
                   CAS = scf_table(cas, "CAS", header, fn))

    long <- unique(c(tables$CAR$catchAllocationGroupNameTarget,
                     tables$CAS$catchAllocationGroupName))
    long <- long[! is.na(long) & nchar(long) > 25]
    if (length(long))
        warning(fn, "(): the group names ", paste0("'", long, "'", collapse = ", "),
                " are longer than the 25 characters the SCF takes", call. = FALSE)
    tables
}
