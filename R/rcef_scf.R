# =====================================================================
#  rcef_scf.R   (the Stock Coordination Format: Catch Combined and Distributions Combined)
#
#  ORIGIN: this project (ILVO / RCEF Workbench). Freely refactorable.
#
#  The end of the chain: what catchRaising() and catchAllocationAoL() made is
#  added up into the two tables of the stock coordination format (SCF v3), CC
#  and DC.
#
#    catchCombined()          the CC: the catch added up by year, stock, catch
#                             category and whatever else the SCF reports by;
#    distributionsCombined()  the DC: the numbers and the mean weight at length
#                             or age of the same, in the same long format.
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
#' @return A tibble with the `grouping` columns, `distributionType`,
#'   `distributionClass` (the class; the lowest class of the plus group for the
#'   classes pooled into it), `plusClass` (that lowest class when a plus group
#'   was asked for, `NA` otherwise), `variableUnit` and `value` (the numbers).
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
                       plusGroup = TRUE)
{
    catch_data <- alloc_domain(catch_data, distribution_data)

    ## Conversion (only if relevant unit):
    if (! is.null(unit))
    {
        distribution_data <- distribution_data %>%
            convert_field(valueField = "value",
                          to = unit)
    }

    tmp <- distribution_data %>%
        inner_join(catch_data %>%
                   filter(originType %in% originType_catch,
                          variableType %in% variableType_catch) %>%
                   select(vesselFlagCountry, year, workingGroup,
                          stock, speciesCode, catchCategory,
                          domainBiology,
                          any_of(grouping)),
                   by = join_by(vesselFlagCountry, year, workingGroup,
                                stock, speciesCode, catchCategory, domainBiology),
                   suffix = c(".c", "")) %>%
        filter(variableType %in% c("Number"),
               distributionType %in% !!distributionType)

    classes <- aol_classes(tmp, minAoL, maxAoL, plusGroup)

    tmp %>%
        select(all_of(c(grouping, "distributionClass", "variableUnit", "value"))) %>%
        filter(between(distributionClass, minAoL, classes$last)) %>%
        mutate(distributionClass = pmin(distributionClass, classes$from)) %>%
        dplyr::group_by(across(all_of(c(grouping, "variableUnit", "distributionClass")))) %>%
        summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
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
                         plusGroup = TRUE)
{
    weighting <- match.arg(weighting, c("NumberAtAoL", "CATON"), several.ok = FALSE)

    variableType_dist <- match.arg(variableType_dist,
                                   c("WeightLive"), several.ok = FALSE)

    catch_data <- alloc_domain(catch_data, distribution_data)

    ## Conversion (only if relevant unit):
    if (! is.null(unit))
    {
        distribution_data <- distribution_data %>%
            convert_field(valueField = "value",
                          to = unit)
    }

    catch_data_sel <- catch_data %>%
        filter(originType %in% originType_catch,
               variableType %in% variableType_catch) %>%
        mutate(CATON = total) %>%
        select(vesselFlagCountry, year, workingGroup,
               stock, speciesCode, catchCategory,
               domainBiology, CATON,
               any_of(grouping))

    tmp <- distribution_data %>%
        inner_join(catch_data_sel,
                   by = join_by(vesselFlagCountry, year, workingGroup,
                                stock, speciesCode, catchCategory, domainBiology),
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

    tmp %>%
        select(all_of(c(grouping, "distributionClass", "variableUnit", "value", "CATON", "NaAoL"))) %>%
        filter(between(distributionClass, minAoL, classes$last)) %>%
        mutate(distributionClass = pmin(distributionClass, classes$from)) %>%
        ungroup() %>%
        dplyr::group_by(across(all_of(c(grouping, "variableUnit", "distributionClass")))) %>%
        filter(! is.na(!!sym(wgField))) %>% # Does not handle NAs in weights.
        summarize(value = weighted.mean(x = value,
                                        w = !!sym(wgField),
                                        na.rm = TRUE),
                  .groups = "drop") %>%
        aol_finish(grouping, distributionType, classes, round)
}

# The domainBiology an allocation gives what it estimated a distribution for. The strata
# that have no distribution get one estimated by catchAllocationAoL(), and a domain made up
# from the stratum - "[alloc]_<seasonValue>_<areaValue>_<fisheriesManagementUnit>_
# <fleetValue>_<catchCategory>" - on the distribution and on the stratum of the `$catch` it
# returns. The catch the allocation was run on, the raised catch, has no such domain: its
# strata with no domainBiology are the ones the allocation estimated, and with it none of
# those distributions would be found (a catch of the allocation result is not touched).
# When the distributions have such domains and the catch has none, the catch rows with no
# domainBiology are given the domain the allocation gives them - the same recipe - and it is
# said. The three adding-up functions call it, so they read either catch the same way.
#' @noRd
#' @export
alloc_domain <- function(catch_data, distribution_data)
{
    made_up <- function(x) startsWith(as.character(x), "[alloc]")
    if (! any(made_up(distribution_data$domainBiology), na.rm = TRUE) ||
        any(made_up(catch_data$domainBiology), na.rm = TRUE))
        return(catch_data)

    recipe <- c("seasonValue", "areaValue", "fisheriesManagementUnit", "fleetValue", "catchCategory")
    if (! all(c("domainBiology", recipe) %in% names(catch_data)))
    {
        warning("the distributions have domains an allocation made up (\"[alloc]...\"), which a ",
                "raised catch is given from its ", paste(recipe, collapse = ", "),
                " - the catch lacks ", paste(setdiff(c("domainBiology", recipe), names(catch_data)),
                                             collapse = ", "), ", so they cannot be found",
                call. = FALSE)
        return(catch_data)
    }

    none <- is.na(catch_data$domainBiology)
    if (any(none))
    {
        catch_data$domainBiology <- as.character(catch_data$domainBiology)
        catch_data$domainBiology[none] <-
            paste("[alloc]", catch_data$seasonValue[none], catch_data$areaValue[none],
                  catch_data$fisheriesManagementUnit[none], catch_data$fleetValue[none],
                  catch_data$catchCategory[none], sep = "_")
        message("The catch has no \"[alloc]\" domainBiology, which the distributions of an allocation ",
                "have: the ", sum(none), " catch rows with no domainBiology were given the one the ",
                "allocation gives (season, area, management unit, fleet and catch category). ",
                "The catch of the allocation result (alloc$catch) has them already.")
    }
    catch_data
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
# the plus group named, the columns in their order.
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
               variableUnit, value)
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

# The table as the SCF has it: every field there is, in the order of the format -
# the ones the data has filled, each of its type, and the others `NA` - the header
# of the file (version, calendar year) on every row, the codes checked against
# the ones the format accepts (a code of another kind is a warning, not an error:
# the table is still the table, and the vocabularies grow), and sorted.
#' @noRd
#' @export
scf_table <- function(d, record, version, versionDescription, calendarYear,
                      yearStartDate, fn)
{
    ## The header of the file:
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
        if (! (is.character(yearStartDate) && length(yearStartDate) == 1 &&
               grepl("^(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])$", yearStartDate)))
            stop(fn, "(): with calendarYear = \"N\" `yearStartDate` is the start ",
                 "of the model year, as \"MM-DD\" (\"07-01\")", call. = FALSE)
    }else{
        if (! (length(yearStartDate) == 1 && is.na(yearStartDate)))
            warning(fn, "(): `yearStartDate` is left blank with calendarYear = ",
                    "\"Y\"; the one given is not used", call. = FALSE)
        yearStartDate <- NA_character_
    }

    d <- d %>%
        mutate(recordType = record,
               version = as.integer(version),
               versionDescription = as.character(versionDescription),
               calendarYear = calendarYear,
               yearStartDate = as.character(yearStartDate))

    ## Every field there is, in the order of the format, each of its type:
    head <- c("recordType", "version", "versionDescription", "calendarYear",
              "yearStartDate", "year", "workingGroup", "stock", "catchCategory",
              "seasonType", "seasonValue", "areaType", "areaValue", "fleetValue")
    fields <- switch(record,
                     CC = c(head, "variableType", "variableUnit", "total", "variance",
                            "numTrips"),
                     DC = c(head, "distributionType", "distributionUnit",
                            "distributionClass", "ageGroupPlus", "attributeType",
                            "attributeValue", "variableType", "variableUnit",
                            "valueType", "value", "variance", "numTrips",
                            "numMeasurements"))
    integer_fields <- c("version", "year", "seasonValue", "distributionClass",
                        "ageGroupPlus", "numTrips", "numMeasurements")
    numeric_fields <- c("total", "value", "variance")
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
    check <- function(field, accepted)
    {
        bad <- unique(d[[field]])
        bad <- setdiff(as.character(bad[! is.na(bad)]), accepted)
        if (length(bad))
            warning(fn, "(): ", field, " ", paste0("'", bad, "'", collapse = ", "),
                    " is not a code the SCF accepts (", paste(accepted, collapse = ", "),
                    ")", call. = FALSE)
    }
    check("catchCategory", c("BMS", "Dis", "DisBMS", "Lan", "Catch"))
    check("seasonType", c("Quarter", "Month", "Half-year"))
    check("areaType", c("ICESArea", "ICESAreaNonFAO"))
    weight_units <- if (identical(record, "CC")) c("t", "kg") else c("g", "kg")
    unit_ok <- ifelse(d$variableType %in% "WeightLive", d$variableUnit %in% weight_units,
                      ifelse(d$variableType %in% "Number",
                             d$variableUnit %in% c("N", "NE3"), TRUE))
    if (any(! unit_ok))
        warning(fn, "(): variableUnit ",
                paste0("'", unique(d$variableUnit[! unit_ok]), "'", collapse = ", "),
                " does not go with its variableType (WeightLive: ",
                paste(weight_units, collapse = ", "), "; Number: N, NE3) - give `unit`",
                call. = FALSE)
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
    ## the classes in their order within each:
    order_by <- c("year", "workingGroup", "stock", "catchCategory", "seasonType",
                  "seasonValue", "areaType", "areaValue", "fleetValue",
                  if (identical(record, "DC"))
                      c("distributionType", "attributeType", "attributeValue",
                        "variableType", "distributionClass")
                  else "variableType")
    d %>%
        arrange(across(all_of(order_by)))
}

#' Catch Combined (CC): the catch of a stock, added up, in the format of the stock coordination
#'
#' @description
#' The catch of a stock, the way the stock coordination format (SCF v3) wants it:
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
#' `version` and `versionDescription` name the run, so more than one can be kept
#' (a benchmark, a sensitivity test). `calendarYear` is `"Y"` when `year` is the
#' calendar year, which it is for most stocks, and `"N"` when it is a model year
#' that starts on another date, `yearStartDate` (`"07-01"`), the same for every
#' row of the stock.
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
#' @param version The version of the file, a whole number (default `1`).
#' @param versionDescription What the version is, a string, or `NA` (default).
#' @param calendarYear `"Y"` (default) or `"N"`.
#' @param yearStartDate With `calendarYear = "N"`, the start of the model year
#'   as `"MM-DD"`; left out otherwise.
#' @return A tibble with the CC fields of the SCF: `recordType`, `version`,
#'   `versionDescription`, `calendarYear`, `yearStartDate`, `year`,
#'   `workingGroup`, `stock`, `catchCategory`, `seasonType`, `seasonValue`,
#'   `areaType`, `areaValue`, `fleetValue`, `variableType`, `variableUnit`,
#'   `total`, `variance` and `numTrips`. A code the SCF does not accept is
#'   warned about.
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
#'               version = 1, versionDescription = "WGNSSK 2026, first run")
#'
#' # by quarter as well
#' catchCombined(raised, grouping = c("catchCategory", "seasonValue"),
#'               originType_catch = "WGEstimate", unit = "t")
#'
#' # a model year that starts on 1 July
#' catchCombined(raised, originType_catch = "WGEstimate", unit = "t",
#'               calendarYear = "N", yearStartDate = "07-01")
#' }
#' @export
catchCombined <- function(catch_data,
                          grouping = "catchCategory",
                          originType_catch = c("WGEstimate", "Official"),
                          variableType_catch = c("WeightLive", "Number"),
                          unit = NULL,
                          round = NULL,
                          version = 1,
                          versionDescription = NA_character_,
                          calendarYear = "Y",
                          yearStartDate = NA_character_)
{
    fn <- "catchCombined"
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

    res <- res %>%
        dplyr::group_by(across(all_of(c(grouping, "variableType", "variableUnit")))) %>%
        summarize(total = sum(total, na.rm = TRUE), .groups = "drop")

    if (!is.null(round) && is.numeric(round))
        res <- res %>% mutate(total = round(total, digits = round))

    scf_table(res, "CC", version, versionDescription, calendarYear, yearStartDate, fn)
}

#' Distributions Combined (DC): the numbers and mean weights at length or age, in the format of the stock coordination
#'
#' @description
#' The length or age distributions of a stock, the way the stock coordination
#' format (SCF v3) wants them: the catch's numbers at length (or age) and the
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
#' The distributions are linked to the catch through `domainBiology`, the
#' catch category and the country, and the `grouping` is read off the catch they
#' are linked to. Take **both tables from the same allocation**:
#' `distributionsCombined(alloc$distribution, alloc$catch, ...)`. The
#' distributions the allocation estimated for strata with no biological domain
#' carry a domain it made up from the stratum,
#' `"[alloc]_<season>_<area>_<unit>_<fleet>_<category>"`, and so do those strata in
#' its `$catch`. The raised catch the allocation was run on has no such domain, so
#' the rows of it with no `domainBiology` are given the one the allocation gives
#' (a message says so) and the estimated distributions are found there too - as
#' long as it is the raised catch the allocation was run on. A distribution with no
#' catch row to go with is left out, and a warning says so. Take one
#' `originType_catch` and one
#' `variableType_catch` too, as in [catchAtAoLAggregation()].
#'
#' The `grouping` is the one of [catchCombined()] (and of the same file: a
#' distribution needs a catch of the same year, stock, category, season, area and
#' fleet in the CC), with the sex of the distribution on top -
#' `attributeType` and `attributeValue`, the default - which is `NA` when the
#' distributions do not have one.
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
#' @return A tibble with the DC fields of the SCF: `recordType`, `version`,
#'   `versionDescription`, `calendarYear`, `yearStartDate`, `year`,
#'   `workingGroup`, `stock`, `catchCategory`, `seasonType`, `seasonValue`,
#'   `areaType`, `areaValue`, `fleetValue`, `distributionType`,
#'   `distributionUnit`, `distributionClass`, `ageGroupPlus`, `attributeType`,
#'   `attributeValue`, `variableType`, `variableUnit`, `valueType`, `value`,
#'   `variance`, `numTrips` and `numMeasurements`.
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
#'                       version = 1, versionDescription = "WGNSSK 2026, first run")
#'
#' # ages, with every age from 10 up in a plus group, and the lengths as they are
#' distributionsCombined(alloc$distribution, alloc$catch,
#'                       originType_catch = "WGEstimate",
#'                       variableType_catch = "WeightLive",
#'                       distributionType = c("Age", "LengthTotal"),
#'                       maxAoL = c(Age = 10), plusGroup = c(Age = TRUE, LengthTotal = FALSE))
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
                                  version = 1,
                                  versionDescription = NA_character_,
                                  calendarYear = "Y",
                                  yearStartDate = NA_character_)
{
    fn <- "distributionsCombined"
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

    ## The catch an allocation was run on has no domain for the strata it estimated a distribution
    ## for: given the one the allocation gives (nothing to do with the catch of its result):
    catch_data <- alloc_domain(catch_data, distribution_data)

    ## Every distribution has a catch that goes with it: those that do not are said:
    keys <- c("vesselFlagCountry", "year", "workingGroup", "stock", "speciesCode",
              "catchCategory", "domainBiology")
    dist_keys <- distribution_data %>%
        filter(distributionType %in% types) %>%
        distinct(across(all_of(keys)))
    unlinked <- dist_keys %>%
        anti_join(distinct(catch_data, across(all_of(keys))), by = keys)
    if (nrow(unlinked))
        warning(fn, "(): ", nrow(unlinked), " of the ", nrow(dist_keys),
                " distribution domains have no catch row in `catch_data` and are left ",
                "out - give it the catch of the allocation result (alloc$catch), or the ",
                "raised catch the allocation was run on: the distributions it estimated for ",
                "strata with no biological domain carry a domain made up from the stratum ",
                "(\"[alloc]_...\"), which those two have", call. = FALSE)

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
                            minAoL = min_c, maxAoL = max_c, plusGroup = plus_c) %>%
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

    scf_table(res, "DC", version, versionDescription, calendarYear, yearStartDate, fn)
}
