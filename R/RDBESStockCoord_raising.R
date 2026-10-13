# =====================================================================
#  RDBESStockCoord_raising.R
#
#  ORIGIN: RDBESStockCoord (ICES working group code).
#  Upstream file: R/0_Functions_discards_raising.R
#
#  This file is VENDORED, not authored here. The algorithm, the
#  argument lists and the return shapes are upstream's and must not
#  be changed locally: fixes belong upstream, then get re-vendored.
#
#  Local changes are limited to what packaging requires and are
#  marked with  ## [pkg] :
#    - in-function library()/require() removed (declared in
#      DESCRIPTION Imports instead; attaching from inside a
#      function is an R CMD check failure)
#    - cross-package calls namespace-qualified
#    - CRLF -> LF
#    - roxygen @family / @source added for provenance
#    - a help page for catchRaising(), written here (see the block above it)
#
#  Exported: catchRaising(), grp_catch_raising(), grp_catch_raising_condition()
#
#  Consumes condition_raising_st_list / condition_matched_data_list.
# =====================================================================

#-*- coding: utf-8 -*-

### File: 0_Functions_discards_raising.R
### Time-stamp: <2026-03-13 10:46:38 a23579>
###
### Created: 13/06/2025	15:14:36
### Author: Yves Reecht
###
####################################################################################################
### Description:
### 
### Definition of functions specific to discards raising.
####################################################################################################


##' Discards and BMS raising functions
##'
##' Catch raising for one raising group
##' @title Discards and BMS raising functions
##' @name dis_raising
##' @param raising_st_catch Catch data frame (unified census and estimates) for the raised group,
##'      including data with and without Dis/BMS estimates.
##' @param matched_data_catch Catch data (unified census and estimates) used to estimate the
##'     discard/BMS ratio. Commonly the same as raising_st_census (in which case it can be ignored),
##'     but possibility to borrow information from a wider stratum.
##' @param type The type of catch data to estimate. One of "discards" or "BMS".
##' @return A tibble with `raising_st_catch`, and appended estimated catches + a dataType field
##'     indicating "census", "estimated" or "raised".
##' @author Yves Reecht
##'
##' @noRd
NULL

##' @rdname dis_raising
##'
##' @export
##' @noRd
grp_catch_raising <- function(raising_st_catch,
                              matched_data_catch,
                              originType = c("WGEstimate", "Official"),
                              variableType = c("WeightLive", "Number"),
                              type = c("discards", "BMS"),
                              groupName = NA_character_,
                              verbose = TRUE)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 23 Oct 2024, 12:23

    ## if (groupName %in% c("G10")) browser()

    ## Only for one source and variable type at once:
    originType <- match.arg(originType,
                            c("WGEstimate", "Official"),
                            several.ok = FALSE)

    variableType <- match.arg(variableType,
                              c("WeightLive", "Number"),
                              several.ok = FALSE)

    type <- match.arg(tolower(type), c("discards", "bms"))

    if (type == "bms") stop("BMS not implemented yet!")
    ## Needs to implement matching key switch +
    ##   figure out how to handle possible BMS census information.

    ## Basic check: must work within one year and stock
    if (nrow(unique(raising_st_catch %>% select(year, stock))) > 1)
    {
        stop("Several years and/or stocks in the raising group")
    }

    if (missing(matched_data_catch))
    {
        if (isTRUE(verbose))
        {
            message("matching to \"same\" (matched_data_catch = raising_st_catch)")
        }
        matched_data_catch <- raising_st_catch
    }

    estField <- case_when(type == "discards" ~ "domainCatchDis",
                          type == "bms" ~ "domainCatchBMS",
                          TRUE ~ NA)

    ## Domains that need to be set to NA for raised data:
    otherDomains <- c("domainCatchDis", "domainCatchBMS", "domainBiology") %>%
        {.[! . %in% estField]}

    estCateg <- case_when(type == "discards" ~ "Dis",
                          type == "bms" ~ "BMS",
                          TRUE ~ NA)

    raising_st_catch <- raising_st_catch %>%
        bind_rows(tibble(importedOrRaised = character()))
    
    ## Identify matched data with estimates:
    catch_estimates_cat <- matched_data_catch %>%
        filter(catchCategory %in% c(estCateg),
               ! is.na(total),
               originType %in% {{originType}},
               variableType %in% {{variableType}}, # force evaluating the argument "outside",
                                        # instead of using the fields "variableType".
               ! is.na(!!sym(estField))) %>% 
        dplyr::rename(all_of(c("domainCatch" = estField))) %>%
        dplyr::select(vesselFlagCountry:catchCategory, domainCatch, originType, variableType, total)

    ## Identify categories with estimates:
    reported_catch_cat <- raising_st_catch %>%
        filter(catchCategory %in% c(estCateg),
               ! is.na(total),
               originType %in% {{originType}},
               variableType %in% {{variableType}},
               ! is.na(!!sym(estField))) %>% 
        dplyr::rename(all_of(c("domainCatch" = estField))) %>%
        dplyr::select(vesselFlagCountry:catchCategory, domainCatch, originType, variableType, total)

    ## Separating landings with and without provided discards (or BMS)
    ##   (NA in domain is not reliable => using semi and anti_join):
    land_w_est <- raising_st_catch %>%
        filter(toupper(catchCategory) %in% c("LAN"),
               originType %in% {{originType}},
               variableType %in% {{variableType}}) %>%
        mutate(domainCatch = !!sym(estField)) %>%
        semi_join(reported_catch_cat, 
                  by = c("vesselFlagCountry", "year", "workingGroup", "stock", "speciesCode",
                         "domainCatch", "originType", "variableType")) %>%
        mutate(domainCatch = NULL)

    ## Landings without estimates for the category considered: 
    land_wo_est <- raising_st_catch %>%
        filter(toupper(catchCategory) %in% c("LAN"),
               originType %in% {{originType}},
               variableType %in% {{variableType}}) %>%
        mutate(domainCatch = !!sym(estField)) %>%
        anti_join(reported_catch_cat, 
                  by = c("vesselFlagCountry", "year", "workingGroup", "stock", "speciesCode",
                         "domainCatch", "originType", "variableType"))%>%
        mutate(domainCatch = NULL)

    ## All remaining data:
    catch_other <- raising_st_catch %>%
        mutate(domainCatch = !!sym(estField)) %>%
        anti_join(land_wo_est %>%
                  mutate(domainCatch = !!sym(estField)), 
                  by = c("vesselFlagCountry", "year", "workingGroup", "stock", "speciesCode",
                         "domainCatch", "originType", "variableType")) %>%
        mutate(domainCatch = NULL,
               importedOrRaised = coalesce(importedOrRaised,
                                          "imported"))

    ## dim(raising_st_catch)
    ## dim(land_wo_est)
    ## dim(catch_other)

    ## Need to add conversion (=> harmonized unit) + unit in the output. [!!!]

    ## Data used for estimating the ratio (possibly borrowing info from a wider group):
    data_ratio <- matched_data_catch %>%
        filter(toupper(catchCategory) %in% c("LAN")) %>% # landings on one side...
        dplyr::group_by(vesselFlagCountry, year, workingGroup, stock,
                        speciesCode, originType, variableType, !!sym(estField)) %>%
        dplyr::summarize(landings = sum(total, na.rm = TRUE),
                         .groups = "drop") %>% # weighting based on landing CATON
                                        # (can be made more generic if needed).
        dplyr::rename(all_of(c("domainCatch" = estField))) %>% # make it generic for different types.
        ## ...joined to the corresponding estimates:
        left_join(catch_estimates_cat %>% ## 
                  filter(toupper(catchCategory) %in% toupper(estCateg), # switch if BMS
                         originType %in% {{originType}},
                         variableType %in% {{variableType}}),
                  by = join_by(vesselFlagCountry, year, workingGroup,
                               stock, speciesCode, originType, variableType, domainCatch)) #

    ## Estimate the ratio (hard-coded weighting factor for now, but could become a parameter)
    estimated_ratio <- weighted.mean(x = data_ratio$total / data_ratio$landings,
                                     w = data_ratio$landings, na.rm = TRUE)
    ## ## Alternatively, to account for discards with zero landings:
    ## estimated_ratio <- sum(data_ratio$total, na.rm = TRUE) /
    ##     sum(data_ratio$landings, na.rm = TRUE)

    raisGrpField <- paste0(sub("^(.).*$", "\\1", estCateg),
                           "rGroup")

    land_catch_raised <- land_wo_est %>%
        dplyr::rename(Lan = total) %>%
        dplyr::mutate(!!estCateg := Lan * estimated_ratio, # Generic for BMS.
                      catchCategory = NULL) %>%
        tidyr::pivot_longer(any_of(c("Lan", "Dis", "BMS")),
                            names_to = "catchCategory",
                            values_to = "total") %>%
        mutate(importedOrRaised = if_else(catchCategory %in% c("Dis", "BMS"),
                                          "raised", "imported"),
               across(all_of(otherDomains),
                      ~ if_else(importedOrRaised == "raised",
                                NA_character_, .x)),
               across(all_of(estField),
                      ~coalesce(.x,
                                paste("[raised]",
                                      seasonValue,
                                      areaValue,
                                      fisheriesManagementUnit,
                                      fleetValue, sep = "_"))))
    
    ## Trick to re-order the fields as original + assembled output:
    land_catch_result <- head(land_w_est, 0) %>%
        bind_rows(land_catch_raised) %>%
        bind_rows(catch_other) %>%
        mutate(!!raisGrpField := groupName) # Group name for all records in the stratum.

    ## land_catch_result %>% group_by(cc = catchCategory, importedOrRaised) %>% slice_head(n = 1) %>% as.data.frame()

    return(land_catch_result)
}


##'
##' @rdname dis_raising
##' @param catch_data The catch data table (unified census and estimates).
##' @param condition_raising_st Condition for the selection of a raising startum, provided as a
##'     quosure (`quo(...)`) or text (later converted to a quosure), as usable by the
##'     `dplyr::filter()` function. The function also accepts a logical index vector that matches
##'     the number of rows in `census_data`.
##' @param condition_matched_data Condition for the selection of matching estimates. Similar format
##'     specification as for `condition_raising_st`.
##'
##' @export
##' @noRd
grp_catch_raising_condition <- function(catch_data, 
                                        condition_raising_st,
                                        condition_matched_data = condition_raising_st,
                                        groupName = NA_character_,
                                        originType = c("WGEstimate", "Official"),
                                        variableType = c("WeightLive", "Number"),
                                        type = c("discards", "BMS"), verbose = TRUE)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 24 Oct 2024, 11:04
    ## [pkg] library(rlang) -> Imports: rlang

    ## Only for one source and variable type at once:
    originType <- match.arg(originType,
                            c("WGEstimate", "Official"),
                            several.ok = FALSE)

    variableType <- match.arg(variableType,
                              c("WeightLive", "Number"),
                              several.ok = FALSE)
    
    type <- match.arg(tolower(type), c("discards", "bms"))

    if (is.character(condition_raising_st))
    {
        condition_raising_st <- eval(parse_expr(paste0("quo(", condition_raising_st, ")")))
    }

    if (is.character(condition_matched_data))
    {
        condition_matched_data <- eval(parse_expr(paste0("quo(", condition_matched_data, ")")))
    }

    ## if (groupName %in% c("G5", "G6")) debugonce(grp_catch_raising)#browser()

    ## Catch DF for one group in which the same discard ratio will be applied.
    ##   contains bot landings with discards estimates and landings without (see domainCatchDis key).
    ##
    ## Based on condition 1:
    raising_st_cdf <- catch_data %>% 
        filter(!!condition_raising_st)

    ## raising_st_cdf %>% group_by(is.na(domainCatchDis)) %>%
    ##     sample_n(1) %>% as.data.frame()

    ## Catch DF with data used to estimate the discard ratio:
    ##  * most commonly the same as `raising_st_cdf`, but might include extra data.
    ##  * must make sure the all data corresponding to a given domainCatchDis key are included
    ##    (otherwise the corresponding ratio estimate is wrong).
    ##
    ## Based on condition 2:

    estField <- case_when(type == "discards" ~ "domainCatchDis",
                          type == "bms" ~ "domainCatchBMS",
                          TRUE ~ NA)

    estCateg <- case_when(type == "discards" ~ "Dis",
                          type == "bms" ~ "BMS",
                          TRUE ~ NA)

    ## To be able to check whether all data included, we store a logical group index:
    gidx <- seq_len(nrow(catch_data)) %in%
        (catch_data %>%
         mutate(idxTmp = 1:n()) %>%
         filter(!!condition_matched_data) %>%
         pull(idxTmp))

    catch_data <- catch_data %>%
        ## Add a foreign key that does not include the catchCategory, but the appropriate domain:
        mutate(key = paste(vesselFlagCountry,
                           year,
                           workingGroup,
                           stock,
                           speciesCode,
                           ## seasonType,
                           ## seasonValue,
                           ## areaType,
                           ## areaValue,
                           ## fisheriesManagementUnit,
                           ## metier6,
                           ## fleetType,
                           ## fleetValue,
                           !!sym(estField),
                           originType,
                           variableType))

    matched_data_cdf <- catch_data[gidx, ]

    matched_data_cdf <- matched_data_cdf %>%
        ## Add any missing data with the same domainCatchDis key:
        bind_rows(catch_data[! gidx, ] %>%
                  filter(! is.na(!!sym(estField))) %>% ## Only data linked catch info is relevant.
                  ## Should additionnaly match on stock and year as can be duplicates otherwise(?)...
                  ##   ...now matching the unique key (incl. domain, excl. catchType):
                  filter(key %in%
                         na.omit(dplyr::pull(matched_data_cdf, "key")))) %>% #
        ## Filter out data without discard/BMS/... estimates:
        filter(! is.na(!!sym(estField))) #

    if (is.null(groupName)) groupName <- NA_character_

    ## groupName <<- groupName

    ## Make get raised data (catch format + importedOrRaised):
    grp_catch_raising(raising_st_catch = raising_st_cdf,
                      matched_data_catch = matched_data_cdf,
                      originType = originType,
                      variableType = variableType,
                      type = type,
                      groupName = groupName,
                      verbose = verbose)##  %>%
        ## mutate(DrGroup = groupName) # should be moved within the upper function [!!!]

}



## [pkg] Help page written here: the upstream block documents the three raising
## functions together on one page that is not built (@noRd).
##'
##' Raise the discards of each group from its donor pool
##'
##' @description
##' Gives the landings that have no discard estimate a raised one, group by
##' group. The discard ratio comes from a donor pool of matched data that can be
##' wider than the group itself. The result is the catch with the raised
##' discards added, every row flagged `"imported"` or `"raised"` and tagged with
##' its group.
##'
##' @details
##' ## Groups
##'
##' A group is a pair of conditions, one from `condition_raising_st_list` and
##' one from `condition_matched_data_list`: the first selects the catch rows
##' that are raised together (the raising stratum), the second the rows whose
##' landings and discards give the ratio (the donor pool, or matched data). The
##' two lists have the same length and the same names - a group's name, `G1`,
##' `G2`, ... when the lists have none - and each condition is written as in
##' `dplyr::filter()`: a quosure (`rlang::quo(metier4 == "TBB")`), a text that
##' is turned into one, or a logical vector with a value for every row of
##' `catch_data`.
##'
##' The raising strata must not overlap - a catch row in two of them is an
##' error, which [check_group_conditions()] reports before anything is raised.
##' The matched data may overlap, and usually do: a group can draw on a wider
##' pool than its own strata. Every group is raised within one year and one
##' stock.
##'
##' ## What is raised
##'
##' In a raising stratum the landings that have no discard estimate are the
##' ones raised: those whose `domainCatchDis` has no discards row of the chosen
##' `originType` and `variableType`. Each gets a discards row of its landings
##' times the group's discard ratio. The ratio is the mean of discards over
##' landings of the matched data, weighted by the landings - the total discards
##' over the total landings of the matched strata that have a discard estimate.
##'
##' A raised row has `catchCategory` `"Dis"` and `importedOrRaised`
##' `"raised"`; its `domainCatchDis` is the landings' own, or
##' `"[raised]_<season>_<area>_<management unit>_<fleet>"` when they had none,
##' and `domainBiology` and `domainCatchBMS` are `NA`. Every other row of the
##' stratum - landings, the discards that were reported, BMS - is kept as it is
##' and flagged `"imported"`.
##'
##' `total` is converted first, to kg for weights and to thousands (`NE3`) for
##' numbers. `catch_data` is a CEF long-format table whose columns run in CEF
##' order - `vesselFlagCountry`, `year`, `workingGroup`, `stock`, `speciesCode`,
##' `catchCategory` one after another - as they do in a table read from CEF
##' files.
##'
##' @param catch_data The catch, a CEF long-format table, with the columns the
##'     conditions refer to (see [derive_fields()]).
##' @param condition_raising_st_list The conditions that select the raising
##'     strata, one per group - see *Groups*.
##' @param condition_matched_data_list The conditions that select the matched
##'     data of each group; same length and names as
##'     `condition_raising_st_list`. Defaults to the raising strata themselves.
##' @param originType `"WGEstimate"` (default) or `"Official"`: the origin of the
##'     rows that are raised. One at a time.
##' @param variableType `"WeightLive"` (default) or `"Number"`: the variable of
##'     the rows that are raised. One at a time.
##' @param type `"discards"` (default) or `"BMS"`. Only discards are
##'     implemented; `"BMS"` stops with an error.
##' @param verbose Print messages while raising.
##' @param assembled_output `TRUE` (default): the raised discards together with
##'     the imported rows of the raising strata; `FALSE`: only the raised
##'     discards.
##' @param append Whether to append to the log file (`FALSE`: overwrite it).
##' @param ... Passed on to [check_group_conditions()], e.g. `logFile`, where the
##'     check of the conditions is written.
##' @return A tibble with the columns of `catch_data` plus `importedOrRaised`
##'     (`"imported"` or `"raised"`) and `DrGroup`, the name of the group, on
##'     every row. Rows of `catch_data` that fall in no raising stratum are not
##'     part of it.
##' @seealso [define_raising_groups_auto()] and [define_raising_groups_manual()]
##'     for conditions made from the data; [plot_raised_catches()],
##'     [plot_raised_DR()] and [raised_overview_table()] to look at the result;
##'     [catchAllocationAoL()] for the lengths and ages.
##' @author Yves Reecht
##' @md
##' @examples
##' \dontrun{
##' # one group per fleet, each raised from its own discard ratio
##' strataCond <- list(TBB = rlang::quo(metier4 == "TBB"),
##'                    OTB = rlang::quo(metier4 == "OTB"))
##' raised <- catchRaising(catch, strataCond, verbose = FALSE,
##'                        logFile = tempfile(fileext = ".txt"))
##' table(raised$DrGroup, raised$importedOrRaised)
##'
##' # the same groups, both drawing their ratio on every beam and otter trawl
##' donors <- rlang::quo(metier4 %in% c("TBB", "OTB"))
##' raised <- catchRaising(catch, strataCond,
##'                        condition_matched_data_list = list(TBB = donors, OTB = donors),
##'                        verbose = FALSE, logFile = tempfile(fileext = ".txt"))
##' }
##' @export
catchRaising <- function(catch_data, 
                         condition_raising_st_list,
                         condition_matched_data_list = condition_raising_st_list,
                         originType = c("WGEstimate", "Official"),
                         variableType = c("WeightLive", "Number"),
                         type = c("discards", "BMS"),
                         verbose = TRUE,
                         assembled_output = TRUE,
                         append = FALSE,
                         ...)
{
    ## Purpose: 
    ##   * Tests for validity of conditions.
    ##   * Loops over condition pairs (raised stratum, matched data) and
    ##     apply raising.
    ##   * Formats data (output with or without census and estimated data).
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 24 Oct 2024, 13:55
    ## [pkg] library(rlang) -> Imports: rlang
    ## [pkg] library(dplyr) -> Imports: dplyr

    ## Only for one source and variable type at once:
    originType <- match.arg(originType,
                            c("WGEstimate", "Official"),
                            several.ok = FALSE)

    variableType <- match.arg(variableType,
                              c("WeightLive", "Number"),
                              several.ok = FALSE)

    type <- match.arg(tolower(type), c("discards", "bms"))
    domainLookup <- c(discards = "domainCatchDis",
                      bms = "domainCatchBMS")

    ## Harmonization of units:
    catch_data <- catch_data %>%
        convert_field(valueField = "total",
                      to = c("kg", "NE3"))

    ## browser()

    ## ##################################################
    ## Check consistency:

    ## between condition list length:
    stopifnot(length(condition_raising_st_list) == length(condition_matched_data_list))

    ##
    condition_raising_st_list <-
        check_group_conditions(catch_data = catch_data,
                               condition_list = condition_raising_st_list,
                               conditionType = "strata",
                               domain = domainLookup[type],
                               originType = originType,
                               variableType = variableType,
                               append = append,
                               ...)

    condition_matched_data_list <-
        check_group_conditions(catch_data = catch_data,
                               condition_list = condition_matched_data_list,
                               conditionType = "matched_data",
                               domain = domainLookup[type],
                               originType = originType,
                               variableType = variableType,
                               append = TRUE,
                               ...)

    ## Fill in group names as needed:
    if (is.null(names(condition_raising_st_list)))
    {
        names(condition_raising_st_list) <-
            paste0("G", seq_len(length.out = length(condition_raising_st_list)))
    }

    if (is.null(names(condition_matched_data_list)))
    {
        names(condition_matched_data_list) <-
            names(condition_raising_st_list)
    }

    ## ##################################################
    ## Raising:

    ## Loops through conditions
    res <- mapply(grp_catch_raising_condition,
                  condition_raising_st = condition_raising_st_list,
                  condition_matched_data = condition_matched_data_list,
                  groupName = names(condition_raising_st_list),
                  MoreArgs = list(catch_data, 
                                  type = type, 
                                  originType = originType,
                                  variableType = variableType,
                                  verbose = verbose),
                  SIMPLIFY = FALSE)

    res <- bind_rows(res)

    ## head(res, 3) %>% as.data.frame()
    ## table(res$DrGroup, res$importedOrRaised, useNA = "ifany")
    ## table(res$catchCategory, res$variableType)

    ## ##################################################
    ## Post-processing:

    catch_data %>%
        group_by(catchCategory, is.na(domainCatchDis), is.na(total)) %>%
        slice_sample(n = 1) %>% as.data.frame()

    ## Already unified, remove imported and other categories/dataTypes if required:
    if (!isTRUE(assembled_output))
    {
        estCateg <- case_when(type == "discards" ~ "Dis",
                          type == "bms" ~ "BMS",
                          TRUE ~ NA)
        ## Practically not used... should it include landings if it ever is? 
        res <- res %>%
            filter(originType %in% {{originType}},
                   variableType %in% {{variableType}},
                   importedOrRaised %in% "raised", #  
                   catchCategory %in% c(estCateg))
    }

    return(res)
}






### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 100
### End:
