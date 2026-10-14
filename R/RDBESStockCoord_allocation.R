# =====================================================================
#  RDBESStockCoord_allocation.R
#
#  ORIGIN: RDBESStockCoord (ICES working group code).
#  Upstream file: R/0_Functions_age-length_alloc.R
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
#    - help pages for catchAllocationAoL(), catchAtAoLAggregation() and
#      meanWoLatAoLAggregation(), written here (upstream documents none)
#    - meanWoLatAoLAggregation(weighting = "CATON") reads the CATON column (the catch's
#      total weight) instead of `total`, which is not there
#    - distributionType takes CEF's own value "LengthTotal" where upstream
#      says "Length": the four argument defaults and the three match.arg()
#      calls (seven places, each marked)
#
#  Exported: catchAllocationAoL(), catchAtAoLAggregation(), meanWoLatAoLAggregation(),
#            grp_AoL_alloc_N(), grp_AoL_alloc_WL(), grp_AoL_alloc_condition()
#
#  distributionType is matched against c("Age","LengthTotal"), the values
#  CEF writes, so the distributions table goes in as it is and the
#  aggregations name their columns N_LengthTotal_<class> / W_LengthTotal_<class>.
#  Upstream matches "Length" and callers had to rewrite the column before
#  the call; a re-vendor has to carry the seven marked edits over.
# =====================================================================

#-*- coding: utf-8 -*-

### File: 0_Functions_age-length_alloc.R
### Time-stamp: <2026-06-15 12:59:08 a23579>
###
### Created: 21/10/2025	16:54:17
### Author: Yves Reecht
###
####################################################################################################
### Description:
### 
### 
####################################################################################################

#' @noRd
#' @export
grp_AoL_alloc_N <- function(alloc_st_catch,
                            matched_data_catch,
                            distribution_data,
                            groupName = NA_character_,
                            distributionType = c("Age", "LengthTotal"), ## [pkg] CEF value
                            verbose = TRUE)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 22 Oct 2025, 14:58

    distributionType <- match.arg(distributionType, c("Age", "LengthTotal"), several.ok = FALSE) ## [pkg] CEF value

    ## Basic check: must work within one year and stock
    if (nrow(unique(alloc_st_catch %>% select(year, stock))) > 1)
    {
        stop("Several years and/or stocks in the alloc group")
    }

    if (missing(matched_data_catch))
    {
        if (isTRUE(verbose))
        {
            message("matching to \"same\" (matched_data_catch = alloc_st_catch)")
        }
        matched_data_catch <- alloc_st_catch
    }

    ## dim(distribution_data)
    ## distribution_data %>% group_by(variableType) %>% slice(2) %>% as.data.frame()
    ## matched_data_catch %>% group_by(originType, variableType) %>% slice_head(n = 1) %>% as.data.frame()

    ## In case distribution_data is provided with sampledOrEstimated, this is kept; added otherwise:
    distribution_data <- distribution_data %>%
        bind_rows(tibble(sampledOrEstimated = character())) # Adds the column if missing (to
                                        # allow coalesce(...)).

    ## Merged catch data with sampled catch-at-age|length:
    catch_samp_N <- matched_data_catch %>%
        filter(! is.na(domainBiology)) %>%
        semi_join(distribution_data %>%
                  filter(distributionType %in% {{distributionType}},
                         variableType %in% "Number"),
                  by = c("vesselFlagCountry", "year", "workingGroup", "stock", "speciesCode",
                         "catchCategory", "domainBiology")) %>%
        mutate(CATON = sum(total, na.rm = TRUE)) %>% # After semi_join to ensure correct estimate.
        inner_join(distribution_data %>%
                   filter(distributionType %in% {{distributionType}},
                          variableType %in% "Number"),
                  by = c("vesselFlagCountry", "year", "workingGroup", "stock", "speciesCode",
                         "catchCategory", "domainBiology"),
                  suffix = c(".c", ""))

    ## catch_samp_N %>% head(2) %>% as.data.frame()
    ## ## In the unlikely even where there are NA-catch weights, the data should not be used for
    ## ## raising the catch numbers (but kept if provided numbers):
    ## catch_samp_N_unused <- catch_samp_N %>%
    ##     filter(is.na(total)) ## actually not necessary.

    catch_samp_N <- catch_samp_N %>%
        filter( ! is.na(total))

    if (nrow(catch_samp_N) == 0)
    {
        if (verbose)
            message("\n## No distribution info for raising N-at-", distributionType,
                    " in group \"", groupName, "\"")
        
        return(NULL)
    }

    ## dim(catch_samp_N)
    ## catch_samp_N %>% group_by(variableType) %>% slice_head(n=2) %>% as.data.frame()
    
    
    ## table(distribution_data$distributionType)
    ## distributionType <- distributionType2
    ## distributionType <- "bla"

    ## Catch data with sampled catch-at-age|length:
    catch_w_est_N_samp <- alloc_st_catch %>%
        inner_join(distribution_data %>%
                   filter(distributionType %in% {{distributionType}},
                          variableType %in% "Number"),
                   by = c("vesselFlagCountry", "year", "workingGroup", "stock", "speciesCode",
                          "catchCategory", "domainBiology"),
                   suffix = c(".c", ""))
        ## semi_join(distribution_data %>%
        ##           filter(distributionType %in% {{distributionType}},
        ##                  variableType %in% "Number") %>%
        ##           select(all_of(c("vesselFlagCountry", "year",
        ##                           "workingGroup", "stock", "speciesCode",
        ##                           "catchCategory", "domainBiology"))),
        ##           by = c("vesselFlagCountry", "year",
        ##                  "workingGroup", "stock", "speciesCode",
        ##                  "catchCategory", "domainBiology"))

    ## Catch data without sampled catch-at-age|length:
    catch_wo_est_N <- alloc_st_catch %>%
        anti_join(distribution_data %>%
                  filter(distributionType %in% {{distributionType}},
                         variableType %in% "Number") %>%
                  select(all_of(c("vesselFlagCountry", "year",
                                  "workingGroup", "stock", "speciesCode",
                                  "catchCategory", "domainBiology"))),
                  by = c("vesselFlagCountry", "year",
                         "workingGroup", "stock", "speciesCode",
                         "catchCategory", "domainBiology"))

    any(!is.na(catch_wo_est_N$domainBiology)) # Tests
    any(is.na(catch_w_est_N_samp$domainBiology))
    any(is.na(catch_w_est_N_samp$total))
    
    ## dim(alloc_st_catch)
    ## dim(catch_wo_est_N)
    ## dim(catch_w_est_N_samp)

    ## Aggregated sampled-numbers at age|length:
    aggr_N_samp <- catch_samp_N %>%
        dplyr::group_by(distributionType, distributionUnit, distributionClass,
                        variableType, variableUnit,
                        attributeType, attributeValue,
                        valueType) %>%
        dplyr::summarize(CATON.samp = mean(CATON, na.rm = TRUE), # Already aggregated for sampled catch
                         value.samp = sum(value, na.rm = TRUE),
                         across(all_of(c("PSU")), # Not sure how those should be handled.
                                ~paste(unique(.x, collapse = "+"))),
                         ageGroupPlus = any(ageGroupPlus), # Not sure how those should be handled.
                         across(all_of(c("numPSUs", "numTrips",
                                         "numMeasurements")),
                                ~ NA),
                         .groups = "drop") 

    ## aggr_N_samp %>% as.data.frame() %>% head()

    ## catch_wo_est_N %>% slice_head(n = 2) %>% as.data.frame()

    ## Estimated catch numbers at age|length, collated with unsampled catch:
    catch_bio_est_N <- catch_wo_est_N %>%
        mutate(rowk = 1:n()) %>%
        group_by(rowk) %>%
        group_modify(function(cdata, key, aggr_N_samp)
        {
            ## browser()
            cdata %>%
                rename_with(~paste0(.x, ".c"),
                            all_of(c("variableType", "variableUnit", "numTrips", "numPSUs", "PSU"))) %>%
                bind_cols(aggr_N_samp) %>%
                mutate(value = value.samp * total / CATON.samp,
                       sampledOrEstimated = "estimated") 
                ## head(2) %>% as.data.frame()
        }, aggr_N_samp = aggr_N_samp) %>%
        bind_rows() %>%
        ungroup()

    ## Combined N-distributions:
    distribution_N <- head(distribution_data, 0) %>%
        ## Sampled N-ditributions:
        bind_rows(catch_w_est_N_samp %>%
                  dplyr::select(all_of(colnames(distribution_data))) %>%                  
                  mutate(sampledOrEstimated =  coalesce(sampledOrEstimated,
                                                        "sampled"))) %>% # do not override if
                                                          # already provided.
        ## collated with estimated N-distributions
        bind_rows(catch_bio_est_N %>%
                  ## new domainBiology for estimated distributions (with a prefix):
                  mutate(domainBiology =
                             coalesce(domainBiology,
                                      paste("[alloc]",
                                            seasonValue,
                                            areaValue,
                                            fisheriesManagementUnit,
                                            fleetValue,
                                            catchCategory, sep = "_"))) %>%
                  dplyr::select(c(any_of(colnames(distribution_data)),
                                  "sampledOrEstimated"))) %>%
        mutate(allocGroup = groupName)

    ## distribution_N %>%
    ##     group_by(sampledOrEstimated) %>%
    ##     slice_head(n = 2) %>% as.data.frame()

    return(distribution_N)

    ## catch_bio_est_N %>% group_by(!is.na(domainBiology)) %>% slice_head(n = 2) %>% as.data.frame()
}

#' @noRd
#' @export
grp_AoL_alloc_WL <- function(alloc_st_catch,
                             matched_data_catch,
                             distribution_data,
                             ## distribution_data_N = distribution_data, # in case there may be
                                        # some estimated N with provided W or L. 
                             groupName = NA_character_,
                             variableType_mean = c("WeightLive"), # Here for the averaged
                                        # quantity. Only mean weight for now.
                             weighting = c("NumberAtAoL", "CATON"), # weighting for mean weights at age|length.
                             distributionType = c("Age", "LengthTotal"), ## [pkg] CEF value
                             verbose = TRUE)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 22 Oct 2025, 14:58
    ## ###########################################################################
    ## Weights-at-age or -length:

    distributionType <- match.arg(distributionType, c("Age", "LengthTotal"), several.ok = FALSE) ## [pkg] CEF value
    weighting <- match.arg(weighting,
                           c("NumberAtAoL", "CATON"), several.ok = FALSE)

    variableType_mean <- match.arg(variableType_mean,
                                   c("WeightLive"), # Mean size?
                                   several.ok = FALSE)

    ## In the hypothetical case weight samples are provided without numbers sampled,
    ##   use number estimates for the weighting.
    ## [???] Is this a case that can actually arise? Or is this behaviour even desirable?
    ##   * if not, remove distribution_data_N parameter and use distribution_data instead.
    ##   * if so, may be better to loop on all number estimations (fun catchAtAoLAggregation)
    ##     and pass all estimates when looping over groups for weights.
    ##  Decision is NOT to use allocated distribution for now: => overriden with distribution_data.
    distribution_data_N <- distribution_data ## %>%
        ## bind_rows(filter(distribution_data_N, sampledOrEstimated %in% "estimated"))

    ## table(distribution_data$variableType)
    ## table(distribution_data_N$sampledOrEstimated, useNA = "always")

    ## In case distribution_data is provided with sampledOrEstimated, this is kept; added otherwise:
    distribution_data <- distribution_data %>%
        bind_rows(tibble(sampledOrEstimated = character())) # Adds the column if missing (to
                                        # allow coalesce(...)).
    
    distribution_data_N <- distribution_data_N %>%
        bind_rows(tibble(sampledOrEstimated = character())) # Adds the column if missing (to
                                        # allow coalesce(...)).

    ## 
    catch_samp_W <- matched_data_catch %>%
        filter(! is.na(domainBiology)) %>%
        ## Add sum of catch weight for the sampled fraction:
        semi_join(distribution_data %>%
                  filter(distributionType %in% {{distributionType}},
                         variableType %in% variableType_mean), # "WeightLive"
                  by = c("vesselFlagCountry", "year", "workingGroup", "stock", "speciesCode",
                         "catchCategory", "domainBiology")) %>%
        mutate(CATON = sum(total, na.rm = TRUE)) %>% # After semi_join (~filtering) to ensure
                                        # correct estimate. 
        ## Linked to sample-based mean weights at age|length:
        inner_join(distribution_data %>%
                   filter(distributionType %in% {{distributionType}},
                          variableType %in% variableType_mean),
                   by = c("vesselFlagCountry", "year", "workingGroup", "stock", "speciesCode",
                          "catchCategory", "domainBiology"),
                   suffix = c(".c", "")) %>%
        left_join(distribution_data_N  %>%
                  filter(distributionType %in% {{distributionType}},
                         variableType %in% "Number") %>% 
                  select(all_of(c("vesselFlagCountry", "year", "workingGroup", "stock", "speciesCode",
                                  "catchCategory", "domainBiology",
                                  "distributionType", "distributionUnit", "distributionClass", "ageGroupPlus",
                                  "attributeType", "attributeValue",
                                  "value"))) %>%
                  rename(NaAoL = value),
                  by = c("vesselFlagCountry", "year", "workingGroup", "stock", "speciesCode",
                         "catchCategory", "domainBiology",
                         "distributionType", "distributionUnit", "distributionClass", "ageGroupPlus",
                         "attributeType", "attributeValue"))

    ## select(filter(catch_samp_W, distributionClass == 2),
    ##        CATON, fleetValue, total, value, NaAoL) %>% as.data.frame()

    ## In case of CATON weighting... Nope, not needed because of na.rm = TRUE
    ## if (weighting == "CATON")
    ## {
    ##     catch_samp_W_unused <- catch_samp_W %>%
    ##         filter(is.na(total))

    ##     catch_samp_W <- catch_samp_W %>%
    ##         filter( ! is.na(total))
    ## }else{
    ##     catch_samp_W_unused <- catch_samp_W %>%
    ##         head(0)
    ## }

    ## catch_samp_W %>%
    ##     head(2) %>% as.data.frame()

    ## dim(catch_samp_W)
    ## catch_samp_W %>% group_by(variableType) %>% slice_head(n=2) %>% as.data.frame()

    catch_w_est_W_samp <- alloc_st_catch %>%
        ## Linked to sample-based mean weights at age|length:
        inner_join(distribution_data %>%
                   filter(distributionType %in% {{distributionType}},
                          variableType %in% variableType_mean),
                   by = c("vesselFlagCountry", "year", "workingGroup", "stock", "speciesCode",
                          "catchCategory", "domainBiology"),
                   suffix = c(".c", ""))
        ## semi_join(distribution_data %>%
        ##           filter(distributionType %in% {{distributionType}},
        ##                  variableType %in% "WeightLive") %>%
        ##           select(all_of(c("vesselFlagCountry", "year",
        ##                           "workingGroup", "stock", "speciesCode",
        ##                           "catchCategory", "domainBiology"))),
        ##           by = c("vesselFlagCountry", "year",
        ##                  "workingGroup", "stock", "speciesCode",
        ##                  "catchCategory", "domainBiology"))

    catch_wo_est_W <- alloc_st_catch %>%
        anti_join(distribution_data %>%
                  filter(distributionType %in% {{distributionType}},
                         variableType %in% variableType_mean) %>% # "WeightLive"
                  select(all_of(c("vesselFlagCountry", "year",
                                  "workingGroup", "stock", "speciesCode",
                                  "catchCategory", "domainBiology"))),
                  by = c("vesselFlagCountry", "year",
                         "workingGroup", "stock", "speciesCode",
                         "catchCategory", "domainBiology"))

    ## any(!is.na(catch_wo_est_W$domainBiology)) # Tests
    ## any(is.na(catch_w_est_W_samp$domainBiology))
    
    ## dim(alloc_st_catch)
    ## dim(catch_wo_est_W)
    ## dim(catch_w_est_W_samp)

    ## Field used for weighting, based on user choice:
    wgField <- case_when(weighting == "NumberAtAoL" ~ "NaAoL",
                         weighting == "CATON" ~ "total",
                         TRUE ~ NA)
    if (is.na(wgField))
        stop("mean weighting \"", weighting,
             "\" not implemented for weights-at-age|length")
    ## if (any(is.na(catch_samp_W$NaAoL))) browser() [!!!]
    ## Aggregation of mean W or L over per Age/LengthTotal class + Attributes:
    aggr_W_samp <- catch_samp_W %>%
        mutate(across(all_of(wgField), ~ ifelse(is.na(.x), 0, .x))) %>%
        dplyr::group_by(distributionType, distributionUnit, distributionClass,
                        variableType, variableUnit,
                        attributeType, attributeValue,
                        valueType) %>%
        dplyr::summarize(CATON.samp = mean(CATON, na.rm = TRUE), # Already aggregated for sampled
                                        # catch
                         N.samp = sum(NaAoL, na.rm = TRUE),
                         ## The main estimate... weighted mean weight or length at age/size class:
                         value.samp = weighted.mean(x = value,
                                                    w = !!sym(wgField),
                                                    na.rm = TRUE),
                         across(all_of(c("PSU")), # Not sure how those should be handled.
                                ~paste(unique(.x, collapse = "+"))),
                         ageGroupPlus = any(ageGroupPlus), # Not sure how those should be handled.
                         across(all_of(c("numPSUs", "numTrips",
                                         "numMeasurements")),
                                ~ NA),
                         .groups = "drop")

    ## aggr_W_samp %>% head(5) %>% as.data.frame()

    ## catch_wo_est_W %>% slice_head(n = 2) %>% as.data.frame()

    ## Estimated catch numbers at age|length, collated with unsampled catch:
    catch_bio_est_W <- catch_wo_est_W %>%
        mutate(rowk = 1:n()) %>%
        group_by(rowk) %>%
        group_modify(function(cdata, key, aggr_W_samp)
        {
            ## browser()
            cdata %>%
                rename_with(~paste0(.x, ".c"),
                            all_of(c("variableType", "variableUnit", "numTrips", "numPSUs", "PSU"))) %>%
                bind_cols(aggr_W_samp) %>%
                mutate(value = value.samp, # just repeating the estimated means for all catches
                                        # without samples.
                       sampledOrEstimated = "estimated") 
        }, aggr_W_samp = aggr_W_samp) %>%
        bind_rows() %>%
        ungroup()

    ## Combined mean W/L-at-age/length distributions:
    distribution_W <-  head(distribution_data, 0) %>%
        ## Sampled N-ditributions:
        bind_rows(catch_w_est_W_samp %>%
                  dplyr::select(all_of(colnames(distribution_data))) %>%
                  mutate(sampledOrEstimated =  coalesce(sampledOrEstimated,
                                                        "sampled"))) %>% # do not override if
                                        # already provided.
        ## collated with estimated W(or L)-distributions:
        bind_rows(catch_bio_est_W %>%
                  ## new domainBiology for estimated distributions (with a prefix):
                  mutate(domainBiology =
                             coalesce(domainBiology,
                                      paste("[alloc]",
                                            seasonValue,
                                            areaValue,
                                            fisheriesManagementUnit,
                                            fleetValue,
                                            catchCategory, sep = "_"))) %>%
                  dplyr::select(c(any_of(colnames(distribution_data)),
                                  "sampledOrEstimated"))) %>%
        mutate(allocGroup = groupName)

    ## distribution_W %>%
    ##     group_by(sampledOrEstimated) %>%
    ##     slice_head(n = 2) %>% as.data.frame()

    return(distribution_W)
}


#' @noRd
#' @export
grp_AoL_alloc_condition <- function(catch_data, 
                                    distribution_data,
                                    condition_alloc_st,
                                    condition_matched_data = condition_alloc_st,
                                    groupName = NA_character_,
                                    originType = c("WGEstimate", "Official"),
                                    variableType = c("WeightLive", "Number"),
                                    variableType_mean = c("WeightLive"), # For the averaged
                                        # quantity. Only mean weight for now; complete for mean
                                        # length when defined.
                                    weighting = c("NumberAtAoL", "CATON"), # weighting for mean weights at age|length.
                                    distributionType = c("Age", "LengthTotal"), ## [pkg] CEF value
                                    verbose = TRUE)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 22 Oct 2025, 09:47
    ## [pkg] library(rlang) -> Imports: rlang

    ## Only for one source and variable type at once:
    originType <- match.arg(originType,
                            c("WGEstimate", "Official"),
                            several.ok = FALSE) # Or should it allows to raise together WG and
                                        # Official weights or numbers? 

    variableType <- match.arg(variableType,
                                    c("WeightLive", "Number"),
                                    several.ok = FALSE) # (not relevant to mix weights
                                        # and numbers for catch-number-at-age or length.)
    
    distributionType <- match.arg(distributionType, c("Age", "LengthTotal"), several.ok = TRUE) ## [pkg] CEF value
    variableType_mean <- match.arg(variableType_mean,
                                   c("WeightLive"), several.ok = TRUE)

    if (is.character(condition_alloc_st))
    {
        condition_alloc_st <- eval(parse_expr(paste0("quo(", condition_alloc_st, ")")))
    }

    if (is.character(condition_matched_data))
    {
        condition_matched_data <- eval(parse_expr(paste0("quo(", condition_matched_data, ")")))
    }

    ## browser()

    ## Catch DF for one group in which the same age/length structure will be applied.
    ##   contains contains both catches with and without estimates (see domainBiology key).
    ##
    ## Based on condition 1:
    alloc_st_cdf <- catch_data %>% 
        filter(!!condition_alloc_st,
               originType %in% {{originType}},
               variableType %in% {{variableType}}) # Limited to data of one variable type.
                                        # {{}} necessary to force evaluating from the argument.

    ## table(catch_data$variableType)
    ## table(catch_data$originType)
    ## table(alloc_st_cdf$variableType)
    ## head(alloc_st_cdf, 2) %>% as.data.frame()

    ## Catch DF with data used to estimate the age/length structure:
    ##  * most commonly the same as `raising_st_cdf`, but might include extra data.
    ##  * must make sure that all data corresponding to a given domainBiology key are included
    ##    (otherwise the corresponding ratio estimates - N/CATON, in particular - are wrong).
    ##
    ## Based on condition 2:

    ## To be able to check whether all data included, we store a logical group index:
    gidx <- seq_len(nrow(catch_data)) %in%
        (catch_data %>%
         mutate(idxTmp = 1:n()) %>%
         filter(!!condition_matched_data) %>%
         pull(idxTmp))

    ## head(na.omit(catch_data$domainBiology))

    catch_data <- catch_data %>%
        ## Add a foreign key that includes the appropriate domain
        ## + catch category and variable type: 
        mutate(key = paste(vesselFlagCountry,
                           year,
                           workingGroup,
                           stock,
                           speciesCode,
                           catchCategory, # important to avoid duplication.
                           domainBiology, # domaine
                           originType,
                           variableType))

    ## catch_data %>% group_by(cc = catchCategory, !is.na(domainBiology)) %>% slice(1) %>% as.data.frame()

    matched_data_cdf <- catch_data[gidx, ]

    matched_data_cdf <- matched_data_cdf %>%
        ## Add any missing data with the same domainBiology key:
        bind_rows(catch_data[! gidx, ] %>%
                  filter(! is.na(domainBiology)) %>% # Only data with biological info is relevant.
                  ## Should additionnaly match on stock and year as can be duplicates otherwise(?)...
                  ##   ...now matching the foreign key (incl. domain and catchCategory):
                  filter(key %in%
                         na.omit(dplyr::pull(matched_data_cdf, "key")))) %>% #
        ## Filter out data without discard/BMS/... estimates:
        filter(! is.na(domainBiology), # Only data with biological info is relevant.
               originType %in% {{originType}},
               variableType %in% {{variableType}})

    if (is.null(groupName)) groupName <- NA_character_

    resN <- sapply(distributionType,
                  function(distributionType.i)
           {
               grp_AoL_alloc_N(alloc_st_catch = alloc_st_cdf,
                               matched_data_catch = matched_data_cdf,
                               distribution_data = distribution_data,
                               groupName = groupName,
                               distributionType = distributionType.i,
                               verbose = verbose)
           }, simplify = FALSE) %>%
        bind_rows()

    ## colnames(resN)


    ## resN %>% group_by(allocGroup) %>%
    ##     slice_sample(n = 1) %>% as.data.frame()

    resWL <- sapply(distributionType,
                    function(distributionType.i)
             {
                 sapply(variableType_mean,
                        function(variableType_mean.i)
                 {
                     grp_AoL_alloc_WL(alloc_st_catch = alloc_st_cdf,
                                      matched_data_catch = matched_data_cdf,
                                      distribution_data = distribution_data,
                                      ## distribution_data_N = resN, # decision not to use allocated distributions.
                                      groupName = groupName,
                                      distributionType = distributionType.i,
                                      variableType_mean = variableType_mean.i,
                                      weighting = weighting,
                                      verbose = verbose)
                 }, simplify = FALSE)
             }, simplify = FALSE) %>%
        do.call(what = c) %>%
        bind_rows()

    ## colnames(resWL)
    ## resWL %>% group_by(variableType, sampledOrEstimated) %>% slice_tail(n = 2) %>% as.data.frame()

    res_distribution <- bind_rows(resN, resWL)

    res_catch <- alloc_st_cdf %>%
        mutate(domainBiology =
                   coalesce(domainBiology,
                            paste("[alloc]", # Should it include originType?
                                  seasonValue,
                                  areaValue,
                                  fisheriesManagementUnit,
                                  fleetValue,
                                  catchCategory, sep = "_"))) %>%
        left_join(res_distribution %>%
                  dplyr::select(vesselFlagCountry,
                                year,
                                workingGroup,
                                stock,
                                speciesCode,
                                catchCategory, # important to avoid duplication.
                                domainBiology,
                                allocGroup) %>%
                  dplyr::distinct(),
                  by = c("vesselFlagCountry", "year", "workingGroup", "stock",
                         "speciesCode", "catchCategory", # important to avoid duplication.
                         "domainBiology"))

    return(list(distribution = res_distribution,
                catch = res_catch))
}



## [pkg] Help page written here: upstream documents none of the allocation functions.
#' Allocate length or age distributions to the catch
#'
#' @description
#' Gives every stratum of the catch a length (or age) distribution. Where the
#' catch of a stratum has a sampled distribution it is kept; where it has none,
#' one is estimated from the sampled distributions of a group of matching strata
#' and marked as estimated. The result holds, for each stratum, the numbers and
#' the mean weight at every length or age class - what [catchAtAoLAggregation()]
#' and [meanWoLatAoLAggregation()] add up into catch-at-length / catch-at-age
#' tables, and what [plot_NaLoA()] and [plot_MWaLoA()] draw.
#'
#' @details
#' ## Groups
#'
#' The allocation is done group by group. A group is a pair of conditions, one
#' from `condition_alloc_st_list` and one from `condition_matched_data_list`:
#' the first selects the catch rows that are allocated together, the second the
#' catch rows whose sampled distributions are pooled to estimate the
#' distribution of those that have none. The two lists have the same length and
#' the same names - a group's name, `GA1`, `GA2`, ... when the lists have none -
#' and each condition is written as in `dplyr::filter()`: a quosure
#' (`rlang::quo(metier4 == "TBB")`), a text that is turned into one, or a
#' logical vector with a value for every row of `catch_data`.
#'
#' The allocation strata must not overlap - a catch row in two of them is an
#' error, which [check_group_conditions()] reports before anything is
#' allocated. The matched data may overlap, and usually do: a group can draw
#' on a wider pool than its own strata. Every group is allocated within one
#' year and one stock.
#'
#' ## How a distribution is estimated
#'
#' The numbers at length (or age) of a stratum without a sampled distribution
#' are the pooled sampled numbers of the matched data, scaled by the weight of
#' the stratum's catch over the weight of the catch that was sampled. Its mean
#' weight at length (or age) is the mean of the sampled mean weights, weighted
#' by the numbers at that class (`weighting = "NumberAtAoL"`) or by the catch
#' (`"CATON"`). A stratum that has a sampled distribution keeps it as it is.
#'
#' Catch and distributions are linked on `vesselFlagCountry`, `year`,
#' `workingGroup`, `stock`, `speciesCode`, `catchCategory` and
#' `domainBiology`. Units are harmonised first - catch weight to kg and
#' thousands, distribution values to g and numbers (N) - so what comes back is
#' in those.
#'
#' ## What comes back
#'
#' A list of two tables:
#'
#' * `distribution`: every distribution row - the sampled ones as they came, and
#'   the estimated ones - with `sampledOrEstimated` (`"sampled"` or
#'   `"estimated"`) and `allocGroup`, the group they were allocated in.
#'   Numbers are `variableType == "Number"` rows, mean weights
#'   `"WeightLive"` rows.
#' * `catch`: the allocated catch rows, with `allocGroup`. A stratum without a
#'   `domainBiology` is given one (`"[alloc]_<season>_<area>_<unit>_<fleet>_<category>"`),
#'   and its estimated distributions carry it, so the two tables join on
#'   `domainBiology` - which is why anything that reads an allocation takes both
#'   tables from this result, not the catch that went in.
#'
#' With `assembled_output = TRUE` the rows that were not allocated (another
#' origin or variable type, or in no group) are added, with `allocGroup` `NA`.
#'
#' @param catch_data The catch, a CEF long-format table, with the stratum
#'   columns the conditions refer to (see [derive_fields()]).
#' @param distribution_data The CEF distributions table: `distributionType`,
#'   `distributionClass`, `variableType` (`"Number"` or `"WeightLive"`),
#'   `value`, `domainBiology` and the keys above.
#' @param condition_alloc_st_list The conditions that select the strata that are
#'   allocated together, one per group - see *Groups*.
#' @param condition_matched_data_list The conditions that select the matched
#'   data of each group; same length and names as `condition_alloc_st_list`.
#'   Defaults to the allocation strata themselves.
#' @param distributionType `"LengthTotal"`, `"Age"` or both (the default):
#'   which distributions are allocated. These are CEF's own values.
#' @param originType_catch `"WGEstimate"` (default) or `"Official"`: the origin
#'   of the catch rows that are allocated. One at a time.
#' @param variableType_catch `"WeightLive"` (default) or `"Number"`: the
#'   variable of the catch rows that are allocated. One at a time.
#' @param variableType_mean The variable whose mean at length or age is
#'   allocated; only `"WeightLive"`.
#' @param weighting How the mean weights at length or age are weighted:
#'   `"NumberAtAoL"` (default), by the numbers at that class, or `"CATON"`, by
#'   the catch.
#' @param verbose Report what is done, group by group.
#' @param assembled_output `TRUE` (default): add the rows that were not
#'   allocated to the result; `FALSE`: only the allocated rows.
#' @param append Whether to append to the log file (`FALSE`: overwrite it).
#' @param ... Passed on to [check_group_conditions()], e.g. `logFile`, where the
#'   check of the conditions is written.
#' @return A list with `distribution` and `catch`, see *What comes back*.
#' @seealso [catchAtAoLAggregation()] and [meanWoLatAoLAggregation()] to add the
#'   allocation up; [check_group_conditions()] to check the groups;
#'   [catchRaising()] for the discards.
#' @author Yves Reecht
#' @md
#' @examples
#' \dontrun{
#' # one group: every stratum, matched against every stratum
#' alloc <- catchAllocationAoL(catch, distributions,
#'                             condition_alloc_st_list = list(ALL = rlang::quo(TRUE)),
#'                             distributionType = "LengthTotal",
#'                             logFile = tempfile(fileext = ".txt"))
#' table(alloc$distribution$sampledOrEstimated)
#'
#' # two groups: beam trawls are filled from beam trawls, the rest from every stratum
#' alloc <- catchAllocationAoL(catch, distributions,
#'   condition_alloc_st_list     = list(TBB  = rlang::quo(metier4 == "TBB"),
#'                                      REST = rlang::quo(metier4 != "TBB")),
#'   condition_matched_data_list = list(TBB  = rlang::quo(metier4 == "TBB"),
#'                                      REST = rlang::quo(TRUE)),
#'   distributionType = "LengthTotal", logFile = tempfile(fileext = ".txt"))
#' }
#' @export
catchAllocationAoL <- function(catch_data,
                               distribution_data,
                               condition_alloc_st_list,
                               condition_matched_data_list = condition_alloc_st_list,
                               distributionType = c("Age", "LengthTotal"), ## [pkg] CEF value
                               originType_catch = c("WGEstimate", "Official"),
                               variableType_catch = c("WeightLive", "Number"), # fraction to apply it to.
                               variableType_mean = c("WeightLive"), # For the averaged
                                        # quantity. Only mean weight for now; complete for mean
                                        # length when defined.
                               weighting = c("NumberAtAoL", "CATON"), # weighting for mean weights at age|length.
                               verbose = TRUE,
                               assembled_output = TRUE,
                               append = FALSE,
                               ...)
{
    ## Purpose: Catch allocations (numbers and mean weights) at age or length
    ##   * Tests for validity of conditions.
    ##   * Loops over condition pairs (raised stratum, matched data) and
    ##     apply allocations.
    ##   * Formats data ().
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 24 Oct 2024, 13:55
    ## [pkg] library(rlang) -> Imports: rlang
    ## [pkg] library(dplyr) -> Imports: dplyr

    ## Only for one source and variable type at once:
    originType_catch <- match.arg(originType_catch,
                                  c("WGEstimate", "Official"),
                                  several.ok = FALSE) # Or should it allows to raise together WG and
                                        # Official weights or numbers? (not relevant to mix weights
                                        # and numbers for catch-number-at-age or length.)

    variableType_catch <- match.arg(variableType_catch,
                                    c("WeightLive", "Number"),
                                    several.ok = FALSE)

    ## browser()

    ## ##################################################
    ## Check consistency:

    ## between condition list length:
    stopifnot(length(condition_alloc_st_list) == length(condition_matched_data_list))

    ##
    condition_alloc_st_list <-
        check_group_conditions(catch_data = catch_data,
                               condition_list =  condition_alloc_st_list,
                               conditionType = "strata",
                               domain = "domainBiology",
                               originType = originType_catch,
                               variableType = variableType_catch,
                               append = append,
                               ...)

    condition_matched_data_list <-
        check_group_conditions(catch_data = catch_data,
                               condition_list = condition_matched_data_list,
                               conditionType = "matched_data",
                               domain = "domainBiology",
                               originType = originType_catch,
                               variableType = variableType_catch,
                               append = TRUE,
                               ...)

    ## Fill in group names as needed:
    if (is.null(names(condition_alloc_st_list)))
    {
        names(condition_alloc_st_list) <-
            paste0("GA", seq_len(length.out = length(condition_alloc_st_list)))
    }

    if (is.null(names(condition_matched_data_list)))
    {
        names(condition_matched_data_list) <-
            names(condition_alloc_st_list)
    }

    
    ## ##################################################
    ## Harmonization of units:
    catch_data <- catch_data %>%
        convert_field(valueField = "total",
                      to = c("kg", "NE3"))
    
    distribution_data <- distribution_data %>%
        convert_field(valueField = "value",
                      to = c("g", "N"))

    ## ##################################################
    ## Allocations:

    ## Loops through conditions
    res <- mapply(grp_AoL_alloc_condition,
                  condition_alloc_st = condition_alloc_st_list,
                  condition_matched_data = condition_matched_data_list,
                  groupName = names(condition_alloc_st_list),
                  MoreArgs = list(catch_data = catch_data,
                                  distribution_data = distribution_data,
                                  distributionType = distributionType,
                                  originType = originType_catch,
                                  variableType = variableType_catch,
                                  variableType_mean = variableType_mean,
                                  weighting = weighting,
                                  verbose = verbose),
                  SIMPLIFY = FALSE)

    ## names(res)

    res_distribution <- sapply(res,
                               function(x){return(x$distribution)},
                               simplify = FALSE) %>%
        bind_rows()

    res_catch <- sapply(res,
                        function(x){return(x$catch)},
                        simplify = FALSE) %>%
        bind_rows()

    ## res_distribution %>% group_by(variableType, allocGroup) %>% slice_sample(n = 1) %>% as.data.frame()
    ## res_catch %>% group_by(originType, variableType, allocGroup) %>% slice_sample(n = 1) %>% as.data.frame()

    if (isTRUE(assembled_output))
    {
        ## identifications of variable combinations actually allocated:
        varCombAlloc <- res_catch %>%
            select(vesselFlagCountry, year, workingGroup,
                   stock, speciesCode, catchCategory,
                   domainBiology, originType,
                   variableType.c = variableType) %>%
            right_join(res_distribution,
                       by = join_by(vesselFlagCountry, year,
                                    workingGroup, stock, speciesCode,
                                    catchCategory, domainBiology)) %>%
            dplyr::select(c("originType", "variableType.c", "variableType", "distributionType")) %>%
            distinct()

        ## Non allocated distributions based on variable:
        distribution_non_alloc <-
            catch_data %>%
            select(vesselFlagCountry, year, workingGroup,
                   stock, speciesCode, catchCategory,
                   domainBiology, originType,
                   variableType.c = variableType) %>%
            right_join(distribution_data,
                       by = join_by(vesselFlagCountry, year,
                                    workingGroup, stock, speciesCode,
                                    catchCategory, domainBiology)) %>%
            anti_join(varCombAlloc,
                      by = join_by(originType, variableType.c, distributionType, variableType)) %>%
            select(-c(variableType.c, originType))

        ## Catch data not selected (not matched to any group or other variableType):
        catch_data_non_used <-  catch_data %>%
            filter(! apply(sapply(condition_alloc_st_list, I),
                           1, any) |
                   ! (variableType %in% {{variableType_catch}} &
                      originType %in% {{originType_catch}}))

        ## Non allocated distributions based on non selected matching catch_data:
        distribution_non_alloc2 <-
            catch_data_non_used %>%
            select(vesselFlagCountry, year, workingGroup,
                   stock, speciesCode, catchCategory,
                   domainBiology, originType,
                   variableType.c = variableType) %>%
            right_join(distribution_data,
                       by = join_by(vesselFlagCountry, year,
                                    workingGroup, stock, speciesCode,
                                    catchCategory, domainBiology)) %>%
            semi_join(varCombAlloc,
                      by = join_by(originType, variableType.c, distributionType, variableType)) %>%
            select(-c(variableType.c, originType))

        ## Assembling results and leftover data:
        res_distribution <- res_distribution %>%
            bind_rows(distribution_non_alloc,
                      distribution_non_alloc2)

        res_catch <- res_catch %>%
            bind_rows(catch_data_non_used)

        ## res_catch %>% group_by(originType, variableType, allocGroup) %>% slice_sample(n = 1) %>% as.data.frame()
    }

    return(list(distribution = res_distribution,
                catch = res_catch))

}

## ###########################################################################
## Reporting functions:

## [pkg] Help page written here: upstream documents none of the allocation functions.
#' Catch at length or age: the numbers of an allocation, added up
#'
#' @description
#' Adds the numbers at length (or age) of an allocation - or of any CEF
#' distributions linked to a catch - up into a table with one column per length
#' or age class, for each combination of the variables in `grouping`: the catch
#' at length, or at age.
#'
#' @details
#' The distributions are linked to the catch rows on `vesselFlagCountry`,
#' `year`, `workingGroup`, `stock`, `speciesCode`, `catchCategory` and
#' `domainBiology`, and only the `"Number"` distributions of `distributionType`
#' are used. A distribution is counted once for every catch row it is linked
#' to. Take both tables from one [catchAllocationAoL()] result, whose domains
#' belong to one catch row each, and pass one `originType_catch` and one
#' `variableType_catch`: with the defaults, which take both, a stratum that has
#' rows for both is counted twice.
#'
#' ## Classes
#'
#' `minAoL` drops the classes below it. With `maxAoL` set and `plusGroup`
#' `TRUE`, every class from `maxAoL` up is pooled into one plus group, whose
#' column is labelled `"<maxAoL>+"`; with `plusGroup` `FALSE` the classes above
#' `maxAoL` are left out and `maxAoL` is a class like the others. With `maxAoL`
#' `NA` the highest class found closes the table, without a plus.
#'
#' @param distribution_data The distributions: a CEF distributions table, or the
#'   `distribution` table of a [catchAllocationAoL()] result.
#' @param catch_data The catch they belong to: a CEF catch table, or the `catch`
#'   table of the same allocation.
#' @param grouping The variables to add up by: columns of the catch rows
#'   (`catchCategory`, `seasonValue`, `metier4`, ...) or of the distributions
#'   (`attributeType`, `attributeValue`).
#' @param originType_catch `"WGEstimate"` or `"Official"`: the origin of the
#'   catch rows to link. Pass one.
#' @param variableType_catch `"WeightLive"` or `"Number"`: the variable of the
#'   catch rows to link. Pass one.
#' @param distributionType `"Age"` (default) or `"LengthTotal"`, CEF's own
#'   values. One at a time.
#' @param unit The unit to convert the numbers to before adding them up: `"N"`
#'   (fish) or `"NE3"` (thousands of fish). `NULL` (default) leaves them as
#'   they are.
#' @param round Digits to round the numbers to; `NULL` (default) does not
#'   round.
#' @param minAoL The lowest class kept.
#' @param maxAoL The class from which the plus group starts, or the highest
#'   class kept without one; `NA` (default) for the highest class found.
#' @param plusGroup Pool the classes from `maxAoL` up into a plus group
#'   (`TRUE`, default) or leave them out (`FALSE`).
#' @return A tibble with the `grouping` columns, `unit` (the unit of the numbers)
#'   and one column per class, named `N_<distributionType>_<class>`:
#'   `N_LengthTotal_300`, `N_Age_3`, `N_Age_7+`.
#' @seealso [meanWoLatAoLAggregation()] for the mean weights;
#'   [catchAllocationAoL()] for the allocation; [plot_NaLoA()] to look
#'   at it.
#' @author Yves Reecht
#' @md
#' @examples
#' \dontrun{
#' alloc <- catchAllocationAoL(catch, distributions,
#'                             condition_alloc_st_list = list(ALL = rlang::quo(TRUE)),
#'                             distributionType = "LengthTotal",
#'                             logFile = tempfile(fileext = ".txt"))
#'
#' # catch at length, in thousands of fish, by catch category and season,
#' # with every class from 400 up as one plus group
#' catchAtAoLAggregation(alloc$distribution, alloc$catch,
#'                       grouping = c("catchCategory", "seasonValue"),
#'                       originType_catch = "WGEstimate",
#'                       variableType_catch = "WeightLive",
#'                       distributionType = "LengthTotal",
#'                       unit = "NE3", round = 0, maxAoL = 400)
#' }
#' @export
catchAtAoLAggregation <- function(distribution_data,
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
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 27 Oct 2025, 13:36
    ## browser()

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
               distributionType %in% {{distributionType}})

    if (is.na(maxAoL))
    {
        maxAoL2 <- max(tmp$distributionClass, na.rm = TRUE)
    }else{
        maxAoL2 <- maxAoL
    }

    maxAoLdata <- ifelse(plusGroup || is.na(maxAoL),
                         max(tmp$distributionClass, na.rm = TRUE),
                         maxAoL)

    res <- tmp %>%
        select(all_of(c(grouping, "distributionClass", "variableUnit", "value"))) %>%
        filter(between(distributionClass, minAoL, maxAoLdata)) %>% ## pull(distributionClass) %>% unique() %>% sort()
        mutate(grp = if_else(distributionClass < maxAoL2,
                             as.character(distributionClass),
                             paste0(maxAoL2,
                                    ifelse(plusGroup & ! is.na(maxAoL),
                                           "+", ""))),
               grp = factor(grp,
                            levels = unique(grp)[order(as.numeric(sub("+",
                                                                      "",
                                                                      unique(grp),
                                                                      fixed = TRUE)))])) %>%
        dplyr::group_by(across(all_of(c(grouping, unit = "variableUnit", "grp")))) %>%
        summarize(N = sum(value, na.rm = TRUE), .groups = "drop") %>%
        arrange(grp) %>%
        pivot_wider(names_from = "grp", names_prefix = paste0("N_", distributionType, "_"),
                    values_from = "N")  %>%
        mutate(across(starts_with(paste0("N_", distributionType, "_")),
                          ~replace_na(.x , 0)))

    if (!is.null(round) && is.numeric(round))
    {
        res <- res %>%
            mutate(across(starts_with(paste0("N_", distributionType, "_")),
                          ~round(.x , digits = round)))
    }

    return(res)
}


## [pkg] Help page written here: upstream documents none of the allocation functions.
#' Mean weight at length or age: the mean weights of an allocation, averaged
#'
#' @description
#' Averages the mean weights at length (or age) of an allocation - or of any
#' CEF distributions linked to a catch - into a table with one column per
#' length or age class, for each combination of the variables in `grouping`: the
#' mean weight at length, or at age.
#'
#' @details
#' The distributions are linked to the catch rows as in
#' [catchAtAoLAggregation()], which has the same warning about taking both
#' tables from one allocation and passing a single `originType_catch` and
#' `variableType_catch`. The mean weights of the `"WeightLive"` distributions
#' are averaged within each group and class, weighted by the numbers at that
#' class (`weighting = "NumberAtAoL"`). The numbers come from the `"Number"`
#' distributions of the same domain, so a mean weight without numbers is left
#' out of the average.
#'
#' `minAoL`, `maxAoL` and `plusGroup` cut and pool the classes exactly as they
#' do in [catchAtAoLAggregation()].
#'
#' @inheritParams catchAtAoLAggregation
#' @param weighting How the mean weights are weighted: `"NumberAtAoL"` (default),
#'   by the numbers at length or age of each distribution, or `"CATON"`, by the
#'   total weight of the catch row each distribution belongs to.
#' @param variableType_dist The variable of the distributions that is averaged;
#'   only `"WeightLive"`.
#' @param unit The unit to convert the weights to before averaging: `"g"`,
#'   `"kg"` or `"t"`. `NULL` (default) leaves them as they are.
#' @param round Digits to round the weights to; `NULL` (default) does not round.
#' @param samplesOnly Not used.
#' @return A tibble with the `grouping` columns, `unit` (the unit of the
#'   weights) and one column per class, named `W_<distributionType>_<class>`:
#'   `W_LengthTotal_300`, `W_Age_3`, `W_Age_7+`.
#' @seealso [catchAtAoLAggregation()] for the numbers;
#'   [catchAllocationAoL()] for the allocation; [plot_MWaLoA()] to look
#'   at it.
#' @author Yves Reecht
#' @md
#' @examples
#' \dontrun{
#' alloc <- catchAllocationAoL(catch, distributions,
#'                             condition_alloc_st_list = list(ALL = rlang::quo(TRUE)),
#'                             distributionType = "LengthTotal",
#'                             logFile = tempfile(fileext = ".txt"))
#'
#' # mean weight at length in kg, by catch category
#' meanWoLatAoLAggregation(alloc$distribution, alloc$catch,
#'                         grouping = "catchCategory",
#'                         originType_catch = "WGEstimate",
#'                         variableType_catch = "WeightLive",
#'                         distributionType = "LengthTotal",
#'                         unit = "kg", round = 3)
#' }
#' @export
meanWoLatAoLAggregation <- function(distribution_data,
                                    catch_data,
                                    grouping = c("catchCategory", "attributeType",
                                                 "attributeValue"),
                                    weighting = c("NumberAtAoL", "CATON"), # weighting for mean
                                        # weights|length at age|length class. 
                                    originType_catch = c("WGEstimate", "Official"),
                                    variableType_catch = c("WeightLive", "Number"),
                                    variableType_dist = "WeightLive",
                                    distributionType = "Age",
                                    unit = NULL,
                                    round = NULL,
                                    minAoL = 0, maxAoL = NA,
                                    plusGroup = TRUE,
                                    samplesOnly = TRUE)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 27 Oct 2025, 14:57
    ## browser()

    weighting <- match.arg(weighting, c("NumberAtAoL", "CATON"), several.ok = FALSE)

    variableType_dist <- match.arg(variableType_dist,
                                   c("WeightLive"), several.ok = FALSE)

    varPfx <- sub("^(.).*$", "\\1", variableType_dist)

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
               distributionType %in% {{distributionType}}) %>%
        ## Add sampled and eatimated catch numbers:
        left_join(distribution_data %>%
                  filter(variableType %in% "Number",
                         distributionType %in% {{distributionType}}) %>%
                  select(vesselFlagCountry, year, workingGroup,
                         stock, speciesCode, catchCategory, domainBiology,
                         attributeType, attributeValue, distributionType, distributionUnit, distributionClass,
                         NaAoL = value),
                  by = join_by(vesselFlagCountry, year, workingGroup,
                               stock, speciesCode, catchCategory, domainBiology,
                               attributeType, attributeValue, distributionType, distributionUnit, distributionClass),
                  suffix = c(".c", ""))

    distribution_data %>% group_by(variableType) %>% slice_head(n = 1) %>% as.data.frame()
    tmp %>% group_by(variableType) %>% slice_head(n = 1) %>% as.data.frame()

    if (is.na(maxAoL))
    {
        maxAoL2 <- max(tmp$distributionClass, na.rm = TRUE)
    }else{
        maxAoL2 <- maxAoL
    }

    maxAoLdata <- ifelse(plusGroup || is.na(maxAoL),
                         max(tmp$distributionClass, na.rm = TRUE),
                         maxAoL)
    
    ## Field used for weighting, based on user choice:
    ## [pkg] "CATON" weights by the total weight of the stratum's catch, which is held in the
    ##   CATON column here: `total` is not among the columns selected below, so looking for it
    ##   stopped with "object 'total' not found".
    wgField <- case_when(weighting == "NumberAtAoL" ~ "NaAoL",
                         weighting == "CATON" ~ "CATON",
                         TRUE ~ NA)
    if (is.na(wgField))
        stop("mean weighting \"", weighting,
             "\" not implemented for weights-at-age|length")
    
    res <- tmp %>%
        select(all_of(c(grouping, "distributionClass", "variableUnit", "value", "CATON", "NaAoL"))) %>%
        filter(between(distributionClass, minAoL, maxAoLdata)) %>% ## pull(distributionClass) %>% unique() %>% sort()
        mutate(grp = if_else(distributionClass < maxAoL2,
                             as.character(distributionClass),
                             paste0(maxAoL2,
                                    ifelse(plusGroup & ! is.na(maxAoL),
                                           "+", ""))),
               grp = factor(grp,
                            levels = unique(grp)[order(as.numeric(sub("+",
                                                                      "",
                                                                      unique(grp),
                                                                      fixed = TRUE)))])) %>%
        ungroup() %>%
        dplyr::group_by(across(all_of(c(grouping, unit = "variableUnit", "grp")))) %>%
        filter(! is.na(!!sym(wgField))) %>% # Does not handle NAs in weights.
        summarize(res = weighted.mean(x = value,
                                      w = !!sym(wgField),
                                      na.rm = TRUE),
                  .groups = "drop") %>%
        arrange(grp) %>%
        pivot_wider(names_from = "grp", names_prefix = paste0(varPfx, "_", distributionType, "_"),
                    values_from = "res")

    if (!is.null(round) && is.numeric(round))
    {
        res <- res %>%
            mutate(across(starts_with(paste0(varPfx, "_", distributionType, "_")),
                          ~round(.x , digits = round)))
    }
    
    return(res)
}




### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 100
### End:
