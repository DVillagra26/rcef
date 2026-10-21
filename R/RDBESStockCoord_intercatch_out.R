# =====================================================================
#  RDBESStockCoord_intercatch_out.R
#
#  ORIGIN: RDBESStockCoord (ICES working group code).
#  Upstream file: R/funICoutCEF.R
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
#    - the length classes of the StockOverview / Numbers files are in mm (class 130 is
#      a fish of 13 cm), and are now labelled "mm": upstream labels them "cm", and
#      funIntercatchCEF() then multiplies by 10, so the CEF had the classes ten times
#      too large (1300 to 7000 for a brill of 13 to 70 cm). Nothing is multiplied now;
#      the unit of the mean lengths of the age distributions (cm) is left as it was.
#      Belongs upstream.
#
#  Exported: funICoutCEF()
#
#  InterCatch OUTPUT (StockOverview / NumbersAtAgeLength ...) -> CEF.
#  Calls funMakeRelation() from RDBESStockCoord_relation.R.
# =====================================================================

#' InterCatch output to CEF format
#'
#' @param dat_path Path to the folder in which IC data is stored by year
#' @param years year for whih IC data will be loaded
#' @param stock_relation The stock / area / species lookup that gives every record its stock code and
#'   working group, as made by funMakeRelation(). NULL (default): made here, by funMakeRelation(max(years)).
#' @param output_format What to do with the result, any of "to_environment" (the tables catches,
#'   distributions and effort are assigned in the global environment), "to_file" (they are written as csv
#'   files in out_path), "to_upload" (they are written as the upload files, without a header) and "to_list"
#'   (they are returned, invisibly). The default is the whole vector, which does the first two; it lists
#'   "to_upload_file", which the code does not act on.
#' @param out_path Folder the files are written to (default: the working directory).
#' @param keep_temp_file Keep the folder tmp that is made inside dat_path, which holds the IC data as the
#'   exchange format files (hi.csv, si.csv and sd.csv) that funIntercatchCEF() reads? Default FALSE: it is removed.
#' @param file_prefix Prefix of the names of the files written (default: none), as in
#'   pok_2022_catches_CEF_v17.1.csv.
#' @param include_BMS_Caton Default TRUE. FALSE leaves out the BMS rows of the working group's estimate (Caton);
#'   the official BMS landings stay.
#' @param patch_missing_DIS_PSU Default FALSE. TRUE gives a discard domain that has no sampling unit the PSU
#'   "FishingTrip" and 0 sampled trips (with a warning).
#' @param metier6 NULL (default) writes the metier6 as MIS_MIS_0_0_0. "Fleet" copies the fleet into it and
#'   writes the unit k of the numbers as NE3.
#'
#' @export
#'
funICoutCEF <- function(dat_path,
                       years,
                       stock_relation = NULL,
                       output_format = c("to_environment", "to_file", "to_upload_file"),
                       out_path = getwd(),
                       keep_temp_file = FALSE,
                       file_prefix = "",
                       include_BMS_Caton = TRUE,
                       patch_missing_DIS_PSU = FALSE,
                       metier6 = NULL)
{
    ## read in and adjust
    lst <- list.files(paste0(dat_path, "/", years),
                      pattern = ".txt", full.names = TRUE)

    stopifnot(any(grepl("StockOverview.txt", lst)))
    overview <- data.table::rbindlist(lapply(lst[lst %like% "StockOverview"],
                                             read.table,
                                             header = TRUE, sep = "\t"),
                                      fill = TRUE)


    overview$Species <- toupper(gsub("\\..*", "", overview$Stock))
    overview$Catch.Cat.[overview$Catch.Cat. == "Landings"] <- "Lan"
    overview$Catch.Cat.[overview$Catch.Cat. == "Discards"] <- "Dis"
    overview$Catch.Cat.[overview$Catch.Cat. == "BMS landing"] <- "BMS"

    ##
    stopifnot(any(grepl("Numbers", lst)))
    numbers <- rbindlist(lapply(lst[lst %like% "Numbers"],
                                read.table,
                                header = TRUE, sep = "\t", skip = 2),
                         fill = TRUE)

    numbers$Catch.Cat.[numbers$Catch.Cat. == "L"] <- "Lan"
    numbers$Catch.Cat.[numbers$Catch.Cat. == "D"] <- "Dis"
    numbers$Catch.Cat.[numbers$Catch.Cat. == "B"] <- "BMS"
    suppressWarnings(numbers$X <- NULL)

                                        #
    stopifnot(any(grepl("MeanWeigth", lst)))
    mw <- rbindlist(lapply(lst[lst %like% "MeanWeigth"], read.table,
                           header = TRUE, sep = "\t", skip = 1),
                    fill = TRUE)

    mw$Catch.Cat.[mw$Catch.Cat. == "Landings"] <- "Lan"
    mw$Catch.Cat.[mw$Catch.Cat. == "Discards"] <- "Dis"
    mw$Catch.Cat.[mw$Catch.Cat. == "BMS landing"] <- "BMS"
    suppressWarnings(mw$X <- NULL)

    ## wide to long tables of biological data
    num_l <- melt(numbers, id.vars = names(numbers)[1:15],
                  measure.vars = names(numbers)[16:ncol(numbers)],
                  variable.name = "CANUMtype", value.name = "NumberCaught")

    mw_l <- melt(mw, id.vars = names(mw)[1:15],
                 measure.vars = names(mw)[16:ncol(mw)],
                 variable.name = "CANUMtype", value.name = "MeanWeight")


    sd <- merge(num_l, mw_l)
    ## Need to add season type (or suppress Season.type altogether [YR]):
    sd <- merge(sd, unique(overview[ , c("Country", "Season", "Season.type",
                                         "Year", "Stock", "Area", "Fleets",
                                         "Effort", "Catch.Cat.", "Report.cat.")]),
                by = c("Country", "Season",
                       "Year", "Stock", "Area", "Fleets",
                       "Effort", "Catch.Cat.", "Report.cat."),
                all.x = TRUE, all.y = FALSE)
    sd$AgeLength <- as.numeric(str_extract(sd$CANUMtype, "[0-9]+"))
    sd$sex <- str_extract(sd$CANUMtype, "Undetermined|Male|Female")

    sd$sex[sd$sex == "Undetermined"] <- "N"
    sd$sex[sd$sex == "Male"] <- "M"
    sd$sex[sd$sex == "Female"] <- "F"

    sd$CANUMtype <- str_extract(sd$CANUMtype, "Age|Lngt")
    sd$UnitAgeOrLength <- ifelse(sd$CANUMtype == "Age", "year", "mm") # [pkg] the classes are in mm: see the header
    sd$UnitMeanLength <- ifelse(sd$CANUMtype == "Age", "cm", NA)

    ## only include reported records
    sd <- sd[sd$NumberCaught > 0, ]

############# create exchange format
    hi <- data.frame(RecordType = "HI",
                     Country = overview$Country,
                     Year = overview$Year,
                     SeasonType = overview$Season.type,
                     Season = overview$Season,
                     Fleet = overview$Fleets,
                     AreaType = NA,
                     FishingArea = overview$Area,
                     DepthRange = NA,
                     UnitEffort = overview$UnitEffort,
                     Effort = overview$Effort,
                     AreaQualifier = NA)

    si <- data.frame(RecordType = "SI",
                     Country = overview$Country,
                     Year = overview$Year,
                     SeasonType = overview$Season.type,
                     Season = overview$Season,
                     Fleet = overview$Fleets,
                     AreaType = NA,
                     FishingArea = overview$Area,
                     DepthRange = NA,
                     Species = overview$Species,
                     Stock_orig = overview$Stock,
                     CatchCategory = overview$Catch.Cat.,
                     ReportingCategory = overview$Report.cat.,
                     DataToForm = NA,
                     Usage = "H",
                     SamplesOri00gin = NA,
                     QualityFlag = NA,
                     UnitCaton = "kg",
                     Caton = ifelse(overview$Catch.Cat. %in% "Logbook Registered Discard",
                                    NA, overview$Catch..kg),
                     OffLandings = ifelse(overview$Catch.Cat. %in% "Logbook Registered Discard",
                                          overview$Catch..kg, NA),
                     VarCaton = -9,
                     InfoFleet = NA,
                     InfoStockCoordinator = NA,
                     InfoGeneral = NA)

    sd <- data.frame(RecordType = "SD",
                     Country = sd$Country,
                     Year = sd$Year,
                     SeasonType = sd$Season.type, # Several season type can coexist!
                     Season = sd$Season,
                     Fleet = sd$Fleet,
                     AreaType = NA,
                     FishingArea = sd$Area,
                     DepthRange = "",
                     Species = unique(si$Species),
                     Stock_orig = sd$Stock,
                     CatchCategory = sd$Catch.Cat.,
                     ReportingCategory = sd$Report.cat.,
                     Sex = sd$sex,
                     CANUMtype = sd$CANUMtype,
                     AgeLength = sd$AgeLength,
                     PlusGroup = -9,
                     SampledCatch = sd$SampledCatch,
                     NumSamplesLngt = sd$NumSamplesLength,
                     NumLngtMeas = sd$NumLengthMeasurements,
                     NumSamplesAge = sd$NumSamplesAge,
                     NumAgeMeas = sd$NumAgeMeasurement,
                     UnitMeanWeight = "g",
                     UnitCANUM = "k",
                     UnitAgeOrLength = sd$UnitAgeOrLength,
                     UnitMeanLength = sd$UnitMeanLength,
                     Maturity = "",
                     NumberCaught = sd$NumberCaught/1000,
                     MeanWeight = sd$MeanWeight,
                     MeanLength = NA,
                     varNumLanded = -9,
                     varWgtLande = -9,
                     varLgtLanded = -9)


    ## create temp files ti input into the other fuction
    dir.create(paste0(dat_path, "/tmp"), showWarnings = FALSE)
    write.csv(sd, paste0(dat_path, "/tmp/sd.csv"),
              row.names = FALSE, quote = FALSE)
    write.csv(si, paste0(dat_path, "/tmp/si.csv"),
              row.names = FALSE, quote = 13)
    write.csv(hi, paste0(dat_path, "/tmp/hi.csv"),
              row.names = FALSE, quote = FALSE)


    if(is.null(stock_relation))
    {
        stock_relation <- funMakeRelation(max(years))
    }

    res <- funIntercatchCEF(dat_path = paste0(dat_path, "/tmp"),
                        stock_relation = stock_relation,
                        metier6 = metier6,
                        output_format = output_format,
                        out_path = out_path,
                        file_prefix = file_prefix)

    if( ! keep_temp_file)
    {
        unlink(paste0(dat_path, "/tmp"), recursive = TRUE)
    }

    return(invisible(res))
}





