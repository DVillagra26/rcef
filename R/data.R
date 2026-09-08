# =====================================================================
#  data.R   Documentation for the bundled example data sets.
#
#  The .rda files live in data/. They are written by a throwaway
#  generator kept out of the tarball. Every value in them is invented -
#  nothing is copied or relabelled from the bll / pok CEF files, so the
#  coverage pattern is the anchoveta fishery's own, not a real stock's.
# =====================================================================

#' Synthetic Peruvian-anchoveta CEF catch table
#'
#' A fully made-up CEF (RDBES long-format) catch table for a fictional
#' Southeast-Pacific anchoveta (\emph{Engraulis ringens}) fishery, for
#' trying the plotting functions and for timing the base-R vs ggplot2
#' engines on a heavier table without needing real data. About 3,500
#' rows - several times the turbot walkthrough table - over four years
#' (2020--2023) and four quarters.
#'
#' Already through [derive_fields()], so it drops straight into any plot:
#' the derived columns (`Country`, `Season`, `metier4`, the parsed area
#' parts, ...) are present and consistent. Pair it with [anchoveta_dist]
#' for the `what = "length"` / `what = "age"` coverage views and for
#' [plot_numbers_at_length_age()] / [plot_meanweight_at_length_age()].
#'
#' Nothing here comes from another data set: the fleets, areas, metiers,
#' weights and sampling coverage are all drawn from invented
#' distributions. The three kinds of coverage are driven by SEPARATE
#' per-country / per-metier / per-quarter patterns, so the discard,
#' length and age coverage plots look genuinely different: Peru has the
#' best discard programme, Chile the best length sampling, Russia the
#' best age reading, and Russia / Vanuatu run almost no discard observers
#' (their cells read "no discard sampled").
#'
#' @format A data frame of catch records, one row per
#'   stock / catch category / year / quarter / area / fleet cell. Key
#'   columns:
#' \describe{
#'   \item{catchCategory}{`"Lan"`, `"Dis"` or `"BMS"`.}
#'   \item{vesselFlagCountry, Country}{Flag state: `"Peru"`, `"Chile"`,
#'     `"Ecuador"`, `"China"`, `"South Korea"`, `"Russia"`, `"Vanuatu"`.}
#'   \item{seasonValue, Season}{Quarter, `"1"`--`"4"`.}
#'   \item{areaValue, ICESDivision, ...}{FAO Area 87 (Pacific, Southeast)
#'     division code and its parsed parts.}
#'   \item{metier6, metier4, ...}{Gear / metier (`"PS"`, `"MPT"`,
#'     `"LHP"`) and its parsed parts.}
#'   \item{fleetValue}{Fleet segment.}
#'   \item{total}{Weight (kg) for the row's `variableType`.}
#'   \item{numTrips, domainBiology}{Sampling-trip count and the key that
#'     links a catch stratum to its length / age distribution.}
#' }
#' @seealso [anchoveta_dist], [coverage-plots], [plot_discard_rate()]
#' @family example data
#' @keywords datasets
#' @name anchoveta_catch
#' @docType data
"anchoveta_catch"

#' Synthetic Peruvian-anchoveta CEF distributions table
#'
#' The length- and age-distribution companion to [anchoveta_catch]: a
#' made-up CEF distributions table carrying, for both `LengthTotal` (mm,
#' classes 40--200) and `Age` (years, 0--4), a `Number` row (numbers at
#' class, `valueType = "Total"`) AND a `WeightLive` row (mean weight at
#' class in grams, `valueType = "Mean"`) - so
#' [plot_numbers_at_length_age()] and [plot_meanweight_at_length_age()]
#' both have real data. Every row links back to a catch stratum through
#' `domainBiology` + `catchCategory`. The distribution shape varies:
#' the modal size and age shift by country and quarter, spreads and skew
#' are drawn per stratum, and one quarter carries a recruitment pulse
#' (a second mode of small, young fish).
#'
#' Pass it as the `distributions` argument:
#' \preformatted{
#' plot_coverage_heat(anchoveta_catch, y = "metier4", x = "Country",
#'                    what = "length", distributions = anchoveta_dist,
#'                    min_fish = 50)
#' plot_numbers_at_length_age(anchoveta_catch, anchoveta_dist,
#'                            distributionType = "Age", fill = "Country")
#' }
#'
#' @format A data frame of distribution records, two rows per
#'   class / stratum (one `Number`, one `WeightLive`). Key columns:
#' \describe{
#'   \item{distributionType}{`"LengthTotal"` or `"Age"`.}
#'   \item{distributionClass}{Length in mm, or age in whole years.}
#'   \item{distributionUnit}{`"mm"` or `"y"`.}
#'   \item{catchCategory}{`"Lan"` or `"Dis"` - which catch part the
#'     distribution describes.}
#'   \item{variableType, valueType, value}{`"Number"` / `"Total"` gives
#'     numbers at the class; `"WeightLive"` / `"Mean"` gives mean weight
#'     at the class in grams.}
#'   \item{numMeasurements, numTrips}{Sampling effort behind the row.}
#'   \item{domainBiology}{Join key back to [anchoveta_catch].}
#' }
#' @seealso [anchoveta_catch], [coverage-plots],
#'   [plot_numbers_at_length_age()], [plot_meanweight_at_length_age()]
#' @family example data
#' @keywords datasets
#' @name anchoveta_dist
#' @docType data
"anchoveta_dist"
