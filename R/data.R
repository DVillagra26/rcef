# =====================================================================
#  data.R   Documentation for the bundled example data sets.
#
#  The .rda files live in data/. They are written by a throwaway
#  generator kept out of the tarball. Every value in them is invented -
#  nothing is copied or relabelled from the bll / pok CEF files, so the
#  coverage pattern is the anchoveta fishery's own, not a real stock's.
#
#  One edit was made to anchoveta_catch after it was generated: originType
#  was relabelled from "Estimate" to "WGEstimate". The raising and
#  allocation engines, and the app's originType picker, select rows on
#  that column and accept only "WGEstimate" or "Official", so with
#  "Estimate" a raising run found no estimates and raised nothing without
#  saying so. dev/tests/test_anchoveta_example.R keeps it that way.
# =====================================================================

#' Synthetic Peruvian-anchoveta CEF catch table
#'
#' @description
#' A made-up CEF (RDBES long-format) catch table for a fictional
#' Southeast-Pacific anchoveta (*Engraulis ringens*) fishery: about 2,700
#' rows over four years (2020--2023) and four quarters, seven flag states
#' and three metiers. It runs the whole package without any real data:
#' the coverage tables and plots, discard raising, and length and age
#' allocation. The tests and the README examples use it.
#'
#' @details
#' Already through [derive_fields()], so it drops straight into any table or
#' plot: the derived columns (`Country`, `Season`, `metier4`, the parsed area
#' parts, ...) are present and consistent. Pair it with [anchoveta_dist] for
#' the `what = "length"` / `what = "age"` coverage views and for
#' [plot_numbers_at_length_age()] and [plot_meanweight_at_length_age()].
#'
#' ## Raising the discards
#'
#' The rows are shaped the way the raising engine reads them:
#'
#' * every row has `originType = "WGEstimate"`, the value the engines and the
#'   application select on (`catchRaising()` takes `"WGEstimate"` or
#'   `"Official"`);
#' * a `Dis` row shares its `domainCatchDis` with the `Lan` rows it was
#'   observed on, and a landing with no discard estimate has a blank domain.
#'   About a quarter of the landings rows - roughly half of the landed
#'   weight - carry an estimate; those are the donors, and the rest are what
#'   a raising fills in;
#' * it holds four years and the engine works on **one stock and year at a
#'   time**, so pick a year first: `subset(anchoveta_catch, year == 2023)`.
#'
#' ## What is invented
#'
#' Nothing here comes from another data set: the fleets, areas, metiers,
#' weights and sampling coverage are all drawn from invented distributions.
#' The three kinds of coverage are driven by SEPARATE per-country /
#' per-metier / per-quarter patterns, so the discard, length and age
#' coverage plots look genuinely different: Peru has the best discard
#' programme, Chile the best length sampling, Russia the best age reading,
#' and Russia and Vanuatu run almost no discard observers (their cells read
#' "no discard sampled").
#'
#' Not every fleet works every metier: several (country, metier)
#' combinations have no rows at all - so a coverage grid grouped by `metier4`
#' x `Country` has genuine holes - and a few strata carry a zero-weight `Lan`
#' row (the fleet was there but landed nothing), which classifies as "no
#' landings / no discards".
#'
#' @format A data frame of catch records, one row per stock / catch category
#'   / year / quarter / area / fleet cell. Key columns:
#' \describe{
#'   \item{catchCategory}{`"Lan"`, `"Dis"` or `"BMS"`.}
#'   \item{year}{2020--2023. The raising engine takes one at a time.}
#'   \item{vesselFlagCountry, Country}{Flag state: `"Peru"`, `"Chile"`,
#'     `"Ecuador"`, `"China"`, `"South Korea"`, `"Russia"`, `"Vanuatu"`.}
#'   \item{seasonValue, Season}{Quarter, `"1"`--`"4"`.}
#'   \item{areaValue, ICESDivision, ...}{FAO Area 87 (Pacific, Southeast)
#'     division code and its parsed parts.}
#'   \item{fleetValue, metier4, ...}{The metier6 string (`derive_fields()`
#'     parses `metier4` - `"PS"` / `"MPT"` / `"LHP"` - and the gear /
#'     target / mesh parts out of it).}
#'   \item{originType}{`"WGEstimate"` on every row.}
#'   \item{total}{Weight (kg) for the row's `variableType`; `0` for a
#'     reported-nothing stratum.}
#'   \item{domainCatchDis, domainCatchBMS}{The key that links a landing to
#'     its discard (or BMS) estimate; blank when there is none.}
#'   \item{numTrips, domainBiology}{Sampling-trip count and the key that
#'     links a catch stratum to its length / age distribution.}
#' }
#' @seealso [anchoveta_dist], [coverage-plots], [build_coverage_strata()],
#'   [plot_discard_rate()]
#' @family example data
#' @keywords datasets
#' @name anchoveta_catch
#' @docType data
#' @md
#' @examples
#' data(anchoveta_catch)
#'
#' # discard rate and sampling coverage per stratum ...
#' head(build_coverage_strata(anchoveta_catch, c("metier4", "Country")))
#'
#' # ... and as a figure
#' plot_coverage_heat(anchoveta_catch, y = "metier4", x = "Country")
#'
#' # raise the discards of one year: define the groups, then raise
#' cef    <- subset(anchoveta_catch, year == 2023)
#' dims   <- c("metier4", "ICESDivision", "Season")
#' groups <- define_raising_groups_auto(cef, hierarchy = dims,
#'                                      thresholds = c(50, 40, 30))
#' raised <- run_native_raising(cef, dims, groups, type = "discards",
#'                              log_file = tempfile(fileext = ".txt"))
#' table(raised$catchCategory, raised$importedOrRaised)
"anchoveta_catch"

#' Synthetic Peruvian-anchoveta CEF distributions table
#'
#' @description
#' The length- and age-distribution companion to [anchoveta_catch]: a made-up
#' CEF distributions table carrying, for both `LengthTotal` (mm, classes
#' 40--200) and `Age` (years, 0--7), a `Number` row (numbers at class,
#' `valueType = "Total"`) AND a `WeightLive` row (mean weight at class in
#' grams, `valueType = "Mean"`) - so [plot_numbers_at_length_age()] and
#' [plot_meanweight_at_length_age()] both have real data, and so does the
#' length and age allocation.
#'
#' @details
#' Every row links back to a catch stratum through `domainBiology` +
#' `catchCategory`. The distribution shape varies: the modal size and age
#' shift by country and quarter, spreads and skew are drawn per stratum, and
#' one quarter carries a recruitment pulse (a second mode of small, young
#' fish).
#'
#' The **discards run smaller, younger and thinner** than the landings, as
#' you would expect: within a stratum the discard length mode sits ~3 cm
#' below the landings mode, the age mode ~1 year younger, and the mean weight
#' at a given length or age class is ~0.75x the landings value (a lower
#' length-weight condition factor). Discard numbers are a small fraction of
#' the landings.
#'
#' Like the catch table it spans four years; subset both to the same year
#' (`year == 2023`) before allocating, since the engines work on one stock
#' and year at a time.
#'
#' @format A data frame of distribution records, two rows per class /
#'   stratum (one `Number`, one `WeightLive`). Key columns:
#' \describe{
#'   \item{distributionType}{`"LengthTotal"` or `"Age"`.}
#'   \item{distributionClass}{Length in mm (40--200), or age in whole years
#'     (0--7).}
#'   \item{distributionUnit}{`"mm"` or `"y"`.}
#'   \item{catchCategory}{`"Lan"` or `"Dis"` - which catch part the
#'     distribution describes.}
#'   \item{variableType, valueType, value}{`"Number"` / `"Total"` gives
#'     numbers at the class; `"WeightLive"` / `"Mean"` gives mean weight at
#'     the class in grams.}
#'   \item{numMeasurements, numTrips}{Sampling effort behind the row.}
#'   \item{domainBiology}{Join key back to [anchoveta_catch].}
#' }
#' @seealso [anchoveta_catch], [coverage-plots],
#'   [plot_numbers_at_length_age()], [plot_meanweight_at_length_age()]
#' @family example data
#' @keywords datasets
#' @name anchoveta_dist
#' @docType data
#' @md
#' @examples
#' data(anchoveta_catch)
#' data(anchoveta_dist)
#'
#' # how much of the catch has a length distribution linked
#' head(build_coverage_strata(anchoveta_catch, c("metier4", "Country"),
#'                            distributions = anchoveta_dist, what = "length"))
#'
#' plot_coverage_heat(anchoveta_catch, y = "metier4", x = "Country",
#'                    what = "length", distributions = anchoveta_dist,
#'                    min_fish = 50)
#' plot_numbers_at_length_age(anchoveta_catch, anchoveta_dist,
#'                            distributionType = "Age", fill = "Country")
"anchoveta_dist"
