# ============================================================
#  make_test_data_anchoveta.R
#  Synthetic test data for the RCEF Workbench: Peruvian anchoveta.
# ------------------------------------------------------------
#  WHAT THIS IS AND IS NOT
#  Entirely invented. No IMARPE landing, survey or sampling figure was
#  consulted and none should be read out of it. The magnitudes are plausible
#  for the fishery (a multi-million-tonne pelagic purse-seine stock split
#  into a North-Central and a South stock, two seasons a year) so that plots
#  look sensible and axis labels are not absurd, but every number is drawn
#  from a random generator. Do not cite it, do not use it to test anything
#  about the real stock, and do not leave it in a folder where somebody
#  might mistake it for data.
#
#  Anchoveta is not an ICES stock, so the ICES-shaped fields are borrowed
#  rather than real: areaValue carries Peruvian maritime zones, and the
#  metier strings follow the ICES gear_target_mesh convention because
#  derive_fields() splits on it. That is deliberate - the point is to
#  exercise the workflow, and the workflow parses ICES conventions.
#
#  WHY IT EXISTS
#  The three entry routes cannot be demonstrated without input for each, and
#  the raising steps cannot be demonstrated without coverage that VARIES:
#  a data set where everything is sampled raises nothing and shows nothing.
#  So the sampling here is deliberately uneven, in a patterned way:
#
#    * the industrial fleet is well sampled, the artisanal and wooden-boat
#      fleets poorly - the usual shape, where effort follows landings;
#    * one season is better covered than the other;
#    * a few strata have discards but no landings, and a few the reverse,
#      because both occur in real submissions and both have to be handled;
#    * length sampling is broader than age sampling, and age sampling is
#      thin enough in the small fleets to trip the measurement threshold
#      without being absent - the case that separates "no data" from "not
#      enough data", which is the distinction the two-axis biology
#      classification exists to draw.
#
#  WHAT IT WRITES  (into <out>/, default ./test_data_anchoveta)
#    cef/CEF_catches.csv           the CEF route: three tables
#    cef/CEF_distributions.csv
#    cef/CEF_effort.csv
#    ic_exchange/EXCHANGE_anchoveta.csv     HI / SI / SD records
#    ic_output/StockOverview.txt            the InterCatch output route
#    ic_output/NumbersAtAgeLength.txt
#    ic_output/MeanWeigthAtAgeLength.txt
#    README.txt
#
#  USAGE   Rscript make_test_data_anchoveta.R [out_dir]
# ============================================================

suppressMessages({library(dplyr); library(tidyr)})
set.seed(20260803)          # fixed, so two runs give the same demo

OUT <- {
  a <- commandArgs(trailingOnly = TRUE)
  if (length(a)) a[1] else "test_data_anchoveta"
}
YEAR  <- 2025
STOCK <- "ancho.27.nc"      # invented code, ICES-shaped so the app parses it
SPECIES <- 126426           # a plausible AphiaID slot; not verified

# ----------------------------------------------------------
#  The fishery, as invented
# ----------------------------------------------------------
# metier6 = gear_target_mesh, because derive_fields() splits on "_" and
# builds metier4/5/6 from the pieces.
# fleet     the metier6 string, gear_target_mesh
# share     fraction of the total landings
# cover     fraction of its strata that carry a discard estimate
# dr        its own discard rate
# dL        shift in mean length (mm) relative to the reference fleet
# selN      how sharply it selects: a small mesh takes a wider size range
# ----------------------------------------------------------
#  The fishery, as invented
# ----------------------------------------------------------
# Anchoveta is fished off Peru and Chile by national fleets, off Ecuador by
# a smaller one, and - in this invention - by distant-water vessels working
# the offshore edge. That gives the demo what a single-country data set
# cannot: several flags landing from the same zones, sampling at different
# intensities, and catching visibly different fish. A raising demonstration
# needs donors that are plausible neighbours but not clones.
#
# metier6 = gear_target_mesh, because derive_fields() splits on "_" and
# builds metier4/5/6 from the pieces.
FLEETS <- tibble::tribble(
  ~fleet,           ~label,                            ~share, ~cover, ~dr,
  "PS_SPF_13-15",   "industrial purse seine",            0.44,  0.85, 0.06,
  "PS_SPF_10-12",   "industrial purse seine, small mesh",0.19,  0.70, 0.11,
  "PSA_SPF_10-12",  "artisanal purse seine",             0.09,  0.25, 0.19,
  "GNS_SPF_20-30",  "wooden-boat gillnet",               0.04,  0.10, 0.24,
  "PTM_SPF_16-31",  "pelagic pair trawl (distant water)",0.16,  0.55, 0.14,
  "OTM_SPF_16-31",  "pelagic otter trawl (distant water)",0.06, 0.45, 0.16,
  "MIS_SPF_0",      "unclassified",                      0.02,  0.00, 0.30)

# Who fishes what, and how well they sample it. `mu_off` shifts the mean
# length in mm: the southern (Chilean) fish run larger, the equatorial
# (Ecuadorian) smaller, and the distant-water trawlers work further offshore
# on older fish. `samp` scales sampling effort - a fleet can land a lot and
# sample almost none, which is the situation the whole workflow exists for.
COUNTRIES <- tibble::tribble(
  ~country,        ~weight, ~mu_off, ~age_shift, ~samp,
  "Peru",             0.52,     0,       0.0,     1.00,
  "Chile",            0.26,   +11,      +0.5,     0.85,
  "Ecuador",          0.09,   -13,      -0.4,     0.45,
  "China",            0.06,    +6,      +0.7,     0.30,
  "Korea",  0.04,    +5,      +0.6,     0.25,
  "Spain",            0.02,    +8,      +0.8,     0.70,
  "Japan",            0.01,    +7,      +0.7,     0.55)

# Which unit each flag reports numbers in. This is a submission CONVENTION,
# not a function of magnitude: a national institute picks one and sticks to
# it, so a file routinely carries both and the reader has to convert. Making
# it depend on the size of the number instead would have produced a file
# that was all NE3, which would never have exercised the conversion.
NUM_UNIT <- c("Peru" = "NE3", "Chile" = "NE3", "Ecuador" = "N",
              "China" = "NE3", "Korea" = "N", "Spain" = "N",
              "Japan" = "N")

# Which fleets each flag operates. The distant-water flags do not run
# artisanal boats, and the coastal states do not send pair trawlers.
FLEET_BY_COUNTRY <- list(
  "Peru"            = c("PS_SPF_13-15","PS_SPF_10-12","PSA_SPF_10-12",
                        "GNS_SPF_20-30","MIS_SPF_0"),
  "Chile"           = c("PS_SPF_13-15","PS_SPF_10-12","PSA_SPF_10-12",
                        "MIS_SPF_0"),
  "Ecuador"         = c("PS_SPF_10-12","PSA_SPF_10-12","GNS_SPF_20-30"),
  "China"           = c("PTM_SPF_16-31","OTM_SPF_16-31"),
  "Korea" = c("PTM_SPF_16-31","OTM_SPF_16-31"),
  "Spain"           = c("PTM_SPF_16-31","OTM_SPF_16-31"),
  "Japan"           = c("OTM_SPF_16-31","PTM_SPF_16-31"))

# Zones, written in the dotted style the ICES area splitter understands so
# ICESSubArea / ICESDivision come out non-trivial. 87.1 is the northern
# (Ecuador-Peru) shelf, 87.2 central Peru, 87.3 the Peru-Chile south, and
# 87.4 the offshore high seas where the distant-water fleets work.
AREAS <- c("87.1.a", "87.1.b", "87.2.a", "87.2.b", "87.3.a", "87.3.b", "87.4.a")
AREA_W <- c("87.1.a" = 0.13, "87.1.b" = 0.20, "87.2.a" = 0.22,
            "87.2.b" = 0.15, "87.3.a" = 0.13, "87.3.b" = 0.08,
            "87.4.a" = 0.09)
# a flag fishes where its fleets can reach
AREA_BY_COUNTRY <- list(
  "Peru"            = c("87.1.a","87.1.b","87.2.a","87.2.b","87.3.a"),
  "Chile"           = c("87.3.a","87.3.b","87.2.b"),
  "Ecuador"         = c("87.1.a","87.1.b"),
  "China"           = c("87.4.a","87.3.b"),
  "Korea" = c("87.4.a","87.3.b"),
  "Spain"           = c("87.4.a","87.3.b"),
  "Japan"           = c("87.4.a","87.3.b"))

SEASONS <- c("1", "2")
SEASON_COVER <- c("1" = 1.00, "2" = 0.55)   # the second is less well covered
TOTAL_LANDINGS_T <- 4.2e6                   # invented, order of magnitude only

# ----------------------------------------------------------
#  Strata and their landings
# ----------------------------------------------------------
strata <- purrr::map_dfr(COUNTRIES$country, function(ct) {
  expand.grid(fleetValue = FLEET_BY_COUNTRY[[ct]],
              areaValue = AREA_BY_COUNTRY[[ct]],
              seasonValue = SEASONS, stringsAsFactors = FALSE) %>%
    mutate(vesselFlagCountry = ct)
}) %>%
  left_join(FLEETS, by = c("fleetValue" = "fleet")) %>%
  left_join(COUNTRIES, by = c("vesselFlagCountry" = "country")) %>%
  mutate(
    area_w   = AREA_W[areaValue],
    season_w = ifelse(seasonValue == "1", 0.58, 0.42),
    landings_t = TOTAL_LANDINGS_T * weight * share * area_w * season_w *
                 runif(n(), 0.7, 1.3))
# renormalise so the invented total still means what the README says
strata$landings_t <- strata$landings_t * TOTAL_LANDINGS_T / sum(strata$landings_t)

# a handful of strata that simply did not fish
strata <- strata %>% mutate(landings_t = ifelse(runif(n()) < 0.07, 0, landings_t))

# ---- which strata carry a discard estimate --------------------------
# Coverage is the fleet's rate, scaled by the flag's sampling effort and
# knocked down in the poorly covered season. This is what makes the
# discard-raising demo work: without gaps there is nothing to raise.
strata <- strata %>%
  mutate(p_cov = pmin(1, cover * samp * SEASON_COVER[seasonValue]),
         has_dis = runif(n()) < p_cov,
         discards_t = ifelse(has_dis,
                             landings_t * dr * runif(n(), 0.6, 1.5), NA_real_))

# ---- the two awkward cases every real submission contains -----------
# discards reported with no landings at all, and landings with a zero
# discard estimate rather than a missing one. Both are legal, both look
# like errors if the app does not handle them, so both are here.
pick <- function(pool, k) {
  if (!length(pool)) return(integer(0))
  pool[sample.int(length(pool), min(k, length(pool)))]
}
i <- pick(which(strata$landings_t > 0 & strata$has_dis), 3)
strata$landings_t[i] <- 0                                  # discards only
j <- pick(setdiff(which(strata$has_dis & strata$landings_t > 0), i), 4)
strata$discards_t[j] <- 0                                  # a true zero

# ---- who was measured, decided once ---------------------------------
# This has to be settled BEFORE the catch rows are written, because a catch
# row's domainBiology is a promise that a distribution exists for that key.
# Drawing it separately in the distributions loop left 102 catch rows
# pointing at distributions that were never generated - a dangling
# reference, which the app flags as a file error and is right to.
strata <- strata %>%
  mutate(p_bio = pmin(1, cover * samp * SEASON_COVER[seasonValue]),
         has_len = runif(n()) < p_bio * 1.70,
         has_age = has_len & runif(n()) < 0.62,
         # discards are measured far less often than landings, and only
         # where there are discards to measure
         has_dis_bio = has_dis & !is.na(discards_t) & discards_t > 0 &
                       has_len & runif(n()) < 0.55)

cat(sprintf("strata: %d | countries: %d | with a discard estimate: %d (%.0f%%)\n",
            nrow(strata), dplyr::n_distinct(strata$vesselFlagCountry),
            sum(strata$has_dis), 100 * mean(strata$has_dis)))
cat(sprintf("  measured: length %d, age %d, discard biology %d\n",
            sum(strata$has_len), sum(strata$has_age), sum(strata$has_dis_bio)))

# ----------------------------------------------------------
#  domainBiology keys
# ----------------------------------------------------------
# Shaped like the ones InterCatch writes:
#   IC_<n>_<area>_<metier6>_0_0_all
# The key is what ties a catch row to its length and age distributions, and
# it is per catch category - landings and discards are measured separately
# and can be covered independently, which is exactly the case the biology
# coverage view has to show.
dom_of <- function(area, fleet, cat, k)
  sprintf("IC_%d_%s_%s_0_0_%s", k, area, fleet,
          if (cat == "Dis") "dis" else "all")
strata$dom_id <- seq_len(nrow(strata))

# ----------------------------------------------------------
#  1. CEF route: catches / distributions / effort
# ----------------------------------------------------------
# CEF carries catch weights in kg; the app converts to tonnes on the way in.
cef_row <- function(cat, tonnes, s) tibble::tibble(
  recordType = "CN", vesselFlagCountry = s$vesselFlagCountry, year = YEAR,
  workingGroup = "WGANCHOVETA", stock = STOCK, speciesCode = SPECIES,
  catchCategory = cat, seasonType = "Season", seasonValue = s$seasonValue,
  areaType = "FAOArea", areaValue = s$areaValue,
  fisheriesManagementUnit = NA_character_, metier6 = s$fleetValue,
  fleetType = "Fleet", fleetValue = s$fleetValue,
  # domainCatchDis marks a LANDING as covered by a discard estimate;
  # build_strata() sums the landings carrying it to get coverage. It is
  # therefore set on the Lan row, not the Dis row.
  domainCatchDis = if (cat == "Lan" && isTRUE(s$has_dis))
    sprintf("dis_%s_%s_%s", s$fleetValue, s$areaValue, s$seasonValue)
    else NA_character_,
  domainCatchBMS = NA_character_,
  # Filled ONLY when a distribution actually exists for this key. A filled
  # key with nothing behind it is a dangling reference, not evidence of
  # sampling, and the app reports it as a data problem.
  domainBiology = if ((cat == "Lan" && isTRUE(s$has_len)) ||
                      (cat == "Dis" && isTRUE(s$has_dis_bio)))
    dom_of(s$areaValue, s$fleetValue, cat, s$dom_id) else NA_character_,
  originType = if (cat == "Dis") "WGEstimate" else "OfficialLandings",
  variableType = "WeightLive", variableUnit = "kg",
  total = round(tonnes * 1000, 1), variance = NA_real_,
  PSU = NA_character_, numPSUs = NA_integer_,
  numTrips = NA_integer_, comment = NA_character_)

catches <- purrr::map_dfr(seq_len(nrow(strata)), function(k) {
  s <- strata[k, ]
  out <- list()
  if (s$landings_t > 0) out <- c(out, list(cef_row("Lan", s$landings_t, s)))
  if (isTRUE(s$has_dis) && !is.na(s$discards_t))
    out <- c(out, list(cef_row("Dis", s$discards_t, s)))
  if (!length(out)) NULL else dplyr::bind_rows(out)
})

# ----------------------------------------------------------
#  Length and age distributions
# ----------------------------------------------------------
# Anchoveta is small and short-lived: 50-200 mm, ages 0-4. Lengths are
# carried in MILLIMETRES, which is what the exchange format uses and what
# prep_distributions() converts from; the app displays cm.
#
# Two variables per class, and they are the CEF ones:
#   variableType "Number"     valueType "Total"  unit N or NE3
#   variableType "WeightLive" valueType "Mean"   unit g
# NOT "NumberCaught" / "MeanWeight" - those are InterCatch output column
# names, not CEF variable types, and the app matches on "Number".
#
# Numbers at length run to billions in a fishery this size, so the bigger
# submissions are reported in thousands (NE3) and the small ones in
# individuals (N). Mixing them on purpose is the point: unit_factor()
# converts both to N, and a file where every row shares one unit would
# never show whether that works.
LEN_MM <- seq(50, 200, by = 5)
AGES   <- 0:4

# read the decision made above rather than drawing a new one, so the
# distributions written here are exactly the ones the catch rows promise
bio_of <- function(s) list(len = isTRUE(s$has_len), age = isTRUE(s$has_age))

# a length-weight relation in mm -> g, so the mean weights are consistent
# with the lengths they sit beside rather than invented separately
w_at_len <- function(mm) 0.0000045 * mm^3.17

dn_row <- function(s, cat, type, unit, cls, vtype, vunit, vt, val,
                   nsamp, nmeas) tibble::tibble(
  recordType = "DN", vesselFlagCountry = s$vesselFlagCountry, year = YEAR,
  workingGroup = "WGANCHOVETA", stock = STOCK, speciesCode = SPECIES,
  catchCategory = cat,
  domainBiology = dom_of(s$areaValue, s$fleetValue, cat, s$dom_id),
  distributionType = type, distributionUnit = unit,
  distributionClass = as.character(cls),
  ageGroupPlus = if (type == "Age") ifelse(cls == max(AGES), 1L, 0L)
                 else NA_integer_,
  attributeType = NA_character_, attributeValue = NA_character_,
  variableType = vtype, variableUnit = vunit, valueType = vt,
  value = val, variance = NA_real_,
  PSU = "FishingTrip", numPSUs = nsamp, numTrips = nsamp,
  numMeasurements = nmeas)

dist_rows <- list()
add <- function(x) dist_rows[[length(dist_rows) + 1]] <<- x

for (k in seq_len(nrow(strata))) {
  s <- strata[k, ]
  b <- bio_of(s)
  # Landings and discards are sampled separately. Discards are measured far
  # less often, and when they are the fish are smaller - that is what makes
  # a discard length distribution worth having.
  cats <- c(if (s$landings_t > 0) "Lan",
            if (isTRUE(s$has_dis_bio)) "Dis")
  for (cat in cats) {
    tonnes <- if (cat == "Lan") s$landings_t else s$discards_t
    if (tonnes <= 0) next
    # a bigger mesh takes bigger fish; the flag shifts it too, and discarded
    # fish are the small ones
    mu <- 122 + s$mu_off +
      ifelse(grepl("13-15", s$fleetValue), 14, 0) +
      ifelse(grepl("20-30", s$fleetValue), 22, 0) +
      ifelse(grepl("16-31", s$fleetValue), 9, 0) +
      ifelse(s$seasonValue == "2", -5, 0) +
      ifelse(cat == "Dis", -26, 0)
    sdev <- if (cat == "Dis") 14 else 19

    if (b$len) {
      w <- dnorm(LEN_MM, mu, sdev); w <- w / sum(w)
      mw <- w_at_len(LEN_MM)
      # how many fish were measured: the small and distant-water fleets get
      # thin samples, which is what makes the measurement threshold do
      # something rather than just separating sampled from unsampled
      nmeas <- round(switch(substr(s$fleetValue, 1, 3),
                            "PS_" = runif(1, 700, 2600),
                            "PTM" = runif(1, 120, 700),
                            "OTM" = runif(1, 90, 520),
                            "PSA" = runif(1, 45, 260),
                            "GNS" = runif(1, 20, 110),
                            runif(1, 15, 70)) * s$samp)
      nmeas <- max(8L, as.integer(nmeas))
      nsamp <- max(1L, as.integer(round(nmeas / 55)))
      # total numbers in the catch, from weight / mean weight
      n_tot <- tonnes * 1e6 / sum(w * mw)
      n_at  <- w * n_tot
      keep  <- n_at >= 1
      use_ne3 <- identical(NUM_UNIT[[s$vesselFlagCountry]], "NE3")
      add(dn_row(s, cat, "LengthTotal", "mm", LEN_MM[keep],
                 "Number", if (use_ne3) "NE3" else "N", "Total",
                 round(if (use_ne3) n_at[keep] / 1000 else n_at[keep], 2),
                 nsamp, nmeas))
      add(dn_row(s, cat, "LengthTotal", "mm", LEN_MM[keep],
                 "WeightLive", "g", "Mean", round(mw[keep], 3),
                 nsamp, nmeas))
    }
    if (b$age) {
      # a young stock: mostly ages 1 and 2, shifted by flag and by mesh
      centre <- 1.7 + s$age_shift + ifelse(cat == "Dis", -0.6, 0)
      aw <- dnorm(AGES, centre, 1.05); aw <- aw / sum(aw)
      amw <- w_at_len(70 + 34 * AGES - 2.4 * AGES^2)   # growth, mm -> g
      nage <- round(switch(substr(s$fleetValue, 1, 3),
                           "PS_" = runif(1, 150, 620),
                           "PTM" = runif(1, 40, 210),
                           "OTM" = runif(1, 30, 160),
                           "PSA" = runif(1, 14, 70),
                           runif(1, 6, 34)) * s$samp)
      nage <- max(4L, as.integer(nage))
      nsa  <- max(1L, as.integer(round(nage / 22)))
      n_tot <- tonnes * 1e6 / sum(aw * amw)
      n_at  <- aw * n_tot
      use_ne3 <- identical(NUM_UNIT[[s$vesselFlagCountry]], "NE3")
      add(dn_row(s, cat, "Age", "year", AGES,
                 "Number", if (use_ne3) "NE3" else "N", "Total",
                 round(if (use_ne3) n_at / 1000 else n_at, 2), nsa, nage))
      add(dn_row(s, cat, "Age", "year", AGES,
                 "WeightLive", "g", "Mean", round(amw, 3), nsa, nage))
    }
  }
}
distributions <- dplyr::bind_rows(dist_rows)

# One key -> one stratum. The InterCatch routes below need to get from a
# domainBiology back to the fleet, area and season it belongs to; doing that
# by splitting the string on "_" breaks the moment a metier contains one,
# which every metier here does (PS_SPF_13-15). A lookup cannot break that way.
dom_lookup <- purrr::map_dfr(seq_len(nrow(strata)), function(k) {
  s <- strata[k, ]
  tibble::tibble(
    domainBiology = c(dom_of(s$areaValue, s$fleetValue, "Lan", s$dom_id),
                      dom_of(s$areaValue, s$fleetValue, "Dis", s$dom_id)),
    catchCategory = c("Lan", "Dis"),
    fleetValue = s$fleetValue,
    areaValue = s$areaValue, seasonValue = s$seasonValue)
}) %>% distinct()


effort <- strata %>%
  filter(landings_t > 0) %>%
  transmute(recordType = "EN", vesselFlagCountry, year = YEAR,
            workingGroup = "WGANCHOVETA", stock = STOCK,
            seasonType = "Season", seasonValue, areaType = "FAOArea",
            areaValue, metier6 = fleetValue, fleetType = "Fleet",
            fleetValue, variableType = "ScientificDaysAtSea",
            variableUnit = "d", value = round(landings_t / 380 +
                                              runif(n(), 5, 40)),
            variance = NA_real_)

dir.create(file.path(OUT, "cef"), recursive = TRUE, showWarnings = FALSE)
write.csv(catches, file.path(OUT, "cef", "CEF_catches.csv"), row.names = FALSE)
write.csv(distributions, file.path(OUT, "cef", "CEF_distributions.csv"),
          row.names = FALSE)
write.csv(effort, file.path(OUT, "cef", "CEF_effort.csv"), row.names = FALSE)

# ----------------------------------------------------------
#  2. InterCatch exchange route: HI / SI / SD in one file
# ----------------------------------------------------------
# The exchange file is headerless and ragged: HI, SI and SD rows have
# different widths and sit in the same file. funReadIntercatch() slices each
# record type to its own column count, so the rows are padded to the widest.
HI_N <- 12; SI_N <- 24; SD_N <- 33

hi <- strata %>%
  filter(landings_t > 0) %>%
  distinct(vesselFlagCountry, seasonValue, fleetValue, areaValue) %>%
  transmute(V1 = "HI", V2 = vesselFlagCountry, V3 = YEAR, V4 = "Season",
            V5 = seasonValue, V6 = fleetValue, V7 = "FAOArea", V8 = areaValue,
            V9 = "", V10 = "day", V11 = round(runif(n(), 20, 400)), V12 = "")

si_rows <- list()
for (k in seq_len(nrow(strata))) {
  s <- strata[k, ]
  add <- function(cat, tonnes, samp) tibble::tibble(
    V1 = "SI", V2 = s$vesselFlagCountry, V3 = as.character(YEAR), V4 = "Season",
    V5 = s$seasonValue, V6 = s$fleetValue, V7 = "FAOArea", V8 = s$areaValue,
    V9 = "", V10 = "Engraulis ringens", V11 = STOCK, V12 = cat,
    V13 = "", V14 = "", V15 = if (cat == "Lan") "L" else "R",
    V16 = samp, V17 = "", V18 = "kg",
    V19 = as.character(round(tonnes * 1000, 1)),
    V20 = if (cat == "Lan") as.character(round(tonnes * 1000, 1)) else "",
    V21 = "", V22 = "", V23 = "", V24 = "")
  if (s$landings_t > 0) si_rows[[length(si_rows) + 1]] <- add("Lan", s$landings_t, "Off")
  if (isTRUE(s$has_dis) && !is.na(s$discards_t))
    si_rows[[length(si_rows) + 1]] <- add("Dis", s$discards_t, "Est")
  if (isTRUE(s$has_bms) && !is.na(s$bms_t) && s$bms_t > 0)
    si_rows[[length(si_rows) + 1]] <- add("BMS", s$bms_t, "Est")
}
si <- dplyr::bind_rows(si_rows)

# SD: one row per length (and per age) class of a sampled stratum
sd_rows <- list()
for (k in seq_len(nrow(strata))) {
  s <- strata[k, ]
  if (s$landings_t <= 0) next
  b <- bio_of(s)
  mk <- function(type, classes, num, mw, nsl, nlm, nsa, nam) tibble::tibble(
    V1 = "SD", V2 = s$vesselFlagCountry, V3 = as.character(YEAR), V4 = "Season",
    V5 = s$seasonValue, V6 = s$fleetValue, V7 = "FAOArea", V8 = s$areaValue,
    V9 = "", V10 = "Engraulis ringens", V11 = STOCK, V12 = "Lan",
    V13 = "", V14 = "N", V15 = type, V16 = classes, V17 = "",
    V18 = round(s$landings_t * 1000, 1), V19 = nsl, V20 = nlm,
    V21 = nsa, V22 = nam, V23 = "g", V24 = "n",
    V25 = if (type == "Age") "year" else "mm",
    V26 = "mm", V27 = "", V28 = num, V29 = mw, V30 = classes,
    V31 = "", V32 = "", V33 = "")
  # The InterCatch SD carries lengths in mm and the same size composition
  # the CEF file does - the two routes describe ONE fishery, so a demo that
  # gave them different distributions would be its own kind of lie.
  mu <- 122 + s$mu_off +
    ifelse(grepl("13-15", s$fleetValue), 14, 0) +
    ifelse(grepl("20-30", s$fleetValue), 22, 0) +
    ifelse(grepl("16-31", s$fleetValue), 9, 0) +
    ifelse(s$seasonValue == "2", -5, 0)
  if (b$len) {
    w <- dnorm(LEN_MM, mu, 19); w <- w / sum(w)
    keep <- w > 1e-4
    n_tot <- s$landings_t * 1e6 / sum(w * w_at_len(LEN_MM))
    nmeas <- max(8L, as.integer(runif(1, 40, 2000) * s$samp))
    sd_rows[[length(sd_rows) + 1]] <- mk(
      "Lngt", LEN_MM[keep], round(w[keep] * n_tot),
      round(w_at_len(LEN_MM[keep]), 3),
      max(1L, as.integer(nmeas / 55)), nmeas, 0L, 0L)
  }
  if (b$age) {
    centre <- 1.7 + s$age_shift
    aw <- dnorm(AGES, centre, 1.05); aw <- aw / sum(aw)
    amw <- w_at_len(70 + 34 * AGES - 2.4 * AGES^2)
    n_tot <- s$landings_t * 1e6 / sum(aw * amw)
    nage <- max(4L, as.integer(runif(1, 10, 600) * s$samp))
    sd_rows[[length(sd_rows) + 1]] <- mk(
      "Age", AGES, round(aw * n_tot), round(amw, 3),
      0L, 0L, max(1L, as.integer(nage / 22)), nage)
  }
}
sd <- dplyr::bind_rows(sd_rows)

pad <- function(d, n) {
  d <- as.data.frame(lapply(d, as.character), stringsAsFactors = FALSE)
  while (ncol(d) < n) d[[paste0("V", ncol(d) + 1)]] <- ""
  setNames(d[, seq_len(n)], paste0("V", seq_len(n)))
}
W <- max(HI_N, SI_N, SD_N)
exch <- dplyr::bind_rows(pad(hi, W), pad(si, W), pad(sd, W))
dir.create(file.path(OUT, "ic_exchange"), recursive = TRUE, showWarnings = FALSE)
write.table(exch, file.path(OUT, "ic_exchange", "EXCHANGE_anchoveta.csv"),
            sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE,
            na = "")

# ----------------------------------------------------------
#  3. InterCatch output route: the three overview tables
# ----------------------------------------------------------
so <- purrr::map_dfr(seq_len(nrow(strata)), function(k) {
  s <- strata[k, ]
  out <- list()
  base <- tibble::tibble(
    Stock = STOCK, Country = s$vesselFlagCountry, Area = s$areaValue,
    Season = s$seasonValue, Fleet = s$fleetValue, SeasonType = "Season",
    CatchCat = NA_character_, ReportingCategory = "R",
    DiscardsImportedOrRaised = NA_character_,
    Sampled = NA_character_, CatchWeight = NA_real_)
  if (s$landings_t > 0)
    out <- c(out, list(base %>% mutate(
      CatchCat = "Landings", DiscardsImportedOrRaised = "Imported_Data",
      Sampled = "Yes", CatchWeight = round(s$landings_t * 1000, 1))))
  if (isTRUE(s$has_dis) && !is.na(s$discards_t))
    out <- c(out, list(base %>% mutate(
      CatchCat = "Discards", DiscardsImportedOrRaised = "Imported_Data",
      Sampled = "Yes", CatchWeight = round(s$discards_t * 1000, 1))))
  if (isTRUE(s$has_bms) && !is.na(s$bms_t) && s$bms_t > 0)
    out <- c(out, list(base %>% mutate(
      CatchCat = "BMS landing", DiscardsImportedOrRaised = "Imported_Data",
      Sampled = "Yes", CatchWeight = round(s$bms_t * 1000, 1))))
  if (!length(out)) NULL else dplyr::bind_rows(out)
})

# The InterCatch OUTPUT format is a different thing again: it is what
# InterCatch WRITES, so its columns are NumberCaught and MeanWeight, and its
# lengths are in mm as integers. Those names are correct here and wrong in
# the CEF file - CEF uses variableType Number / WeightLive - which is why
# the two are built separately rather than one being a rename of the other.
nal <- distributions %>%
  filter(variableType == "Number") %>%
  left_join(dom_lookup, by = c("domainBiology", "catchCategory")) %>%
  transmute(Stock = STOCK, Country = vesselFlagCountry, Area = areaValue,
            Season = seasonValue, Fleet = fleetValue,
            CatchCat = c(Lan = "Landings", Dis = "Discards")[catchCategory],
            AgeOrLength = distributionClass,
            CANUMtype = ifelse(distributionType == "Age", "Age", "Lngt"),
            UnitAgeOrLength = distributionUnit,
            # back to individuals: the CEF rows carry NE3 where the numbers
            # are large, and InterCatch output has no unit column for them
            NumberCaught = round(ifelse(variableUnit == "NE3",
                                        value * 1000, value)),
            UnitCANUM = "n",
            NumSamples = numPSUs, NumMeasurements = numMeasurements)

mwal <- distributions %>%
  filter(variableType == "WeightLive") %>%
  left_join(dom_lookup, by = c("domainBiology", "catchCategory")) %>%
  transmute(Stock = STOCK, Country = vesselFlagCountry, Area = areaValue,
            Season = seasonValue, Fleet = fleetValue,
            CatchCat = c(Lan = "Landings", Dis = "Discards")[catchCategory],
            AgeOrLength = distributionClass,
            CANUMtype = ifelse(distributionType == "Age", "Age", "Lngt"),
            UnitAgeOrLength = distributionUnit,
            MeanWeight = value, UnitMeanWeight = "g")

dir.create(file.path(OUT, "ic_output"), recursive = TRUE, showWarnings = FALSE)
wt <- function(d, f) write.table(d, file.path(OUT, "ic_output", f), sep = "\t",
                                 row.names = FALSE, quote = FALSE, na = "")
wt(so,   "StockOverview.txt")
wt(nal,  "NumbersAtAgeLength.txt")
wt(mwal, "MeanWeigthAtAgeLength.txt")

# ----------------------------------------------------------
#  README
# ----------------------------------------------------------
cov_by_fleet <- strata %>% group_by(fleetValue) %>%
  summarise(strata = n(), with_discards = sum(has_dis),
            pct = round(100 * mean(has_dis)),
            landings_kt = round(sum(landings_t) / 1000), .groups = "drop") %>%
  arrange(desc(landings_kt))
cov_by_ctry <- strata %>% group_by(vesselFlagCountry) %>%
  summarise(strata = n(), pct_dis = round(100 * mean(has_dis)),
            landings_kt = round(sum(landings_t) / 1000), .groups = "drop") %>%
  arrange(desc(landings_kt))
len_by_ctry <- distributions %>%
  filter(distributionType == "LengthTotal", variableType == "Number",
         catchCategory == "Lan") %>%
  group_by(vesselFlagCountry) %>%
  summarise(mean_cm = round(weighted.mean(as.numeric(distributionClass),
                                          value) / 10, 1), .groups = "drop") %>%
  arrange(mean_cm)

by_ctry <- strata %>% group_by(vesselFlagCountry) %>%
  summarise(strata = n(), kt = round(sum(landings_t)/1000),
            dis = sum(has_dis), len = sum(has_len), age = sum(has_age),
            .groups = "drop") %>% arrange(desc(kt))

writeLines(c(
  "SYNTHETIC TEST DATA - Peruvian anchoveta (Engraulis ringens)",
  "===========================================================",
  "",
  "INVENTED. Every number here came out of a random generator. No IMARPE,",
  "SUBPESCA or other real figure was used, and nothing in this folder",
  "should be cited, reported, or treated as an estimate of anything. It",
  "exists to exercise the RCEF Workbench, and for no other purpose.",
  "",
  "Anchoveta is not an ICES stock. The ICES-shaped fields are borrowed so",
  "that the app's parsers have something to parse: areaValue holds FAO-87",
  "style zones and the metier strings follow gear_target_mesh, because",
  "derive_fields() splits on those conventions.",
  "",
  sprintf("Year %s | stock code '%s' | total landings ~%.1f million t",
          YEAR, STOCK, TOTAL_LANDINGS_T / 1e6),
  "",
  "SEVEN FLAGS, CATCHING DIFFERENT FISH",
  "Peru, Chile and Ecuador fish their own shelves; China, Korea, Spain and",
  "Japan work the offshore edge with pelagic trawls. They sample at very",
  "different intensities and take visibly different size and age",
  "compositions - southern fish run larger, equatorial smaller, and the",
  "distant-water trawlers take older fish. Several flags landing from the",
  "same zones is what makes a pooling decision interesting: a donor has to",
  "be a plausible neighbour, and not every neighbour is.",
  "",
  "  flag        strata   landings(kt)  w/ discards  len  age",
  paste0("  ", sprintf("%-11s %6d %13d %12d %4d %4d",
                       by_ctry$vesselFlagCountry, by_ctry$strata, by_ctry$kt,
                       by_ctry$dis, by_ctry$len, by_ctry$age)),
  "",
  "THREE ENTRY ROUTES, THE SAME FISHERY",
  "  cef/           CEF_catches.csv, CEF_distributions.csv, CEF_effort.csv",
  "                 -> load with 'CEF files'",
  "  ic_exchange/   EXCHANGE_anchoveta.csv, holding HI/SI/SD records",
  "                 -> load with 'InterCatch exchange format'",
  "  ic_output/     StockOverview.txt, NumbersAtAgeLength.txt,",
  "                 MeanWeigthAtAgeLength.txt",
  "                 -> load with 'InterCatch output format'",
  "",
  "Only one route can own the data at a time; loading a second replaces",
  "the first and everything built on it.",
  "",
  "UNITS AND VARIABLE TYPES (CEF distributions)",
  "  distributionType   LengthTotal (mm) or Age (year)",
  "  variableType       Number      valueType Total, unit N or NE3",
  "                     WeightLive  valueType Mean,  unit g",
  "",
  "  NE3 means thousands. Which unit a flag uses is a reporting CONVENTION,",
  "  not a function of magnitude - Peru, Chile and China submit in NE3,",
  "  Ecuador, Korea, Spain and Japan in N - so the file carries both and",
  "  the reader has to convert. A file where every row shared one unit",
  "  would never show whether that conversion works.",
  "",
  "  Note these are the CEF variable types. The InterCatch OUTPUT files in",
  "  ic_output/ use NumberCaught and MeanWeight, which are that format's",
  "  column names; they are not interchangeable.",
  "",
  "  Both Lan and Dis carry length and age distributions. Discarded fish",
  "  are the small ones, so a discard distribution says something the",
  "  landings one does not.",
  "",
  "SAMPLING IS DELIBERATELY UNEVEN",
  "A data set where everything is sampled raises nothing and demonstrates",
  "nothing, so coverage varies on purpose:",
  "",
  sprintf("  %d of %d strata carry a discard estimate (%.0f%%).",
          sum(strata$has_dis), nrow(strata), 100*mean(strata$has_dis)),
  sprintf("  %d carry a length distribution, %d an age distribution.",
          sum(strata$has_len), sum(strata$has_age)),
  sprintf("  Season 1 is fully covered; season 2 at about %.0f%% of that.",
          100 * SEASON_COVER[["2"]]),
  "  A few strata carry discards with no landings, and a few carry a true",
  "  zero discard estimate rather than a missing one - both occur in real",
  "  submissions and both have to be handled.",
  "",
  "  Age samples in the small and distant-water fleets are thin enough to",
  "  fall below a sensible measurement threshold WITHOUT being absent.",
  "  That is the case that separates 'no data' from 'not enough data',",
  "  which is what the two-axis biology classification is for.",
  "",
  "  Every catch row's domainBiology points at a distribution that exists.",
  "  There are no dangling references and no orphaned distributions, so",
  "  anything the app reports as a file problem is a real one.",
  "",
  "SUGGESTED DEMONSTRATION",
  "  1. Load cef/, set the coverage threshold to 50%, and look at the",
  "     coverage heatmap: the industrial Peruvian and Chilean fleets are",
  "     green, the artisanal and distant-water ones are not.",
  "  2. Build an automatic cascade on fleet -> area -> country and watch",
  "     the ladder resolve the poorly covered strata from the covered ones.",
  "  3. Then try it manually: one rule for the industrial purse seines, one",
  "     for the distant-water trawls, and a catch-all. Compare the two.",
  "  4. Raise, then go to Length and Age. Set a minimum measurement count",
  "     of about 100 and the age tab separates the thin strata from the",
  "     unsampled ones.",
  "  5. Colour a length distribution by Country to see the flags catching",
  "     different fish.",
  "",
  sprintf("Generated %s by make_test_data_anchoveta.R (seed 20260803).",
          format(Sys.Date()))), file.path(OUT, "README.txt"))

cat(sprintf("\nwritten to %s/\n  catches       %6d rows\n  distributions %6d rows\n  effort        %6d rows\n  exchange      %6d rows\n  StockOverview %6d rows\n",
            OUT, nrow(catches), nrow(distributions), nrow(effort),
            nrow(exch), nrow(so)))
