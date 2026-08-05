# ============================================================
#  test_anchoveta_data.R
#  The synthetic demo data must actually exercise the workflow: partial
#  coverage with donors AND recipients, several flags catching visibly
#  different fish, the CEF variable contract the app matches on, and no
#  dangling references. A demo set where everything is sampled
#  demonstrates nothing, and one where nothing is sampled demonstrates
#  less.
#
#  Run make_test_data_anchoveta.R first; pass its output dir as argument.
# ============================================================

APP_DIR <- local({
  for (p in c(".", "R_development", file.path("..", "R_development")))
    if (file.exists(file.path(p, "0_Functions_utils.R"))) return(p)
  stop("cannot find 0_Functions_utils.R from ", normalizePath("."))
})
src_ <- function(f) source(file.path(APP_DIR, f))
DATA <- { a <- commandArgs(trailingOnly = TRUE)
          if (length(a)) a[1] else "test_data_anchoveta" }

ok <- TRUE
say <- function(...) cat(sprintf(...), "\n")
chk <- function(c, m) { if (!isTRUE(c)) { ok <<- FALSE; say("FAIL: %s", m) }
                        else say("pass: %s", m) }
suppressMessages({library(dplyr); library(tidyr); library(ggplot2)})
stub <- function(...) invisible(TRUE); environment(stub) <- globalenv()
assign("library", stub, envir = globalenv())
assign("require", stub, envir = globalenv())
for (f in c("0_Functions_utils.R", "0_Functions_theme.R",
            "0_Functions_classify.R", "mod_length.R")) src_(f)

# ----------------------------------------------------------
#  Catches
# ----------------------------------------------------------
d <- read.csv(file.path(DATA, "cef", "CEF_catches.csv"), stringsAsFactors = FALSE)
chk(nrow(d) > 0, sprintf("read catches (%d rows)", nrow(d)))
dd <- derive_fields(d)
chk(all(c("metier4","Country","Season","ICESDivision") %in% names(dd)),
    "derive_fields works on the synthetic catches")
say("   fleets   : %s", paste(sort(unique(dd$metier4)), collapse = ", "))
say("   areas    : %s", paste(sort(unique(dd$ICESDivision)), collapse = ", "))

ctry <- sort(unique(dd$Country))
chk(length(ctry) >= 5, sprintf("several flags fish this stock (%d: %s)",
                               length(ctry), paste(ctry, collapse = ", ")))
# the exchange file is written as unquoted CSV, so a comma inside a field
# silently shifts every column after it
chk(!any(grepl(",", ctry)), "no country name contains a comma")

DIMS <- c("metier4", "Country", "ICESDivision", "Season")
s <- build_strata(dd, DIMS)
chk(nrow(s) > 20, sprintf("build_strata -> %d strata", nrow(s)))
sc <- classify_strata(s, 50)
tb <- table(sc$Status)
say("   discard coverage: %d / %d strata (%.0f%%)", sum(sc$has_disc_data),
    nrow(sc), 100 * mean(sc$has_disc_data))

# Counted, not named. The status factor has a fixed level set, so checking
# that a level EXISTS passes even when every stratum sits in one of them -
# which is how an earlier version of this suite passed on a file where all
# 40 strata read "no discard data".
cnt <- function(pat) sum(tb[grepl(pat, names(tb))])
chk(sum(tb > 0) >= 3, sprintf("at least three statuses are non-empty (%d)",
                              sum(tb > 0)))
chk(cnt("^SAFE") >= 5, sprintf("enough covered strata to pool from (%d)",
                               cnt("^SAFE")))
chk(cnt("^WARNING") >= 5, sprintf("enough under-covered strata to raise (%d)",
                                  cnt("^WARNING")))
chk(cnt("^SAFE")/nrow(sc) > 0.1 && cnt("^SAFE")/nrow(sc) < 0.9,
    sprintf("coverage is genuinely partial (%.0f%% safe)",
            100 * cnt("^SAFE")/nrow(sc)))

# coverage is read from domainCatchDis on the LANDINGS row; getting that
# backwards leaves every stratum reading "no discard data" while the raw
# discard rows sit there looking fine
lan <- d[d$catchCategory == "Lan", ]
chk(sum(!is.na(lan$domainCatchDis) & nzchar(lan$domainCatchDis)) > 0,
    "landings rows carry domainCatchDis, which coverage is read from")
chk(sum(sc$landings_with_discards) > 0, "so the covered-landings total is non-zero")

# the two awkward cases every real submission contains
key <- function(x) paste(x$vesselFlagCountry, x$fleetValue, x$areaValue,
                         x$seasonValue)
kl <- key(d[d$catchCategory == "Lan", ]); kd <- key(d[d$catchCategory == "Dis", ])
chk(length(setdiff(kd, kl)) > 0,
    sprintf("some strata have discards but no landings (%d)",
            length(setdiff(kd, kl))))
chk(any(d$total[d$catchCategory == "Dis"] == 0),
    "and some carry a true zero discard rather than a missing one")

# ----------------------------------------------------------
#  Distributions: the CEF variable contract
# ----------------------------------------------------------
di <- read.csv(file.path(DATA, "cef", "CEF_distributions.csv"),
               stringsAsFactors = FALSE)
chk(all(c("domainBiology","distributionType","distributionClass",
          "variableType","value") %in% names(di)),
    "distributions carry the columns prep_distributions needs")
for (col in c("catchCategory","distributionUnit","ageGroupPlus","attributeType",
              "attributeValue","valueType","variableUnit","variance","PSU",
              "numPSUs","numTrips","numMeasurements"))
  chk(col %in% names(di), sprintf("column present: %s", col))

# The app matches on variableType == "Number"; anything else is read as a
# mean weight. An earlier version wrote "NumberCaught" / "MeanWeight" -
# those are InterCatch OUTPUT column names, not CEF variable types, and
# every number would have been read as a weight.
chk(setequal(unique(di$variableType), c("Number","WeightLive")),
    sprintf("variableType is Number / WeightLive (%s)",
            paste(sort(unique(di$variableType)), collapse = ", ")))
chk(all(di$valueType[di$variableType == "Number"] == "Total"),
    "numbers carry valueType Total")
chk(all(di$valueType[di$variableType == "WeightLive"] == "Mean"),
    "weights carry valueType Mean")
chk(all(di$variableUnit[di$variableType == "WeightLive"] == "g"),
    "weights are in grammes")
nu <- sort(unique(di$variableUnit[di$variableType == "Number"]))
chk(all(nu %in% c("N","NE3")), sprintf("numbers are in N or NE3 (%s)",
                                       paste(nu, collapse = ", ")))
chk(length(nu) == 2,
    "both units appear - a single-unit file would never exercise the conversion")
chk(all(di$distributionUnit[di$distributionType == "LengthTotal"] == "mm"),
    "lengths are in mm")
chk(setequal(unique(di$catchCategory), c("Lan","Dis")),
    "distributions cover both Lan and Dis")

# ---- and the units actually convert ----------------------------------
pl <- prep_distributions(di, "LengthTotal")
chk(!is.null(pl) && nrow(pl) > 0, "prep_distributions accepts the file")
chk(all(c("numbers","meanweight") %in% unique(pl$.var_kind)),
    "both kinds are recognised - not one silently read as the other")
cls <- range(pl$.class)
chk(cls[1] >= 4 && cls[2] <= 25,
    sprintf("length classes land in a sane cm range (%.1f - %.1f)", cls[1], cls[2]))
nn <- pl[pl$.var_kind == "numbers", ]
ne3 <- di$variableType == "Number" & di$variableUnit == "NE3"
chk(any(ne3), "some rows are reported in NE3")
chk(max(nn$.value_std) > max(as.numeric(di$value[ne3])),
    "and NE3 rows were scaled to N, not taken at face value")
mwr <- range(pl$.value_std[pl$.var_kind == "meanweight"])
chk(mwr[1] > 0.2 && mwr[2] < 200,
    sprintf("mean weights are plausible grammes for anchoveta (%.1f - %.1f)",
            mwr[1], mwr[2]))

# discarded fish should be the small ones - otherwise a discard length
# distribution tells the reader nothing they did not already know
mean_cm <- function(x) sum(x$.class * x$.value_std) / sum(x$.value_std)
mL <- mean_cm(nn[nn$catchCategory == "Lan", ])
mD <- mean_cm(nn[nn$catchCategory == "Dis", ])
chk(mD < mL, sprintf("discards are smaller than landings (%.1f vs %.1f cm)", mD, mL))

# several flags is only useful if they catch different fish
by_ctry <- nn %>% filter(catchCategory == "Lan") %>%
  group_by(vesselFlagCountry) %>%
  summarise(cm = sum(.class * .value_std)/sum(.value_std), .groups = "drop") %>%
  arrange(cm)
chk(nrow(by_ctry) >= 3, sprintf("at least three flags sample lengths (%d)",
                                nrow(by_ctry)))
chk(diff(range(by_ctry$cm)) > 0.8,
    sprintf("and their size compositions differ (%.1f cm spread)",
            diff(range(by_ctry$cm))))
say("   mean landed length by flag: %s",
    paste(sprintf("%s %.1f", by_ctry$vesselFlagCountry, by_ctry$cm),
          collapse = " | "))

pa <- prep_distributions(di, "Age")
chk(!is.null(pa) && setequal(sort(unique(pa$.class)), 0:4),
    "ages read back as 0-4")
nl <- length(unique(di$domainBiology[di$distributionType == "LengthTotal"]))
na <- length(unique(di$domainBiology[di$distributionType == "Age"]))
chk(nl > na, sprintf("length sampled more widely than age (%d vs %d strata)",
                     nl, na))
m <- di %>% filter(distributionType == "Age") %>%
  distinct(domainBiology, numMeasurements)
chk(any(m$numMeasurements < 100) && any(m$numMeasurements > 100),
    sprintf("age measurement counts straddle a 100-fish threshold (%d..%d)",
            min(m$numMeasurements), max(m$numMeasurements)))

# ---- no dangling references ------------------------------------------
# A catch row's domainBiology is a promise that a distribution exists for
# that key. Filling it on every row left 102 catch rows pointing at nothing,
# which the app correctly reports as a file error.
bl <- biology_link(dd, di, DIMS, type = "LengthTotal")
chk(sum(bl$n_dangling) == 0,
    sprintf("no catch row promises a distribution that is absent (%d)",
            sum(bl$n_dangling)))
chk(is.null(dist_without_catch(dd, di, type = "LengthTotal")),
    "and no distribution is orphaned from its catch")
chk(sum(bl$sampled) > 0 && sum(!bl$sampled) > 0,
    sprintf("biology coverage is partial too (%d of %d strata measured)",
            sum(bl$sampled), nrow(bl)))

# ----------------------------------------------------------
#  The other two routes
# ----------------------------------------------------------
ex <- readLines(file.path(DATA, "ic_exchange", "EXCHANGE_anchoveta.csv"),
                warn = FALSE)
tp <- table(sub(",.*", "", ex))
say("   exchange records: %s", paste(sprintf("%s=%d", names(tp), tp),
                                     collapse = " "))
chk(all(c("HI","SI","SD") %in% names(tp)), "exchange file holds HI, SI and SD")
nf <- vapply(strsplit(ex, ",", fixed = TRUE), length, integer(1))
chk(length(unique(nf)) == 1,
    sprintf("every exchange row has the same field count (%s)",
            paste(unique(nf), collapse = "/")))

so <- read.delim(file.path(DATA, "ic_output", "StockOverview.txt"),
                 stringsAsFactors = FALSE)
chk(all(c("Stock","Country","Area","Season","Fleet","CatchCat","CatchWeight")
        %in% names(so)), "StockOverview has the expected columns")
chk(all(c("Landings","Discards") %in% so$CatchCat), "and both catch categories")
nal <- read.delim(file.path(DATA, "ic_output", "NumbersAtAgeLength.txt"),
                  stringsAsFactors = FALSE)
# the OUTPUT format is what InterCatch writes, so NumberCaught / MeanWeight
# are the right names there and the wrong ones in the CEF file
chk("NumberCaught" %in% names(nal),
    "the InterCatch output table uses its own column names")
chk(all(c("Lngt","Age") %in% nal$CANUMtype), "and carries both class types")

cat("\n"); say("=== %s ===", if (ok) "ALL CHECKS PASSED" else "THERE WERE FAILURES")
cat("DONE test_anchoveta_data\n")
