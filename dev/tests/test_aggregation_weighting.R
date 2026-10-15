## meanWoLatAoLAggregation(): what each weighting weights by.
##
##  * "NumberAtAoL" (the default): each distribution's mean weight at a class counts as many
##    times as its numbers at that class;
##  * "CATON": each counts as many times as the total weight of the catch row it belongs to.
##
## "CATON" used to look for a `total` column the function had already dropped and stopped with
## "object 'total' not found". Both are recomputed here from the allocation result, by hand.

source("dev/tests/fixture.R")
FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}
quiet <- function(expr) { out <- NULL; invisible(utils::capture.output(out <- suppressMessages(expr))); out }
near <- function(a, b) isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = 1e-9))

## the allocation of the fixture: one catch row for every domain, as the aggregations take it (a distribution is
## counted once for each stratum it is linked to)
alloc <- fx_allocation()$len

agg <- function(w) quiet(meanWoLatAoLAggregation(alloc$distribution, alloc$catch, grouping = "catchCategory", weighting = w,
                                                 originType_catch = "WGEstimate", variableType_catch = "WeightLive",
                                                 distributionType = "LengthTotal"))
n_caton <- tryCatch(agg("CATON"), error = function(e) e)
n_num   <- agg("NumberAtAoL")
ok(!inherits(n_caton, "error"), sprintf("weighting = \"CATON\" runs%s",
   if (inherits(n_caton, "error")) paste0(" - ", conditionMessage(n_caton)) else ""))

## ---- the same thing by hand --------------------------------------------------------------
keys <- c("vesselFlagCountry", "year", "workingGroup", "stock", "speciesCode", "catchCategory", "domainBiology")
cs <- alloc$catch[alloc$catch$originType == "WGEstimate" & alloc$catch$variableType == "WeightLive", c(keys, "total")]
D  <- alloc$distribution[alloc$distribution$distributionType == "LengthTotal", ]
W  <- D[D$variableType == "WeightLive", ]
N  <- D[D$variableType == "Number", c(keys, "attributeType", "attributeValue", "distributionClass", "value")]
names(N)[names(N) == "value"] <- "n"

by_hand <- function(w) {
  m <- dplyr::inner_join(W, cs, by = keys, suffix = c("", ".catch"))
  m <- dplyr::left_join(m, N, by = c(keys, "attributeType", "attributeValue", "distributionClass"))
  m$w <- if (identical(w, "CATON")) m$total else m$n
  m <- m[!is.na(m$w), ]
  out <- stats::aggregate(cbind(wv = value * w, w = w) ~ catchCategory + distributionClass, data = m, FUN = sum)
  out$mean <- out$wv / out$w
  out
}
check <- function(res, w) {
  h <- by_hand(w)
  cols <- grep("^W_LengthTotal_", names(res), value = TRUE)
  cls  <- as.numeric(sub("^W_LengthTotal_", "", cols))
  all(vapply(seq_len(nrow(res)), function(i) {
    hh <- h[h$catchCategory == res$catchCategory[i], ]
    v  <- as.numeric(res[i, cols])
    ref <- hh$mean[match(cls, hh$distributionClass)]
    near(v[!is.na(ref)], ref[!is.na(ref)])
  }, NA))
}
if (!inherits(n_caton, "error")) {
  ok(check(n_caton, "CATON"), "\"CATON\": every class is the mean weight weighted by the total weight of the catch row")
  ok(check(n_num, "NumberAtAoL"), "\"NumberAtAoL\": every class is the mean weight weighted by the numbers at that class")
  cols <- grep("^W_LengthTotal_", names(n_caton), value = TRUE)
  ok(!isTRUE(all.equal(as.matrix(n_caton[, cols]), as.matrix(n_num[, cols]), tolerance = 1e-9)),
     "... and the two weightings do not give the same table")
  ok(identical(names(n_caton), names(n_num)) && nrow(n_caton) == nrow(n_num), "the shape is the same either way")
}
ok(inherits(tryCatch(agg("nope"), error = function(e) e), "error"), "a weighting it does not know is refused")

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
