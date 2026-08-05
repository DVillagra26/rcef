## Donor exclusion: named, coverage, measurements, discard rate.
FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}

s <- data.frame(
  metier4        = c("TBB", "TBB", "OTB", "OTB", "GNS"),
  ICESDivision   = c("27.4.b", "27.4.c", "27.4.b", "27.4.c", "27.4.b"),
  landings       = c(1000, 50, 800, 5, 0),
  coverage       = c(85, 12, 40, 95, NA),
  discard_rate   = c(0.20, 0.65, 0.30, 0.02, NA),
  n_measurements = c(900, 40, 300, 12, 0),
  stringsAsFactors = FALSE)
dims <- c("metier4", "ICESDivision")

## --- the dropdown carries the numbers the decision is made on --------
ch <- exclusion_choices(s, dims, order_by = "coverage")
ok(length(ch) == 5, "every stratum is offered")
ok(grepl("cov 12%", names(ch)[1]), "the label carries % coverage")
ok(grepl("DR ",  names(ch)[1]), "the label carries the discard rate")
ok(grepl("meas", names(ch)[1]), "the label carries the measurement count")
ok(identical(unname(ch[1]), "TBB | 27.4.c"),
   "ordering by coverage puts the thinnest stratum first")
ok(identical(unname(exclusion_choices(s, dims, "weight")[1]), "TBB | 27.4.b"),
   "ordering by weight puts the heaviest first")

## --- each route on its own -------------------------------------------
ok(setequal(excluded_keys(s, dims, cov_min = 50),
            c("TBB | 27.4.c", "OTB | 27.4.b")),
   "a coverage bar drops the thinly sampled strata")
ok(!"GNS | 27.4.b" %in% excluded_keys(s, dims, cov_min = 50),
   "NA coverage is left in: unknown is not the same as low")
ok(setequal(excluded_keys(s, dims, meas_min = 100),
            c("TBB | 27.4.c", "OTB | 27.4.c", "GNS | 27.4.b")),
   "a measurement bar drops the barely measured strata")
ok(identical(excluded_keys(s, dims, named = "OTB | 27.4.c"), "OTB | 27.4.c"),
   "a named stratum is dropped")
ok(setequal(excluded_keys(s, dims, dr_min = 0.05, dr_max = 0.5),
            c("TBB | 27.4.c", "OTB | 27.4.c")),
   "a rate window drops the strata outside it")
ok(identical(excluded_keys(s, dims), character(0)),
   "nothing set excludes nothing")

## --- the routes combine, and the reason says which fired -------------
ov <- exclusion_overview(s, dims, named = "TBB | 27.4.b", cov_min = 50,
                         meas_min = 100)
ok(nrow(ov) == 5, "the three routes union")
ok(grepl("named", ov$reason[ov$key == "TBB | 27.4.b"]),
   "a named stratum says so")
ok(grepl("coverage.*measurements",
         ov$reason[ov$key == "TBB | 27.4.c"]),
   "a stratum failing two tests names both")
ok(identical(ov$key[1], "TBB | 27.4.b"), "heaviest first")

## --- the flag reaches the cascade ------------------------------------
f <- flag_donor_exclusion(s, cov_min = 50)
ok(identical(f$donor_ok, c(TRUE, FALSE, FALSE, TRUE, TRUE)),
   "flag_donor_exclusion applies a coverage bar")
f2 <- flag_donor_exclusion(s, excluded = "OTB | 27.4.c", dims = dims)
ok(identical(f2$donor_ok, c(TRUE, TRUE, TRUE, FALSE, TRUE)),
   "flag_donor_exclusion applies a named exclusion")
ok(all(flag_donor_exclusion(s)$donor_ok),
   "with nothing set every stratum stays eligible")

## --- the biology condition carries the exclusion ---------------------
mc <- alloc_matched_cond(list(catchCategory = "Dis"), "TBB | 27.4.b", dims)
ok(grepl("catchCategory", mc$text) && grepl("!paste", mc$text),
   "the clause and the exclusion are ANDed")
## quo_text() line-wraps a long expression, so compare the parsed form
ok(identical(deparse(rlang::quo_get_expr(mc$quo)),
             deparse(str2lang(mc$text))),
   "the text and the quosure are the same expression")
mc2 <- alloc_matched_cond(list(), "TBB | 27.4.b", dims)
ok(!grepl("TRUE", mc2$text),
   "a catch-all with an exclusion is not left as a bare TRUE")
ok(identical(alloc_matched_cond(list(catchCategory = "Dis"))$text,
             "catchCategory == \"Dis\""),
   "with nothing excluded the clause is untouched")

## --- exclusion never removes a stratum from what is RAISED -----------
ok(nrow(flag_donor_exclusion(s, cov_min = 50)) == nrow(s),
   "excluding a donor does not drop the row from the stratum table")

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
