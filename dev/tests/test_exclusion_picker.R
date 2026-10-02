## The "exclude a specific stratum" picker, through the live server.
##
##  - it offers strata AS REPORTED, and only those inside the group's
##    current matched (donor) condition;
##  - it never offers a stratum with no landings AND no discards - that is
##    not a data point - but does offer one with landings and a discard
##    rate of 0;
##  - a pick moves to the excluded list and leaves the picker; the cross
##    puts it back.
## Checked on the length/age allocation box and the discard manual box,
## which share the one exclusion server.

FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}

if (!requireNamespace("shiny", quietly = TRUE)) {
  cat("shiny unavailable - skipping\n")
} else {
  ap <- system.file("app", package = "rcef")
  if (!nzchar(ap)) stop("rcef is not installed; R CMD INSTALL first")
  data("anchoveta_catch", package = "rcef", envir = environment())
  data("anchoveta_dist",  package = "rcef", envir = environment())
  DEF <- c("vesselFlagCountry", "fleetValue", "areaValue", "seasonValue")
  `%||%` <- function(a, b) if (is.null(a)) b else a
  html_of <- function(o) as.character(o$html %||% o)
  opts <- function(o) {
    h <- html_of(o)
    m <- regmatches(h, gregexpr('<option value="[^"]*"[^>]*>', h))[[1]]
    sub('^<option value="([^"]*)".*$', "\\1", m)[-1]     # drop the blank rest
  }

  ## two strata made to order: one with no landings and no discards, one
  ## with landings and a discard rate of exactly 0
  cd <- as.data.frame(anchoveta_catch)
  st <- build_strata(cd, DEF)
  both <- st[st$landings > 0 & st$discards > 0, ]
  k_key <- function(r) stratum_keys(r, DEF)
  empty  <- both[1, ]; landed <- both[2, ]
  row_of <- function(r) Reduce(`&`, lapply(DEF, function(v) cd[[v]] == r[[v]]))
  cd$total[row_of(empty)] <- 0                               # -> 0 landings, 0 discards
  cd$total[row_of(landed) & cd$catchCategory != "Lan"] <- 0  # -> landings, DR 0
  ok(k_key(empty) != k_key(landed), "(fixture: two different strata)")

  shiny::testServer(ap, {
    rv$cef_catches <- cd; rv$dist <- anchoveta_dist; rv$load_method <- "cef"
    session$setInputs(sep = ",", stock = "", year = "", threshold = 50)
    session$flushReact()

    for (id in c("len_ga_x_xui")) {
      o <- opts(output[[id]])
      ok(length(o) > 5, "the picker offers strata")
      ok(!k_key(empty) %in% o,
         "a stratum with no landings and no discards is not offered")
      ok(k_key(landed) %in% o,
         "a stratum with landings and a discard rate of 0 IS offered")
      ok(all(grepl(" \\| ", o)), "options are the strata as reported (Country | fleet | area | season)")
    }

    ## ---- scope: the matched condition narrows the list ---------------
    all_keys <- opts(output[["len_ga_x_xui"]])
    rv$len_match_pend <- list(metier4 = "PS")
    session$flushReact()
    ps <- opts(output[["len_ga_x_xui"]])
    ok(length(ps) > 0 && length(ps) < length(all_keys) && all(grepl("PS_", ps)),
       "a matched condition on metier4 narrows the picker to that metier")
    ## the allocation clause is used until a matched one is set
    rv$len_match_pend <- NULL; rv$len_alloc_pend <- list(metier4 = "MPT")
    session$flushReact()
    mp <- opts(output[["len_ga_x_xui"]])
    ok(length(mp) > 0 && all(grepl("MPT_", mp)),
       "with only an allocation clause set, that is the scope")
    rv$len_alloc_pend <- NULL; rv$len_match_pend <- list(metier4 = "PS")
    session$flushReact()

    ## ---- pick and remove ---------------------------------------------
    key <- ps[1]
    session$setInputs(len_ga_x_xpick = key)
    session$flushReact()
    ok(grepl(substr(key, 1, 20), html_of(output[["len_ga_x_xlist"]]), fixed = TRUE),
       "a pick lands in the excluded list")
    after <- opts(output[["len_ga_x_xui"]])
    ok(!key %in% after && length(after) == length(ps) - 1L,
       "... and leaves the picker, which does not otherwise fill up")
    session$setInputs(len_ga_x_xdel = list(k = key))
    session$flushReact()
    ok(key %in% opts(output[["len_ga_x_xui"]]), "removing it puts it back")

    ## ---- the discard manual builder: the same server, its own clause -
    rv$pend_raise <- list(list(var = "metier4", values = "PS", negate = FALSE))
    session$flushReact()
    dm <- opts(output[["mg_xui"]])
    ok(length(dm) > 0 && all(grepl("PS_", dm)),
       "the discard manual box is scoped by its own matched condition too")
    ok(!k_key(empty) %in% opts(output[["mg_xui"]]),
       "... and drops the no-landings-no-discards stratum as well")
  })
}

cat("\nDONE ", FAIL, "failure(s)", "\n")
