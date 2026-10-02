## A manual group defined on a column that is not one of the reported strata
## keys (vesselFlagCountry / fleetValue / areaValue / seasonValue) - metier4 == TBB, say -
## must actually take its strata.
##
## Seen in the app as "the TBB group takes nothing; every stratum is STILL
## MISSING". The strata are fixed as reported now, so the ONLY way a
## clause's column reaches the table the group is tested against is the
## builder adding it. The discard manual builder read the clause columns
## from r$clauses, a field a rule does not have (its clauses are in
## $raise and $source), so none were ever added and the clause silently
## matched nothing. The length/age builder collects them from its saved
## and pending groups; both are pinned here, on the live server.

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

  cd <- derive_fields(as.data.frame(anchoveta_catch))
  ref <- build_strata(cd, c(DEF, "metier4"))
  n_ps <- sum(ref$metier4 == "PS")
  ok(n_ps > 0 && n_ps < nrow(ref), "(fixture: PS is some, not all, of the strata)")

  shiny::testServer(ap, {
    rv$cef_catches <- anchoveta_catch; rv$dist <- anchoveta_dist
    rv$load_method <- "cef"
    session$setInputs(sep = ",", stock = "", year = "", threshold = 50)
    session$flushReact()

    ## ---- discard manual builder --------------------------------------
    cl <- list(list(var = "metier4", values = "PS", negate = FALSE))
    ok(!"metier4" %in% mg_dims(), "before a rule exists, metier4 is not a stratum column")
    rv$rules <- list(new_rule("G1", "PS", raise = cl))
    session$flushReact()
    ok("metier4" %in% mg_dims(),
       "a saved rule's clause column is added to the manual builder's strata")
    ok(all(DEF %in% mg_dims()), "... on top of the strata as reported, not instead of them")
    st <- man_strata()
    ok("metier4" %in% names(st), "the strata the rule is tested against carry metier4")
    pr <- rules_progress(st, rv$rules, rv$sets)
    g1 <- pr[pr$group == "G1", ]
    ok(nrow(g1) == 1 && g1$n_strata == n_ps,
       sprintf("the group takes its %d PS strata (not 0)", n_ps))
    ok(nrow(st) == nrow(ref),
       "adding the clause column did not split any stratum")

    ## a clause only PENDING (dropped in a zone, not saved) counts too
    rv$rules <- list()
    rv$pend_source <- list(list(var = "metier4", values = "PS", negate = FALSE))
    session$flushReact()
    ok("metier4" %in% mg_dims(), "so does a clause that is still pending")
    rv$pend_source <- NULL

    ## ---- length/age allocation builder -------------------------------
    gl <- list(GA1 = list(label = "PS", alloc = alloc_cond_string(list(metier4 = "PS")),
      matched = alloc_cond_string(list(metier4 = "PS")), excluded = character(0),
      alloc_expr = alloc_quo(list(metier4 = "PS")),
      matched_expr = alloc_quo(list(metier4 = "PS"))))
    bl <- suppressWarnings(biology_link(cd, anchoveta_dist,
            c("catchCategory", DEF, "metier4"), NULL, "LengthTotal"))
    rv$len_groups <- gl
    session$flushReact()
    chk <- as.character(output[["len_ga_check"]]$html %||% output[["len_ga_check"]])
    m <- regmatches(chk, regexpr("[0-9]+(?= strata [(])", chk, perl = TRUE))
    miss <- if (length(m)) as.integer(m) else NA_integer_
    ok(!is.na(miss) && miss == sum(bl$metier4 != "PS"),
       sprintf("length/age: the PS group takes its strata; %d others are still uncovered",
               sum(bl$metier4 != "PS")))
  })
}

cat("\nDONE ", FAIL, "failure(s)", "\n")
