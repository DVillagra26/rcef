## ---------------------------------------------------------------
##  dev/check_server.R
##
##  RUNS the server function. check_app.R only builds the UI, which is
##  why it passed while the app died on launch: a module server called a
##  helper that was defined a thousand lines further down, the server
##  function aborted, and every definition after that point never ran.
##  Nothing that only renders tags can see that.
##
##  This starts a real session, lets the reactive graph settle, and fails
##  on any error raised while the server body executes.
##
##  Usage:  Rscript dev/check_server.R
## ---------------------------------------------------------------
for (pk in c("shiny", "DT", "rcef"))
  if (!requireNamespace(pk, quietly = TRUE))
    stop("check_server.R needs ", pk)

d <- system.file("app", package = "rcef")
if (!nzchar(d)) stop("rcef is not installed; R CMD INSTALL first")

fail <- 0L
say <- function(good, what, extra = "") {
  if (good) cat("ok    ", what, "\n")
  else { fail <<- fail + 1L; cat("FAIL  ", what, "\n", extra, "\n") }
}

res <- tryCatch({
  shiny::testServer(d, {
    session$flushReact()
    ## the helpers a module server calls directly while the body runs
    for (fn in c("exclusion_server", "flt_inputs", "explore_dat",
                 "filter_server", "excl_num", "dmap", "strata_p",
                 "strata_x", "dat"))
      if (!exists(fn, inherits = TRUE))
        stop("helper not defined in the server body: ", fn)
    TRUE
  })
  TRUE
}, error = function(e) conditionMessage(e))

say(isTRUE(res), "the server function runs to completion", res)

## ---------------------------------------------------------------
##  The filter, end to end, on a real CEF table.
##
##  Not "the box renders" - that a filter narrows the FIGURE data and
##  leaves the RAISING data alone, and that changing it makes the gate
##  go stale so Apply has something to apply. Every one of those was
##  wrong at some point and nothing caught it.
## ---------------------------------------------------------------
fx <- list.files(system.file("extdata", package = "rcef"),
                 pattern = "catches_CEF.*\\.csv$", recursive = TRUE,
                 full.names = TRUE)
if (!length(fx)) {
  cat("skip   no CEF fixture installed; filter round-trip not run\n")
} else {
  res2 <- tryCatch({
    shiny::testServer(d, {
      rv$cef_catches <- utils::read.csv(fx[1], stringsAsFactors = FALSE)
      session$flushReact()
      dd <- dat()
      lv <- expf_levels()
      if (!length(lv)) stop("no filterable columns in the fixture")
      v   <- names(lv)[1]
      one <- lv[[v]][1]

      before <- cov_sig()
      n_all  <- nrow(dd)
      n_x0   <- nrow(explore_dat("cov"))

      rv$flt[["cov"]] <- stats::setNames(list(one), v)
      session$flushReact()

      if (!identical(flt_inputs("cov"), clean_filters(stats::setNames(list(one), v))))
        stop("flt_inputs did not pick the filter up from rv$flt")
      n_x1 <- nrow(explore_dat("cov"))
      if (!(n_x1 < n_x0)) stop("the filter did not narrow explore_dat()")
      if (nrow(dat()) != n_all) stop("the filter reached dat() - it must not")
      if (identical(cov_sig(), before))
        stop("cov_sig() did not change, so Apply can never redraw")
      if (nrow(strata_x("cov")) >= nrow(strata_p("cov")))
        stop("strata_x() is not narrower than strata_p()")

      rv$flt[["cov"]] <- list()
      session$flushReact()
      if (nrow(explore_dat("cov")) != n_x0) stop("clearing the filter did not restore")
      TRUE
    })
    TRUE
  }, error = function(e) conditionMessage(e))
  say(isTRUE(res2),
      "a filter narrows the figure data, not the raising data, and moves the gate",
      res2)
}

## ---------------------------------------------------------------
##  Apply must redraw EVERY filtered panel, not just the two that
##  happened to be written first.
##
##  This is the check that was missing. Every panel builds its gate in
##  module-local scope, so the only way to test "does Apply redraw this
##  one" was to open the app and look - and the coverage and discard-rate
##  panels worked while all six biology panels did not, because their
##  signature wrapped the filter in reactive(). A fresh reactive object
##  was built on every evaluation and thrown away, so a filter change
##  invalidated something nothing held, the signature stayed cached, and
##  the panel never went stale. gate_of() registers its gate now, which
##  is what makes this testable at all.
## ---------------------------------------------------------------
if (!length(fx)) {
  cat("skip   no CEF fixture installed; Apply round-trip not run\n")
} else {
  dxf <- list.files(system.file("extdata", package = "rcef"),
                    pattern = "distributions_CEF.*\\.csv$",
                    recursive = TRUE, full.names = TRUE)
  # testServer() does not return the value of its expression, so the
  # result is carried out in an environment rather than by return value.
  outbox <- new.env(parent = emptyenv())
  bad <- tryCatch({
    shiny::testServer(d, {
      rv$cef_catches <- utils::read.csv(fx[1], stringsAsFactors = FALSE)
      if (length(dxf)) rv$dist <- utils::read.csv(dxf[1], stringsAsFactors = FALSE)
      session$flushReact()
      lv <- expf_levels()
      if (!length(lv)) stop("no filterable columns in the fixture")
      v <- names(lv)[1]; one <- lv[[v]][1]
      broken <- character(0)
      for (pfx in c("cov", "dr", "len_cov", "len_n", "len_w",
                    "age_cov", "age_n", "age_w")) {
        g <- gate_reg[[pfx]]
        if (is.null(g)) { broken <- c(broken, paste0(pfx, " (no gate)")); next }
        rv$flt[[pfx]] <- list(); session$flushReact()
        s0 <- g$sig(); before <- g$now()
        rv$flt[[pfx]] <- stats::setNames(list(one), v); session$flushReact()
        if (identical(g$sig(), s0)) {
          broken <- c(broken, paste0(pfx, " (signature never moves)"))
        } else {
          do.call(session$setInputs,
                  stats::setNames(list(1), paste0(pfx, "_apply")))
          session$flushReact()
          if (identical(g$now(), before))
            broken <- c(broken, paste0(pfx, " (Apply does not redraw)"))
        }
        rv$flt[[pfx]] <- list(); session$flushReact()
      }
      outbox$broken <- broken
    })
    outbox$broken
  }, error = function(e) conditionMessage(e))
  say(is.character(bad) && length(bad) == 0,
      "a filter change makes every plot panel stale, and Apply redraws it",
      if (is.character(bad)) paste(bad, collapse = "; ") else bad)

  ## ---------------------------------------------------------------
  ##  Per-tab level of detail is independent.
  ##
  ##  Ported from the retired test_app_server.R. Each figure tab picks its
  ##  own fleet and area resolution; changing one must not move another.
  ##  The old test asserted this against Area1, a dimension since removed,
  ##  so only the independence check survives - the mechanism (dmap reads
  ##  <prefix>_fleet / <prefix>_area) is what matters, not the columns.
  ## ---------------------------------------------------------------
  lod <- tryCatch({
    shiny::testServer(d, {
      rv$cef_catches <- utils::read.csv(fx[1], stringsAsFactors = FALSE)
      session$flushReact()
      session$setInputs(cov_fleet = "metier5", cov_area = "ICESDivision",
                        dr_fleet  = "metier4", dr_area  = "ICESSubArea")
      session$flushReact()
      a <- unname(dmap("cov")["Fleet"]); b <- unname(dmap("dr")["Fleet"])
      c1 <- unname(dmap("cov")["Area"]); c2 <- unname(dmap("dr")["Area"])
      if (!(a == "metier5" && b == "metier4"))
        stop("fleet level bled across tabs: cov=", a, " dr=", b)
      if (!(c1 == "ICESDivision" && c2 == "ICESSubArea"))
        stop("area level bled across tabs: cov=", c1, " dr=", c2)
      TRUE
    })
    TRUE
  }, error = function(e) conditionMessage(e))
  say(isTRUE(lod),
      "each tab's fleet/area level of detail is independent of the others",
      lod)

  ## ---------------------------------------------------------------
  ##  Force the code / status outputs to actually render.
  ##
  ##  These are the ones a wiring typo hides in - the mg_excl bug lived in
  ##  a renderUI that never ran until the panel was opened. They take no
  ##  inputs beyond loaded data, so rendering them here is cheap and
  ##  cannot raise a false validate() failure. Top-level ids only;
  ##  module-namespaced ones need the module session and are covered by
  ##  the gate round-trip above.
  outs <- c("auto_conds", "man_conds", "code_txt", "final_status")
  rend <- tryCatch({
    shiny::testServer(d, {
      rv$cef_catches <- utils::read.csv(fx[1], stringsAsFactors = FALSE)
      session$flushReact()
      broke <- character(0)
      for (o in outs) {
        v <- tryCatch({ force(output[[o]]); NULL },
                      error = function(e) conditionMessage(e))
        if (!is.null(v)) broke <- c(broke, paste0(o, ": ", v))
      }
      outbox$broke <- broke
    })
    outbox$broke
  }, error = function(e) conditionMessage(e))
  say(is.character(rend) && length(rend) == 0,
      "the code and status outputs render without error",
      if (is.character(rend)) paste(rend, collapse = "; ") else rend)
}

cat(sprintf("\nDONE  %d failure(s)\n", fail))
if (fail) quit(status = 1L)
