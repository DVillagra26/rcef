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
                 "filter_server", "excl_num", "strata_p",
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
fx <- "anchoveta"
if (!length(fx)) {
  cat("skip   no CEF fixture installed; filter round-trip not run\n")
} else {
  res2 <- tryCatch({
    shiny::testServer(d, {
      rv$cef_catches <- {data("anchoveta_catch", package = "rcef", envir = environment()); anchoveta_catch}
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
  dxf <- "anchoveta"
  # testServer() does not return the value of its expression, so the
  # result is carried out in an environment rather than by return value.
  outbox <- new.env(parent = emptyenv())
  bad <- tryCatch({
    shiny::testServer(d, {
      rv$cef_catches <- {data("anchoveta_catch", package = "rcef", envir = environment()); anchoveta_catch}
      if (length(dxf)) rv$dist <- {data("anchoveta_dist", package = "rcef", envir = environment()); anchoveta_dist}
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
  ##  Per-tab axis selections are independent.
  ##
  ##  Ported from the retired test_app_server.R, twice now. The original
  ##  asserted on Area1, a dimension since removed. The first port
  ##  checked dmap("cov")/dmap("dr") - an abstract "Fleet"/"Area" picker
  ##  per tab - which the strata-definition-box rework superseded: a
  ##  figure's axes name real columns directly now (cov_y, dr_x, ...),
  ##  there is no separate level-of-detail control to read. dmap() itself
  ##  was confirmed dead and removed. What survives is the actual claim:
  ##  changing one tab's axis selection must not move another's.
  ## ---------------------------------------------------------------
  lod <- tryCatch({
    shiny::testServer(d, {
      rv$cef_catches <- {data("anchoveta_catch", package = "rcef", envir = environment()); anchoveta_catch}
      session$flushReact()
      session$setInputs(cov_y = "metier5", cov_fill = "ICESDivision",
                        dr_x  = "metier4", dr_fill  = "ICESSubArea")
      session$flushReact()
      a <- panel_dims("cov"); b <- panel_dims("dr")
      if (!("metier5" %in% a && "ICESDivision" %in% a))
        stop("cov's own axes did not reach panel_dims('cov'): ",
             paste(a, collapse = ","))
      if (!("metier4" %in% b && "ICESSubArea" %in% b))
        stop("dr's own axes did not reach panel_dims('dr'): ",
             paste(b, collapse = ","))
      if ("metier5" %in% b || "ICESDivision" %in% b)
        stop("cov's axis selection bled into dr: ", paste(b, collapse = ","))
      if ("metier4" %in% a || "ICESSubArea" %in% a)
        stop("dr's axis selection bled into cov: ", paste(a, collapse = ","))
      TRUE
    })
    TRUE
  }, error = function(e) conditionMessage(e))
  say(isTRUE(lod),
      "each tab's axis selections are independent of the others",
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
      rv$cef_catches <- {data("anchoveta_catch", package = "rcef", envir = environment()); anchoveta_catch}
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

## ---------------------------------------------------------------
##  Changing the aggregation level must not crash recommend_raising().
##
##  The strata were built from the LIVE level-of-detail control while the
##  hierarchy came from the FROZEN applied signature, so a level change
##  moved the strata columns instantly while the hierarchy stayed frozen
##  - and recommend_raising() aborted with "hierarchy %in% names(strata)
##  is not TRUE". Both must come from the same (applied) dims.
##
##  Ported forward again: the level-of-detail control used to be
##  ar_fleet/ar_area, superseded by the strata-definition box
##  (ar_sd_sdmode/ar_sd_sd1..5). Those two old inputs no longer bind to
##  anything, so setting them was a silent no-op and this check was
##  passing without exercising the scenario it names.
## ---------------------------------------------------------------
if (length(fx)) {
  box <- new.env(parent = emptyenv())
  crash <- tryCatch({
    shiny::testServer(d, {
      rv$cef_catches <- {data("anchoveta_catch", package = "rcef", envir = environment()); anchoveta_catch}
      session$flushReact()
      session$setInputs(hier_2 = "Country", hier_3 = "Season", threshold = 50,
                        ar_sd_sdmode = "custom", ar_sd_sd1 = "metier4",
                        ar_sd_sd2 = "", ar_sd_sd3 = "", ar_sd_sd4 = "",
                        ar_sd_sd5 = "")
      session$flushReact()
      session$setInputs(ar_apply = 1); session$flushReact()
      box$n1 <- tryCatch(nrow(rec()), error = function(e) NA)
      session$setInputs(ar_sd_sd1 = "metier6", ar_sd_sd2 = "areaValue")
      session$flushReact()
      box$before <- tryCatch(nrow(rec()), error = function(e) conditionMessage(e))
      session$setInputs(ar_apply = 2); session$flushReact()
      box$after <- tryCatch(nrow(rec()), error = function(e) conditionMessage(e))
    })
    list(n1 = box$n1, before = box$before, after = box$after)
  }, error = function(e) conditionMessage(e))
  if (is.list(crash)) {
    say(is.numeric(crash$before),
        "a level change before Apply does not crash recommend_raising",
        crash$before)
    say(is.numeric(crash$after),
        "and Apply at the new level recomputes cleanly",
        crash$after)
  } else {
    say(FALSE, "level-change round trip runs without error", crash)
  }
}

## ---------------------------------------------------------------
##  The plot options reach the plot function AND the script.
##
##  Discard rate: which rate is the y axis, and a reference line typed in
##  percent (handed over as a fraction). Mean weight: bars of the fish
##  measured on both tabs and, on the age tab, a plus group. Each must move
##  the gate's signature (so Apply has something to apply), draw, and be
##  written into the kept script - and the script must reproduce the
##  picture: a control that only changed what the screen shows would leave a
##  script that cannot. The kept text is run here, against the data the panel
##  used, rather than only read.
## ---------------------------------------------------------------
if (length(fx)) {
  ob <- new.env(parent = emptyenv())
  res5 <- tryCatch({
    shiny::testServer(d, {
      rv$cef_catches <- {data("anchoveta_catch", package = "rcef", envir = environment()); anchoveta_catch}
      rv$dist <- {data("anchoveta_dist", package = "rcef", envir = environment()); anchoveta_dist}
      session$flushReact()
      has_layer <- function(p, cls)
        any(vapply(p$layers, function(l) inherits(l$geom, cls), logical(1)))
      kept_text <- function() {
        k <- rv$kept[[length(rv$kept)]]
        paste(c(k$prep, k$call), collapse = "\n")
      }
      x_labels <- function(p)
        ggplot_build(p)$layout$panel_params[[1]]$x$get_labels()

      ## ---- discard rate ----
      session$setInputs(dr_x = "metier4", dr_fill = "Country", dr_shape = "",
                        dr_facet = "", dr_jitter = TRUE, dr_rate = "ratio",
                        dr_ref = NA)
      session$setInputs(dr_apply = 1); session$flushReact()
      p0 <- dr_plot_obj(); s0 <- gate_reg[["dr"]]$sig()
      session$setInputs(dr_rate = "rate", dr_ref = 20); session$flushReact()
      ob$dr_stale <- !identical(gate_reg[["dr"]]$sig(), s0) &&
        isTRUE(gate_reg[["dr"]]$stale())
      session$setInputs(dr_apply = 2); session$flushReact()
      p1 <- dr_plot_obj()
      ob$dr_before <- identical(p0$labels$y, "Discard ratio") &&
        !has_layer(p0, "GeomHline")
      hl <- Filter(function(l) inherits(l$geom, "GeomHline"), p1$layers)
      ob$dr_after <- identical(p1$labels$y, "Discard rate") &&
        length(hl) == 1 && isTRUE(all.equal(hl[[1]]$data$yintercept, 20)) &&
        isTRUE(all.equal(p1$data$.y, p1$data$discardRate * 100))
      ob$dr_live <- grepl('rate = "rate"', active_plots()[[1]], fixed = TRUE) &&
        grepl("reference = 0.2", active_plots()[[1]], fixed = TRUE)
      session$setInputs(dr_plot_keep = 1); session$flushReact()
      kt <- kept_text()
      ob$dr_script <- grepl('rate = "rate"', kt, fixed = TRUE) &&
        grepl("reference = 0.2", kt, fixed = TRUE)
      env <- list2env(list(catch_data = explore_dat("dr")), parent = environment())
      pr <- tryCatch(eval(parse(text = kt), env), error = function(e) NULL)
      ob$dr_repro <- inherits(pr, "ggplot") &&
        identical(pr$labels$y, p1$labels$y) &&
        isTRUE(all.equal(pr$data$.y, p1$data$.y))
      ## a reference the rate cannot reach is said so, not drawn off the plot
      session$setInputs(dr_ref = 150, dr_apply = 3); session$flushReact()
      e150 <- tryCatch({ dr_plot_obj(); "no error" }, error = function(e) class(e))
      ob$dr_bad <- "shiny.silent.error" %in% e150
      ## and without a reference, and the default rate, nothing extra is written
      session$setInputs(dr_rate = "ratio", dr_ref = NA, dr_apply = 4)
      session$flushReact()
      session$setInputs(dr_plot_keep = 2); session$flushReact()
      kt0 <- kept_text()
      ob$dr_plain <- !grepl("rate =", kt0, fixed = TRUE) &&
        !grepl("reference", kt0, fixed = TRUE)

      ## ---- discard coverage: the live heatmap and bubbles still render ----
      ## coverage_call()'s min_fish defaults to NULL and is only ever attached
      ## for length / age - min_fish = 0 (its old default) is active per
      ## thr_active(), which plot_coverage_heat()/_bubble() now refuse for
      ## what = "discard", and this tab never sets `what` at all.
      session$setInputs(cov_y = "metier4", cov_fill = "Country", cov_facet = "",
                        threshold = 50, cov_trips = 2, cov_apply = 1)
      session$flushReact()
      session$setInputs(cov_type = "heatmap"); session$flushReact()
      v <- tryCatch({ force(output$cov_plot); "rendered" },
                    error = function(e) paste("ERROR:", conditionMessage(e)))
      ob$cov_heat_render <- identical(v, "rendered")
      session$setInputs(cov_type = "bubbles"); session$flushReact()
      v <- tryCatch({ force(output$cov_plot); "rendered" },
                    error = function(e) paste("ERROR:", conditionMessage(e)))
      ob$cov_bubble_render <- identical(v, "rendered")

      ## ---- mean weight: bars on both tabs, a plus group on the age tab ----
      for (pf in c("len", "age")) {
        ty <- if (identical(pf, "age")) "Age" else "LengthTotal"
        base <- stats::setNames(
          list("catlan", "Country", "", "", "", FALSE, FALSE, 3),
          paste0(pf, "_w_", c("cc", "r_fill", "r_facet", "r_shape", "pt",
                              "samples", "plus", "plus_from")))
        do.call(session$setInputs, base)
        do.call(session$setInputs,
                stats::setNames(list(1), paste0(pf, "_w_apply")))
        session$flushReact()
        g <- gate_reg[[paste0(pf, "_w")]]; sg <- g$sig()
        do.call(session$setInputs,
                stats::setNames(list(TRUE), paste0(pf, "_w_samples")))
        session$flushReact()
        ob[[paste0(pf, "_stale")]] <- !identical(g$sig(), sg) && isTRUE(g$stale())
        do.call(session$setInputs,
                stats::setNames(list(2), paste0(pf, "_w_apply")))
        session$flushReact()
        v <- tryCatch({ force(output[[paste0(pf, "_w_plot")]]); "rendered" },
                      error = function(e) paste("ERROR:", conditionMessage(e)))
        ob[[paste0(pf, "_bars_render")]] <- identical(v, "rendered")
        do.call(session$setInputs,
                stats::setNames(list(1), paste0(pf, "_w_plot_keep")))
        session$flushReact()
        kt <- kept_text()
        env <- list2env(list(catch_for_biology = explore_dat(paste0(pf, "_w")),
                             distributions = rv$dist), parent = environment())
        pr <- tryCatch(eval(parse(text = kt), env), error = function(e) NULL)
        ob[[paste0(pf, "_bars_script")]] <- grepl("show_samples = TRUE", kt, fixed = TRUE) &&
          inherits(pr, "ggplot") && inherits(pr$layers[[1]]$geom, "GeomCol")
        ## a length tab has no plus group: even with the inputs set, none is written
        if (identical(pf, "len"))
          ob$len_no_plus <- !grepl("pool_plus_group", kt, fixed = TRUE)
      }
      ## the age tab: a plus group from age 3
      do.call(session$setInputs, list(age_w_plus = TRUE, age_w_plus_from = 3))
      session$flushReact()
      session$setInputs(age_w_apply = 3); session$flushReact()
      v <- tryCatch({ force(output[["age_w_plot"]]); "rendered" },
                    error = function(e) paste("ERROR:", conditionMessage(e)))
      ob$age_plus_render <- identical(v, "rendered")
      session$setInputs(age_w_plot_keep = 2); session$flushReact()
      kt <- kept_text()
      env <- list2env(list(catch_for_biology = explore_dat("age_w"),
                           distributions = rv$dist), parent = environment())
      pr <- tryCatch(eval(parse(text = kt), env), error = function(e) NULL)
      ob$age_plus_script <- grepl("pool_plus_group(linked, 3, dims_biology)", kt, fixed = TRUE) &&
        inherits(pr, "ggplot") && max(pr$data$.class) == 3 &&
        "3+" %in% x_labels(pr) && inherits(pr$layers[[1]]$geom, "GeomCol")
      ## a plus group that no age reaches is said so
      session$setInputs(age_w_plus_from = 50, age_w_apply = 4); session$flushReact()
      e50 <- tryCatch({ force(output[["age_w_plot"]]); "no error" },
                      error = function(e) conditionMessage(e))
      ob$age_plus_none <- grepl("No age reaches 50", e50, fixed = TRUE)
    })
    TRUE
  }, error = function(e) conditionMessage(e))
  say(isTRUE(res5), "the new plot options run through the app", res5)
  if (isTRUE(res5)) {
    say(isTRUE(ob$dr_stale), "discard rate: changing rate / reference makes the panel stale")
    say(isTRUE(ob$dr_before), "discard rate: the default is the ratio, with no reference line")
    say(isTRUE(ob$dr_after),
        "discard rate: the rate is plotted and 20% becomes a reference line at 20")
    say(isTRUE(ob$dr_live), "discard rate: the live script shows both options")
    say(isTRUE(ob$dr_script), "discard rate: the kept script carries both options")
    say(isTRUE(ob$dr_repro), "discard rate: the kept script reproduces the plot")
    say(isTRUE(ob$dr_bad), "discard rate: a reference the rate cannot reach is refused")
    say(isTRUE(ob$dr_plain), "discard rate: the defaults write nothing extra")
    say(isTRUE(ob$cov_heat_render), "discard coverage: the live heatmap still renders (min_fish default fix)")
    say(isTRUE(ob$cov_bubble_render), "discard coverage: the live bubble grid still renders (min_fish default fix)")
    for (pf in c("len", "age")) {
      say(isTRUE(ob[[paste0(pf, "_stale")]]),
          sprintf("%s mean weight: ticking the bars makes the panel stale", pf))
      say(isTRUE(ob[[paste0(pf, "_bars_render")]]),
          sprintf("%s mean weight: the plot with bars renders", pf))
      say(isTRUE(ob[[paste0(pf, "_bars_script")]]),
          sprintf("%s mean weight: the kept script draws the bars", pf))
    }
    say(isTRUE(ob$len_no_plus),
        "length mean weight: no plus group is written, ages only")
    say(isTRUE(ob$age_plus_render), "age mean weight: the plus group renders")
    say(isTRUE(ob$age_plus_script),
        "age mean weight: the kept script pools the ages and labels the class 3+")
    say(isTRUE(ob$age_plus_none),
        "age mean weight: a plus group no age reaches is said so, not drawn empty")
  }
}

cat(sprintf("\nDONE  %d failure(s)\n", fail))
if (fail) quit(status = 1L)
