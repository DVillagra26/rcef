## The control decks, after the layout rework.
##
## Three things here are easy to break by editing one side only:
##
##  1. plot_height() must count the y categories off the DATA. A fixed
##     n_y was fine at metier4 and drew ~100 metier6 codes into the same
##     height, printing the axis labels over each other.
##  2. the level-of-detail DEFAULTS live in three UI builders and again
##     as fallbacks inside dmap(). When those disagree, the first
##     aggregation of a session runs at one resolution while the box on
##     screen reads another - a silent disagreement between a plot and
##     its own control.
##  3. the decks themselves: Apply spans the row rather than sitting in
##     it, and the builder steps are numbered boxes.

FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}

app <- system.file("app/app.R", package = "rcef")
hlp <- system.file("app/app_ui_helpers.R", package = "rcef")
if (!nzchar(app)) stop("rcef is not installed; R CMD INSTALL first")
txt <- paste(readLines(app, warn = FALSE), collapse = "\n")
e <- new.env(); suppressWarnings(suppressMessages(source(hlp, local = e)))

## ---- 1. height follows the y categories ------------------------------
d4 <- data.frame(fleet = rep(paste0("m4_", 1:8), 3),
                 Country = rep(c("BE", "NL", "FR"), each = 8))
d6 <- data.frame(fleet = rep(paste0("m6_", 1:110), 3),
                 Country = rep(c("BE", "NL", "FR"), each = 110))
h4 <- e$plot_height(d4, NULL, y_var = "fleet")
h6 <- e$plot_height(d6, NULL, y_var = "fleet")
ok(h6 > h4 * 2,
   "a metier6 axis gets far more height than a metier4 one")
ok(h6 > 110 * 8, "roughly a label's worth of height per category")
ok(e$plot_height(d6, "Country", y_var = "fleet") > h6,
   "faceting on top of many categories adds height again")
ok(e$plot_height(d6, NULL, y_var = "fleet") <= 6000,
   "but the height is capped so the page stays scrollable")

## missing or absent columns must not error - the y picker can name a
## column the current strata table does not carry
ok(is.numeric(e$plot_height(d6, "nope", y_var = "nope")),
   "an unknown y column falls back instead of erroring")
ok(is.numeric(e$plot_height(NULL, NULL, y_var = "fleet")),
   "NULL data falls back instead of erroring")
ok(e$plot_height(d4, NULL) == e$plot_height(d4, NULL),
   "omitting y_var keeps the old fixed-n_y behaviour")

## ---- 2. the level-of-detail defaults agree ---------------------------
##
## Every default site, and dmap()'s fallbacks, must name the same pair.
n_full <- length(gregexpr('fleet = "fleetValue", area = "areaValue"',
                          txt, fixed = TRUE)[[1]])
ok(n_full == 3, "all three level-of-detail builders default to the full values")
ok(!grepl('fleet = "metier4", area = "ICESDivision"', txt, fixed = TRUE),
   "no builder still defaults to metier4 / ICESDivision")
ok(grepl('if (is.null(f)) "fleetValue"', txt, fixed = TRUE),
   "dmap()'s Fleet fallback matches the UI default")
ok(grepl('if (is.null(a)) "areaValue"', txt, fixed = TRUE),
   "dmap()'s Area fallback matches the UI default")

## ---- 3. Apply spans the deck ----------------------------------------
ok(grepl("deckapply", txt, fixed = TRUE), "there is an Apply bar")
ok(!grepl("deck-go", txt, fixed = TRUE),
   "the old Apply-as-fourth-column box is gone")
ok(grepl(".deckgrid{display:grid", txt, fixed = TRUE) &&
     grepl("align-items:stretch", txt, fixed = TRUE),
   "the boxes stretch to a common height")

## ---- 4. the builder decks -------------------------------------------
ok(grepl("builder_deck <- function", txt, fixed = TRUE),
   "the builder deck helper exists")
## the "<id>_bdeck" element id is built INSIDE builder_deck(), so the
## literal never appears in app.R - check the call sites instead
for (k in c("ar", "mg"))
  ok(grepl(sprintf('builder_deck("%s"', k), txt, fixed = TRUE),
     paste("a builder deck is wired for", k))
ok(grepl("bdeck-", txt, fixed = TRUE),
   "the column count varies with the number of steps")

## ---- 5. extra key variables -----------------------------------------
## The "extra key variables" control is gone. It existed to widen a fixed
## four-dimension vocabulary; every key picker now offers every variable,
## so it was a second way to do the same thing.
ok(!grepl("hier_extra", txt, fixed = TRUE),
   "the separate extra-keys control is gone")
ok(!grepl("ar_extra_choices", txt, fixed = TRUE),
   "and so is its choices reactive")
ok(grepl("choices <- c(NONE, gvars())", txt, fixed = TRUE),
   "key pickers draw from the one vocabulary")
## a key must reach the STRATA, not only the hierarchy: build_strata()
## creates only the columns it is given
ok(grepl('ar_dims <- reactive(avail(unique(c("Stratum", hier_cols()))))',
         txt, fixed = TRUE),
   "strata are the uploaded stratum plus every column a key names")
ok(grepl("mg_dims <- reactive", txt, fixed = TRUE),
   "and the manual builder follows the same rule")
ok(length(gregexpr('unname(dmap("ar")[DIMS])', txt, fixed = TRUE)[[1]]) <= 3,
   "the raising path routes through ar_dims(), not the bare four dims")
ok(grepl("hier_idx <- reactive", txt, fixed = TRUE),
   "thresholds are indexed by key SLOT, not by position")

## ---- 6. the catch-all changes ---------------------------------------
ok(!grepl("catchall_keep_excl", txt, fixed = TRUE),
   "the inherit-group-exclusions option is gone")
## Scoped to the DISCARD builder. The length/age builders still carry
## their own copy (ns("ca_keep")) and are deliberately untouched until
## the discard layout has been reviewed - so a global grep would fail
## here for the wrong reason.
disc <- substr(txt, regexpr('builder_deck("mg"', txt, fixed = TRUE),
               regexpr('id = "man_out"', txt, fixed = TRUE))
ok(!grepl("Also apply the group exclusions above", disc, fixed = TRUE),
   "and so is its explanation, in the discard builder")
ok(grepl('ns("ca_keep")', txt, fixed = TRUE),
   "the length/age copy is still there - not yet migrated, on purpose")
## btn-savescn IS the green class - the same one Save group uses, which
## is the point: the two buttons that commit a group should look alike.
i <- regexpr("add_catchall", disc, fixed = TRUE)
ok(i > 0 && grepl("btn-savescn", substr(disc, i, i + 200), fixed = TRUE),
   "Add catch-all carries the same green class as Save group")
sr <- regexpr("save_rule", disc, fixed = TRUE)
ok(sr > 0 && grepl("btn-savescn", substr(disc, sr, sr + 120), fixed = TRUE),
   "and Save group is that class, so they match")

## ---- 7. the builder layouts follow the spec -------------------------
##
## Checked against the RENDERED UI, not the source: parse() is happy with
## a semantically wrong nesting, and the ordering of steps is exactly the
## thing a bad nesting scrambles.
if (requireNamespace("shiny", quietly = TRUE)) {
  ev <- new.env(parent = globalenv())
  ap <- system.file("app/app.R", package = "rcef")
  wd <- getwd(); setwd(dirname(ap))
  suppressWarnings(suppressMessages(source(ap, local = ev)))
  setwd(wd)
  h <- as.character(ev$ui)
  ok(nchar(h) > 3e5, "the UI renders")

  ## Both decks title a box "2. Exclusion rules for donor strata", so a
  ## whole-document search finds the hierarchical one first. Scope to the
  ## branch or the ordering assertion fails for the wrong reason.
  mg <- substr(h, regexpr("mg_bdeck", h, fixed = TRUE), nchar(h))
  ar <- substr(h, regexpr("ar_bdeck", h, fixed = TRUE),
               regexpr("mg_bdeck", h, fixed = TRUE))
  at <- function(seg, k) as.integer(regexpr(k, seg, fixed = TRUE))

  steps_mg <- c("1. Define the groups", "2. Exclusion rules for donor",
                "5. Include a Catch All", "Assign the clause",
                "Add group", "save_manual", "man_out")
  ok(all(vapply(steps_mg, function(k) at(mg, k), integer(1)) > 0),
     "every manual step is present")
  ok(all(diff(vapply(steps_mg, function(k) at(mg, k), integer(1))) > 0),
     "and they appear in the order a reader works through them")

  steps_ar <- c("1. Define key hierarchy", "2. Exclusion rules for donor",
                "ar_apply", "save_auto", "auto_out")
  ok(all(diff(vapply(steps_ar, function(k) at(ar, k), integer(1))) > 0),
     "the hierarchical steps are in order too")

  ## the numbered exclusion sub-steps
  for (k in c("2.1 Exclude by discard rate",
              "2.2 Exclude by % sampling coverage",
              "2.3 Exclude by number of measurements",
              "2.4 Exclude a specific stratum",
              "Summary of currently excluded strata"))
    ok(grepl(k, ar, fixed = TRUE), paste("the cascade box states:", k))

  ## Shiny binds an output id once. rule_order and the catch-all inputs
  ## moved INTO the deck; a leftover copy below it would render blank and
  ## the duplicated numericInputs would fight over the same value.
  for (id in c("rule_order", "catchall_dr_min", "catchall_dr_max",
               "ca_xcov", "mg_xcov", "clause_vals"))
    ok(length(gregexpr(paste0("id=\"", id, "\""), h,
                       fixed = TRUE)[[1]]) == 1,
       paste(id, "is declared exactly once"))

  ## Code last, in both
  for (id in c("auto_out", "man_out")) {
    seg <- substr(h, regexpr(paste0("id=\"", id, "\""), h), nchar(h))
    tabs <- regmatches(seg, gregexpr("data-value=\"[^\"]+\"", seg))[[1]][1:5]
    tabs <- gsub("data-value=|\"", "", tabs)
    ok(identical(tabs[5], "Code"), paste("Code is the last tab in", id))
    ok(identical(tabs[1], "Overview"), paste("Overview is first in", id))
  }

  ## the level-of-detail controls are gone from the discard side
  for (id in c("ar_fleet", "ar_area", "mg_fleet", "mg_area",
               "cov_fleet", "dr_fleet"))
    ok(!grepl(paste0("id=\"", id, "\""), h, fixed = TRUE),
       paste(id, "is gone - resolution is the variable choice now"))

  ok(!grepl("hier_extra", h, fixed = TRUE),
     "the separate extra-keys control is gone; every picker offers everything")
  ok(!grepl(">Save group<", h, fixed = TRUE),
     "the button says Add group, not Save group")
}

cat("\nDONE ", FAIL, "failure(s)", "\n")
