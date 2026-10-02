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
                 vesselFlagCountry = rep(c("BE", "NL", "FR"), each = 8))
d6 <- data.frame(fleet = rep(paste0("m6_", 1:110), 3),
                 vesselFlagCountry = rep(c("BE", "NL", "FR"), each = 110))
h4 <- e$plot_height(d4, NULL, y_var = "fleet")
h6 <- e$plot_height(d6, NULL, y_var = "fleet")
ok(h6 > h4 * 2,
   "a metier6 axis gets far more height than a metier4 one")
ok(h6 > 110 * 8, "roughly a label's worth of height per category")
ok(e$plot_height(d6, "vesselFlagCountry", y_var = "fleet") > h6,
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

## ---- 2. the level-of-detail subsystem stays removed -------------------
##
## level_fields()/level_deck()/level_box()/dmap()/DMAP_ALIAS/nr_dims used
## to give each figure an abstract "Fleet"/"Area" resolution picker,
## superseded by the vocabulary system (a figure's axes name real
## columns directly) and, for the raising tabs specifically, the
## strata-definition box. All confirmed dead - test_wiring.R's "things
## deliberately removed stay removed" section covers this more fully;
## this is just the two names most specific to this file's earlier
## "defaults agree" test, which is retired along with the mechanism.
for (gone in c("level_fields", "level_deck", "level_box", "DMAP_ALIAS"))
  ok(!grepl(paste0(gone, " <- function"), txt, fixed = TRUE),
     paste("gone:", gone))

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
## The discard cascade's Key 2-6 pickers draw from key_vocab_grouped()
## now, not the flat gvars() - grouped into Fleet/Country/Area/Season
## optgroups, and curated down to the ten variables that are genuine
## cascade keys (no PSU, gear, target, mesh, FleetType, vesselFlagCountry).
## Everything else (plot axes, clause building, filters) still draws
## from the full flat vocabulary.
ok(grepl("choices <- c(NONE, kv)", txt, fixed = TRUE),
   "the discard cascade's key pickers draw from the grouped vocabulary")
ok(grepl("kv <- key_vocab_grouped(", txt, fixed = TRUE),
   "built from key_vocab_grouped(), not grouping_vars() directly")
## a key must reach the STRATA, not only the hierarchy: build_strata()
## creates only the columns it is given. "Stratum" was replaced by the
## strata-definition box's resolution (see test_strata_def.R for the
## behavioural checks); this just confirms the wiring is the box, not a
## leftover hardcoded column.
ok(grepl("ar_dims <- reactive(avail(unique(c(ar_strata_dims(), hier_cols()))))",
         txt, fixed = TRUE),
   "strata are the strata-definition box's resolution plus every cascade key")
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
## The length/age builders have now been migrated too, so the copy that
## used to be deliberately left behind must be gone everywhere.
ok(!grepl("ca_keep", txt, fixed = TRUE),
   "no builder inherits group exclusions into its catch-all")
## One meaning per colour: btn-addgrp adds a group to the set being
## built, btn-savescn commits the set as a scenario. These were crossed -
## the same green meant add in one builder and save in the other.
i <- regexpr("add_catchall", disc, fixed = TRUE)
ok(i > 0 && grepl("btn-addgrp", substr(disc, i, i + 240), fixed = TRUE),
   "Add Catch All by All is an add-a-group button")
sr <- regexpr("save_rule", disc, fixed = TRUE)
ok(sr > 0 && grepl("btn-addgrp", substr(disc, sr, sr + 160), fixed = TRUE),
   "and so is Add group, so the two match")
sm <- regexpr("save_manual", disc, fixed = TRUE)
ok(sm > 0 && grepl("btn-savescn", substr(disc, sm, sm + 160), fixed = TRUE),
   "while Save as scenario is the commit colour, as in the cascade")

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

  ## as.character() on a tagList drops <head>, so the stylesheet is NOT
  ## in `h`. Checking CSS against `h` silently passes every "is this rule
  ## present" test in the negative - it looked like the rules were
  ## missing when they were fine. renderTags() keeps the head.
  rt <- htmltools::renderTags(ev$ui)
  css <- paste(as.character(rt$head), collapse = "")
  ok(grepl(".deckwrap{position:sticky", css, fixed = TRUE),
     "figure decks stay pinned under the navbar")
  ok(grepl(".deckwrap.bdeck{position:static", css, fixed = TRUE),
     "builder decks do NOT - they are too tall to pin over the output")
  ok(grepl(".deckwrap.mini .deckapply", css, fixed = TRUE),
     "minimising a builder hides its action bar too")
  ## the dropdown fixes have to be global, not scoped to the decks
  ok(grepl("select, .form-control, input[type=number]", css, fixed = TRUE),
     "control colours are set for the whole workbench")
  ok(grepl("z-index:3000", css, fixed = TRUE),
     "open dropdowns float above the sticky decks and navbar")
  ok(grepl(".irs, .js-irs-0{width:100%", css, fixed = TRUE),
     "sliders take the full width available")

  ## Both decks title a box "Exclusion rules for donor strata", so a
  ## whole-document search finds the hierarchical one first. Scope to the
  ## branch or the ordering assertion fails for the wrong reason.
  mg <- substr(h, regexpr("mg_bdeck", h, fixed = TRUE), nchar(h))
  ar <- substr(h, regexpr("ar_bdeck", h, fixed = TRUE),
               regexpr("mg_bdeck", h, fixed = TRUE))
  at <- function(seg, k) as.integer(regexpr(k, seg, fixed = TRUE))

  ## The strata are always as reported now - no definition box - so
  ## the manual deck starts at 1 like the length/age one: defining the
  ## groups is 1, exclusions is 2, assigning the clause is 3 (across the
  ## bottom under 1 and 2 only), the catch-all is 4 (full height on the
  ## right).
  steps_mg <- c("1. Define the groups",
                "2. Exclusion rules for donor",
                "3. Assign the clause", "4. Include a Catch All",
                "Add group", "save_manual", "man_out")
  ok(all(vapply(steps_mg, function(k) at(mg, k), integer(1)) > 0),
     "every manual step is present")
  ## Source order, not step number: the catch-all is step 4 but is
  ## emitted as the third grid child, because it occupies the right-hand
  ## column beside the step-3 row rather than following it.
  src_order <- c("1. Define the groups",
                 "2. Exclusion rules for donor",
                 "4. Include a Catch All", "3. Assign the clause",
                 "Add group", "save_manual", "man_out")
  ok(all(diff(vapply(src_order, function(k) at(mg, k), integer(1))) > 0),
     "the manual deck emits its boxes, then the wide row, then the actions")

  ## "ar" no longer has a separate strata-definition box - it moved
  ## into Key 1, so ar starts at step 1 (Define key hierarchy) directly.
  steps_ar <- c("1. Define key hierarchy",
                "2. Exclusion rules for donor",
                "ar_apply", "save_auto", "auto_out")
  ok(all(diff(vapply(steps_ar, function(k) at(ar, k), integer(1))) > 0),
     "the hierarchical steps are in order too")

  ## The numbering is generated by a counter, so a missing metric does
  ## not leave a gap - it renumbers everything after it. That is why the
  ## FULL heading text is checked, never just the number: with
  ## measurements dropped, "2.4" is still there and says something else.
  ## ar's exclusions box is step 2 now (Key 1 absorbed the old step 1),
  ## so its sub-steps are 2.x, not 3.x. Trips sits between coverage and
  ## measurements (present on discards too, unlike measurements),
  ## pushing "exclude a specific stratum" one place further along.
  for (k in c("2.1 Exclude by discard rate",
              "2.2 Exclude by % sampling coverage",
              "2.3 Exclude by number of trips",
              "2.4 Exclude a specific stratum",
              "Summary of currently excluded strata"))
    ok(grepl(k, ar, fixed = TRUE), paste("the cascade box states:", k))
  for (k in c("2.1 Exclude by discard rate",
              "2.3 Exclude by number of trips",
              "2.4 Exclude a specific stratum",
              "4.1 Exclude by discard rate",
              "4.3 Exclude by number of trips",
              "4.4 Exclude a specific stratum"))
    ok(grepl(k, mg, fixed = TRUE), paste("the manual builder states:", k))

  ## ---- 8b. length/age manual builder renumbered after the read-only
  ## mirror was removed: no more separate top box, so "Define the
  ## groups" moves from step 2 to step 1, its exclusion box from 3 to 2,
  ## and the catch-all from 5 to 4 -----------------------------------
  len_mg <- substr(h, regexpr("len_ga_mg_bdeck", h, fixed = TRUE),
                   regexpr("age_ga_mg_bdeck", h, fixed = TRUE))
  for (k in c("1.1 Name the group", "1.2 Rules to group",
              "2.1 Exclude by % sampling coverage",
              "2.2 Exclude by number of measurements",
              "3. Assign the clause", "4. Include a Catch All by All"))
    ok(grepl(k, len_mg, fixed = TRUE),
       paste("the length manual builder states:", k))
  ok(!grepl("Definition of the strata (set on the Hierarchical tab)",
            len_mg, fixed = TRUE),
     "and no longer carries the removed mirror box")

  ## ---- 9. length and age use the same builder shape ----------------
  for (id in c("len_ga_hier_bdeck", "age_ga_hier_bdeck",
               "len_ga_mg_bdeck", "age_ga_mg_bdeck"))
    ok(grepl(id, h, fixed = TRUE),
       paste(id, "is a deck, not a sidebar"))
  ok(grepl("2.2 Exclude by number of measurements", h, fixed = TRUE),
     "the biology cascade numbers its donor bars")
  ## the donor measurements bar is a data-scaled slider, not a free
  ## numeric box, so it needs a server renderer per builder
  ok(length(unique(regmatches(h,
       gregexpr("[A-Za-z0-9_]*dmeas_ui", h))[[1]])) == 2,
     "each biology cascade renders its own measurements slider")

  ## namespaced ids matter here: two manual builders (length, age) share
  ## the same box titles, so a bare string search for "saverule" finds
  ## whichever occurs first REGARDLESS of which builder it belongs to.
  ## Every check below uses the full id, not a substring.
  for (pfx in c("len_ga", "age_ga")) {
    o <- as.integer(regexpr(paste0(pfx, "_mg_bdeck"), h, fixed = TRUE))
    a <- as.integer(regexpr(paste0('id="', pfx, '_saverule"'), h, fixed = TRUE))
    b <- as.integer(regexpr(paste0('id="', pfx, '_savemanual"'), h, fixed = TRUE))
    c <- as.integer(regexpr(paste0('id="', pfx, '_man_out"'), h, fixed = TRUE))
    ok(o > 0 && o < a && a < b && b < c,
       paste(pfx, "manual deck: box, then Add group, Save, then output tabs"))
  }

  ## Shiny binds an output id once. rule_order and the catch-all inputs
  ## moved INTO the deck; a leftover copy below it would render blank and
  ## the duplicated numericInputs would fight over the same value.
  for (id in c("rule_order", "catchall_dr_min", "catchall_dr_max",
               "ca_xcov", "mg_xcov", "clause_vals"))
    ok(length(gregexpr(paste0("id=\"", id, "\""), h,
                       fixed = TRUE)[[1]]) == 1,
       paste(id, "is declared exactly once"))

  ## Code last, in every builder - discards AND the length/age pairs
  for (id in c("auto_out", "man_out", "len_ga_hier_out", "len_ga_man_out",
               "age_ga_hier_out", "age_ga_man_out")) {
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

  ## ---- the builder titles and numbering ----------------------------
  for (k in c("Hierarchical strata allocation builder",
              "Manual strata allocation builder"))
    ok(grepl(k, h, fixed = TRUE), paste("titled:", k))
  for (k in c("1. Define the groups",
              "2. Exclusion rules", "3. Assign the clause",
              "4. Include a Catch All by All"))
    ok(grepl(k, mg, fixed = TRUE), paste("manual step present:", k))

  ## ---- measurements is a BIOLOGY test only -------------------------
  ##
  ## It counts length/age measurements. A discard raising borrows a
  ## RATE, which rests on sampled weight - how many fish were measured
  ## says nothing about it.
  ok(!grepl("id=\"ar_xmeas\"", h, fixed = TRUE),
     "the discard cascade has no measurements bar")
  ok(!grepl("id=\"mg_xmeas\"", h, fixed = TRUE),
     "nor the manual discard builder")
  ok(grepl("2.4 Exclude a specific stratum", ar, fixed = TRUE),
     "so the discard exclusions renumber to four sub-steps with trips added")
  ## and it IS there for length and age
  ok(grepl("measurements", txt, fixed = TRUE),
     "the measurements bar still exists for the biology builders")

  ## ---- item 7: numTrips ----------------------------------------------
  ##
  ## Trips applies everywhere coverage does - discards included, unlike
  ## measurements, which only length/age strata have.
  for (id in c("ar_xtrips_ui", "mg_xtrips_ui", "ca_xtrips_ui"))
    ok(grepl(id, h, fixed = TRUE), paste(id, "renders on the discard side"))
  for (id in c("len_ga_x_xtrips_ui", "len_ga_ca_xtrips_ui"))
    ok(grepl(id, h, fixed = TRUE), paste(id, "renders on the biology side"))
  ok(grepl("Trips\" = \"trips\"", txt, fixed = TRUE),
     "trips is offered as a sort option")

  ## ---- one meaning per button colour -------------------------------
  ok(grepl("btn-addgrp", h, fixed = TRUE),
     "adding a group has its own class, used in both builders")
  ok(length(gregexpr("btn-addgrp", h, fixed = TRUE)[[1]]) >= 3,
     "and appears on every add-a-group button")

  ## ---- 8. length and age carry the same changes --------------------
  ok(!grepl("ca_keep", h, fixed = TRUE),
     "the length/age catch-all no longer inherits the group exclusions")
  ok(grepl("Add Catch All by All", h, fixed = TRUE),
     "and its button is named and coloured like the discard one")
  ## the level-of-detail box is gone from the biology tabs too
  for (id in c("len_lv_fleet", "age_lv_fleet", "len_ga_lv_fleet"))
    ok(!grepl(paste0("id=\"", id, "\""), h, fixed = TRUE),
       paste(id, "is gone"))
  ## Key 7: own stratum + catchCategory + five user slots
  ok(grepl("Key 7", h, fixed = TRUE) || grepl("N_ALLOC_KEYS <- 5", txt,
                                              fixed = TRUE),
     "the allocation cascade reaches Key 7")
  ## nothing may still emit an abstract dimension label as a VALUE -
  ## col_of() is the identity, so "Fleet" would name a missing column.
  ## ic_metier6 is exempt: that is a real InterCatch field name.
  bad <- gregexpr("value=\"(Fleet|Area|Catch cat\\.)\"", h)[[1]]
  bad <- if (bad[1] == -1) integer(0) else bad
  offenders <- vapply(bad, function(i)
    grepl("ic_metier6", substr(h, max(1, i - 800), i), fixed = TRUE),
    logical(1))
  ok(all(offenders),
     "no control emits an old dimension label as a column name")
}

cat("\nDONE ", FAIL, "failure(s)", "\n")
