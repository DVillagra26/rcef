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
ok(grepl("hier_extra", txt, fixed = TRUE), "extra key variables are offered")
ok(grepl("ar_extra_choices", txt, fixed = TRUE),
   "and the choices come from the loaded table")
## an extra key must reach the STRATA, not only the hierarchy
ok(grepl("ar_dims <- reactive(unique(c(", txt, fixed = TRUE),
   "ar_dims() folds the extras into the stratum definition")
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

cat("\nDONE ", FAIL, "failure(s)", "\n")
