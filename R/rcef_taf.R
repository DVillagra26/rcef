# =====================================================================
#  rcef_taf.R   (the same session, emitted as an ICES TAF script set)
#
#  ORIGIN: this project (ILVO / RCEF Workbench). Freely refactorable.
#  Was: R_development/0_Functions_taf.R
#  No shiny in this file. If it needs `input`, `session` or a tag,
#  it belongs in app/, not here.
# =====================================================================

# ============================================================
#  0_Functions_taf.R
#  Emit the session as an ICES TAF script set.
# ------------------------------------------------------------
#  0_Functions_emit.R and session_log.R produce ONE running script: a
#  transcript of the analysis, top to bottom, meant to be read. That is the
#  right shape for checking what happened, and the wrong shape for handing an
#  assessment over. TAF (Transparent Assessment Framework) expects the work
#  split into stages that each read from disk and write to disk, so any stage
#  can be re-run on its own and the intermediate objects are inspectable
#  between them.
#
#  This module writes that second shape. Every function here returns a
#  CHARACTER VECTOR of R code; nothing is evaluated. The app assembles them
#  on the "R script" tab and offers them for download singly or as a set.
#
#  THE SEVEN FILES
#    data.R                            read and wrangle; nothing is decided
#    model_explore_discards.R          discard coverage and discard rates
#    model_allocate_raise_discards.R   group definition + catchRaising()
#    model_explore_length_age.R        length/age sampling coverage
#    model_allocate_length_age.R       group definition + catchAllocationAoL()
#    output.R                          every plot the user chose to keep
#    report.R                          placeholder
#
#  The four model scripts are the "model" stage of TAF, split because the
#  discard raising and the length/age allocation are two different decisions
#  taken at two different times, and because in each case the exploration
#  that JUSTIFIES the grouping is worth keeping apart from the grouping
#  itself. A reviewer who only wants to know why a group was drawn the way it
#  was reads the explore script and stops there.
#
#  ORDER MATTERS AND IS ENFORCED BY THE FILES ON DISK
#  model_allocate_length_age.R reads model/catch_data_raised.rds, not
#  data/catch_data.rds: lengths and ages are allocated over the RAISED
#  catches, so the discard step has to have run first. Making that a file
#  dependency rather than a comment means running them out of order fails
#  loudly instead of quietly allocating over unraised landings.
#
#  DEPENDENCIES  Imports: none beyond base R (string building only).
#                Uses fmt_vals() and the emitters in 0_Functions_emit.R,
#                mod_length.R and mod_alloc_raising.R.
# ============================================================

# ----------------------------------------------------------
#  The file set
# ----------------------------------------------------------

#' The standard header comment block for a TAF script
#'
#' Every one of the seven files opens with the same shape: what the
#' script is for, in one line, then what it reads and what it writes, so
#' a reader (or the working group, a year later) can tell what feeds
#' what without opening every file.
#'
#' Called seven times across this file and, until now, never defined -
#' a gap invisible to `check_all.sh`, since nothing in the active test
#' suite exercises the TAF emitters (only `dev/tests/legacy/`, which is
#' not wired into the harness, ever called them). `roxygenise()`'s
#' namespace audit is what surfaced it: `taf_header` was listed as an
#' export with no function behind it.
#'
#' @param script The script's TAF name, without the extension, e.g.
#'   `"data"` or `"model_explore_discards"`.
#' @param purpose One-line description of what the script does.
#' @param reads,writes What the script reads from and writes to disk,
#'   each a character vector of paths or descriptions.
#' @param notes Further lines of commentary, one bullet each.
#' @return A character vector of R code (comment lines).
#' @export
taf_header <- function(script, purpose, reads = character(0),
                       writes = character(0), notes = character(0)) {
  bullet <- function(label, x)
    if (!length(x)) NULL else c(sprintf("##   %s:", label),
                                sprintf("##     %s", x))
  c("## ===========================================================",
    sprintf("## %s.R", script),
    sprintf("## %s", purpose),
    "## ===========================================================",
    bullet("Reads", reads),
    bullet("Writes", writes),
    if (length(notes)) c("##", sprintf("## %s", notes)),
    "")
}

#' The TAF script set: part id -> file name
#'
#' Order is the order the scripts must be run in, which is also the order
#' they are shown and concatenated in.
#' @export
TAF_FILES <- c(
  data                          = "data.R",
  model_explore_discards        = "model_explore_discards.R",
  model_allocate_raise_discards = "model_allocate_raise_discards.R",
  model_explore_length_age      = "model_explore_length_age.R",
  model_allocate_length_age     = "model_allocate_length_age.R",
  output                        = "output.R",
  report                        = "report.R")

#' The four ways into the workflow, and what to call them
#'
#' Exactly one owns the data at a time. The app enforces it; this is the
#' wording both the warning dialog and the emitted header use, so they
#' cannot describe the same route differently.
#' @export
TAF_METHOD_LABEL <- c(
  ic_exchange = "InterCatch exchange format (HI / SI / SD)",
  ic_output   = "InterCatch output format (StockOverview / NumbersAtAge...)",
  cef         = "CEF files (catches / distributions / effort)",
  rdbes       = "RDBES / CEF database")

#' Short human labels, used for the sub-tabs
#' @export
TAF_LABEL <- c(
  data                          = "data.R",
  model_explore_discards        = "1. Explore discards",
  model_allocate_raise_discards = "2. Allocate + raise discards",
  model_explore_length_age      = "3. Explore length / age",
  model_allocate_length_age     = "4. Allocate length / age",
  output                        = "output.R",
  report                        = "report.R")

#' The library block every TAF script opens with
#' @param extra Extra package names.
#' @return A character vector of R code.
#' @export
taf_libs <- function(extra = character(0)) {
  base <- c("dplyr", "tidyr", "ggplot2", "rlang")
  c("library(dplyr); library(tidyr); library(ggplot2); library(rlang)",
    if (length(setdiff(extra, base)))
      sprintf("library(%s)", setdiff(extra, base)),
    "")
}

#' Create the TAF stage directory a script writes into
#' @param stage Directory name.
#' @return A character vector of R code.
#' @export
taf_mkdir <- function(stage)
  c(sprintf('if (!dir.exists("%s")) dir.create("%s", recursive = TRUE)',
            stage, stage), "")

#' The library block that loads the workflow functions
#'
#' Was a block of `source()` calls into a folder of loose scripts. The
#' engines are vendored in the package now, so it is one `library()`.
#' @param external_dir Ignored; kept so old callers do not break.
#' @return A character vector of R code.
#' @export
taf_sources <- function(external_dir = NULL) c(
  "## ---- Workflow functions ----------------------------------",
  "## catchRaising(), catchAllocationAoL() and the CEF readers are not on",
  "## CRAN. They are vendored in the rcef package (R/RDBESStockCoord_*.R)",
  "## together with the workbench functions, so one library() call is",
  "## enough and the script runs outside Shiny unchanged.",
  "##",
  "##   remotes::install_github(\"DVillagra26/rcef\")",
  "library(rcef)",
  "")

#' The one line that derives every field the workflow needs
#'
#' Earlier versions wrote a `.ices_lvl()` helper into the script and then a
#' twelve-line `mutate()` using it. That was a mistake twice over: it
#' duplicated a function the package already exports, and it meant a fix to
#' the area splitting had to be made in two places that could silently
#' disagree. The script sources the package, so it should call the package.
#'
#' @return A character vector of R code.
#' @export
taf_derive <- function() c(
  "## ---- Derived fields ---------------------------------------",
  "## derive_fields() adds, from fleetValue and areaValue:",
  "##   metier4 / metier5 / metier6, target, mesh, metier4_mesh, Stratum",
  "##   ICESSubArea (27.X), ICESDivision (27.X.X), ICESSubdivision",
  "## The ICES area split is ices_area_levels(); a code already coarser",
  "## than the level asked for repeats rather than becoming NA, because",
  "## 27.4 IS its own division as far as the data goes.",
  "catch_data <- derive_fields(catch_data)",
  "")

#' The `save_plot()` helper every script that draws a figure needs
#'
#' Figures are written to `output/` wherever they are drawn from. The plots
#' themselves now live beside the analysis that motivated them - a coverage
#' plot belongs next to the coverage it describes - but a reader still wants
#' every figure in one folder, so the destination stays fixed while the call
#' site moves.
#'
#' @return A character vector of R code.
#' @export
taf_save_plot_helper <- function() c(
  "## ---- Figures ----------------------------------------------",
  "## Drawn here, beside the analysis they belong to; written to output/,",
  "## because that is where a reader expects to find every figure.",
  'if (!dir.exists("output")) dir.create("output", recursive = TRUE)',
  "save_plot <- function(p, file, width = 10, height = 7) {",
  "    if (is.null(p)) return(invisible(NULL))",
  '    ggsave(file.path("output", file), p, width = width, height = height,',
  '           dpi = 300, bg = "white")',
  "    invisible(p)",
  "}",
  "")

#' Emit the kept plots belonging to one script
#'
#' Numbering is GLOBAL across the whole set, not per script: every figure
#' lands in the same `output/` folder, so `01_...png` must mean one thing.
#'
#' @param kept The full list of kept-plot records.
#' @param section Which script's plots to emit.
#' @return A character vector of R code, empty when the section kept none.
#' @export
taf_plot_blocks <- function(kept, section) {
  if (!length(kept)) return(character(0))
  keep_i <- which(vapply(kept, function(k)
    identical(k$section %||% "output", section), logical(1)))
  if (!length(keep_i)) return(character(0))
  c(taf_save_plot_helper(),
    unlist(lapply(keep_i, function(i) {
      k <- kept[[i]]
      obj <- sprintf("p%02d", i)
      file <- sprintf("%02d_%s.png", i, taf_slug(k$title))
      c(sprintf("## ---- Figure %s: %s %s", i, k$title,
                strrep("-", max(4, 46 - nchar(k$title)))),
        if (length(k$note)) paste0("## ", k$note) else NULL,
        sprintf("## kept at %s", k$stamp %||% "?"),
        if (length(k$prep)) k$prep else NULL,
        sprintf("%s <- %s", obj, k$call[1]),
        if (length(k$call) > 1) paste0("      ", k$call[-1]) else NULL,
        sprintf('save_plot(%s, "%s")', obj, file),
        sprintf("print(%s)", obj),
        "")
    }), use.names = FALSE))
}

# ----------------------------------------------------------
#  data.R
# ----------------------------------------------------------

#' Emit data.R
#'
#' Reading and wrangling only. No threshold, no grouping and no raising
#' happens here: if a decision can change the answer it belongs in a model
#' script, so that re-running the exploration never silently re-reads a
#' different input.
#'
#' ONE ROUTE, NOT FOUR. The app offers four ways in - InterCatch exchange,
#' InterCatch output, CEF files, the RDBES database - and exactly one of them
#' owns the data at any moment. The script says so: it writes the route that
#' was actually used and nothing else. A data.R carrying all four with three
#' commented out invites the reader to guess which one ran, and a stock
#' whose provenance is a guess is a stock whose numbers cannot be defended.
#'
#' @param ctx Session context. Uses `load_method`, `load_lines`, `stock`,
#'   `year`, `external_dir`, `input_desc`.
#' @return A character vector of R code.
#' @export
taf_data <- function(ctx) {
  meth <- ctx$load_method %||% "cef"
  filt <- character(0)
  if (length(ctx$stock) && nzchar(ctx$stock %||% ""))
    filt <- c(filt, sprintf('catch_data <- catch_data %%>%% filter(stock == "%s")',
                            ctx$stock))
  if (length(ctx$year) && nzchar(as.character(ctx$year %||% "")))
    filt <- c(filt, sprintf("catch_data <- catch_data %%>%% filter(year == %s)",
                            ctx$year))

  c(taf_header("data",
      sprintf("Read the catch, distribution and effort tables (%s) and derive the fields",
              TAF_METHOD_LABEL[[meth]] %||% meth),
      reads = c(ctx$input_desc %||% "the files named below"),
      writes = c("data/catch_data.rds", "data/distributions.rds",
                 "data/effort.rds"),
      notes = c(
        sprintf("Input route: %s.", TAF_METHOD_LABEL[[meth]] %||% meth),
        "This is the only route in the script. The workbench allows exactly",
        "one at a time, so switching route in the app replaces the data and",
        "rewrites this script rather than adding a second source.",
        "",
        "Nothing here depends on a threshold, a grouping or a decision.",
        "That separation is the point of the TAF split: this script can be",
        "re-run whenever the input files change without touching any of the",
        "analytical choices made downstream.")),
    taf_libs(),
    taf_sources(ctx$external_dir %||% "."),
    taf_mkdir("data"),
    ctx$load_lines %||% c("## (no input route was completed in the session)", ""),
    taf_derive(),
    if (length(filt))
      c("## ---- Filter to the stock and year under assessment --------",
        filt, "")
    else NULL,
    "## ---- Save for the model scripts ---------------------------",
    "## Written as .rds rather than .csv so factor levels, types and NA",
    "## handling survive the hand-off between scripts unchanged.",
    'saveRDS(catch_data, "data/catch_data.rds")',
    'if (exists("distributions")) saveRDS(distributions, "data/distributions.rds")',
    'if (exists("effort")) saveRDS(effort, "data/effort.rds")',
    "")
}

# ----------------------------------------------------------
#  model_explore_discards.R
# ----------------------------------------------------------

#' Emit model_explore_discards.R
#'
#' The exploration that justifies the grouping: how much of the landings
#' carries a discard estimate, and how the discard rate varies over the
#' strata. It settles the threshold and the level of detail, and stops
#' short of drawing a single group.
#'
#' @param ctx Session context. Uses `dims_disc`, `threshold`, `dr_min`,
#'   `dr_max`.
#' @param kept Kept-plot records; those marked for this section are drawn
#'   at the end of the script.
#' @return A character vector of R code.
#' @export
taf_model_explore_discards <- function(ctx, kept = list()) {
  dims <- ctx$dims_disc %||%
    c("metier4", "vesselFlagCountry", "ICESDivision", "seasonValue")
  thr  <- ctx$threshold %||% 50
  c(taf_header("model_explore_discards",
      "Discard sampling coverage and discard rates, at the chosen resolution",
      reads = "data/catch_data.rds",
      writes = c("model/strata_discards.rds", "model/coverage_summary.csv"),
      notes = c(
        "Two decisions are taken here and nowhere else:",
        sprintf("  * the strata definition : %s", paste(dims, collapse = " + ")),
        sprintf("  * the coverage threshold: %s%%", thr),
        "Both are written as plain objects at the top so they can be changed",
        "and the script re-run, which is exactly what the app is doing when",
        "you move the slider.")),
    taf_libs(),
    taf_sources(ctx$external_dir %||% "."),
    taf_mkdir("model"),
    "## ---- Input ------------------------------------------------",
    'catch_data <- readRDS("data/catch_data.rds")',
    "",
    "## ---- The two choices --------------------------------------",
    sprintf("dims_discards <- %s", fmt_vals(dims)),
    sprintf("threshold     <- %s   ## %% of landings that must carry a discard estimate",
            thr),
    "",
    "## ---- Strata -----------------------------------------------",
    "## build_strata() sums landings, the landings that carry a discard",
    "## estimate (the numerator of coverage) and the discards, per stratum,",
    "## all converted to tonnes.",
    "strata_discards <- build_strata(catch_data, dims_discards)",
    "",
    "## ---- Classification ---------------------------------------",
    "## classify_strata() turns coverage into a verdict: raise on own data,",
    "## pool, borrow, or nothing sampled at all. Every plot, the automatic",
    "## cascade and the manual builder read the SAME classification, so the",
    "## colours on screen and the algorithm can never disagree.",
    "strata_discards <- classify_strata(strata_discards, threshold)",
    "",
    if (!is.na(ctx$dr_min %||% NA) || !is.na(ctx$dr_max %||% NA))
      c("## ---- Donor exclusion --------------------------------------",
        "## Strata with extreme discard rates make bad donors: pooling them",
        "## in drags the group rate towards an outlier. They stay in the",
        "## data and keep being raised, they are only barred from lending.",
        sprintf("strata_discards <- flag_donor_exclusion(strata_discards, %s, %s)",
                ctx$dr_min %||% "NA", ctx$dr_max %||% "NA"),
        "")
    else NULL,
    "## ---- What the exploration answers -------------------------",
    "coverage_summary <- strata_discards %>%",
    "    count(Status, Raising_Decision, name = \"n_strata\") %>%",
    "    arrange(desc(n_strata))",
    "print(coverage_summary)",
    "",
    "## how much of the stock's landings sits behind each verdict - the",
    "## count above can look reassuring while the tonnage does not",
    "coverage_by_weight <- strata_discards %>%",
    "    group_by(Status) %>%",
    "    summarise(landings_t = sum(landings, na.rm = TRUE),",
    "              share_pct  = NA_real_, .groups = \"drop\") %>%",
    "    mutate(share_pct = round(landings_t / sum(landings_t) * 100, 1))",
    "print(coverage_by_weight)",
    "",
    "## the spread of discard rates across the sampled strata: the width of",
    "## this is what says whether pooling is defensible at all",
    "discard_rate_summary <- strata_discards %>%",
    "    filter(!is.na(discard_rate)) %>%",
    "    summarise(n = n(),",
    "              min = min(discard_rate), median = median(discard_rate),",
    "              max = max(discard_rate))",
    "print(discard_rate_summary)",
    "",
    "## ---- Save -------------------------------------------------",
    'saveRDS(strata_discards, "model/strata_discards.rds")',
    'write.csv(coverage_summary, "model/coverage_summary.csv", row.names = FALSE)',
    "",
    # The coverage and discard-rate figures belong HERE, next to the numbers
    # they are a picture of, not in a distant output.R. A reviewer reading
    # why a threshold was chosen should not have to open a second file to
    # see the plot that decided it.
    taf_plot_blocks(kept, "model_explore_discards"))
}

# ----------------------------------------------------------
#  model_allocate_raise_discards.R
# ----------------------------------------------------------

#' Emit model_allocate_raise_discards.R
#'
#' The grouping and the raising. Automatic groupings are written as the RULE
#' that produced them, never as this year's group membership, so re-running
#' next year reapplies the policy instead of replaying old strata. Manual
#' groupings are the opposite case and are written out in full, because
#' there the membership IS the decision.
#'
#' @param ctx Session context. Uses `group_lines`, `raise_lines`,
#'   `compare_lines`, `final_lines`, `dims_disc`.
#' @return A character vector of R code.
#' @export
taf_model_allocate_raise_discards <- function(ctx) {
  c(taf_header("model_allocate_raise_discards",
      "Define the raising groups and run catchRaising()",
      reads = c("data/catch_data.rds", "model/strata_discards.rds"),
      writes = c("model/catch_data_raised.rds", "model/raising_comparison.csv"),
      notes = c(
        "An AUTOMATIC grouping is emitted as the rule that produced it - the",
        "hierarchy, the thresholds, the donor window - and never as the list",
        "of stratum ids this year happened to yield. Re-run on next year's",
        "data it reapplies the policy and produces next year's groups.",
        "",
        "A MANUAL grouping is emitted the other way round, in full, because",
        "there the membership is itself the decision and a rule would be a",
        "reconstruction of it.")),
    taf_libs(),
    taf_sources(ctx$external_dir %||% "."),
    taf_mkdir("model"),
    "## ---- Input ------------------------------------------------",
    'catch_data      <- readRDS("data/catch_data.rds")',
    'strata_discards <- readRDS("model/strata_discards.rds")',
    "",
    ctx$group_lines %||% c("## (no allocation was configured in the session)", ""),
    ctx$compare_lines %||% character(0),
    ctx$final_lines %||% character(0),
    "## ---- Save -------------------------------------------------",
    "## catch_data_raised is the object the whole length/age stage is built",
    "## on. Writing it here, and reading it there, is what stops the ages",
    "## being allocated over unraised landings.",
    'if (exists("catch_data_raised")) saveRDS(catch_data_raised, "model/catch_data_raised.rds")',
    'if (exists("comparison")) write.csv(comparison, "model/raising_comparison.csv", row.names = FALSE)',
    "")
}

# ----------------------------------------------------------
#  model_explore_length_age.R
# ----------------------------------------------------------

#' Emit model_explore_length_age.R
#'
#' The biology equivalent of the discard exploration: which strata carry a
#' length or age distribution, and how many fish stand behind it. Two axes
#' rather than one, because a fully covered stratum measured on nine fish is
#' not a stratum to raise on.
#'
#' @param ctx Session context. Uses `dims_len`, `dims_age`, `len_threshold`,
#'   `len_min_meas`, `age_threshold`, `age_min_meas`.
#' @param kept Kept-plot records; those marked for this section are drawn
#'   at the end of the script.
#' @return A character vector of R code.
#' @export
taf_model_explore_length_age <- function(ctx, kept = list()) {
  dims_length <- ctx$dims_len %||% c("catchCategory", "metier4",
                                     "vesselFlagCountry", "ICESDivision",
                                     "seasonValue")
  # Length and age can have DIFFERENT strata definitions in the app -
  # own_key_strata_server() runs independently for "len" and "age", and
  # customizing one never touches the other. Falling back to dims_length
  # only when dims_age was never supplied, not silently reusing it.
  dims_age <- ctx$dims_age %||% dims_length
  blk <- function(tag, what, thr, mm, dims_var) c(
    sprintf("## ---- %s ---------------------------------------------", tag),
    sprintf('bio_%s <- biology_link(catch_for_biology, distributions, %s, type = "%s")',
            what, dims_var, if (identical(tag, "Age")) "Age" else "LengthTotal"),
    sprintf('meas_%s <- stratum_measurements(catch_for_biology, distributions, %s, type = "%s")',
            what, dims_var, if (identical(tag, "Age")) "Age" else "LengthTotal"),
    sprintf("bio_%s <- add_sufficiency(bio_%s, meas_%s, %s, %s)",
            what, what, what, thr, mm),
    "## coverage keeps its real value; a thinly measured stratum is FLAGGED",
    "## rather than demoted to unsampled, so the two failures stay legible",
    "## as two different problems with two different remedies.",
    sprintf("bio_%s <- bio_%s %%>%% mutate(landings = weight,", what, what),
    "                     landings_with_discards = weight_measured,",
    "                     discards = 0, discard_rate = NA_real_)",
    sprintf('bio_%s <- classify_strata(bio_%s, threshold = %s, what = "%s", min_meas = %s)',
            what, what, thr, what, mm),
    sprintf("print(count(bio_%s, Status, name = \"n_strata\"))", what),
    sprintf('saveRDS(bio_%s, "model/bio_link_%s.rds")', what, what),
    "")

  c(taf_header("model_explore_length_age",
      "Length- and age-sampling coverage, and how many fish stand behind it",
      reads = c("data/catch_data.rds", "data/distributions.rds",
                "model/catch_data_raised.rds (used when it exists)"),
      writes = c("model/bio_link_length.rds", "model/bio_link_age.rds"),
      notes = c(
        "Biology sufficiency is a TWO-axis question, unlike discard coverage.",
        "A stratum can be fully covered and still useless if the distribution",
        "rests on a handful of fish, so coverage and the measurement count are",
        "carried separately and a stratum has to clear both to count as a",
        "donor. Collapsing them into one number would hide which of the two",
        "failed, and they have different fixes.")),
    taf_libs(),
    taf_sources(ctx$external_dir %||% "."),
    taf_mkdir("model"),
    "## ---- Input ------------------------------------------------",
    "## Explore on the raised catches when the discard step has run, so the",
    "## coverage shown is coverage of the catch that will actually be",
    "## allocated over. Falls back to the unraised table so this script can",
    "## still be run on its own.",
    'distributions <- readRDS("data/distributions.rds")',
    'catch_for_biology <- if (file.exists("model/catch_data_raised.rds"))',
    '    readRDS("model/catch_data_raised.rds") else readRDS("data/catch_data.rds")',
    "",
    sprintf("dims_biology_length <- %s", fmt_vals(dims_length)),
    sprintf("dims_biology_age    <- %s", fmt_vals(dims_age)),
    "",
    blk("Length", "length", ctx$len_threshold %||% 50, ctx$len_min_meas %||% 0,
        "dims_biology_length"),
    blk("Age", "age", ctx$age_threshold %||% 50, ctx$age_min_meas %||% 0,
        "dims_biology_age"),
    # same reasoning as the discard exploration: the picture sits with the
    # numbers it describes
    taf_plot_blocks(kept, "model_explore_length_age"))
}

# ----------------------------------------------------------
#  model_allocate_length_age.R
# ----------------------------------------------------------

#' Emit model_allocate_length_age.R
#'
#' Group definition and `catchAllocationAoL()` for lengths and then ages,
#' over the RAISED catches produced by the discard stage.
#'
#' @param ctx Session context. Uses `len_alloc_lines`, `age_alloc_lines`.
#' @return A character vector of R code.
#' @export
taf_model_allocate_length_age <- function(ctx) {
  none <- function(tag) c(
    sprintf("## (no %s allocation was saved in the session)", tag), "")
  c(taf_header("model_allocate_length_age",
      "Allocation groups and catchAllocationAoL() for length and age",
      reads = c("model/catch_data_raised.rds", "data/distributions.rds",
                "model/bio_link_length.rds", "model/bio_link_age.rds"),
      writes = c("model/alloc_length.rds", "model/alloc_age.rds",
                 "model/numbers_at_length.csv", "model/numbers_at_age.csv"),
      notes = c(
        "This script reads model/catch_data_raised.rds, NOT data/catch_data.rds.",
        "Numbers at length and age are raised to the total catch, and the",
        "total catch only exists once the discards have been raised. Reading",
        "the unraised table would allocate over official landings alone and",
        "silently lose the whole discard fraction - so the dependency is a",
        "file the script cannot run without, not a comment asking you to",
        "remember.",
        "",
        "Length is done before age because an age distribution is usually",
        "built through an age-length key over the same strata; run in this",
        "order the length result is on disk if you want to check against it.")),
    taf_libs(),
    taf_sources(ctx$external_dir %||% "."),
    taf_mkdir("model"),
    "## ---- Input ------------------------------------------------",
    "## A raw readRDS() here fails with \"cannot open the connection\" -",
    "## true, but not why. The real reason is always the same: no Final",
    "## raising has been adopted and run yet, in the app or in",
    "## model_allocate_raise_discards.R.",
    'if (!file.exists("model/catch_data_raised.rds"))',
    '  stop("model/catch_data_raised.rds not found. Run a Final raising ",',
    '       "first - either \\"Raise with this allocation\\" in the app, or ",',
    '       "model_allocate_raise_discards.R, with an allocation adopted.")',
    'catch_data_raised <- readRDS("model/catch_data_raised.rds")',
    'distributions     <- readRDS("data/distributions.rds")',
    'bio_length        <- readRDS("model/bio_link_length.rds")',
    'bio_age           <- readRDS("model/bio_link_age.rds")',
    "",
    "## =========================================================",
    "##  LENGTH",
    "## =========================================================",
    ctx$len_alloc_lines %||% none("length"),
    "## =========================================================",
    "##  AGE",
    "## =========================================================",
    ctx$age_alloc_lines %||% none("age"),
    "## ---- Save -------------------------------------------------",
    'if (exists("alloc_length")) saveRDS(alloc_length, "model/alloc_length.rds")',
    'if (exists("alloc_age")) saveRDS(alloc_age, "model/alloc_age.rds")',
    "")
}

# ----------------------------------------------------------
#  output.R
# ----------------------------------------------------------

#' Emit output.R
#'
#' output.R used to hold every kept plot. It now holds only the RESULT
#' figures - the raised composition, the allocation comparison, the numbers
#' and stock weight at length and age. The exploratory coverage and
#' discard-rate plots moved into the model scripts that compute what they
#' show, which is where a reader looks for them.
#'
#' Every figure still lands in `output/`, from wherever it was drawn. The
#' folder is the deliverable; the call site is an editorial choice about
#' what sits next to what.
#'
#' @param kept The full list of kept-plot records; those marked for the
#'   output section are drawn here.
#' @return A character vector of R code.
#' @export
taf_output <- function(kept = list()) {
  blocks <- taf_plot_blocks(kept, "output")
  head <- c(taf_header("output",
      "Redraw and save the result figures kept during the session",
      reads = c("model/catch_data_raised.rds", "model/alloc_length.rds",
                "model/alloc_age.rds"),
      writes = "output/*.png",
      notes = c(
        "Result figures only. The coverage and discard-rate plots kept",
        "during exploration are drawn by model_explore_discards.R and",
        "model_explore_length_age.R, beside the numbers they describe -",
        "but they are written to output/ like everything else, so the",
        "figure folder is still complete on its own.",
        "",
        "These are the plots that were KEPT, not the last one left on",
        "screen. Each was added by pressing 'Keep for script' with the",
        "settings shown in its comment, so several cuts of the same result",
        "can and do appear side by side here.")),
    taf_libs(),
    taf_sources("."),
    "## ---- Input ------------------------------------------------",
    "## Everything the model stage left behind; missing files are skipped so",
    "## a partial run still draws what it can.",
    'ld <- function(p) if (file.exists(p)) readRDS(p) else NULL',
    'catch_data        <- ld("data/catch_data.rds")',
    'distributions     <- ld("data/distributions.rds")',
    'strata_discards   <- ld("model/strata_discards.rds")',
    'catch_data_raised <- ld("model/catch_data_raised.rds")',
    '## biology plots are drawn over the raised catches where they exist',
    'catch_for_biology <- if (!is.null(catch_data_raised)) catch_data_raised else catch_data',
    'bio_length        <- ld("model/bio_link_length.rds")',
    'bio_age           <- ld("model/bio_link_age.rds")',
    'alloc_length      <- ld("model/alloc_length.rds")',
    'alloc_age         <- ld("model/alloc_age.rds")',
    "")
  if (!length(blocks))
    return(c(head,
      "## ---- Figures ----------------------------------------------",
      "## No result figures were kept. In the app, press 'Keep for script'",
      "## under a plot on the raising or allocation tabs and it appears",
      "## here with the settings it was kept with.",
      ""))
  c(head, blocks)
}

#' A file-name-safe slug
#' @param x A title.
#' @return A lower-case, underscore-separated string.
#' @export
taf_slug <- function(x) {
  x <- tolower(gsub("[^A-Za-z0-9]+", "_", x %||% "plot"))
  x <- gsub("^_+|_+$", "", x)
  if (!nzchar(x)) "plot" else substr(x, 1, 48)
}

# ----------------------------------------------------------
#  report.R
# ----------------------------------------------------------

#' Emit the (empty) report.R placeholder
#' @return A character vector of R code.
#' @export
taf_report <- function() c(
  taf_header("report",
    "Assemble the tables and figures for the working-group report",
    reads = c("output/*.png", "model/*.csv"),
    writes = "report/*",
    notes = c(
      "Left empty on purpose. What a report needs is decided by the working",
      "group, not by the workbench, and a guessed template is harder to",
      "throw away than an empty file is to fill.")),
  taf_libs(),
  taf_mkdir("report"),
  "## ---- To do ------------------------------------------------",
  "## Copy the figures and tables this stock's report needs out of",
  "## output/ and model/, and write whatever summary goes with them.",
  "")

# ----------------------------------------------------------
#  Assembly
# ----------------------------------------------------------

#' Build the whole TAF script set
#'
#' @param ctx Session context assembled by the app.
#' @param kept The kept-plot records, for output.R.
#' @return A named list of character vectors, one per part, in run order.
#' @export
taf_bundle <- function(ctx = list(), kept = list()) {
  safe <- function(f) tryCatch(f(), error = function(e)
    c("## This script could not be generated from the current session:",
      paste0("##   ", conditionMessage(e))))
  list(
    data                          = safe(function() taf_data(ctx)),
    model_explore_discards        = safe(function() taf_model_explore_discards(ctx, kept)),
    model_allocate_raise_discards = safe(function() taf_model_allocate_raise_discards(ctx)),
    model_explore_length_age      = safe(function() taf_model_explore_length_age(ctx, kept)),
    model_allocate_length_age     = safe(function() taf_model_allocate_length_age(ctx)),
    output                        = safe(function() taf_output(kept)),
    report                        = safe(function() taf_report()))
}

#' Concatenate a set of TAF parts into one readable listing
#'
#' Used for the "Live script (full)" view, where the whole set is read top to
#' bottom, and for the fallback download when zipping is unavailable.
#'
#' @param bundle Output of [taf_bundle()].
#' @param parts Which parts to include; defaults to all, in run order.
#' @return A character vector.
#' @export
taf_concat <- function(bundle, parts = names(TAF_FILES)) {
  parts <- intersect(names(TAF_FILES), parts)
  unlist(lapply(parts, function(p) c(
    "",
    sprintf("## %s", strrep("#", 68)),
    sprintf("## >>> FILE: %s", TAF_FILES[[p]]),
    sprintf("## %s", strrep("#", 68)),
    bundle[[p]])), use.names = FALSE)
}

#' Write a set of TAF parts to a directory
#' @param bundle Output of [taf_bundle()].
#' @param dir Target directory.
#' @param parts Which parts to write.
#' @return The paths written, invisibly.
#' @export
taf_write <- function(bundle, dir, parts = names(TAF_FILES)) {
  parts <- intersect(names(TAF_FILES), parts)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  paths <- vapply(parts, function(p) {
    f <- file.path(dir, TAF_FILES[[p]])
    writeLines(bundle[[p]], f)
    f }, character(1))
  invisible(unname(paths))
}

#' Zip a set of TAF parts, falling back to a single concatenated file
#'
#' `utils::zip()` shells out to a zip binary that is not guaranteed to exist
#' on a deployment host. Rather than handing the user a broken download, the
#' fallback writes the same scripts concatenated into one .R file, which is
#' still runnable after splitting.
#'
#' @param bundle Output of [taf_bundle()].
#' @param parts Which parts to include.
#' @param file The path the download handler wants written.
#' @return `"zip"` or `"concat"`, invisibly, saying which happened.
#' @export
taf_zip <- function(bundle, parts, file) {
  parts <- intersect(names(TAF_FILES), parts)
  tmp <- file.path(tempdir(), paste0("taf_", as.integer(Sys.time())))
  taf_write(bundle, tmp, parts)
  ok <- tryCatch({
    old <- setwd(tmp); on.exit(setwd(old), add = TRUE)
    utils::zip(zipfile = file, files = unname(TAF_FILES[parts]), flags = "-q")
    file.exists(file) && file.size(file) > 0
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!isTRUE(ok)) {
    writeLines(taf_concat(bundle, parts), file)
    return(invisible("concat"))
  }
  invisible("zip")
}

# ----------------------------------------------------------
#  Blurbs for the UI
# ----------------------------------------------------------

#' One-line description of each script, shown above its listing
#' @export
TAF_BLURB <- c(
  data = paste("Reads the catch, distribution and effort tables and derives",
               "the metier and ICES area levels. No thresholds, no groups,",
               "no decisions."),
  model_explore_discards = paste("Builds the strata at the chosen level of",
    "detail, classifies them against the coverage threshold, and draws the",
    "coverage and discard-rate figures kept here."),
  model_allocate_raise_discards = paste("Turns that evidence into groups -",
    "automatically as a rule, or manually as explicit conditions - and runs",
    "catchRaising(). Writes catch_data_raised."),
  model_explore_length_age = paste("The same question for biology, on two",
    "axes: is a length or age distribution there, and how many fish is it",
    "built on. Draws the biology figures kept here."),
  model_allocate_length_age = paste("Allocation groups and",
    "catchAllocationAoL() for length then age, over the RAISED catches from",
    "the discard step."),
  output = paste("Redraws the kept RESULT figures. The exploratory ones are",
                 "drawn by the model scripts; all of them land in output/."),
  report = paste("Empty by design. What the working group's report needs is",
                 "not something the workbench should guess at."))

# ----------------------------------------------------------
#  Small builders for the plot calls kept from the app
# ----------------------------------------------------------

#' Format a value for emitted code
#'
#' Empty, `NA` and `NULL` all mean "this role was left unset" in the app, and
#' all become a literal `NULL` in the script - which is what the plot
#' functions expect for an unused role.
#'
#' @param v A length-one value, or `NULL`.
#' @return A quoted string, or `"NULL"`.
#' @export
taf_q <- function(v) {
  if (is.null(v) || !length(v)) return("NULL")
  v <- v[1]
  if (is.na(v) || !nzchar(as.character(v))) return("NULL")
  sprintf('"%s"', as.character(v))
}

#' Build a function call as tidy, wrapped script lines
#'
#' @param fn The function name.
#' @param args A named list of already-formatted argument strings; unnamed
#'   entries are emitted positionally, `NULL` entries dropped.
#' @return A character vector, one element per line.
#' @export
taf_call <- function(fn, args) {
  args <- args[!vapply(args, is.null, logical(1))]
  nm <- names(args); if (is.null(nm)) nm <- rep("", length(args))
  vals <- vapply(args, function(v) paste(as.character(v), collapse = ""),
                 character(1))
  txt <- ifelse(nzchar(nm), paste0(nm, " = ", vals), vals)
  n <- length(txt)
  if (!n) return(paste0(fn, "()"))
  if (n == 1) return(paste0(fn, "(", txt, ")"))
  # one argument per line: these calls carry eight or nine roles and a
  # single line of them is unreadable and undiffable
  c(paste0(fn, "(", txt[1], ","),
    if (n > 2) paste0(txt[2:(n - 1)], ","),
    paste0(txt[n], ")"))
}
