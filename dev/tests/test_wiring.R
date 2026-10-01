# ============================================================
#  test_wiring.R  -- a structural audit of the installed app.R
#
#  Ported from the pre-package suite. It reads the app.R that SHIPS,
#  not a file in the working tree, so it audits what a user installs.
#
#  Three rounds of regex surgery on app.R once silently deleted a
#  reactive, two outputs and a whole server block. Each time the
#  behavioural tests caught it, but only after a confusing failure deep
#  inside an unrelated assertion. These checks fail loudly and name the
#  missing thing.
#
#  Syntax is check_syntax.R's job and the server actually running is
#  check_server.R's; this file is only about wiring.
# ============================================================
FAIL <- 0L
ok <- function(l, c, detail = NULL) {
  if (isTRUE(c)) cat("ok    ", l, "\n")
  else {
    FAIL <<- FAIL + 1L
    cat("FAIL  ", l, "\n")
    if (!is.null(detail) && length(detail))
      cat("       ", paste(detail, collapse = ", "), "\n")
  }
}

app <- system.file("app/app.R", package = "rcef")
if (!nzchar(app)) stop("rcef is not installed; R CMD INSTALL first")
src <- readLines(app, warn = FALSE)
txt <- paste(src, collapse = "\n")

cat("=== 1. the file is valid R ===\n")
ok("app.R parses", !inherits(try(parse(app), silent = TRUE), "try-error"))

i_sv <- regexpr("server <- function\\(input, output, session\\)", txt)
ui <- substr(txt, 1, i_sv - 1); sv <- substr(txt, i_sv, nchar(txt))

# Strip trailing comments before scanning for CALLS. A function named in
# prose - "biology_link() walks the whole table" - is not a call site, and
# treating it as one reports six perfectly well-defined functions as
# missing, which is exactly the noise that makes a real miss easy to skip
# past. Only comments are removed; strings are left alone, since some
# checks below legitimately read literal ids out of them.
strip_comments <- function(s) {
  ln <- strsplit(s, "\n", fixed = TRUE)[[1]]
  out <- vapply(ln, function(l) {
    ch <- strsplit(l, "")[[1]]
    inq <- ""; esc <- FALSE; cut <- NA_integer_
    for (i in seq_along(ch)) {
      c1 <- ch[i]
      if (esc) { esc <- FALSE; next }
      if (nzchar(inq)) {
        if (c1 == "\\") { esc <- TRUE; next }
        if (c1 == inq) inq <- ""
        next
      }
      if (c1 %in% c('"', "'")) { inq <- c1; next }
      if (c1 == "#") { cut <- i; break }
    }
    if (is.na(cut)) l else substr(l, 1, cut - 1)
  }, character(1), USE.NAMES = FALSE)
  paste(out, collapse = "\n")
}

# Comments AND string contents removed. Used only for the call scan, which
# must not see the emitted-script text: those strings are full of R code
# that is not being run here - "compare_scenarios() needs the raised runs"
# is a comment the app WRITES, not a call the app MAKES.
code_only <- function(s) {
  s <- strip_comments(s)
  gsub('"[^"]*"', '""', gsub("'[^']*'", "''", s))
}

grab <- function(s, pat) unique(unlist(regmatches(s, gregexpr(pat, s))))
cap <- function(s, pat, grp = 1) {
  m <- gregexpr(pat, s, perl = TRUE)
  r <- regmatches(s, m)[[1]]
  if (!length(r)) return(character(0))
  unique(sub(pat, paste0("\\", grp), r, perl = TRUE))
}

cat("\n=== 2. every UI output has a server definition ===\n")
# Several UI builders each carry their own ns() prefix (cef_result_ui uses
# ic_/cef_, the length views use len_cov_/len_n_/len_w_/len_ga_). Crossing
# every prefix with every slot invents combinations that never exist, so
# namespaced slots are compared by NAME only: the invariant that matters is
# that a ns("x") in the UI has an output[[ns("x")]] in the server.
ns_ui <- cap(ui, '(?:tbl_slot|plot_slot|uiOutput|plotOutput|DTOutput|verbatimTextOutput)[(]ns[(]["]([^"]+)["][)]')
ns_sv <- cap(sv, "output[[][[]ns[(][\"]([^\"]+)[\"]")
ns_sv <- c(ns_sv, sub("^.*_", "", cap(sv, 'output[[][[]paste0[(]p, ["]_([a-z_]+)["]')))
ok("every namespaced UI slot has a server output",
   length(setdiff(ns_ui, c(ns_sv, paste0(ns_sv, "_ui"),
                           sub("_ui$", "", ns_sv)))) == 0,
   setdiff(ns_ui, c(ns_sv, paste0(ns_sv, "_ui"), sub("_ui$", "", ns_sv))))

ui_out <- c(
  cap(ui, '(?:uiOutput|plotOutput|verbatimTextOutput|textOutput|DTOutput|tbl_slot|plot_slot)[(][ ]*["]([^"]+)["]'),
  # dropzone("x") renders uiOutput(paste0(x, "_txt_ui")) at run time
  paste0(c("strata", "matched"), "_txt_ui"))
ui_out <- unique(ui_out[!is.na(ui_out) & nzchar(ui_out)])
sv_out <- c(cap(sv, 'output[$]([A-Za-z0-9_.]+)[ ]*<-'),
            cap(sv, "output[[][[][ ]*[\"]([^\"]+)[\"]"))
sv_out <- unique(sv_out[!is.na(sv_out) & nzchar(sv_out)])
missing <- setdiff(ui_out, c(sv_out, paste0(sv_out, "_ui"),
                            sub("_ui$", "", sv_out)))
# plot_slot("x") renders uiOutput("x_ui"), so x_ui satisfies x.
# NOTE the length guard: character(0)[TRUE] is NA, not character(0),
# which would turn a clean result into a phantom failure.
if (length(missing))
  missing <- missing[!(paste0(missing, "_ui") %in% sv_out)]
ok("no UI output left dangling", length(missing) == 0, missing)
cat("   UI outputs:", length(ui_out), "| server outputs:", length(sv_out), "\n")

cat("\n=== 3. no orphaned server outputs ===\n")
dl <- cap(ui, 'downloadButton\\("([^"]+)"')
dead <- setdiff(sv_out, c(ui_out, sub("_ui$", "", ui_out),
                          paste0(ui_out, "_ui"), dl, "river_"))
dead <- dead[!grepl("^river_", dead)]
ok("no server output without a UI slot", length(dead) == 0, dead)

cat("\n=== 4. every reactive called is defined ===\n")
# top level of the server (2 spaces) AND inside module functions such as
# length_server(), which indent their reactives further
# gated() returns a reactive just as reactive() does - it is the Apply-gate
# wrapper - so a plot built with it is as much a reactive as one built the
# plain way. debounce()/throttle() are the same: shiny's own reactive
# wrappers, used to keep an expensive render from firing on every tick of
# a slider being dragged. Missing any of these here does not fail
# loudly: the audit simply stops knowing those objects exist, and then
# reports every one of them as "called before it exists", which buries
# the one name that might be real.
defined <- c(cap(sv, '\\n  ([A-Za-z_][A-Za-z0-9_.]*)\\s*<-\\s*(?:reactive|gated|debounce|throttle|function)'),
             cap(sv, '\\n\\s{4,}([A-Za-z_][A-Za-z0-9_.]*)\\s*<-\\s*(?:reactive|gated|debounce|throttle|function)'),
             ## A reactive pulled out of a list a *_server() helper
             ## returned - `x_keys <- srv$keys` - is as much a reactive
             ## as one written inline. Without this the audit reports
             ## every accessor as undefined, and five false names is how
             ## a real one gets skipped past.
             cap(sv, '\\n\\s*([A-Za-z_][A-Za-z0-9_.]*)\\s*<-\\s*[A-Za-z_][A-Za-z0-9_.]*\\$[A-Za-z_][A-Za-z0-9_.]*'),
             cap(sv, '\\n\\s*([A-Za-z_][A-Za-z0-9_.]*)\\s*<-\\s*[a-z_]+_server\\('),
             ## and a reactive reached through the list itself -
             ## `mg_excl$bars()` - where the name never appears on the
             ## left of an assignment at all
             cap(sv, '[A-Za-z_][A-Za-z0-9_.]*\\$([A-Za-z_][A-Za-z0-9_.]*)\\(\\)'))
# scanned on the comment-free source, so a name mentioned in prose is not
# mistaken for a call
called  <- cap(code_only(sv), '\\b([a-z_][a-z0-9_.]*)\\(\\)')
base_fns <- c("br","list","character","invisible","return","function","globalenv",
              "log_new","Sys.time","Sys.Date","tempdir","removeModal","new.env",
              # formals of the gate/keep/load helpers: `sig()`, `refresh()`,
              # `live()`, `action()` are arguments being invoked, not
              # reactives the server is expected to define
              "sig","refresh","live","action","obj","now","val","x","hr",
              "isolate","ld","fn","rd","stub")
outside <- c("emit_derive_code","emit_area_helper","startup_modal","ic_ui","log_ui","alloc_box",
             "fragments_ui","final_ui","coverage_ui","drates_ui","cef_load_ui",
             "ic_load_ui","ic_output_ui","shinyDirLink")
undef <- setdiff(called, c(defined, base_fns, outside))
undef <- undef[vapply(undef, function(f) !exists(f), logical(1))]
ok("no reactive called before it exists", length(undef) == 0, undef)
cat("   reactives/functions defined in server:", length(defined), "\n")

## `mg_excl$bars()` calls a member of an object. The audit above only
## looks for `name(`, so the OBJECT name was never checked - and a typo
## in it (mg_excl for mg_excl_keys_srv) sailed through into a release,
## failing only when the user opened the panel that rendered it.
obj_used <- unique(cap(sv, '[^A-Za-z0-9_.$]([A-Za-z_][A-Za-z0-9_.]*)\\$[A-Za-z_][A-Za-z0-9_.]*\\('))

## Anything assigned ANYWHERE in the server counts, not just the
## reactives: module-local helpers and loop variables are real
## definitions too. Function arguments count as well.
assigned_any <- unique(cap(sv, '\\n\\s*([A-Za-z_][A-Java-z0-9_.]*)\\s*<-'))
assigned_any <- unique(c(assigned_any,
                         cap(sv, '\\n\\s*([A-Za-z_][A-Za-z0-9_.]*)\\s*<-')))
args_any <- unique(unlist(strsplit(
  gsub("[^A-Za-z0-9_., ]", "", cap(sv, 'function\\(([^)]*)\\)')), "\\s*,\\s*")))
args_any <- trimws(sub("=.*$", "", args_any))

## `tags` is shiny's, and `gate` is a formal of a helper whose signature
## spans two lines, which the argument regex above does not reach.
## Listed rather than loosened: a broader regex would start swallowing
## the typos this check exists to find.
known <- unique(c(defined, assigned_any, args_any,
                  "rv", "cfg", "input", "output", "session",
                  "tags", "gate"))
obj_missing <- setdiff(obj_used, known)
obj_missing <- obj_missing[!vapply(obj_missing, exists, logical(1))]
ok("objects reached with $ are defined in the server", length(obj_missing) == 0,
   obj_missing)

cat("\n=== 5. inputs referenced by the server exist in the UI ===\n")
# comment-free, for the same reason as the call scan above: prose that
# says "reads input$... as before" is not a reference to a control
sv_in <- cap(strip_comments(sv), 'input\\$([A-Za-z0-9_.]+)')
sv_in <- setdiff(sv_in, cap(sv, "input[[][[]([A-Za-z0-9_.]+)"))
ui_in <- c(cap(ui, '(?:selectInput|selectizeInput|numericInput|sliderInput|textInput|passwordInput|checkboxInput|checkboxGroupInput|radioButtons|actionButton|fileInput|dateInput)\\(\\s*"([^"]+)"'),
           cap(ui, 'paste0\\("([a-z_]+)", *"?_'),
           cap(ui, "setInputValue\\('([A-Za-z0-9_]+)'"),
           cap(ui, 'pick_bar\\("([^"]+)"'),
           cap(ui, 'role_slots\\("([^"]+)"'),
           cap(ui, 'level_box\\("([^"]+)"'),
           cap(ui, 'dropzone\\("([^"]+)"'),
           cap(ui, 'dir_picker\\("([^"]+)"'),
           # dir_picker(id) creates id plus id_browse / id_cd / id_pick
           as.vector(outer(cap(ui, 'dir_picker\\("([^"]+)"'),
                           c("", "_browse", "_cd", "_pick"), paste0)),
           cap(ui, 'var_chips\\("([^"]+)"'))
# ids built at run time from prefixes
dyn <- c("thr_","icf_","hier_","river_")
# inputs that exist but not as a static UI control: the navbarPage id, and
# controls created inside server-side modalDialog()s (confirm buttons etc)
srv_dynamic <- c("nav", "clear_confirm", "clear_screen",
                 cap(sv, 'modalButton\\("([^"]+)"'),
                 cap(sv, 'actionButton\\("([^"]+)"'))
srv_made <- cap(sv, 'inputId = paste0\\("([a-z_]+)"')
unknown <- sv_in[!vapply(sv_in, function(i)
  any(startsWith(i, dyn)) ||
  i %in% ui_in ||
  i %in% srv_dynamic ||
  any(vapply(ui_in, function(p) startsWith(i, paste0(p, "_")), logical(1))) ||
  grepl("^(cfg_|ic_|nr_|cov_|dr_|box_|hm_|bb_|cb_|ar_|mg_|dt_|cmp_|fin_|log_|code_|clause_|rule_|man_|auto_|self_|final_|drop_|set_|move_|del_|up_|start_|build_|run_|save_|add_|apply_|mirror|to_|file|sep|stock|year|threshold)", i),
  logical(1))]
ok("no server input reference with no UI control", length(unknown) == 0, unknown)

cat("\n=== 6. the package exports everything app.R relies on ===\n")
suppressMessages(library(rcef))
need <- c("build_strata","build_coverage_strata","classify_strata",
          "recommend_raising","raising_steps",
          "decision_tree","raising_groups",
          "plot_coverage_heat","plot_coverage_bubble","plot_coverage_bar",
          "plot_coverage_map","plot_DR",
          "plot_NaLoA","plot_MWaLoA",
          "plot_river","plot_river_level","plot_comparison",
          "comparison_table","group_palette","var_palette","app_theme",
          "to_tonnes","fmt_t3","catches_summary","catches_by_category",
          "conds_from_auto","conds_from_rules",
          "run_native_raising","cef_prepare","cef_check",
          "code_auto_rule","code_manual_rule","code_raise","code_compare",
          "code_final","log_set","log_render","state_apply",
          "ic_code","new_rule","assign_rule_groups","flag_donor_exclusion",
          # added since the package split - the audit is only worth
          # keeping if it grows with the app
          "apply_filters","clean_filters","explore_filter_levels",
          "filter_label","filter_coverage",
          "exclusion_choices","excluded_keys","exclusion_overview",
          "exclusion_label","stratum_keys","stratum_metrics",
          "alloc_matched_cond","alloc_overview","assign_alloc_groups",
          "manual_progress","rules_progress","rules_unassigned",
          "alloc_steps","assign_alloc_cascade","alloc_cascade_table",
          "cc_bracket","as_cc_spec","cc_step_value","fmt_cc_spec",
          "rcef_sources","rcef_status","run_app")
absent <- need[!vapply(need, exists, logical(1), mode = "function")]
ok("all helpers app.R relies on are exported", length(absent) == 0, absent)
ok("constants defined", all(vapply(c("OUTPUT_FORMATS","OUTPUT_DESC",
                                     "IC_FILE_ROLES","IC_FILE_PATTERN",
                                     "FILTER_VARS",
                                     "LOG_ORDER","CEF_ORDER","CEF_REQUIRED",
                                     "FILTER_ANY","RCEF_APP_PKGS"),
                                   exists, logical(1))))

cat("\n=== 7. things deliberately removed stay removed ===\n")
# quoted, so raised_ov does not match inside raised_overview_table
for (gone in c("nr_engine", "hm_plot", "bb_plot", "cb_plot",
               "dr_raw_plot", "dr_box_plot", "raised_ov", "donor_ov",
               "sc_tbl_auto", "sc_tbl_man", "clause_txt", "summary_tbl",
               "fine_tbl", "ladder_ui", "schema_html", "schematic",
               "cov_schema", "dr_schema"))
  ok(paste("gone:", gone),
     !grepl(paste0('"', gone, '"'), txt, fixed = TRUE))
ok("gone: built-in raising engine", !grepl("Built-in", txt, fixed = TRUE))
## the "Groups from: auto/manual" toggle and everything only it fed -
## the level of detail shown on the final raising tab now always comes
## from the SELECTED allocation's own dims, which is what run_final()
## actually uses, rather than a separate choice that could disagree
for (gone_fn in c("DMAP_ALIAS", "nr_dims", "level_deck", "level_fields",
                  "level_box", "input$nr_source", "input$ar_fleet",
                  "input$ar_area", "input$mg_fleet", "input$mg_area"))
  ok(paste("gone:", gone_fn), !grepl(gone_fn, txt, fixed = TRUE))
## Cross-tab on the final raising tab - Rows/Columns duplicated what
## Split by / Facet already offered, and raised_crosstab() had no other
## caller
for (gone_ct in c("fin_row", "fin_col", "fin_crosstab", "raised_crosstab"))
  ok(paste("gone:", gone_ct), !grepl(gone_ct, txt, fixed = TRUE))

cat("\n=== 8. every render output actually renders ===\n")
# Rendering each output catches things static analysis cannot: a tag given
# a character VECTOR instead of a string, or a sprintf with %% where %s
# was meant. Both shipped once and were invisible until rendered.
ui_txt <- ui
bad_vec <- cap(ui_txt, 'div[(]class = "note", [a-z_]+[)]')
ok("no tag handed a bare variable as its only child",
   length(bad_vec) == 0, bad_vec)
pct <- grep("%%[sdf]", src, value = TRUE)
ok("no doubled format specifier in sprintf", length(pct) == 0,
   substr(pct, 1, 60))

cat("\n=== 9. icon panels are siblings, not nested ===\n")
# A conditionalPanel keyed on the SAME picker id must never sit inside
# another one: "sec3 == stgexpl" wrapping "sec3 == stgraise" can never be
# true, so that section renders blank. This walks the paren depth and
# checks every panel for a given picker opens at the same level.
panel_depths <- function(txt, pick_id) {
  ln <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  pat <- sprintf('conditionalPanel("input.%s ==', pick_id)
  depth <- 0; instr <- FALSE; esc <- FALSE; out <- integer(0)
  for (l in ln) {
    d0 <- depth
    for (ch in strsplit(l, "")[[1]]) {
      if (esc) { esc <- FALSE; next }
      if (ch == "\\") { esc <- TRUE; next }
      if (ch == '"') { instr <- !instr; next }
      if (instr) next
      if (ch == "(") depth <- depth + 1 else if (ch == ")") depth <- depth - 1
    }
    if (grepl(pat, l, fixed = TRUE)) out <- c(out, d0)
  }
  out
}
for (pk in c("sec3", "sec4", "sec5", "src")) {
  d <- panel_depths(txt, pk)
  ok(sprintf("panels for '%s' are siblings (%s panel(s))", pk, length(d)),
     length(d) == 0 || length(unique(d)) == 1, paste(d, collapse = ", "))
}

cat("\n=== 10. every icon panel has a picker that publishes its value ===\n")
pick_ids <- cap(ui, 'pick_bar[(]["]([^"]+)["]')
pick_ids <- c(pick_ids, sub('^paste0[(]"', '',
              cap(ui, 'pick_bar[(]paste0[(]["]([a-z_]+)["]')))
# pick_bar() is not the only thing that publishes a value a panel can key
# on: a plain radioButtons() does too, and ic_mode has always been one.
# Without this the audit reports a control that is right there in the UI.
pick_ids <- c(pick_ids, cap(ui, 'radioButtons[(]["]([^"]+)["]'))
cond_ids <- unique(cap(ui, 'conditionalPanel[(]["]input[.]([A-Za-z0-9_]+) =='))
# ids built at run time from a prefix are matched loosely
unpub <- cond_ids[!vapply(cond_ids, function(i)
  any(i == pick_ids) || any(vapply(pick_ids, function(p)
    startsWith(i, p), logical(1))) ||
  grepl("^(input|dr_on|self_on|cef_mode|ic_rel_mode|raise_mode|grp_method|nr_)",
        i), logical(1))]
ok("no panel keyed on an id nothing publishes", length(unpub) == 0, unpub)
ok("pickers initialise their value on load",
   grepl("pickInit", txt, fixed = TRUE))

cat("\n=== 11. things this port added ===\n")
ok("the figure filter is a chip builder, not a wall of dropdowns",
   grepl("_flt_chips", txt, fixed = TRUE) &&
   !grepl('paste0(p, "_flt_", v)', txt, fixed = TRUE))
ok("the busy dots name the task", grepl("WORK_WHAT", txt, fixed = TRUE))
ok("no slider left where a number box belongs",
   !grepl("catchall_dr\"", txt))

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
