# ============================================================
#  RCEF Workbench  --  shiny front end over the rcef package
#  Run with:  rcef::run_app()
#  This file is inst/app/app.R: it is NOT package code and is never
#  seen by R CMD check.
# ============================================================

library(shiny)
library(DT)

# ============================================================
#  Loading
#
#  Everything that is not shiny now lives in the rcef package:
#  the workbench functions AND the vendored RDBESStockCoord engines
#  (catchRaising(), catchAllocationAoL(), funIntercatchCEF(), ...).
#  There is no external loader and no source() of data modules.
#
#  Two ways in:
#    installed  library(rcef); rcef::run_app()
#    working tree  pkgload::load_all(".") then shiny::runApp("inst/app")
# ============================================================
if (!requireNamespace("rcef", quietly = TRUE) &&
    !exists("catchRaising", mode = "function")) {
  stop("The rcef package is not installed and not loaded.\n",
       "  remotes::install_github(\"DVillagra26/rcef\")\n",
       "or, in the working tree:  pkgload::load_all(\".\")", call. = FALSE)
}
if (requireNamespace("rcef", quietly = TRUE)) library(rcef)

# Shiny-only helpers. These are the ONLY files the app sources; they are
# excluded from the package because they build tags or read reactives.
APP_DIR <- getOption(
  "rcef.app_dir",
  if (file.exists("app_ui_helpers.R")) "." else "inst/app")
source(file.path(APP_DIR, "app_ui_helpers.R"))        # icons, heights, legend
source(file.path(APP_DIR, "app_mod_intercatch_ui.R")) # ic_ui()
source(file.path(APP_DIR, "app_state.R"))             # state_capture()

# What the installed package can actually do. Same shape the old
# load_external()/external_status() pair returned, so the Availability
# panel and the TAF context are unchanged.
EXT <- rcef_sources()
EXT_STATUS <- rcef_status(EXT)

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(rlang)
  library(purrr); library(ggalluvial); library(data.table); library(stringr)
  # jsonlite is NOT attached: its validate() would mask shiny::validate()
  requireNamespace("jsonlite", quietly = TRUE)
})


alloc_box <- function() uiOutput("alloc_box")


# ---------------------------------------------------------
#  Section 1: InterCatch, split across three tabs
# ---------------------------------------------------------
ic_load_ui <- function() tagList(
  uiOutput("ic_input_note"),
  sidebarLayout(
  sidebarPanel(width = 4, class = "simplecard",
    h4("Input files"),
    radioButtons("ic_mode", NULL,
      c("Upload files" = "upload", "Point at a folder" = "dir"), "upload"),
    conditionalPanel("input.ic_mode == 'upload'",
      fileInput("ic_upl", NULL, multiple = TRUE, accept = c(".csv", ".txt")),
      span(class = "note",
           "Select the InterCatch files. They are matched to roles on the ",
           "right; fix any row matched to the wrong file.")),
    conditionalPanel("input.ic_mode == 'dir'",
      dir_picker("ic_path", "."),
      span(class = "note",
           "Type a path, or click the folder to browse into subdirectories.")),
    conditionalPanel("input.ic_source == 'icout'",
      textInput("ic_years", "years (comma separated)", "2022")),
    tags$hr(),
    h4("stock_relation"),
    radioButtons("ic_rel_mode", NULL,
      c("Build with funMakeRelation(year)" = "build",
        "Upload a csv" = "upload",
        "Let funICoutCEF build it" = "auto")),
    conditionalPanel("input.ic_rel_mode == 'build'",
      numericInput("ic_rel_year", "year", 2022, 1990, 2100, 1),
      actionButton("build_rel", "Build stock_relation", class = "btn-sm")),
    conditionalPanel("input.ic_rel_mode == 'upload'",
      fileInput("ic_rel_file", NULL, accept = ".csv")),
    uiOutput("rel_status")),
  mainPanel(width = 8,
    uiOutput("cfg_banner"),
    h4("Files and their roles"),
    uiOutput("ic_file_map"),
    tags$hr(),
    h4("Availability"), uiOutput("ic_status"))))

ic_output_ui <- function() tagList(
  span(class = "note",
       "Select the output type required. If to_file or to_upload is chosen, ",
       "fill in out_path, file_prefix and file_suffix (those fields appear ",
       "only when a file is being written)."),
  sidebarLayout(
  sidebarPanel(width = 4, class = "simplecard",
    h4("Output"),
    checkboxGroupInput("ic_output_format", "output_format",
      choiceNames = lapply(names(OUTPUT_DESC),
                           function(k) opt_lab(k, OUTPUT_DESC[[k]])),
      choiceValues = names(OUTPUT_DESC),
      selected = c("to_list", "to_file")),
    # out_path / file_prefix / file_suffix are only needed when a file is
    # actually written, so they stay hidden until to_file or to_upload is
    # ticked - nothing to fill in otherwise
    conditionalPanel(
      paste0("(input.ic_output_format || []).indexOf('to_file') > -1 || ",
             "(input.ic_output_format || []).indexOf('to_upload') > -1"),
      textInput("ic_out_path", "out_path", "./data/data_CEF_format"),
      textInput("ic_prefix", "file_prefix", ""),
      span(class = "note", "Taken from the stock code and year set at start-up."),
      textInput("ic_suffix", "file_suffix", "_CEF_v17.1")),
    tags$hr(), h4("Options"),
    checkboxInput("ic_incl_bms",
      opt_lab("include_BMS_Caton",
              "keep BMS rows in the Caton stack; off drops them"), TRUE),
    checkboxInput("ic_patch_psu",
      opt_lab("patch_missing_DIS_PSU",
              "insert FishingTrip and 0 where a discard domain has no PSU"),
      FALSE),
    checkboxInput("ic_keep_temp",
      opt_lab("keep_temp_file",
              "keep the temporary exchange files funICoutCEF() writes (output format only)"),
      FALSE),
    selectInput("ic_metier6", "metier6",
                c("NULL (MIS_MIS_0_0_0)" = "", "Fleet" = "Fleet")),
    span(class = "note",
         "Fleet copies fleetValue into metier6 and remaps count units to NE3; ",
         "NULL leaves the MIS_MIS_0_0_0 placeholder."),
    tags$hr(),
    actionButton("run_ic", "Run conversion", class = "btn btn-run btn-lg")),
  mainPanel(width = 8,
    h4("Status"), uiOutput("ic_result"))))

# ---------------------------------------------------------
#  A CEF result view, shared by section 1 and section 2
# ---------------------------------------------------------
cef_result_ui <- function(src) {
  ns <- function(x) paste0(src, "_", x)
  tagList(
  span(class = "note",
       "Overview of the three CEF tables (CN: catch, DN: distributions and ",
       "EN: effort)."),
  tabsetPanel(
    tabPanel("catches",
      fluidRow(column(5, h4("Overview"), tbl_slot(ns("cat_sum"))),
               column(7, h4("By catch category"), tbl_slot(ns("cat_cat")))),
      h4("Preview"), tbl_slot(ns("cat_prev"))),
    tabPanel("distributions",
      span(class = "note",
           "Biology is linked to the catch table through domainBiology: only ",
           "catch strata carrying that key, and whose key appears here, have ",
           "been sampled. distributionType says what was measured ",
           "(LengthTotal / Age); variableType says what the value is ",
           "(Number / WeightLive)."),
      fluidRow(column(5, h4("Overview"), tbl_slot(ns("dist_sum"))),
               column(7, h4("What was measured"), tbl_slot(ns("dist_type")))),
      h4("Biology sampling by stratum"),
      fluidRow(column(3, uiOutput(ns("bio_dim_ui"))),
               column(9, tbl_slot(ns("bio_cov")))),
      h4("Preview"), tbl_slot(ns("dist_prev"))),
    tabPanel("effort",
      h4("Overview"), tbl_slot(ns("eff_sum")),
      h4("Preview"), tbl_slot(ns("eff_prev")))))
}

# ---------------------------------------------------------
#  CEF database call: credentials and request
#  Framework only. The actual fetch lives in db_fetch_cef() on the
#  server side; these two panels collect what it will need.
# ---------------------------------------------------------
db_cred_ui <- function() sidebarLayout(
  sidebarPanel(width = 4, class = "simplecard",
    h4("Credentials"),
    textInput("db_user", "Username", ""),
    passwordInput("db_pass", "Password", ""),
    span(class = "note",
         "Used only to authenticate this session's database call; not stored."),
    actionButton("db_connect", "Connect", class = "btn btn-run")),
  mainPanel(width = 8, uiOutput("db_cred_status")))

db_request_ui <- function() sidebarLayout(
  sidebarPanel(width = 4, class = "simplecard",
    h4("Request"),
    textInput("db_stock", "Stock code", "", placeholder = "bll.27.3a47de"),
    numericInput("db_year", "Year",
                 as.integer(format(Sys.Date(), "%Y")) - 1, 1990, 2100, 1),
    tags$hr(),
    h4("CEF tables to request"),
    checkboxInput("db_cn", opt_lab("CN", "catch"), TRUE),
    checkboxInput("db_dn", opt_lab("DN", "distributions"), TRUE),
    checkboxInput("db_en", opt_lab("EN", "effort"), TRUE),
    tags$hr(),
    actionButton("db_request", "Request data", class = "btn btn-run btn-lg")),
  mainPanel(width = 8, load_owner_ui(3), uiOutput("db_request_status")))

# ---------------------------------------------------------
#  Section 2: load the three CEF tables
# ---------------------------------------------------------
cef_load_ui <- function() sidebarLayout(
  sidebarPanel(width = 4, class = "simplecard",
    h4("CEF files"),
    span(class = "note",
         "catches is required. distributions is only needed when lengths or ",
         "ages are being allocated; effort is optional."),
    radioButtons("cef_mode", NULL,
      c("Upload files" = "upload", "Point at a folder" = "dir"), "upload"),
    conditionalPanel("input.cef_mode == 'upload'",
      load_owner_ui(4),
      fileInput("file", "catches", accept = c(".csv", ".txt")),
      uiOutput("cef_dist_input"),
      fileInput("file_eff", "effort (optional)", accept = c(".csv", ".txt")),
      # Selecting a file used to BE the load: raw() read input$file
      # directly, so picking a catches file replaced whatever was already
      # loaded - an InterCatch conversion included - with no warning and
      # no way back. Every other route asks first. This button is what
      # makes the upload route a load rather than a side effect, and it
      # is what the replace-confirmation hangs off.
      span(class = "note",
           "Nothing is read until this is pressed, so all three files can ",
           "be chosen first."),
      actionButton("cef_load_upl", "Load these files",
                   class = "btn-sm btn-primary")),
    conditionalPanel("input.cef_mode == 'dir'",
      load_owner_ui(2),
      dir_picker("cef_dir", "."),
      span(class = "note",
           "The three CEF tables are matched by name: catches, ",
           "distributions, effort."),
      actionButton("cef_load_dir", "Load from folder", class = "btn-sm btn-primary")),
    selectInput("sep", "Separator", c("," = ",", ";" = ";", "tab" = "\t")),
    tags$hr(),
    uiOutput("stock_ui"), uiOutput("year_ui"),
    span(class = "note",
         "Each figure tab carries its own filter, beside the axis pickers."),
    span(class = "note",
         "Sufficiency thresholds are set where they are used: on each ",
         "Sampling coverage tab.")),
  mainPanel(width = 8,
    uiOutput("cfg_banner"),
    uiOutput("data_msg"),
    h4("Files"), uiOutput("cef_file_check"),
    span(class = "note",
         "Raw preview only here - coverage and thresholds come later.")))

# ---------------------------------------------------------
#  Section 3: merged plot tabs
# ---------------------------------------------------------
coverage_ui <- function() tagList(
  control_deck("cov", label = "Sampling coverage controls",
    options = tagList(uiOutput("cov_type_bar")),
    settings = tagList(
      role_chips("cov", c(y = "Y axis", fill = "X / Fill", facet = "Facet"),
                 c("metier4", "Season", "Country"), stacked = TRUE),
      # The threshold belongs with the axis pickers because it is what the
      # colours in the heatmap and bubbles MEAN. The bars view splits
      # weight into sampled and unsampled and never consults a threshold,
      # so showing the control there invites the reader to move it and
      # wonder why nothing happened. The condition mirrors cov_type_eff():
      # with no X / Fill the heatmap and bubbles are unavailable and bars
      # is what is drawn.
      conditionalPanel("input.cov_type != 'bars' && input.cov_fill != ''",
        threshold_box("threshold", NULL, "% coverage",
                      id_trips = "cov_trips"))),
    ),
  deck_page(
    picker = NULL,
    div(class = "legend-strip", plotOutput("cov_legend", height = "150px")),
    filter_banner("cov"),
    plot_slot("cov_plot", capture = TRUE)))

drates_ui <- function() tagList(
  control_deck("dr", label = "Discard rate controls",
    options = tagList(
      pick_bar("dr_type", list(raw = "Raw strata", box = "Boxplot"), "raw")),
    settings = tagList(
      conditionalPanel("input.dr_type != 'box'",
        role_chips("dr", c(x = "X axis", fill = "Colour", shape = "Shape",
                           facet = "Facet"),
                   c("metier4", "ICESDivision", "Season", "Country"),
                   stacked = TRUE),
        checkboxInput("dr_jitter", "Jitter", TRUE)),
      conditionalPanel("input.dr_type == 'box'",
        role_chips("box", c(x = "X axis", fill = "Fill", facet = "Facet"),
                   c("Country", "ICESDivision", ""), stacked = TRUE),
        checkboxInput("box_points", "Show individual strata", TRUE),
        checkboxInput("box_pooled", "Show catch-weighted pooled rate", TRUE))),
    ),
  deck_page(
    picker = NULL,
    div(class = "legend-strip", plotOutput("dr_legend", height = "120px")),
    filter_banner("dr"),
    plot_slot("dr_plot", capture = TRUE),
    tags$hr(), h4("Underlying strata"), tbl_slot("dr_tbl")))

final_ui <- function() tagList(
  control_deck("fin", label = "Adopted allocation controls",
    filter = FALSE, sticky = FALSE, narrow_options = TRUE,
    options = tagList(
      # split by whichever variable is on top of the vocabulary by
      # default: the first question of a final raising is almost always
      # how the catch falls across the fleet, and an empty split opened
      # on the whole-stock total, which looked like one bar
      selectInput("final_alloc", "Adopted allocation", choices = character(0)),
      span(class = "note",
           "The one that feeds the assessment. It is the allocation the ",
           "script records as final; the rest stay as exploration."),
      actionButton("run_final", "Raise with this allocation",
                   class = "btn btn-run btn-block")),
    settings = tagList(
      selectInput("fin_x", "Split by", c("(whole stock)" = ""), ""),
      selectInput("fin_facet", "Facet", NONE, ""),
      tags$hr(style = "margin:6px 0"),
      # catchRaising() settings for the run this button actually
      # triggers. These used to live only on nr_ui() - a "Raise groups"
      # panel that was superseded by this one and never wired into any
      # navigation tree, so the settings it held were unreachable and
      # run_final() silently ran on their defaults (discards /
      # WGEstimate / WeightLive, no log file) regardless of what the
      # data actually needed.
      tags$b("catchRaising settings", style = "font-size:11.5px"),
      selectInput("nr_type", "type", c("discards", "BMS")),
      selectInput("nr_origin", "originType", c("WGEstimate", "Official")),
      selectInput("nr_vartype", "variableType", c("WeightLive", "Number")),
      textInput("nr_log", "logFile", "Log.txt"))),
  deck_page(
    picker = NULL,
    uiOutput("fin_lvl_note"), alloc_box(),
    uiOutput("final_status"),
    tabsetPanel(id = "fin_out", type = "tabs",
      tabPanel("Plot",
        plot_slot("fin_plot", capture = TRUE),
        h4("Imported and raised data overview"), tbl_slot("fin_overview")))))

fragments_ui <- function() fluidPage(fluidRow(
  column(3,
    checkboxGroupInput("code_parts", "Include",
      c("Header" = "head", "Load and derive" = "derive",
        "Exploration plots" = "explore",
        "Allocations and raising" = "auto"),
      selected = c("head", "derive", "explore", "auto")),
    span(class = "note",
         "Same text as the live script; these only choose which blocks ",
         "are shown.")),
  column(9, verbatimTextOutput("code_txt"))))

# ---------------------------------------------------------
#  Length exploration: three views over the same linked data.
#  catchCategory is now part of the stratum keys rather than a
#  tab, so landings and discards can be compared side by side or
#  separated with a facet, exactly like any other dimension.
# ---------------------------------------------------------

# Landings and discards are sampled and allocated separately, so exactly
# one is plotted at a time - a picker, not a filter.
len_cc_filter <- function(ns)
  tagList(h4("Catch category"),
          pick_bar(ns("cc"), list(catlan = "Landings", catdis = "Discards"),
                   "catlan"))

# The sufficiency thresholds, shown where they are used rather than at
# load time. Coverage asks how much of the weight carries a
# distribution; measurements asks how many fish that distribution was
# built on; trips asks how many trips back the sample, independent of
# either. A stratum has to clear all that apply to be shown as SAFE, so
# a domain resting on eight fish from one trip cannot pass on coverage
# alone.
threshold_box <- function(id_cov, id_meas = NULL, cov_label = "% coverage",
                          cov_default = 50, meas_default = 0,
                          id_trips = NULL, trips_default = 2) {
  n <- sum(!is.null(id_meas), !is.null(id_trips)) + 1
  w <- if (n == 1) 12 else if (n == 2) 6 else 4
  tagList(
    h4("Sufficiency"),
    fluidRow(
      column(w, numericInput(id_cov, cov_label, cov_default, 0, 100, 5)),
      if (!is.null(id_trips))
        column(w, numericInput(id_trips, "min trips", trips_default,
                               0, 1000, 1)) else NULL,
      if (!is.null(id_meas))
        column(w, numericInput(id_meas, "min measurements", meas_default,
                               0, 100000, 10)) else NULL),
    span(class = "note",
         if (is.null(id_meas) && is.null(id_trips))
           "Strata at or above this share of sampled weight count as covered."
         else
           paste("Coverage sets the sampling status; trips and measurements",
                 "(where shown) refine it. A stratum short on either is",
                 "flagged as a warning (a lighter shade) rather than",
                 "trusted - it is not dropped, so its coverage still shows.",
                 "The legend below updates to match whatever is set here.")))
}

# --- 3rd level 1: sampling coverage (same shapes as the discards) ---
len_coverage_ui <- function(p, type = "LengthTotal") {
  ns <- function(x) paste0(p, "_cov_", x)
  tagList(
    control_deck(paste0(p, "_cov"), label = "Sampling coverage controls",
      options = tagList(
        len_cc_filter(ns),
        pick_bar(ns("type"),
                 list(heatmap = "Heatmap", bubbles = "Bubbles", bars = "Bars"),
                 "heatmap")),
      settings = tagList(
        role_chips(ns("r"), c(y = "Y axis", fill = "X / Fill", facet = "Facet"),
                   c("metier4", "Country", "ICESDivision"), stacked = TRUE),
        # same reasoning as the discard coverage tab: the two thresholds
        # are what the heatmap and bubble colours mean, nothing to bars
        conditionalPanel(sprintf("input.%s != 'bars'", ns("type")),
          threshold_box(paste0(p, "_thr_cov"), paste0(p, "_thr_meas"),
                        id_trips = paste0(p, "_thr_trips")))),
      ),
    deck_page(
      picker = NULL,
      uiOutput(paste0(p, "_status")),
      div(class = "legend-strip", plotOutput(ns("legend"), height = "150px")),
      filter_banner(ns("")),
      plot_slot(ns("plot"), capture = TRUE),
      tags$hr(), h4("Link summary"), tbl_slot(paste0(p, "_sum"))))
}

# Free the panel scales. Fixed scales keep panels comparable but squash
# a small stratum against a big one; freeing an axis makes the shape
# readable at the cost of comparability, so both are offered.
free_scale_ticks <- function(ns)
  div(style = "display:flex;gap:12px;margin:2px 0 6px 0",
      checkboxInput(ns("freex"), "free x", FALSE),
      checkboxInput(ns("freey"), "free y", FALSE))

# --- 3rd level 2: numbers at length -------------------------------
len_numbers_ui <- function(p, type = "LengthTotal") {
  ns <- function(x) paste0(p, "_n_", x)
  tagList(
    control_deck(paste0(p, "_n"), label = "Numbers at class controls",
      options = tagList(
        len_cc_filter(ns),
        pick_bar(ns("type"), list(hist = "Histogram", ridge = "Density"), "hist")),
      settings = tagList(
        role_chips(ns("r"), c(fill = "Stack / colour", facet = "Facet",
                              shape = "Facet 2"),
                   c("Country", "metier4", ""), stacked = TRUE),
        free_scale_ticks(ns),
        conditionalPanel(sprintf("input.%s != 'ridge'", ns("type")),
          checkboxInput(ns("stack"), "Stack the bars", TRUE)),
        conditionalPanel(sprintf("input.%s == 'ridge'", ns("type")),
          sliderInput(ns("smooth"), "Smoothing", 0.2, 3, 1, 0.1),
          span(class = "note",
               "Turn it down for a well-sampled stratum to keep the detail, ",
               "up for a sparse or noisy one so the shape stays readable."))),
      ),
    deck_page(
      picker = NULL,
      uiOutput(paste0(p, "_status")),
      div(class = "legend-strip", plotOutput(ns("legend"), height = "120px")),
      filter_banner(ns("")),
      plot_slot(ns("plot"), capture = TRUE)))
}

# --- 3rd level 3: mean weight at length ---------------------------
len_meanw_ui <- function(p, type = "LengthTotal") {
  ns <- function(x) paste0(p, "_w_", x)
  tagList(
    control_deck(paste0(p, "_w"), label = "Mean weight controls",
      options = tagList(
        len_cc_filter(ns),
        pick_bar(ns("type"),
                 list(linedot = "Line + dots", box = "Boxplot"), "linedot")),
      settings = tagList(
        role_chips(ns("r"), c(fill = "Colour", facet = "Facet",
                              shape = "Facet 2"),
                   c("metier4", "Country", ""), stacked = TRUE),
        free_scale_ticks(ns),
        selectInput(ns("pt"), "Shape",
                    c("-- none (average the rest) --" = ""), ""),
        checkboxInput(ns("points"), "Show points", TRUE)),
      ),
    deck_page(
      picker = NULL,
      uiOutput(paste0(p, "_status")),
      div(class = "chain", style = "font-size:11.5px;line-height:1.5",
          tags$b("How the points are built"), tags$br(),
          "Whatever is not given a Colour, Facet or Shape is averaged ",
          "over. The mean is weighted by the numbers measured, and the ",
          "band around the dots is one standard deviation across the ",
          "strata that were combined - so the spread hidden by the ",
          "grouping stays visible. Give Shape a variable and those ",
          "strata are drawn separately instead, and the band disappears ",
          "because nothing is being averaged."),
      div(class = "legend-strip", plotOutput(ns("legend"), height = "120px")),
      filter_banner(ns("")),
      plot_slot(ns("plot"), capture = TRUE)))
}

# --- 2nd level: allocation groups ---------------------------------
len_alloc_ui <- function(p, type = "LengthTotal") {
  ns <- function(x) paste0(p, "_ga_", x)
  div(style = "padding:8px 4px",
    pick_bar(ns("method"),
             list(ladder = "Hierarchical", manual = "Manual groups"), "ladder"),
    checkboxInput(ns("bms"), "Treat BMS as discards", TRUE),
    span(class = "note",
         "BMS is a landing in the exchange format but behaves like a ",
         "discard. With this on, any clause naming Dis also picks up BMS."),
    conditionalPanel(sprintf("input.%s != 'manual'", ns("method")),
      builder_deck(ns("hier"), "Hierarchical strata allocation builder",
        numbers = c("1", "2"),
        boxes = list(
          "Define key hierarchy to group strata" = tagList(
            checkboxInput(ns("keepcc"), "Never drop catchCategory", TRUE),
            span(class = "note",
                 "On: the cascade stops at the catch category, so a landings ",
                 "stratum is only ever matched to landings. Off: once the ",
                 "other keys are exhausted the category is dropped too and ",
                 "landings and discards pool together."),
            uiOutput(ns("keys")),
            tags$b("Order actually tested",
                   style = "font-size:11.5px;display:block;margin-top:6px"),
            uiOutput(ns("chain"))),
          "Exclusion rules for donor strata" = tagList(
            span(class = "note",
                 "A separate question from the thresholds beside. Those ",
                 "decide WHICH STRATA a rung resolves; these decide WHICH ",
                 "STRATA MAY LEND their distribution. A stratum failing ",
                 "these bars is still allocated - it is just not used as a ",
                 "donor, which is the whole point of it needing to borrow."),
            tags$b("2.1 Exclude by % sampling coverage",
                   style = "font-size:11.5px;display:block;margin-top:5px"),
            sliderInput(ns("dcov"), "donor % coverage above", 0, 100,
                        c(0, 100), 5, width = "100%"),
            tags$b("2.2 Exclude by number of measurements",
                   style = "font-size:11.5px;display:block;margin-top:5px"),
            uiOutput(ns("dmeas_ui")),
            span(class = "note",
                 "A coverage floor of 0 switches that bar off. These apply ",
                 "to every rung, including the last-resort pool: 'use ",
                 "everything except the strata under 50% coverage and under ",
                 "100 measurements' is 50 and 100 here."))),
        cols = 2,
        actions = tagList(
          # Nothing below recomputes until Apply is pressed. Seven keys,
          # each with four numbers, means twenty-odd controls that would
          # otherwise re-walk the whole cascade on every keystroke -
          # setting up a ladder was slower than running it.
          actionButton(ns("apply"), "Apply settings", class = "btn btn-apply"),
          uiOutput(ns("stale"), inline = TRUE),
          textInput(ns("name"), NULL, "alloc_1", width = "100%"),
          actionButton(ns("save"), "Save as scenario",
                       class = "btn btn-savescn"))),
      div(
          uiOutput(ns("status")),
          tabsetPanel(id = ns("hier_out"), type = "tabs",
                        tabPanel("Overview",
              h4("What each step resolves"),
              span(class = "note",
                   "One row per rung in the order it is tested, with the ",
                   "running total. The grouping column names what each rung ",
                   "still distinguishes."),
              tbl_slot(ns("steps")),
              tags$hr(),
              h4("Strata the ladder never resolved"),
              span(class = "note",
                   "Heaviest first. If this is not empty, either a threshold ",
                   "is too strict or the category must be allowed to drop."),
              tbl_slot(ns("cmissing"))),
            tabPanel("Strata to allocate",
              h4("The data being allocated a distribution"),
              span(class = "note",
                   "One row per stratum: the columns that define it, its ",
                   "catch weight and coverage, and which step of the cascade ",
                   "supplies its length/age distribution."),
              tbl_slot(ns("alloc_raise_tbl"))),
            tabPanel("Data used to allocate",
              h4("The donor pools (bioMatchedCond)"),
              span(class = "note",
                   "The same strata, grouped by how they SUPPLY the ",
                   "distributions. usable_donor is FALSE where a coverage or ",
                   "measurement bar excluded the stratum from the pool."),
              tbl_slot(ns("alloc_donor_tbl"))),
            tabPanel("Rivers",
              h4("Where each step draws its data from"),
              span(class = "note",
                   "One river per rung of the cascade: the strata resolved at ",
                   "that step on the left, the donor pool they drew from on ",
                   "the right."),
              uiOutput(ns("step_rivers"))),
            tabPanel("Code",
              h4("allocStrataCond / bioMatchedCond"),
              span(class = "note",
                   "The grouping this cascade resolves to, as a script - ",
                   "strata clauses written plainly, the partition applied ",
                   "for you."),
              verbatimTextOutput(ns("hier_conds")))))),
    conditionalPanel(sprintf("input.%s == 'manual'", ns("method")),
      builder_deck(ns("mg"), "Manual strata allocation builder",
        numbers = c("1", "2", "4"), grid = "mg-grid",
        boxes = list(
          "Define the groups of strata to allocate" = tagList(
            tags$b("1.1 Name the group",
                   style = "font-size:11.5px;display:block"),
            fluidRow(column(5, textInput(ns("gid"), "Id", "GA1")),
                     column(7, textInput(ns("glab"), "Label", ""))),
            tags$b("1.2 Rules to group",
                   style = "font-size:11.5px;display:block;margin-top:4px"),
            span(class = "note", "A clause is one or more variable ",
                 "restrictions combined with AND. Add them one at a time."),
            div(tags$b("Variable", style = "font-size:11.5px")),
            uiOutput(ns("var_chips")),
            uiOutput(ns("vals")),
            checkboxInput(ns("neg"), "Negate (NOT in)", FALSE),
            div(style = "display:flex;gap:4px",
              actionButton(ns("add"), "Add to clause",
                           class = "btn-xs btn-primary"),
              actionButton(ns("clear"), "Clear clause", class = "btn-xs")),
            uiOutput(ns("clause")),
            tags$b("Overview of the groups defined",
                   style = "font-size:11.5px;display:block;margin-top:6px"),
            uiOutput(ns("order"))),
          "Exclusion rules for donor strata" = tagList(
            span(class = "note",
                 "Narrows bioMatchedCond only - never the strata being ",
                 "allocated. A stratum too thinly measured to lend a ",
                 "distribution is exactly the one that needs to borrow one."),
            exclude_box(ns("x"), metrics = c("coverage", "trips", "measurements"),
                        title = NULL, num = "2",
                        note = paste("Coverage is the share of catch weight",
                                     "carrying a distribution; measurements is",
                                     "the number of fish behind it. A pool can",
                                     "reach 80% coverage on eleven fish, and",
                                     "that is not a distribution to allocate",
                                     "from."))),
          "Include a Catch All by All group" = tagList(
            span(class = "note",
                 "A final group with no clause. Its allocStrataCond is the ",
                 "COMPLEMENT of everything above it, so it allocates only ",
                 "the strata no earlier group claimed - nothing is ",
                 "allocated twice. Its bioMatchedCond is every stratum, ",
                 "including those earlier groups already used: a donor may ",
                 "be reused, only an allocated stratum may not."),
            exclude_box(ns("ca"), metrics = c("coverage", "trips", "measurements"),
                        title = NULL, num = "4",
                        note = paste("The catch-all is the last resort, so it",
                                     "usually wants the widest pool - but a",
                                     "stratum with almost nothing measured is",
                                     "still a bad donor here. Set its own bars",
                                     "rather than inheriting the ones above.")),
            actionButton(ns("addcatchall"), "Add Catch All by All",
                         class = "btn btn-addgrp btn-block",
                         style = "margin-top:6px"))),
        wide = list(
          "3. Assign the clause" = div(
            style = "display:flex;gap:8px;align-items:stretch",
            div(style = "flex:1",
              div(class = "condbox",
                  div(class = "condtitle", "allocStrataCond"),
                  span(class = "note", "strata to allocate"),
                  uiOutput(ns("alloc_txt")),
                  actionButton(ns("to_alloc"), "Put clause here",
                               class = "btn-xs btn-info"))),
            div(style = "flex:0 0 40px;display:flex;flex-direction:column;
                         justify-content:center;gap:5px",
              actionButton(ns("mirror"), HTML("&rarr;"),
                           class = "btn-xs btn-warning",
                           title = "copy to bioMatchedCond"),
              actionButton(ns("mirror_back"), HTML("&larr;"),
                           class = "btn-xs btn-warning",
                           title = "copy back to allocStrataCond")),
            div(style = "flex:1",
              div(class = "condbox",
                  div(class = "condtitle", "bioMatchedCond"),
                  span(class = "note", "sampled data used"),
                  uiOutput(ns("matched_txt")),
                  actionButton(ns("to_matched"), "Put clause here",
                               class = "btn-xs btn-info"))))),
        actions = tagList(
          actionButton(ns("saverule"), "Add group", class = "btn btn-addgrp"),
          textInput(ns("mname"), NULL, "manual_1", width = "100%"),
          actionButton(ns("savemanual"), "Save as scenario",
                       class = "btn btn-savescn"))),
      div(
          uiOutput(ns("bmsnote")),
          tabsetPanel(id = ns("man_out"), type = "tabs",
                        tabPanel("Overview",
              uiOutput(ns("check")),
              h4("What is assigned, and what is still missing"),
              span(class = "note",
                   "One row per group in the order it is tested, with the ",
                   "running total. The last row is whatever no group has ",
                   "claimed yet - the gap to close."),
              tbl_slot(ns("progress")),
              h4("Strata still unassigned"),
              span(class = "note", "Heaviest first, so the biggest hole is on top."),
              tbl_slot(ns("missing")),
),
            tabPanel("Strata to allocate",
              h4("The data being allocated a distribution"),
              span(class = "note",
                   "One row per stratum: the columns that define it, its ",
                   "catch weight and coverage, and which group allocates its ",
                   "length/age distribution. Heaviest first."),
              tbl_slot(ns("man_raise_tbl"))),
            tabPanel("Data used to allocate",
              h4("The donor pools (bioMatchedCond)"),
              span(class = "note",
                   "The same strata, grouped by how they SUPPLY the ",
                   "distributions rather than whether they were allocated. ",
                   "Each stratum can feed several groups."),
              tbl_slot(ns("man_donor_tbl"))),
            tabPanel("Rivers",
              uiOutput(ns("check2")),
              h4("Where each rule draws its data from"),
              span(class = "note",
                   "One river per rule: the strata it allocates on the left, ",
                   "the donor pool it draws from on the right."),
              uiOutput(ns("rule_rivers"))),
            tabPanel("Code",
              h4("allocStrataCond / bioMatchedCond"),
              span(class = "note",
                   "The two lists as they would be written in a script - ",
                   "strata clauses written plainly (first match wins), the ",
                   "partition applied for you; donor conditions as metric ",
                   "thresholds, not stratum lists."),
              verbatimTextOutput(ns("conds")))))))
}

# --- 2nd level: raising ------------------------------------------
len_raise_ui <- function(p, what = "Length") {
  ns <- function(x) paste0(p, "_rz_", x)
  div(style = "padding:8px 4px",
    # same two-step shape as the discard raising: compare first, adopt one
    pick_bar(ns("mode"), list(stgexpl = "Compare allocations",
                              stgraise = "Final allocation"), "stgraise"),
    tags$hr(),
    conditionalPanel(sprintf("input.%s == 'stgexpl'", ns("mode")),
      # Same shape as the discard side's compare-allocation tab:
      # settings in a plain sidebarPanel on the left, output on the
      # right - no deck, no filter box. A filter here would apply to
      # the COMPARISON PLOT, but there is nothing to filter a
      # cross-allocation comparison BY that is not already Facet /
      # Facet 2 - every allocation's own strata definition already
      # decided what data feeds it, before comparison ever runs.
      sidebarLayout(
        sidebarPanel(width = 3, class = "simplecard",
          h4("Comparison plot"),
          # The three views are not alternatives. Numbers, stock weight
          # and mean weight are the three things an allocation can
          # change, and the question "did this grouping matter" is only
          # answerable by seeing all three: two groupings routinely
          # agree on numbers and still put different weights on the
          # same classes. Making them a radio meant the second and
          # third were, in practice, never looked at.
          role_chips(ns("cmpr"), c(facet = "Facet", shape = "Facet 2"),
                     c("catchCategory", "")),
          free_scale_ticks(ns),
          numericInput(ns("cmpncol"), "Facet columns", 2, 1, 6, 1),
          apply_bar(ns("cmp")),
          tags$hr(),
          span(class = "note",
               "Every saved allocation is re-run against the data currently ",
               "loaded, so the comparison is like-for-like."),
          actionButton(ns("runall"), "Run all allocations",
                       class = "btn btn-run btn-lg")),
        mainPanel(width = 9,
          uiOutput(ns("cmpstatus")),
          h4("Totals by allocation"), tbl_slot(ns("cmptot")),
          div(class = "legend-strip", plotOutput(ns("cmplegend"), height = "90px")),
          h4("Numbers at class"),
          plot_slot(ns("cmpplot"), capture = TRUE),
          h4("Stock weight (tonnes)"),
          plot_slot(ns("cmpplotw"), capture = TRUE),
          h4("Mean weight (g)"),
          span(class = "note",
               "Derived as tonnes / numbers and re-weighted by the numbers ",
               "behind each class, so it cannot disagree with the two above."),
          plot_slot(ns("cmpplotmw"), capture = TRUE)))),
    conditionalPanel(sprintf("input.%s == 'stgraise'", ns("mode")),
  sidebarLayout(
    sidebarPanel(width = 3, class = "simplecard",
      h4("Allocation to apply"),
      selectInput(ns("alloc"), NULL, choices = character(0)),
      span(class = "note",
           "Both the hierarchical and the manual builders save here."),
      tags$hr(),
      h4("catchAllocationAoL settings"),
      selectInput(ns("origin"), "originType_catch",
                  c("WGEstimate", "Official"), "WGEstimate"),
      selectInput(ns("vartype"), "variableType_catch",
                  c("WeightLive", "Number"), "WeightLive"),
      selectInput(ns("weighting"), "weighting",
                  c("NumberAtAoL", "CATON"), "NumberAtAoL"),
      tags$hr(),
      actionButton(ns("run"), sprintf("Allocate %s", tolower(what)),
                   class = "btn btn-run btn-lg"),
      tags$hr(),
      h4("Aggregation"),
      selectInput(ns("group"), "Group by", c("catchCategory", NONE),
                  "catchCategory"),
      numericInput(ns("minaol"), sprintf("min %s", what), 0, 0, 100, 1),
      numericInput(ns("maxaol"), sprintf("max %s (blank = all)", what),
                   NA, 0, 1000, 1),
      checkboxInput(ns("plus"), "plus group", FALSE),
      apply_bar(ns("fig"))),
    mainPanel(width = 9,
      uiOutput(ns("raisewarn")),
      uiOutput(ns("status")),
      h4("Imported vs allocated"), tbl_slot(ns("overview")),
      div(class = "legend-strip", plotOutput(ns("legend"), height = "90px")),
      h4(sprintf("Numbers at %s", tolower(what))),
      plot_slot(ns("nplot"), capture = TRUE),
      h4(sprintf("Stock weight at %s (tonnes)", tolower(what))),
      plot_slot(ns("wplot"), capture = TRUE),
      tags$hr(),
      h4("Aggregated numbers"), tbl_slot(ns("ntab")),
      h4("Aggregated mean weights"), tbl_slot(ns("wtab"))))))
}

# ---------------------------------------------------------
#  Length / Age helpers
# ---------------------------------------------------------
# (sampling_stub / grouping_stub / alloc_stub / stub_ui / stub_note were
#  placeholders drawn before the length and age sections were built out.
#  Nothing calls them any more - len_coverage_ui(), the builder_deck()
#  cascades and len_alloc_ui() replaced them - so they are removed rather
#  than carried as dead weight.)

# A clause you can pick up and drop into either box. The buttons stay as
# a fallback, since drag-and-drop is awkward on a trackpad.
dropzone <- function(which) tags$div(
  id = paste0("drop_", which), class = "dropzone",
  ondragover = "event.preventDefault(); this.classList.add('over')",
  ondragleave = "this.classList.remove('over')",
  ondrop = sprintf(
    "event.preventDefault(); this.classList.remove('over');
     Shiny.setInputValue('drop_target','%s',{priority:'event'})", which),
  uiOutput(paste0(which, "_txt_ui")))

# Variable chips: the ones that matter are shown, the rest are one click
# away, rather than every column being dumped into a dropdown.
var_chips <- function(id, shown, extra, selected) {
  tagList(
    div(lapply(shown, function(v)
      tags$span(class = paste("varchip", if (identical(v, selected)) "on" else ""),
                onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})",
                                  id, v), v))),
    if (length(extra))
      tags$details(style = "margin-top:4px",
        tags$summary(class = "note", style = "cursor:pointer",
                     sprintf("more variables (%s)", length(extra))),
        div(lapply(extra, function(v)
          tags$span(class = paste("varchip", if (identical(v, selected)) "on" else ""),
                    onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})",
                                      id, v), v))))
    else NULL)
}



stub_ui <- function(kind, what) {
  lbl <- if (kind == "length") "Length" else "Age"
  var <- if (kind == "length") "length class" else "age class"
  body <- switch(what,
    provision = list(
      "Same shape as the discard sampling tabs, with one extra stratum ",
      "dimension: catchCategory. Landings and discards are sampled ",
      "separately, so a stratum here is fleet x country x area x season x ",
      "catch category. The coverage question becomes: which strata have a ",
      "distribution at all?"),
    dist = list(
      "Replaces the discard-rate view. Instead of one rate per stratum ",
      "there are two curves: numbers at ", var, " and mean weight at ",
      var, ". The equivalent of the boxplot is a set of distributions ",
      "overlaid by stratum, so you can see whether pooling two strata ",
      "would merge compatible shapes or smear two different ones together."),
    groups = list(
      "The same two builders as the discard section - a threshold cascade ",
      "and a manual rule table - producing strataCond and matchedDataCond. ",
      "The rules will need different criteria: number of samples and ",
      "number of measurements rather than a sampled fraction of landings."),
    alloc = list(
      "Applies the groups through the age-length allocation functions ",
      "and compares scenarios, as the discard section does."))
  div(style = "padding:18px;max-width:760px",
      h3(paste(lbl, "-", tools::toTitleCase(what))),
      div(class = "chain", style = "font-size:13px;line-height:1.5",
          tags$b("Not built yet."), tags$br(), do.call(tagList, body)),
      div(class = "note", style = "margin-top:12px",
          "The stratum machinery (build_strata, the role slots, the group ",
          "builders, the scenario store and the script log) is shared, so ",
          "this section reuses it rather than duplicating it."))
}

# A copy-to-clipboard button for a given verbatimTextOutput id. Factored out
# because every one of the seven script views needs one.
copy_btn <- function(id) tags$button(
  "Copy to clipboard", class = "btn btn-sm btn-primary",
  style = "margin-bottom:6px",
  onclick = sprintf(
    "var t=document.getElementById('%s').innerText;
     navigator.clipboard.writeText(t);
     var b=this; b.innerHTML='Copied';
     setTimeout(function(){b.innerHTML='Copy to clipboard';},1400);", id))

# ---------------------------------------------------------
#  The TAF script set
# ---------------------------------------------------------
# One tab per file. Each shows only that file, because the whole point of
# the TAF split is that a reader can open the stage they care about without
# reading the other six - a reviewer checking why a group was drawn opens
# model_explore_discards.R and stops there.

taf_ui <- function(part) {
  fid <- paste0("taf_", part)
  fluidPage(fluidRow(
    column(3,
      div(class = "tafhead",
          div(class = "taffile", TAF_FILES[[part]]),
          div(class = "note", TAF_BLURB[[part]])),
      downloadButton(paste0("dl_", fid),
                     sprintf("Download %s", TAF_FILES[[part]]),
                     class = "btn-sm btn-primary")),
    column(9, copy_btn(paste0(fid, "_txt")),
           verbatimTextOutput(paste0(fid, "_txt")))))
}

# The kept-plot list, with a way to drop one again. Shown beside output.R
# because that is the file it controls: this list IS the contents of
# output.R, in this order.
# Lives on the whole-set view rather than on one file's tab, because the
# kept plots no longer all go to one file: an exploratory figure is written
# into the model script that computes what it shows, a result figure into
# output.R. Each row says where it will land.
kept_box <- function() tagList(
  h4("Plots kept"),
  span(class = "note",
       "Each 'Keep for script' press adds a figure block to the script the ",
       "plot belongs to. Keeping the same plot twice with different ",
       "settings is deliberate - both blocks are written. Every figure is ",
       "saved into output/ wherever it is drawn from."),
  uiOutput("kept_list"),
  actionButton("kept_clear", "Clear all", class = "btn-xs btn-warning"))

log_ui <- function() {
  fluidPage(fluidRow(
    column(3,
      h4("Write the set"),
      textInput("log_dir", "Folder", "."),
      checkboxInput("log_auto", "Write on every change", FALSE),
      actionButton("log_write_now", "Write all 7 files now",
                   class = "btn-sm btn-primary"),
      span(class = "note",
           "Writes data.R, the four model scripts, output.R and report.R ",
           "into the folder above, ready to run in place."),
      tags$hr(),
      h4("Download"),
      checkboxGroupInput("taf_parts", NULL,
        setNames(names(TAF_FILES), unname(TAF_FILES)),
        selected = names(TAF_FILES)),
      downloadButton("dl_taf_sel", "Download ticked (.zip)",
                     class = "btn-sm btn-primary"),
      downloadButton("dl_taf_all", "Download all (.zip)", class = "btn-sm"),
      tags$hr(),
      downloadButton("dl_log", "Download as one .R file", class = "btn-sm"),
      span(class = "note",
           "The same seven scripts concatenated, for reading in one go."),
      tags$hr(),
      kept_box(),
      tags$hr(),
      h4("Session state"),
      span(class = "note",
           "The scripts are for reading and re-running outside the app. ",
           "The state file is what the app reads back to refill every ",
           "control."),
      downloadButton("dl_state", "Download state (.json)"),
      fileInput("up_state", "Restore state", accept = ".json"),
      uiOutput("state_status")),
    column(9, copy_btn("log_txt"), verbatimTextOutput("log_txt"))))
}

# ------------------------------------------------------------
#  The control deck: four boxes across the top of a plot tab
# ------------------------------------------------------------
#
# The sidebar was losing the argument. Every figure tab carried the axis
# pickers, the figure filter, the level of detail and the Apply gate in a
# 3-wide column, which left the plot 9 columns wide and pushed the level
# of detail so far down the page that it was routinely scrolled past -
# and it is the one control that silently changes what everything else on
# the tab MEANS.
#
# So the four move to a strip across the top, in the order they are
# reasoned about: what am I plotting (settings), on what subset (filter),
# at what resolution (level), go (apply). The strip is sticky, so it
# stays put while the plots scroll underneath it, and it collapses to a
# one-line bar when it is in the way of a tall figure.
#
# Only the plot-type picker stays on the left, because it is a choice of
# WHICH figure rather than a setting OF the figure, and it is three
# icons wide.
#
# The ids are unchanged. filter_box(p) and apply_bar(p) are the same
# calls in the same places they always were, just re-parented, so every
# observer, gate and renderer behind them is untouched.

# Collapse is pure client-side. A Shiny input round-trip to hide a div
# would invalidate the panel's reactives and redraw the plot underneath,
# which is the opposite of what "get this out of my way" means.
deck_js <- "
function rcefDeck(id){
  var d = document.getElementById(id);
  if (!d) return;
  d.classList.toggle('mini');
  var b = d.querySelector('.deckbtn');
  if (b) b.innerHTML = d.classList.contains('mini') ? '&#9660;' : '&#9650;';
}"

deck_box <- function(title, ..., cls = "") div(class = paste("deckbox", cls),
  div(class = "decktitle", title), div(class = "deckbody", ...))

# A builder deck: the numbered steps across the top, then one action bar.
#
# The grouping builders had their whole workflow in a 3-wide sidebar -
# method note, keys with their sliders, donor bars, scenario name - so
# the steps ran off the bottom of the screen and the output tabs beside
# them were a third of the width they needed. Worse, the order was only
# implied by vertical position, and step 3 was usually below the fold.
#
# Numbered boxes across the top make the sequence the layout: read left
# to right, act on the bar underneath. Same sticky/minimise behaviour as
# the figure decks, so it can be pushed out of the way once the groups
# are settled and the reader is working through the output tabs.
# ------------------------------------------------------------
#  Definition of the strata
# ------------------------------------------------------------
#
# What a "stratum" IS, for a builder. Before this there was one answer:
# the raw uploaded stratum - Country + fleetValue + areaValue + Season,
# the four native CEF keys - was the only resolution "own data" could
# mean, and a manual group's exclusion bars were measured at that same
# raw resolution. A single uploaded stratum is never itself split by a
# discard sample, so its coverage is always exactly 0% or 100% - there
# is no in-between until strata are grouped into something coarser.
#
# This box is that grouping. "Use as provided" keeps the four native
# keys - the resolution the data was submitted at, unchanged. "Customize"
# offers five slots drawn from the same vocabulary as everything else in
# the workbench: swap fleetValue for metier4, drop Area or Country
# entirely, leave a slot empty for a coarser definition than the
# default. Whatever is resolved becomes what "own stratum" means to a
# hierarchical cascade, and the resolution a manual group's donor pool is
# measured at.
DEFAULT_STRATA_VARS <- c("Country", "fleetValue", "areaValue", "Season")

# p is a full, unique prefix - callers with more than one strata
# definition on a page (length/age, which has an independent one for
# the hierarchical builder and the manual builder) pass distinct
# prefixes, not the panel's bare id, or the two would collide on the
# same input ids.
strata_def_ui <- function(p) {
  ns <- function(x) paste0(p, "_", x)
  slot <- function(i) column(2, selectInput(ns(paste0("sd", i)),
    sprintf("Variable %d", i), NONE, DEFAULT_STRATA_VARS[i] %||% "",
    width = "100%"))
  tagList(
    radioButtons(ns("sdmode"), NULL,
                 c("Use as provided" = "asis",
                   "Customize strata definition" = "custom"),
                 "asis", inline = TRUE),
    conditionalPanel(sprintf("input.%s == 'asis'", ns("sdmode")),
      span(class = "note",
           sprintf("The four native CEF keys, as submitted: %s.",
                   paste(DEFAULT_STRATA_VARS, collapse = ", ")))),
    conditionalPanel(sprintf("input.%s == 'custom'", ns("sdmode")),
      span(class = "note",
           "Up to five variables. A slot left at \"-- none --\" is simply ",
           "not used - a coarser definition than the default, never a ",
           "narrower one. This changes what every group and every rung ",
           "below is built on."),
      fluidRow(slot(1), slot(2), slot(3), slot(4), slot(5))),
    uiOutput(ns("sd_summary")))
}


builder_deck <- function(id, label, boxes, actions, cols = NULL,
                         wide = NULL, numbers = NULL, grid = NULL,
                         top = NULL) {
  did <- paste0(id, "_bdeck")
  # Step numbers are usually 1..n, but a deck may want to keep the
  # numbering of a longer workflow - the manual builder's catch-all is
  # step 5 even though it is the third box, because the reader meets
  # steps 3 and 4 below the columns.
  nm <- function(i) sprintf("%s. %s",
                            if (is.null(numbers)) i else numbers[i],
                            names(boxes)[i])
  # The manual builder has more steps than the cascade, so the column
  # count follows the number of boxes rather than being fixed at three.
  n <- cols %||% length(boxes)
  div(class = "deckwrap bdeck", id = did,
    div(class = "deckbar",
      tags$span(class = "decklabel", label),
      tags$button(class = "deckbtn", HTML("&#9650;"), title = "Minimise / expand",
                  onclick = sprintf("rcefDeck('%s')", did))),
    div(class = "deckbody-wrap",
    # The strata definition, when given, is step 1 and precedes every
    # other box - what a group or a rung IS depends on it, so it has to
    # be read and set before the reader even reaches "define the groups"
    # or "select keys". Full width regardless of how many columns the
    # rest of the deck has: it is one decision, not one column's worth.
    if (!is.null(top)) div(class = "deckstrata",
      deck_box(sprintf("1. %s", names(top)[1]), top[[1]],
               cls = "deck-step deck-step-strata")),
    # The wide step goes INSIDE the grid, not after it, so a named-area
    # layout can place it under some columns and leave another running
    # full height beside it. Assigning a clause means reading strataCond
    # and matchedDataCond against each other, and squeezed into a third
    # of the deck the two boxes are too narrow to compare.
    div(class = paste("deckgrid bdeckgrid", paste0("bdeck-", n), grid),
      lapply(seq_along(boxes), function(i)
        deck_box(nm(i), boxes[[i]],
                 cls = paste0("deck-step deck-step-", i))),
      if (!is.null(wide)) div(class = "deckwide",
        deck_box(names(wide)[1], wide[[1]],
                 cls = "deck-step deck-step-wide"))),
    # Inside the collapsible region, not after it. Apply / Add group /
    # Save as scenario belong to the builder, so minimising the builder
    # to read the output should take them with it - left outside, a
    # collapsed deck still occupied two rows of the screen.
    div(class = "deckapply bdeckapply", actions)))
}

#' One sticky strip of controls above a figure.
#'
#' @param p        The panel prefix. filter_box(p) / apply_bar(p) hang off it.
#' @param settings What goes in box 1: the axis chips, and the threshold
#'                 where the figure has one.
#' @param level    The level-of-detail box, already built - the prefix
#'                 differs from `p` on the length and age tabs, where one
#'                 box serves three figures.
#' @param label    Shown on the collapsed bar, so a minimised deck still
#'                 says which panel it belongs to.
control_deck <- function(p, settings, level = NULL, label = "Plot controls",
                         options = NULL, filter = TRUE, sticky = TRUE,
                         narrow_options = FALSE) {
  id <- paste0(p, "_deck")
  div(class = paste("deckwrap", if (!sticky) "nostick"), id = id,
    div(class = "deckbar",
      tags$span(class = "decklabel", label),
      tags$button(class = "deckbtn", HTML("&#9650;"), title = "Minimise / expand",
                  onclick = sprintf("rcefDeck('%s')", id))),
    # Three boxes across, then Apply as one bar underneath them. Apply
    # is not a fourth setting - it acts on all three - so a narrow column
    # beside them said the wrong thing about what it does, and left a
    # tall empty box next to three full ones.
    # Order of reading: which figure, then what is on its axes, then on
    # what subset. The plot-type picker moved off the left rail and into
    # box 1 because it is the first decision, not a permanent fixture
    # beside the figure. There is no "level of detail" box any more - the
    # axes name real columns, so the resolution IS the axis choice.
    # filter = FALSE drops the third box entirely - the adopted-raising
    # tab has no per-figure filter server wired to it (there is nothing
    # to filter a crosstab BY that isn't already a Split/Rows/Columns
    # choice), so the box rendered as three dead buttons.
    # sticky = FALSE is for a deck sitting above a TALL plot: pinned
    # under the navbar, the plot scrolls in underneath it and stays
    # hidden there, which is the opposite of "get out of the way" - a
    # short summary plot benefits from staying visible while scrolling
    # past it; a tall one just gets permanently obscured.
    div(class = paste("deckgrid",
                      if (is.null(options) && !filter) "deck1"
                      else if (is.null(options) || !filter) "deck2" else "deck3",
                      if (narrow_options) "deck-narrowopt"),
      if (!is.null(options)) deck_box("Plot options", options, cls = "deck-opt"),
      deck_box("Plot settings", settings, cls = "deck-set"),
      if (filter) deck_box("Filter this figure", uiOutput(paste0(p, "_flt_chips")),
               uiOutput(paste0(p, "_flt_vals")),
               div(style = "display:flex;gap:4px;margin-top:4px",
                   actionButton(paste0(p, "_flt_add"), "Add filter",
                                class = "btn-xs btn-primary"),
                   actionButton(paste0(p, "_flt_clear"), "Clear all",
                                class = "btn-xs")),
               uiOutput(paste0(p, "_flt_list")),
               uiOutput(paste0(p, "_flt_note")), cls = "deck-flt")),
    div(class = "deckapply",
      actionButton(paste0(p, "_apply"), "Apply",
                   class = "btn btn-apply btn-block"),
      uiOutput(paste0(p, "_apply_note"), inline = TRUE)))
}

# The body of a deck-topped tab: a narrow plot-type rail on the left and
# everything else given over to the figure.
# With the plot-type picker now inside the deck, the left rail is empty
# on most tabs - so the figure takes the full width unless a tab still
# passes something for it.
deck_page <- function(picker = NULL, ...) {
  if (is.null(picker)) return(div(class = "plotarea", ...))
  fluidRow(column(2, class = "plotrail", picker),
           column(10, class = "plotarea", ...))
}

# Retained only as the fallback list the pickers show before data lands;
# nothing resolves through it any more. Selections ARE column names now,
# so these are real columns rather than the old abstract dimension
# labels - a control still emitting "Fleet" would name a column that
# does not exist.
DIMS <- c("metier4", "Country", "ICESDivision", "Season")
# catchCategory is a stratum key in the length/age sections
LEN_DIMS <- c("catchCategory", DIMS)
# The empty option every role chip carries. It used to be blank, so
# clearing a facet meant selecting a row with nothing in it and hoping -
# the label makes it an option you can aim at, and the CSS below tints it
# so it does not read as the name of a variable.
NONE <- c("-- none --" = "")

# ------------------------------------------------------------
#  Small UI factories
# ------------------------------------------------------------

# Per-tab level-of-detail box, pinned to the bottom of each sidebar.
# Pinned to the bottom-right of the window and highlighted, because it
# silently changes what every other control on the tab means.
# A filter that belongs to ONE figure tab, and lives beside the axis
# pickers because that is where it gets used: pick metier4 on the Y axis,
# see the spread, then pin metier4 to TBB and look again. Walking back to
# the Data tab to do that broke the loop.
#
# Temporary by design. It is not stored in the scenario, it does not
# survive a tab change, and it never touches dat() - the grouping and the
# raising always run on the whole table. The box is coloured like a
# warning for that reason.
# The exclusion block, in one place because it now appears four times:
# the discard cascade, the discard manual rules, the discard catch-all
# and the biology manual rules. Written out four times it drifted; the
# discard tab grew a "% coverage" box that the biology tab never got.
#
# `metrics` says which tests this instance offers. Discard rate is
# meaningless for a biology pool and measurement count is meaningless for
# a discard pool, so neither is shown where it does not apply.
exclude_box <- function(p, metrics = c("coverage", "measurements", "rate"),
                        cov_default = NA, meas_default = NA,
                        title = "Exclude donors", note = NULL,
                        num = NULL, rate_ui = NULL) {
  n <- function(x) paste0(p, "_x", x)
  # Numbered sub-headings (2.1, 2.2, ...) when the caller gives a stem,
  # so the box reads as a checklist rather than a stack of unrelated
  # controls. Plain labels otherwise, for the places that are not a
  # numbered step.
  k <- 0
  sub <- function(txt) {
    k <<- k + 1
    tags$b(if (is.null(num)) txt else sprintf("%s.%d %s", num, k, txt),
           style = "font-size:11.5px;display:block;margin-top:5px")
  }
  covdef <- if (is.na(cov_default)) 0 else cov_default
  # The discard-rate control belongs to the caller (its bounds depend on
  # the data), so it is passed in and numbered here with the rest.
  rate_blk <- if (!is.null(rate_ui))
    tagList(sub("Exclude by discard rate"), rate_ui)
  cov_ui <- if ("coverage" %in% metrics)
    # min-only: the handle is the coverage floor, blue runs up to 100. A
    # stratum below the handle is dropped from the donor pool.
    tagList(sub("Exclude by % sampling coverage"),
            sliderInput(n("cov"), "% coverage below", 0, 100,
                        c(covdef, 100), 5, width = "100%"))
  # A min-only slider, like the coverage one: the handle is the floor and
  # the blue runs up to the ceiling, so the shaded side is the side that
  # QUALIFIES. The ceiling is the largest measurement count present, so
  # the scale fits the data instead of running to an arbitrary 100000
  # that makes the useful range a few pixels wide. Rendered server-side
  # for that reason - the maximum is not known until the data is loaded.
  meas_ui <- if ("measurements" %in% metrics)
    tagList(sub("Exclude by number of measurements"),
            uiOutput(n("meas_ui")))
  # Same reasoning as measurements: a fixed slider range would put every
  # real trip count in a sliver of the track, so the ceiling is rendered
  # server-side from the largest count actually present. Offered
  # wherever coverage is - trips are a discard concept as much as a
  # biology one, unlike measurements, which only length/age strata have.
  trips_ui <- if ("trips" %in% metrics)
    tagList(sub("Exclude by number of trips"),
            uiOutput(n("trips_ui")))
  ord <- c("Weight" = "weight",
           if ("coverage" %in% metrics) c("Coverage" = "coverage"),
           # Gated on the CONTROL being present (rate_ui), not on a
           # "rate" %in% metrics flag: every caller passes a discard-rate
           # slider via rate_ui, but none of them ever added "rate" to
           # metrics, so this option never appeared - the sort-by-rate
           # feature was there in code but unreachable from any screen.
           if (!is.null(rate_ui)) c("Discard rate" = "rate"),
           if ("measurements" %in% metrics) c("Measurements" = "measurements"),
           if ("trips" %in% metrics) c("Trips" = "trips"))
  tagList(
    h4(title),
    if (!is.null(note)) span(class = "note", note),
    span(class = "note",
         "Blank switches a test off. A stratum kept out of a donor pool ",
         "is still raised or allocated itself - being too thin to lend is ",
         "exactly the reason to borrow."),
    rate_blk, cov_ui, trips_ui, meas_ui,
    sub("Exclude a specific stratum"),
    span(class = "note",
         "By expert judgement, at either resolution. Picking from BOTH ",
         "lists is fine - an outlier spotted in the raw submission and a ",
         "pooled group that never reached its coverage bar are different ",
         "kinds of decision, and either can apply."),
    radioButtons(n("sort"), "Order the list by", ord, ord[[1]], inline = TRUE),
    tags$b("As submitted", style = "font-size:11px;display:block;margin-top:4px"),
    span(class = "note",
         "The four native CEF keys - the resolution coverage is always ",
         "either 0% or 100% at, since a submitted row is never itself ",
         "split by a sample."),
    uiOutput(n("ui_sub")),
    tags$b("As aggregated",
           style = "font-size:11px;display:block;margin-top:6px"),
    span(class = "note",
         "Whatever the Definition of the strata box resolved to. A group ",
         "here can be partially covered, which the submitted list above ",
         "cannot show."),
    uiOutput(n("ui")),
    uiOutput(n("note")),
    tags$b("Summary of currently excluded strata",
           style = "font-size:11.5px;display:block;margin-top:6px"),
    tbl_slot(n("tbl")),
    tags$b("Summary of data that will be used to raise",
           style = "font-size:11.5px;display:block;margin-top:8px"),
    span(class = "note",
         "The donor pool as it stands: everything matching matchedDataCond, ",
         "after every bar above. A warning here means the pool that is ",
         "left may not be enough to raise from."),
    uiOutput(n("used_warn")),
    tbl_slot(n("used_tbl")))
}

# Built like the clause builder on the grouping tabs: pick a variable
# from the chips, pick values, add. Listing every filterable column as a
# dropdown from the start filled the sidebar with a dozen empty boxes,
# eleven of which were never going to be used.
# Shown ABOVE the figure, not only beside the controls. A filter set on
# one visit and forgotten on the next reads as a finding rather than as a
# view, and the sidebar is usually scrolled away from the plot.
filter_banner <- function(p) uiOutput(paste0(p, "_flt_banner"))

filter_box <- function(p) div(class = "fltbox",
  div(class = "flttitle", "Filter this figure"),
  uiOutput(paste0(p, "_flt_chips")),
  uiOutput(paste0(p, "_flt_vals")),
  div(style = "display:flex;gap:4px;margin-top:4px",
    actionButton(paste0(p, "_flt_add"), "Add filter",
                 class = "btn-xs btn-primary"),
    actionButton(paste0(p, "_flt_clear"), "Clear all", class = "btn-xs")),
  uiOutput(paste0(p, "_flt_list")),
  uiOutput(paste0(p, "_flt_note")))

# Plot slot whose container height is set from the server, so tables and
# other plots are pushed down instead of being drawn over.
# A plot slot, optionally with its own two-button toolbar.
#
# "Keep for script" is the answer to a real problem with the emitted script:
# the app can draw the same coverage a dozen ways in a minute, but only the
# last configuration survives into the log, so the version that was actually
# worth having is usually the one that got overwritten. Keeping is explicit
# and additive - press it on each cut you want and all of them end up in
# output.R, in the order you kept them.
#
# "Save image" is the other half: the picture itself, at print resolution,
# without going through the script at all.
#
# capture = FALSE for the diagnostic plots (the rivers, the step views),
# which illustrate a decision rather than reporting a result and have no
# business in output.R.
# The toolbar is pinned to the top-right CORNER of the plot area rather
# than stacked above it. A bar in the flow pushed every plot down by its own
# height and put a strip of chrome between the legend and the thing it
# describes; in the corner it costs no vertical space and sits over the
# plot's own margin, which is empty in every one of these panels.
plot_slot <- function(id, capture = FALSE)
  div(class = if (isTRUE(capture)) "plotwrap has-tools" else "plotwrap",
      if (isTRUE(capture)) plot_tools(id),
      uiOutput(paste0(id, "_ui")))

# The "which route owns the data" banner. Shiny binds one output id once,
# so each place that shows it needs its own id; the server fills them all
# from the same renderer.
LOAD_OWNER_SLOTS <- 1:4
load_owner_ui <- function(k) uiOutput(paste0("load_owner_", k))

# Apply gate for a plot panel.
#
# Every control on these panels is reactive, so dragging a slider or walking
# a dropdown fires a redraw per step - on a stock with a few hundred
# thousand catch rows that is several seconds of work per keystroke, all of
# it thrown away. The panel therefore draws from a FROZEN set of settings,
# and this button is what refreshes them.
#
# The plot still redraws by itself when the DATA changes, because that is
# not a control change and making the user press a button to see their own
# newly loaded file would be obtuse.
apply_bar <- function(p, label = "Apply") div(class = "applybar",
  actionButton(paste0(p, "_apply"), label, class = "btn btn-sm btn-apply"),
  uiOutput(paste0(p, "_apply_note"), inline = TRUE))

plot_tools <- function(id) div(class = "plottools",
  # title= rather than a note beside them: the explanation was only ever
  # read once, and in the corner there is no room for a sentence
  tags$span(title = paste("Adds this plot, with its current settings, to the",
                          "R script. Several cuts of the same plot can be",
                          "kept - each becomes its own figure block."),
    actionButton(paste0(id, "_keep"), "Keep for script",
                 class = "btn btn-xs btn-keep")),
  tags$span(title = "Save this plot as a PNG at print resolution.",
    downloadButton(paste0(id, "_png"), "Save image",
                   class = "btn btn-xs btn-img")))
tbl_slot  <- function(id) div(class = "dtwrap", DTOutput(id))

# Colored role "chips" that are themselves the variable selectors: each is
# a compact dropdown tinted by role. Laid out in a row by default, or
# stacked vertically (stacked=TRUE) when they live in the narrow sidebar.
# Every chip carries the shared NONE option so a role can be left empty
# without having to clear the box by hand.
ROLE_COL <- c(y = "#59A14F", x = "#59A14F", fill = "#F28E2B",
              facet = "#E15759", shape = "#B07AA1", colour = "#F28E2B")
role_chips <- function(p, roles, defaults,
                       choices = NULL,
                       stacked = FALSE) {
  # choices = NULL means "every variable in the loaded table", filled in
  # by role_choices_server() once the data lands. Declaring them here
  # would freeze the vocabulary at whatever the four dimensions used to
  # be, which is the indirection this release removes.
  if (is.null(choices)) choices <- NONE
  div(class = if (stacked) "rolechips rolechips-stacked" else "rolechips",
    lapply(seq_along(roles), function(i) {
      rk <- names(roles)[i]
      col <- ROLE_COL[[rk]] %||% "#777"
      div(class = "rolechip-wrap",
        tags$label(class = "rolechip-lab",
                   style = sprintf("background:%s", col), roles[[i]]),
        selectInput(paste0(p, "_", rk), NULL, choices, defaults[i],
                    width = if (stacked) "100%" else "130px"))
    }))
}

# ============================================================
#  UI
# ============================================================
ui <- navbarPage(
  "RCEF Workbench", id = "nav",
  header = tags$head(tags$style(HTML("
    .well{padding:10px} .form-group{margin-bottom:6px}
    .note{color:#666;font-size:11px;display:block;margin:3px 0 6px 0}
    h4{margin-top:6px;font-size:15px}
    .btn-sm{margin:2px 2px 6px 0}
    .chain{background:#fcfcfd;border:1px solid #e3e3e8;border-radius:6px;
           padding:6px;font-size:12px;margin:6px 0}
    .clausebox{background:#fff;border:1px solid #ddd;border-radius:6px;
               padding:6px;min-height:52px;font-family:monospace;font-size:11px;
               margin-bottom:6px}
    /* plots get their height from renderUI, so nothing can overlap */
    .plotwrap{margin-bottom:26px;clear:both;overflow:hidden}
    .shiny-plot-output{margin-bottom:10px}
    .dtwrap{margin-top:18px;margin-bottom:30px;clear:both}
    /* level-of-detail panel, pinned bottom right */
    .fltbox{border:1px solid #d9a441;border-radius:6px;padding:6px 8px;
  margin:6px 0;background:#fffaf0}
.flttitle{font-size:11px;font-weight:600;color:#9a5000;text-transform:uppercase;
  letter-spacing:.4px;margin-bottom:4px}
.fltbox .form-group{margin-bottom:4px}
.fltbox .control-label{font-size:11px;margin-bottom:1px}
    .setall-box{background:#eef2f7;border:1px solid #cbd7e3;border-radius:6px;padding:6px 8px;margin-bottom:6px}
    .cascade-dir{background:#f4f7f4;border-left:4px solid #59A14F;border-radius:4px;padding:4px 8px;margin-bottom:6px;font-size:12px}
.fltapplied{display:flex;flex-wrap:wrap;gap:4px;margin-top:6px}
.fltpill{display:inline-flex;align-items:center;gap:5px;font-size:11px;
  background:#fff;border:1px solid #d9a441;border-radius:10px;
  padding:1px 4px 1px 8px;color:#7a4a00;max-width:100%}
.fltpill b{font-weight:600}
.fltpill .x{cursor:pointer;font-weight:700;color:#b00;padding:0 3px}
.fltpill .x:hover{background:#fdd;border-radius:8px}
.hier-key-row .form-group{margin-bottom:2px}
.hier-key-row .control-label{font-size:11px;margin-bottom:1px}
.ccpane{border-radius:5px;padding:4px 6px}
.ccpane-lan{background:#eef6ee;border:1px solid #cfe3cf}
.ccpane-dis{background:#fdf1e7;border:1px solid #f0d3b6}
.ccpane-title{font-size:11px;font-weight:600;letter-spacing:.3px}
.lodbox{position:static;width:auto;margin-top:10px;
            background:#fff6e5;border:2px solid #F28E2B;border-radius:9px;
            padding:9px 11px}
    /* ---- the sticky control deck above each figure ---- */
    /* 46px is the navbar; the deck sits directly under it and stays there
       while the plots scroll. z-index below the navbar, above the plots. */
    .deckwrap{position:sticky;top:46px;z-index:1024;background:#f2f5f8;
              border:1px solid #cfd8e0;border-radius:0 0 9px 9px;
              box-shadow:0 3px 10px rgba(0,0,0,.13);margin:0 0 14px 0;
              padding:0 8px 8px 8px}
    .deckbar{display:flex;align-items:center;justify-content:space-between;
             padding:3px 2px 2px 2px}
    .decklabel{font-size:10.5px;font-weight:700;color:#5b7186;
               text-transform:uppercase;letter-spacing:.5px}
    .deckbtn{border:1px solid #cfd8e0;background:#fff;border-radius:5px;
             color:#12324f;font-size:11px;line-height:1;padding:2px 9px;
             cursor:pointer}
    .deckbtn:hover{background:#e6eef5}
    /* Three equal columns, and align-items:stretch so the boxes are the
       SAME height - align-items:start let each shrink to its content,
       which is what made the three read as unrelated panels rather than
       one row of controls. */
    .deckgrid{display:grid;gap:8px;align-items:stretch;
              grid-template-columns:repeat(3, minmax(210px, 1fr))}
    /* Plot options is a short box; settings and the filter need the room */
    .deckgrid.deck3{grid-template-columns:minmax(150px,.6fr)
                                          minmax(240px,1.2fr)
                                          minmax(240px,1.2fr)}
    .deckgrid.deck2{grid-template-columns:minmax(260px,1fr) minmax(260px,1fr)}
    .deckgrid.deck1{grid-template-columns:minmax(260px,1fr)}
    @media (max-width:1100px){.deckgrid,.deckgrid.deck3,.deckgrid.deck2,.deckgrid.deck1{
              grid-template-columns:1fr 1fr}}
    /* No fixed height and no inner scrollbar. A control the reader has
       to scroll INSIDE a box to reach is a control they will not find -
       the box grows to fit its contents and the row grows with it. The
       whole deck still collapses to one line when it is in the way. */
    .deckbox{background:#fff;border:1px solid #d3dae1;border-radius:7px;
             padding:6px 9px 8px 9px;overflow:visible;height:auto}
    /* A one-shot form - credentials, a data request, a folder loader -
       is the wrong SHAPE for control_deck()/builder_deck(): those are
       built around repeated iteration (adjust, Apply, adjust again) or
       a numbered sequence of steps, and neither fits a form the reader
       fills in once and presses Connect on. This borrows the deck's visual language -
       white card, same border radius, same left-border accent by
       purpose - without pulling in machinery the panel does not need. */
    .simplecard{background:#fff;border:1px solid #d3dae1;border-radius:7px;
             border-left:3px solid #4E79A7;padding:10px 14px}
    .simplecard h4{margin-top:0}
    /* ---- one type scale for everything inside the deck ----
       The three boxes inherited whatever their contents happened to
       bring - bootstrap's 14px selects next to 11px chips next to 13px
       headings - so nothing lined up. Everything below is stated once,
       here, and the boxes are no longer free to disagree. */
    .deckbox, .deckbox *{font-size:11.5px !important;line-height:1.35}
    /* ---- readability of EVERY control in the workbench ----
       The colour rules below were written for .deckbox and left every
       other dropdown in the app - the builders, the loaders, the
       InterCatch tabs, the comparison controls - inheriting whatever
       tint sat behind them, which is exactly the unreadable state the
       deck rules were added to fix. Stated once here, unscoped; the
       .deckbox rules that follow only set SIZE.
       The :not(.selectize-input input) exclusion matters: selectize puts
       its own <input type=text> inside the widget to type into, and
       giving that a white fill and a border draws an empty box beside
       the selected value. */
    select, .form-control, input[type=number],
    input[type=text]:not(.selectize-input input){
             background:#fff !important;color:#1b2b3a !important;
             border:1px solid #c3ccd5 !important}
    select option{background:#fff;color:#1b2b3a}
    .selectize-input, .selectize-dropdown, .selectize-dropdown-content{
             background:#fff !important;color:#1b2b3a !important}
    .selectize-input{border:1px solid #c3ccd5 !important;
             box-shadow:none !important}
    .selectize-input > input{background:transparent !important;
             border:0 !important;box-shadow:none !important;
             padding:0 !important;min-height:0}
    .selectize-dropdown [data-selectable]{background:#fff;color:#1b2b3a}
    .selectize-dropdown .active, select option:checked{
             background:#e6eef5 !important;color:#12324f !important}
    /* the open list must float above a sticky deck and the navbar, or
       the options are painted behind them and cannot be clicked */
    .selectize-dropdown{z-index:3000 !important;
             border:1px solid #c3ccd5;box-shadow:0 4px 14px rgba(0,0,0,.18)}
    .selectize-input .item{background:#e6eef5;color:#12324f;
             border-radius:3px;padding:0 4px;margin:1px 2px 1px 0}
    /* Sliders take the width they are given. The discard-rate slider sat
       in a default-width form-group, so its scale labels were unreadable
       while the coverage slider beside it - built full width - was
       fine. (No quotes in this comment: the whole CSS block is one R
       string, so a stray double quote ends it and the rest of the
       stylesheet parses as code.) */
    .irs, .js-irs-0{width:100% !important}
    .shiny-input-container:not(.shiny-input-container-inline){
             width:100% !important;max-width:100% !important}

    /* An explicit white background on every control. Inheriting the
       box tint made the selected value almost unreadable against it,
       which is fatal on a picker whose whole job is to say which
       variable is on the axis.
       The :not(.selectize-input input) exclusions matter. Shiny renders
       selectInput through selectize, which puts its own <input
       type=text> INSIDE the widget to type into. Styling that as a
       control gave it a white fill and a border of its own, so it
       showed up as a small empty box sitting next to the selected
       value. It has to stay transparent and borderless. */
    .deckbox .form-control, .deckbox select,
    .deckbox input[type=number]{height:26px;padding:2px 6px;
             font-size:11.5px !important;
             background:#fff !important;color:#1b2b3a !important;
             border:1px solid #c3ccd5 !important}
    .deckbox input[type=text]:not(.selectize-input input){
             height:26px;padding:2px 6px;font-size:11.5px !important;
             background:#fff !important;color:#1b2b3a !important;
             border:1px solid #c3ccd5 !important}
    .deckbox select option{background:#fff;color:#1b2b3a}
    .deckbox .selectize-input, .deckbox .selectize-dropdown,
    .deckbox .selectize-dropdown-content{
             background:#fff !important;color:#1b2b3a !important}
    .deckbox .selectize-input{border:1px solid #c3ccd5 !important;
             min-height:26px;padding:2px 6px;box-shadow:none !important}
    /* the typing field: no box of its own */
    .deckbox .selectize-input > input{background:transparent !important;
             border:0 !important;box-shadow:none !important;height:auto;
             padding:0 !important;min-height:0}
    .deckbox .selectize-dropdown .active{background:#e6eef5 !important}
    /* A dropdown is absolutely positioned; without a z-index above the
       buttons beneath it, the open list is painted under the Add
       filter button, and the click lands on the button. */
    .deckbox .selectize-dropdown{z-index:2200 !important;
             border:1px solid #c3ccd5;box-shadow:0 4px 14px rgba(0,0,0,.18)}
    .deckbox .selectize-control{margin-bottom:4px}
    .deckbox .selectize-control.multi .selectize-input.has-items{
             padding:2px 6px}
    .deckbox .selectize-input .item{background:#e6eef5;color:#12324f;
             border-radius:3px;padding:0 4px;margin:1px 2px 1px 0}
    .deckbox .btn{font-size:11px !important;padding:2px 8px;line-height:1.4}
    .deckbox .selectize-input{min-height:26px;padding:2px 6px;
             font-size:11.5px !important}
    .deckbox .checkbox{margin:2px 0}
    .deckbox .checkbox label{font-size:11.5px !important}
    .deckbox .irs{font-size:10px}
    .deckbox .decktitle{font-size:10.5px !important}
    .deckbox .note, .deckbox .note *{font-size:10.5px !important;
             line-height:1.35;color:#7a8791}
    .deckbox.deck-flt{border-color:#d9a441;background:#fffaf0}
    .deckbox.deck-lod{border:2px solid #F28E2B;background:#fff6e5}
    /* Apply spans the whole deck: it acts on all three boxes, so it is
       a bar under them rather than a fourth column beside them. */
    .deckapply{display:flex;align-items:center;gap:10px;margin-top:8px}
    .deckapply .btn-apply{background:#12324f;border-color:#12324f;color:#fff;
             font-weight:700;flex:1;font-size:12.5px;padding:6px 10px;
             letter-spacing:.3px}
    .deckapply .btn-apply:hover{background:#1d4b73;border-color:#1d4b73}
    .deckapply .note{font-size:10.5px;color:#7a8791;white-space:nowrap;
             margin:0}
    .decktitle{font-size:10.5px;font-weight:700;color:#5b7186;
               text-transform:uppercase;letter-spacing:.4px;margin-bottom:4px}
    .deck-flt .decktitle,.deck-lod .decktitle{color:#9a5000}
    .deckbox .form-group{margin-bottom:4px}
    .deckbox .control-label{font-size:11px;margin-bottom:1px}
    .deckbox .note{margin:2px 0 4px 0}
    .deckbox .rolechips-stacked{gap:5px;margin:0}
    .deckbox h4{margin:2px 0 3px 0;font-size:12px}
    .deckbox .picker{margin:0 0 4px 0}
    /* collapsed: the bar stays, so it can always be opened again */
    .deckwrap.mini{padding-bottom:2px;box-shadow:0 2px 6px rgba(0,0,0,.10)}
    .deckwrap.mini .deckgrid,
    .deckwrap.mini .deckbody-wrap,
    .deckwrap.mini .deckapply,
    .deckwrap.mini .deckwide{display:none}
    .deckwrap.mini .deckstrata{display:none}
    /* The BUILDER decks do not follow the page. A figure deck pinned
       under the navbar is useful - the plot scrolls past it. A builder
       deck is half the screen tall, so pinning it left almost nothing
       of the output tabs visible, which is the thing the builder exists
       to produce. They scroll away; the minimise button is how they get
       out of the way on purpose. */
    .deckwrap.bdeck{position:static;top:auto;box-shadow:0 2px 8px rgba(0,0,0,.10)}
    /* sticky = FALSE: same box, just not pinned. For a deck that sits
       above a plot tall enough that staying pinned would permanently
       cover it once the reader scrolls past the deck's own height. */
    .deckwrap.nostick{position:static;top:auto}
    /* narrow_options = TRUE: the options box holds a short, fixed set
       of controls (pick one allocation, press Run) that never needs as
       much room as a settings box listing five or six fields - giving
       them equal width left the short box mostly empty and the long one
       cramped. */
    .deckgrid.deck-narrowopt{grid-template-columns:minmax(200px,.7fr)
                                                    minmax(280px,1.3fr)}
    .deckwrap.mini .decklabel:after{content:' - minimised';font-weight:400;
                                    text-transform:none;letter-spacing:0}
    .lvdeck .deckgrid{grid-template-columns:minmax(210px,300px)}
    /* The builder deck: three numbered steps, taller than a figure deck
       because the key list and the donor bars need the room. */
    /* The wide step sits under the FIRST TWO boxes only; the catch-all
       column runs full height beside it. Laid out as a grid row rather
       than a block, so the two line up with the columns above. */
    .deckwide{margin-top:8px}
    /* the strata-definition box: full width, sits above the columns */
    .deckstrata{margin-bottom:8px}
    .deckstrata .deckbox{padding:8px 10px}
    .deck-step-strata{border-left-color:#4E79A7}
    .deckstrata .radio-inline{margin-right:14px;font-size:11.5px}
    .bdeck-3.mg-grid{grid-template-areas:'b1 b2 b3' 'wide wide b3';
             grid-template-columns:minmax(230px,1fr) minmax(260px,1.15fr)
                                   minmax(240px,1fr)}
    .mg-grid > .deck-step-1{grid-area:b1}
    .mg-grid > .deck-step-2{grid-area:b2}
    .mg-grid > .deck-step-3{grid-area:b3}
    .mg-grid > .deckwide{grid-area:wide;margin-top:0}
    @media (max-width:1150px){
      .bdeck-3.mg-grid{grid-template-areas:'b1' 'b2' 'b3' 'wide';
               grid-template-columns:1fr}}
    .deck-step-wide{border-left-color:#E15759}
    .bdeck-2{grid-template-columns:minmax(320px,1fr) minmax(320px,1fr)}
    .bdeck-3{grid-template-columns:minmax(230px,.85fr) minmax(330px,1.5fr)
                                   minmax(250px,1fr)}
    .bdeck-4{grid-template-columns:minmax(190px,.7fr) minmax(240px,1.1fr)
                                   minmax(220px,1fr) minmax(280px,1.3fr)}
    @media (max-width:1400px){.bdeck-4{grid-template-columns:1fr 1fr}}
    @media (max-width:1150px){.bdeckgrid{grid-template-columns:1fr}}
    .bdeckgrid .deckbox{height:auto;overflow:visible}
    .deck-step-4{border-left-color:#B07AA1}
    .deck-step{border-left:3px solid #c9d4de}
    .deck-step-1{border-left-color:#F28E2B}
    .deck-step-2{border-left-color:#4E79A7}
    .deck-step-3{border-left-color:#59A14F}
    .bdeckapply{gap:8px}
    .bdeckapply .btn-apply{flex:2}
    /* One meaning per colour, in both builders:
         green (btn-addgrp)  adds a group to the set being built
         teal  (btn-savescn) commits the whole set as a scenario
         dark  (btn-apply)   recomputes what is on screen
       These were crossed before: Save as scenario was green in the
       cascade and dark in the manual builder, and green meant add in
       one and save in the other. */
    .btn-addgrp{background:#3d8b40 !important;border-color:#3d8b40 !important;
             color:#fff !important;font-weight:700}
    .btn-addgrp:hover{background:#2f6f32 !important;border-color:#2f6f32 !important}
    .bdeckapply .btn-savescn{background:#12324f;border-color:#12324f;
             color:#fff;font-weight:700;flex:1;font-size:12.5px;
             padding:6px 10px}
    .bdeckapply .btn-savescn:hover{background:#1d4b73;border-color:#1d4b73}
    .bdeckapply .btn-addgrp{flex:1;font-size:12.5px;padding:6px 10px}
    /* A fourth role, for buttons that RUN an engine call rather than
       change a definition - Run conversion, Request data, Run raising,
       Allocate, Start. These used to be plain Bootstrap green
       (btn-success), which is a different green from btn-addgrp and
       meant three shades of green were doing three different jobs on
       the same screen. F28E2B is already the accent used for Level of
       detail and Exclude donors, so reusing it here signals a
       significant, engine-touching action with a colour the app
       already trained the eye on. */
    .btn-run{background:#F28E2B !important;border-color:#F28E2B !important;
             color:#fff !important;font-weight:700}
    .btn-run:hover{background:#d97a1c !important;border-color:#d97a1c !important}
    .bdeckapply .form-group{margin:0;flex:1}
    .bdeckapply input[type=text]{height:32px;font-size:12px}
    /* the key rows have to stay compact or three of them fill the box */
    .deckbox .hier-key-row{margin:0 0 2px 0}
    .deckbox .hier-key-row .irs{margin-top:-4px}
    .deckbox .hier-key-fixed{padding:2px 4px;font-size:11px}
    .plotrail{padding-right:4px}
    .plotrail .picker{flex-direction:column}
    .plotrail .pick{width:100%}
    .lodbox .form-group{margin-bottom:5px}
    .lodtitle{color:#9a5000;font-weight:700;font-size:12px;
              text-transform:uppercase;letter-spacing:.4px}
    .container-fluid{padding-bottom:20px}
    /* --- top bar: keep it to its natural height (no tall blue gap) --- */
    .navbar{margin-bottom:0!important;position:sticky;top:0;z-index:1040;
            min-height:46px;box-shadow:0 1px 4px rgba(0,0,0,.12)}
    .navbar-default{background:#12324f;border-color:#0d2740}
    .navbar .container-fluid{min-height:46px;padding-bottom:0}
    .navbar-header{min-height:46px}
    .navbar-default .navbar-brand{color:#fff;font-weight:700;letter-spacing:.3px;
            height:46px;padding-top:13px}
    .navbar-default .navbar-nav>li>a{color:#cfe0ee;padding-top:13px;
            padding-bottom:13px}
    .navbar-default .navbar-nav>.active>a,
    .navbar-default .navbar-nav>.active>a:hover{background:#1d4b73;color:#fff}
    /* the tab-pane wrapper must not add its own blank height */
    .navbar+.container-fluid{padding-top:0}
    .tab-content>.tab-pane{padding-top:0}

    /* navbarPage nests the tab content as
         .container-fluid > .tab-content > .tab-pane > .tabbable > .nav-tabs
       for the 2nd level, and one .tabbable deeper for the 3rd. Pin every
       nav-tabs bar sticky; the offset grows with depth so they stack
       directly under the top bar and each other. */
    ul.nav-tabs{position:sticky;z-index:1030;margin:0;
                border-bottom:1px solid #b5c9db}

    /* 2nd level: directly under the 46px top bar, mid-blue */
    .container-fluid>.tab-content>.tab-pane>.tabbable>ul.nav-tabs{
            top:46px;z-index:1036;background:#1d4b73;padding:0 8px;
            border-bottom:2px solid #12324f}
    .container-fluid>.tab-content>.tab-pane>.tabbable>ul.nav-tabs>li>a{
            color:#cfe0ee;font-size:13px;padding:8px 14px;border:none;
            border-radius:0;margin:0}
    .container-fluid>.tab-content>.tab-pane>.tabbable>ul.nav-tabs>li>a:hover{
            background:#2a5c88;border:none}
    .container-fluid>.tab-content>.tab-pane>.tabbable>ul.nav-tabs>li.active>a,
    .container-fluid>.tab-content>.tab-pane>.tabbable>ul.nav-tabs>li.active>a:hover{
            background:#f2f5f8;color:#12324f;font-weight:700;border:none}

    /* 3rd level: under the 2nd (46+37), lighter blue */
    .tab-content>.tab-pane>.tabbable .tab-content>.tab-pane>.tabbable>ul.nav-tabs{
            top:83px;z-index:1032;background:#dbe6f0;padding:2px 10px 0 10px;
            border-bottom:1px solid #b5c9db}
    .tab-content>.tab-pane>.tabbable .tab-content>.tab-pane>.tabbable>ul.nav-tabs>li>a{
            color:#39536b;font-size:12.5px;padding:5px 12px;border-radius:0}
    .tab-content>.tab-pane>.tabbable .tab-content>.tab-pane>.tabbable>ul.nav-tabs>li.active>a{
            font-weight:700;color:#12324f;background:#f2f5f8}

    /* 4th level (e.g. inside cef_result_ui): under the 3rd (83+34) */
    .tab-content>.tab-pane>.tabbable .tab-content>.tab-pane>.tabbable
      .tab-content>.tab-pane>.tabbable>ul.nav-tabs{
            top:116px;z-index:1028;background:#eef3f8;padding:2px 10px 0 10px}
    .tab-content{padding-top:8px}
    .busy-spinner{width:38px;height:38px;margin:0 auto;border:4px solid #dbe6f0;
                  border-top-color:#12324f;border-radius:50%;
                  animation:busyspin 0.8s linear infinite}
    @keyframes busyspin{to{transform:rotate(360deg)}}
    /* plot-type switcher */
    .picker{display:flex;gap:8px;margin:2px 0 8px 0;flex-wrap:wrap}
    .pick{border:2px solid #d3dae1;border-radius:8px;padding:5px;cursor:pointer;
          background:#fff;text-align:center;width:74px}
    .pick:hover{border-color:#7fa8c9}
    .pick.on{border-color:#12324f;background:#eaf1f7;box-shadow:0 1px 3px rgba(0,0,0,.15)}
    .pick div{font-size:10px;color:#39536b;margin-top:2px}
    .pick.disabled{opacity:0.32;cursor:not-allowed;pointer-events:none}
    .rolechips{display:flex;gap:10px;flex-wrap:wrap;align-items:flex-end;
               margin:2px 0 6px 0}
    .rolechips-stacked{flex-direction:column;align-items:stretch;gap:7px}
    .rolechip-wrap{display:flex;flex-direction:column}
    .rolechip-lab{font-size:10px;font-weight:700;color:#fff;text-transform:uppercase;
                  letter-spacing:.3px;border-radius:5px 5px 0 0;padding:2px 8px;
                  margin:0}
    .rolechip-wrap .form-group{margin-bottom:0}
    .rolechip-wrap .selectize-input,.rolechip-wrap select{border-radius:0 0 5px 5px;
                  border-top:none;min-height:30px}
    /* the top strip that holds only the extracted legend */
    .legend-strip{border-bottom:1px solid #e2e8ee;margin-bottom:8px;
                  padding-bottom:4px;min-height:80px}
    /* the apply gate under each panel's controls */
    .applybar{display:flex;align-items:center;gap:9px;margin:10px 0 2px 0;
              padding:7px 8px;background:#eef3f8;border:1px solid #cfd8e0;
              border-radius:7px}
    .applybar .btn-apply{background:#12324f;border-color:#12324f;color:#fff;
                         font-weight:700;min-width:78px}
    .applybar .btn-apply:hover{background:#1d4b73;border-color:#1d4b73}
    .applybar.stale{background:#fff4e0;border-color:#e0a458}
    .applystale{font-size:10.5px;color:#9a5000;font-weight:600;line-height:1.25}
    .applyok{font-size:10.5px;color:#5b7186;line-height:1.25}
    /* per-plot toolbar, floated into the plot's own top-right margin so it
       costs no vertical space and never sits between legend and plot */
    .plotwrap.has-tools{position:relative}
    .plottools{position:absolute;top:2px;right:2px;z-index:5;
               display:flex;align-items:center;gap:5px;
               padding:3px 5px;background:rgba(255,255,255,.92);
               border:1px solid #e2e8ee;border-radius:6px;
               opacity:.55;transition:opacity .12s}
    .plotwrap.has-tools:hover .plottools{opacity:1}
    .plottools:hover{opacity:1}
    .plottools .btn{font-size:11px;padding:2px 9px;border-radius:5px;
                    font-weight:600}
    .plottools .btn-keep{background:#12324f;border-color:#12324f;color:#fff}
    .plottools .btn-keep:hover{background:#1d4b73;border-color:#1d4b73}
    .plottools .btn-img{background:#fff;border:1px solid #59A14F;color:#2f6b2a}
    .plottools .btn-img:hover{background:#eef6ef}
    /* the none row in every variable picker: tinted and italic, so it
       reads as an instruction rather than as a variable of that name.
       Two selectors because shiny renders selectInput through selectize
       (a div list) but falls back to a plain <select> when selectize is
       off, and the two share no markup. */
    .selectize-dropdown [data-value=''],
    .selectize-dropdown-content .option[data-value='']{
      color:#9a5000 !important;font-style:italic;font-weight:600;
      background:#fff6e5 !important;border-bottom:1px solid #f0d9b5}
    .selectize-dropdown [data-value=''].active{background:#ffe9c7 !important}
    .selectize-input .item[data-value='']{color:#9a5000;font-style:italic}
    select option[value='']{color:#9a5000;font-style:italic;background:#fff6e5}
    .plottools .note{font-size:10.5px}
    /* the TAF script viewer */
    .tafhead{background:#eef3f8;border-left:4px solid #12324f;
             padding:7px 11px;margin-bottom:8px;border-radius:0 6px 6px 0}
    .taffile{font-family:monospace;font-weight:700;color:#12324f;font-size:13px}
    .keptrow{display:flex;align-items:center;gap:8px;font-size:11.5px;
             border-left:4px solid #59A14F;background:#fff;border-radius:4px;
             padding:4px 7px;margin:4px 0}
    .keptrow .ttl{font-weight:600;color:#12324f}
    .keptrow .sub{color:#666;font-size:10.5px}
    .hier-key-row{align-items:center;margin-bottom:2px}
    .hier-key-fixed{background:#eef3f8;border:1px solid #cfd8e0;border-radius:6px;
                    padding:6px 9px;font-size:12px;color:#12324f}
    .allocbox{position:fixed;top:96px;right:14px;z-index:1030;width:236px;
              background:#eef6ef;border:2px solid #59A14F;border-radius:9px;
              padding:8px 10px;box-shadow:0 3px 12px rgba(0,0,0,.18);
              max-height:210px;overflow:auto}
    .alloctitle{color:#14532d;font-weight:700;font-size:11px;
                text-transform:uppercase;letter-spacing:.4px}
    .dragchip{cursor:grab;background:#12324f;color:#fff;border-radius:14px;
              padding:5px 12px;font-family:monospace;font-size:11px;
              display:inline-block;max-width:100%;overflow:hidden;
              text-overflow:ellipsis;white-space:nowrap;
              box-shadow:0 1px 3px rgba(0,0,0,.25)}
    .dragchip.empty{background:#c9ccd1;cursor:default}
    .dropzone{border:2px dashed #b8c4cf;border-radius:6px;padding:8px;
              min-height:46px;background:#fff;font-family:monospace;
              font-size:11px;transition:.12s}
    .dropzone.over{border-color:#12324f;background:#eaf1f7}
    .varchip{display:inline-block;background:#eef2f6;border:1px solid #cfd8e0;
             border-radius:12px;padding:2px 9px;margin:2px;font-size:11px;
             color:#39536b;cursor:pointer}
    .varchip.on{background:#12324f;color:#fff;border-color:#12324f}
    .condbox{border:2px solid #9bb7d4;border-radius:8px;padding:8px;
             margin-bottom:6px;background:#f6f9fc}
    .condtitle{font-weight:700;font-size:12px;color:#2c5578;margin-bottom:4px}
    .rulerow{display:flex;align-items:center;gap:6px;padding:4px 6px;
             margin-bottom:4px;border-radius:6px;border:1px solid #ddd;
             background:#fff;font-size:12px}
    .rulechip{width:34px;text-align:center;border-radius:5px;color:#fff;
              font-weight:700;padding:2px 0}
    .rulebtn{border:1px solid #bbb;background:#f4f4f4;border-radius:4px;
             cursor:pointer;font-size:11px;padding:1px 6px}

    /* Shiny puts .recalculating on an output while it is being redrawn.
       Without a cue the app looks frozen on anything slow - the biology
       link and the cascade both walk the whole distributions table - and
       a user presses Apply again, which queues a second pass. */
    .shiny-bound-output.recalculating{opacity:.35;transition:opacity .1s}
    .dtwrap, .plotwrap{position:relative}
    .busydots{position:absolute;top:50%;left:50%;transform:translate(-50%,-50%);
      z-index:20;background:rgba(255,255,255,.94);border:1px solid #cbd7e3;
      border-radius:8px;padding:8px 14px;font-size:12px;color:#12324f;
      box-shadow:0 2px 10px rgba(0,0,0,.12);white-space:nowrap}
    .busydots i{display:inline-block;width:5px;height:5px;border-radius:50%;
      background:#2f6f9f;margin-left:3px;animation:bd 1s infinite}
    .busydots i:nth-child(2){animation-delay:.15s}
    .busydots i:nth-child(3){animation-delay:.3s}
    @keyframes bd{0%,60%,100%{opacity:.25;transform:translateY(0)}
                  30%{opacity:1;transform:translateY(-3px)}}

    #work_pill{position:fixed;right:14px;bottom:14px;z-index:3500;display:none;
      background:#12324f;color:#fff;font-size:12px;padding:6px 12px;
      border-radius:16px;box-shadow:0 3px 12px rgba(0,0,0,.3)}
    #work_pill i{display:inline-block;width:5px;height:5px;border-radius:50%;
      background:#9fd0ff;margin-left:3px;animation:bd 1s infinite}
    #work_pill i:nth-child(3){animation-delay:.15s}
    #work_pill i:nth-child(4){animation-delay:.3s}
  ")),
  tags$script(HTML(deck_js)),
  tags$script(HTML(
    "// A picker only sent its value on click, so on first load every
     // conditionalPanel keyed on one was hidden and the section looked
     // empty. This publishes each picker's initial .on choice as soon as
     // it exists, and again whenever new pickers arrive with a panel.
     function pickInit(){
       document.querySelectorAll('.picker').forEach(function(p){
         var id = p.getAttribute('data-pick-id');
         if (!id) return;
         var on = p.querySelector('.pick.on') || p.querySelector('.pick');
         if (!on) return;
         var v = on.getAttribute('data-val');
         if (v != null) Shiny.setInputValue(id, v);
       });
     }
     $(document).on('shiny:connected', function(){ setTimeout(pickInit, 0); });
     $(document).on('shiny:value shiny:visualchange', function(){
       setTimeout(pickInit, 0); });
     function pickOn(el){var p=el.closest('.picker');if(!p)return;
     p.querySelectorAll('.pick').forEach(function(x){x.classList.remove('on');});
     el.classList.add('on');}")),

  # a "clear this screen" button, always visible top-right of the bar
  tags$div(style = "position:fixed;top:7px;right:14px;z-index:1050",
    actionButton("clear_screen", "Clear this screen", icon = icon("eraser"),
                 class = "btn-sm",
                 style = "background:#2a5c88;color:#fff;border:1px solid #12324f")),

  # a centred busy overlay shown while an action runs, to stop double-clicks
  tags$div(id = "busy_overlay",
    style = "display:none;position:fixed;inset:0;z-index:3000;
             background:rgba(18,50,79,0.28);",
    tags$div(style = "position:absolute;top:50%;left:50%;
                      transform:translate(-50%,-50%);background:#fff;
                      border-radius:10px;padding:20px 30px;
                      box-shadow:0 6px 26px rgba(0,0,0,.3);text-align:center",
      tags$div(class = "busy-spinner"),
      tags$div(id = "busy_msg", style = "margin-top:10px;color:#12324f;
                                         font-weight:600", "Working..."))),
  # A floating preview of whatever stratum the pointer is over in an
  # exclusion list. The SVGs are sent once and cached client-side, so the
  # hover is instant and costs no round-trip.
  # A quieter companion to the centred overlay: the overlay is for a
  # deliberate action the user is waiting on, this is for everything else
  # Shiny happens to be recomputing. It names the task rather than saying
  # "loading", because "loading" does not tell you whether to wait two
  # seconds or twenty.
  tags$div(id = "work_pill",
           tags$span(id = "work_pill_txt", "Working"),
           tags$i(), tags$i(), tags$i()),
  tags$script(HTML(
    "// Name the slow things. Anything not listed falls back to a generic
     // line rather than claiming to know what is happening.
     var WORK_WHAT = {
       cov_plot:'Building sampling-coverage strata',
       dr_plot:'Computing discard rates per stratum',
       dr_tbl:'Aggregating the stratum table',
       len_cov_plot:'Linking length distributions to catch strata',
       len_n_plot:'Summing numbers at length',
       len_w_plot:'Averaging weights at length',
       age_cov_plot:'Linking age distributions to catch strata',
       age_n_plot:'Summing numbers at age',
       age_w_plot:'Averaging weights at age',
       man_progress:'Testing each rule against the strata',
       man_missing:'Finding the strata no rule has claimed',
       tree_tbl:'Walking the raising cascade',
       diag_tbl:'Classifying strata against the threshold'
     };
     function bdMark(){
       document.querySelectorAll('.shiny-bound-output.recalculating')
         .forEach(function(el){
           var w = el.closest('.dtwrap, .plotwrap');
           if (!w || w.querySelector('.busydots')) return;
           var what = WORK_WHAT[el.id] || 'Recomputing';
           var d = document.createElement('div');
           d.className = 'busydots';
           d.innerHTML = what + '<i></i><i></i><i></i>';
           w.appendChild(d);
         });
       document.querySelectorAll('.busydots').forEach(function(d){
         var w = d.parentNode;
         if (!w.querySelector('.shiny-bound-output.recalculating')) d.remove();
       });
     }
     function bdPill(){
       var busy = document.documentElement.classList.contains('shiny-busy');
       var pill = document.getElementById('work_pill');
       if (!pill) return;
       if (busy) {
         var first = document.querySelector('.shiny-bound-output.recalculating');
         var txt = (first && WORK_WHAT[first.id]) || 'Working';
         document.getElementById('work_pill_txt').innerText = txt;
       }
       pill.style.display = busy ? 'block' : 'none';
     }
     setInterval(function(){ bdMark(); bdPill(); }, 120);")),

  tags$div(id = "strat_tip",
    style = "display:none;position:fixed;z-index:4000;pointer-events:none;
             box-shadow:0 4px 16px rgba(0,0,0,.28);border-radius:6px"),
  tags$script(HTML(
    "var STRAT_SVG = {};
     Shiny.addCustomMessageHandler('stratSvg', function(m){ STRAT_SVG = m.map || {}; });
     (function(){
       var tip = null;
       function key(el){
         var t = (el.textContent || '').trim();
         var i = t.indexOf('   (');
         return i > 0 ? t.slice(0, i) : t;
       }
       document.addEventListener('mouseover', function(e){
         var o = e.target.closest('.selectize-dropdown-content .option, .item');
         if (!o) return;
         var k = key(o), svg = STRAT_SVG[k];
         if (!svg) return;
         tip = document.getElementById('strat_tip');
         tip.innerHTML = svg; tip.style.display = 'block';
       });
       document.addEventListener('mousemove', function(e){
         if (!tip || tip.style.display === 'none') return;
         tip.style.left = Math.min(e.clientX + 16, window.innerWidth - 240) + 'px';
         tip.style.top  = Math.min(e.clientY + 16, window.innerHeight - 110) + 'px';
       });
       document.addEventListener('mouseout', function(e){
         if (!e.target.closest('.selectize-dropdown-content .option, .item')) return;
         if (tip) tip.style.display = 'none';
       });
     })();")),
  tags$script(HTML(
    "Shiny.addCustomMessageHandler('busy', function(m){
       document.getElementById('busy_msg').innerText = m.msg || 'Working...';
       document.getElementById('busy_overlay').style.display =
         m.show ? 'block' : 'none';
     });"))),  # closes tags$head

  # =========================================================
  tabPanel("1. Load data",
    div(style = "padding:8px 4px",
      pick_bar("src", list(srcic = "InterCatch files",
                           srccef = "CEF files",
                           srcdb = "CEF database call"), "srcic"),
      span(class = "note",
           "Choose the method to be used to import the data into the App. ",
           "Only one route can own the data at a time - switching route ",
           "replaces what is loaded, and everything built on it."),
      load_owner_ui(1),
      tags$hr(),

      # --- InterCatch: exchange or output, then the three steps ---
      conditionalPanel("input.src == 'srcic'",
        pick_bar("ic_source", list(icexch = "IC exchange",
                                   icout = "IC output"), "icexch"),
        uiOutput("ic_source_note"),
        tags$hr(),
        pick_bar("ic_step", list(stepin = "Input", stepout = "Output CEF",
                                 stepres = "Result"), "stepin"),
        tags$hr(),
        conditionalPanel("input.ic_step == 'stepin'",  ic_load_ui()),
        conditionalPanel("input.ic_step == 'stepout'", ic_output_ui()),
        conditionalPanel("input.ic_step == 'stepres'", cef_result_ui("ic"))),

      # --- CEF: load, then result ---
      conditionalPanel("input.src == 'srccef'",
        span(class = "note",
             "Load the CEF files directly. These can be the result of the ",
             "conversion of InterCatch files from the previous section, or ",
             "CEF files downloaded from the CEF database. If the latter, we ",
             "recommend extracting the data directly from the database using ",
             "the next option (CEF database call)."),
        pick_bar("cef_step", list(stepin = "Load", stepres = "Result"),
                 "stepin"),
        tags$hr(),
        conditionalPanel("input.cef_step == 'stepin'",  cef_load_ui()),
        conditionalPanel("input.cef_step == 'stepres'", cef_result_ui("cef"))),

      # --- CEF database call: credentials -> request -> result ---
      conditionalPanel("input.src == 'srcdb'",
        span(class = "note",
             "Pull the CEF tables straight from the CEF database. Enter your ",
             "credentials, describe the request, and download - the result ",
             "joins the same pipeline as the other two sources."),
        pick_bar("db_step", list(dbcred = "Credentials", dbreq = "Request",
                                 stepres = "Result"), "dbcred"),
        tags$hr(),
        conditionalPanel("input.db_step == 'dbcred'", db_cred_ui()),
        conditionalPanel("input.db_step == 'dbreq'",  db_request_ui()),
        conditionalPanel("input.db_step == 'stepres'", cef_result_ui("db"))))),

  # =========================================================
  tabPanel("2. Discards",
    div(style = "padding:8px 4px",
      pick_bar("sec3", list(stgexpl = "Data exploration",
                            stggrp = "Allocation groups",
                            stgraise = "Raise groups"), "stgexpl"),
      tags$hr(),

      conditionalPanel("input.sec3 == 'stgexpl'",
        pick_bar("sec3a", list(stgexpl = "Sampling coverage",
                               stgdr = "Discard rates"), "stgexpl"),
        tags$hr(),
        conditionalPanel("input.sec3a == 'stgexpl'", coverage_ui()),
        conditionalPanel("input.sec3a == 'stgdr'", drates_ui())),

      conditionalPanel("input.sec3 == 'stggrp'",
        div(style = "padding:8px 4px",
          pick_bar("grp_method",
                   list(ladder = "Hierarchical", manual = "Manual groups"),
                   "ladder"),
          conditionalPanel("input.grp_method != 'manual'", 
          builder_deck("ar", "Hierarchical strata allocation builder",
            numbers = c("1", "2"),
            boxes = list(
              "Define key hierarchy to group strata" = tagList(
                span(class = "note",
                     "Key 1 is the stratum's own data - at whatever ",
                     "resolution is set there. Each key below is one ",
                     "grouping, tried in the order picked (Key 2 first), so ",
                     "the cascade widens top to bottom: Key 2 is the first ",
                     "thing pooled across, the last key picked is kept ",
                     "until everything before it runs out. The threshold ",
                     "is the share of a pool's landings weight that must ",
                     "carry a discard estimate before the pool is accepted."),
                # a min-only slider: the blue reaches from the handle up to
                # 100, the handle is the threshold. Range c(value, 100) with
                # the right end pinned paints the qualifying side; the reader
                # takes element [1].
                fluidRow(
                  column(8, sliderInput("thr_all", "Set all thresholds to",
                                        0, 100, c(50, 100), 5)),
                  column(4, br(), actionButton("apply_all", "Set",
                                               class = "btn-xs btn-primary"))),
                uiOutput("hier_keys"),
                tags$b("Order actually tested",
                       style = "font-size:11.5px;display:block;margin-top:6px"),
                uiOutput("hier_chain")),
              "Exclusion rules for donor strata" = exclude_box(
                "ar", metrics = c("coverage", "trips"), title = NULL,
                num = "2", rate_ui = uiOutput("ar_dr_ui"),
                note = paste("Applies to every rung of the cascade. A stratum",
                             "kept out still gets raised itself - being too",
                             "thin to lend is exactly the reason to borrow."))),
            cols = 2,
            actions = tagList(
              actionButton("ar_apply", "Apply settings",
                           class = "btn btn-apply"),
              uiOutput("ar_hier_stale", inline = TRUE),
              textInput("auto_name", NULL, "auto_1", width = "100%"),
              actionButton("save_auto", "Save as scenario",
                           class = "btn btn-savescn"))),
          alloc_box(),
          fluidRow(
            column(12,
          tabsetPanel(id = "auto_out", type = "tabs",
                        tabPanel("Overview",
              h4("What each step resolves"),
              span(class = "note",
                   "One row per rung in the order it is tested, with the ",
                   "running total. The grouping column names what each rung ",
                   "still distinguishes."),
              uiOutput("tree_check"),
              tbl_slot("tree_tbl"),
              tags$hr(),
              h4("What is assigned, and what is still missing"),
              tbl_slot("grp_tbl")),
            tabPanel("Strata to raise",
              h4("The data being raised"),
              span(class = "note",
                   "One row per stratum: the columns that define it, its ",
                   "landings, discards, rate and coverage, its sampling status, ",
                   "and which rule raises it. Heaviest first."),
              tbl_slot("diag_tbl")),
            tabPanel("Data used to raise",
              h4("The donor pools (matchedDataCond)"),
              span(class = "note",
                   "The same strata, grouped by how they SUPPLY the raising ",
                   "rather than whether they were raised. usable_donor is ",
                   "FALSE where a donor bar (coverage, measurements, rate) ",
                   "excluded the stratum from the pool."),
              tbl_slot("diag_donor_tbl")),
            tabPanel("Rivers",
              h4("Where each step draws its data from"),
              span(class = "note",
                   "One river per rung of the cascade, read the same way as ",
                   "the step table: the strata resolved at that step on the ",
                   "left, the donor pool they were raised from on the right."),
              uiOutput("tree_ui")),
            tabPanel("Code",
              h4("The functions that build the raising"),
              span(class = "note",
                   "The hierarchy, thresholds and any donor exclusions - the ",
                   "recipe - then the strataCond / matchedDataCond it produces. ",
                   "For the automated method the recipe is the point; the ",
                   "groups are what it resolves to."),
              verbatimTextOutput("auto_conds")),
          )))),
          conditionalPanel("input.grp_method == 'manual'", 
          builder_deck("mg", "Manual strata allocation builder",
            # Catch-all is step 5 on the right, assigning the clause is
            # step 4 across the bottom - as in the mock-up, shifted one
            # place by the new strata-definition box at step 1.
            top = list("Definition of the strata" = strata_def_ui("mg_sd")),
            numbers = c("2", "3", "5"), grid = "mg-grid",
            boxes = list(
              "Define the groups of strata to raise / allocate" = tagList(
                tags$b("2.1 Name the group",
                       style = "font-size:11.5px;display:block"),
                fluidRow(column(5, textInput("rule_id", "Id", "G1")),
                         column(7, textInput("rule_label", "Label", ""))),
                tags$b("2.2 Rules to group",
                       style = "font-size:11.5px;display:block;margin-top:4px"),
                span(class = "note", "A clause is one or more variable ",
                     "restrictions combined with AND. Add them one at a time."),
                div(tags$b("Variable", style = "font-size:11.5px")),
                uiOutput("clause_var_chips"),
                selectizeInput("clause_vals", "Values", NULL, multiple = TRUE),
                checkboxInput("clause_neg", "Negate (NOT in)", FALSE),
                div(style = "display:flex;gap:4px",
                  actionButton("add_clause", "Add to clause",
                               class = "btn-xs btn-primary"),
                  actionButton("clear_clause", "Clear clause", class = "btn-xs")),
                uiOutput("clause_chip"),
                tags$b("Overview of the groups defined",
                       style = "font-size:11.5px;display:block;margin-top:6px"),
                uiOutput("rule_order")),
              "Exclusion rules for donor strata" = exclude_box(
                "mg", metrics = c("coverage", "trips"), title = NULL,
                num = "3", rate_ui = uiOutput("rule_dr_ui"),
                note = paste("Narrows THIS group's matchedDataCond only -",
                             "never the strata being raised. The stratum",
                             "stays available to later groups and to the",
                             "catch-all.")),
              "Include a Catch All by All group" = tagList(
                span(class = "note",
                     "A final group with no clause. Its strataCond is the ",
                     "COMPLEMENT of everything above it, so it raises only ",
                     "the strata no earlier rule claimed - nothing is raised ",
                     "twice. Its matchedDataCond is every stratum, including ",
                     "those earlier groups already used: a donor may be ",
                     "reused, only a raised stratum may not."),
                exclude_box("ca", metrics = c("coverage", "trips"),
                            title = NULL, num = "5",
                            rate_ui = tagList(
                              fluidRow(
                                column(6, numericInput("catchall_dr_min",
                                                       "rate above", NA, 0, 5,
                                                       0.01, width = "100%")),
                                column(6, numericInput("catchall_dr_max",
                                                       "rate below", NA, 0, 5,
                                                       0.01, width = "100%"))),
                              span(class = "note",
                                   "Blank switches a bound off.")),
                            note = paste("The catch-all is the last resort, so",
                                         "it usually wants the widest pool -",
                                         "but a stratum too thin to lend is",
                                         "still too thin here.")),
                actionButton("add_catchall", "Add Catch All by All",
                             class = "btn btn-addgrp btn-block",
                             style = "margin-top:6px"))),
            wide = list(
              "4. Assign the clause" = div(
                style = "display:flex;gap:8px;align-items:stretch",
                div(style = "flex:1",
                  div(class = "condbox",
                      div(class = "condtitle", "strataCond"),
                      span(class = "note", "strata to be raised"),
                      dropzone("strata"),
                      actionButton("to_strata", "Put clause here",
                                   class = "btn-xs btn-info"))),
                div(style = "flex:0 0 40px;display:flex;flex-direction:column;
                             justify-content:center;gap:5px",
                  actionButton("mirror", HTML("&rarr;"),
                               class = "btn-xs btn-warning",
                               title = "copy to matchedDataCond"),
                  actionButton("mirror_back", HTML("&larr;"),
                               class = "btn-xs btn-warning",
                               title = "copy back to strataCond")),
                div(style = "flex:1",
                  div(class = "condbox",
                      div(class = "condtitle", "matchedDataCond"),
                      span(class = "note", "data used to raise"),
                      dropzone("matched"),
                      actionButton("to_matched", "Put clause here",
                                   class = "btn-xs btn-info"))))),
            actions = tagList(
              # "Add group", not "Save group": it appends this group to the
              # set being built. "Save as scenario" is the one that commits
              # the whole set, and two buttons both saying save invited the
              # reader to press the wrong one and wonder why nothing was
              # stored.
              actionButton("save_rule", "Add group",
                           class = "btn btn-addgrp"),
              textInput("man_name", NULL, "manual_1", width = "100%"),
              actionButton("save_manual", "Save as scenario",
                           class = "btn btn-savescn"))),
          alloc_box(),
          fluidRow(
            column(12,
          tabsetPanel(id = "man_out", type = "tabs",
                        tabPanel("Overview",
              h4("What is assigned, and what is still missing"),
              span(class = "note",
                   "One row per rule in the order it is tested, with the ",
                   "running total. The last row is whatever no rule has ",
                   "claimed yet - the gap to close."),
              uiOutput("man_check"),
              tbl_slot("man_progress"),
              h4("Strata still unassigned"),
              span(class = "note", "Heaviest first, so the biggest hole is on top."),
              tbl_slot("man_missing"),
),
            tabPanel("Strata to raise",
              h4("The data being raised"),
              span(class = "note",
                   "One row per stratum: the columns that define it, its ",
                   "landings, discards, rate and coverage, its status, and ",
                   "which rule raises it. Heaviest first."),
              tbl_slot("man_raise_tbl")),
            tabPanel("Data used to raise",
              h4("The donor pools (matchedDataCond)"),
              span(class = "note",
                   "The same strata, grouped by how they SUPPLY the raising. ",
                   "usable_donor is FALSE where a donor bar excluded it."),
              tbl_slot("man_donor_tbl")),
            tabPanel("Rivers",
              uiOutput("man_check2"),
              h4("Where each rule draws its data from"),
              span(class = "note",
                   "One river per rule: the strata it raises on the left, the ",
                   "donor pool it draws from on the right."),
              uiOutput("man_rule_rivers")),
            tabPanel("Code",
              h4("strataCond / matchedDataCond"),
              span(class = "note",
                   "The two lists as they would be written in a script - ",
                   "paste them straight into the raising call. strataCond ",
                   "is written as plain clauses (first match wins); the ",
                   "partition is applied for you. matchedDataCond is not ",
                   "complemented, because donors are reusable."),
              verbatimTextOutput("man_conds")),
          )))))),

      conditionalPanel("input.sec3 == 'stgraise'",
        div(style = "padding:8px 4px",
          pick_bar("raise_mode",
                   list(compare = "Compare allocations", final = "Final raising"),
                   "compare"),
          conditionalPanel("input.raise_mode != 'final'", 
          sidebarLayout(
            sidebarPanel(width = 3,
              h4("Comparison plot"),
              # Fleet by default rather than (none): with no X axis the panel
              # opened blank and looked broken, when the commonest first
              # question - how do the allocations differ by fleet - is
              # exactly what it should have been showing.
              role_chips("cmp", c(x = "X axis", facet = "Facet"),
                         c("metier4", ""), stacked = TRUE),
              span(class = "note",
                   "Pick the keys the allocations share; leave Facet on ",
                   "(none) for a single panel."),
              apply_bar("cmp"),
              tags$hr(),
              h4("Saved group allocations"),
              span(class = "note",
                   "Allocations live in this session only. Download writes a ",
                   ".json to your browser's download folder; load it back next ",
                   "year to reuse the setup."),
              downloadButton("dl_sc", "Download (.json)", class = "btn-sm"),
              fileInput("up_sc", "Load (.json)", accept = ".json"),
              selectInput("del_sc_id", "Remove", choices = character(0)),
              actionButton("del_sc", "Remove", class = "btn-sm btn-danger")),
            mainPanel(width = 9,
              tbl_slot("sc_tbl"),
              uiOutput("cmp_note"),
              div(class = "legend-strip", plotOutput("cmp_legend", height = "110px")),
              plot_slot("cmp_plot", capture = TRUE),
              h4("Discards by allocation"),
              span(class = "note", "Imported = already in the data. The remaining ",
                   "columns are the estimated total after raising, one per allocation."),
              tbl_slot("cmp_tbl")))),
          conditionalPanel("input.raise_mode == 'final'", final_ui()))))),

  # =========================================================
  tabPanel("3. Length",
    div(style = "padding:8px 4px",
      pick_bar("sec4", list(stgexpl = "Exploration",
                            stggrp = "Allocation groups",
                            stgraise = "Raising"), "stgexpl"),
      tags$hr(),
      conditionalPanel("input.sec4 == 'stgexpl'",
        pick_bar("sec4x", list(stgexpl = "Sampling coverage",
                               lengthvar = "Numbers at length",
                               weightvar = "Mean weight at length"), "stgexpl"),
        tags$hr(),
        conditionalPanel("input.sec4x == 'stgexpl'", len_coverage_ui("len")),
        conditionalPanel("input.sec4x == 'lengthvar'", len_numbers_ui("len")),
        conditionalPanel("input.sec4x == 'weightvar'", len_meanw_ui("len"))),
      conditionalPanel("input.sec4 == 'stggrp'", len_alloc_ui("len")),
      conditionalPanel("input.sec4 == 'stgraise'",
                       len_raise_ui("len", "Length")))),

  # =========================================================
  tabPanel("4. Age",
    div(style = "padding:8px 4px",
      pick_bar("sec5", list(stgexpl = "Exploration",
                            stggrp = "Allocation groups",
                            stgraise = "Raising"), "stgexpl"),
      tags$hr(),
      conditionalPanel("input.sec5 == 'stgexpl'",
        pick_bar("sec5x", list(stgexpl = "Sampling coverage",
                               agevar = "Numbers at age",
                               ageweightvar = "Mean weight at age"), "stgexpl"),
        tags$hr(),
        conditionalPanel("input.sec5x == 'stgexpl'", len_coverage_ui("age")),
        conditionalPanel("input.sec5x == 'agevar'", len_numbers_ui("age")),
        conditionalPanel("input.sec5x == 'ageweightvar'", len_meanw_ui("age"))),
      conditionalPanel("input.sec5 == 'stggrp'", len_alloc_ui("age")),
      conditionalPanel("input.sec5 == 'stgraise'",
                       len_raise_ui("age", "Age")))),

  # =========================================================
  # The script is written in ICES TAF shape: read -> model -> present.
  # The model stage is four files rather than one because the discard
  # raising and the length/age allocation are separate decisions taken at
  # separate times, and in each case the exploration that justifies a
  # grouping is worth keeping apart from the grouping itself.
  tabPanel("5. R script",
    tabsetPanel(id = "sec6",
      tabPanel("Live script (full)", log_ui()),
      tabPanel("data", taf_ui("data")),
      tabPanel("model",
        tabsetPanel(id = "sec6m",
          tabPanel(TAF_LABEL[["model_explore_discards"]],
                   taf_ui("model_explore_discards")),
          tabPanel(TAF_LABEL[["model_allocate_raise_discards"]],
                   taf_ui("model_allocate_raise_discards")),
          tabPanel(TAF_LABEL[["model_explore_length_age"]],
                   taf_ui("model_explore_length_age")),
          tabPanel(TAF_LABEL[["model_allocate_length_age"]],
                   taf_ui("model_allocate_length_age")))),
      tabPanel("output", taf_ui("output")),
      tabPanel("report", taf_ui("report")),
      tabPanel("Fragments", fragments_ui())))
)

startup_modal <- function() modalDialog(
  title = NULL, size = "l", easyClose = FALSE, fade = TRUE,
  footer = tagList(actionButton("start_go", "Start", class = "btn btn-run")),
  div(style = "text-align:center;margin-bottom:10px",
      h3(style = "margin:0;color:#12324f", "RCEF Workbench"),
      div(class = "note",
          "Sampling coverage, group allocation and raising for ICES ",
          "catch data. Set the run up here; the answers decide which ",
          "tabs open and which files are asked for.")),
  fluidRow(
    column(6,
      h4("Stock"),
      textInput("cfg_stock", "Stock code", "", placeholder = "bll.27.3a47de"),
      textInput("cfg_coord", "Stock coordinator", "", placeholder = "name"),
      numericInput("cfg_year", "Year", as.integer(format(Sys.Date(), "%Y")) - 1,
                   1990, 2100, 1),
      h4("Where the input files are"),
      dir_picker("cfg_dir", "."),
      span(class = "note",
           "Type a path, or click the folder to browse. It is checked as ",
           "soon as you start.")),
    column(6,
      h4("Starting point"),
      radioButtons("cfg_entry", NULL,
        c("InterCatch exchange format (HI / SI / SD)" = "ic_exchange",
          "InterCatch output format (StockOverview / Numbers / MeanWeigth)" = "ic_output",
          "CEF files already prepared" = "cef")),
      span(class = "note",
           "Only the matching loader stays enabled; the others are ",
           "locked so two routes cannot be mixed by accident."),
      tags$hr(),
      h4("What has to be raised or allocated"),
      checkboxInput("cfg_disc", "Discards", TRUE),
      span(class = "note", "Always required."),
      checkboxInput("cfg_len", "Lengths", FALSE),
      checkboxInput("cfg_age", "Ages", FALSE),
      span(class = "note",
           "Leaving lengths and ages off means no distribution file is ",
           "asked for, and those sections stay closed."))))

server <- function(input, output, session) {

  # =========================================================
  #  Shared helpers, defined FIRST
  # =========================================================
  # These were defined further down, next to the tabs that use them.
  # That worked for anything called from inside a reactive - lookup is
  # lazy - but not for a module server that calls one while the server
  # body is still executing: length_alloc_server() is invoked ~1000
  # lines above where exclusion_server() used to be defined, so it hit
  # 'could not find function', the server function aborted, and every
  # definition after that point never ran. Hence the cascade of
  # unrelated-looking 'could not find flt_inputs' errors: they were all
  # one failure.
  #
  # Server half of strata_def_ui(). Nested inside server() rather than a
  # top-level function like the discard/manual servers below, because it
  # needs input/output/session directly - dir_picker_server() takes
  # those as explicit arguments instead, and either shape works, but
  # this one has no other reason to live outside the server body.
  #
  # Returns a reactive() of the resolved column names. Called once per
  # strata-definition box; the id passed here must match the one given
  # to strata_def_ui().
  strata_def_server <- function(p) {
    ns <- function(x) paste0(p, "_", x)
    observe({
      ch <- c(NONE, gvars())
      for (i in 1:5) {
        id  <- ns(paste0("sd", i))
        cur <- isolate(input[[id]])
        def <- DEFAULT_STRATA_VARS[i] %||% ""
        sel <- if (!is.null(cur) && (identical(cur, "") || cur %in% ch)) cur
               else if (def %in% ch) def else ""
        updateSelectInput(session, id, choices = ch, selected = sel)
      }
    })
    dims <- reactive({
      v <- if (identical(input[[ns("sdmode")]] %||% "asis", "custom")) {
        picked <- vapply(1:5, function(i) input[[ns(paste0("sd", i))]] %||% "",
                         character(1))
        avail(unique(Filter(nzchar, picked)))
      } else avail(DEFAULT_STRATA_VARS)
      if (!length(v)) avail(DEFAULT_STRATA_VARS) else v
    })
    output[[ns("sd_summary")]] <- renderUI({
      v <- dims()
      div(class = "note", style = "margin-top:5px;color:#12324f;font-weight:600",
          "Strata resolved to: ", paste(v, collapse = " + "))
    })
    dims
  }
  # Server half of the compact strata definition embedded in Key 1 of a
  # hierarchical builder (see key1_strata_ui() further down, defined
  # inline where it is used since it is small and specific to one
  # layout). Same "as provided / customize" shape as strata_def_server(),
  # but four family-named slots instead of five generic ones, each
  # restricted to its OWN family's variables via key_vocab_grouped() -
  # the Fleet slot only ever offers Fleet columns.
  own_key_strata_server <- function(p) {
    ns <- function(x) paste0(p, "_", x)
    observe({
      kv <- key_vocab_grouped(tryCatch(dat(), error = function(e) NULL))
      upd <- function(fam, id, dflt) {
        ch <- c(NONE, kv[[fam]] %||% character(0))
        cur <- isolate(input[[id]])
        sel <- if (!is.null(cur) && (identical(cur, "") || cur %in% ch)) cur
               else if (dflt %in% ch) dflt else ""
        updateSelectInput(session, id, choices = ch, selected = sel)
      }
      upd("Fleet",   ns("fleet"),   "fleetValue")
      upd("Country", ns("country"), "Country")
      upd("Area",    ns("area"),    "areaValue")
      upd("Season",  ns("season"),  "Season")
    })
    dims <- reactive({
      v <- if (identical(input[[ns("mode")]] %||% "asis", "custom")) {
        picked <- c(input[[ns("fleet")]], input[[ns("country")]],
                   input[[ns("area")]], input[[ns("season")]])
        avail(unique(Filter(nzchar, picked)))
      } else avail(DEFAULT_STRATA_VARS)
      if (!length(v)) avail(DEFAULT_STRATA_VARS) else v
    })
    output[[ns("summary")]] <- renderUI({
      div(class = "note", style = "margin-top:3px;color:#12324f;font-weight:600",
          "Own stratum resolved to: ", paste(dims(), collapse = " + "))
    })
    dims
  }


  # ---------------------------------------------------------
  #  Exploration filter
  # ---------------------------------------------------------
  # dat()          -> what gets GROUPED and RAISED. Never filtered here.
  # explore_dat()  -> what gets PLOTTED. Filtered.
  #
  # Two objects rather than one switch, because the failure mode of a
  # single filtered object is silent and expensive: a stock raised on one
  # metier while the figures looked complete. Anything feeding
  # recommend_raising(), the manual builder, the donor pool or
  # catchRaising() reads dat() and cannot see the filter at all.
  expf_levels <- reactive({
    d <- tryCatch(dat(), error = function(e) NULL)
    if (is.null(d)) list() else explore_filter_levels(d)
  })

  # One filter per figure tab. The inputs are named <prefix>_flt_<column>,
  # so two tabs never share a control and pinning metier4 on the coverage
  # figure leaves the discard-rate figure alone.
  filter_prefixes <- reactiveVal(character(0))

  # Held in rv$flt, one named list per figure tab, rather than read back
  # out of a dozen selectize inputs. The chip builder adds one variable at
  # a time, so there is no input to read for a variable nobody has picked.
  flt_inputs <- function(p) clean_filters(rv$flt[[p]] %||% list())

  flt_set <- function(p, var, vals) {
    cur <- rv$flt[[p]] %||% list()
    if (!length(vals)) cur[[var]] <- NULL else cur[[var]] <- vals
    rv$flt[[p]] <- cur
  }

  # Registered lazily: a renderUI for a tab that has never been opened
  # never runs, so the registry fills in as tabs are visited and the
  # script only ever reports filters that were really set.
  # The variables offered first, in the order a fisheries scientist
  # reaches for them. Everything else is behind "more variables", the same
  # split the clause builder uses.
  FLT_CORE <- c("metier4", "metier5", "metier6", "Country",
                "ICESDivision", "ICESSubArea", "Season", "FleetType")

  filter_server <- function(p) {
    id <- function(x) paste0(p, "_flt_", x)

    output[[id("chips")]] <- renderUI({
      lv <- expf_levels()
      if (!length(lv))
        return(span(class = "note", "Nothing filterable in this table."))
      cur <- filter_prefixes()
      if (!p %in% cur) filter_prefixes(c(cur, p))
      core  <- intersect(FLT_CORE, names(lv))
      extra <- setdiff(names(lv), core)
      sel <- input[[id("var")]] %||% (if (length(core)) core[1] else names(lv)[1])
      tagList(div(tags$b("Variable", style = "font-size:12px")),
              var_chips(id("var"), core, extra, sel))
    })

    output[[id("vals")]] <- renderUI({
      lv <- expf_levels()
      v <- input[[id("var")]] %||% (if (length(lv)) names(lv)[1] else NULL)
      if (is.null(v) || is.null(lv[[v]])) return(NULL)
      selectizeInput(id("valsel"), NULL, choices = lv[[v]],
                     selected = isolate(rv$flt[[p]][[v]]),
                     multiple = TRUE, width = "100%",
                     options = list(placeholder = paste0("any ", v)))
    })

    observeEvent(input[[id("add")]], {
      lv <- expf_levels()
      v <- input[[id("var")]] %||% (if (length(lv)) names(lv)[1] else NULL)
      req(!is.null(v))
      flt_set(p, v, input[[id("valsel")]])
    })

    observeEvent(input[[id("clear")]], rv$flt[[p]] <- list())

    observeEvent(input[[id("del")]], {
      flt_set(p, input[[id("del")]], NULL)
    })

    # Everything currently applied, one removable pill each. The overview
    # is the point: with the values hidden behind a chip you cannot see
    # what is on without it.
    output[[id("banner")]] <- renderUI({
      f <- flt_inputs(p)
      if (!length(f)) return(NULL)
      div(style = paste("background:#fff8e8;border:1px solid #d9a441;",
                        "border-left:6px solid #d9a441;border-radius:6px;",
                        "padding:5px 10px;margin-bottom:6px;font-size:12px;",
                        "color:#7a4a00"),
          tags$b("Filtered view - "), filter_label(f),
          tags$span(class = "note", style = "margin-left:8px",
                    "figure only; grouping and raising use the full table")) })

    output[[id("list")]] <- renderUI({
      f <- flt_inputs(p)
      if (!length(f))
        return(div(class = "note", style = "margin-top:6px",
                   "No filter - the whole table."))
      div(class = "fltapplied", lapply(names(f), function(v)
        tags$span(class = "fltpill",
          tags$span(tags$b(v), ": ",
                    if (length(f[[v]]) > 3)
                      sprintf("%d values", length(f[[v]]))
                    else paste(f[[v]], collapse = ", ")),
          tags$span(class = "x", HTML("&times;"), title = paste("remove", v),
            onclick = sprintf(
              "Shiny.setInputValue('%s','%s',{priority:'event'})",
              id("del"), v)))))
    })

    # Rows can survive a filter while weight does not. A figure drawn on
    # 40% of the rows and 2% of the landings is not a view of the fleet,
    # so both numbers sit under the control rather than in a tooltip.
    output[[paste0(p, "_flt_note")]] <- renderUI({
      f <- flt_inputs(p)
      if (!length(f)) return(NULL)
      d <- tryCatch(dat(), error = function(e) NULL)
      if (is.null(d)) return(NULL)
      cv <- tryCatch(filter_coverage(d, f), error = function(e) NULL)
      if (is.null(cv)) return(NULL)
      div(class = "note", style = "color:#9a5000",
          sprintf("%d of %d rows (%.0f%%)", cv$n_kept, cv$n_all, cv$pct_rows),
          if (!is.null(cv$pct_weight))
            sprintf(", %.1f%% of the weight.", cv$pct_weight) else ".")
    })
  }

  # every prefix that owns a figure and therefore a filter box
  # The grouping tabs are deliberately absent: a cascade must be built on
  # the whole table, and a filter there would be a foot-gun.
  for (.p in c("cov", "dr",
               as.vector(outer(c("len", "age"), c("cov", "n", "w"),
                               paste, sep = "_"))))
    local({ pp <- .p; filter_server(pp) })

  # the filtered table for one tab
  # "Nothing to plot." on a filtered panel is the wrong message: it reads
  # as a fault in the data when it is usually a filter that went one step
  # too far. plot_coverage() drops strata with no landings, so filtering
  # to a single thin stratum legitimately leaves nothing to draw - and the
  # user, seeing the plot replaced by a bare sentence, reports that Apply
  # "did nothing".
  no_plot_msg <- function(p, base = "Nothing to plot with these settings.") {
    f <- flt_inputs(p)
    if (!length(f)) return(base)
    paste0(base, " The filter on this figure (", filter_label(f),
           ") leaves no stratum to draw - clear or widen it.")
  }

  explore_dat <- function(p) {
    d <- dat(); f <- flt_inputs(p)
    if (!length(f)) return(d)
    out <- apply_filters(d, f)
    validate(need(nrow(out) > 0,
                  paste0("No rows left after this figure's filter (",
                         filter_label(f), "). Clear it to see the figure.")))
    out
  }

  # every filter that is actually set, for the emitted script
  explore_filters_all <- reactive({
    out <- list()
    for (p in names(rv$flt %||% list())) {
      f <- flt_inputs(p); if (length(f)) out[[p]] <- f }
    out
  })


  # ---------------------------------------------------------
  #  Exclusion, one server for all four blocks
  # ---------------------------------------------------------
  # Every block resolves to the same thing: a set of stratum keys. A
  # threshold is turned into keys HERE, against the stratum table,
  # because the condition that reaches catchRaising() is evaluated on
  # CEF rows and those carry no `coverage` column - coverage is a
  # property of a stratum, computed from many rows. So what the engine
  # receives is always an explicit list, whichever route produced it.
  #
  # `strata_of` is a function rather than a fixed prefix because the
  # biology blocks are built on a biology link, not on build_strata().
  excl_num <- function(p, which) {
    x <- input[[paste0(p, "_x", which)]]
    if (is.null(x) || !length(x) || all(is.na(x))) return(NA_real_)
    v <- as.numeric(x)[1]
    # the slider's floor (0) means "no bar" - the same as a blank box did
    if (isTRUE(v <= 0)) NA_real_ else v
  }

  exclusion_server <- function(p, strata_of, dims_of,
                               metrics = c("coverage", "measurements", "rate"),
                               dr_of = function() c(NA, NA),
                               scope_of = function() NULL) {
    tbl <- function() {
      st <- try(strata_of(), silent = TRUE)
      if (inherits(st, "try-error") || is.null(st) || !nrow(st)) NULL else st
    }

    # A second strata table, at the resolution the data was SUBMITTED
    # at (the strata-definition box's "Use as provided"), built fresh
    # from the raw catch data rather than re-aggregating tbl() - tbl()
    # is already collapsed to dims_of() and the finer columns are gone
    # by the time it exists. Carries both resolutions' columns, which is
    # what lets a submitted-level pick be translated up to dims_of() via
    # roll_up_excluded_keys() without guessing.
    fine_tbl <- reactive({
      d <- tryCatch(dat(), error = function(e) NULL)
      if (is.null(d)) return(NULL)
      fd <- unique(c(DEFAULT_STRATA_VARS, dims_of()))
      tryCatch(build_strata(d, fd), error = function(e) NULL)
    })

    # The measurements bar, scaled to the data. A fixed 0-100000 slider
    # put every real value in the first few pixels; the ceiling here is
    # the largest count actually present, so the whole track is usable.
    # Min-only, like coverage: the handle is the floor and the shaded
    # side is the side that qualifies.
    if ("measurements" %in% metrics)
      output[[paste0(p, "_xmeas_ui")]] <- renderUI({
        st <- tbl()
        top <- if (is.null(st) || !"n_measurements" %in% names(st)) 1000
               else max(c(1, suppressWarnings(
                 as.numeric(st$n_measurements))), na.rm = TRUE)
        top <- max(200, ceiling(top / 50) * 50)
        cur <- input[[paste0(p, "_xmeas")]]
        val <- if (is.null(cur) || all(is.na(cur))) min(100, top)
               else min(as.numeric(cur)[1], top)
        sliderInput(paste0(p, "_xmeas"), "measurements below",
                    0, top, c(val, top), step = max(1, round(top / 200)),
                    width = "100%")
      })

    # The trips bar, same reasoning and same shape as measurements: a
    # data-scaled minimum, rendered server-side because the ceiling is
    # not known until the strata table exists. Trip counts run far
    # smaller than measurement counts (tens, not hundreds), so the
    # default floor and step are scaled down to match.
    if ("trips" %in% metrics)
      output[[paste0(p, "_xtrips_ui")]] <- renderUI({
        st <- tbl()
        top <- if (is.null(st) || !"numTrips" %in% names(st)) 20
               else max(c(1, suppressWarnings(
                 as.numeric(st$numTrips))), na.rm = TRUE)
        top <- max(5, ceiling(top / 2) * 2)
        cur <- input[[paste0(p, "_xtrips")]]
        val <- if (is.null(cur) || all(is.na(cur))) min(2, top)
               else min(as.numeric(cur)[1], top)
        sliderInput(paste0(p, "_xtrips"), "trips below",
                    0, top, c(val, top), step = 1, width = "100%")
      })

    # Submitted-level picks, rolled up to dims_of() resolution so
    # everything downstream (the expression, the report, the pool
    # summary) sees ONE unified named set and never has to know two
    # resolutions were involved.
    named_resolved <- reactive({
      agg <- input[[paste0(p, "_xnamed")]] %||% character(0)
      sub <- input[[paste0(p, "_xnamed_sub")]] %||% character(0)
      ft  <- fine_tbl()
      rolled <- if (length(sub) && !is.null(ft))
        roll_up_excluded_keys(ft, sub, DEFAULT_STRATA_VARS, dims_of())
      else character(0)
      unique(c(agg, rolled))
    })

    # One settings object, so the expression on screen, the expression in
    # the emitted script and the table below can never disagree.
    spec <- reactive({
      dr <- if ("rate" %in% metrics) dr_of() else c(NA, NA)
      list(named     = named_resolved(),
           cov_min   = if ("coverage" %in% metrics) excl_num(p, "cov") else NA,
           meas_min  = if ("measurements" %in% metrics) excl_num(p, "meas") else NA,
           trips_min = if ("trips" %in% metrics) excl_num(p, "trips") else NA,
           dr_min    = dr[1], dr_max = dr[2])
    })

    # What goes in the CODE: a comparison on the metric columns, not a
    # list of several hundred stratum keys. add_stratum_metrics() puts
    # coverage / discard_rate / n_measurements on the CEF rows, so the
    # engines evaluate this directly.
    expr <- reactive({
      sp <- spec()
      exclusion_expr(sp$cov_min, sp$meas_min, sp$dr_min, sp$dr_max,
                     sp$named, dims_of(), trips_min = sp$trips_min)
    })

    # What goes in the TABLE: only strata inside this group's own scope,
    # and only the ones whose exclusion was a decision rather than
    # arithmetic - a stratum with no landings or no coverage at all was
    # never a candidate donor.
    report <- reactive({
      st <- tbl(); if (is.null(st)) return(NULL)
      sp <- spec()
      exclusion_report(st, dims_of(), sp$named, sp$cov_min, sp$meas_min,
                       sp$dr_min, sp$dr_max, scope = scope_of(),
                       trips_min = sp$trips_min)
    })

    keys <- reactive({
      d <- report(); if (is.null(d)) character(0) else d$key
    })

    output[[paste0(p, "_xui")]] <- renderUI({
      st <- tbl()
      selectizeInput(paste0(p, "_xnamed"), NULL,
        choices = if (is.null(st)) character(0)
                  else exclusion_choices(st, dims_of(),
                         order_by = input[[paste0(p, "_xsort")]] %||% "weight"),
        selected = isolate(input[[paste0(p, "_xnamed")]]),
        multiple = TRUE, width = "100%",
        options = list(placeholder = "no stratum excluded")) })

    # The submitted-resolution twin. Built on fine_tbl(), grouped down
    # to DEFAULT_STRATA_VARS rather than dims_of() - "as submitted" means
    # the four native CEF keys regardless of what the strata-definition
    # box currently resolves to, so a customised (coarser) definition
    # does not also coarsen this list.
    output[[paste0(p, "_xui_sub")]] <- renderUI({
      st <- fine_tbl()
      selectizeInput(paste0(p, "_xnamed_sub"), NULL,
        choices = if (is.null(st)) character(0)
                  else exclusion_choices(st, DEFAULT_STRATA_VARS,
                         order_by = input[[paste0(p, "_xsort")]] %||% "weight"),
        selected = isolate(input[[paste0(p, "_xnamed_sub")]]),
        multiple = TRUE, width = "100%",
        options = list(placeholder = "no stratum excluded")) })

    output[[paste0(p, "_xnote")]] <- renderUI({
      e <- expr(); n <- length(keys())
      if (is.null(e))
        return(div(class = "note", "Nothing excluded - the whole pool."))
      div(class = "note",
          tags$b(sprintf("%d stratum/strata in this group's scope are out. ", n)),
          "This is what the script carries; the table below lists which ",
          "strata it removes.",
          tags$br(),
          tags$span(style = paste("font-family:monospace;font-size:11px;",
                                  "background:#fffaf0;padding:1px 4px;",
                                  "border-radius:3px"), e)) })

    output[[paste0(p, "_xtbl")]] <- renderDT({
      validate(need(!is.null(tbl()), "No strata yet."))
      d <- report()
      validate(need(!is.null(d) && nrow(d), "Nothing excluded."))
      keep <- names(d)[vapply(d, function(x) !all(is.na(x)), logical(1))]
      d <- d[, keep, drop = FALSE]
      num <- vapply(d, is.numeric, logical(1))
      d[num] <- lapply(d[num], function(x) round(x, 3))
      datatable(d, rownames = FALSE,
                options = list(scrollX = TRUE, pageLength = 8)) })

    # The pool as it stands after every bar: what a group would actually
    # borrow from. donor_pool_summary() checks the POOL's own combined
    # coverage against a sanity threshold (50% by default) that is
    # deliberately separate from cov_min - a pool built with no
    # per-stratum floor at all can still look too thin once combined,
    # which is exactly what a per-stratum bar cannot catch.
    pool <- reactive({
      st <- tbl(); if (is.null(st)) return(NULL)
      sp <- spec()
      warn_meas  <- if ("measurements" %in% metrics) 100 else NA
      warn_trips <- if ("trips" %in% metrics) 2 else NA
      donor_pool_summary(st, dims_of(), sp$named, sp$cov_min, sp$meas_min,
                         sp$dr_min, sp$dr_max,
                         warn_cov_min = 50, warn_meas_min = warn_meas,
                         trips_min = sp$trips_min, warn_trips_min = warn_trips)
    })
    # Every coverage / trips / measurements slider above feeds spec(),
    # which feeds pool() directly and un-gated (there is no Apply here -
    # a donor bar is meant to react as you set it). A slider fires many
    # input events per second while being dragged, and donor_pool_summary()
    # is not cheap (it walks the strata table twice, once for its own
    # aggregate and once inside exclusion_overview()'s per-stratum
    # pass) - so on a real dataset the render queue never caught up with
    # the drag, and the "recalculating" overlay looked stuck rather than
    # slow. debounce() lets pool() keep recomputing on every tick
    # internally (cheap consumers can still read it live), but only
    # pool_d() - what the two expensive renders below actually use -
    # settles 400ms after the LAST tick, so a drag produces one render
    # instead of dozens.
    pool_d <- debounce(pool, 400)

    output[[paste0(p, "_used_warn")]] <- renderUI({
      s <- pool_d()
      if (is.null(s) || s$n_strata == 0)
        return(div(class = "note", style = "color:#7a1f1f;font-weight:600",
                   "Nothing is left to raise from - every stratum was excluded."))
      if (isTRUE(s$ok)) return(NULL)
      bits <- character(0)
      if (!is.na(s$n_measurements) && "measurements" %in% metrics &&
            s$n_measurements < 100)
        bits <- c(bits, sprintf("only %d measurements", round(s$n_measurements)))
      if (!is.na(s$numTrips) && "trips" %in% metrics && s$numTrips < 2)
        bits <- c(bits, sprintf("only %d trips", round(s$numTrips)))
      div(class = "note", style = "color:#9a5000;font-weight:600",
          sprintf("Warning: the combined pool covers %s%% of its weight%s. ",
                  if (is.na(s$coverage)) "an unknown" else round(s$coverage, 1),
                  if (length(bits)) paste0(" with ", paste(bits, collapse = " and "))
                  else ""),
          "That may not be enough to raise from - widen the group, loosen ",
          "a bar, or accept the risk knowingly.")
    })

    output[[paste0(p, "_used_tbl")]] <- renderDT({
      s <- pool_d()
      validate(need(!is.null(s) && s$n_strata > 0, "Nothing left to raise from."))
      d <- data.frame(
        `strata in`   = s$n_strata,
        `weight`      = round(s$weight, 2),
        `coverage %`  = if (is.na(s$coverage)) NA else round(s$coverage, 1),
        check.names = FALSE, stringsAsFactors = FALSE)
      if ("rate" %in% metrics)
        d[["discard rate"]] <- if (is.na(s$discard_rate)) NA
                                else round(s$discard_rate, 4)
      if ("trips" %in% metrics)
        d[["trips"]] <- if (is.na(s$numTrips)) NA else round(s$numTrips)
      if ("measurements" %in% metrics)
        d[["measurements"]] <- round(s$n_measurements)
      datatable(d, rownames = FALSE, options = list(dom = "t")) })

    # `bars` is what a rule should STORE: the thresholds themselves plus
    # only the hand-picked keys. `keys` is the resolved list, for the
    # table on screen. Storing the resolved list on the rule is what put
    # two hundred strata into the emitted condition.
    list(keys = keys, expr = expr, report = report, bars = spec)
  }


  showModal(startup_modal())

  # show/hide the centred busy overlay. with_busy() wraps a slow action so
  # the overlay is guaranteed to come down even if the action errors.
  set_busy <- function(show, msg = "Working...")
    session$sendCustomMessage("busy", list(show = show, msg = msg))
  with_busy <- function(msg, expr) {
    set_busy(TRUE, msg); on.exit(set_busy(FALSE))
    force(expr)
  }

  cfg <- reactiveValues(done = FALSE, stock = "", coord = "", year = NA,
                        dir = ".", entry = "cef",
                        need = c("discards"))

  observeEvent(input$start_go, {
    cfg$done <- TRUE
    cfg$stock <- input$cfg_stock %||% ""
    cfg$coord <- input$cfg_coord %||% ""
    cfg$year  <- input$cfg_year
    cfg$dir   <- input$cfg_dir %||% "."
    cfg$entry <- input$cfg_entry %||% "cef"
    cfg$need  <- c("discards",
                   if (isTRUE(input$cfg_len)) "lengths",
                   if (isTRUE(input$cfg_age)) "ages")
    # lock the routes that were not chosen
    if (cfg$entry != "cef") {
      updateRadioButtons(session, "ic_source",
        selected = if (cfg$entry == "ic_exchange") "exchange" else "output")
      updateTextInput(session, "ic_path", value = cfg$dir)
    }
    # prefix from stock and year, as asked
    updateTextInput(session, "ic_prefix",
      value = if (nzchar(cfg$stock) && !is.na(cfg$year))
        paste0(gsub("[^A-Za-z0-9]", "_", cfg$stock), "_", cfg$year, "_") else "")
    for (sec in c("4. Length", "5. Age")) {
      want <- (sec == "4. Length" && "lengths" %in% cfg$need) ||
              (sec == "5. Age"    && "ages"    %in% cfg$need)
      if (want) showTab("nav", sec) else hideTab("nav", sec)
    }
    if (cfg$entry == "cef") hideTab("nav", "1. From InterCatch")
    showNotification(sprintf("Set up for %s (%s)",
                             if (nzchar(cfg$stock)) cfg$stock else "unnamed stock",
                             paste(cfg$need, collapse = ", ")),
                     duration = 5)
    removeModal() })

  output$cfg_banner <- renderUI({
    if (!cfg$done) return(NULL)
    div(class = "note", style = "font-size:12px;color:#12324f",
        sprintf("%s | %s | %s | raising: %s",
                if (nzchar(cfg$stock)) cfg$stock else "unnamed",
                if (nzchar(cfg$coord)) cfg$coord else "no coordinator",
                cfg$dir, paste(cfg$need, collapse = ", "))) })

  # Clear the work on the CURRENT section without touching the rest. Which
  # state that is depends on which top-level tab is open.
  observeEvent(input$clear_screen, {
    sec <- input$nav %||% ""
    showModal(modalDialog(
      title = "Clear this screen?",
      sprintf("This resets the work on '%s' only. Everything on the other sections stays.", sec),
      footer = tagList(modalButton("Cancel"),
                       actionButton("clear_confirm", "Clear", class = "btn-danger")),
      easyClose = TRUE, size = "s")) })

  observeEvent(input$clear_confirm, {
    sec <- input$nav %||% ""
    if (grepl("InterCatch|Load from IC", sec)) {
      rv$ic <- NULL
    } else if (grepl("Load from CEF", sec)) {
      rv$cef_catches <- NULL; rv$dist <- NULL; rv$eff <- NULL
    } else if (grepl("Discards", sec)) {
      rv$rules <- list(); rv$clause <- list(); rv$pend_raise <- NULL
      rv$pend_source <- NULL; rv$scenarios <- list(); rv$native <- NULL
      rv$final <- NULL
    }
    removeModal()
    showNotification(sprintf("Cleared '%s'", sec), duration = 3) })

  # files the chosen route needs, and whether they are there
  REQ_FILES <- list(
    ic_exchange = c("HI", "SI", "SD"),
    ic_output   = c("StockOverview", "NumbersAtAgeLength", "MeanWeigthAtAgeLength"),
    cef         = c("catches", "distributions", "effort"))

  file_check <- reactive({
    d <- if (isTruthy(input$ic_path)) input$ic_path else cfg$dir
    pat <- REQ_FILES[[cfg$entry]]
    fs <- if (dir.exists(d)) list.files(d, recursive = TRUE) else character(0)
    need <- pat
    # distributions / age-length files are only needed if asked for
    if (cfg$entry == "cef" && !any(c("lengths","ages") %in% cfg$need))
      need <- setdiff(need, "distributions")
    if (cfg$entry == "ic_output" && !any(c("lengths","ages") %in% cfg$need))
      need <- setdiff(need, c("NumbersAtAgeLength", "MeanWeigthAtAgeLength"))
    data.frame(file = pat,
               required = pat %in% need,
               found = vapply(pat, function(k)
                 any(grepl(k, fs, ignore.case = TRUE)), logical(1)),
               stringsAsFactors = FALSE) })


  rv <- reactiveValues(sets = list(), rules = list(), clause = list(),
                       # per-figure-tab exploration filters, keyed by prefix
                       flt = list(),
                       pend_raise = NULL, pend_source = NULL, scenarios = list(),
                       ic = NULL, native = NULL, log = log_new(),
                       dist = NULL, eff = NULL, final = NULL, restore = NULL,
                       cef_catches = NULL, len_allocs = list(), len_raised = NULL, len_raised_err = NULL,
                       len_raised_runs = NULL, age_raised_runs = NULL,
                       age_raised_err = NULL,
                       age_allocs = list(), age_raised = NULL,
                       len_clause = list(), len_groups = list(),
                       len_alloc_pend = NULL, len_match_pend = NULL,
                       len_alloc = NULL,
                       age_clause = list(), age_groups = list(),
                       age_alloc_pend = NULL, age_match_pend = NULL,
                       age_alloc = NULL,
                       # plots the user explicitly kept, in the order kept;
                       # this list IS the body of output.R. kept_seq only
                       # ever increases, so a record's uid stays valid after
                       # something above it is removed.
                       kept = list(), kept_seq = 0,
                       # which of the four input routes owns the data, the
                       # files it read, and a counter that ticks whenever
                       # the data underneath everything is replaced
                       load_method = NULL, cef_files = NULL, data_version = 0)

  # =========================================================
  #  One input route at a time
  # =========================================================
  # The four routes - InterCatch exchange, InterCatch output, CEF files,
  # the RDBES database - all end up filling the same slots. Letting two of
  # them fill those slots at once produces a catch table whose provenance
  # nobody can state, and an emitted data.R that would have to claim two
  # origins for one object. So a route takes ownership, and taking it from
  # another route is a deliberate, confirmed replacement.
  data_loaded <- reactive(
    !is.null(rv$ic) || !is.null(rv$cef_catches) ||
    !is.null(rv$dist) || !is.null(rv$eff))

  # Everything downstream is derived from the catch table, so replacing the
  # table invalidates all of it. Leaving a raising or an allocation behind
  # after the data changed under it is the worst kind of stale: it looks
  # like a result.
  clear_all_data <- function() {
    rv$ic <- NULL; rv$cef_catches <- NULL; rv$dist <- NULL; rv$eff <- NULL
    rv$cef_files <- NULL
    rv$native <- NULL; rv$final <- NULL
    rv$scenarios <- list(); rv$rules <- list(); rv$clause <- list()
    rv$len_allocs <- list(); rv$age_allocs <- list()
    rv$len_raised <- NULL; rv$age_raised <- NULL
    rv$len_raised_runs <- NULL; rv$age_raised_runs <- NULL
    rv$len_groups <- list(); rv$age_groups <- list()
  }

  pending_load <- reactiveVal(NULL)

  # Run `action` under this route, asking first if another route owns the
  # data. `action` is a function so the confirm path can run exactly the
  # same code the direct path would have run.
  with_load_guard <- function(method, action) {
    cur <- rv$load_method
    if (is.null(cur) || identical(cur, method) || !isTRUE(data_loaded())) {
      rv$load_method <- method
      rv$data_version <- rv$data_version + 1
      return(action())
    }
    pending_load(list(method = method, action = action))
    showModal(modalDialog(
      title = "Replace the data already loaded?",
      div(class = "chain", style = "color:#7a1f1f;line-height:1.55",
          tags$b("This stock is already loaded from another route."), tags$br(),
          sprintf("Loaded from: %s", TAF_METHOD_LABEL[[cur]] %||% cur), tags$br(),
          sprintf("About to load: %s", TAF_METHOD_LABEL[[method]] %||% method)),
      tags$hr(),
      div(class = "note", style = "line-height:1.55",
          "Only one route can own the data at a time. Continuing will ",
          "discard the catch, distribution and effort tables loaded so far, ",
          tags$b("and everything built on them"), " - saved group ",
          "allocations, raisings, and length and age allocations. The ",
          "emitted data.R will then describe the new route only."),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("load_replace_ok", "Replace everything",
                     class = "btn-danger")),
      easyClose = TRUE, size = "m"))
  }

  observeEvent(input$load_replace_ok, {
    p <- pending_load(); removeModal(); req(!is.null(p))
    clear_all_data()
    rv$load_method <- p$method
    rv$data_version <- rv$data_version + 1
    pending_load(NULL)
    p$action()
    showNotification(sprintf("Replaced. Data now comes from %s.",
                             TAF_METHOD_LABEL[[p$method]] %||% p$method),
                     type = "warning", duration = 7) })

  # A banner on every load tab, so the owning route is visible before a
  # click rather than only in the dialog that interrupts it.
  load_owner_note <- reactive({
    if (!isTRUE(data_loaded()) || is.null(rv$load_method))
      return(div(class = "note", "No data loaded yet."))
    div(class = "chain", style = "color:#14532d",
        sprintf("Data is loaded from: %s.",
                TAF_METHOD_LABEL[[rv$load_method]] %||% rv$load_method),
        tags$br(),
        tags$span(style = "color:#9a5000",
          "Loading from a different route will replace it and everything ",
          "built on it.")) })
  for (.k in LOAD_OWNER_SLOTS) local({
    k <- .k
    output[[paste0("load_owner_", k)]] <- renderUI(load_owner_note()) })

  # =========================================================
  #  Apply gates
  # =========================================================
  # `sig` is a reactive returning the panel's current control settings as a
  # plain list. `gate_of()` returns the settings the panel should DRAW from,
  # which lag the live ones until Apply is pressed.
  #
  # Three things refresh them:
  #   * the Apply button - the point of the exercise;
  #   * the first time a usable signature exists, so the panel is not blank
  #     on arrival (an Apply button that hides the plot until pressed reads
  #     as a broken tab, not as a feature);
  #   * new data, which is not a control change and must land unasked.
  # `live` is the settings that BYPASS the gate. Switching plot type is one:
  # picking Bubbles instead of Heatmap asks to see a different picture of the
  # same thing, and making that wait for a second click is the opposite of
  # what the gate is for. The gate exists to stop a half-configured redraw
  # firing on every keystroke of a CONFIGURATION - axes, thresholds, level of
  # detail - not to slow down a toggle.
  gate_reg <- list()

  gate_of <- function(p, sig, refresh = NULL, live = NULL) {
    applied <- reactiveVal(NULL)
    observe({ if (is.null(applied())) applied(sig()) })
    observeEvent(input[[paste0(p, "_apply")]], applied(sig()), ignoreInit = TRUE)
    observeEvent(rv$data_version, applied(sig()), ignoreInit = TRUE)
    # a result the panel exists to show (a raising, an allocation) is not a
    # control either; producing one must show it
    if (!is.null(refresh))
      observeEvent(refresh(), applied(sig()), ignoreInit = TRUE)
    # A live setting must also refresh the frozen ones, or the two drift:
    # switch to Bubbles after editing an axis and you would get the new type
    # drawn with the old axis - a configuration nobody chose.
    if (!is.null(live)) observeEvent(live(), applied(sig()), ignoreInit = TRUE)

    stale <- reactive(!is.null(applied()) && !identical(applied(), sig()))
    # Without this the button is a mystery: nothing on screen says whether
    # what you are looking at reflects the controls beside it.
    output[[paste0(p, "_apply_note")]] <- renderUI(
      if (isTRUE(stale()))
        div(class = "applystale", "Settings changed", tags$br(),
            "press Apply to redraw")
      else div(class = "applyok", "Up to date"))

    # Registered so a gate inside a module can be reached from a test.
    # Every panel builds its gate in module-local scope, which meant the
    # only way to check "does Apply actually redraw this one" was to open
    # the app and look - and the coverage panel worked while a biology
    # panel did not, for a whole release, because nothing could see it.
    gate_reg[[p]] <<- list(now = applied, stale = stale, sig = sig)
    list(now = applied, stale = stale)
  }

  # Build a reactive that recomputes only when its gate opens.
  #
  # The body is evaluated inside isolate(), so it can go on reading
  # input$... and the panel's own helpers exactly as before - no rewrite of
  # every builder to thread a settings list through it. The values it reads
  # are the ones live at the moment the gate opened, which is precisely the
  # moment Apply was pressed.
  # `live` is read OUTSIDE the isolate, so the reactive re-runs when it
  # changes; everything else is read inside and therefore frozen.
  gated <- function(g, expr, live = NULL) {
    e <- substitute(expr); env <- parent.frame()
    reactive({ g$now(); if (!is.null(live)) live(); isolate(eval(e, env)) })
  }

  # =========================================================
  #  Keeping a plot for the script, and saving it as an image
  # =========================================================
  # Registering a plot here does two things: wires its "Keep for script"
  # button to append the CURRENT configuration to rv$kept, and wires its
  # "Save image" button to a download of the plot as drawn.
  #
  # `call` and `prep` are functions rather than values because they have to
  # be read at the moment the button is pressed - that is the whole point.
  # Evaluating them at registration time would freeze whatever the controls
  # happened to say when the app started.
  # `section` says which TAF script the plot is written into. Exploratory
  # figures belong beside the analysis that produced them, not in a distant
  # output.R - so a coverage plot lands in model_explore_discards.R and a
  # result plot in output.R. Every one of them is still SAVED to output/.
  keep_plot <- function(id, obj, title, note = NULL, call = NULL, prep = NULL,
                        section = "output", gate = NULL, w = 10, h = 7) {
    val <- function(x, fallback = NULL)
      tryCatch(if (is.function(x)) x() else x, error = function(e) fallback)

    observeEvent(input[[paste0(id, "_keep")]], {
      # `call` and `title` read the controls live, but the plot on screen was
      # drawn from the settings the last Apply froze. With the panel stale
      # those two disagree, and keeping would write a block that does not
      # reproduce the picture the user is looking at - the one failure this
      # whole feature exists to prevent.
      if (!is.null(gate) && isTRUE(gate$stale())) {
        showNotification(
          paste("Press Apply first. The plot on screen was drawn from the",
                "previous settings, so keeping now would record something",
                "you cannot see."),
          type = "warning", duration = 7)
        return()
      }
      cl <- val(call)
      if (!length(cl)) {
        showNotification(
          "Nothing to keep yet - the plot has to be drawn first.",
          type = "warning", duration = 4)
        return()
      }
      ttl <- val(title, id)
      rv$kept_seq <- rv$kept_seq + 1
      rv$kept <- c(rv$kept, list(list(
        uid = rv$kept_seq, section = section,
        id = id, title = ttl, note = val(note), prep = val(prep), call = cl,
        stamp = format(Sys.time(), "%H:%M:%S"))))
      showNotification(
        sprintf("Kept '%s'. output.R now has %s plot%s.", ttl,
                length(rv$kept), if (length(rv$kept) == 1) "" else "s"),
        type = "message", duration = 4)
    }, ignoreInit = TRUE)

    output[[paste0(id, "_png")]] <- downloadHandler(
      filename = function()
        sprintf("%s_%s.png", taf_slug(val(title, id)),
                format(Sys.time(), "%Y%m%d_%H%M%S")),
      content = function(f) {
        p <- tryCatch(obj(), error = function(e) NULL)
        if (is.null(p)) {
          # a broken download is worse than an honest one: write a legible
          # placeholder rather than letting the handler error out
          grDevices::png(f, 1000, 700, res = 110)
          on.exit(grDevices::dev.off(), add = TRUE)
          graphics::plot.new()
          graphics::text(0.5, 0.5, "Nothing to save - the plot is not drawn.")
          return()
        }
        ggplot2::ggsave(f, p, width = w, height = h, dpi = 300, bg = "white")
      })
  }

  # =========================================================
  #  SECTION 4 - Length sampling
  #  catchCategory is a stratum key here, not a tab, so the same
  #  three views cover landings and discards together or apart.
  # =========================================================
  # Selections are real columns on the length and age tabs too, so this
  # is the identity apart from the one alias the biology views add.
  # The columns a length or age tab aggregates to: whatever its own role
  # pickers put on screen, on the same principle as panel_dims() for the
  # discard figures. No level-of-detail control any more - the
  # resolution IS the variable choice.
  len_dims <- function(p) {
    v <- unique(Filter(nzchar, unlist(lapply(
      c("cov", "n", "w"), function(view)
        vapply(c("y", "fill", "facet", "shape"), function(r)
          input[[paste0(p, "_", view, "_r_", r)]] %||% "", character(1))),
      use.names = FALSE)))
    v <- setdiff(v, "Catch cat.")
    v <- avail(v)
    if (length(v)) v else avail(DEF_DIMS)
  }

  len_dim_col <- function(p, label) {
    if (is.null(label) || !nzchar(label)) return(NULL)
    if (label == "Catch cat.") return("catchCategory")
    unname(label)
  }

  length_server <- function(p, type = "LengthTotal") {
    what_word <- if (identical(type, "Age")) "age" else "length"
    thr_cov   <- reactive(input[[paste0(p, "_thr_cov")]] %||% 50)
    thr_meas  <- reactive(input[[paste0(p, "_thr_meas")]] %||% 0)
    thr_trips <- reactive(input[[paste0(p, "_thr_trips")]] %||% 2)
    dims_of <- function()
      unique(c("catchCategory", len_dims(p)))

    dist_tbl <- reactive({
      if (!is.null(rv$dist)) rv$dist
      else if (!is.null(rv$ic$distributions)) as.data.frame(rv$ic$distributions)
      else NULL })

    # the category icons return catlan / catdis; one is always chosen
    cc_of <- function(view) {
      v <- input[[paste0(p, "_", view, "_cc")]] %||% "catlan"
      if (identical(v, "catdis")) "Dis" else "Lan" }
    cc_word <- function(view)
      if (identical(cc_of(view), "Dis")) "discards" else "landings"

    linked_for <- function(view) {
      d <- tryCatch(explore_dat(paste0(p, "_", view)), error = function(e) NULL)
      req(!is.null(d), !is.null(dist_tbl()))
      link_dist_to_strata(d, dist_tbl(), dims_of(), cc_of(view), type) }

    blink_for <- function(view) {
      d <- tryCatch(explore_dat(paste0(p, "_", view)), error = function(e) NULL)
      req(!is.null(d))
      biology_link(d, dist_tbl(), dims_of(), cc_of(view), type) }

    rc <- function(view, role) len_dim_col(p, input[[paste0(p, "_", view, "_r_", role)]])

    # Same vocabulary as the discard figures. Without this the length and
    # age pickers were left with the placeholder role_chips() ships -
    # "-- none --" and nothing else - so no variable could be chosen at
    # all.
    local({
      spec <- list(cov = c(y = "metier4", fill = "Country", facet = "ICESDivision"),
                   n   = c(fill = "Country", facet = "metier4", shape = ""),
                   w   = c(fill = "metier4", facet = "Country", shape = ""))
      for (.v in names(spec)) local({
        v <- .v; d <- spec[[v]]
        observe({
          ch <- c(NONE, "Catch category" = "catchCategory", gvars())
          for (r in names(d)) {
            id  <- paste0(p, "_", v, "_r_", r)
            cur <- isolate(input[[id]])
            sel <- if (is.null(cur)) { if (d[[r]] %in% ch) d[[r]] else "" }
                   else if (!nzchar(cur) || cur %in% ch) cur else ""
            updateSelectInput(session, id, choices = ch, selected = sel)
          }
        })
      })
    })

    observe({
      id  <- paste0(p, "_w_pt")
      cur <- isolate(input[[id]])
      ch  <- c("-- none (average the rest) --" = "", gvars())
      updateSelectInput(session, id, choices = ch,
                        selected = if (!is.null(cur) && cur %in% ch) cur else "")
    })

    # One gate per view. The biology builders are the most expensive things
    # in the app - biology_link() and stratum_measurements() both walk the
    # whole distributions table - so an ungated dropdown here costs seconds
    # per step, and the level-of-detail box re-aggregates everything.
    i_ <- function(x) input[[paste0(p, "_", x)]]
    lv_sig <- function() list(i_("lv_fleet"), i_("lv_area"))
    # the plot TYPE is deliberately absent from both signatures - switching
    # heatmap/bubbles/bars, or histogram/density, is a different view of the
    # same configuration and should be immediate
    # NOT `reactive(flt_inputs(...))`. Wrapping it made a brand-new
    # reactive object on every evaluation of the signature, which was then
    # thrown away - so a change to rv$flt invalidated an object nothing
    # held any more, the signature stayed cached at its old value, the
    # panel never went stale and Apply had nothing to apply. The coverage
    # and discard-rate gates read flt_inputs() directly, which is exactly
    # why those two worked and all six biology panels did not.
    flt_sig <- function(view) flt_inputs(paste0(p, "_", view))
    cov_sig <- reactive(c(list(i_("cov_cc"), i_("thr_cov"),
                               i_("thr_meas"), i_("thr_trips"), i_("cov_r_y"),
                               i_("cov_r_fill"),
                               i_("cov_r_facet"), flt_sig("cov")), lv_sig()))
    cov_live <- reactive(i_("cov_type"))
    n_sig <- reactive(c(list(i_("n_cc"), i_("n_r_fill"),
                             i_("n_r_facet"), i_("n_r_shape"), i_("n_stack"),
                             i_("n_smooth"), i_("n_freex"), i_("n_freey"),
                             flt_sig("n")),
                        lv_sig()))
    n_live <- reactive(i_("n_type"))
    w_sig <- reactive(c(list(i_("w_cc"), i_("w_r_fill"), i_("w_r_facet"),
                             i_("w_r_shape"), i_("w_pt"), i_("w_points"),
                             i_("w_freex"), i_("w_freey"), i_("w_type"),
                             flt_sig("w")),
                        lv_sig()))
    dist_ref <- reactive(rv$dist)
    cov_gate <- gate_of(paste0(p, "_cov"), cov_sig, refresh = dist_ref,
                        live = cov_live)
    n_gate   <- gate_of(paste0(p, "_n"),   n_sig,   refresh = dist_ref,
                        live = n_live)
    w_gate   <- gate_of(paste0(p, "_w"),   w_sig,   refresh = dist_ref)

    # ---- shared status + summary --------------------------------
    output[[paste0(p, "_status")]] <- renderUI({
      if (is.null(dist_tbl()))
        return(div(class = "chain", style = "color:#7a1f1f",
                   sprintf("No distributions table loaded - %s work needs it.",
                           what_word)))
      bl <- tryCatch(blink_for("cov"), error = function(e) NULL)
      if (is.null(bl) || !nrow(bl))
        return(div(class = "note",
                   sprintf("No %s rows in the catch table.", cc_word("cov"))))
      lk <- tryCatch(linked_for("cov"), error = function(e) NULL)
      un <- distribution_units(dist_tbl(), type)
      sc <- tryCatch(length_scale_check(lk), error = function(e) NULL)
      tagList(
        div(class = "chain", style = "color:#14532d",
            sprintf("%s: %s of %s strata have %s data (%s%% of the %s weight). Shown in %s.",
                    tools::toTitleCase(cc_word("cov")),
                    sum(bl$sampled), nrow(bl), what_word,
                    round(sum(bl$weight_measured) / sum(bl$weight) * 100, 1),
                    cc_word("cov"),
                    if (identical(type, "Age")) "years and g" else "cm and g")),
        if (!is.null(un) && isTRUE(un$mixed))
          div(class = "chain", style = "color:#9a5000",
              sprintf("The file mixes units (%s); all rows were converted before plotting.",
                      paste(c(un$class, un$weight), collapse = ", "))) else NULL,
        if (sum(bl$n_dangling, na.rm = TRUE) > 0)
          div(class = "chain", style = "color:#9a5000",
              sprintf("%s catch row(s) carry a domainBiology that matches nothing in the distributions table.",
                      format(sum(bl$n_dangling), big.mark = ","))) else NULL,
        {
          orph <- tryCatch(dist_without_catch(
                             tryCatch(dat(), error = function(e) NULL),
                             dist_tbl(), cc_of("cov"), type),
                           error = function(e) NULL)
          if (!is.null(orph) && nrow(orph))
            div(class = "chain", style = "color:#7a1f1f",
                sprintf(paste("%s %s distribution domain(s) have no matching",
                              "catch row (no catch carries that domainBiology).",
                              "The catch and biology files disagree - check the",
                              "submitted data."),
                        nrow(orph), what_word)) else NULL
        },
        {
          m2 <- tryCatch(stratum_measurements(tryCatch(dat(), error = function(e) NULL),
                                              dist_tbl(), dims_of(), cc_of("cov"), type),
                         error = function(e) NULL)
          b2 <- tryCatch(add_sufficiency(bl, m2, thr_cov(), thr_meas()),
                         error = function(e) NULL)
          w <- if (!is.null(b2)) measurement_warning(b2, thr_meas()) else NULL
          if (!is.null(w)) div(class = "chain", style = "color:#9a5000", w) else NULL
        },
        if (!is.null(sc) && !isTRUE(sc$plausible))
          div(class = "chain", style = "color:#7a1f1f",
              sprintf(paste("Length scale looks wrong: fitting weight against",
                            "length gives an exponent of %.2f but a coefficient",
                            "of %.2g, about %.0fx off. The class column may be",
                            "on a finer scale than its unit says - check it",
                            "before using these numbers."),
                      sc$b, sc$a, sc$implied_factor)) else NULL) })

    output[[paste0(p, "_sum")]] <- renderDT({
      bl <- blink_for("cov")
      validate(need(!is.null(bl) && nrow(bl), "No catch rows."))
      datatable(length_link_summary(bl, linked_for("cov"), type), rownames = FALSE,
                colnames = c("", ""), options = list(dom = "t", scrollX = TRUE)) })

    # ---- view 1: sampling coverage ------------------------------
    cov_obj <- gated(cov_gate, {
      bl <- blink_for("cov")
      validate(need(!is.null(bl) && nrow(bl), "No catch rows."))
      # Re-shape the biology link into the same columns the discard
      # coverage plots expect, then run it through classify_strata so it
      # gets its Status / cat_key exactly like a discard stratum. Here
      # "covered" means a length distribution exists, not a discard one.
      # a stratum is only a usable donor if it clears BOTH thresholds, so
      # the measurement count is joined on and a stratum that is covered
      # but thinly measured is demoted rather than shown as fine
      m <- tryCatch(stratum_measurements(tryCatch(dat(), error = function(e) NULL),
                                         dist_tbl(), dims_of(), cc_of("cov"), type),
                    error = function(e) NULL)
      bl <- add_sufficiency(bl, m, thr_cov(), thr_meas())
      # keep the real coverage and measurement count; a thinly measured
      # stratum is flagged by classify_strata (via min_meas), not demoted
      # to "unsampled" as the old filter did
      sc <- bl %>%
        mutate(landings = weight,
               landings_with_discards = weight_measured,
               discards = 0, discard_rate = NA_real_)
      sc <- classify_strata(sc, thr_cov(), what_word, thr_meas(), thr_trips())
      y <- rc("cov", "y"); f <- rc("cov", "fill"); fc <- rc("cov", "facet")
      validate(need(!is.null(y), "Pick a Y axis."))
      wl <- cc_word("cov"); th <- thr_cov()
      plot_biology_coverage(sc, type = input[[paste0(p, "_cov_type")]] %||% "heatmap",
        y = y, fill = f, facet = fc, threshold = th, what = what_word,
        weight_label = wl, min_meas = thr_meas(), min_trips = thr_trips()) },
      live = cov_live)

    # how many rows the plot will actually draw, so the panel grows with them
    n_levels_of <- function(d, v)
      if (!is.null(d) && !is.null(v) && length(v) == 1 && !is.na(v) &&
          nzchar(v) && v %in% names(d))
        length(unique(as.character(d[[v]]))) else 10

    output[[paste0(p, "_cov_plot_ui")]] <- renderUI({
      cov_gate$now(); cov_live()
      bl <- tryCatch(isolate(blink_for("cov")), error = function(e) NULL)
      plotOutput(paste0(p, "_cov_plot"),
        height = paste0(length_plot_height(bl, rc("cov","facet"), NULL,
                                           n_y = n_levels_of(bl, rc("cov","y"))),
                        "px")) })
    output[[paste0(p, "_cov_plot")]] <- renderPlot({
      pl <- cov_obj(); validate(need(!is.null(pl), "Nothing to plot."))
      pl + theme(legend.position = "none") })
    output[[paste0(p, "_cov_legend")]] <- renderPlot({
      pl <- cov_obj(); validate(need(!is.null(pl), "")); draw_legend(pl) },
      bg = "transparent")

    # The biology objects are rebuilt in output.R rather than reloaded,
    # because the level of detail is a plotting choice here: the same saved
    # bio_link can be redrawn at metier4 or metier6 without re-running the
    # model stage. The dims are written out literally so the block stands
    # on its own.
    bio_obj <- function() if (identical(type, "Age")) "bio_age" else "bio_length"
    bio_prep <- function(view, out = "bio_sc") c(
      sprintf("dims_biology <- %s", fmt_vals(dims_of())),
      sprintf('bl <- biology_link(catch_for_biology, distributions, dims_biology, %s, "%s")',
              taf_q(cc_of(view)), type),
      sprintf('ms <- stratum_measurements(catch_for_biology, distributions, dims_biology, %s, "%s")',
              taf_q(cc_of(view)), type),
      sprintf("bl <- add_sufficiency(bl, ms, %s, %s)", thr_cov(), thr_meas()),
      "## reshaped into the columns classify_strata() expects, so a biology",
      "## stratum is classified by exactly the same code as a discard one",
      sprintf("%s <- bl %%>%% mutate(landings = weight,", out),
      "        landings_with_discards = weight_measured,",
      "        discards = 0, discard_rate = NA_real_)",
      sprintf('%s <- classify_strata(%s, %s, "%s", %s, %s)',
              out, out, thr_cov(), what_word, thr_meas(), thr_trips()))

    keep_plot(paste0(p, "_cov_plot"), cov_obj, gate = cov_gate,
      section = "model_explore_length_age",
      title = function() sprintf("%s coverage - %s, %s",
        tools::toTitleCase(what_word),
        input[[paste0(p, "_cov_type")]] %||% "heatmap", cc_word("cov")),
      note = function() sprintf(
        "%s at %s coverage / %s trips / %s measurements; fleet = %s, area = %s",
        tools::toTitleCase(cc_word("cov")), paste0(thr_cov(), "%"),
        thr_trips(), thr_meas(), paste(len_dims(p), collapse = ", "), ""),
      prep = function() bio_prep("cov"),
      call = function() taf_call("plot_biology_coverage", list(
        "bio_sc",
        type = taf_q(input[[paste0(p, "_cov_type")]] %||% "heatmap"),
        y = taf_q(rc("cov", "y")), fill = taf_q(rc("cov", "fill")),
        facet = taf_q(rc("cov", "facet")), threshold = thr_cov(),
        what = taf_q(what_word), weight_label = taf_q(cc_word("cov")),
        min_meas = thr_meas(), min_trips = thr_trips())))

    # ---- view 2: numbers at length ------------------------------
    num_data <- reactive({
      lk <- linked_for("n")
      validate(need(!is.null(lk) && nrow(lk), "No length distributions."))
      gv <- unique(na.omit(c(rc("n","fill"), rc("n","facet"), rc("n","shape"))))
      length_numbers(lk, gv) })
    num_obj <- gated(n_gate, {
      d <- num_data()
      validate(need(!is.null(d) && nrow(d), "No numbers-at-length rows."))
      plot_biology_distribution(d, quantity = "numbers",
        type = if (identical(input[[paste0(p, "_n_type")]], "ridge"))
                 "density" else "histogram",
        fill = rc("n","fill"), facet1 = rc("n","facet"), facet2 = rc("n","shape"),
        stacked = isTRUE(input[[paste0(p, "_n_stack")]]),
        smooth = input[[paste0(p, "_n_smooth")]] %||% 1,
        scales = facet_scales(isTRUE(input[[paste0(p, "_n_freex")]]),
                              isTRUE(input[[paste0(p, "_n_freey")]])),
        class_label = class_axis_label(type)) },
      live = n_live)
    output[[paste0(p, "_n_plot_ui")]] <- renderUI({
      n_gate$now(); n_live()
      d <- tryCatch(isolate(num_data()), error = function(e) NULL)
      plotOutput(paste0(p, "_n_plot"),
        height = paste0(length_plot_height(d, rc("n","facet"), rc("n","shape"),
                          n_y = if (identical(input[[paste0(p, "_n_type")]], "ridge"))
                                  n_levels_of(d, rc("n","fill")) else 10),
                        "px")) })
    output[[paste0(p, "_n_plot")]] <- renderPlot({
      pl <- num_obj(); validate(need(!is.null(pl), "Nothing to plot."))
      pl + theme(legend.position = "none") })
    output[[paste0(p, "_n_legend")]] <- renderPlot({
      pl <- num_obj(); validate(need(!is.null(pl), "")); draw_legend(pl) },
      bg = "transparent")

    keep_plot(paste0(p, "_n_plot"), num_obj, gate = n_gate,
      section = "model_explore_length_age",
      title = function() sprintf("Numbers at %s - %s", what_word,
        if (identical(input[[paste0(p, "_n_type")]], "ridge")) "density"
        else "histogram"),
      note = function() sprintf("%s, grouped by %s", cc_word("n"),
        paste(na.omit(c(rc("n","fill"), rc("n","facet"), rc("n","shape"))),
              collapse = ", ")),
      prep = function() c(
        sprintf("dims_biology <- %s", fmt_vals(dims_of())),
        sprintf('linked <- link_dist_to_strata(catch_for_biology, distributions, dims_biology, %s, "%s")',
                taf_q(cc_of("n")), type),
        sprintf("d_num <- length_numbers(linked, %s)",
                fmt_vals(unique(na.omit(c(rc("n","fill"), rc("n","facet"),
                                          rc("n","shape"))))))),
      call = function() taf_call("plot_biology_distribution", list(
        "d_num", quantity = '"numbers"',
        type = if (identical(input[[paste0(p, "_n_type")]], "ridge"))
                 '"density"' else '"histogram"',
        fill = taf_q(rc("n","fill")), facet1 = taf_q(rc("n","facet")),
        facet2 = taf_q(rc("n","shape")),
        stacked = if (isTRUE(input[[paste0(p, "_n_stack")]])) "TRUE" else "FALSE",
        smooth = input[[paste0(p, "_n_smooth")]] %||% 1,
        class_label = taf_q(class_axis_label(type)))))

    # ---- view 3: mean weight at length --------------------------
    mw_data <- reactive({
      lk <- linked_for("w")
      validate(need(!is.null(lk) && nrow(lk), "No length distributions."))
      pt <- len_dim_col(p, input[[paste0(p, "_w_pt")]])
      gv <- unique(na.omit(c(rc("w","fill"), rc("w","facet"), rc("w","shape"), pt)))
      length_meanweight(lk, gv) })
    mw_type <- reactive(input[[paste0(p, "_w_type")]] %||% "linedot")

    # The boxes need the values BEFORE the collapse, so this is a
    # different table, not a different geom on the same one.
    mw_raw <- reactive({
      lk <- linked_for("w")
      validate(need(!is.null(lk) && nrow(lk), "No length distributions."))
      pt <- len_dim_col(p, input[[paste0(p, "_w_pt")]])
      gv <- unique(na.omit(c(rc("w","fill"), rc("w","facet"), rc("w","shape"), pt)))
      length_meanweight_raw(lk, gv) })

    mw_obj <- gated(w_gate, {
      if (identical(mw_type(), "box")) {
        d <- mw_raw()
        validate(need(!is.null(d) && nrow(d), "No mean-weight rows."))
        plot_length_meanweight_box(d,
          colour_var = rc("w","fill"), facet1 = rc("w","facet"),
          facet2 = rc("w","shape"),
          shape_var = len_dim_col(p, input[[paste0(p, "_w_pt")]]),
          show_points = isTRUE(input[[paste0(p, "_w_points")]]),
          scales = facet_scales(isTRUE(input[[paste0(p, "_w_freex")]]),
                                isTRUE(input[[paste0(p, "_w_freey")]])),
          class_label = class_axis_label(type))
      } else {
        d <- mw_data()
        validate(need(!is.null(d) && nrow(d), "No mean-weight rows."))
        plot_biology_distribution(d, quantity = "meanweight", type = "line",
          colour = rc("w","fill"), facet1 = rc("w","facet"),
          facet2 = rc("w","shape"),
          shape = len_dim_col(p, input[[paste0(p, "_w_pt")]]),
          show_points = isTRUE(input[[paste0(p, "_w_points")]]),
          scales = facet_scales(isTRUE(input[[paste0(p, "_w_freex")]]),
                                isTRUE(input[[paste0(p, "_w_freey")]])),
          class_label = class_axis_label(type))
      } })
    output[[paste0(p, "_w_plot_ui")]] <- renderUI({
      w_gate$now()
      d <- tryCatch(isolate(mw_data()), error = function(e) NULL)
      plotOutput(paste0(p, "_w_plot"),
        height = paste0(length_plot_height(d, rc("w","facet"), rc("w","shape")), "px")) })
    output[[paste0(p, "_w_plot")]] <- renderPlot({
      pl <- mw_obj(); validate(need(!is.null(pl), "Nothing to plot."))
      pl + theme(legend.position = "none") })
    output[[paste0(p, "_w_legend")]] <- renderPlot({
      pl <- mw_obj(); validate(need(!is.null(pl), "")); draw_legend(pl) },
      bg = "transparent")

    keep_plot(paste0(p, "_w_plot"), mw_obj, gate = w_gate,
      section = "model_explore_length_age",
      title = function() sprintf("Mean weight at %s", what_word),
      note = function() sprintf(
        "%s; whatever is not given a role is averaged over, weighted by the numbers measured",
        cc_word("w")),
      prep = function() c(
        sprintf("dims_biology <- %s", fmt_vals(dims_of())),
        sprintf('linked <- link_dist_to_strata(catch_for_biology, distributions, dims_biology, %s, "%s")',
                taf_q(cc_of("w")), type),
        sprintf("d_mw <- length_meanweight(linked, %s)",
                fmt_vals(unique(na.omit(c(rc("w","fill"), rc("w","facet"),
                          rc("w","shape"),
                          len_dim_col(p, input[[paste0(p, "_w_pt")]]))))))),
      call = function() taf_call("plot_biology_distribution", list(
        "d_mw", quantity = '"meanweight"', type = '"line"',
        colour = taf_q(rc("w","fill")), facet1 = taf_q(rc("w","facet")),
        facet2 = taf_q(rc("w","shape")),
        shape = taf_q(len_dim_col(p, input[[paste0(p, "_w_pt")]])),
        show_points = if (isTRUE(input[[paste0(p, "_w_points")]])) "TRUE" else "FALSE",
        class_label = taf_q(class_axis_label(type)))))
  }

  length_server("len", "LengthTotal")
  length_server("age", "Age")

  # ---- allocation groups for length -----------------------------
  length_alloc_server <- function(p, type = "LengthTotal") {
    ns <- function(x) paste0(p, "_ga_", x)
    # each section keeps its own clause and groups
    R <- function(slot) paste0(p, "_", slot)
    getrv <- function(slot) rv[[R(slot)]]
    setrv <- function(slot, v) rv[[R(slot)]] <- v
    keep_cc <- reactive(isTRUE(input[[ns("keepcc")]]))

    # The allocation works on the whole catch table, both categories, so
    # it links without a category filter.
    #
    # Its strata follow ITS OWN keys, not the figure pickers on the
    # exploration tabs - len_dims() reads those, and a builder whose
    # strata changed because someone had faceted a plot by mesh would be
    # a nasty surprise. Same shape as the discard cascade: the stratum as
    # uploaded, plus every column a key names.
    # ONE strata definition for this module, shared by both the
    # hierarchical cascade and the manual builder. They are not
    # independent the way the discard side's ar/mg are: hier and manual
    # here feed the SAME downstream "adopted allocation" for this p
    # (len/age), and dims_of() already had 17 call sites spread across
    # both before this box existed. Splitting it into two independent
    # definitions would mean auditing every one of those to classify it
    # hier-only or manual-only correctly - real risk of misclassifying
    # one and quietly breaking it, for a benefit (independent strata per
    # sub-mode within the SAME p) neither method actually asked for.
    # The editable box lives on the hierarchical panel; the manual panel
    # shows a read-only mirror of the same resolved columns.
    strata_dims <- own_key_strata_server(ns("hsd"))
    dims_of <- function()
      avail(unique(c("catchCategory", strata_dims(), alloc_cols())))

    dist_tbl <- reactive({
      if (!is.null(rv$dist)) rv$dist
      else if (!is.null(rv$ic$distributions)) as.data.frame(rv$ic$distributions)
      else NULL })
    blink_for <- function(...) {
      d <- tryCatch(dat(), error = function(e) NULL)
      req(!is.null(d))
      biology_link(d, dist_tbl(), dims_of(), NULL, type) }

    # the key labels resolved to actual columns
    alloc_cols <- reactive(
      vapply(alloc_keys(), function(k) len_dim_col(p, k), character(1)))

    # Key 1 is the stratum's own data and Key 2 is catchCategory, both
    # fixed, so the five user slots below are Keys 3 to 7.
    N_ALLOC_KEYS <- 5
    alloc_keys <- reactive({
      k <- vapply(seq_len(N_ALLOC_KEYS),
                  function(i) input[[ns(paste0("k", i))]] %||% "",
                  character(1))
      k[nzchar(k)] })

    # Landings and discards get their own thresholds, per rung. Discards
    # are sampled less nearly everywhere, so one shared bar either lets
    # landings resolve on too little data or pushes every discard stratum
    # to the bottom of the ladder. Both pairs apply whether or not the
    # category is droppable: the bar is a property of the stratum being
    # allocated, not of the pool it draws on.
    num_in <- function(id, default) {
      x <- input[[ns(id)]]
      if (is.null(x) || !length(x) || all(is.na(x))) default
      else as.numeric(x)[1] }

    # t1 is the OWN-STRATUM bar; t2..t5 are the four key boxes (Fleet,
    # Country, Season, Area), already fine-to-coarse and already aligned
    # to alloc_keys(). Returning them per key, plus own separately, means
    # alloc_steps() never has to reconstruct which box drove which key -
    # the misalignment that put the Area box on the catchCategory rung.
    nk <- reactive(length(alloc_keys()))
    alloc_thr <- reactive(list(
      own = c(Lan = num_in("t1_lan", 50), Dis = num_in("t1_dis", 30)),
      Lan = vapply(seq_len(nk()), function(i) num_in(paste0("t", i+1, "_lan"), 50), numeric(1)),
      Dis = vapply(seq_len(nk()), function(i) num_in(paste0("t", i+1, "_dis"), 30), numeric(1))))

    alloc_meas <- reactive(list(
      own = c(Lan = num_in("m1_lan", 0), Dis = num_in("m1_dis", 0)),
      Lan = vapply(seq_len(nk()), function(i) num_in(paste0("m", i+1, "_lan"), 0), numeric(1)),
      Dis = vapply(seq_len(nk()), function(i) num_in(paste0("m", i+1, "_dis"), 0), numeric(1))))

    # One narrow key column, then a Landings pane and a Discards pane.
    # The coverage control is a number box rather than the old range
    # slider: only the lower handle was ever read, so the upper one was a
    # control that did nothing, and two of them per row would not fit.
    cc_pane <- function(rung, side, cov_default, meas_default) {
      lab <- if (side == "lan") "Landings" else "Discards"
      # coverage as a min-only slider (blue up to 100, the handle is the
      # threshold), to read the same way as the discard tab. Measurements
      # stay a number box: there is no 0-100 ceiling to pin a slider to.
      div(class = paste0("ccpane ccpane-", side),
        div(class = "ccpane-title",
            style = if (side == "lan") "color:#3d6b3d" else "color:#9a5000", lab),
        sliderInput(ns(sprintf("t%d_%s", rung, side)), "% coverage",
                    0, 100, c(cov_default, 100), 5, width = "100%"),
        numericInput(ns(sprintf("m%d_%s", rung, side)), "min meas.",
                     meas_default, 0, 100000, 10, width = "100%"))
    }
    key_row <- function(rung, keybox)
      fluidRow(class = "hier-key-row",
        column(4, keybox),
        column(4, cc_pane(rung, "lan", 50, 0)),
        column(4, cc_pane(rung, "dis", 30, 0)))

    # Set-all controls: type a coverage and a measurement count once, then
    # push them to every key box - to both brackets, or to just one. Five
    # keys times two brackets times two metrics is twenty boxes, and a
    # sweep like "everything to 0/0 to force the last resort" was twenty
    # edits.
    set_all <- function(side) {
      cv <- num_in("setall_cov", NA); mm <- num_in("setall_meas", NA)
      for (i in 1:6) {
        if (!is.na(cv))
          updateSliderInput(session, ns(sprintf("t%d_%s", i, side)),
                            value = c(cv, 100))
        if (!is.na(mm))
          updateNumericInput(session, ns(sprintf("m%d_%s", i, side)), value = mm)
      }
    }
    observeEvent(input[[ns("setall_both")]], { set_all("lan"); set_all("dis") })
    observeEvent(input[[ns("setall_lan")]],  set_all("lan"))
    observeEvent(input[[ns("setall_dis")]],  set_all("dis"))

    output[[ns("keys")]] <- renderUI({
      setall <- div(class = "setall-box",
        div(class = "note", tags$b("Set all thresholds to")),
        sliderInput(ns("setall_cov"), "% coverage", 0, 100, c(0, 100), 5,
                    width = "100%"),
        numericInput(ns("setall_meas"), "min meas.", NA, 0, 100000, 10,
                     width = "100%"),
        div(style="display:flex;gap:4px",
          actionButton(ns("setall_both"), "Apply to all", class="btn-xs btn-primary"),
          actionButton(ns("setall_lan"),  "To landings",  class="btn-xs"),
          actionButton(ns("setall_dis"),  "To discards",  class="btn-xs")))
      # Key 1 carries its OWN strata-definition choice now, the same
      # compact "Use as provided / Customize" widget the discard cascade
      # uses - what "own stratum" means IS a Key 1 decision, not a
      # preamble to it. Wider than the other rows' keybox (8 columns
      # instead of 4) and the threshold panes narrower to match, since
      # four family slots need more room than a fixed label did.
      k1ns <- function(x) paste0(ns("hsd"), "_", x)
      kv <- key_vocab_grouped(tryCatch(dat(), error = function(e) NULL))
      slot <- function(fam, id) column(3,
        selectInput(k1ns(id), fam, c(NONE, kv[[fam]] %||% character(0)),
                   input[[k1ns(id)]] %||% "", width = "100%"))
      k1 <- tagList(
        fluidRow(class = "hier-key-row",
          column(8,
            tags$b("Key 1"), tags$span(class = "note", " - Own stratum"),
            radioButtons(k1ns("mode"), NULL,
                         c("Use as provided" = "asis",
                           "Customize" = "custom"), "asis", inline = TRUE)),
          column(2, cc_pane(1, "lan", 50, 0)),
          column(2, cc_pane(1, "dis", 30, 0))),
        conditionalPanel(sprintf("input.%s == 'custom'", k1ns("mode")),
          fluidRow(class = "hier-key-row",
            slot("Fleet", "fleet"), slot("Country", "country"),
            slot("Area", "area"), slot("Season", "season"))),
        uiOutput(k1ns("summary")))
      cc <- fluidRow(class = "hier-key-row",
        column(4, div(class = "hier-key-fixed",
                      style = if (keep_cc()) "border-left:5px solid #59A14F" else
                              "border-left:5px solid #BFBFBF",
                      tags$b("Key 2"), tags$br(), "catchCategory",
                      tags$span(class = "note",
                                if (keep_cc()) "kept to the end" else "droppable"))),
        column(8, tags$span(class = "note",
                            "no threshold - this is the ceiling of the cascade")))
      # Keys 3-7: the cascade-key vocabulary, grouped into Fleet /
      # Country / Area / Season optgroups, same as the discard cascade -
      # a bold subtitle per family instead of ten items in one flat
      # list. Rows past the last filled one are not drawn, so a
      # three-key cascade is three rows rather than five empty slots.
      ch <- c(NONE, kv)
      picked <- vapply(seq_len(N_ALLOC_KEYS),
                       function(i) input[[ns(paste0("k", i))]] %||% "",
                       character(1))
      fl <- which(nzchar(picked))
      last <- if (length(fl)) max(fl) else 4
      dflt <- c("Season", "areaValue", "Country", "fleetValue", "")
      rows <- lapply(seq_len(min(N_ALLOC_KEYS, last + 1)), function(i)
        key_row(i + 1,
                selectInput(ns(paste0("k", i)), paste("Key", i + 2), ch,
                            input[[ns(paste0("k", i))]] %||% dflt[i],
                            width = "100%")))

      # The two live checks, same as the discard cascade: a key finer
      # than the strata definition can never split anything, and within
      # the key sequence itself a coarser-then-finer pair in the same
      # family means the later key has nothing left to distinguish.
      key_picks <- picked[nzchar(picked)]
      w1 <- key_vs_strata_warnings(key_picks, strata_dims())
      w2 <- key_order_warnings(key_picks)
      warn <- if (length(c(w1, w2)))
        div(class = "note", style = "color:#7a1f1f;margin-top:4px",
            lapply(c(w1, w2), function(m) tagList(HTML("&#9888; "), m, tags$br())))

      tagList(
        div(class = "note",
            "Landings and discards are held to their own bars. Discards are ",
            "sampled less, so a lower coverage and a smaller measurement ",
            "count are usually the right settings on the right. Both apply ",
            "whether or not catchCategory is droppable."),
        setall, tags$hr(style = "margin:6px 0"),
        # A banner making the reading direction explicit. The boxes run
        # own -> catchCategory -> Fleet -> ... -> Area, top to bottom, and
        # that IS the order the cascade tries: the stratum's own data
        # first, then progressively coarser pools. Each threshold is the
        # bar the pool at that level must clear.
        div(class = "cascade-dir",
            HTML("&darr;"), tags$b(" tries own data first, widens downward"),
            tags$br(),
            tags$span(class = "note",
                      "each row's bar is the coverage the pool at that ",
                      "level must reach before it is used")),
        k1, cc, tags$hr(style = "margin:6px 0"), rows, warn) })

    # The donor measurements bar, scaled to the data. Same reasoning as
    # the exclusion boxes: a fixed 0-100000 numeric put every real value
    # in the first pixel of the range, so the ceiling is the largest
    # count actually present. Min-only, so the shaded side is the side
    # that qualifies as a donor.
    output[[ns("dmeas_ui")]] <- renderUI({
      bl <- tryCatch(bl_metrics(), error = function(e) NULL)
      top <- if (is.null(bl) || !"n_measurements" %in% names(bl)) 1000
             else max(c(1, suppressWarnings(
               as.numeric(bl$n_measurements))), na.rm = TRUE)
      top <- max(200, ceiling(top / 50) * 50)
      cur <- input[[ns("dmeas")]]
      val <- if (is.null(cur) || all(is.na(cur))) min(100, top)
             else min(as.numeric(cur)[1], top)
      sliderInput(ns("dmeas"), "donor measurements above", 0, top,
                  c(val, top), step = max(1, round(top / 200)),
                  width = "100%")
    })

    # The live settings, and the ones the panel is currently drawn from.
    donor_bars <- reactive({
      cv <- num_in("dcov", NA)        # slider floor 0 means "no bar"
      list(cov = if (isTRUE(cv <= 0)) NA_real_ else cv,
           meas = num_in("dmeas", NA))
    })

    # dims_of() in the signature so a level-of-detail change marks the
    # panel stale: alloc_cols() resolves the same key labels to different
    # columns at a new level, and the cascade must be re-run to match.
    hier_sig <- reactive(list(alloc_keys(), alloc_thr(), keep_cc(),
                              alloc_meas(), donor_bars(), dims_of()))
    hier_applied <- reactiveVal(NULL)

    # first pass: draw something rather than an empty panel
    observe({
      if (is.null(hier_applied())) {
        sg <- tryCatch(hier_sig(), error = function(e) NULL)
        if (!is.null(sg)) hier_applied(sg)
      }
    })
    observeEvent(input[[ns("apply")]], hier_applied(hier_sig()))

    output[[ns("stale")]] <- renderUI({
      a <- hier_applied()
      if (is.null(a) || identical(a, hier_sig())) return(NULL)
      tags$span(class = "note", style = "color:#9a5000;margin-left:8px",
                "settings changed - press Apply") })

    steps <- reactive({
      a <- hier_applied()
      req(!is.null(a))
      # a[[2]] and a[[4]] each carry $own plus the per-key vectors, so the
      # own-stratum bar reaches its own rung and the key bars stay aligned
      alloc_steps(a[[1]], a[[2]], a[[3]], min_meas = a[[4]],
                  donor_cov_min  = a[[5]]$cov,
                  donor_meas_min = a[[5]]$meas)
    })

    output[[ns("chain")]] <- renderUI({
      st <- steps()
      lab <- vapply(st, `[[`, character(1), "label")
      joined <- vapply(st, function(x) isTRUE(x$drop_cc), logical(1))
      # mark where the ladder restarts with the categories joined
      first_join <- which(joined)[1]
      if (length(first_join) && !is.na(first_join))
        lab[first_join] <- paste0("<b>&#8635; back to full precision:</b> ",
                                  lab[first_join])
      div(class = "chain", tags$b("Order actually tested:"), tags$br(),
          HTML(paste(lab, collapse = " &rarr; "))) })

    # what each rung ACTUALLY resolved, not just what it would test
    output[[ns("steps")]] <- renderDT({
      a <- tryCatch(cascade_assigned(), error = function(e) NULL)
      st <- steps()
      d <- if (is.null(a)) data.frame(
        step = seq_along(st),
        scope = vapply(st, alloc_step_scope, character(1)),
        pool = vapply(st, `[[`, character(1), "label"),
        # keys = the cascade's own widening order; dims = the fuller
        # strata definition (dims_of()), which is usually broader - the
        # uploaded stratum carries columns nobody chose as a cascade
        # key, and those need to show as "pooled across" too.
        grouping = vapply(st, alloc_step_grouping, character(1),
                          keys = alloc_cols(), dims = dims_of()),
        threshold = vapply(st, function(x) unname(x$threshold["Lan"]), numeric(1)),
        stringsAsFactors = FALSE)
      else alloc_cascade_table(a, st, keys = alloc_cols(), dims = dims_of())
      datatable(d, rownames = FALSE, options = list(dom = "t", scrollX = TRUE)) })

    output[[ns("status")]] <- renderUI({
      st <- steps()
      n_join <- sum(vapply(st, function(x) isTRUE(x$drop_cc), logical(1)))
      div(class = "chain",
        if (keep_cc())
          "catchCategory is kept throughout: landings are only ever allocated from landings, and the widest pool is all other data in the same category."
        else
          sprintf(paste("Once the within-category ladder is exhausted the",
                        "categories are joined and the ladder RESTARTS at full",
                        "precision, widening again over %s further step(s).",
                        "Pooling everything is the last of them, not the first",
                        "thing tried."), n_join)) })

    cascade_assigned <- reactive({
      bl <- blink_for()
      validate(need(!is.null(bl) && nrow(bl), "No catch rows."))
      assign_alloc_cascade(bl, alloc_cols(), steps(), bms_on()) })

    # the strata the cascade never resolved, so a too-strict threshold or
    # a category that must be released is visible rather than inferred
    output[[ns("cmissing")]] <- renderDT({
      a <- cascade_assigned()
      # zero-weight strata never enter the ladder and need no resolution,
      # so listing them here would send a user chasing thresholds for
      # strata that have nothing to allocate
      un <- a[is.na(a$alloc_step) & !isTRUE(a$alloc_empty %in% TRUE), ,
              drop = FALSE]
      if ("alloc_empty" %in% names(a))
        un <- a[is.na(a$alloc_step) & !a$alloc_empty, , drop = FALSE]
      validate(need(nrow(un), "Every stratum with catch was resolved."))
      keep <- intersect(c(dims_of(), "weight", "coverage", "n_measurements"),
                        names(un))
      d <- un[order(-un$weight), keep, drop = FALSE]
      num <- vapply(d, is.numeric, logical(1))
      d[num] <- lapply(d[num], function(x) round(x, 3))
      datatable(d, rownames = FALSE,
                options = list(scrollX = TRUE, pageLength = 10)) })

    # one river per rung, matching the manual per-rule rivers
    output[[ns("step_rivers")]] <- renderUI({
      a <- tryCatch(cascade_assigned(), error = function(e) NULL)
      st <- steps()
      if (is.null(a)) return(div(class = "note", "No strata yet."))
      tagList(lapply(seq_along(st), function(i) {
        n <- sum(a$alloc_step == i, na.rm = TRUE)
        div(style = "margin-bottom:8px",
            tags$b(sprintf("Step %s - %s - %s stratum/strata", i,
                           st[[i]]$label, n)),
            if (n > 0) plotOutput(ns(paste0("sriver_", i)),
                                  height = paste0(max(200, 60 + n * 24), "px"))
            else div(class = "note", "resolves nothing"))
      })) })

    observe({
      a <- tryCatch(cascade_assigned(), error = function(e) NULL)
      st <- steps()
      if (is.null(a)) return()
      for (i in seq_along(st)) local({
        j <- i
        output[[ns(paste0("sriver_", j))]] <- renderPlot({
          sub <- a[!is.na(a$alloc_step) & a$alloc_step == j, , drop = FALSE]
          validate(need(nrow(sub), "resolves nothing"))
          g <- cascade_to_groups(sub, alloc_cols())
          validate(need(length(g), "nothing to plot"))
          pl <- plot_alloc_river(sub, dims_of(), g, group_palette(names(g)))
          validate(need(!is.null(pl), "Nothing to plot.")); pl })
      }) })

    output[[ns("river_ui")]] <- renderUI(
      plotOutput(ns("river"), height = "620px"))

    # the data BEING allocated a distribution
    output[[ns("alloc_raise_tbl")]] <- renderDT({
      a <- cascade_assigned()
      validate(need(!is.null(a) && nrow(a), "Apply settings first."))
      keep <- intersect(c("catchCategory", dims_of()), names(a))
      num <- function(v) if (v %in% names(a)) round(a[[v]], 3) else NA
      lvl <- if ("alloc_level" %in% names(a)) a$alloc_level else NA
      d <- data.frame(a[, keep, drop = FALSE],
        weight       = num("weight"),
        coverage     = if ("coverage" %in% names(a)) round(a$coverage, 1) else NA,
        allocated_by = lvl,
        check.names = FALSE, stringsAsFactors = FALSE)
      d <- d[order(-(d$weight %||% rep(0, nrow(d)))), , drop = FALSE]
      datatable(d, options = list(scrollX = TRUE, pageLength = 15),
                rownames = FALSE) })

    # the data USED to allocate (bioMatchedCond donor pools)
    output[[ns("alloc_donor_tbl")]] <- renderDT({
      a <- cascade_assigned()
      validate(need(!is.null(a) && nrow(a), "Apply settings first."))
      keep <- intersect(c("catchCategory", dims_of()), names(a))
      dok <- if ("donor_ok" %in% names(a)) a$donor_ok else rep(TRUE, nrow(a))
      d <- data.frame(a[, keep, drop = FALSE],
        weight         = if ("weight" %in% names(a)) round(a$weight, 3) else NA,
        coverage       = if ("coverage" %in% names(a)) round(a$coverage, 1) else NA,
        n_measurements = if ("n_measurements" %in% names(a)) a$n_measurements else NA,
        usable_donor   = dok,
        donor_group    = if ("alloc_level" %in% names(a)) a$alloc_level else NA,
        check.names = FALSE, stringsAsFactors = FALSE)
      d <- d[order(-(d$weight %||% rep(0, nrow(d)))), , drop = FALSE]
      datatable(d, options = list(scrollX = TRUE, pageLength = 15),
                rownames = FALSE) })

    # saving an allocation: both methods land in the same store, so the
    # raising tab can pick either
    save_alloc <- function(nm, groups, method) {
      req(nzchar(nm), length(groups))
      # getrv/setrv resolve to len_allocs or age_allocs from the module
      # prefix. Writing rv$len_allocs directly would file every Age
      # allocation under Length, leaving age_allocs empty and the Age
      # grouping out of the emitted script.
      a <- getrv("allocs") %||% list()
      a[[nm]] <- list(name = nm, method = method, groups = groups,
                      saved = format(Sys.time(), "%H:%M:%S"),
                      # how the grouping was reached, so the script can
                      # reproduce it rather than only its resolved conditions
                      spec = list(keys = alloc_keys(), thresholds = alloc_thr(),
                                  min_meas = alloc_meas(), keep_cc = keep_cc(),
                                  donor_cov_min = donor_bars()$cov,
                                  donor_meas_min = donor_bars()$meas,
                                  cols = alloc_cols(), bms = bms_on(),
                                  dims = dims_of()))
      setrv("allocs", a)
      showNotification(sprintf("Allocation '%s' saved (%s groups)", nm,
                               length(groups)), duration = 3) }

    observeEvent(input[[ns("save")]], {
      g <- cascade_to_groups(cascade_assigned(), alloc_cols())
      save_alloc(input[[ns("name")]] %||% "alloc_1", g, "hierarchical") })

    observeEvent(input[[ns("savemanual")]], {
      save_alloc(input[[ns("mname")]] %||% "manual_1",
                 rv$len_groups %||% list(), "manual") })

    # ---- manual groups ------------------------------------------
    bms_on <- reactive(isTRUE(input[[ns("bms")]]))

    output[[ns("bmsnote")]] <- renderUI(
      if (bms_on())
        div(class = "chain", style = "color:#14532d",
            "BMS is being treated as a discard: a clause naming Dis also matches BMS.")
      else
        div(class = "chain", style = "color:#9a5000",
            "BMS is being kept separate from Dis."))

    output[[ns("vals")]] <- renderUI({
      d <- tryCatch(dat(), error = function(e) NULL)
      col <- len_dim_col(p, input[[ns("var")]])
      validate(need(!is.null(d) && !is.null(col), "Load data first."))
      selectizeInput(ns("v"), NULL, sort(unique(as.character(d[[col]]))),
                     multiple = TRUE) })

    output[[ns("clause")]] <- renderUI({
      cl <- getrv("clause") %||% list()
      div(class = "clausebox",
          if (!length(cl)) "(empty clause)" else alloc_cond_string(cl)) })

    observeEvent(input[[ns("add")]], {
      col <- len_dim_col(p, input[[ns("var")]])
      req(!is.null(col), length(input[[ns("v")]]))
      vals <- input[[ns("v")]]
      if (identical(col, "catchCategory")) vals <- expand_cc(vals, bms_on())
      cl <- getrv("clause") %||% list()
      cl[[col]] <- if (isTRUE(input[[ns("neg")]]))
        structure(vals, negate = TRUE) else vals
      setrv("clause", cl) })

    observeEvent(input[[ns("clear")]], setrv("clause", list()))

    observeEvent(input[[ns("to_alloc")]], {
      req(length(getrv("clause")))
      setrv("alloc_pend", getrv("clause")); setrv("clause", list()) })
    observeEvent(input[[ns("to_matched")]], {
      req(length(getrv("clause")))
      setrv("match_pend", getrv("clause")); setrv("clause", list()) })
    observeEvent(input[[ns("mirror")]], setrv("match_pend", getrv("alloc_pend")))
    observeEvent(input[[ns("mirror_back")]], setrv("alloc_pend", getrv("match_pend")))

    # Same chip picker as the discard clause builder: the main stratum
    # variables up front, everything else behind "more variables".
    output[[ns("var_chips")]] <- renderUI({
      # Real columns, plus catchCategory - which IS meaningful here, in a
      # way it is not as a raising stratum key: an allocation group very
      # often IS "the discards", and allocStrataCond is not what
      # build_strata() splits on.
      vars <- unname(gvars())
      sel <- input[[ns("var")]] %||%
        if ("catchCategory" %in% c("catchCategory", vars)) "catchCategory"
        else vars[1]
      var_chips(ns("var"), c("catchCategory", vars), character(0), sel) })

    output[[ns("alloc_txt")]] <- renderUI(div(class = "clausebox",
      if (is.null(getrv("alloc_pend"))) "(not set - matches every stratum)"
      else alloc_cond_string(getrv("alloc_pend"))))
    output[[ns("matched_txt")]] <- renderUI(div(class = "clausebox",
      if (is.null(getrv("match_pend")))
        "(mirrors allocStrataCond)"
      else alloc_cond_string(getrv("match_pend"))))

    # The link the exclusion lists are read off. biology_link() gives
    # coverage but not the measurement counts, so those are joined on
    # here - otherwise the "measurements below" box would be a control
    # with nothing behind it.
    bl_metrics <- reactive({
      bl <- tryCatch(blink_for("cov"), error = function(e) NULL)
      if (is.null(bl) || !nrow(bl)) return(NULL)
      ms <- tryCatch(stratum_measurements(tryCatch(dat(), error = function(e) NULL),
                                          dist_tbl(), dims_of(), NULL, type),
                     error = function(e) NULL)
      tryCatch(add_sufficiency(bl, ms, 0, 0), error = function(e) bl) })

    alloc_dims <- function() alloc_cols_manual()
    alloc_cols_manual <- function()
      intersect(c("catchCategory", len_dims(p)),
                names(bl_metrics() %||% data.frame()))

    x_keys_srv <- exclusion_server(ns("x"),
      strata_of = function() bl_metrics(),
      dims_of   = alloc_dims,
      metrics   = c("coverage", "trips", "measurements"))

    ca_keys_srv <- exclusion_server(ns("ca"),
      strata_of = function() bl_metrics(),
      dims_of   = alloc_dims,
      metrics   = c("coverage", "trips", "measurements"))

    x_keys  <- x_keys_srv$keys
    ca_keys <- ca_keys_srv$keys

    save_group <- function(gid, label, a, m, excl, bars = NULL) {
      g <- getrv("groups") %||% list()
      mc <- alloc_matched_cond(m, excl, alloc_dims(),
                               cov_min  = bars$cov_min %||% NA,
                               meas_min = bars$meas_min %||% NA)
      g[[gid]] <- list(
        label   = label,
        alloc   = alloc_cond_string(a),
        matched = mc$text,
        excluded = excl,
        alloc_expr   = alloc_quo(a),
        matched_expr = mc$quo)
      setrv("groups", g)
      g
    }

    observeEvent(input[[ns("saverule")]], {
      req(nzchar(input[[ns("gid")]] %||% ""))
      a <- getrv("alloc_pend") %||% list()
      m <- getrv("match_pend") %||% a
      b <- x_keys_srv$bars()
      g <- save_group(input[[ns("gid")]], input[[ns("glab")]] %||% "",
                      a, m, b$named, bars = b)
      setrv("alloc_pend", NULL); setrv("match_pend", NULL)
      updateTextInput(session, ns("gid"),
                      value = paste0("GA", length(g) + 1))
      showNotification(paste("Saved", input[[ns("gid")]]), duration = 2) })

    observeEvent(input[[ns("addcatchall")]], {
      g <- getrv("groups") %||% list()
      gid <- paste0("GA", length(g) + 1)
      cb <- ca_keys_srv$bars()
      # Its own bars only. An exclusion made for one group is not a
      # reason to remove a stratum from the last resort as well.
      excl <- cb$named
      # no clause on either side: allocStrataCond is the complement of
      # everything above (assign_alloc_groups is first-match-wins), and
      # bioMatchedCond is every stratum less whatever the bars here drop
      save_group(gid, "catch-all", list(), list(), excl, bars = cb)
      updateTextInput(session, ns("gid"), value = paste0("GA", length(g) + 2))
      showNotification("Catch-all added at the end", duration = 2) })

    # reorder / delete: first match wins, so order is meaningful
    output[[ns("order")]] <- renderUI({
      g <- getrv("groups") %||% list()
      if (!length(g)) return(div(class = "note", "No groups yet."))
      pal <- group_palette(names(g))
      tagList(lapply(seq_along(g), function(i) {
        gid <- names(g)[i]
        btn <- function(dir, sym) tags$button(class = "rulebtn", HTML(sym),
          onclick = sprintf(
            "Shiny.setInputValue('%s',{i:%d,dir:'%s'},{priority:'event'})",
            ns("move"), i, dir))
        div(class = "rulerow",
          div(class = "rulechip", style = paste0("background:", pal[[gid]]), gid),
          div(style = "flex:1;overflow:hidden",
              tags$b(if (nzchar(g[[gid]]$label %||% "")) g[[gid]]$label else gid),
              tags$br(),
              tags$span(style = "font-family:monospace;font-size:10px;color:#555",
                        g[[gid]]$alloc)),
          btn("up", "&#9650;"), btn("down", "&#9660;"), btn("del", "&#10005;"))
      })) })

    observeEvent(input[[ns("move")]], {
      g <- getrv("groups"); i <- input[[ns("move")]]$i; d <- input[[ns("move")]]$dir
      if (d == "del") { g[[i]] <- NULL; setrv("groups", g); return(invisible()) }
      j <- if (d == "up") i - 1 else i + 1
      if (j >= 1 && j <= length(g)) {
        o <- seq_along(g); o[c(i, j)] <- o[c(j, i)]
        setrv("groups", g[o]) } })

    man_assigned <- reactive({
      g <- getrv("groups") %||% list()
      validate(need(length(g), "No groups yet."))
      bl <- blink_for("cov")
      validate(need(!is.null(bl) && nrow(bl), "No catch rows."))
      assign_alloc_groups(bl, g, bms_on()) })

    # The same green/red verdict the discard builder shows, on both the
    # Overview and Rivers tabs so either landing point shows it.
    check_ui <- reactive({
      g <- getrv("groups") %||% list()
      if (!length(g)) return(NULL)
      d <- tryCatch(manual_progress(man_assigned(), g), error = function(e) NULL)
      if (is.null(d)) return(NULL)
      miss <- d[d$group == "STILL MISSING", , drop = FALSE]
      ok <- !nrow(miss) || miss$n_strata[1] == 0
      div(style = sprintf(
        "border-radius:7px;padding:7px 10px;font-size:12px;margin:6px 0;
         background:%s;border:1px solid %s;color:%s",
        if (ok) "#eef7ee" else "#fff3f3", if (ok) "#59A14F" else "#c0392b",
        if (ok) "#14532d" else "#7a1f1f"),
        if (ok) tagList(tags$b("Every stratum is covered."),
                        " The groups partition the catch, so nothing is ",
                        "allocated twice and nothing is left out.")
        else tagList(tags$b(sprintf(
          "%s strata (%s t) are not covered by any group.",
          miss$n_strata[1], fmt_t3(miss$weight[1]))),
          " They will reach catchAllocationAoL() in no group. Add a ",
          "catch-all as the last group, or widen one above.")) })
    output[[ns("check")]]  <- renderUI(check_ui())
    output[[ns("check2")]] <- renderUI(check_ui())

    # the data BEING allocated a distribution
    output[[ns("man_raise_tbl")]] <- renderDT({
      g <- getrv("groups") %||% list()
      validate(need(length(g), "No groups yet."))
      a <- man_assigned()
      keep <- intersect(c("catchCategory", dims_of()), names(a))
      num <- function(v) if (v %in% names(a)) round(a[[v]], 3) else NA
      d <- data.frame(a[, keep, drop = FALSE],
        weight       = num("weight"),
        coverage     = if ("coverage" %in% names(a)) round(a$coverage, 1) else NA,
        allocated_by = a$alloc_group,
        check.names = FALSE, stringsAsFactors = FALSE)
      d <- d[order(-(d$weight %||% rep(0, nrow(d)))), , drop = FALSE]
      dt_group_colours(
        datatable(d, rownames = FALSE,
                  options = list(scrollX = TRUE, pageLength = 15)),
        "allocated_by", group_palette(names(g))) })

    # the data USED to allocate (bioMatchedCond donor pools)
    output[[ns("man_donor_tbl")]] <- renderDT({
      g <- getrv("groups") %||% list()
      validate(need(length(g), "No groups yet."))
      a <- man_assigned()
      keep <- intersect(c("catchCategory", dims_of()), names(a))
      d <- data.frame(a[, keep, drop = FALSE],
        weight         = if ("weight" %in% names(a)) round(a$weight, 3) else NA,
        coverage       = if ("coverage" %in% names(a)) round(a$coverage, 1) else NA,
        n_measurements = if ("n_measurements" %in% names(a)) a$n_measurements else NA,
        donates_to     = a$usedToAllocGroups_txt,
        check.names = FALSE, stringsAsFactors = FALSE)
      d <- d[order(-(d$weight %||% rep(0, nrow(d)))), , drop = FALSE]
      datatable(d, rownames = FALSE,
                options = list(scrollX = TRUE, pageLength = 15)) })

    output[[ns("conds")]] <- renderText(
      paste(emit_alloc_conditions(getrv("groups") %||% list()), collapse = "\n"))

    # The hierarchical Code tab shows the RECIPE first - keys, per-rung
    # bars, donor exclusions - and only then the conditions it resolves
    # to, exactly as the discard cascade does. Printing the resolved
    # lists alone said which strata ended up together but not the rule
    # that put them there, and could not be re-run on next year's data
    # because the answer was baked into it.
    live_spec <- reactive(list(
      keys = alloc_keys(), thresholds = alloc_thr(), min_meas = alloc_meas(),
      keep_cc = keep_cc(), donor_cov_min = donor_bars()$cov,
      donor_meas_min = donor_bars()$meas, cols = alloc_cols(),
      bms = bms_on(), dims = dims_of()))

    output[[ns("hier_conds")]] <- renderText({
      g <- tryCatch(cascade_to_groups(cascade_assigned(), alloc_cols()),
                    error = function(e) NULL)
      if (is.null(g) || !length(g))
        return("Apply settings to see the resolved grouping.")
      sp <- tryCatch(live_spec(), error = function(e) NULL)
      paste(emit_alloc_recipe_code(sp, g, tag = if (identical(type, "Age"))
                                   "Age" else "Length",
                                   method = "hierarchical"),
            collapse = "\n") })

    # what each rule claims, and what nothing has claimed yet
    output[[ns("progress")]] <- renderDT({
      g <- getrv("groups") %||% list()
      validate(need(length(g), "No groups yet."))
      d <- manual_progress(man_assigned(), g)
      validate(need(!is.null(d), "Nothing to report."))
      DT::formatStyle(
        datatable(d, rownames = FALSE,
                  options = list(scrollX = TRUE, dom = "t")),
        "group", target = "row",
        backgroundColor = DT::styleEqual("STILL MISSING", "#fdece9")) })

    output[[ns("missing")]] <- renderDT({
      g <- getrv("groups") %||% list()
      validate(need(length(g), "No groups yet."))
      d <- manual_missing(man_assigned(), dims_of())
      validate(need(!is.null(d), "Every stratum is assigned."))
      datatable(d, rownames = FALSE,
                options = list(scrollX = TRUE, pageLength = 10)) })

    # one river per rule, same reading as the automated ladder
    output[[ns("rule_rivers")]] <- renderUI({
      g <- getrv("groups") %||% list()
      if (!length(g)) return(div(class = "note", "No groups yet."))
      a <- tryCatch(man_assigned(), error = function(e) NULL)
      if (is.null(a)) return(div(class = "note", "No strata yet."))
      tagList(lapply(seq_along(g), function(i) {
        gid <- names(g)[i]
        n <- sum(a$alloc_group == gid, na.rm = TRUE)
        div(style = "margin-bottom:8px",
            tags$b(sprintf("%s%s - %s stratum/strata", gid,
                           if (nzchar(fld(g[[gid]], "label", "")))
                             paste0(" (", fld(g[[gid]], "label", ""), ")") else "",
                           n)),
            if (n > 0) plotOutput(ns(paste0("rriver_", i)),
                                  height = paste0(max(200, 60 + n * 24), "px"))
            else div(class = "note", "claims nothing"))
      })) })

    observe({
      g <- getrv("groups") %||% list()
      a <- tryCatch(man_assigned(), error = function(e) NULL)
      if (!length(g) || is.null(a)) return()
      pal <- group_palette(names(g))
      for (i in seq_along(g)) local({
        j <- i; gid <- names(g)[j]
        output[[ns(paste0("rriver_", j))]] <- renderPlot({
          sub <- a[!is.na(a$alloc_group) & a$alloc_group == gid, , drop = FALSE]
          validate(need(nrow(sub), "claims nothing"))
          pl <- plot_alloc_river(sub, dims_of(), g[gid], pal[gid])
          validate(need(!is.null(pl), "Nothing to plot.")); pl })
      }) })

    output[[ns("mriver_ui")]] <- renderUI(
      plotOutput(ns("mriver"), height = "560px"))

    # Exposed so the TAF export context can ask this module what
    # resolution it is actually running at - the ctx builder used to
    # read panel_dims("len_lv") for this, a "level of detail" panel
    # that never existed (no len_lv_y / len_lv_x inputs anywhere), so it
    # silently fell through to a hardcoded fallback regardless of what
    # the user had actually configured in this builder.
    list(dims_of = dims_of)
  }
  len_alloc <- length_alloc_server("len", "LengthTotal")
  age_alloc <- length_alloc_server("age", "Age")

  # ---- raising: run catchAllocationAoL and compare -------------
  length_raise_server <- function(p, app_type = "LengthTotal",
                                  what = "Length", store = "len_allocs",
                                  out = "len_raised") {
    ns <- function(x) paste0(p, "_rz_", x)
    dist_tbl <- reactive({
      if (!is.null(rv$dist)) rv$dist
      else if (!is.null(rv$ic$distributions)) as.data.frame(rv$ic$distributions)
      else NULL })

    # The SAME definition length_alloc_server() uses. It was missing here,
    # so `a$dims %||% dims_of()` and the compare run both died with
    # "could not find function dims_of" the moment an allocation was run
    # from this tab. Duplicated deliberately rather than hoisted: the two
    # servers are called with the same `p`, so both resolve to the same
    # level-of-detail box, and a stored allocation still overrides it.
    dims_of <- function()
      unique(c("catchCategory", len_dims(p)))

    observe(updateSelectInput(session, ns("alloc"),
                              choices = names(rv[[store]] %||% list())))

    observeEvent(input[[ns("run")]], {
      nm <- input[[ns("alloc")]]
      a <- (rv[[store]] %||% list())[[nm %||% ""]]
      if (is.null(a)) {
        showNotification("Save an allocation first.", type = "error"); return() }
      # allocation runs on the RAISED catch table when there is one, so
      # length/age work follows the discard raising rather than the import
      cd <- if (!is.null(rv$final)) rv$final
            else if (!is.null(rv$native)) rv$native
            else tryCatch(dat(), error = function(e) NULL)
      if (is.null(cd)) {
        showNotification("No catch data.", type = "error"); return() }
      res <- with_busy(sprintf("Allocating %s...", tolower(what)),
        suppressWarnings(run_allocation(
          cd, dist_tbl(), a$groups,
          alloc_dims = a$dims %||% dims_of(),
          app_type = app_type,
          originType_catch = input[[ns("origin")]] %||% "WGEstimate",
          variableType_catch = input[[ns("vartype")]] %||% "WeightLive",
          weighting = input[[ns("weighting")]] %||% "NumberAtAoL",
          log_file = file.path(tempdir(), "alloc_log.txt"))))
      if (inherits(res, "alloc_error")) {
        rv[[out]] <- NULL
        rv[[paste0(out, "_err")]] <- res$message
        showNotification(paste("Allocation failed:", res$message),
                         type = "error", duration = 12)
      } else {
        res$alloc_name <- nm
        rv[[paste0(out, "_err")]] <- NULL
        rv[[out]] <- res
        showNotification(sprintf("Allocated with '%s': %s distribution rows",
                                 nm, format(nrow(res$distribution),
                                            big.mark = ",")), duration = 5)
      } })

    # ---- comparison across saved allocations --------------------
    observeEvent(input[[ns("runall")]], {
      allocs <- rv[[store]] %||% list()
      if (!length(allocs)) {
        showNotification("Save at least one allocation first.",
                         type = "error"); return() }
      cd <- if (!is.null(rv$final)) rv$final
            else if (!is.null(rv$native)) rv$native
            else tryCatch(dat(), error = function(e) NULL)
      if (is.null(cd)) {
        showNotification("No catch data.", type = "error"); return() }
      runs <- with_busy(sprintf("Running %s allocation(s)...", length(allocs)),
        # Same call as the single/final allocation, alloc_dims included -
        # otherwise the comparison infers dims from column names while the
        # final run uses the explicit set, and the two can silently
        # diverge. They must be the same function with the same arguments,
        # or "compare works but final fails" is inevitable.
        run_allocation_set(allocs, cd, dist_tbl(), app_type,
          alloc_dims = dims_of(),
          originType_catch = input[[ns("origin")]] %||% "WGEstimate",
          variableType_catch = input[[ns("vartype")]] %||% "WeightLive",
          weighting = input[[ns("weighting")]] %||% "NumberAtAoL",
          log_file = file.path(tempdir(), "alloc_cmp.txt")))
      rv[[paste0(out, "_runs")]] <- runs
      bad <- names(runs)[vapply(runs, function(r)
        inherits(r, "alloc_error"), logical(1))]
      showNotification(sprintf("Ran %s allocation(s)%s", length(runs),
        if (length(bad)) paste0("; failed: ", paste(bad, collapse = ", ")) else ""),
        duration = 6, type = if (length(bad)) "warning" else "message") })

    cmp_runs <- reactive(rv[[paste0(out, "_runs")]])

    # Both facet variables have to be carried into the comparison table,
    # or faceting on one of them silently collapses to a single panel:
    # compare_allocations() aggregates to its `grouping` and a column that
    # was never grouped is simply not there afterwards. catchCategory is
    # always kept - it is the split the assessment reads.
    cmp_facets <- reactive(unique(c("catchCategory",
      Filter(nzchar, c(input[[ns("cmpr_facet")]] %||% "",
                       input[[ns("cmpr_shape")]] %||% "")))))

    cmp_data <- reactive({
      r <- cmp_runs(); req(!is.null(r), length(r))
      compare_allocations(r, cmp_facets()) })

    output[[ns("cmpstatus")]] <- renderUI({
      r <- cmp_runs()
      if (is.null(r))
        return(div(class = "note",
                   "Run the saved allocations to compare them."))
      bad <- names(r)[vapply(r, function(x) inherits(x, "alloc_error"),
                             logical(1))]
      tagList(
        div(class = "chain", style = "color:#14532d",
            sprintf("%s allocation(s) run against the current data.",
                    length(r) - length(bad))),
        if (length(bad))
          div(class = "chain", style = "color:#7a1f1f",
              sprintf("Failed: %s", paste(bad, collapse = ", "))) else NULL) })

    output[[ns("cmptot")]] <- renderDT({
      d <- tryCatch(comparison_totals(cmp_data()),
                    error = function(e) {
                      validate(need(FALSE, paste("Could not build the totals:",
                                                 conditionMessage(e))))
                    })
      validate(need(!is.null(d), "Nothing to compare."))
      datatable(d, rownames = FALSE, options = list(dom = "t", scrollX = TRUE)) })

    cmp_sig <- reactive(list(input[[ns("cmpr_facet")]], input[[ns("cmpr_shape")]],
                             input[[ns("cmpncol")]], input[[ns("freex")]],
                             input[[ns("freey")]]))
    cmp_gate <- gate_of(ns("cmp"), cmp_sig,
                        refresh = reactive(rv[[paste0(p, "_groups")]]))

    # One builder, three values. Written as a function of `value` rather
    # than three near-identical blocks so a change to the faceting cannot
    # reach one plot and miss the others - which is exactly how the old
    # single-plot version drifted from the tables beside it.
    cmp_plot_of <- function(value) gated(cmp_gate, {
      d <- cmp_data()
      validate(need(!is.null(d) && nrow(d), "Nothing to compare."))
      f  <- input[[ns("cmpr_facet")]] %||% ""
      f2 <- input[[ns("cmpr_shape")]] %||% ""
      # Wrapped: an error this deep used to crash the whole comparison
      # tab, including the other two plots and the totals table, which
      # made a single bad facet combination look like the feature was
      # broken rather than one input being wrong. Failing here shows the
      # real message instead - if it fires, the text on screen IS the
      # diagnostic, not a guess at one.
      tryCatch(
        plot_allocation_comparison(d, value,
          facet_var = if (nzchar(f)) f else NULL,
          what = what,
          ncol = input[[ns("cmpncol")]] %||% 2,
          facet2 = if (nzchar(f2)) f2 else NULL,
          free_x = isTRUE(input[[ns("freex")]]),
          free_y = !isFALSE(input[[ns("freey")]])),
        error = function(e) {
          validate(need(FALSE, paste("Could not build this comparison:",
                                     conditionMessage(e))))
        }) })

    cmp_obj   <- cmp_plot_of("numbers")
    cmp_obj_w <- cmp_plot_of("tonnes")
    cmp_obj_m <- cmp_plot_of("meanweight")

    # Height follows the facet count, or a grid of eight panels is drawn
    # in the same 460px as a single one and every panel is a smear.
    cmp_h <- function() {
      d <- tryCatch(cmp_data(), error = function(e) NULL)
      f <- input[[ns("cmpr_facet")]] %||% ""
      if (is.null(d) || !nzchar(f) || !f %in% names(d)) return(460)
      n <- length(unique(as.character(d[[f]])))
      f2 <- input[[ns("cmpr_shape")]] %||% ""
      rows <- if (nzchar(f2) && f2 %in% names(d)) n
              else ceiling(n / max(1, input[[ns("cmpncol")]] %||% 2))
      max(360, min(1600, 200 + rows * 200))
    }

    # Written out three times rather than registered in a loop, on
    # purpose. check_server / test_wiring scan for a literal
    # output[[ns("x")]] to prove every UI slot is filled; a slot bound
    # through a loop variable is invisible to that scan, so a typo in a
    # plot id would go back to being something only a human clicking the
    # tab could find. The shared logic is in cmp_plot_of() above, so
    # there is nothing here to drift.
    cmp_panel <- function(obj) renderPlot({
      pl <- obj(); validate(need(!is.null(pl), ""))
      pl + theme(legend.position = "none") })

    # The comparison facets were left with role_chips()' placeholder -
    # "-- none --" and nothing else - so neither picker could be set and
    # the figures could not be split at all.
    local({
      d <- c(facet = "catchCategory", shape = "")
      observe({
        ch <- c(NONE, "Catch category" = "catchCategory", gvars())
        for (r in names(d)) {
          id  <- ns(paste0("cmpr_", r))
          cur <- isolate(input[[id]])
          sel <- if (is.null(cur)) { if (d[[r]] %in% ch) d[[r]] else "" }
                 else if (!nzchar(cur) || cur %in% ch) cur else ""
          updateSelectInput(session, id, choices = ch, selected = sel)
        }
      })
    })

    output[[ns("cmpplot_ui")]] <- renderUI(
      plotOutput(ns("cmpplot"), height = sprintf("%dpx", cmp_h())))
    output[[ns("cmpplot")]] <- cmp_panel(cmp_obj)

    output[[ns("cmpplotw_ui")]] <- renderUI(
      plotOutput(ns("cmpplotw"), height = sprintf("%dpx", cmp_h())))
    output[[ns("cmpplotw")]] <- cmp_panel(cmp_obj_w)

    output[[ns("cmpplotmw_ui")]] <- renderUI(
      plotOutput(ns("cmpplotmw"), height = sprintf("%dpx", cmp_h())))
    output[[ns("cmpplotmw")]] <- cmp_panel(cmp_obj_m)

    output[[ns("cmplegend")]] <- renderPlot({
      pl <- cmp_obj(); validate(need(!is.null(pl), "")); draw_legend(pl) },
      bg = "transparent")

    grp <- reactive(if (isTruthy(input[[ns("group")]]))
                      input[[ns("group")]] else character(0))
    agg_args <- reactive(list(
      minAoL = input[[ns("minaol")]] %||% 0,
      maxAoL = if (is.na(input[[ns("maxaol")]] %||% NA)) NA
               else input[[ns("maxaol")]],
      plusGroup = isTRUE(input[[ns("plus")]])))

    # Length/age allocation uses the catch weights as the fraction to
    # apply. If the discards have not been raised yet those weights are
    # only the landings plus whatever discards were imported, so the
    # allocation is applied to an incomplete catch. That is sometimes
    # what you want and sometimes a mistake, so it is flagged rather than
    # blocked.
    discards_raised <- reactive(!is.null(rv$final) || !is.null(rv$native))

    output[[ns("raisewarn")]] <- renderUI({
      if (discards_raised())
        return(div(class = "chain", style = "color:#14532d",
                   sprintf("Discards have been raised; the %s allocation will use the raised catch table.",
                           tolower(what))))
      div(class = "chain", style = "color:#9a5000;border-left:5px solid #E15759",
          tags$b("Discard raising has not been performed yet."), tags$br(),
          sprintf(paste("%s allocations will only be applied to landings and",
                        "imported discards. Consider whether that is sensible,",
                        "or go back to the Discards tab and raise the",
                        "corresponding discards first."), what)) })

    output[[ns("status")]] <- renderUI({
      r <- rv[[out]]; er <- rv[[paste0(out, "_err")]]
      if (is.null(r) && !is.null(er))
        return(div(class = "chain", style = "color:#7a1f1f",
                   tags$b("Allocation failed:"), tags$br(), er))
      if (is.null(r))
        return(div(class = "note",
          sprintf("Pick an allocation and run it. %s classes come from the distributions table; the catch weights come from the raised catch table when one exists.",
                  what)))
      div(class = "chain", style = "color:#14532d",
          sprintf("Allocated with '%s': %s distribution rows, %s catch rows. Imported rows are kept and flagged, so nothing measured is overwritten.",
                  r$alloc_name %||% "?",
                  format(nrow(r$distribution), big.mark = ","),
                  format(nrow(r$catch), big.mark = ","))) })

    cmp_n <- reactive({ r <- rv[[out]]; req(!is.null(r))
      suppressWarnings(alloc_compare_numbers(r, grp())) })
    cmp_w <- reactive({ r <- rv[[out]]; req(!is.null(r))
      suppressWarnings(alloc_stock_weight(r, grp())) })

    # both figures come off the same aggregation, so they share a gate
    fig_sig <- reactive(list(input[[ns("group")]], input[[ns("minaol")]],
                             input[[ns("maxaol")]], input[[ns("plus")]]))
    fig_gate <- gate_of(ns("fig"), fig_sig, refresh = reactive(rv[[out]]))

    nplot_obj <- gated(fig_gate, {
      d <- cmp_n(); validate(need(!is.null(d) && nrow(d), "Nothing to compare."))
      plot_alloc_numbers(d, if (length(grp())) grp()[1] else NULL, what) })
    wplot_obj <- gated(fig_gate, {
      d <- cmp_w(); validate(need(!is.null(d) && nrow(d), "Nothing to compare."))
      plot_alloc_stockweight(d, if (length(grp())) grp()[1] else NULL, what) })

    output[[ns("nplot_ui")]] <- renderUI(plotOutput(ns("nplot"), height = "420px"))
    output[[ns("wplot_ui")]] <- renderUI(plotOutput(ns("wplot"), height = "420px"))
    output[[ns("nplot")]] <- renderPlot({
      pl <- nplot_obj(); validate(need(!is.null(pl), ""))
      pl + theme(legend.position = "none") })
    output[[ns("wplot")]] <- renderPlot({
      pl <- wplot_obj(); validate(need(!is.null(pl), ""))
      pl + theme(legend.position = "none") })
    output[[ns("legend")]] <- renderPlot({
      pl <- nplot_obj(); validate(need(!is.null(pl), "")); draw_legend(pl) },
      bg = "transparent")

    obj_nm <- if (identical(what, "Age")) "alloc_age" else "alloc_length"
    keep_plot(ns("nplot"), nplot_obj, gate = fig_gate,
      section = "output",
      title = sprintf("Numbers at %s - imported vs allocated", tolower(what)),
      note = function() sprintf("grouped by %s",
        if (length(grp())) paste(grp(), collapse = ", ") else "(nothing)"),
      prep = function() sprintf("d_n <- alloc_compare_numbers(%s, %s)",
                                obj_nm, fmt_vals(grp())),
      call = function() taf_call("plot_alloc_numbers", list(
        "d_n", taf_q(if (length(grp())) grp()[1] else NULL), taf_q(what))))

    keep_plot(ns("wplot"), wplot_obj, gate = fig_gate,
      section = "output",
      title = sprintf("Stock weight at %s (tonnes)", tolower(what)),
      note = function() sprintf("grouped by %s",
        if (length(grp())) paste(grp(), collapse = ", ") else "(nothing)"),
      prep = function() sprintf("d_w <- alloc_stock_weight(%s, %s)",
                                obj_nm, fmt_vals(grp())),
      call = function() taf_call("plot_alloc_stockweight", list(
        "d_w", taf_q(if (length(grp())) grp()[1] else NULL), taf_q(what))))

    output[[ns("overview")]] <- renderDT({
      r <- rv[[out]]; validate(need(!is.null(r), "Run the allocation first."))
      d <- suppressWarnings(alloc_overview(r))
      validate(need(!is.null(d), "Nothing to summarise."))
      datatable(d, rownames = FALSE, options = list(dom = "t", scrollX = TRUE)) })

    output[[ns("ntab")]] <- renderDT({
      r <- rv[[out]]; validate(need(!is.null(r), "Run the allocation first."))
      a <- agg_args()
      d <- suppressWarnings(agg_numbers(r,
        grouping = unique(c(grp(), "sampledOrEstimated")),
        minAoL = a$minAoL, maxAoL = a$maxAoL, plusGroup = a$plusGroup))
      validate(need(!is.null(d), "Aggregation returned nothing."))
      datatable(d, rownames = FALSE, options = list(scrollX = TRUE, dom = "tp")) })

    output[[ns("wtab")]] <- renderDT({
      r <- rv[[out]]; validate(need(!is.null(r), "Run the allocation first."))
      a <- agg_args()
      d <- suppressWarnings(agg_meanweight(r,
        grouping = unique(c(grp(), "sampledOrEstimated")),
        minAoL = a$minAoL, maxAoL = a$maxAoL, plusGroup = a$plusGroup,
        unit = "g"))
      validate(need(!is.null(d), "Aggregation returned nothing."))
      datatable(d, rownames = FALSE, options = list(scrollX = TRUE, dom = "tp")) })
  }

  length_raise_server("len", "LengthTotal", "Length", "len_allocs", "len_raised")
  length_raise_server("age", "Age", "Age", "age_allocs", "age_raised")

  # =========================================================
  #  SECTION 1 - InterCatch -> CEF
  # =========================================================
  dir_picker_server("ic_path", input, output, session)
  dir_picker_server("cef_dir", input, output, session)
  dir_picker_server("cfg_dir", input, output, session)

  # the source icons carry icexch / icout; the rest of the code (and
  # IC_FILE_ROLES) speaks exchange / output
  ic_src <- reactive(
    if (identical(input$ic_source %||% "icexch", "icout")) "output" else "exchange")

  # files can come from an upload or a folder on disk
  ic_mode <- reactive(input$ic_mode %||% "upload")
  ic_uploads <- reactive(input$ic_upl)   # data frame: name, datapath, ...

  # resolve a chosen file NAME to an actual path on disk: the temp datapath
  # for an upload, dir/name for a folder. Must be called reactively.
  ic_path_of <- function(name) {
    if (!isTruthy(name)) return(NA_character_)
    if (ic_mode() == "upload") {
      u <- ic_uploads()
      if (is.null(u)) return(NA_character_)
      hit <- u$datapath[match(name, u$name)]
      if (length(hit) && !is.na(hit)) hit else NA_character_
    } else file.path(ic_dir(), name)
  }

  # the role -> file mapping the user set in the file-map table
  ic_mapping <- reactive({
    roles <- names(IC_FILE_ROLES[[ic_src()]])
    setNames(lapply(roles, function(k) ic_path_of(input[[paste0("icf_", k)]])),
             roles)
  })

  # year: detected from the files, overridable by the ic_years box
  ic_years_detected <- reactive({
    p <- ic_file_paths()
    if (!length(p)) return(integer(0))
    ic_detect_year(p)
  })

  ic_inputs <- reactive(list(
    source = ic_src(),
    path = input$ic_path %||% ".",
    years = {
      typed <- suppressWarnings(as.numeric(
        strsplit(input$ic_years %||% "", "\\s*,\\s*")[[1]]))
      typed <- typed[!is.na(typed)]
      if (length(typed)) typed
      else if (length(ic_years_detected())) ic_years_detected()
      else cfg$year
    },
    rel_mode = input$ic_rel_mode %||% "build",
    rel_year = input$ic_rel_year %||% 2022,
    rel_file = if (!is.null(input$ic_rel_file)) input$ic_rel_file$name else NULL,
    output_format = input$ic_output_format,
    out_path = input$ic_out_path %||% ".",
    prefix = input$ic_prefix %||% "", suffix = input$ic_suffix %||% "",
    incl_bms = isTRUE(input$ic_incl_bms), patch_psu = isTRUE(input$ic_patch_psu),
    keep_temp = isTRUE(input$ic_keep_temp), metier6 = input$ic_metier6))

  # push the detected year into the box (and the rel year) once found
  observeEvent(ic_years_detected(), {
    y <- ic_years_detected()
    if (length(y)) {
      updateTextInput(session, "ic_years", value = paste(y, collapse = ", "))
      updateNumericInput(session, "ic_rel_year", value = max(y))
    }
  }, ignoreInit = TRUE)


  output$ic_status <- renderUI({
    st <- EXT_STATUS
    row <- function(lbl, ok) div(style = sprintf("color:%s;font-size:12px",
      if (ok) "#14532d" else "#7a1f1f"),
      HTML(sprintf("%s %s", if (ok) "&#10003;" else "&#10007;", lbl)))
    div(class = "chain",
      row("funIntercatchCEF()", st$ic_exchange),
      row("funICoutCEF()", st$ic_output),
      row("funMakeRelation()", st$make_rel),
      row("check_group_conditions()", st$group_cond),
      row("catchRaising()", st$raising),
      row("catchAllocationAoL()", st$catch_alloc),
      row("catchAtAoLAggregation()", st$catch_agg),
      row("meanWoLatAoLAggregation()", st$mean_agg),
      if (length(st$pkgs_missing))
        div(class = "note", style = "color:#9a5000;margin-top:6px",
            "Packages not installed here: ",
            paste(st$pkgs_missing, collapse = ", "),
            ". The call above is still written to the R script, so it can ",
            "be run where they are available.") else NULL,
      if (length(EXT$missing))
        div(class = "note", style = "margin-top:4px",
            "Scripts not found in ", EXT$dir, ": ",
            paste(EXT$missing, collapse = ", ")) else NULL) })

  output$ic_source_note <- renderUI({
    src <- ic_src()
    # a bare character VECTOR is not a valid tag child - htmltools needs
    # one string, or the pieces passed as separate arguments
    txt <- if (src == "exchange")
      paste("One csv file holding HI, SI and SD records. Conversion to CEF",
            "will be done using the funIntercatchCEF() function.")
    else
      paste("The .txt InterCatch export files. The tables are first",
            "re-written into exchange format with funICoutCEF() into a",
            "temporary file. Conversion to CEF is then done using the",
            "funIntercatchCEF() function.")
    div(class = "note", txt) })

  # the note at the top of the Input step - depends on which route
  output$ic_input_note <- renderUI({
    txt <- if (ic_src() == "exchange")
      paste("Select the directory or files to be loaded. A single csv file",
            "with HI, SI and SD records is expected. If data is split into",
            "several files, please merge those first before loading them.")
    else
      paste("Select the directory or files to be loaded. Three txt files:",
            "StockOverview, NumbersAtLengthAge and MeanWeightAtLengthAge .txt",
            "are expected.")
    div(class = "note", txt) })

  # which files are needed, which were found, and which is which
  ic_dir <- reactive(if (isTruthy(input$ic_path)) input$ic_path else cfg$dir)

  ic_files <- reactive({
    if (ic_mode() == "upload") {
      u <- ic_uploads(); if (is.null(u)) character(0) else sort(u$name)
    } else {
      d <- ic_dir()
      if (!dir.exists(d)) character(0)
      else sort(list.files(d, pattern = "\\.(csv|txt)$", ignore.case = TRUE))
    } })

  # actual paths behind the listed file names (for year detection etc.)
  ic_file_paths <- reactive({
    fs <- ic_files(); if (!length(fs)) return(character(0))
    unname(vapply(fs, ic_path_of, character(1))) })

  ic_roles <- reactive({
    roles <- IC_FILE_ROLES[[ic_src()]]
    # SD / the age-length files are only needed when lengths or ages are wanted
    if (!any(c("lengths", "ages") %in% cfg$need))
      roles <- roles[!names(roles) %in%
                     c("SD", "NumbersAtAgeLength", "MeanWeigthAtAgeLength")]
    roles })

  output$ic_file_map <- renderUI({
    fs <- ic_files(); roles <- ic_roles()
    all_roles <- IC_FILE_ROLES[[ic_src()]]
    if (ic_mode() == "dir" && !dir.exists(ic_dir()))
      return(div(class = "chain", style = "color:#7a1f1f",
                 sprintf("Directory not found: %s", ic_dir())))
    if (!length(fs))
      return(div(class = "chain", style = "color:#9a5000",
                 if (ic_mode() == "upload")
                   "Upload the InterCatch files on the left, then map each role here."
                 else sprintf("No csv or txt files in %s", ic_dir())))
    rows <- lapply(names(all_roles), function(k) {
      needed <- k %in% names(roles)
      guess <- fs[grepl(IC_FILE_PATTERN[[k]], fs, ignore.case = TRUE)]
      cur <- input[[paste0("icf_", k)]]
      sel <- if (isTruthy(cur) && cur %in% fs) cur else
             if (length(guess)) guess[1] else ""
      okf <- nzchar(sel)
      # green tick when a file is present (required or optionally mapped);
      # red cross only for a REQUIRED file that is missing; grey dash for
      # an optional role left empty
      col <- if (okf) "#14532d" else if (needed) "#7a1f1f" else "#8a8a8a"
      sym <- if (okf) "&#10003;" else if (needed) "&#10007;" else "&#8211;"
      fluidRow(style = "margin-bottom:2px;align-items:center",
        column(1, HTML(sprintf('<span style="color:%s;font-size:16px">%s</span>',
                               col, sym))),
        column(4, tags$b(k, style = "font-size:12px"), tags$br(),
               tags$span(style = "color:#8a8a8a;font-size:10px;
                                  text-transform:uppercase;letter-spacing:.3px",
                         all_roles[[k]])),
        column(7, selectInput(paste0("icf_", k), NULL,
                              choices = c(NONE, fs), selected = sel,
                              width = "100%")))
    })
    div(class = "chain",
      div(class = "note",
          if (ic_mode() == "upload")
            sprintf("%s file(s) uploaded. The guess comes from the file name - ",
                    length(fs))
          else sprintf("%s file(s) in %s. The guess comes from the file name - ",
                       length(fs), ic_dir()),
          "change any row that was matched to the wrong file."),
      do.call(tagList, rows),
      if (!any(c("lengths","ages") %in% cfg$need))
        div(class = "note", style = "margin-top:6px",
            "Sample and age-length files are optional for this run (no length ",
            "or age allocation was requested), but you can still map them to ",
            "explore the biology sampling.") else NULL) })

  observeEvent(input$build_rel, {
    if (!EXT_STATUS$make_rel) {
      showNotification("funMakeRelation() is not loaded.", type = "error"); return()
    }
    r <- tryCatch(funMakeRelation(input$ic_rel_year), error = function(e) e)
    if (inherits(r, "error"))
      showNotification(paste("stock_relation failed:", conditionMessage(r)),
                       type = "error", duration = 8)
    else showNotification("stock_relation built", type = "message")
  })

  output$rel_status <- renderUI({
    ok <- exists("stock_relation", envir = globalenv())
    div(class = "note", style = if (ok) "color:#14532d" else "color:#9a5000",
        if (ok) sprintf("stock_relation in memory (%s rows)",
                        nrow(get("stock_relation", envir = globalenv())))
        else "stock_relation not built yet") })

  # The conversion body, lifted out of the observer so the "replace the
  # data?" path can call exactly the same code the direct path calls.
  do_ic_load <- function() {
    i <- ic_inputs()
    fn <- if (i$source == "exchange") "funIntercatchCEF" else "funICoutCEF"
    if (!exists(fn, mode = "function")) {
      showNotification(paste0(fn, "() is not loaded - check the script folder."),
                       type = "error", duration = 8); return()
    }
    # Stage the mapped files into the layout the function expects, with the
    # canonical names its internal grep looks for. This is what lets an
    # arbitrary directory work instead of failing on the StockOverview
    # grep. If no mapping was made, fall back to the raw path.
    mp <- ic_mapping()
    have_map <- any(vapply(mp, function(x) isTruthy(x) && file.exists(x),
                           logical(1)))
    stage_path <- if (have_map)
      ic_stage_mapping(mp, i$source, i$years[1]) else i$path

    of <- unique(c(i$output_format, "to_list"))
    res <- with_busy("Converting InterCatch to CEF...", tryCatch({
      if (i$source == "exchange")
        funIntercatchCEF(dat_path = stage_path,
          stock_relation = get("stock_relation", envir = globalenv()),
          output_format = of, out_path = i$out_path, file_prefix = i$prefix,
          file_suffix = i$suffix, include_BMS_Caton = i$incl_bms,
          patch_missing_DIS_PSU = i$patch_psu,
          metier6 = if (nzchar(i$metier6)) i$metier6 else NULL)
      else
        funICoutCEF(dat_path = stage_path, years = i$years,
          stock_relation = if (i$rel_mode == "auto") NULL else
            get("stock_relation", envir = globalenv()),
          output_format = of, out_path = i$out_path,
          keep_temp_file = i$keep_temp, file_prefix = i$prefix,
          include_BMS_Caton = i$incl_bms, patch_missing_DIS_PSU = i$patch_psu,
          metier6 = if (nzchar(i$metier6)) i$metier6 else NULL)
    }, error = function(e) e))
    if (inherits(res, "error")) {
      rv$ic <- NULL
      showNotification(paste("Conversion failed:", conditionMessage(res)),
                       type = "error", duration = 10)
    } else {
      rv$ic <- res
      showNotification("Conversion finished", type = "message", duration = 4)
    }
    invisible(NULL)
  }

  observeEvent(input$run_ic, with_load_guard(
    if (identical(input$ic_source %||% "icexch", "icout")) "ic_output"
    else "ic_exchange", do_ic_load))

  output$ic_result <- renderUI({
    if (is.null(rv$ic)) return(div(class = "note", "Nothing converted yet."))
    r <- rv$ic
    div(class = "chain", sprintf(
      "catches: %s rows | distributions: %s | effort: %s",
      if (!is.null(r$catches)) nrow(r$catches) else 0,
      if (!is.null(r$distributions)) nrow(r$distributions) else 0,
      if (!is.null(r$effort)) nrow(r$effort) else 0)) })


  # --- data ------------------------------------------------
  # CEF either uploaded here or handed over from the InterCatch section
  # rv$cef_catches only - never input$file. Reading the upload here made
  # SELECTING a file the load: the reactive fired on the file-chooser, the
  # whole app re-derived, and whatever was loaded before was gone before
  # any guard could ask about it. Both CEF routes now fill rv$cef_catches
  # from a button, and both go through with_load_guard() to get there.
  raw <- reactive({
    if (!is.null(rv$ic$catches)) return(as.data.frame(rv$ic$catches))
    req(!is.null(rv$cef_catches))
    rv$cef_catches
  })

  # load the three CEF tables from a folder, matched by name
  do_cef_load <- function() {
    d <- input$cef_dir %||% "."
    if (!dir.exists(d)) {
      showNotification(paste("Folder not found:", d), type = "error"); return()
    }
    set_busy(TRUE, sprintf("Reading the CEF tables from %s...", basename(d)))
    on.exit(set_busy(FALSE), add = TRUE)
    fs <- list.files(d, pattern = "\\.(csv|txt)$", ignore.case = TRUE,
                     full.names = TRUE)
    pick <- function(pat) {
      m <- fs[grepl(pat, basename(fs), ignore.case = TRUE)]
      if (length(m)) m[1] else NA_character_
    }
    rd <- function(p) if (is.na(p)) NULL else
      read.csv(p, sep = input$sep %||% ",", stringsAsFactors = FALSE)
    cat_f <- pick("catches|catch"); dis_f <- pick("distribution")
    eff_f <- pick("effort")
    if (is.na(cat_f)) {
      showNotification("No catches file found in that folder.",
                       type = "error", duration = 8); return()
    }
    rv$cef_catches <- rd(cat_f)
    rv$dist <- rd(dis_f); rv$eff <- rd(eff_f)
    # remember WHICH files were matched, so data.R can name them instead of
    # re-running the same guesswork on the reader's machine
    rv$cef_files <- list(dir = d, catches = basename(cat_f),
                         distributions = if (is.na(dis_f)) NA_character_
                                         else basename(dis_f),
                         effort = if (is.na(eff_f)) NA_character_
                                  else basename(eff_f))
    showNotification(sprintf("Loaded catches%s%s from folder",
                             if (!is.na(dis_f)) ", distributions" else "",
                             if (!is.na(eff_f)) ", effort" else ""),
                     type = "message", duration = 5)
    invisible(NULL)
  }

  observeEvent(input$cef_load_dir, with_load_guard("cef", do_cef_load))

  # Colour every level of every shared key from the FULL table, the moment
  # the data lands. var_palette() would otherwise assign colours in the
  # order plots happen to ask for them, so opening the length tab before the
  # discard tab could give a country a different colour than the reverse -
  # consistent within the session, but not reproducible between runs, which
  # is not a property a report should have.
  observe({
    d <- tryCatch(derived(), error = function(e) NULL)
    if (!is.null(d) && nrow(d)) seed_palettes(d)
  })

  derived <- reactive({
    d <- raw()
    req_cols <- c("fleetValue","areaValue","vesselFlagCountry","seasonValue",
                  "catchCategory","total")
    validate(need(all(req_cols %in% names(d)),
                  paste("Missing:", paste(setdiff(req_cols, names(d)),
                                          collapse = ", "))))
    if (!"domainCatchDis" %in% names(d)) d$domainCatchDis <- NA_character_
    if (!"stock" %in% names(d)) d$stock <- "(unknown)"
    if (!"year"  %in% names(d)) d$year  <- NA
    derive_fields(d)
  })


  output$stock_ui <- renderUI({ req(derived())
    selectInput("stock", "Stock", sort(unique(derived()$stock))) })
  output$year_ui <- renderUI({ req(derived())
    selectInput("year", "Year",
                c("(all)" = "", sort(unique(as.character(derived()$year))))) })

  dat <- reactive({
    d <- derived()
    if (isTruthy(input$stock)) d <- d[d$stock == input$stock, ]
    if (isTruthy(input$year))  d <- d[as.character(d$year) == input$year, ]
    validate(need(nrow(d) > 0, "No rows for this stock/year."))
    d
  })

  # The level of detail shown here now ALWAYS matches what run_final()
  # actually raises with: the dims saved on whichever allocation is
  # picked in "Adopted allocation" (rv$scenarios[[input$final_alloc]]),
  # which is exactly what its own observeEvent() reads. Before this it
  # showed a SEPARATE choice - "Groups from: auto cascade / manual
  # groups" - that was disconnected from the actual raise (run_final()
  # never read it) and asked the reader to track two ideas of "which
  # level" that were not guaranteed to agree.
  lvl_note <- function() {
    sc <- rv$scenarios[[input$final_alloc %||% ""]]
    if (is.null(sc)) return(NULL)
    div(class = "lodbox", style = "margin-top:10px",
        div(class = "lodtitle", "Level of detail"),
        div(style = "font-size:11.5px;color:#7a4a00;line-height:1.5",
            "Strata built on: ", tags$b(paste(sc$dims, collapse = ", "))),
        tags$span(class = "note", style = "color:#9a5000",
                  "As saved with '", input$final_alloc, "' - the level this ",
                  "raise will actually run at."))
  }
  output$fin_lvl_note <- renderUI(lvl_note())

  # ------------------------------------------------------------------
  #  The variable vocabulary
  # ------------------------------------------------------------------
  #
  # There is no longer a "level of detail". Axes, cascade keys and clause
  # variables all name REAL COLUMNS, so metier4 and metier6 are two
  # different choices rather than one "Fleet" seen at two resolutions -
  # which is what lets a cascade widen from one to the other, and a
  # figure put metier4 on y and mesh on fill at the same time.
  gvars <- reactive({
    d <- tryCatch(dat(), error = function(e) NULL)
    grouping_vars(d)
  })

  # Used only until data lands, so the controls have something sensible
  # to show rather than an empty dropdown.
  DEF_DIMS <- c("metier4", "Country", "ICESDivision", "Season")
  avail <- function(x) {
    d <- tryCatch(dat(), error = function(e) NULL)
    if (is.null(d)) x else intersect(x, names(d))
  }

  # A selection IS a column now, so this is the identity. Kept as a
  # function because ~30 call sites read through it, and because it
  # still drops the empty "-- none --" choice.
  col_of <- function(p, sel) {
    if (is.null(sel) || !length(sel) || !nzchar(sel)) return(NULL)
    unname(sel)
  }

  # The strata a panel is aggregated to: exactly the columns it puts on
  # screen. Anything not given a role is pooled over, which is what makes
  # one cell one value instead of a pile of overlapping rows.
  panel_dims <- function(p, roles = c("y", "x", "fill", "shape", "facet")) {
    v <- unique(Filter(nzchar, vapply(roles, function(r)
      input[[paste0(p, "_", r)]] %||% "", character(1))))
    v <- avail(v)
    if (length(v)) v else avail(DEF_DIMS)
  }
  role <- function(p, r) col_of(p, input[[paste0(p, "_", r)]])

  # strata at the tab's full resolution (all four dimensions).
  # ANALYSIS path: unfiltered, on purpose.
  strata_p <- function(p) build_strata(dat(), panel_dims(p))

  # EXPLORATION path: the same thing on the filtered view. Every figure
  # reads this one; recommend_raising() and the manual builder read the
  # one above.
  strata_x <- function(p) build_strata(explore_dat(p), panel_dims(p))

  # strata aggregated to EXACTLY the columns used by the plot. Without this
  # the table still holds every dimension, so a heatmap cell or a bubble
  # gets several rows stacked on the same spot.
  strata_roles <- function(p, sels) {
    cols <- unique(unlist(lapply(sels, function(x) col_of(p, x))))
    validate(need(length(cols) >= 1, "Pick at least one variable."))
    build_strata(explore_dat(p), cols)
  }

  # --- data tab summary ------------------------------------

  output$data_msg <- renderUI({ req(dat())
    tagList(
      div(class = "note", style = "font-size:12px",
          sprintf("%s rows | %s strata at the level of detail set bottom-right",
                  nrow(dat()), nrow(strata_p("dt")))),
      if (length(explore_filters_all()))
        div(class = "chain", style = "color:#9a5000",
            tags$b("Figure filters are set on: "),
            paste(names(explore_filters_all()), collapse = ", "),
            tags$br(),
            span(class = "note",
                 "Figures only. Grouping and raising use the full table."))
      else NULL) })



  # --- plot sizing: the container is sized here, so nothing overlaps ----
  # y_sel is the Y AXIS selection, so the height can be counted off the
  # data instead of guessed. Without it a metier6 view drew ~100 fleet
  # codes into the height that suited metier4's handful, and the labels
  # printed over each other.
  hgt <- function(p, facet_sel, n_y = 10, y_sel = NULL) {
    s <- try(strata_x(p), silent = TRUE)
    if (inherits(s, "try-error")) return(460)
    plot_height(s, col_of(p, facet_sel), n_y,
                y_var = if (!is.null(y_sel)) col_of(p, y_sel) else NULL)
  }
  slot_ui <- function(id, h) renderUI(plotOutput(id, height = paste0(h, "px")))

  output$cmp_plot_ui <- renderUI({
    fc <- cmp_col(input$cmp_facet)
    n <- if (!is.null(fc) && length(rv$scenarios))
      length(unique(as.character(cmp()[[fc]]))) else 1
    plotOutput("cmp_plot",
               height = paste0(max(420, 300 + ceiling(n / 2) * 210), "px")) })

  # --- 2. discard rates ------------------------------------


  output$dr_tbl <- renderDT(datatable(
    strata_x("dr") %>% filter(!is.na(discard_rate)) %>%
      mutate(discard_rate = round(discard_rate, 3), coverage = round(coverage, 1)),
    options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE))

  # --- 3/4/5: aggregate to the plotted roles only ----------



  # Key 1 is always the stratum's own data; keys 2..5 are the widening
  # dimensions the user picks. Each key row carries its own threshold
  # slider right beside it.
  # Up to eight widening keys now, not four: the extras the user added are
  # only useful if they can actually be placed in the cascade.
  # Key 1 is the stratum's own data, so Key 6 is the sixth rung: the
  # cascade in the spec - Stratum, metier4, Country, areaValue,
  # ICESSubArea, Season - is exactly six.
  N_KEYS <- 6
  hier <- reactive({
    h <- vapply(2:N_KEYS, function(i) input[[paste0("hier_", i)]] %||% "",
                character(1))
    h <- h[nzchar(h)]
    validate(need(length(h) >= 1, "Pick at least one widening key (Key 2 onward)."),
             need(!any(duplicated(h)), "Each key can be used once."))
    h })

  # A key is either one of the four DIM labels, which resolve through the
  # level of detail, or an extra column, which IS its own name. dmap()
  # returns NA for anything it does not know, and an NA column name
  # reaches build_strata() as a missing column, so the two cases are
  # separated here rather than left to subsetting.
  key_col <- function(k) k
  hier_cols <- reactive(vapply(hier(), key_col, character(1),
                               USE.NAMES = FALSE))

  # Strata are whatever the strata-definition box resolves to, plus
  # every column a cascade key names (a key not covered by the strata
  # definition still needs its own column present for build_strata() to
  # group by it). "Use as provided" resolves to the four native CEF
  # keys, which is what "Stratum" used to mean here directly; the box
  # replaces that hardcoded choice with one the user can widen or swap.
  #
  # Defined here, right after hier_cols() and before output$hier_keys -
  # which reads the strata-definition reactive directly, to warn when a
  # key is finer than what Key 1 resolved to. A renderUI body is lazy,
  # so referencing something assigned later in server() still worked
  # when tested live, but the static wiring check correctly flags it as
  # fragile: it only works because nothing forces hier_keys to render
  # before this line runs once at session start, which is not a
  # guarantee worth relying on when moving the assignment costs nothing.
  ar_strata_dims <- own_key_strata_server("ar_k1")
  ar_dims <- reactive(avail(unique(c(ar_strata_dims(), hier_cols()))))

  # the own-stratum threshold is Key 1's slider
  self_thr_val <- reactive({
    x <- input$thr_1
    if (is.null(x) || all(is.na(x))) 50 else as.numeric(x)[1] })

  output$hier_keys <- renderUI({
    # Key 1: own stratum, with its OWN strata-definition mini-widget
    # ("Use as provided" / "Customize", four family-named slots) instead
    # of a separate top box - what "own stratum" means IS a Key 1
    # decision, not a preamble to it.
    k1ns <- function(x) paste0("ar_k1_", x)
    kv <- key_vocab_grouped(tryCatch(dat(), error = function(e) NULL))
    slot <- function(fam, id) column(3,
      selectInput(k1ns(id), fam, c(NONE, kv[[fam]] %||% character(0)),
                 input[[k1ns(id)]] %||% "", width = "100%"))
    key1 <- tagList(
      fluidRow(class = "hier-key-row",
        column(8,
          tags$b("Key 1"), tags$span(class = "note", " - Own stratum"),
          radioButtons(k1ns("mode"), NULL,
                       c("Use as provided" = "asis",
                         "Customize" = "custom"), "asis", inline = TRUE)),
        column(4, sliderInput("thr_1", "threshold %", 0, 100,
                              c(input$threshold %||% 50, 100), 5))),
      conditionalPanel(sprintf("input.%s == 'custom'", k1ns("mode")),
        fluidRow(class = "hier-key-row",
          slot("Fleet", "fleet"), slot("Country", "country"),
          slot("Area", "area"), slot("Season", "season"))),
      uiOutput(k1ns("summary")))

    # Keys 2..N: the cascade-key vocabulary, grouped into Fleet / Country
    # / Area / Season optgroups so the dropdown reads as four questions
    # rather than ten items in one flat list. Rows past the last filled
    # one are not drawn, so a cascade of three keys is three rows and
    # not six - the picker grows as it is used instead of presenting
    # empty slots.
    choices <- c(NONE, kv)
    picked <- vapply(2:N_KEYS, function(i) input[[paste0("hier_", i)]] %||% "",
                     character(1))
    # The spec's cascade, as the out-of-the-box default: season, then
    # area, then country, then fleet, with the last slot left empty.
    # Indexed [i - 1] for Key 2 onward, because Key 1 is fixed as the
    # stratum's own data.
    dflts <- c("Season", "areaValue", "Country", "fleetValue",
              "")[seq_len(N_KEYS - 1)]
    filled <- which(nzchar(picked))
    last <- if (length(filled)) max(filled) else 4
    show <- 2:min(N_KEYS, last + 2)
    krows <- lapply(show, function(i) {
      dflt <- dflts[i - 1]
      fluidRow(class = "hier-key-row",
        column(8, selectInput(paste0("hier_", i), paste("Key", i), choices,
                              input[[paste0("hier_", i)]] %||% dflt)),
        column(4, sliderInput(paste0("thr_", i), "threshold %", 0, 100,
                              c(input$threshold %||% 50, 100), 5)))
    })
    dir <- div(class = "cascade-dir",
               HTML("&darr;"), tags$b(" tries own data first, widens downward"),
               tags$br(),
               tags$span(class = "note",
                         "each key's threshold is the coverage the pool at ",
                         "that level must reach before it is used"))

    # Two live checks, both red text, both non-blocking - a warning, not
    # a validation error, because a user mid-edit (about to fix the next
    # key) should never see the picker itself refuse the value.
    key_picks <- picked[nzchar(picked)]
    w1 <- key_vs_strata_warnings(key_picks, ar_strata_dims())
    w2 <- key_order_warnings(key_picks)
    warn <- if (length(c(w1, w2)))
      div(class = "note", style = "color:#7a1f1f;margin-top:4px",
          lapply(c(w1, w2), function(m) tagList(HTML("&#9888; "), m, tags$br())))

    tagList(dir, key1, tags$hr(style = "margin:6px 0"), krows, warn) })

  output$hier_chain <- renderUI({
    st <- raising_steps(hier_cols(), hier_thr(),
                        self_thr_val(),
                        "nested", ar_dims())
    div(class = "chain", tags$b("Order actually tested:"), tags$br(),
        HTML(paste(c(vapply(st, `[[`, character(1), "label"), "ALL"),
                   collapse = " &rarr; "))) })

  # the ladder: a step card on the left, its own river on the right
  output$tree_ui <- renderUI({
    tr <- tree_tbl_r()
    pal <- c("#00441B","#41AE76","#7FCDBB","#FFD92F","#E66101","#BFBFBF")
    rows <- lapply(seq_len(nrow(tr)), function(i) {
      r <- tr[i, ]; col <- pal[min(i, length(pal))]
      fluidRow(style = "margin-bottom:10px",
        column(4,
          div(style = sprintf(
            "border-left:6px solid %s;background:#fbfbfd;border:1px solid #e2e2e8;
             border-radius:7px;padding:7px 10px;font-size:12px", col),
            tags$b(paste0(if (r$step == "Fallback") "Fallback" else
                          paste("Step", r$step), ": ", r$pool)),
            if (!is.na(r$threshold))
              tags$span(style = "color:#555",
                        sprintf("  (needs %s%%)", r$threshold)) else NULL,
            tags$br(),
            tags$span(style = "color:#666", r$what_it_pools), tags$br(),
            tags$span(style = sprintf("color:%s;font-weight:700", col),
              sprintf("%s strata | %s t (%s%% of landings)",
                      r$n_strata, fmt_t3(r$landings), r$pct_landings)),
            tags$br(),
            tags$span(style = "color:#777;font-size:11px",
              sprintf("cumulative: %s%% of strata, %s%% of landings resolved",
                      r$cum_pct_strata, r$cum_pct_landings)))),
        column(8,
          if (r$n_strata > 0)
            plotOutput(paste0("river_", i),
                       height = paste0(max(240, 60 + r$n_strata * 26), "px"))
          else div(class = "note", style = "padding-top:24px",
                   "nothing resolved at this step")))
    })
    do.call(tagList, rows) })

  # One walk of the cascade, not two. This used to be a pair of reactives
  # calling decision_tree() and decision_tree2() with identical arguments,
  # so every threshold change resolved the whole cascade twice to build
  # one table that was a prefix of the other. check_partition() reads only
  # n_strata and landings, both present here.
  tree_tbl_r <- reactive(decision_tree(
    rec(), hier_cols(), hier_thr(),
    self_thr_val(),
    "nested", ar_dims(), cumulative = TRUE))

  output$tree_tbl <- renderDT(datatable(
    tree_tbl_r(), rownames = FALSE, options = list(dom = "t", scrollX = TRUE)))


  observe({
    tr <- tree_tbl_r()
    ids <- c("0", vapply(raising_steps(
      hier_cols(), hier_thr(),
      self_thr_val(),
      "nested", ar_dims()),
      `[[`, character(1), "id"), "fallback")
    rg <- rec_g(); pal <- auto_pal(); dd <- ar_dims()
    for (i in seq_along(ids)) local({
      j <- i; id <- ids[j]
      output[[paste0("river_", j)]] <- renderPlot({
        p <- plot_river_level(rg, id, dd, pal)
        validate(need(!is.null(p), "")); p })
    }) })

  # every stratum must leave at exactly one step
  # the raising conditions alone, for the Code tab of the hierarchical
  # builder - the full script with headers stays in the R-script section
  output$auto_conds <- renderText({
    r <- tryCatch(rec(), error = function(e) NULL)
    if (is.null(r)) return("Apply settings to see the conditions.")
    b <- ar_excl_keys_srv$bars()
    paste(emit_hier_code(r, hier_cols(), hier_thr(),
                         "fine_to_coarse", ar_dims(),
                         self_threshold = self_thr_val(),
                         donor_cov_min = b$cov_min, donor_meas_min = b$meas_min,
                         donor_excluded = b$named),
          collapse = "\n") })

  output$tree_check <- renderUI({
    ck <- check_partition(rec(), tree_tbl_r())
    good <- ck$n_ok && ck$l_ok && ck$unassigned == 0
    div(style = sprintf(
      "border-radius:6px;padding:7px 10px;margin:4px 0 12px 0;font-size:12px;
       background:%s;border:1px solid %s;color:%s",
      if (good) "#eaf7ee" else "#fdeaea",
      if (good) "#41AE76" else "#E15759",
      if (good) "#14532d" else "#7a1f1f"),
      tags$b(if (good) "Checks out: the steps partition the data exactly."
             else "Problem: the steps do not partition the data."),
      tags$br(),
      sprintf("Strata assigned %s of %s. Landings across steps %s t of %s t total.",
              format(ck$n_steps, big.mark = ","), format(ck$n_total, big.mark = ","),
              fmt_t3(ck$l_steps), fmt_t3(ck$l_total)),
      if (ck$unassigned > 0)
        tags$div(sprintf("%s strata never resolved.", ck$unassigned)) else NULL)
  })

  # A single-value slider paints the region BELOW the handle, i.e. the part
  # that fails. Using a range pinned at 100 paints the qualifying side
  # instead, which is what the threshold actually means.
  observeEvent(input$apply_all, {
    v <- as.numeric(input$thr_all)[1]   # min side of the range slider
    updateSliderInput(session, "thr_1", value = c(v, 100))
    for (i in 2:N_KEYS)
      updateSliderInput(session, paste0("thr_", i), value = c(v, 100))
    showNotification(paste("All thresholds set to", v, "%"),
                     type = "message", duration = 2) })

  # thresholds for the widening keys (2..5), in the order they are tested
  # Which key SLOTS are filled. hier() drops the empty ones, so indexing
  # thresholds by position in hier() reads thr_2 for whatever the first
  # non-empty key happens to be - leaving Key 2 blank and filling Key 3
  # silently applied Key 2's threshold to Key 3. Harmless with four
  # contiguous defaults, easy to hit now there are eight slots.
  hier_idx <- reactive({
    h <- vapply(2:N_KEYS, function(i) input[[paste0("hier_", i)]] %||% "",
                character(1))
    (2:N_KEYS)[nzchar(h)] })

  hier_thr <- reactive({
    vapply(hier_idx(), function(i) {
      x <- input[[paste0("thr_", i)]]
      if (is.null(x) || all(is.na(x))) input$threshold else as.numeric(x)[1]
    }, numeric(1)) })

  # a discard rate can exceed 1 (discards with almost no landings), so the
  # slider always reaches 5% past the largest rate present - otherwise
  # those strata cannot be included or excluded explicitly
  dr_top <- function(p) {
    s <- try(strata_p(p), silent = TRUE)
    if (inherits(s, "try-error")) return(1)
    m <- suppressWarnings(max(s$discard_rate, na.rm = TRUE))
    if (!is.finite(m)) 1 else max(1, ceiling(m * 1.05 * 100) / 100) }

  # strata_of and dims_of MUST agree. strata_p() aggregates to
  # panel_dims(), which reads the plot role inputs - and a builder has
  # none, so it fell back to DEF_DIMS while dims_of() returned the
  # cascade keys. exclusion_choices() then asked for Stratum, areaValue
  # and ICESSubArea in a table built without them and returned nothing,
  # so the "exclude a specific stratum" dropdown was empty.
  ar_excl_keys_srv <- exclusion_server("ar",
    strata_of = function() build_strata(dat(), ar_dims()),
    dims_of   = function() ar_dims(),
    metrics   = c("coverage", "trips", "rate"),
    dr_of     = function() ar_dr())

  mg_excl_keys_srv <- exclusion_server("mg",
    strata_of = function() build_strata(dat(), mg_dims()),
    dims_of   = function() mg_dims(),
    metrics   = c("coverage", "trips", "rate"),
    dr_of     = function() rule_dr())

  ca_excl_keys_srv <- exclusion_server("ca",
    strata_of = function() build_strata(dat(), mg_dims()),
    dims_of   = function() mg_dims(),
    metrics   = c("coverage", "trips"))

  ar_excl_keys <- ar_excl_keys_srv$keys
  mg_excl_keys <- mg_excl_keys_srv$keys
  ca_excl_keys <- ca_excl_keys_srv$keys

  output$man_progress <- renderDT({
    validate(need(length(rv$rules), "No groups yet."))
    d <- rules_progress(man_strata(), rv$rules, rv$sets)
    validate(need(!is.null(d), "Nothing to report."))
    datatable(d, rownames = FALSE, options = list(scrollX = TRUE, dom = "t")) })

  output$man_missing <- renderDT({
    d <- rules_unassigned(man_strata(), rv$rules, rv$sets,
                          mg_dims())
    validate(need(!is.null(d) && nrow(d), "Every stratum is claimed."))
    datatable(d, rownames = FALSE,
              options = list(scrollX = TRUE, pageLength = 10)) })

  # the two condition lists, ready to paste into a script
  output$man_conds <- renderText({
    if (!length(rv$rules)) return("(no groups yet)")
    paste(emit_rules_code(rv$rules, rv$sets, mg_dims()),
          collapse = "\n") })

  # push a sparkline per stratum so the hover preview has something to
  # show. Only the length link carries distributions, so this is a no-op
  # until one is loaded.
  observe({
    d <- tryCatch(dat(), error = function(e) NULL)
    dst <- if (!is.null(rv$dist)) rv$dist
           else if (!is.null(rv$ic$distributions)) as.data.frame(rv$ic$distributions)
           else NULL
    if (is.null(d) || is.null(dst)) return()
    dims <- ar_dims()
    lk <- tryCatch(link_dist_to_strata(d, dst, c("catchCategory", dims), NULL),
                   error = function(e) NULL)
    if (is.null(lk) || !nrow(lk)) return()
    keys <- unique(do.call(paste, c(lapply(dims, function(k)
      as.character(lk[[k]])), sep = " | ")))
    m <- tryCatch(stratum_sparkline_map(lk, dims, keys),
                  error = function(e) list())
    if (length(m)) session$sendCustomMessage("stratSvg", list(map = m)) })

  # max-only: only an upper bound on discard rate is meaningful
  output$ar_dr_ui <- renderUI({
    top <- dr_top("ar")
    sliderInput("ar_dr", "Allowed discard rate (max)", 0, top, top, 0.01) })

  ar_dr <- reactive({
    v <- input$ar_dr; if (is.null(v)) return(c(NA, NA))
    top <- dr_top("ar")
    c(NA, if (as.numeric(v)[1] < top) as.numeric(v)[1] else NA) })

  # Every role picker on the discard figures draws from the same
  # vocabulary, refreshed when the data changes. Selections are
  # preserved where the column still exists, so switching dataset does
  # not silently reset a figure to defaults.
  role_choices_server <- function(p, roles, defaults) {
    observe({
      ch <- c(NONE, gvars())
      # isolate(): this observer WRITES the inputs it would otherwise
      # read reactively, so without it every update retriggers it.
      for (i in seq_along(roles)) {
        id  <- paste0(p, "_", roles[i])
        cur <- isolate(input[[id]])
        # Three states, and they are not the same:
        #   NULL  never set - use the default
        #   ""    the reader chose "-- none --" - KEEP IT
        #   name  a column - keep it if it still exists
        # Treating "" as "unset" made choosing none impossible: the
        # value came back empty, failed nzchar(), and was overwritten
        # with the default on the very next flush, so the picker
        # silently snapped back to a variable.
        sel <- if (is.null(cur)) {
                 if (defaults[i] %in% ch) defaults[i] else ""
               } else if (!nzchar(cur) || cur %in% ch) {
                 cur                      # "" or a still-valid column
               } else {
                 ""                       # column vanished with the data
               }
        updateSelectInput(session, id, choices = ch, selected = sel)
      }
    })
  }
  role_choices_server("cov", c("y", "fill", "facet"),
                      c("metier4", "Season", "Country"))
  role_choices_server("dr", c("x", "fill", "shape", "facet"),
                      c("metier4", "ICESDivision", "Season", "Country"))
  role_choices_server("box", c("x", "fill", "facet"),
                      c("Country", "ICESDivision", ""))
  # The compare-allocation tabs' Facet / Facet 2 (or X axis / Facet)
  # pickers never had this wired up. role_chips() renders them with
  # NONE (just "-- none --") as a placeholder until something calls
  # updateSelectInput() with the real vocabulary - nothing ever did, so
  # every one of these three dropdowns was permanently stuck on that one
  # placeholder entry regardless of what data was loaded.
  role_choices_server("cmp", c("x", "facet"), c("metier4", ""))
  role_choices_server("len_rz_cmpr", c("facet", "shape"), c("catchCategory", ""))
  role_choices_server("age_rz_cmpr", c("facet", "shape"), c("catchCategory", ""))

  # The adopted-raising composition controls. Same vocabulary as
  # everywhere else, but these are plain selectInputs rather than role
  # chips - only Split by and Facet remain now that Cross-tab is gone.
  observe({
    ch <- gvars()
    if (!length(ch)) return()
    pick <- function(id, prefer, allow_none) {
      cur <- input[[id]]
      opts <- if (allow_none) c("(whole stock)" = "", ch) else ch
      sel <- if (!is.null(cur) && nzchar(cur) && cur %in% ch) cur
             else if (prefer %in% ch) prefer else if (allow_none) "" else ch[[1]]
      updateSelectInput(session, id, choices = opts, selected = sel)
    }
    pick("fin_x",     "metier4", TRUE)
    pick("fin_facet", "",        TRUE)
  })

  ar_hier_sig <- reactive(list(input$threshold, hier_cols(), hier_thr(),
                               self_thr_val(), ar_dr(), ar_excl_keys(),
                               ar_dims()))
  ar_hier_applied <- reactiveVal(NULL)
  observe({ if (is.null(ar_hier_applied())) {
    sg <- tryCatch(ar_hier_sig(), error = function(e) NULL)
    if (!is.null(sg)) ar_hier_applied(sg) } })
  observeEvent(input$ar_apply, ar_hier_applied(ar_hier_sig()))
  observeEvent(input$save_auto, ar_hier_applied(ar_hier_sig()))
  output$ar_hier_stale <- renderUI({
    a <- ar_hier_applied()
    if (is.null(a) || identical(a, ar_hier_sig())) return(NULL)
    tags$span(class = "note", style = "color:#9a5000;margin-left:8px",
              "settings changed - press Apply") })

  rec <- reactive({
    a <- ar_hier_applied(); req(!is.null(a))
    thr0 <- a[[1]]; cols <- a[[2]]; thr <- a[[3]]; self <- a[[4]]
    dr <- a[[5]]; excl <- a[[6]]; dms <- a[[7]]
    # strata built from the APPLIED dims, so the hierarchy (also frozen in
    # this signature) always names columns the strata actually have. Using
    # the live dmap here was the bug: a level change moved the strata
    # columns instantly while cols stayed frozen, and they disagreed.
    s <- classify_strata(build_strata(dat(), dms), thr0)
    s <- flag_donor_exclusion(s, dr[1], dr[2],
                              excluded = excl, dims = dms)
    recommend_raising(s, cols, thr,
                      self_threshold = self,
                      mode = "nested",
                      dims = dms) })

  auto_groups <- reactive(raising_groups(rec()))
  auto_pal <- reactive(group_palette(auto_groups()$group_id))

  # attach the group id to every stratum so the colour is per G, not per level
  rec_g <- reactive({
    g <- auto_groups()
    rec() %>% mutate(group_id = g$group_id[match(raise_condition, g$raise_condition)]) })

  # the data BEING raised - focused columns, TRUE shown as "Catch All by All"
  output$diag_tbl <- renderDT({
    d <- strata_raise_table(rec_g(), ar_dims())
    validate(need(!is.null(d), "Apply settings first."))
    dt_group_colours(
      datatable(d, options = list(scrollX = TRUE, pageLength = 15), rownames = FALSE),
      "group_id", auto_pal()) })

  # the data USED to raise - the donor-pool view of the same strata
  output$diag_donor_tbl <- renderDT({
    d <- strata_donor_table(rec_g(), ar_dims())
    validate(need(!is.null(d), "Apply settings first."))
    datatable(d, options = list(scrollX = TRUE, pageLength = 15), rownames = FALSE) })

  output$grp_tbl <- renderDT(dt_group_colours(
    datatable(auto_groups(), options = list(scrollX = TRUE), rownames = FALSE),
    "group_id", auto_pal()))

  # --- 7. manual -------------------------------------------
  # relevant variables first, everything else behind a disclosure
  output$clause_var_chips <- renderUI({
    d <- tryCatch(dat(), error = function(e) NULL)
    shown <- intersect(FILTER_VARS, names(d %||% list()))
    if (!length(shown)) shown <- FILTER_VARS
    core <- intersect(c("metier4","metier5","metier6","Country",
                        "ICESDivision","Season","FleetType"), shown)
    var_chips("clause_var", core, setdiff(c(shown,
      setdiff(names(d %||% list()), c("total","stratum_id"))), core),
      input$clause_var %||% "Country") })

  observe({ req(dat(), input$clause_var, input$clause_var %in% names(dat()))
    updateSelectizeInput(session, "clause_vals",
      choices = sort(unique(as.character(dat()[[input$clause_var]]))), server = TRUE) })

  observeEvent(input$add_clause, {
    req(length(input$clause_vals) >= 1)
    rv$clause <- c(rv$clause, list(list(var = input$clause_var,
                                        values = input$clause_vals,
                                        negate = isTRUE(input$clause_neg)))) })
  observeEvent(input$clear_clause, rv$clause <- list())

  output$clause_chip <- renderUI({
    empty <- !length(rv$clause)
    div(style = "margin:6px 0",
      tags$div(class = paste("dragchip", if (empty) "empty" else ""),
        draggable = if (empty) "false" else "true",
        ondragstart = "event.dataTransfer.setData('text/plain','clause')",
        if (empty) "(build a clause above)" else cond_string(rv$clause))) })

  observeEvent(input$drop_target, {
    req(length(rv$clause) >= 1)
    if (input$drop_target == "strata") rv$pend_raise <- rv$clause
    else rv$pend_source <- rv$clause
    rv$clause <- list() })

  output$strata_txt_ui <- renderUI(tags$span(
    if (is.null(rv$pend_raise)) "(drop a clause here - matches every stratum)"
    else cond_string(rv$pend_raise)))

  output$matched_txt_ui <- renderUI({
    src <- if (is.null(rv$pend_source)) rv$pend_raise else rv$pend_source
    b <- mg_excl_keys_srv$bars()
    tmp <- new_rule("tmp", "", list(), src %||% list(), rule_dr()[1], rule_dr()[2],
                    excluded = b$named, cov_min = b$cov_min,
                    meas_min = b$meas_min)
    tags$span(paste0(source_cond_string(tmp),
      if (is.null(rv$pend_source)) "  (mirroring strataCond)" else "")) })

  observeEvent(input$to_strata,  { rv$pend_raise  <- rv$clause; rv$clause <- list() })
  observeEvent(input$to_matched, { rv$pend_source <- rv$clause; rv$clause <- list() })
  observeEvent(input$mirror,      rv$pend_source <- rv$pend_raise)
  observeEvent(input$mirror_back, rv$pend_raise  <- rv$pend_source)

  observeEvent(input$save_rule, {
    req(nzchar(input$rule_id))
    b <- mg_excl_keys_srv$bars()
    # the BARS, not the keys they resolve to today: the emitted
    # matchedDataCond then reads `coverage >= 50`, which is checkable
    r <- new_rule(input$rule_id, input$rule_label, rv$pend_raise %||% list(),
                  rv$pend_source, rule_dr()[1], rule_dr()[2],
                  excluded = b$named, cov_min = b$cov_min,
                  meas_min = b$meas_min)
    ids <- vapply(rv$rules, `[[`, character(1), "id")
    if (r$id %in% ids) {
      rv$rules[[which(ids == r$id)]] <- r
      showNotification(paste0("Group '", r$id, "' overwritten"),
                       type = "warning", duration = 3)
    } else {
      rv$rules <- c(rv$rules, list(r))
      showNotification(paste0("Group '", r$id, "' saved"),
                       type = "message", duration = 2)
    }
    rv$clause <- list(); rv$pend_raise <- NULL; rv$pend_source <- NULL
    updateTextInput(session, "rule_id", value = paste0("G", length(rv$rules) + 1)) })

  ca_dr <- reactive({
    g <- function(id) {
      x <- input[[id]]
      if (is.null(x) || !length(x) || all(is.na(x))) NA_real_ else as.numeric(x)[1]
    }
    c(g("catchall_dr_min"), g("catchall_dr_max"))
  })

  observeEvent(input$add_catchall, {
    dr <- ca_dr()
    cb <- ca_excl_keys_srv$bars()
    rv$rules <- c(rv$rules, list(new_rule(paste0("G", length(rv$rules) + 1),
      "catch-all", list(), NULL, dr[1], dr[2],
      # The catch-all's own bars, and only those. An exclusion made for
      # one group is not a reason to remove a stratum from the last
      # resort as well.
      cov_min  = cb$cov_min,
      meas_min = cb$meas_min,
      excluded = cb$named)))
    showNotification("Catch-all added at the end", duration = 2) })

  # The manual builder's strata: whatever the strata-definition box
  # resolves to, plus every column any clause mentions (a clause can
  # reference a variable the strata definition never included, and that
  # column still has to be present for the clause to be evaluated).
  #
  # This is where the box matters most for the manual method: a group's
  # exclusion bars are measured on THESE strata, and at raw upload
  # resolution a stratum is never itself split by a discard sample, so
  # its coverage was always exactly 0% or 100% - no group could ever be
  # "50% covered". Grouping the strata coarser than submission is what
  # makes a coverage bar mean anything here.
  mg_strata_dims <- strata_def_server("mg_sd")
  mg_dims <- reactive({
    vars <- unlist(lapply(rv$rules, function(r) {
      cl <- fld(r, "clauses", list())
      vapply(cl, function(c) fld(c, "var", ""), character(1))
    }), use.names = FALSE)
    vars <- c(vars, unlist(lapply(rv$sets %||% list(), function(z)
      vapply(z, function(c) fld(c, "var", ""), character(1))),
      use.names = FALSE))
    avail(unique(c(mg_strata_dims(), Filter(nzchar, vars %||% character(0)))))
  })

  man_pal <- reactive(group_palette(vapply(rv$rules, `[[`, character(1), "id")))

  output$rule_order <- renderUI({
    if (!length(rv$rules)) return(div(class = "note", "No groups yet."))
    pal <- man_pal()
    tagList(lapply(seq_along(rv$rules), function(i) {
      r <- rv$rules[[i]]
      btn <- function(dir, sym) tags$button(
        class = "rulebtn", HTML(sym),
        onclick = sprintf(
          "Shiny.setInputValue('rule_move',{i:%d,dir:'%s'},{priority:'event'})",
          i, dir))
      div(class = "rulerow",
          div(class = "rulechip", style = paste0("background:", pal[[r$id]]), r$id),
          div(style = "flex:1;overflow:hidden;text-overflow:ellipsis",
              tags$b(r$label), tags$br(),
              tags$span(style = "font-family:monospace;font-size:10px;color:#555",
                        cond_string(r$raise))),
          btn("up", "&#9650;"), btn("down", "&#9660;"), btn("del", "&#10005;"))
    })) })

  observeEvent(input$rule_move, {
    i <- input$rule_move$i; d <- input$rule_move$dir
    if (d == "del") { rv$rules[[i]] <- NULL; return(invisible()) }
    j <- if (d == "up") i - 1 else i + 1
    if (j >= 1 && j <= length(rv$rules)) {
      t <- rv$rules[[i]]; rv$rules[[i]] <- rv$rules[[j]]; rv$rules[[j]] <- t } })

  # The manual builder's OWN strata, at ITS OWN resolution - the strata-
  # definition box plus every column a clause mentions (mg_dims()). This
  # was strata_p("mg") = build_strata(dat(), panel_dims("mg")) - but
  # panel_dims() reads y/x/fill/shape/facet role inputs, and the manual
  # builder has none of those; it is not a plot. Every one of those
  # lookups came back empty and panel_dims() fell through to its
  # generic fallback (metier4/Country/ICESDivision/Season), a default
  # that has nothing to do with what the manual groups are actually
  # defined on. Downstream, man_raise_tbl/man_donor_tbl already computed
  # dd <- mg_dims() and did intersect(dd, names(a)) to select their
  # columns - correctly asking for the full strata definition - but
  # `a` only ever HAD the fallback's 2-4 columns, so the intersect
  # silently kept only whatever happened to overlap (Country and Season,
  # if the real keys were metier4/areaValue/fleetValue - exactly what
  # was reported missing).
  man_strata <- reactive(build_strata(dat(), mg_dims()))

  # ---------------------------------------------------------
  #  The manual decision ladder
  # ---------------------------------------------------------
  # The same view the hierarchical tab has always had, because the question
  # is the same one: what does this rule take, how much of the stock is
  # that, and what is left afterwards. Reading it as cards rather than as a
  # table is what makes an over-broad rule obvious - it shows up as one card
  # swallowing 80% of the landings before the specific rules below it get a
  # chance.
  man_decision <- reactive({
    req(length(rv$rules) >= 1)
    rule_decision_table(man_strata(), rv$rules, rv$sets) })

  man_river_d <- reactive({
    req(length(rv$rules) >= 1)
    rule_river_data(man_strata(), rv$rules, rv$sets) })

  observe({
    tr <- man_decision(); req(!is.null(tr))
    d <- man_river_d(); pal <- man_pal(); dd <- mg_dims()
    for (i in seq_len(nrow(tr))) local({
      j <- i; id <- tr$rule[j]
      output[[paste0("man_river_", j)]] <- renderPlot({
        p <- plot_river_level(d, id, dd, pal)
        validate(need(!is.null(p), "")); p })
    }) })

  # Does the rule set partition the stock? The one thing a reader cannot
  # check by eye, and the one that silently loses landings when it fails.
  # one verdict banner, shown on both the Overview and Rivers tabs, so a
  # user who lands on either sees whether the stock is fully covered
  man_check_ui <- reactive({
    tr <- man_decision(); req(!is.null(tr))
    miss <- tr[tr$rule == "(unassigned)", , drop = FALSE]
    ok <- !nrow(miss)
    div(style = sprintf(
      "border-radius:7px;padding:7px 10px;font-size:12px;margin:6px 0;
       background:%s;border:1px solid %s;color:%s",
      if (ok) "#eef7ee" else "#fff3f3", if (ok) "#59A14F" else "#c0392b",
      if (ok) "#14532d" else "#7a1f1f"),
      if (ok) tagList(tags$b("Every stratum is covered."),
                      " The rules partition the stock, so nothing is raised ",
                      "twice and nothing is left out.")
      else tagList(tags$b(sprintf(
        "%s strata (%s t) are not covered by any rule.",
        miss$n_strata, fmt_t3(miss$landings))),
        " They will reach catchRaising() in no group. Add a catch-all as ",
        "the last rule, or widen one above.")) })
  output$man_check  <- renderUI(man_check_ui())
  output$man_check2 <- renderUI(man_check_ui())

  man_assigned <- reactive({
    req(length(rv$rules) >= 1)
    assign_rule_groups(man_strata(), rv$rules, rv$sets) })

  # the data BEING raised: stratum columns, metrics, status, the rule
  # that raises it. usedToRaise columns dropped - they belong on the
  # donor tab.
  output$man_raise_tbl <- renderDT({
    validate(need(length(rv$rules) >= 1, "No groups yet."))
    dd <- mg_dims()
    a <- man_assigned()
    keep <- intersect(dd, names(a))
    num <- function(v) if (v %in% names(a)) round(a[[v]], 3) else NA
    d <- data.frame(a[, keep, drop = FALSE],
      landings     = num("landings"),
      discards     = num("discards"),
      discard_rate = if ("discard_rate" %in% names(a)) round(a$discard_rate, 3) else NA,
      coverage     = if ("coverage" %in% names(a)) round(a$coverage, 1) else NA,
      Status       = if ("Status" %in% names(a)) as.character(a$Status) else NA,
      raised_by    = a$toBeRaisedGroup,
      check.names = FALSE, stringsAsFactors = FALSE)
    if (all(is.na(d$discards)) || all(d$discards == 0, na.rm = TRUE)) d$discards <- NULL
    d <- d[order(-(d$landings %||% rep(0, nrow(d)))), , drop = FALSE]
    dt_group_colours(datatable(d, options = list(scrollX = TRUE, pageLength = 15),
                               rownames = FALSE), "raised_by", man_pal()) })

  # the data USED to raise: which groups each stratum donates to
  output$man_donor_tbl <- renderDT({
    validate(need(length(rv$rules) >= 1, "No groups yet."))
    dd <- mg_dims()
    a <- man_assigned()
    keep <- intersect(dd, names(a))
    d <- data.frame(a[, keep, drop = FALSE],
      landings     = if ("landings" %in% names(a)) round(a$landings, 3) else NA,
      discards     = if ("discards" %in% names(a)) round(a$discards, 3) else NA,
      discard_rate = if ("discard_rate" %in% names(a)) round(a$discard_rate, 3) else NA,
      coverage     = if ("coverage" %in% names(a)) round(a$coverage, 1) else NA,
      donates_to   = a$usedToRaiseGroups_txt,
      check.names = FALSE, stringsAsFactors = FALSE)
    if (all(is.na(d$discards)) || all(d$discards == 0, na.rm = TRUE)) d$discards <- NULL
    d <- d[order(-(d$landings %||% rep(0, nrow(d)))), , drop = FALSE]
    datatable(d, options = list(scrollX = TRUE, pageLength = 15),
              rownames = FALSE) })

  # One river per rule, the same view the length and age builders have
  # had. The combined river below can hide a rule whose strata are few
  # but whose donor pool is wide - it is one band among forty.
  man_river_label <- function(a, dd) {
    rl <- setNames(vapply(rv$rules, function(r) {
      cl <- cond_string(r$raise)
      base <- if (nzchar(r$label %||% "")) r$label else r$id
      if (identical(cl, "TRUE")) paste0(base, ": all remaining strata  [", r$id, "]")
      else paste0(cl, "  [", r$id, "]")
    }, character(1)), vapply(rv$rules, `[[`, character(1), "id"))
    right <- unname(rl[as.character(a$toBeRaisedGroup)])
    right[is.na(right)] <- as.character(a$toBeRaisedGroup)[is.na(right)]
    right
  }

  output$man_rule_rivers <- renderUI({
    if (!length(rv$rules)) return(div(class = "note", "No groups yet."))
    a <- tryCatch(man_assigned(), error = function(e) NULL)
    if (is.null(a)) return(div(class = "note", "No strata yet."))
    tagList(lapply(seq_along(rv$rules), function(i) {
      r <- rv$rules[[i]]
      n <- sum(a$toBeRaisedGroup == r$id, na.rm = TRUE)
      div(style = "margin-bottom:8px",
          tags$b(sprintf("%s%s - %s stratum/strata", r$id,
                         if (nzchar(r$label %||% ""))
                           paste0(" (", r$label, ")") else "", n)),
          if (n > 0) plotOutput(paste0("mrriver_", i),
                                height = paste0(max(200, 60 + n * 24), "px"))
          else div(class = "note", "claims nothing"))
    })) })

  observe({
    a <- tryCatch(man_assigned(), error = function(e) NULL)
    if (!length(rv$rules) || is.null(a)) return()
    dd <- mg_dims(); pal <- man_pal()
    for (i in seq_along(rv$rules)) local({
      j <- i; gid <- rv$rules[[j]]$id
      output[[paste0("mrriver_", j)]] <- renderPlot({
        sub <- a[!is.na(a$toBeRaisedGroup) & a$toBeRaisedGroup == gid, ,
                 drop = FALSE]
        validate(need(nrow(sub), "claims nothing"))
        pl <- plot_river(sub, left_var = NULL, right_var = "toBeRaisedGroup",
                         value_var = "landings", pal = pal[gid],
                         title = NULL,
                         left_label = stratum_label(sub, dd),
                         right_label = man_river_label(sub, dd),
                         stratum_axis = stratum_axis_label(dd))
        validate(need(!is.null(pl), "Nothing to plot.")); pl })
    }) })

  # ---- group allocations (saved setups) ----
  save_sc <- function(name, sc) {
    dup <- name %in% names(rv$scenarios)
    rv$scenarios[[name]] <- sc
    showNotification(
      if (dup) paste0("Allocation '", name, "' OVERWRITTEN")
      else paste0("Allocation '", name, "' saved - ", length(rv$scenarios),
                  " in this session"),
      type = if (dup) "warning" else "message", duration = 5) }

  observeEvent(input$save_auto, {
    req(nzchar(input$auto_name))
    save_sc(input$auto_name, scenario_auto(
      input$auto_name, ar_dims(), hier_cols(), hier_thr(),
      self_threshold = self_thr_val(),
      mode = "nested",
      dr_min = ar_dr()[1], dr_max = ar_dr()[2])) })

  observeEvent(input$save_manual, {
    req(nzchar(input$man_name), length(rv$rules) >= 1)
    save_sc(input$man_name, scenario_manual(
      input$man_name, mg_dims(), rv$rules, rv$sets)) })

  observe(updateSelectInput(session, "del_sc_id", choices = names(rv$scenarios)))
  observeEvent(input$del_sc, { req(input$del_sc_id)
    nm <- input$del_sc_id; rv$scenarios[[nm]] <- NULL
    showNotification(paste0("Removed '", nm, "'"), duration = 2) })

  observeEvent(input$up_sc, {
    req(input$up_sc)
    sc <- scenarios_from_json(readLines(input$up_sc$datapath, warn = FALSE))
    for (x in sc) rv$scenarios[[x$name]] <- x
    showNotification(paste("Loaded", length(sc), "allocation(s)"), duration = 3) })

  output$dl_sc <- downloadHandler(
    filename = function() paste0("group_allocations_", Sys.Date(), ".json"),
    content = function(f) writeLines(scenarios_to_json(unname(rv$scenarios)), f))

  sc_table <- reactive({
    if (!length(rv$scenarios))
      return(data.frame(name = character(0), method = character(0),
                        detail = character(0), strata = character(0),
                        saved = character(0)))
    data.frame(
      name = names(rv$scenarios),
      method = vapply(rv$scenarios, `[[`, character(1), "method"),
      strata = vapply(rv$scenarios, function(x)
        paste(x$dims, collapse = " x "), character(1)),
      detail = vapply(rv$scenarios, function(x)
        if (x$method == "auto")
          paste0(paste(x$hierarchy, collapse = " > "), "  @ ",
                 paste(x$thresholds, collapse = "/"), "%",
                 if (!is.null(x$self_threshold) && !is.na(x$self_threshold))
                   paste0(" (self ", x$self_threshold, "%)") else "")
        else paste(length(x$rules), "groups"), character(1)),
      saved = vapply(rv$scenarios, `[[`, character(1), "saved"),
      stringsAsFactors = FALSE) })

  output$sc_tbl <- renderDT(datatable(sc_table(), rownames = FALSE,
                                      options = list(dom = "t", scrollX = TRUE)))

  runs <- reactive({
    req(length(rv$scenarios) >= 1)
    setNames(lapply(rv$scenarios, run_scenario, catch_data = dat()),
             names(rv$scenarios)) })

  cmp_keys <- reactive({
    k <- common_keys(runs())
    intersect(k, c("fleetValue","metier4","metier5","metier6","Country",
                   "areaValue","Season")) })

  output$cmp_note <- renderUI({
    validate(need(length(rv$scenarios) >= 1,
                  "Save at least one group allocation first."))
    # Was: paste(x$fleet_level, x$area_level) - a level-of-detail concept
    # the strata-definition box replaced. Both fields stopped being set
    # once the fleet/area pickers they came from were removed, so x$...
    # was always NULL and paste(NULL, NULL) is character(0) - a length-0
    # result fed into vapply's length-1 contract, which is exactly the
    # "FUN(X[[1]]) result is length 0" crash this was.
    # dims already carries the same information, more completely (it is
    # not limited to exactly one fleet column and one area column), and
    # was already being saved on every scenario correctly.
    if (length(unique(vapply(rv$scenarios, function(x)
      paste(sort(x$dims), collapse = ","), character(1)))) > 1)
      div(class = "note", style = "color:#9a5000",
          "Allocations use different strata definitions, so only the keys ",
          "they share can be compared.") else NULL })

  # max-only: a single handle at the ceiling, blue from 0 up to it. Only
  # an upper bound on discard rate is meaningful - you exclude a donor for
  # having too HIGH a rate, never too low. A single-value slider paints
  # the left (kept) side; the handle IS the max.
  output$rule_dr_ui <- renderUI({
    top <- dr_top("mg")
    sliderInput("rule_dr", NULL, 0, top, top, 0.01) })

  rule_dr <- reactive({
    v <- input$rule_dr; if (is.null(v)) return(c(NA, NA))
    top <- dr_top("mg")
    # no minimum bound; the handle at the ceiling means "no maximum either"
    c(NA, if (as.numeric(v)[1] < top) as.numeric(v)[1] else NA) })

  # the comparison chips speak in DIM labels (Fleet, Area, ...); the runs
  # are keyed by the resolved columns. Translate through the level actually
  # used, falling back to the canonical default for each dimension.
  cmp_col <- function(dim_label) {
    if (is.null(dim_label) || !nzchar(dim_label)) return(NULL)
    # Resolve through the level of detail actually in force rather than a
    # hardcoded default: with the boxes now defaulting to fleetValue /
    # areaValue, a fixed "metier4" here named a column the runs were
    # never keyed by, and the chip silently selected nothing.
    # chips now carry column names directly
    col <- dim_label
    if (col %in% cmp_keys()) col else NULL
  }

  cmp <- reactive({
    x <- cmp_col(input$cmp_x)
    req(!is.null(x))
    keys <- unique(c(x, cmp_col(input$cmp_facet)))
    compare_scenarios(runs(), keys) })

  cmp_sig <- reactive(list(input$cmp_x, input$cmp_facet,
                           input$cmp_fleet, input$cmp_area))
  cmp_g <- gate_of("cmp", cmp_sig, refresh = reactive(rv$scenarios))

  cmp_plot_obj <- gated(cmp_g, {
    x <- cmp_col(input$cmp_x); req(!is.null(x))
    plot_comparison(cmp(), x, cmp_col(input$cmp_facet)) })

  output$cmp_plot <- renderPlot({
    validate(need(length(rv$scenarios) >= 1, "Save or load at least one group allocation."),
             need(isTruthy(input$cmp_x), "Pick an X axis from the chips."))
    p <- cmp_plot_obj()
    validate(need(!is.null(p), "Nothing to plot."))
    p + theme(legend.position = "none") })

  output$cmp_legend <- renderPlot({
    validate(need(length(rv$scenarios) >= 1, ""), need(isTruthy(input$cmp_x), ""))
    p <- cmp_plot_obj(); validate(need(!is.null(p), ""))
    draw_legend(p) }, bg = "transparent")

  keep_plot("cmp_plot", cmp_plot_obj, gate = cmp_g,
    section = "output",
    title = "Allocation comparison",
    note = function() sprintf("across %s saved allocation(s)",
                              length(rv$scenarios)),
    prep = function() c(
      "## compare_scenarios() needs the raised runs; re-run the allocations",
      "## in model_allocate_raise_discards.R and collect them into `runs`",
      sprintf("cmp <- compare_scenarios(runs, %s)",
              fmt_vals(unique(c(cmp_col(input$cmp_x),
                                cmp_col(input$cmp_facet)))))),
    call = function() taf_call("plot_comparison", list(
      "cmp", taf_q(cmp_col(input$cmp_x)), taf_q(cmp_col(input$cmp_facet)))))

  output$cmp_tbl <- renderDT({
    validate(need(length(rv$scenarios) >= 1, "Save or load at least one group allocation."))
    x <- cmp_col(input$cmp_x); validate(need(!is.null(x), "Pick an X axis."))
    keys <- unique(c(x, cmp_col(input$cmp_facet)))
    datatable(comparison_table(cmp(), keys),
              options = list(scrollX = TRUE, pageLength = 15), rownames = FALSE) })

  # One description of where the data came from, so the header, the live
  # script and the TAF set cannot disagree about it. rv$cef_files is set
  # by whichever CEF route ran; the InterCatch routes leave it NULL.
  cef_src <- reactive({
    f <- rv$cef_files
    list(catches = f$catches %||% "catch_data.csv",
         dir     = f$dir %||% NA_character_,
         dist    = f$distributions %||% NA_character_,
         eff     = f$effort %||% NA_character_)
  })

  # --- 9. code ---------------------------------------------
  code_lines <- reactive({
    out <- character(0)
    if ("head" %in% input$code_parts)
      out <- c(out, emit_header(cef_src()$catches, input$stock, nrow(dat())))
    if ("derive" %in% input$code_parts)
      out <- c(out,
        emit_load_code(cef_src()$catches, input$sep, dir = cef_src()$dir,
                       dist_file = cef_src()$dist, eff_file = cef_src()$eff),
        c(emit_area_helper(), emit_derive_code()),
        emit_filter_code(input$stock, input$year, ar_dims(),
                         ar_dr()[1], ar_dr()[2]))
    if ("auto" %in% input$code_parts)
      out <- c(out, {
        b <- ar_excl_keys_srv$bars()
        emit_hier_code(rec(), hier_cols(), hier_thr(),
                       "fine_to_coarse", ar_dims(),
                       self_threshold = self_thr_val(),
                       donor_cov_min = b$cov_min, donor_meas_min = b$meas_min,
                       donor_excluded = b$named) })
    if ("manual" %in% input$code_parts && length(rv$rules) >= 1)
      out <- c(out, emit_rules_code(rv$rules, rv$sets,
                                    mg_dims()))
    out })

  # Fragments and the live script are the same text; the checkboxes only
  # choose which blocks are shown, never a different rendering.
  output$code_txt <- renderText({
    keep <- input$code_parts %||% LOG_ORDER
    map <- c(head = "header", derive = "load_cef", auto = "auto_raising",
             manual = "auto_raising")
    ids <- unique(c("header", "setup", unname(map[keep]), "load_cef",
                    "strata", "explore", "auto_raising", "session"))
    lg <- rv$log; lg$blocks <- lg$blocks[intersect(names(lg$blocks), ids)]
    paste(log_render(lg), collapse = "\n") })

  # =========================================================
  #  SECTION 3 - group allocations box
  # =========================================================

  # group allocations, listed in a small box like the level-of-detail one
  output$alloc_box <- renderUI({
    n <- length(rv$scenarios)
    div(class = "allocbox",
      div(class = "alloctitle", sprintf("Group allocations (%s)", n)),
      if (!n) div(class = "note", "None saved yet.")
      else tagList(lapply(names(rv$scenarios), function(k) {
        s <- rv$scenarios[[k]]
        div(style = "font-size:11px;margin:3px 0;padding:3px 5px;
                     border-left:4px solid #59A14F;background:#fff;
                     border-radius:3px",
            tags$b(k), tags$br(),
            tags$span(style = "color:#555",
              if (s$method == "auto")
                sprintf("auto | %s | %s%%", paste(s$hierarchy, collapse = ">"),
                        paste(s$thresholds, collapse = "/"))
              else sprintf("manual | %s groups", length(s$rules))))
      }))) })

  # the plots actually on screen, as code
  active_plots <- reactive({
    r <- function(v) if (is.null(v) || !nzchar(v)) "NULL" else sprintf('"%s"', v)
    list(
      sprintf('plot_discard_rate(strata, type = "raw", x = %s, colour = %s, shape = %s, facet = %s)',
              r(role("dr","x")), r(role("dr","fill")),
              r(role("dr","shape")), r(role("dr","facet"))),
      sprintf('plot_discard_rate(strata, type = "box", x = %s, fill = %s, facet = %s)',
              r(col_of("dr", input$box_x)), r(col_of("dr", input$box_fill)),
              r(col_of("dr", input$box_facet))),
      sprintf('plot_coverage(classify_strata(strata, %s), type = "heatmap", y = %s, fill = %s, facet = %s, threshold = %s)',
              input$threshold %||% 50, r(col_of("hm", input$hm_y)),
              r(col_of("hm", input$hm_x)), r(col_of("hm", input$hm_facet)),
              input$threshold %||% 50),
      sprintf('plot_coverage(classify_strata(strata, %s), type = "bars", y = %s, fill = %s, facet = %s)',
              input$threshold %||% 50, r(col_of("cb", input$cb_y)),
              r(col_of("cb", input$cb_fill)), r(col_of("cb", input$cb_facet)))) })

  # =========================================================
  #  Merged coverage plot (heatmap / bubbles / bars share one menu)
  # =========================================================
  cov_sc <- reactive({
    req(isTruthy(input$cov_y))
    classify_strata(strata_roles("cov", c(input$cov_y, input$cov_fill,
                                          input$cov_facet)), input$threshold,
                    min_trips = input$cov_trips %||% 2) })

  # A heatmap or a bubble grid needs two axes. With X / Fill on (none)
  # there is nothing to put on the x axis, so those two icons are greyed
  # out and only Bars stays clickable.
  cov_type_off <- reactive(
    if (isTruthy(input$cov_fill)) character(0) else c("heatmap", "bubbles"))

  # the type actually drawn: falls back to bars when the chosen type needs
  # an x axis that is no longer set
  cov_type_eff <- reactive({
    ty <- input$cov_type %||% "heatmap"
    if (ty %in% cov_type_off()) "bars" else ty })

  output$cov_type_bar <- renderUI(
    pick_bar("cov_type",
             list(heatmap = "Heatmap", bubbles = "Bubbles", bars = "Bars"),
             cov_type_eff(), disabled = cov_type_off()))

  # build once; the main panel shows it with no legend, the legend is drawn
  # separately beside the plot-type icons
  # input$cov_type is deliberately absent from the signature: switching
  # heatmap/bubbles/bars is a different view of the same configuration, not
  # a reconfiguration, so it goes through as `live` and redraws at once.
  # flt_inputs() belongs in here. Without it a filter change never altered
  # the signature, so the panel never went stale and Apply re-froze the
  # same value - the figure could not redraw, because as far as the gate
  # was concerned nothing had been reconfigured. The biology panels had
  # it from the start; these two did not.
  cov_sig <- reactive(list(input$cov_y, input$cov_fill, input$cov_facet,
                           input$threshold, input$cov_trips, input$cov_fleet,
                           input$cov_area, flt_inputs("cov")))
  cov_live <- reactive(input$cov_type)
  cov_g <- gate_of("cov", cov_sig, live = cov_live)

  cov_plot_obj <- gated(cov_g, {
    y <- col_of("cov", input$cov_y); f <- col_of("cov", input$cov_fill)
    fc <- col_of("cov", input$cov_facet)
    plot_coverage(cov_sc(), type = cov_type_eff(), y = y, fill = f,
                  facet = fc, threshold = input$threshold, what = "discard",
                  min_trips = input$cov_trips %||% 2) },
    live = cov_live)

  output$cov_plot_ui <- renderUI({
    cov_g$now(); cov_live()
    plotOutput("cov_plot",
               height = paste0(isolate(
                 hgt("cov", input$cov_facet, 16,
                     # bars put the fleet on y; heatmap/bubbles use the
                     # Y AXIS picker, which is the same control
                     y_sel = input$cov_y)), "px")) })

  output$cov_plot <- renderPlot({
    p <- cov_plot_obj()
    validate(need(!is.null(p), no_plot_msg("cov")))
    p + theme(legend.position = "none") })

  output$cov_legend <- renderPlot({
    p <- cov_plot_obj(); validate(need(!is.null(p), ""))
    draw_legend(p) }, bg = "transparent")

  keep_plot("cov_plot", cov_plot_obj, gate = cov_g,
    section = "model_explore_discards",
    title = function() sprintf("Discard coverage - %s", cov_type_eff()),
    note = function() sprintf("threshold %s%%, min %s trips; fleet = %s, area = %s",
      input$threshold %||% 50, input$cov_trips %||% 2,
      paste(panel_dims("cov"), collapse = ", "), ""),
    # strata_discards comes off disk from the model stage, but it was built
    # at that stage's level of detail; re-deriving it here keeps the plot
    # honest when the coverage tab was looking at a different resolution
    prep = function() c(
      sprintf("strata_cov <- classify_strata(build_strata(catch_data, %s), %s, min_trips = %s)",
              fmt_vals(panel_dims("cov")), input$threshold %||% 50,
              input$cov_trips %||% 2)),
    call = function() taf_call("plot_coverage", list(
      "strata_cov", type = taf_q(cov_type_eff()),
      y = taf_q(col_of("cov", input$cov_y)),
      fill = taf_q(col_of("cov", input$cov_fill)),
      facet = taf_q(col_of("cov", input$cov_facet)),
      threshold = input$threshold %||% 50, what = '"discard"',
      min_trips = input$cov_trips %||% 2)))

  # merged discard-rate plot
  # same for the raw/boxplot toggle
  dr_sig <- reactive(list(input$dr_x, input$dr_fill,
                          input$dr_shape, input$dr_facet, input$dr_jitter,
                          input$box_x, input$box_fill, input$box_facet,
                          input$box_points, input$box_pooled,
                          input$dr_fleet, input$dr_area,
                          flt_inputs("dr")))
  dr_live <- reactive(input$dr_type)
  dr_g <- gate_of("dr", dr_sig, live = dr_live)

  dr_plot_obj <- gated(dr_g, {
    if (identical(input$dr_type, "box")) {
      req(isTruthy(input$box_x))
      plot_discard_rate(strata_x("dr"), type = "box",
                  x = col_of("dr", input$box_x),
                  fill = col_of("dr", input$box_fill),
                  facet = col_of("dr", input$box_facet),
                  show_points = input$box_points, show_pooled = input$box_pooled)
    } else
      plot_discard_rate(strata_x("dr"), type = "raw",
                  x = role("dr","x"), colour = role("dr","fill"),
                  shape = role("dr","shape"), facet = role("dr","facet"),
                  jitter = input$dr_jitter) },
    live = dr_live)

  output$dr_plot_ui <- renderUI({
    dr_g$now(); dr_live()
    plotOutput("dr_plot", height = paste0(isolate(
      hgt("dr", if (identical(input$dr_type, "box")) input$box_facet
                else input$dr_facet,
          y_sel = if (identical(input$dr_type, "box")) input$box_x
                  else input$dr_x)), "px")) })

  output$dr_plot <- renderPlot({
    p <- dr_plot_obj()
    validate(need(!is.null(p), no_plot_msg("dr", "No strata with discard data.")))
    p + theme(legend.position = "none") })

  output$dr_legend <- renderPlot({
    p <- dr_plot_obj(); validate(need(!is.null(p), ""))
    draw_legend(p) }, bg = "transparent")

  keep_plot("dr_plot", dr_plot_obj, gate = dr_g,
    section = "model_explore_discards",
    title = function() sprintf("Discard rate - %s",
      if (identical(input$dr_type, "box")) "boxplot" else "per stratum"),
    note = function() sprintf("fleet = %s, area = %s",
      paste(panel_dims("dr"), collapse = ", "), ""),
    prep = function()
      sprintf("strata_dr <- build_strata(catch_data, %s)",
              fmt_vals(panel_dims("dr"))),
    call = function() if (identical(input$dr_type, "box"))
      taf_call("plot_discard_rate", list(
        "strata_dr", type = '"box"', x = taf_q(col_of("dr", input$box_x)),
        fill = taf_q(col_of("dr", input$box_fill)),
        facet = taf_q(col_of("dr", input$box_facet)),
        show_points = if (isTRUE(input$box_points)) "TRUE" else "FALSE",
        show_pooled = if (isTRUE(input$box_pooled)) "TRUE" else "FALSE"))
    else taf_call("plot_discard_rate", list(
        "strata_dr", type = '"raw"', x = taf_q(role("dr", "x")),
        colour = taf_q(role("dr", "fill")), shape = taf_q(role("dr", "shape")),
        facet = taf_q(role("dr", "facet")),
        jitter = if (isTRUE(input$dr_jitter)) "TRUE" else "FALSE")))

  # =========================================================
  #  CEF database call (framework only)
  #  db_fetch_cef() is the single seam where the real database call
  #  will go. Everything around it - credentials, the request form, the
  #  busy overlay, and the Result view - is wired, so only this function
  #  body needs filling in. When implemented it should return a list with
  #  $catches / $distributions / $effort, which flow into the same slots
  #  the CEF folder loader fills.
  # =========================================================
  db_fetch_cef <- function(user, pass, stock, year, tables) {
    stop("The CEF database call is not implemented yet.")
  }

  observeEvent(input$db_connect, {
    rv$db_auth <- list(user = input$db_user %||% "",
                       ok = nzchar(input$db_user %||% ""))
    showNotification(
      if (isTRUE(rv$db_auth$ok)) "Credentials captured." else "Enter a username.",
      type = if (isTRUE(rv$db_auth$ok)) "message" else "warning", duration = 3) })

  output$db_cred_status <- renderUI({
    a <- rv$db_auth
    if (is.null(a))
      return(div(class = "note",
                 "Enter your database username and password, then Connect."))
    div(class = "chain",
        style = if (isTRUE(a$ok)) "color:#14532d" else "color:#9a5000",
        if (isTRUE(a$ok))
          sprintf("Connected as '%s'. Move on to the Request tab.", a$user)
        else "No username entered yet.") })

  do_db_load <- function() {
    tables <- c(if (isTRUE(input$db_cn)) "CN",
                if (isTRUE(input$db_dn)) "DN",
                if (isTRUE(input$db_en)) "EN")
    if (!length(tables)) {
      showNotification("Tick at least one CEF table (CN, DN or EN).",
                       type = "warning", duration = 5); return() }
    r <- with_busy("Requesting data from the CEF database...",
      tryCatch(db_fetch_cef(input$db_user, input$db_pass, input$db_stock,
                            input$db_year, tables),
               error = function(e) e))
    if (inherits(r, "error")) {
      rv$db_err <- conditionMessage(r)
      showNotification(paste("Request failed:", conditionMessage(r)),
                       type = "error", duration = 10)
    } else {
      rv$cef_catches <- r$catches; rv$dist <- r$distributions
      rv$eff <- r$effort; rv$db_err <- NULL
      rv$cef_files <- NULL
      showNotification("Data received from the CEF database.",
                       type = "message", duration = 4)
    }
    invisible(NULL)
  }

  observeEvent(input$db_request, with_load_guard("rdbes", do_db_load))

  output$db_request_status <- renderUI({
    if (!is.null(rv$db_err))
      return(div(class = "chain", style = "color:#7a1f1f",
                 tags$b("Request failed:"), tags$br(), rv$db_err,
                 tags$br(), tags$br(),
                 tags$span(class = "note",
                   "The database call itself is not built yet - this is the ",
                   "framework around it. Fill in db_fetch_cef() to enable it.")))
    div(class = "note",
        "Set the stock, year and tables, then Request. Results appear on the ",
        "Result tab and feed the rest of the workbench.") })

  # =========================================================
  #  CEF result views (same code for both entry points)
  # =========================================================
  cef_tables <- function(src) {
    if (src == "ic") list(catches = rv$ic$catches,
                          distributions = rv$ic$distributions,
                          effort = rv$ic$effort)
    else list(catches = tryCatch(dat(), error = function(e) NULL),
              distributions = rv$dist, effort = rv$eff)
  }

  local({
    # "db" reuses the same tables slots as the CEF folder loader, so the
    # else branch of cef_tables() covers it once the fetch is implemented
    for (src in c("ic", "cef", "db")) local({
      sc <- src
      ns <- function(x) paste0(sc, "_", x)
      dt <- function(d, ...) datatable(d, rownames = FALSE,
        options = list(scrollX = TRUE, dom = "t", pageLength = 25), ...)

      output[[ns("cat_sum")]] <- renderDT({
        t <- cef_tables(sc)$catches
        validate(need(!is.null(t) && nrow(t), "No catches table yet."))
        dt(catches_summary(t), colnames = c("", "")) })
      output[[ns("cat_cat")]] <- renderDT({
        t <- cef_tables(sc)$catches
        validate(need(!is.null(t) && nrow(t), "No catches table yet."))
        dt(catches_by_category(t)) })
      output[[ns("cat_prev")]] <- renderDT({
        t <- cef_tables(sc)$catches
        validate(need(!is.null(t) && nrow(t), "No catches table yet."))
        datatable(head(t, 200), rownames = FALSE,
                  options = list(scrollX = TRUE, pageLength = 8)) })

      output[[ns("dist_sum")]] <- renderDT({
        t <- cef_tables(sc)
        validate(need(!is.null(t$distributions) && nrow(t$distributions),
                      "No distributions table loaded."))
        dt(distributions_summary(t$distributions, t$catches), colnames = c("","")) })
      output[[ns("dist_type")]] <- renderDT({
        t <- cef_tables(sc)$distributions
        validate(need(!is.null(t) && nrow(t), "No distributions table loaded."))
        dt(distributions_by_type(t)) })
      output[[ns("bio_dim_ui")]] <- renderUI(
        selectInput(ns("bio_dim"), "Group by", c(NONE, gvars()), "metier4"))
      output[[ns("bio_cov")]] <- renderDT({
        t <- cef_tables(sc)
        validate(need(!is.null(t$catches) && nrow(t$catches), "No catches table."))
        d <- derive_fields(as.data.frame(t$catches))
        dim1 <- input[[ns("bio_dim")]] %||% "metier4"
        b <- biology_coverage(d, t$distributions, dim1) %>%
          mutate(across(where(is.numeric), ~round(.x, 3)))
        datatable(b, rownames = FALSE,
                  options = list(scrollX = TRUE, pageLength = 10)) })
      output[[ns("dist_prev")]] <- renderDT({
        t <- cef_tables(sc)$distributions
        validate(need(!is.null(t) && nrow(t), "No distributions table loaded."))
        datatable(head(t, 200), rownames = FALSE,
                  options = list(scrollX = TRUE, pageLength = 8)) })

      output[[ns("eff_sum")]] <- renderDT({
        t <- cef_tables(sc)$effort
        validate(need(!is.null(t) && nrow(t), "No effort table loaded."))
        dt(effort_summary(t), colnames = c("", "")) })
      output[[ns("eff_prev")]] <- renderDT({
        t <- cef_tables(sc)$effort
        validate(need(!is.null(t) && nrow(t), "No effort table loaded."))
        datatable(head(t, 200), rownames = FALSE,
                  options = list(scrollX = TRUE, pageLength = 8)) })
    })
  })

  # extra CEF uploads
  output$cef_dist_input <- renderUI({
    needBio <- any(c("lengths", "ages") %in% cfg$need)
    tagList(
      fileInput("file_dist",
                if (needBio) "distributions" else "distributions (optional)",
                accept = c(".csv", ".txt")),
      if (!needBio)
        div(class = "note",
            "Not required for this run, but you can still load it to explore ",
            "the length or age sampling.") else NULL) })

  rv_read <- function(f) if (is.null(f)) NULL else
    read.csv(f$datapath, sep = input$sep %||% ",", stringsAsFactors = FALSE)

  # Uploading CEF tables by hand is the CEF route by another door, so it
  # claims ownership the same way the folder loader does - and, since this
  # release, at the same moment: on a button, not on a file-chooser.
  #
  # All three files are read in ONE action. The old pair of bare
  # observe()s on file_dist / file_eff wrote straight into rv$dist and
  # rv$eff with no guard at all, so an InterCatch conversion could have
  # its distributions swapped underneath it by a stray upload.
  do_cef_upload <- function() {
    if (is.null(input$file)) {
      showNotification("Choose a catches file first.", type = "error")
      return(invisible(NULL))
    }
    set_busy(TRUE, "Reading the uploaded CEF tables...")
    on.exit(set_busy(FALSE), add = TRUE)
    rv$cef_catches <- read.csv(input$file$datapath, sep = input$sep %||% ",",
                               stringsAsFactors = FALSE)
    rv$dist <- rv_read(input$file_dist)
    rv$eff  <- rv_read(input$file_eff)
    # The uploads land in a per-session temp directory whose name means
    # nothing to a reader, so the script gets the names the user chose.
    rv$cef_files <- list(
      dir = NA_character_, catches = input$file$name,
      distributions = if (is.null(input$file_dist)) NA_character_
                      else input$file_dist$name,
      effort = if (is.null(input$file_eff)) NA_character_
               else input$file_eff$name)
    showNotification(sprintf("Loaded catches%s%s",
                             if (!is.null(input$file_dist)) ", distributions" else "",
                             if (!is.null(input$file_eff)) ", effort" else ""),
                     type = "message", duration = 5)
    invisible(NULL)
  }

  observeEvent(input$cef_load_upl, with_load_guard("cef", do_cef_upload))

  output$cef_file_check <- renderUI({
    row <- function(lbl, ok, req) div(style = sprintf("color:%s;font-size:12px",
      if (!req) "#8a8a8a" else if (ok) "#14532d" else "#7a1f1f"),
      HTML(sprintf("%s %s", if (!req) "&#8211;" else if (ok) "&#10003;" else
        "&#10007;", lbl)))
    needBio <- any(c("lengths","ages") %in% cfg$need)
    div(class = "chain",
        row("catches", !is.null(rv$cef_catches) || !is.null(rv$ic$catches), TRUE),
        row("distributions", !is.null(rv$dist), needBio),
        row("effort (optional)", !is.null(rv$eff), FALSE)) })

  # =========================================================
  #  Final raising
  # =========================================================
  observe(updateSelectInput(session, "final_alloc", choices = names(rv$scenarios)))

  observeEvent(input$run_final, {
    req(isTruthy(input$final_alloc))
    sc <- rv$scenarios[[input$final_alloc]]
    conds <- if (sc$method == "auto") {
      st <- flag_donor_exclusion(build_strata(dat(), sc$dims),
                                 sc$dr_min %||% NA, sc$dr_max %||% NA)
      conds_from_auto(recommend_raising(st, sc$hierarchy, sc$thresholds,
        self_threshold = sc$self_threshold %||% NA,
        mode = sc$mode %||% "nested", dims = sc$dims))
    } else conds_from_rules(sc$rules, sc$sets %||% list(), sc$dims)
    r <- with_busy(sprintf("Raising with '%s'...", input$final_alloc),
      tryCatch(run_native_raising(dat(), sc$dims, conds,
      type = input$nr_type %||% "discards",
      originType = input$nr_origin %||% "WGEstimate",
      variableType = input$nr_vartype %||% "WeightLive",
      log_file = if (nzchar(input$nr_log %||% "")) input$nr_log else NULL),
      error = function(e) e))
    if (inherits(r, "error")) {
      rv$final <- NULL
      showNotification(paste("Raising failed:", conditionMessage(r)),
                       type = "error", duration = 10)
    } else {
      rv$final <- r
      showNotification(sprintf("Raised with '%s': %s rows",
                               input$final_alloc, nrow(r)), duration = 4)
    } })

  output$final_status <- renderUI({
    if (!is.null(rv$final))
      return(div(class = "chain", style = "color:#14532d",
        sprintf("Adopted: %s | %s rows | this is the allocation the script records as final.",
                input$final_alloc, format(nrow(rv$final), big.mark = ","))))
    # Nothing run yet - say why it might not work before the user finds
    # out from a failed-raising notification instead. This is the
    # pre-flight check nr_status used to give; final_ui() is now the only
    # place "Raise with this allocation" is triggered from, so this is
    # where a reader needs to see it.
    if (!EXT_STATUS$raising)
      return(div(class = "chain", style = "color:#7a1f1f",
        tags$b("catchRaising() is not loaded."),
        " Point the external script folder at 0_Functions_discards_raising.R ",
        "and 0_Functions_other.R first."))
    ck <- cef_check(tryCatch(dat(), error = function(e) NULL))
    if (!is.null(ck) && !ck$ok)
      return(div(class = "chain", style = "color:#9a5000",
        tags$b("The CEF table is missing columns catchRaising() needs: "),
        paste(ck$missing, collapse = ", ")))
    div(class = "note", "Pick an allocation and run the raising.") })

  fin_d <- reactive({ req(!is.null(rv$final)); derive_fields(rv$final) })

  output$fin_plot_ui <- renderUI(plotOutput("fin_plot", height = paste0(
    plot_height(if (is.null(rv$final)) NULL else fin_d(),
                col_of("nr", input$fin_facet), 12), "px")))

  fin_sig <- reactive(list(input$fin_x, input$fin_facet,
                           input$nr_fleet, input$nr_area))
  fin_g <- gate_of("fin", fin_sig, refresh = reactive(rv$final))

  fin_plot_obj <- gated(fin_g, {
    validate(need(!is.null(rv$final), "Run the raising first."))
    plot_raised_composition(fin_d(), col_of("nr", input$fin_x),
                            col_of("nr", input$fin_facet)) })

  output$fin_plot <- renderPlot({
    p <- fin_plot_obj()
    validate(need(!is.null(p), "Nothing to plot.")); p })

  keep_plot("fin_plot", fin_plot_obj, gate = fin_g,
    section = "output",
    title = "Raised catch composition",
    note = function() sprintf("adopted allocation: %s",
                              input$final_alloc %||% "(none)"),
    prep = "d_fin <- derive_fields(catch_data_raised)",
    call = function() taf_call("plot_raised_composition", list(
      "d_fin", taf_q(col_of("nr", input$fin_x)),
      taf_q(col_of("nr", input$fin_facet)))))

  output$fin_overview <- renderDT({
    validate(need(!is.null(rv$final), "Run the raising first."))
    datatable(raised_overview_table(rv$final), rownames = FALSE,
              options = list(dom = "t", scrollX = TRUE)) })

  # =========================================================
  #  The analysis, as code
  # =========================================================
  # The grouping and raising code is wanted in two places at once: the
  # running log on the Fragments tab, and model_allocate_raise_discards.R
  # in the TAF set. Building it once and reading it from both is what stops
  # the two drifting into telling different stories about the same session.
  raise_code <- reactive(tryCatch({
      out <- character(0)
      emitted <- character(0)

      # Only SAVED allocations are written. The on-screen configuration
      # used to be emitted too, as live_auto / live_manual, so that an
      # unsaved exploration was still reproducible. In practice it meant
      # every script opened with a full grouping and raising block that
      # was usually a near-duplicate of the first saved one, and a reader
      # could not tell which of the two had produced the numbers. Saving
      # is one click; guessing which block is authoritative is not.
      #
      # The single exception is below: with nothing saved at all, the
      # script would have no analysis in it, so the current auto cascade
      # is written and labelled as unsaved.

      # (2) each SAVED allocation
      for (nm in names(rv$scenarios)) {
        sc <- rv$scenarios[[nm]]
        out <- c(out,
          sprintf("## ===== Saved allocation '%s' =====", nm),
          if (sc$method == "auto")
            code_auto_rule(nm, sc$dims, sc$hierarchy, sc$thresholds,
                           sc$self_threshold %||% NA, sc$mode %||% "nested",
                           sc$dr_min %||% NA, sc$dr_max %||% NA)
          else code_manual_rule(nm, sc$dims, sc$rules, sc$sets %||% list()),
          code_raise(nm, sc$dims, input$nr_type %||% "discards",
                     input$nr_origin %||% "WGEstimate",
                     input$nr_vartype %||% "WeightLive",
                     input$nr_log %||% "Log.txt"))
        emitted <- c(emitted, nm)
      }

      # Nothing saved: emit the live cascade rather than an empty script,
      # clearly marked so it is never mistaken for a saved decision.
      if (!length(emitted)) {
        live_auto <- tryCatch(
          code_auto_rule("unsaved_auto", ar_dims(),
                         hier_cols(), hier_thr(), self_thr_val(),
                         "nested",
                         ar_dr()[1], ar_dr()[2]),
          error = function(e) NULL)
        if (!is.null(live_auto)) {
          out <- c(out,
            "## ===== UNSAVED - the cascade as configured on screen =====",
            "## Nothing was saved in this session, so this is the current",
            "## configuration, not a decision. Save it in the app to make",
            "## it a named allocation.",
            live_auto,
            code_raise("unsaved_auto", ar_dims(),
                       input$nr_type %||% "discards",
                       input$nr_origin %||% "WGEstimate",
                       input$nr_vartype %||% "WeightLive",
                       input$nr_log %||% "Log.txt"))
          emitted <- c(emitted, "unsaved_auto")
        }
      }

      # (3) and (4) are returned SEPARATELY rather than glued onto `out`,
      # because the TAF split puts them in the same file but the running
      # log wants them in a fixed order, and appending them here as well
      # would print each of them twice.
      final <- if (isTruthy(input$final_alloc)) input$final_alloc
               else if (length(emitted)) emitted[1] else NULL

      # The length/age allocation used to be tacked onto the end of this
      # block. It now has its own script (model_allocate_length_age.R),
      # because it runs on the RAISED catches this block produces and that
      # dependency is worth making visible.
      list(groups = out, emitted = emitted,
           compare = if (length(emitted) > 1) code_compare(emitted) else NULL,
           final = if (!is.null(final)) code_final(final) else NULL)
    }, error = function(e)
      list(groups = c("## the raising code could not be generated:",
                      paste0("##   ", conditionMessage(e))),
           emitted = character(0), compare = NULL, final = NULL)))

  # The length/age allocations, per store. Emitted per saved allocation so
  # a session that tried three groupings leaves all three in the script,
  # with the aggregation that followed each.
  bio_alloc_code <- function(store, type, tag, pfx, obj) {
    allocs <- rv[[store]] %||% list()
    if (!length(allocs)) return(NULL)
    unlist(lapply(names(allocs), function(nm) c(
      sprintf("## --- allocation '%s' (%s) ---", nm,
              allocs[[nm]]$method %||% "hierarchical"),
      # recipe, then the conditions it resolves to - the same block the
      # Code tab shows, so the script and the screen cannot diverge
      emit_alloc_recipe_code(allocs[[nm]]$spec, allocs[[nm]]$groups, tag,
                             allocs[[nm]]$method %||% "hierarchical"),
      "",
      # conditions already printed by the recipe block above
      emit_alloc_code(allocs[[nm]]$groups, type,
                      input[[paste0(pfx, "_rz_origin")]] %||% "WGEstimate",
                      input[[paste0(pfx, "_rz_vartype")]] %||% "WeightLive",
                      with_conditions = FALSE),
      "",
      emit_alloc_agg_code(tag),
      "",
      sprintf("%s <- alloc_res", obj),
      sprintf('write.csv(numbers_at, "model/numbers_at_%s.csv", row.names = FALSE)',
              tolower(tag)),
      "")), use.names = FALSE)
  }

  # The two sufficiency thresholds of a biology module, addressed the same
  # way the module itself addresses them.
  bio_thr <- function(p, which, default)
    input[[paste0(p, "_thr_", which)]] %||% default

  # The load block for data.R - ONE route, the one that owns the data.
  # Emitting all four with three commented out would leave the reader
  # guessing which one ran, and a stock whose provenance is a guess is a
  # stock whose numbers cannot be defended.
  taf_load_lines <- reactive({
    m <- rv$load_method
    sep_txt <- switch(input$sep %||% ",", "," = '","', ";" = '";"',
                      "\t" = '"\\t"', '","')
    if (is.null(m))
      return(c("## No input route has been completed yet. Load the data in the",
               "## workbench and this block will describe the route you used.",
               ""))
    if (m %in% c("ic_exchange", "ic_output")) {
      i <- tryCatch(ic_inputs(), error = function(e) NULL)
      if (is.null(i)) return(c("## InterCatch conversion settings unavailable.", ""))
      return(c(
        sprintf("## ---- Input route: %s ----", TAF_METHOD_LABEL[[m]]),
        "## The conversion IS the data step: the CEF tables this workflow",
        "## runs on do not exist until it has run, so it belongs here and",
        "## not in a preparatory script somebody has to remember to run.",
        ic_code(i),
        "distributions <- res$distributions",
        "effort        <- res$effort",
        ""))
    }
    if (identical(m, "rdbes")) {
      return(c(
        sprintf("## ---- Input route: %s ----", TAF_METHOD_LABEL[[m]]),
        "## The workbench holds the framework for this call; the fetch",
        "## itself is not implemented yet. Replace db_fetch_cef() with your",
        "## own query and the rest of the set runs unchanged.",
        sprintf('cef <- db_fetch_cef(stock = "%s", year = %s,',
                input$db_stock %||% (cfg$stock %||% ""),
                input$db_year %||% (cfg$year %||% "NA")),
        sprintf("                    tables = %s)",
                fmt_vals(c(if (isTRUE(input$db_cn)) "CN",
                           if (isTRUE(input$db_dn)) "DN",
                           if (isTRUE(input$db_en)) "EN"))),
        "catch_data    <- cef$catches",
        "distributions <- cef$distributions",
        "effort        <- cef$effort",
        ""))
    }
    # CEF files: name the files that were actually read, rather than making
    # the reader re-run the app's name matching on their own machine
    f <- rv$cef_files %||% list(dir = ".", catches = "catches.csv",
                                distributions = NA_character_,
                                effort = NA_character_)
    rd <- function(obj, nm, required) {
      if (is.na(nm %||% NA))
        return(sprintf("## %s: no file was matched in the session", obj))
      sprintf('%s <- read.csv(file.path(cefPath, "%s"), sep = %s, stringsAsFactors = FALSE)',
              obj, nm, sep_txt)
    }
    c(sprintf("## ---- Input route: %s ----", TAF_METHOD_LABEL[["cef"]]),
      "## All three CEF tables, read as they were read in the session.",
      sprintf('cefPath <- "%s"', f$dir %||% "."),
      rd("catch_data", f$catches, TRUE),
      rd("distributions", f$distributions, FALSE),
      rd("effort", f$effort, FALSE),
      "")
  })

  # Everything the TAF emitters need, in one place. Assembled defensively:
  # a half-configured session should still produce a readable script set
  # rather than a blank tab.
  taf_ctx <- reactive({
    rc <- raise_code()
    tc <- function(x) tryCatch(x, error = function(e) NULL)
    list(
      external_dir = EXT$dir,
      load_method = rv$load_method,
      input_desc = {
        f <- rv$cef_files
        if (!is.null(f) && !is.null(f$catches))
          file.path(f$dir %||% ".", f$catches)
        else TAF_METHOD_LABEL[[rv$load_method %||% "cef"]] %||% "the input files"
      },
      # one route, and derive_fields() from the package rather than a
      # hand-rolled copy of the area splitter written into the script
      load_lines = tc(taf_load_lines()),
      stock = cfg$stock %||% input$stock, year = cfg$year %||% input$year,
      dims_disc   = tc(ar_dims()),
      threshold   = input$threshold %||% 50,
      dr_min = tc(ar_dr()[1]), dr_max = tc(ar_dr()[2]),
      group_lines = rc$groups, compare_lines = rc$compare,
      final_lines = rc$final,
      # Was panel_dims("len_lv") - a "level of detail" panel that never
      # existed (no len_lv_y / len_lv_x inputs anywhere), so this always
      # fell through to a hardcoded fallback regardless of what the user
      # had actually configured. len_alloc$dims_of() / age_alloc$dims_of()
      # are the SAME reactives length_alloc_server() itself allocates
      # from - length and age run independently, so each needs its own,
      # not one shared value silently reused for both.
      dims_len = tc(len_alloc$dims_of()),
      dims_age = tc(age_alloc$dims_of()),
      # read the way the modules write them - threshold_box() is called as
      # threshold_box(paste0(p, "_thr_cov"), ...), so the id only exists as
      # a constructed string. Spelling it out literally here worked, but it
      # made two halves of one id that must agree look unrelated, and the
      # structural audit could no longer match them up.
      len_threshold = bio_thr("len", "cov", 50),
      len_min_meas  = bio_thr("len", "meas", 0),
      age_threshold = bio_thr("age", "cov", 50),
      age_min_meas  = bio_thr("age", "meas", 0),
      len_alloc_lines = tc(bio_alloc_code("len_allocs", "LengthTotal",
                                          "Length", "len", "alloc_length")),
      age_alloc_lines = tc(bio_alloc_code("age_allocs", "Age",
                                          "Age", "age", "alloc_age")))
  })

  # The seven scripts, rebuilt whenever anything they depend on moves.
  taf_files <- reactive(taf_bundle(taf_ctx(), rv$kept))

  # =========================================================
  #  Running R script + session state
  # =========================================================
  # There is no single script file any more, so the only path the session
  # still needs is the one the state JSON sits at, beside the seven scripts.
  state_path <- reactive(file.path(input$log_dir %||% ".",
                                   "session_state.json"))

  # every block re-derives itself, so the script always matches the app
  observe({
    lg <- log_new()
    lg <- log_set(lg, "header", code_header("the TAF script set"))
    lg <- log_set(lg, "setup", code_setup(EXT$dir))
    lg <- log_set(lg, "intercatch", tryCatch(ic_code(ic_inputs()), error = function(e) NULL))
    lg <- log_set(lg, "load_cef", tryCatch(c(
      emit_load_code(cef_src()$catches, input$sep %||% ",",
                     dir = cef_src()$dir, dist_file = cef_src()$dist,
                     eff_file = cef_src()$eff),
      emit_derive_code()), error = function(e) NULL))
    lg <- log_set(lg, "strata", tryCatch(
      emit_filter_code(input$stock, input$year, ar_dims(),
                       ar_dr()[1], ar_dr()[2]), error = function(e) NULL))
    # The script must reproduce the WHOLE analysis, not only what was
    # saved. So it always writes:
    #   1. the allocation method(s) currently on screen (live), even if
    #      nothing was saved yet - the auto cascade as a RULE, the manual
    #      groups as explicit lists;
    #   2. every saved allocation, the same way;
    #   3. the comparison across them, if more than one exists;
    #   4. which allocation was adopted as final.
    lg <- log_set(lg, "auto_raising", tryCatch({
      rc <- raise_code()
      c(rc$groups, rc$compare, rc$final,
        if (length(rv$len_allocs)) c("## ===== Length allocation =====",
          bio_alloc_code("len_allocs", "LengthTotal", "Length", "len",
                         "alloc_length")),
        if (length(rv$age_allocs)) c("## ===== Age allocation =====",
          bio_alloc_code("age_allocs", "Age", "Age", "age", "alloc_age")))
      }, error = function(e) NULL))
    lg <- log_set(lg, "manual_groups", NULL)
    lg <- log_set(lg, "native_raising", NULL)
    lg <- log_set(lg, "explore", tryCatch({
      fl <- explore_filters_all()
      c(if (length(fl))
          unlist(lapply(names(fl), function(p)
            emit_explore_filter_code(fl[[p]], to = paste0("explore_", p))),
            use.names = FALSE)
        else emit_explore_filter_code(list()),
        code_plots(active_plots()))
      }, error = function(e) NULL))
    lg <- log_set(lg, "session", code_session(state_path()))
    rv$log <- lg
  })

  # ---------------------------------------------------------
  #  The seven scripts: one view and one download each
  # ---------------------------------------------------------
  # Built in a loop rather than written out seven times: the parts are
  # already named in TAF_FILES, and hand-repeating them is exactly how a
  # tab ends up quietly rendering the wrong file.
  for (.pt in names(TAF_FILES)) local({
    pt <- .pt
    output[[paste0("taf_", pt, "_txt")]] <- renderText(
      paste(taf_files()[[pt]], collapse = "\n"))
    output[[paste0("dl_taf_", pt)]] <- downloadHandler(
      filename = function() TAF_FILES[[pt]],
      content  = function(f) writeLines(taf_files()[[pt]], f))
  })

  stock_slug <- reactive(taf_slug(cfg$stock %||% input$stock %||% "stock"))

  # the "full" view is the same seven files, concatenated in run order
  output$log_txt <- renderText(paste(taf_concat(taf_files()), collapse = "\n"))

  output$dl_taf_all <- downloadHandler(
    filename = function() sprintf("%s_taf.zip", stock_slug()),
    content  = function(f) taf_zip(taf_files(), names(TAF_FILES), f))

  output$dl_taf_sel <- downloadHandler(
    filename = function() sprintf("%s_taf_selected.zip", stock_slug()),
    content  = function(f)
      taf_zip(taf_files(), input$taf_parts %||% names(TAF_FILES), f))

  output$dl_log <- downloadHandler(
    filename = function() sprintf("%s_taf_all_in_one.R", stock_slug()),
    content  = function(f) writeLines(taf_concat(taf_files()), f))

  # Writing straight to a folder is off by default now: seven files landing
  # in the working directory on every slider move is not a courtesy.
  observe({
    req(isTRUE(input$log_auto))
    try(taf_write(taf_files(), input$log_dir %||% "."), silent = TRUE) })

  observeEvent(input$log_write_now, {
    r <- tryCatch(taf_write(taf_files(), input$log_dir %||% "."),
                  error = function(e) e)
    if (inherits(r, "error"))
      showNotification(paste("Could not write:", conditionMessage(r)),
                       type = "error", duration = 8)
    else
      showNotification(sprintf("Wrote %s scripts to %s", length(r),
                               normalizePath(input$log_dir %||% ".",
                                             mustWork = FALSE)),
                       type = "message", duration = 6) })

  # ---------------------------------------------------------
  #  The kept-plot list
  # ---------------------------------------------------------
  output$kept_list <- renderUI({
    if (!length(rv$kept))
      return(div(class = "note",
                 "Nothing kept yet. Press 'Keep for script' under any plot."))
    tagList(lapply(rv$kept, function(k)
      div(class = "keptrow",
          div(style = "flex:1",
              div(class = "ttl", k$title),
              div(class = "sub", sprintf("-> %s | kept %s%s",
                  TAF_FILES[[k$section %||% "output"]] %||% "output.R",
                  k$stamp %||% "?",
                  if (length(k$note)) paste0(" - ", k$note) else ""))),
          actionButton(paste0("kept_del_", k$uid), "remove",
                       class = "btn-xs btn-danger")))) })

  # Delete buttons are wired by UID, never by position. Wiring by position
  # is the classic version of this bug: remove the second of four and the
  # observers still point at the old indices, so the next click deletes
  # something the user did not choose.
  kept_env <- new.env(parent = emptyenv())
  observe({
    for (k in rv$kept) local({
      u  <- k$uid
      id <- paste0("kept_del_", u)
      if (!exists(id, envir = kept_env, inherits = FALSE))
        assign(id, observeEvent(input[[id]], {
          rv$kept <- Filter(function(z) !identical(z$uid, u), rv$kept)
        }, ignoreInit = TRUE), envir = kept_env)
    }) })

  observeEvent(input$kept_clear, {
    rv$kept <- list()
    showNotification("Cleared. output.R is empty again.", duration = 3) })

  output$dl_state <- downloadHandler(
    filename = function() "session_state.json",
    content = function(f) writeLines(state_to_json(state_capture(input, rv)), f))

  observeEvent(input$up_state, {
    req(input$up_state)
    st <- tryCatch(state_from_json(readLines(input$up_state$datapath, warn = FALSE)),
                   error = function(e) e)
    if (inherits(st, "error")) {
      showNotification(paste("Could not read state:", conditionMessage(st)),
                       type = "error", duration = 8); return()
    }
    if (!is.null(st$rules)) rv$rules <- st$rules
    if (!is.null(st$scenarios) && length(st$scenarios))
      rv$scenarios <- setNames(st$scenarios,
                               vapply(st$scenarios, `[[`, character(1), "name"))
    res <- state_apply(session, st)
    rv$restore <- res
    showNotification(sprintf("Restored %s settings, %s groups, %s scenarios",
                             length(res$applied), length(rv$rules),
                             length(rv$scenarios)),
                     type = "message", duration = 6) })

  output$state_status <- renderUI({
    if (is.null(rv$restore)) return(NULL)
    div(class = "note",
        sprintf("Restored %s controls.", length(rv$restore$applied)),
        if (length(rv$restore$skipped))
          tags$div(style = "color:#9a5000",
                   sprintf("Not restored: %s",
                           paste(head(rv$restore$skipped, 8), collapse = ", ")))
        else NULL) })
}

shinyApp(ui, server)
