# ============================================================
#  app_ui_helpers.R
#  SHINY-ONLY helpers. Nothing in here processes data.
# ------------------------------------------------------------
#  This is the dividing line in the codebase. Everything in the
#  0_Functions_*.R scripts is data work that runs in a plain R session and is
#  meant to become the package. Everything here needs shiny, DT or grid, exists
#  only to make the interface behave, and is NOT part of the package.
#
#  If a function in here starts doing arithmetic on catch data, it is in the
#  wrong file.
#
#  WHAT IS HERE
#    THUMB, pick_bar()          the icon strip used instead of radio buttons;
#    opt_lab()                  an inline grey description beside a control;
#    dir_picker(), _server()    a text path with a browse button;
#    plot_height(),
#    length_plot_height()       how tall a plotOutput must be for the number
#                               of rows it will draw;
#    compact_legend(),
#    extract_legend(),
#    draw_legend()              pull a plot's legend out so it can sit in its
#                               own panel beside the icons;
#    dt_group_colours()         colour a DT column by group;
#    donor_choices()            build the choices for a donor selectInput.
#
#  DEPENDENCIES  Imports: shiny, DT, grid, ggplot2 (for the legend grobs),
#  dplyr. Needs 0_Functions_theme.R and 0_Functions_plots.R loaded.
# ============================================================

# ----------------------------------------------------------
#  Icon strip and small widgets
# ----------------------------------------------------------

#' Inline SVG icons used across the interface
#'
#' A named list of small inline SVGs, one per choice the icon strips offer.
#' They are inline rather than files so the app needs no www/ directory.
#'
#' The length and age icons are deliberately near-identical twins: the same
#' fish over a measuring tape or a scale, distinguished by the tag beside it
#' ("18cm" / "3yo") and by the value under the instrument ("x15" numbers, or
#' "153 g" mean weight). The age pair swaps the tape for an otolith with growth
#' rings, with an arrow to the fish's head to say where it came from.
#'
#' @keywords internal
#' @noRd
THUMB <- list(
  # --- section-level: what stage of the work -------------------
  # magnifying glass over a small chart: exploration
  stgexpl = '<svg width="40" height="28"><rect x="3" y="16" width="4" height="9" fill="#59A14F"/><rect x="8" y="11" width="4" height="14" fill="#F28E2B"/><rect x="13" y="18" width="4" height="7" fill="#B07AA1"/><circle cx="26" cy="12" r="8" fill="none" stroke="#12324f" stroke-width="2"/><circle cx="26" cy="12" r="6" fill="#dbe6f0" fill-opacity="0.55"/><line x1="32" y1="18" x2="38" y2="25" stroke="#12324f" stroke-width="2.6" stroke-linecap="round"/></svg>',
  # a branching decision: an unknown stratum routed to a sampled one
  stggrp = '<svg width="40" height="28"><circle cx="7" cy="14" r="5.5" fill="#BFBFBF" stroke="#12324f" stroke-width="1.1"/><text x="7" y="17" font-size="8" text-anchor="middle" fill="#12324f" font-weight="bold">?</text><path d="M13 14 h6 l0 -7 h6" fill="none" stroke="#12324f" stroke-width="1.4"/><path d="M13 14 h6 l0 7 h6" fill="none" stroke="#12324f" stroke-width="1.4"/><rect x="26" y="3" width="11" height="8" rx="1.5" fill="#59A14F" stroke="#12324f" stroke-width="1"/><rect x="26" y="17" width="11" height="8" rx="1.5" fill="#F28E2B" stroke="#12324f" stroke-width="1"/></svg>',
  # a bar lifted to its raised height
  stgraise = '<svg width="40" height="28"><rect x="5" y="17" width="8" height="8" fill="#59A14F" stroke="#12324f" stroke-width="1"/><rect x="16" y="17" width="8" height="8" fill="#59A14F" stroke="#12324f" stroke-width="1"/><rect x="16" y="7" width="8" height="10" fill="#F28E2B" fill-opacity="0.75" stroke="#12324f" stroke-width="1" stroke-dasharray="2 1.5"/><rect x="27" y="17" width="8" height="8" fill="#59A14F" stroke="#12324f" stroke-width="1"/><rect x="27" y="3" width="8" height="14" fill="#F28E2B" fill-opacity="0.75" stroke="#12324f" stroke-width="1" stroke-dasharray="2 1.5"/><path d="M31 1 l-2.5 3 h5 z" fill="#12324f"/></svg>',
  # a fish above a percent-ish split: the discard-rate view
  stgdr = '<svg width="40" height="28"><path d="M4 9 C9 4, 17 4, 21 9 C17 14, 9 14, 4 9 Z" fill="#E15759" stroke="#222" stroke-width="0.7"/><path d="M21 9 l3.5 -3 v6 z" fill="#E15759" stroke="#222" stroke-width="0.7"/><circle cx="8" cy="8" r="0.9" fill="#222"/><rect x="4" y="18" width="22" height="6" rx="1" fill="#59A14F" stroke="#12324f" stroke-width="0.8"/><rect x="4" y="18" width="8" height="6" rx="1" fill="#E15759" stroke="#12324f" stroke-width="0.8"/><text x="33" y="16" font-size="10" text-anchor="middle" fill="#12324f" font-weight="bold">%</text></svg>',
  # --- data sources -------------------------------------------
  # a stack of exchange records with an arrow in
  srcic = '<svg width="40" height="28"><rect x="4" y="5" width="20" height="19" rx="2" fill="#fff" stroke="#12324f" stroke-width="1.3"/><line x1="8" y1="10" x2="20" y2="10" stroke="#59A14F" stroke-width="1.6"/><line x1="8" y1="14" x2="20" y2="14" stroke="#F28E2B" stroke-width="1.6"/><line x1="8" y1="18" x2="16" y2="18" stroke="#B07AA1" stroke-width="1.6"/><path d="M28 14 h8 m-3 -3 l3 3 l-3 3" fill="none" stroke="#12324f" stroke-width="1.6"/></svg>',
  # a single tidy table: the CEF product
  srccef = '<svg width="40" height="28"><rect x="5" y="5" width="30" height="19" rx="2" fill="#fff" stroke="#12324f" stroke-width="1.3"/><rect x="5" y="5" width="30" height="5" fill="#12324f"/><line x1="15" y1="10" x2="15" y2="24" stroke="#cfd8e0" stroke-width="1"/><line x1="25" y1="10" x2="25" y2="24" stroke="#cfd8e0" stroke-width="1"/><line x1="5" y1="15" x2="35" y2="15" stroke="#cfd8e0" stroke-width="1"/><line x1="5" y1="20" x2="35" y2="20" stroke="#cfd8e0" stroke-width="1"/></svg>',
  # a database cylinder with a network node
  srcdb = '<svg width="40" height="28"><ellipse cx="16" cy="7" rx="11" ry="4" fill="#dbe6f0" stroke="#12324f" stroke-width="1.2"/><path d="M5 7 v13 a11 4 0 0 0 22 0 v-13" fill="#dbe6f0" stroke="#12324f" stroke-width="1.2"/><ellipse cx="16" cy="14" rx="11" ry="4" fill="none" stroke="#12324f" stroke-width="0.8"/><circle cx="34" cy="20" r="3" fill="#59A14F" stroke="#12324f" stroke-width="0.9"/><line x1="27" y1="19" x2="31" y2="20" stroke="#12324f" stroke-width="1"/></svg>',
  # database sub-tabs: a padlock for credentials, a cylinder + down arrow
  # for the request
  dbcred = '<svg width="40" height="28"><rect x="12" y="12" width="16" height="12" rx="2" fill="#dbe6f0" stroke="#12324f" stroke-width="1.3"/><path d="M15 12 v-3 a5 5 0 0 1 10 0 v3" fill="none" stroke="#12324f" stroke-width="1.5"/><circle cx="20" cy="17" r="1.8" fill="#12324f"/><rect x="19.2" y="18" width="1.6" height="3.6" fill="#12324f"/></svg>',
  dbreq = '<svg width="40" height="28"><ellipse cx="13" cy="7" rx="10" ry="3.5" fill="#dbe6f0" stroke="#12324f" stroke-width="1.1"/><path d="M3 7 v11 a10 3.5 0 0 0 20 0 v-11" fill="#dbe6f0" stroke="#12324f" stroke-width="1.1"/><ellipse cx="13" cy="13" rx="10" ry="3.5" fill="none" stroke="#12324f" stroke-width="0.7"/><path d="M31 8 v10 m-4 -4 l4 4 l4 -4" fill="none" stroke="#59A14F" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/></svg>',
  # IC exchange (one combined file) vs IC output (the txt exports)
  icexch = '<svg width="40" height="28"><rect x="9" y="4" width="22" height="21" rx="2" fill="#fff" stroke="#12324f" stroke-width="1.3"/><text x="20" y="12" font-size="6" text-anchor="middle" fill="#59A14F" font-family="monospace">HI</text><text x="20" y="18" font-size="6" text-anchor="middle" fill="#F28E2B" font-family="monospace">SI</text><text x="20" y="24" font-size="6" text-anchor="middle" fill="#B07AA1" font-family="monospace">SD</text></svg>',
  icout = '<svg width="40" height="28"><rect x="3" y="6" width="13" height="17" rx="1.5" fill="#fff" stroke="#12324f" stroke-width="1.1"/><rect x="14" y="6" width="13" height="17" rx="1.5" fill="#fff" stroke="#12324f" stroke-width="1.1"/><rect x="25" y="6" width="13" height="17" rx="1.5" fill="#fff" stroke="#12324f" stroke-width="1.1"/><line x1="6" y1="11" x2="13" y2="11" stroke="#59A14F" stroke-width="1.3"/><line x1="17" y1="11" x2="24" y2="11" stroke="#F28E2B" stroke-width="1.3"/><line x1="28" y1="11" x2="35" y2="11" stroke="#B07AA1" stroke-width="1.3"/></svg>',
  # input / output / result within a source
  stepin  = '<svg width="40" height="28"><path d="M4 14 h16 m-5 -5 l5 5 l-5 5" fill="none" stroke="#12324f" stroke-width="1.8"/><rect x="24" y="6" width="13" height="17" rx="2" fill="#dbe6f0" stroke="#12324f" stroke-width="1.2"/></svg>',
  stepout = '<svg width="40" height="28"><rect x="3" y="6" width="13" height="17" rx="2" fill="#dbe6f0" stroke="#12324f" stroke-width="1.2"/><path d="M20 14 h16 m-5 -5 l5 5 l-5 5" fill="none" stroke="#12324f" stroke-width="1.8"/></svg>',
  stepres = '<svg width="40" height="28"><rect x="5" y="5" width="30" height="19" rx="2" fill="#fff" stroke="#12324f" stroke-width="1.2"/><path d="M10 15 l4 4 l8 -9" fill="none" stroke="#59A14F" stroke-width="2.4"/></svg>',
  # --- catch category: what the fish did next -----------------
  # a fish going back over the side, with the splash
  catdis = '<svg width="40" height="28"><path d="M2 21 q5 -3 10 0 q5 3 10 0 q5 -3 10 0 q4 2 6 1" fill="none" stroke="#4292C6" stroke-width="1.5"/><path d="M2 25 q6 -3 12 0 q6 3 12 0 q5 -2 10 1" fill="none" stroke="#4292C6" stroke-width="1.1" opacity="0.55"/><path d="M20 3 C25 8, 25 16, 20 21 C15 16, 15 8, 20 3 Z" fill="#E15759" stroke="#222" stroke-width="0.8"/><path d="M20 3 l-3 -3 h6 z" fill="#E15759" stroke="#222" stroke-width="0.8"/><circle cx="20" cy="17" r="1.1" fill="#222"/><path d="M13 19 l-2 3 m18 -3 l2 3" stroke="#4292C6" stroke-width="1.2" stroke-linecap="round"/></svg>',
  # a fish with a price tag: landed and sold
  # the fish is the subject of this icon, so it is drawn at the icon's full
  # height and centred on y = 14 (the middle of the 28px box) rather than
  # tucked into the top-left corner as a small mark
  catlan = '<svg width="40" height="28"><path d="M1 14 C5.5 7.5, 13.5 7.5, 16.5 14 C13.5 20.5, 5.5 20.5, 1 14 Z" fill="#59A14F" stroke="#222" stroke-width="0.9"/><path d="M16.5 14 l3.2 -4 v8 z" fill="#59A14F" stroke="#222" stroke-width="0.9"/><circle cx="4.8" cy="12.6" r="1.35" fill="#222"/><rect x="25" y="4" width="14" height="9" rx="1.2" fill="#8FBF7F" stroke="#1d5c2a" stroke-width="0.9"/><circle cx="32" cy="8.5" r="2.6" fill="none" stroke="#1d5c2a" stroke-width="0.9"/><path d="M32 6.4 v4.2" stroke="#1d5c2a" stroke-width="0.9"/><path d="M26.5 5.5 v6 M37.5 5.5 v6" stroke="#1d5c2a" stroke-width="0.6"/><ellipse cx="32" cy="24.5" rx="5" ry="2" fill="#EDC948" stroke="#1d5c2a" stroke-width="0.8"/><ellipse cx="32" cy="21.5" rx="5" ry="2" fill="#EDC948" stroke="#1d5c2a" stroke-width="0.8"/><ellipse cx="32" cy="18.5" rx="5" ry="2" fill="#F6DE7A" stroke="#1d5c2a" stroke-width="0.8"/><path d="M20.5 20 h3.5 m-1.6 -1.6 l1.6 1.6 l-1.6 1.6" fill="none" stroke="#1d5c2a" stroke-width="1.1" stroke-linecap="round"/></svg>',
  # a stack of smoothed curves: the ridge / density view
  ridge = '<svg width="40" height="28"><path d="M2 10 q6 -8 11 0 q5 6 11 -3 q6 -6 14 3" fill="#59A14F" fill-opacity="0.35" stroke="#59A14F" stroke-width="1.2"/><path d="M2 17 q7 -7 12 0 q6 5 10 -2 q6 -5 14 2" fill="#F28E2B" fill-opacity="0.35" stroke="#F28E2B" stroke-width="1.2"/><path d="M2 24 q6 -6 12 0 q5 4 11 -2 q6 -4 13 2" fill="#B07AA1" fill-opacity="0.35" stroke="#B07AA1" stroke-width="1.2"/></svg>',
  # --- length section: what is being measured -----------------
  # A fish over a measuring tape, tagged with a length, versus a fish on
  # a scale. The little "18 cm" / "3 yo" tag is what separates the
  # length icons from the identical-looking age ones.
  lengthvar = '<svg width="40" height="28"><path d="M2 8 C6 4, 13 4, 16 8 C13 12, 6 12, 2 8 Z" fill="#59A14F" stroke="#222" stroke-width="0.8"/><path d="M16 8 l3.5 -2.5 v5 z" fill="#59A14F" stroke="#222" stroke-width="0.8"/><circle cx="5" cy="7" r="0.9" fill="#222"/><text x="31" y="10" font-size="7" text-anchor="middle" fill="#12324f" font-weight="bold">18cm</text><rect x="2" y="15" width="22" height="4.5" fill="#F5E6A8" stroke="#222" stroke-width="0.7"/><line x1="6" y1="15" x2="6" y2="17.5" stroke="#222" stroke-width="0.7"/><line x1="10" y1="15" x2="10" y2="19.5" stroke="#222" stroke-width="0.7"/><line x1="14" y1="15" x2="14" y2="17.5" stroke="#222" stroke-width="0.7"/><line x1="18" y1="15" x2="18" y2="19.5" stroke="#222" stroke-width="0.7"/><line x1="22" y1="15" x2="22" y2="17.5" stroke="#222" stroke-width="0.7"/><text x="13" y="26.5" font-size="7.5" text-anchor="middle" fill="#12324f" font-weight="bold">x15</text></svg>',
  weightvar = '<svg width="40" height="28"><path d="M2 7 C6 3, 13 3, 16 7 C13 11, 6 11, 2 7 Z" fill="#B07AA1" stroke="#222" stroke-width="0.8"/><path d="M16 7 l3.5 -2.5 v5 z" fill="#B07AA1" stroke="#222" stroke-width="0.8"/><circle cx="5" cy="6" r="0.9" fill="#222"/><text x="31" y="9" font-size="7" text-anchor="middle" fill="#12324f" font-weight="bold">18cm</text><rect x="3" y="13.5" width="19" height="2" fill="#888" stroke="#222" stroke-width="0.5"/><line x1="12.5" y1="11.5" x2="12.5" y2="13.5" stroke="#222" stroke-width="0.9"/><path d="M6 15.5 h13 l2 4.5 h-17 z" fill="#EDC948" stroke="#222" stroke-width="0.8"/><text x="12" y="26.5" font-size="7" text-anchor="middle" fill="#12324f" font-weight="bold">153 g</text></svg>',
  # the age twins: same fish, tagged with an age instead
  agevar = '<svg width="40" height="28"><path d="M5 6 C8 3, 13 3, 16 6 C13 9, 8 9, 5 6 Z" fill="#59A14F" stroke="#222" stroke-width="0.7"/><path d="M5 6 l-3 -2 v4 z" fill="#59A14F" stroke="#222" stroke-width="0.7"/><circle cx="13.5" cy="5.5" r="0.8" fill="#222"/><path d="M22 11.5 L18.4 7.8" stroke="#12324f" stroke-width="1.2" stroke-linecap="round"/><path d="M17.4 6.7 l3.1 1.1 l-1.5 1.6 z" fill="#12324f"/><ellipse cx="27" cy="15" rx="8" ry="6.5" fill="#F3E2C0" stroke="#7A5C2E" stroke-width="1"/><ellipse cx="27" cy="15" rx="5.92" ry="4.81" fill="none" stroke="#7A5C2E" stroke-width="0.7"/><ellipse cx="27" cy="15" rx="4.00" ry="3.25" fill="none" stroke="#7A5C2E" stroke-width="0.7"/><ellipse cx="27" cy="15" rx="2.16" ry="1.76" fill="none" stroke="#7A5C2E" stroke-width="0.7"/><circle cx="27" cy="15" r="0.9" fill="#7A5C2E"/><text x="4" y="17.5" font-size="7" text-anchor="start" fill="#12324f" font-weight="bold">3yo</text><text x="14" y="26.5" font-size="7.5" text-anchor="middle" fill="#12324f" font-weight="bold">x15</text></svg>',
  ageweightvar = '<svg width="40" height="28"><path d="M5 5.5 C8 2.5, 13 2.5, 16 5.5 C13 8.5, 8 8.5, 5 5.5 Z" fill="#B07AA1" stroke="#222" stroke-width="0.7"/><path d="M5 5.5 l-3 -2 v4 z" fill="#B07AA1" stroke="#222" stroke-width="0.7"/><circle cx="13.5" cy="5" r="0.8" fill="#222"/><path d="M23 6 L18.4 5.4" stroke="#12324f" stroke-width="1.1" stroke-linecap="round"/><path d="M17.4 4.6 l3 0.8 l-0.6 1.9 z" fill="#12324f"/><ellipse cx="30" cy="7" rx="6.5" ry="5.2" fill="#F3E2C0" stroke="#7A5C2E" stroke-width="1"/><ellipse cx="30" cy="7" rx="4.81" ry="3.85" fill="none" stroke="#7A5C2E" stroke-width="0.7"/><ellipse cx="30" cy="7" rx="3.25" ry="2.60" fill="none" stroke="#7A5C2E" stroke-width="0.7"/><ellipse cx="30" cy="7" rx="1.76" ry="1.40" fill="none" stroke="#7A5C2E" stroke-width="0.7"/><circle cx="30" cy="7" r="0.9" fill="#7A5C2E"/><text x="30" y="17.5" font-size="6.5" text-anchor="middle" fill="#12324f" font-weight="bold">3yo</text><rect x="3" y="14" width="17" height="2" fill="#888" stroke="#222" stroke-width="0.5"/><line x1="11.5" y1="12" x2="11.5" y2="14" stroke="#222" stroke-width="0.9"/><path d="M5.5 16 h11.5 l1.8 4.5 h-15 z" fill="#EDC948" stroke="#222" stroke-width="0.8"/><text x="11" y="26.5" font-size="7" text-anchor="middle" fill="#12324f" font-weight="bold">153 g</text></svg>',
  # --- length section: plot type ------------------------------
  hist = '<svg width="40" height="28"><rect x="3" y="20" width="5" height="6" fill="#59A14F" stroke="#222" stroke-width="0.5"/><rect x="9" y="13" width="5" height="13" fill="#59A14F" stroke="#222" stroke-width="0.5"/><rect x="15" y="5" width="5" height="21" fill="#F28E2B" stroke="#222" stroke-width="0.5"/><rect x="21" y="9" width="5" height="17" fill="#F28E2B" stroke="#222" stroke-width="0.5"/><rect x="27" y="16" width="5" height="10" fill="#B07AA1" stroke="#222" stroke-width="0.5"/><rect x="33" y="22" width="5" height="4" fill="#B07AA1" stroke="#222" stroke-width="0.5"/></svg>',
  linedot = '<svg width="40" height="28"><polyline points="4,23 12,17 20,12 28,8 36,5" fill="none" stroke="#59A14F" stroke-width="1.8"/><polyline points="4,25 12,21 20,18 28,15 36,13" fill="none" stroke="#F28E2B" stroke-width="1.8"/><circle cx="4" cy="23" r="2.3" fill="#59A14F" stroke="#222" stroke-width="0.6"/><circle cx="20" cy="12" r="2.3" fill="#59A14F" stroke="#222" stroke-width="0.6"/><circle cx="36" cy="5" r="2.3" fill="#59A14F" stroke="#222" stroke-width="0.6"/><circle cx="12" cy="21" r="2.3" fill="#F28E2B" stroke="#222" stroke-width="0.6"/><circle cx="28" cy="15" r="2.3" fill="#F28E2B" stroke="#222" stroke-width="0.6"/></svg>',
  # --- discard section ----------------------------------------
  heatmap = '<svg width="40" height="28"><rect x="1" y="1" width="12" height="8" fill="#41AE76"/><rect x="14" y="1" width="12" height="8" fill="#FFD92F"/><rect x="27" y="1" width="12" height="8" fill="#E66101"/><rect x="1" y="10" width="12" height="8" fill="#FFD92F"/><rect x="14" y="10" width="12" height="8" fill="#00441B"/><rect x="27" y="10" width="12" height="8" fill="#41AE76"/><rect x="1" y="19" width="12" height="8" fill="#4292C6"/><rect x="14" y="19" width="12" height="8" fill="#E66101"/><rect x="27" y="19" width="12" height="8" fill="#FFD92F"/></svg>',
  bubbles = '<svg width="40" height="28"><circle cx="8" cy="8" r="6" fill="#41AE76" stroke="#222"/><circle cx="24" cy="6" r="4" fill="#FFD92F" stroke="#222"/><circle cx="34" cy="14" r="7" fill="#E66101" stroke="#222"/><circle cx="12" cy="21" r="5" fill="#4292C6" stroke="#222"/></svg>',
  bars = '<svg width="40" height="28"><rect x="2" y="4" width="26" height="5" fill="#4E79A7" stroke="#222" stroke-width="0.6"/><rect x="2" y="11" width="34" height="5" fill="#F28E2B" stroke="#222" stroke-width="0.6"/><rect x="2" y="18" width="16" height="5" fill="#59A14F" stroke="#222" stroke-width="0.6"/></svg>',
  raw = '<svg width="40" height="28"><circle cx="8" cy="20" r="3" fill="#4E79A7" stroke="#222"/><circle cx="16" cy="10" r="3" fill="#F28E2B" stroke="#222"/><circle cx="24" cy="16" r="3" fill="#59A14F" stroke="#222"/><circle cx="32" cy="7" r="3" fill="#E15759" stroke="#222"/></svg>',
  box = '<svg width="40" height="28"><rect x="5" y="8" width="10" height="12" fill="#4E79A7" stroke="#222"/><line x1="10" y1="3" x2="10" y2="8" stroke="#222"/><line x1="10" y1="20" x2="10" y2="25" stroke="#222"/><rect x="24" y="5" width="10" height="14" fill="#F28E2B" stroke="#222"/><line x1="29" y1="2" x2="29" y2="5" stroke="#222"/><line x1="29" y1="19" x2="29" y2="24" stroke="#222"/></svg>',
  ladder = '<svg width="40" height="28"><rect x="2" y="2" width="34" height="5" fill="#00441B"/><rect x="6" y="9" width="28" height="5" fill="#41AE76"/><rect x="10" y="16" width="22" height="5" fill="#FFD92F"/><rect x="14" y="23" width="16" height="4" fill="#BFBFBF"/></svg>',
  manual = '<svg width="40" height="28"><rect x="2" y="3" width="16" height="7" fill="#4E79A7" stroke="#222" stroke-width="0.6"/><rect x="22" y="3" width="16" height="7" fill="#F28E2B" stroke="#222" stroke-width="0.6"/><rect x="2" y="13" width="16" height="7" fill="#59A14F" stroke="#222" stroke-width="0.6"/><rect x="22" y="13" width="16" height="7" fill="#E15759" stroke="#222" stroke-width="0.6"/><line x1="18" y1="6" x2="22" y2="6" stroke="#222"/><line x1="18" y1="16" x2="22" y2="16" stroke="#222"/></svg>',
  compare = '<svg width="40" height="28"><rect x="3" y="12" width="7" height="14" fill="#4E79A7" stroke="#222" stroke-width="0.5"/><rect x="3" y="6" width="7" height="6" fill="#4E79A7" opacity="0.45" stroke="#222" stroke-width="0.5"/><rect x="13" y="15" width="7" height="11" fill="#F28E2B" stroke="#222" stroke-width="0.5"/><rect x="13" y="8" width="7" height="7" fill="#F28E2B" opacity="0.45" stroke="#222" stroke-width="0.5"/><rect x="23" y="10" width="7" height="16" fill="#59A14F" stroke="#222" stroke-width="0.5"/><rect x="23" y="4" width="7" height="6" fill="#59A14F" opacity="0.45" stroke="#222" stroke-width="0.5"/></svg>',
  final = '<svg width="40" height="28"><rect x="4" y="8" width="14" height="18" fill="#4E79A7" stroke="#222" stroke-width="0.6"/><rect x="4" y="2" width="14" height="6" fill="#4E79A7" opacity="0.45" stroke="#222" stroke-width="0.6"/><path d="M24 16 l4 5 l8 -12" stroke="#59A14F" stroke-width="3.4" fill="none"/></svg>')

#' Icon strip acting as a radio group
#'
#' Renders `choices` as clickable icons instead of radio buttons, so a choice
#' is recognised by its picture rather than read.
#'
#' @param id Input id.
#' @param choices Named vector: names are labels, values match [THUMB] keys.
#' @param selected The value selected initially.
#' @param disabled Values to show greyed out and unclickable.
#' @return A shiny tag.
#' @keywords internal
#' @noRd
pick_bar <- function(id, choices, selected, disabled = character(0)) {
  # The highlight is toggled in the browser the instant a thumbnail is
  # clicked: pickOn() clears .on from the siblings in the same .picker
  # and sets it on the clicked one. Doing it server-side would need the
  # whole bar to re-render, which it never did - which is why the outline
  # used to stay stuck on the initial choice.
  # `disabled` names the choices that do not apply to the current context;
  # they render greyed out and are not clickable (.pick.disabled).
  div(class = "picker", `data-pick-id` = id,
    lapply(names(choices), function(k) {
      off <- k %in% disabled
      tags$div(class = paste("pick",
                             if (identical(k, selected)) "on" else "",
                             if (off) "disabled" else ""),
        `data-val` = k,
        onclick = if (off) NULL else sprintf(
          "pickOn(this);Shiny.setInputValue('%s','%s',{priority:'event'})", id, k),
        HTML(THUMB[[k]]), tags$div(choices[[k]]))
    }))
}

#' Grey inline description beside a control
#'
#' @param name The control's name.
#' @param desc The description shown after it.
#' @return An HTML string.
#' @keywords internal
#' @noRd
opt_lab <- function(name, desc) HTML(sprintf(
  paste0('%s<span style="color:#8a8a8a;font-size:10px;text-transform:uppercase;',
         'letter-spacing:.3px;margin-left:6px">%s</span>'),
  name, desc))

#' Text path input with a browse button
#'
#' Damian's note was that finding a folder outside the project was
#' painful, and that the file-upload dialog is easier. It is - but a
#' browser will not hand back an absolute path from that dialog, for
#' security reasons that are not going away, and an absolute path is
#' exactly what the emitted script needs in order to run on someone
#' else's machine. A native picker would buy convenience here and cost
#' reproducibility there.
#'
#' So the explorer stays server-side, where the real filesystem is
#' visible, and the navigability problem is fixed directly: shortcuts to
#' the roots and the home/working directories, a clickable breadcrumb, a
#' list of recently used folders, and a preview of the data files in the
#' folder so it can be confirmed before loading rather than after.
#'
#' @param id Input id.
#' @param value Initial path.
#' @param pattern Files to preview, as a regular expression.
#' @return A shiny tag.
#' @seealso [dir_picker_server()]
#' @keywords internal
#' @noRd
dir_picker <- function(id, value = ".", pattern = "\\.(csv|txt)$") {
  tagList(
    fluidRow(
      column(9, textInput(id, NULL, value, width = "100%")),
      column(3, actionButton(paste0(id, "_browse"), NULL, icon = icon("folder-open"),
                             class = "btn-default", style = "margin-top:0;width:100%",
                             title = "Browse folders"))),
    uiOutput(paste0(id, "_browser")))
}

#' Server half of the directory picker
#'
#' @param id Input id used by [dir_picker()].
#' @param input,output,session The module's shiny objects.
#' @param start Folder to open on first use.
#' @param pattern Files to preview in each folder.
#' @param ... Further arguments.
#' @return Called for its side effects.
#' @seealso [dir_picker()]
#' @keywords internal
#' @noRd
dir_picker_server <- function(id, input, output, session,
                              start = getOption("rcef.root", getwd()),
                              pattern = "\\.(csv|txt)$", ...) {
  open_flag <- reactiveVal(FALSE)
  # Recent folders are per session and shared across every picker, so a
  # path found once on the CEF tab is one click away on the config tab.
  observeEvent(input[[paste0(id, "_browse")]], open_flag(!open_flag()))

  observeEvent(input[[paste0(id, "_cd")]], {
    updateTextInput(session, id, value = input[[paste0(id, "_cd")]])
  }, ignoreInit = TRUE)

  observeEvent(input[[paste0(id, "_pick")]], {
    pth <- input[[paste0(id, "_pick")]]
    updateTextInput(session, id, value = pth)
    dir_picker_recent(pth)
    open_flag(FALSE)
  }, ignoreInit = TRUE)

  output[[paste0(id, "_browser")]] <- renderUI({
    if (!isTRUE(open_flag())) return(NULL)
    cur <- input[[id]]
    if (is.null(cur) || !nzchar(cur) || !dir.exists(cur)) cur <- start
    cur <- normalizePath(cur, mustWork = FALSE)
    subs <- tryCatch(list.dirs(cur, recursive = FALSE, full.names = TRUE),
                     error = function(e) character(0))
    files <- tryCatch(list.files(cur, pattern = pattern, ignore.case = TRUE),
                      error = function(e) character(0))
    parent <- dirname(cur)

    go <- function(path) sprintf(
      "Shiny.setInputValue('%s_cd', %s, {priority:'event'})",
      id, jsonlite::toJSON(path, auto_unbox = TRUE))
    cd_btn <- function(path, label, ic, cls = "btn-xs btn-default") tags$button(
      class = paste("btn", cls), style = "margin:1px;text-align:left",
      icon(ic), label, onclick = go(path))

    # A breadcrumb, so getting from a deep folder back to a sibling near
    # the top is one click instead of six presses of "up one level".
    crumbs <- dir_picker_crumbs(cur)
    quick <- dir_picker_shortcuts()
    recent <- setdiff(dir_picker_recent(), cur)

    div(style = "border:1px solid #cfd8e0;border-radius:6px;padding:8px;
                 margin-top:6px;background:#fbfcfd",
      # --- shortcuts: roots, home, working directory ---
      div(style = "margin-bottom:5px",
        tags$span(class = "note", style = "display:inline;margin-right:4px",
                  "Go to:"),
        lapply(seq_along(quick), function(i)
          cd_btn(quick[[i]], names(quick)[i], "hdd-o", "btn-xs btn-info"))),
      if (length(recent))
        div(style = "margin-bottom:5px",
          tags$span(class = "note", style = "display:inline;margin-right:4px",
                    "Recent:"),
          lapply(recent, function(r)
            cd_btn(r, basename(r), "clock-o", "btn-xs btn-default"))),
      # --- breadcrumb ---
      div(style = "font-family:monospace;font-size:11px;margin-bottom:4px;
                   padding-bottom:4px;border-bottom:1px solid #e2e8ee",
        lapply(seq_along(crumbs), function(i) tagList(
          tags$a(href = "#", style = "color:#12324f", names(crumbs)[i],
                 onclick = paste0(go(crumbs[[i]]), "; return false;")),
          if (i < length(crumbs)) tags$span(style = "color:#aaa", " / ")))),
      div(style = "display:flex;gap:10px",
        # --- folders ---
        div(style = "flex:1;max-height:210px;overflow:auto",
          if (parent != cur) div(cd_btn(parent, ".. (up one level)", "level-up-alt")),
          if (length(subs))
            div(lapply(subs, function(x) div(cd_btn(x, basename(x), "folder"))))
          else div(class = "note", "no subfolders")),
        # --- what is actually in it ---
        # Without this the folder was chosen blind and the mismatch only
        # showed up as "No catches file found in that folder" after the
        # load button, one screen away from the control that caused it.
        div(style = "flex:1;max-height:210px;overflow:auto;border-left:1px solid #e2e8ee;padding-left:8px",
          tags$b(style = "font-size:11px;color:#5b7186",
                 sprintf("Data files here (%d)", length(files))),
          if (length(files))
            div(style = "font-family:monospace;font-size:10.5px;color:#39536b",
                lapply(utils::head(files, 40), function(f) div(f)),
                if (length(files) > 40)
                  div(class = "note", sprintf("... and %d more",
                                              length(files) - 40)))
          else div(class = "note", "none - this is probably not the folder"))),
      tags$button(class = "btn btn-sm btn-success", style = "margin-top:6px",
        icon("check"), "Use this folder",
        onclick = sprintf("Shiny.setInputValue('%s_pick', %s, {priority:'event'})",
                          id, jsonlite::toJSON(cur, auto_unbox = TRUE))))
  })
}

# Where a person plausibly wants to jump to. On Windows the drive letters
# are the whole point of the request - a data folder on a network share is
# not reachable by walking down from the project directory.
#' @keywords internal
#' @noRd
dir_picker_shortcuts <- function() {
  out <- list()
  wd <- tryCatch(normalizePath(getwd(), mustWork = FALSE), error = function(e) NULL)
  hm <- tryCatch(normalizePath("~", mustWork = FALSE), error = function(e) NULL)
  if (!is.null(wd)) out[["Project"]] <- wd
  if (!is.null(hm) && !identical(hm, wd)) out[["Home"]] <- hm
  if (.Platform$OS.type == "windows") {
    drives <- paste0(LETTERS, ":/")
    ok <- drives[vapply(drives, function(d)
      isTRUE(tryCatch(dir.exists(d), error = function(e) FALSE)), logical(1))]
    for (d in ok) out[[sub("/$", "", d)]] <- d
  } else out[["/"]] <- "/"
  out
}

# The path split into cumulative prefixes, for the breadcrumb.
#' @keywords internal
#' @noRd
dir_picker_crumbs <- function(path) {
  path <- gsub("\\\\", "/", path)
  # The root is whatever cannot be walked above, and it is not the same
  # thing on every platform:
  #   //server/share  a UNC share - the first TWO components are the root
  #                   together, and "/server" alone is not a place
  #   C:/             a drive root; the bare "C:" is NOT it, that means
  #                   the current directory on C:
  #   /               the unix root
  # Getting this wrong gives a breadcrumb whose top entries lead nowhere.
  if (grepl("^//[^/]", path)) {
    rest <- sub("^//", "", path)
    parts <- strsplit(rest, "/", fixed = TRUE)[[1]]
    parts <- parts[nzchar(parts)]
    if (length(parts) < 2) return(stats::setNames(list(path), path))
    root <- paste0("//", parts[1], "/", parts[2])
    out <- stats::setNames(list(root), root)
    acc <- root
    for (p in parts[-(1:2)]) { acc <- paste0(acc, "/", p); out[[p]] <- acc }
    return(out)
  }
  parts <- strsplit(path, "/", fixed = TRUE)[[1]]
  parts <- parts[nzchar(parts)]
  if (grepl("^[A-Za-z]:", path) && length(parts)) {
    root <- paste0(parts[1], "/")
    out <- stats::setNames(list(root), root)
    acc <- parts[1]
    for (p in parts[-1]) { acc <- paste0(acc, "/", p); out[[p]] <- acc }
    return(out)
  }
  out <- list(); acc <- ""
  if (startsWith(path, "/")) { out[["/"]] <- "/"; acc <- "" }
  for (p in parts) {
    acc <- if (startsWith(path, "/") || nzchar(acc)) paste0(acc, "/", p) else p
    out[[p]] <- acc
  }
  if (!length(out)) out[["."]] <- path
  out
}

# Session-scoped list of folders that were actually chosen.
#' @keywords internal
#' @noRd
dir_picker_recent <- local({
  seen <- character(0)
  function(add = NULL) {
    if (!is.null(add) && nzchar(add))
      seen <<- utils::head(unique(c(add, seen)), 6)
    seen
  }
})

# ----------------------------------------------------------
#  How tall a plot panel must be
# ----------------------------------------------------------

#' Pixel height for a discard plot panel
#'
#' A plot with many rows or facets needs a taller panel, or the rows compress
#' until the labels are unreadable. Returns a height in pixels from the number
#' of facet rows and y levels.
#'
#' @param df The data to be plotted.
#' @param facet_var Facet column, if any.
#' @param n_y Number of y-axis levels.
#' @param ncol Facet columns.
#' @param base,per_row,per_y Fixed height, height per facet row, height per y level.
#' @return A height in pixels.
#' @seealso [length_plot_height()]
#' @keywords internal
#' @noRd
plot_height <- function(df, facet_var = NULL, n_y = 10, ncol = 2,
                        base = 300, per_row = 190, per_y = 17) {
  rows <- 1
  if (!is.null(facet_var) && nzchar(facet_var) && !is.null(df))
    rows <- ceiling(length(unique(as.character(df[[facet_var]]))) / ncol)
  max(340, round(base + rows * per_row + n_y * per_y))
}

#' Pixel height for a length or age plot panel
#'
#' The [plot_height()] geometry for the biology plots, which may facet on two
#' variables. Uses the same base and per-row sizes, so a length panel opens at
#' the size its discard equivalent does.
#'
#' @param d The data to be plotted.
#' @param facet1,facet2 The facet columns, if any.
#' @param ncol Facet columns.
#' @param base,per_row,n_y,per_y As in [plot_height()].
#' @return A height in pixels.
#' @seealso [plot_height()]
#' @keywords internal
#' @noRd
length_plot_height <- function(d, facet1 = NULL, facet2 = NULL, ncol = 2,
                               base = 300, per_row = 190, n_y = 10,
                               per_y = 17) {
  has <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
  n1 <- if (has(facet1) && !is.null(d) && facet1 %in% names(d))
    length(unique(as.character(d[[facet1]]))) else 1
  n2 <- if (has(facet2) && !is.null(d) && facet2 %in% names(d))
    length(unique(as.character(d[[facet2]]))) else 1
  rows <- if (has(facet1) && has(facet2)) n1          # facet_grid: rows = f1
          else ceiling(max(n1, n2) / ncol)            # facet_wrap, 2 columns
  # same geometry as plot_height() for the discard plots, so a length or age
  # panel opens at the size its discard equivalent does instead of a third of
  # it, and grows with the number of y rows (heatmap / bubble rows, ridges)
  max(340, round(base + max(1, rows) * per_row + n_y * per_y))
}

# ----------------------------------------------------------
#  Pulling a legend into its own panel
# ----------------------------------------------------------

#' Force a plot's legend into a few compact rows
#'
#' Applied before the legend is extracted, so a legend with many levels lays
#' out across the top strip instead of growing to the height of the plot.
#'
#' @param p A ggplot object.
#' @param max_rows Most rows the legend may use.
#' @param key Glyph the discrete keys are drawn with, `"point"` or `"tile"`.
#'   With `"tile"` no shape override is applied, so the rect key the geom
#'   supplies survives and the legend matches a heatmap.
#' @param keep_size Keep a size guide instead of dropping it. Off by default:
#'   for most plots the size aesthetic is decoration, but a bubble plot
#'   encodes weight in it and the guide is the only way to read it.
#' @return `p` with its guides and legend theme rewritten.
#' @seealso [extract_legend()]
#' @keywords internal
#' @noRd
compact_legend <- function(p, max_rows = 3, key = c("point", "tile"),
                           keep_size = FALSE) {
  key <- match.arg(key)
  # count the discrete levels the plot will show, to choose a row count
  n_levels <- tryCatch({
    b <- ggplot_build(p)
    max(vapply(b$plot$scales$scales, function(s)
      if (!is.null(s$get_breaks)) length(stats::na.omit(s$get_breaks())) else 0L,
      integer(1)), 1L)
  }, error = function(e) 6L)
  # laid out horizontally across the top strip: at most `max_rows` rows,
  # so the number of columns is levels / rows (rounded up)
  nrow_use <- min(max_rows, n_levels)
  # For a discrete colour/shape scale, the point key defaults to an open
  # shape that ignores fill - so the swatch shows no colour. Forcing a
  # filled shape (21) and mapping the fill through makes the key match the
  # circles in the plot.
  # A rect key ignores shape and size; forcing them on would either be
  # dropped with a warning or shrink the swatch, so the tile case overrides
  # nothing but the alpha the plot draws its cells with.
  ov <- if (identical(key, "tile")) list(alpha = 1)
        else list(shape = 21, size = 3.4, alpha = 1)
  p + guides(
        fill   = guide_legend(nrow = nrow_use, byrow = FALSE,
                              override.aes = ov),
        colour = guide_legend(nrow = nrow_use, byrow = FALSE,
                              override.aes = ov),
        shape  = guide_legend(nrow = nrow_use, byrow = FALSE,
                              override.aes = list(size = 3.4)),
        # the size keys must stay at their MAPPED diameters - that is the
        # whole point of the guide - so nothing is overridden but the fill
        size   = if (isTRUE(keep_size))
                   guide_legend(nrow = max_rows, byrow = FALSE,
                                override.aes = list(shape = 21,
                                                    fill = "grey75",
                                                    colour = "black"))
                 else "none") +
    theme(legend.position = "top",
          legend.direction = "horizontal",
          legend.box = "horizontal",
          legend.justification = "left",
          legend.key.size = unit(1.05, "lines"),
          legend.title    = element_text(face = "bold", size = 10.5),
          legend.text     = element_text(size = 10),
          legend.spacing.x = unit(5, "pt"),
          legend.spacing.y = unit(2, "pt"),
          legend.margin   = margin(2, 6, 2, 2))
}

#' Pull only the legend out of a plot, as a grob
#'
#' @param p A ggplot object.
#' @param key Glyph for the discrete keys, passed to [compact_legend()].
#' @param keep_size Keep a size guide, passed to [compact_legend()].
#' @return A gtable holding the legend, or `NULL` when the plot has none.
#' @seealso [compact_legend()], [draw_legend()]
#' @keywords internal
#' @noRd
extract_legend <- function(p, key = "point", keep_size = FALSE) {
  # compact_legend() deliberately re-specifies override.aes so the pulled-out
  # legend gets its own key sizes. When the plot already set guides for its
  # own inline legend ggplot notes the duplicate and keeps the later one -
  # that is the intent, so the note is not surfaced.
  p <- suppressWarnings(compact_legend(p, key = key, keep_size = keep_size))
  g <- suppressWarnings(ggplot2::ggplotGrob(p))
  keep <- which(vapply(g$grobs, function(x) !is.null(x$name) &&
                       grepl("guide-box", x$name), logical(1)))
  if (!length(keep)) return(NULL)
  lg <- g$grobs[[keep[1]]]
  # a guide-box with no actual key rows is effectively empty
  if (inherits(lg, "zeroGrob")) return(NULL)
  lg
}

#' Draw a plot's legend on its own
#'
#' Renders the legend alone, for the panel that sits beside the plot-type
#' icons. A coverage plot carries a `"status_legend"` attribute naming the
#' thresholds it was built with; when present the legend is rebuilt from
#' [status_legend_plot()] so EVERY sampling status appears with its colour,
#' including categories absent from the data being shown.
#'
#' @param p A ggplot object.
#' @return Called for its side effect; draws on the current device.
#' @seealso [extract_legend()], [status_legend_plot()]
#' @keywords internal
#' @noRd
draw_legend <- function(p) {
  # A coverage plot carries the arguments needed to rebuild a legend holding
  # EVERY status, not just those present in the data. Pulling the legend from
  # that synthetic plot is what gives absent categories a filled swatch: a
  # discrete scale can list an unused level, but has no data row to draw its
  # key from. Built here (not in the builder) so it costs nothing unless the
  # legend panel is actually rendered.
  spec <- attr(p, "status_legend", exact = TRUE)
  # The key glyph is chosen by the plot that asked for the legend, not here:
  # a heatmap sends key = "tile", a bubble plot key = "point" plus the size
  # breaks. A legend whose swatches do not look like the marks on the panel
  # is a legend the reader has to translate.
  key <- "point"; sz <- NULL
  if (!is.null(spec)) {
    key <- if (identical(spec$key, "tile")) "tile" else "point"
    sz  <- spec$size
    p <- tryCatch(status_legend_plot(spec$threshold, spec$what, spec$min_meas,
                                     key = key, size = sz),
                  error = function(e) p)
  }
  lg <- tryCatch(extract_legend(p, key = key, keep_size = !is.null(sz)),
                 error = function(e) NULL)
  grid::grid.newpage()
  if (is.null(lg)) return(invisible(NULL))
  grid::grid.draw(lg)
}

# ----------------------------------------------------------
#  Tables and choice lists
# ----------------------------------------------------------

#' Colour a DT column by group id
#'
#' @param dt A DT datatable.
#' @param col The column to colour.
#' @param pal A named colour vector, e.g. from [group_palette()].
#' @return The datatable with the column styled.
#' @keywords internal
#' @noRd
dt_group_colours <- function(dt, col, pal) {
  if (!length(pal)) return(dt)
  DT::formatStyle(dt, col, backgroundColor = DT::styleEqual(names(pal), unname(pal)),
                  color = "white", fontWeight = "bold")
}

#' Choices for a donor-stratum selectInput
#'
#' Builds the picker's choices from the strata themselves, labelling each with
#' its discard rate and landings so an informed exclusion can be made without
#' leaving the control.
#'
#' @param strata A stratum table.
#' @param dims The stratum key columns.
#' @param value_col The rate column to show.
#' @param order_by Order by "landings" or "rate".
#' @return A named character vector for `selectInput(choices = )`.
#' @keywords internal
#' @noRd
donor_choices <- function(strata, dims, value_col = "discard_rate",
                          order_by = c("landings", "rate")) {
  order_by <- match.arg(order_by)
  if (is.null(strata) || !nrow(strata)) return(character(0))
  d <- as.data.frame(strata)
  present <- intersect(dims, names(d))
  if (!length(present)) return(character(0))
  key <- do.call(paste, c(lapply(present, function(k) as.character(d[[k]])),
                          sep = " | "))
  rate <- if (value_col %in% names(d)) d[[value_col]] else rep(NA_real_, nrow(d))
  lan  <- if ("landings" %in% names(d)) d$landings else rep(NA_real_, nrow(d))
  agg <- data.frame(key = key, rate = rate, lan = lan,
                    stringsAsFactors = FALSE) %>%
    group_by(key) %>%
    summarise(rate = suppressWarnings(mean(rate, na.rm = TRUE)),
              lan = sum(lan, na.rm = TRUE), .groups = "drop") %>%
    arrange(if (order_by == "rate") desc(rate) else desc(lan))
  lab <- sprintf("%s   (DR %s%%, %s t)", agg$key,
                 ifelse(is.finite(agg$rate), round(agg$rate * 100, 1), "-"),
                 fmt_t3(agg$lan))
  setNames(agg$key, lab)
}
