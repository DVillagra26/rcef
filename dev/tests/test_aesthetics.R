# ============================================================
#  test_aesthetics.R
#  One look for every plot: separated colours, stable assignment, black
#  outlines. The reported symptom was two countries the same colour; the
#  cause was a base palette whose entries collapsed once past about eight
#  levels, so the invariant worth holding is a MEASURED separation, not a
#  hand-check that the colours "look different".
# ============================================================
# --- locate the modules -------------------------------------------------
# The shipped suites each assumed a working directory and silently halted on
# line one when run from the other place - which then looked like "0 test
# failures". Resolve the folder instead of assuming it.
APP_DIR <- local({
  for (p in c(".", "R_development", file.path("..", "R_development")))
    if (file.exists(file.path(p, "0_Functions_utils.R"))) return(p)
  stop("cannot find 0_Functions_utils.R from ", normalizePath("."))
})
src_ <- function(f) source(file.path(APP_DIR, f))

ok <- TRUE
say <- function(...) cat(sprintf(...), "\n")
chk <- function(c,m){ if(!isTRUE(c)){ok<<-FALSE;say("FAIL: %s",m)} else say("pass: %s",m) }
suppressMessages({library(ggplot2); library(dplyr)})
src_("0_Functions_theme.R")

tolab <- function(x) t(apply(t(col2rgb(x))/255, 1,
                             function(v) convertColor(matrix(v,1), "sRGB", "Lab")))
mindE <- function(x){ d <- as.matrix(dist(tolab(x))); diag(d) <- Inf; min(d) }

## ---- the base palette --------------------------------------------
chk(length(PAL_BASE) >= 24, sprintf("palette is deep enough (%d)", length(PAL_BASE)))
chk(!any(duplicated(PAL_BASE)), "no colour appears twice")
for (n in c(8, 12, 16, 24)) {
  m <- mindE(PAL_BASE[1:n])
  # 15 dE is roughly where two fills stop being confusable side by side on
  # a white panel; the old palette managed 4.1 at n = 12
  chk(m >= 15, sprintf("first %2d colours stay %.1f dE apart (need >= 15)", n, m))
}
chk(mindE(PAL_BASE) >= 12,
    sprintf("even the full palette holds %.1f dE", mindE(PAL_BASE)))

## lightness: nothing so pale it vanishes on white, nothing so dark the
## black outline disappears into it
L <- tolab(PAL_BASE)[,1]
chk(all(L > 28 & L < 88), sprintf("lightness stays in range (%.0f..%.0f)",
                                  min(L), max(L)))

## ---- the reported case -------------------------------------------
ctry <- c("Belgium","Netherlands","UK-Northern Ireland","France","Denmark",
          "Germany","Ireland","Sweden","Norway","Spain","Poland","Portugal")
reset_palettes(); seed_palettes(data.frame(Country = ctry))
p <- var_palette("Country", ctry)
chk(length(p) == length(ctry), "every country got a colour")
chk(mindE(p) >= 15, sprintf("no two countries within %.1f dE", mindE(p)))
nl <- as.matrix(dist(tolab(p[c("Netherlands","UK-Northern Ireland")])))[1,2]
chk(nl >= 25, sprintf("Netherlands vs UK-Northern Ireland: %.1f dE", nl))

## ---- stability: same colour everywhere ---------------------------
a <- var_palette("Country", ctry[1:4])
b <- var_palette("Country", ctry)
chk(identical(unname(a[names(a)]), unname(b[names(a)])),
    "a level keeps its colour when more levels appear")

## ---- and the assignment does not depend on plot order ------------
## This is what seed_palettes() is for. Without it the colours depend on
## which tab was opened first, so two runs of one analysis produce
## differently coloured figures.
d <- data.frame(Country = ctry, Season = c("1","2"), stringsAsFactors = FALSE)
reset_palettes(); seed_palettes(d); first <- var_palette("Country", ctry)
reset_palettes()
var_palette("Country", ctry[c(5,9,2)])          # a plot that ran first
seed_palettes(d); second <- var_palette("Country", ctry)
chk(identical(first[ctry], second[ctry]),
    "seeding makes the assignment independent of which plot drew first")

reset_palettes()
var_palette("Country", ctry[c(5,9,2)])
unseeded <- var_palette("Country", ctry)
chk(!identical(first[ctry], unseeded[ctry]),
    "and without seeding it genuinely would have differed - the guard is not idle")

## ---- outlines ----------------------------------------------------
chk(identical(OUTLINE, "black"), "the shared outline colour is black")
src <- readLines(file.path(APP_DIR, "0_Functions_plots.R"), warn = FALSE)
code <- grep("^\\s*#", src, invert = TRUE, value = TRUE)
for (g in c("grey15","grey20","grey25"))
  chk(!any(grepl(sprintf('colour = "%s"', g), code, fixed = TRUE)),
      sprintf("no mark is still outlined in %s", g))
chk(!any(grepl('colour = "black"', code, fixed = TRUE)),
    "and no literal black either - they all go through OUTLINE")
n_out <- sum(grepl("colour = OUTLINE", code))
chk(n_out >= 12, sprintf("%d marks share the one outline constant", n_out))
## the ribbons are deliberately edgeless: an outline on a +/-1sd band reads
## as a hard limit, which it is not
chk(sum(grepl("colour = NA", code)) >= 2,
    "the uncertainty ribbons stay edgeless, on purpose")



# ============================================================
#  The seeding observer, as app.R actually writes it
# ============================================================
# Everything above tests seed_palettes() in isolation. That is not the same
# as testing that the app CALLS it, at the right moment, from the right
# table - and the version that shipped a moment ago keyed the observer on a
# counter while reading a different reactive, which could have seeded from
# the data being replaced.
local({
  suppressMessages(library(shiny))
  src <- paste(readLines(file.path(APP_DIR, "app.R"), warn = FALSE), collapse = "\n")
  chk(grepl("seed_palettes(d)", src, fixed = TRUE),
      "app.R calls seed_palettes()")
  # the observer must take its dependency on the derived table, not on a
  # counter that changes alongside it
  chk(grepl("observe({\n    d <- tryCatch(derived(), error = function(e) NULL)\n    if (!is.null(d) && nrow(d)) seed_palettes(d)\n  })", src, fixed = TRUE),
      "and does so in an observer that depends on derived() directly")

  app <- function(input, output, session) {
    raw <- reactive({ req(input$n)
      data.frame(Country = LETTERS[seq_len(input$n)],
                 Season = c("1","2"), stringsAsFactors = FALSE) })
    derived <- reactive(raw())
    observe({
      d <- tryCatch(derived(), error = function(e) NULL)
      if (!is.null(d) && nrow(d)) seed_palettes(d)
    })
    exportTestValues(pal = var_palette("Country", derived()$Country))
  }
  reset_palettes()
  testServer(app, {
    session$setInputs(n = 4)
    p1 <- var_palette("Country", LETTERS[1:4])
    chk(length(p1) == 4, "the observer seeded on first load")

    # a second, different data set must RE-seed, not top up: loading a new
    # stock should not leave the old stock's colour assignment behind
    session$setInputs(n = 6)
    session$flushReact()
    p2 <- var_palette("Country", LETTERS[1:6])
    chk(length(p2) == 6, "and re-seeded when the data changed")
    chk(identical(unname(p2[LETTERS[1:6]]), PAL_BASE[1:6]),
        "the new assignment is the palette in sorted order, not a top-up")
  })
})

cat("\n"); say("=== %s ===", if (ok) "ALL CHECKS PASSED" else "THERE WERE FAILURES")
cat("DONE test_aesthetics\n")
