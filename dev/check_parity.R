## ---------------------------------------------------------------
##  dev/check_parity.R
##
##  The discard section and the length/age sections answer the same
##  question about different quantities. Where they do the same job they
##  should look the same, and a difference should be a deliberate
##  addition rather than something one of them never got.
##
##  This does not enforce sameness - some differences are real. It LISTS
##  the panels and the control types side by side so a drift is visible
##  instead of being discovered by a user.
##
##  Usage (repo root):  Rscript dev/check_parity.R
## ---------------------------------------------------------------
src <- readLines("inst/app/app.R", warn = FALSE)

block <- function(from, to) {
  i <- grep(from, src, fixed = TRUE)[1]
  j <- grep(to, src, fixed = TRUE); j <- j[j > i][1]
  if (is.na(i) || is.na(j)) return(character(0))
  src[i:j]
}

heads <- function(b) {
  m <- regmatches(b, regexpr('h4\\("[^"]+"', b))
  sub('"$', "", sub('^h4\\("', "", m))
}
controls <- function(b) {
  m <- regmatches(b, gregexpr(
    "(sliderInput|numericInput|selectInput|selectizeInput|radioButtons|checkboxInput|actionButton|textInput)\\(", b))
  sub("\\($", "", unlist(m))
}

disc <- block('conditionalPanel("input.grp_method == \'manual\'"',
              'uiOutput("man_rule_rivers")')
## the MANUAL panel only - the hierarchical one above it has no
## counterpart on the discard manual tab and would show as a false gap
bio  <- block('conditionalPanel(sprintf("input.%s == \'manual\'", ns("method")),',
              'uiOutput(ns("rule_rivers"))')
stopifnot(length(disc) > 20, length(bio) > 20)

## Headings that differ only because the two sections name the same
## thing for their own quantity. Listed explicitly so that a REAL gap -
## a panel one side has and the other does not - is the only thing left
## in the "only" lines. Adding a pair here is a decision; letting a gap
## sit in the output is not.
SYNONYM <- list(
  c("8. Save scenario",                    "8. Save allocation"),
  c("strataCond / matchedDataCond",        "allocStrataCond / bioMatchedCond"),
  c("What each rule actually raises",      "What each group actually allocates"),
  c("Overview: raised vs donated per group",
    "Overview: allocated vs donated per group"),
  c("Which strata are raised from where",
    "Strata and the data they are allocated from"))

canon <- function(x) {
  for (pr in SYNONYM) if (x %in% pr) return(pr[1])
  x
}

cat("=== panel headings ===\n")
hd <- vapply(heads(disc), canon, "")
hb <- vapply(heads(bio),  canon, "")
only_d <- setdiff(hd, hb); only_b <- setdiff(hb, hd)
cat("discard only :", if (length(only_d)) paste(only_d, collapse = " | ")
                      else "(none)", "\n")
cat("biology only :", if (length(only_b)) paste(only_b, collapse = " | ")
                      else "(none)", "\n")
cat("shared       :", length(intersect(hd, hb)), "of",
    length(union(hd, hb)), "\n")
cat("synonym pairs:", length(SYNONYM), "\n\n")

cat("=== control mix ===\n")
cd <- table(controls(disc)); cb <- table(controls(bio))
all_c <- sort(union(names(cd), names(cb)))
cat(sprintf("%-18s %8s %8s\n", "control", "discard", "biology"))
for (k in all_c)
  cat(sprintf("%-18s %8d %8d\n", k,
              if (k %in% names(cd)) cd[[k]] else 0L,
              if (k %in% names(cb)) cb[[k]] else 0L))

## the one hard rule: a slider and a number box must not be doing the
## same job in two places that sit next to each other
sl <- grep("sliderInput", c(disc, bio), value = TRUE)
cat("\nsliders still in the manual builders:", length(sl), "\n")
if (length(sl)) cat(paste0("  ", trimws(sl), collapse = "\n"), "\n")

## Every builder should offer the same output tabs, so a user who learns
## one layout knows them all. This counts the tabPanel titles in each.
cat("\n=== output tabs ===\n")
tabs <- function(pat) {
  hits <- grep(pat, src)
  if (!length(hits)) return(character(0))
  # tab titles are the first string arg of tabPanel() near the builder
  m <- regmatches(src, regexpr('tabPanel\\("[^"]+"', src))
  sub('tabPanel\\("', "", m)
}
alltabs <- sort(unique(sub('"$', "",
  sub('tabPanel\\("', "",
      regmatches(src, regexpr('tabPanel\\("[^"]+"', src))))))
cat("tab titles in app.R:", paste(alltabs, collapse = ", "), "\n")
want <- c("Code", "Overview", "Rivers")
have <- all(want %in% alltabs)
cat(if (have) "ok   " else "WARN ",
    "Code / Overview / Rivers tabs present\n")

cat("\nDONE  review the two 'only' lines above\n")
