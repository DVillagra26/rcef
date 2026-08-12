#!/usr/bin/env Rscript
## Double quotes inside the CSS/JS string literals in app.R.
##
## The stylesheet is one long R string passed to HTML(). A double quote
## anywhere inside it - in a comment, in content:"...", in
## grid-template-areas:"a b" - closes the string early, and everything
## after it parses as R code. The error surfaces hundreds of lines later
## as "unexpected symbol" or "possible missing comma", pointing at
## something entirely unrelated.
##
## This has broken the build three times. Single quotes work everywhere
## CSS needs quoting, so the rule is: no double quotes in there.

fail <- 0L
f <- "inst/app/app.R"
if (!file.exists(f)) f <- file.path("rcef", f)
src <- readLines(f, warn = FALSE)

## Find the tags$style(HTML(" ... ")) and tags$script(HTML(" ... ")) spans
opens <- grep('tags\\$(style|script)\\(HTML\\("', src)
for (o in opens) {
  ## the block ends at the first line that closes the string: ")) or ")
  end <- NA_integer_
  for (i in seq(o + 1, min(length(src), o + 800))) {
    if (grepl('^\\s*("\\)\\)|"\\))', src[i])) { end <- i; break }
  }
  if (is.na(end)) next
  body <- seq(o + 1, end - 1)
  bad <- body[grepl('"', src[body])]
  ## an escaped \" is deliberate and fine
  bad <- bad[!grepl('\\\\"', src[bad])]
  for (b in bad) {
    fail <- fail + 1L
    cat(sprintf("FAIL  %s:%d  unescaped double quote inside a style/script string\n",
                f, b))
    cat("       ", trimws(src[b]), "\n")
    cat("        use single quotes here - CSS accepts them everywhere\n")
  }
}

cat(sprintf("\nDONE  %d line(s) with a quote that would end the string early\n",
            fail))
if (fail > 0) quit(status = 1)
