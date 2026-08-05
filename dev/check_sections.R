## ---------------------------------------------------------------
##  dev/check_sections.R
##
##  The discard manual builder and the biology manual builder are the
##  same decision made on a different quantity. They must present the
##  same numbered steps, or a user who has learnt one has to learn the
##  other. This compares the two section lists.
##
##  Usage (repo root):  Rscript dev/check_sections.R
## ---------------------------------------------------------------
src <- readLines("inst/app/app.R", warn = FALSE)

steps <- function(from, to) {
  i <- grep(from, src, fixed = TRUE)[1]
  j <- grep(to, src, fixed = TRUE)
  j <- j[j > i][1]
  if (is.na(i) || is.na(j)) return(character(0))
  blk <- src[i:j]
  m <- regmatches(blk, regexpr('"[1-9]\\. [^"]+"', blk))
  sub('^"[1-9]\\. ', "", sub('"$', "", m))
}

disc <- steps('conditionalPanel("input.grp_method == \'manual\'"',
              'h4("8. Save scenario")')
bio  <- steps("len_alloc_ui <- function", 'h4("8. Save allocation")')

cat("discard manual :", paste(disc, collapse = " | "), "\n")
cat("biology manual :", paste(bio,  collapse = " | "), "\n\n")

n <- max(length(disc), length(bio))
fail <- 0L
for (i in seq_len(n)) {
  a <- if (i <= length(disc)) disc[i] else "(missing)"
  b <- if (i <= length(bio))  bio[i]  else "(missing)"
  same <- identical(a, b) ||
    ## the last step differs only in what it names the thing saved
    (i == n && grepl("^Save", a) && grepl("^Save", b))
  if (!same) fail <- fail + 1L
  cat(sprintf("%s  %d. %-22s | %s\n", if (same) "ok  " else "DIFF", i, a, b))
}
cat(sprintf("\nDONE  %d step(s) differ\n", fail))
