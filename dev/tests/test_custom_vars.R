## Custom derived variables: a NEW column built from first-match-wins
## rules, the way the ICES scripts add a TR1 fleet by hand. Once built it
## must be indistinguishable from a native column.
FAIL <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) cat("ok    ", what, "\n")
  else { FAIL <<- FAIL + 1L; cat("FAIL  ", what, "\n") }
}

d <- data.frame(mesh = c(130, 100, 140, 80),
                Country = c("BEL", "BEL", "NLD", "BEL"),
                metier4 = c("TBB", "OTB", "TBB", "GNS"),
                stringsAsFactors = FALSE)

## the exact example from the request
spec <- list(name = "POPO", otherwise = "other",
  rules = list(list(cond = 'mesh > 120 & Country == "BEL"', value = "CHACHACHA")))
r <- apply_custom_var(d, spec)
ok("POPO" %in% names(r), "the new column is added")
ok(r$POPO[1] == "CHACHACHA", "a row meeting the condition gets the value")
ok(all(r$POPO[2:4] == "other"), "rows that do not get the otherwise value")

## first match wins
spec2 <- list(name = "grp", otherwise = "rest", rules = list(
  list(cond = 'metier4 == "TBB"', value = "beam"),
  list(cond = 'Country == "BEL"', value = "belgian")))
r2 <- apply_custom_var(d, spec2)
ok(r2$grp[1] == "beam",
   "a TBB Belgian row takes the FIRST matching rule, not the second")
ok(r2$grp[2] == "belgian", "a Belgian non-TBB row takes the second rule")
ok(r2$grp[4] == "belgian", "and so does the GNS Belgian row")
ok(r2$grp[3] == "beam", "the NLD TBB row still matches the TBB rule")

## a broken condition must NOT blank the dataset
bad <- list(name = "X", otherwise = "ok", rules = list(
  list(cond = "no_such_column > 5", value = "bad")))
rb <- apply_custom_var(d, bad)
ok(all(rb$X == "ok"), "a condition on a missing column is treated as no-match, not an error")

## variables stack - a later one can use an earlier one
specs <- list(
  list(name = "A", otherwise = "a0", rules = list(list(cond = 'mesh > 120', value = "big"))),
  list(name = "B", otherwise = "b0", rules = list(list(cond = 'A == "big"', value = "fromA"))))
rs <- apply_custom_vars(d, specs)
ok(rs$B[1] == "fromA", "a later variable can build on an earlier one")

## the emitted code is the case_when idiom and round-trips
code <- custom_var_code(spec)
ok(any(grepl("case_when", code)), "emits a mutate(case_when(...))")
ok(any(grepl("CHACHACHA", code)), "and carries the value")
env <- new.env(); env$catch_data <- d
suppressPackageStartupMessages({library(dplyr)})
ev <- eval(parse(text = paste(code, collapse = "\n")), envir = env)
ok(identical(ev$POPO, r$POPO), "the emitted code reproduces the column exactly")

cat(sprintf("\nDONE  %d failure(s)\n", FAIL))
if (FAIL) quit(status = 1L)
