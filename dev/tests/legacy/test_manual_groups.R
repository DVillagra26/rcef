# ============================================================
#  test_manual_groups.R
#  The manual rule set must PARTITION the strata it raises: every stratum
#  raised by exactly one group, never two. The reported table must agree
#  with what catchRaising() will actually be handed, and the donor pools
#  must stay reusable across groups.
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
chk <- function(c, m) { if (!isTRUE(c)) { ok <<- FALSE; say("FAIL: %s", m) }
                        else say("pass: %s", m) }
suppressMessages(library(dplyr))
stub <- function(...) invisible(TRUE); environment(stub) <- globalenv()
assign("library", stub, envir = globalenv()); assign("require", stub, envir = globalenv())
src_("0_Functions_utils.R"); src_("0_Functions_classify.R")
src_("0_Functions_groups.R")

set.seed(11)
n <- 24
s <- data.frame(
  metier4 = rep(c("OTB", "TBB", "GNS", "LLS"), each = 6),
  Country = rep(c("BE", "NL", "FR"), 8),
  Season  = rep(c("Q1", "Q2"), 12),
  landings = round(runif(n, 5, 900), 2),
  stringsAsFactors = FALSE)
s$landings_with_discards <- s$landings * sample(c(0, .4, 1), n, TRUE)
s$discards <- s$landings * runif(n, 0, .3)
s$coverage <- s$landings_with_discards / s$landings * 100
s$discard_rate <- s$discards / (s$landings + s$discards)
dims <- c("metier4", "Country", "Season")

cl <- function(var, vals) list(list(var = var, values = vals, negate = FALSE))
rules <- list(
  new_rule("G1", "beam trawl",  cl("metier4", "TBB")),
  new_rule("G2", "Belgian otter",
           c(cl("metier4", "OTB"), list(list(var = "Country", values = "BE",
                                             negate = FALSE)))),
  new_rule("G3", "otter (rest)", cl("metier4", "OTB")),
  new_rule("G4", "catch-all",    list()))          # no clause: claims all

# ---- 1. the effective sets are a partition ---------------------------
eff <- rule_effective_sets(s, rules)
m <- do.call(cbind, eff)
chk(all(rowSums(m) <= 1), "no stratum is claimed by two rules")
chk(all(rowSums(m) == 1), "and every stratum is claimed by one")
chk(!any(attr(eff, "unassigned")), "nothing is left unassigned")
say("      per rule: %s", paste(sprintf("%s=%d", names(eff), colSums(m)),
                                collapse = " "))

# G3 claims every OTB stratum but G2 took the Belgian ones first
claim_g3 <- sum(s$metier4 == "OTB")
chk(sum(eff$G3) < claim_g3,
    sprintf("G3 claims %s OTB strata but effectively takes %s",
            claim_g3, sum(eff$G3)))
chk(sum(eff$G2) == sum(s$metier4 == "OTB" & s$Country == "BE"),
    "G2 takes exactly the Belgian otter strata")

# the catch-all: claims everything, raises only the remainder
chk(sum(eff$G4) == n - sum(eff$G1) - sum(eff$G2) - sum(eff$G3),
    "the catch-all raises exactly the remainder")
chk(sum(eff$G4) < n, "and not the whole stock")

# ---- 2. the REPORTED table agrees with that --------------------------
sm <- rule_group_summary(s, rules)
chk(identical(sm$n_strata_raised, as.integer(colSums(m))),
    "rule_group_summary reports the effective counts")
chk(sm$n_strata_claimed[sm$group_id == "G4"] == n,
    "and still shows that the catch-all's clause claims everything")
chk(sm$n_strata_raised[sm$group_id == "G4"] == sum(eff$G4),
    "while reporting what it raises")
chk(abs(sum(sm$landings_raised) - sum(s$landings)) < 0.5,
    sprintf("landings raised sum to the stock total (%s vs %s)",
            round(sum(sm$landings_raised), 1), round(sum(s$landings), 1)))

# ---- 3. the emitted strataCond is the same partition -----------------
cd <- conds_from_rules(rules)
ev <- lapply(cd$strataCond, function(q) rlang::eval_tidy(q, data = s))
me <- do.call(cbind, ev)
chk(all(rowSums(me) == 1), "the emitted strataCond partition the strata")
chk(identical(unname(colSums(me)), unname(colSums(m))),
    "and select exactly the same strata as the reported table")
for (i in seq_along(ev))
  chk(identical(as.logical(ev[[i]]), as.logical(eff[[i]])),
      sprintf("%s: emitted condition == reported set", names(eff)[i]))

# ---- 4. donors stay reusable ----------------------------------------
md <- lapply(cd$matchedDataCond, function(q) rlang::eval_tidy(q, data = s))
chk(all(md$G4), "the catch-all's donor pool is every stratum")
chk(sum(md$G3 & md$G2) > 0,
    "donor pools overlap across groups - a donor can be reused")
chk(!any(grepl("!\\(", vapply(cd$matchedDataCond, rlang::quo_text, character(1)))),
    "no complement is applied to any matchedDataCond")

# ---- 5. an incomplete rule set is REPORTED as incomplete -------------
partial <- rules[1:2]
tr <- rule_decision_table(s, partial)
chk("(unassigned)" %in% tr$rule, "a gap in the rules shows as an unassigned row")
u <- tr[tr$rule == "(unassigned)", ]
chk(u$n_strata == n - sum(eff$G1) - sum(eff$G2), "with the right count")
chk(abs(tail(tr$cum_pct_landings, 1) - 100) < 0.2,
    "and the cumulative landings still reach 100%")

tr_full <- rule_decision_table(s, rules)
chk(!("(unassigned)" %in% tr_full$rule),
    "a complete rule set has no unassigned row")
chk(abs(tail(tr_full$cum_pct_strata, 1) - 100) < 0.2,
    "cumulative strata reach 100%")
chk(all(diff(tr_full$cum_pct_landings) >= -0.01),
    "cumulative landings never go backwards")

# ---- 6. river data tags each stratum with its owning rule ------------
rd <- rule_river_data(s, rules)
chk(all(!is.na(rd$raise_step)), "every stratum is tagged with a rule")
chk(identical(rd$raise_step, rd$group_id), "group_id mirrors it, for colour")
for (id in names(eff))
  chk(sum(rd$raise_step == id) == sum(eff[[id]]),
      sprintf("river rows for %s match its effective set", id))

# ---- 7. the EMITTED script is the same partition ---------------------
# Two emitters write the manual rules - the Fragments tab's
# emit_rules_code() and the TAF set's code_manual_rule() - and a script
# that re-raises what an earlier rule already raised is worse than a wrong
# number on screen, because it leaves the app looking right.
src_("0_Functions_emit.R")
suppressMessages(library(rlang))
code <- emit_rules_code(rules, list())
chk(isTRUE(tryCatch({ parse(text = code); TRUE }, error = function(e) FALSE)),
    "emit_rules_code() output parses")

# pull the strataCond block back out and evaluate it against the strata
ev_block <- function(txt, obj) {
  e <- new.env(parent = globalenv())
  e$quo <- function(x) rlang::enquo(x)
  eval(parse(text = txt), e)
  lapply(get(obj, e), function(q) rlang::eval_tidy(q, data = s))
}
blk <- paste(code[seq(grep("^strataCond <- list\\($", code),
                      grep("^matchedDataCond <- list\\($", code) - 1)],
             collapse = "\n")
es <- ev_block(blk, "strataCond")
mm <- do.call(cbind, es)
chk(all(rowSums(mm) <= 1), "the EMITTED strataCond never raise a stratum twice")
chk(all(rowSums(mm) == 1), "and cover every stratum exactly once")
chk(identical(unname(colSums(mm)), unname(colSums(m))),
    "and select exactly what the app reported")
chk(any(grepl("!\\(", code)),
    "the emitted raise conditions carry the complements")

# the catch-all is the case that would have re-raised the whole stock
g4 <- es[[4]]
chk(sum(g4) == sum(eff$G4) && sum(g4) < n,
    sprintf("the emitted catch-all raises %s strata, not all %s", sum(g4), n))

cat("\n"); say("=== %s ===", if (ok) "ALL CHECKS PASSED" else "THERE WERE FAILURES")
cat("DONE test_manual_groups\n")
