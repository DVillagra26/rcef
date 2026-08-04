# workbench modules live where app.R says they do; the ICES scripts stay
# in EXT_DIR. One convention means the tests exercise what ships.
APP_DIR <- getOption("rcef.app_dir", "R_development")
EXT_DIR <- getOption("rcef.external_dir", "R")
source(file.path(APP_DIR, "0_Functions_utils.R")); source(file.path(APP_DIR, "0_Functions_theme.R"))
  source(file.path(APP_DIR, "0_Functions_classify.R")); source(file.path(APP_DIR, "0_Functions_groups.R"))
  source(file.path(APP_DIR, "0_Functions_raising.R")); source(file.path(APP_DIR, "0_Functions_emit.R"))
  source(file.path(APP_DIR, "0_Functions_plots.R")); source(file.path(APP_DIR, "app_ui_helpers.R"))
ok <- function(l, c) cat(if (isTRUE(c)) "PASS" else "**FAIL**", "-", l, "\n")

census <- read.csv("test_census.csv", stringsAsFactors = FALSE)
d  <- derive_fields(census)
DIMS4 <- c("metier4","Country","Area1","Season")
sf <- build_strata(d, DIMS4)
cat("strata:", nrow(sf), "\n\n")

cat("=== A. fine -> coarse recommendation ===\n")
rec <- recommend_raising(sf, DIMS4, thresholds = c(30,40,50,60),
                         self_threshold = 60, mode = "nested", dims = DIMS4)
print(table(rec$raise_level))
ok("every stratum resolved", !any(is.na(rec$raise_condition)))
ok("100% coverage -> no raising needed",
   all(rec$raise_level[!is.na(rec$coverage) & rec$coverage >= 100] == "no raising needed"))
ok("own stratum label used when hierarchy covers all dims",
   "own stratum" %in% rec$raise_level)
# a stratum matched at 'own stratum' must itself clear the finest threshold
own <- rec %>% filter(raise_level == "own stratum")
ok("own-stratum matches clear their threshold", all(own$coverage >= 60))
ok("stratum_id present", "stratum_id" %in% names(rec))
cat("\n")

cat("=== B. thresholds are applied finest-first ===\n")
r1 <- recommend_raising(sf, DIMS4, thresholds = c(0,0,0,101),
                        self_threshold = 101, mode = "nested", dims = DIMS4)
ok("impossible finest threshold pushes work coarser",
   !("own stratum" %in% r1$raise_level))
r2 <- recommend_raising(sf, DIMS4, thresholds = c(101,101,101,101),
                        self_threshold = 101, mode = "nested", dims = DIMS4)
ok("all-impossible thresholds fall back to ALL",
   all(r2$raise_level[r2$coverage < 100 | is.na(r2$coverage)] == "ALL"))
cat("\n")

cat("=== C. multi-valued donor groups ===\n")
sets <- list(mainCo = c("FRA","DEU"))
rules <- list(
  new_rule("G1","TBB Q1",
           raise  = list(list(var="metier4", values="TBB"),
                         list(var="Season", values="1")),
           source = list(list(var="metier4", values="TBB"),
                         list(var="Season", values="1"))),
  new_rule("G2","TBB all year",
           raise  = list(list(var="metier4", values="TBB")),
           source = list(list(var="metier4", values="TBB"))),
  new_rule("G3","catch-all", list()))
a <- assign_rule_groups(sf, rules, sets)
ok("toBeRaisedGroup is single valued", is.character(a$toBeRaisedGroup))
ok("usedToRaiseGroup is a list column", is.list(a$usedToRaiseGroup))
tbb1 <- which(a$metier4=="TBB" & a$Season=="1")
ok("TBB Q1 raised by G1 only", all(a$toBeRaisedGroup[tbb1]=="G1"))
ok("TBB Q1 donates to G1, G2 AND G3",
   all(vapply(a$usedToRaiseGroup[tbb1], function(v)
     all(c("G1","G2","G3") %in% v), logical(1))))
ok("some stratum donates to >1 group", max(a$n_donates_to) > 1)
cat("   donation counts:", paste(names(table(a$n_donates_to)),
                                 table(a$n_donates_to), sep="x", collapse=" "), "\n\n")

cat("=== D. apply_raising ===\n")
ra <- apply_raising(rec, "raise_condition")
ok("raised >= observed discards",
   all(ra$total_discards_est >= ra$discards - 1e-9, na.rm=TRUE))
fully <- ra %>% filter(coverage >= 100)
ok("fully sampled strata unchanged",
   all(abs(fully$total_discards_est - fully$discards) < 1e-6))
uns <- ra %>% filter(coverage == 0, landings > 0, !is.na(donor_rate))
ok("unsampled strata get an estimate", all(uns$total_discards_est > 0))
cat("   observed total:", round(sum(ra$discards),1),
    "| raised total:", round(sum(ra$total_discards_est),1), "\n")

rm2 <- apply_raising(a, "toBeRaisedGroup", donor_members = a$usedToRaiseGroup)
ok("manual raising runs", all(!is.na(rm2$total_discards_est)))
cat("   manual raised total:", round(sum(rm2$total_discards_est),1), "\n\n")

cat("=== E. scenarios save / load / run ===\n")
s1 <- scenario_auto("auto_strict", DIMS4, DIMS4, c(30,40,50,60), "metier4","Area1")
s2 <- scenario_auto("auto_loose",  DIMS4, DIMS4, c(10,10,10,10), "metier4","Area1")
s3 <- scenario_manual("manual_v1", DIMS4, rules, sets, "metier4","Area1")
js <- scenarios_to_json(list(s1,s2,s3))
back <- scenarios_from_json(js)
ok("json round trip keeps 3 scenarios", length(back)==3)
ok("auto scenario keeps thresholds", identical(as.numeric(back[[1]]$thresholds), c(30,40,50,60)))
ok("manual scenario keeps rules", length(back[[3]]$rules)==3)
ok("restored manual rules still evaluate",
   !any(is.na(assign_rule_groups(sf, back[[3]]$rules, back[[3]]$sets)$toBeRaisedGroup)))
runs <- setNames(lapply(back, run_scenario, catch_data = d),
                 vapply(back, `[[`, character(1), "name"))
ok("all scenarios run", length(runs)==3 && all(vapply(runs, nrow, 1L) == nrow(sf)))
cat("   totals:", paste(names(runs),
    round(vapply(runs, function(r) sum(r$total_discards_est, na.rm=TRUE), 1), 1),
    sep="=", collapse="  "), "\n\n")

cat("=== F. comparison table ===\n")
cmp <- compare_scenarios(runs, c("metier4"))
print(cmp %>% arrange(metier4, scenario) %>% head(9))
wide <- cmp %>% select(metier4, scenario, estimated_discards) %>%
  tidyr::pivot_wider(names_from=scenario, values_from=estimated_discards)
print(wide)
ok("wide table has one column per scenario", ncol(wide)==4)
cmp4 <- compare_scenarios(runs, DIMS4)
ok("full aggregation = raw strata", nrow(cmp4)==nrow(sf)*3)
cat("\n")

cat("=== G. plots ===\n")
bld <- function(p) !is.null(p) && !inherits(try(ggplot_build(p), silent=TRUE),"try-error")
ok("boxplot x+fill uses full data",
   bld(plot_dr_box(sf, "Country", "Area1", NULL)))
ok("boxplot with facet", bld(plot_dr_box(sf, "Country", "Area1", "metier4")))
ok("boxplot no fill", bld(plot_dr_box(sf, "metier4", NULL, NULL)))
# the key regression: boxes must summarise several points
nb <- sf %>% filter(!is.na(discard_rate)) %>% count(Country, Area1)
ok("boxes contain multiple strata", max(nb$n) > 1)
cat("   max strata per Country x Area box:", max(nb$n), "\n")
sc <- classify_strata(sf, 50)
ok("coverage bars y/fill/facet", bld(plot_coverage_bars(sc,"metier4","Country","Season")))
ok("coverage bars no facet",     bld(plot_coverage_bars(sc,"metier4","Country",NULL)))
# force an ALL group so the fallback colour is exercised
rec_all <- recommend_raising(sf, DIMS4, thresholds = c(99,99,99,99),
                             self_threshold = 99, mode = "nested", dims = DIMS4)
pal <- group_palette(unique(c(rec_all$raise_level, rec$raise_level)))
ok("ALL fallback appears at impossible thresholds", "ALL" %in% rec_all$raise_level)
ok("river plot", bld(plot_river(rec %>% filter(raise_level!="no raising needed"),
                                "metier4","raise_level","landings", pal)))
ok("comparison plot", bld(plot_comparison(cmp,"metier4",NULL)))
ok("group palette greys the ALL pool", unname(pal["ALL"])=="#BFBFBF")
ok("group palette gives distinct colours",
   length(unique(pal[!names(pal) %in% c("ALL","no raising needed")])) ==
     sum(!names(pal) %in% c("ALL","no raising needed")))
ok("plot_height scales with facet rows",
   plot_height(sf,"Country",10,2) > plot_height(sf,NULL,10,2))
cat("\n")

cat("=== H. emitted code ===\n")
code <- c(emit_header("test.csv","bll",nrow(census)),
          emit_hier_code(rec, DIMS4, c(30,40,50,60), "fine_to_coarse", DIMS4))
tf <- tempfile(fileext=".R"); writeLines(code, tf)
ok("auto code parses", !inherits(try(parse(tf), silent=TRUE),"try-error"))
ok("auto code defines strataCond", any(grepl("^strataCond", code)))
ok("auto code defines matchedDataCond", any(grepl("^matchedDataCond", code)))
e <- new.env(); eval(parse(tf), envir=e)
ok("both lists same length", length(e$strataCond)==length(e$matchedDataCond))
sel <- sum(vapply(e$strataCond, function(q) sum(eval_tidy(q, data=rec)), 1))
ok("strataCond covers every stratum exactly once", sel==nrow(rec))
cat("   strataCond groups:", length(e$strataCond), "covering", sel, "strata\n")
cat("\n--- sample ---\n")
cat(paste(head(emit_hier_code(rec, DIMS4, c(30,40,50,60), "fine_to_coarse", DIMS4), 22),
          collapse="\n"), "\n")
cat("\nDONE\n")
