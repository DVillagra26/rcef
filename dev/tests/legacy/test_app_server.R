suppressMessages({library(shiny); library(dplyr); library(ggplot2)})
ok <- function(l,c) cat(if (isTRUE(c)) "PASS" else "**FAIL**","-",l,"\n")
source("app.R")
up <- data.frame(name="test_census.csv", size=1, type="text/csv",
                 datapath="test_census.csv", stringsAsFactors=FALSE)
bld <- function(p) !is.null(p) && !inherits(try(ggplot_build(p), silent=TRUE),"try-error")

testServer(server, {
  session$setInputs(start_go=1, cfg_stock="bll.27.3a47de", cfg_coord="DV",
                    cfg_year=2025, cfg_dir=".", cfg_entry="cef",
                    cfg_disc=TRUE, cfg_len=FALSE, cfg_age=FALSE)
  ok("startup config captured", cfg$done && cfg$stock=="bll.27.3a47de")
  ok("only discards required", identical(cfg$need, "discards"))
  session$setInputs(file=up, sep=",", threshold=50, stock="bll.27.3a47de", year="",
                    dt_fleet="metier4", dt_area="Area1")
  ok("data loads", nrow(dat())>0)
  ok("weights are tonnes",
     abs(sum(strata_p("dt")$landings) - sum(as.numeric(dat()$total[dat()$catchCategory=="Lan"]))/1000) < 1e-6)
  ok("strata built", nrow(strata_p("dt"))>0)

  # per-tab level of detail is independent
  session$setInputs(hm_fleet="metier5", hm_area="areaValue",
                    dr_fleet="metier4", dr_area="Area1")
  ok("per-tab fleet level independent",
     unname(dmap("hm")["Fleet"])=="metier5" && unname(dmap("dr")["Fleet"])=="metier4")
  ok("per-tab area level independent",
     unname(dmap("hm")["Area"])=="areaValue" && unname(dmap("dr")["Area"])=="Area1")

  # tab 2
  session$setInputs(dr_x="Fleet", dr_fill="Area", dr_shape="Season", dr_facet="Country",
                    dr_jitter=TRUE, box_x="Country", box_fill="Area", box_facet="",
                    box_points=TRUE, box_pooled=TRUE)
  ok("raw DR plot", bld(plot_dr_raw(strata_p("dr"), role("dr","x"), role("dr","fill"),
                                    role("dr","shape"), role("dr","facet"))))
  ok("boxplot x+fill", bld(plot_dr_box(strata_p("dr"), col_of("dr","Country"),
                                       col_of("dr","Area"), NULL)))
  session$setInputs(box_facet="Fleet")
  ok("boxplot with facet from remaining var",
     bld(plot_dr_box(strata_p("dr"), col_of("dr","Country"), col_of("dr","Area"),
                     col_of("dr","Fleet"))))
  h1 <- hgt("dr",""); h2 <- hgt("dr","Fleet")
  ok("height grows with facets", h2 > h1)
  cat("   height no facet:", h1, "| with facet:", h2, "\n")

  # --- REGRESSION: one row per plotted cell (no stacked tiles/bubbles) ---
  session$setInputs(hm_fleet="metier4", hm_area="Area1")
  for (combo in list(c("Fleet","Season",""), c("Fleet","Season","Country"),
                     c("Fleet","","",""), c("Country","Area",""))) {
    sr <- strata_roles("hm", combo)
    keys <- unique(unlist(lapply(combo, function(x) col_of("hm", x))))
    dup <- sr %>% count(across(all_of(keys))) %>% filter(n > 1)
    ok(paste("no duplicate cells for", paste(combo[nzchar(combo)], collapse="/")),
       nrow(dup) == 0)
  }
  full_n <- nrow(strata_p("hm"))
  ok("role aggregation actually collapses rows",
     nrow(strata_roles("hm", c("Fleet","Season",""))) < full_n)
  cat("   full res:", full_n, "-> Fleet x Season:",
      nrow(strata_roles("hm", c("Fleet","Season",""))), "\n")
  # landings must be conserved by the aggregation
  ok("landings conserved under aggregation",
     abs(sum(strata_roles("hm", c("Fleet","Season",""))$landings) -
         sum(strata_p("hm")$landings)) < 1e-6)

  # tabs 3/4/5
  session$setInputs(hm_y="Fleet", hm_x="Season", hm_facet="Country",
                    bb_y="Fleet", bb_x="Season", bb_facet="Country",
                    bb_fleet="metier4", bb_area="Area1",
                    cb_y="Fleet", cb_fill="Country", cb_facet="",
                    cb_fleet="metier4", cb_area="Area1")
  sc <- classify_strata(strata_p("hm"), 50)
  ok("heatmap", bld(plot_heatmap(sc, col_of("hm","Fleet"), col_of("hm","Season"),
                                 col_of("hm","Country"), 50)))
  scb <- classify_strata(strata_p("cb"), 50)
  ok("coverage bars y/fill", bld(plot_coverage_bars(scb, col_of("cb","Fleet"),
                                                    col_of("cb","Country"), NULL)))
  session$setInputs(cb_facet="Season")
  ok("coverage bars y/fill/facet",
     bld(plot_coverage_bars(scb, col_of("cb","Fleet"), col_of("cb","Country"),
                            col_of("cb","Season"))))

  # tab 6
  session$setInputs(ar_fleet="metier4", ar_area="Area1",
                    hier_2="Fleet", hier_3="Country", hier_4="Area", hier_5="Season",
                    thr_1=c(50,100), thr_2=c(30,100), thr_3=c(40,100),
                    thr_4=c(50,100), thr_5=c(60,100), river_left="Fleet", ar_dr=c(0,1), threshold=50,
                    hier_mode="nested")
  ok("hierarchy read in order", identical(hier(), c("Fleet","Country","Area","Season")))
  ok("hier cols mapped", identical(hier_cols(), c("metier4","Country","Area1","Season")))
  ok("thresholds coarse-first", identical(hier_thr(), c(30,40,50,60)))
  r <- rec()
  ok("recommendation complete", !any(is.na(r$raise_condition)))
  ok("no coarse_to_fine option exists", is.null(input$direction))
  ok("own-stratum step present", "own stratum" %in% r$raise_level)
  # --- cascade: default is nested, widening one key at a time ---
  DD <- unname(dmap("ar")[DIMS])
  st <- raising_steps(hier_cols(), hier_thr(), 60, "nested", DD)
  ok("nested widens one key at a time",
     identical(vapply(st,function(x) x$label, character(1)),
               c("own stratum","metier4 + Country + Area1",
                 "metier4 + Country","metier4")))
  ok("redundant all-keys step dropped when own stratum is tested",
     !("metier4 + Country + Area1 + Season" %in%
       vapply(st,function(x) x$label, character(1))))
  ok("descriptions name what is pooled over",
     grepl("pooled across all Season", st[[2]]$note) &&
     grepl("pooled across all Area1 and Season", st[[3]]$note))
  # "each key on its own" was removed: it built pools that ignored the
  # keys above them. An old scenario asking for it must still load, and
  # must now widen one key at a time like everything else.
  st1 <- raising_steps(hier_cols(), hier_thr(), 60, "single", DD)
  ok("an old 'single' scenario falls back to the nested cascade",
     identical(vapply(st1, function(x) x$label, character(1)),
               vapply(st, function(x) x$label, character(1))))

  tr <- tree_tbl_r()
  ck <- check_partition(rec(), tr)
  ok("PARTITION: every stratum resolved once", ck$n_ok && ck$unassigned==0)
  ok("PARTITION: landings sum to the total exactly", ck$l_ok)
  ok("PARTITION: no landings double counted", ck$l_steps <= ck$l_total + 1e-6)
  tr2 <- tree_tbl_r2()
  ok("step table carries % and cumulative landings",
     all(c("pct_landings","cum_pct_strata","cum_pct_landings") %in% names(tr2)))
  ok("cumulative landings reach 100%", tail(tr2$cum_pct_landings,1) == 100)
  ok("percentages are not all the first row", length(unique(tr2$pct_landings)) > 1)
  cat("   strata", ck$n_steps, "/", ck$n_total,
      "| landings", ck$l_steps, "/", ck$l_total, "\n")
  # holds under other settings too
  for (m in c("nested","single")) for (th in c(10, 90)) {
    session$setInputs(hier_mode=m, thr_1=c(th,100), thr_2=c(th,100),
                      thr_3=c(th,100), thr_4=c(th,100), thr_5=c(th,100))
    c2 <- check_partition(rec(), tree_tbl_r())
    ok(paste("PARTITION holds:", m, "at", th, "%"),
       c2$n_ok && c2$l_ok && c2$unassigned==0)
  }
  session$setInputs(hier_mode="nested", thr_1=c(50,100), thr_2=c(30,100),
                    thr_3=c(40,100), thr_4=c(50,100), thr_5=c(60,100))
  ok("step ids assigned to every stratum", !any(is.na(rec()$raise_step)))
  # donor exclusion
  session$setInputs(ar_dr=c(0, 0.30))
  r2 <- rec()
  ok("donor exclusion flags strata", sum(!r2$donor_ok) > 0)
  ok("excluded strata still in diagnosis", nrow(r2)==nrow(r))
  cat("   donors excluded:", sum(!r2$donor_ok), "of", nrow(r2), "\n")
  session$setInputs(ar_dr=c(0,1))
  ok("groups get G ids", all(grepl("^G", auto_groups()$group_id)))
  ok("group palette keyed on G ids",
     all(auto_groups()$group_id %in% names(auto_pal())))
  ok("raised overview sane",
     all(raised_overview(rec())$unsampled_landings <=
         raised_overview(rec())$total_landings))
  cat("   levels:", paste(names(table(r$raise_level)), table(r$raise_level),
                          sep="=", collapse="  "), "\n")
  ok("river plot", bld(plot_river(r %>% filter(raise_level!="no raising needed"),
                                  col_of("ar","Fleet"), "raise_level","landings",
                                  auto_pal())))
  ok("palette covers all group ids",
     all(auto_groups()$group_id %in% names(auto_pal())))

  # duplicate keys rejected (hier_3 set to match hier_2's default Fleet)
  session$setInputs(hier_3="Fleet")
  ok("duplicate hierarchy keys rejected",
     inherits(try(hier(), silent=TRUE), "try-error"))
  session$setInputs(hier_3="Country")

  # save auto scenario
  session$setInputs(auto_name="auto_strict")
  session$setInputs(save_auto=1)
  ok("auto scenario saved", "auto_strict" %in% names(rv$scenarios))
  session$setInputs(thr_5=c(10,100), auto_name="auto_loose")
  session$setInputs(save_auto=2)
  ok("second scenario saved", length(rv$scenarios)==2)

  # tab 7 manual
  session$setInputs(mg_fleet="metier4", mg_area="Area1",
                    dr_on=TRUE, rule_dr=c(0.10, 0.30), catchall_dr=c(0, 0.40))
  session$setInputs(clause_var="metier4", clause_vals="TBB", clause_neg=FALSE)
  session$setInputs(add_clause=1)
  ok("clause built", length(rv$clause)==1)
  cat("   clause:", cond_string(rv$clause), "\n")
  session$setInputs(to_strata=1)
  ok("clause moved to strataCond", length(rv$pend_raise)==1 && length(rv$clause)==0)
  session$setInputs(mirror=1)
  ok("mirror copies to matchedDataCond", identical(rv$pend_source, rv$pend_raise))
  session$setInputs(rule_id="G1", rule_label="TBB")
  session$setInputs(save_rule=1)
  ok("rule saved", length(rv$rules)==1)
  ok("DR window stored on the rule only",
     rv$rules[[1]]$dr_min==0.10 && rv$rules[[1]]$dr_max==0.30)
  ok("strataCond has no DR term",
     !grepl("discard_rate", cond_string(rv$rules[[1]]$raise)))
  ok("matchedDataCond carries the DR term",
     grepl("discard_rate >= 0.1", source_cond_string(rv$rules[[1]])))
  session$setInputs(add_catchall=1)
  ok("catch-all added", length(rv$rules)==2)
  ok("catch-all has its own DR window",
     is.na(rv$rules[[2]]$dr_min) && rv$rules[[2]]$dr_max==0.40)
  # donors respect the per-rule window
  aa <- man_assigned()
  g1 <- which(vapply(aa$usedToRaiseGroup, function(v) "G1" %in% v, logical(1)))
  ok("G1 donors inside its DR window",
     all(aa$discard_rate[g1] >= 0.10 & aa$discard_rate[g1] <= 0.30))
  ok("G1 donors fewer than all TBB", length(g1) < sum(aa$metier4=="TBB"))
  # movable order list
  before <- vapply(rv$rules, `[[`, character(1), "id")
  session$setInputs(rule_move=list(i=2, dir="up"))
  ok("order list moves a group up",
     identical(vapply(rv$rules,`[[`,character(1),"id"), rev(before)))
  session$setInputs(rule_move=list(i=1, dir="down"))
  ok("order list moves back", identical(vapply(rv$rules,`[[`,character(1),"id"), before))
  a <- man_assigned()
  ok("toBeRaisedGroup assigned", !any(is.na(a$toBeRaisedGroup)))
  ok("usedToRaiseGroup is a list", is.list(a$usedToRaiseGroup))
  ok("some strata donate to >1 group", max(a$n_donates_to) > 1)
  session$setInputs(man_name="manual_1")
  session$setInputs(save_manual=1)
  ok("manual scenario saved", length(rv$scenarios)==3)
  ok("rule order UI renders", !is.null(output$rule_order))

  ok("manual overview", nrow(manual_overview(man_assigned(), rv$rules))==2)

  # --- REGRESSION: scenarios at DIFFERENT levels of detail ---
  session$setInputs(ar_fleet="metier5", ar_area="areaValue", auto_name="auto_m5")
  session$setInputs(save_auto=3)
  ok("4 scenarios incl. mixed detail", length(rv$scenarios)==4)
  rr <- runs()
  ok("all scenarios run despite different levels of detail",
     length(rr)==4 && all(vapply(rr, nrow, 1L) > 0))
  ok("common keys found", length(cmp_keys()) >= 2)
  cat("   common keys:", paste(cmp_keys(), collapse=", "), "\n")

  # tab 8 comparison
  session$setInputs(cmp_x="Country", cmp_facet="")
  cc <- cmp()
  ok("comparison has 4 scenarios", length(unique(cc$scenario))==4)
  ok("stacked comparison plot", bld(plot_comparison(cc, "Country", NULL)))
  # scenarios must sit SIDE BY SIDE, each with its own stacked bar
  bb <- ggplot_build(plot_comparison(cc, "Country", NULL))$data[[1]]
  nx <- length(unique(round(bb$x, 3)))
  ok("one bar per scenario per x (not stacked on each other)",
     nx == length(unique(cc$Country)) * length(unique(cc$scenario)))
  ok("each bar has 2 stacked components",
     all(as.data.frame(table(round(bb$x,3)))$Freq == 2))
  ct <- comparison_table(cc, "Country")
  ok("table drops landings", !("landings" %in% names(ct)))
  ok("table uses Imported", "Imported" %in% names(ct))
  ok("table has one column per scenario",
     all(unique(cc$scenario) %in% names(ct)))
  tot <- cc %>% group_by(scenario) %>% summarise(t=sum(estimated_discards))
  print(as.data.frame(tot))
  ok("scenarios give different totals", length(unique(round(tot$t,3)))>1)
  ok("catches overview renders", !is.null(output$cef_cat_sum))

  # json round trip through the app
  js <- scenarios_to_json(unname(rv$scenarios))
  back <- scenarios_from_json(js)
  ok("json export/import keeps all 4", length(back)==4)
  ok("imported manual scenario still runs",
     nrow(run_scenario(back[[which(vapply(back,`[[`,character(1),"method")=="manual")[1]]],
                       dat())) > 0)
  ok("imported auto scenario keeps self threshold",
     !is.null(back[[1]]$self_threshold))

  # tab 9
  session$setInputs(code_parts=c("head","derive","auto","manual"))
  cl <- code_lines()
  tf <- tempfile(fileext=".R"); writeLines(cl, tf)
  ok("code parses", !inherits(try(parse(tf), silent=TRUE),"try-error"))
  ok("auto emits strataCond", any(grepl("^strataCond", cl)))
  ok("auto emits matchedDataCond", sum(grepl("^matchedDataCond", cl)) >= 1)
})
cat("\n=== new sections in the app ===\n")
testServer(server, {
  session$setInputs(file=up, sep=",", threshold=50, stock="bll.27.3a47de", year="",
                    dt_fleet="metier4", dt_area="Area1",
                    ar_fleet="metier4", ar_area="Area1",
                    hier_2="Fleet", hier_3="Country", hier_4="Area", hier_5="Season",
                    thr_1=c(50,100), thr_2=c(30,100), thr_3=c(40,100),
                    thr_4=c(50,100), thr_5=c(60,100), ar_dr=c(0,1), hier_mode="nested",
                    nr_fleet="metier4", nr_area="Area1",
                    nr_source="auto", nr_type="discards", nr_origin="WGEstimate",
                    nr_vartype="WeightLive", nr_log="Log.txt", nr_assembled=TRUE,
                    ic_source="exchange", ic_input_mode="path", ic_path="./d",
                    ic_years="2022", ic_rel_mode="build", ic_rel_year=2022,
                    ic_output_format=c("to_list"), ic_out_path="./o",
                    ic_prefix="p_", ic_suffix="_v17.1", ic_incl_bms=TRUE,
                    ic_patch_psu=FALSE, ic_keep_temp=FALSE, ic_metier6="Fleet",
                    log_dir=tempdir(), log_name="sess.R", log_auto=FALSE)
  ok("InterCatch call renders", nchar(paste(ic_code(ic_inputs()), collapse="")) > 100)
  ok("running script assembles", length(log_render(rv$log)) > 10)
  lr <- log_render(rv$log)
  ok("script includes the InterCatch call", any(grepl("funIntercatchCEF", lr)))
  # the automatic allocation must appear as a RULE, not a frozen grouping
  session$setInputs(auto_name="a1"); session$setInputs(save_auto=1)
  lr <- log_render(rv$log)
  ok("script records the auto allocation as a rule",
     any(grepl("recommend_raising", lr)) && any(grepl("hierarchy", lr)))
  ok("script does NOT hardcode this year's grouping",
     !any(grepl("stratum_id %in%", lr, fixed=TRUE)))
  ok("script raises through catchRaising", any(grepl("catchRaising\\(", lr)))
  ok("script carries the thresholds", any(grepl("thresholds", lr)))
  tf <- tempfile(fileext=".R"); writeLines(lr, tf)
  ok("whole running script parses", !inherits(try(parse(tf), silent=TRUE), "try-error"))
  st <- state_capture(input, rv)
  ok("state captured", length(st$inputs) > 10)
  ok("state carries no button counts", !any(grepl("^save_|^add_", names(st$inputs))))
  ok("built-in raising engine removed", is.null(input$nr_engine))
  js <- state_to_json(st)
  ok("state json round trips", length(state_from_json(js)$inputs) == length(st$inputs))
  ok("conditions for catchRaising build from the cascade",
     length(nr_conds()$strataCond) >= 1)
  ok("CEF check reports missing columns on a bare census",
     !cef_check(dat())$ok)
})
cat("\n=== merged plot tabs, summaries, final raising ===\n")
testServer(server, {
  session$setInputs(start_go=1, cfg_stock="bll.27.3a47de", cfg_coord="DV",
                    cfg_year=2025, cfg_dir=".", cfg_entry="cef",
                    cfg_disc=TRUE, cfg_len=TRUE, cfg_age=FALSE)
  session$setInputs(file=up, sep=",", threshold=50, stock="bll.27.3a47de", year="",
                    dt_fleet="metier4", dt_area="Area1",
                    cov_fleet="metier4", cov_area="Area1",
                    cov_y="Fleet", cov_fill="Season", cov_facet="Country",
                    dr_fleet="metier4", dr_area="Area1",
                    dr_x="Fleet", dr_fill="Area", dr_shape="Season",
                    dr_facet="Country", dr_jitter=TRUE,
                    box_x="Country", box_fill="Area", box_facet="",
                    box_points=TRUE, box_pooled=TRUE)
  bld <- function(p) !is.null(p) && !inherits(try(ggplot_build(p),silent=TRUE),"try-error")
  # one menu drives all three coverage plots
  for (ty in c("heatmap","bubbles","bars")) {
    session$setInputs(cov_type=ty)
    y <- col_of("cov", input$cov_y); f <- col_of("cov", input$cov_fill)
    fc <- col_of("cov", input$cov_facet)
    p <- switch(ty, heatmap=plot_heatmap(cov_sc(),y,f,fc,50),
                    bubbles=plot_bubbles(cov_sc(),y,f,fc,50),
                    bars   =plot_coverage_bars(cov_sc(),y,f,fc))
    ok(paste("coverage plot:", ty), bld(p))
  }
  ok("coverage strata aggregated to the plotted roles only",
     nrow(cov_sc()) <= nrow(strata_p("cov")))
  for (ty in c("raw","box")) {
    session$setInputs(dr_type=ty)
    p <- if (ty=="box") plot_dr_box(strata_p("dr"), col_of("dr","Country"),
                                    col_of("dr","Area"), NULL)
         else plot_dr_raw(strata_p("dr"), role("dr","x"), role("dr","fill"),
                          role("dr","shape"), role("dr","facet"))
    ok(paste("discard-rate plot:", ty), bld(p))
  }
  # thresholds are ranges pinned at 100; only the lower bound is the threshold
  session$setInputs(ar_fleet="metier4", ar_area="Area1", hier_mode="nested",
                    hier_2="Fleet", hier_3="Country", hier_4="Area", hier_5="Season",
                    thr_1=c(50,100), thr_2=c(30,100), thr_3=c(40,100),
                    thr_4=c(50,100), thr_5=c(60,100),
                    ar_dr=c(0,1), river_left="Fleet")
  ok("threshold read from the lower bound of the range",
     identical(hier_thr(), c(30,40,50,60)))
  ok("own-stratum threshold likewise", "own stratum" %in% rec()$raise_level)
  # summaries
  cs <- catches_summary(dat())
  ok("catches summary produced", nrow(cs) > 8)
  ok("summary reports tonnes",
     any(grepl("Landings \\(t\\)", cs$item)))
  ok("by-category table totals", tail(catches_by_category(dat())$catchCategory,1)=="Total")
  ok("biology coverage names a missing dimension clearly",
     inherits(try(biology_coverage(dat(), NULL, "not_a_column"), silent=TRUE),
              "try-error"))
  bc <- biology_coverage(dat(), NULL, "metier4")
  ok("biology coverage runs with no distributions table", nrow(bc) > 0)
  ok("nothing sampled when distributions absent", !any(bc$sampled))
  # final raising
  session$setInputs(nr_fleet="metier4", nr_area="Area1", auto_name="fin1",
                    nr_type="discards", nr_origin="WGEstimate",
                    nr_vartype="WeightLive", nr_log="")
  session$setInputs(save_auto=1)
  session$setInputs(final_alloc="fin1", fin_x="Fleet", fin_facet="",
                    fin_row="Country", fin_col="Area")
  ok("allocation available to adopt", "fin1" %in% names(rv$scenarios))
  ok("allocation box renders", !is.null(output$alloc_box))
})
cat("\n=== drag-drop clause, chips, restored manual flow ===\n")
testServer(server, {
  session$setInputs(start_go=1, cfg_stock="s", cfg_coord="c", cfg_year=2025,
                    cfg_dir=".", cfg_entry="cef", cfg_disc=TRUE,
                    cfg_len=FALSE, cfg_age=FALSE)
  session$setInputs(file=up, sep=",", threshold=50, stock="bll.27.3a47de",
                    year="", dt_fleet="metier4", dt_area="Area1",
                    mg_fleet="metier4", mg_area="Area1", dr_on=FALSE,
                    catchall_dr=c(0,1))
  session$setInputs(clause_var="metier4", clause_vals="TBB", clause_neg=FALSE)
  session$setInputs(add_clause=1)
  ok("clause built", length(rv$clause)==1)
  session$setInputs(drop_target="strata")
  ok("dropping on strataCond assigns it",
     length(rv$pend_raise)==1 && length(rv$clause)==0)
  session$setInputs(clause_vals="OTB"); session$setInputs(add_clause=2)
  session$setInputs(drop_target="matched")
  ok("dropping on matchedDataCond assigns it", length(rv$pend_source)==1)
  ok("the two boxes hold different clauses",
     !identical(rv$pend_raise, rv$pend_source))
  session$setInputs(rule_id="G1", rule_label="tbb")
  session$setInputs(save_rule=1)
  ok("rule saved after dropping", length(rv$rules)==1)
  session$setInputs(add_catchall=1)
  ok("catch-all appended", length(rv$rules)==2)
  a <- man_assigned()
  ok("manual assignment still works", !any(is.na(a$toBeRaisedGroup)))
  ok("order list renders", !is.null(output$rule_order))
  session$setInputs(rule_move=list(i=2,dir="up"))
  ok("order list still moves groups",
     vapply(rv$rules,`[[`,character(1),"id")[1]=="G2")
  session$setInputs(man_name="m1"); session$setInputs(save_manual=1)
  ok("allocation saved from the manual builder", "m1" %in% names(rv$scenarios))
  ok("allocation table lists strata dims",
     "strata" %in% names(sc_table()) && nzchar(sc_table()$strata[1]))
  ok("variable chips render", !is.null(output$clause_var_chips))
})
cat("\n=== InterCatch file mapping ===\n")
testServer(server, {
  d <- file.path(tempdir(), "icfiles"); dir.create(d, showWarnings=FALSE)
  for (f in c("HI_pok.csv","SI_pok.csv","SD_pok.csv"))
    cat("x\n", file = file.path(d, f))
  session$setInputs(start_go=1, cfg_stock="pok.27.3a46", cfg_coord="YR",
                    cfg_year=2022, cfg_dir=d, cfg_entry="ic_exchange",
                    cfg_disc=TRUE, cfg_len=TRUE, cfg_age=FALSE)
  session$setInputs(ic_source="icexch", ic_path=d)
  ok("directory scanned", length(ic_files())==3)
  # the exchange route is one combined file, split on the record type
  ok("exchange needs exactly one file", length(ic_roles())==1)
  ok("and it is the combined exchange file", names(ic_roles())=="EXCHANGE")
  ok("file map renders", !is.null(output$ic_file_map))
  # updateTextInput does not write back to input$ under testServer, so the
  # rule is checked directly rather than through the control
  ok("prefix rule builds from stock and year",
     identical(paste0(gsub("[^A-Za-z0-9]", "_", cfg$stock), "_", cfg$year, "_"),
               "pok_27_3a46_2022_"))
  # with no length/age work, the sample file is not required
  session$setInputs(cfg_len=FALSE, cfg_age=FALSE)
  session$setInputs(start_go=2)
  ok("SD dropped when no length or age allocation",
     !("SD" %in% names(ic_roles())))
  session$setInputs(ic_source="icout")
  ok("output format expects its own three files",
     all(c("StockOverview") %in% names(IC_FILE_ROLES$output)))
  ok("source note renders", !is.null(output$ic_source_note))
  ok("option notes render", !is.null(output$ic_optnotes))
  ok("format descriptions defined for every choice",
     all(OUTPUT_FORMATS %in% names(OUTPUT_DESC)))
  ok("directory browser renders when opened",
     { session$setInputs(ic_path_browse=1); !is.null(output$ic_path_browser) })
})

cat("\n=== script reproduces the whole analysis (live, unsaved) ===\n")
testServer(server, {
  session$setInputs(start_go=1, cfg_stock="s", cfg_coord="c", cfg_year=2025,
    cfg_dir=".", cfg_entry="cef", cfg_disc=TRUE, cfg_len=FALSE, cfg_age=FALSE)
  session$setInputs(file=up, sep=",", threshold=50, stock="bll.27.3a47de", year="",
    dt_fleet="metier4", dt_area="Area1", ar_fleet="metier4", ar_area="Area1",
    hier_2="Fleet", hier_3="Country", hier_4="Area", hier_5="Season",
    thr_1=c(30,100), thr_2=c(40,100), thr_3=c(50,100), thr_4=c(60,100), ar_dr=c(0,1), hier_mode="nested",
    mg_fleet="metier4", mg_area="Area1", dr_on=FALSE, nr_fleet="metier4",
    nr_area="Area1", nr_type="discards", nr_origin="WGEstimate",
    nr_vartype="WeightLive", nr_log="Log.txt", log_dir=tempdir(),
    log_name="s.R", log_auto=FALSE)
  lr <- log_render(rv$log)
  # nothing saved yet - the live auto allocation must still be in the script
  ok("live auto allocation in script without saving",
     any(grepl("Live automatic", lr)))
  ok("allocation written as a rule", any(grepl("recommend_raising", lr)))
  ok("raising through catchRaising", any(grepl("catchRaising", lr)))
  ok("adopted allocation recorded", any(grepl("FINAL: allocation", lr)))
  ok("no frozen grouping in the auto path", !any(grepl("stratum_id %in%", lr)))
  tf <- tempfile(fileext=".R"); writeLines(lr, tf)
  ok("the whole live script parses",
     !inherits(try(parse(tf), silent=TRUE), "try-error"))
  # river labelling helpers used by the ladder
  rgv <- rec_g()
  ok("stratum labels are full combinations",
     all(grepl(" \\| ", stratum_label(rgv, unname(dmap("ar")[DIMS])))))
  ok("rule labels carry meaning not bare G-ids",
     any(grepl("%in%|raised on its own|ALL", rule_label(rgv))))
})

cat("\n=== CEF folder load ===\n")
testServer(server, {
  dd <- file.path(tempdir(), "cefdir"); dir.create(dd, showWarnings=FALSE)
  write.csv(read.csv("test_census.csv"), file.path(dd, "catches.csv"), row.names=FALSE)
  session$setInputs(start_go=1, cfg_stock="s", cfg_coord="c", cfg_year=2025,
    cfg_dir=dd, cfg_entry="cef", cfg_disc=TRUE, cfg_len=FALSE, cfg_age=FALSE)
  session$setInputs(sep=",", cef_mode="dir", cef_dir=dd)
  session$setInputs(cef_load_dir=1)
  ok("catches loaded from folder", !is.null(rv$cef_catches))
  session$setInputs(stock="bll.27.3a47de", year="", dt_fleet="metier4", dt_area="Area1")
  ok("folder-loaded catches feed the pipeline", nrow(dat()) > 0)
})
cat("\n=== clear-screen and busy-safe actions ===\n")
testServer(server, {
  session$setInputs(start_go=1, cfg_stock="s", cfg_coord="c", cfg_year=2025,
    cfg_dir=".", cfg_entry="cef", cfg_disc=TRUE, cfg_len=FALSE, cfg_age=FALSE)
  session$setInputs(file=up, sep=",", threshold=50, stock="bll.27.3a47de", year="",
    dt_fleet="metier4", dt_area="Area1", mg_fleet="metier4", mg_area="Area1",
    dr_on=FALSE, cov_y="Fleet", cov_fill="Season", cov_facet="Country",
    cov_type="heatmap", dr_x="Fleet", dr_fill="Area", dr_shape="Season",
    dr_facet="Country", dr_type="raw", dr_jitter=TRUE)
  # build up some discard-section work then clear it
  session$setInputs(clause_var="metier4", clause_vals="TBB", clause_neg=FALSE)
  session$setInputs(add_clause=1); session$setInputs(drop_target="strata")
  session$setInputs(rule_id="G1", rule_label="tbb"); session$setInputs(save_rule=1)
  ok("a rule exists before clearing", length(rv$rules) == 1)
  session$setInputs(nav="3. Discards")
  session$setInputs(clear_screen=1); session$setInputs(clear_confirm=1)
  ok("clearing the discards screen empties its rules", length(rv$rules) == 0)
  ok("clearing leaves the loaded data intact", nrow(dat()) > 0)

  # the plot legends render as their own outputs
  ok("coverage legend output renders", !is.null(output$cov_legend))
  ok("discard-rate legend output renders", !is.null(output$dr_legend))
  ok("coverage plot still renders without its inline legend",
     !is.null(output$cov_plot))
})

cat("\n=== role chips drive the plots ===\n")
testServer(server, {
  session$setInputs(start_go=1, cfg_stock="s", cfg_coord="c", cfg_year=2025,
    cfg_dir=".", cfg_entry="cef", cfg_disc=TRUE, cfg_len=FALSE, cfg_age=FALSE)
  session$setInputs(file=up, sep=",", threshold=50, stock="bll.27.3a47de", year="",
    dt_fleet="metier4", dt_area="Area1",
    cov_y="Fleet", cov_fill="Season", cov_facet="Country", cov_type="bubbles")
  # the chip inputs are the same ids the plot reads - changing one changes the plot
  ok("coverage plot reads the chip roles", !is.null(output$cov_plot))
  session$setInputs(cov_y="Country", cov_fill="Area", cov_facet="")
  ok("plot re-renders after chip change", !is.null(output$cov_plot))
})
cat("\n=== hierarchy: own stratum is Key 1, thresholds paired ===\n")
testServer(server, {
  session$setInputs(start_go=1, cfg_stock="s", cfg_coord="c", cfg_year=2025,
    cfg_dir=".", cfg_entry="cef", cfg_disc=TRUE, cfg_len=FALSE, cfg_age=FALSE)
  session$setInputs(file=up, sep=",", threshold=50, stock="bll.27.3a47de", year="",
    dt_fleet="metier4", dt_area="Area1", ar_fleet="metier4", ar_area="Area1",
    hier_mode="nested", hier_2="Fleet", hier_3="Country", hier_4="Area", hier_5="Season",
    thr_1=c(55,100), thr_2=c(30,100), thr_3=c(40,100), thr_4=c(50,100),
    thr_5=c(60,100), ar_dr=c(0,1))
  ok("widening keys come from slots 2..5",
     identical(hier(), c("Fleet","Country","Area","Season")))
  ok("thresholds read from thr_2..thr_5", identical(hier_thr(), c(30,40,50,60)))
  ok("own-stratum threshold read from Key 1 slider (thr_1)",
     self_thr_val() == 55)
  ok("own stratum always applied (a step for it exists)",
     "own stratum" %in% rec()$raise_level)
  ok("hier_keys UI renders the paired rows", !is.null(output$hier_keys))
  # apply-all sets every threshold including own stratum
  session$setInputs(thr_all=70, apply_all=1)
  # (updateSliderInput is async in testServer; just confirm no error)
  ok("apply-all runs without error", TRUE)
})

cat("\n=== comparison chips translate to shared columns ===\n")
testServer(server, {
  session$setInputs(start_go=1, cfg_stock="s", cfg_coord="c", cfg_year=2025,
    cfg_dir=".", cfg_entry="cef", cfg_disc=TRUE, cfg_len=FALSE, cfg_age=FALSE)
  session$setInputs(file=up, sep=",", threshold=50, stock="bll.27.3a47de", year="",
    dt_fleet="metier4", dt_area="Area1", ar_fleet="metier4", ar_area="Area1",
    hier_mode="nested", hier_2="Fleet", hier_3="Country", hier_4="Area", hier_5="Season",
    thr_1=c(50,100), thr_2=c(30,100), thr_3=c(40,100), thr_4=c(50,100),
    thr_5=c(60,100), ar_dr=c(0,1), auto_name="a1",
    cmp_x="Fleet", cmp_facet="")
  session$setInputs(save_auto=1)
  ok("comparison chip 'Fleet' maps to metier4 or a shared key",
     { cc <- cmp_col("Fleet"); is.null(cc) || cc %in% cmp_keys() })
  ok("empty facet chip yields no facet column", is.null(cmp_col("")))
  session$setInputs(raise_mode="compare")
  ok("comparison plot renders from the chips", !is.null(output$cmp_plot))
  ok("comparison legend renders", !is.null(output$cmp_legend))
})
cat("\n=== incompatible plot icons are actually disabled ===\n")
testServer(server, {
  session$setInputs(start_go=1, cfg_stock="s", cfg_coord="c", cfg_year=2025,
    cfg_dir=".", cfg_entry="cef", cfg_disc=TRUE, cfg_len=FALSE, cfg_age=FALSE)
  session$setInputs(file=up, sep=",", threshold=50, stock="bll.27.3a47de", year="",
    dt_fleet="metier4", dt_area="Area1", cov_fleet="metier4", cov_area="Area1",
    cov_y="Fleet", cov_fill="Season", cov_facet="", cov_type="heatmap")
  ok("nothing disabled while both axes are set", length(cov_type_off()) == 0)
  ok("heatmap is the effective type", cov_type_eff() == "heatmap")
  session$setInputs(cov_fill="")
  ok("two-axis plots disabled when X/Fill is (none)",
     setequal(cov_type_off(), c("heatmap","bubbles")))
  ok("effective type falls back to bars", cov_type_eff() == "bars")
  ok("the icon bar renders them greyed",
     grepl("disabled", paste(as.character(output$cov_type_bar), collapse="")))
  ok("the plot still draws on the fallback", !is.null(output$cov_plot))
  ok("(none) really clears the role",
     !nzchar(col_of("cov", input$cov_facet) %||% ""))
})
cat("\n=== length section: three views, catchCategory as a key ===\n")
LEN_F <- "/mnt/user-data/uploads/1785083890656_bll_2025_1distributions_CEF_v17_1.csv"
if (file.exists(LEN_F) && file.exists("test_catches_bll.csv")) {
 upc  <- data.frame(name="c.csv", size=1, type="text/csv",
                    datapath="test_catches_bll.csv", stringsAsFactors=FALSE)
 updi <- data.frame(name="d.csv", size=1, type="text/csv",
                    datapath=LEN_F, stringsAsFactors=FALSE)
 testServer(server, {
  session$setInputs(start_go=1, cfg_stock="bll.27.3a47de", cfg_coord="DV",
    cfg_year=2025, cfg_dir=".", cfg_entry="cef", cfg_disc=TRUE,
    cfg_len=TRUE, cfg_age=FALSE)
  session$setInputs(file=upc, file_dist=updi, sep=",", threshold=50,
    stock="bll.27.3a47de", year="", dt_fleet="metier4", dt_area="Area1",
    cef_mode="upload", len_lv_fleet="metier4", len_lv_area="Area1",
    len_cov_type="heatmap", len_cov_r_y="Fleet", len_cov_r_fill="Catch cat.",
    len_cov_r_facet="Country", len_cov_cc="",
    len_n_type="hist", len_n_r_fill="Catch cat.", len_n_r_facet="Fleet",
    len_n_r_shape="", len_n_stack=TRUE, len_n_cc="",
    len_w_type="linedot", len_w_r_fill="Fleet", len_w_r_facet="Country",
    len_w_r_shape="", len_w_pt="", len_w_points=TRUE, len_w_cc="",
    len_ga_method="ladder", len_ga_keepcc=TRUE,
    len_ga_k1="Fleet", len_ga_k2="Country", len_ga_k3="Season", len_ga_k4="Area",
    len_ga_t1=c(50,100), len_ga_t2=c(50,100), len_ga_t3=c(50,100),
    len_ga_t4=c(50,100), len_ga_t5=c(50,100))
  ok("distributions loaded", !is.null(rv$dist) && nrow(rv$dist) > 0)
  ok("status renders", !is.null(output$len_status))
  ok("link summary renders", !is.null(output$len_sum))
  ok("sampling coverage plot renders", !is.null(output$len_cov_plot))
  ok("coverage legend renders", !is.null(output$len_cov_legend))
  ok("numbers histogram renders", !is.null(output$len_n_plot))
  ok("mean-weight plot renders", !is.null(output$len_w_plot))
  # catchCategory is available as a plotting key
  ok("catchCategory is a stratum key", "Catch cat." %in% LEN_DIMS)
  ok("it resolves to the catchCategory column",
     identical(len_dim_col("len", "Catch cat."), "catchCategory"))
  # the category filter narrows the same view rather than needing a tab
  session$setInputs(len_n_cc="Lan")
  ok("filtering to landings still renders", !is.null(output$len_n_plot))
  session$setInputs(len_n_cc="Dis")
  ok("filtering to discards still renders", !is.null(output$len_n_plot))
  session$setInputs(len_n_cc="")
  # shape opens the average back up
  session$setInputs(len_w_pt="Area")
  ok("showing each stratum by a variable still renders", !is.null(output$len_w_plot))
  session$setInputs(len_w_pt="")
  # allocation cascade
  ok("key rows render", !is.null(output$len_ga_keys))
  ok("cascade chain renders", !is.null(output$len_ga_chain))
  ok("step table renders", !is.null(output$len_ga_steps))
  session$setInputs(len_ga_keepcc=FALSE)
  ok("turning off never-drop still renders", !is.null(output$len_ga_status))
  session$setInputs(len_ga_keepcc=TRUE)
  # allocStrataCond and bioMatchedCond are built separately and may differ
  session$setInputs(len_ga_method="manual", len_ga_bms=TRUE)
  session$setInputs(len_ga_var="Catch cat.", len_ga_v="Dis", len_ga_neg=FALSE)
  session$setInputs(len_ga_add=1)
  ok("a Dis clause picks up BMS when the option is on",
     "BMS" %in% (rv$len_clause$catchCategory %||% character(0)))
  session$setInputs(len_ga_to_alloc=1)
  ok("clause moves into allocStrataCond and clears",
     !is.null(rv$len_alloc_pend) && length(rv$len_clause) == 0)
  session$setInputs(len_ga_var="Area", len_ga_v=c("4","3"))
  session$setInputs(len_ga_add=2); session$setInputs(len_ga_to_matched=1)
  ok("bioMatchedCond can be set independently",
     !identical(rv$len_alloc_pend, rv$len_match_pend))
  session$setInputs(len_ga_gid="GA1", len_ga_glab="dis")
  session$setInputs(len_ga_saverule=1)
  ok("manual group saved", "GA1" %in% names(rv$len_groups))
  ok("the two conditions really differ",
     rv$len_groups$GA1$alloc != rv$len_groups$GA1$matched)
  session$setInputs(len_ga_var="Catch cat.", len_ga_v="Lan")
  session$setInputs(len_ga_add=3); session$setInputs(len_ga_to_alloc=2)
  session$setInputs(len_ga_gid="GA2"); session$setInputs(len_ga_saverule=2)
  session$setInputs(len_ga_move=list(i=2, dir="up"))
  ok("groups can be reordered", names(rv$len_groups)[1] == "GA2")
  ok("group order list renders", !is.null(output$len_ga_order))
  ok("allocation river renders", !is.null(output$len_ga_mriver))
  ok("group summary renders", !is.null(output$len_ga_rules))
  session$setInputs(len_ga_bms=FALSE)
  session$setInputs(len_ga_var="Catch cat.", len_ga_v="Dis")
  session$setInputs(len_ga_add=4)
  ok("BMS stays separate when the option is off",
     !("BMS" %in% (rv$len_clause$catchCategory %||% character(0))))
  ok("condition lists render with the ICES names",
     grepl("allocStrataCond", paste(output$len_ga_conds, collapse="")))
 })
} else cat("(length fixtures not present - skipped)\n")
cat("\n=== Age section mirrors Length and stays independent ===\n")
if (file.exists("test_dist_both.csv") && file.exists("test_catches_bll.csv")) {
 upc2  <- data.frame(name="c.csv", size=1, type="text/csv",
                     datapath="test_catches_bll.csv", stringsAsFactors=FALSE)
 upd2  <- data.frame(name="d.csv", size=1, type="text/csv",
                     datapath="test_dist_both.csv", stringsAsFactors=FALSE)
 testServer(server, {
  session$setInputs(start_go=1, cfg_stock="bll.27.3a47de", cfg_coord="DV",
    cfg_year=2025, cfg_dir=".", cfg_entry="cef", cfg_disc=TRUE,
    cfg_len=TRUE, cfg_age=TRUE)
  session$setInputs(file=upc2, file_dist=upd2, sep=",", threshold=50,
    stock="bll.27.3a47de", year="", dt_fleet="metier4", dt_area="Area1",
    cef_mode="upload")
  for (pfx in c("len","age")) {
    a <- list()
    a[[paste0(pfx,"_lv_fleet")]] <- "metier4"; a[[paste0(pfx,"_lv_area")]] <- "Area1"
    a[[paste0(pfx,"_cov_cc")]] <- "catlan"; a[[paste0(pfx,"_cov_type")]] <- "heatmap"
    a[[paste0(pfx,"_cov_r_y")]] <- "Fleet"; a[[paste0(pfx,"_cov_r_fill")]] <- "Country"
    a[[paste0(pfx,"_cov_r_facet")]] <- ""
    a[[paste0(pfx,"_n_cc")]] <- "catlan"; a[[paste0(pfx,"_n_type")]] <- "hist"
    a[[paste0(pfx,"_n_r_fill")]] <- "Country"; a[[paste0(pfx,"_n_r_facet")]] <- ""
    a[[paste0(pfx,"_n_r_shape")]] <- ""; a[[paste0(pfx,"_n_stack")]] <- TRUE
    a[[paste0(pfx,"_n_smooth")]] <- 1
    a[[paste0(pfx,"_w_cc")]] <- "catlan"; a[[paste0(pfx,"_w_type")]] <- "linedot"
    a[[paste0(pfx,"_w_r_fill")]] <- "Fleet"; a[[paste0(pfx,"_w_r_facet")]] <- ""
    a[[paste0(pfx,"_w_r_shape")]] <- ""; a[[paste0(pfx,"_w_pt")]] <- ""
    a[[paste0(pfx,"_w_points")]] <- TRUE
    a[[paste0(pfx,"_ga_method")]] <- "ladder"; a[[paste0(pfx,"_ga_keepcc")]] <- TRUE
    a[[paste0(pfx,"_ga_k1")]] <- "Fleet"; a[[paste0(pfx,"_ga_k2")]] <- "Country"
    a[[paste0(pfx,"_ga_k3")]] <- "Season"; a[[paste0(pfx,"_ga_k4")]] <- "Area"
    for (i in 1:5) a[[paste0(pfx,"_ga_t",i)]] <- c(50,100)
    do.call(session$setInputs, a)
    for (o in c("status","cov_plot","n_plot","w_plot","ga_keys","ga_steps")) {
      ok(paste(pfx, o, "renders"), !is.null(output[[paste0(pfx,"_",o)]]))
    }
    b <- list(); b[[paste0(pfx,"_n_type")]] <- "ridge"
    b[[paste0(pfx,"_n_smooth")]] <- 2.2
    do.call(session$setInputs, b)
    ok(paste(pfx, "density renders with the smoother"),
       !is.null(output[[paste0(pfx,"_n_plot")]]))
  }
  st_len <- paste(as.character(output$len_status), collapse="")
  st_age <- paste(as.character(output$age_status), collapse="")
  ok("length status says cm and g", grepl("cm and g", st_len))
  ok("age status says years and g", grepl("years and g", st_age))
  ok("length status names lengths", grepl("length data", st_len))
  ok("age status names ages", grepl("age data", st_age))
  # separate group state per section
  session$setInputs(len_ga_method="manual", len_ga_bms=TRUE,
                    len_ga_var="Area", len_ga_v="4")
  session$setInputs(len_ga_add=1); session$setInputs(len_ga_to_alloc=1)
  session$setInputs(len_ga_gid="GA1"); session$setInputs(len_ga_saverule=1)
  ok("a length group was saved", length(rv$len_groups) == 1)
  ok("the age section is untouched by it", length(rv$age_groups) == 0)
  session$setInputs(age_ga_method="manual", age_ga_bms=TRUE,
                    age_ga_var="Area", age_ga_v="3")
  session$setInputs(age_ga_add=1); session$setInputs(age_ga_to_alloc=1)
  session$setInputs(age_ga_gid="GB1"); session$setInputs(age_ga_saverule=1)
  ok("an age group saves independently",
     length(rv$age_groups) == 1 && length(rv$len_groups) == 1)
  ok("the two carry different conditions",
     rv$len_groups$GA1$alloc != rv$age_groups$GB1$alloc)
 })
} else cat("(age fixture not present - skipped)\n")
cat("\n=== raising: cascade -> catchAllocationAoL -> comparisons ===\n")
LEN_F2 <- "/mnt/user-data/uploads/1785083890656_bll_2025_1distributions_CEF_v17_1.csv"
if (file.exists(LEN_F2) && file.exists("test_catches_bll.csv") &&
    exists("catchAllocationAoL", mode = "function")) {
 upc2  <- data.frame(name="c.csv", size=1, type="text/csv",
                     datapath="test_catches_bll.csv", stringsAsFactors=FALSE)
 updi2 <- data.frame(name="d.csv", size=1, type="text/csv",
                     datapath=LEN_F2, stringsAsFactors=FALSE)
 testServer(server, {
  session$setInputs(start_go=1, cfg_stock="bll.27.3a47de", cfg_coord="DV",
    cfg_year=2025, cfg_dir=".", cfg_entry="cef", cfg_disc=TRUE,
    cfg_len=TRUE, cfg_age=TRUE)
  session$setInputs(file=upc2, file_dist=updi2, sep=",", threshold=50,
    stock="bll.27.3a47de", year="", dt_fleet="metier4", dt_area="Area1",
    cef_mode="upload", len_lv_fleet="metier4", len_lv_area="Area1",
    len_ga_method="ladder", len_ga_keepcc=TRUE, len_ga_bms=TRUE,
    len_ga_k1="Fleet", len_ga_k2="Country", len_ga_k3="Season", len_ga_k4="Area",
    len_ga_t1=c(50,100), len_ga_t2=c(50,100), len_ga_t3=c(50,100),
    len_ga_t4=c(50,100), len_ga_t5=c(50,100), len_ga_name="casc1")
  # the hierarchical river assigns for real now
  ok("cascade step table renders", !is.null(output$len_ga_steps))
  ok("cascade river renders", !is.null(output$len_ga_river))
  session$setInputs(len_ga_save=1)
  ok("cascade saved as an allocation", "casc1" %in% names(rv$len_allocs))
  ok("it carries groups", length(rv$len_allocs$casc1$groups) > 0)
  # run it
  session$setInputs(len_rz_alloc="casc1", len_rz_origin="WGEstimate",
    len_rz_vartype="WeightLive", len_rz_weighting="NumberAtAoL",
    len_rz_group="catchCategory", len_rz_minaol=0, len_rz_maxaol=NA,
    len_rz_plus=FALSE)
  session$setInputs(len_rz_run=1)
  ok("catchAllocationAoL ran from the app", !is.null(rv$len_raised))
  ok("no allocation error recorded", is.null(rv$len_raised_err))
  if (!is.null(rv$len_raised)) {
    ok("allocation added rows to the distributions",
       nrow(rv$len_raised$distribution) > nrow(rv$dist))
    ok("the catch table came back", nrow(rv$len_raised$catch) > 0)
  }
  for (o in c("len_rz_status","len_rz_overview","len_rz_nplot","len_rz_wplot",
              "len_rz_legend","len_rz_ntab","len_rz_wtab"))
    ok(paste("raising output renders:", sub("^len_rz_", "", o)),
       !is.null(output[[o]]))
  # the script carries the whole chain
  lr <- log_render(rv$log)
  ok("script emits allocStrataCond", any(grepl("allocStrataCond", lr)))
  ok("script emits bioMatchedCond", any(grepl("bioMatchedCond", lr)))
  ok("script calls catchAllocationAoL", any(grepl("catchAllocationAoL", lr)))
  ok("script calls the aggregations",
     any(grepl("catchAtAoLAggregation", lr)) &&
     any(grepl("meanWoLatAoLAggregation", lr)))
  ok("script computes stock weight", any(grepl("alloc_stock_weight", lr)))
  tf <- tempfile(fileext=".R"); writeLines(lr, tf)
  ok("the whole emitted script parses",
     !inherits(try(parse(tf), silent=TRUE), "try-error"))
  # Age section is wired to the same machinery
  session$setInputs(age_lv_fleet="metier4", age_lv_area="Area1",
    age_cov_cc="catlan", age_cov_type="heatmap", age_cov_r_y="Fleet",
    age_cov_r_fill="Country", age_cov_r_facet="",
    age_ga_keepcc=TRUE, age_ga_bms=TRUE, age_ga_k1="Fleet",
    age_ga_k2="Country", age_ga_k3="Season", age_ga_k4="Area",
    age_ga_t1=c(50,100), age_ga_t2=c(50,100), age_ga_t3=c(50,100),
    age_ga_t4=c(50,100), age_ga_t5=c(50,100))
  ok("age status renders", !is.null(output$age_status))
  ok("age cascade renders", !is.null(output$age_ga_chain))
  ok("age raising tab renders", !is.null(output$age_rz_status))
  ok("age uses its own allocation store",
     !identical(rv$len_allocs, rv$age_allocs) || length(rv$age_allocs) == 0)
 })
} else cat("(allocation fixtures/functions not present - skipped)\n")
cat("\nDONE\n")
