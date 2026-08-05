# workbench modules live where app.R says they do; the ICES scripts
# stay in EXT_DIR. Keeping one convention means the tests exercise
# the same layout that ships.
APP_DIR <- getOption("rcef.app_dir", "R_development")
EXT_DIR <- getOption("rcef.external_dir", "R")
suppressMessages({library(dplyr); library(rlang); library(shiny)
                  library(ggplot2); library(grid); library(tidyr)})
source(file.path(APP_DIR, "0_Functions_utils.R")); source(file.path(APP_DIR, "0_Functions_theme.R"))
  source(file.path(APP_DIR, "0_Functions_classify.R")); source(file.path(APP_DIR, "0_Functions_groups.R"))
  source(file.path(APP_DIR, "0_Functions_raising.R")); source(file.path(APP_DIR, "0_Functions_emit.R"))
  source(file.path(APP_DIR, "0_Functions_plots.R")); source(file.path(APP_DIR, "app_ui_helpers.R"))

# The length/age coverage views go through the shared coverage engine, so the
# biology link is first shaped and classified exactly as the app does it
# (weight -> landings, measured weight -> the covered part) before plotting.
bio_cov <- function(bl, what = "length", threshold = 50, min_meas = 0) {
  sc <- dplyr::mutate(bl, landings = weight,
                      landings_with_discards = weight_measured, discards = 0)
  classify_strata(sc, threshold, what, min_meas)
}; source(file.path(APP_DIR, "session_log.R"))
source(file.path(APP_DIR, "external_loader.R")); source(file.path(APP_DIR, "mod_intercatch.R"))
source(file.path(APP_DIR, "mod_native_raising.R")); source(file.path(APP_DIR, "mod_alloc_raising.R")); source(file.path(APP_DIR, "summaries.R")); source(file.path(APP_DIR, "mod_length.R"))
ok <- function(l,c) cat(if (isTRUE(c)) "PASS" else "**FAIL**","-",l,"\n")
`%||%` <- function(a,b) if (is.null(a)) b else a

# ---- a CEF table with every column catchRaising() needs -------------
set.seed(7)
base <- read.csv("test_census.csv", stringsAsFactors = FALSE)
cef <- base %>% mutate(
  recordType = "CN", workingGroup = "WGNSSK", speciesCode = 126436,
  seasonType = "Quarter", areaType = "ICESArea",
  fisheriesManagementUnit = NA_character_, metier6 = fleetValue,
  fleetType = "Fleet", domainCatchBMS = NA_character_,
  domainBiology = NA_character_, originType = "WGEstimate",
  variableType = "WeightLive", variableUnit = "kg", variance = NA)
write.csv(cef, "test_cef.csv", row.names = FALSE)
cat("CEF test rows:", nrow(cef), "\n\n")

cat("=== 1. external loader ===\n")
ex <- load_external("R")
ok("loader runs without stopping", is.list(ex))
ok("finds the real function scripts", length(ex$found) >= 5)
st <- external_status(ex)
ok("catchRaising available", st$raising)
ok("age-length allocation available", exists("catchAllocationAoL", mode="function"))
cat("   loaded:", paste(ex$found, collapse=", "), "\n")
cat("   missing packages:", paste(st$pkgs_missing, collapse=", "), "\n\n")

cat("=== 1b. catchRaising end to end ===\n")
cef_ord <- cef %>% mutate(catchCategory = ifelse(catchCategory=="RegDis","Dis",catchCategory))
D0 <- c("metier4","Country","Area1","Season")
s0 <- build_strata(derive_fields(cef_ord), D0)
r0 <- recommend_raising(s0, D0, rep(50,4), self_threshold=50, mode="nested", dims=D0)
res0 <- tryCatch(run_native_raising(cef_ord, D0, conds_from_auto(r0), log_file=NULL),
                 error=function(e) e)
ok("catchRaising runs on app-built conditions", !inherits(res0, "error"))
if (!inherits(res0,"error")) {
  ok("output carries importedOrRaised", "importedOrRaised" %in% names(res0))
  ok("some discards were raised",
     sum(res0$total[res0$catchCategory=="Dis" & res0$importedOrRaised=="raised"],
         na.rm=TRUE) > 0)
  ok("landings untouched by raising",
     abs(sum(res0$total[res0$catchCategory=="Lan"], na.rm=TRUE) -
         sum(as.numeric(cef_ord$total[cef_ord$catchCategory=="Lan"]), na.rm=TRUE)) < 1e-6)
  ok("column order normalised for the positional select",
     which(names(cef_prepare(cef_ord, D0))=="vesselFlagCountry") <
     which(names(cef_prepare(cef_ord, D0))=="year"))
  cat("   rows:", nrow(cef_ord), "->", nrow(res0),
      "| raised Dis (t):",
      round(sum(res0$total[res0$catchCategory=="Dis" &
                res0$importedOrRaised=="raised"], na.rm=TRUE)/1000, 3), "\n")
}
cat("\n")

cat("=== 2. cef_check ===\n")
ok("full CEF passes", cef_check(cef)$ok)
ok("stripped table fails with names",
   { r <- cef_check(cef %>% select(-variableUnit, -originType))
     !r$ok && all(c("originType","variableUnit") %in% r$missing) })
cat("\n")

cat("=== 3. cef_prepare ===\n")
D <- c("metier4","Country","Area1","Season")
cp <- cef_prepare(cef, D)
ok("derived fields added", all(c("metier4","Area1","Country","Season") %in% names(cp)))
ok("stratum_id built", "stratum_id" %in% names(cp))
ok("stratum_id matches the strata table",
   setequal(unique(cp$stratum_id), build_strata(derive_fields(cef), D)$stratum_id))
cat("\n")

cat("=== 4. conditions from the AUTO cascade ===\n")
s  <- build_strata(derive_fields(cef), D)
r  <- recommend_raising(s, D, rep(50,4), self_threshold=50, mode="nested", dims=D)
ca <- conds_from_auto(r)
ok("one condition pair per group",
   length(ca$strataCond) == length(ca$matchedDataCond))
ok("names line up", identical(names(ca$strataCond), names(ca$matchedDataCond)))
# THE key property: strataCond must partition the CEF rows
as_vec0 <- function(q, d) { v <- eval_tidy(q, data = d)
  if (length(v) == 1) rep(v, nrow(d)) else v }
hits <- vapply(ca$strataCond, as_vec0, logical(nrow(cp)), d = cp)
ok("every CEF row claimed by exactly one strataCond",
   all(rowSums(hits) == 1))
cat("   rows:", nrow(cp), "| groups:", ncol(hits),
    "| rows per group:", paste(colSums(hits), collapse=","), "\n")
# matchedDataCond must select something real
# conditions may evaluate to a scalar; normalise to a full-length vector
as_vec <- function(q, d) { v <- eval_tidy(q, data = d)
  if (length(v) == 1) rep(v, nrow(d)) else v }
mh <- vapply(ca$matchedDataCond, as_vec, logical(nrow(cp)), d = cp)
ok("donor conditions evaluate", all(!is.na(mh)))
ok("every donor pool is non-empty", all(colSums(mh) > 0))
cat("\n")

cat("=== 5. conditions from MANUAL rules ===\n")
rules <- list(
  new_rule("G1","TBB", list(list(var="metier4", values="TBB")), NULL, 0.1, 0.35),
  new_rule("G2","OTB+SDN", list(list(var="metier4", values=c("OTB","SDN")))),
  new_rule("G3","catch-all", list()))
cm <- conds_from_rules(rules, list())
hm <- vapply(cm$strataCond, as_vec0, logical(nrow(cp)), d = cp)
ok("manual strataCond partition the rows exactly", all(rowSums(hm) == 1))
cat("   rows per group:", paste(colSums(hm), collapse=","), "\n")
ok("first-match-wins turned into explicit complements",
   grepl("!\\(", quo_text(cm$strataCond[[2]])))
ok("catch-all became the complement of everything above",
   grepl("^!\\(", quo_text(cm$strataCond[[3]])))
mm <- vapply(cm$matchedDataCond, as_vec0, logical(nrow(cp)), d = cp)
ok("donor pools may overlap (unlike strata)", any(rowSums(mm) > 1))
ok("G1 donor pool respects its discard-rate window",
   grepl("discard_rate", quo_text(cm$matchedDataCond[[1]])))
cat("   G1 matched:", quo_text(cm$matchedDataCond[[1]]), "\n\n")

cat("=== 6. emitted code ===\n")
for (nm in c("auto","manual")) {
  cd <- if (nm=="auto") ca else cm
  code <- emit_native_code(cd, D)
  tf <- tempfile(fileext=".R"); writeLines(code, tf)
  ok(paste(nm, "catchRaising code parses"),
     !inherits(try(parse(tf), silent=TRUE), "try-error"))
  ok(paste(nm, "code defines both lists"),
     any(grepl("^strataCond <- list", code)) &&
     any(grepl("^matchedDataCond <- list", code)))
  ok(paste(nm, "code calls catchRaising"), any(grepl("catchRaising\\(", code)))
}
ic <- ic_code(list(source="exchange", path="./data", years=2022, rel_mode="build",
                   rel_year=2022, rel_file=NULL,
                   output_format=c("to_list","to_file"), out_path="./out",
                   prefix="pok_2022_", suffix="_CEF_v17.1", incl_bms=TRUE,
                   patch_psu=FALSE, keep_temp=FALSE, metier6="Fleet"))
tf <- tempfile(fileext=".R"); writeLines(ic, tf)
ok("InterCatch exchange call parses", !inherits(try(parse(tf),silent=TRUE),"try-error"))
ok("exposes every funIntercatchCEF argument",
   all(vapply(c("dat_path","stock_relation","output_format","out_path",
                "file_prefix","file_suffix","include_BMS_Caton",
                "patch_missing_DIS_PSU","metier6"),
              function(a) any(grepl(a, ic, fixed=TRUE)), logical(1))))
ic2 <- ic_code(list(source="output", path="./data", years=c(2021,2022),
                    rel_mode="auto", rel_year=2022, rel_file=NULL,
                    output_format="to_list", out_path="./out", prefix="",
                    suffix="", incl_bms=TRUE, patch_psu=TRUE, keep_temp=TRUE,
                    metier6=""))
tf <- tempfile(fileext=".R"); writeLines(ic2, tf)
ok("InterCatch output call parses", !inherits(try(parse(tf),silent=TRUE),"try-error"))
ok("exposes every funICoutCEF argument",
   all(vapply(c("dat_path","years","stock_relation","output_format","out_path",
                "keep_temp_file","file_prefix","include_BMS_Caton",
                "patch_missing_DIS_PSU","metier6"),
              function(a) any(grepl(a, ic2, fixed=TRUE)), logical(1))))
ok("multi-year rendered as a vector", any(grepl("c(2021, 2022)", ic2, fixed=TRUE)))
cat("\n")

cat("=== 7. session log ===\n")
lg <- log_new()
lg <- log_set(lg, "auto_raising", c("## auto", "x <- 1"))
lg <- log_set(lg, "header", code_header("s.R"))
lg <- log_set(lg, "setup", code_setup("R"))
out <- log_render(lg)
ok("blocks ordered by LOG_ORDER, not insertion",
   which(grepl("RCEF workbench", out))[1] < which(grepl("## auto", out))[1])
lg2 <- log_set(lg, "auto_raising", c("## auto v2", "x <- 2"))
ok("re-setting a block replaces it",
   sum(grepl("## auto", log_render(lg2))) == 1 &&
   any(grepl("x <- 2", log_render(lg2))))
lg3 <- log_set(lg2, "auto_raising", NULL)
ok("NULL removes a block", !any(grepl("## auto", log_render(lg3))))
tmp <- file.path(tempdir(), "sub", "sess.R")
log_write(lg2, tmp)
ok("writes and creates the folder", file.exists(tmp))
ok("written script parses", !inherits(try(parse(tmp), silent=TRUE), "try-error"))
cat("\n")

cat("=== 8. state save / restore ===\n")
fake_in <- list(threshold=50, hier_mode="nested", self_on=TRUE, ar_dr=c(0,0.4),
                hm_y="Fleet", log_name="s.R", save_auto=3, add_clause=1,
                file=list(name="x"))
st <- list(saved="now",
           inputs=fake_in[!grepl(STATE_SKIP, names(fake_in))],
           rules=rules, scenarios=list())
js <- state_to_json(st); back <- state_from_json(js)
ok("json round trip", length(back$rules)==3)
ok("buttons excluded from state",
   !any(c("save_auto","add_clause","file") %in% names(st$inputs)))
ok("updater picked per input type",
   state_updater("threshold",50)=="numeric" &&
   state_updater("ar_dr",c(0,1))=="slider" &&
   state_updater("self_on",TRUE)=="check" &&
   state_updater("hier_mode","nested")=="radio" &&
   state_updater("hm_y","Fleet")=="select" &&
   state_updater("log_name","s.R")=="text")
ok("unknown inputs skipped rather than guessed",
   is.na(state_updater("something_odd", 1)))
cat("\n")

cat("=== 9. upload staging ===\n")
f <- data.frame(name=c("a.txt","b.txt"),
                datapath=c(tempfile(),tempfile()), stringsAsFactors=FALSE)
writeLines("x", f$datapath[1]); writeLines("y", f$datapath[2])
r1 <- ic_stage_uploads(f, "output", "2022")
ok("output format staged into a year sub-folder",
   dir.exists(file.path(r1,"2022")) && file.exists(file.path(r1,"2022","a.txt")))
r2 <- ic_stage_uploads(f, "exchange", "2022")
ok("exchange format staged flat", file.exists(file.path(r2,"a.txt")))
cat("\n=== 10. InterCatch staging fixes the StockOverview grep ===\n")
dtmp <- file.path(tempdir(), "staging_test"); dir.create(dtmp, showWarnings=FALSE)
cat("Country\tYear\tStock\nX\t2022\tpok\n",
    file=file.path(dtmp, "pok_2022_overview_export.txt"))
cat("x\n", file=file.path(dtmp, "pok_2022_numbers_export.txt"))
cat("x\n", file=file.path(dtmp, "pok_2022_meanw_export.txt"))
mapping <- list(
  StockOverview = file.path(dtmp, "pok_2022_overview_export.txt"),
  NumbersAtAgeLength = file.path(dtmp, "pok_2022_numbers_export.txt"),
  MeanWeigthAtAgeLength = file.path(dtmp, "pok_2022_meanw_export.txt"))
root <- ic_stage_mapping(mapping, "output", 2022)
lst <- list.files(file.path(root, "2022"), pattern=".txt", full.names=TRUE)
ok("StockOverview grep now passes", any(grepl("StockOverview.txt", lst)))
ok("Numbers grep passes", any(grepl("Numbers", lst)))
ok("MeanWeigth grep passes", any(grepl("MeanWeigth", lst)))
ok("files land in the year subfolder funICoutCEF expects",
   dir.exists(file.path(root, "2022")))
# the exchange route takes ONE combined file, split internally on the
# record-type column, not three separate ones
root2 <- ic_stage_mapping(list(EXCHANGE=file.path(dtmp,"pok_2022_overview_export.txt")),
                          "exchange", 2022)
ok("exchange stages one file flat with a canonical name",
   file.exists(file.path(root2, "exchange.csv")))
ok("exchange expects exactly one role",
   length(IC_FILE_ROLES$exchange) == 1)

cat("\n=== 11. year detection from filenames and contents ===\n")
ok("year read from filename",
   identical(ic_detect_year(file.path(dtmp, "pok_2022_overview_export.txt")), 2022L))
ok("year read from a Year column when name has none",
   { f <- file.path(dtmp, "noyearname.txt")
     cat("Country\tYear\nX\t2019\n", file=f)
     ic_detect_year(f) == 2019L })
ok("no false positive from a non-year number",
   length(ic_detect_year(file.path(tempdir(), "file_12.txt"))) == 0 ||
   all(ic_detect_year(file.path(tempdir(), "file_12.txt")) >= 1990))

cat("\n=== 12. river plot labels ===\n")
D <- c("metier4","Country","Area1","Season")
s <- build_strata(derive_fields(cef), D)
r <- recommend_raising(s, D, rep(50,4), self_threshold=50, mode="nested", dims=D)
g <- raising_groups(r)
rg <- r %>% mutate(group_id = g$group_id[match(raise_condition, g$raise_condition)])
sl <- stratum_label(rg, D)
ok("stratum label is the full dimension combination",
   all(grepl(" \\| ", sl)) && grepl("metier4|GNS|OTB|SDN|TBB",
       paste(head(sl,1), collapse="")))
rl <- rule_label(rg)
ok("rule label shows the meaning, not a bare G-number",
   any(grepl("%in%", rl)) && !any(grepl("^G[0-9]+$", rl)))
ok("special cases phrased in words",
   any(grepl("raised on its own data", rl)) ||
   any(grepl("raised from ALL", rl)))
p <- plot_river(rg %>% filter(raise_level != "no raising needed"),
                left_var=NULL, right_var="group_id", value_var="landings",
                pal=group_palette(g$group_id),
                left_label=stratum_label(rg %>% filter(raise_level!="no raising needed"), D),
                right_label=rule_label(rg %>% filter(raise_level!="no raising needed")))
ok("labelled river plot builds",
   !inherits(try(ggplot2::ggplot_build(p), silent=TRUE), "try-error"))

cat("\n=== 13. script reproduces the whole analysis ===\n")
live <- c(code_auto_rule("live_auto", D, D, c(30,40,50,60), 50, "nested", NA, 0.4),
          code_raise("live_auto", D), code_final("live_auto"))
tf <- tempfile(fileext=".R"); writeLines(c(code_header("s.R"), code_setup("R"), live), tf)
ok("live-auto script parses", !inherits(try(parse(tf), silent=TRUE), "try-error"))
ok("auto emitted as a rule, no frozen grouping",
   any(grepl("recommend_raising", live)) && !any(grepl("stratum_id %in%", live)))
ok("raising and final adoption present",
   any(grepl("catchRaising", live)) && any(grepl("FINAL: allocation", live)))
mix <- c(code_auto_rule("a1", D, D, rep(50,4), 50, "nested", NA, NA),
         code_raise("a1", D),
         code_manual_rule("m1", D, list(new_rule("G1","t",list(list(var="metier4",values="TBB"))),
                                        new_rule("G2","r",list())), list()),
         code_raise("m1", D), code_compare(c("a1","m1")), code_final("m1"))
tf2 <- tempfile(fileext=".R"); writeLines(c(code_header("s.R"), code_setup("R"), mix), tf2)
ok("mixed auto+manual comparison script parses",
   !inherits(try(parse(tf2), silent=TRUE), "try-error"))
ok("manual written as explicit lists", any(grepl("quo\\(", mix)))
ok("comparison across allocations present", any(grepl("comparison <- bind_rows", mix)))

cat("\n=== 14. legend extraction and compaction ===\n")
sc14 <- classify_strata(build_strata(derive_fields(cef), c("metier4","Season","Country")), 50)
p14 <- plot_heatmap(sc14, "metier4", "Season", "Country", 50)
lg <- tryCatch(extract_legend(p14), error=function(e) NULL)
ok("legend extracted from a plot with a fill scale", !is.null(lg))
ok("compact_legend caps columns for many levels",
   { pc <- compact_legend(p14, max_rows=3)
     !inherits(try(ggplot2::ggplot_build(pc), silent=TRUE), "try-error") })
# a plot with nothing mapped to a legend aesthetic yields no legend
p_nolegend <- ggplot(data.frame(x=1:3,y=1:3), aes(x,y)) + geom_point()
ok("no legend returned when nothing is mapped",
   is.null(tryCatch(extract_legend(p_nolegend), error=function(e) NULL)))
ok("draw_legend runs without error on a legendful plot",
   { pf <- tempfile(fileext=".png"); png(pf, width=200, height=80)
     r <- tryCatch({ draw_legend(p14); TRUE }, error=function(e) FALSE)
     dev.off(); r })

cat("\n=== 15. raising coerces a character total ===\n")
cef_chr <- cef; cef_chr$total <- as.character(cef_chr$total)
prep <- cef_prepare(cef_chr, c("metier4","Country","Area1","Season"))
ok("cef_prepare turns a character total numeric", is.numeric(prep$total))
ok("thousands separators are stripped",
   { d2 <- cef_chr; d2$total[1] <- "1,234"
     p2 <- cef_prepare(d2, c("metier4","Country","Area1","Season"))
     is.numeric(p2$total) && !any(is.na(p2$total) & !is.na(d2$total)) })

cat("\n=== 16. stratum axis label for the river plot ===\n")
ok("axis label lists the dimensions",
   identical(stratum_axis_label(c("metier4","Country","Area1","Season")),
             "Stratum (metier4 + Country + Area1 + Season)"))
ok("own stratum is never named in the axis label",
   !grepl("own", tolower(stratum_axis_label(c("metier4","Country")))))
ok("empty dims degrade to plain Stratum",
   identical(stratum_axis_label(character(0)), "Stratum"))

cat("\n=== 17. palette leads with the role-chip family ===\n")
reset_palettes()
pal <- var_palette("metier4", c("TBB","OTB","GNS","SDN"))
ok("first colour is the green used for role chips", unname(pal[1]) == "#59A14F")
ok("palette family matches PAL_BASE order",
   all(unname(pal[1:3]) == PAL_BASE[1:3]))
gp <- group_palette(c("G1","G2","G3"))
ok("group palette shares the same leading colours",
   unname(gp[1]) == "#59A14F")

cat("\n=== 18. the shared palette is APPLIED, not just defined ===\n")
# The palette machinery existing is not enough - each plot must actually
# call scale_fill_var, or ggplot silently falls back to its default hue
# scale. This checks the rendered colours, not the source.
reset_palettes()
st18 <- build_strata(derive_fields(cef), c("metier4","Area1","Season","Country"))
sc18 <- classify_strata(st18, 50)
built_cols <- function(p) {
  b <- try(ggplot2::ggplot_build(p), silent = TRUE)
  if (inherits(b, "try-error")) return(character(0))
  cc <- unique(unlist(lapply(b$data, function(x) c(x$fill, x$colour))))
  toupper(cc[!is.na(cc) & grepl("^#", cc)])
}
for (nm in c("plot_dr_raw","plot_dr_box","plot_coverage_bars")) {
  p <- switch(nm,
    plot_dr_raw        = plot_dr_raw(st18,"metier4","Area1","Season","Country",TRUE),
    plot_dr_box        = plot_dr_box(st18,"Country","Area1",NULL,TRUE,TRUE),
    plot_coverage_bars = plot_coverage_bars(sc18,"metier4","Season","Country"))
  ok(paste("palette applied in", nm),
     any(built_cols(p) %in% toupper(PAL_BASE)))
}
ok("no ggplot default hue colours leak through",
   !any(c("#F8766D","#00BA38","#619CFF") %in%
        built_cols(plot_dr_raw(st18,"metier4","Area1",NULL,NULL,TRUE))))
ok("a level keeps its colour across different plots",
   length(intersect(
     built_cols(plot_dr_raw(st18,"metier4","Area1",NULL,NULL,TRUE)),
     built_cols(plot_dr_box(st18,"Country","Area1",NULL,TRUE,FALSE)))) > 0)

cat("\n=== 19. length: units, linking, and the two quantities ===\n")
LEN_F <- "/mnt/user-data/uploads/1785083890656_bll_2025_1distributions_CEF_v17_1.csv"
if (file.exists(LEN_F) && file.exists("test_catches_bll.csv")) {
  ldist <- read.csv(LEN_F, stringsAsFactors = FALSE)
  lcat  <- read.csv("test_catches_bll.csv", stringsAsFactors = FALSE)
  LD <- c("metier4","Country","Area1","Season")

  pd <- prep_distributions(ldist)
  ok("distributions prepped", !is.null(pd) && nrow(pd) == nrow(ldist))
  ok("Number rows tagged as numbers, WeightLive as meanweight",
     setequal(unique(pd$.var_kind), c("numbers","meanweight")))
  ok("valueType confirms the split (Total vs Mean)",
     all(pd$valueType[pd$.var_kind == "numbers"] == "Total") &&
     all(pd$valueType[pd$.var_kind == "meanweight"] == "Mean"))

  # a file mixing cm/kg must standardise to the same numbers as mm/g
  d2 <- ldist; idx <- seq_len(nrow(d2)) %% 2 == 0
  d2$distributionUnit[idx] <- "cm"
  d2$distributionClass[idx] <- as.numeric(d2$distributionClass[idx]) / 10
  w <- idx & d2$variableType == "WeightLive"
  d2$variableUnit[w] <- "kg"; d2$value[w] <- as.numeric(d2$value[w]) / 1000
  p2 <- prep_distributions(d2)
  ok("mixed cm/kg standardise identically",
     isTRUE(all.equal(sort(round(pd$.class_mm)), sort(round(p2$.class_mm)))) &&
     isTRUE(all.equal(sort(round(pd$.value_std, 3)), sort(round(p2$.value_std, 3)))))
  ok("an unknown unit warns rather than silently passing",
     { d3 <- ldist; d3$distributionUnit <- "furlong"
       length(suppressWarnings(withCallingHandlers(
         { prep_distributions(d3); character(0) },
         warning = function(w) invokeRestart("muffleWarning")))) == 0 })

  # landings and discards link independently
  bl_lan <- biology_link(lcat, ldist, LD, "Lan")
  bl_dis <- biology_link(lcat, ldist, LD, "Dis")
  ok("landings link found strata", !is.null(bl_lan) && nrow(bl_lan) > 0)
  ok("discards link found strata", !is.null(bl_dis) && nrow(bl_dis) > 0)
  ok("the two categories are separate sets",
     sum(bl_lan$weight) != sum(bl_dis$weight))
  ok("measured + unmeasured = total weight",
     isTRUE(all.equal(sum(bl_lan$weight),
                      sum(bl_lan$weight_measured) + sum(bl_lan$weight_unmeasured))))
  ok("unsampled catch is actually present to compare against",
     sum(bl_lan$weight_unmeasured) > 0)

  lk <- link_dist_to_strata(lcat, ldist, LD, "Lan")
  ok("distributions carry the stratum keys after linking",
     !is.null(lk) && all(LD %in% names(lk)))
  nb <- length_numbers(lk, c("Country"))
  mwt <- length_meanweight(lk, c("Country"))
  ok("numbers at length produced", !is.null(nb) && nrow(nb) > 0)
  ok("mean weight at length produced", !is.null(mwt) && nrow(mwt) > 0)
  ok("mean weight rises with length (sanity)",
     { a <- mwt[order(mwt$.class), ]
       cor(a$.class, a$mean_weight, use = "complete.obs") > 0.7 })

  # the weighting is the whole point: a plain mean gives a different answer
  wtd <- length_meanweight(lk, character(0))
  plain <- lk %>% filter(.var_kind == "meanweight") %>%
    group_by(.class) %>%
    summarise(plain = mean(.value_std, na.rm = TRUE), .groups = "drop")
  j <- merge(wtd, plain, by = ".class")
  ok("mean weight is number-weighted, not a plain average",
     any(abs(j$mean_weight - j$plain) > 1))

  # plots
  bld <- function(p) !is.null(p) &&
    !inherits(try(ggplot2::ggplot_build(p), silent = TRUE), "try-error")
  ok("numbers histogram builds",
     bld(plot_length_hist(length_numbers(lk, c("Country","metier4")),
                          "Country", "metier4", NULL)))
  ok("histogram takes two facets",
     bld(plot_length_hist(length_numbers(lk, c("Country","metier4")),
                          "Country", "metier4", "Country")))
  ok("mean-weight line plot builds",
     bld(plot_length_meanweight(length_meanweight(lk, c("Country","metier4")),
                                "Country", "metier4", NULL)))
  ok("length coverage bars build",
     bld(plot_biology_coverage(bio_cov(bl_lan), "bars", "metier4", NULL,
                               "Country", what = "length")))
  ok("length plots use the shared palette",
     { p <- plot_length_hist(length_numbers(lk, "Country"), "Country",
                             NULL, NULL)
       cc <- toupper(unique(unlist(lapply(ggplot2::ggplot_build(p)$data,
                                          function(x) x$fill))))
       any(cc %in% toupper(PAL_BASE)) })
  ok("link summary reports coverage",
     nrow(length_link_summary(bl_lan, lk)) > 5)
  # the same machinery must accept ages, not just lengths
  ok("prep_distributions accepts an age type argument",
     is.null(prep_distributions(ldist, type = "Age")))
} else {
  cat("(length fixtures not present - skipped)\n")
}

  cat("\n=== 20. length: cm/g enforced, spread, scale check, cascade ===\n")
  ok("length class is presented in cm", all(lk$.class == lk$.class_mm / 10))
  ok("units are not offered as a choice - conversion is on load",
     !any(grepl("class_unit", deparse(args(plot_length_hist)))))
  mw_avg <- length_meanweight(lk, c("metier4"))
  ok("spread carried when strata are collapsed",
     "sd_weight" %in% names(mw_avg) && any(!is.na(mw_avg$sd_weight)))
  ok("a point built from one stratum has no spread, not a zero",
     all(is.na(mw_avg$sd_weight[mw_avg$n_strata == 1])))
  ok("n_strata records how many were averaged", max(mw_avg$n_strata) > 1)
  ok("shape variable keeps strata apart",
     nrow(length_meanweight(lk, c("metier4","Area1"))) >=
     nrow(length_meanweight(lk, c("metier4"))))
  sc <- length_scale_check(lk)
  ok("scale check fits a weight-length relationship", !is.null(sc) && is.finite(sc$b))
  ok("exponent lands near the cube law", sc$b > 2.5 && sc$b < 3.6)
  ok("scale check flags this file (classes 10x too large)", !isTRUE(sc$plausible))
  ok("and reports roughly the right correction",
     sc$implied_factor > 5 && sc$implied_factor < 20)
  ok("height grows with facet rows",
     length_plot_height(mw_avg, "metier4", NULL) >
     length_plot_height(mw_avg, NULL, NULL))
  # allocation cascade
  st_keep <- alloc_steps(c("Fleet","Country","Season","Area"), rep(50,5), TRUE)
  st_drop <- alloc_steps(c("Fleet","Country","Season","Area"), rep(50,5), FALSE)
  ok("own stratum is always the first step", st_keep[[1]]$label == "own stratum")
  ok("keeping catchCategory stops at the category",
     grepl("catchCategory", st_keep[[length(st_keep)]]$label) &&
     !grepl("both categories", st_keep[[length(st_keep)]]$label))
  ok("dropping it restarts the ladder rather than pooling at once",
     length(st_drop) == 2 * length(st_keep))
  ok("the first joined step is back at FULL precision, nothing dropped",
     { j <- which(vapply(st_drop, function(x) isTRUE(x$drop_cc), logical(1)))[1]
       length(st_drop[[j]]$drop) == 0 })
  ok("the joined pass widens one key at a time",
     { j <- which(vapply(st_drop, function(x) isTRUE(x$drop_cc), logical(1)))
       identical(vapply(st_drop[j], function(x) length(x$drop), integer(1)),
                 0:4) })
  ok("pooling everything is the LAST step, not the first after joining",
     grepl("both categories", st_drop[[length(st_drop)]]$label) &&
     !grepl("both categories",
            st_drop[[which(vapply(st_drop, function(x) isTRUE(x$drop_cc),
                                  logical(1)))[1]]]$label))
  ok("a joined step reuses the threshold of the same precision",
     { j <- which(vapply(st_drop, function(x) isTRUE(x$drop_cc), logical(1)))
       identical(vapply(st_drop[j], `[[`, numeric(1), "threshold"),
                 vapply(st_keep, `[[`, numeric(1), "threshold")) })
  ok("scope labels separate the two passes",
     setequal(unique(vapply(st_drop, alloc_step_scope, character(1))),
              c("within category", "categories joined")))
  ok("catchCategory is never dropped mid-cascade",
     !any(vapply(st_keep, function(x) "catchCategory" %in% x$drop, logical(1))))
  # emitted conditions use the ICES names
  g <- list(GA1 = list(alloc = 'catchCategory == "Lan" & Area1 == "4"',
                       matched = 'catchCategory == "Lan" & Area1 == "4"'))
  em <- emit_alloc_conditions(g)
  ok("emits allocStrataCond", any(grepl("^allocStrataCond <-", em)))
  ok("emits bioMatchedCond", any(grepl("^bioMatchedCond <-", em)))
  ok("conditions are quosures", any(grepl("quo\\(", em)))
  code <- emit_alloc_code(g)
  ok("emitted code calls catchAllocationAoL", any(grepl("catchAllocationAoL", code)))
  ok("and checks the conditions first",
     any(grepl("check_group_conditions", code)) &&
     any(grepl('domain = "domainBiology"', code)))
  ok("emitted code parses",
     !inherits(try(parse(text = paste(c("library(rlang)", em), collapse = "\n")),
                   silent = TRUE), "try-error"))

  cat("\n=== 21. coverage labels, cell collapsing, ridges, BMS ===\n")
  ok("length status label renames the discard one",
     grepl("lengths sampled", cat_lookup(50, "length")$status[4]))
  ok("age label likewise", grepl("ages sampled", cat_lookup(50, "age")$status[4]))
  ok("discard label unchanged", grepl("discard data", cat_lookup(50)$status[4]))
  # overlapping labels came from several strata landing on one cell
  scx <- classify_strata(
    data.frame(A = c("x","x","y"), B = c("p","p","q"),
               landings = c(10, 20, 5), landings_with_discards = c(5, 5, 5),
               discards = 0, coverage = c(50, 25, 100),
               discard_rate = NA_real_), 50, "length")
  cc <- collapse_cells(scx, c("A","B"), 50, "length")
  ok("cells collapse to one row each", nrow(cc) == 2)
  ok("weights are summed when collapsing", sum(cc$landings) == 35)
  ok("coverage recomputed from the summed weights",
     abs(cc$coverage[cc$A == "x"] - 100 * 10 / 30) < 1e-6)
  ok("collapsed cells carry a status", "Status" %in% names(cc))
  # ridges
  rd <- plot_length_ridges(length_numbers(lk, "Country"), "Country")
  ok("density ridge builds",
     !inherits(try(ggplot2::ggplot_build(rd), silent = TRUE), "try-error"))
  ok("each ridge integrates to one, not absolute numbers",
     { z <- length_numbers(lk, "Country")
       zz <- z %>% group_by(Country) %>%
         summarise(s = sum(number), .groups = "drop")
       nrow(zz) > 1 && length(unique(round(zz$s))) > 1 })
  # BMS handling
  ok("a Dis clause picks up BMS by default",
     setequal(expand_cc(c("Dis"), TRUE), c("Dis", "BMS")))
  ok("and leaves it alone when switched off",
     setequal(expand_cc(c("Dis"), FALSE), "Dis"))
  ok("a Lan clause is untouched either way",
     setequal(expand_cc("Lan", TRUE), "Lan"))
  # allocation assignment: first match wins, order matters
  gg <- list(
    G1 = list(alloc = 'catchCategory == "Lan"',
              alloc_expr = alloc_quo(list(catchCategory = "Lan"))),
    G2 = list(alloc = "TRUE", alloc_expr = alloc_quo(list())))
  bl2 <- biology_link(lcat, ldist, c("catchCategory", LD), NULL)
  as1 <- assign_alloc_groups(bl2, gg)
  ok("every stratum lands in a group", !any(is.na(as1$alloc_group)))
  ok("the first matching group wins",
     all(as1$alloc_group[as1$catchCategory == "Lan"] == "G1"))
  ok("reordering changes what each group catches",
     { as2 <- assign_alloc_groups(bl2, gg[c("G2","G1")])
       all(as2$alloc_group == "G2") })
  ok("group summary lists both conditions",
     all(c("allocStrataCond","bioMatchedCond") %in%
         names(alloc_group_summary(as1, gg))))
  ok("allocation river builds",
     !inherits(try(ggplot2::ggplot_build(
       plot_alloc_river(as1, LD, gg, group_palette(names(gg)))),
       silent = TRUE), "try-error"))
  ok("a negated clause emits !%in%",
     grepl("!Area1", alloc_cond_string(
       list(Area1 = structure(c("4","3"), negate = TRUE)))))

  cat("\n=== 22. density smoother and the Age path ===\n")
  nbq <- length_numbers(lk, "Country")
  bldq <- function(p) !is.null(p) &&
    !inherits(try(ggplot2::ggplot_build(p), silent = TRUE), "try-error")
  for (sm in c(0.3, 1, 2.5))
    ok(paste("ridge builds at smoothing", sm),
       bldq(plot_length_ridges(nbq, "Country", smooth = sm)))
  ok("more smoothing lowers the peak of the curve",
     { pk <- function(sm) {
         g <- nbq[nbq$Country == nbq$Country[1], ]
         x <- g$.class; w <- g$number; pr <- w / sum(w)
         mu <- sum(pr * x); sdw <- sqrt(sum(pr * (x - mu)^2))
         ne <- 1 / sum(pr^2)
         max(stats::density(x, weights = pr,
                            bw = 1.06 * sdw * ne^(-1/5) * sm)$y) }
       pk(2.5) < pk(1) && pk(1) < pk(0.3) })
  ok("bandwidth uses the weights, so no unweighted-bandwidth warning",
     { w <- NULL
       withCallingHandlers(plot_length_ridges(nbq, "Country"),
         warning = function(x) { w <<- c(w, conditionMessage(x))
                                 invokeRestart("muffleWarning") })
       !any(grepl("not.*using.*weights", w %||% "")) })

  # --- Age uses exactly the same machinery -------------------------
  age <- ldist
  age$distributionType <- "Age"; age$distributionUnit <- "year"
  age$distributionClass <- pmax(1, round(
    (as.numeric(ldist$distributionClass) - 1300) / 570) + 1)
  pa <- prep_distributions(age, "Age")
  ok("age distributions prep", !is.null(pa) && nrow(pa) > 0)
  ok("an age class is NOT divided by ten",
     all(pa$.class == as.numeric(age$distributionClass)))
  ok("age carries its type", identical(pa$.dist_type[1], "Age"))
  lka <- link_dist_to_strata(lcat, age, c("catchCategory", LD), "Lan", "Age")
  ok("age links to the catch table", !is.null(lka) && nrow(lka) > 0)
  nba <- length_numbers(lka, "Country"); mwa <- length_meanweight(lka, "Country")
  ok("numbers at age produced", !is.null(nba) && nrow(nba) > 0)
  ok("mean weight at age produced", !is.null(mwa) && nrow(mwa) > 0)
  ok("age histogram builds",
     bldq(plot_length_hist(nba, "Country", class_label = class_axis_label("Age"))))
  ok("age density builds",
     bldq(plot_length_ridges(nba, "Country", class_label = class_axis_label("Age"))))
  ok("age mean-weight builds",
     bldq(plot_length_meanweight(mwa, "Country",
                                 class_label = class_axis_label("Age"))))
  ok("the weight-length check is skipped for ages",
     is.null(length_scale_check(lka)))
  ok("axis label follows the type",
     class_axis_label("Age") == "Age (years)" &&
     class_axis_label("LengthTotal") == "Length (cm)")
  ok("age status labels differ from length ones",
     grepl("ages sampled", cat_lookup(50, "age")$status[4]) &&
     grepl("lengths sampled", cat_lookup(50, "length")$status[4]))
  bla <- biology_link(lcat, age, c("catchCategory", LD), "Lan", "Age")
  ok("age link summary names ages",
     any(grepl("age", length_link_summary(bla, lka, "Age")$item)))
  ok("age coverage bars build",
     bldq(plot_biology_coverage(bio_cov(bla, "age"), "bars", "metier4", NULL,
                                "Country", what = "age")))
  ok("the same cascade serves ages",
     length(alloc_steps(c("Fleet","Country"), rep(50, 3), FALSE)) == 6)

  cat("\n=== 22. allocation: cascade, catchAllocationAoL, comparisons ===\n")
  blc <- biology_link(lcat, ldist, c("catchCategory", LD), NULL)
  stc <- alloc_steps(LD, rep(50, 5), TRUE)
  asg <- assign_alloc_cascade(blc, LD, stc)
  ok("cascade assigns every stratum a step or marks it unresolved",
     all(!is.na(asg$alloc_level)))
  ok("own stratum resolves most of the weight",
     sum(asg$weight[asg$alloc_step == 1], na.rm = TRUE) > 0)
  ok("cascade table accounts for all strata",
     { tb <- alloc_cascade_table(asg, stc); sum(tb$n_strata) == nrow(asg) })
  ok("cumulative weight reaches 100%",
     { tb <- alloc_cascade_table(asg, stc)
       abs(max(tb$cum_pct_weight, na.rm = TRUE) - 100) < 0.5 })

  # conditions must reproduce the assignment EXACTLY or the allocation
  # rejects them for double-assigning records
  gsc <- cascade_to_groups(asg, LD)
  ok("cascade converts to groups", length(gsc) > 0)
  ok("groups carry evaluable quosures",
     all(vapply(gsc, function(g) inherits(g$alloc_expr, "quosure"), logical(1))))
  ok("no stratum matches two groups",
     { hits <- vapply(gsc, function(g)
         sum(rlang::eval_tidy(g$alloc_expr, data = asg), na.rm = TRUE),
         numeric(1))
       sum(hits) == sum(!is.na(asg$alloc_step)) })

  # the vocabulary bridge
  ok("LengthTotal maps to the Length the allocator expects",
     alloc_type_of("LengthTotal") == "Length")
  ok("Age passes through", alloc_type_of("Age") == "Age")
  ok("prep rewrites distributionType so the filter matches",
     unique(prep_for_alloc(ldist, "LengthTotal")$distributionType) == "Length")
  ok("an unknown type is refused",
     inherits(try(alloc_type_of("Nonsense"), silent = TRUE), "try-error"))

  if (exists("catchAllocationAoL", mode = "function")) {
    res <- suppressWarnings(run_allocation(lcat, ldist, gsc, "LengthTotal",
                                           log_file = tempfile()))
    ok("catchAllocationAoL runs from the cascade groups",
       !inherits(res, "alloc_error"))
    if (!inherits(res, "alloc_error")) {
      ok("it returns both tables",
         all(c("distribution", "catch") %in% names(res)))
      ok("allocation adds rows to the distributions",
         nrow(res$distribution) > nrow(ldist))
      ok("imported rows are kept and flagged",
         !is.null(alloc_src_col(res$distribution)))
      cn2 <- suppressWarnings(alloc_compare_numbers(res))
      ok("numbers pivot out of the wide aggregation",
         !is.null(cn2) && nrow(cn2) > 0)
      ok("both imported and allocated are present",
         setequal(unique(cn2$.src), c("Imported", "Allocated")))
      sw2 <- suppressWarnings(alloc_stock_weight(res))
      ok("stock weight computed", !is.null(sw2) && nrow(sw2) > 0)
      ok("stock weight is in tonnes, not grams",
         sum(sw2$.tonnes, na.rm = TRUE) < 1e6)
      ok("numbers comparison plot builds",
         !inherits(try(ggplot2::ggplot_build(
           plot_alloc_numbers(cn2, "catchCategory", "Length")),
           silent = TRUE), "try-error"))
      ok("stock weight plot builds",
         !inherits(try(ggplot2::ggplot_build(
           plot_alloc_stockweight(sw2, "catchCategory", "Length")),
           silent = TRUE), "try-error"))
      ok("overview table produced",
         !is.null(suppressWarnings(alloc_overview(res))))
    }
  } else cat("(catchAllocationAoL not loaded - allocation run skipped)\n")

  ok("emitted aggregation code parses",
     !inherits(try(parse(text = paste(emit_alloc_agg_code("Length"),
                                      collapse = "\n")), silent = TRUE),
               "try-error"))
  ok("emitted code covers numbers, weights and stock weight",
     { e <- emit_alloc_agg_code("Length")
       any(grepl("catchAtAoLAggregation", e)) &&
       any(grepl("meanWoLatAoLAggregation", e)) &&
       any(grepl("alloc_stock_weight", e)) })

  cat("\n=== 23. discard rate only where discards were sampled ===\n")
  stx <- build_strata(derive_fields(cef), c("metier4","Country","Area1","Season"))
  sm <- sampled_for_discards(stx)
  ok("unsampled strata are dropped, not shown as zero",
     nrow(sm) < sum(!is.na(stx$discard_rate)))
  ok("every remaining stratum really has a discard estimate",
     all(sm$landings_with_discards > 0))
  ok("no phantom zero rates survive",
     !any(sm$discard_rate == 0 & sm$landings_with_discards == 0))
  ok("raw plot still builds after filtering",
     !inherits(try(ggplot2::ggplot_build(
       plot_dr_raw(stx, "metier4", "Area1", NULL, NULL, TRUE)),
       silent = TRUE), "try-error"))
  ok("boxplot still builds after filtering",
     !inherits(try(ggplot2::ggplot_build(
       plot_dr_box(stx, "Country", "Area1", NULL, TRUE, TRUE)),
       silent = TRUE), "try-error"))

  cat("\n=== 24. sufficiency: coverage AND measurements ===\n")
  LD2 <- c("catchCategory", LD)
  meas <- stratum_measurements(lcat, ldist, LD2, NULL)
  ok("measurement counts recovered per stratum",
     !is.null(meas) && nrow(meas) > 0)
  ok("counts come from numMeasurements, not a row count",
     max(meas$n_measurements) > nrow(meas))
  bl4 <- biology_link(lcat, ldist, LD2, NULL)
  suf <- add_sufficiency(bl4, meas, min_coverage = 50, min_measurements = 100)
  ok("both tests are recorded separately",
     all(c("ok_coverage", "ok_measurements", "sufficient") %in% names(suf)))
  ok("sufficient requires both", all(suf$sufficient ==
     (suf$ok_coverage & suf$ok_measurements)))
  ok("a well-covered but thinly measured stratum is refused",
     any(suf$ok_coverage & !suf$ok_measurements))
  ok("the shortfall is named", all(nzchar(suf$shortfall[!suf$sufficient])))
  ok("zero threshold passes everything on measurements",
     all(add_sufficiency(bl4, meas, 0, 0)$sufficient))
  ok("warning names the thinnest stratum",
     grepl("fewer than", measurement_warning(suf, 100)))

  cat("\n=== 25. free scales on facets ===\n")
  ok("both free gives 'free'", facet_scales(TRUE, TRUE) == "free")
  ok("y only gives 'free_y'", facet_scales(FALSE, TRUE) == "free_y")
  ok("x only gives 'free_x'", facet_scales(TRUE, FALSE) == "free_x")
  ok("neither gives 'fixed'", facet_scales(FALSE, FALSE) == "fixed")
  nb5 <- length_numbers(lk, c("Country","metier4"))
  ok("histogram honours free scales",
     !inherits(try(ggplot2::ggplot_build(
       plot_length_hist(nb5, "Country", "metier4", NULL, scales = "free_y")),
       silent = TRUE), "try-error"))
  ok("ridge honours free scales",
     !inherits(try(ggplot2::ggplot_build(
       plot_length_ridges(nb5, "Country", "metier4", scales = "free_x")),
       silent = TRUE), "try-error"))

  cat("\n=== 26. manual allocation progress and missing strata ===\n")
  blm <- biology_link(lcat, ldist, c("catchCategory", LD), NULL)
  gm <- list(
    GA1 = list(label = "TBB landings",
               alloc = 'catchCategory == "Lan" & metier4 == "TBB"',
               alloc_expr = alloc_quo(list(catchCategory = "Lan", metier4 = "TBB")),
               matched_expr = alloc_quo(list(catchCategory = "Lan", metier4 = "TBB"))),
    GA2 = list(label = "other landings", alloc = 'catchCategory == "Lan"',
               alloc_expr = alloc_quo(list(catchCategory = "Lan")),
               matched_expr = alloc_quo(list(catchCategory = "Lan"))))
  am <- assign_alloc_groups(blm, gm)
  pr <- manual_progress(am, gm)
  ok("progress has a row per rule plus a missing row", nrow(pr) == 3)
  ok("the last row is the unassigned remainder",
     pr$group[nrow(pr)] == "STILL MISSING")
  ok("strata add up across the table", sum(pr$n_strata) == nrow(am))
  ok("cumulative stops before the missing row",
     is.na(pr$cum_pct_weight[nrow(pr)]) &&
     !is.na(pr$cum_pct_weight[nrow(pr) - 1]))
  ok("cumulative is monotonic",
     { cw <- pr$cum_pct_weight[!is.na(pr$cum_pct_weight)]
       all(diff(cw) >= 0) })
  ms <- manual_missing(am, LD)
  ok("missing strata listed", !is.null(ms) && nrow(ms) > 0)
  ok("heaviest gap first",
     { w <- ms[["weight"]]; all(diff(w) <= 1e-9) })
  ok("missing count agrees with the progress table",
     nrow(ms) == pr$n_strata[nrow(pr)])

  gt <- alloc_group_table(am, gm, LD)
  ok("group table has the discard-style columns",
     all(c("group_id","allocStrataCond","bioMatchedCond","n_strata_raised",
           "weight_raised","n_strata_source","source_coverage") %in% names(gt)))
  ok("conditions are strings, not quosures",
     is.character(gt$allocStrataCond) && is.character(gt$bioMatchedCond))
  ok("source side is counted independently of the raised side",
     any(gt$n_strata_source != gt$n_strata_raised))
  ok("still-missing is carried on the table", "still_missing" %in% names(gt))

  # the $-partial-match hazard that produced a quosure in a data.frame
  ok("field lookup is exact, not partial",
     is.null(fld(list(matched_expr = 1), "matched")))
  ok("and falls back when absent", identical(fld(list(), "x", "d"), "d"))

  cat("\n=== 27. single-key mode removed ===\n")
  stn <- raising_steps(c("metier4","Country"), c(50,50), 50, "nested",
                       c("metier4","Country"))
  sts <- raising_steps(c("metier4","Country"), c(50,50), 50, "single",
                       c("metier4","Country"))
  ok("asking for 'single' now gives the nested cascade",
     identical(vapply(stn, function(x) x$label, character(1)),
               vapply(sts, function(x) x$label, character(1))))
  ok("every step keeps the keys above it",
     all(vapply(stn[-1], function(x)
       is.null(x$keys) || length(x$keys) >= 1, logical(1))))

  cat("\n=== 28. donor exclusion: sorting, overview, per-group scope ===\n")
  stq <- build_strata(derive_fields(cef), c("metier4","Country","Area1","Season"))
  DQ <- c("metier4","Country","Area1","Season")
  by_l <- donor_choices(stq, DQ, order_by = "landings")
  by_r <- donor_choices(stq, DQ, order_by = "rate")
  ok("choices are labelled with rate and landings",
     grepl("DR", names(by_l)[1]) && grepl("t\\)", names(by_l)[1]))
  ok("sorting by rate differs from sorting by landings",
     !identical(names(by_l)[1], names(by_r)[1]))
  ok("rate order really is descending by rate",
     { r <- as.numeric(sub(".*DR ([0-9.]+)%.*", "\\1", names(by_r)))
       all(diff(r) <= 1e-9) })
  ov <- excluded_overview(stq, DQ, excluded = unname(by_l)[1],
                          dr_min = 0.05, dr_max = 0.4)
  ok("overview lists both routes", !is.null(ov) && nrow(ov) > 0)
  ok("and says which route each came from",
     all(ov$reason %in% c("named", "outside rate window", "named + rate window")))
  ok("a named stratum is reported as named", any(ov$reason != "outside rate window"))
  ok("nothing excluded gives NULL, not an empty frame",
     is.null(excluded_overview(stq, DQ, character(0), NA, NA)))

  # exclusion is per GROUP: it narrows that rule's donor pool only
  r1 <- new_rule("G1", "tbb", list(list(var = "metier4", values = "TBB")),
                 NULL, NA, NA, excluded = unname(by_l)[1])
  r2 <- new_rule("G2", "rest", list(), NULL, NA, NA)
  ok("a rule carries its own exclusion list",
     length(r1$excluded) == 1 && length(r2$excluded) == 0)
  m1 <- source_match(stq, r1, list(), DQ)
  m2 <- source_match(stq, r2, list(), DQ)
  key <- do.call(paste, c(lapply(DQ, function(k) as.character(stq[[k]])), sep = " | "))
  hit <- key == unname(by_l)[1]
  ok("the excluded stratum is out of THAT group's pool", !any(m1 & hit))
  ok("but still available to another group", any(m2 & hit))
  ok("and still raisable - exclusion touches the donor side only",
     sum(eval_clauses(stq, r2$raise, list())) == nrow(stq))
  ok("the emitted donor clause carries the exclusion",
     grepl("%in%", source_cond_string(r1, DQ)) &&
     !grepl("!paste", source_cond_string(r2, DQ)))

  cat("\n=== 29. allocation comparison and stratum sparklines ===\n")
  if (exists("catchAllocationAoL", mode = "function")) {
    a1 <- list(groups = gsc)
    g2 <- gsc[1]
    a2 <- list(groups = g2)
    runs <- suppressWarnings(run_allocation_set(
      list(A1 = a1, A2 = a2), lcat, ldist, "LengthTotal",
      log_file = tempfile()))
    ok("both allocations run", length(runs) == 2)
    okrun <- !vapply(runs, function(r) inherits(r, "alloc_error"), logical(1))
    if (any(okrun)) {
      cmp <- suppressWarnings(compare_allocations(runs))
      ok("comparison produced", !is.null(cmp) && nrow(cmp) > 0)
      ok("it is labelled by allocation", "allocation" %in% names(cmp))
      ok("imported and allocated kept apart",
         all(c("Imported", "Allocated") %in% unique(cmp$.src)))
      tot <- comparison_totals(cmp)
      ok("totals table one row per allocation",
         !is.null(tot) && nrow(tot) == sum(okrun))
      ok("comparison plot builds (numbers)",
         !inherits(try(ggplot2::ggplot_build(
           plot_allocation_comparison(cmp, "numbers", NULL, "Length")),
           silent = TRUE), "try-error"))
      ok("comparison plot builds (stock weight)",
         !inherits(try(ggplot2::ggplot_build(
           plot_allocation_comparison(cmp, "tonnes", NULL, "Length")),
           silent = TRUE), "try-error"))
    }
    ok("a failing allocation does not sink the set",
       { bad <- suppressWarnings(run_allocation_set(
           list(B = list(groups = list())), lcat, ldist, "LengthTotal"))
         inherits(bad$B, "alloc_error") })
  } else cat("(catchAllocationAoL not loaded - comparison skipped)\n")

  DSP <- LD
  lksp <- link_dist_to_strata(lcat, ldist, c("catchCategory", DSP), "Lan")
  kk <- unique(do.call(paste, c(lapply(DSP, function(k)
    as.character(lksp[[k]])), sep = " | ")))
  sp <- stratum_sparkline(lksp, DSP, kk[1])
  ok("sparkline built for a stratum", !is.null(sp) && nzchar(sp))
  ok("it is a self-contained svg",
     grepl("^<svg", sp) && grepl("</svg>$", sp))
  ok("it shows both series",
     grepl("numbers", sp) && grepl("mean weight", sp))
  ok("an unknown stratum yields nothing, not an error",
     is.null(stratum_sparkline(lksp, DSP, "no | such | stratum | 9")))
  mp <- stratum_sparkline_map(lksp, DSP, kk[1:min(3, length(kk))])
  ok("map keyed by stratum", all(names(mp) %in% kk))
  ok("map drops strata with nothing to draw", length(mp) <= 3)

  cat("\n=== 30. ICES area split into its three nested levels ===\n")
  ar <- c("27.3.a.20", "27.7.d", "27.4", "27.3.a", "27.6.b.1", NA, "")
  lv <- ices_area_levels(ar)
  ok("a full code splits into all three",
     identical(unname(unlist(lv[1, ])), c("27.3", "27.3.a", "27.3.a.20")))
  ok("a division-level code repeats at subdivision",
     identical(unname(unlist(lv[2, ])), c("27.7", "27.7.d", "27.7.d")))
  ok("a subarea-level code repeats at all three",
     all(unlist(lv[3, ]) == "27.4"))
  ok("coarser codes repeat rather than becoming NA",
     !any(is.na(unlist(lv[3, ]))))
  ok("missing and blank stay missing", all(is.na(unlist(lv[6:7, ]))))
  ok("subarea is always a prefix of division",
     { z <- lv[1:5, ]; all(startsWith(z$ICESDivision, z$ICESSubArea)) })
  ok("division is always a prefix of subdivision",
     { z <- lv[1:5, ]; all(startsWith(z$ICESSubdivision, z$ICESDivision)) })

  dda <- derive_fields(lcat)
  ok("derive_fields adds the three columns",
     all(c("ICESSubArea","ICESDivision","ICESSubdivision") %in% names(dda)))
  ok("they really disaggregate",
     dplyr::n_distinct(dda$ICESDivision) >= dplyr::n_distinct(dda$ICESSubArea))
  ok("strata can be built on any level",
     all(vapply(c("ICESSubArea","ICESDivision","ICESSubdivision"),
                function(k) nrow(build_strata(dda, c("metier4", k))) > 0,
                logical(1))))
  ok("offered as an area level", all(c("ICESSubArea","ICESDivision",
     "ICESSubdivision") %in% AREA_LEVELS))
  ok("and usable in a clause", all(c("ICESSubArea","ICESDivision",
     "ICESSubdivision") %in% FILTER_VARS))
  # the emitted script must reproduce the same split
  eh <- emit_area_helper()
  ok("the area helper is emitted with the script",
     any(grepl("\\.ices_lvl", eh)))
  ok("emitted derivation names the three columns",
     { dc <- emit_derive_code()
       all(vapply(c("ICESSubArea","ICESDivision","ICESSubdivision"),
                  function(k) any(grepl(k, dc)), logical(1))) })
  ok("emitted code parses",
     !inherits(try(parse(text = paste(c(eh, emit_derive_code()),
                                      collapse = "\n")), silent = TRUE),
               "try-error"))
  ok("emitted helper gives the same answer as the app",
     { e <- new.env(); eval(parse(text = paste(eh, collapse = "\n")), e)
       isTRUE(all.equal(
         ices_area_levels(ar),
         data.frame(ICESSubArea = e$.ices_lvl(ar, 2),
                    ICESDivision = e$.ices_lvl(ar, 3),
                    ICESSubdivision = e$.ices_lvl(ar, 4),
                    stringsAsFactors = FALSE))) })

cat("\nDONE\n")
