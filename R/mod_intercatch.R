# ============================================================
#  mod_intercatch.R
#  Section 1: build CEF from InterCatch, by either route.
#
#    Exchange format  -> funIntercatchCEF()   (HI / SI / SD csv files)
#    Output format    -> funICoutCEF()        (StockOverview / Numbers /
#                                              MeanWeigth txt, per year)
#
#  Every argument of both functions is exposed, including the ones that
#  are not fully wired up in the functions yet, so nothing has to be
#  edited by hand afterwards.
# ============================================================

OUTPUT_FORMATS <- c("to_environment", "to_file", "to_upload", "to_list")

# shown in small grey caps beside each choice
OUTPUT_DESC <- list(
  to_environment = "assigns catches / distributions / effort to the global env",
  to_file        = "writes the three CEF csv files, with headers",
  to_upload      = "writes the comma-separated upload files, no headers",
  to_list        = "returns a named list - this is what the workbench reads")

# which physical files each entry point expects, and how to spot them
# The exchange route takes ONE file. funIntercatchCEF() reads every csv in
# the directory, rbinds them and splits on the record-type column (HI /
# SI / SD), so a single combined export is the normal input; asking for
# three separate files was wrong.
IC_FILE_ROLES <- list(
  exchange = list(
    EXCHANGE = "one InterCatch exchange file, holding the HI, SI and SD records"),
  output = list(
    StockOverview = "landings and discards per stratum",
    NumbersAtAgeLength = "numbers at age or length",
    MeanWeigthAtAgeLength = "mean weight at age or length"))

IC_FILE_PATTERN <- c(EXCHANGE = "\\.csv$",
                     StockOverview = "stockoverview",
                     NumbersAtAgeLength = "number",
                     MeanWeigthAtAgeLength = "meanweig|mean_weig")

# The call, as text, for the script and the preview pane
ic_code <- function(i) {
  fmt <- if (length(i$output_format))
    paste0('c("', paste(i$output_format, collapse = '", "'), '")') else '"to_list"'
  m6 <- if (nzchar(i$metier6 %||% "")) sprintf('"%s"', i$metier6) else "NULL"

  rel <- switch(i$rel_mode,
    build  = c(sprintf("stock_relation <- funMakeRelation(year = %s)", i$rel_year),
               "## funMakeRelation assigns stock_relation to the global env too"),
    upload = c(sprintf('stock_relation <- read.csv("%s", stringsAsFactors = FALSE)',
                       i$rel_file %||% "stock_relation.csv")),
    auto   = "## stock_relation left to funICoutCEF(), which builds it from max(years)")

  call <- if (i$source == "exchange") c(
    "res <- funIntercatchCEF(",
    sprintf('    dat_path              = "%s",', i$path),
    "    stock_relation        = stock_relation,",
    sprintf("    output_format         = %s,", fmt),
    sprintf('    out_path              = "%s",', i$out_path),
    sprintf('    file_prefix           = "%s",', i$prefix),
    sprintf('    file_suffix           = "%s",', i$suffix),
    sprintf("    include_BMS_Caton     = %s,", i$incl_bms),
    sprintf("    patch_missing_DIS_PSU = %s,", i$patch_psu),
    sprintf("    metier6               = %s)", m6))
  else c(
    "res <- funICoutCEF(",
    sprintf('    dat_path              = "%s",', i$path),
    sprintf("    years                 = %s,",
            if (length(i$years) > 1)
              paste0("c(", paste(i$years, collapse = ", "), ")")
            else i$years),
    if (i$rel_mode == "auto") "    stock_relation        = NULL," else
      "    stock_relation        = stock_relation,",
    sprintf("    output_format         = %s,", fmt),
    sprintf('    out_path              = "%s",', i$out_path),
    sprintf("    keep_temp_file        = %s,", i$keep_temp),
    sprintf('    file_prefix           = "%s",', i$prefix),
    sprintf("    include_BMS_Caton     = %s,", i$incl_bms),
    sprintf("    patch_missing_DIS_PSU = %s,", i$patch_psu),
    sprintf("    metier6               = %s)", m6))

  c("## ---- InterCatch -> CEF -----------------------------------", rel, "",
    call, "",
    "catch_data <- res$catches",
    'attr(res, "CEF_version")')
}

# Lay uploaded files out the way the functions expect
ic_stage_uploads <- function(files, source_type, years) {
  root <- file.path(tempdir(), paste0("ic_", as.integer(Sys.time())))
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  # funICoutCEF looks in dat_path/<year>/ ; funIntercatchCEF in dat_path/
  dest <- if (source_type == "output") file.path(root, years[1]) else root
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  file.copy(files$datapath, file.path(dest, files$name))
  root
}

# The canonical filename each role must have so the functions' internal
# grep/`%like%` finds it. funICoutCEF() re-globs dat_path/<year>/ and
# matches "StockOverview", "Numbers", "MeanWeigth"; funIntercatchCEF()
# reads every csv and splits on the RecordType column, so exchange files
# only need to be csv - but we still normalise the names for clarity.
IC_CANONICAL <- list(
  output = c(StockOverview = "StockOverview.txt",
             NumbersAtAgeLength = "NumbersAtAgeLength.txt",
             MeanWeigthAtAgeLength = "MeanWeigthAtAgeLength.txt"),
  exchange = c(EXCHANGE = "exchange.csv"))

# Copy the files the user mapped (role -> source path) into the exact
# layout the conversion functions expect, with canonical names. This is
# what makes an arbitrary directory work: without it funICoutCEF() globs
# dat_path/<year>/ and fails with
#   "any(grepl("StockOverview.txt", lst)) is not TRUE"
# whenever the files are named differently or sit directly in dat_path.
#
# mapping: named character vector, names are roles, values are absolute
#          source file paths (skip empty/NA - role not supplied).
# Returns the dat_path to hand to the function (the ROOT, not the year
# subfolder), so funICoutCEF(dat_path = <this>, years = <year>) works.
ic_stage_mapping <- function(mapping, source_type, year) {
  canon <- IC_CANONICAL[[source_type]]
  root <- file.path(tempdir(),
                    paste0("icmap_", as.integer(Sys.time()), "_",
                           sample.int(1e5, 1)))
  dest <- if (source_type == "output") file.path(root, year) else root
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  for (role in names(mapping)) {
    src <- mapping[[role]]
    if (is.null(src) || is.na(src) || !nzchar(src) || !file.exists(src)) next
    nm <- if (role %in% names(canon)) canon[[role]] else basename(src)
    file.copy(src, file.path(dest, nm), overwrite = TRUE)
  }
  root
}

# Pull a four-digit year out of a filename or its contents, so the user
# does not have to type it. Looks at the file names first (e.g.
# "pok_2022_StockOverview.txt"), then at the Year column if present.
ic_detect_year <- function(files, contents_ok = TRUE) {
  from_name <- unlist(regmatches(files,
    gregexpr("(19|20)[0-9]{2}", basename(files))))
  yrs <- suppressWarnings(as.integer(from_name))
  yrs <- yrs[!is.na(yrs) & yrs >= 1990 & yrs <= 2100]
  if (length(yrs)) return(sort(unique(yrs)))
  if (!contents_ok) return(integer(0))
  # fall back to a Year column in the first readable file
  for (f in files) {
    d <- tryCatch(utils::read.table(f, header = TRUE, sep = "\t",
                                    nrows = 50, fill = TRUE,
                                    stringsAsFactors = FALSE),
                  error = function(e) NULL)
    if (!is.null(d) && "Year" %in% names(d)) {
      y <- suppressWarnings(as.integer(unique(d$Year)))
      y <- y[!is.na(y) & y >= 1990 & y <= 2100]
      if (length(y)) return(sort(y))
    }
  }
  integer(0)
}
