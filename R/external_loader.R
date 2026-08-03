# ============================================================
#  external_loader.R
#  Sources the InterCatch conversion and raising functions.
#
#  These are YOUR scripts, kept alongside the app rather than rewritten
#  into it, so fixes upstream flow through without a second copy to
#  maintain. Point rcef.external_dir at the folder that holds them.
# ============================================================

EXTERNAL_FILES <- c(
  "funMakeRelation.R",              # stock_relation from ICES vocab + SD
  "funIntercatchCEF.R",             # IC exchange format -> CEF
  "funICoutCEF.R",                  # IC output (StockOverview etc) -> CEF
  "0_Functions_other.R",            # check_group_conditions, convert_field, ...
  "0_Functions_discards_raising.R", # grp_catch_raising, catchRaising
  "0_Functions_age-length_alloc.R"  # age/length allocation (section 4-5)
)

# Packages each script needs. Missing ones are reported rather than
# thrown, so the rest of the app still runs.
EXTERNAL_PKGS <- c("data.table", "icesVocab", "icesSD", "RstoxData",
                   "plyr", "tidyr", "dplyr", "stringr", "readr")

load_external <- function(dir = getOption("rcef.external_dir", "R")) {
  found <- missing <- character(0)
  errs <- list()
  for (f in EXTERNAL_FILES) {
    p <- file.path(dir, f)
    if (!file.exists(p)) { missing <- c(missing, f); next }
    e <- tryCatch({ source(p, local = FALSE); NULL },
                  error = function(e) conditionMessage(e))
    if (is.null(e)) found <- c(found, f) else errs[[f]] <- e
  }
  pk <- vapply(EXTERNAL_PKGS, requireNamespace, logical(1), quietly = TRUE)
  list(dir = dir, found = found, missing = missing, errors = errs,
       packages_ok = names(pk)[pk], packages_missing = names(pk)[!pk])
}

# Which app features are usable given what actually loaded
external_status <- function(x) {
  has <- function(f) exists(f, mode = "function")
  list(
    ic_exchange = has("funIntercatchCEF"),
    ic_output   = has("funICoutCEF"),
    make_rel    = has("funMakeRelation"),
    raising     = has("catchRaising") && has("check_group_conditions"),
    group_cond  = has("check_group_conditions"),
    catch_alloc = has("catchAllocationAoL"),
    catch_agg   = has("catchAtAoLAggregation"),
    mean_agg    = has("meanWoLatAoLAggregation"),
    agelength   = has("age_length_alloc") || "0_Functions_age-length_alloc.R" %in% x$found,
    pkgs_missing = x$packages_missing)
}
