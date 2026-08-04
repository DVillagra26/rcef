# ---------------------------------------------------------------
#  app_state.R  -- SHINY ONLY. state_capture() reads reactiveValues;
#  everything else it used to sit next to is now R/rcef_session.R.
# ---------------------------------------------------------------

state_capture <- function(input, rv) {
  vals <- reactiveValuesToList(input)
  vals <- vals[!grepl(STATE_SKIP, names(vals))]
  vals <- vals[!vapply(vals, is.null, logical(1))]
  vals <- vals[vapply(vals, function(v) is.atomic(v) && length(v) <= 8, logical(1))]
  list(saved = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
       inputs = vals,
       rules = rv$rules, scenarios = unname(rv$scenarios))
}
