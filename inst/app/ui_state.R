
state_capture <- function(input, rv) {
  vals <- reactiveValuesToList(input)
  vals <- vals[!grepl(STATE_SKIP, names(vals))]
  vals <- vals[!vapply(vals, is.null, logical(1))]
  vals <- vals[vapply(vals, function(v) is.atomic(v) && length(v) <= 8, logical(1))]
  list(saved = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
       inputs = vals,
       rules = rv$rules, scenarios = unname(rv$scenarios))
}

state_apply <- function(session, st) {
  applied <- skipped <- character(0)
  for (id in names(st$inputs)) {
    v <- st$inputs[[id]]
    u <- state_updater(id, v)
    if (is.na(u)) { skipped <- c(skipped, id); next }
    ok <- tryCatch({
      switch(u,
        slider    = updateSliderInput(session, id, value = v),
        check     = updateCheckboxInput(session, id, value = as.logical(v)),
        radio     = updateRadioButtons(session, id, selected = v),
        numeric   = updateNumericInput(session, id, value = as.numeric(v)),
        select    = updateSelectInput(session, id, selected = v),
        selectize = updateSelectizeInput(session, id, selected = v),
        text      = updateTextInput(session, id, value = as.character(v)))
      TRUE }, error = function(e) FALSE)
    if (isTRUE(ok)) applied <- c(applied, id) else skipped <- c(skipped, id)
  }
  list(applied = applied, skipped = skipped)
}
