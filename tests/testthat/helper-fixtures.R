# Fixtures shared by every test file. The edge cases live here rather than in
# one test, so that the conformance suite runs all of them against every engine.

wf_input <- function() {
  data.frame(
    id       = sprintf("01-%03d", 1:8),
    change   = c(-100, -62, -41, -30, -8, 12, 24, 57),
    response = c("CR", "PR", "PR", "PR", "SD", "SD", "PD", "PD"),
    stringsAsFactors = FALSE
  )
}

sl_input <- function() {
  data.frame(
    subject = c("01-001", "01-001", "01-001", "01-002", "01-002", "01-003"),
    day     = c(1, 15, 40, 1, 22, 1),
    stop    = c(84, NA, 60, 56, NA, 120),
    what    = c("dosing", "adverse event", "dosing",
                "dosing", "adverse event", "dosing"),
    stringsAsFactors = FALSE
  )
}

waterfall_fixtures <- function() {
  d <- wf_input()
  suppressWarnings(list(
    plain   = prepare_waterfall(d, id, change),
    grouped = prepare_waterfall(d, id, change, color_var = response,
                                hlines = c(20, -30), title = "Best response"),
    empty   = prepare_waterfall(d[0, ], id, change),
    one_row = prepare_waterfall(d[1, ], id, change),
    with_na = prepare_waterfall(
      transform(d, change = replace(d$change, 1, NA_real_)), id, change),
    all_na_group = prepare_waterfall(
      transform(d, response = NA_character_), id, change, color_var = response)
  ))
}

swimlane_fixtures <- function() {
  d <- sl_input()
  suppressWarnings(list(
    plain      = prepare_swimlane(d, subject, day, stop, what),
    untyped    = prepare_swimlane(d, subject, day, stop),
    events_only = prepare_swimlane(d[is.na(d$stop), ], subject, day),
    intervals_only = prepare_swimlane(d[!is.na(d$stop), ], subject, day, stop),
    empty      = prepare_swimlane(d[0, ], subject, day, stop),
    one_row    = prepare_swimlane(d[1, ], subject, day, stop),
    vlines     = prepare_swimlane(d, subject, day, stop, what,
                                  vlines = 28, title = "On study")
  ))
}

available_engines <- function() {
  out <- list(ggplot2 = eng_ggplot2())
  if (requireNamespace("plotly", quietly = TRUE)) {
    out$plotly <- eng_plotly()
  }
  if (requireNamespace("echarts4r", quietly = TRUE) &&
      requireNamespace("dplyr", quietly = TRUE)) {
    out$echarts4r <- eng_echarts4r()
  }
  out
}

expected_class <- function(engine) {
  switch(
    engine_name(engine),
    ggplot2   = "ggplot",
    plotly    = "plotly",
    echarts4r = "echarts4r",
    rlang::abort("Unknown engine in the test helper.")
  )
}
