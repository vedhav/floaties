# Small datasets with non-CDISC column names.
wf_data <- function() {
  data.frame(
    id = c("a", "b", "c", "d", "e"),
    change = c(10, -50, 150, -20, 40),
    group = factor(c("x", "y", "x", NA, "y"), levels = c("y", "x", "unused")),
    note = c("<b>one</b>", "two", "three", "four", "five")
  )
}

sw_lanes <- function() {
  data.frame(
    pid = c("p1", "p2", "p3", "p4"),
    begin = c(0, -2, 1, 0),
    stop = c(10, 4, 12, 7),
    cohort = c("low", "high", "low", "high"),
    still_on = c(TRUE, FALSE, NA, TRUE),
    site = c("s1", "s2", "s1", "s3")
  )
}

sw_events <- function() {
  data.frame(
    pid = c("p1", "p1", "p3", "p2", "p4"),
    at = c(2, 8, 5, 3, 6),
    what = factor(
      c("respond", "progress", "respond", "progress", "respond"),
      levels = c("respond", "progress")
    )
  )
}

# A swimlane with every option, for the rendering tests.
swim <- function(engine, ...) {
  plot_swimlane(
    sw_lanes(), "pid", "stop",
    start = "begin", fill = "cohort", ongoing = "still_on",
    events = sw_events(), event_time = "at", event = "what",
    ...,
    engine = engine
  )
}
