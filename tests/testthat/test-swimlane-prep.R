test_that("prep_swimlane() works with any column names", {
  spec <- prep_swimlane(sw_lanes(), subject = pid, end = stop)
  expect_s3_class(spec, c("floaties_swimlane_spec", "floaties_spec"))
  expect_equal(spec$plot, "swimlane")
  expect_equal(nrow(spec$data), 4)
  expect_named(
    spec$data,
    c(".subject", ".start", ".end", ".fill", ".ongoing", ".tooltip")
  )
  expect_equal(spec$labels$y, "pid")
  expect_equal(spec$labels$x, "stop")
  expect_null(spec$events)
})

test_that("lanes start at 0 unless start is given", {
  no_start <- prep_swimlane(sw_lanes(), pid, stop, sort = "none")
  expect_equal(no_start$data$.start, c(0, 0, 0, 0))

  with_start <- prep_swimlane(
    sw_lanes(), pid, stop,
    start = begin, sort = "none"
  )
  expect_equal(with_start$data$.start, c(0, -2, 1, 0))
  expect_equal(with_start$data$.end, c(10, 4, 12, 7))
})

test_that("lanes are sorted by end time, top to bottom", {
  d <- sw_lanes()
  desc <- prep_swimlane(d, pid, stop)
  asc <- prep_swimlane(d, pid, stop, sort = "ascending")
  none <- prep_swimlane(d, pid, "stop", sort = "none")

  expect_equal(levels(desc$data$.subject), c("p3", "p1", "p4", "p2"))
  expect_equal(as.character(desc$data$.subject), c("p3", "p1", "p4", "p2"))
  expect_equal(levels(asc$data$.subject), c("p2", "p4", "p1", "p3"))
  expect_equal(levels(none$data$.subject), d$pid)
})

test_that("fill colors are light by default and can be set", {
  spec <- prep_swimlane(sw_lanes(), pid, stop, fill = cohort)
  expect_equal(levels(spec$data$.fill), c("high", "low"))
  expect_equal(spec$labels$fill, "cohort")
  expect_equal(spec$scales$fill, c(high = "#B3D5E8", low = "#F2CFB3"))

  custom <- prep_swimlane(
    sw_lanes(), pid, stop,
    fill = cohort, colors = c(low = "grey80", high = "pink")
  )
  expect_equal(custom$scales$fill, c(high = "pink", low = "grey80"))

  no_fill <- prep_swimlane(sw_lanes(), pid, stop)
  expect_null(no_fill$labels$fill)
})

test_that("ongoing accepts logical and Y/N flags", {
  logical_flag <- prep_swimlane(
    sw_lanes(), pid, stop,
    ongoing = still_on, sort = "none"
  )
  expect_equal(logical_flag$data$.ongoing, c(TRUE, FALSE, FALSE, TRUE))

  d <- sw_lanes()
  d$flag <- c("Y", "N", "", NA)
  character_flag <- prep_swimlane(d, pid, stop, ongoing = flag, sort = "none")
  expect_equal(character_flag$data$.ongoing, c(TRUE, FALSE, FALSE, FALSE))

  d$flag <- factor(d$flag)
  factor_flag <- prep_swimlane(d, pid, stop, ongoing = flag, sort = "none")
  expect_equal(factor_flag$data$.ongoing, c(TRUE, FALSE, FALSE, FALSE))

  none <- prep_swimlane(sw_lanes(), pid, stop)
  expect_false(any(none$data$.ongoing))
})

test_that("ongoing lanes get an Ongoing marker at the lane end", {
  spec <- prep_swimlane(sw_lanes(), pid, stop, ongoing = still_on)
  events <- spec$events
  expect_equal(as.character(events$.subject), c("p1", "p4"))
  expect_equal(events$.time, c(10, 7))
  expect_equal(levels(events$.event), "Ongoing")
  expect_equal(spec$scales$event_shape, c(Ongoing = "arrow"))
  expect_equal(spec$scales$event_color, c(Ongoing = "#404040"))

  with_events <- prep_swimlane(
    sw_lanes(), pid, stop,
    ongoing = still_on,
    events = sw_events(), event_time = at, event = what
  )
  expect_equal(
    levels(with_events$events$.event),
    c("respond", "progress", "Ongoing")
  )
  expect_equal(nrow(with_events$events), 7)
})

test_that("events are matched to lanes", {
  spec <- prep_swimlane(
    sw_lanes(), pid, stop,
    events = sw_events(), event_time = at, event = what
  )
  events <- spec$events
  expect_equal(nrow(events), 5)
  expect_equal(levels(events$.subject), levels(spec$data$.subject))
  expect_equal(as.character(events$.subject), c("p1", "p1", "p3", "p2", "p4"))
  expect_equal(events$.time, c(2, 8, 5, 3, 6))
  expect_equal(levels(events$.event), c("respond", "progress"))
  expect_equal(spec$labels$event, "what")
  expect_equal(
    spec$scales$event_shape,
    c(respond = "circle", progress = "square")
  )
  expect_equal(
    spec$scales$event_color,
    c(respond = "#0072B2", progress = "#D55E00")
  )
  expect_equal(
    events$.tooltip[[1]],
    "<b>pid:</b> p1<br><b>what:</b> respond<br><b>at:</b> 2"
  )
})

test_that("events can use a different subject column", {
  ev <- sw_events()
  names(ev)[1] <- "subject_id"
  spec <- prep_swimlane(
    sw_lanes(), pid, stop,
    events = ev, event_time = at, event = what, event_subject = subject_id
  )
  expect_equal(nrow(spec$events), 5)

  expect_snapshot(
    error = TRUE,
    prep_swimlane(
      sw_lanes(), pid, stop,
      events = ev, event_time = at, event = what
    )
  )
})

test_that("events for unknown subjects or missing times are dropped", {
  ev <- rbind(
    sw_events(),
    data.frame(pid = c("nobody", "p1"), at = c(1, NA), what = "respond")
  )
  expect_snapshot(
    spec <- prep_swimlane(
      sw_lanes(), pid, stop,
      events = ev, event_time = at, event = what
    )
  )
  expect_equal(nrow(spec$events), 5)
  expect_warning(
    expect_warning(
      prep_swimlane(
        sw_lanes(), pid, stop,
        events = ev, event_time = at, event = what
      ),
      class = "floaties_warning_missing_values"
    ),
    class = "floaties_warning_unknown_subjects"
  )
})

test_that("event colors and shapes can be customized", {
  spec <- prep_swimlane(
    sw_lanes(), pid, stop,
    events = sw_events(), event_time = at, event = what,
    event_colors = c(progress = "red", respond = "green"),
    event_shapes = c("star", "cross")
  )
  expect_equal(spec$scales$event_color, c(respond = "green", progress = "red"))
  expect_equal(spec$scales$event_shape, c(respond = "star", progress = "cross"))
})

test_that("lanes with a missing time are dropped with a warning", {
  d <- sw_lanes()
  d$stop[2] <- NA
  expect_warning(
    spec <- prep_swimlane(d, pid, stop),
    class = "floaties_warning_missing_values"
  )
  expect_equal(nrow(spec$data), 3)
})

test_that("prep_swimlane() validates its input", {
  d <- sw_lanes()
  ev <- sw_events()

  expect_snapshot(error = TRUE, {
    prep_swimlane(d, pid)
    prep_swimlane(d, pid, cohort)
    prep_swimlane(d, pid, stop, start = cohort)
    prep_swimlane(d, pid, stop, ongoing = begin)
    prep_swimlane(d, pid, begin, start = stop)
    prep_swimlane(d, pid, stop, events = ev, event = what)
    prep_swimlane(d, pid, stop, events = ev, event_time = what, event = what)
    prep_swimlane(d, pid, stop, events = "ev", event_time = at, event = what)
    prep_swimlane(
      d, pid, stop,
      events = ev, event_time = at, event = what,
      event_shapes = c("circle", "hexagon")
    )
    prep_swimlane(
      d, pid, stop,
      events = ev, event_time = at, event = what,
      event_colors = c(respond = "red")
    )
  })

  expect_error(
    prep_swimlane(d, pid, begin, start = stop),
    class = "floaties_error_invalid_range"
  )
  expect_error(
    prep_swimlane(
      d, pid, stop,
      events = ev, event_time = at, event = what,
      event_shapes = c("circle", "hexagon")
    ),
    class = "floaties_error_argument"
  )
})

test_that("default shapes run out after six event types", {
  ev <- data.frame(pid = "p1", at = 1:7, what = letters[1:7])
  expect_error(
    prep_swimlane(
      sw_lanes(), pid, stop,
      events = ev, event_time = at, event = what
    ),
    class = "floaties_error_argument"
  )
  # Supplying shapes avoids the limit.
  spec <- prep_swimlane(
    sw_lanes(), pid, stop,
    events = ev, event_time = at, event = what,
    event_shapes = rep("circle", 7)
  )
  expect_length(spec$scales$event_shape, 7)
})

test_that("subjects must be unique", {
  d <- sw_lanes()
  d$pid[2] <- "p1"
  expect_error(
    prep_swimlane(d, pid, stop),
    class = "floaties_error_duplicate_subject"
  )
})

test_that("spec prints lanes and events", {
  spec <- prep_swimlane(
    sw_lanes(), pid, stop,
    events = sw_events(), event_time = at, event = what
  )
  out <- capture.output(print(spec))
  expect_true("<floaties swimlane spec: 4 subjects>" %in% out)
  expect_true("<5 events>" %in% out)
})
