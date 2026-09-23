# The point of dispatching on an engine object rather than branching on a
# string: covering every (plot, engine) pair is a loop, not a discipline. A new
# engine is held to this on the day it lands, including the edge cases nobody
# remembers to retest by hand.

test_that("every engine renders every waterfall fixture", {
  fixtures <- waterfall_fixtures()
  for (engine in available_engines()) {
    for (nm in names(fixtures)) {
      p <- render_waterfall(engine, fixtures[[nm]])
      expect_s3_class(p, expected_class(engine))
    }
  }
})

test_that("every engine renders every swimlane fixture", {
  fixtures <- swimlane_fixtures()
  for (engine in available_engines()) {
    for (nm in names(fixtures)) {
      p <- render_swimlane(engine, fixtures[[nm]])
      expect_s3_class(p, expected_class(engine))
    }
  }
})

test_that("ggplot2 output actually builds, not merely inherits", {
  # A broken aes() still returns something of class "ggplot"; it only fails at
  # draw time. Building is the cheapest way to catch that in a test.
  for (nm in names(waterfall_fixtures())) {
    p <- render_waterfall(eng_ggplot2(), waterfall_fixtures()[[nm]])
    expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
  }
  for (nm in names(swimlane_fixtures())) {
    p <- render_swimlane(eng_ggplot2(), swimlane_fixtures()[[nm]])
    expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
  }
})

test_that("an engine with no renderer is refused, clearly", {
  pd <- waterfall_fixtures()$plain
  expect_error(
    render_waterfall(new_engine("nope"), pd),
    "No waterfall renderer"
  )
  expect_error(
    render_swimlane(new_engine("nope"), swimlane_fixtures()$plain),
    "No swimlane renderer"
  )
})

test_that("a non-engine is refused before anything is drawn", {
  expect_error(waterfall(wf_input(), id, change, engine = "plotly"),
               "must be a floaties engine")
  expect_error(swimlane(sl_input(), subject, day, engine = NULL),
               "must be a floaties engine")
})
