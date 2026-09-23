test_that("plot_swimlane() returns each engine's native object", {
  expect_s3_class(swim("ggplot2"), "ggplot")
  expect_s3_class(swim("plotly"), "plotly")
  expect_s3_class(swim("echarts4r"), "echarts4r")
})

test_that("plot_swimlane() works without events or options", {
  d <- sw_lanes()
  expect_s3_class(plot_swimlane(d, pid, stop), "ggplot")
  expect_s3_class(plot_swimlane(d, pid, stop, engine = "plotly"), "plotly")
  e <- plot_swimlane(d, pid, stop, engine = "echarts4r")
  expect_length(e$x$opts$series, 1)
  expect_false(e$x$opts$legend$show)
})

test_that("plot_swimlane() validates engine and passes arguments on", {
  expect_snapshot(error = TRUE, {
    plot_swimlane(sw_lanes(), pid, stop, engine = "base")
    plot_swimlane(sw_lanes(), pid, stop, x_label = c("a", "b"))
  })
})

test_that("title and axis labels can be overridden", {
  p <- swim("ggplot2", title = "T", x_label = "Weeks", y_label = "Patient")
  expect_equal(p$labels$title, "T")
  expect_equal(p$labels$x, "Weeks")
  expect_equal(p$labels$y, "Patient")
})

test_that("ggplot2 swimlane renders", {
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(
    "swimlane-full",
    swim("ggplot2", ref_lines = 5, title = "Swimlane")
  )
  vdiffr::expect_doppelganger(
    "swimlane-lanes-only",
    plot_swimlane(sw_lanes(), pid, stop)
  )
})

test_that("ggplot2 swimlane puts the first subject at the top", {
  p <- swim("ggplot2")
  lanes <- ggplot2::layer_data(p, 1)
  # p3 has the longest lane, so it is first and drawn highest.
  expect_equal(max(lanes$ymax), 4.35)
  y_scale <- ggplot2::layer_scales(p)$y
  expect_equal(y_scale$get_labels(), c("p3", "p1", "p4", "p2"))
})

test_that("plotly swimlane has lanes, events, and lines", {
  built <- plotly::plotly_build(swim("plotly", ref_lines = 5))$x
  names <- vapply(built$data, function(t) t$name, character(1))
  expect_equal(names, c("high", "low", "respond", "progress", "Ongoing"))

  lanes <- built$data[[1]]
  expect_equal(lanes$orientation, "h")
  expect_equal(as.numeric(lanes$base), c(0, -2))

  symbols <- vapply(built$data[3:5], function(t) t$marker$symbol, character(1))
  expect_equal(symbols, c("circle", "square", "triangle-right"))
  # plotly draws the first category at the bottom.
  expect_equal(built$layout$yaxis$categoryarray, c("p2", "p4", "p1", "p3"))
  expect_equal(built$layout$shapes[[1]]$x0, 5)
})

test_that("echarts4r swimlane has lanes, events, and lines", {
  e <- swim("echarts4r", ref_lines = 5)
  opts <- e$x$opts
  types <- vapply(opts$series, function(s) s$type, character(1))
  names <- vapply(opts$series, function(s) s$name, character(1))

  expect_equal(types, c("custom", "custom", "scatter", "scatter", "scatter"))
  expect_equal(names, c("high", "low", "respond", "progress", "Ongoing"))
  expect_equal(opts$yAxis$data, c("p2", "p4", "p1", "p3"))
  expect_equal(
    unlist(opts$legend$data),
    c("high", "low", "respond", "progress", "Ongoing")
  )
  expect_match(opts$series[[5]]$symbol, "^path://")

  # p2 (bottom, index 0) starts at -2.
  high <- opts$series[[1]]$data
  expect_equal(high[[2]]$value, list(0, -2, 4))
  expect_equal(opts$series[[1]]$markLine$data[[1]]$xAxis, 5)
})
