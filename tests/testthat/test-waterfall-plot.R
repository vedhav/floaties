test_that("plot_waterfall() returns each engine's native object", {
  d <- wf_data()
  expect_s3_class(plot_waterfall(d, id, change), "ggplot")
  expect_s3_class(plot_waterfall(d, id, change, engine = "plotly"), "plotly")
  expect_s3_class(
    plot_waterfall(d, id, change, engine = "echarts4r"),
    "echarts4r"
  )
})

test_that("plot_waterfall() validates engine and passes arguments on", {
  d <- wf_data()
  expect_snapshot(error = TRUE, {
    plot_waterfall(d, id, change, engine = "base")
    plot_waterfall(d, id, change, title = 1)
  })
})

test_that("title and axis labels can be overridden", {
  p <- plot_waterfall(
    wf_data(), id, change,
    title = "T", x_label = "Subjects", y_label = "Change (%)"
  )
  expect_equal(p$labels$title, "T")
  expect_equal(p$labels$x, "Subjects")
  expect_equal(p$labels$y, "Change (%)")
})

test_that("ggplot2 waterfall renders", {
  skip_if_not_installed("vdiffr")
  d <- wf_data()
  vdiffr::expect_doppelganger(
    "waterfall-fill",
    plot_waterfall(d, id, change, fill = group, title = "Waterfall")
  )
  vdiffr::expect_doppelganger(
    "waterfall-no-fill",
    plot_waterfall(d, id, change, ref_lines = NULL)
  )
})

test_that("plotly waterfall keeps order, colors, and lines", {
  p <- plot_waterfall(wf_data(), id, change, fill = group, engine = "plotly")
  built <- plotly::plotly_build(p)$x

  expect_equal(
    vapply(built$data, function(t) t$name, character(1)),
    c("y", "x", "Missing")
  )
  expect_equal(built$data[[2]]$marker$color, "#D55E00")
  expect_equal(built$layout$xaxis$categoryarray, c("c", "e", "a", "d", "b"))
  expect_equal(
    vapply(built$layout$shapes, function(s) s$y0, numeric(1)),
    c(20, -30)
  )
  expect_true(built$layout$showlegend)
})

test_that("echarts4r waterfall keeps order, colors, and lines", {
  e <- plot_waterfall(wf_data(), id, change, fill = group, engine = "echarts4r")
  opts <- e$x$opts

  expect_equal(
    vapply(opts$series, function(s) s$name, character(1)),
    c("y", "x", "Missing")
  )
  expect_equal(opts$xAxis$data, c("c", "e", "a", "d", "b"))
  expect_equal(opts$series[[2]]$itemStyle$color, "#D55E00")
  expect_equal(
    vapply(opts$series[[1]]$markLine$data, function(l) l$yAxis, numeric(1)),
    c(20, -30)
  )

  # Each subject has a value in exactly one series.
  y_series <- opts$series[[1]]$data
  expect_true(is.na(y_series[[1]]))
  expect_equal(y_series[[2]]$value, 40)
})

test_that("echarts4r waterfall without fill or ref lines has no extras", {
  e <- plot_waterfall(
    wf_data(), id, change,
    ref_lines = NULL, engine = "echarts4r"
  )
  expect_length(e$x$opts$series, 1)
  expect_null(e$x$opts$series[[1]]$markLine)
  expect_false(e$x$opts$legend$show)
})
