tumor <- data.frame(
  subject = c("A", "B", "C", "D"),
  pchg = c(-50, 30, -10, 5),
  arm = c("Placebo", "Drug", "Drug", NA)
)

lanes <- data.frame(
  subject = c("S1", "S1", "S2", "S3", "S3"),
  weeks = c(10, 10, 30, 20, 20),
  arm = c("A", "A", "B", "A", "A"),
  time = c(4, 8, NA, 6, 12),
  response = c("SD", "PR", NA, "PD", "Death")
)

plotly_traces <- function(p) plotly::plotly_build(p)$x$data

test_that("invalid engine is rejected", {
  expect_error(waterfall_plot(tumor, subject, pchg, engine = "lattice"), "should be one of")
  expect_error(swimlane_plot(lanes, subject, weeks, engine = "lattice"), "should be one of")
})

test_that("shared colours match ggplot2 defaults", {
  expect_equal(hue_colours(3), scales::hue_pal()(3))
  expect_equal(
    pastel_colours(3),
    scales::brewer_pal(palette = "Pastel1")(3)
  )
  expect_equal(group_levels(c("b", NA, "a")), c("a", "b", "NA"))
})

# plotly ---------------------------------------------------------------------

test_that("plotly waterfall has one sorted trace per fill level", {
  skip_if_not_installed("plotly")
  p <- waterfall_plot(tumor, subject, pchg, fill = arm, engine = "plotly")
  expect_s3_class(p, "plotly")

  traces <- plotly_traces(p)
  expect_equal(vapply(traces, `[[`, "", "name"), c("Drug", "Placebo", "NA"))
  expect_equal(traces[[1]]$marker$color, hue_colours(2)[1])
  expect_equal(traces[[3]]$marker$color, "#7F7F7F")
  expect_match(traces[[2]]$hovertext, "subject: A<br>pchg: -50<br>arm: Placebo")

  layout <- plotly::plotly_build(p)$x$layout
  expect_equal(layout$xaxis$categoryarray, c("B", "D", "C", "A"))
  expect_equal(vapply(layout$shapes, `[[`, 0, "y0"), c(20, -30))
})

test_that("plotly waterfall without fill hides the legend", {
  skip_if_not_installed("plotly")
  traces <- plotly_traces(waterfall_plot(tumor, subject, pchg, ref_lines = NULL, engine = "plotly"))
  expect_length(traces, 1)
  expect_false(traces[[1]]$showlegend)
})

test_that("plotly swimlane has lanes, styled events, and lane order", {
  skip_if_not_installed("plotly")
  p <- swimlane_plot(lanes, subject, weeks, time, response, fill = arm, engine = "plotly")
  traces <- plotly_traces(p)
  types <- vapply(traces, `[[`, "", "type")
  names <- vapply(traces, `[[`, "", "name")

  expect_equal(names[types == "bar"], c("A", "B"))
  expect_equal(names[types == "scatter"], c("PR", "SD", "PD", "Death"))
  death <- traces[[which(names == "Death")]]
  expect_equal(death$marker$symbol, "x")
  expect_equal(as.vector(death$x), 12)

  layout <- plotly::plotly_build(p)$x$layout
  expect_equal(layout$yaxis$categoryarray, c("S1", "S3", "S2"))
})

# echarts4r ------------------------------------------------------------------

test_that("echarts4r waterfall has one sorted series per fill level", {
  skip_if_not_installed("echarts4r")
  p <- waterfall_plot(tumor, subject, pchg, fill = arm, engine = "echarts4r")
  expect_s3_class(p, "echarts4r")

  series <- p$x$opts$series
  expect_equal(vapply(series, `[[`, "", "name"), c("Drug", "Placebo", "NA"))
  expect_equal(series[[1]]$itemStyle$color, hue_colours(2)[1])
  expect_equal(p$x$opts$xAxis[[1]]$data, c("B", "D", "C", "A"))
  expect_equal(
    vapply(series[[1]]$markLine$data, `[[`, 0, "yAxis"),
    c(20, -30)
  )
  expect_equal(unlist(p$x$opts$legend$data), c("Drug", "Placebo", "NA"))
})

test_that("echarts4r swimlane has lanes and events on a flipped axis", {
  skip_if_not_installed("echarts4r")
  p <- swimlane_plot(lanes, subject, weeks, time, response, fill = arm, engine = "echarts4r")
  series <- p$x$opts$series
  types <- vapply(series, `[[`, "", "type")
  names <- vapply(series, `[[`, "", "name")

  expect_equal(names[types == "bar"], c("A", "B"))
  expect_equal(names[types == "scatter"], c("PR", "SD", "PD", "Death"))
  expect_equal(p$x$opts$xAxis[[1]]$type, "value")
  expect_equal(p$x$opts$yAxis[[1]]$data, c("S1", "S3", "S2"))

  death <- series[[which(names == "Death")]]
  expect_equal(death$data[[1]]$value, list(12, "S3"))
  expect_match(death$symbol, "^path://")
  expect_equal(unlist(p$x$opts$legend$data), c("A", "B", "PR", "SD", "PD", "Death"))
})

test_that("echarts4r swimlane without events or fill hides the legend", {
  skip_if_not_installed("echarts4r")
  p <- swimlane_plot(lanes, subject, weeks, engine = "echarts4r")
  expect_false(p$x$opts$legend$show)
})

test_that("unknown event types fall back to default colours", {
  skip_if_not_installed("plotly")
  lanes$response[1] <- "Dose change"
  traces <- plotly_traces(swimlane_plot(lanes, subject, weeks, time, response, engine = "plotly"))
  points <- traces[vapply(traces, `[[`, "", "type") == "scatter"]
  expect_equal(
    vapply(points, function(t) t$marker$color, ""),
    hue_colours(4)
  )
})
