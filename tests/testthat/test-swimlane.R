test_that("swimlane_ggplot draws lanes, events and ongoing arrows", {
  skip_if_not_installed("ggplot2")
  p <- do.call(swimlane_ggplot, swimlane_args)
  built <- ggplot2::ggplot_build(p)
  expect_length(built$data, 3)
  expect_equal(nrow(built$data[[2]]), nrow(events_df))
  expect_equal(nrow(built$data[[3]]), sum(lanes_df$on_study))
  expect_equal(p$labels$caption, "Arrow indicates ongoing")
})

test_that("swimlane_ggplot works without optional layers", {
  skip_if_not_installed("ggplot2")
  p <- swimlane_ggplot(lanes_df, "subject", "start", "end")
  expect_length(ggplot2::ggplot_build(p)$data, 1)
})

test_that("swimlane_plotly builds one trace per arm plus event and ongoing traces", {
  skip_if_not_installed("plotly")
  built <- plotly::plotly_build(do.call(swimlane_plotly, swimlane_args))
  types <- vapply(built$x$data, `[[`, character(1), "type")
  expect_equal(sum(types == "bar"), 2)
  expect_equal(built$x$layout$barmode, "overlay")
  expect_equal(built$x$layout$yaxis$categoryarray, c("S04", "S02", "S01", "S03"))
})

test_that("swimlane_echarts4r builds custom lane series and scatter events", {
  skip_if_not_installed("echarts4r")
  opts <- do.call(swimlane_echarts4r, swimlane_args)$x$opts
  types <- vapply(opts$series, `[[`, character(1), "type")
  names <- vapply(opts$series, `[[`, character(1), "name")
  expect_equal(types, c("custom", "custom", "scatter", "scatter", "scatter", "scatter"))
  expect_equal(names, c("A", "B", "CR", "PD", "PR", "Ongoing"))
  expect_equal(unlist(opts$yAxis$data), c("S04", "S02", "S01", "S03"))
  # S03 is the top lane (index 3) and runs from 0 to 200.
  expect_equal(opts$series[[1]]$data[[2]], list(0, 200, 3L, "S03"))
  expect_null(opts$title)
})
