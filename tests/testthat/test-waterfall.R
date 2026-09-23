test_that("waterfall_ggplot draws sorted bars and reference lines", {
  skip_if_not_installed("ggplot2")
  p <- do.call(waterfall_ggplot, waterfall_args)
  built <- ggplot2::ggplot_build(p)
  expect_equal(built$data[[1]]$y, c(35, 12, -5, -31, -45, -100))
  expect_equal(built$data[[3]]$yintercept, c(20, -30))
})

test_that("waterfall_ggplot omits reference lines when ref_lines = NULL", {
  skip_if_not_installed("ggplot2")
  p <- waterfall_ggplot(tumour_df, "subject", "best_change", ref_lines = NULL)
  expect_length(p$layers, 2)
})

test_that("waterfall_plotly draws one trace per response with reference shapes", {
  skip_if_not_installed("plotly")
  built <- plotly::plotly_build(do.call(waterfall_plotly, waterfall_args))
  expect_length(built$x$data, 4)
  expect_length(built$x$layout$shapes, 2)
  expect_equal(built$x$layout$xaxis$categoryarray, c("S02", "S04", "S06", "S05", "S01", "S03"))
})

test_that("waterfall_echarts4r pads each series to every subject", {
  skip_if_not_installed("echarts4r")
  opts <- do.call(waterfall_echarts4r, c(waterfall_args, title = "Best response"))$x$opts
  expect_length(opts$series, 4)
  expect_equal(vapply(opts$series, `[[`, character(1), "name"), c("CR", "PD", "PR", "SD"))
  expect_equal(opts$series[[1]]$data, list("-", "-", "-", "-", "-", -100))
  expect_length(opts$series[[1]]$markLine$data, 2)
  expect_equal(opts$title$text, "Best response")
})
