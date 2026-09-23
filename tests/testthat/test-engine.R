test_that("dispatchers reject unknown engines", {
  expect_error(do.call(swimlane_plot, c(swimlane_args, engine = "lattice")), "engine")
  expect_error(do.call(waterfall_plot, c(waterfall_args, engine = "lattice")), "engine")
})

test_that("dispatchers route to the requested engine", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("plotly")
  skip_if_not_installed("echarts4r")

  expect_s3_class(do.call(swimlane_plot, c(swimlane_args, engine = "ggplot2")), "ggplot")
  expect_s3_class(do.call(swimlane_plot, c(swimlane_args, engine = "plotly")), "plotly")
  expect_s3_class(do.call(swimlane_plot, c(swimlane_args, engine = "echarts4r")), "echarts4r")
  expect_s3_class(do.call(waterfall_plot, c(waterfall_args, engine = "ggplot2")), "ggplot")
  expect_s3_class(do.call(waterfall_plot, c(waterfall_args, engine = "plotly")), "plotly")
  expect_s3_class(do.call(waterfall_plot, c(waterfall_args, engine = "echarts4r")), "echarts4r")
})

test_that("the floaties.engine option sets the default engine", {
  skip_if_not_installed("plotly")
  withr::local_options(floaties.engine = "plotly")
  expect_s3_class(do.call(waterfall_plot, waterfall_args), "plotly")
})
