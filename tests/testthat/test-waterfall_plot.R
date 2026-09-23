tumor <- data.frame(
  subject = c("A", "B", "C", "D"),
  pchg = c(-50, 30, -10, 5),
  arm = c("Placebo", "Treatment", "Treatment", "Placebo")
)

test_that("returns a ggplot", {
  p <- waterfall_plot(tumor, subject, pchg)
  expect_s3_class(p, "ggplot")
})

test_that("bars are sorted from largest increase to largest decrease", {
  bars <- ggplot2::layer_data(waterfall_plot(tumor, subject, pchg), 1)
  expect_equal(bars$y[order(bars$x)], c(30, 5, -10, -50))
})

test_that("fill maps a column to bar colour", {
  bars <- ggplot2::layer_data(waterfall_plot(tumor, subject, pchg, fill = arm), 1)
  expect_length(unique(bars$fill), 2)
})

test_that("reference lines are configurable", {
  default <- ggplot2::layer_data(waterfall_plot(tumor, subject, pchg), 3)
  expect_equal(default$yintercept, c(20, -30))

  none <- waterfall_plot(tumor, subject, pchg, ref_lines = NULL)
  expect_length(none$layers, 2)
})

test_that("missing values are dropped with a warning", {
  tumor$pchg[2] <- NA
  expect_warning(p <- waterfall_plot(tumor, subject, pchg), "Removed 1 row")
  expect_equal(nrow(ggplot2::layer_data(p, 1)), 3)
})

test_that("invalid input is rejected", {
  expect_error(waterfall_plot(list(), subject, pchg), "data frame")
  expect_error(waterfall_plot(tumor, subject, arm), "numeric")
  expect_error(waterfall_plot(rbind(tumor, tumor), subject, pchg), "unique")
  expect_error(waterfall_plot(tumor, subject, pchg, ref_lines = "a"), "numeric")
})
