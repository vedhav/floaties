lanes <- data.frame(
  subject = c("S1", "S1", "S2", "S3", "S3"),
  weeks = c(10, 10, 30, 20, 20),
  arm = c("A", "A", "B", "A", "A"),
  time = c(4, 8, NA, 6, 12),
  response = c("SD", "PR", NA, "PD", "Death")
)

test_that("returns a ggplot with one lane per subject", {
  p <- swimlane_plot(lanes, subject, weeks, time, response)
  expect_s3_class(p, "ggplot")
  expect_equal(nrow(ggplot2::layer_data(p, 1)), 3)
})

test_that("longest lane is on top", {
  bars <- ggplot2::layer_data(swimlane_plot(lanes, subject, weeks), 1)
  expect_equal(bars$xmax[order(bars$y)], c(10, 20, 30))
})

test_that("events are drawn as points, skipping rows without events", {
  p <- swimlane_plot(lanes, subject, weeks, time, response)
  points <- ggplot2::layer_data(p, 2)
  expect_equal(nrow(points), 4)
  expect_equal(sort(points$x), c(4, 6, 8, 12))
})

test_that("response events get standard colours", {
  p <- swimlane_plot(lanes, subject, weeks, time, response)
  points <- ggplot2::layer_data(p, 2)
  expect_equal(points$colour[points$x == 12], "black")
  expect_equal(points$shape[points$x == 12], 4)

  other <- lanes
  other$response[1] <- "Dose change"
  expect_s3_class(swimlane_plot(other, subject, weeks, time, response), "ggplot")
})

test_that("fill colours lanes", {
  bars <- ggplot2::layer_data(swimlane_plot(lanes, subject, weeks, fill = arm), 1)
  expect_length(unique(bars$fill), 2)
})

test_that("missing durations are dropped with a warning", {
  lanes$weeks[lanes$subject == "S2"] <- NA
  expect_warning(p <- swimlane_plot(lanes, subject, weeks), "Removed 1 subject")
  expect_equal(nrow(ggplot2::layer_data(p, 1)), 2)
})

test_that("invalid input is rejected", {
  expect_error(swimlane_plot(list(), subject, weeks), "data frame")
  expect_error(swimlane_plot(lanes, subject, arm), "numeric")
  expect_error(swimlane_plot(lanes, subject, weeks, time), "together")

  lanes$weeks[1] <- 99
  expect_error(swimlane_plot(lanes, subject, weeks), "same on every row")
})
