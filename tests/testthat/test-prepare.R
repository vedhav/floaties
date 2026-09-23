test_that("prepare_swimlane orders lanes by duration, longest at the top", {
  prep <- do.call(prepare_swimlane, swimlane_args)
  expect_equal(levels(prep$lanes$.id), c("S04", "S02", "S01", "S03"))
  expect_equal(prep$lanes$.ongoing, c(FALSE, FALSE, TRUE, TRUE))
  expect_true(prep$has_color)
  expect_equal(levels(prep$events$.id), levels(prep$lanes$.id))
})

test_that("prepare_swimlane keeps data order top-to-bottom when sort = FALSE", {
  prep <- prepare_swimlane(lanes_df, "subject", "start", "end", sort = FALSE)
  expect_equal(levels(prep$lanes$.id), rev(lanes_df$subject))
  expect_false(prep$has_color)
  expect_null(prep$events)
})

test_that("prepare_swimlane validates its inputs", {
  expect_error(prepare_swimlane(list(), "subject", "start", "end"), "must be a data frame")
  expect_error(prepare_swimlane(lanes_df, "subject", "start", "stop"), "not found in `data`: stop")
  expect_error(prepare_swimlane(lanes_df, "subject", "start", "arm"), "must be numeric")
  expect_error(
    prepare_swimlane(rbind(lanes_df, lanes_df), "subject", "start", "end"),
    "one row per subject"
  )
  bad <- lanes_df
  bad$end[1] <- -1
  expect_error(prepare_swimlane(bad, "subject", "start", "end"), "must not be earlier")
  expect_error(
    prepare_swimlane(lanes_df, "subject", "start", "end", events = events_df),
    "are required"
  )
})

test_that("prepare_swimlane drops events for unknown subjects", {
  ev <- rbind(events_df, data.frame(subject = "S99", day = 1, response = "PR"))
  expect_warning(
    prep <- prepare_swimlane(
      lanes_df, "subject", "start", "end",
      events = ev, event_time = "day", event_type = "response"
    ),
    "Dropping 1 event"
  )
  expect_equal(nrow(prep$events), nrow(events_df))
})

test_that("prepare_waterfall sorts bars from largest increase to deepest reduction", {
  prep <- do.call(prepare_waterfall, waterfall_args)
  expect_equal(prep$bars$.value, c(35, 12, -5, -31, -45, -100))
  expect_equal(levels(prep$bars$.id), c("S02", "S04", "S06", "S05", "S01", "S03"))
  expect_equal(as.character(prep$bars$.fill), c("PD", "SD", "SD", "PR", "PR", "CR"))
})

test_that("prepare_waterfall validates its inputs", {
  expect_error(prepare_waterfall(tumour_df, "subject", "response"), "must be numeric")
  expect_error(prepare_waterfall(tumour_df, c("a", "b"), "best_change"), "single column name")
  bad <- tumour_df
  bad$best_change[1] <- NA
  expect_error(prepare_waterfall(bad, "subject", "best_change"), "missing values")
})
