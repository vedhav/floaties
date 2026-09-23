test_that("the canonical contract holds", {
  pd <- prepare_swimlane(sl_input(), subject, day, stop, what)

  expect_s3_class(pd, "floaties_data_swimlane")
  expect_identical(sort(names(pd)), c(".end", ".id", ".start", ".type"))
  expect_true(is.factor(pd$.id))
  expect_true(is.numeric(pd$.start))
  expect_false(any(is.na(pd$.start)))
})

test_that("a missing end means a point event, not a dropped row", {
  pd <- prepare_swimlane(sl_input(), subject, day, stop, what)
  expect_equal(nrow(pd), nrow(sl_input()))
  expect_equal(sum(is.na(pd$.end)), 2L)
})

test_that("end_var can be omitted entirely for event-only data", {
  pd <- prepare_swimlane(sl_input(), subject, day)
  expect_true(all(is.na(pd$.end)))
})

test_that("lanes sort longest-on-study last, so the longest is drawn on top", {
  pd <- prepare_swimlane(sl_input(), subject, day, stop, what)
  # 01-003 runs to 120, the longest, so it is the last level.
  expect_identical(utils::tail(levels(pd$.id), 1), "01-003")
})

test_that("lane ordering is selectable", {
  d <- sl_input()
  expect_identical(
    levels(prepare_swimlane(d, subject, day, stop, sort_lanes = "id")$.id),
    sort(unique(d$subject))
  )
  expect_identical(
    levels(prepare_swimlane(d, subject, day, stop, sort_lanes = "input")$.id),
    unique(d$subject)
  )
  expect_error(prepare_swimlane(d, subject, day, sort_lanes = "nope"))
})

test_that("an interval that ends before it starts becomes an event, loudly", {
  d <- sl_input()
  d$stop[1] <- d$day[1] - 5

  expect_warning(pd <- prepare_swimlane(d, subject, day, stop), "end before")
  expect_true(is.na(pd$.end[pd$.id == "01-001" & pd$.start == 1]))
})

test_that("a missing start is dropped, and the caller is told", {
  d <- sl_input()
  d$day[1] <- NA_real_

  expect_warning(pd <- prepare_swimlane(d, subject, day, stop), "Dropped 1 row")
  expect_equal(nrow(pd), nrow(d) - 1L)
})

test_that("overlapping intervals on one lane are kept, not merged", {
  # 01-001 is dosed 1-84 and again 40-60. Both must survive: merging them
  # would silently rewrite the subject's history.
  pd <- prepare_swimlane(sl_input(), subject, day, stop)
  ints <- pd[pd$.id == "01-001" & !is.na(pd$.end), ]
  expect_equal(nrow(ints), 2L)
})

test_that("zero rows is a plot, not an error", {
  pd <- prepare_swimlane(sl_input()[0, ], subject, day, stop)
  expect_equal(nrow(pd), 0L)
  expect_equal(nlevels(pd$.id), 0L)
})

test_that("the validator rejects a broken contract", {
  pd <- prepare_swimlane(sl_input(), subject, day, stop)

  broken <- pd
  broken$.start[1] <- NA_real_
  expect_error(validate_swimlane_data(broken), "must not contain missing")

  missing <- pd
  missing$.type <- NULL
  expect_error(validate_swimlane_data(missing), "missing column")
})
