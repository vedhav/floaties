# Nearly all the logic worth testing is in prepare_waterfall(), and none of it
# needs a plotting library. These run fast and are where a regression shows up.

test_that("the canonical contract holds, whatever the columns were called", {
  pd <- prepare_waterfall(wf_input(), id, change, color_var = response)

  expect_s3_class(pd, "floaties_data_waterfall")
  expect_identical(sort(names(pd)), c(".group", ".id", ".value"))
  expect_true(is.factor(pd$.id))
  expect_true(is.numeric(pd$.value))
})

test_that("sort_var = NULL sorts by value, descending", {
  pd <- prepare_waterfall(wf_input(), id, change)

  expect_false(is.unsorted(rev(pd$.value)))
  expect_identical(as.character(pd$.id[1]), "01-008")   # +57, the worst
  expect_identical(levels(pd$.id), as.character(pd$.id))
})

test_that("sort_var = FALSE keeps the input order", {
  pd <- prepare_waterfall(wf_input(), id, change, sort_var = FALSE)
  expect_identical(as.character(pd$.id), wf_input()$id)
})

test_that("an explicit sort_var is used", {
  d <- transform(wf_input(), rank = 8:1)
  pd <- prepare_waterfall(d, id, change, sort_var = rank)
  expect_identical(as.character(pd$.id[1]), "01-001")
})

test_that("missing values are dropped, and the caller is told", {
  d <- transform(wf_input(), change = replace(wf_input()$change, 1, NA_real_))

  expect_warning(pd <- prepare_waterfall(d, id, change), "Dropped 1 row")
  expect_equal(nrow(pd), nrow(d) - 1L)
})

test_that("duplicated subjects are a warning, not a silent wrong chart", {
  d <- rbind(wf_input(), wf_input()[1, ])
  expect_warning(prepare_waterfall(d, id, change), "duplicated value")
})

test_that("no color_var gives an all-NA group rather than a missing column", {
  pd <- prepare_waterfall(wf_input(), id, change)
  expect_true(".group" %in% names(pd))
  expect_true(all(is.na(pd$.group)))
})

test_that("zero rows is a plot, not an error", {
  pd <- prepare_waterfall(wf_input()[0, ], id, change)
  expect_equal(nrow(pd), 0L)
  expect_s3_class(pd, "floaties_data_waterfall")
})

test_that("a non-numeric value_var is refused with a useful message", {
  d <- transform(wf_input(), change = as.character(response))
  expect_error(prepare_waterfall(d, id, change), "must be numeric")
})

test_that("data must be a data frame", {
  expect_error(prepare_waterfall(1:10, id, change), "must be a data frame")
})

test_that("the .data pronoun works, so Shiny inputs need no special case", {
  col <- "change"
  pd <- prepare_waterfall(wf_input(), id, .data[[col]])
  expect_identical(pd$.value, sort(wf_input()$change, decreasing = TRUE))
})

test_that("hlines and title survive as attributes", {
  pd <- prepare_waterfall(wf_input(), id, change,
                          hlines = c(20, -30), title = "t")
  expect_identical(attr(pd, "hlines"), c(20, -30))
  expect_identical(attr(pd, "title"), "t")
})

test_that("the validator rejects a broken contract", {
  pd <- prepare_waterfall(wf_input(), id, change)

  broken <- pd
  broken$.id <- as.character(broken$.id)
  expect_error(validate_waterfall_data(broken), "must be a factor")

  missing <- pd
  missing$.value <- NULL
  expect_error(validate_waterfall_data(missing), "missing column")

  expect_error(validate_waterfall_data(data.frame(a = 1)),
               "must be a <floaties_data_waterfall>")
})
