test_that("engine constructors produce dispatchable objects", {
  e <- eng_ggplot2()
  expect_s3_class(e, "floaties_ggplot2")
  expect_s3_class(e, "floaties_engine")
  expect_identical(engine_name(e), "ggplot2")
})

test_that("new_engine is enough to add an engine from outside the package", {
  e <- new_engine("text", width = 40)
  expect_s3_class(e, c("floaties_text", "floaties_engine"))
  expect_identical(e$width, 40)
})

test_that("new_engine insists on a single string", {
  expect_error(new_engine(1), "must be a single string")
  expect_error(new_engine(c("a", "b")), "must be a single string")
})

test_that("engines print readably", {
  expect_output(print(eng_ggplot2()), "floaties engine: ggplot2")
  expect_output(print(new_engine("text", width = 40)), "options: width")
})
