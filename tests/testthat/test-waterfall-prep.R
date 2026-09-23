test_that("prep_waterfall() works with any column names", {
  spec <- prep_waterfall(wf_data(), subject = id, value = change)
  expect_s3_class(spec, c("floaties_waterfall_spec", "floaties_spec"))
  expect_equal(spec$plot, "waterfall")
  expect_equal(nrow(spec$data), 5)
  expect_named(spec$data, c(".subject", ".value", ".fill", ".tooltip"))
  expect_equal(spec$labels$x, "id")
  expect_equal(spec$labels$y, "change")
  expect_equal(spec$ref_lines, c(20, -30))
})

test_that("columns can be given as strings", {
  spec <- prep_waterfall(wf_data(), subject = "id", value = "change")
  expect_equal(as.character(spec$data$.subject), c("c", "e", "a", "d", "b"))
})

test_that("bars are sorted by value", {
  d <- wf_data()
  desc <- prep_waterfall(d, id, change)
  asc <- prep_waterfall(d, id, change, sort = "ascending")
  none <- prep_waterfall(d, id, change, sort = "none")

  expect_equal(desc$data$.value, c(150, 40, 10, -20, -50))
  expect_equal(asc$data$.value, c(-50, -20, 10, 40, 150))
  expect_equal(none$data$.value, d$change)
  expect_equal(levels(desc$data$.subject), c("c", "e", "a", "d", "b"))
})

test_that("ties keep the original row order", {
  d <- data.frame(id = c("p", "q", "r"), change = c(5, 5, 5))
  spec <- prep_waterfall(d, id, change)
  expect_equal(as.character(spec$data$.subject), c("p", "q", "r"))
})

test_that("fill keeps factor level order and marks missing values", {
  spec <- prep_waterfall(wf_data(), id, change, fill = group)
  expect_equal(levels(spec$data$.fill), c("y", "x", "Missing"))
  expect_equal(
    as.character(spec$data$.fill),
    c("x", "y", "x", "Missing", "y")
  )
  expect_equal(spec$labels$fill, "group")
})

test_that("character fill columns become factors", {
  d <- wf_data()
  d$group <- c("b", "a", "b", "a", "a")
  spec <- prep_waterfall(d, id, change, fill = group)
  expect_equal(levels(spec$data$.fill), c("a", "b"))
})

test_that("no fill gives a single level and no legend", {
  spec <- prep_waterfall(wf_data(), id, change)
  expect_equal(nlevels(spec$data$.fill), 1)
  expect_null(spec$labels$fill)
  expect_length(spec$scales$fill, 1)
})

test_that("labels come from label attributes or arguments", {
  d <- wf_data()
  attr(d$id, "label") <- "Unique Subject Identifier"
  attr(d$change, "label") <- "Percent Change from Baseline"
  spec <- prep_waterfall(d, id, change)
  expect_equal(spec$labels$x, "Unique Subject Identifier")
  expect_equal(spec$labels$y, "Percent Change from Baseline")
  expect_match(
    spec$data$.tooltip[[1]],
    "<b>Unique Subject Identifier:</b>",
    fixed = TRUE
  )

  spec <- prep_waterfall(d, id, change, title = "T", y_label = "Change (%)")
  expect_equal(spec$labels$title, "T")
  expect_equal(spec$labels$y, "Change (%)")
})

test_that("tooltips include extra columns and escape HTML", {
  spec <- prep_waterfall(
    wf_data(), id, change,
    fill = group, tooltip = note, sort = "none"
  )
  expect_equal(
    spec$data$.tooltip[[1]],
    paste(
      "<b>id:</b> a", "<b>change:</b> 10", "<b>group:</b> x",
      "<b>note:</b> &lt;b&gt;one&lt;/b&gt;",
      sep = "<br>"
    )
  )
  expect_match(spec$data$.tooltip[[4]], "<b>group:</b> Missing", fixed = TRUE)
})

test_that("missing values are dropped with a warning", {
  d <- wf_data()
  d$change[2] <- NA
  expect_warning(
    spec <- prep_waterfall(d, id, change),
    class = "floaties_warning_missing_values"
  )
  expect_equal(nrow(spec$data), 4)
  expect_false("b" %in% spec$data$.subject)
})

test_that("colors can be named, unnamed, or default", {
  d <- wf_data()
  named <- prep_waterfall(
    d, id, change,
    fill = group,
    colors = c(x = "red", Missing = "grey", y = "blue", extra = "green")
  )
  expect_equal(named$scales$fill, c(y = "blue", x = "red", Missing = "grey"))

  unnamed <- prep_waterfall(
    d, id, change,
    fill = group, colors = c("blue", "red", "grey", "green")
  )
  expect_equal(unnamed$scales$fill, c(y = "blue", x = "red", Missing = "grey"))

  default <- prep_waterfall(d, id, change, fill = group)
  expect_equal(
    default$scales$fill,
    c(y = "#0072B2", x = "#D55E00", Missing = "#999999")
  )
})

test_that("prep_waterfall() validates its input", {
  d <- wf_data()

  expect_snapshot(error = TRUE, {
    prep_waterfall(list(), id, change)
    prep_waterfall(d, value = change)
    prep_waterfall(d, id, missing_col)
    prep_waterfall(d, id, c(change, id))
    prep_waterfall(d, id, note)
    prep_waterfall(d, id, change, sort = "up")
    prep_waterfall(d, id, change, ref_lines = "20")
    prep_waterfall(d, id, change, title = 1)
    prep_waterfall(d, id, change, fill = group, colors = c(x = "red"))
    prep_waterfall(d, id, change, fill = group, colors = "red")
  })

  expect_error(
    prep_waterfall(d, value = change),
    class = "floaties_error_argument"
  )
  expect_error(prep_waterfall(d, id, note), class = "floaties_error_argument")
  expect_error(
    prep_waterfall(d, id, change, fill = group, colors = "red"),
    class = "floaties_error_argument"
  )
})

test_that("subjects must be unique and not missing", {
  dup <- wf_data()
  dup$id[2] <- "a"
  expect_snapshot(error = TRUE, prep_waterfall(dup, id, change))
  expect_error(
    prep_waterfall(dup, id, change),
    class = "floaties_error_duplicate_subject"
  )

  na_id <- wf_data()
  na_id$id[2] <- NA
  expect_error(
    prep_waterfall(na_id, id, change),
    class = "floaties_error_missing_subject"
  )
})

test_that("all-missing values give a clear error", {
  d <- wf_data()
  d$change <- NA_real_
  expect_error(
    suppressWarnings(prep_waterfall(d, id, change)),
    class = "floaties_error_empty_data"
  )
})

test_that("spec has a print method", {
  spec <- prep_waterfall(wf_data(), id, change)
  expect_output(print(spec), "<floaties waterfall spec: 5 subjects>")
})
