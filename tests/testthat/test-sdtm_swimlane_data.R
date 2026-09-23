dm <- data.frame(
  USUBJID = c("01", "02", "03", "04"),
  ACTARM = c("Drug", "Placebo", "Drug", "Screen Failure"),
  RFXSTDTC = c("2024-01-01", "2024-01-01", "2024-01-01", ""),
  RFXENDTC = c("2024-01-29", "2024-01-15", "2024-02-01", ""),
  DTHDTC = c(NA, "2024-02-10", "", NA)
)

rs <- data.frame(
  USUBJID = c("01", "01", "01", "02", "02"),
  RSTESTCD = c("OVRLRESP", "OVRLRESP", "TRGRESP", "OVRLRESP", "OVRLRESP"),
  RSSTRESC = c("SD", "PR", "PR", "PD", "CHECK"),
  RSEVAL = "INVESTIGATOR",
  RSDTC = c("2024-01-08", "2024-01-22", "2024-01-22", "2024-01-15", "2024-01-20")
)

test_that("durations and event times use study days", {
  expect_message(sw <- sdtm_swimlane_data(dm), "Dropped 1 subject")
  expect_equal(unique(sw$USUBJID), c("01", "02", "03"))
  expect_equal(sw$DURATION, c(29, 15, 32))
  # Only subject 02 died; the others have lanes without events.
  expect_equal(sw$TIME, c(NA, 41, NA))
  expect_equal(as.character(sw$EVENT), c(NA, "Death", NA))
})

test_that("rs adds overall response events and restricts subjects", {
  sw <- sdtm_swimlane_data(dm, rs = rs)
  expect_equal(unique(sw$USUBJID), c("01", "02"))
  expect_equal(sw$TIME, c(8, 22, 15, 41))
  expect_equal(as.character(sw$EVENT), c("SD", "PR", "PD", "Death"))
  expect_equal(levels(sw$EVENT), c("CR", "PR", "SD", "NON-CR/NON-PD", "PD", "NE", "Death"))
})

test_that("time_unit rescales durations and times", {
  sw <- sdtm_swimlane_data(dm, rs = rs, time_unit = "weeks")
  expect_equal(sw$DURATION[1], 29 / 7)
  expect_equal(sw$TIME[1], 8 / 7)
})

test_that("events with partial dates are dropped with a message", {
  rs$RSDTC[1] <- "2024-01"
  expect_message(sw <- sdtm_swimlane_data(dm, rs = rs), "Dropped 1 event")
  expect_equal(as.character(sw$EVENT), c("PR", "PD", "Death"))
})

test_that("study days have no day 0", {
  expect_equal(study_day(c("2024-01-01", "2024-01-02", "2023-12-31"), "2024-01-01"), c(1, 2, -1))
  expect_equal(study_day("2024-01-05T10:30", "2024-01-01"), 5)
})

test_that("invalid input is rejected", {
  expect_error(sdtm_swimlane_data(list()), "data frame")
  expect_error(sdtm_swimlane_data(dm[, -2]), "missing column")
  expect_error(sdtm_swimlane_data(dm, time_unit = "years"), "should be one of")
  expect_error(sdtm_swimlane_data(dm, rs, evaluator = "NOBODY"), "Available")
})

test_that("works on pharmaversesdtm oncology data", {
  skip_if_not_installed("pharmaversesdtm")

  sw <- sdtm_swimlane_data(
    pharmaversesdtm::dm,
    rs = pharmaversesdtm::rs_onco,
    time_unit = "weeks"
  )
  expect_equal(length(unique(sw$USUBJID)), 205)
  expect_false(anyNA(sw$DURATION))
  expect_s3_class(
    swimlane_plot(sw, USUBJID, DURATION, TIME, EVENT, fill = ACTARM),
    "ggplot"
  )
})
