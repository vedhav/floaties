adtr <- data.frame(
  USUBJID = c("01", "01", "01", "01", "01", "02", "03", "03"),
  PARAMCD = c("SDIAM", "SDIAM", "SDIAM", "SDIAM", "LDIAM1", "SDIAM", "SDIAM", "SDIAM"),
  BASE = c(100, 100, 100, 100, 30, 80, 50, 50),
  PCHG = c(0, -20, -60, -10, -99, 0, 0, 20),
  ABLFL = c("Y", NA, NA, NA, NA, "Y", "Y", NA),
  # Week 2 of subject 01 is an incomplete assessment.
  ANL01FL = c("Y", "Y", NA, "Y", "Y", "Y", "Y", "Y")
)

adrs <- data.frame(
  USUBJID = c("01", "01", "03", "01", "01", "01", "02"),
  PARAMCD = c("BOR", "CBOR", "BOR", "OVR", "OVR", "OVR", "OVR"),
  AVALC = c("PR", "SD", "MISSING", "SD", "PR", "PD", "PD"),
  ADT = as.Date(c(NA, NA, NA, "2024-01-08", "2024-01-22", "2024-01-25", "2024-01-15")),
  ANL01FL = c("Y", "Y", "Y", "Y", "Y", NA, "Y")
)

adsl <- data.frame(
  USUBJID = c("01", "02", "03", "04"),
  TRT01A = c("Drug", "Placebo", "Drug", ""),
  TRTSDT = as.Date(c("2024-01-01", "2024-01-01", "2024-01-01", NA)),
  TRTEDT = as.Date(c("2024-01-29", "2024-01-15", "2024-02-01", NA)),
  DTHDT = as.Date(c(NA, "2024-02-10", NA, NA))
)

# adam_waterfall_data() ------------------------------------------------------

test_that("best percent change uses flagged post-baseline records", {
  expect_message(wf <- adam_waterfall_data(adtr), "Dropped 1 subject")
  expect_equal(wf$USUBJID, c("01", "03"))
  expect_equal(wf$BASE, c(100, 50))
  expect_equal(wf$PCHG, c(-20, 20))

  all_records <- suppressMessages(adam_waterfall_data(adtr, anl_flag = NULL))
  expect_equal(all_records$PCHG, c(-60, 20))
})

test_that("adrs and adsl add best overall response and arm", {
  wf <- suppressMessages(adam_waterfall_data(adtr, adrs = adrs, adsl = adsl))
  expect_equal(as.character(wf$BOR), c("PR", NA))
  expect_equal(levels(wf$BOR), c("CR", "PR", "SD", "NON-CR/NON-PD", "PD", "NE"))
  expect_equal(wf$TRT01A, c("Drug", "Drug"))

  confirmed <- suppressMessages(adam_waterfall_data(adtr, adrs = adrs, bor_param = "CBOR"))
  expect_equal(as.character(confirmed$BOR), c("SD", NA))
})

test_that("adam_waterfall_data rejects invalid input", {
  expect_error(adam_waterfall_data(list()), "data frame")
  expect_error(adam_waterfall_data(adtr[, -4]), "missing column")
  expect_error(adam_waterfall_data(adtr[, -6]), "ANL01FL")
  expect_error(adam_waterfall_data(adtr, param = "XX"), "no records")
  expect_error(adam_waterfall_data(adtr[adtr$ABLFL %in% "Y", ]), "no post-baseline")
  expect_error(
    suppressMessages(adam_waterfall_data(adtr, adrs = rbind(adrs, adrs))),
    "more than one"
  )
})

# adam_swimlane_data() -------------------------------------------------------

test_that("responses and deaths become events relative to first dose", {
  sw <- adam_swimlane_data(adsl, adrs = adrs)
  expect_equal(unique(sw$USUBJID), c("01", "02"))
  expect_equal(sw$DURATION, c(29, 29, 15, 15))
  expect_equal(sw$TIME, c(8, 22, 15, 41))
  expect_equal(as.character(sw$EVENT), c("SD", "PR", "PD", "Death"))
  expect_equal(sw$TRT01A, c("Drug", "Drug", "Placebo", "Placebo"))

  unflagged <- adam_swimlane_data(adsl, adrs = adrs, anl_flag = NULL)
  expect_equal(as.character(unflagged$EVENT), c("SD", "PR", "PD", "PD", "Death"))
})

test_that("without adrs every treated subject gets a lane", {
  expect_message(sw <- adam_swimlane_data(adsl), "Dropped 1 subject")
  expect_equal(unique(sw$USUBJID), c("01", "02", "03"))
  expect_equal(sw$TIME, c(NA, 41, NA))
})

test_that("character dates and time units are handled", {
  chr <- adsl
  chr[c("TRTSDT", "TRTEDT", "DTHDT")] <- lapply(chr[c("TRTSDT", "TRTEDT", "DTHDT")], as.character)
  expect_equal(adam_swimlane_data(chr, adrs), adam_swimlane_data(adsl, adrs))

  weeks <- adam_swimlane_data(adsl, adrs, time_unit = "weeks")
  expect_equal(weeks$TIME[1], 8 / 7)
})

test_that("adam_swimlane_data rejects invalid input", {
  expect_error(adam_swimlane_data(adsl[, -3]), "missing column")
  expect_error(adam_swimlane_data(adsl, adrs, param = "XX"), "no records")
  expect_error(adam_swimlane_data(adsl, adrs, arm = "ACTARM"), "ACTARM")
})

# pharmaverseadam ------------------------------------------------------------

test_that("works on pharmaverseadam oncology data with every engine", {
  skip_if_not_installed("pharmaverseadam")

  wf <- adam_waterfall_data(
    pharmaverseadam::adtr_onco,
    adrs = pharmaverseadam::adrs_onco,
    adsl = pharmaverseadam::adsl
  )
  expect_equal(nrow(wf), 6)
  expect_false(anyNA(wf$BOR))
  # ANL01FL drops this subject's incomplete week 9 assessment (-82%).
  expect_equal(wf$PCHG[wf$USUBJID == "01-701-1118"], -57.692308, tolerance = 1e-6)

  sw <- adam_swimlane_data(
    pharmaverseadam::adsl,
    adrs = pharmaverseadam::adrs_onco,
    time_unit = "weeks"
  )
  expect_equal(length(unique(sw$USUBJID)), 8)
  expect_false(anyNA(sw$DURATION))

  engines <- c(
    "ggplot2",
    if (requireNamespace("plotly", quietly = TRUE)) "plotly",
    if (requireNamespace("echarts4r", quietly = TRUE)) "echarts4r"
  )
  for (engine in engines) {
    expect_no_error(waterfall_plot(wf, USUBJID, PCHG, fill = BOR, engine = engine))
    expect_no_error(
      swimlane_plot(sw, USUBJID, DURATION, TIME, EVENT, fill = TRT01A, engine = engine)
    )
  }
})
