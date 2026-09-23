tr_rec <- function(usubjid, visitnum, testcd, value, lnkid = NA, eval = "INVESTIGATOR") {
  data.frame(
    USUBJID = usubjid,
    TRGRPID = "TARGET",
    TRLNKID = lnkid,
    TRTESTCD = testcd,
    TRSTRESN = value,
    VISIT = ifelse(visitnum == 1, "BASELINE", paste("WEEK", visitnum)),
    VISITNUM = visitnum,
    TREVAL = eval,
    TRDTC = paste0("2024-01-0", visitnum)
  )
}

tr_sum <- rbind(
  tr_rec("01", c(1, 2, 3), "SUMDIAM", c(100, 80, 90)),
  tr_rec("02", c(1, 2), "SUMDIAM", c(50, 60)),
  tr_rec("03", 1, "SUMDIAM", 40)
)

test_that("best percent change is the minimum post-baseline change", {
  expect_message(wf <- sdtm_waterfall_data(tr_sum), "Dropped 1 subject")
  expect_equal(wf$USUBJID, c("01", "02"))
  expect_equal(wf$BASE, c(100, 50))
  expect_equal(wf$PCHG, c(-20, 20))
})

test_that("baseline_visit selects the baseline record", {
  tr <- tr_rec("01", c(1, 2, 3), "SUMDIAM", c(100, 80, 40))
  tr$VISIT[2] <- "SCREENING2"
  wf <- sdtm_waterfall_data(tr, baseline_visit = "SCREENING2")
  expect_equal(wf$PCHG, -50)
})

test_that("lesion diameters are summed when SUMDIAM is absent", {
  tr <- rbind(
    tr_rec("01", c(1, 2, 3), "LDIAM", c(30, 15, 10), lnkid = "T01"),
    tr_rec("01", c(1, 2), "LDIAM", c(20, 15), lnkid = "T02")
  )
  # Week 3 is missing T02, so only week 2 (sum 30 vs 50) counts.
  wf <- sdtm_waterfall_data(tr)
  expect_equal(wf$BASE, 50)
  expect_equal(wf$PCHG, -40)
})

test_that("evaluator filter keeps accepted independent reads", {
  ind <- rbind(
    tr_rec("01", c(1, 2), "SUMDIAM", c(100, 50), eval = "INDEPENDENT ASSESSOR"),
    tr_rec("01", c(1, 2), "SUMDIAM", c(100, 90), eval = "INDEPENDENT ASSESSOR")
  )
  ind$TRACPTFL <- c("Y", "Y", NA, NA)
  tr <- rbind(tr_sum[tr_sum$USUBJID == "01", ], ind[, names(tr_sum)])
  tr$TRACPTFL <- c(NA, NA, NA, ind$TRACPTFL)

  expect_equal(sdtm_waterfall_data(tr)$PCHG, -20)
  expect_equal(sdtm_waterfall_data(tr, evaluator = "INDEPENDENT ASSESSOR")$PCHG, -50)
  expect_error(sdtm_waterfall_data(tr, evaluator = "NOBODY"), "Available")
})

test_that("rs and dm add best overall response and arm", {
  rs <- data.frame(
    USUBJID = c("01", "01", "01", "02", "02"),
    RSTESTCD = "OVRLRESP",
    RSSTRESC = c("SD", "PR", "PD", "PD", "CHECK"),
    RSEVAL = "INVESTIGATOR"
  )
  dm <- data.frame(USUBJID = c("02", "01"), ACTARM = c("Placebo", "Drug"))

  wf <- suppressMessages(sdtm_waterfall_data(tr_sum, rs = rs, dm = dm))
  expect_equal(as.character(wf$BOR), c("PR", "PD"))
  expect_equal(levels(wf$BOR), c("CR", "PR", "SD", "NON-CR/NON-PD", "PD", "NE"))
  expect_equal(wf$ACTARM, c("Drug", "Placebo"))
})

test_that("invalid input is rejected", {
  expect_error(sdtm_waterfall_data(list()), "data frame")
  expect_error(sdtm_waterfall_data(tr_sum[, -1]), "missing column")
  expect_error(sdtm_waterfall_data(tr_sum[tr_sum$VISITNUM == 1, ]), "No subject")
})

test_that("works on pharmaversesdtm oncology data", {
  skip_if_not_installed("pharmaversesdtm")

  wf <- suppressMessages(sdtm_waterfall_data(
    pharmaversesdtm::tr_onco,
    rs = pharmaversesdtm::rs_onco,
    dm = pharmaversesdtm::dm
  ))
  expect_false(anyDuplicated(wf$USUBJID) > 0)
  expect_true(all(is.finite(wf$PCHG)))
  expect_false(anyNA(wf$ACTARM))
  expect_s3_class(waterfall_plot(wf, USUBJID, PCHG, fill = BOR), "ggplot")

  # RECIST dataset has no SUMDIAM and uses SCREENING as baseline.
  # Two of its subjects have no investigator target lesions.
  expect_message(
    recist <- sdtm_waterfall_data(pharmaversesdtm::tr_onco_recist),
    "Dropped 2 subject"
  )
  expect_equal(nrow(recist), 6)
})
