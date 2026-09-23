#' Derive waterfall plot data from ADaM tumor datasets
#'
#' Turns an ADaM tumor results dataset (ADTR), and optionally tumor response
#' (ADRS) and subject-level (ADSL) datasets, into one row per subject with the
#' best percent change from baseline. The result can be passed straight to
#' [waterfall_plot()].
#'
#' Unlike [sdtm_waterfall_data()], nothing is derived from raw measurements:
#' the percent change (`PCHG`) and baseline (`BASE`, `ABLFL`) come from ADTR,
#' and the best overall response is read from its ADRS parameter. The best
#' percent change is the smallest (most negative) `PCHG` across post-baseline
#' records. Subjects without any post-baseline `PCHG` are dropped with a
#' message.
#'
#' @param adtr ADaM ADTR data frame, e.g. `pharmaverseadam::adtr_onco`.
#' @param adrs Optional ADaM ADRS data frame, e.g.
#'   `pharmaverseadam::adrs_onco`. When given, the best overall response is
#'   added as `BOR`.
#' @param adsl Optional ADaM ADSL data frame, e.g. `pharmaverseadam::adsl`.
#'   When given, the treatment arm is added.
#' @param param `PARAMCD` of the sum of diameters in `adtr`.
#' @param bor_param `PARAMCD` of the best overall response in `adrs`. Use
#'   `"CBOR"` for the confirmed best overall response where the study has it.
#' @param arm Name of the treatment arm column in `adsl`.
#' @param anl_flag Name of the analysis flag column in `adtr`; only records
#'   where it is `"Y"` are used. In the admiral ADTR template `ANL01FL`
#'   excludes visits where not every target lesion was assessed, whose sums
#'   of diameters would overstate shrinkage. Use `NULL` to use all records.
#'
#' @return A data frame with one row per subject and columns `USUBJID`,
#'   `BASE`, `PCHG` (best percent change from baseline), and, when
#'   `adrs`/`adsl` are supplied, `BOR` (a factor ordered `CR`, `PR`, `SD`,
#'   `NON-CR/NON-PD`, `PD`, `NE`, with other values such as `"MISSING"` set
#'   to `NA`) and the `arm` column.
#' @export
#'
#' @examplesIf requireNamespace("pharmaverseadam", quietly = TRUE)
#' wf <- adam_waterfall_data(
#'   pharmaverseadam::adtr_onco,
#'   adrs = pharmaverseadam::adrs_onco,
#'   adsl = pharmaverseadam::adsl
#' )
#' wf
#'
#' waterfall_plot(wf, USUBJID, PCHG, fill = BOR)
adam_waterfall_data <- function(
  adtr,
  adrs = NULL,
  adsl = NULL,
  param = "SDIAM",
  bor_param = "BOR",
  arm = "TRT01A",
  anl_flag = "ANL01FL"
) {
  adtr <- check_domain(
    adtr,
    "adtr",
    c("USUBJID", "PARAMCD", "BASE", "PCHG", "ABLFL", anl_flag)
  )
  adtr <- adtr[adtr$PARAMCD %in% param, , drop = FALSE]
  if (!is.null(anl_flag)) {
    adtr <- adtr[adtr[[anl_flag]] %in% "Y", , drop = FALSE]
  }
  if (nrow(adtr) == 0) {
    stop(sprintf("`adtr` has no records with PARAMCD \"%s\".", param), call. = FALSE)
  }

  post <- adtr[
    !adtr$ABLFL %in% "Y" & !is.na(adtr$PCHG) & !is.na(adtr$BASE) & adtr$BASE > 0,
    ,
    drop = FALSE
  ]
  if (nrow(post) == 0) {
    stop("`adtr` has no post-baseline records with a percent change.", call. = FALSE)
  }

  best <- stats::aggregate(PCHG ~ USUBJID + BASE, data = post, FUN = min)
  out <- best[order(best$USUBJID), c("USUBJID", "BASE", "PCHG")]

  n_dropped <- length(unique(adtr$USUBJID)) - nrow(out)
  if (n_dropped > 0) {
    message(sprintf(
      "Dropped %d subject(s) without a post-baseline percent change.",
      n_dropped
    ))
  }

  if (!is.null(adrs)) {
    adrs <- check_domain(adrs, "adrs", c("USUBJID", "PARAMCD", "AVALC"))
    bor <- adrs[adrs$PARAMCD %in% bor_param, , drop = FALSE]
    if (nrow(bor) == 0) {
      stop(sprintf("`adrs` has no records with PARAMCD \"%s\".", bor_param), call. = FALSE)
    }
    if (anyDuplicated(bor$USUBJID)) {
      stop(
        sprintf("`adrs` has more than one \"%s\" record per subject.", bor_param),
        call. = FALSE
      )
    }
    out$BOR <- factor(
      bor$AVALC[match(out$USUBJID, bor$USUBJID)],
      levels = response_levels
    )
  }

  if (!is.null(adsl)) {
    adsl <- check_domain(adsl, "adsl", c("USUBJID", arm))
    out[[arm]] <- adsl[[arm]][match(out$USUBJID, adsl$USUBJID)]
  }

  rownames(out) <- NULL
  out
}
