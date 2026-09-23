#' Derive waterfall plot data from SDTM tumor datasets
#'
#' Turns SDTM tumor results (TR), and optionally disease response (RS) and
#' demographics (DM), into one row per subject with the best percent change
#' from baseline in the sum of target lesion diameters. The result can be
#' passed straight to [waterfall_plot()].
#'
#' The sum of diameters is taken from `TRTESTCD == "SUMDIAM"` records when
#' `tr` has them. Otherwise it is summed from the target lesion longest
#' diameters (`TRTESTCD == "LDIAM"`), and a visit counts only when every
#' target lesion measured at baseline was measured again.
#'
#' Baseline is the subject's earliest visit (lowest `VISITNUM`) unless
#' `baseline_visit` is given. The best percent change is the smallest
#' (most negative) change across all post-baseline visits. Subjects without
#' target lesions, a usable baseline, or any post-baseline assessment are
#' dropped with a message.
#'
#' @param tr SDTM TR (Tumor Results) data frame, e.g.
#'   `pharmaversesdtm::tr_onco`.
#' @param rs Optional SDTM RS (Disease Response) data frame. When given, the
#'   best overall response (`RSTESTCD == "OVRLRESP"`) is added as `BOR`.
#' @param dm Optional SDTM DM (Demographics) data frame. When given, the
#'   actual treatment arm is added as `ACTARM`.
#' @param evaluator Value of `TREVAL`/`RSEVAL` to use, such as
#'   `"INVESTIGATOR"` or `"INDEPENDENT ASSESSOR"`. Where an evaluator has
#'   several readers, only records flagged as accepted (`--ACPTFL == "Y"`)
#'   are kept.
#' @param baseline_visit Optional `VISIT` value that marks baseline, such as
#'   `"BASELINE"` or `"SCREENING"`.
#'
#' @return A data frame with one row per subject and columns `USUBJID`,
#'   `BASE` (baseline sum of diameters), `PCHG` (best percent change from
#'   baseline), and, when `rs`/`dm` are supplied, `BOR` (a factor ordered
#'   `CR`, `PR`, `SD`, `NON-CR/NON-PD`, `PD`, `NE`) and `ACTARM`. `BOR` is
#'   the best response recorded at any visit; it does not apply RECIST
#'   confirmation rules.
#' @export
#'
#' @examplesIf requireNamespace("pharmaversesdtm", quietly = TRUE)
#' wf <- sdtm_waterfall_data(
#'   pharmaversesdtm::tr_onco,
#'   rs = pharmaversesdtm::rs_onco,
#'   dm = pharmaversesdtm::dm
#' )
#' head(wf)
#'
#' waterfall_plot(wf, USUBJID, PCHG, fill = BOR)
sdtm_waterfall_data <- function(
  tr,
  rs = NULL,
  dm = NULL,
  evaluator = "INVESTIGATOR",
  baseline_visit = NULL
) {
  tr <- check_domain(
    tr,
    "tr",
    c("USUBJID", "TRGRPID", "TRTESTCD", "TRSTRESN", "VISIT", "VISITNUM", "TREVAL")
  )
  tr <- filter_evaluator(tr, "TR", evaluator)
  n_subjects <- length(unique(tr$USUBJID))
  tr <- tr[tr$TRGRPID %in% "TARGET", , drop = FALSE]
  if (nrow(tr) == 0) {
    stop("`tr` has no target lesion records.", call. = FALSE)
  }

  sums <- sum_of_diameters(tr)

  # Baseline: one record per subject, from `baseline_visit` or earliest visit.
  if (is.null(baseline_visit)) {
    first_visit <- stats::ave(sums$VISITNUM, sums$USUBJID, FUN = min)
    is_base <- sums$VISITNUM == first_visit
  } else {
    is_base <- sums$VISIT %in% baseline_visit
  }
  base <- sums[is_base, , drop = FALSE]
  base <- base[!duplicated(base$USUBJID), , drop = FALSE]
  base <- base[!is.na(base$AVAL) & base$AVAL > 0, , drop = FALSE]
  base <- data.frame(
    USUBJID = base$USUBJID,
    BASE = base$AVAL,
    BASE_VISITNUM = base$VISITNUM,
    BASE_N = base$N
  )

  post <- merge(sums, base, by = "USUBJID")
  post <- post[
    post$VISITNUM > post$BASE_VISITNUM &
      !is.na(post$AVAL) &
      (is.na(post$N) | post$N == post$BASE_N),
    ,
    drop = FALSE
  ]
  if (nrow(post) == 0) {
    stop("No subject has both a baseline and a post-baseline sum of diameters.", call. = FALSE)
  }
  post$PCHG <- 100 * (post$AVAL - post$BASE) / post$BASE

  best <- stats::aggregate(PCHG ~ USUBJID + BASE, data = post, FUN = min)
  out <- best[order(best$USUBJID), c("USUBJID", "BASE", "PCHG")]

  n_dropped <- n_subjects - nrow(out)
  if (n_dropped > 0) {
    message(sprintf(
      "Dropped %d subject(s) without target lesions measured at baseline and post-baseline.",
      n_dropped
    ))
  }

  if (!is.null(rs)) {
    out$BOR <- best_overall_response(rs, evaluator)[out$USUBJID]
    out$BOR <- factor(out$BOR, levels = response_levels)
  }

  if (!is.null(dm)) {
    dm <- check_domain(dm, "dm", c("USUBJID", "ACTARM"))
    out$ACTARM <- dm$ACTARM[match(out$USUBJID, dm$USUBJID)]
  }

  rownames(out) <- NULL
  out
}

# Best-to-worst order of RECIST overall response.
response_levels <- c("CR", "PR", "SD", "NON-CR/NON-PD", "PD", "NE")

check_domain <- function(x, name, cols) {
  if (!is.data.frame(x)) {
    stop(sprintf("`%s` must be a data frame.", name), call. = FALSE)
  }
  missing <- setdiff(cols, names(x))
  if (length(missing) > 0) {
    stop(
      sprintf("`%s` is missing column(s): %s.", name, paste(missing, collapse = ", ")),
      call. = FALSE
    )
  }
  as.data.frame(x)
}

filter_evaluator <- function(x, prefix, evaluator) {
  eval_col <- paste0(prefix, "EVAL")
  kept <- x[x[[eval_col]] %in% evaluator, , drop = FALSE]
  if (nrow(kept) == 0) {
    stop(
      sprintf(
        "No %s records for evaluator \"%s\". Available: %s.",
        prefix,
        evaluator,
        paste(unique(stats::na.omit(x[[eval_col]])), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  accept_col <- paste0(prefix, "ACPTFL")
  if (accept_col %in% names(kept) && any(kept[[accept_col]] %in% "Y")) {
    kept <- kept[kept[[accept_col]] %in% "Y", , drop = FALSE]
  }
  kept
}

# One row per subject and assessment with the sum of target diameters (AVAL)
# and, when summed from lesions, the number of lesions measured (N).
sum_of_diameters <- function(tr) {
  if (any(tr$TRTESTCD == "SUMDIAM")) {
    sums <- tr[tr$TRTESTCD == "SUMDIAM", , drop = FALSE]
    return(data.frame(
      USUBJID = sums$USUBJID,
      VISIT = sums$VISIT,
      VISITNUM = sums$VISITNUM,
      AVAL = sums$TRSTRESN,
      N = NA_integer_
    ))
  }

  lesions <- tr[tr$TRTESTCD == "LDIAM", , drop = FALSE]
  if (nrow(lesions) == 0) {
    stop("`tr` has neither SUMDIAM nor LDIAM target lesion records.", call. = FALSE)
  }

  # Unscheduled visits can share a VISITNUM, so keep assessment dates apart.
  date <- if ("TRDTC" %in% names(lesions)) lesions$TRDTC else ""
  key <- paste(lesions$USUBJID, lesions$VISITNUM, date, sep = "\r")
  first <- !duplicated(key)
  data.frame(
    USUBJID = lesions$USUBJID[first],
    VISIT = lesions$VISIT[first],
    VISITNUM = lesions$VISITNUM[first],
    AVAL = as.vector(tapply(lesions$TRSTRESN, key, sum)[key[first]]),
    N = as.vector(tapply(lesions$TRSTRESN, key, length)[key[first]])
  )
}

# Named character vector of best overall response per subject.
best_overall_response <- function(rs, evaluator) {
  rs <- check_domain(rs, "rs", c("USUBJID", "RSTESTCD", "RSSTRESC", "RSEVAL"))
  rs <- rs[rs$RSTESTCD %in% "OVRLRESP", , drop = FALSE]
  rs <- filter_evaluator(rs, "RS", evaluator)

  rank <- match(rs$RSSTRESC, response_levels)
  rs <- rs[!is.na(rank), , drop = FALSE]
  rank <- rank[!is.na(rank)]

  best <- tapply(rank, rs$USUBJID, min)
  stats::setNames(response_levels[best], names(best))
}
