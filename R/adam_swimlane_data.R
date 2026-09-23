#' Derive swimlane plot data from ADaM datasets
#'
#' Turns an ADaM subject-level dataset (ADSL) and, optionally, tumor response
#' (ADRS) into the long format used by [swimlane_plot()]: each subject's time
#' on treatment plus one row per response assessment and death.
#'
#' Time on treatment runs from `TRTSDT` to `TRTEDT`. Event times are counted
#' from `TRTSDT` like `ADY`: day 1 is the day of first dose and there is no
#' day 0. Response assessments are dated by `ADT` and deaths by `DTHDT`.
#' Records with missing dates are dropped with a message.
#'
#' @param adsl ADaM ADSL data frame, e.g. `pharmaverseadam::adsl`.
#' @param adrs Optional ADaM ADRS data frame, e.g.
#'   `pharmaverseadam::adrs_onco`. When given, records of the `param`
#'   parameter become events and only subjects with such records are kept.
#' @param param `PARAMCD` of the per-visit overall response in `adrs`.
#' @param arm Name of the treatment arm column in `adsl`.
#' @param anl_flag Name of the analysis flag column in `adrs`; only records
#'   where it is `"Y"` are used. Use `NULL` to use all records.
#' @param time_unit Unit for `DURATION` and `TIME`: `"days"`, `"weeks"`, or
#'   `"months"` (30.4375 days).
#'
#' @return A data frame with columns `USUBJID`, the `arm` column, `DURATION`
#'   (time on treatment), `TIME` (time of the event), and `EVENT` (a factor
#'   of response categories plus `"Death"`). Subjects without events have one
#'   row with `TIME` and `EVENT` set to `NA`.
#' @export
#'
#' @examplesIf requireNamespace("pharmaverseadam", quietly = TRUE)
#' sw <- adam_swimlane_data(
#'   pharmaverseadam::adsl,
#'   adrs = pharmaverseadam::adrs_onco,
#'   time_unit = "weeks"
#' )
#' sw
#'
#' swimlane_plot(
#'   sw,
#'   USUBJID,
#'   DURATION,
#'   TIME,
#'   EVENT,
#'   fill = TRT01A,
#'   xlab = "Weeks since first dose"
#' )
adam_swimlane_data <- function(
  adsl,
  adrs = NULL,
  param = "OVR",
  arm = "TRT01A",
  anl_flag = "ANL01FL",
  time_unit = c("days", "weeks", "months")
) {
  time_unit <- match.arg(time_unit)
  adsl <- check_domain(adsl, "adsl", c("USUBJID", arm, "TRTSDT", "TRTEDT"))

  events <- list(empty_adam_events())
  if (!is.null(adrs)) {
    adrs <- check_domain(adrs, "adrs", c("USUBJID", "PARAMCD", "AVALC", "ADT", anl_flag))
    adrs <- adrs[adrs$PARAMCD %in% param, , drop = FALSE]
    if (!is.null(anl_flag)) {
      adrs <- adrs[adrs[[anl_flag]] %in% "Y", , drop = FALSE]
    }
    if (nrow(adrs) == 0) {
      stop(sprintf("`adrs` has no records with PARAMCD \"%s\".", param), call. = FALSE)
    }
    adsl <- adsl[adsl$USUBJID %in% adrs$USUBJID, , drop = FALSE]

    adrs <- adrs[adrs$AVALC %in% response_levels, , drop = FALSE]
    events$adrs <- data.frame(
      USUBJID = adrs$USUBJID,
      DATE = as_date(adrs$ADT),
      EVENT = adrs$AVALC
    )
  }
  if ("DTHDT" %in% names(adsl)) {
    died <- !is.na(adsl$DTHDT)
    events$death <- data.frame(
      USUBJID = adsl$USUBJID[died],
      DATE = as_date(adsl$DTHDT[died]),
      EVENT = rep("Death", sum(died))
    )
  }

  swimlane_long(
    subjects = data.frame(
      USUBJID = adsl$USUBJID,
      ARM = adsl[[arm]],
      START = as_date(adsl$TRTSDT),
      END = as_date(adsl$TRTEDT)
    ),
    events = do.call(rbind, unname(events)),
    arm_col = arm,
    time_unit = time_unit
  )
}

# ADaM dates are usually Date, but may arrive as datetimes or ISO strings.
as_date <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  parse_dtc(as.character(x))
}

empty_adam_events <- function() {
  data.frame(USUBJID = character(), DATE = as.Date(character()), EVENT = character())
}
