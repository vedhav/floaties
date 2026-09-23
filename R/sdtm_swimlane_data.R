#' Derive swimlane plot data from SDTM datasets
#'
#' Turns SDTM demographics (DM) and, optionally, disease response (RS) into
#' the long format used by [swimlane_plot()]: each subject's time on
#' treatment plus one row per response assessment and death.
#'
#' Time on treatment runs from first to last dose (`RFXSTDTC` to `RFXENDTC`).
#' Event times use the SDTM study day convention relative to first dose:
#' day 1 is the day of first dose and there is no day 0. Records with
#' missing or partial dates are dropped with a message.
#'
#' @param dm SDTM DM (Demographics) data frame, e.g. `pharmaversesdtm::dm`.
#' @param rs Optional SDTM RS (Disease Response) data frame, e.g.
#'   `pharmaversesdtm::rs_onco`. When given, overall response assessments
#'   (`RSTESTCD == "OVRLRESP"`) become events and only subjects with
#'   response records are kept.
#' @param evaluator Value of `RSEVAL` to use, such as `"INVESTIGATOR"` or
#'   `"INDEPENDENT ASSESSOR"`. Where an evaluator has several readers, only
#'   records flagged as accepted (`RSACPTFL == "Y"`) are kept.
#' @param time_unit Unit for `DURATION` and `TIME`: `"days"`, `"weeks"`, or
#'   `"months"` (30.4375 days).
#'
#' @return A data frame with columns `USUBJID`, `ACTARM`, `DURATION` (time on
#'   treatment), `TIME` (time of the event), and `EVENT` (a factor of
#'   response categories plus `"Death"`). Subjects without events have one
#'   row with `TIME` and `EVENT` set to `NA`.
#' @export
#'
#' @examplesIf requireNamespace("pharmaversesdtm", quietly = TRUE)
#' sw <- sdtm_swimlane_data(
#'   pharmaversesdtm::dm,
#'   rs = pharmaversesdtm::rs_onco,
#'   time_unit = "weeks"
#' )
#' head(sw)
#'
#' # Swimlane plots usually show responders only.
#' responders <- unique(sw$USUBJID[sw$EVENT %in% c("CR", "PR")])
#' swimlane_plot(
#'   sw[sw$USUBJID %in% responders[1:20], ],
#'   USUBJID,
#'   DURATION,
#'   TIME,
#'   EVENT,
#'   fill = ACTARM,
#'   xlab = "Weeks since first dose"
#' )
sdtm_swimlane_data <- function(
  dm,
  rs = NULL,
  evaluator = "INVESTIGATOR",
  time_unit = c("days", "weeks", "months")
) {
  time_unit <- match.arg(time_unit)
  dm <- check_domain(dm, "dm", c("USUBJID", "ACTARM", "RFXSTDTC", "RFXENDTC"))

  if (!is.null(rs)) {
    rs <- check_domain(rs, "rs", c("USUBJID", "RSTESTCD", "RSSTRESC", "RSEVAL", "RSDTC"))
    rs <- rs[rs$RSTESTCD %in% "OVRLRESP", , drop = FALSE]
    rs <- filter_evaluator(rs, "RS", evaluator)
    dm <- dm[dm$USUBJID %in% rs$USUBJID, , drop = FALSE]
  }

  events <- list()
  if (!is.null(rs)) {
    rs <- rs[rs$RSSTRESC %in% response_levels, , drop = FALSE]
    events$rs <- data.frame(
      USUBJID = rs$USUBJID,
      DTC = rs$RSDTC,
      EVENT = rs$RSSTRESC
    )
  }
  if ("DTHDTC" %in% names(dm)) {
    died <- !is.na(dm$DTHDTC) & dm$DTHDTC != ""
    events$death <- data.frame(
      USUBJID = dm$USUBJID[died],
      DTC = dm$DTHDTC[died],
      EVENT = rep("Death", sum(died))
    )
  }
  events <- do.call(rbind, c(list(empty_events()), unname(events)))

  swimlane_long(
    subjects = data.frame(
      USUBJID = dm$USUBJID,
      ARM = dm$ACTARM,
      START = parse_dtc(dm$RFXSTDTC),
      END = parse_dtc(dm$RFXENDTC)
    ),
    events = data.frame(
      USUBJID = events$USUBJID,
      DATE = parse_dtc(events$DTC),
      EVENT = events$EVENT
    ),
    arm_col = "ACTARM",
    time_unit = time_unit
  )
}

# Long swimlane data shared by sdtm_swimlane_data() and adam_swimlane_data().
# `subjects` has USUBJID, ARM, START, and END (first and last dose dates);
# `events` has USUBJID, DATE, and EVENT. The arm column is named `arm_col`.
swimlane_long <- function(subjects, events, arm_col, time_unit) {
  subjects$DURATION <- day_number(subjects$END, subjects$START)
  no_dates <- is.na(subjects$DURATION)
  if (any(no_dates)) {
    message(sprintf(
      "Dropped %d subject(s) without complete first and last dose dates.",
      sum(no_dates)
    ))
    subjects <- subjects[!no_dates, , drop = FALSE]
  }
  if (nrow(subjects) == 0) {
    stop("No subjects with complete first and last dose dates.", call. = FALSE)
  }

  events <- events[events$USUBJID %in% subjects$USUBJID, , drop = FALSE]
  first_dose <- subjects$START[match(events$USUBJID, subjects$USUBJID)]
  events$TIME <- day_number(events$DATE, first_dose)
  undated <- is.na(events$TIME)
  if (any(undated)) {
    message(sprintf("Dropped %d event(s) without a complete date.", sum(undated)))
    events <- events[!undated, , drop = FALSE]
  }

  # Subjects without events keep one row so their lane is still drawn.
  no_events <- setdiff(subjects$USUBJID, events$USUBJID)
  events <- rbind(
    events[, c("USUBJID", "TIME", "EVENT")],
    data.frame(
      USUBJID = no_events,
      TIME = rep(NA_real_, length(no_events)),
      EVENT = rep(NA_character_, length(no_events))
    )
  )

  out <- merge(subjects[, c("USUBJID", "ARM", "DURATION")], events, by = "USUBJID")
  divisor <- c(days = 1, weeks = 7, months = 30.4375)[[time_unit]]
  out$DURATION <- out$DURATION / divisor
  out$TIME <- out$TIME / divisor
  out$EVENT <- factor(out$EVENT, levels = c(response_levels, "Death"))

  out <- out[order(out$USUBJID, out$TIME), , drop = FALSE]
  names(out)[names(out) == "ARM"] <- arm_col
  rownames(out) <- NULL
  out
}

# Day number of `date` relative to `ref`, with no day 0 (SDTM --DY / ADaM ADY).
day_number <- function(date, ref) {
  days <- as.numeric(date - ref)
  ifelse(days >= 0, days + 1, days)
}

# Complete ISO 8601 dates as Date; partial dates become NA.
parse_dtc <- function(dtc) {
  as.Date(substr(dtc, 1, 10), format = "%Y-%m-%d")
}

# Study day of `dtc` relative to `ref_dtc`, with no day 0. NA for partial dates.
study_day <- function(dtc, ref_dtc) {
  day_number(parse_dtc(dtc), parse_dtc(ref_dtc))
}

empty_events <- function() {
  data.frame(USUBJID = character(), DTC = character(), EVENT = character())
}
