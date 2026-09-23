#' Synthetic tumor response data
#'
#' A small, made-up dataset with one row per patient, for trying out
#' [plot_waterfall()]. Column names are deliberately not CDISC names, to show
#' that floaties works with any data standard.
#'
#' @format A data frame with 40 rows and 5 columns:
#' \describe{
#'   \item{patient}{Patient identifier.}
#'   \item{arm}{Treatment arm, `"Drug A"` or `"Drug B"`.}
#'   \item{best_change}{Best percentage change from baseline in the sum of
#'     target lesion diameters.}
#'   \item{response}{Best overall response, a factor with levels `"CR"`,
#'     `"PR"`, `"SD"` and `"PD"`.}
#'   \item{age}{Age in years.}
#' }
#' @source Simulated; see `data-raw/tumor_change.R`.
"tumor_change"

#' Synthetic treatment duration data
#'
#' A small, made-up dataset with one row per patient, for trying out
#' [plot_swimlane()]. Pair it with [response_events]. Column names are
#' deliberately not CDISC names.
#'
#' @format A data frame with 20 rows and 4 columns:
#' \describe{
#'   \item{patient}{Patient identifier.}
#'   \item{arm}{Treatment arm, `"Drug A"` or `"Drug B"`.}
#'   \item{months}{Months on treatment.}
#'   \item{ongoing}{`TRUE` if the patient is still on treatment.}
#' }
#' @source Simulated; see `data-raw/swimlane.R`.
"treatment_duration"

#' Synthetic response events
#'
#' A small, made-up dataset of tumor response events for the patients in
#' [treatment_duration], with any number of rows per patient. For trying out
#' [plot_swimlane()].
#'
#' @format A data frame with 40 rows and 3 columns:
#' \describe{
#'   \item{patient}{Patient identifier.}
#'   \item{month}{Months from the start of treatment to the event.}
#'   \item{event}{Event type, a factor with levels `"CR"`, `"PR"`, `"PD"`
#'     and `"Death"`.}
#' }
#' @source Simulated; see `data-raw/swimlane.R`.
"response_events"
