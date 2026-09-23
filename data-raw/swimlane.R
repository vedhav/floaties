## Code to prepare the synthetic `treatment_duration` and `response_events`
## datasets used for swimlane plots.
##
## Column names are deliberately not CDISC names, to show that floaties works
## with any data standard.

set.seed(20260924)

n <- 20
patients <- sprintf("PT-%03d", seq_len(n))

treatment_duration <- data.frame(
  patient = patients,
  arm = sample(c("Drug A", "Drug B"), n, replace = TRUE),
  months = round(runif(n, 2, 24), 1),
  ongoing = runif(n) < 0.35
)

make_events <- function(patient, months, ongoing) {
  events <- data.frame(
    patient = character(),
    month = numeric(),
    event = character()
  )
  add <- function(events, month, event) {
    rbind(events, data.frame(patient = patient, month = month, event = event))
  }

  outcome <- sample(c("CR", "PR", "SD"), 1, prob = c(0.25, 0.45, 0.3))
  first_scan <- round(runif(1, 1.5, 2.5), 1)
  if (outcome == "PR" || outcome == "CR") {
    events <- add(events, min(first_scan, months), "PR")
  }
  if (outcome == "CR" && months > first_scan + 2) {
    cr_month <- round(runif(1, first_scan + 1, months - 0.5), 1)
    events <- add(events, cr_month, "CR")
  }
  if (!ongoing) {
    events <- add(events, months, "PD")
    if (runif(1) < 0.3) {
      events <- add(events, months, "Death")
    }
  }
  events
}

response_events <- do.call(rbind, Map(
  make_events,
  treatment_duration$patient,
  treatment_duration$months,
  treatment_duration$ongoing
))
response_events$event <- factor(
  response_events$event,
  levels = c("CR", "PR", "PD", "Death")
)
rownames(response_events) <- NULL

usethis::use_data(treatment_duration, response_events, overwrite = TRUE)
