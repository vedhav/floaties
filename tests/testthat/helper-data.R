lanes_df <- data.frame(
  subject = c("S01", "S02", "S03", "S04"),
  start = 0,
  end = c(120, 85, 200, 45),
  arm = c("A", "B", "A", "B"),
  on_study = c(TRUE, FALSE, TRUE, FALSE)
)

events_df <- data.frame(
  subject = c("S01", "S01", "S02", "S03", "S04"),
  day = c(30, 90, 60, 150, 40),
  response = c("PR", "CR", "PD", "PR", "PD")
)

tumour_df <- data.frame(
  subject = sprintf("S%02d", 1:6),
  best_change = c(-45, 35, -100, 12, -31, -5),
  response = c("PR", "PD", "CR", "SD", "PR", "SD")
)

swimlane_args <- list(
  data = lanes_df, id = "subject", start = "start", end = "end",
  color = "arm", ongoing = "on_study",
  events = events_df, event_time = "day", event_type = "response"
)

waterfall_args <- list(
  data = tumour_df, id = "subject", value = "best_change", fill = "response"
)
