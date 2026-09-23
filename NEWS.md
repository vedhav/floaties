# floaties 0.0.0.9000

* New `plot_waterfall()` draws waterfall plots with ggplot2, plotly, or
  echarts4r from any data frame with one row per subject. It supports fill
  colors, tooltips, sorting, and reference lines.
* New `plot_swimlane()` draws swimlane plots with ggplot2, plotly, or
  echarts4r from a subject-level data frame of lanes and an optional
  event-level data frame of markers. It supports start times, fill colors,
  ongoing markers, event shapes and colors, tooltips, sorting, and reference
  lines.
* New `prep_waterfall()` and `prep_swimlane()` return the prepared data behind
  each plot, for QC.
* New example datasets `tumor_change`, `treatment_duration`, and
  `response_events`.
