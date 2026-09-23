# floaties 0.0.0.9000

* First working version.
* `waterfall()` draws one bar per subject, sorted worst to best, with optional
  grouping colour and reference lines.
* `swimlane()` draws one lane per subject from a single long data frame, with
  intervals and point events on the same timeline.
* Both render with `eng_ggplot2()`, `eng_plotly()` or `eng_echarts4r()`. The
  engine is an S3 object dispatched on, so a new engine is new methods in a new
  file and no change to any existing plot function. `new_engine()` is exported
  for engines shipped from other packages.
* `prepare_waterfall()` and `prepare_swimlane()` are exported so the canonical
  data can be built, inspected and tested without a plotting library installed.
* Known engine difference: the echarts4r swimlane does not colour by event
  type. See `?render_swimlane`.
