# plot_swimlane() validates engine and passes arguments on

    Code
      plot_swimlane(sw_lanes(), pid, stop, engine = "base")
    Condition
      Error in `plot_swimlane()`:
      ! `engine` must be one of "ggplot2", "plotly", or "echarts4r", not "base".
    Code
      plot_swimlane(sw_lanes(), pid, stop, x_label = c("a", "b"))
    Condition
      Error in `prep_swimlane()`:
      ! `x_label`: Must have length 1

