# plot_waterfall() validates engine and passes arguments on

    Code
      plot_waterfall(d, id, change, engine = "base")
    Condition
      Error in `plot_waterfall()`:
      ! `engine` must be one of "ggplot2", "plotly", or "echarts4r", not "base".
    Code
      plot_waterfall(d, id, change, title = 1)
    Condition
      Error in `prep_waterfall()`:
      ! `title`: Must be of type 'string' (or 'NULL'), not 'double'

