#' Waterfall plot
#'
#' @description
#' Draw a waterfall plot: one bar per subject, sorted by value. A typical use
#' is the best percentage change from baseline in tumor size, colored by best
#' overall response.
#'
#' `plot_waterfall()` works with any data standard. `data` must have **one row
#' per subject**. Filter or derive the value to plot (for example the best
#' percentage change) before calling it.
#'
#' `prep_waterfall()` runs the data preparation step on its own and returns the
#' exact data the plot is drawn from. This helps with QC.
#'
#' @param data A data frame with one row per subject.
#' @param ... Arguments passed on to `prep_waterfall()`.
#' @param engine Graphics engine: `"ggplot2"`, `"plotly"`, or `"echarts4r"`.
#' @param subject <[`tidy-select`][tidyselect::language]> Column that
#'   identifies each subject. Must not contain missing values.
#' @param value <[`tidy-select`][tidyselect::language]> Numeric column with the
#'   bar height. Rows where it is missing are dropped with a warning.
#' @param fill <[`tidy-select`][tidyselect::language]> Optional column used to
#'   color the bars. Factor level order is kept; missing values are shown as
#'   `"Missing"`.
#' @param tooltip <[`tidy-select`][tidyselect::language]> Optional extra columns
#'   shown in the tooltip, for example `c(age, arm)`. The subject, value and
#'   fill are always shown. Tooltips are only used by interactive engines.
#' @param sort How to order the bars: `"descending"` (largest value on the
#'   left, the default), `"ascending"`, or `"none"` (keep the row order of
#'   `data`).
#' @param ref_lines Numeric vector of y values for dashed reference lines, or
#'   `NULL` for none. Defaults to `c(20, -30)`, the RECIST 1.1 thresholds for
#'   progression and partial response.
#' @param colors Optional colors for the fill levels: either a named character
#'   vector (names are fill levels) or an unnamed one used in level order.
#' @param title Optional plot title.
#' @param x_label,y_label Optional axis titles. By default the `label`
#'   attribute of the `subject` and `value` columns is used, or the column
#'   name if there is none.
#'
#' @returns
#' * `plot_waterfall()`: the engine's own object, a `ggplot` for ggplot2 or an
#'   htmlwidget for plotly and echarts4r, which you can keep customizing.
#' * `prep_waterfall()`: a `floaties_waterfall_spec` object. Its `data`
#'   element has one row per subject in plot order, with columns `.subject`,
#'   `.value`, `.fill`, and `.tooltip`.
#'
#' @export
#' @examples
#' plot_waterfall(
#'   tumor_change,
#'   subject = patient,
#'   value = best_change,
#'   fill = response
#' )
#'
#' plot_waterfall(
#'   tumor_change,
#'   subject = patient,
#'   value = best_change,
#'   fill = response,
#'   tooltip = c(arm, age),
#'   engine = "plotly"
#' )
#'
#' plot_waterfall(
#'   tumor_change,
#'   subject = patient,
#'   value = best_change,
#'   fill = arm,
#'   colors = c("Drug A" = "#0072B2", "Drug B" = "#E69F00"),
#'   engine = "echarts4r"
#' )
#'
#' # Inspect the data behind the plot
#' prep_waterfall(tumor_change, subject = patient, value = best_change)
plot_waterfall <- function(data,
                           ...,
                           engine = c("ggplot2", "plotly", "echarts4r")) {
  engine <- rlang::arg_match(engine)
  render_spec(prep_waterfall(data, ...), engine)
}

#' @rdname plot_waterfall
#' @export
prep_waterfall <- function(data,
                           subject,
                           value,
                           fill = NULL,
                           tooltip = NULL,
                           sort = c("descending", "ascending", "none"),
                           ref_lines = c(20, -30),
                           colors = NULL,
                           title = NULL,
                           x_label = NULL,
                           y_label = NULL) {
  assert_arg(checkmate::check_data_frame(data), "data")
  sort <- rlang::arg_match(sort)
  assert_arg(
    checkmate::check_numeric(
      ref_lines,
      any.missing = FALSE, finite = TRUE, null.ok = TRUE
    ),
    "ref_lines"
  )
  check_labels(title, x_label, y_label)

  subject_col <- resolve_column(data, rlang::enquo(subject), "subject")
  value_col <- resolve_column(
    data, rlang::enquo(value), "value",
    check = checkmate::check_numeric
  )
  fill_col <- resolve_column(data, rlang::enquo(fill), "fill", required = FALSE)
  tooltip_cols <- resolve_columns(data, rlang::enquo(tooltip))
  col_labels <- column_labels(data)

  data <- drop_missing(data, value_col, subject_col)
  check_subjects(data, subject_col)
  data <- data[order_rows(data[[value_col]], sort), , drop = FALSE]

  subjects <- as.character(data[[subject_col]])
  fill_values <- if (is.null(fill_col)) {
    factor(rep("All subjects", nrow(data)))
  } else {
    as_level_factor(data[[fill_col]])
  }
  fill_levels <- levels(fill_values)

  new_floaties_spec(
    "waterfall",
    data = data.frame(
      .subject = factor(subjects, levels = subjects),
      .value = data[[value_col]],
      .fill = fill_values,
      .tooltip = build_tooltip(
        data,
        unique(c(subject_col, value_col, fill_col, tooltip_cols)),
        col_labels
      ),
      row.names = NULL
    ),
    scales = list(
      fill = resolve_scale(
        colors, fill_levels, default_colors(fill_levels), "color"
      )
    ),
    labels = list(
      title = title,
      x = x_label %||% col_labels[[subject_col]],
      y = y_label %||% col_labels[[value_col]],
      fill = if (!is.null(fill_col)) col_labels[[fill_col]]
    ),
    ref_lines = ref_lines
  )
}

render_waterfall_ggplot2 <- function(spec) {
  labels <- spec$labels

  p <- ggplot2::ggplot(
    spec$data,
    ggplot2::aes(x = .data$.subject, y = .data$.value, fill = .data$.fill)
  ) +
    ggplot2::geom_col(width = 0.8) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.3)

  if (length(spec$ref_lines) > 0) {
    p <- p + ggplot2::geom_hline(
      yintercept = spec$ref_lines,
      linetype = "dashed",
      colour = "grey45",
      linewidth = 0.4
    )
  }

  p +
    ggplot2::scale_fill_manual(values = spec$scales$fill, name = labels$fill) +
    ggplot2::labs(title = labels$title, x = labels$x, y = labels$y) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = if (is.null(labels$fill)) "none" else "bottom"
    )
}

render_waterfall_plotly <- function(spec) {
  d <- spec$data
  labels <- spec$labels

  p <- plotly::plot_ly()
  # One trace per fill level so each level gets its own legend entry.
  for (level in levels(d$.fill)) {
    level_data <- d[d$.fill == level, , drop = FALSE]
    p <- plotly::add_bars(
      p,
      x = as.character(level_data$.subject),
      y = level_data$.value,
      name = level,
      marker = list(color = spec$scales$fill[[level]]),
      text = level_data$.tooltip,
      textposition = "none",
      hovertemplate = "%{text}<extra></extra>"
    )
  }

  plotly::layout(
    p,
    title = list(text = labels$title, x = 0, xanchor = "left"),
    margin = list(t = if (is.null(labels$title)) 20 else 50),
    barmode = "relative",
    bargap = 0.2,
    showlegend = !is.null(labels$fill),
    legend = list(
      orientation = "h",
      x = 0,
      y = -0.1,
      title = list(text = labels$fill)
    ),
    xaxis = list(
      title = labels$x,
      type = "category",
      categoryorder = "array",
      categoryarray = levels(d$.subject),
      showticklabels = FALSE
    ),
    yaxis = list(title = labels$y, zeroline = TRUE),
    shapes = lapply(spec$ref_lines, function(y) {
      list(
        type = "line",
        xref = "paper",
        x0 = 0,
        x1 = 1,
        y0 = y,
        y1 = y,
        line = list(color = "grey", dash = "dash", width = 1)
      )
    })
  )
}

render_waterfall_echarts4r <- function(spec) {
  d <- spec$data
  labels <- spec$labels

  # One stacked bar series per fill level. Each subject has a value in exactly
  # one series (NA, sent as null, elsewhere), which keeps the bar order.
  series <- lapply(levels(d$.fill), function(level) {
    list(
      type = "bar",
      name = level,
      stack = "waterfall",
      barCategoryGap = "20%",
      itemStyle = list(color = spec$scales$fill[[level]]),
      data = lapply(seq_len(nrow(d)), function(i) {
        if (d$.fill[[i]] != level) {
          return(NA)
        }
        list(value = d$.value[[i]], name = d$.tooltip[[i]])
      })
    )
  })
  if (length(spec$ref_lines) > 0) {
    series[[1]]$markLine <- echarts_ref_lines(spec$ref_lines, "yAxis")
  }

  echarts4r::e_charts() |>
    echarts4r::e_list(list(
      title = list(text = labels$title %||% ""),
      legend = list(show = !is.null(labels$fill), bottom = 0),
      tooltip = echarts_tooltip,
      xAxis = list(
        type = "category",
        data = levels(d$.subject),
        name = labels$x,
        nameLocation = "middle",
        nameGap = 10,
        axisLabel = list(show = FALSE),
        axisTick = list(show = FALSE)
      ),
      yAxis = list(
        type = "value",
        name = labels$y,
        nameLocation = "middle",
        nameGap = 40
      ),
      series = series
    ))
}
