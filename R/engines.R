# Helpers shared by the ggplot2, plotly, and echarts4r renderers.

check_engine <- function(engine) {
  if (engine != "ggplot2") {
    rlang::check_installed(
      engine,
      reason = sprintf("to use `engine = \"%s\"`.", engine)
    )
  }
}

# ggplot2's default discrete palette, so every engine uses the same colours.
hue_colours <- function(n) {
  if (n == 0) {
    return(character())
  }
  grDevices::hcl(h = seq(15, 375 - 360 / n, length.out = n), c = 100, l = 65)
}

# ColorBrewer "Pastel1", as used by ggplot2::scale_fill_brewer().
pastel_colours <- function(n) {
  pastel1 <- c(
    "#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", "#FED9A6",
    "#FFFFCC", "#E5D8BD", "#FDDAEC", "#F2F2F2"
  )
  rep_len(pastel1, n)
}

# Distinct values of a discrete variable in legend order, with NA last as "NA".
group_levels <- function(x) {
  levels <- levels(droplevels(as.factor(x)))
  if (anyNA(x)) c(levels, "NA") else levels
}

group_values <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- "NA"
  x
}

# Named colours for `levels`, drawing missing values in ggplot2's NA grey.
group_colours <- function(levels, palette) {
  present <- setdiff(levels, "NA")
  colours <- stats::setNames(palette(length(present)), present)
  if ("NA" %in% levels) {
    colours["NA"] <- "#7F7F7F"
  }
  colours
}

# Colours and symbols for swimlane events. RECIST responses and death get
# fixed styles; anything else gets the default palette and circles.
event_styles <- function(events) {
  present <- unique(as.character(events))
  if (all(present %in% names(event_colours))) {
    levels <- intersect(names(event_colours), present)
    return(list(
      levels = levels,
      known = TRUE,
      colour = event_colours[levels],
      plotly = plotly_symbols[levels],
      echarts = echarts_symbols[levels]
    ))
  }
  levels <- group_levels(events)
  list(
    levels = levels,
    known = FALSE,
    colour = stats::setNames(hue_colours(length(levels)), levels),
    plotly = stats::setNames(rep("circle", length(levels)), levels),
    echarts = stats::setNames(rep("circle", length(levels)), levels)
  )
}

event_colours <- c(
  "CR" = "#1B7837",
  "PR" = "#5AAE61",
  "SD" = "#F1A340",
  "NON-CR/NON-PD" = "#998EC3",
  "PD" = "#D73027",
  "NE" = "#999999",
  "Death" = "black"
)

event_shapes <- c(
  "CR" = 16,
  "PR" = 16,
  "SD" = 16,
  "NON-CR/NON-PD" = 16,
  "PD" = 17,
  "NE" = 1,
  "Death" = 4
)

plotly_symbols <- c(
  "CR" = "circle",
  "PR" = "circle",
  "SD" = "circle",
  "NON-CR/NON-PD" = "circle",
  "PD" = "triangle-up",
  "NE" = "circle-open",
  "Death" = "x"
)

echarts_symbols <- c(
  "CR" = "circle",
  "PR" = "circle",
  "SD" = "circle",
  "NON-CR/NON-PD" = "circle",
  "PD" = "triangle",
  "NE" = "emptyCircle",
  "Death" = "path://M2,0L5,3L8,0L10,2L7,5L10,8L8,10L5,7L2,10L0,8L3,5L0,2Z"
)

format_number <- function(x) {
  format(round(x, 1), trim = TRUE)
}

# Quote a string for embedding in JavaScript source.
js_string <- function(x) {
  encodeString(x, quote = '"')
}

# Vertical legend on the right, as in ggplot2 and plotly, listing `entries`.
# The grid is narrowed so the legend never overlaps the plot or title.
echarts_legend <- function(p, entries) {
  if (length(entries) == 0) {
    p$x$opts$legend <- list(show = FALSE)
    return(p)
  }
  width <- 40 + 7 * max(nchar(entries))
  p$x$opts$legend <- list(
    show = TRUE,
    type = "scroll",
    orient = "vertical",
    right = 10,
    top = "middle",
    data = as.list(entries)
  )
  grid <- if (length(p$x$opts$grid)) p$x$opts$grid[[1]] else list()
  grid$right <- width + 40
  p$x$opts$grid <- list(grid)
  p
}

plotly_title <- function(title) {
  if (is.null(title)) NULL else list(text = title)
}
