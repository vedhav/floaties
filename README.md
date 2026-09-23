# floaties

<!-- badges: start -->
<!-- badges: end -->

Swimlane and waterfall plots for clinical trial monitoring. The two charts a
medical monitor reaches for first while a trial is running.

Both plots render with **ggplot2**, **plotly** or **echarts4r** from the same
call, and take column roles as arguments, so it does not matter where the data
came from as long as it has the shape each plot documents.

The name is a joke with two halves. Both charts this package draws are named
after water, which nobody seems to find odd. And it is a teaching package, so
armbands are the correct equipment.

## Installation

``` r
# install.packages("pak")
pak::pak("vedhav/floaties")
```

`ggplot2` is a hard dependency because it is the default engine. `plotly` and
`echarts4r` are suggested: install them only if you want them.

## Waterfall

One bar per subject, sorted worst to best.

``` r
library(floaties)

subjects <- data.frame(
  id       = sprintf("01-%03d", 1:8),
  change   = c(-100, -62, -41, -30, -8, 12, 24, 57),
  response = c("CR", "PR", "PR", "PR", "SD", "SD", "PD", "PD")
)

waterfall(subjects, id, change, color_var = response, hlines = c(20, -30))
```

`hlines` is in the units of `value_var`. For RECIST against a percentage change
column that is `c(20, -30)`, not `c(0.2, -0.3)`.

## Swimlane

One lane per subject. One long data frame, one row per interval or event; a row
with no end is a point event.

``` r
events <- data.frame(
  subject = c("01-001", "01-001", "01-002", "01-002", "01-003"),
  day     = c(1, 15, 1, 22, 1),
  stop    = c(84, NA, 56, NA, 120),
  what    = c("dosing", "adverse event", "dosing", "adverse event", "dosing")
)

swimlane(events, subject, day, stop, what)
```

Bringing ADSL, ADAE and ADEX into that shape is your job. It is one `rbind()`
you understand, and it keeps this package out of the business of knowing which
datasets exist in your study.

## Switching engines

The same call, three ways:

``` r
waterfall(subjects, id, change, engine = eng_ggplot2())
waterfall(subjects, id, change, engine = eng_plotly())
waterfall(subjects, id, change, engine = eng_echarts4r())
```

Each returns that library's own object, so you keep customising with the verbs
you already know:

``` r
waterfall(subjects, id, change) + ggplot2::coord_flip()
```

## With CDISC data

Nothing in the package knows about CDISC. Derive the value you want to plot,
then pass the columns:

``` r
library(dplyr)

best <- pharmaverseadam::adtr_onco |>
  filter(PARAMCD == "SDIAM", !is.na(PCHG)) |>
  slice_min(PCHG, n = 1, by = USUBJID, with_ties = FALSE)

waterfall(best, USUBJID, PCHG, hlines = c(20, -30))
```

## Adding an engine

A plot function is a generic that dispatches on an engine object. Adding a
fourth engine means writing methods in a new file; no existing plot function
changes.

``` r
eng_text <- function(width = 44, ...) new_engine("text", width = width, ...)

render_waterfall.floaties_text <- function(engine, x, ...) {
  # `x` is canonical: `.id` is a factor in plot order, `.value` is numeric.
  # It never carries the caller's column names.
  structure(
    sprintf("%-12s %s %+.1f", x$.id, strrep("#", abs(round(x$.value / 5))), x$.value),
    class = "floaties_text_plot"
  )
}

print.floaties_text_plot <- function(x, ...) cat(unclass(x), sep = "\n")

waterfall(subjects, id, change, engine = eng_text())
```

## Design

Three stages, and only the last knows about a plotting library:

```
waterfall(data, ...) -> prepare_waterfall() -> floaties_data_waterfall -> render_waterfall(engine)
                         tidy eval, validation,     canonical: .id, .value,      the only
                         sorting, domain rules      .group                       engine-aware part
```

Renderers never see your column names, which is what keeps them small, keeps
tidy evaluation in one place, and stops a new engine reimplementing clinical
logic. The full reasoning is in `package-architecture.md` in the workshop repo.

## What the tests do not cover

No test can tell you a swimlane reads correctly. `testthat` checks that every
engine renders every fixture, including empty data, missing values, one row and
overlapping intervals, and that the canonical contract holds. Whether the
picture is right is still a human looking at it.
