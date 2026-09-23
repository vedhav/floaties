# floaties

Swimlane and waterfall plots for clinical trial monitoring.

## Architecture

Every plot is available in three engines: ggplot2, plotly, echarts4r. A plot
function is a generic that dispatches on an engine object. It is never a single
function with an `engine = "ggplot2"` argument.

Adding a fourth engine must not require touching any existing plot function.

Three stages: `prepare_*()` does tidy evaluation, validation, sorting and all
domain logic; it returns canonical data with fixed column names; `render_*()`
draws it. A renderer never sees a caller's column names.

One file per (plot, engine) pair, so both new plots and new engines are purely
additive.

## Contracts

An export is a promise. No export lands without roxygen, `@examples`, a test and
a NEWS.md entry.

Every plot function returns the plotting library's native object, so users keep
customising with that library's own verbs. We do not wrap it.

`validate_*_data()` defines what a renderer may rely on. Every `prepare_*()`
ends by calling it.

## Clinical domain

Data arrives as whatever the caller has. Column roles are arguments. We are
agnostic about where the data came from, opinionated about what each chart
means: a waterfall is sorted by value, reference lines are in the units of the
value column, there is no Day 0.

Missing values are normal, not errors. Dropping a row is a warning, never
silent.

## Working agreement

Run `devtools::check()` after every meaningful change, not after every edit.
Write the test before the implementation. I review the test; you write the code.

Test `prepare_*()` heavily with no engine loaded. Cover engines with the
conformance loop in `tests/testthat/test-conformance.R`, which runs every
fixture against every engine; add fixtures there rather than one-off tests.
