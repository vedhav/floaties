# floaties

R package for waterfall and swimlane plots for medical data review. Each plot
renders with ggplot2, plotly, or echarts4r (`engine` argument). Inputs are
domain-agnostic: the user passes a data frame and bare column names, and floaties
uses tidy eval for all plot-related data manipulation. floaties never derives
clinical quantities.

## Architecture

- `prep_<plot>()`: owns every argument, validates input, does all data
  manipulation, and returns a spec from `new_floaties_spec()` (fields are
  documented in `R/spec.R`).
- `plot_<plot>(data, ..., engine)`: passes `...` to `prep_<plot>()` and calls
  `render_spec()`. Don't add arguments here.
- `render_<plot>_<engine>()`: only reads the spec. No data manipulation.
  Register new renderers in `render_spec()`.
- Files are organised by plot: `R/<plot>.R` holds its prep and all three
  renderers. Shared code: `R/args.R` (argument and data checks),
  `R/scales.R` (colors, shapes), `R/spec.R` (spec, dispatch, tooltips).
- New markers go through events and `shape_map`, not engine-specific code.
- echarts renderers build raw options with `echarts4r::e_list()`.
- All three engines (ggplot2, plotly, echarts4r) are in Imports. Call them as
  `pkg::fun()`.

## Conventions

- Follow the tidyverse style guide. Run `styler::style_pkg()` and get a clean
  `lintr::lint_package()`.
- Column arguments use tidyselect via `resolve_column()` /
  `resolve_columns()`; `.data$` / `.env$` in package code.
- Read column labels with `column_labels()` before subsetting rows.
- Argument checks: checkmate `check_*()` wrapped in `assert_arg()`, which
  raises `floaties_error_argument`. Data problems use `cli::cli_abort()` with
  their own class. Use `rlang::arg_match()` for enumerated arguments.
- testthat 3e: vdiffr snapshots for ggplot2, widget-structure checks for plotly
  and echarts4r.

## Definition of done

After every completed feature, run in order:

```sh
Rscript -e "devtools::document()"
Rscript -e "styler::style_pkg()"
Rscript -e "devtools::install(upgrade = 'never')"  # else lintr flags internal functions
Rscript -e "lintr::lint_package()"
Rscript -e "devtools::test()"
Rscript -e "devtools::check()"
```

The feature is not done until `devtools::check()` reports **0 errors**. Never
skip tests or wrap examples in `\dontrun{}` to get a clean check.
