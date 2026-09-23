# prep_waterfall() validates its input

    Code
      prep_waterfall(list(), id, change)
    Condition
      Error in `prep_waterfall()`:
      ! `data`: Must be of type 'data.frame', not 'list'
    Code
      prep_waterfall(d, value = change)
    Condition
      Error in `prep_waterfall()`:
      ! `subject` is required.
    Code
      prep_waterfall(d, id, missing_col)
    Condition
      Error in `prep_waterfall()`:
      ! Can't select columns that don't exist.
      x Column `missing_col` doesn't exist.
    Code
      prep_waterfall(d, id, c(change, id))
    Condition
      Error in `prep_waterfall()`:
      ! `value` must select exactly one column, not 2.
    Code
      prep_waterfall(d, id, note)
    Condition
      Error in `prep_waterfall()`:
      ! `value`: Must be of type 'numeric', not 'character'
    Code
      prep_waterfall(d, id, change, sort = "up")
    Condition
      Error in `prep_waterfall()`:
      ! `sort` must be one of "descending", "ascending", or "none", not "up".
    Code
      prep_waterfall(d, id, change, ref_lines = "20")
    Condition
      Error in `prep_waterfall()`:
      ! `ref_lines`: Must be of type 'numeric' (or 'NULL'), not 'character'
    Code
      prep_waterfall(d, id, change, title = 1)
    Condition
      Error in `prep_waterfall()`:
      ! `title`: Must be of type 'string' (or 'NULL'), not 'double'
    Code
      prep_waterfall(d, id, change, fill = group, colors = c(x = "red"))
    Condition
      Error in `prep_waterfall()`:
      ! `colors` must have a color for every level.
      x No color for "y" and "Missing".
    Code
      prep_waterfall(d, id, change, fill = group, colors = "red")
    Condition
      Error in `prep_waterfall()`:
      ! `colors` must have at least 3 colors.
      x 1 color supplied for 3 levels.

# subjects must be unique and not missing

    Code
      prep_waterfall(dup, id, change)
    Condition
      Error in `prep_waterfall()`:
      ! `data` must have one row per subject.
      x 1 subject in id appears more than once: "a".
      i Filter `data` to one record per subject before plotting.

