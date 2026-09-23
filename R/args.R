# Argument and data checks shared by every `prep_*()` function.
#
# Argument checks use checkmate; `assert_arg()` turns a failed check into a
# cli error with the class `floaties_error_argument`. Problems with the data
# itself (missing or duplicate subjects, no rows left) get their own classes.

assert_arg <- function(check, arg, call = rlang::caller_env()) {
  if (!isTRUE(check)) {
    cli::cli_abort(
      "{.arg {arg}}: {check}",
      class = "floaties_error_argument",
      call = call
    )
  }
  invisible()
}

check_labels <- function(title, x_label, y_label, call = rlang::caller_env()) {
  assert_arg(checkmate::check_string(title, null.ok = TRUE), "title", call)
  assert_arg(checkmate::check_string(x_label, null.ok = TRUE), "x_label", call)
  assert_arg(checkmate::check_string(y_label, null.ok = TRUE), "y_label", call)
}

# Resolve a single-column tidyselect argument to a column name, or `NULL` for
# an optional argument that was not supplied. `check` is an optional checkmate
# check function applied to the column.
resolve_column <- function(data,
                           col,
                           arg,
                           required = TRUE,
                           check = NULL,
                           call = rlang::caller_env()) {
  if (rlang::quo_is_missing(col) || rlang::quo_is_null(col)) {
    if (required) {
      cli::cli_abort(
        "{.arg {arg}} is required.",
        class = "floaties_error_argument",
        call = call
      )
    }
    return(NULL)
  }

  name <- names(tidyselect::eval_select(
    col,
    data,
    allow_rename = FALSE,
    error_call = call
  ))
  if (length(name) != 1) {
    cli::cli_abort(
      "{.arg {arg}} must select exactly one column, not {length(name)}.",
      class = "floaties_error_argument",
      call = call
    )
  }
  if (!is.null(check)) {
    assert_arg(check(data[[name]]), arg, call)
  }
  name
}

# Resolve a multi-column tidyselect argument to column names.
resolve_columns <- function(data, cols, call = rlang::caller_env()) {
  if (rlang::quo_is_missing(cols) || rlang::quo_is_null(cols)) {
    return(character())
  }
  names(tidyselect::eval_select(
    cols,
    data,
    allow_rename = FALSE,
    error_call = call
  ))
}

# Each column's `label` attribute (common in SDTM/ADaM), or its name, named
# by column. Call it before subsetting rows, which drops the attributes.
column_labels <- function(data) {
  vapply(names(data), function(col) {
    label <- attr(data[[col]], "label", exact = TRUE)
    if (is.character(label) && length(label) == 1 && nzchar(label)) {
      label
    } else {
      col
    }
  }, character(1))
}

# Drop rows with a missing value in any of `cols`, with a warning.
drop_missing <- function(data,
                         cols,
                         subject_col,
                         what = "subject",
                         allow_empty = FALSE,
                         call = rlang::caller_env()) {
  missing <- Reduce(`|`, lapply(cols, function(col) is.na(data[[col]])))
  if (any(missing)) {
    cli::cli_warn(
      c(
        "Dropped {sum(missing)} {what}{?s} with a missing {.field {cols}}.",
        "i" = "Affected subjects:
               {.val {unique(data[[subject_col]][missing])}}."
      ),
      class = "floaties_warning_missing_values"
    )
    data <- data[!missing, , drop = FALSE]
  }
  if (!allow_empty && nrow(data) == 0) {
    cli::cli_abort(
      "{.arg data} has no rows to plot.",
      class = "floaties_error_empty_data",
      call = call
    )
  }
  data
}

# Subjects must be present and unique: one row per subject.
check_subjects <- function(data, col, call = rlang::caller_env()) {
  ids <- data[[col]]
  if (anyNA(ids)) {
    cli::cli_abort(
      c(
        "{.arg subject} must not contain missing values.",
        "x" = "Column {.field {col}} has {sum(is.na(ids))} missing value{?s}."
      ),
      class = "floaties_error_missing_subject",
      call = call
    )
  }
  dups <- unique(ids[duplicated(ids)])
  if (length(dups) > 0) {
    cli::cli_abort(
      c(
        "{.arg data} must have one row per subject.",
        "x" = "{length(dups)} subject{?s} in {.field {col}} appear{?s/} more
               than once: {.val {dups}}.",
        "i" = "Filter {.arg data} to one record per subject before plotting."
      ),
      class = "floaties_error_duplicate_subject",
      call = call
    )
  }
}

# Row order for `sort`. Ties keep their original order.
order_rows <- function(x, sort) {
  switch(sort,
    descending = order(x, decreasing = TRUE, method = "radix"),
    ascending = order(x, method = "radix"),
    none = seq_along(x)
  )
}
