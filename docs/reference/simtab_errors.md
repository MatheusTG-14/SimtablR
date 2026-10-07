# SimtablR condition classes

SimtablR errors inherit from `simtab_error`. More specific classes
identify the failing surface so callers can assert on behavior without
matching prose: `simtab_error_input` for invalid user-supplied arguments
at a public entry point, `simtab_error_binding` for variable binding and
role-selection failures, `simtab_error_spec` for invalid SimtablR
specifications, `simtab_error_engine` for engine validation or compute
failures, `simtab_error_flag` for invalid table flags,
`simtab_error_render` for missing renderer methods, and
`simtab_error_dependency` for a suggested package that is needed but not
installed. `simtab_error_export` identifies unsafe paths, destination
collisions, and failed file writes. `simtab_error_model` identifies
unsupported model access and invalid outcome selectors on retained model
evidence.

## Value

This documentation-only topic evaluates to `NULL`; condition helpers
signal classed errors rather than returning values.

## Details

Messages follow one house form: an `x` line stating the problem, an `i`
line giving the context that produced it, and a `v` line naming the next
action.

[`{}`](https://rdrr.io/r/base/Paren.html) expressions in the message are
evaluated in the calling function's environment, so a caller can
interpolate its own locals directly rather than pre-building the string
with [`sprintf()`](https://rdrr.io/r/base/sprintf.html).

## Examples

``` r
tryCatch(
  tb(epitabl, non_existent_var, diabetes),
  simtab_error = function(e) message("Caught simtab error")
)
#> Caught simtab error
```
