# Convert a SimtablR object to a gt table

`as_gt()` is a soft-dependency renderer for HTML and Quarto workflows.
It auto-computes a `simtab_spec`, then dispatches by result subclass.
Built-in results with specialised renderers retain their display
features; every other result with an `as.data.frame` renderer is
converted from its display data frame. Extension engines can register a
custom `as_gt` renderer.

## Usage

``` r
as_gt(x, ...)

# S3 method for class 'simtab_spec'
as_gt(x, ...)

# S3 method for class 'simtab_result'
as_gt(x, ...)

# S3 method for class 'simtab_rbind_tb'
as_gt(x, ...)

# S3 method for class 'simtab_report'
as_gt(x, ...)
```

## Arguments

- x:

  A SimtablR result or spec.

- ...:

  Passed to [`gt::gt()`](https://gt.rstudio.com/reference/gt.html).

## Value

A `gt_tbl` object, or a named list of `gt_tbl` objects for a
`simtab_report`.

## Examples

``` r
if (requireNamespace("gt", quietly = TRUE)) {
  res <- tb(epitabl, sex, diabetes)
  as_gt(res)
}


  

Sex recorded for clinical assessment
```
