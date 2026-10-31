# Generate a manuscript methods sentence

Builds concise manuscript prose from the decisions recorded in a
computed SimtablR result. Passing a bare `simtab_spec` computes it
first, matching the other render/extract verbs.

## Usage

``` r
as_methods(x, ...)

# S3 method for class 'simtab_result'
as_methods(x, software = FALSE, ...)

# S3 method for class 'simtab_report'
as_methods(x, software = FALSE, ...)
```

## Arguments

- x:

  A `simtab_result`, `simtab_spec`, or `simtab_report`.

- ...:

  Passed to methods; otherwise ignored.

- software:

  Logical. If `TRUE`, append a sentence naming the R and SimtablR
  versions and the suggested packages the analysis used.

## Value

A character string of length one.

## Examples

``` r
res <- tb(epitabl, sex, diabetes)
as_methods(res)
#> [1] "Sex recorded for clinical assessment was cross-tabulated with History of diabetes as counts. The analysis included the 1500 participants with complete data on the tabulated variables."
as_methods(res, software = TRUE)
#> [1] "Sex recorded for clinical assessment was cross-tabulated with History of diabetes as counts. The analysis included the 1500 participants with complete data on the tabulated variables. Analyses used R 4.6.2 and SimtablR 3.1.2."
```
