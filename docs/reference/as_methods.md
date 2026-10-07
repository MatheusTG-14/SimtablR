# Generate a manuscript methods sentence

Builds concise manuscript prose from the decisions recorded in a
computed SimtablR result. Passing a bare `simtab_spec` computes it
first, matching the other render/extract verbs.

## Usage

``` r
as_methods(x, ...)
```

## Arguments

- x:

  A `simtab_result` or `simtab_spec`.

- ...:

  Ignored.

## Value

A character string of length one.

## Examples

``` r
res <- tb(epitabl, sex, diabetes)
as_methods(res)
#> [1] "Categorical variables were summarised with counts and percentages."
```
