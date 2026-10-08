# Convert a SimtablR result or spec to a data frame

Convert a SimtablR result or spec to a data frame

## Usage

``` r
# S3 method for class 'simtab_result'
as.data.frame(x, row.names = NULL, optional = FALSE, tidy = FALSE, ...)

# S3 method for class 'simtab_spec'
as.data.frame(x, row.names = NULL, optional = FALSE, tidy = FALSE, ...)
```

## Arguments

- x:

  A `simtab_result` or `simtab_spec`.

- row.names:

  Unused.

- optional:

  Unused.

- tidy:

  Logical. `FALSE` returns the formatted display frame; `TRUE` returns
  the long numeric table.

- ...:

  Passed to subclass renderers. For `regtab`, `vif = TRUE` adds maximum
  VIF-equivalent columns by outcome.

## Value

A data.frame.

## Examples

``` r
res <- tb(epitabl, sex, diabetes)
as.data.frame(res)
#>   Sex recorded for clinical assessment   No Yes Total
#> 1                               Female  517 169   686
#> 2                                 Male  610 204   814
#> 3                                Total 1127 373  1500
as.data.frame(res, tidy = TRUE)
#>   variable  level estimate lower_ci upper_ci p_value outcome   n
#> 1      sex Female       NA       NA       NA      NA      No 517
#> 2      sex   Male       NA       NA       NA      NA      No 610
#> 3      sex Female       NA       NA       NA      NA     Yes 169
#> 4      sex   Male       NA       NA       NA      NA     Yes 204
```
