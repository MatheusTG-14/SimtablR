# Tidy a SimtablR result

Tidy a SimtablR result

## Usage

``` r
# S3 method for class 'simtab_result'
tidy(x, ...)

# S3 method for class 'simtab_spec'
tidy(x, ...)
```

## Arguments

- x:

  A `simtab_result` or `simtab_spec`.

- ...:

  Ignored.

## Value

A long data.frame. For `table1`, this matches
`as.data.frame(x, tidy = TRUE)`.

## Examples

``` r
res <- tb(epitabl, sex, diabetes)
generics::tidy(res)
#>   variable  level estimate lower_ci upper_ci p_value outcome   n
#> 1      sex Female       NA       NA       NA      NA      No 517
#> 2      sex   Male       NA       NA       NA      NA      No 610
#> 3      sex Female       NA       NA       NA      NA     Yes 169
#> 4      sex   Male       NA       NA       NA      NA     Yes 204
```
