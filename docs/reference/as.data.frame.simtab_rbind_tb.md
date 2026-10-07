# Convert rbind_tb to Data Frame

Convert rbind_tb to Data Frame

## Usage

``` r
# S3 method for class 'simtab_rbind_tb'
as.data.frame(x, row.names = NULL, optional = FALSE, tidy = FALSE, ...)
```

## Arguments

- x:

  An `rbind_tb` object.

- row.names:

  NULL or a character vector giving the row names for the data frame.

- optional:

  Logical. If TRUE, setting row names and converting column names is
  optional.

- tidy:

  Logical. If `TRUE`, returns a long-format tidy data frame.

- ...:

  Additional arguments.

## Value

A data.frame.

## Examples

``` r
t1 <- tb(epitabl, sex, diabetes)
t2 <- tb(epitabl, hypertension, diabetes)
as.data.frame(rbind(t1, t2))
#>                               Variable   No Yes Total
#> 1 Sex recorded for clinical assessment               
#> 2                               Female  517 169   686
#> 3                                 Male  610 204   814
#> 4              History of hypertension               
#> 5                                   No  566 166   732
#> 6                                  Yes  561 207   768
#> 7                                Total 1127 373  1500
```
