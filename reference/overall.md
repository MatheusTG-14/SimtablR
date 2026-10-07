# Toggle the overall column

Toggle the overall column

## Usage

``` r
overall(spec, value = TRUE)
```

## Arguments

- spec:

  A `simtab_spec`.

- value:

  `TRUE` or `FALSE`.

## Value

A modified `simtab_spec`.

## Examples

``` r
overall(simtab(epitabl), FALSE)
#> <simtab_spec>
#>   not yet computed
#>   data: 1500 rows x 22 columns; hash c0787c425db7
#>   describe: <unset>
#>   stratify/by: <unset>
#>   adjust: <unset>
#>   summary: auto
#>   measure: <unset> (ref: <unset>, conf.level: 0.95)
#>   test: <unset>
#>   design: <unset>
#>   style: default
```
