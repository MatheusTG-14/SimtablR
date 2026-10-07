# Start a SimtablR analysis specification

Captures `data` once into a shared data reference and returns an inert
`simtab_spec`. Printing the returned object shows the analysis plan; it
never performs estimation.

## Usage

``` r
simtab(data)
```

## Arguments

- data:

  A data frame.

## Value

A `simtab_spec` object.

## Examples

``` r
spec <- simtab(epitabl)
spec
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
