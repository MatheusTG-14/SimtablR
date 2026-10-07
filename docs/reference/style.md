# Set or update a SimtablR style

Set or update a SimtablR style

## Usage

``` r
style(x, journal)
```

## Arguments

- x:

  A SimtablR object.

- journal:

  A single style or journal preset name.

## Value

A modified SimtablR object.

## Examples

``` r
style(simtab(epitabl), "default")
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
