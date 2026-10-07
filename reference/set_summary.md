# Set the default summary statistic

Set the default summary statistic

## Usage

``` r
set_summary(spec, stat = c("auto", "median", "mean"), .by_var = NULL)
```

## Arguments

- spec:

  A `simtab_spec`.

- stat:

  Summary statistic, one of `"auto"`, `"median"`, or `"mean"`.

- .by_var:

  Optional variable for a per-variable override.

## Value

A modified `simtab_spec`.

## Examples

``` r
set_summary(describe(simtab(epitabl), age), "median")
#> <simtab_spec>
#>   not yet computed
#>   data: 1500 rows x 22 columns; hash c0787c425db7
#>   describe: age
#>   stratify/by: <unset>
#>   adjust: <unset>
#>   summary: median
#>   measure: <unset> (ref: <unset>, conf.level: 0.95)
#>   test: <unset>
#>   design: <unset>
#>   style: default
```
