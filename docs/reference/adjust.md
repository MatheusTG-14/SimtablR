# Capture adjustment covariates

Capture adjustment covariates

## Usage

``` r
adjust(spec, ...)
```

## Arguments

- spec:

  A `simtab_spec`.

- ...:

  Tidyselect expressions for adjustment covariates.

## Value

A modified `simtab_spec`.

## Examples

``` r
adjust(simtab(epitabl), age, sex)
#> <simtab_spec>
#>   not yet computed
#>   data: 1500 rows x 22 columns; hash c0787c425db7
#>   describe: <unset>
#>   stratify/by: <unset>
#>   adjust: age, sex
#>   summary: auto
#>   measure: <unset> (ref: <unset>, conf.level: 0.95)
#>   test: <unset>
#>   design: <unset>
#>   style: default
```
