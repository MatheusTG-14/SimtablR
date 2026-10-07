# Capture variables to describe

Capture variables to describe

## Usage

``` r
describe(spec, ...)
```

## Arguments

- spec:

  A `simtab_spec`.

- ...:

  Tidyselect expressions for variables to describe.

## Value

A modified `simtab_spec`.

## Examples

``` r
describe(simtab(epitabl), age, sex)
#> <simtab_spec>
#>   not yet computed
#>   data: 1500 rows x 22 columns; hash c0787c425db7
#>   describe: age, sex
#>   stratify/by: <unset>
#>   adjust: <unset>
#>   summary: auto
#>   measure: <unset> (ref: <unset>, conf.level: 0.95)
#>   test: <unset>
#>   design: <unset>
#>   style: default
```
