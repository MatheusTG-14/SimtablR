# Capture a stratifying variable

Capture a stratifying variable

## Usage

``` r
stratify(spec, by)
```

## Arguments

- spec:

  A `simtab_spec`.

- by:

  A single data-masked variable.

## Value

A modified `simtab_spec`.

## Examples

``` r
stratify(describe(simtab(epitabl), age, sex), adjudicated_acs)
#> <simtab_spec>
#>   not yet computed
#>   data: 1500 rows x 22 columns; hash c0787c425db7
#>   describe: age, sex
#>   stratify/by: adjudicated_acs
#>   adjust: <unset>
#>   summary: auto
#>   measure: <unset> (ref: <unset>, conf.level: 0.95)
#>   test: <unset>
#>   design: <unset>
#>   style: default
```
