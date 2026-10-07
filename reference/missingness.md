# Set missing-data policy

Set missing-data policy

## Usage

``` r
missingness(spec, display = NULL, denominator = NULL, model = NULL)
```

## Arguments

- spec:

  A `simtab_spec`.

- display:

  Whether to display missingness rows.

- denominator:

  Missing-data denominator policy, `"available"` or `"complete"`.

- model:

  Missing-data model policy, `"drop"`, `"fail"`, or `"explicit"`.

## Value

A modified `simtab_spec`.

## Examples

``` r
missingness(simtab(epitabl), display = TRUE, denominator = "available")
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
