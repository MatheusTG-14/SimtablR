# Set an effect measure

Set an effect measure

## Usage

``` r
measure(spec, m, ref = NULL, conf.level = 0.95, adjust = NULL)
```

## Arguments

- spec:

  A `simtab_spec`.

- m:

  A single effect-measure name.

- ref:

  Optional reference level: one level shared by every variable, or a
  named list with one level per variable, e.g.
  `list(sex = "Female", smoking = "Never")`.

- conf.level:

  Confidence level between 0 and 1.

- adjust:

  Optional tidyselect adjustment covariates, captured as a convenience
  for the builder register.

## Value

A modified `simtab_spec`.

## Examples

``` r
measure(simtab(epitabl), "OR", conf.level = 0.90)
#> <simtab_spec>
#>   not yet computed
#>   data: 1500 rows x 22 columns; hash c0787c425db7
#>   describe: <unset>
#>   stratify/by: <unset>
#>   adjust: <unset>
#>   summary: auto
#>   measure: OR (ref: <unset>, conf.level: 0.9)
#>   test: <unset>
#>   design: <unset>
#>   style: default
```
