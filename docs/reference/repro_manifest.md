# Build a reproducibility manifest

Captures the runtime and recorded analysis decisions needed to reproduce
a SimtablR result. The data hash and ruleset version are read from the
result; the data are not re-hashed.

## Usage

``` r
repro_manifest(result)
```

## Arguments

- result:

  A `simtab_result` or `simtab_spec`.

## Value

A `simtab_manifest` list. `design` and `design_source` describe any
study design recorded on the specification or the source data frame,
whether or not it took part in resolving `measure`; `design_used`
distinguishes the two, and is `TRUE` only when `resolved_from` is
`"design"`. A design recorded only on the data frame
([`set_design()`](https://MatheusTG-14.github.io/SimtablR/reference/set_design.md)
on a `data.frame`) is advisory and never sets `design_used`; see
[`set_design()`](https://MatheusTG-14.github.io/SimtablR/reference/set_design.md).

## Examples

``` r
res <- tb(epitabl, sex, diabetes)
repro_manifest(res)
#> SimtablR reproducibility manifest
#>   R: R version 4.6.0 (2026-04-24 ucrt)
#>   data hash (xxhash64): c0787c425db7e792
#>   design: <unset>
#>   measure: <unset>
#>   ruleset: downscale-2026-09-23
```
