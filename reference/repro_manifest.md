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

A `simtab_manifest` list. `estimator`, `interval_method`, `tests`, and
the corrections in `engine_decisions` are the same recorded decisions
that
[`as_methods()`](https://MatheusTG-14.github.io/SimtablR/reference/as_methods.md)
writes as prose. `packages` lists SimtablR, its Imports, and the
suggested packages the result's engine used (for example pROC for
[`roc()`](https://MatheusTG-14.github.io/SimtablR/reference/roc.md)),
alongside the R version and platform in `r`. `design` and
`design_source` describe any study design recorded on the specification
or the source data frame, whether or not it took part in resolving
`measure`; `design_used` distinguishes the two, and is `TRUE` only when
`resolved_from` is `"design"`. A design recorded only on the data frame
([`set_design()`](https://MatheusTG-14.github.io/SimtablR/reference/set_design.md)
on a `data.frame`) is advisory and never sets `design_used`; see
[`set_design()`](https://MatheusTG-14.github.io/SimtablR/reference/set_design.md).

## Examples

``` r
res <- tb(epitabl, sex, diabetes)
repro_manifest(res)
#> SimtablR reproducibility manifest
#>   R: R version 4.6.2 (2026-10-29)
#>   data hash (xxhash64): c0787c425db7e792
#>   design: <unset>
#>   measure: <unset>
#>   ruleset: downscale-2026-09-23
```
