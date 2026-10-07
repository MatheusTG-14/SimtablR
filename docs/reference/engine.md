# Select a registered computation engine

Selects the registered engine used when a specification is computed.
Because the engine determines the numerical evidence, applying
`engine()` to a computed result updates its stored specification and
recomputes through the same path as other evidence-changing verbs.

## Usage

``` r
engine(spec, name)
```

## Arguments

- spec:

  A `simtab_spec` or `simtab_result`.

- name:

  A single registered engine name. See
  [`list_engines()`](https://MatheusTG-14.github.io/SimtablR/reference/list_engines.md).

## Value

A modified `simtab_spec`, or a recomputed `simtab_result`.

## See also

[`register_engine()`](https://MatheusTG-14.github.io/SimtablR/reference/register_engine.md),
[`evaluate()`](https://MatheusTG-14.github.io/SimtablR/reference/evaluate.md)

## Examples

``` r
engine(simtab(epitabl), "descriptive")
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
