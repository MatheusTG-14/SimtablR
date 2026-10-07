# Validate a SimtablR specification before compute

Keeps only engine-agnostic checks: spec shape and effect-measure
existence. Per-engine hard errors live in that engine's own `validate`
hook, registered via
[`register_engine()`](https://MatheusTG-14.github.io/SimtablR/reference/register_engine.md),
and run by
[`evaluate()`](https://MatheusTG-14.github.io/SimtablR/reference/evaluate.md)
after this shared check.

## Usage

``` r
validate(spec)
```

## Arguments

- spec:

  A `simtab_spec`.

## Value

Invisibly, `spec`.

## Examples

``` r
sp <- simtab(epitabl) |> describe(hypertension)
validate(sp)
```
