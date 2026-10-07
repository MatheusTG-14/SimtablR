# Read a recorded study design

Returns the study design recorded by
[`set_design()`](https://MatheusTG-14.github.io/SimtablR/reference/set_design.md),
without requiring callers to read the internal `"simtablr.design"`
attribute directly. On a data frame this reads the advisory attribute;
on a `simtab_spec` or `simtab_result` it reads the specification's
`design` field, falling back to the source data frame's attribute for a
`simtab_result` (matching what
[`repro_manifest()`](https://MatheusTG-14.github.io/SimtablR/reference/repro_manifest.md)
reports as `design`/`design_source`). It does not indicate whether the
design took part in effect-measure resolution; see `resolved_from` on
`spec$effect` or `design_used` in
[`repro_manifest()`](https://MatheusTG-14.github.io/SimtablR/reference/repro_manifest.md)
for that.

## Usage

``` r
design(x)
```

## Arguments

- x:

  A data.frame, `simtab_spec`, or `simtab_result`.

## Value

A single design string, or `NULL` if none is recorded.

## Examples

``` r
cohort <- set_design(epitabl, "cohort")
design(cohort)
#> [1] "cohort"
```
