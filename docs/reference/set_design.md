# Set study design metadata

Records a study design on a data frame or a `simtab_spec`. The two forms
behave differently: on a `simtab_spec`, the design participates in
[`evaluate()`](https://MatheusTG-14.github.io/SimtablR/reference/evaluate.md)'s
effect-measure resolution, so a call such as
`set_design(spec, "cohort") |> evaluate()` can pick RR over an unset
measure. On a data frame, the design is recorded only as an attribute
for educator advice (methodological guidance rules can read it); it is
*not* consulted when resolving an effect measure, even for a
[`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md)/[`table1()`](https://MatheusTG-14.github.io/SimtablR/reference/table1.md)
call built directly from that data frame. To resolve a measure from
design, pass `design =` to the direct function or call `set_design()` on
a `simtab_spec` (see `vignette("study-design-effect-measures")`).

## Usage

``` r
set_design(data, design)
```

## Arguments

- data:

  A data.frame or `simtab_spec`.

- design:

  Study design, e.g. `"cross_sectional"`, `"cohort"`, or
  `"case_control"`.

## Value

`data` with a SimtablR design attribute, or a modified `simtab_spec`.

## Examples

``` r
set_design(simtab(epitabl), "cohort")
#> <simtab_spec>
#>   not yet computed
#>   data: 1500 rows x 22 columns; hash c0787c425db7
#>   describe: <unset>
#>   stratify/by: <unset>
#>   adjust: <unset>
#>   summary: auto
#>   measure: <unset> (ref: <unset>, conf.level: 0.95)
#>   test: <unset>
#>   design: cohort
#>   style: default
cohort_data <- set_design(epitabl, "cohort") # advisory only; see Details
```
