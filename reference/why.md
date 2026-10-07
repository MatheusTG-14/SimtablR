# Report why SimtablR made each recorded decision

`why()` prints the decisions already recorded on a spec, result, or
report. Passing a `simtab_spec` is inert: it describes the planned
analysis without computing it.

## Usage

``` r
why(x, ...)
```

## Arguments

- x:

  A `simtab_spec`, `simtab_result`, or `simtab_report`.

- ...:

  Ignored.

## Value

A `simtab_explanation` object, invisibly when printed.

## Examples

``` r
res <- tb(epitabl, sex, diabetes)
why(res)
#> SimtablR explanation
#> Status: computed result.
#> Engine: bivariate
#> Design: not recorded
#> Measure: not selected
#> Measure source: not recorded
#> Ruleset version: downscale-2026-09-23
```
