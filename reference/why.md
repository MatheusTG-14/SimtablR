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

A `simtab_explanation`: a character vector with one line per recorded
decision. It prints one line per row;
[`as.character()`](https://rdrr.io/r/base/character.html) or
`cat(x, sep = "\n")` give the plain lines.

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
