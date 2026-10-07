# Run educator advice rules

Evaluates all applicable rules against a computed result. Rule failures
are caught and skipped so advice can never break analysis.

## Usage

``` r
advise(result, audit = FALSE)
```

## Arguments

- result:

  A computed `simtab_result` or `simtab_report`.

- audit:

  Logical. `FALSE` (default) returns the fired advice entries. `TRUE`
  runs every applicable rule regardless of the guidance dial and returns
  a `simtab_audit` report of checked, fired, and silent rules (single
  results only).

## Value

A list of advice entries deduplicated by rule id, or a `simtab_audit`
object when `audit = TRUE`.

## See also

[simtablr_references](https://MatheusTG-14.github.io/SimtablR/reference/simtablr_references.md)
for the full references behind the citations shown in advice, and
[`simtablr_guidance()`](https://MatheusTG-14.github.io/SimtablR/reference/simtablr_guidance.md)
to control display.

## Examples

``` r
res <- tb(epitabl, sex, diabetes)
advise(res)
#> list()
advise(res, audit = TRUE)
#> SimtablR audit
#> Ruleset version: downscale-2026-09-23
#> Checked rules: 2
#> Fired: 0 | Silent: 2
#> 
#> Silent rules:
#>   complete_case_unreported_missingness, multiplicity_unadjusted
```
