# Set or query SimtablR educator guidance

Guidance changes only which stored advice is displayed. `"default"`
shows severity 3-4 advice and counts hidden severity 1-2 notes;
`"important"` and its compatibility alias `"quiet"` show severity 3-4
advice only; `"teaching"` shows severity 1-4 advice with external
citations and recorded rationale; `"strict"` shows severity 1-4 advice
in compact prose; and `"off"` hides automatic advice. Severity 0 remains
audit-only. When a result has only one non-audit advice entry, that
entry is shown under every profile except `"off"` instead of being
reported as hidden.

## Usage

``` r
simtablr_guidance(level = NULL)
```

## Arguments

- level:

  One of `"default"`, `"important"`, `"quiet"`, `"teaching"`,
  `"strict"`, or `"off"`. If omitted, returns the current level.

## Value

The active guidance level, invisibly when setting.

## Details

Guidance is a session-level display setting read when a stored result is
printed. Call `simtablr_guidance()` separately; do not place it inside
the `...` of
[`table1()`](https://MatheusTG-14.github.io/SimtablR/reference/table1.md)
or [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md).
Changing guidance does not recompute results.

## See also

[simtablr_references](https://MatheusTG-14.github.io/SimtablR/reference/simtablr_references.md)
for the full references behind the citations shown in advice.

## Examples

``` r
previous <- simtablr_guidance()
result <- table1(epitabl, "sex")
simtablr_guidance("teaching")
print(result)
#> Characteristic                               Overall (N=1500) 
#> -------------------------------------------------------------
#> Sex recorded for clinical assessment, n (%)                   
#>   Female                                          686 (45.7%) 
#>   Male                                            814 (54.3%) 
simtablr_guidance(previous)
suppressMessages(print(result))
#> Characteristic                               Overall (N=1500) 
#> -------------------------------------------------------------
#> Sex recorded for clinical assessment, n (%)                   
#>   Female                                          686 (45.7%) 
#>   Male                                            814 (54.3%) 
```
