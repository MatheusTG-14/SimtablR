# Coerce a SimtablR report to a data.frame

A `simtab_report` bundles items that can have different shapes (for
example
[`simtablr()`](https://MatheusTG-14.github.io/SimtablR/reference/simtablr.md)'s
descriptive Table 1 and association Table 2), so there is no single
natural rectangular table for a report in general. This default method
instead returns a named list of
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) results,
one per item, mirroring
[`as_gt.simtab_report()`](https://MatheusTG-14.github.io/SimtablR/reference/as_gt.md)
and
[`as_flextable.simtab_report()`](https://MatheusTG-14.github.io/SimtablR/reference/as_flextable.simtab_result.md).
Subclasses whose items always share one shape, such as
[`sensitivity()`](https://MatheusTG-14.github.io/SimtablR/reference/sensitivity.md),
provide their own rectangular
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) method
instead of using this default.

## Usage

``` r
# S3 method for class 'simtab_report'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
```

## Arguments

- x:

  A `simtab_report`.

- row.names, optional, ...:

  Passed to each item's
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

## Value

A named list of data.frames, one per report item.

## Examples

``` r
rep <- simtablr(epitabl, outcome = mace_event, exposure = renal_impairment,
                vars = c(age, sex), design = "cohort")
as.data.frame(rep)
#> $table1
#>                                  Characteristic Overall (N=1500) No (N=1184)
#> 1 Age at index presentation (years) [Mean (SD)]      62.0 (13.2) 59.3 (12.5)
#> 2   Sex recorded for clinical assessment, n (%)                             
#> 3                                        Female      686 (45.7%) 550 (46.5%)
#> 4                                          Male      814 (54.3%) 634 (53.5%)
#>   Yes (N=316) P-value
#> 1 71.9 (10.5)  <0.001
#> 2               0.279
#> 3 136 (43.0%)        
#> 4 180 (57.0%)        
#> 
#> $table2
#>                                           Characteristic Overall (N=1500)
#> 1 Renal impairment (eGFR below 60 mL/min/1.73 m2), n (%)                 
#> 2                                                     No     1184 (78.9%)
#> 3                                                    Yes      316 (21.1%)
#>    No (N=1280) Yes (N=220) P-value        RR (95% CI)
#> 1                           <0.001                   
#> 2 1039 (81.2%) 145 (65.9%)                 1.00 (Ref)
#> 3  241 (18.8%)  75 (34.1%)         1.94 (1.51 - 2.49)
#> 
```
