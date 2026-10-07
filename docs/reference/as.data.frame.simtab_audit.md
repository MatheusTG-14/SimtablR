# Coerce a SimtablR audit to a data.frame

Returns the checked-rules table (`$checked`) with a `fired` logical
column joined on, so the fired/silent distinction shown by
[`print()`](https://rdrr.io/r/base/print.html) is available as a single
rectangular table.

## Usage

``` r
# S3 method for class 'simtab_audit'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
```

## Arguments

- x:

  A `simtab_audit`.

- row.names, optional, ...:

  Ignored; accepted for S3 consistency.

## Value

A data.frame with one row per checked rule.

## Examples

``` r
res <- tb(epitabl, sex, diabetes)
as.data.frame(advise(res, audit = TRUE))
#>                                     id severity            citation
#> 1 complete_case_unreported_missingness        2      STROBE item 14
#> 2              multiplicity_unadjusted        2 Bender & Lange 2001
#>                                                                                                                    fix
#> 1                                 Add the miss flag (tb) or missing = TRUE (table1), or report missingness separately.
#> 2 Pipe into test(p.adjust = 'holm') for family-wise control or test(p.adjust = 'BH') for false-discovery-rate control.
#>                version fired
#> 1 downscale-2026-09-23 FALSE
#> 2 downscale-2026-09-23 FALSE
```
