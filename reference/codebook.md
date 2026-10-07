# Build a SimtablR codebook

Build a SimtablR codebook

## Usage

``` r
codebook(x, ...)
```

## Arguments

- x:

  A data frame, `simtab_spec`, `simtab_result`, or `simtab_report`.

- ...:

  Ignored.

## Value

A `simtab_codebook` data frame with one row per variable.

## Details

For reports, a publication label recorded consistently by one or more
items is used. If report items record conflicting labels for the same
variable, `codebook()` warns and uses the neutral label stored on the
source column rather than choosing an arbitrary item by position.

## Examples

``` r
cb <- codebook(epitabl[, c("age", "sex", "diabetes")])
head(cb)
#> <simtab_codebook>
#>  variable                                label        type n_unique
#>       age    Age at index presentation (years)  continuous      503
#>       sex Sex recorded for clinical assessment categorical        2
#>  diabetes                  History of diabetes categorical        2
#>  levels/units n_missing (%)                                        summary
#>                    0 (0.0%) mean 61.97; median 61.90; range 18.00 to 94.00
#>  Female, Male      0 (0.0%)                                   Female, Male
#>       No, Yes      0 (0.0%)                                        No, Yes
```
