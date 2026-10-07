# Add or replace the effect-measure column(s) of a table1

Recomputes `tab` with a crude (and optionally adjusted) effect-measure
column. Equivalent to passing `measure=`/`adjust=` to
[`table1()`](https://MatheusTG-14.github.io/SimtablR/reference/table1.md)
directly; calling it when an effect column already exists replaces it.

## Usage

``` r
add_effect(tab, measure, adjust.for = NULL, ref = NULL, conf.level = NULL, ...)
```

## Arguments

- tab:

  A `table1` object.

- measure:

  One of `"OR"`, `"PR"`, `"RR"`.

- adjust.for:

  Optional covariate names for an adjusted column.

- ref:

  Optional reference level(s); see
  [`table1()`](https://MatheusTG-14.github.io/SimtablR/reference/table1.md).

- conf.level:

  Optional confidence level; defaults to the table's.

- ...:

  Ignored.

## Value

The recomputed `table1` object.

## See also

[`table1()`](https://MatheusTG-14.github.io/SimtablR/reference/table1.md),
[`test()`](https://MatheusTG-14.github.io/SimtablR/reference/test.md)

## Examples

``` r
data(epitabl)
table1(epitabl, c("sex", "smoking"), by = "adjudicated_acs") |> add_effect("OR")
#> Characteristic                               Overall (N=1500)   No (N=928)  Yes (N=572)         OR (95% CI) 
#> -----------------------------------------------------------------------------------------------------------
#> Sex recorded for clinical assessment, n (%)                                                                 
#>   Female                                          686 (45.7%)  446 (48.1%)  240 (42.0%)          1.00 (Ref) 
#>   Male                                            814 (54.3%)  482 (51.9%)  332 (58.0%)  1.28 (1.04 - 1.58) 
#> Smoking status, n (%)                                                                                       
#>   Never                                           742 (49.5%)  474 (51.1%)  268 (46.9%)          1.00 (Ref) 
#>   Former                                          469 (31.3%)  302 (32.5%)  167 (29.2%)  0.98 (0.77 - 1.24) 
#>   Current                                         289 (19.3%)  152 (16.4%)  137 (24.0%)  1.59 (1.21 - 2.10) 
#> ! Methodological warning
#>   Outcome is common (38.1%); odds ratios can overstate the prevalence/risk
#>   ratio. Consider measure = 'PR' for cross-sectional tables or
#>   Poisson/log-binomial models for adjusted estimates.
#>   Run simtablr_guidance("off") separately before printing to hide advice.
```
