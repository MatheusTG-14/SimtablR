# Recompute reviewer-loop sensitivity variations

`sensitivity()` takes a computed result and recomputes named variations
of the recorded specification. Variations are either functions that take
a `simtab_spec` and return a modified `simtab_spec`, or supported
shorthand values such as `denominator = "complete"` and
`measure = "OR"`.

## Usage

``` r
sensitivity(result, ...)

# S3 method for class 'simtab_sensitivity'
as_flextable(x, ...)
```

## Arguments

- result:

  A computed `simtab_result`.

- ...:

  Named sensitivity variations. Unnamed variations are rejected.

- x:

  A `simtab_sensitivity` report.

## Value

A `simtab_sensitivity` report.

## Examples

``` r
res <- tb(epitabl, renal_impairment, adjudicated_acs, measure = "rr", ref = "No")
sensitivity(res, measure = "or")
#> <simtab_sensitivity>
#>  Variation Measure Estimate 95% CI low 95% CI high Delta %
#>    primary      RR 1.091065  0.9373810    1.269945       0
#>    measure      OR 1.153885  0.8956577    1.486562      NA
#>                                                 Note
#>                                                     
#>  Estimand changed from RR to OR; delta not computed.
#> 
#> Notes:
#>   - Estimand changed from RR to OR; delta not computed.
#> ! Methodological warning
#>   Outcome is common (38.1%); odds ratios can overstate the prevalence/risk
#>   ratio. Consider measure = 'PR' for cross-sectional tables or
#>   Poisson/log-binomial models for adjusted estimates.
#>   1 additional methodological note hidden. Run simtablr_guidance("teaching")
#>   separately before printing to show all advice.
```
