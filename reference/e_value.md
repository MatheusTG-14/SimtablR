# Compute E-values for SimtablR ratio estimates

Computes VanderWeele-Ding E-values for ratio-scale estimates and
confidence limits. Risk ratios are used directly. For common outcomes,
odds ratios use the square-root approximation and hazard ratios use the
VanderWeele-Ding conversion `(1 - 0.5^sqrt(HR)) / (1 - 0.5^sqrt(1/HR))`;
with `rare = TRUE` the supplied ratio is treated as a rare-outcome
risk-ratio approximation.

## Usage

``` r
e_value(x, ...)

# S3 method for class 'simtab_result'
e_value(x, measure = NULL, rare = FALSE, ...)
```

## Arguments

- x:

  A computed SimtablR result containing ratio estimates.

- ...:

  Reserved for future options.

- measure:

  Optional ratio measure override: `"RR"`, `"OR"`, or `"HR"`.

- rare:

  Logical. Treat OR/HR estimates as rare-outcome approximations to risk
  ratios instead of applying the square-root approximation.

## Value

A `simtab_e_value` result with raw E-value numerics.

## Details

E-values require positive, finite ratio estimates and confidence limits
from a supported SimtablR result; unsupported scales fail with a classed
input condition. Missing or non-finite source estimates are not
converted into evidence. An E-value is a sensitivity-analysis threshold,
not proof that uncontrolled confounding is absent, and must be
interpreted with the identification assumptions of the parent analysis.

## References

VanderWeele, T. J., & Ding, P. (2017). Sensitivity analysis in
observational research: introducing the E-value. *Annals of Internal
Medicine*, 167(4), 268–274.
[doi:10.7326/M16-2607](https://doi.org/10.7326/M16-2607) .

## Examples

``` r
ratio_result <- tb(epitabl, diabetes, adjudicated_acs, or, ref = "No")
e_value(ratio_result)
#> E-values
#> 
#>     source outcome     term measure estimate conf.low conf.high  e_value
#>  bivariate    <NA> diabetes      OR 1.000000       NA        NA 1.000000
#>  bivariate    <NA> diabetes      OR 1.748705 1.379415  2.216859 1.975317
#>  e_value_ci approximation  rare
#>          NA          TRUE FALSE
#>    1.627177          TRUE FALSE
#> ℹ Methodological guidance
#>   E-values summarise the minimum unmeasured-confounding strength needed to
#>   explain away a ratio estimate. Interpret E-values alongside design quality,
#>   measured confounding control, and outcome prevalence.
#>   Run simtablr_guidance("off") separately before printing to hide advice.
```
