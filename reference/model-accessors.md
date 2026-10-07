# Access retained model evidence

[`regtab()`](https://MatheusTG-14.github.io/SimtablR/reference/regtab.md)
and
[`survtab()`](https://MatheusTG-14.github.io/SimtablR/reference/survtab.md)
are reporting-evidence objects rather than mutable fitted-model objects.
These methods expose the conventional model evidence that SimtablR
retains: link-scale coefficients, confidence intervals, formulas,
analysed observation counts, and covariance matrices.

## Usage

``` r
# S3 method for class 'simtab_result'
coef(object, outcome = NULL, ...)

# S3 method for class 'simtab_result'
vcov(object, outcome = NULL, complete = TRUE, ...)

# S3 method for class 'simtab_result'
formula(x, outcome = NULL, ...)

# S3 method for class 'simtab_result'
nobs(object, outcome = NULL, ...)

# S3 method for class 'simtab_result'
confint(object, parm, level = 0.95, outcome = NULL, ...)
```

## Arguments

- object, x:

  A computed
  [`regtab()`](https://MatheusTG-14.github.io/SimtablR/reference/regtab.md)
  or
  [`survtab()`](https://MatheusTG-14.github.io/SimtablR/reference/survtab.md)
  result.

- outcome:

  Optional single outcome name. Omit it for the conventional
  single-outcome value or a predictably named multi-outcome collection.

- ...:

  Unused.

- complete:

  Retained for compatibility with
  [`stats::vcov()`](https://rdrr.io/r/stats/vcov.html). SimtablR returns
  the complete copied covariance matrix.

- parm:

  Optional coefficient names or positions for
  [`stats::confint()`](https://rdrr.io/r/stats/confint.html).

- level:

  Confidence level for
  [`stats::confint()`](https://rdrr.io/r/stats/confint.html).

## Value

[`coef()`](https://rdrr.io/r/stats/coef.html) returns a named numeric
vector or named list of vectors;
[`confint()`](https://rdrr.io/r/stats/confint.html) and
[`vcov()`](https://rdrr.io/r/stats/vcov.html) return a matrix or named
list of matrices; [`formula()`](https://rdrr.io/r/stats/formula.html)
returns a formula or named list of formulas;
[`nobs()`](https://rdrr.io/r/stats/nobs.html) returns an integer scalar
or named integer vector.

## Details

A single-outcome result returns the conventional vector, matrix,
formula, or scalar. A multi-outcome result returns a list named by
outcome, except [`stats::nobs()`](https://rdrr.io/r/stats/nobs.html)
which returns a named integer vector. Supply `outcome` to select one
outcome. Unknown, non-scalar, and failed-outcome selections raise
`simtab_error_model`.

Confidence intervals replay the inference represented by the result.
Wald intervals can be returned at another `level` from copied
coefficient and covariance evidence. Firth profile intervals are
retained only at the level used to fit the result; requesting another
level requires refitting and is therefore rejected.

SimtablR deliberately does not retain fitted values, residuals, response
vectors, mutable fitted-model objects, or a prediction/forecasting
surface. Use [`stats::glm()`](https://rdrr.io/r/stats/glm.html),
[`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html), or
[`logistf::logistf()`](https://rdrr.io/pkg/logistf/man/logistf.html)
directly when those model-object workflows are required.

## Examples

``` r
data(epitabl)
fit <- regtab(
  epitabl,
  outcomes = "rehospitalized",
  predictors = ~ age + sex,
  family = binomial("logit"),
  robust = FALSE
)
coef(fit)
#>  (Intercept)          age      sexMale 
#> -0.359063959 -0.004323802 -0.013302179 
confint(fit)
#>                   2.5 %      97.5 %
#> (Intercept) -0.88136355 0.163235630
#> age         -0.01240443 0.003756827
#> sexMale     -0.22679836 0.200193998
formula(fit)
#> rehospitalized ~ age + sex
#> <environment: 0x55ca0f19a9e0>
nobs(fit)
#> [1] 1500
vcov(fit)
#>              (Intercept)           age       sexMale
#> (Intercept)  0.071013871 -1.047785e-03 -6.235531e-03
#> age         -0.001047785  1.699786e-05 -3.091932e-06
#> sexMale     -0.006235531 -3.091932e-06  1.186544e-02
```
