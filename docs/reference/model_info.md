# Inspect retained model convergence information

Returns the stable model-information rows retained by
[`regtab()`](https://MatheusTG-14.github.io/SimtablR/reference/regtab.md)
or
[`survtab()`](https://MatheusTG-14.github.io/SimtablR/reference/survtab.md),
including analysed N, estimator, convergence, boundary, failure, and
error fields where applicable. Unlike the conventional accessors,
`model_info()` may select a failed outcome because its purpose is to
inspect that failure.

## Usage

``` r
model_info(object, outcome = NULL, ...)
```

## Arguments

- object:

  A computed
  [`regtab()`](https://MatheusTG-14.github.io/SimtablR/reference/regtab.md)
  or
  [`survtab()`](https://MatheusTG-14.github.io/SimtablR/reference/survtab.md)
  result.

- outcome:

  Optional single outcome name.

- ...:

  Unused.

## Value

A data frame with one row per requested model.

## Examples

``` r
data(epitabl)
fit <- regtab(
  epitabl, "rehospitalized", ~ age + sex,
  family = binomial("logit"), robust = FALSE
)
model_info(fit)
#>          outcome    n   family  link robust vcov method dispersion events
#> 1 rehospitalized 1500 binomial logit  FALSE none    glm         NA    520
#>   exponentiate converged boundary failed error
#> 1         TRUE      TRUE    FALSE  FALSE  <NA>
```
