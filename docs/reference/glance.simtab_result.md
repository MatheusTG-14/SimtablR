# Glance a SimtablR result

Glance a SimtablR result

## Usage

``` r
# S3 method for class 'simtab_result'
glance(x, ...)

# S3 method for class 'simtab_spec'
glance(x, ...)
```

## Arguments

- x:

  A `simtab_result` or `simtab_spec`.

- ...:

  Ignored.

## Value

A one-row summary data.frame, where available.

## Examples

``` r
fit <- regtab(epitabl, "adjudicated_acs", ~ age + sex, family = binomial())
generics::glance(fit)
#>           outcome    n   family  link robust vcov method dispersion events
#> 1 adjudicated_acs 1500 binomial logit   TRUE  HC0    glm         NA    572
#>   exponentiate converged boundary failed error n_succeeded n_failed
#> 1         TRUE      TRUE    FALSE  FALSE  <NA>           1        0
```
