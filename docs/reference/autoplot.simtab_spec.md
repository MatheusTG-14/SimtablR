# Plot a computed SimtablR spec

Plot a computed SimtablR spec

## Usage

``` r
# S3 method for class 'simtab_spec'
autoplot(object, ...)
```

## Arguments

- object:

  A `simtab_spec`.

- ...:

  Passed to the computed result's autoplot method.

## Value

A `ggplot` object when the computed result supports autoplot.

## Examples

``` r
if (requireNamespace("ggplot2", quietly = TRUE)) {
  sp <- regtab(epitabl, "adjudicated_acs", ~ age + sex, family = binomial())$spec
  ggplot2::autoplot(sp)
}
```
