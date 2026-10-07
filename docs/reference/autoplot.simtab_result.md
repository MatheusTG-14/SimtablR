# Autoplot a computed SimtablR result

Dispatches to the engine's registered `autoplot` renderer.

## Usage

``` r
# S3 method for class 'simtab_result'
autoplot(object, ...)
```

## Arguments

- object:

  A `simtab_result`.

- ...:

  Passed to the engine's autoplot renderer.

## Value

A `ggplot` object.

## Examples

``` r
if (requireNamespace("ggplot2", quietly = TRUE)) {
  r <- roc(epitabl, poc_hstn_value, adjudicated_acs)
  ggplot2::autoplot(r)
}
#> Removed 601 observation(s) with missing values (40.1%).
#> Auto-detected outcome positive level: 'Yes'
```
