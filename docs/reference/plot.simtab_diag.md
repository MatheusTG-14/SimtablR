# Plot diagnostic test results

Draws a fourfold display of the retained confusion matrix with
sensitivity and specificity annotated on the bottom margin.

## Usage

``` r
# S3 method for class 'simtab_diag'
plot(x, col = c("#ffcccc", "#ccffcc"), main = "Confusion Matrix", ...)
```

## Arguments

- x:

  A `simtab_diag` result.

- col:

  Character vector of length 2. Fill colours for the negative and
  positive quadrants respectively. Default: `c("#ffcccc", "#ccffcc")`.

- main:

  Character. Plot title. Default: `"Confusion Matrix"`.

- ...:

  Additional arguments passed to
  [`graphics::fourfoldplot()`](https://rdrr.io/r/graphics/fourfoldplot.html).

## Value

Invisibly returns `x`.

## Examples

``` r
d <- diag_test(epitabl, poc_hstn_positive, adjudicated_acs,
               positive = "Yes", test_positive = "Positive")
#> Removed 601 observation(s) with missing values (40.1%).
plot(d)
```
