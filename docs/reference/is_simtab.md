# Test whether an object is a SimtablR result, optionally of a given preset

Namespaced subclasses (`simtab_tb`, `simtab_table1`, `simtab_regtab`,
`simtab_diag`, `simtab_roc`, ...) are the supported way to test result
identity; bare legacy tags (kept in the class vector for external,
backward-compatible [`inherits()`](https://rdrr.io/r/base/class.html)
callers) should not be tested directly in new package code. Use
`is_simtab(x, preset)` instead.

## Usage

``` r
is_simtab(x, preset = NULL)
```

## Arguments

- x:

  Any object.

- preset:

  Optional character engine/preset name (e.g. `"tb"`, `"roc"`).

## Value

`TRUE`/`FALSE`.

## Examples

``` r
res <- tb(epitabl, sex, diabetes)
is_simtab(res)
#> [1] TRUE
is_simtab(res, preset = "tb")
#> [1] TRUE
is_simtab(iris)
#> [1] FALSE
```
