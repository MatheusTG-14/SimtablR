# Build a participant flow table from recorded facts

`flow()` assembles source, subset, rendered-cohort, complete-case, and
model Ns from a result or report without fitting or recomputing
anything.

## Usage

``` r
flow(x, ...)

# S3 method for class 'simtab_flow'
as_flextable(x, ...)

# S3 method for class 'simtab_flow'
autoplot(object, ...)
```

## Arguments

- x:

  A `simtab_flow` object.

- ...:

  Ignored.

- object:

  A `simtab_flow` object.

## Value

A `simtab_flow` object.

## Examples

``` r
res <- tb(epitabl, sex, diabetes)
flow(res)
#> <simtab_flow>
#>                    stage    N excluded
#>  Unknown before SimtablR   NA       NA
#>              Source data 1500       NA
#>                                                        reason
#>  Rows excluded before data reached SimtablR are not recorded.
#>                                    Rows captured by SimtablR.
```
