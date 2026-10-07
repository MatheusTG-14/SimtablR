# Combine tb Objects by Rows

Vertical stacking of `tb` objects to create multi-variable tables.

## Usage

``` r
# S3 method for class 'simtab_tb'
rbind(..., deparse.level = 1)
```

## Arguments

- ...:

  Objects of class `simtab_tb` to be combined.

- deparse.level:

  Integer controlling label deparsing (unused).

## Value

A combined object of class `c("simtab_rbind_tb", "rbind_tb", "simtab")`.

## Details

This method is registered as an S3 method on
[`base::rbind()`](https://rdrr.io/r/base/cbind.html) (it does not mask
[`base::rbind`](https://rdrr.io/r/base/cbind.html)), so
`rbind(tb1, tb2)` dispatches here while ordinary matrix/data.frame
[`rbind()`](https://rdrr.io/r/base/cbind.html) is unaffected.

The resulting `rbind_tb` object combines multiple bivariate tables
sharing a common stratifying column into a stacked summary table.

## See also

[`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md)

## Examples

``` r
t1 <- tb(epitabl, sex, diabetes)
t2 <- tb(epitabl, hypertension, diabetes)
rbind(t1, t2)
#>                                       | History of diabetes  
#>                              Variable |  No   Yes | Total 
#> --------------------------------------+-----------+-------
#>  Sex recorded for clinical assessment |           |       
#>                                Female | 517   169 |  686  
#>                                  Male | 610   204 |  814  
#>               History of hypertension |           |       
#>                                    No | 566   166 |  732  
#>                                   Yes | 561   207 |  768  
#> --------------------------------------+-----------+-------
#>                                 Total | 1127  373 | 1500  
```
