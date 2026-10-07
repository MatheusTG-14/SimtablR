# Print Method for simtab_rbind_tb Objects

Print Method for simtab_rbind_tb Objects

## Usage

``` r
# S3 method for class 'simtab_rbind_tb'
print(x, digits = NULL, ...)
```

## Arguments

- x:

  A `simtab_rbind_tb` object.

- digits:

  Minimum number of significant digits to be printed.

- ...:

  Additional arguments.

## Value

Invisibly returns `x`.

## Examples

``` r
t1 <- tb(epitabl, sex, diabetes)
t2 <- tb(epitabl, hypertension, diabetes)
print(rbind(t1, t2))
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
