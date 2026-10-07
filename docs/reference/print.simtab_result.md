# Print a computed SimtablR result

Dispatches to the engine's registered `print` renderer. Falls back to an
honest generic (header + `as_data_frame` renderer if available + advice)
when the engine registered no `print` renderer.

## Usage

``` r
# S3 method for class 'simtab_result'
print(x, ...)
```

## Arguments

- x:

  A `simtab_result`.

- ...:

  Passed to subclass renderers. For `table1` and `regtab`,
  `details = TRUE` restores the decorative result summary and stored
  call. For `regtab`, `vif = TRUE` adds VIF-equivalent columns to the
  display frame.

## Value

Invisibly returns `x`.

## Examples

``` r
res <- tb(epitabl, sex, diabetes)
print(res)
#>                                       | History of diabetes  
#>  Sex recorded for clinical assessment |  No   Yes | Total 
#> --------------------------------------+-----------+-------
#>                                Female | 517   169 |  686  
#>                                  Male | 610   204 |  814  
#> --------------------------------------+-----------+-------
#>                                 Total | 1127  373 | 1500  
```
