# Export regtab results to Excel

Export regtab results to Excel

## Usage

``` r
export_regtab_xlsx(x, file, overwrite = FALSE, ...)
```

## Arguments

- x:

  A `regtab` result.

- file:

  Output file path. A missing `.xlsx` suffix is added.

- overwrite:

  Logical. Existing files are protected by default; pass `TRUE` to
  replace the destination explicitly.

- ...:

  Passed to
  [`export_xlsx()`](https://MatheusTG-14.github.io/SimtablR/reference/export_xlsx.md).

## Value

Invisibly returns `x`.

## Examples

``` r
if (FALSE) { # \dontrun{
mod <- regtab(epitabl, outcomes = "rehospitalized", predictors = ~ age + sex)
export_regtab_xlsx(mod, tempfile(fileext = ".xlsx"))
} # }
```
