# Export regtab results to CSV

Export regtab results to CSV

## Usage

``` r
export_regtab_csv(x, file, overwrite = FALSE, ...)
```

## Arguments

- x:

  A `regtab` result.

- file:

  Output file path. A missing `.csv` suffix is added.

- overwrite:

  Logical. Existing files are protected by default; pass `TRUE` to
  replace the destination explicitly.

- ...:

  Passed to
  [`utils::write.csv()`](https://rdrr.io/r/utils/write.table.html).

## Value

Invisibly returns `x`.

## Details

The file is completed in the destination directory before it is
published. Backend failures remove partial output and preserve any
existing destination.

## Examples

``` r
if (FALSE) { # \dontrun{
mod <- regtab(epitabl, outcomes = "rehospitalized", predictors = ~ age + sex)
export_regtab_csv(mod, tempfile(fileext = ".csv"))
} # }
```
