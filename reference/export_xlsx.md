# Export a SimtablR table to an Excel (.xlsx) file

Writes three worksheets: a long machine-readable tidy sheet with numeric
statistic cells, the formatted display sheet, and a data dictionary,
named `machine-readable`, `display`, and `data-dictionary`. Reports
write one display sheet per item instead of a single `display` sheet.

## Usage

``` r
export_xlsx(x, path, overwrite = FALSE, ...)
```

## Arguments

- x:

  A SimtablR object with an
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) method.

- path:

  Output file path. A missing `.xlsx` suffix is added; any other suffix
  is rejected.

- overwrite:

  Logical. Existing files are protected by default; pass `TRUE` to
  replace the destination explicitly.

- ...:

  Passed to
  [`openxlsx::writeData()`](https://rdrr.io/pkg/openxlsx/man/writeData.html).

## Value

Invisibly returns the normalized output path.

## See also

[`export_docx()`](https://MatheusTG-14.github.io/SimtablR/reference/export_docx.md),
[`export_pptx()`](https://MatheusTG-14.github.io/SimtablR/reference/export_pptx.md)

## Examples

``` r
if (FALSE) { # \dontrun{
data(epitabl)
table1(epitabl, c("age", "sex"), by = "adjudicated_acs") |>
  export_xlsx(tempfile(fileext = ".xlsx"))
} # }
```
