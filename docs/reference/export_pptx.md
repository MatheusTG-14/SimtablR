# Export a SimtablR table to a PowerPoint (.pptx) file

Export a SimtablR table to a PowerPoint (.pptx) file

## Usage

``` r
export_pptx(x, path, font_size = 14, overwrite = FALSE, ...)
```

## Arguments

- x:

  A SimtablR object with an `as_flextable()` method.

- path:

  Output file path. A missing `.pptx` suffix is added; any other suffix
  is rejected.

- font_size:

  Font size applied before export. Default `14`.

- overwrite:

  Logical. Existing files are protected by default; pass `TRUE` to
  replace the destination explicitly.

- ...:

  Passed to
  [`flextable::save_as_pptx()`](https://davidgohel.github.io/flextable/reference/save_as_pptx.html).

## Value

Invisibly returns the normalized output path.

## See also

[`export_docx()`](https://MatheusTG-14.github.io/SimtablR/reference/export_docx.md),
[`export_xlsx()`](https://MatheusTG-14.github.io/SimtablR/reference/export_xlsx.md)

## Examples

``` r
if (FALSE) { # \dontrun{
data(epitabl)
table1(epitabl, c("age", "sex"), by = "adjudicated_acs") |>
  export_pptx(tempfile(fileext = ".pptx"))
} # }
```
