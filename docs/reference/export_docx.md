# Export a SimtablR table to a Word (.docx) file

Export a SimtablR table to a Word (.docx) file

## Usage

``` r
export_docx(x, path, footnotes = NULL, methods = FALSE, overwrite = FALSE, ...)
```

## Arguments

- x:

  A SimtablR object with an `as_flextable()` method (e.g. from
  [`table1()`](https://MatheusTG-14.github.io/SimtablR/reference/table1.md)
  or [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md)).

- path:

  Output file path. A missing `.docx` suffix is added; any other suffix
  is rejected.

- footnotes:

  Optional character vector passed to table renderers that support
  footnotes.

- methods:

  Logical; when `TRUE`, append recorded
  [`as_methods()`](https://MatheusTG-14.github.io/SimtablR/reference/as_methods.md)
  prose after the exported table(s).

- overwrite:

  Logical. Existing files are protected by default; pass `TRUE` to
  replace the destination explicitly.

- ...:

  Passed to
  [`flextable::save_as_docx()`](https://davidgohel.github.io/flextable/reference/save_as_docx.html)
  (e.g. page properties).

## Value

Invisibly returns the normalized output path.

## Details

Exports are written to a temporary file in the destination directory and
published only after the backend succeeds. Existing files are never
changed unless `overwrite = TRUE`.

## See also

[`export_pptx()`](https://MatheusTG-14.github.io/SimtablR/reference/export_pptx.md),
[`export_xlsx()`](https://MatheusTG-14.github.io/SimtablR/reference/export_xlsx.md),
[`table1()`](https://MatheusTG-14.github.io/SimtablR/reference/table1.md)

## Examples

``` r
if (FALSE) { # \dontrun{
data(epitabl)
table1(epitabl, c("age", "sex"), by = "adjudicated_acs") |>
  export_docx(tempfile(fileext = ".docx"))
} # }
```
