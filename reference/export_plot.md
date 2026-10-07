# Export a SimtablR plot to an image file

Writes a plot through
[`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html).
When `width` and `height` are not supplied, the plot's own recommended
dimensions are used; an explicit value always overrides the
recommendation.

## Usage

``` r
export_plot(
  x,
  path,
  width = NULL,
  height = NULL,
  dpi = 300,
  overwrite = FALSE,
  ...
)
```

## Arguments

- x:

  A `ggplot` object, or a SimtablR object with an `autoplot()` method.

- path:

  Output file path; the extension selects the device. When absent,
  `.png` is added. Supported suffixes are `.png`, `.pdf`, `.svg`,
  `.jpeg`, `.jpg`, `.tiff`, `.tif`, `.bmp`, `.eps`, `.ps`, `.tex`,
  `.wmf`, and `.emf`; device availability still depends on the platform.

- width, height:

  Canvas size in inches. Default `NULL`, meaning the plot's recommended
  dimensions.

- dpi:

  Resolution for raster devices. Default `300`.

- overwrite:

  Logical. Existing files are protected by default; pass `TRUE` to
  replace the destination explicitly.

- ...:

  Passed to
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html).

## Value

Invisibly returns the normalized output path.

## See also

[`export_docx()`](https://MatheusTG-14.github.io/SimtablR/reference/export_docx.md),
[`export_pptx()`](https://MatheusTG-14.github.io/SimtablR/reference/export_pptx.md)

## Examples

``` r
if (FALSE) { # \dontrun{
data(epitabl)
roc(epitabl, poc_hstn_value, adjudicated_acs) |>
  export_plot(tempfile(fileext = ".png"))
} # }
```
