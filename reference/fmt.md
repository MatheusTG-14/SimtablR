# Set render formatting options

Set render formatting options

## Usage

``` r
fmt(
  spec,
  d = NULL,
  conf_pct = NULL,
  labels = NULL,
  percent = NULL,
  big_mark = NULL,
  decimal_mark = NULL
)
```

## Arguments

- spec:

  A `simtab_spec`.

- d:

  Decimal places for percentages/estimates, or `NULL` to leave
  unchanged.

- conf_pct:

  Confidence-interval percentage label, or `NULL` to leave unchanged.

- labels:

  Optional named character labels, merged by variable name through
  [`label()`](https://MatheusTG-14.github.io/SimtablR/reference/label.md).

- percent:

  Logical, or `NULL` to leave unchanged. When `TRUE`, diagnostic and ROC
  proportion metrics render as percentages. Other engines ignore this
  field.

- big_mark:

  Character inserted between every three digits of the integer part of
  rendered numbers (e.g. `","` yields `4,391.2`), or `NULL` to leave
  unchanged. Default `""` (no grouping). Currently honoured by
  [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md).

- decimal_mark:

  Character used as the radix point in rendered numbers (e.g. `","` for
  many European locales), or `NULL` to leave unchanged. Default `"."`.
  Must differ from `big_mark`. Currently honoured by
  [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md).

## Value

A modified `simtab_spec`.

## Examples

``` r
fmt(simtab(epitabl), d = 1, conf_pct = 95)
#> <simtab_spec>
#>   not yet computed
#>   data: 1500 rows x 22 columns; hash c0787c425db7
#>   describe: <unset>
#>   stratify/by: <unset>
#>   adjust: <unset>
#>   summary: auto
#>   measure: <unset> (ref: <unset>, conf.level: 0.95)
#>   test: <unset>
#>   design: <unset>
#>   style: default
fmt(simtab(epitabl), big_mark = ",", decimal_mark = ".")
#> <simtab_spec>
#>   not yet computed
#>   data: 1500 rows x 22 columns; hash c0787c425db7
#>   describe: <unset>
#>   stratify/by: <unset>
#>   adjust: <unset>
#>   summary: auto
#>   measure: <unset> (ref: <unset>, conf.level: 0.95)
#>   test: <unset>
#>   design: <unset>
#>   style: default
```
