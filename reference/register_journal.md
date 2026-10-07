# Register a named journal style preset

Stores a `simtab_style` in SimtablR's preset registry under `name` so it
can be used as `style = name` in
[`table1()`](https://MatheusTG-14.github.io/SimtablR/reference/table1.md)
and the `add_*()` helpers. Registering a name that already exists
overwrites it.

## Usage

``` r
register_journal(name, style)
```

## Arguments

- name:

  Character preset name (case-insensitive).

- style:

  A `simtab_style` object from
  [`journal_style()`](https://MatheusTG-14.github.io/SimtablR/reference/journal_style.md).

## Value

Invisibly, `name`.

## See also

[`journal_style()`](https://MatheusTG-14.github.io/SimtablR/reference/journal_style.md),
[`list_journals()`](https://MatheusTG-14.github.io/SimtablR/reference/list_journals.md)

## Examples

``` r
register_journal("mylab", journal_style(count_header = "No. (%)", ci_sep = " to "))
list_journals()
#> [1] "default"        "jama"           "lancet"         "mylab"         
#> [5] "nejm"           "strobe-default"
```
