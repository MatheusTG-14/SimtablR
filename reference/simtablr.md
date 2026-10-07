# Build a manuscript-ready SimtablR report in one call

`simtablr()` composes existing presets into a transparent
`simtab_report`. It creates a cohort-description Table 1 and an
exposure-to-outcome Table 2 using the same
[`table1()`](https://MatheusTG-14.github.io/SimtablR/reference/table1.md)
effect path as direct users. No new estimation is performed by the
orchestrator.

## Usage

``` r
simtablr(
  data,
  outcome,
  exposure,
  vars = NULL,
  adjust = NULL,
  design = NULL,
  test = TRUE,
  style = "default",
  ...
)
```

## Arguments

- data:

  A data.frame.

- outcome:

  Bare column name for the outcome.

- exposure:

  Bare column name for the exposure.

- vars:

  Optional tidyselect expression of Table 1 variables. Defaults to all
  columns except `outcome` and `exposure`.

- adjust:

  Optional tidyselect expression of adjustment covariates for Table 2.

- design:

  Optional per-call study design used by the existing design-aware
  measure resolver.

- test:

  Logical or test name passed to
  [`table1()`](https://MatheusTG-14.github.io/SimtablR/reference/table1.md).

- style:

  Display style passed to
  [`table1()`](https://MatheusTG-14.github.io/SimtablR/reference/table1.md).

- ...:

  Additional
  [`table1()`](https://MatheusTG-14.github.io/SimtablR/reference/table1.md)
  arguments. Effect-specific arguments such as `measure` are applied to
  Table 2 only.

## Value

A `simtab_report` with named `table1` and `table2` results.

## Examples

``` r
if (FALSE) { # \dontrun{
data(epitabl)
simtablr(epitabl, outcome = adjudicated_acs, exposure = smoking,
         vars = c(age, sex, smoking), adjust = c(age, sex),
         design = "cross_sectional")
} # }
```
