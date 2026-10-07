# Create a SimtablR style / journal preset

Builds a `simtab_style` object: the single specification that controls
both the text conventions of a table (how counts, percentages,
confidence intervals and p-values are written) and its visual flextable
theme. Pass the result to the `style` argument of
[`table1()`](https://MatheusTG-14.github.io/SimtablR/reference/table1.md),
or register it under a name with
[`register_journal()`](https://MatheusTG-14.github.io/SimtablR/reference/register_journal.md)
so it can be referred to as `style = "myjournal"`.

## Usage

``` r
journal_style(
  np_template = NULL,
  count_header = "n (%)",
  digits_cont = 1,
  digits_est = 2,
  ci_sep = " - ",
  ci_parens = "()",
  est_template = NULL,
  pval_thresh = 0.001,
  pval_digits = 3,
  pval_upper = NULL,
  pval_leading_zero = TRUE,
  pval_adaptive = FALSE,
  percent_sign = TRUE,
  flex = NULL
)
```

## Arguments

- np_template:

  Character template for a categorical "count (percent)" cell, using
  `{n}` and `{p}` placeholders. If `NULL`, derived from `percent_sign`
  (`"{n} ({p}%)"` or `"{n} ({p})"`).

- count_header:

  Character appended to categorical variable headers to note the cell
  convention, e.g. `"n (%)"` or `"No. (%)"`.

- digits_cont:

  Integer decimals for continuous summaries. Default `1`.

- digits_est:

  Integer decimals for effect-measure estimates. Default `2`.

- ci_sep:

  Character separating confidence-interval bounds (and IQR bounds), e.g.
  `" - "`, `"\u2013"`, `" to "`. Default `" - "`.

- ci_parens:

  Two characters wrapping the confidence interval, e.g. `"()"` or
  `"[]"`. Default `"()"`.

- est_template:

  Character template for an estimate-with-CI string, using `{est}`,
  `{lower}`, `{upper}`, `{sep}` and the bracket placeholders
  `{lp}`/`{rp}`. If `NULL`, `"{est} {lp}{lower}{sep}{upper}{rp}"`.

- pval_thresh:

  Numeric; p-values below this print as `"<thresh"`. Default `0.001`.

- pval_digits:

  Integer decimals for p-values. Default `3`.

- pval_upper:

  Optional numeric ceiling; p-values above this print as `">upper"`.
  Default `NULL`.

- pval_leading_zero:

  Logical; whether p-values keep a leading zero before the decimal
  point. Default `TRUE`.

- pval_adaptive:

  Logical; whether p-values above `0.01` use two decimals and smaller
  p-values use `pval_digits`. Default `FALSE`.

- percent_sign:

  Logical; whether the default `np_template` includes a `%`. Ignored
  when `np_template` is supplied. Default `TRUE`.

- flex:

  A function of one argument `function(ft) ...` that styles and returns
  a flextable, applied by `as_flextable()` on a `table1` result. If
  `NULL`, the SimtablR house theme (booktabs styling).

## Value

An object of class `"simtab_style"`.

## See also

[`register_journal()`](https://MatheusTG-14.github.io/SimtablR/reference/register_journal.md),
[`list_journals()`](https://MatheusTG-14.github.io/SimtablR/reference/list_journals.md),
[`table1()`](https://MatheusTG-14.github.io/SimtablR/reference/table1.md)

## Examples

``` r
s <- journal_style(count_header = "No. (%)", ci_sep = " to ", percent_sign = FALSE)
```
