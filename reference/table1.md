# Describe a cohort across many variables (Table 1)

`table1()` builds the descriptive "Table 1" of a study in one call:
numeric variables as mean (SD) or median (IQR), categorical variables as
counts and percentages. Add `by` to get one column per group, optionally
with p-values and crude or adjusted effect measures (PR, RR, or OR). For
a single cross-tabulation use
[`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md); for
full regression output use
[`regtab()`](https://MatheusTG-14.github.io/SimtablR/reference/regtab.md).

## Usage

``` r
table1(
  data,
  vars,
  by = NULL,
  ...,
  flags = character(),
  measure = NULL,
  adjust = NULL,
  test = FALSE,
  summary = "auto",
  overall = TRUE,
  missing = TRUE,
  denominator = c("available", "complete"),
  na_model = c("drop", "fail", "explicit"),
  ref = NULL,
  var.type = NULL,
  labels = NULL,
  style = "default",
  d = 1,
  conf.level = 0.95,
  design = NULL
)
```

## Arguments

- data:

  A data frame.

- vars:

  Variables to describe: bare names in
  [`c()`](https://rdrr.io/r/base/c.html), a character vector, or a
  tidyselect expression such as `starts_with("cm_")`.

- by:

  Optional grouping variable: a bare name or string. Produces one column
  per group.

- ...:

  Terse flags such as `p` or `or`, written after `by`; see *Terse
  flags*. Only flags are allowed here.

- flags:

  Character vector of terse flags, e.g. `c("p", "or")`. The programmatic
  form of the bare flags in `...`.

- measure:

  String. Effect measure to add: `"PR"`, `"RR"`, or `"OR"`. Requires a
  binary `by`, whose last level is taken as the event.

- adjust:

  Optional covariates for an adjusted effect-measure column, given like
  `vars`. Requires `measure`.

- test:

  Logical or string. `TRUE` adds a p-value column with an automatically
  chosen test. A string forces one test: `"chisq"`, `"fisher"`,
  `"mcnemar"`, or `"trend"` for categorical variables, and `"t"`,
  `"wilcoxon"`, `"anova"`, or `"kruskal"` for numeric ones.

- summary:

  String. Summary for numeric variables: `"auto"` chooses between
  `"mean"` (mean and SD) and `"median"` (median and IQR) based on sample
  size and skewness. A named list sets it per variable, e.g.
  `list(age = "mean", bmi = "median")`.

- overall:

  Logical. If `TRUE`, add an "Overall" column for the whole cohort.

- missing:

  Logical. If `TRUE`, add a "Missing" count row under each variable that
  has missing values. This does not change any other number.

- denominator:

  String. Which rows the percentages and summaries use: `"available"`
  uses every non-missing value of each variable; `"complete"` uses only
  rows complete on all of `vars` and `by`.

- na_model:

  String. How adjusted models handle missing values: `"drop"` removes
  incomplete rows, `"fail"` stops with an error, and `"explicit"` treats
  missing categorical predictors as a `"(Missing)"` level.

- ref:

  Reference level for effect measures: one level for all variables, or a
  named list per variable. If `NULL`, each variable's first level is
  used.

- var.type:

  Force variable types: `"continuous"` or `"categorical"`, either one
  string for all variables or a named vector such as
  `c(score = "continuous")`. If `NULL`, types are detected automatically
  (see *Statistical methods*).

- labels:

  Named character vector of display labels, e.g.
  `c(smoking = "Smoking status")`. Variable labels already stored in
  `data` are used by default.

- style:

  Display style: a journal preset name (see
  [`list_journals()`](https://MatheusTG-14.github.io/SimtablR/reference/list_journals.md)),
  a
  [`journal_style()`](https://MatheusTG-14.github.io/SimtablR/reference/journal_style.md)
  object, `"n_pct"` or `"pct_n"`, a template using `{n}` and `{p}`, or a
  named list per variable.

- d:

  Integer. Decimal places for percentages and continuous summaries.

- conf.level:

  Number between 0 and 1. Confidence level for effect measure intervals.

- design:

  String. Study design used to choose the effect measure when `measure`
  is not set: `"cross_sectional"` gives PR, `"cohort"` gives RR, and
  `"case_control"` gives OR.

## Value

A `simtab_result` of class `simtab_table1`. Print it to see the
formatted table, convert it with
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html), or save
it with
[`export_docx()`](https://MatheusTG-14.github.io/SimtablR/reference/export_docx.md),
[`export_pptx()`](https://MatheusTG-14.github.io/SimtablR/reference/export_pptx.md),
or
[`export_xlsx()`](https://MatheusTG-14.github.io/SimtablR/reference/export_xlsx.md).
Unrounded results are stored in `$data`.

## Details

### Statistical methods

Crude effect measures for categorical variables use the same estimators
as [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md):
the Katz log interval for prevalence and risk ratios (Katz et al., 1978)
and the Woolf logit interval for odds ratios (Woolf, 1955). For numeric
variables, and for every adjusted estimate, a separate generalized
linear model is fitted for each described variable, with the `adjust`
covariates added. Odds ratios come from logistic regression. Prevalence
and risk ratios come from log-binomial regression, falling back to
modified Poisson regression with robust standard errors when that model
does not converge (Barros & Hirakata, 2003; Zou, 2004). PR and RR are
computed identically; only the label differs.

With `test = TRUE`, 2x2 tables use the N-1 chi-squared test (Campbell,
2007) and larger tables the Pearson chi-squared test, without continuity
correction; Fisher's exact test is chosen only when an expected count is
below 1. Numeric variables are compared with Welch's t-test or ANOVA
when summarised by the mean, and with the Wilcoxon or Kruskal-Wallis
test when summarised by the median.

Numeric variables are treated as continuous unless they look like coded
categories: exactly two distinct whole numbers, or at most seven
distinct whole numbers with at least 20 observations. Use `var.type` to
override.

### Missing data

By default each variable is described using its own non-missing values,
and a "Missing" row reports how many are absent (STROBE item 14). Tests
are always computed on observed values. `denominator = "complete"`
restricts the whole table to complete cases, so the displayed numbers
can change. `na_model` controls only the adjusted models.

### Modifying the result

Multiplicity adjustment, paired tests, and standardized mean differences
(Austin, 2009) are set afterwards with
[`test()`](https://MatheusTG-14.github.io/SimtablR/reference/test.md),
e.g.
`table1(df, vars, by = group, test = TRUE) |> test(p.adjust = "holm", smd = TRUE)`.
With `paired = TRUE`, two equal-sized groups are treated as matched
pairs in row order. Use
[`add_effect()`](https://MatheusTG-14.github.io/SimtablR/reference/add_effect.md)
to add or replace the effect-measure columns and
[`fmt()`](https://MatheusTG-14.github.io/SimtablR/reference/fmt.md) to
change decimals.

## Terse flags

Flags may be supplied as bare words in `...` after `by`, or
programmatically with `flags =`. Anything else in `...` is treated as a
typo.

- `col`:

  Column percentages (the default).

- `row`, `cell`, `perc`:

  Not yet supported by `table1()`.

- `pr`, `rr`, `or`:

  Add the corresponding crude effect measure.

- `p`:

  Add a p-value from an automatically chosen test, like `test = TRUE`.

- `miss`:

  Show missing values.

The old flags `rp` and `m` still work as aliases for `pr` and `miss` but
are deprecated.

## References

Austin, P. C. (2009). Balance diagnostics for comparing the distribution
of baseline covariates between treatment groups in propensity-score
matched samples. *Statistics in Medicine*, 28(25), 3083–3107.
[doi:10.1002/sim.3697](https://doi.org/10.1002/sim.3697) .

Barros, A. J. D., & Hirakata, V. N. (2003). Alternatives for logistic
regression in cross-sectional studies: an empirical comparison of models
that directly estimate the prevalence ratio. *BMC Medical Research
Methodology*, 3, 21.
[doi:10.1186/1471-2288-3-21](https://doi.org/10.1186/1471-2288-3-21) .

Campbell, I. (2007). Chi-squared and Fisher-Irwin tests of two-by-two
tables with small sample recommendations. *Statistics in Medicine*,
26(19), 3661–3675.
[doi:10.1002/sim.2832](https://doi.org/10.1002/sim.2832) .

Katz, D., Baptista, J., Azen, S. P., & Pike, M. C. (1978). Obtaining
confidence intervals for the risk ratio in cohort studies. *Biometrics*,
34(3), 469–474. [doi:10.2307/2530610](https://doi.org/10.2307/2530610) .

Woolf, B. (1955). On estimating the relation between blood group and
disease. *Annals of Human Genetics*, 19(4), 251–253.
[doi:10.1111/j.1469-1809.1955.tb01348.x](https://doi.org/10.1111/j.1469-1809.1955.tb01348.x)
.

Zou, G. (2004). A modified Poisson regression approach to prospective
studies with binary data. *American Journal of Epidemiology*, 159(7),
702–706. [doi:10.1093/aje/kwh090](https://doi.org/10.1093/aje/kwh090) .

## See also

[`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md) for a
single cross-tabulation,
[`add_effect()`](https://MatheusTG-14.github.io/SimtablR/reference/add_effect.md)
and
[`test()`](https://MatheusTG-14.github.io/SimtablR/reference/test.md) to
modify the table,
[`journal_style()`](https://MatheusTG-14.github.io/SimtablR/reference/journal_style.md)
for formatting, and
[simtablr_references](https://MatheusTG-14.github.io/SimtablR/reference/simtablr_references.md)
for all references cited by SimtablR.

## Examples

``` r
# Describe the whole cohort
table1(epitabl, c(age, sex, smoking, bmi))
#> Characteristic                                 Overall (N=1500) 
#> ---------------------------------------------------------------
#> Age at index presentation (years) [Mean (SD)]       62.0 (13.2) 
#> Sex recorded for clinical assessment, n (%)                     
#>   Female                                            686 (45.7%) 
#>   Male                                              814 (54.3%) 
#> Smoking status, n (%)                                           
#>   Never                                             742 (49.5%) 
#>   Former                                            469 (31.3%) 
#>   Current                                           289 (19.3%) 
#> Body mass index (kg/m2) [Mean (SD)]                  27.8 (4.9) 
#>   Missing                                                    63 

# Compare groups, with p-values
table1(epitabl, c(age, sex, smoking), by = adjudicated_acs, flags = "p")
#> `p` now adds the p-value column (SimtablR 3.0); use `perc` or `cell` for total
#> percentages.
#> Characteristic                                 Overall (N=1500)   No (N=928)  Yes (N=572)  P-value 
#> --------------------------------------------------------------------------------------------------
#> Age at index presentation (years) [Mean (SD)]       62.0 (13.2)  60.5 (12.8)  64.4 (13.4)   <0.001 
#> Sex recorded for clinical assessment, n (%)                                                  0.021 
#>   Female                                            686 (45.7%)  446 (48.1%)  240 (42.0%)          
#>   Male                                              814 (54.3%)  482 (51.9%)  332 (58.0%)          
#> Smoking status, n (%)                                                                        0.001 
#>   Never                                             742 (49.5%)  474 (51.1%)  268 (46.9%)          
#>   Former                                            469 (31.3%)  302 (32.5%)  167 (29.2%)          
#>   Current                                           289 (19.3%)  152 (16.4%)  137 (24.0%)          
#> 
#> Tests: Welch t-test; N-1 chi-squared; Pearson's Chi-squared test
#> ℹ Methodological guidance
#>   3 unadjusted p-values are reported in this table. Pipe into test(p.adjust =
#>   'holm') for family-wise control or test(p.adjust = 'BH') for
#>   false-discovery-rate control.
#>   Run simtablr_guidance("off") separately before printing to hide advice.

# Crude and age- and sex-adjusted prevalence ratios
table1(
  epitabl, c(smoking, diabetes, hypertension),
  by = adjudicated_acs, measure = "PR", adjust = c(age, sex)
)
#> Characteristic                  Overall (N=1500)   No (N=928)  Yes (N=572)         PR (95% CI)  Adjusted PR (95% CI) 
#> --------------------------------------------------------------------------------------------------------------------
#> Smoking status, n (%)                                                                                                
#>   Never                              742 (49.5%)  474 (51.1%)  268 (46.9%)          1.00 (Ref)            1.00 (Ref) 
#>   Former                             469 (31.3%)  302 (32.5%)  167 (29.2%)  0.99 (0.84 - 1.15)    0.98 (0.84 - 1.14) 
#>   Current                            289 (19.3%)  152 (16.4%)  137 (24.0%)  1.31 (1.12 - 1.53)    1.26 (1.08 - 1.47) 
#> History of diabetes, n (%)                                                                                           
#>   No                                1127 (75.1%)  735 (79.2%)  392 (68.5%)          1.00 (Ref)            1.00 (Ref) 
#>   Yes                                373 (24.9%)  193 (20.8%)  180 (31.5%)  1.39 (1.22 - 1.58)    1.32 (1.16 - 1.50) 
#> History of hypertension, n (%)                                                                                       
#>   No                                 732 (48.8%)  463 (49.9%)  269 (47.0%)          1.00 (Ref)            1.00 (Ref) 
#>   Yes                                768 (51.2%)  465 (50.1%)  303 (53.0%)  1.07 (0.94 - 1.22)    0.98 (0.86 - 1.11) 

# Add standardized mean differences after the fact
table1(epitabl, c(age, sex), by = adjudicated_acs, test = TRUE) |>
  test(smd = TRUE)
#> Characteristic                                 Overall (N=1500)   No (N=928)  Yes (N=572)   SMD  P-value 
#> --------------------------------------------------------------------------------------------------------
#> Age at index presentation (years) [Mean (SD)]       62.0 (13.2)  60.5 (12.8)  64.4 (13.4)  0.30   <0.001 
#> Sex recorded for clinical assessment, n (%)                                                0.12    0.021 
#>   Female                                            686 (45.7%)  446 (48.1%)  240 (42.0%)                
#>   Male                                              814 (54.3%)  482 (51.9%)  332 (58.0%)                
#> 
#> Tests: Welch t-test; N-1 chi-squared
#> ℹ Methodological guidance
#>   2 additional methodological notes hidden. Run simtablr_guidance("teaching")
#>   separately before printing to show all advice.
```
