# Cross-tabulate one or two variables

`tb()` builds a frequency table for one variable, or a cross-tabulation
of two, with optional percentages, an association test, and crude effect
measures (PR, RR, or OR). Put the exposure first (rows) and the outcome
second (columns); a numeric first variable is summarised as mean (SD) or
median (IQR) within each outcome group. For many variables at once use
[`table1()`](https://MatheusTG-14.github.io/SimtablR/reference/table1.md);
for adjusted estimates use
[`regtab()`](https://MatheusTG-14.github.io/SimtablR/reference/regtab.md).

## Usage

``` r
tb(
  data,
  ...,
  m = FALSE,
  miss = FALSE,
  d = 1,
  big_mark = "",
  decimal_mark = ".",
  style = "n_pct",
  style.rp = "{rp} ({lower} - {upper})",
  style.or = "{or} ({lower} - {upper})",
  test = FALSE,
  subset = NULL,
  strat = NULL,
  rp = FALSE,
  or = FALSE,
  ref = NULL,
  conf.level = 0.95,
  var.type = NULL,
  summary = "auto",
  flags = NULL,
  labels = NULL,
  measure = NULL,
  design = NULL
)
```

## Arguments

- data:

  A data frame, or a single vector to tabulate on its own.

- ...:

  One or two variables to tabulate: bare names, strings, or tidyselect
  expressions such as `all_of(v)`. The first becomes the rows and the
  second the columns. Terse flags such as `row` or `or` may follow; see
  *Terse flags*.

- m:

  Deprecated; use `miss` instead.

- miss:

  Logical. If `TRUE`, show missing values as their own row and column.
  Same as the `miss` flag.

- d:

  Integer. Decimal places for percentages and continuous summaries.
  Effect measures always use two decimals.

- big_mark:

  String inserted between every three digits, e.g. `","` prints `4391`
  as `4,391`.

- decimal_mark:

  String used as the decimal point, e.g. `","`. Must differ from
  `big_mark`.

- style:

  String. How counts and percentages are shown: `"n_pct"` (`12 (5.0%)`),
  `"pct_n"` (`5.0% (12)`), or a template using `{n}` and `{p}`, such as
  `"{n} [{p}%]"`.

- style.rp:

  String template for prevalence and risk ratios, using `{rp}`,
  `{lower}`, and `{upper}`.

- style.or:

  String template for odds ratios, using `{or}`, `{lower}`, and
  `{upper}`.

- test:

  Logical or string. `TRUE` adds a p-value from an automatically chosen
  test; or force `"chisq"`, `"fisher"`, or `"mcnemar"`. See *Statistical
  methods* for how the test is chosen.

- subset:

  A logical expression evaluated in `data` to keep only some rows, e.g.
  `subset = age >= 65`.

- strat:

  A variable to stratify by: a bare name or string. The table is
  repeated within each stratum and, if an effect measure is requested, a
  Mantel-Haenszel pooled estimate is added.

- rp:

  Deprecated; use the `pr` flag or `measure = "PR"` instead.

- or:

  Logical. If `TRUE`, add odds ratios. Same as the `or` flag.

- ref:

  Reference level of the row (exposure) variable for effect measures,
  given as a level name or its position. If `NULL`, the first level is
  used and a note is printed.

- conf.level:

  Number between 0 and 1. Confidence level for effect measure intervals.

- var.type:

  Force variable types: `"continuous"` or `"categorical"`, either one
  string for all variables or a named vector such as
  `c(score = "continuous")`. If `NULL`, types are detected automatically
  (see *Statistical methods*).

- summary:

  String. Summary for numeric variables: `"auto"` chooses between
  `"mean"` (mean and SD) and `"median"` (median and IQR) based on sample
  size and skewness.

- flags:

  Character vector of terse flags, e.g. `c("row", "or", "p")`. The
  programmatic form of the bare flags in `...`.

- labels:

  Named character vector of display labels, e.g.
  `c(smoking = "Smoking status")`. Variable labels already stored in
  `data` are used by default.

- measure:

  String. Effect measure to add: `"PR"`, `"RR"`, or `"OR"`. Overrides
  any measure implied by `design`.

- design:

  String. Study design used to choose the effect measure when none is
  requested: `"cross_sectional"` gives PR, `"cohort"` gives RR, and
  `"case_control"` gives OR.

## Value

A `simtab_result` of class `simtab_tb`. Print it to see the formatted
table, convert it with
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html), or save
it with
[`export_docx()`](https://MatheusTG-14.github.io/SimtablR/reference/export_docx.md),
[`export_pptx()`](https://MatheusTG-14.github.io/SimtablR/reference/export_pptx.md),
or
[`export_xlsx()`](https://MatheusTG-14.github.io/SimtablR/reference/export_xlsx.md).
Unrounded results are stored in `$data`.

## Details

### Statistical methods

Effect measures compare each row level with the reference level (`ref`),
taking the last column level as the event. Prevalence and risk ratios
use the Katz log interval (Katz et al., 1978) and odds ratios the Woolf
logit interval (Woolf, 1955). With `strat`, stratum-specific tables are
pooled with the Mantel-Haenszel estimator and tested with the
Cochran-Mantel-Haenszel test.

With `test = TRUE`, 2x2 tables use the N-1 chi-squared test (Campbell,
2007) and larger tables the Pearson chi-squared test, without continuity
correction. Fisher's exact test is chosen automatically only when an
expected count is below 1; request it with `test = "fisher"`. Numeric
variables are compared with the t-test or ANOVA when summarised by the
mean, and with the Wilcoxon or Kruskal-Wallis test when summarised by
the median.

Numeric variables are treated as continuous unless they look like coded
categories: exactly two distinct whole numbers, or at most seven
distinct whole numbers with at least 20 observations. Use `var.type` to
override.

### Missing data

Missing values are excluded from counts, percentages, tests, and effect
measures. Use the `miss` flag to display them as a separate category.

### Modifying the result

Paired tests and multiplicity adjustment are set afterwards with
[`test()`](https://MatheusTG-14.github.io/SimtablR/reference/test.md),
e.g. `tb(df, value, group, test = TRUE) |> test(paired = TRUE)`. Use
[`fmt()`](https://MatheusTG-14.github.io/SimtablR/reference/fmt.md) to
change decimals and [`rbind()`](https://rdrr.io/r/base/cbind.html) to
stack several `tb()` tables that share the same column variable.

## Terse flags

Flags may be supplied as bare words in `...` alongside the selected
variables, or programmatically with `flags =`.

- `row`:

  Row percentages.

- `col`:

  Column percentages.

- `cell`, `perc`:

  Percentages of the table total. `perc` reads better in one-variable
  tables.

- `pr`, `rr`, `or`:

  Add the corresponding crude effect measure.

- `p`:

  Add a p-value from an automatically chosen test, like `test = TRUE`.

- `miss`:

  Show missing values.

The old flags `rp` and `m` still work as aliases for `pr` and `miss` but
are deprecated.

## References

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

Pearson, K. (1900). On the criterion that a given system of deviations
from the probable in the case of a correlated system of variables is
such that it can be reasonably supposed to have arisen from random
sampling. *Philosophical Magazine, Series 5*, 50(302), 157–175.
[doi:10.1080/14786440009463897](https://doi.org/10.1080/14786440009463897)
.

Fisher, R. A. (1935). *The Design of Experiments*. Oliver & Boyd.

## See also

[`table1()`](https://MatheusTG-14.github.io/SimtablR/reference/table1.md)
for many variables at once,
[`regtab()`](https://MatheusTG-14.github.io/SimtablR/reference/regtab.md)
for adjusted effect measures,
[`diag_test()`](https://MatheusTG-14.github.io/SimtablR/reference/diag_test.md)
for diagnostic accuracy, and
[simtablr_references](https://MatheusTG-14.github.io/SimtablR/reference/simtablr_references.md)
for all references cited by SimtablR.

## Examples

``` r
# Frequencies of one variable
tb(epitabl, smoking)
#>  Smoking status | Freq 
#> ----------------+------
#>           Never | 742  
#>          Former | 469  
#>         Current | 289  
#> ----------------+------
#>           Total | 1500 

# Row percentages, a p-value, and crude prevalence ratios
tb(epitabl, smoking, adjudicated_acs, flags = c("row", "pr", "p"), ref = "Never")
#>                 | Adjudicated acute coronary syndrome 
#>  Smoking status |     No           Yes     | Total 
#> ----------------+--------------------------+-------
#>           Never | 474 (63.9%)  268 (36.1%) |  742  
#>          Former | 302 (64.4%)  167 (35.6%) |  469  
#>         Current | 152 (52.6%)  137 (47.4%) |  289  
#> ----------------+--------------------------+-------
#>           Total |     928          572     | 1500  
#> 
#>  Smoking status |          PR (95% CI)          
#> ----------------+-------------------------------
#>           Never |          1.00 (Ref)           
#>          Former | 0.99 (0.84 - 1.15), p = 0.857 
#>         Current | 1.31 (1.12 - 1.53), p < 0.001 
#> ----------------+-------------------------------
#>           Total |                               
#> 
#>   Test: Pearson's Chi-squared test  p-value = 0.001 

# The same request with bare flags (shorthand)
tb(epitabl, smoking, adjudicated_acs, row, pr, p, ref = "Never")
#>                 | Adjudicated acute coronary syndrome 
#>  Smoking status |     No           Yes     | Total 
#> ----------------+--------------------------+-------
#>           Never | 474 (63.9%)  268 (36.1%) |  742  
#>          Former | 302 (64.4%)  167 (35.6%) |  469  
#>         Current | 152 (52.6%)  137 (47.4%) |  289  
#> ----------------+--------------------------+-------
#>           Total |     928          572     | 1500  
#> 
#>  Smoking status |          PR (95% CI)          
#> ----------------+-------------------------------
#>           Never |          1.00 (Ref)           
#>          Former | 0.99 (0.84 - 1.15), p = 0.857 
#>         Current | 1.31 (1.12 - 1.53), p < 0.001 
#> ----------------+-------------------------------
#>           Total |                               
#> 
#>   Test: Pearson's Chi-squared test  p-value = 0.001 

# Odds ratio stratified by sex, with a Mantel-Haenszel pooled estimate
tb(epitabl, renal_impairment, adjudicated_acs, strat = sex, flags = "or", ref = "No")
#>                                                  | adjudicated_acs (Stratified) 
#>  Renal impairment (eGFR below 60 mL/min/1.73 m2) | Female : No  Female : Yes 
#> -------------------------------------------------+---------------------------
#>                                               No |     362          188      
#>                                              Yes |     84            52      
#>                                            Total |     446          240      
#>                      Mantel-Haenszel pooled: Yes |                           
#> 
#>                                                  | adjudicated_acs (Stratified) 
#>  Renal impairment (eGFR below 60 mL/min/1.73 m2) | Male : No  Male : Yes 
#> -------------------------------------------------+-----------------------
#>                                               No |    379        255     
#>                                              Yes |    103         77     
#>                                            Total |    482        332     
#>                      Mantel-Haenszel pooled: Yes |                       
#> 
#>                                                  | adjudicated_acs (Stratified) 
#>  Renal impairment (eGFR below 60 mL/min/1.73 m2) | Total 
#> -------------------------------------------------+-------
#>                                               No | 1184  
#>                                              Yes |  316  
#>                                            Total | 1500  
#>                      Mantel-Haenszel pooled: Yes |       
#> 
#>                                                  |           adjudicated_acs (Stratified)            
#>  Renal impairment (eGFR below 60 mL/min/1.73 m2) |                 OR MH (95% CI)                  
#> -------------------------------------------------+-------------------------------------------------
#>                                               No |                                                 
#>                                              Yes |                                                 
#>                                            Total |                                                 
#>                      Mantel-Haenszel pooled: Yes | 1.14 (0.89 - 1.48), CMH p = 0.327, BD p = 0.788 
#> ! Methodological warning
#>   Outcome is common (22.1%); odds ratios can overstate the prevalence/risk
#>   ratio. Consider measure = 'PR' for cross-sectional tables or
#>   Poisson/log-binomial models for adjusted estimates.
#>   Run simtablr_guidance("off") separately before printing to hide advice.

# A numeric variable summarised by group
tb(epitabl, age, adjudicated_acs, test = TRUE)
#> Variable 'age' automatically treated as continuous because it is numeric. Use 'var.type' to override.
#>                                                | Adjudicated acute coronary syndrome 
#>              Age at index presentation (years) |     No           Yes     
#> -----------------------------------------------+--------------------------
#>  Age at index presentation (years) [Mean (SD)] | 60.5 (12.8)  64.4 (13.4) 
#> 
#>                                                | Adjudicated acute coronary syndrome 
#>              Age at index presentation (years) |    Total    
#> -----------------------------------------------+-------------
#>  Age at index presentation (years) [Mean (SD)] | 62.0 (13.2) 
#> 
#>   Test: Welch Two Sample t-test  p-value < 0.001 
```
