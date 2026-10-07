# Assess the accuracy of a binary diagnostic test

`diag_test()` compares a binary index test with a binary reference
standard and reports the confusion matrix with sensitivity, specificity,
predictive values, likelihood ratios, and related metrics, each with a
confidence interval. Positive levels are detected automatically but can
be set with `positive` and `test_positive`. For a continuous marker use
[`roc()`](https://MatheusTG-14.github.io/SimtablR/reference/roc.md).

## Usage

``` r
diag_test(
  data,
  test,
  ref,
  positive = NULL,
  test_positive = NULL,
  conf.level = 0.95,
  ci = c("exact", "wilson"),
  d = 2,
  percent = TRUE
)
```

## Arguments

- data:

  A data frame.

- test:

  The index test column: a bare name or string. Must have two levels.

- ref:

  The reference standard column: a bare name or string. Must have two
  levels.

- positive:

  The level of `ref` that means "disease present". If `NULL`, common
  labels such as `"Yes"`, `"1"`, or `"Positive"` are detected, falling
  back to the last level.

- test_positive:

  The level of `test` that means "test positive". If `NULL`, `positive`
  is reused when `test` has that level; otherwise it is detected the
  same way.

- conf.level:

  Number between 0 and 1. Confidence level for intervals.

- ci:

  String. Interval method for the proportions: `"exact"`
  (Clopper-Pearson) or `"wilson"`.

- d:

  Integer. Decimal places for all displayed estimates and intervals.

- percent:

  Logical. If `TRUE`, show sensitivity, specificity, predictive values,
  accuracy, and prevalence as percentages. Ratios and indices are always
  shown as decimals.

## Value

A `simtab_result` of class `simtab_diag`. Print it to see the confusion
matrix and metrics, convert it with
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html), or save
it with
[`export_docx()`](https://MatheusTG-14.github.io/SimtablR/reference/export_docx.md),
[`export_pptx()`](https://MatheusTG-14.github.io/SimtablR/reference/export_pptx.md),
or
[`export_xlsx()`](https://MatheusTG-14.github.io/SimtablR/reference/export_xlsx.md).
Unrounded results are stored in `$data`.

## Details

### Statistical methods

Sensitivity, specificity, PPV, NPV, accuracy, and prevalence are
proportions from the confusion matrix, with Clopper-Pearson intervals by
default or Wilson intervals with `ci = "wilson"`. Predictive values and
accuracy depend on the prevalence in `data`. Likelihood ratios use the
log-method interval for a ratio of proportions (Simel et al., 1991;
Altman et al., 2000). The diagnostic odds ratio uses the Woolf logit
interval, with standard error \\\sqrt{1/TP + 1/FP + 1/FN + 1/TN}\\ (Glas
et al., 2003). No continuity correction is applied, so both intervals
are `NA` when any cell of the matrix is zero. Cohen's kappa measures
chance-corrected agreement between test and reference, with the Fleiss,
Cohen & Everitt (1969) standard error. The Youden index and F1 score are
reported without intervals.

### Missing data

Rows missing either the test or the reference are dropped, and a message
reports how many.

### Modifying the result

Use [`fmt()`](https://MatheusTG-14.github.io/SimtablR/reference/fmt.md)
to change decimals or percentages,
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) for a fourfold
display, and
[`ggplot2::autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
with `type = "matrix"` or `type = "metrics"` for a heatmap or a plot of
metrics with intervals. `as.data.frame(x, tidy = TRUE)` returns the
unrounded metrics.

## References

Clopper, C. J., & Pearson, E. S. (1934). The use of confidence or
fiducial limits illustrated in the case of the binomial. *Biometrika*,
26(4), 404–413.
[doi:10.1093/biomet/26.4.404](https://doi.org/10.1093/biomet/26.4.404) .

Simel, D. L., Samsa, G. P., & Matchar, D. B. (1991). Likelihood ratios
with confidence: sample size estimation for diagnostic test studies.
*Journal of Clinical Epidemiology*, 44(8), 763–770.
[doi:10.1016/0895-4356(91)90128-V](https://doi.org/10.1016/0895-4356%2891%2990128-V)
.

Altman, D. G., Machin, D., Bryant, T. N., & Gardner, M. J. (2000).
*Statistics with Confidence* (2nd ed.). BMJ Books.

Glas, A. S., Lijmer, J. G., Prins, M. H., Bonsel, G. J., & Bossuyt, P.
M. M. (2003). The diagnostic odds ratio: a single indicator of test
performance. *Journal of Clinical Epidemiology*, 56(11), 1129–1135.
[doi:10.1016/S0895-4356(03)00177-X](https://doi.org/10.1016/S0895-4356%2803%2900177-X)
.

Cohen, J. (1960). A coefficient of agreement for nominal scales.
*Educational and Psychological Measurement*, 20(1), 37–46.
[doi:10.1177/001316446002000104](https://doi.org/10.1177/001316446002000104)
.

Fleiss, J. L., Cohen, J., & Everitt, B. S. (1969). Large sample standard
errors of kappa and weighted kappa. *Psychological Bulletin*, 72(5),
323–327. [doi:10.1037/h0028106](https://doi.org/10.1037/h0028106) .

## See also

[`roc()`](https://MatheusTG-14.github.io/SimtablR/reference/roc.md) for
continuous markers,
[`plot.simtab_diag()`](https://MatheusTG-14.github.io/SimtablR/reference/plot.simtab_diag.md),
and
[simtablr_references](https://MatheusTG-14.github.io/SimtablR/reference/simtablr_references.md)
for all references cited by SimtablR.

## Examples

``` r
# Point-of-care troponin against adjudicated ACS
substudy <- subset(epitabl, diagnostic_substudy == "Yes")
acc <- diag_test(substudy, test = poc_hstn_positive, ref = adjudicated_acs)
#> Removed 21 observation(s) with missing values (2.3%).
#> Auto-detected reference positive level: 'Yes'
#> Auto-detected test positive level: 'Positive'
acc
#> 
#> ============================================================
#>   DIAGNOSTIC TEST EVALUATION
#> ============================================================
#> 
#>   Sample size      : 899
#>   Confidence level : 95%
#>   CI method        : exact
#> 
#>   Reference standard (gold standard):
#>     Positive = 'Yes'   |   Negative = 'No'
#> 
#>   Diagnostic test:
#>     Positive = 'Positive'   |   Negative = 'Negative'
#> 
#> ------------------------------------------------------------
#>   Confusion Matrix
#> ------------------------------------------------------------
#>           Ref
#> Test       Yes  No
#>   Positive 249  56
#>   Negative  95 499
#> 
#> ============================================================
#>   Performance Metrics  (95% CI)
#> ============================================================
#> Sensitivity           :    72.38%  (67.33% - 77.04%)
#> Specificity           :    89.91%  (87.10% - 92.29%)
#> Pos Pred Value (PPV)  :    81.64%  (76.83% - 85.82%)
#> Neg Pred Value (NPV)  :    84.01%  (80.81% - 86.86%)
#> Accuracy              :    83.20%  (80.60% - 85.59%)
#> Prevalence            :    38.26%  (35.07% - 41.53%)
#> ------------------------------------------------------------
#> Likelihood Ratio +    :      7.17  (5.55 - 9.27)
#> Likelihood Ratio -    :      0.31  (0.26 - 0.37)
#> Youden Index          :      0.62
#> F1 Score              :      0.77
#> Diagnostic Odds Ratio :     23.36  (16.24 - 33.59)
#> Cohen's Kappa         :      0.64  (0.58 - 0.69)
#> 

# Wilson intervals, decimals instead of percentages
diag_test(
  substudy, poc_hstn_positive, adjudicated_acs,
  positive = "Yes", test_positive = "Positive",
  ci = "wilson", percent = FALSE
)
#> Removed 21 observation(s) with missing values (2.3%).
#> 
#> ============================================================
#>   DIAGNOSTIC TEST EVALUATION
#> ============================================================
#> 
#>   Sample size      : 899
#>   Confidence level : 95%
#>   CI method        : wilson
#> 
#>   Reference standard (gold standard):
#>     Positive = 'Yes'   |   Negative = 'No'
#> 
#>   Diagnostic test:
#>     Positive = 'Positive'   |   Negative = 'Negative'
#> 
#> ------------------------------------------------------------
#>   Confusion Matrix
#> ------------------------------------------------------------
#>           Ref
#> Test       Yes  No
#>   Positive 249  56
#>   Negative  95 499
#> 
#> ============================================================
#>   Performance Metrics  (95% CI)
#> ============================================================
#> Sensitivity           :      0.72  (0.67 - 0.77)
#> Specificity           :      0.90  (0.87 - 0.92)
#> Pos Pred Value (PPV)  :      0.82  (0.77 - 0.86)
#> Neg Pred Value (NPV)  :      0.84  (0.81 - 0.87)
#> Accuracy              :      0.83  (0.81 - 0.86)
#> Prevalence            :      0.38  (0.35 - 0.41)
#> ------------------------------------------------------------
#> Likelihood Ratio +    :      7.17  (5.55 - 9.27)
#> Likelihood Ratio -    :      0.31  (0.26 - 0.37)
#> Youden Index          :      0.62
#> F1 Score              :      0.77
#> Diagnostic Odds Ratio :     23.36  (16.24 - 33.59)
#> Cohen's Kappa         :      0.64  (0.58 - 0.69)
#> 

# Metrics as a data frame
as.data.frame(acc)
#>                   Metric Estimate                CI
#> 1            Sensitivity   72.38% (67.33% - 77.04%)
#> 2            Specificity   89.91% (87.10% - 92.29%)
#> 3   Pos Pred Value (PPV)   81.64% (76.83% - 85.82%)
#> 4   Neg Pred Value (NPV)   84.01% (80.81% - 86.86%)
#> 5               Accuracy   83.20% (80.60% - 85.59%)
#> 6             Prevalence   38.26% (35.07% - 41.53%)
#> 7     Likelihood Ratio +     7.17     (5.55 - 9.27)
#> 8     Likelihood Ratio -     0.31     (0.26 - 0.37)
#> 9           Youden Index     0.62                  
#> 10              F1 Score     0.77                  
#> 11 Diagnostic Odds Ratio    23.36   (16.24 - 33.59)
#> 12         Cohen's Kappa     0.64     (0.58 - 0.69)
```
