# Evaluate continuous markers with ROC curves

`roc()` measures how well one or more continuous markers discriminate a
binary outcome. It reports the area under the ROC curve (AUC) with a
confidence interval and, by default, the Youden-optimal cutpoint with
its sensitivity, specificity, and predictive values. With two or more
markers, their AUCs are compared pairwise with the DeLong test. For a
test that is already binary use
[`diag_test()`](https://MatheusTG-14.github.io/SimtablR/reference/diag_test.md).
Requires the pROC package.

## Usage

``` r
roc(
  data,
  marker,
  outcome,
  positive = NULL,
  direction = c("auto", ">", "<"),
  cutpoint = c("youden", "none"),
  conf.level = 0.95,
  d = 2,
  percent = TRUE
)
```

## Arguments

- data:

  A data frame.

- marker:

  One or more numeric marker columns: a bare name,
  [`c()`](https://rdrr.io/r/base/c.html) of names, or a tidyselect
  expression.

- outcome:

  The binary outcome column: a bare name or string.

- positive:

  The level of `outcome` that means "disease present". If `NULL`, common
  labels such as `"Yes"`, `"1"`, or `"Positive"` are detected, falling
  back to the last level.

- direction:

  String. Which marker values indicate disease: `"<"` if higher values
  do, `">"` if lower values do, or `"auto"` to let pROC choose by
  comparing the group medians.

- cutpoint:

  String. `"youden"` reports the cutpoint that maximises sensitivity +
  specificity - 1; `"none"` reports the AUC only.

- conf.level:

  Number between 0 and 1. Confidence level for AUC intervals.

- d:

  Integer. Decimal places for displayed estimates.

- percent:

  Logical. If `TRUE`, show the cutpoint's sensitivity, specificity, and
  predictive values as percentages. The AUC and the cutpoint itself are
  always shown as decimals.

## Value

A `simtab_result` of class `simtab_roc`. Print it to see the AUC table
and comparisons, convert it with
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html), or save
it with
[`export_docx()`](https://MatheusTG-14.github.io/SimtablR/reference/export_docx.md),
[`export_pptx()`](https://MatheusTG-14.github.io/SimtablR/reference/export_pptx.md),
or
[`export_xlsx()`](https://MatheusTG-14.github.io/SimtablR/reference/export_xlsx.md).
Unrounded results are stored in `$data`.

## Details

### Statistical methods

ROC curves and AUCs are computed with pROC (Robin et al., 2011), using
DeLong intervals for the AUC and the paired DeLong test to compare
markers measured on the same patients (DeLong et al., 1988). A cutpoint
chosen from the same data is optimistic: its sensitivity and specificity
will usually be lower in new patients (Ewald, 2006), and a note says so.
The AUC describes discrimination only, not calibration.

### Missing data

Rows with a missing outcome or marker value are dropped, across all
markers at once, so every marker is evaluated on the same patients. A
message reports how many rows were removed. Each marker needs both
outcome classes among the remaining rows.

### Modifying the result

Use [`fmt()`](https://MatheusTG-14.github.io/SimtablR/reference/fmt.md)
to change decimals or percentages and
[`ggplot2::autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
to draw the ROC curves. The curve coordinates and DeLong comparisons are
stored in `$data`.

## References

Robin, X., Turck, N., Hainard, A., et al. (2011). pROC: an open-source
package for R and S+ to analyze and compare ROC curves. *BMC
Bioinformatics*, 12, 77.
[doi:10.1186/1471-2105-12-77](https://doi.org/10.1186/1471-2105-12-77) .

DeLong, E. R., DeLong, D. M., & Clarke-Pearson, D. L. (1988). Comparing
the areas under two or more correlated receiver operating characteristic
curves: a nonparametric approach. *Biometrics*, 44(3), 837–845.
[doi:10.2307/2531595](https://doi.org/10.2307/2531595) .

Ewald, B. (2006). Post hoc choice of cut points introduced bias to
diagnostic research. *Journal of Clinical Epidemiology*, 59(8), 798–801.
[doi:10.1016/j.jclinepi.2005.11.025](https://doi.org/10.1016/j.jclinepi.2005.11.025)
.

## See also

[`diag_test()`](https://MatheusTG-14.github.io/SimtablR/reference/diag_test.md)
for binary tests and
[simtablr_references](https://MatheusTG-14.github.io/SimtablR/reference/simtablr_references.md)
for all references cited by SimtablR.

## Examples

``` r
if (requireNamespace("pROC", quietly = TRUE)) {
  substudy <- subset(epitabl, diagnostic_substudy == "Yes")

  # AUC and Youden cutpoint for point-of-care troponin
  roc(substudy, poc_hstn_value, adjudicated_acs)

  # Compare two markers with the paired DeLong test
  fit <- roc(substudy, c(poc_hstn_value, systolic_bp), adjudicated_acs,
             positive = "Yes")
  fit

  # AUC only, no data-driven cutpoint
  roc(substudy, poc_hstn_value, adjudicated_acs, cutpoint = "none")
}
#> Removed 21 observation(s) with missing values (2.3%).
#> Auto-detected outcome positive level: 'Yes'
#> Removed 21 observation(s) with missing values (2.3%).
#> Removed 21 observation(s) with missing values (2.3%).
#> Auto-detected outcome positive level: 'Yes'
#> 
#> ROC Curve Analysis
#> ==================
#> Outcome: adjudicated_acs (positive = 'Yes')
#> CI method: delong | Direction: auto
#> 
#>          Marker       AUC (95% CI) Direction Cutpoint Sensitivity Specificity
#>  poc_hstn_value 0.90 (0.89 - 0.92)         <                                 
#>  PPV NPV
#>         
#> ℹ Methodological guidance
#>   2 additional methodological notes hidden. Run simtablr_guidance("teaching")
#>   separately before printing to show all advice.
```
