# Fit a Cox proportional hazards model

`survtab()` fits a Cox model to time-to-event data and returns a
publication-style table of hazard ratios with confidence intervals and
p-values. Give the follow-up time, the event indicator, and the
predictors; the proportional hazards assumption is checked
automatically. For outcomes without follow-up time use
[`regtab()`](https://MatheusTG-14.github.io/SimtablR/reference/regtab.md).

## Usage

``` r
survtab(
  data,
  time,
  event,
  predictors,
  design = NULL,
  conf.level = 0.95,
  d = 2,
  labels = NULL,
  style = "default"
)
```

## Arguments

- data:

  A data frame.

- time:

  Follow-up time column: a bare name or string.

- event:

  Event indicator column: a bare name or string. May be 0/1, logical, or
  a two-level factor whose second level is the event.

- predictors:

  The right-hand side of the model, as a one-sided formula
  (`~ age + sex`) or a character vector of column names.

- design:

  String. Optional study design recorded on the result.

- conf.level:

  Number between 0 and 1. Confidence level for intervals.

- d:

  Integer. Decimal places for hazard ratios and intervals.

- labels:

  Named character vector of display labels for model terms, e.g.
  `c(age = "Age (years)", sexMale = "Male sex")`. Variable labels
  already stored in `data` are used by default.

- style:

  String. A journal preset name (see
  [`list_journals()`](https://MatheusTG-14.github.io/SimtablR/reference/list_journals.md))
  that controls formatting such as p-values.

## Value

A `simtab_result` of class `simtab_cox`. Print it to see the formatted
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

The model is fitted with
[`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html) using
the Efron method for ties. Hazard ratios have Wald confidence intervals.
The proportional hazards assumption is tested with the Grambsch-Therneau
test on Schoenfeld residuals
([`survival::cox.zph()`](https://rdrr.io/pkg/survival/man/cox.zph.html));
when any term fails, a warning is shown with the result. Concordance and
the likelihood-ratio test p-value are available from
[`generics::glance()`](https://generics.r-lib.org/reference/glance.html).

### Missing data

Rows with a missing time, event, or predictor are dropped. The number of
rows and events analysed is shown in the table header and in
[`model_info()`](https://MatheusTG-14.github.io/SimtablR/reference/model_info.md).

### Modifying the result

[`coef()`](https://rdrr.io/r/stats/coef.html),
[`confint()`](https://rdrr.io/r/stats/confint.html),
[`vcov()`](https://rdrr.io/r/stats/vcov.html),
[`formula()`](https://rdrr.io/r/stats/formula.html), and
[`nobs()`](https://rdrr.io/r/stats/nobs.html) work on the result.
[`model_info()`](https://MatheusTG-14.github.io/SimtablR/reference/model_info.md)
reports convergence,
[`generics::tidy()`](https://generics.r-lib.org/reference/tidy.html)
returns one row per term, and
[`ggplot2::autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
draws a forest plot.

### Limitations

The result is a reporting table, not a fitted model: it keeps no
residuals or fitted values and cannot predict. For stratified or
time-varying models, survival curves, or residual diagnostics, fit
[`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html)
directly.

## References

Cox, D. R. (1972). Regression models and life-tables. *Journal of the
Royal Statistical Society, Series B*, 34(2), 187–202.
[doi:10.1111/j.2517-6161.1972.tb00899.x](https://doi.org/10.1111/j.2517-6161.1972.tb00899.x)
.

Grambsch, P. M., & Therneau, T. M. (1994). Proportional hazards tests
and diagnostics based on weighted residuals. *Biometrika*, 81(3),
515–526.
[doi:10.1093/biomet/81.3.515](https://doi.org/10.1093/biomet/81.3.515) .

## See also

[`regtab()`](https://MatheusTG-14.github.io/SimtablR/reference/regtab.md)
for outcomes without follow-up time,
[`model_info()`](https://MatheusTG-14.github.io/SimtablR/reference/model_info.md)
for convergence, and
[simtablr_references](https://MatheusTG-14.github.io/SimtablR/reference/simtablr_references.md)
for all references cited by SimtablR.

## Examples

``` r
if (requireNamespace("survival", quietly = TRUE)) {
  # Hazard ratios for major adverse cardiovascular events
  fit <- survtab(
    epitabl,
    time = mace_time_days, event = mace_event,
    predictors = ~ age + sex + diabetes
  )
  fit

  # Concordance, likelihood-ratio test, and events analysed
  generics::glance(fit)
}
#>      n events concordance         lr_p  ties
#> 1 1500    220   0.6048674 1.443295e-06 efron
```
