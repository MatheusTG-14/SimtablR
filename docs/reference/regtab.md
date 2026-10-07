# Fit one regression model per outcome

`regtab()` fits the same set of predictors to one or more outcomes and
returns the estimates side by side in a single publication-style table,
one column per outcome. The default Poisson-log model with robust
standard errors gives rate ratios for counts and prevalence or risk
ratios for 0/1 outcomes (modified Poisson); use `family = binomial()`
for odds ratios or [`gaussian()`](https://rdrr.io/r/stats/family.html)
for mean differences. For crude estimates alongside a descriptive table
use
[`table1()`](https://MatheusTG-14.github.io/SimtablR/reference/table1.md).

## Usage

``` r
regtab(
  data,
  outcomes,
  predictors,
  family = poisson(link = "log"),
  offset = NULL,
  robust = TRUE,
  method = "glm",
  exponentiate = NULL,
  labels = NULL,
  predictor_labels = NULL,
  d = 2,
  conf.level = 0.95,
  include_intercept = FALSE,
  p_values = FALSE,
  design = NULL
)
```

## Arguments

- data:

  A data frame.

- outcomes:

  Character vector of outcome column names. Each outcome gets its own
  model with the same predictors.

- predictors:

  The right-hand side of the model, as a one-sided formula
  (`~ age + sex`) or a string (`"age * sex + smoking"`). Write
  transformations such as centring directly in the formula.

- family:

  A [`stats::family()`](https://rdrr.io/r/stats/family.html) object.
  Poisson-log needs a numeric outcome (counts or 0/1);
  [`binomial()`](https://rdrr.io/r/stats/family.html) also accepts
  two-level factors.

- offset:

  Optional person-time column: a bare name or string. With a Poisson-log
  model, `offset(log(offset))` is added so estimates become
  incidence-rate ratios.

- robust:

  Standard errors: `TRUE` for HC0 robust errors, `FALSE` for model-based
  errors, or one of `"HC0"`, `"HC1"`, `"HC2"`, `"HC3"`. Prefer `"HC3"`
  in small samples (Long & Ervin, 2000).

- method:

  String. `"glm"` fits with
  [`stats::glm()`](https://rdrr.io/r/stats/glm.html); `"firth"` fits
  Firth penalised logistic regression with the logistf package, for
  binomial-logit models only.

- exponentiate:

  Logical. Whether to report exponentiated estimates. If `NULL`, they
  are exponentiated for Poisson, binomial, and quasi- families and left
  on the original scale for Gaussian models.

- labels:

  Named character vector of display labels for outcomes, e.g.
  `c(ed_visits = "ED revisits")`.

- predictor_labels:

  Named character vector of display labels for model terms, e.g.
  `c(sexMale = "Male sex")`.

- d:

  Integer. Decimal places for estimates and intervals.

- conf.level:

  Number between 0 and 1. Confidence level for intervals.

- include_intercept:

  Logical. If `TRUE`, show the intercept row.

- p_values:

  Logical. If `TRUE`, add a p-value column for each outcome.

- design:

  String. Optional study design recorded on the result. With a
  Poisson-log model and an `offset`, it lets the estimate be labelled as
  an incidence-rate ratio.

## Value

A `simtab_result` of class `simtab_regtab`. Print it to see the
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

Each outcome is fitted with
[`stats::glm()`](https://rdrr.io/r/stats/glm.html) and the requested
family. Intervals are Wald intervals on the link scale, using HC0
sandwich standard errors by default; a Poisson-log model with robust
errors on a 0/1 outcome is the modified Poisson approach for prevalence
and risk ratios (Zou, 2004). `method = "firth"` uses
penalised-likelihood (profile) inference instead, so the HC options do
not apply.

For models with two or more predictor terms, generalized variance
inflation factors (Fox & Monette, 1992) are stored on the result. Show
them with `as.data.frame(fit, vif = TRUE)` or
`generics::glance(fit, vif = TRUE)`.

### Missing data

Rows with a missing outcome or predictor are dropped from that outcome's
model, so the N can differ between outcomes. It is shown in the table
and in
[`model_info()`](https://MatheusTG-14.github.io/SimtablR/reference/model_info.md).

### Modifying the result

[`coef()`](https://rdrr.io/r/stats/coef.html),
[`confint()`](https://rdrr.io/r/stats/confint.html),
[`vcov()`](https://rdrr.io/r/stats/vcov.html),
[`formula()`](https://rdrr.io/r/stats/formula.html), and
[`nobs()`](https://rdrr.io/r/stats/nobs.html) work on the result,
returning one entry per outcome or a single one with `outcome = "name"`.
[`model_info()`](https://MatheusTG-14.github.io/SimtablR/reference/model_info.md)
reports convergence and failed models,
[`generics::tidy()`](https://generics.r-lib.org/reference/tidy.html)
returns one row per term, and
[`ggplot2::autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
draws a forest plot.

### Limitations

The result is a reporting table, not a fitted model: it keeps no fitted
values or residuals and cannot predict. For conditional logistic
regression, prediction, or model diagnostics, fit
[`stats::glm()`](https://rdrr.io/r/stats/glm.html),
[`logistf::logistf()`](https://rdrr.io/pkg/logistf/man/logistf.html), or
[`survival::clogit()`](https://rdrr.io/pkg/survival/man/clogit.html)
directly.

## References

Zou, G. (2004). A modified Poisson regression approach to prospective
studies with binary data. *American Journal of Epidemiology*, 159(7),
702–706. [doi:10.1093/aje/kwh090](https://doi.org/10.1093/aje/kwh090) .

Firth, D. (1993). Bias reduction of maximum likelihood estimates.
*Biometrika*, 80(1), 27–38.
[doi:10.1093/biomet/80.1.27](https://doi.org/10.1093/biomet/80.1.27) .

Fox, J., & Monette, G. (1992). Generalized collinearity diagnostics.
*Journal of the American Statistical Association*, 87(417), 178–183.
[doi:10.1080/01621459.1992.10475190](https://doi.org/10.1080/01621459.1992.10475190)
.

Long, J. S., & Ervin, L. H. (2000). Using heteroscedasticity consistent
standard errors in the linear regression model. *The American
Statistician*, 54(3), 217–224.
[doi:10.1080/00031305.2000.10474549](https://doi.org/10.1080/00031305.2000.10474549)
.

## See also

[`model_info()`](https://MatheusTG-14.github.io/SimtablR/reference/model_info.md)
for convergence,
[`survtab()`](https://MatheusTG-14.github.io/SimtablR/reference/survtab.md)
for time-to-event outcomes,
[`table1()`](https://MatheusTG-14.github.io/SimtablR/reference/table1.md)
for crude and adjusted effects in a descriptive table, and
[simtablr_references](https://MatheusTG-14.github.io/SimtablR/reference/simtablr_references.md)
for all references cited by SimtablR.

## Examples

``` r
# Rate ratios for two count outcomes (Poisson, robust SEs)
fit <- regtab(
  epitabl,
  outcomes = c("ed_visits", "length_of_stay"),
  predictors = ~ age + sex + smoking
)
fit
#> Variable  Emergency department revisits during 1-year follow-up  Index hospital length of stay (days)
#> Age at …  1.00 (1.00 - 1.01)                                     1.01 (1.00 - 1.01)
#> Sex rec…  0.99 (0.89 - 1.11)                                     1.00 (0.88 - 1.14)
#> Smoking…  1.09 (0.96 - 1.24)                                     1.01 (0.87 - 1.17)
#> Smoking…  1.17 (1.01 - 1.36)                                     1.29 (1.09 - 1.53)
#> N         1500                                                   1500
#> ℹ Methodological guidance
#>   Adjusted coefficients for covariates are conditional associations, not
#>   automatically total causal effects. Footnote covariate rows or present the
#>   pre-specified exposure estimate separately.
#>   Run simtablr_guidance("off") separately before printing to hide advice.

# Odds ratios for a binary outcome, with p-values
regtab(
  epitabl, "rehospitalized", ~ age + sex + diabetes,
  family = binomial(), p_values = TRUE
)
#> Variable  Hospital readmission during 1-year follow-up  Hospital readmission during 1-year follow-up p-value
#> Age at …  0.99 (0.99 - 1.00)                            0.132
#> Sex rec…  0.98 (0.79 - 1.22)                            0.890
#> History…  1.57 (1.23 - 2.00)                            <0.001
#> N         1500
#> ℹ Methodological guidance
#>   Adjusted coefficients for covariates are conditional associations, not
#>   automatically total causal effects. Footnote covariate rows or present the
#>   pre-specified exposure estimate separately.
#>   Run simtablr_guidance("off") separately before printing to hide advice.

# One row per term, for further processing
generics::tidy(fit)
#>          outcome           term estimate  conf.low conf.high     p.value
#> 1      ed_visits            age 1.003753 0.9995735  1.007951 0.078475251
#> 2      ed_visits        sexMale 0.993393 0.8889834  1.110065 0.906860372
#> 3      ed_visits  smokingFormer 1.091323 0.9629039  1.236869 0.171263315
#> 4      ed_visits smokingCurrent 1.172102 1.0078954  1.363061 0.039199469
#> 5 length_of_stay            age 1.005659 1.0008851  1.010455 0.020102785
#> 6 length_of_stay        sexMale 1.001773 0.8786926  1.142094 0.978868858
#> 7 length_of_stay  smokingFormer 1.008002 0.8681319  1.170409 0.916710451
#> 8 length_of_stay smokingCurrent 1.289284 1.0893462  1.525918 0.003123299
```
