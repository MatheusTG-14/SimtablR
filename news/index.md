# Changelog

## SimtablR 3.2.0

### New features

- Improved methods for printing text
  [`as_methods()`](https://MatheusTG-14.github.io/SimtablR/reference/as_methods.md)
  prose reads as manuscript text. Table 1 names the variables under each
  summary (“Age and BMI were summarised as mean (SD) for approximately
  symmetric distributions; presentation hours as median \[IQR\] owing to
  skewness; and sex as counts and percentages”); categorical variables
  were previously omitted when continuous ones were present. Test lists
  became sentences naming the variables each test covered. Every engine
  now states its missing-data handling and the N analysed (for example
  “Models used complete cases … (n = 1470 of 1500)”), and the same
  sentence is recorded in
  [`repro_manifest()`](https://MatheusTG-14.github.io/SimtablR/reference/repro_manifest.md).
  [`diag_test()`](https://MatheusTG-14.github.io/SimtablR/reference/diag_test.md)
  and
  [`roc()`](https://MatheusTG-14.github.io/SimtablR/reference/roc.md)
  prose use variable labels instead of raw column names
- [`regtab()`](https://MatheusTG-14.github.io/SimtablR/reference/regtab.md)
  and
  [`survtab()`](https://MatheusTG-14.github.io/SimtablR/reference/survtab.md)
  gain `exposure =`, recorded on the specification as an `exposure`
  role; the other formula terms form the adjustment set. Reviewer tools
  no longer treat covariates as effects (the Table 2 fallacy):
  [`e_value()`](https://MatheusTG-14.github.io/SimtablR/reference/e_value.md)
  keeps only the exposure rows,
  [`sensitivity()`](https://MatheusTG-14.github.io/SimtablR/reference/sensitivity.md)
  uses the exposure as the primary estimate with its measure label (RR,
  PR, OR, IRR, HR) instead of the first term labelled “ESTIMATE”, and
  `sensitivity(adjust = NULL)` now drops the formula covariates (and any
  added with
  [`adjust()`](https://MatheusTG-14.github.io/SimtablR/reference/adjust.md))
  to give the crude estimate. The forest plot shows the exposure rows,
  the `table2_fallacy` advice names the covariate rows, and
  [`why()`](https://MatheusTG-14.github.io/SimtablR/reference/why.md),
  [`as_methods()`](https://MatheusTG-14.github.io/SimtablR/reference/as_methods.md)
  and
  [`repro_manifest()`](https://MatheusTG-14.github.io/SimtablR/reference/repro_manifest.md)
  record the exposure and adjustment set. Without an exposure,
  [`e_value()`](https://MatheusTG-14.github.io/SimtablR/reference/e_value.md)
  returns every term with a warning and
  [`sensitivity()`](https://MatheusTG-14.github.io/SimtablR/reference/sensitivity.md)
  errors instead of picking a covariate.
- The engine contract gains an optional `decisions` renderer (see
  [`register_engine()`](https://MatheusTG-14.github.io/SimtablR/reference/register_engine.md)):
  it returns the recorded estimator, interval method, tests,
  corrections, and Suggests packages of a result.
  [`as_methods()`](https://MatheusTG-14.github.io/SimtablR/reference/as_methods.md)
  and
  [`repro_manifest()`](https://MatheusTG-14.github.io/SimtablR/reference/repro_manifest.md)
  both read it.

### Bug fixes

- [`why()`](https://MatheusTG-14.github.io/SimtablR/reference/why.md) no
  longer prints twice. It printed inside the function and then returned
  the object, so `print(why(x))` (or any visible call in a script)
  showed the explanation twice; assigning it also printed. It now
  returns a `simtab_explanation` that is a character vector of lines,
  printed once by auto-print, so `cat(why(x), sep = "\n")` and
  `as.character(why(x))` work. Code that read the former `$lines` field
  should use [`as.character()`](https://rdrr.io/r/base/character.html).
- [`diag_test()`](https://MatheusTG-14.github.io/SimtablR/reference/diag_test.md)
  prints in the same layout as the other results (titled header,
  variable labels, confusion matrix, metric table, advice) and says
  “Reference standard” instead of “Reference standard (gold standard)”.
- `tb(..., miss)` no longer adds an empty missing-value row or column:
  they appear only where values are missing (an `NA` outcome column
  without an all-zero `NA` exposure row).
- The “No reference level specified” note of
  [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md) now
  appears only when the default first level is not a conventional
  reference (“No”, “0”, “Never”, “Negative”, “None”, “Absent”, “FALSE”,
  “Unexposed”, “Control”), and names the measure.
- [`strobe()`](https://MatheusTG-14.github.io/SimtablR/reference/strobe.md)
  and
  [`stard()`](https://MatheusTG-14.github.io/SimtablR/reference/stard.md)
  print one line per item, shortening the pointer to the console width
  instead of wrapping it into a separate block.
- Printing a
  [`simtablr()`](https://MatheusTG-14.github.io/SimtablR/reference/simtablr.md)
  report shows each table’s advice under that table, so Table 1’s advice
  no longer appears to describe Table 2.
- A descriptive Table 1 (no effect measure) with several p-values no
  longer gets the Holm/BH multiplicity advice. A new
  `descriptive_table_tests` rule (severity 1) notes that such p-values
  do not test pre-specified hypotheses, citing the STROBE explanation
  and elaboration (Vandenbroucke et al., 2007), and suggests
  standardized mean differences via `test(smd = TRUE)`.
  `multiplicity_unadjusted` still applies to association tables. Tests
  in Table 1 remain available with `test = TRUE`.
- Vignettes no longer treat adjudicated acute coronary syndrome, which
  is diagnosed at the index presentation, as a cohort outcome with a
  risk ratio. “Getting started” now shows a cross-sectional prevalence
  ratio for ACS and a cohort risk ratio for 365-day MACE; the reporting
  and regression vignettes use `mace_event` for their cohort examples.
  The former text also claimed a significantly higher ACS incidence with
  renal impairment, which the data do not show (PR 1.09, 95% CI 0.94 to
  1.27).
- Tests no longer require bit-identical estimates from two independent
  fits of the same GLM. CRAN’s MKL check flagged `test-style.R` because
  an optimised BLAS can differ in the last bit between refits; these
  comparisons now use a numerical tolerance. Comparisons where the
  evidence is copied rather than refitted
  ([`style()`](https://MatheusTG-14.github.io/SimtablR/reference/style.md),
  [`label()`](https://MatheusTG-14.github.io/SimtablR/reference/label.md),
  [`fmt()`](https://MatheusTG-14.github.io/SimtablR/reference/fmt.md) on
  a result) stay exact.
- Stratified
  [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md)
  tables now show the stratum-specific estimates (“Stratum Female: Yes”,
  “Stratum Male: Yes”) above the Mantel-Haenszel pooled row; they were
  computed in `$data$mh` but the effect panel showed only the pooled
  line. Row and cell percentages now use each stratum as the
  denominator. The `row` percentage previously divided by the row total
  across all strata (72/1184 = 6.1% for “Female: Yes”), which matches no
  epidemiological quantity; it is now the risk within the stratum
  (72/550 = 13.1%), consistent with the stratum RR shown beside it.
  Column percentages were already within stratum and are unchanged.
- [`regtab()`](https://MatheusTG-14.github.io/SimtablR/reference/regtab.md)
  no longer prints a separated coefficient as an estimate. Under
  separation a binomial or Poisson coefficient diverges, and the table
  showed a number such as `0.00 (0.00 - 0.00)` with a falsely narrow
  robust interval. A coefficient whose link-scale model-based standard
  error exceeds 100 is now flagged in `meta$not_estimable`;
  [`print()`](https://rdrr.io/r/base/print.html),
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html),
  [`as_gt()`](https://MatheusTG-14.github.io/SimtablR/reference/as_gt.md)
  and `as_flextable()` show a dash with a footnote pointing to
  `method = "firth"`, and the forest plot and
  [`e_value()`](https://MatheusTG-14.github.io/SimtablR/reference/e_value.md)
  skip it. The raw estimate stays in `$data`.
- [`regtab()`](https://MatheusTG-14.github.io/SimtablR/reference/regtab.md)
  now uses the declared `design` beyond IRR labelling. Each outcome
  column in [`print()`](https://rdrr.io/r/base/print.html),
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html),
  [`as_gt()`](https://MatheusTG-14.github.io/SimtablR/reference/as_gt.md)
  and `as_flextable()` names its measure, resolved from link and design
  as in the forest plot (for example “Adjudicated acute coronary
  syndrome, PR (95% CI)”). A logistic model with a cohort or
  cross-sectional design and an outcome above 10% now fires the
  `or_as_rr_common_outcome` advice.
  [`why()`](https://MatheusTG-14.github.io/SimtablR/reference/why.md)
  reports the measure, its source (family, link and design) and, under a
  log link, convergence;
  [`as_methods()`](https://MatheusTG-14.github.io/SimtablR/reference/as_methods.md)
  names the measure, the design and, under a log link, whether the
  models converged. The family is never switched; evidence is unchanged.
- [`repro_manifest()`](https://MatheusTG-14.github.io/SimtablR/reference/repro_manifest.md)
  now records the same decisions that
  [`as_methods()`](https://MatheusTG-14.github.io/SimtablR/reference/as_methods.md)
  writes, so `estimator`, `interval_method`, `tests`, and any
  Haldane-Anscombe zero-cell correction are filled for every engine
  instead of `NULL` (for example the Katz log prevalence-ratio estimator
  of a cross-sectional
  [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md)).
  The prose now also names what it previously omitted: the
  Mantel-Haenszel pooled estimator, Greenland-Robins (or
  Robins-Breslow-Greenland) interval and homogeneity test of a
  stratified
  [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md), the
  interval of each crude and adjusted estimate, zero-cell corrections,
  and the Cox ties method. `packages` now lists SimtablR, its Imports,
  and only the Suggests the result’s engine used (pROC, survival,
  logistf), instead of every package loaded in the session.
- [`regtab()`](https://MatheusTG-14.github.io/SimtablR/reference/regtab.md)
  with the default Poisson-log family now accepts a two-level factor,
  character, or logical outcome instead of erroring and pointing to
  [`binomial()`](https://rdrr.io/r/stats/family.html). The outcome is
  coded 1 for the second level, the same convention
  [`simtablr()`](https://MatheusTG-14.github.io/SimtablR/reference/simtablr.md)
  and [`binomial()`](https://rdrr.io/r/stats/family.html) use, with a
  note; the coding is recorded in `meta$outcome_coding`. Estimates equal
  those of the corresponding 0/1 outcome (modified Poisson).
- [`e_value()`](https://MatheusTG-14.github.io/SimtablR/reference/e_value.md)
  now accepts prevalence ratios, which previously failed with “`measure`
  must be one of RR, OR, or HR” and broke the cross-sectional workflow
  (`e_value(report$table2)` on a `simtablr(design = "cross_sectional")`
  report, or a PR
  [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md)). A
  prevalence ratio is a ratio of proportions, so the risk-ratio formula
  applies (VanderWeele & Ding, 2017); the result keeps the “PR” label
  and
  [`as_methods()`](https://MatheusTG-14.github.io/SimtablR/reference/as_methods.md)
  says so. For
  [`regtab()`](https://MatheusTG-14.github.io/SimtablR/reference/regtab.md),
  the measure now follows the table (link and design): modified Poisson,
  the
  [`regtab()`](https://MatheusTG-14.github.io/SimtablR/reference/regtab.md)
  default, is accepted as PR or RR instead of being rejected, and a
  log-binomial model under a cross-sectional design reads PR instead of
  RR. Count models (IRR) are rejected with an error that names the
  outcome.
  [`as_methods()`](https://MatheusTG-14.github.io/SimtablR/reference/as_methods.md)
  and
  [`repro_manifest()`](https://MatheusTG-14.github.io/SimtablR/reference/repro_manifest.md)
  record the scale conversion used for each measure (PR, OR, HR).

## SimtablR 3.1.1

CRAN release: 2026-10-10

### Bug fixes

- Journal styles now reach the bivariate
  ([`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md))
  and GLM
  ([`regtab()`](https://MatheusTG-14.github.io/SimtablR/reference/regtab.md))
  renderers. Previously `style(result, "lancet")` or a
  [`journal_style()`](https://MatheusTG-14.github.io/SimtablR/reference/journal_style.md)
  object left estimate/CI text unchanged (for example
  `1.94 (1.51 - 2.49)` instead of `1.94 (1.51, 2.49)`).
- [`survtab()`](https://MatheusTG-14.github.io/SimtablR/reference/survtab.md)
  results now honour journal styles for the hazard-ratio interval
  (separator, brackets, digits); the unstyled output is unchanged.
- [`style()`](https://MatheusTG-14.github.io/SimtablR/reference/style.md)
  now works on a
  [`simtablr()`](https://MatheusTG-14.github.io/SimtablR/reference/simtablr.md)
  report and restyles every table in it; it previously failed with “no
  applicable method”.
- [`stratify()`](https://MatheusTG-14.github.io/SimtablR/reference/stratify.md)
  on a [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md)
  result now stratifies the table (Mantel-Haenszel pooling included),
  matching `tb(..., strat = )`. Previously the verb recorded the
  stratifier but recomputed an unstratified table.
- [`adjust()`](https://MatheusTG-14.github.io/SimtablR/reference/adjust.md)
  on a
  [`regtab()`](https://MatheusTG-14.github.io/SimtablR/reference/regtab.md)
  or
  [`survtab()`](https://MatheusTG-14.github.io/SimtablR/reference/survtab.md)
  result now refits the models with the added covariates. Previously the
  covariates were recorded but silently left out of the model, so the
  “adjusted” estimates were unchanged. This also makes
  `sensitivity(x, adjust = NULL)` meaningful for covariates added this
  way.
- [`label()`](https://MatheusTG-14.github.io/SimtablR/reference/label.md)
  on a
  [`regtab()`](https://MatheusTG-14.github.io/SimtablR/reference/regtab.md)
  result now relabels predictor rows; previously only outcome labels
  changed.
- [`measure()`](https://MatheusTG-14.github.io/SimtablR/reference/measure.md)
  on a computed result keeps the recorded reference level and confidence
  level unless new ones are supplied. Previously it reset them to the
  first factor level and 95%, which could silently invert the
  comparison.
- [`e_value()`](https://MatheusTG-14.github.io/SimtablR/reference/e_value.md)
  on a [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md)
  result no longer reports an E-value for the reference row, and labels
  each row with its level (e.g. `smoking: Former`) so multi-level
  variables can be told apart.
- Stratified
  [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md)
  tables use the column variable’s label (not its raw column name) in
  the “(Stratified)” header.
- A stratified
  [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md) now
  tests the exposure-outcome association with the
  Cochran-Mantel-Haenszel test, as documented. Without an effect measure
  it previously ran a Pearson chi-squared test on the stratum-by-outcome
  columns, testing the exposure against stratum and outcome jointly;
  with a multi-level exposure the table-level test was the first level’s
  2x2 CMH test instead of the generalized CMH test. 2x2 results are
  unchanged.
- [`register_engine()`](https://MatheusTG-14.github.io/SimtablR/reference/register_engine.md)
  no longer silently replaces a built-in engine (`descriptive`,
  `bivariate`, `glm`, `accuracy`, `roc`, `cox`, `e_value`); doing so is
  a classed error unless `overwrite = TRUE`. Replacing `"descriptive"`
  previously broke
  [`table1()`](https://MatheusTG-14.github.io/SimtablR/reference/table1.md)
  for the rest of the session. Re-registering your own engines is
  unchanged.
- `table1(na_model = "explicit")` now shows the adjusted estimate for
  the “(Missing)” category in the Missing row; it was computed but never
  displayed. A table note states that missing values were modelled as
  their own category.
- [`table1()`](https://MatheusTG-14.github.io/SimtablR/reference/table1.md)
  now notes when the Overall column includes participants whose `by`
  value is missing (e.g. “Overall includes 50 participants with missing
  sex”), so Overall no longer silently exceeds the sum of the groups.
  The note appears in print,
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)
  (attribute `"notes"`),
  [`as_gt()`](https://MatheusTG-14.github.io/SimtablR/reference/as_gt.md),
  and `as_flextable()`; the numbers are unchanged.
- Evidence verbs that an engine does not use now warn and return the
  result unchanged instead of being silently accepted: for example
  [`measure()`](https://MatheusTG-14.github.io/SimtablR/reference/measure.md)
  or
  [`test()`](https://MatheusTG-14.github.io/SimtablR/reference/test.md)
  on a
  [`diag_test()`](https://MatheusTG-14.github.io/SimtablR/reference/diag_test.md),
  [`roc()`](https://MatheusTG-14.github.io/SimtablR/reference/roc.md),
  or
  [`regtab()`](https://MatheusTG-14.github.io/SimtablR/reference/regtab.md)
  result, or
  [`adjust()`](https://MatheusTG-14.github.io/SimtablR/reference/adjust.md)
  on a [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md)
  result.
  [`label()`](https://MatheusTG-14.github.io/SimtablR/reference/label.md),
  [`fmt()`](https://MatheusTG-14.github.io/SimtablR/reference/fmt.md),
  and
  [`style()`](https://MatheusTG-14.github.io/SimtablR/reference/style.md)
  still apply everywhere.
  [`register_engine()`](https://MatheusTG-14.github.io/SimtablR/reference/register_engine.md)
  gains `verbs` to declare which of
  [`stratify()`](https://MatheusTG-14.github.io/SimtablR/reference/stratify.md),
  [`adjust()`](https://MatheusTG-14.github.io/SimtablR/reference/adjust.md),
  [`measure()`](https://MatheusTG-14.github.io/SimtablR/reference/measure.md),
  [`test()`](https://MatheusTG-14.github.io/SimtablR/reference/test.md),
  [`set_summary()`](https://MatheusTG-14.github.io/SimtablR/reference/set_summary.md),
  and
  [`missingness()`](https://MatheusTG-14.github.io/SimtablR/reference/missingness.md)
  an engine uses (`NULL`, the default, accepts all).
- [`stratify()`](https://MatheusTG-14.github.io/SimtablR/reference/stratify.md)
  on a
  [`survtab()`](https://MatheusTG-14.github.io/SimtablR/reference/survtab.md)
  result now fits a stratified Cox model
  ([`survival::strata()`](https://rdrr.io/pkg/survival/man/strata.html),
  separate baseline hazards); it previously recorded the stratifier and
  refitted the same unstratified model. A stratifier that is also a
  predictor is removed from the linear predictor.
- Continuous standardized mean differences now use the unweighted
  average of the group variances (Austin, 2009), the same convention as
  categorical SMDs, and are reported as absolute values. They previously
  used the sample-size-weighted pooled SD and could be negative, so
  values differ when group sizes are unequal (e.g. 0.339 vs 0.347 for
  1280 vs 220).
- `table1(d =)` now also sets the decimals of continuous summaries, as
  documented, when supplied directly or through `fmt(d = )`. `d`
  defaults to `NULL`, which keeps the previous output (one-decimal
  percentages, continuous digits from the journal style).
- Tidy categorical
  [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md)
  output attaches each ratio only to the event-outcome row of its level
  (other rows are `NA`); it previously repeated the ratio on the
  non-event rows. Stratified tables gain a `stratum` column with the
  stratum-specific ratios and one row per pooled Mantel-Haenszel
  estimate.
- Numeric haven/labelled columns with only some values labelled (for
  example ages with `999 = "Unknown"`) now stay numeric, with a message,
  instead of becoming a factor with one level per value. Fully labelled
  columns are still converted to factors.
- A stratified numeric
  [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md) now
  compares groups within strata: the F-test for the group term in a
  linear model with stratum (mean summaries), or the van Elteren
  stratified Wilcoxon test for two groups (median summaries). It
  previously ran a one-way ANOVA or Kruskal-Wallis test across the
  stratum-by-group cells. Where no stratified test applies (a rank test
  with more than two groups, or paired data) no p-value is reported and
  a warning says why.
- Stratified PR/RR tables now test homogeneity of the ratio with
  Cochran’s Q on the stratum log ratios (printed as “homogeneity p”).
  They previously reported the Breslow-Day test, which tests homogeneity
  of odds ratios, as “BD p”. Odds-ratio tables still use Breslow-Day.
  The test name is stored in the new `homogeneity_method` column of
  `$data$mh`.
- Interval separators that would be misread are replaced for that
  interval only: a comma separator becomes `"; "` when a bound contains
  a comma (`1,94 (1,51; 2,49)` rather than `1,94 (1,51, 2,49)`), and a
  dash separator becomes `" to "` when a bound is negative
  (`1.38 (-0.58 to 3.34)` rather than `1.38 (-0.58 - 3.34)`).
  Unambiguous output is unchanged.
- [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md) no
  longer reports a “Chi-squared test for given probabilities” when the
  complete-case table has a single observed row or column level. That
  goodness-of-fit test is not a test of association; no p-value is
  reported and a warning explains why.
- [`regtab()`](https://MatheusTG-14.github.io/SimtablR/reference/regtab.md)
  now marks an outcome that takes a single value among complete cases as
  failed in
  [`model_info()`](https://MatheusTG-14.github.io/SimtablR/reference/model_info.md)
  and the “Failed outcomes” footer, instead of warning about
  non-convergence and reporting `1.00 (1.00 - 1.00)`.
- [`as_methods()`](https://MatheusTG-14.github.io/SimtablR/reference/as_methods.md)
  on a
  [`table1()`](https://MatheusTG-14.github.io/SimtablR/reference/table1.md)
  result with an adjusted column now describes both estimands and names
  the adjustment covariates. An adjusted odds ratio was previously
  described as the crude Woolf estimator, with no mention of logistic
  regression, and adjusted PR/RR prose omitted the covariates.
- `table1(ref = list(sex = "Female", smoking = "Never"))` and
  `measure(spec, ref = list(...))` now work as documented; a
  per-variable reference list previously failed with “`ref` must be one
  non-missing reference value”. A reference level that is never applied
  (not a level of any categorical variable, or of the named variable)
  now warns instead of silently falling back to the first level.
- [`test()`](https://MatheusTG-14.github.io/SimtablR/reference/test.md)
  on a [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md)
  result honours `"t"`, `"wilcoxon"`, `"anova"`, and `"kruskal"` for
  numeric variables, and a test that does not fit the variable type
  (e.g. `"fisher"` on a numeric variable, `"t"` on a categorical one) is
  now a classed error, as in
  [`table1()`](https://MatheusTG-14.github.io/SimtablR/reference/table1.md).
  Previously the request was silently replaced by the summary-driven
  default or dropped.
- `test(x, smd = TRUE)` on a
  [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md)
  result now warns that
  [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md)
  tables do not report SMDs instead of silently ignoring the request.
- `sensitivity(x, adjust = NULL)` warns when the result records no
  adjustment covariates to drop (for example a
  [`regtab()`](https://MatheusTG-14.github.io/SimtablR/reference/regtab.md)
  whose covariates are in the model formula), instead of reporting the
  unchanged estimate as “materially unchanged”.
- [`e_value()`](https://MatheusTG-14.github.io/SimtablR/reference/e_value.md),
  [`sensitivity()`](https://MatheusTG-14.github.io/SimtablR/reference/sensitivity.md),
  and `autoplot()` on a stratified
  [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md)
  result now use its Mantel-Haenszel estimates;
  [`e_value()`](https://MatheusTG-14.github.io/SimtablR/reference/e_value.md)
  and `autoplot()` previously failed with “No ratio-scale estimates” /
  “No effect estimates”.
- [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md)
  forest plots draw each ratio once. They previously repeated every
  estimate in a panel for each outcome level, including the non-event
  level.
- [`style()`](https://MatheusTG-14.github.io/SimtablR/reference/style.md)
  now works on an [`rbind()`](https://rdrr.io/r/base/cbind.html) of
  [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md)
  results; it previously failed with “no applicable method”.
- [`label()`](https://MatheusTG-14.github.io/SimtablR/reference/label.md)
  on a stratified
  [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md)
  result now updates the “(Stratified)” column header, and
  [`label()`](https://MatheusTG-14.github.io/SimtablR/reference/label.md)
  on a
  [`roc()`](https://MatheusTG-14.github.io/SimtablR/reference/roc.md)
  result relabels markers, the outcome, and the DeLong comparison lines.
- `as.data.frame(x, tidy = TRUE)` on a categorical
  [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md)
  result (and on stacked tables) gains an `n` column with the cell
  count.
- Stratified
  [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md)
  printing shows the Mantel-Haenszel row once, in the effect panel,
  rather than as an empty row in every frequency panel, and draws the
  rule above the Total row.
- Values that round to zero no longer print with a minus sign (for
  example a degenerate
  [`diag_test()`](https://MatheusTG-14.github.io/SimtablR/reference/diag_test.md)
  kappa interval `(-0.00 - 0.00)`). Raw evidence is unchanged.
- [`register_engine()`](https://MatheusTG-14.github.io/SimtablR/reference/register_engine.md)
  now rejects renderers that are not functions with a classed error at
  registration, instead of failing later with “could not find function”.
- [`export_docx()`](https://MatheusTG-14.github.io/SimtablR/reference/export_docx.md),
  [`export_pptx()`](https://MatheusTG-14.github.io/SimtablR/reference/export_pptx.md),
  and
  [`export_xlsx()`](https://MatheusTG-14.github.io/SimtablR/reference/export_xlsx.md)
  reject inputs that are neither SimtablR objects nor data frames;
  `export_xlsx(NULL, path)` previously wrote an empty workbook.
- New advice rule `ratio_in_case_control` flags a risk or prevalence
  ratio reported for a case-control design. The explicit measure is
  still used; the advice is non-blocking.

## SimtablR 3.1.0

### New features

- [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md)
  gains `big_mark` and `decimal_mark` to control how numbers are written
  in the rendered table (print,
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html),
  [`as_gt()`](https://MatheusTG-14.github.io/SimtablR/reference/as_gt.md),
  `as_flextable()`). For example `tb(data, x, y, big_mark = ",")`
  renders `4391.2` as `4,391.2`, and
  `big_mark = ".", decimal_mark = ","` gives the `4.391,2` convention
  used in many locales. Defaults (`""` / `"."`) reproduce the previous
  output exactly. Both are also reachable on a computed result through
  `fmt(result, big_mark = ..., decimal_mark = ...)`; raw evidence in
  `$data` is never reformatted.
- [`diag_test()`](https://MatheusTG-14.github.io/SimtablR/reference/diag_test.md)
  gains `d` (decimal places for the rendered table) and `percent` (show
  sensitivity, specificity, predictive values, accuracy, and prevalence
  as percentages).
  [`roc()`](https://MatheusTG-14.github.io/SimtablR/reference/roc.md)
  gains the same `d` and `percent` switches for its cutpoint proportion
  columns. `percent` defaults to `TRUE` and `d` to `2`; pass
  `percent = FALSE` for decimal proportions. Both are also reachable on
  a computed result through `fmt(result, d = ..., percent = ...)`; raw
  evidence in `$data` is never rounded or scaled.
- Likelihood ratios from
  [`diag_test()`](https://MatheusTG-14.github.io/SimtablR/reference/diag_test.md)
  now carry asymptotic log-method confidence intervals (Simel et
  al. 1991; Altman 2000), computed from the Katz ratio-of-proportions
  standard error. The interval is `NA` when any confusion-matrix cell is
  zero; the point estimate is unchanged.
- [`diag_test()`](https://MatheusTG-14.github.io/SimtablR/reference/diag_test.md)
  results add Cohen’s kappa for index-test / reference-standard
  agreement, with a Fleiss, Cohen & Everitt (1969) large-sample
  confidence interval.

#### Earlier 3.1 changes

- Removed `km()` and the Kaplan-Meier engine. Use
  [`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html)
  directly;
  [`survtab()`](https://MatheusTG-14.github.io/SimtablR/reference/survtab.md)
  still covers Cox models.

- Removed `stdtab()`/`standardize()` (marginal standardization),
  `additive_interaction()`, and `subgroups()`.

- Removed `sampling_weights()` and the `weights` argument of
  [`table1()`](https://MatheusTG-14.github.io/SimtablR/reference/table1.md)
  and [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md).
  Use the `survey` package directly for weighted analyses.

- Removed `regtab(method = "clogit")` and its `strata` argument. Fit
  matched case-control models with
  [`survival::clogit()`](https://rdrr.io/pkg/survival/man/clogit.html).

- Removed the recode helpers `bin()`, `lump_levels()`,
  `make_missing_explicit()`, and `set_reference()`. Use base R or
  `forcats` before analysis; reference levels can also be set with
  `ref =`.

- Removed `patch()`, `add_pvalue()`, `plot_export_dim()`, and
  `simtab_report_skeleton()`; `simtab_theme()` is now internal. Use
  [`test()`](https://MatheusTG-14.github.io/SimtablR/reference/test.md)
  on a result instead of `add_pvalue()`.

- `check()` and `audit()` are replaced by
  `advise(result, audit = TRUE)`.

- [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md)
  renames `stat.cont` to `summary` (matching
  [`table1()`](https://MatheusTG-14.github.io/SimtablR/reference/table1.md))
  and drops `fast`, `format`, `paired`, and `p.adjust`.
  [`table1()`](https://MatheusTG-14.github.io/SimtablR/reference/table1.md)
  drops `p.adjust`, `paired`, and `smd`. Set those options on the result
  with
  [`test()`](https://MatheusTG-14.github.io/SimtablR/reference/test.md),
  e.g. `table1(...) |> test(p.adjust = "holm", smd = TRUE)`. When
  [`test()`](https://MatheusTG-14.github.io/SimtablR/reference/test.md)
  is given only these options, it keeps the existing test choice.

- [`roc()`](https://MatheusTG-14.github.io/SimtablR/reference/roc.md)
  drops the `ci` argument (DeLong intervals only) and the
  `"closest.topleft"` cutpoint.

- [`export_xlsx()`](https://MatheusTG-14.github.io/SimtablR/reference/export_xlsx.md)
  drops `sheet`, `machine_sheet`, and `dict_sheet`; sheets are always
  named `machine-readable`, `display`, and `data-dictionary`.

- Study designs accept only their canonical names (`cross_sectional`,
  `cohort`, `case_control`, `cohort_person_time`,
  `cohort_time_to_event`), with spaces or hyphens in place of
  underscores. Synonyms such as `"prevalence"`, `"risk"`, `"rate"`, and
  `"survival"` are rejected.

- `register_measure()`, `register_rule()`, `list_measures()`, and
  `list_rules()` are now internal. The public extension API is
  [`register_engine()`](https://MatheusTG-14.github.io/SimtablR/reference/register_engine.md),
  [`register_journal()`](https://MatheusTG-14.github.io/SimtablR/reference/register_journal.md),
  [`list_engines()`](https://MatheusTG-14.github.io/SimtablR/reference/list_engines.md),
  and
  [`list_journals()`](https://MatheusTG-14.github.io/SimtablR/reference/list_journals.md).

- The advice rule set shrinks from 46 to 37 rules. Rules for removed
  features are gone, `fisher_low_expected` is folded into
  `small_expected_cells`, and `normality_heuristic_large_n` into
  `continuous_test_stat_choice`. The ruleset version is now
  `downscale-2026-09-23`.

- Removed the `migration-3-0` and `simtablr-for-spss-stata-users`
  vignettes; a short SPSS/Stata orientation now lives in the
  getting-started guide.

- Removed `register_test()` and `list_tests()`. The comparison-test
  registry only stored functions; no engine ever dispatched to a
  registered test. Built-in tests are selected with
  [`test()`](https://MatheusTG-14.github.io/SimtablR/reference/test.md)
  / `test =` as before.

- Stratified
  [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md)
  PR/OR no longer prints the one-off “additive change” message about
  Mantel-Haenszel pooling.

- Renamed two exported verbs that masked **dplyr** generics when both
  packages were attached. `compute()` is now
  [`evaluate()`](https://MatheusTG-14.github.io/SimtablR/reference/evaluate.md)
  and `explain()` is now
  [`why()`](https://MatheusTG-14.github.io/SimtablR/reference/why.md);
  the S3 methods move with them (`why.simtab_spec()`,
  `why.simtab_result()`, `why.simtab_report()`). Behaviour is unchanged.
  No compatibility aliases are provided, so attaching SimtablR no longer
  masks any dplyr export. The engine contract keeps its `compute` field:
  `register_engine(compute = ...)` is unaffected.

- **Named change.** Adjusted PR/RR on data that previously reached the
  modified Poisson fallback may now report a log-binomial estimate
  instead, and the number changes. **Old behaviour:**
  [`stats::glm()`](https://rdrr.io/r/stats/glm.html) has no usable
  default initialization for a log link on a binomial family; it aborts
  before the first iteration with “no valid set of coefficients has been
  found”. SimtablR recorded that as a convergence failure and
  substituted modified Poisson. **New behaviour:** when, and only when,
  the unseeded fit produces no model at all, the log-binomial is retried
  with the conventional starting values (intercept at the marginal log
  risk, slopes at zero) and its estimate is reported if it converges. On
  `epitabl`, the adjusted renal-impairment RR for 365-day MACE moves
  from 1.825 (robust Poisson) to 1.771 (log-binomial), matching a
  hand-fitted [`stats::glm()`](https://rdrr.io/r/stats/glm.html).
  **Scope:** a model that already fitted keeps its exact numerical path
  and its estimates are unchanged; the retry fires only where the
  previous result came from an unrecoverable fit.

- `flextable` and `openxlsx` moved from Imports to Suggests, and `dplyr`
  was removed from Imports (it is retained only as a test-only Suggests,
  for the regression test that keeps the `summarise()` unmasking
  guarantee honest). This cuts the recursive non-base dependency
  footprint from 62 packages to 12. Every affected entry point already
  checked for its backend at runtime and continues to raise the classed
  `simtab_error_dependency` with installation guidance, so
  `as_flextable()`, `simtab_theme()`,
  [`export_docx()`](https://MatheusTG-14.github.io/SimtablR/reference/export_docx.md),
  [`export_pptx()`](https://MatheusTG-14.github.io/SimtablR/reference/export_pptx.md),
  [`export_xlsx()`](https://MatheusTG-14.github.io/SimtablR/reference/export_xlsx.md),
  [`export_regtab_xlsx()`](https://MatheusTG-14.github.io/SimtablR/reference/export_regtab_xlsx.md),
  and the journal-style flextable themes now require an explicit
  [`install.packages()`](https://rdrr.io/r/utils/install.packages.html)
  on a minimal installation. Computing, printing,
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html),
  `tidy()`, `glance()`,
  [`as_gt()`](https://MatheusTG-14.github.io/SimtablR/reference/as_gt.md),
  methods prose, and reproducibility manifests are unaffected. `dplyr`
  was imported only for a `%>%` that no code in the package used and
  that was never re-exported, so its removal has no user-visible effect.

### Compliance remediation and release infrastructure

- Diagnostic LR+ and LR- now retain mathematical `Inf` when a positive
  numerator is divided by zero; an indeterminate zero-over-zero remains
  `NA`. Raw evidence stays numeric, while renderers and machine exports
  handle the boundary at the presentation edge.
- Every public file writer now adds an omitted suffix, rejects an
  incompatible suffix, protects an existing destination by default, and
  replaces it only with `overwrite = TRUE` through a same-directory
  transactional write.
- Regression and Cox results implement
  [`coef()`](https://rdrr.io/r/stats/coef.html),
  [`confint()`](https://rdrr.io/r/stats/confint.html),
  [`formula()`](https://rdrr.io/r/stats/formula.html),
  [`nobs()`](https://rdrr.io/r/stats/nobs.html), and
  [`vcov()`](https://rdrr.io/r/stats/vcov.html) over copied model
  evidence.
  [`model_info()`](https://MatheusTG-14.github.io/SimtablR/reference/model_info.md)
  reports convergence and per-outcome failures without exposing mutable
  fitted models.
- Added deterministic edge-case tests, opt-in multi-seed and scaling
  tests via `SIMTABLR_EXTENDED_TESTS=true`, unmodified-project coverage
  reporting, and an executable documentation contract. Advice rules are
  split into table, regression, ROC, data-quality, and reviewer families
  without changing their IDs, order, wording, deduplication, or
  non-blocking behavior.
- Added contributor guidance, a Code of Conduct, package citation
  metadata, statistical scope and lifecycle statements, and expanded
  references, assumptions, edge behavior, interpretation, and runnable
  examples for the major analysis and grammar interfaces.
- ROC plots now distinguish groups with both a discrete viridis palette
  and line type, improving accessibility without changing their stored
  numerical evidence.

### Correctness and honest output

- A log-binomial model that could not be fitted and one that was fitted
  and failed to converge are now reported as the different findings they
  are. Advice and
  [`as_methods()`](https://MatheusTG-14.github.io/SimtablR/reference/as_methods.md)
  prose say “the log-binomial model could not be fitted” for the first
  and keep “did not converge” for the second, instead of describing
  every fallback as non-convergence. The distinction is recorded in
  result metadata as `logbinomial_status`.

- Naming a built-in measure that a descriptive or bivariate table cannot
  compute now names the function that can: `"hr"` points to
  [`survtab()`](https://MatheusTG-14.github.io/SimtablR/reference/survtab.md)
  and `"auc"` to
  [`roc()`](https://MatheusTG-14.github.io/SimtablR/reference/roc.md).
  The previous advice — “register the measure with a supported
  estimator” — was not actionable, because all of these measures are
  already registered.

- The advice raised when a declared `cohort_person_time` or
  `cohort_time_to_event` design resolves no effect measure now names the
  engine and entry point that would estimate it, and no longer describes
  the survival engine as unavailable.
  [`survtab()`](https://MatheusTG-14.github.io/SimtablR/reference/survtab.md)
  has resolved HR since 3.1.0; the message was left over from drafting.

- **Named change.** A
  [`table1()`](https://MatheusTG-14.github.io/SimtablR/reference/table1.md)
  adjusted effect whose model failed to converge now raises the rung-4
  `adjusted_effect_not_converged` advice rule. **Old behaviour:** the
  engine recorded `effect_converged = FALSE` but nothing read it, so a
  separated model printed an estimate such as
  `1.19e23 (8.27e22 - 1.71e23)` under an “Adjusted OR (95% CI)” heading
  with no warning. **New behaviour:** the reader is told the estimate is
  not trustworthy. **Why:**
  [`regtab()`](https://MatheusTG-14.github.io/SimtablR/reference/regtab.md)
  already warned on separation; the descriptive adjusted path did not.
  Healthy models are unaffected.

- [`survtab()`](https://MatheusTG-14.github.io/SimtablR/reference/survtab.md)
  rejects a zero-event cohort with a classed `simtab_error_engine`
  instead of failing inside `cox.zph()` with an error naming the
  engine’s own locals. The proportional-hazards diagnostic is now
  computed defensively, so a degenerate diagnostic cannot discard the
  fit.

- An all-missing `event` column is rejected with a classed binding error
  rather than passing the 0/1 check vacuously and failing later in base
  R.

- **Named change.**
  [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md) no
  longer presents an unadjusted p-value as adjusted. **Old behaviour:**
  the column was relabelled “Adjusted P-value” and `$data$tests`
  recorded the method, but a
  [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md)
  table holds a single p-value and every method is the identity on a
  family of one, so the number never changed. **New behaviour:** the
  argument warns that it was ignored and the display stays unadjusted.
  **Escape hatch:** `table1(...) |> test(p.adjust =)` adjusts a real
  family of p-values across variables.

- [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md)
  warns instead of silently returning nothing when a requested effect
  measure cannot be estimated - an outcome with fewer than two observed
  levels, or a stratified request with fewer than two usable strata -
  and names the reason. The empty column previously read as “no
  association”.

- [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md)
  announces which outcome level a ratio measure scores as the event when
  the outcome has more than two levels, instead of silently collapsing
  to “last level versus the rest”.

- [`table1()`](https://MatheusTG-14.github.io/SimtablR/reference/table1.md)
  drops the grouping variable from its own `adjust` set with a warning.
  Adjusting an effect for its own outcome is degenerate and reported an
  odds ratio of exactly 1.00 (1.00 - 1.00) from a model that never
  converged.

- p-value boundaries print at the precision the boundary itself needs:
  the default `pval_thresh = 0.001` rendered as `"<0.00"` at
  `pval_digits = 2` and `"<0"` at `pval_digits = 0`.

- [`label()`](https://MatheusTG-14.github.io/SimtablR/reference/label.md)
  accepts a named character vector passed positionally
  (`label(x, c(age = "Age"))`), the form `table1(labels =)` and
  `fmt(labels =)` already take; it previously reported the inner names
  as missing.

- The
  [`sensitivity()`](https://MatheusTG-14.github.io/SimtablR/reference/sensitivity.md)
  “requires at least one named variation” error suggested
  `denominator("complete")`, a function that does not exist, under a
  variation name the code rejects. It now names the supported
  shorthands.

- The
  [`diag_test()`](https://MatheusTG-14.github.io/SimtablR/reference/diag_test.md)
  methods sentence starts capitalised.

- [`repro_manifest()`](https://MatheusTG-14.github.io/SimtablR/reference/repro_manifest.md)
  gains `design_used`, a logical distinguishing a design that actually
  resolved `measure` (`resolved_from == "design"`) from a design that is
  merely recorded (for example a
  [`set_design()`](https://MatheusTG-14.github.io/SimtablR/reference/set_design.md)
  data-frame attribute that is never consulted by resolution). **Old
  behaviour:** the manifest reported `design = "cohort"` alongside
  `measure = NULL` with no way to tell the design took no part in
  computation. **New behaviour:** `design_used` makes that explicit, and
  [`print()`](https://rdrr.io/r/base/print.html) on the manifest notes
  it. `design` and `design_source` are unchanged. Added an exported
  [`design()`](https://MatheusTG-14.github.io/SimtablR/reference/design.md)
  accessor so callers no longer need to read the internal
  `"simtablr.design"` attribute directly;
  [`set_design()`](https://MatheusTG-14.github.io/SimtablR/reference/set_design.md)’s
  documentation now states explicitly that the data-frame form is
  advisory-only and does not itself resolve an effect measure
  (resolution has always required `design =` on the call or
  [`set_design()`](https://MatheusTG-14.github.io/SimtablR/reference/set_design.md)
  on a `simtab_spec` - this is a documentation and manifest fix, not a
  change to resolution behaviour).

- Fixed `vignettes/study-design-effect-measures.Rmd`, which read the
  wrong internal attribute name (`"simtab_design"` instead of
  `"simtablr.design"`, always printing `NULL`) and then demonstrated
  [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md)/[`table1()`](https://MatheusTG-14.github.io/SimtablR/reference/table1.md)
  calls that, under the always-advisory data-frame design contract
  above, silently never resolved the effect measure the surrounding
  prose claimed. The vignette now uses the
  [`design()`](https://MatheusTG-14.github.io/SimtablR/reference/design.md)
  accessor and passes `design =` explicitly where a measure is meant to
  resolve.

- Added an
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) method
  for audit results, which previously failed with a generic “cannot
  coerce class” error. `as.data.frame(advise(result, audit = TRUE))`
  returns the checked-rules table with a `fired` logical column.
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) on a
  general `simtab_report` (for example
  [`simtablr()`](https://MatheusTG-14.github.io/SimtablR/reference/simtablr.md)’s
  output) returns a named list of per-item data.frames, mirroring the
  existing
  [`as_gt()`](https://MatheusTG-14.github.io/SimtablR/reference/as_gt.md)/`as_flextable()`
  behaviour for reports whose items are not all the same shape.

- `print.simtab_audit()` (`advise(audit = TRUE)` output) now wraps its
  `Message`/`Citation`/`Fix` fields to `getOption("width")` instead of
  printing lines up to 160 characters wide regardless of console width.

- `print.simtab_regtab()` no longer splits a narrow console’s output
  into disconnected vertical column blocks (base `print.data.frame`’s
  behaviour when a table is wider than `options(width)`). Each row now
  always prints on one line; the `Variable` label is abbreviated with an
  ellipsis when the console is too narrow to show it in full, rather
  than letting a term’s label and its estimate land in separate,
  unlabelled blocks.

  ### Advanced visualization

- **Named change.** Forest plots now resolve their axis from the effect
  measure instead of always using a log axis. Ratio measures (OR, RR,
  PR, HR, IRR) keep the log axis with the null at 1; difference-scale
  quantities - including `regtab(exponentiate = FALSE)` coefficients -
  now use a linear axis with the null at 0. **Old behaviour:** every
  estimate was forced onto a log scale and any estimate at or below zero
  was silently dropped from the plot. **New behaviour:** those estimates
  are plotted. **Why:** a negative log-odds coefficient is a legitimate
  estimate, and silently omitting it misrepresented the model. **Escape
  hatch:** plot an exponentiated result to keep the ratio presentation.

- `autoplot()` now works on
  [`diag_test()`](https://MatheusTG-14.github.io/SimtablR/reference/diag_test.md)
  results, drawing either the stored confusion matrix
  (`type = "matrix"`, default) or sensitivity, specificity, and
  predictive values as points with their stored confidence intervals
  (`type = "metrics"`). Calibration curves are deliberately not offered:
  a binary index test provides no risk scale to calibrate. The existing
  base
  [`plot.simtab_diag()`](https://MatheusTG-14.github.io/SimtablR/reference/plot.simtab_diag.md)
  fourfold display is unchanged.

- SimtablR plots now carry the canvas size that suits them - a forest
  plot grows with its row count. The new
  [`export_plot()`](https://MatheusTG-14.github.io/SimtablR/reference/export_plot.md)
  honours that recommendation, while explicit `width`/`height` arguments
  always override.

### Error reporting

- Added the `simtab_error_input` condition class for invalid arguments
  at a public entry point, alongside the existing
  `simtab_error_binding`, `simtab_error_spec`, `simtab_error_engine`,
  `simtab_error_flag`, and `simtab_error_render` classes. Input
  validation in
  [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md),
  [`table1()`](https://MatheusTG-14.github.io/SimtablR/reference/table1.md),
  [`regtab()`](https://MatheusTG-14.github.io/SimtablR/reference/regtab.md),
  [`diag_test()`](https://MatheusTG-14.github.io/SimtablR/reference/diag_test.md),
  [`roc()`](https://MatheusTG-14.github.io/SimtablR/reference/roc.md),
  and
  [`survtab()`](https://MatheusTG-14.github.io/SimtablR/reference/survtab.md)
  now raises classed conditions with the house `x`/`i`/`v` layout rather
  than bare [`stop()`](https://rdrr.io/r/base/stop.html) calls.
  **Message text has changed**; assert on the condition class rather
  than on prose.
- SimtablR condition helpers now evaluate
  [`{}`](https://rdrr.io/r/base/Paren.html) expressions in the calling
  function’s environment, so error messages can interpolate the
  offending value directly.
- Added the `simtab_error_dependency` class, raised when a suggested
  package is needed but not installed.
- **Named change.** Every error the package raises now carries a
  SimtablR condition class. **Old behaviour:** roughly 160 validation
  failures used bare [`stop()`](https://rdrr.io/r/base/stop.html), so
  the only way to test for them was to match their prose. **New
  behaviour:** each one raises a classed condition with the house
  `x`/`i`/`v` layout. **Why:** a message is not an API; a class is.
  **Migration:** message text has changed throughout - assert on the
  class (`expect_error(..., class = "simtab_error_input")`) rather than
  on wording. Note that `cli` renders argument names with backticks, so
  patterns matching the old `'arg'` quoting no longer match. The single
  remaining bare [`stop()`](https://rdrr.io/r/base/stop.html) is in the
  autocomplete handler, where erroring is how the completer declines a
  token rather than a failure the user sees; a regression test keeps it
  the only one.

### Statistical corrections

- Diagnostic likelihood ratios now distinguish a defined infinite
  boundary from an undefined ratio.
  [`diag_test()`](https://MatheusTG-14.github.io/SimtablR/reference/diag_test.md)
  returns `Inf` for LR+ or LR- when a positive numerator is divided by
  zero, and retains `NA` for zero divided by zero. Raw result and tidy
  values remain numeric; display and spreadsheet renderers represent
  infinity only at the presentation edge.

### Export safety

- **Named change.** All public file writers now protect existing files
  by default.
  [`export_docx()`](https://MatheusTG-14.github.io/SimtablR/reference/export_docx.md),
  [`export_pptx()`](https://MatheusTG-14.github.io/SimtablR/reference/export_pptx.md),
  [`export_xlsx()`](https://MatheusTG-14.github.io/SimtablR/reference/export_xlsx.md),
  [`export_regtab_csv()`](https://MatheusTG-14.github.io/SimtablR/reference/export_regtab_csv.md),
  [`export_regtab_xlsx()`](https://MatheusTG-14.github.io/SimtablR/reference/export_regtab_xlsx.md),
  and
  [`export_plot()`](https://MatheusTG-14.github.io/SimtablR/reference/export_plot.md)
  require `overwrite = TRUE` before replacing a destination. Missing
  format suffixes are added (`.png` is the plot default), while
  incompatible suffixes raise `simtab_error_export`.
- File writers create a same-directory temporary artifact and publish it
  only after the backend succeeds. A failed write removes the temporary
  artifact and leaves any existing destination untouched. This
  intentionally replaces the former backend-dependent, sometimes silent
  overwrite behaviour.

### Regression interoperability

- [`regtab()`](https://MatheusTG-14.github.io/SimtablR/reference/regtab.md)
  and
  [`survtab()`](https://MatheusTG-14.github.io/SimtablR/reference/survtab.md)
  results now support [`coef()`](https://rdrr.io/r/stats/coef.html),
  [`confint()`](https://rdrr.io/r/stats/confint.html),
  [`formula()`](https://rdrr.io/r/stats/formula.html),
  [`nobs()`](https://rdrr.io/r/stats/nobs.html), and
  [`vcov()`](https://rdrr.io/r/stats/vcov.html) over copied, link-scale
  model evidence. Single-outcome results return conventional values;
  multi-outcome results return predictably named collections, and
  `outcome =` selects one model. Unknown, ambiguous, or failed
  selections raise `simtab_error_model`.
- Added
  [`model_info()`](https://MatheusTG-14.github.io/SimtablR/reference/model_info.md)
  for stable convergence, boundary, failure, sample-size, and event
  information. SimtablR still does not expose mutable fitted-model
  objects, fitted values, residuals, prediction, or forecasting; users
  needing those workflows should fit the underlying model package
  directly.

### Autocomplete is now RStudio-only

- **Named change.** `simtab_completions(TRUE)` installs the SimtablR
  completer in RStudio only. **Old behaviour:** the completer was
  installed on any front end, and on Rterm or radian it handed
  unrecognised tokens back to R’s own completion machinery. **New
  behaviour:** outside RStudio it declines to install, says why, and
  leaves `rc.options(custom.completer)` untouched. **Why:** handing a
  token back required calling an unexported `utils` function, which a
  CRAN package may not do; the alternative - installing anyway - would
  have left every non-SimtablR token with no completions at all.
  **Escape hatch:** none needed; declining changes nothing about how
  SimtablR itself behaves, and completion in unsupported hosts works
  exactly as it did before the completer existed.
- The reported `mode` is now `"throw"` or `"unsupported"`; the former
  `"delegate"` mode has been removed.

### Default-resolution hardening

- The 2x2 routing guard is now an exact `identical(dim(tab), c(2L, 2L))`
  check rather than `all(dim(tab) == c(2, 2))`, which recycled silently
  against higher-dimensional arrays. Tables with non-finite counts are
  also excluded. Numbers are unchanged for genuine 2x2 tables.
- [`table1()`](https://MatheusTG-14.github.io/SimtablR/reference/table1.md)
  now documents its display-denominator convention explicitly:
  percentages for observed categories use the non-missing denominator,
  the “Missing” row reports a raw count rather than a percentage of the
  column total, and association tests are identical whether or not
  missingness is displayed.

### Extension and reproducibility infrastructure

- Added the public evidence verb
  [`engine()`](https://MatheusTG-14.github.io/SimtablR/reference/engine.md).
  Registered engines can now be selected on either a `simtab_spec` or a
  computed `simtab_result`; result edits recompute through the shared
  verb-closure path and retain data-drift warnings.
- `digest` is now an imported dependency. Captured-data fingerprints and
  reproducibility manifests always use and identify `xxhash64`; the
  former weak fallback fingerprint has been removed.

### `set_summary()` replaces the conflicting `summarise()` builder verb

- This is an intentional breaking API change: use `set_summary("mean")`,
  `set_summary("median")`, or `set_summary("auto")` when configuring a
  SimtablR specification or result. SimtablR’s former `summarise()`
  generic has been removed completely, including its export and S3
  methods.
- The rename prevents SimtablR from masking
  [`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html).
  Previously, attaching SimtablR after dplyr broke ordinary data-frame
  summaries, while attaching dplyr after SimtablR broke SimtablR’s
  builder dispatch.

### Numeric-coded categorical variables are now auto-detected

- [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md) and
  [`table1()`](https://MatheusTG-14.github.io/SimtablR/reference/table1.md)
  no longer summarise every numeric column as continuous. A numeric
  variable is now treated as categorical when all its non-missing values
  are whole numbers and either exactly two distinct values exist (0/1
  dummies, 1/2 sex codes – at any sample size) or at most seven distinct
  values exist with at least 20 non-missing observations (Likert-type
  codes). This boundary matches the existing `ordinal_as_continuous`
  advice rule, so the default detection now agrees with the package’s
  own advice.
  [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md)
  announces the automatic choice with a message, and `var.type`
  overrides it in either direction.
- [`table1()`](https://MatheusTG-14.github.io/SimtablR/reference/table1.md)
  gains a `var.type` argument (scalar or per-variable named vector),
  matching
  [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md), as
  the escape hatch for the new default.
- Integer codes with many levels (e.g. numeric study-site or project
  IDs) are still detected as continuous – no cardinality threshold can
  separate them from genuine counts. Convert such columns with
  [`factor()`](https://rdrr.io/r/base/factor.html) or pass
  `var.type = "categorical"`.
- Regression machinery
  ([`regtab()`](https://MatheusTG-14.github.io/SimtablR/reference/regtab.md),
  adjusted effects, `focus` terms) keeps the strict
  numeric-means-continuous rule, so model terms and estimates are
  unchanged.

## SimtablR 3.0.0

SimtablR 3.0.0 is a major release and a deliberate clean break from the
2.x series. It reorganises the package around a single grammar of table
specifications and immutable result objects, adds survival/Firth/E-value
engines, a suite of workflow verbs, and aligns several statistical
defaults with current epidemiological practice. See
`vignette("migration-3-0")` for a full migration guide with the escape
hatch for every changed default that has one (three of the changes are
correctness fixes with no escape hatch by design).

### Progressive advice display

- Automatic advice now uses one grouped `cli` message callout per result
  or report, ordered from highest to lowest severity. The concise prose
  omits internal rule IDs, rung labels, `Fix:` prefixes, and
  SimtablR-internal citations while preserving the complete structured
  advice record.
- The default profile shows severity 3–4 advice and reports how many
  severity 1–2 notes are hidden. The new `important` profile shows
  severity 3–4 only; `quiet` is now its compatibility alias, correcting
  the previous behavior that retained only severity 1 advice. `teaching`
  shows all non-audit advice with external citations and rationale,
  while `strict` shows it in compact prose.
- Advice display can be disabled with `simtablr_guidance("off")`,
  standard [`suppressMessages()`](https://rdrr.io/r/base/message.html),
  or knitr’s `message = FALSE`; stored advice is unchanged.
- Advice now prints as one titled `cli` callout: a warning heading when
  any displayed entry has severity 3–4, and an information heading
  otherwise. Each callout shows only one action hint. Guidance hints now
  explain that
  [`simtablr_guidance()`](https://MatheusTG-14.github.io/SimtablR/reference/simtablr_guidance.md)
  is run separately before printing, and misplaced calls inside
  [`table1()`](https://MatheusTG-14.github.io/SimtablR/reference/table1.md)
  or [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md)
  receive a targeted error.
- When a result has only one non-audit advice entry, the entry itself is
  now shown under every guidance profile except `"off"`; it is no longer
  replaced by a singular hidden-note count.

### Compact result printing

- `table1` and `regtab` results now print the table without their
  decorative summary and `Call:` lines by default. Use
  `print(x, details = TRUE)` to restore both. Stored calls, report item
  headings, exports, and other engine printers are unchanged.

### Breaking changes

#### Result classes were renamed

- Every result now carries a namespaced `simtab_*` class first, so
  SimtablR’s S3 methods dispatch only on names it owns:
  [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md) -\>
  `c("simtab_tb", "tb", "simtab_result", "simtab")`,
  [`regtab()`](https://MatheusTG-14.github.io/SimtablR/reference/regtab.md)
  -\> `c("simtab_regtab", "regtab", ...)`,
  [`diag_test()`](https://MatheusTG-14.github.io/SimtablR/reference/diag_test.md)
  -\> `c("simtab_diag", "diag_test", ...)`,
  [`table1()`](https://MatheusTG-14.github.io/SimtablR/reference/table1.md)
  -\> `c("simtab_table1", "simtab_result", "simtab")`,
  [`roc()`](https://MatheusTG-14.github.io/SimtablR/reference/roc.md)
  -\> `c("simtab_roc", "simtab_result", "simtab")`.
- The bare legacy tags `tb`, `regtab`, and `diag_test` are **kept** for
  users’ [`inherits()`](https://rdrr.io/r/base/class.html) checks. The
  bare tags `table1` and `roc` are **dropped** because they collide with
  the CRAN `table1` package and
  [`pROC::roc`](https://rdrr.io/pkg/pROC/man/roc.html). Test the preset
  with `is_simtab(x, "table1")` / `is_simtab(x, "roc")` instead of
  [`inherits()`](https://rdrr.io/r/base/class.html).

#### The `p` flag was reassigned

- Bare `p` now **adds the p-value column** (equivalent to
  `test = TRUE`); it no longer means “percentage”. Use the new `perc`
  alias, or the canonical `cell` flag, for total/cell percentages. The
  first `p` use per session prints a one-time migration note.

#### Aligned statistical defaults (numbers may move)

The defaults below changed; each has an escape hatch (except the
correctness fixes, which have none by design), tabulated under “Your
numbers may move” at the end of this section.

- 2x2 chi-squared tests in
  [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md) and
  [`table1()`](https://MatheusTG-14.github.io/SimtablR/reference/table1.md)
  now use the N-1 chi-squared statistic recommended by Campbell (2007),
  replacing base Pearson/Yates output for 2x2 tables; larger r x c
  tables still use classical Pearson chi-squared. Escape:
  `test = "fisher"` for an exact test.
- Automatic 2x2 test selection now follows Campbell’s expected-cell
  boundary: `test = TRUE` uses Fisher-Irwin only when any expected cell
  is below 1, and keeps N-1 chi-squared otherwise (with educator advice
  when expected cells are between 1 and 5). This retires the previous
  expected-counts-below-5 Fisher auto-switch. Escape: `test = "fisher"`
  or `test = "chisq"`.
- Adjusted PR/RR estimates now try log-binomial regression first and
  fall back to modified Poisson regression with robust standard errors
  only when log-binomial does not converge. The estimator used is
  recorded in result metadata, announced when substituted, and reflected
  in
  [`as_methods()`](https://MatheusTG-14.github.io/SimtablR/reference/as_methods.md)
  prose.
  [`regtab()`](https://MatheusTG-14.github.io/SimtablR/reference/regtab.md)
  remains family-driven and is not rerouted. Escape: none needed – the
  fallback reproduces the previous robust-Poisson estimate exactly.
- Continuous summaries now default to `summary = "auto"` in
  [`table1()`](https://MatheusTG-14.github.io/SimtablR/reference/table1.md)
  and `stat.cont = "auto"` in
  [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md),
  using complete-case N and skewness bands from Ghasemi &
  Zahediasl (2012) to choose mean (SD) for approximately symmetric
  variables and median (IQR) otherwise. Escape: `summary = "median"` /
  `"mean"` or `stat.cont = "median"` / `"mean"`.
- [`table1()`](https://MatheusTG-14.github.io/SimtablR/reference/table1.md)
  now shows per-variable Missing rows by default, aligning descriptive
  tables with STROBE item 14.
  [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md)
  remains opt-in via `m = TRUE` or the `miss` flag. Escape:
  `missing = FALSE` or `missingness(display = FALSE)`.
- Crude Prevalence/Risk Ratio (Katz) and Odds Ratio (Woolf) calculations
  now apply a Haldane-Anscombe +0.5 continuity correction to all four
  cells of a 2x2 comparison when any cell is zero, instead of returning
  `NA`. The correction is announced via a rung-1 `zero_cell_correction`
  educator rule. Affects only rows with a zero cell.

#### Deprecations

- The bare flag aliases `rp` (-\> `pr`) and `m` (-\> `miss`) are
  soft-deprecated and warn once per session; they are slated for removal
  in 3.1.

### New engines and workflow verbs

- Added
  [`survtab()`](https://MatheusTG-14.github.io/SimtablR/reference/survtab.md)
  for Cox proportional hazards tables and `km()` for Kaplan-Meier
  summaries (via the `survival` package), with KM `autoplot()`, survival
  `tidy()`/`glance()`/[`as_methods()`](https://MatheusTG-14.github.io/SimtablR/reference/as_methods.md)
  renderers, log-rank tests, Schoenfeld PH checks, and graceful install
  guidance. The design resolver now selects HR for cohort analyses with
  time and event roles.
- Added `regtab(method = "firth")` for Firth penalised binomial-logit
  models (via `logistf`), including profile-penalised confidence
  intervals, methods prose, and separation advice pointing to a
  one-click refit.
- Added
  [`e_value()`](https://MatheusTG-14.github.io/SimtablR/reference/e_value.md)
  for native VanderWeele-Ding E-values from ratio estimates, with OR/HR
  approximation flags.
- Added `subgroups()` to recompute an existing effect result within
  levels of a subgroup variable, returning a `simtab_report`, reporting
  Breslow-Day or interaction-LRT heterogeneity, rendering a subgroup
  forest, and emitting subgroup-credibility/multiplicity advice.
- Added
  [`sensitivity()`](https://MatheusTG-14.github.io/SimtablR/reference/sensitivity.md)
  to re-estimate a result under named variations (`measure`,
  `denominator`, …) and
  [`flow()`](https://MatheusTG-14.github.io/SimtablR/reference/flow.md)
  to record the analytic sample as a participant-flow object.
- Added `explain()` and
  [`as_methods()`](https://MatheusTG-14.github.io/SimtablR/reference/as_methods.md)
  (now covering every engine) to narrate the analytic decisions and
  write the methods sentence from the result itself.
- Added
  [`strobe()`](https://MatheusTG-14.github.io/SimtablR/reference/strobe.md)
  /
  [`stard()`](https://MatheusTG-14.github.io/SimtablR/reference/stard.md)
  reporting-checklist objects (classed, with their own print method;
  they advise, they never grade or block) and
  [`codebook()`](https://MatheusTG-14.github.io/SimtablR/reference/codebook.md)
  for a one-row-per-variable data dictionary.
- Added a classed error taxonomy – `simtab_error_binding`, `_spec`,
  `_engine`, `_render`, `_flag`, all inheriting `simtab_error` –
  documented at
  [`?simtab_errors`](https://MatheusTG-14.github.io/SimtablR/reference/simtab_errors.md),
  so callers can catch failures by class.

### Language and grammar

- Added the shared terse flag vocabulary `row`, `col`, `cell`, `or`,
  `pr`, `rr`, `p`, and `miss`, enabled on both
  [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md) and
  [`table1()`](https://MatheusTG-14.github.io/SimtablR/reference/table1.md)
  ([`table1()`](https://MatheusTG-14.github.io/SimtablR/reference/table1.md)
  reads flags from `...` after `by`; unknown dots error as likely
  typos). `perc` is a quiet alias for `cell` in
  [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md)
  because it reads better for univariate percentages. Named `measure =`
  and `test =` arguments always override flags; flags apply
  left-to-right.
- Unified the binding idiom: bare names, quoted strings, and tidyselect
  helpers (`all_of()`, `starts_with()`) route through one path on every
  preset. Existing string-based code keeps working unchanged.
- Added the
  [`simtab()`](https://MatheusTG-14.github.io/SimtablR/reference/simtab.md)
  builder and setter verbs; built-in presets desugar through the verbs,
  and every setter verb has a `simtab_result` method (verb closure) that
  re-estimates via one shared recompute helper over the result’s
  captured data.
- Added the extension registry:
  [`register_engine()`](https://MatheusTG-14.github.io/SimtablR/reference/register_engine.md),
  `register_measure()`, `register_test()`,
  [`register_journal()`](https://MatheusTG-14.github.io/SimtablR/reference/register_journal.md),
  `register_rule()`, each with a `list_*()` reader. A registered engine
  reaches full method dispatch with zero edits to SimtablR (see
  `vignette("customizing-extending")`).

### Statistical methods

- Added forest-plot rendering for effect-carrying
  [`regtab()`](https://MatheusTG-14.github.io/SimtablR/reference/regtab.md),
  [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md),
  [`table1()`](https://MatheusTG-14.github.io/SimtablR/reference/table1.md),
  and Cox results via engine-local `autoplot` renderers; forest data are
  built from raw effect numerics and are invariant to
  restyling/rounding.
- [`regtab()`](https://MatheusTG-14.github.io/SimtablR/reference/regtab.md)
  now computes native GVIF/VIF-equivalent diagnostics for
  multi-predictor models, matching pinned `car::vif()` references
  without adding `car` as a dependency. Default printed/tidy/display
  tables are unchanged; request VIF via `as.data.frame(fit, vif = TRUE)`
  or `generics::glance(fit, vif = TRUE)`. Advice flags VIF above 5 and
  escalates above 10; single-predictor models stay silent.
- `regtab(robust = )` now accepts `"HC0"`, `"HC1"`, `"HC2"`, `"HC3"`, or
  `"none"` in addition to `TRUE`/`FALSE`. A rung-2 `hc0_small_n`
  educator rule suggests `robust = "HC3"` at N \< 100 (Long & Ervin,
  2000).
- `check()`/`audit()` now includes a standing, severity-0
  [`regtab()`](https://MatheusTG-14.github.io/SimtablR/reference/regtab.md)
  prompt asking whether predictors were pre-specified (formula
  provenance cannot detect stepwise selection), and a rung-2
  `many_predictors_no_design` rule fires when a
  [`regtab()`](https://MatheusTG-14.github.io/SimtablR/reference/regtab.md)
  formula has more than 10 predictor terms and no design was stated.
- Added data-shape educator rules that inspect the described variable(s)
  directly: `id_like_column`, `constant_column`,
  `high_cardinality_factor`, `ordinal_as_continuous`,
  `date_column_described`, `repeated_ids_independence`,
  `overdispersion_poisson`, `forced_mean_skewed`, and
  `tiny_denominator_pct`.
- Added survival educator rules `cox_ph_violation` (rung 4) and
  `km_median_unreached` (rung 1).

### Corrections

- [`export_docx()`](https://MatheusTG-14.github.io/SimtablR/reference/export_docx.md),
  [`export_pptx()`](https://MatheusTG-14.github.io/SimtablR/reference/export_pptx.md),
  and `as_flextable()` now work for **every** engine result –
  [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md),
  [`rbind()`](https://rdrr.io/r/base/cbind.html)-stacked tables,
  [`diag_test()`](https://MatheusTG-14.github.io/SimtablR/reference/diag_test.md),
  [`regtab()`](https://MatheusTG-14.github.io/SimtablR/reference/regtab.md),
  [`roc()`](https://MatheusTG-14.github.io/SimtablR/reference/roc.md),
  and the survival tables – and accept the documented `footnotes`
  argument. Previously only
  [`table1()`](https://MatheusTG-14.github.io/SimtablR/reference/table1.md)
  results exported; every other engine’s `as_flextable()` renderer
  forwarded `footnotes` into
  [`flextable::flextable()`](https://davidgohel.github.io/flextable/reference/flextable.html)
  and errored with `unused argument (footnotes = NULL)`.
- The Mantel-Haenszel pooled **risk-ratio confidence interval** now uses
  the Greenland-Robins (1985) variance estimator, gate-checked against
  `epiR` and the longhand formula. Earlier drafts used an incorrect
  variance; stratified PR/RR confidence intervals (from
  `tb(..., strat = )` and `subgroups()`) become wider and correct. The
  point estimate is unchanged.
- [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md)
  combined with the missing-display flag (`miss`/`m`) **and** an effect
  measure or the p-value now computes the effect ratios and the
  association test on complete cases, exactly as it does without the
  flag. Previously the flag’s `<NA>` display column was mistaken for the
  outcome’s event column, so `tb(data, exposure, outcome, or, miss)`
  reported a wrong odds/risk ratio plus a spurious `<NA>` effect row,
  and `tb(data, exposure, outcome, p, miss)` returned a corrupted
  chi-squared test (df = 4, `NaN`). The missing category is still shown
  in the frequency/percentage display; only the statistics are now
  complete-case, matching
  [`table1()`](https://MatheusTG-14.github.io/SimtablR/reference/table1.md).
- [`sensitivity()`](https://MatheusTG-14.github.io/SimtablR/reference/sensitivity.md)
  now reports the actual effect estimate for each variation instead of
  the reference row. Previously the headline extractor filtered
  reference rows with [`isTRUE()`](https://rdrr.io/r/base/Logic.html) on
  a whole logical column, which collapses to a single `FALSE` and never
  removed the reference row, so every variation reported `estimate = 1`
  with missing confidence limits regardless of the true effect.

## SimtablR 2.0.0

### Major Changes

#### tb() Function

*Overhauled tb() to return a structured list. The object inherits the S3
class vector c(“tb”, “simtab”). Matrices with attributes are no longer
returned directly from the primary function loop.* Ratio Schema
Standardization: Renamed fields within the internal ratios data frame to
lower_ci and upper_ci to establish strict compatibility with
multivariable regression tables (regtab()). \*Simplified Continuous
Syntax: Enhanced var.type parsing to accept an unnamed scalar character
string shorthand (e.g., var.type = “continuous”) and map it
automatically to the main row variable.

##### New Features

*Table Stacking (rbind.tb): Implemented the rbind.tb() S3 method to
support the vertical stacking of discrete tb objects sharing the same
column variables.* Dual Export Modes: Expanded as.data.frame.tb() and
as.data.frame.rbind_tb() to support a tidy toggle. tidy = FALSE
(default) provides display-ready character strings for manuscripts,
while tidy = TRUE returns unformatted numeric data frames optimized for
ggplot2 workflows. *RStudio Autocomplete Replacement: Integrated an
unexported interactive completion replacement hook inside zzz.R using
.rs.registerAutocompleteReplacement() to dynamically expose dataset
column names inside RStudio console environments.* Wald-Aligned Ratio
Statistics: Upgraded unadjusted Prevalence Ratio (PR) and Odds Ratio
(OR) calculations to compute Wald z-score p-values aligned directly
alongside confidence intervals. *Added new runtime educational message()
notifications that fire automatically under specific conditions* Added
explicit registerS3method() entries for rbind, print, and as.data.frame
generics within .onLoad() to guarantee stable dispatch across
development environments, source routines, and unattached package
builds.

##### Other changes

*Fixed a vulnerability where common column names (like p or col)
matching formatting flags were silently intercepted by the NSE symbol
parser.* Extracted all text formatting, cell stitching matrices, margin
additions, and string template processing out of core workflows and
isolated them within a unified internal builder called
.build_display_matrix().
