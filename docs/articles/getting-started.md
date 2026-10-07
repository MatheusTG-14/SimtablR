# table1() & tb(): Cohort Baseline Characteristics and 2x2 Tables

Epidemiological investigations routinely begin with two foundational
reporting tasks: characterizing study participants across exposure or
outcome strata (the so-called “Table 1”), and evaluating focused
bivariate associations using 2x2 contingency tables.

In this guide, we follow the admission and triage phase of the
**ESTROBE-ACS study**: a prospective cohort of 1,500 adult patients
presenting with acute chest pain or suspected acute coronary syndrome
(ACS) across eight emergency departments. We demonstrate how
[`table1()`](https://MatheusTG-14.github.io/SimtablR/reference/table1.md)
and [`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md)
provide fast, design-aware table generation for epidemiologists and
researchers, specially for those transitioning to R from software such
as Stata, SAS, or SPSS.

Along this guide, we will frame each aspect of the analysis with a
clinical context and research question, and show how you can use
SimtablR to generate publication-ready tables and explore your dataset
with minimal code.

------------------------------------------------------------------------

## 1. Multi-Variable Baseline Cohort Description with `table1()`

> **Clinical Context & Research Question:** In adult patients presenting
> to the emergency department with suspected ACS, how do baseline
> demographics, cardiovascular comorbidities, and arrival delays differ
> between patients with confirmed ACS versus non-ACS?

The primary baseline table characterizes the cohort by strata of the
clinical reference standard (`adjudicated_acs`), preserving sample
denominators and distribution shapes:

``` r

baseline <- table1(
  epitabl, # Input our dataset (dataframe object)
  c(age, sex, bmi, smoking, hypertension, diabetes, renal_impairment, presentation_hours),  # Choose variables of interest
  by = adjudicated_acs, # Stratify by the adjudicated ACS outcome
  test = TRUE, # Add hypothesis testing (p-values) for group comparisons
  labels = c(  # Rename variables for publication-ready output
    age = "Age (years)",
    bmi = "Body mass index (kg/m²)",
    smoking = "Smoking status",
    hypertension = "Hypertension",
    diabetes = "Diabetes mellitus",
    renal_impairment = "Renal impairment (eGFR < 60)",
    presentation_hours = "Time from symptom onset to ED (hours)"
  )
)
baseline
#> Characteristic                                        Overall (N=1500)       No (N=928)      Yes (N=572)  P-value 
#> -----------------------------------------------------------------------------------------------------------------
#> Age (years) [Mean (SD)]                                    62.0 (13.2)      60.5 (12.8)      64.4 (13.4)   <0.001 
#> Sex recorded for clinical assessment, n (%)                                                                 0.021 
#>   Female                                                   686 (45.7%)      446 (48.1%)      240 (42.0%)          
#>   Male                                                     814 (54.3%)      482 (51.9%)      332 (58.0%)          
#> Body mass index (kg/m²) [Mean (SD)]                         27.8 (4.9)       27.7 (4.8)       27.9 (5.1)    0.413 
#>   Missing                                                           63               38               25          
#> Smoking status, n (%)                                                                                       0.001 
#>   Never                                                    742 (49.5%)      474 (51.1%)      268 (46.9%)          
#>   Former                                                   469 (31.3%)      302 (32.5%)      167 (29.2%)          
#>   Current                                                  289 (19.3%)      152 (16.4%)      137 (24.0%)          
#> Hypertension, n (%)                                                                                         0.281 
#>   No                                                       732 (48.8%)      463 (49.9%)      269 (47.0%)          
#>   Yes                                                      768 (51.2%)      465 (50.1%)      303 (53.0%)          
#> Diabetes mellitus, n (%)                                                                                   <0.001 
#>   No                                                      1127 (75.1%)      735 (79.2%)      392 (68.5%)          
#>   Yes                                                      373 (24.9%)      193 (20.8%)      180 (31.5%)          
#> Renal impairment (eGFR < 60), n (%)                                                                         0.268 
#>   No                                                      1184 (78.9%)      741 (79.8%)      443 (77.4%)          
#>   Yes                                                      316 (21.1%)      187 (20.2%)      129 (22.6%)          
#> Time from symptom onset to ED (hours) [Median (IQR)]   5.6 (3.3 - 8.8)  5.7 (3.2 - 8.9)  5.6 (3.5 - 8.6)    0.650 
#>   Missing                                                           86               51               35          
#> 
#> Tests: Welch t-test; N-1 chi-squared; Pearson's Chi-squared test; Wilcoxon rank-sum
#> ℹ Methodological guidance
#>   8 unadjusted p-values are reported in this table. Pipe into test(p.adjust =
#>   'holm') for family-wise control or test(p.adjust = 'BH') for
#>   false-discovery-rate control.
#>   Run simtablr_guidance("off") separately before printing to hide advice.
```

### Key Statistical Behaviors of `table1()`

1.  **Automatic Continuous Summary Selection (`summary = "auto"`):**
    Continuous metrics are audited for skewness using complete cases.
    Symmetric variables (such as `age`) are automatically summarized as
    **Mean (SD)**. In contrast, right-skewed variables (such as
    `presentation_hours`, reflecting emergency presentation delays) are
    automatically summarized as **Median (IQR)**. Analysts can override
    this behavior across all variables using `summary = "mean"` or
    `summary = "median"`.

2.  **Explicit Denominators & Missingness:** In clinical datasets,
    missing values are rarely missing at random. Variables with missing
    observations (such as `bmi`) display explicit `N Missing (%)` counts
    directly in the table, preventing distorted clinical denominators.

3.  **Hypothesis Testing (`test = TRUE`):** Group comparisons select
    appropriate statistical tests:

    - Categorical variables: $`N-1`$ Pearson chi-squared test by
      default, or Fisher’s exact test when expected cell frequencies are
      $`< 1`$.
    - Symmetric continuous variables: Two-sample Welch $`t`$-test or
      one-way ANOVA.
    - Skewed continuous variables: Wilcoxon rank-sum test or
      Kruskal-Wallis test.

------------------------------------------------------------------------

### Adding Standardized Mean Differences (SMD) via Verb Closure

In observational studies and propensity-score evaluations, p-values
depend heavily on sample size, whereas Standardized Mean Differences
(SMDs) quantify covariate balance independently of $`N`$.

Through SimtablR’s **verb closure**, grammar verbs can be piped directly
into computed `simtab_result` objects without re-specifying the
analysis:

``` r

# Append SMDs to the computed baseline table without repeating parameters
baseline_smd <- baseline |> test(smd = TRUE) # We may also specify smd = "all" to compute SMDs for all covariates, including categorical variables.
baseline_smd
#> Characteristic                                        Overall (N=1500)       No (N=928)      Yes (N=572)    SMD  P-value 
#> ------------------------------------------------------------------------------------------------------------------------
#> Age (years) [Mean (SD)]                                    62.0 (13.2)      60.5 (12.8)      64.4 (13.4)   0.30   <0.001 
#> Sex recorded for clinical assessment, n (%)                                                                0.12    0.021 
#>   Female                                                   686 (45.7%)      446 (48.1%)      240 (42.0%)                 
#>   Male                                                     814 (54.3%)      482 (51.9%)      332 (58.0%)                 
#> Body mass index (kg/m²) [Mean (SD)]                         27.8 (4.9)       27.7 (4.8)       27.9 (5.1)   0.05    0.413 
#>   Missing                                                           63               38               25                 
#> Smoking status, n (%)                                                                                      0.19    0.001 
#>   Never                                                    742 (49.5%)      474 (51.1%)      268 (46.9%)                 
#>   Former                                                   469 (31.3%)      302 (32.5%)      167 (29.2%)                 
#>   Current                                                  289 (19.3%)      152 (16.4%)      137 (24.0%)                 
#> Hypertension, n (%)                                                                                        0.06    0.281 
#>   No                                                       732 (48.8%)      463 (49.9%)      269 (47.0%)                 
#>   Yes                                                      768 (51.2%)      465 (50.1%)      303 (53.0%)                 
#> Diabetes mellitus, n (%)                                                                                   0.24   <0.001 
#>   No                                                      1127 (75.1%)      735 (79.2%)      392 (68.5%)                 
#>   Yes                                                      373 (24.9%)      193 (20.8%)      180 (31.5%)                 
#> Renal impairment (eGFR < 60), n (%)                                                                        0.06    0.268 
#>   No                                                      1184 (78.9%)      741 (79.8%)      443 (77.4%)                 
#>   Yes                                                      316 (21.1%)      187 (20.2%)      129 (22.6%)                 
#> Time from symptom onset to ED (hours) [Median (IQR)]   5.6 (3.3 - 8.8)  5.7 (3.2 - 8.9)  5.6 (3.5 - 8.6)  -0.01    0.650 
#>   Missing                                                           86               51               35                 
#> 
#> Tests: Welch t-test; N-1 chi-squared; Pearson's Chi-squared test; Wilcoxon rank-sum
#> ℹ Methodological guidance
#>   2 additional methodological notes hidden. Run simtablr_guidance("teaching")
#>   separately before printing to show all advice.
```

Covariates with $`\text{SMD} < 0.10`$ are traditionally considered
well-balanced between clinical groups.

------------------------------------------------------------------------

## 2. Focused Bivariate Analyses with `tb()`

> **Clinical Context & Research Question:** Does pre-existing renal
> impairment (eGFR $`< 60\text{ mL/min}/1.73\text{ m}^2`$) associate
> with an increased risk of confirmed ACS, and what is the appropriate
> epidemiological effect measure under prospective cohort sampling?

While
[`table1()`](https://MatheusTG-14.github.io/SimtablR/reference/table1.md)
provides a high-level overview across many covariates,
[`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md)
isolates individual exposures and outcomes to compute contingency
tables, percentage distributions, and design-appropriate effect
measures.

### 2.1 Study Design Resolution: Relative Risk in Prospective Cohorts

In prospective cohorts, the primary parameter of interest is the **Risk
Ratio (RR)**. Specifying `design = "cohort"` instructs SimtablR’s design
resolver to estimate the Relative Risk with Greenland-Robins or Katz log
confidence intervals:

``` r

tab_rr <- tb(
  epitabl,
  renal_impairment,
  adjudicated_acs,
  flags = c("row", "rr"),
  design = "cohort", # By specifying our study design, SimtablR automatically selects the appropriate effect measure (RR) and confidence interval method.
  ref = "No"
)
tab_rr
#>                                                  | Adjudicated acute coronary syndrome 
#>  Renal impairment (eGFR below 60 mL/min/1.73 m2) |     No           Yes     
#> -------------------------------------------------+--------------------------
#>                                               No | 741 (62.6%)  443 (37.4%) 
#>                                              Yes | 187 (59.2%)  129 (40.8%) 
#> -------------------------------------------------+--------------------------
#>                                            Total |     928          572     
#> 
#>                                                  | Adjudicated acute coronary syndrome 
#>  Renal impairment (eGFR below 60 mL/min/1.73 m2) | Total 
#> -------------------------------------------------+-------
#>                                               No | 1184  
#>                                              Yes |  316  
#> -------------------------------------------------+-------
#>                                            Total | 1500  
#> 
#>  Renal impairment (eGFR below 60 mL/min/1.73 m2) |          RR (95% CI)          
#> -------------------------------------------------+-------------------------------
#>                                               No |          1.00 (Ref)           
#>                                              Yes | 1.09 (0.94 - 1.27), p = 0.261 
#> -------------------------------------------------+-------------------------------
#>                                            Total |
```

With this quick and easy code, we can find that patients presenting with
baseline renal impairment experienced a significantly higher absolute
incidence of confirmed ACS compared to those with preserved renal
function.

### 2.2 Flags for quick coding

If you are transitioning from Stata (e.g., `tabulate, row col`) or Epi
Info, you can control table contents using concise string flags just as
you would in those software packages. The following flags are available:

- **Percentage Denominators:**
  - `flags = "row"`: Row percentages (essential when the row represents
    the exposure).
  - `flags = "col"`: Column percentages (standard for case-control
    studies).
  - `flags = "cell"`: Cell percentage out of the total sample size
    ($`N = 1,500`$).
- **Effect Measures:**
  - `flags = "rr"`: Risk Ratio (Relative Risk).
  - `flags = "or"`: Odds Ratio with Woolf / logit intervals.
  - `flags = "pr"`: Prevalence Ratio (for cross-sectional designs).
- **Inference:**
  - `flags = "p"`: Displays the inferential p-value.
  - `flags = "miss"`: Displays missing value rows and columns.

``` r

# Cell percentages with explicit chi-squared p-value
tb(epitabl, 
   smoking, adjudicated_acs, # Evaluate the association between smoking status and confirmed ACS
   flags = c("cell", "p") # Add cell percentages and a chi-squared p-value for the association
   ) 
#> `p` now adds the p-value column (SimtablR 3.0); use `perc` or `cell` for total
#> percentages.
#>                 | Adjudicated acute coronary syndrome 
#>  Smoking status |     No           Yes     | Total 
#> ----------------+--------------------------+-------
#>           Never | 474 (31.6%)  268 (17.9%) |  742  
#>          Former | 302 (20.1%)  167 (11.1%) |  469  
#>         Current | 152 (10.1%)  137 (9.1%)  |  289  
#> ----------------+--------------------------+-------
#>           Total |     928          572     | 1500  
#> 
#>   Test: Pearson's Chi-squared test  p-value = 0.001
```

------------------------------------------------------------------------

## 3. Stratified Analysis & Mantel-Haenszel Pooling

> Does biological sex confound or modify the association between renal
> impairment and confirmed ACS, and what is the common pooled Risk Ratio
> after adjusting for sex?

Stratification allows evaluating potential confounding and effect
modification across subgroups. Passing `strat = sex` calculates
stratum-specific estimates alongside the Greenland-Robins
Mantel-Haenszel pooled estimate and a test of homogeneity:

``` r

tab_strat <- tb( 
  epitabl,
  renal_impairment,
  adjudicated_acs,
  strat = sex, # Stratify by biological sex to evaluate effect measure modification
  flags = c("row", "rr"),
  design = "cohort" 
)
#> Note: No reference level specified for PR/OR calculation. Defaulting to the first level: 'No'.
tab_strat
#>                                                  | adjudicated_acs (Stratified) 
#>  Renal impairment (eGFR below 60 mL/min/1.73 m2) | Female : No  Female : Yes 
#> -------------------------------------------------+---------------------------
#>                                               No | 362 (30.6%)  188 (15.9%)  
#>                                              Yes | 84 (26.6%)    52 (16.5%)  
#>                                            Total |     446          240      
#>                      Mantel-Haenszel pooled: Yes |                           
#> 
#>                                                  | adjudicated_acs (Stratified) 
#>  Renal impairment (eGFR below 60 mL/min/1.73 m2) |  Male : No   Male : Yes  
#> -------------------------------------------------+--------------------------
#>                                               No | 379 (32.0%)  255 (21.5%) 
#>                                              Yes | 103 (32.6%)  77 (24.4%)  
#>                                            Total |     482          332     
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
#>  Renal impairment (eGFR below 60 mL/min/1.73 m2) |                 RR MH (95% CI)                  
#> -------------------------------------------------+-------------------------------------------------
#>                                               No |                                                 
#>                                              Yes |                                                 
#>                                            Total |                                                 
#>                      Mantel-Haenszel pooled: Yes | 1.09 (0.93 - 1.26), CMH p = 0.327, BD p = 0.788
```

The stratum-specific Risk Ratios for males and females remain
consistent, and the test of homogeneity confirms no significant effect
measure modification by sex.

------------------------------------------------------------------------

## 4. Stacking Multiple Bivariate Tables with `rbind()`

> How do crude unadjusted effect measures across multiple clinical risk
> factors compare side-by-side prior to multivariable regression
> modeling?

In manuscript preparation, investigators standardly summarize a battery
of unadjusted bivariate associations in a single summary table. SimtablR
implements an [`rbind()`](https://rdrr.io/r/base/cbind.html) method
specifically for `simtab_tb` objects:

``` r

t_smoke <- tb(epitabl, smoking, adjudicated_acs, flags = c("row", "or"), ref = "Never")
t_htn   <- tb(epitabl, hypertension, adjudicated_acs, flags = c("row", "or"), ref = "No")
t_dm    <- tb(epitabl, diabetes, adjudicated_acs, flags = c("row", "or"), ref = "No")
t_ckd   <- tb(epitabl, renal_impairment, adjudicated_acs, flags = c("row", "or"), ref = "No")

# Stack into a unified crude association table
stacked_crude <- rbind(t_smoke, t_htn, t_dm, t_ckd)
stacked_crude
#>                                                  | Adjudicated acute coronary syndrome 
#>                                         Variable |     No           Yes     
#> -------------------------------------------------+--------------------------
#>                                   Smoking status |                          
#>                                            Never | 474 (63.9%)  268 (36.1%) 
#>                                           Former | 302 (64.4%)  167 (35.6%) 
#>                                          Current | 152 (52.6%)  137 (47.4%) 
#>                          History of hypertension |                          
#>                                               No | 463 (63.3%)  269 (36.7%) 
#>                                              Yes | 465 (60.5%)  303 (39.5%) 
#>                              History of diabetes |                          
#>                                               No | 735 (65.2%)  392 (34.8%) 
#>                                              Yes | 193 (51.7%)  180 (48.3%) 
#>  Renal impairment (eGFR below 60 mL/min/1.73 m2) |                          
#>                                               No | 741 (62.6%)  443 (37.4%) 
#>                                              Yes | 187 (59.2%)  129 (40.8%) 
#> -------------------------------------------------+--------------------------
#>                                            Total |     928          572     
#> 
#>                                                  | Adjudicated acute coronary syndrome 
#>                                         Variable | Total 
#> -------------------------------------------------+-------
#>                                   Smoking status |       
#>                                            Never |  742  
#>                                           Former |  469  
#>                                          Current |  289  
#>                          History of hypertension |       
#>                                               No |  732  
#>                                              Yes |  768  
#>                              History of diabetes |       
#>                                               No | 1127  
#>                                              Yes |  373  
#>  Renal impairment (eGFR below 60 mL/min/1.73 m2) |       
#>                                               No | 1184  
#>                                              Yes |  316  
#> -------------------------------------------------+-------
#>                                            Total | 1500  
#> 
#>                                         Variable |          OR (95% CI)          
#> -------------------------------------------------+-------------------------------
#>                                   Smoking status |                               
#>                                            Never |          1.00 (Ref)           
#>                                           Former | 0.98 (0.77 - 1.24), p = 0.857 
#>                                          Current | 1.59 (1.21 - 2.10), p < 0.001 
#>                          History of hypertension |                               
#>                                               No |          1.00 (Ref)           
#>                                              Yes | 1.12 (0.91 - 1.38), p = 0.281 
#>                              History of diabetes |                               
#>                                               No |          1.00 (Ref)           
#>                                              Yes | 1.75 (1.38 - 2.22), p < 0.001 
#>  Renal impairment (eGFR below 60 mL/min/1.73 m2) |                               
#>                                               No |          1.00 (Ref)           
#>                                              Yes | 1.15 (0.90 - 1.49), p = 0.268 
#> -------------------------------------------------+-------------------------------
#>                                            Total |
```

All underlying numeric estimates, standard errors, and confidence bounds
are preserved in the returned result and can be extracted using
`as.data.frame(stacked_crude)` or formatted for publication with
[`as_gt()`](https://MatheusTG-14.github.io/SimtablR/reference/as_gt.md)
or
[`export_docx()`](https://MatheusTG-14.github.io/SimtablR/reference/export_docx.md).
