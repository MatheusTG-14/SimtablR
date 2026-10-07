# Build a STARD reporting checklist

Build a STARD reporting checklist

## Usage

``` r
stard(x, ...)
```

## Arguments

- x:

  A SimtablR diagnostic or ROC result, report, or spec.

- ...:

  Ignored.

## Value

A `simtab_checklist` object.

## Examples

``` r
d <- diag_test(epitabl, poc_hstn_positive, adjudicated_acs,
               positive = "Yes", test_positive = "Positive")
#> Removed 601 observation(s) with missing values (40.1%).
stard(d)
#> <simtab_checklist>
#>   guideline: STARD
#>  item                                       requirement            status
#>     1       Identification as diagnostic accuracy study      not-assessed
#>     2                                Structured summary      not-assessed
#>     3                Scientific and clinical background      not-assessed
#>     4                                  Study objectives      not-assessed
#>     5                                      Study design      not-assessed
#>     6                              Eligibility criteria      not-assessed
#>     7                             Participant selection      not-assessed
#>     8                           Participant recruitment      not-assessed
#>     9                                     Study setting      not-assessed
#>    10                                Index test methods covered-by-output
#>    11                                Reference standard covered-by-output
#>    12                       Test positivity definitions covered-by-output
#>    13                    Clinical information available      not-assessed
#>    14                   Methods for estimating accuracy      not-assessed
#>    15                    Handling indeterminate results      not-assessed
#>    16                             Handling missing data      not-assessed
#>    17                                       Sample size      not-assessed
#>    18                              Flow of participants      not-assessed
#>    19                          Participant flow diagram covered-by-output
#>    20 Baseline demographic and clinical characteristics      not-assessed
#>    21                  Distribution of disease severity      not-assessed
#>    22                       Time interval between tests      not-assessed
#>    23                       Cross tabulation of results      not-assessed
#>    24                     Diagnostic accuracy estimates covered-by-output
#>    25                                    Adverse events      not-assessed
#>    26                                 Study limitations      not-assessed
#>    27                         Implications for practice      not-assessed
#>    28                               Registration number      not-assessed
#>    29                                   Protocol access      not-assessed
#>    30                                   Funding sources      not-assessed
#>                                                                           pointer
#>                                                                                  
#>                                                                                  
#>                                                                                  
#>                                                                                  
#>                                                                                  
#>                                                                                  
#>                                                                                  
#>                                                                                  
#>                                                                                  
#>              Index test variables and analysis settings are recorded by SimtablR.
#>   Reference-standard variable and positive level are recorded in result metadata.
#>          Positive-test definitions and cutpoints are recorded in result metadata.
#>                                                                                  
#>                                                                                  
#>                                                                                  
#>                                                                                  
#>                                                                                  
#>                                                                                  
#>                                                       See as.data.frame(flow(x)).
#>                                                                                  
#>                                                                                  
#>                                                                                  
#>                                                                                  
#>  Diagnostic accuracy estimates and confidence intervals are rendered by SimtablR.
#>                                                                                  
#>                                                                                  
#>                                                                                  
#>                                                                                  
#>                                                                                  
#>                                                                                  
```
