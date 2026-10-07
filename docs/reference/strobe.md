# Build a STROBE reporting checklist

Build a STROBE reporting checklist

## Usage

``` r
strobe(x, ...)

# S3 method for class 'simtab_checklist'
as_flextable(x, ...)
```

## Arguments

- x:

  A `simtab_checklist` object.

- ...:

  Ignored.

## Value

A `simtab_checklist` object.

## Examples

``` r
res <- tb(epitabl, sex, diabetes)
strobe(res)
#> <simtab_checklist>
#>   guideline: STROBE
#>  item                  requirement            status
#>     1           Title and abstract      not-assessed
#>     2     Background and rationale      not-assessed
#>     3                   Objectives      not-assessed
#>     4                 Study design      not-assessed
#>     5                      Setting      not-assessed
#>     6                 Participants covered-by-output
#>     7                    Variables covered-by-output
#>     8 Data sources and measurement      not-assessed
#>     9                         Bias      not-assessed
#>    10                   Study size covered-by-output
#>    11       Quantitative variables      not-assessed
#>    12          Statistical methods covered-by-output
#>    13        Participants and flow      not-assessed
#>    14             Descriptive data covered-by-output
#>    15                 Outcome data      not-assessed
#>    16                 Main results covered-by-output
#>    17               Other analyses      not-assessed
#>    18                  Key results      not-assessed
#>    19                  Limitations      not-assessed
#>    20               Interpretation      not-assessed
#>    21             Generalisability      not-assessed
#>    22                      Funding      not-assessed
#>                                                                 pointer
#>                                                                        
#>                                                                        
#>                                                                        
#>                                                                        
#>                                                                        
#>                  Captured SimtablR source-data row count and variables.
#>                                 See codebook(x) for recorded variables.
#>                                                                        
#>                                                                        
#>                        Source and rendered Ns are recorded by SimtablR.
#>                                                                        
#>                                                      See as_methods(x).
#>                                                                        
#>  See codebook(x) and table output for descriptive data and missingness.
#>                                                                        
#>               See rendered SimtablR estimates and confidence intervals.
#>                                                                        
#>                                                                        
#>                                                                        
#>                                                                        
#>                                                                        
#>                                                                        
```
