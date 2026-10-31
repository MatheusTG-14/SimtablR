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
#>  item  requirement                   status             pointer
#>     1  Title and abstract            not-assessed
#>     2  Background and rationale      not-assessed
#>     3  Objectives                    not-assessed
#>     4  Study design                  not-assessed
#>     5  Setting                       not-assessed
#>     6  Participants                  covered-by-output  Captured SimtablR sourc…
#>     7  Variables                     covered-by-output  See codebook(x) for rec…
#>     8  Data sources and measurement  not-assessed
#>     9  Bias                          not-assessed
#>    10  Study size                    covered-by-output  Source and rendered Ns …
#>    11  Quantitative variables        not-assessed
#>    12  Statistical methods           covered-by-output  See as_methods(x).
#>    13  Participants and flow         not-assessed
#>    14  Descriptive data              covered-by-output  See codebook(x) and tab…
#>    15  Outcome data                  not-assessed
#>    16  Main results                  covered-by-output  See rendered SimtablR e…
#>    17  Other analyses                not-assessed
#>    18  Key results                   not-assessed
#>    19  Limitations                   not-assessed
#>    20  Interpretation                not-assessed
#>    21  Generalisability              not-assessed
#>    22  Funding                       not-assessed
```
