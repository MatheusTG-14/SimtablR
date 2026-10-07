# Set a comparison test

Set a comparison test

## Usage

``` r
test(spec, method = "auto", p.adjust = NULL, paired = NULL, smd = NULL)
```

## Arguments

- spec:

  A `simtab_spec`.

- method:

  Test method. Defaults to `"auto"`. Categorical methods are `"chisq"`,
  `"fisher"`, `"mcnemar"`, and `"trend"`; continuous methods are `"t"`,
  `"wilcoxon"`, `"anova"`, and `"kruskal"`. Use `"none"` or `FALSE` to
  clear the comparison test. When `method` is omitted but `p.adjust`,
  `paired`, or `smd` is supplied, the existing test choice is kept, so
  `table1(...) |> test(smd = TRUE)` adds SMDs without adding p-values.

- p.adjust:

  Multiplicity adjustment method, or `NULL` to leave unchanged.

- paired:

  Logical; whether paired tests are requested.

- smd:

  Logical; whether standardized mean differences are requested.

## Value

A modified `simtab_spec`.

## Examples

``` r
test(stratify(describe(simtab(epitabl), age), adjudicated_acs), "wilcoxon")
#> <simtab_spec>
#>   not yet computed
#>   data: 1500 rows x 22 columns; hash c0787c425db7
#>   describe: age
#>   stratify/by: adjudicated_acs
#>   adjust: <unset>
#>   summary: auto
#>   measure: <unset> (ref: <unset>, conf.level: 0.95)
#>   test: wilcoxon
#>   design: <unset>
#>   style: default
```
