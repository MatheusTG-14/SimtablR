# Set display labels

Set display labels

## Usage

``` r
label(spec, ..., labels = NULL)
```

## Arguments

- spec:

  A `simtab_spec`.

- ...:

  Named character labels.

- labels:

  Optional named character vector of labels.

## Value

A modified `simtab_spec`.

## Details

Label edits merge by variable name: supplied labels replace matching
entries and all unspecified recorded labels remain unchanged. On
computed results, this is a presentation-only edit; raw evidence is
retained and the result call records the edit.

## Examples

``` r
label(simtab(epitabl), age = "Age, years", sex = "Sex")
#> <simtab_spec>
#>   not yet computed
#>   data: 1500 rows x 22 columns; hash c0787c425db7
#>   describe: <unset>
#>   stratify/by: <unset>
#>   adjust: <unset>
#>   summary: auto
#>   measure: <unset> (ref: <unset>, conf.level: 0.95)
#>   test: <unset>
#>   design: <unset>
#>   style: default
```
