# Evaluate a SimtablR analysis specification

Turns an inert `simtab_spec` into a `simtab_result` by running the
shared, engine-agnostic validator, resolving one registered engine,
running that engine's own hard-error validator (if any), and wrapping
the engine's raw numeric output in the namespaced result subclass the
engine registered. Passing an existing `simtab_result` recomputes from
its stored specification.

## Usage

``` r
evaluate(spec)
```

## Arguments

- spec:

  A `simtab_spec`, or a `simtab_result` to recompute.

## Value

A `simtab_result`.

## Examples

``` r
sp <- simtab(epitabl) |> describe(hypertension) |> stratify(adjudicated_acs)
res <- evaluate(sp)
res
#> Characteristic                  Overall (N=1500)   No (N=928)  Yes (N=572) 
#> --------------------------------------------------------------------------
#> History of hypertension, n (%)                                             
#>   No                                 732 (48.8%)  463 (49.9%)  269 (47.0%) 
#>   Yes                                768 (51.2%)  465 (50.1%)  303 (53.0%) 
```
