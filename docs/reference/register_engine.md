# Register a SimtablR computation engine

Stores the full engine contract — compute function, result subclass,
optional legacy class tag, optional hard-error validator, a named subset
of renderer verbs, and an optional engine-options validator — under a
case-insensitive name. A user-registered engine gets
print/as.data.frame/ as_flextable/as_gt/tidy/glance/autoplot/as_methods
dispatch through
[`evaluate()`](https://MatheusTG-14.github.io/SimtablR/reference/evaluate.md)'s
shared `simtab_result` methods with no further S3 ceremony.

## Usage

``` r
register_engine(
  name,
  compute,
  subclass = paste0("simtab_", .normalise_registry_name(name)),
  legacy_tag = NULL,
  validate = NULL,
  renderers = list(),
  engine_opts = NULL
)
```

## Arguments

- name:

  Character engine name, e.g. `"roc"`.

- compute:

  Function with signature `function(spec, data)` returning
  `list(data = ..., meta = ...)`.

- subclass:

  Character result subclass. Defaults to `paste0("simtab_", name)`.

- legacy_tag:

  Optional extra class tag kept for backward-compatible
  [`inherits()`](https://rdrr.io/r/base/class.html) checks. Never used
  for SimtablR's own dispatch.

- validate:

  Optional function `function(spec) -> invisible(spec)` for hard,
  engine-specific errors only.

- renderers:

  Named list of renderer functions, names drawn from `.renderer_verbs()`
  (`print`, `as_data_frame`, `as_flextable`, `as_gt`, `tidy`, `glance`,
  `autoplot`, `as_methods`). Each function takes `function(x, ...)`.

- engine_opts:

  Optional function `function(opts) -> invisible(opts)` validating the
  engine's spec-level options.

## Value

Invisibly, `name`.

## See also

[`list_engines()`](https://MatheusTG-14.github.io/SimtablR/reference/list_engines.md),
[`is_simtab()`](https://MatheusTG-14.github.io/SimtablR/reference/is_simtab.md)

## Examples

``` r
if (FALSE) { # \dontrun{
register_engine("dummy", compute = function(spec, data) list(data = data, meta = list()))
} # }
```
