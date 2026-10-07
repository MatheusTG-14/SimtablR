# Opt-in RStudio autocomplete for SimtablR calls

Installs (or removes) a session-wide completer that offers column-name
and terse-flag completions inside calls to
[`tb()`](https://MatheusTG-14.github.io/SimtablR/reference/tb.md),
[`table1()`](https://MatheusTG-14.github.io/SimtablR/reference/table1.md),
[`regtab()`](https://MatheusTG-14.github.io/SimtablR/reference/regtab.md),
[`diag_test()`](https://MatheusTG-14.github.io/SimtablR/reference/diag_test.md),
[`roc()`](https://MatheusTG-14.github.io/SimtablR/reference/roc.md), and
[`survtab()`](https://MatheusTG-14.github.io/SimtablR/reference/survtab.md)
— e.g. `tb(df, var<TAB>)`.

## Usage

``` r
simtab_completions(enable = TRUE, quiet = FALSE)
```

## Arguments

- enable:

  Logical. `TRUE` (default) installs the completer; `FALSE` removes it
  and restores whatever `custom.completer` held before.

- quiet:

  Logical. Suppress the confirmation message. Default `FALSE`.

## Value

Invisibly, a list with `enabled` (logical), `mode` (`"throw"` when
installed, `"unsupported"` when the host cannot support it), and `host`
(`"rstudio"` or `"other"`).

## Details

This works by setting `rc.options(custom.completer = )`, the only
completion-extension hook RStudio's editor honors for package authors.
Because that option is **session-global**, enabling it affects
completion behavior everywhere in the session, not just inside SimtablR
calls: on any line that isn't a recognized SimtablR call, this completer
declines the token, and RStudio falls back to its own completion engine
(or to a previously installed custom completer, which is called first).

## Supported hosts

RStudio only. Declining a token relies on the host restoring native
completion, which RStudio does and a plain console does not; on any
other front end (Rterm, radian, batch), enabling the completer would
leave non-SimtablR tokens with no completions at all.
`simtab_completions()` therefore declines to install outside RStudio and
tells you so, rather than degrading completion for the rest of the
session.

Never installed automatically — SimtablR's `.onLoad` does not call this.
Call it yourself, e.g. from `.Rprofile`:
`if (interactive()) SimtablR::simtab_completions()`.

## Examples

``` r
simtab_completions(enable = FALSE, quiet = TRUE)
```
