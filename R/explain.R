# CONVERSATIONAL DECISION EXPLANATION AND AUDIT TRAIL
# Formats narrative explanations of recorded analytical decisions, design resolutions,
# missingness policies, and active advisory notices across specifications and results.

#########
# EXPLANATION GENERATOR DISPATCH
# S3 method dispatch across uncomputed specifications, computed results, and composed reports.

#' Report why SimtablR made each recorded decision
#'
#' `why()` prints the decisions already recorded on a spec, result, or
#' report. Passing a `simtab_spec` is inert: it describes the planned analysis
#' without computing it.
#'
#' @param x A `simtab_spec`, `simtab_result`, or `simtab_report`.
#' @param ... Ignored.
#' @return A `simtab_explanation` object, invisibly when printed.
#' @examples
#' res <- tb(epitabl, sex, diabetes)
#' why(res)
#' @export
why <- function(x, ...) {
  UseMethod("why")
}

#' @export
why.simtab_spec <- function(x, ...) {
  x <- validate_simtab_spec(x)
  lines <- c(
    "SimtablR explanation",
    "Status: not yet computed; this spec is an inert description.",
    sprintf("Engine: %s", x$engine %||% "not resolved"),
    sprintf("Design: %s", x$design %||% "not recorded"),
    sprintf("Measure: %s", x$effect$measure %||% "not selected"),
    sprintf("Measure source: %s", x$effect$resolved_from %||% "not recorded"),
    sprintf("Missingness: display=%s, denominator=%s, model=%s", x$missing$display, x$missing$denominator, x$missing$model_na),
    sprintf("Ruleset version: %s", .simtab_ruleset_version())
  )
  .new_simtab_explanation(lines, subject_class = class(x))
}

#' @export
why.simtab_result <- function(x, ...) {
  x <- validate_simtab_result(x)
  advice <- .filter_advice_for_guidance(x$advice %||% list(), simtablr_guidance())
  lines <- c(
    "SimtablR explanation",
    "Status: computed result.",
    sprintf("Engine: %s", x$meta$engine %||% x$spec$engine %||% "not recorded"),
    sprintf("Design: %s", .result_design(x) %||% "not recorded"),
    .why_exposure_line(x),
    if (inherits(x, "simtab_regtab")) {
      .why_regtab_lines(x)
    } else {
      c(
        sprintf("Measure: %s", x$spec$effect$measure %||% x$meta$effect %||% "not selected"),
        sprintf("Measure source: %s", x$spec$effect$resolved_from %||% "not recorded")
      )
    },
    sprintf("Ruleset version: %s", x$meta$ruleset_version %||% .simtab_ruleset_version())
  )
  if (length(advice) > 0) {
    lines <- c(lines, "Advice:", vapply(advice, .format_advice_line, character(1), level = simtablr_guidance()))
  }
  .new_simtab_explanation(lines, subject_class = class(x))
}

#' @export
why.simtab_report <- function(x, ...) {
  x <- validate_simtab_report(x)
  advice <- .filter_advice_for_guidance(x$advice %||% list(), simtablr_guidance())
  lines <- c(
    "SimtablR explanation",
    sprintf("Status: report with %d item(s).", length(x$items)),
    sprintf("Methods: %s", x$methods %||% "not recorded"),
    sprintf("Ruleset version: %s", .simtab_ruleset_version())
  )
  if (length(advice) > 0) {
    lines <- c(lines, "Advice:", vapply(advice, .format_advice_line, character(1), level = simtablr_guidance()))
  }
  .new_simtab_explanation(lines, subject_class = class(x))
}

#' Describe the exposure and adjustment set of a model result, or nothing
#' @keywords internal
#' @noRd
.why_exposure_line <- function(x) {
  if (!inherits(x, "simtab_regtab") && !inherits(x, "simtab_cox")) {
    return(character())
  }
  decisions <- .model_exposure_decisions(x)
  if (is.null(decisions)) {
    return("Exposure: not recorded; every model term is reported alike")
  }
  sprintf(
    "Exposure: %s; adjustment set: %s",
    decisions$exposure,
    if (length(decisions$adjustment) == 0) "none" else paste(decisions$adjustment, collapse = ", ")
  )
}

#' Describe the measure, its source, and convergence of a regression result
#' @keywords internal
#' @noRd
.why_regtab_lines <- function(x) {
  measures <- unique(.regtab_measures(x))
  explicit <- x$spec$effect$measure
  source <- if (is.character(explicit) && length(explicit) == 1L && nzchar(explicit)) {
    "explicit measure"
  } else {
    sprintf(
      "%s family, %s link, %s",
      x$meta$family %||% "unknown",
      x$meta$link %||% "unknown",
      if (is.null(.result_design(x))) "no recorded design" else paste(.result_design(x), "design")
    )
  }
  lines <- c(
    sprintf("Measure: %s", paste(measures, collapse = ", ")),
    sprintf("Measure source: %s", source)
  )
  if (identical(x$meta$link, "log")) {
    lines <- c(lines, sprintf("Convergence: %s", .regtab_convergence_text(x)))
  }
  lines
}

#########
# EXPLANATION OBJECT CONSTRUCTION AND DISPLAY
# Builds simtab_explanation structures and formats multi-line console output.

#' Construct and print a simtab_explanation container object
#' @keywords internal
#' @noRd
.new_simtab_explanation <- function(lines, subject_class) {
  out <- structure(
    list(lines = lines, subject_class = subject_class),
    class = c("simtab_explanation", "simtab")
  )
  print(out)
  invisible(out)
}

#' @export
print.simtab_explanation <- function(x, ...) {
  cat(paste(x$lines, collapse = "\n"), "\n", sep = "")
  invisible(x)
}
