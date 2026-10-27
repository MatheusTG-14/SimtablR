# MANUSCRIPT METHODS NARRATIVE SYNTHESIZER
# Translates recorded analytical decisions (summary metrics, hypothesis tests,
# effect estimators, regression models, and diagnostic frameworks) into publication-ready prose.

#########
# S3 GENERICS AND METHOD DISPATCH
# S3 method dispatch across uncomputed specifications, computed results, and composed reports.

#' Generate a manuscript methods sentence
#'
#' Builds concise manuscript prose from the decisions recorded in a computed
#' SimtablR result. Passing a bare `simtab_spec` computes it first, matching the
#' other render/extract verbs.
#'
#' @param x A `simtab_result` or `simtab_spec`.
#' @param ... Ignored.
#' @return A character string of length one.
#' @examples
#' res <- tb(epitabl, sex, diabetes)
#' as_methods(res)
#' @export
as_methods <- function(x, ...) {
  UseMethod("as_methods")
}

#' @export
as_methods.simtab_spec <- function(x, ...) {
  as_methods(evaluate(x), ...)
}

#' @export
as_methods.simtab_result <- function(x, ...) {
  x <- validate_simtab_result(x)
  fn <- .engine_renderer(x, "as_methods")
  if (is.null(fn)) {
    engine <- x$meta$engine %||% x$spec$engine %||% "unknown"
    simtab_abort_render(c(
      sprintf("No {.fn as_methods} renderer is registered for engine {.val %s}.", engine),
      "i" = "SimtablR results must report the analytic decisions used to produce the table.",
      ">" = "Register one with {.code register_engine(..., renderers = list(as_methods = function(x, ...) ...))}."
    ))
  }
  fn(x, ...)
}

#' @export
as_methods.simtab_report <- function(x, ...) {
  x <- validate_simtab_report(x)
  if (!is.null(x$methods) && is.character(x$methods) && length(x$methods) == 1 && nzchar(x$methods)) {
    return(x$methods)
  }
  .report_methods(x$items)
}

#########
# ENGINE-SPECIFIC METHODS PARSERS
# Builds narrative methodology statements for Table 1, tb, regtab, and diag_test.

#' Synthesize methodology narrative for Table 1 descriptive results
#' @keywords internal
#' @noRd
.methods_as_table1 <- function(x, ...) {
  x <- validate_simtab_result(x)
  parts <- c(
    .methods_summary_sentence(x),
    .methods_test_sentence(x),
    .methods_effect_sentence(x)
  )
  paste(parts[nzchar(parts)], collapse = " ")
}

#' Synthesize methodology narrative for focused bivariate tb results
#' @keywords internal
#' @noRd
.methods_as_tb <- function(x, ...) {
  x <- validate_simtab_result(x)
  parts <- c(
    .methods_tb_summary_sentence(x),
    .methods_tb_test_sentence(x),
    .methods_effect_sentence(x)
  )
  paste(parts[nzchar(parts)], collapse = " ")
}

#' Synthesize methodology narrative for regression regtab results
#' @keywords internal
#' @noRd
.methods_as_regtab <- function(x, ...) {
  x <- validate_simtab_result(x)
  decisions <- .result_decisions(x)
  ci <- x$meta$conf_pct %||% round((x$meta$conf.level %||% 0.95) * 100)
  if (identical(x$meta$method %||% "glm", "firth")) {
    return(paste0(sprintf(
      "Regression models were fit with %s (Firth 1993; Heinze & Schemper 2002); %d%% %s were reported.",
      decisions$estimator,
      ci,
      decisions$interval_method
    ), .model_exposure_sentence(x, decisions)))
  }
  model <- sprintf("Regression models were fit with a %s", decisions$estimator)
  measure_names <- c(
    OR = "odds ratios", RR = "risk ratios", PR = "prevalence ratios",
    IRR = "incidence-rate ratios"
  )
  named <- decisions$measure %in% names(measure_names)
  if (any(named)) {
    design <- .result_design(x)
    model <- sprintf(
      "%s, with exponentiated estimates reported as %s%s",
      model,
      paste(unname(measure_names[decisions$measure[named]]), collapse = " and "),
      if (is.null(design)) "" else sprintf(" for a %s design", .design_label(design))
    )
  } else if (isTRUE(x$meta$exponentiate)) {
    model <- paste0(model, ", with exponentiated estimates")
  }
  out <- sprintf("%s; %d%% %s were reported.", model, ci, decisions$interval_method)
  out <- paste0(out, .model_exposure_sentence(x, decisions))
  if (!is.null(decisions$convergence)) {
    out <- paste0(out, " ", .regtab_convergence_sentence(decisions$convergence))
  }
  out
}

#' Record the model, interval, and convergence decisions of a regression result
#' @keywords internal
#' @noRd
.decisions_regtab <- function(x, ...) {
  if (identical(x$meta$method %||% "glm", "firth")) {
    return(c(list(
      measure = unique(.regtab_measures(x)),
      estimator = "Firth penalised likelihood",
      interval_method = "profile-penalised CI",
      packages = "logistf"
    ), .model_exposure_decisions(x)))
  }
  c(list(
    measure = unique(.regtab_measures(x)),
    estimator = sprintf("%s family and %s link", x$meta$family, x$meta$link),
    interval_method = if (isTRUE(x$meta$robust)) {
      sprintf("Wald CI (robust %s standard errors)", x$meta$vcov %||% "HC0")
    } else {
      "Wald CI (model-based standard errors)"
    },
    convergence = if (identical(x$meta$link, "log")) .regtab_convergence_text(x)
  ), .model_exposure_decisions(x))
}

#' Describe whether log-link models converged inside the parameter space
#' @keywords internal
#' @noRd
.regtab_convergence_text <- function(x) {
  info <- x$meta$model_info
  fitted <- info[!info$failed, , drop = FALSE]
  problems <- fitted$outcome[!fitted$converged | fitted$boundary %in% TRUE]
  out <- if (length(problems) == 0) {
    "all models converged"
  } else {
    sprintf("did not converge or reached the parameter boundary for %s", paste(problems, collapse = ", "))
  }
  if (any(info$failed)) {
    out <- sprintf("%s; failed to fit for %s", out, paste(info$outcome[info$failed], collapse = ", "))
  }
  out
}

#' Synthesize the convergence sentence for log-link regression results
#' @keywords internal
#' @noRd
.regtab_convergence_sentence <- function(text) {
  paste0(toupper(substr(text, 1, 1)), substr(text, 2, nchar(text)), ".")
}

#' Synthesize methodology narrative for diagnostic accuracy test results
#' @keywords internal
#' @noRd
.methods_as_diag <- function(x, ...) {
  x <- validate_simtab_result(x)
  intervals <- .result_decisions(x)$interval_method
  ci <- x$meta$conf_pct %||% round((x$meta$conf.level %||% 0.95) * 100)
  sprintf(
    paste0(
      "Diagnostic accuracy was evaluated against %s as the reference standard; ",
      "sensitivity, specificity, predictive values, accuracy, and prevalence were ",
      "reported with %s %d%% CI, likelihood ratios with %s ",
      "%d%% confidence intervals, the diagnostic odds ratio with a %s %d%% CI, ",
      "and Cohen's kappa was reported for index-test / reference-standard ",
      "agreement with a %s %d%% CI."
    ),
    x$meta$ref_var,
    intervals[["proportions"]], ci,
    intervals[["likelihood_ratios"]], ci,
    intervals[["diagnostic_odds_ratio"]], ci,
    intervals[["kappa"]], ci
  )
}

#' Record the interval methods of a diagnostic accuracy result
#' @keywords internal
#' @noRd
.decisions_diag <- function(x, ...) {
  list(
    interval_method = c(
      proportions = if (identical(x$meta$ci, "wilson")) "Wilson" else "exact binomial",
      likelihood_ratios = "asymptotic (log-method)",
      diagnostic_odds_ratio = "log",
      kappa = "large-sample"
    )
  )
}

#########
# STATISTICAL SUMMARY AND HYPOTHESIS TESTING CLAUSES
# Formats sentences detailing summary statistics, skewness rationales, and test methods.

#' Construct summary statistics methodology sentence for Table 1 variables
#' @keywords internal
#' @noRd
.methods_summary_sentence <- function(x) {
  stats <- vapply(names(x$data), function(v) {
    rec <- x$data[[v]]
    if (!identical(rec$type, "continuous")) {
      return(NA_character_)
    }
    label <- if (identical(rec$stat, "mean")) "mean (SD)" else "median [IQR]"
    auto <- x$meta$summary_auto[[v]] %||% rec$auto_summary
    if (!is.null(auto)) {
      if (identical(auto$decision, "mean")) {
        return(paste0(label, ", distribution approximately symmetric"))
      }
      if (grepl("skewness", auto$reason %||% "", fixed = TRUE)) {
        return(paste0(label, " owing to skewness"))
      }
    }
    label
  }, character(1))
  stats <- unique(stats[!is.na(stats)])
  if (length(stats) == 0) {
    return("Categorical variables were summarised with counts and percentages.")
  }
  paste0("Continuous variables were summarised as ", paste(stats, collapse = " and "), ".")
}

#' Construct summary statistics methodology sentence for tb bivariate comparisons
#' @keywords internal
#' @noRd
.methods_tb_summary_sentence <- function(x) {
  if (isTRUE(x$meta$is_continuous)) {
    stat <- if (identical(x$meta$stat.cont, "mean")) "mean (SD)" else "median [IQR]"
    auto <- x$meta$summary_auto
    if (!is.null(auto)) {
      if (identical(auto$decision, "mean")) {
        return(paste0("Continuous variables were summarised as ", stat, ", distribution approximately symmetric."))
      }
      if (grepl("skewness", auto$reason %||% "", fixed = TRUE)) {
        return(paste0("Continuous variables were summarised as ", stat, " owing to skewness."))
      }
    }
    return(paste0("Continuous variables were summarised as ", stat, "."))
  }
  "Categorical variables were summarised with counts and percentages."
}

#' Construct hypothesis testing methodology clause for Table 1 results
#' @keywords internal
#' @noRd
.methods_test_sentence <- function(x) {
  decisions <- .result_decisions(x)
  methods <- setdiff(decisions$tests, decisions$homogeneity)
  if (length(methods) == 0) {
    return("")
  }
  paste0("Group comparisons used ", paste(methods, collapse = "; "), ".")
}

#' Construct hypothesis testing methodology clause for tb results
#' @keywords internal
#' @noRd
.methods_tb_test_sentence <- function(x) {
  .methods_test_sentence(x)
}

#' Standardize and format statistical test names for manuscript prose
#' @keywords internal
#' @noRd
.clean_test_methods <- function(methods) {
  methods <- gsub("Pearson's Chi-squared", "Pearson chi-squared", methods, fixed = TRUE)
  methods <- gsub(" with Yates' continuity correction", "", methods, fixed = TRUE)
  methods
}

#########
# RECORDED ANALYTIC DECISIONS
# One record per result, read by both the methods prose and repro_manifest().

#' Collect the analytic decisions recorded on a result
#'
#' Methods prose and [repro_manifest()] both read this record, so every
#' decision the prose names is also in the manifest. Engines supply it through
#' the `decisions` renderer; registered engines without one fall back to
#' `meta$estimator` and `meta$interval_method`.
#' @keywords internal
#' @noRd
.result_decisions <- function(x) {
  fn <- .engine_renderer(x, "decisions")
  out <- if (is.null(fn)) {
    list(estimator = x$meta$estimator, interval_method = x$meta$interval_method)
  } else {
    fn(x)
  }
  defaults <- list(
    measure = NULL, estimator = character(), interval_method = character(),
    tests = character(), corrections = character(), packages = character()
  )
  utils::modifyList(defaults, out[!vapply(out, is.null, logical(1))])
}

#' Record the estimators, intervals, tests, and corrections of a table result
#' @keywords internal
#' @noRd
.decisions_table <- function(x, ...) {
  homogeneity <- character()
  if (inherits(x, "simtab_table1")) {
    tests <- vapply(x$data, function(rec) {
      if (is.null(rec$test)) NA_character_ else rec$test$method %||% NA_character_
    }, character(1))
  } else {
    tests <- x$meta$stats$method %||% NA_character_
    mh <- x$data$mh
    if (is.data.frame(mh) && "homogeneity_method" %in% names(mh)) {
      homogeneity <- unique(stats::na.omit(mh$homogeneity_method))
    }
  }
  tests <- .clean_test_methods(unique(tests[!is.na(tests)]))
  out <- list(
    tests = c(tests, homogeneity),
    homogeneity = homogeneity,
    corrections = .decisions_zero_cell_corrections(x)
  )
  measure <- toupper(x$spec$effect$measure %||% x$meta$effect %||% "")
  if (!nzchar(measure)) {
    return(out)
  }
  out$measure <- measure
  function_decisions <- .methods_function_measure_decisions(x)
  if (!is.null(function_decisions)) {
    out$estimator <- function_decisions$estimator
    out$interval_method <- function_decisions$interval
    return(out)
  }
  estimator <- c(crude = .methods_crude_estimator(measure))
  interval <- c(crude = .methods_crude_interval(measure))
  mh <- x$data$mh
  if (is.data.frame(mh) && any(mh$row_type == "pooled")) {
    estimator[["pooled"]] <- sprintf("Mantel-Haenszel %s estimator", switch(
      measure,
      OR = "odds-ratio",
      PR = "prevalence-ratio",
      "risk-ratio"
    ))
    interval[["pooled"]] <- if (identical(measure, "OR")) {
      "Robins-Breslow-Greenland confidence intervals"
    } else {
      "Greenland-Robins confidence intervals"
    }
  }
  if (length(x$meta$adjust$vars %||% character()) > 0) {
    robust_wald <- "Wald confidence intervals (robust HC0 standard errors)"
    if (identical(measure, "OR")) {
      estimator[["adjusted_logistic"]] <- "logistic regression"
      interval[["adjusted_logistic"]] <- robust_wald
    }
    estimators <- x$meta$effect_estimators %||% character()
    fallback <- x$meta$logbinomial_fallback %||% logical()
    fallback_vars <- names(fallback)[as.logical(fallback)]
    if (measure %in% c("PR", "RR") && length(setdiff(names(estimators), fallback_vars)) > 0) {
      estimator[["adjusted_log_binomial"]] <- "log-binomial regression"
      interval[["adjusted_log_binomial"]] <- "Wald confidence intervals (model-based standard errors)"
    }
    if (measure %in% c("PR", "RR") && length(intersect(names(estimators), fallback_vars)) > 0) {
      estimator[["adjusted_modified_poisson"]] <- "modified Poisson regression"
      interval[["adjusted_modified_poisson"]] <- robust_wald
    }
  }
  out$estimator <- estimator
  out$interval_method <- interval
  out
}

#' Describe Haldane-Anscombe zero-cell corrections recorded on a table result
#' @keywords internal
#' @noRd
.decisions_zero_cell_corrections <- function(x) {
  notes <- .result_test_notes(x, "zero_cell_correction")
  vapply(notes, function(note) {
    sprintf(
      "Haldane-Anscombe +0.5 correction (%s: %s)",
      .resolve_label(note$variable %||% "a variable", x$used$ref$data, x$spec$fmt$labels),
      paste(note$levels, collapse = ", ")
    )
  }, character(1))
}

#########
# EFFECT ESTIMATION AND MODEL CLAUSES
# Synthesizes effect measure rationales, design origin, and log-binomial/Poisson fallback clauses.

#' Construct effect measure estimation methodology clause
#' @keywords internal
#' @noRd
.methods_effect_sentence <- function(x) {
  measure <- x$spec$effect$measure %||% x$meta$effect %||% NULL
  if (is.null(measure)) {
    return("")
  }
  measure <- toupper(measure)
  ci <- x$meta$conf_pct %||% round((x$spec$effect$conf.level %||% 0.95) * 100)
  decisions <- .result_decisions(x)
  if (!is.null(.methods_function_measure_decisions(x))) {
    return(sprintf(
      "%s was estimated using %s with %s %d%% confidence intervals.",
      measure,
      decisions$estimator,
      decisions$interval_method,
      ci
    ))
  }
  adjust_vars <- x$meta$adjust$vars %||% character()
  out <- if (length(adjust_vars) > 0) {
    .methods_adjusted_effect_sentence(x, measure, ci, adjust_vars)
  } else {
    sprintf(
      "%s was estimated using the %s with %d%% %s.%s",
      measure, decisions$estimator[["crude"]], ci, decisions$interval_method[["crude"]],
      .methods_design_rationale(x, measure)
    )
  }
  if ("pooled" %in% names(decisions$estimator)) {
    out <- paste0(out, sprintf(
      " Stratum-specific estimates across %s were pooled with the %s and %d%% %s.",
      .resolve_label(x$meta$strat_var_name, x$used$ref$data, x$spec$fmt$labels),
      decisions$estimator[["pooled"]], ci, decisions$interval_method[["pooled"]]
    ))
  }
  if (length(decisions$homogeneity) > 0) {
    out <- paste0(out, sprintf(" Homogeneity across strata was assessed with the %s.", decisions$homogeneity))
  }
  if (length(decisions$corrections) > 0) {
    out <- paste0(out, sprintf(
      " Zero cells were handled with a %s.",
      paste(decisions$corrections, collapse = "; ")
    ))
  }
  out
}

#' Name the crude 2x2 estimator used for an effect measure
#' @keywords internal
#' @noRd
.methods_crude_estimator <- function(measure) {
  switch(
    measure,
    OR = "Woolf/logit odds-ratio estimator",
    PR = "Katz log prevalence-ratio estimator",
    RR = "Katz log risk-ratio estimator",
    paste0(measure, " estimator")
  )
}

#' Name the crude 2x2 confidence-interval method for an effect measure
#' @keywords internal
#' @noRd
.methods_crude_interval <- function(measure) {
  switch(
    measure,
    OR = "Woolf logit confidence intervals",
    PR = ,
    RR = "Katz log confidence intervals",
    "confidence intervals"
  )
}

#' Sentence explaining a design-resolved effect measure, or ""
#' @keywords internal
#' @noRd
.methods_design_rationale <- function(x, measure) {
  design <- .result_design(x)
  if (identical(x$spec$effect$resolved_from, "design") && !is.null(design)) {
    return(sprintf(" The %s was chosen from the recorded study design (%s).", measure, .design_label(design)))
  }
  ""
}

#' Construct methods prose for crude plus covariate-adjusted effect columns
#'
#' A table with an adjusted column reports two estimands: the crude 2x2 ratio
#' and the model-adjusted ratio. Both, and the adjustment set, belong in the
#' methods.
#' @keywords internal
#' @noRd
.methods_adjusted_effect_sentence <- function(x, measure, ci, adjust_vars) {
  covar_labels <- vapply(
    adjust_vars,
    function(v) .resolve_label(v, x$used$ref$data, x$spec$fmt$labels),
    character(1)
  )
  covars <- if (length(covar_labels) == 1) {
    covar_labels
  } else {
    paste(paste(utils::head(covar_labels, -1), collapse = ", "), "and", utils::tail(covar_labels, 1))
  }
  decisions <- .result_decisions(x)
  crude <- sprintf(
    "Crude %s was estimated using the %s with %d%% %s.",
    measure, decisions$estimator[["crude"]], ci, decisions$interval_method[["crude"]]
  )
  adjusted <- if (measure %in% c("PR", "RR") && length(x$meta$effect_estimators %||% character()) > 0) {
    .methods_prrr_adjusted_sentence(x, measure, ci, covars)
  } else if (identical(measure, "OR")) {
    sprintf(
      "Adjusted OR was estimated by %s adjusting for %s, with %d%% %s.",
      decisions$estimator[["adjusted_logistic"]], covars, ci, decisions$interval_method[["adjusted_logistic"]]
    )
  } else {
    sprintf("Adjusted %s was estimated adjusting for %s, with %d%% confidence intervals.", measure, covars, ci)
  }
  paste0(crude, " ", adjusted, .methods_design_rationale(x, measure))
}

#' Extract custom effect measure and confidence interval metadata
#' @keywords internal
#' @noRd
.methods_function_measure_decisions <- function(x) {
  by_variable <- x$meta$effect_measure_meta %||% list()
  entries <- unlist(by_variable, recursive = FALSE, use.names = FALSE)
  entries <- entries[vapply(entries, is.list, logical(1))]
  if (length(entries) == 0) {
    return(NULL)
  }

  estimators <- unique(vapply(entries, function(entry) {
    entry$estimator %||% NA_character_
  }, character(1)))
  intervals <- unique(vapply(entries, function(entry) {
    entry$interval %||% NA_character_
  }, character(1)))
  estimators <- estimators[!is.na(estimators) & nzchar(estimators)]
  intervals <- intervals[!is.na(intervals) & nzchar(intervals)]
  if (length(estimators) == 0 || length(intervals) == 0) {
    return(NULL)
  }

  list(
    estimator = paste(estimators, collapse = "; "),
    interval = paste(intervals, collapse = "; ")
  )
}

#' Format comma-separated variable display labels for methods prose
#' @keywords internal
#' @noRd
.methods_effect_labels <- function(x, vars) {
  labels <- x$meta$labels[vars] %||% vars
  labels <- unname(ifelse(is.na(labels) | !nzchar(labels), vars, labels))
  paste(labels, collapse = ", ")
}

#' Construct methodology sentence for adjusted prevalence/risk ratio models and fallbacks
#' @keywords internal
#' @noRd
.methods_prrr_adjusted_sentence <- function(x, measure, ci, covars) {
  estimators <- x$meta$effect_estimators %||% character()
  fallbacks <- x$meta$logbinomial_fallback %||% logical()
  vars <- names(estimators)
  fallback_vars <- names(fallbacks)[as.logical(fallbacks)]
  fallback_vars <- intersect(fallback_vars, vars)
  logbin_vars <- setdiff(vars[estimators == "log-binomial"], fallback_vars)
  adjusted_for <- sprintf(" adjusting for %s", covars)
  intervals <- .result_decisions(x)$interval_method

  if (length(fallback_vars) == 0) {
    return(sprintf(
      "Adjusted %s was estimated by log-binomial regression%s, with %d%% %s.",
      measure,
      adjusted_for,
      ci,
      intervals[["adjusted_log_binomial"]]
    ))
  }

  # Methods prose must name the reason the fallback fired. A model that failed
  # to converge and one that could not be fitted at all are different findings,
  # and a manuscript should not report the second as the first.
  reason <- .logbinomial_reason_phrase(x, fallback_vars)

  if (length(logbin_vars) == 0) {
    return(sprintf(
      "Adjusted %s was estimated by modified Poisson regression with robust standard errors%s because %s for %s (Zou, 2004); %d%% %s were reported.",
      measure,
      adjusted_for,
      reason,
      .methods_effect_labels(x, fallback_vars),
      ci,
      intervals[["adjusted_modified_poisson"]]
    ))
  }

  sprintf(
    "Adjusted %s was estimated%s by log-binomial regression for %s and by modified Poisson regression with robust standard errors for %s because %s for those variable(s) (Zou, 2004); %d%% %s and %s were reported, respectively.",
    measure,
    adjusted_for,
    .methods_effect_labels(x, logbin_vars),
    .methods_effect_labels(x, fallback_vars),
    reason,
    ci,
    intervals[["adjusted_log_binomial"]],
    intervals[["adjusted_modified_poisson"]]
  )
}
