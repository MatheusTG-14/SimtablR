#' Diagnostic Test Accuracy Assessment
#'
#' Computes a 2x2 confusion matrix and comprehensive diagnostic performance
#' metrics for a binary classification test, with exact binomial confidence
#' intervals.
#'
#' @param data A data.frame containing `test` and `ref` variables.
#' @param test Unquoted name of the diagnostic test variable (must be binary).
#' @param ref Unquoted name of the reference standard variable (must be binary).
#' @param positive Character or numeric. Level representing "Positive" in the
#'   **reference** variable. If `NULL` (default), auto-detected from common
#'   positive labels (`"Yes"`, `"1"`, `"Positive"`, etc.) or the last level.
#' @param test_positive Character or numeric. Level representing "Positive" in
#'   the **test** variable. If `NULL` (default), mirrors `positive` when the
#'   same label exists in the test variable, then falls back to auto-detection.
#' @param conf.level Numeric. Confidence level for binomial CIs (0-1).
#'   Default: `0.95`.
#'
#' @details
#' ## Confusion Matrix Layout
#' ```
#'            | Ref +   | Ref -
#' -----------+---------+--------
#' Test +     |   TP    |   FP
#' Test -     |   FN    |   TN
#' ```
#'
#' ## Metrics Computed
#' * **Sensitivity** (Recall) = TP / (TP + FN)
#' * **Specificity** = TN / (TN + FP)
#' * **PPV** (Precision) = TP / (TP + FP)
#' * **NPV** = TN / (TN + FN)
#' * **Accuracy** = (TP + TN) / Total
#' * **Prevalence** = (TP + FN) / Total
#' * **Likelihood Ratio +** = Sensitivity / (1 - Specificity)
#' * **Likelihood Ratio -** = (1 - Sensitivity) / Specificity
#' * **Youden's Index** = Sensitivity + Specificity - 1
#' * **F1 Score** = 2 x (PPV x Sensitivity) / (PPV + Sensitivity)
#'
#' Binomial CIs (exact Clopper-Pearson) are computed for the first six metrics.
#' Likelihood Ratios, Youden's Index, and F1 Score do not have CIs.
#'
#' @return An object of class `diag_test` - a named list with:
#' * `$table`: 2x2 `table` object (Test x Ref).
#' * `$stats`: `data.frame` with columns `Metric`, `Estimate`, `LowerCI`,
#'   `UpperCI`.
#' * `$labels`: named list with `ref_pos`, `ref_neg`, `test_pos`, `test_neg`.
#' * `$sample_size`: integer, total valid observations.
#' * `$conf.level`: numeric, confidence level used.
#'
#' @seealso [print.diag_test()], [as.data.frame.diag_test()],
#'   [plot.diag_test()]
#'
#' @examples
#' set.seed(1)
#' n   <- 200
#' ref <- factor(sample(c("No", "Yes"), n, replace = TRUE, prob = c(.55, .45)))
#' tst <- ifelse(ref == "Yes",
#'               ifelse(runif(n) < .80, "Yes", "No"),
#'               ifelse(runif(n) < .85, "No",  "Yes"))
#' df  <- data.frame(rapid_test = factor(tst), lab = ref)
#'
#' result <- diag_test(df, test = rapid_test, ref = lab,
#'                     positive = "Yes", test_positive = "Yes")
#' print(result)
#' as.data.frame(result)
#'
#' @export
diag_test <- function(
    data,
    test,
    ref,
    positive      = NULL,
    test_positive = NULL,
    conf.level    = 0.95
) {
  #Args
  if (missing(data)) {
    stop("No data provided. Please supply a data.frame.", call. = FALSE)
  }
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame, not ", class(data)[1], ".", call. = FALSE)
  }
  if (nrow(data) == 0) {
    stop("'data' is empty (0 rows).", call. = FALSE)
  }
  if (!is.numeric(conf.level) || length(conf.level) != 1 ||
      conf.level <= 0 || conf.level >= 1) {
    stop("'conf.level' must be a single numeric value between 0 and 1.",
         call. = FALSE)
  }

  #  extrair Args da expressao
  test_expr <- substitute(test)
  ref_expr  <- substitute(ref)

  if (is.null(test_expr) || identical(test_expr, quote(expr = ))) {
    stop("'test' variable not specified.", call. = FALSE)
  }
  if (is.null(ref_expr) || identical(ref_expr, quote(expr = ))) {
    stop("'ref' variable not specified.", call. = FALSE)
  }

  val_test <- tryCatch(
    eval(test_expr, data, parent.frame()),
    error = function(e) stop(
      sprintf("'test' variable '%s' not found in data.", deparse(test_expr)),
      call. = FALSE
    )
  )
  val_ref <- tryCatch(
    eval(ref_expr, data, parent.frame()),
    error = function(e) stop(
      sprintf("'ref' variable '%s' not found in data.", deparse(ref_expr)),
      call. = FALSE
    )
  )

  if (length(val_test) != length(val_ref)) {
    stop(sprintf(
      "'test' and 'ref' have different lengths (%d vs %d).",
      length(val_test), length(val_ref)
    ), call. = FALSE)
  }

  #Remover NA
  ok        <- !is.na(val_test) & !is.na(val_ref)
  n_missing <- sum(!ok)

  if (n_missing > 0) {
    message(sprintf(
      "Removed %d observation(s) with missing values (%.1f%%).",
      n_missing, 100 * n_missing / length(val_test)
    ))
  }

  val_test <- val_test[ok]
  val_ref  <- val_ref[ok]

  if (length(val_test) == 0) {
    stop("No valid observations after removing missing values.", call. = FALSE)
  }

  #  Garantir factor
  if (!is.factor(val_test)) val_test <- factor(val_test)
  if (!is.factor(val_ref))  val_ref  <- factor(val_ref)

  levs_test <- levels(val_test)
  levs_ref  <- levels(val_ref)

  #  Necessario binario
  if (length(levs_test) != 2) {
    stop(sprintf(
      "'test' must have exactly 2 levels, but has %d: %s",
      length(levs_test), paste(levs_test, collapse = ", ")
    ), call. = FALSE)
  }
  if (length(levs_ref) != 2) {
    stop(sprintf(
      "'ref' must have exactly 2 levels, but has %d: %s",
      length(levs_ref), paste(levs_ref, collapse = ", ")
    ), call. = FALSE)
  }

  #Niveis positivos para teste
  .candidates_ref  <- c("1", "Sim", "Yes", "Positivo", "Positive",
                        "Doente", "Disease", "Case", "Event", "S", "Y",
                        "TRUE", "True")
  .candidates_test <- c("1", "Sim", "Yes", "Positivo", "Positive",
                        "Reagente", "Detected", "S", "Y", "TRUE", "True")

  pos_ref  <- .resolve_pos_level(positive,      levs_ref,  .candidates_ref,
                                 "reference", "positive")
  pos_test <- .resolve_pos_level_test(test_positive, levs_test, pos_ref,
                                      .candidates_test)

  neg_ref  <- setdiff(levs_ref,  pos_ref)[1]
  neg_test <- setdiff(levs_test, pos_test)[1]

  #  Build ordered confusion matrix
  val_ref_ord  <- factor(val_ref,  levels = c(pos_ref,  neg_ref))
  val_test_ord <- factor(val_test, levels = c(pos_test, neg_test))

  tab   <- table(Test = val_test_ord, Ref = val_ref_ord)
  TP    <- tab[1L, 1L]
  FP    <- tab[1L, 2L]
  FN    <- tab[2L, 1L]
  TN    <- tab[2L, 2L]
  Total <- sum(tab)

  #  Sanity warnings
  if (TP + FN == 0L) warning("No positive cases in reference (TP + FN = 0).", call. = FALSE)
  if (TN + FP == 0L) warning("No negative cases in reference (TN + FP = 0).", call. = FALSE)
  if (TP + FP == 0L) warning("No positive test results (TP + FP = 0).",       call. = FALSE)
  if (TN + FN == 0L) warning("No negative test results (TN + FN = 0).",       call. = FALSE)

  #  Compute metrics
  # Exact Clopper-Pearson CI via binom.test() for proportions.
  .ci <- function(x, n) {
    if (n == 0L) return(c(NA_real_, NA_real_, NA_real_))
    ci <- binom.test(x, n, conf.level = conf.level)$conf.int
    c(x / n, ci[1L], ci[2L])
  }

  sens <- .ci(TP,      TP + FN)
  spec <- .ci(TN,      TN + FP)
  ppv  <- .ci(TP,      TP + FP)
  npv  <- .ci(TN,      TN + FN)
  acc  <- .ci(TP + TN, Total)
  prev <- .ci(TP + FN, Total)

  lr_pos <- if (!is.na(sens[1L]) && !is.na(spec[1L]) && (1 - spec[1L]) > 0)
    sens[1L] / (1 - spec[1L]) else NA_real_

  lr_neg <- if (!is.na(sens[1L]) && !is.na(spec[1L]) && spec[1L] > 0)
    (1 - sens[1L]) / spec[1L] else NA_real_

  youden <- if (!is.na(sens[1L]) && !is.na(spec[1L]))
    sens[1L] + spec[1L] - 1 else NA_real_

  f1 <- if (!is.na(ppv[1L]) && !is.na(sens[1L]) && (ppv[1L] + sens[1L]) > 0)
    2 * (ppv[1L] * sens[1L]) / (ppv[1L] + sens[1L]) else NA_real_

  #  Assemble stats data.frame
  stats_df <- data.frame(
    Metric = c(
      "Sensitivity", "Specificity",
      "Pos Pred Value (PPV)", "Neg Pred Value (NPV)",
      "Accuracy",             "Prevalence",
      "Likelihood Ratio +",   "Likelihood Ratio -",
      "Youden Index",         "F1 Score"
    ),
    Estimate = c(
      sens[1L], spec[1L], ppv[1L], npv[1L], acc[1L], prev[1L],
      lr_pos, lr_neg, youden, f1
    ),
    LowerCI = c(
      sens[2L], spec[2L], ppv[2L], npv[2L], acc[2L], prev[2L],
      NA_real_, NA_real_, NA_real_, NA_real_
    ),
    UpperCI = c(
      sens[3L], spec[3L], ppv[3L], npv[3L], acc[3L], prev[3L],
      NA_real_, NA_real_, NA_real_, NA_real_
    ),
    stringsAsFactors = FALSE
  )

  #  Resultado estruturado
  structure(
    list(
      table       = tab,
      stats       = stats_df,
      labels      = list(
        ref_pos  = pos_ref,  ref_neg  = neg_ref,
        test_pos = pos_test, test_neg = neg_test
      ),
      sample_size = Total,
      conf.level  = conf.level
    ),
    class = "diag_test"
  )
}

# Resolve o nivel positivo para a variavel de referencia.
# Lida com: NULL (automatica), correspondencia de caracteres, indice numerico.
.resolve_pos_level <- function(value, levs, candidates, var_label, arg_name) {
  # NULL para auto-detect
  if (is.null(value)) {
    matched <- intersect(levs, candidates)
    if (length(matched) > 0L) {
      message(sprintf(
        "Auto-detected %s positive level: '%s'", var_label, matched[1L]
      ))
      return(matched[1L])
    }
    last <- levs[length(levs)]
    message(sprintf(
      "Using last %s level as positive: '%s'. Specify '%s' if incorrect.",
      var_label, last, arg_name
    ))
    return(last)
  }

  value_chr <- as.character(value)

  if (value_chr %in% levs) return(value_chr)

  #1 ou 2
  idx <- suppressWarnings(as.integer(value))
  if (!is.na(idx) && idx >= 1L && idx <= length(levs)) {
    message(sprintf(
      "Using %s level %d as positive: '%s'", var_label, idx, levs[idx]
    ))
    return(levs[idx])
  }

  stop(sprintf(
    "Positive level '%s' not found in %s levels: %s",
    value_chr, var_label, paste(levs, collapse = ", ")
  ), call. = FALSE)
}

.resolve_pos_level_test <- function(value, levs_test, pos_ref, candidates) {
  if (is.null(value)) {

    if (pos_ref %in% levs_test) return(pos_ref)
    matched <- intersect(levs_test, candidates)
    if (length(matched) > 0L) {
      message(sprintf("Auto-detected test positive level: '%s'", matched[1L]))
      return(matched[1L])
    }

    stop(sprintf(paste0(
      "Cannot auto-detect test positive level.\n",
      "Reference uses '%s', but this label is not in test levels: %s\n",
      "Please specify 'test_positive'."
    ), pos_ref, paste(levs_test, collapse = ", ")), call. = FALSE)
  }

  value_chr <- as.character(value)
  if (value_chr %in% levs_test) return(value_chr)

  idx <- suppressWarnings(as.integer(value))
  if (!is.na(idx) && idx >= 1L && idx <= length(levs_test)) {
    message(sprintf(
      "Using test level %d as positive: '%s'", idx, levs_test[idx]
    ))
    return(levs_test[idx])
  }

  stop(sprintf(
    "Test positive level '%s' not found in test levels: %s",
    value_chr, paste(levs_test, collapse = ", ")
  ), call. = FALSE)
}


#  Print

#' Print Method for diag_test Objects
#'
#' Displays a formatted summary of the confusion matrix and all diagnostic
#' performance metrics with confidence intervals.
#'
#' @param x A `diag_test` object.
#' @param digits Integer. Decimal places for metrics. Default: `3`.
#' @param ... Additional arguments (unused).
#'
#' @return Invisibly returns `x`.
#' @export
print.diag_test <- function(x, digits = 3L, ...) {
  if (!is.numeric(digits) || length(digits) != 1L || digits < 0) digits <- 3L
  digits <- as.integer(digits)

  sep_major <- strrep("=", 60L)
  sep_minor <- strrep("-", 60L)
  ci_label  <- sprintf("%.0f%%", x$conf.level * 100)

  #  Header
  cat("\n", sep_major, "\n", sep = "")
  cat("  DIAGNOSTIC TEST EVALUATION\n")
  cat(sep_major, "\n\n", sep = "")

  cat(sprintf("  Sample size      : %d\n",   x$sample_size))
  cat(sprintf("  Confidence level : %s\n\n", ci_label))

  cat("  Reference standard (gold standard):\n")
  cat(sprintf("    Positive = '%s'   |   Negative = '%s'\n\n",
              x$labels$ref_pos, x$labels$ref_neg))

  cat("  Diagnostic test:\n")
  cat(sprintf("    Positive = '%s'   |   Negative = '%s'\n\n",
              x$labels$test_pos, x$labels$test_neg))

  #  Confusion matrix
  cat(sep_minor, "\n  Confusion Matrix\n", sep_minor, "\n", sep = "")
  print(x$table)
  cat("\n")

  #  Performance metrics
  cat(sep_major, "\n", sep = "")
  cat(sprintf("  Performance Metrics  (%s CI)\n", ci_label))
  cat(sep_major, "\n", sep = "")

  df        <- x$stats
  pad_width <- max(nchar(df$Metric))

  fmt_ci <- function(est, low, upp) {
    if (is.na(est)) return("-")
    fmt <- paste0("%.", digits, "f")
    if (is.na(low)) {
      sprintf(fmt, est)
    } else {
      sprintf(paste0(fmt, "  (%s - %s)"),
              est,
              sprintf(fmt, low),
              sprintf(fmt, upp))
    }
  }

  formatted <- mapply(fmt_ci, df$Estimate, df$LowerCI, df$UpperCI,
                      SIMPLIFY = TRUE)

  for (i in seq_len(nrow(df))) {
    if (i == 7L) cat(sep_minor, "\n", sep = "")
    pad <- strrep(" ", pad_width - nchar(df$Metric[i]) + 2L)
    cat(df$Metric[i], pad, ":  ", formatted[i], "\n", sep = "")
  }
  cat("\n")

  invisible(x)
}


#  as.data.frame method

#' Convert diag_test to Data Frame
#'
#' Extracts the performance metrics table as a plain `data.frame`.
#'
#' @param x A `diag_test` object.
#' @param ... Additional arguments (unused).
#'
#' @return A `data.frame` with columns `Metric`, `Estimate`, `LowerCI`,
#'   `UpperCI`.
#' @export
as.data.frame.diag_test <- function(x, ...) x$stats


#  plotar matriz de confusao

#' Plot Diagnostic Test Results
#'
#' Draws a fourfold display of the confusion matrix with sensitivity and
#' specificity annotated on the bottom margin.
#'
#' @param x A `diag_test` object.
#' @param col Character vector of length 2. Fill colours for the negative and
#'   positive quadrants respectively. Default: `c("#ffcccc", "#ccffcc")`.
#' @param main Character. Plot title. Default: `"Confusion Matrix"`.
#' @param ... Additional arguments passed to [graphics::fourfoldplot()].
#'
#' @return Invisibly returns `x`.
#' @export
plot.diag_test <- function(
    x,
    col  = c("#ffcccc", "#ccffcc"),
    main = "Confusion Matrix",
    ...
) {
  sens <- round(x$stats$Estimate[x$stats$Metric == "Sensitivity"], 2L)
  spec <- round(x$stats$Estimate[x$stats$Metric == "Specificity"],          2L)

  graphics::fourfoldplot(
    x$table,
    color      = col,
    conf.level = 0,
    margin     = 1L,
    main       = main,
    ...
  )
  graphics::mtext(
    sprintf("Sensitivity: %.2f   |   Specificity: %.2f", sens, spec),
    side = 1L, line = 1L
  )

  invisible(x)
}
