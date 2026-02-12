#' Diagnostic Test Accuracy Assessment
#'
#' Computes a comprehensive confusion matrix and diagnostic performance metrics
#' for binary classification tests.
#'
#' @param data A data.frame containing the test and reference variables.
#' @param test Unquoted name of the diagnostic test variable (binary).
#' @param ref Unquoted name of the reference standard variable (binary).
#' @param positive Character or numeric. Level representing "Positive" in REFERENCE.
#' @param test_positive Character or numeric. Level representing "Positive" in TEST.
#' @param conf.level Numeric. Confidence level (0-1). Default: 0.95.
#'
#' @details
#' **Confusion Matrix Structure**
#' The function creates a 2x2 confusion matrix:
#' * TP: True Positives
#' * TN: True Negatives
#' * FP: False Positives
#' * FN: False Negatives
#'
#' **Metrics Calculated**
#' * Sensitivity, Specificity, PPV, NPV
#' * Accuracy, Prevalence
#' * Likelihood Ratios, Youden's Index, F1 Score
#'
#' @return An object of class \code{diag_test} containing:
#' * \code{table}: 2x2 confusion matrix
#' * \code{stats}: Data frame with metrics and CIs
#' * \code{labels}: List with labels used
#' * \code{sample_size}: Total valid observations
#'
#' @export

diag_test <- function(data, test, ref, positive = NULL, test_positive = NULL,
                      conf.level = 0.95) {

  #verificar que args são válidos
  if (missing(data)) {
    stop("No data provided. Please supply a data.frame.", call. = FALSE)
  }
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame, not ", class(data)[1], ".", call. = FALSE)
  }
  if (nrow(data) == 0) {
    stop("'data' is empty (0 rows).", call. = FALSE)
  }

  # Validate confidence level
  if (!is.numeric(conf.level) || length(conf.level) != 1) {
    stop("'conf.level' must be a single numeric value.", call. = FALSE)
  }
  if (conf.level <= 0 || conf.level >= 1) {
    stop("'conf.level' must be between 0 and 1 (e.g., 0.95 for 95%).", call. = FALSE)
  }

  test_expr <- substitute(test)
  ref_expr <- substitute(ref)

  if (is.null(test_expr) || identical(test_expr, quote(expr = ))) {
    stop("'test' variable not specified.", call. = FALSE)
  }
  if (is.null(ref_expr) || identical(ref_expr, quote(expr = ))) {
    stop("'ref' (reference) variable not specified.", call. = FALSE)
  }

  val_test <- tryCatch(
    eval(test_expr, data, parent.frame()),
    error = function(e) {
      stop(sprintf("'test' variable not found in data: %s",
                   deparse(test_expr)), call. = FALSE)
    }
  )
  val_ref <- tryCatch(
    eval(ref_expr, data, parent.frame()),
    error = function(e) {
      stop(sprintf("'ref' (reference) variable not found in data: %s",
                   deparse(ref_expr)), call. = FALSE)
    }
  )

  if (length(val_test) != length(val_ref)) {
    stop(sprintf("'test' and 'ref' have different lengths (%d vs %d).",
                 length(val_test), length(val_ref)), call. = FALSE)
  }

  ok <- !is.na(val_test) & !is.na(val_ref) #NÃO TIRAR (quebra todo o resto do codigo)
  n_missing <- sum(!ok)

  if (n_missing > 0) {
    message(sprintf("Removed %d observation(s) with missing values (%.1f%%).",
                    n_missing, 100 * n_missing / length(val_test)))
  }

  val_test <- val_test[ok]
  val_ref  <- val_ref[ok]

  if (length(val_test) == 0) {
    stop("No valid observations after removing missing values.", call. = FALSE)
  }

  if (!is.factor(val_test)) val_test <- factor(val_test)
  if (!is.factor(val_ref))  val_ref  <- factor(val_ref)

  levs_test <- levels(val_test)
  levs_ref  <- levels(val_ref)

  # Validate binary
  if (length(levs_test) != 2) {
    stop(sprintf("'test' must be binary (2 levels), but has %d: %s",
                 length(levs_test), paste(levs_test, collapse = ", ")), call. = FALSE)
  }
  if (length(levs_ref) != 2) {
    stop(sprintf("'ref' must be binary (2 levels), but has %d: %s",
                 length(levs_ref), paste(levs_ref, collapse = ", ")), call. = FALSE)
  }

  ## definir referencia
  pos_ref <- positive
#tenta advinhar o valor de referencia positivo
  # se enontrar, utiliza como padrão e printa no console
  if (is.null(pos_ref)) {
    candidates <- c("1", "Sim", "Yes", "Positivo", "Positive", "Doente",
                    "Disease", "Case", "Event", "S", "Y", "TRUE", "True")
    match_ref <- intersect(levs_ref, candidates)

    if (length(match_ref) > 0) {
      pos_ref <- match_ref[1]
      message(sprintf("Auto-detected reference positive level: '%s'", pos_ref))
    } else {
      pos_ref <- levs_ref[length(levs_ref)]
      message(sprintf("Using last reference level as positive: '%s'. Specify 'positive' if incorrect.",
                      pos_ref))
    }
  }

  if (!pos_ref %in% levs_ref) {
    if (is.numeric(positive) && positive %in% 1:2) {
      pos_ref <- levs_ref[positive]
      message(sprintf("Using reference level %d as positive: '%s'", positive, pos_ref))
    } else {
      stop(sprintf("Positive level '%s' not found in reference levels: %s",
                   pos_ref, paste(levs_ref, collapse = ", ")), call. = FALSE)
    }
  }

  ## Test Positive
  pos_test <- test_positive

  if (is.null(pos_test)) {
    if (pos_ref %in% levs_test) {
      pos_test <- pos_ref
    } else {
      candidates <- c("1", "Sim", "Yes", "Positivo", "Positive", "Reagente",
                      "Detected", "S", "Y", "TRUE", "True")
      match_test <- intersect(levs_test, candidates)

      if (length(match_test) > 0) {
        pos_test <- match_test[1]
        message(sprintf("Auto-detected test positive level: '%s'", pos_test))
      } else {
        stop(sprintf(paste0(
          "Cannot auto-detect test positive level.\n",
          "Reference uses '%s', but this is not in test levels: %s\n",
          "Please specify 'test_positive' argument."),
          pos_ref, paste(levs_test, collapse = ", ")), call. = FALSE)
      }
    }
  }

  if (!pos_test %in% levs_test) {
    if (is.numeric(test_positive) && test_positive %in% 1:2) {
      pos_test <- levs_test[test_positive]
      message(sprintf("Using test level %d as positive: '%s'", test_positive, pos_test))
    } else {
      stop(sprintf("Test positive level '%s' not found in test levels: %s",
                   pos_test, paste(levs_test, collapse = ", ")), call. = FALSE)
    }
  }

  neg_ref  <- setdiff(levs_ref, pos_ref)[1]
  neg_test <- setdiff(levs_test, pos_test)[1]

  val_ref_ordered  <- factor(val_ref, levels = c(pos_ref, neg_ref))
  val_test_ordered <- factor(val_test, levels = c(pos_test, neg_test))

  tab <- table(Test = val_test_ordered, Ref = val_ref_ordered)
#inverter
  TP <- tab[1, 1]; FP <- tab[1, 2]
  FN <- tab[2, 1]; TN <- tab[2, 2]
  Total <- sum(tab)

  # Catch erros de NA
  if (TP + FN == 0) warning("No positive cases in reference (TP + FN = 0).", call. = FALSE)
  if (TN + FP == 0) warning("No negative cases in reference (TN + FP = 0).", call. = FALSE)
  if (TP + FP == 0) warning("No positive test results (TP + FP = 0).", call. = FALSE)
  if (TN + FN == 0) warning("No negative test results (TN + FN = 0).", call. = FALSE)

  get_ci <- function(x, n) {
    if (n == 0) return(c(NA_real_, NA_real_, NA_real_))
    ci <- binom.test(x, n, conf.level = conf.level)$conf.int
    return(c(x / n, ci[1], ci[2]))
  }

  sens <- get_ci(TP, TP + FN)
  spec <- get_ci(TN, TN + FP)
  ppv  <- get_ci(TP, TP + FP)
  npv  <- get_ci(TN, TN + FN)
  acc  <- get_ci(TP + TN, Total)
  prev <- get_ci(TP + FN, Total)

  lr_pos <- NA_real_; lr_neg <- NA_real_
  if (!is.na(sens[1]) && !is.na(spec[1])) {
    if ((1 - spec[1]) > 0) lr_pos <- sens[1] / (1 - spec[1])
    if (spec[1] > 0)       lr_neg <- (1 - sens[1]) / spec[1]
  }

  youden <- NA_real_
  if (!is.na(sens[1]) && !is.na(spec[1])) youden <- sens[1] + spec[1] - 1

  f1 <- NA_real_
  if (!is.na(ppv[1]) && !is.na(sens[1]) && (ppv[1] + sens[1]) > 0) {
    f1 <- 2 * (ppv[1] * sens[1]) / (ppv[1] + sens[1])
  }

  stats_df <- data.frame(
    Metric = c("Sensitivity (Recall)", "Specificity", "Pos Pred Value (PPV)",
               "Neg Pred Value (NPV)", "Accuracy", "Prevalence",
               "Likelihood Ratio +", "Likelihood Ratio -", "Youden Index", "F1 Score"),
    Estimate = c(sens[1], spec[1], ppv[1], npv[1], acc[1], prev[1],
                 lr_pos, lr_neg, youden, f1),
    LowerCI = c(sens[2], spec[2], ppv[2], npv[2], acc[2], prev[2],
                NA, NA, NA, NA),
    UpperCI = c(sens[3], spec[3], ppv[3], npv[3], acc[3], prev[3],
                NA, NA, NA, NA),
    stringsAsFactors = FALSE
  )

  res <- list(
    table = tab,
    stats = stats_df,
    labels = list(ref_pos = pos_ref, ref_neg = neg_ref,
                  test_pos = pos_test, test_neg = neg_test),
    sample_size = Total,
    conf.level = conf.level
  )

  class(res) <- "diag_test"
  return(res)
}

# ================
# FORMATAR OUTPUT
# ================

#' Print Method for diag_test Objects
#'
#' @param x A diag_test object
#' @param digits Number of decimal places for metrics (default: 3)
#' @param ... Additional arguments (unused)
#' @return No return value, called for side effects
#'
#' @export
print.diag_test <- function(x, digits = 3, ...) {

  if (!is.numeric(digits) || digits < 0) digits <- 3
  digits <- as.integer(digits)

  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("  DIAGNOSTIC TEST EVALUATION\n")
  cat(strrep("=", 60), "\n\n", sep = "")

  cat("Sample Size:      ", x$sample_size, "\n")
  cat("Confidence Level: ", sprintf("%.0f%%", x$conf.level * 100), "\n\n")

  cat("Reference Standard (Gold Standard):\n")
  cat("  Positive: '", x$labels$ref_pos, "'  |  Negative: '",
      x$labels$ref_neg, "'\n\n", sep = "")

  cat("Diagnostic Test:\n")
  cat("  Positive: '", x$labels$test_pos, "'  |  Negative: '",
      x$labels$test_neg, "'\n\n", sep = "")

  cat(strrep("-", 60), "\n", sep = "")
  cat("Confusion Matrix:\n")
  cat(strrep("-", 60), "\n", sep = "")
  print(x$table)
  cat("\n")

  cat(strrep("=", 60), "\n", sep = "")
  cat("Performance Metrics (with ", sprintf("%.0f%%", x$conf.level * 100), " CI):\n", sep = "")
  cat(strrep("=", 60), "\n", sep = "")

  df_print <- x$stats

  fmt_row <- function(est, low, upp) {
    if (is.na(est)) return("NA")
    if (is.na(low)) {
      return(sprintf(paste0("%.", digits, "f"), est))
    } else {
      return(sprintf(paste0("%.", digits, "f (%.", digits, "f - %.", digits, "f)"),
                     est, low, upp))
    }
  }

  formatted <- mapply(fmt_row, df_print$Estimate, df_print$LowerCI, df_print$UpperCI)
  max_char <- max(nchar(df_print$Metric))

  for (i in 1:nrow(df_print)) {
    if (i == 7) cat(strrep("-", 60), "\n", sep = "")
    pad <- paste(rep(" ", max_char - nchar(df_print$Metric[i]) + 2), collapse = "")
    cat(df_print$Metric[i], pad, ": ", formatted[i], "\n", sep = "")
  }
  cat("\n")
  invisible(x)
}

# ============================================================================
# DATA.FRAME
# ============================================================================

#' Convert diag_test Object to Data Frame
#'
#' @param x A diag_test object
#' @param ... Additional arguments (unused)
#'
#' @return A data.frame containing the performance metrics
#'
#' @export
as.data.frame.diag_test <- function(x, ...) {
  return(x$stats)
}

#' Plot Diagnostic Test Results
#'
#' Visualizes the confusion matrix using a fourfold plot.
#'
#' @param x A diag_test object.
#' @param col Vector of 2 colors for Negative and Positive outcomes.
#' @param main Title of the plot.
#' @param ... Additional arguments passed to \code{fourfoldplot}.
#' @return No return value, called for side effects
#'
#' @export
#teste
plot.diag_test <- function(x, col = c("#ffcccc", "#ccffcc"), main = "Confusion Matrix", ...) {
  tbl <- x$table
  graphics::fourfoldplot(tbl, color = col, conf.level = 0, margin = 1, main = main, ...)
  graphics::mtext(paste("Sens:", round(x$stats$Estimate[1], 2),
                        " Spec:", round(x$stats$Estimate[2], 2)), side = 1, line = 1)
}
