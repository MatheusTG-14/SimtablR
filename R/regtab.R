#' Multi-Outcome Regression Table
#'
#' Fits generalized linear models (GLMs) for multiple outcome variables and generates
#' a formatted wide-format table with point estimates and confidence intervals.
#' Supports robust standard errors, automatic exponentiation for count/binary outcomes,
#' and custom labeling for publication-ready tables.
#'
#' @param data Data.frame containing all variables for analysis.
#' @param outcomes Character vector of dependent variable names. Each outcome is
#'   modeled separately with the same set of predictors.
#' @param predictors Formula or character string specifying predictors. Can be:
#'   \itemize{
#'     \item Formula: \code{~ x1 + x2 + x3}
#'     \item Character: \code{"~ x1 + x2 + x3"} or \code{"x1 + x2 + x3"}
#'   }
#' @param family GLM family specification. Options:
#'   \itemize{
#'     \item \code{poisson(link = "log")} - For count outcomes (default)
#'     \item \code{binomial(link = "logit")} - For binary outcomes
#'     \item \code{gaussian(link = "identity")} - For continuous outcomes
#'     \item \code{quasipoisson()}, \code{quasibinomial()} - For overdispersed data
#'     \item Or character: "poisson", "binomial", "gaussian"
#'   }
#' @param robust Logical. If TRUE (default), calculates heteroskedasticity-consistent
#'   (HC0) robust standard errors via the sandwich package. CIs are based on robust SEs.
#' @param exponentiate Logical. If TRUE, exponentiates coefficients and CIs:
#'   \itemize{
#'     \item Poisson: IRR (Incidence Rate Ratios)
#'     \item Binomial: OR (Odds Ratios)
#'     \item Gaussian: Not typically used (stays on linear scale)
#'   }
#'   If NULL (default), automatically detects: TRUE for Poisson/Binomial,
#'   FALSE for Gaussian.
#' @param labels Named character vector for renaming outcome columns in output.
#'   Format: \code{c("raw_name" = "Pretty Label")}. Useful for publication tables.
#' @param d Integer. Number of decimal places for rounding estimates and CIs. Default: 2.
#' @param conf.level Numeric. Confidence level for intervals (0-1). Default: 0.95.
#' @param include_intercept Logical. If TRUE, includes intercept in output table.
#'   Default: FALSE (typically excluded from publication tables).
#' @param p_values Logical. If TRUE, adds p-values as separate column. Default: FALSE.
#'
#' @details
#' ## Model Fitting
#' For each outcome, the function fits:
#' \code{glm(outcome ~ predictors, family = family, data = data)}
#'
#' ## Robust Standard Errors
#' When \code{robust = TRUE}, the function:
#' 1. Fits the model with standard GLM.
#' 2. Computes sandwich covariance matrix (HC0 estimator).
#' 3. Calculates Wald-type CIs based on robust SEs.
#'
#' This provides protection against heteroskedasticity and mild model misspecification.
#'
#' ## Exponentiation
#' * **Poisson regression**: exp(beta) = Incidence Rate Ratio
#'     * IRR = 1: No association
#'     * IRR > 1: Increased rate
#'     * IRR < 1: Decreased rate
#' * **Logistic regression**: exp(beta) = Odds Ratio
#'     * OR = 1: No association
#'     * OR > 1: Increased odds
#'     * OR < 1: Decreased odds
#'
#' ## Output Format
#' Returns a wide-format data.frame:
#' \preformatted{
#' Variable    | Outcome1          | Outcome2          | ...
#' ------------|-------------------|-------------------|----
#' (Intercept) | 2.34 (1.89-2.91) | 1.98 (1.65-2.38) | ...
#' age         | 1.05 (1.02-1.08) | 1.03 (1.01-1.06) | ...
#' sex         | 0.87 (0.75-1.01) | 0.92 (0.81-1.05) | ...
#' }
#' Each cell contains: "Estimate (Lower CI - Upper CI)"
#'
#' ## Missing Data
#' GLM uses complete cases by default. Observations with missing values in any
#' variable are excluded from that specific model.
#'
#' ## Convergence Issues
#' If a model fails to converge or encounters errors:
#' * A warning is issued with the outcome name and error message
#' * That outcome column is skipped in the output
#' * Other outcomes continue processing
#'
#' @return A data.frame in wide format with:
#' * **Variable**: Predictor names (first column)
#' * **Outcome columns**: One column per outcome with formatted estimates and CIs
#'
#' Can be directly exported to Excel, Word, or LaTeX for publication.
#'
#' @examples
#' # Create example data
#' set.seed(456)
#' n <- 500
#' df <- data.frame(
#'   age = rnorm(n, 50, 10),
#'   sex = factor(sample(c("M", "F"), n, replace = TRUE)),
#'   treatment = factor(sample(c("A", "B"), n, replace = TRUE)),
#'   outcome1 = rpois(n, lambda = 5),
#'   outcome2 = rpois(n, lambda = 8),
#'   outcome3 = rpois(n, lambda = 3)
#' )
#'
#' # Basic usage: Poisson regression for multiple outcomes
#' regtab(df,
#'        outcomes = c("outcome1", "outcome2", "outcome3"),
#'        predictors = ~ age + sex + treatment,
#'        family = poisson(link = "log"))
#'
#' # With custom labels and no robust SEs
#' regtab(df,
#'        outcomes = c("outcome1", "outcome2"),
#'        predictors = "age + sex",
#'        labels = c(outcome1 = "Primary Endpoint", outcome2 = "Secondary Endpoint"),
#'        robust = FALSE)
#'
#' # Logistic regression with p-values
#' df$binary_outcome <- rbinom(n, 1, 0.4)
#' regtab(df,
#'        outcomes = "binary_outcome",
#'        predictors = ~ age + sex,
#'        family = binomial(),
#'        p_values = TRUE)
#'
#' @export

#FUNCIONA EM 3 PARTES:
#A função vai iterar sobre cada variável de "outcomes",
#assumindo o primeiro valor como referencia sempre,
#e depois extrair os coeficientes e z para calcular RR e IC 95.
#Por fim, formata os valores e organiza a tabela para exportar.
regtab <- function(
    data,
    outcomes,
    predictors,
    family            = poisson(link = "log"),
    robust            = TRUE,
    exponentiate      = NULL,
    labels            = NULL,
    d                 = 2,
    conf.level        = 0.95,
    include_intercept = FALSE,
    p_values          = FALSE
) {
  # ── Pacotes necessários ───────────────────────────────────────────────
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required. Install with: install.packages('dplyr')",
         call. = FALSE)
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("Package 'tidyr' is required. Install with: install.packages('tidyr')",
         call. = FALSE)
  }
  if (robust) {
    if (!requireNamespace("sandwich", quietly = TRUE)) {
      stop("Package 'sandwich' is required for robust SEs.", call. = FALSE)
    }
    if (!requireNamespace("lmtest", quietly = TRUE)) {
      stop("Package 'lmtest' is required for robust SEs.", call. = FALSE)
    }
  }

  # ── Inputs corretos ───────────────────────────────────────────────────────
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame, not ", class(data)[1], ".", call. = FALSE)
  }
  if (nrow(data) == 0) {
    stop("'data' is empty (0 rows).", call. = FALSE)
  }
  if (!is.character(outcomes) || length(outcomes) == 0) {
    stop("'outcomes' must be a non-empty character vector.", call. = FALSE)
  }

  missing_outcomes <- setdiff(outcomes, names(data))
  if (length(missing_outcomes) > 0) {
    stop(
      "Outcome(s) not found in data: ",
      paste(missing_outcomes, collapse = ", "),
      call. = FALSE
    )
  }
  if (!is.numeric(d) || d < 0 || d > 10) {
    stop("'d' must be a number between 0 and 10.", call. = FALSE)
  }
  if (!is.numeric(conf.level) || conf.level <= 0 || conf.level >= 1) {
    stop("'conf.level' must be between 0 and 1 (e.g., 0.95).", call. = FALSE)
  }

  d <- as.integer(d)

  # ── Exponeciação necessária? (poisson/binomial) ────────────────────────────────────────────
  if (is.null(exponentiate)) {
    fam_name     <- if (is.character(family)) family else family$family
    exponentiate <- fam_name %in%
      c("poisson", "binomial", "quasipoisson", "quasibinomial")
    if (exponentiate) {
      message(sprintf(
        "Auto-detected family '%s': Coefficients will be exponentiated.",
        fam_name
      ))
    }
  }

  # ── Fórmula ───────────────────────────────────────────────
  form_rhs <- if (is.character(predictors)) {
    if (!grepl("~", predictors, fixed = TRUE)) {
      predictors <- paste("~", predictors)
    }
    as.formula(predictors)
  } else if (inherits(predictors, "formula")) {
    predictors
  } else {
    stop("'predictors' must be a formula or character string.", call. = FALSE)
  }

  # ── Formatação ─────────────────────────────────────────────────────
  fmt <- function(x) format(round(x, d), nsmall = d, trim = TRUE)

  # ── 1 model por outcome  ──────
  results_list <- list()
  n_success    <- 0L
  n_failed     <- 0L

  for (outcome in outcomes) {
    full_formula <- update(form_rhs, paste(outcome, "~ ."))

    res_df <- tryCatch({
      model <- glm(full_formula, family = family, data = data)

      if (!model$converged) {
        warning(
          sprintf("Model for '%s' did not converge. Results may be unreliable.", outcome),
          call. = FALSE, immediate. = TRUE
        )
      }

      coefs    <- coef(model)
      vcov_mat <- NULL

      if (robust) {
        vcov_mat <- sandwich::vcovHC(model, type = "HC0")
        cis      <- lmtest::coefci(model, vcov. = vcov_mat, level = conf.level)
      } else {
        cis <- suppressMessages(confint(model, level = conf.level))
      }

      if (exponentiate) {
        ests <- exp(coefs)
        cis  <- exp(cis)
      } else {
        ests <- coefs
      }

      p_vals <- NULL
      if (p_values) {
        if (robust && !is.null(vcov_mat)) {
          se_robust <- sqrt(diag(vcov_mat))
          z_stats   <- coefs / se_robust
          p_vals    <- 2 * pnorm(-abs(z_stats))
        } else {

          coef_table <- summary(model)$coefficients
          p_col      <- intersect(
            c("Pr(>|z|)", "Pr(>|t|)"),
            colnames(coef_table)
          )[1]
          p_vals <- coef_table[, p_col]
        }
      }

      combined <- paste0(
        fmt(ests), " (",
        fmt(cis[, 1L]), " - ",
        fmt(cis[, 2L]), ")"
      )

      result_df <- data.frame(
        Variable = names(ests),
        Result   = combined,
        Outcome  = outcome,
        stringsAsFactors = FALSE
      )

      if (!is.null(p_vals)) {
        result_df$P_Value <- ifelse(
          p_vals < 0.001, "<0.001",
          sprintf(paste0("%.", max(3L, d), "f"), p_vals)
        )
      }

      n_success <<- n_success + 1L
      result_df

    }, error = function(e) {

      warning(
        sprintf("Model fitting failed for outcome '%s': %s", outcome, e$message),
        call. = FALSE, immediate. = TRUE
      )
      n_failed <<- n_failed + 1L
      NULL
    })

    if (!is.null(res_df)) {
      results_list[[outcome]] <- res_df
    }
  }

  if (length(results_list) == 0) {
    stop("All models failed to fit. Check your data and model specification.",
         call. = FALSE)
  }
  if (n_failed > 0) {
    message(sprintf(
      "Successfully fit %d/%d models. %d failed.",
      n_success, length(outcomes), n_failed
    ))
  }

  # ── Pivot para forma longa ───────

  final_long <- dplyr::bind_rows(results_list)

  if (p_values) {
    est_wide <- final_long |>
      dplyr::select("Variable", "Result", "Outcome") |>
      tidyr::pivot_wider(
        names_from  = "Outcome",
        values_from = "Result"
      )

    p_wide <- final_long |>
      dplyr::select("Variable", "P_Value", "Outcome") |>
      tidyr::pivot_wider(
        names_from   = "Outcome",
        values_from  = "P_Value",
        names_prefix = "P_"
      )

    final_wide <- dplyr::left_join(est_wide, p_wide, by = "Variable")

  } else {

        final_wide <- final_long |>
      dplyr::select("Variable", "Result", "Outcome") |>
      tidyr::pivot_wider(
        names_from  = "Outcome",
        values_from = "Result"
      )
  }

  # ── Formatar nome colunas─────────
  if (!is.null(labels)) {
    if (!is.character(labels) || is.null(names(labels))) {
      warning("'labels' must be a named character vector. Ignoring.", call. = FALSE)
    } else {
      cols_to_rename <- intersect(names(final_wide), names(labels))
      if (length(cols_to_rename) > 0) {
        final_wide <- dplyr::rename_with(
          final_wide,
          .fn   = function(x) labels[x],
          .cols = dplyr::all_of(cols_to_rename)
        )
      }
      if (p_values) {
        p_labels       <- stats::setNames(
          paste0("P_", labels[names(labels)]),
          paste0("P_", names(labels))
        )
        p_cols_present <- intersect(names(final_wide), names(p_labels))
        if (length(p_cols_present) > 0) {
          final_wide <- dplyr::rename_with(
            final_wide,
            .fn   = function(x) p_labels[x],
            .cols = dplyr::all_of(p_cols_present)
          )
        }
      }
    }
  }

  #Remove o intercept
  if (!include_intercept) {
    final_wide <- dplyr::filter(final_wide, .data$Variable != "(Intercept)")
  }


  final_wide <- as.data.frame(final_wide, stringsAsFactors = FALSE)

  attr(final_wide, "family")        <- if (is.character(family)) family else family$family
  attr(final_wide, "exponentiated") <- exponentiate
  attr(final_wide, "robust")        <- robust
  attr(final_wide, "conf.level")    <- conf.level

  final_wide
}


# ── Print method ──────────────────────────────────────────────────────────────

#' Print Method for regtab Results
#'
#' @param x A data.frame returned by `regtab()`.
#' @param ... Additional arguments passed to `print()`.
#' @return Invisibly returns `x`.
#' @export
print.regtab <- function(x, ...) {
  family <- attr(x, "family")
  exp    <- attr(x, "exponentiated")
  robust <- attr(x, "robust")
  conf   <- attr(x, "conf.level")

  if (!is.null(family)) {
    cat("\nMulti-Outcome Regression Table\n")
    cat(strrep("=", 60), "\n")
    cat("Family:         ", family, "\n")
    if (!is.null(exp))    cat("Exponentiated:  ", if (exp) "Yes (IRR/OR)" else "No (log scale)", "\n")
    if (!is.null(robust)) cat("Standard Errors:", if (robust) "Robust (HC0)" else "Model-based", "\n")
    if (!is.null(conf))   cat("Confidence:     ", sprintf("%.0f%%", conf * 100), "\n")
    cat(strrep("=", 60), "\n\n")
  }

  print(as.data.frame(x), row.names = FALSE)
  cat("\n")
  invisible(x)
}


# ── Exportar ────────────────────────────────────────────────────────────

#' Export regtab Results to CSV
#'
#' @param x A data.frame from `regtab()`.
#' @param file File path.
#' @param ... Additional arguments passed to `write.csv()`.
#' @return Invisibly returns `x`.
#' @export
export_regtab_csv <- function(x, file, ...) {
  write.csv(x, file, row.names = FALSE, ...)
  message(sprintf("Table exported to: %s", file))
  invisible(x)
}

#' Export regtab Results to Excel
#'
#' Requires the `openxlsx` package.
#'
#' @param x A data.frame from `regtab()`.
#' @param file File path (.xlsx).
#' @param ... Additional arguments passed to `openxlsx::write.xlsx()`.
#' @return Invisibly returns `x`.
#' @export
export_regtab_xlsx <- function(x, file, ...) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' required. Install with: install.packages('openxlsx')",
         call. = FALSE)
  }
  openxlsx::write.xlsx(x, file, ...)
  message(sprintf("Table exported to: %s", file))
  invisible(x)
}
