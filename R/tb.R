#' Frequency and Summary Tables
#'
#' Creates comprehensive tables for categorical or continuous variables with formatting,
#' statistical tests, prevalence ratios (PR), odds ratios (OR), and column stratification.
#' Supports both univariate and bivariate analysis with flexible output formatting.
#'
#' @param data A data.frame or atomic vector.
#' @param ... Variables to be tabulated. Can include variable names and/or flags
#'   (m, p, row, col, rp, or) for controlling output format.
#' @param m Logical. If TRUE, includes missing values (NA) in the table. Default: FALSE.
#' @param d Integer. Number of decimal places for percentages and statistics. Default: 1.
#' @param format Logical. If TRUE, renders a formatted grid output (Stata-style).
#'   If FALSE, prints as simple matrix. Default: TRUE.
#' @param style Character. Format for displaying counts and percentages. Options:
#'   \itemize{
#'     \item "n_pct": "100 (25.0\%)" (default)
#'     \item "pct_n": "25.0\% (100)"
#'     \item Custom template using {n} and {p} placeholders, e.g., "{n} [{p}\%]"
#'   }
#' @param style.rp Character. Format string for Prevalence Ratio. Default: "{rp} ({lower} - {upper})".
#' @param style.or Character. Format string for Odds Ratio. Default: "{or} ({lower} - {upper})".
#' @param test Logical or Character. Performs statistical test on 2x2+ tables.
#'   \itemize{
#'     \item TRUE: Automatic selection (chi-squared for categorical)
#'     \item "chisq", "fisher", "mcnemar"
#'   }
#' @param subset Logical expression for row filtering.
#' @param strat Variable for column stratification. Disables PR/OR calculations.
#' @param rp Logical. If TRUE, calculates Prevalence Ratios (PR).
#' @param or Logical. If TRUE, calculates Odds Ratios (OR).
#' @param ref Character or numeric. Specifies the reference level for PR/OR calculations.
#' @param conf.level Numeric. Confidence level for intervals (0-1). Default: 0.95.
#' @param var.type Named character vector specifying variable types.
#' @param stat.cont Character. "mean" (Mean/SD) or "median" (Median/IQR).
#'
#' @return An object of class \code{tb}.
#' @export

#principal função

tb <- function(data, ..., m = FALSE, d = 1, format = TRUE, style = "n_pct",
               style.rp = "{rp} ({lower} - {upper})",
               style.or = "{or} ({lower} - {upper})",
               test = FALSE, subset = NULL, strat = NULL,
               rp = FALSE, or = FALSE, ref = NULL,
               conf.level = 0.95,
               var.type = NULL, stat.cont = "median") {

  #verificar que args são válidos
  if (missing(data)) stop("No data provided. Please supply a data.frame or vector.", call. = FALSE)
  if (!is.data.frame(data) && !is.atomic(data)) stop("Invalid input type.", call. = FALSE)

  if (!is.numeric(d) || d < 0 || d > 10) stop("'d' must be a number between 0 and 10.", call. = FALSE)
  d <- as.integer(d)

  if (!is.numeric(conf.level) || conf.level <= 0 || conf.level >= 1) stop("'conf.level' must be between 0 and 1.", call. = FALSE)

  if (is.character(test)) {
    test <- tolower(test)
    valid_tests <- c("chisq", "chsqr", "fisher", "mcnemar")
    if (!test %in% valid_tests) stop("Invalid test method.", call. = FALSE)
  }

  # Capturar argumentos
  subset_expr <- substitute(subset)
  strat_expr  <- substitute(strat)
  dots <- as.list(substitute(list(...)))[-1]

  if (is.data.frame(data)) {
    env_data <- data; env_parent <- parent.frame(); arg_expr <- dots
  } else {
    env_data <- NULL; env_parent <- parent.frame(); data_sym <- substitute(data)
    arg_expr <- c(list(data_sym), dots)
  }

  # FLAGS: possibilitam usar uma só letra para significar todo um parâmetro
  flag_names <- c("m", "p", "row", "col", "rp", "or")
  flags <- list(missing = FALSE, percent = FALSE, by = "total")

  is_flag <- vapply(arg_expr, function(e) is.symbol(e) && as.character(e) %in% flag_names, logical(1))

  if (any(is_flag)) {
    for (f in as.character(arg_expr[is_flag])) {
      if (f == "m")   flags$missing <- TRUE
      if (f == "rp")  rp <- TRUE
      if (f == "or")  or <- TRUE
      if (f == "p")   { flags$percent <- TRUE; flags$by <- "total" }
      if (f == "row") { flags$percent <- TRUE; flags$by <- "row" }
      if (f == "col") { flags$percent <- TRUE; flags$by <- "col" }
    }
    arg_expr <- arg_expr[!is_flag]
  }
  if (m) flags$missing <- TRUE

  vars <- list(); var_labels <- character(); var_names <- character()

  for (i in seq_along(arg_expr)) {
    expr <- arg_expr[[i]]
    nm <- deparse(expr, width.cutoff = 500L)[1]
    val <- tryCatch(eval(expr, env_data, env_parent), error = function(e) stop(sprintf("Variable '%s' not found.", nm), call. = FALSE))
    if (!is.atomic(val) && !is.factor(val)) stop(sprintf("Variable '%s' must be atomic or factor.", nm), call. = FALSE)
    lbl <- attr(val, "label", exact = TRUE); if (is.null(lbl)) lbl <- nm
    vars[[i]] <- val; var_names[i] <- nm; var_labels[i] <- lbl
  }
  names(vars) <- var_names

  if (length(vars) == 0) stop("No variables specified.", call. = FALSE)
  if (length(vars) > 2) stop("Maximum of 2 variables allowed.", call. = FALSE)

  #Caso usuario queria estratificar
  #obs: ainda em construção. Rever a forma com qué feita a divisão. No momento, a estratificação e subestratificação é feita na mesma linha como var2:varstrat
  strat_val <- NULL
  if (!is.null(strat_expr)) {
    strat_val <- tryCatch(eval(strat_expr, env_data, env_parent), error = function(e) stop("Stratification variable not found.", call. = FALSE))
    if (length(strat_val) != length(vars[[1]])) stop("Stratification variable length mismatch.", call. = FALSE)
    if (rp || or) {
      warning("PR/OR disabled when stratification is used.", call. = FALSE, immediate. = TRUE)
      rp <- FALSE; or <- FALSE
    }
  }

  #filtrar (usa a função subset do tidyverse)
  if (!is.null(subset_expr)) {
    subset_val <- tryCatch(eval(subset_expr, env_data, env_parent), error = function(e) stop("Error evaluating subset.", call. = FALSE))
    if (!is.logical(subset_val)) stop("Subset must be logical.", call. = FALSE)
    keep <- subset_val & !is.na(subset_val)
    if (sum(keep) == 0) stop("Subset removed all observations.", call. = FALSE)
    vars <- lapply(vars, function(v) v[keep])
    if (!is.null(strat_val)) strat_val <- strat_val[keep]
  }
#APLICAR A ESTRATIFICAÇÃO PRECISA SER DEPOIS DE FILTRAR!
  if (!is.null(strat_val)) {
    if (length(vars) == 2) {
      col_var <- vars[[2]]
      if (!flags$missing) {
        ok_s <- !is.na(strat_val)
        vars <- lapply(vars, function(v) v[ok_s])
        strat_val <- strat_val[ok_s]
        col_var <- vars[[2]]
      }
      vars[[2]] <- interaction(strat_val, col_var, sep = " : ", drop = TRUE, lex.order = TRUE)
    } else if (length(vars) == 1) {
      vars[[2]] <- factor(strat_val)
    }
  }

  #definir tipo
  is_continuous <- FALSE; row_var_name <- var_names[1]
  if (!is.null(var.type) && row_var_name %in% names(var.type)) {
    type_spec <- tolower(var.type[[row_var_name]])
    if (type_spec %in% c("continuous", "cont", "numeric", "num")) is_continuous <- TRUE
  }

  rp_col <- NULL
  or_col <- NULL


  #A depender do tipo de variável, duas formas de calcular - uma para a variável
  #contíua e outra para a categórica (factor) que vão determinar as formas de valor e o teste estatístico; se for contínua o PR e OR não vão poder ser chamados.
  # ================
  # OPÇÃO1: CONTÍNUA
  # ================
  if (is_continuous) {
    y <- vars[[1]]; x <- if (length(vars) > 1) vars[[2]] else NULL
    if (!is.numeric(y)) stop(sprintf("Variable '%s' is not numeric.", row_var_name), call. = FALSE)

    if (!is.null(x)) {
      if (is.factor(x)) x <- droplevels(x)
      if (!flags$missing) { ok <- !is.na(x) & !is.na(y); x <- x[ok]; y <- y[ok] }
    } else { y <- y[!is.na(y)] }

    calc_stat <- function(val) {
      if (length(val) == 0) return("-")
      if (stat.cont == "mean") {
        m <- mean(val, na.rm = TRUE); s <- sd(val, na.rm = TRUE)
        return(sprintf(paste0("%.", d, "f (%.", d, "f)"), m, s))
      } else {
        q <- quantile(val, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
        return(sprintf(paste0("%.", d, "f (%.", d, "f - %.", d, "f)"), q[2], q[1], q[3]))
      }
    }

    if (is.null(x)) {
      out_mat <- matrix(calc_stat(y), nrow = 1, ncol = 1, dimnames = list(row_var_name, "Total"))
    } else {
      levs <- if (is.factor(x)) levels(x) else sort(unique(x))
      if (flags$missing && any(is.na(x))) levs <- c(levs, NA)
      group_stats <- vapply(levs, function(l) {
        sub_y <- if (is.na(l)) y[is.na(x)] else y[x == l & !is.na(x)]
        calc_stat(sub_y)
      }, character(1))
      out_mat <- matrix(c(group_stats, calc_stat(y)), nrow = 1)
      colnames(out_mat) <- c(as.character(levs), "Total"); rownames(out_mat) <- row_var_name
    }

    stats_res <- NULL
    if ((isTRUE(test) || is.character(test)) && !is.null(x)) {
      tryCatch({
        n_groups <- length(unique(x[!is.na(x)]))
        if (n_groups < 2) warning("Fewer than 2 groups for test.", call. = FALSE)
        else {
          if (stat.cont == "mean") {
            if (n_groups == 2) { stats_res <- t.test(y ~ x); stats_res$method <- "Two-sample t-test" }
            else { fit <- lm(y ~ x); stats_res <- list(p.value = anova(fit)$`Pr(>F)`[1], method = "One-way ANOVA") }
          } else {
            if (n_groups == 2) { stats_res <- wilcox.test(y ~ x, exact = FALSE); stats_res$method <- "Wilcoxon rank-sum test" }
            else stats_res <- kruskal.test(y ~ x)
          }
        }
      }, error = function(e) warning(sprintf("Test failed: %s", e$message), call. = FALSE))
    }
    attr(out_mat, "is_continuous") <- TRUE; attr(out_mat, "stat_label") <- if (stat.cont == "mean") "Mean (SD)" else "Median (IQR)"
    tab <- out_mat

  } else {
    # ===========================
    # OPÇÃO2: CATEGÓRICA (FACTOR)
    # ===========================
    if (!is.null(ref) && length(vars) >= 1) {
      row_var <- vars[[1]]
      if (!is.factor(row_var)) row_var <- factor(row_var)
      ref_str <- as.character(ref)
      if (ref_str %in% levels(row_var)) vars[[1]] <- relevel(row_var, ref = ref_str)
      else if (is.numeric(ref) && ref %in% seq_along(levels(row_var))) vars[[1]] <- relevel(row_var, ref = levels(row_var)[ref])
      else warning(sprintf("Ref '%s' not found.", ref), call. = FALSE)
    }

    vars <- lapply(vars, function(x) if (is.factor(x)) droplevels(x) else x)
    useNA <- if (flags$missing) "always" else "no"
    tab <- table(vars, useNA = useNA)

    if (sum(tab) == 0) stop("Table is empty.", call. = FALSE)
    if (length(dim(tab)) == 1 && flags$percent && flags$by %in% c("row", "col")) stop("Percentages require 2 variables.", call. = FALSE)

    stats_res <- NULL
    if ((isTRUE(test) || is.character(test)) && length(dim(tab)) == 2) {
      type <- if (is.character(test)) test else "chisq"
      tryCatch({
        if (type %in% c("chisq", "chsqr")) {
          if (any(chisq.test(tab)$expected < 5)) warning("Expected counts < 5. Suggest Fisher's.", call. = FALSE)
          stats_res <- chisq.test(tab)
        } else if (type == "fisher") stats_res <- fisher.test(tab, workspace = 2e7)
        else if (type == "mcnemar") { if(nrow(tab)!=2 || ncol(tab)!=2) stop("McNemar needs 2x2.", call.=FALSE); stats_res <- mcnemar.test(tab) }
      }, error = function(e) warning(sprintf("Test failed: %s", e$message), call. = FALSE))
    }

    #=====>PREVALENCE RATIO (RAZÃO DE PREVALENCIA COM IC 95)
    z_crit <- qnorm(1 - (1 - conf.level) / 2)

    if (rp && length(dim(tab)) == 2 && ncol(tab) >= 2) {
      idx_event <- ncol(tab); if (flags$missing && ncol(tab) > 2) idx_event <- ncol(tab) - 1
      events <- tab[, idx_event]; totals <- rowSums(tab); risks <- events / totals; ref_risk <- risks[1]

      if (ref_risk == 0) { warning("Ref risk is 0. Cannot calc RP.", call. = FALSE); rp <- FALSE }
      else {
        rp_vals <- numeric(nrow(tab)); ci_low <- numeric(nrow(tab)); ci_high <- numeric(nrow(tab))
        for (i in 1:nrow(tab)) {
          if (i == 1) { rp_vals[i] <- 1.0; ci_low[i] <- NA; ci_high[i] <- NA }
          else {
            if (events[i] > 0 && events[1] > 0 && risks[i] > 0) {
              est <- risks[i] / ref_risk
              se_log <- sqrt((1/events[i] - 1/totals[i]) + (1/events[1] - 1/totals[1]))
              ci_low[i] <- exp(log(est) - z_crit * se_log); ci_high[i] <- exp(log(est) + z_crit * se_log); rp_vals[i] <- est
            } else { rp_vals[i] <- NA; ci_low[i] <- NA; ci_high[i] <- NA }
          }
        }
        rp_strings <- character(nrow(tab))
        for (i in 1:nrow(tab)) {
          if (i == 1) rp_strings[i] <- "1.00 (Ref)"
          else if (is.na(rp_vals[i])) rp_strings[i] <- "-"
          else { #O gsub é a versão do Rbase do glue; permite que o usuário use o glue direto também ivés de escolher entre os presets
            txt <- style.rp
            txt <- gsub("{rp}", sprintf("%.2f", rp_vals[i]), txt, fixed = TRUE)
            txt <- gsub("{lower}", sprintf("%.2f", ci_low[i]), txt, fixed = TRUE)
            txt <- gsub("{upper}", sprintf("%.2f", ci_high[i]), txt, fixed = TRUE)
            rp_strings[i] <- txt
          }
        }
        rp_col <- rp_strings
      }
    }

    #=====> ODDS RATIO (OR)
    if (or && length(dim(tab)) == 2 && ncol(tab) >= 2) {
      # If m=FALSE: Last=Event, Prev=Non-Event
      # If m=TRUE: Last=NA, 2ndLast=Event, 3rdLast=Non-Event
      n_cols_valid <- ncol(tab) - (if (flags$missing) 1 else 0)

      if (n_cols_valid < 2) {
        warning("Need at least 2 valid columns (Outcome 0/1) for OR.", call. = FALSE); or <- FALSE
      } else {
        idx_evt <- if (flags$missing) ncol(tab) - 1 else ncol(tab)
        idx_non <- idx_evt - 1

        # a=Event (casos), b=Non-Event (controles)
        a <- tab[, idx_evt]
        b <- tab[, idx_non]

        #Reference odds (linha 1)
        if (b[1] == 0) { warning("Ref group has 0 controls. Cannot calc OR.", call.=FALSE); or <- FALSE }
        else {
          ref_odds <- a[1] / b[1]
          or_vals <- numeric(nrow(tab)); ci_low <- numeric(nrow(tab)); ci_high <- numeric(nrow(tab))

          for (i in 1:nrow(tab)) {
            if (i == 1) { or_vals[i] <- 1.0; ci_low[i] <- NA; ci_high[i] <- NA }
            else {
              # Check zeros
              if (a[i]>0 && b[i]>0 && a[1]>0 && b[1]>0) {

                # OR = (a_i * b_1) / (b_i * a_1)
                est <- (a[i] * b[1]) / (b[i] * a[1])

                # SE(lnOR) = sqrt(1/a_i + 1/b_i + 1/a_1 + 1/b_1)
                se_log <- sqrt(1/a[i] + 1/b[i] + 1/a[1] + 1/b[1])

                ci_low[i] <- exp(log(est) - z_crit * se_log)
                ci_high[i] <- exp(log(est) + z_crit * se_log)
                or_vals[i] <- est
              } else { or_vals[i] <- NA; ci_low[i] <- NA; ci_high[i] <- NA }
            }
          }

          or_strings <- character(nrow(tab))
          for (i in 1:nrow(tab)) {
            if (i == 1) or_strings[i] <- "1.00 (Ref)"
            else if (is.na(or_vals[i])) or_strings[i] <- "-"
            else {
              txt <- style.or
              txt <- gsub("{or}", sprintf("%.2f", or_vals[i]), txt, fixed = TRUE)
              txt <- gsub("{lower}", sprintf("%.2f", ci_low[i]), txt, fixed = TRUE)
              txt <- gsub("{upper}", sprintf("%.2f", ci_high[i]), txt, fixed = TRUE)
              or_strings[i] <- txt
            }
          }
          or_col <- or_strings
        }
      }
    }

    if (length(dim(tab)) == 2) tab <- addmargins(tab)

    if (flags$percent) {
      if (length(dim(tab)) == 1) { attr(tab, "percent") <- (tab / sum(tab)) * 100 }
      else {
        nr <- nrow(tab); nc <- ncol(tab)
        row_idx <- seq_len(nrow(tab) - 1); col_idx <- seq_len(ncol(tab) - 1)
        if (!flags$missing) {
          rn <- rownames(tab); cn <- colnames(tab)
          if (!is.null(rn)) row_idx <- row_idx[!is.na(rn[row_idx])]
          if (!is.null(cn)) col_idx <- col_idx[!is.na(cn[col_idx])]
        }
        body <- tab[row_idx, col_idx, drop = FALSE]
        pct <- switch(flags$by, total = body / sum(body), row = body / rowSums(body), col = sweep(body, 2, colSums(body), "/"))
        pct_full <- matrix(NA_real_, nr, nc); pct_full[row_idx, col_idx] <- pct * 100
        attr(tab, "percent") <- pct_full
      }
    }
  }

  attr(tab, "flags") <- flags
  attr(tab, "format") <- format
  attr(tab, "style") <- style
  attr(tab, "var.labels") <- var_labels
  attr(tab, "stats") <- stats_res
  attr(tab, "is_continuous") <- is_continuous
  attr(tab, "rp") <- rp_col
  attr(tab, "or") <- or_col # NEW ATTRIBUTE

  if (isTRUE(flags$percent)) attr(tab, "digits") <- d

  class(tab) <- c("tb", class(tab))
  return(tab)
}

#Função para garantir a apresentação formatada dos dados em um grid. Os dados em output são processados e centralizados e colocados em um grid
#' @export
print.tb <- function(x, digits = NULL, ...) {
  flags <- attr(x, "flags"); use_format <- attr(x, "format"); style <- attr(x, "style")
  stats <- attr(x, "stats"); var_labels <- attr(x, "var.labels")
  rp_col <- attr(x, "rp"); or_col <- attr(x, "or")
  is_continuous <- attr(x, "is_continuous"); stat_label <- attr(x, "stat_label")

  if (is.null(use_format)) use_format <- TRUE
  if (is.null(style)) style <- "n_pct"
  if (is.null(digits)) digits <- if (!is.null(attr(x, "digits"))) attr(x, "digits") else 1

  if (is_continuous) {
    out_mat <- as.matrix(unclass(x))
    if (!is.null(stat_label)) rownames(out_mat) <- paste0(rownames(out_mat), " [", stat_label, "]")
  } else {
    freq <- unclass(x); pct <- attr(x, "percent")
    if (length(dim(freq)) == 1) {
      freq <- as.matrix(freq); colnames(freq) <- "Freq"
      if (!is.null(pct) && is.null(dim(pct))) pct <- as.matrix(pct)
    }
    nr <- nrow(freq); nc <- ncol(freq)
    out_mat <- matrix("", nrow = nr, ncol = nc); dimnames(out_mat) <- dimnames(freq)

    for (i in seq_len(nr)) {
      for (j in seq_len(nc)) {
        val <- freq[i, j]
        has_pct <- !is.null(pct) && i <= nrow(pct) && j <= ncol(pct) && !is.na(pct[i, j])
        if (!isTRUE(flags$percent) || !has_pct) out_mat[i, j] <- as.character(val)
        else {
          p_str <- sprintf(paste0("%.", digits, "f"), pct[i, j])
          if (style == "n_pct") out_mat[i, j] <- sprintf("%d (%s%%)", val, p_str)
          else if (style == "pct_n") out_mat[i, j] <- sprintf("%s%% (%d)", p_str, val)
          else { txt <- gsub("{n}", val, style, fixed = TRUE); txt <- gsub("{p}", p_str, txt, fixed = TRUE); out_mat[i, j] <- txt }
        }
      }
    }
  }

  #ADicionar as colunas de RP e OR
  if (!is.null(rp_col)) {
    full_rp <- c(rp_col, rep("", nrow(out_mat) - length(rp_col)))
    out_mat <- cbind(out_mat, "PR (95% CI)" = full_rp)
  }
  if (!is.null(or_col)) {
    full_or <- c(or_col, rep("", nrow(out_mat) - length(or_col)))
    out_mat <- cbind(out_mat, "OR (95% CI)" = full_or)
  }

  nr <- nrow(out_mat); nc <- ncol(out_mat); dn <- dimnames(out_mat)
  row_var <- if (length(var_labels) >= 1) var_labels[1] else names(dn)[1]
  col_var <- if (length(var_labels) >= 2) var_labels[2] else (if (length(dn) > 1) names(dn)[2] else "")
  if (is.null(row_var)) row_var <- ""; if (is.null(col_var)) col_var <- ""

  if (!use_format) {
    names(dimnames(out_mat))[1] <- row_var
    print(noquote(out_mat))
    if (!is.null(stats)) cat("\nTest:", stats$method, "p-value", if(stats$p.value < 0.001) "< 0.001" else sprintf("= %.3f", stats$p.value), "\n")
    return(invisible(x))
  }

  #Permite ajustar o output para diferentes tamanhos do console para não quebrar a sequencia das linhas, se o output for mais longo que o comprimento do console, quebra a tabela e repete o cabeçalho/linhas com a coluna que faltava abaixo
  safe_nchar <- function(x) nchar(ifelse(is.na(x), "NA", x))
  center_text <- function(txt, width) {
    pad <- width - nchar(if(is.na(txt)) "NA" else txt); if (pad < 0) pad <- 0
    paste0(strrep(" ", floor(pad/2)), if(is.na(txt)) "NA" else txt, strrep(" ", pad - floor(pad/2)))
  }
  right_text <- function(txt, width) sprintf(paste0("%", width, "s"), if(is.na(txt)) "NA" else txt)

  row_labels <- rownames(out_mat); col_labels <- colnames(out_mat)
  has_row_total <- (length(dim(unclass(x))) == 2) && !is_continuous; has_col_total <- TRUE

  width_row_labels <- max(safe_nchar(c(row_var, row_labels))) + 1
  col_widths <- integer(nc); for (j in 1:nc) col_widths[j] <- max(safe_nchar(col_labels[j]), max(safe_nchar(out_mat[, j]))) + 2
  console_width <- getOption("width"); if (is.null(console_width)) console_width <- 80

  j_start <- 1
  while (j_start <= nc) {
    used_width <- width_row_labels + 3; j_end <- j_start
    while (j_end <= nc) {
      new_width <- used_width + col_widths[j_end] + 1
      if (new_width > console_width && j_end > j_start) { j_end <- j_end - 1; break }
      used_width <- new_width; j_end <- j_end + 1
    }
    if (j_end > nc) j_end <- nc
    cols_in_page <- j_start:j_end

    #remove a coluna excedente
    cols_for_header <- cols_in_page
    extra_cols_count <- (!is.null(rp_col)) + (!is.null(or_col))
    if (extra_cols_count > 0) {
      start_extra <- nc - extra_cols_count + 1
      cols_for_header <- cols_in_page[cols_in_page < start_extra]
    }

    if (col_var != "" && length(cols_for_header) > 0) {
      cat(right_text("", width_row_labels), " | ", sep = "")
      data_w <- sum(col_widths[cols_for_header]) + max(0, length(cols_for_header) - 1)
      cat(center_text(col_var, data_w), "\n")
    }

    cat(right_text(row_var, width_row_labels), " |", sep = "")
    for (j in cols_in_page) {
      idx_sum <- nc - extra_cols_count
      is_extra <- j > idx_sum
      if (has_col_total && j == idx_sum && idx_sum > 1 && j != cols_in_page[1]) cat("|")
      if (is_extra && j != cols_in_page[1]) cat("|")
      cat(center_text(col_labels[j], col_widths[j]))
    }
    cat("\n")

    cat(strrep("-", width_row_labels), "-+", sep = "")
    for (j in cols_in_page) {
      idx_sum <- nc - extra_cols_count
      is_extra <- j > idx_sum
      if (has_col_total && j == idx_sum && idx_sum > 1 && j != cols_in_page[1]) cat("+")
      if (is_extra && j != cols_in_page[1]) cat("+")
      cat(strrep("-", col_widths[j]))
    }
    cat("\n")

    for (i in 1:nr) {
      if (i == nr && has_row_total && nr > 1) {
        cat(strrep("-", width_row_labels), "-+", sep = "")
        for (j in cols_in_page) {
          idx_sum <- nc - extra_cols_count
          is_extra <- j > idx_sum
          if (has_col_total && j == idx_sum && idx_sum > 1 && j != cols_in_page[1]) cat("+")
          if (is_extra && j != cols_in_page[1]) cat("+")
          cat(strrep("-", col_widths[j]))
        }
        cat("\n")
      }
      cat(right_text(row_labels[i], width_row_labels), " |", sep = "")
      for (j in cols_in_page) {
        idx_sum <- nc - extra_cols_count
        is_extra <- j > idx_sum
        if (has_col_total && j == idx_sum && idx_sum > 1 && j != cols_in_page[1]) cat("|")
        if (is_extra && j != cols_in_page[1]) cat("|")
        cat(center_text(out_mat[i, j], col_widths[j]))
      }
      cat("\n")
    }
    j_start <- j_end + 1
    if (j_start <= nc) cat("\n")
  }

  if (!is.null(stats)) cat("\n  Test:", stats$method, "  p-value", if(stats$p.value < 0.001) "< 0.001" else sprintf("= %.3f", stats$p.value), "\n")
  invisible(x)
}
#Pega o output e transforma em dataframe na mesma formatação (é útil para exportar par apowerpoint via officer, por exemplo), se usar o results, a tabela perde a formatação pois n passou pela s3
#' @export
as.data.frame.tb <- function(x, ...) {
  flags <- attr(x, "flags"); digits <- if (!is.null(attr(x, "digits"))) attr(x, "digits") else 1
  style <- attr(x, "style"); if (is.null(style)) style <- "n_pct"
  rp_col <- attr(x, "rp"); or_col <- attr(x, "or")
  var_labels <- attr(x, "var.labels"); stats <- attr(x, "stats")
  is_continuous <- attr(x, "is_continuous"); stat_label <- attr(x, "stat_label")

  if (isTRUE(is_continuous)) {
    out_mat <- as.matrix(unclass(x))
    if (!is.null(stat_label)) rownames(out_mat) <- paste0(rownames(out_mat), " [", stat_label, "]")
  } else {
    freq <- unclass(x); pct <- attr(x, "percent")
    if (length(dim(freq)) == 1) {
      freq <- as.matrix(freq); colnames(freq) <- "Freq"
      if (!is.null(pct) && is.null(dim(pct))) pct <- as.matrix(pct)
    }
    nr <- nrow(freq); nc <- ncol(freq)
    out_mat <- matrix("", nrow = nr, ncol = nc); dimnames(out_mat) <- dimnames(freq)

    for (i in seq_len(nr)) {
      for (j in seq_len(nc)) {
        val <- freq[i, j]
        has_pct <- !is.null(pct) && i <= nrow(pct) && j <= ncol(pct) && !is.na(pct[i, j])
        if (!isTRUE(flags$percent) || !has_pct) out_mat[i, j] <- as.character(val)
        else {
          p_str <- sprintf(paste0("%.", digits, "f"), pct[i, j])
          if (style == "n_pct") out_mat[i, j] <- sprintf("%d (%s%%)", val, p_str)
          else if (style == "pct_n") out_mat[i, j] <- sprintf("%s%% (%d)", p_str, val)
          else { txt <- gsub("{n}", val, style, fixed = TRUE); txt <- gsub("{p}", p_str, txt, fixed = TRUE); out_mat[i, j] <- txt }
        }
      }
    }
  }

  if (!is.null(rp_col)) {
    if (nrow(out_mat) > length(rp_col)) rp_col <- c(rp_col, rep("", nrow(out_mat) - length(rp_col)))
    out_mat <- cbind(out_mat, `PR (95% CI)` = rp_col)
  }
  if (!is.null(or_col)) {
    if (nrow(out_mat) > length(or_col)) or_col <- c(or_col, rep("", nrow(out_mat) - length(or_col)))
    out_mat <- cbind(out_mat, `OR (95% CI)` = or_col)
  }

  df <- as.data.frame(out_mat, stringsAsFactors = FALSE)

  # Headers
  extra_cols <- c()
  if (!is.null(rp_col)) extra_cols <- c(extra_cols, "PR (95% CI)")
  if (!is.null(or_col)) extra_cols <- c(extra_cols, "OR (95% CI)")

  n_core <- ncol(unclass(x)); if(length(dim(unclass(x)))==1) n_core <- 1
  original_cols <- colnames(x); if(is.null(original_cols)) original_cols <- paste0("Col", 1:n_core)
  colnames(df) <- c(original_cols, extra_cols)

  row_labels <- rownames(x)
  if (isTRUE(is_continuous) && !is.null(stat_label)) row_labels <- paste0(row_labels, " [", stat_label, "]")
  if(is.null(row_labels)) row_labels <- as.character(1:nrow(df))
  row_var_name <- "Var1"
  dn <- dimnames(x)
  if (!is.null(names(dn)[1])) row_var_name <- names(dn)[1]
  if (length(var_labels) >= 1 && !is.na(var_labels[1])) row_var_name <- var_labels[1]

  df <- cbind(Row_Label = row_labels, df)
  colnames(df)[1] <- row_var_name
  attr(df, "stats") <- stats
  return(df)
}
#integração com o flextable
#' @export
as_flextable.tb <- function(x, ...) {
  if (!requireNamespace("flextable", quietly = TRUE)) stop("Package 'flextable' needed.", call. = FALSE)

  df <- as.data.frame(x)
  stats <- attr(x, "stats"); var_labels <- attr(x, "var.labels")
  rp_col <- attr(x, "rp"); or_col <- attr(x, "or")
  is_cont <- isTRUE(attr(x, "is_continuous"))
  dn <- dimnames(x); col_var_name <- ""

  if (!is_cont) {
    if (length(var_labels) >= 2) col_var_name <- var_labels[2]
    else if (!is.null(names(dn)[2])) col_var_name <- names(dn)[2]
  }

  ft <- flextable::flextable(df)

  if (col_var_name != "" && ncol(df) > 2) {
    has_sum <- any(names(df) == "Total" | names(df) == "Sum")
    has_rp  <- !is.null(rp_col)
    has_or  <- !is.null(or_col)

    n_total <- ncol(df)
    n_fixed_left <- 1
    n_fixed_right <- (if(has_sum) 1 else 0) + (if(has_rp) 1 else 0) + (if(has_or) 1 else 0)

    n_cats <- n_total - n_fixed_left - n_fixed_right

    if (n_cats > 0) {
      values <- c("", col_var_name); widths <- c(1, n_cats)
      if (has_sum) { values <- c(values, ""); widths <- c(widths, 1) }
      if (has_rp)  { values <- c(values, ""); widths <- c(widths, 1) }
      if (has_or)  { values <- c(values, ""); widths <- c(widths, 1) }

      ft <- flextable::add_header_row(ft, values = values, colwidths = widths)
    }
  }

  if (!is.null(stats)) {
    p_str <- if(stats$p.value < 0.001) "< 0.001" else sprintf("= %.3f", stats$p.value)
    stat_text <- paste0(stats$method, ": p-value ", p_str)
    ft <- flextable::add_footer_lines(ft, values = stat_text)
  }

  ft <- flextable::theme_booktabs(ft)
  ft <- flextable::autofit(ft)
  ft <- flextable::align(ft, align = "center", part = "header")
  ft <- flextable::align(ft, j = -1, align = "center", part = "body")
  ft <- flextable::align(ft, j = 1, align = "left", part = "body")
  if (!is.null(stats)) ft <- flextable::align(ft, part = "footer", align = "right")

  return(ft)
}

if (requireNamespace("flextable", quietly = TRUE)) {
  s3_register <- function(generic, class, method = NULL) {
    if (is.null(method)) method <- get(paste(generic, class, sep = "."), mode = "function")
    generic <- match.fun(generic)
    registerS3method(generic, class, method, envir = parent.frame())
  }
  tryCatch(s3_register("flextable::as_flextable", "tb", as_flextable.tb), error = function(e) NULL)
}
