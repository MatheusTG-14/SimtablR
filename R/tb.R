#' Frequency and Summary Tables
#'
#' Creates comprehensive tables for categorical or continuous variables with formatting,
#' statistical tests, prevalence ratios (PR), odds ratios (OR), and column stratification.
#'
#' @param data A data.frame or atomic vector.
#' @param ... Variables to be tabulated. Accepts variable names and/or flags
#'   (`m`, `p`, `row`, `col`, `rp`, `or`) for controlling output format.
#' @param m Logical. Include missing values (NA) in the table. Default: `FALSE`.
#' @param d Integer. Decimal places for percentages and statistics. Default: `1`.
#' @param format Logical. Render a formatted grid output. Default: `TRUE`.
#' @param style Character. Format for displaying counts and percentages.
#'   Options: `"n_pct"` (default), `"pct_n"`, or a custom template with `{n}` and
#'   `{p}` placeholders, e.g. `"{n} [{p}%]"`.
#' @param style.rp Character. Format string for Prevalence Ratio.
#'   Default: `"{rp} ({lower} - {upper})"`.
#' @param style.or Character. Format string for Odds Ratio.
#'   Default: `"{or} ({lower} - {upper})"`.
#' @param test Logical or Character. Performs statistical test on 2x2+ tables.
#'   `TRUE` for automatic selection, or one of `"chisq"`, `"fisher"`, `"mcnemar"`.
#' @param subset Logical expression for row filtering.
#' @param strat Variable for column stratification. Disables PR/OR calculations.
#' @param rp Logical. Calculate Prevalence Ratios (PR). Default: `FALSE`.
#' @param or Logical. Calculate Odds Ratios (OR). Default: `FALSE`.
#' @param ref Character or numeric. Reference level for PR/OR calculations.
#' @param conf.level Numeric. Confidence level for intervals (0–1). Default: `0.95`.
#' @param var.type Named character vector specifying variable types, e.g.
#'   `c(age = "continuous")`.
#' @param stat.cont Character. `"mean"` (Mean/SD) or `"median"` (Median/IQR).
#'   Default: `"median"`.
#'
#' @return An object of class `tb` (a matrix with attributes).
#' @export
tb <- function(
    data,
    ...,
    m           = FALSE,
    d           = 1,
    format      = TRUE,
    style       = "n_pct",
    style.rp    = "{rp} ({lower} - {upper})",
    style.or    = "{or} ({lower} - {upper})",
    test        = FALSE,
    subset      = NULL,
    strat       = NULL,
    rp          = FALSE,
    or          = FALSE,
    ref         = NULL,
    conf.level  = 0.95,
    var.type    = NULL,
    stat.cont   = "median"
) {
  # ── Args ────────────────────────────────────────────────────
  if (missing(data)) {
    stop("No data provided. Please supply a data.frame or vector.", call. = FALSE)
  }
  if (!is.data.frame(data) && !is.atomic(data)) {
    stop("'data' must be a data.frame or atomic vector.", call. = FALSE)
  }
  if (!is.numeric(d) || d < 0 || d > 10) {
    stop("'d' must be a number between 0 and 10.", call. = FALSE)
  }
  if (!is.numeric(conf.level) || conf.level <= 0 || conf.level >= 1) {
    stop("'conf.level' must be between 0 and 1.", call. = FALSE)
  }
  if (is.character(test)) {
    test <- tolower(test)
    valid_tests <- c("chisq", "chsqr", "fisher", "mcnemar")
    if (!test %in% valid_tests) {
      stop("Invalid test method. Use one of: chisq, fisher, mcnemar.", call. = FALSE)
    }
  }

  d <- as.integer(d)

  # ── Captura de expressões para função ───────────────────────────────────────────────────
  subset_expr <- substitute(subset)
  strat_expr  <- substitute(strat)
  dots        <- as.list(substitute(list(...)))[-1]

  if (is.data.frame(data)) {
    env_data   <- data
    env_parent <- parent.frame()
    arg_expr   <- dots
  } else {
    env_data   <- NULL
    env_parent <- parent.frame()
    data_sym   <- substitute(data)
    arg_expr   <- c(list(data_sym), dots)
  }

  # ── Flags na expressão ────────────────────────────────────────────────────
  flag_names <- c("m", "p", "row", "col", "rp", "or")
  flags      <- list(missing = FALSE, percent = FALSE, by = "total")

  is_flag <- vapply(
    arg_expr,
    function(e) is.symbol(e) && as.character(e) %in% flag_names,
    logical(1)
  )

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

  # ── Vars ────────────────────────────────────────────────────
  vars       <- list()
  var_labels <- character()
  var_names  <- character()

  for (i in seq_along(arg_expr)) {
    expr <- arg_expr[[i]]
    nm   <- deparse(expr, width.cutoff = 500L)[1]
    val  <- tryCatch(
      eval(expr, env_data, env_parent),
      error = function(e) stop(sprintf("Variable '%s' not found.", nm), call. = FALSE)
    )
    if (!is.atomic(val) && !is.factor(val)) {
      stop(sprintf("Variable '%s' must be atomic or factor.", nm), call. = FALSE)
    }
    lbl            <- attr(val, "label", exact = TRUE) %||% nm
    vars[[i]]      <- val
    var_names[i]   <- nm
    var_labels[i]  <- lbl
  }

  names(vars) <- var_names

  if (length(vars) == 0) stop("No variables specified.", call. = FALSE)
  if (length(vars) > 2)  stop("Maximum of 2 variables allowed.", call. = FALSE)

  # ── Strat ───────────────────────────────────────────────
  strat_val <- NULL
  if (!is.null(strat_expr)) {
    strat_val <- tryCatch(
      eval(strat_expr, env_data, env_parent),
      error = function(e) stop("Stratification variable not found.", call. = FALSE)
    )
    if (length(strat_val) != length(vars[[1]])) {
      stop("Stratification variable length mismatch.", call. = FALSE)
    }
    if (rp || or) {
      warning("PR/OR disabled when stratification is used.", call. = FALSE, immediate. = TRUE)
      rp <- FALSE
      or <- FALSE
    }
  }

  # ── Subset ───────────────────────────────────────────────────
  if (!is.null(subset_expr)) {
    subset_val <- tryCatch(
      eval(subset_expr, env_data, env_parent),
      error = function(e) stop("Error evaluating subset.", call. = FALSE)
    )
    if (!is.logical(subset_val)) stop("Subset must be logical.", call. = FALSE)

    keep <- subset_val & !is.na(subset_val)
    if (sum(keep) == 0) stop("Subset removed all observations.", call. = FALSE)

    vars      <- lapply(vars, `[`, keep)
    strat_val <- strat_val[keep]  # safe even if strat_val is NULL
  }

  # ── Aplica a strat após subset ──────────────
  if (!is.null(strat_val)) {
    if (length(vars) == 2) {
      if (!flags$missing) {
        ok        <- !is.na(strat_val)
        vars      <- lapply(vars, `[`, ok)
        strat_val <- strat_val[ok]
      }
      vars[[2]] <- interaction(
        strat_val, vars[[2]],
        sep = " : ", drop = TRUE, lex.order = TRUE
      )
    } else {
      vars[[2]] <- factor(strat_val)
    }
  }

  # ── DEtecta tipo de var ──────────────────────────────────────────────────
  row_var_name  <- var_names[1]
  is_continuous <- FALSE

  if (!is.null(var.type) && row_var_name %in% names(var.type)) {
    type_spec <- tolower(var.type[[row_var_name]])
    if (type_spec %in% c("continuous", "cont", "numeric", "num")) {
      is_continuous <- TRUE
    }
  }

  rp_col <- NULL
  or_col <- NULL

  # ═══════════════════════════════════════════════════════════════════════════
  # BRANCH A: Var contínua
  # ═══════════════════════════════════════════════════════════════════════════
  if (is_continuous) {
    y <- vars[[1]]
    x <- if (length(vars) > 1) vars[[2]] else NULL

    if (!is.numeric(y)) {
      stop(sprintf("Variable '%s' is not numeric.", row_var_name), call. = FALSE)
    }

    if (!is.null(x)) {
      if (is.factor(x)) x <- droplevels(x)
      if (!flags$missing) {
        ok <- !is.na(x) & !is.na(y)
        x  <- x[ok]
        y  <- y[ok]
      }
    } else {
      y <- y[!is.na(y)]
    }

    calc_stat <- function(val) {
      if (length(val) == 0) return("-")
      if (stat.cont == "mean") {
        sprintf(
          paste0("%.", d, "f (%.", d, "f)"),
          mean(val, na.rm = TRUE),
          sd(val,   na.rm = TRUE)
        )
      } else {
        q <- quantile(val, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
        sprintf(paste0("%.", d, "f (%.", d, "f - %.", d, "f)"), q[2], q[1], q[3])
      }
    }

    if (is.null(x)) {
      out_mat <- matrix(
        calc_stat(y), nrow = 1, ncol = 1,
        dimnames = list(row_var_name, "Total")
      )
    } else {
      levs <- if (is.factor(x)) levels(x) else sort(unique(x))
      if (flags$missing && any(is.na(x))) levs <- c(levs, NA)

      group_stats <- vapply(levs, function(l) {
        sub_y <- if (is.na(l)) y[is.na(x)] else y[x == l & !is.na(x)]
        calc_stat(sub_y)
      }, character(1))

      out_mat <- matrix(
        c(group_stats, calc_stat(y)), nrow = 1,
        dimnames = list(row_var_name, c(as.character(levs), "Total"))
      )
    }

    stats_res <- NULL
    if ((isTRUE(test) || is.character(test)) && !is.null(x)) {
      tryCatch({
        n_groups <- length(unique(x[!is.na(x)]))
        if (n_groups < 2) {
          warning("Fewer than 2 groups for test.", call. = FALSE)
        } else if (stat.cont == "mean") {
          stats_res <- if (n_groups == 2) {
            t_res <- t.test(y ~ x)
            t_res$method <- "Two-sample t-test"
            t_res
          } else {
            fit <- lm(y ~ x)
            list(p.value = anova(fit)$`Pr(>F)`[1], method = "One-way ANOVA")
          }
        } else {
          stats_res <- if (n_groups == 2) {
            w_res <- wilcox.test(y ~ x, exact = FALSE)
            w_res$method <- "Wilcoxon rank-sum test"
            w_res
          } else {
            kruskal.test(y ~ x)
          }
        }
      }, error = function(e) {
        warning(sprintf("Test failed: %s", e$message), call. = FALSE)
      })
    }

    attr(out_mat, "is_continuous") <- TRUE
    attr(out_mat, "stat_label")    <- if (stat.cont == "mean") "Mean (SD)" else "Median (IQR)"
    tab <- out_mat

  } else {
    # ═══════════════════════════════════════════════════════════════════════
    # BRANCH B:Var categórica
    # ═══════════════════════════════════════════════════════════════════════

    # Reordenar níveis para cat
    if (!is.null(ref) && length(vars) >= 1) {
      row_var <- if (is.factor(vars[[1]])) vars[[1]] else factor(vars[[1]])
      ref_str <- as.character(ref)

      if (ref_str %in% levels(row_var)) {
        vars[[1]] <- relevel(row_var, ref = ref_str)
      } else if (is.numeric(ref) && ref %in% seq_along(levels(row_var))) {
        vars[[1]] <- relevel(row_var, ref = levels(row_var)[ref])
      } else {
        warning(sprintf("Ref '%s' not found.", ref), call. = FALSE)
      }
    }

    vars   <- lapply(vars, function(v) if (is.factor(v)) droplevels(v) else v)
    useNA  <- if (flags$missing) "always" else "no"
    tab    <- table(vars, useNA = useNA)

    if (sum(tab) == 0) stop("Table is empty.", call. = FALSE)

    if (length(dim(tab)) == 1 && flags$percent && flags$by %in% c("row", "col")) {
      message("Note: 'row'/'col' percentages require 2 variables. Using total percentages.")
      flags$by <- "total"
    }

    # teste
    stats_res <- NULL
    if ((isTRUE(test) || is.character(test)) && length(dim(tab)) == 2) {
      type <- if (is.character(test)) test else "chisq"
      tryCatch({
        stats_res <- switch(type,
                            chisq  = , chsqr = {
                              if (any(chisq.test(tab)$expected < 5)) {
                                warning("Expected counts < 5. Consider Fisher's exact test.", call. = FALSE)
                              }
                              chisq.test(tab)
                            },
                            fisher  = fisher.test(tab, workspace = 2e7),
                            mcnemar = {
                              if (nrow(tab) != 2 || ncol(tab) != 2) {
                                stop("McNemar's test requires a 2x2 table.", call. = FALSE)
                              }
                              mcnemar.test(tab)
                            }
        )
      }, error = function(e) {
        warning(sprintf("Test failed: %s", e$message), call. = FALSE)
      })
    }

    z_crit <- qnorm(1 - (1 - conf.level) / 2)

    # ── PR  ────────────────────────────────────────────────────
    if (rp && length(dim(tab)) == 2 && ncol(tab) >= 2) {
      idx_event <- if (flags$missing && ncol(tab) > 2) ncol(tab) - 1L else ncol(tab)
      events    <- tab[, idx_event]
      totals    <- rowSums(tab)
      risks     <- events / totals
      ref_risk  <- risks[1]

      if (ref_risk == 0) {
        warning("Reference risk is 0. Cannot calculate PR.", call. = FALSE)
        rp <- FALSE
      } else {
        n         <- nrow(tab)
        rp_vals   <- numeric(n)
        ci_low    <- numeric(n)
        ci_high   <- numeric(n)

        rp_vals[1] <- 1.0
        ci_low[1]  <- NA_real_
        ci_high[1] <- NA_real_

        for (i in seq_len(n)[-1]) {
          if (events[i] > 0 && events[1] > 0 && risks[i] > 0) {
            est         <- risks[i] / ref_risk
            se_log      <- sqrt((1 / events[i] - 1 / totals[i]) +
                                  (1 / events[1] - 1 / totals[1]))
            rp_vals[i]  <- est
            ci_low[i]   <- exp(log(est) - z_crit * se_log)
            ci_high[i]  <- exp(log(est) + z_crit * se_log)
          } else {
            rp_vals[i]  <- NA_real_
            ci_low[i]   <- NA_real_
            ci_high[i]  <- NA_real_
          }
        }

        rp_col <- vapply(seq_len(n), function(i) {
          if (i == 1)          return("1.00 (Ref)")
          if (is.na(rp_vals[i])) return("-")
          txt <- style.rp
          txt <- gsub("{rp}",    sprintf("%.2f", rp_vals[i]),  txt, fixed = TRUE)
          txt <- gsub("{lower}", sprintf("%.2f", ci_low[i]),   txt, fixed = TRUE)
          txt <- gsub("{upper}", sprintf("%.2f", ci_high[i]),  txt, fixed = TRUE)
          txt
        }, character(1))
      }
    }

    # ── OR ──────────────────────────────────────────────────────────
    if (or && length(dim(tab)) == 2 && ncol(tab) >= 2) {
      n_cols_valid <- ncol(tab) - if (flags$missing) 1L else 0L

      if (n_cols_valid < 2) {
        warning("Need at least 2 valid columns for OR.", call. = FALSE)
        or <- FALSE
      } else {
        idx_evt <- if (flags$missing) ncol(tab) - 1L else ncol(tab)
        idx_non <- idx_evt - 1L
        a       <- tab[, idx_evt]
        b       <- tab[, idx_non]

        if (b[1] == 0) {
          warning("Reference group has 0 controls. Cannot calculate OR.", call. = FALSE)
          or <- FALSE
        } else {
          n        <- nrow(tab)
          or_vals  <- numeric(n)
          ci_low   <- numeric(n)
          ci_high  <- numeric(n)

          or_vals[1] <- 1.0
          ci_low[1]  <- NA_real_
          ci_high[1] <- NA_real_

          for (i in seq_len(n)[-1]) {
            if (a[i] > 0 && b[i] > 0 && a[1] > 0 && b[1] > 0) {
              est        <- (a[i] * b[1]) / (b[i] * a[1])
              se_log     <- sqrt(1 / a[i] + 1 / b[i] + 1 / a[1] + 1 / b[1])
              or_vals[i] <- est
              ci_low[i]  <- exp(log(est) - z_crit * se_log)
              ci_high[i] <- exp(log(est) + z_crit * se_log)
            } else {
              or_vals[i]  <- NA_real_
              ci_low[i]   <- NA_real_
              ci_high[i]  <- NA_real_
            }
          }

          or_col <- vapply(seq_len(n), function(i) {
            if (i == 1)            return("1.00 (Ref)")
            if (is.na(or_vals[i])) return("-")
            txt <- style.or
            txt <- gsub("{or}",    sprintf("%.2f", or_vals[i]),  txt, fixed = TRUE)
            txt <- gsub("{lower}", sprintf("%.2f", ci_low[i]),   txt, fixed = TRUE)
            txt <- gsub("{upper}", sprintf("%.2f", ci_high[i]),  txt, fixed = TRUE)
            txt
          }, character(1))
        }
      }
    }

    #Adicionar margem de total
    if (length(dim(tab)) == 1) {
      tab <- addmargins(tab)
      names(tab)[length(tab)] <- "Total"
    } else {
      tab <- addmargins(tab)
    }

    if (flags$percent) {
      if (length(dim(tab)) == 1) {
        #Percentuais apenas sobre as categorias reais
        n_all   <- length(tab)
        n_cats  <- n_all - 1L
        pct_vec <- rep(NA_real_, n_all)
        pct_vec[seq_len(n_cats)] <- (tab[seq_len(n_cats)] / tab[n_all]) * 100
        attr(tab, "percent") <- pct_vec
      } else {
        nr      <- nrow(tab)
        nc      <- ncol(tab)
        row_idx <- seq_len(nr - 1)
        col_idx <- seq_len(nc - 1)

        if (!flags$missing) {
          rn      <- rownames(tab)
          cn      <- colnames(tab)
          if (!is.null(rn)) row_idx <- row_idx[!is.na(rn[row_idx])]
          if (!is.null(cn)) col_idx <- col_idx[!is.na(cn[col_idx])]
        }

        body <- tab[row_idx, col_idx, drop = FALSE]
        pct  <- switch(
          flags$by,
          total = body / sum(body),
          row   = body / rowSums(body),
          col   = sweep(body, 2, colSums(body), "/")
        )

        pct_full                    <- matrix(NA_real_, nr, nc)
        pct_full[row_idx, col_idx]  <- pct * 100
        attr(tab, "percent")        <- pct_full
      }
    }
  }

  # ── Adiciona e retornar atributos ───────────────────────────────────────────
  attr(tab, "flags")         <- flags
  attr(tab, "format")        <- format
  attr(tab, "style")         <- style
  attr(tab, "var.labels")    <- var_labels
  attr(tab, "stats")         <- stats_res
  attr(tab, "is_continuous") <- is_continuous
  attr(tab, "rp")            <- rp_col
  attr(tab, "or")            <- or_col

  if (isTRUE(flags$percent)) attr(tab, "digits") <- d

  class(tab) <- c("tb", class(tab))
  tab
}

# ── Operatdor null in ──────────────────────────────────────
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ── Print  ─────────────────────────────────────────────────────────────

#' Print Method for tb Objects
#'
#' @param x A `tb` object.
#' @param digits Number of decimal places to display.
#' @param ... Additional arguments (unused).
#' @return Invisibly returns `x`, called for side effects.
#' @export
print.tb <- function(x, digits = NULL, ...) {
  flags        <- attr(x, "flags")
  use_format   <- attr(x, "format") %||% TRUE
  style        <- attr(x, "style")  %||% "n_pct"
  stats        <- attr(x, "stats")
  var_labels   <- attr(x, "var.labels")
  rp_col       <- attr(x, "rp")
  or_col       <- attr(x, "or")
  is_continuous <- attr(x, "is_continuous")
  stat_label   <- attr(x, "stat_label")

  digits <- digits %||% attr(x, "digits") %||% 1L

  # Build display matrix
  if (is_continuous) {
    out_mat <- as.matrix(unclass(x))
    if (!is.null(stat_label)) {
      rownames(out_mat) <- paste0(rownames(out_mat), " [", stat_label, "]")
    }
  } else {
    out_mat <- .build_display_matrix(x, flags, style, digits)
  }

  # Append PR / OR columns
  out_mat <- .append_ratio_cols(out_mat, rp_col, or_col)

  nr  <- nrow(out_mat)
  nc  <- ncol(out_mat)
  dn  <- dimnames(out_mat)
  row_var <- (if (length(var_labels) >= 1) var_labels[1] else names(dn)[1]) %||% ""
  col_var <- (if (length(var_labels) >= 2) var_labels[2] else
    if (length(dn) > 1) names(dn)[2] else "") %||% ""

  if (!use_format) {
    names(dimnames(out_mat))[1] <- row_var
    print(noquote(out_mat))
    if (!is.null(stats)) {
      cat("\nTest:", stats$method, "p-value",
          if (stats$p.value < 0.001) "< 0.001"
          else sprintf("= %.3f", stats$p.value), "\n")
    }
    return(invisible(x))
  }

  .print_grid(out_mat, row_var, col_var, rp_col, or_col,
              is_continuous, x, stats)

  invisible(x)
}

.build_display_matrix <- function(x, flags, style, digits) {
  freq <- unclass(x)
  pct  <- attr(x, "percent")

  if (length(dim(freq)) == 1) {
    freq <- as.matrix(freq)
    colnames(freq) <- "Freq"
    if (!is.null(pct) && length(dim(pct)) != 2) pct <- as.matrix(pct)
  }

  nr      <- nrow(freq)
  nc      <- ncol(freq)
  out_mat <- matrix("", nrow = nr, ncol = nc, dimnames = dimnames(freq))

  for (i in seq_len(nr)) {
    for (j in seq_len(nc)) {
      val     <- freq[i, j]
      has_pct <- !is.null(pct) && i <= nrow(pct) && j <= ncol(pct) && !is.na(pct[i, j])

      out_mat[i, j] <- if (!isTRUE(flags$percent) || !has_pct) {
        as.character(val)
      } else {
        p_str <- sprintf(paste0("%.", digits, "f"), pct[i, j])
        if (style == "n_pct") {
          sprintf("%d (%s%%)", val, p_str)
        } else if (style == "pct_n") {
          sprintf("%s%% (%d)", p_str, val)
        } else {
          txt <- gsub("{n}", val,   style, fixed = TRUE)
          txt <- gsub("{p}", p_str, txt,   fixed = TRUE)
          txt
        }
      }
    }
  }
  out_mat
}

.append_ratio_cols <- function(out_mat, rp_col, or_col) {
  pad_to <- function(col, n) {
    if (length(col) < n) c(col, rep("", n - length(col))) else col
  }
  if (!is.null(rp_col)) {
    out_mat <- cbind(out_mat, "PR (95% CI)" = pad_to(rp_col, nrow(out_mat)))
  }
  if (!is.null(or_col)) {
    out_mat <- cbind(out_mat, "OR (95% CI)" = pad_to(or_col, nrow(out_mat)))
  }
  out_mat
}

.print_grid <- function(out_mat, row_var, col_var, rp_col, or_col,
                        is_continuous, x, stats) {
  safe_nchar    <- function(s) nchar(ifelse(is.na(s), "NA", s))
  center_text   <- function(txt, width) {
    txt <- if (is.na(txt)) "NA" else txt
    pad <- max(0L, width - nchar(txt))
    paste0(strrep(" ", floor(pad / 2)), txt, strrep(" ", pad - floor(pad / 2)))
  }
  right_text    <- function(txt, width) sprintf(paste0("%", width, "s"), if (is.na(txt)) "NA" else txt)

  nr  <- nrow(out_mat)
  nc  <- ncol(out_mat)

  row_labels    <- rownames(out_mat)
  col_labels    <- colnames(out_mat)
  is_1d         <- length(dim(unclass(x))) == 1
  has_row_total <- !is_continuous && (
    (length(dim(unclass(x))) == 2) ||
      (is_1d && !is.null(names(unclass(x))) &&
         utils::tail(names(unclass(x)), 1L) == "Total")
  )
  extra_cols    <- (!is.null(rp_col)) + (!is.null(or_col))

  width_row <- max(safe_nchar(c(row_var, row_labels))) + 1L
  col_widths <- vapply(seq_len(nc), function(j) {
    max(safe_nchar(col_labels[j]), max(safe_nchar(out_mat[, j]))) + 2L
  }, integer(1))

  # Consultar a largura do terminal dinamicamente a cada chamada de print().
  # getOption("width") é atualizado pelo RStudio/terminal quando o usuário redimensiona a janela, mas apenas se o usuário tiver as opções corretas. cli::console_width() usa ioctl() para ler a largura real do PTY em tempo real, funcionando mesmo sem opções configuradas. Fallback para 80 se cli não estiver disponível e options("width") não tiver sido definido.
  console_width <- if (requireNamespace("cli", quietly = TRUE)) {
    cli::console_width()
  } else {
    getOption("width") %||% 80L
  }
  j_start <- 1L

  while (j_start <= nc) {
    used  <- width_row + 3L
    j_end <- j_start
    while (j_end <= nc) {
      if (used + col_widths[j_end] + 1L > console_width && j_end > j_start) {
        j_end <- j_end - 1L
        break
      }
      used  <- used + col_widths[j_end] + 1L
      j_end <- j_end + 1L
    }
    if (j_end > nc) j_end <- nc
    cols_page   <- j_start:j_end
    start_extra <- nc - extra_cols + 1L
    header_cols <- cols_page[cols_page < start_extra]

    if (col_var != "" && length(header_cols) > 0) {
      cat(right_text("", width_row), " | ", sep = "")
      data_w <- sum(col_widths[header_cols]) + max(0L, length(header_cols) - 1L)
      cat(center_text(col_var, data_w), "\n")
    }

    cat(right_text(row_var, width_row), " |", sep = "")
    for (j in cols_page) {
      idx_sum  <- nc - extra_cols
      is_extra <- j > idx_sum
      if (isTRUE(has_row_total) && j == idx_sum && idx_sum > 1 && j != cols_page[1]) cat("|")
      if (is_extra && j != cols_page[1]) cat("|")
      cat(center_text(col_labels[j], col_widths[j]))
    }
    cat("\n", strrep("-", width_row), "-+", sep = "")
    for (j in cols_page) {
      idx_sum  <- nc - extra_cols
      is_extra <- j > idx_sum
      if (isTRUE(has_row_total) && j == idx_sum && idx_sum > 1 && j != cols_page[1]) cat("+")
      if (is_extra && j != cols_page[1]) cat("+")
      cat(strrep("-", col_widths[j]))
    }
    cat("\n")

    for (i in seq_len(nr)) {
      if (i == nr && has_row_total && nr > 1) {
        cat(strrep("-", width_row), "-+", sep = "")
        for (j in cols_page) {
          idx_sum  <- nc - extra_cols
          is_extra <- j > idx_sum
          if (isTRUE(has_row_total) && j == idx_sum && idx_sum > 1 && j != cols_page[1]) cat("+")
          if (is_extra && j != cols_page[1]) cat("+")
          cat(strrep("-", col_widths[j]))
        }
        cat("\n")
      }
      cat(right_text(row_labels[i], width_row), " |", sep = "")
      for (j in cols_page) {
        idx_sum  <- nc - extra_cols
        is_extra <- j > idx_sum
        if (isTRUE(has_row_total) && j == idx_sum && idx_sum > 1 && j != cols_page[1]) cat("|")
        if (is_extra && j != cols_page[1]) cat("|")
        cat(center_text(out_mat[i, j], col_widths[j]))
      }
      cat("\n")
    }
    j_start <- j_end + 1L
    if (j_start <= nc) cat("\n")
  }

  if (!is.null(stats)) {
    p_str <- if (stats$p.value < 0.001) "< 0.001" else sprintf("= %.3f", stats$p.value)
    cat("\n  Test:", stats$method, " p-value", p_str, "\n")
  }
}

# ── as.data.frame

#' Convert tb to Data Frame
#'
#' @param x A `tb` object.
#' @param ... Additional arguments (unused).
#' @return A data.frame with the formatted table.
#' @export
as.data.frame.tb <- function(x, ...) {
  flags        <- attr(x, "flags")
  digits       <- attr(x, "digits") %||% 1L
  style        <- attr(x, "style")  %||% "n_pct"
  rp_col       <- attr(x, "rp")
  or_col       <- attr(x, "or")
  var_labels   <- attr(x, "var.labels")
  stats        <- attr(x, "stats")
  is_continuous <- attr(x, "is_continuous")
  stat_label   <- attr(x, "stat_label")

  if (isTRUE(is_continuous)) {
    out_mat <- as.matrix(unclass(x))
    if (!is.null(stat_label)) {
      rownames(out_mat) <- paste0(rownames(out_mat), " [", stat_label, "]")
    }
  } else {
    out_mat <- .build_display_matrix(x, flags, style, digits)
  }

  out_mat <- .append_ratio_cols(out_mat, rp_col, or_col)

  df <- as.data.frame(out_mat, stringsAsFactors = FALSE)

  extra_col_names <- c(
    if (!is.null(rp_col)) "PR (95% CI)",
    if (!is.null(or_col)) "OR (95% CI)"
  )

  n_core        <- if (length(dim(unclass(x))) == 1) 1L else ncol(unclass(x))
  # Para table 1D, colnames(x) é NULL; o cabeçalho correto é "Freq"
  # (mesmo nome que .build_display_matrix usa para tabelas univariadas).
  original_cols <- colnames(x) %||% if (n_core == 1L) "Freq" else paste0("Col", seq_len(n_core))
  colnames(df)   <- c(original_cols, extra_col_names)

  row_labels <- rownames(x)
  if (isTRUE(is_continuous) && !is.null(stat_label)) {
    row_labels <- paste0(row_labels, " [", stat_label, "]")
  }
  if (is.null(row_labels)) row_labels <- as.character(seq_len(nrow(df)))

  dn          <- dimnames(x)
  row_var_name <- if (!is.null(names(dn)[1])) names(dn)[1] else "Var1"
  if (length(var_labels) >= 1 && !is.na(var_labels[1])) row_var_name <- var_labels[1]

  df <- cbind(Row_Label = row_labels, df)
  colnames(df)[1] <- row_var_name

  attr(df, "stats") <- stats
  df
}

# ── as_flextable  ───────────────────────────────────────────────────────

#' Convert tb Object to Flextable
#'
#' @param x A `tb` object.
#' @param ... Additional arguments passed to `flextable::flextable()`.
#' @return A `flextable` object.
#' @importFrom flextable as_flextable
#' @method as_flextable tb
#' @export
as_flextable.tb <- function(x, ...) {
  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("Package 'flextable' needed.", call. = FALSE)
  }

  df           <- as.data.frame(x)
  stats        <- attr(x, "stats")
  var_labels   <- attr(x, "var.labels")
  rp_col       <- attr(x, "rp")
  or_col       <- attr(x, "or")
  is_cont      <- isTRUE(attr(x, "is_continuous"))
  dn           <- dimnames(x)
  col_var_name <- ""

  if (!is_cont) {
    col_var_name <- if (length(var_labels) >= 2) var_labels[2] else
      if (!is.null(names(dn)[2])) names(dn)[2] else ""
  }

  ft <- flextable::flextable(df)

  if (col_var_name != "" && ncol(df) > 2) {
    has_sum <- any(names(df) %in% c("Total", "Sum"))
    has_rp  <- !is.null(rp_col)
    has_or  <- !is.null(or_col)
    n_total <- ncol(df)
    n_right <- (if (has_sum) 1L else 0L) + (if (has_rp) 1L else 0L) + (if (has_or) 1L else 0L)
    n_cats  <- n_total - 1L - n_right

    if (n_cats > 0) {
      values <- c("", col_var_name)
      widths <- c(1L, n_cats)
      if (has_sum) { values <- c(values, ""); widths <- c(widths, 1L) }
      if (has_rp)  { values <- c(values, ""); widths <- c(widths, 1L) }
      if (has_or)  { values <- c(values, ""); widths <- c(widths, 1L) }
      ft <- flextable::add_header_row(ft, values = values, colwidths = widths)
    }
  }

  if (!is.null(stats)) {
    p_str     <- if (stats$p.value < 0.001) "< 0.001" else sprintf("= %.3f", stats$p.value)
    stat_text <- paste0(stats$method, ": p-value ", p_str)
    ft        <- flextable::add_footer_lines(ft, values = stat_text)
  }

  ft |>
    flextable::theme_booktabs() |>
    flextable::autofit() |>
    flextable::align(align = "center", part = "header") |>
    flextable::align(j = -1, align = "center", part = "body") |>
    flextable::align(j = 1,  align = "left",   part = "body") |>
    (\(f) if (!is.null(stats)) flextable::align(f, part = "footer", align = "right") else f)()
}

if (requireNamespace("flextable", quietly = TRUE)) {
  s3_register <- function(generic, class, method = NULL) {
    if (is.null(method)) method <- get(paste(generic, class, sep = "."), mode = "function")
    generic <- match.fun(generic)
    registerS3method(generic, class, method, envir = parent.frame())
  }
  tryCatch(
    s3_register("flextable::as_flextable", "tb", as_flextable.tb),
    error = function(e) NULL
  )
}
