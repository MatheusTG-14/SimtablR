# BIVARIATE CONTINGENCY TABLE STATISTICAL ENGINE
# Fast two-way cross-tabulation and continuous group summary computation engine.
# Computes raw frequencies, margins, 2x2 effects (PR/RR/OR), Mantel-Haenszel pooling, and tests.

#########
# BIVARIATE ENGINE DISPATCH
# Orchestrates 1D/2D frequency cross-tabulations and continuous summary calculations.

#' Execute bivariate statistical engine on a specification and dataset
#' @keywords internal
#' @noRd
.engine_tb <- function(spec, data) {
  args <- .tb_args_from_spec(spec)
  vars <- lapply(args$var_names, function(nm) {
    if (!nm %in% names(data)) {
      simtab_abort_binding(c(
        "Variable {.val {nm}} not found.",
        "i" = "Columns available: {.val {names(data)}}.",
        "v" = "Check the spelling, or select an existing column."
      ))
    }
    val <- data[[nm]]
    if (!is.atomic(val) && !is.factor(val)) {
      simtab_abort_binding(c(
        "Variable {.val {nm}} must be atomic or a factor.",
        "i" = "Received a column of class {.cls {class(val)[[1]]}}.",
        "v" = "List columns and matrices cannot be tabulated; flatten it first."
      ))
    }
    val
  })
  names(vars) <- args$var_names
  var_labels <- vapply(
    args$var_names,
    function(nm) .resolve_label(nm, data, args$labels),
    character(1)
  )

  row_var_name <- args$var_names[1]
  row_label <- var_labels[1]
  col_var_name <- if (length(vars) == 2) args$var_names[2] else NULL
  col_label <- if (length(vars) == 2) var_labels[2] else NULL

  strat_val <- NULL
  if (!is.null(args$strat)) {
    strat_val <- .tb_eval_expr(
      args$strat,
      data,
      args$strat_env,
      "Stratification variable not found."
    )
    if (length(strat_val) != length(vars[[1]])) {
      simtab_abort_binding(c(
        "Stratification variable length mismatch.",
        "i" = "The stratifier has {length(strat_val)} values but the analysis
               variables have {length(vars[[1]])}.",
        "v" = "Use a column from the same data frame, not an external vector."
      ))
    }
  }

  if (!is.null(args$subset)) {
    subset_val <- tryCatch(
      eval(args$subset, data, args$subset_env),
      error = function(e) {
        simtab_abort_input(c(
          "Could not evaluate {.arg subset}.",
          "i" = "The expression failed with: {conditionMessage(e)}",
          "v" = "Refer only to columns of the data frame, e.g. {.code subset = age > 65}."
        ))
      }
    )
    if (!is.logical(subset_val)) {
      simtab_abort_input(c(
        "{.arg subset} must evaluate to a logical vector.",
        "i" = "It produced a value of class {.cls {class(subset_val)[[1]]}}.",
        "v" = "Use a comparison, e.g. {.code subset = age > 65}."
      ))
    }

    keep <- subset_val & !is.na(subset_val)
    if (sum(keep) == 0) {
      simtab_abort_input(c(
        "{.arg subset} removed all observations.",
        "i" = "No row satisfied the condition, so there is nothing to tabulate.",
        "v" = "Loosen the condition, or check the values it compares against."
      ))
    }

    vars <- lapply(vars, `[`, keep)
    if (!is.null(strat_val)) {
      strat_val <- strat_val[keep]
    }
  }

  measure <- args$measure
  mh_col_val <- if (!is.null(strat_val) && !is.null(measure) && length(vars) == 2) {
    vars[[2]]
  } else {
    NULL
  }

  is_stratified <- FALSE
  strat_var_name <- NULL
  if (!is.null(strat_val)) {
    strat_var_name <- .tb_expr_name(args$strat)
    if (length(vars) == 2) {
      if (!args$flags$missing) {
        ok <- !is.na(strat_val)
        vars <- lapply(vars, `[`, ok)
        if (!is.null(mh_col_val)) {
          mh_col_val <- mh_col_val[ok]
        }
        strat_val <- strat_val[ok]
      }
      vars[[2]] <- interaction(
        strat_val,
        vars[[2]],
        sep = " : ",
        drop = TRUE,
        lex.order = TRUE
      )
      is_stratified <- TRUE
    } else {
      vars[[2]] <- factor(strat_val)
    }
    col_var_name <- if (is.null(col_var_name)) {
      "Stratum"
    } else {
      paste0(col_var_name, " (Stratified)")
    }
    col_label <- col_var_name
  }

  type_override <- .resolve_per_var(args$var.type, row_var_name, NULL)
  is_continuous <- identical(
    .detect_var_type(vars[[1]], override = type_override),
    "continuous"
  )
  if (is.null(type_override) && is_continuous) {
    message(sprintf(
      paste0(
        "Variable '%s' automatically treated as continuous because it is numeric. ",
        "Use 'var.type' to override."
      ),
      row_var_name
    ))
  }
  if (is.null(type_override) && !is_continuous &&
      is.numeric(vars[[1]]) && !is.factor(vars[[1]])) {
    message(sprintf(
      paste0(
        "Variable '%s' automatically treated as categorical because it has ",
        "only %d distinct integer value(s). Use 'var.type' to override."
      ),
      row_var_name,
      length(unique(vars[[1]][!is.na(vars[[1]])]))
    ))
  }

  res_data <- list()
  res_meta <- list(
    is_continuous = is_continuous,
    stat.cont = args$stat.cont,
    stat.cont_requested = args$stat.cont,
    d = args$d,
    big_mark = args$big_mark %||% "",
    decimal_mark = args$decimal_mark %||% ".",
    style = args$style,
    style.rp = args$style.rp,
    style.or = args$style.or,
    conf.level = args$conf.level,
    labels = args$labels,
    row_var_name = row_var_name,
    row_label = row_label,
    col_var_name = col_var_name,
    col_label = col_label,
    flags = args$flags,
    is_stratified = is_stratified,
    strat_var_name = strat_var_name,
    stats = NULL,
    test_notes = list(),
    p.adjust = args$p.adjust,
    engine = "bivariate"
  )

  if (is_continuous) {
    y <- vars[[1]]
    x <- if (length(vars) > 1) vars[[2]] else NULL
    test_y <- y
    test_x <- x
    stat_v <- args$stat.cont

    if (!is.numeric(y)) {
      simtab_abort_binding(c(
        "Variable {.val {row_var_name}} is not numeric.",
        "i" = "It was requested as a continuous summary but has class
               {.cls {class(y)[[1]]}}.",
        "v" = "Convert it, or set {.code var.type = \"categorical\"}."
      ))
    }
    .check_finite_continuous(y, row_var_name)

    if (!is.null(x)) {
      if (is.factor(x)) {
        x <- droplevels(x)
      }
      if (!args$flags$missing) {
        ok <- !is.na(x) & !is.na(y)
        x <- x[ok]
        y <- y[ok]
      }
    } else {
      y <- y[!is.na(y)]
    }

    if (identical(stat_v, "auto")) {
      summary_auto <- .auto_summary_decision(y)
      stat_v <- summary_auto$decision
      res_meta$stat.cont <- stat_v
      res_meta$summary_auto <- summary_auto
    }

    calc_raw_stats <- function(val) {
      if (length(val) == 0) {
        return(numeric(0))
      }
      if (stat_v == "mean") {
        return(c(mean = mean(val, na.rm = TRUE), sd = .sample_sd_stable(val)))
      }
      as.numeric(stats::quantile(
        val,
        probs = c(0.5, 0.25, 0.75),
        na.rm = TRUE,
        type = 7
      ))
    }

    summary_list <- list()
    counts_list <- numeric(0)
    if (is.null(x)) {
      summary_list[["Total"]] <- calc_raw_stats(y)
      counts_list["Total"] <- sum(!is.na(y))
    } else {
      levs <- if (is.factor(x)) levels(x) else sort(unique(x))
      if (args$flags$missing && any(is.na(x))) {
        levs <- c(levs, NA)
      }

      for (l in levs) {
        sub_y <- if (is.na(l)) y[is.na(x)] else y[x == l & !is.na(x)]
        lbl_l <- if (is.na(l)) "<NA>" else as.character(l)
        summary_list[[lbl_l]] <- calc_raw_stats(sub_y)
        counts_list[lbl_l] <- sum(!is.na(sub_y))
      }
      summary_list[["Total"]] <- calc_raw_stats(y)
      counts_list["Total"] <- sum(!is.na(y))
    }

    res_data$summary <- summary_list
    res_data$counts <- counts_list

    stats_res <- NULL
    if ((isTRUE(args$test) || is.character(args$test)) && !is.null(x)) {
      if (isTRUE(args$paired)) {
        n_groups_paired <- length(unique(test_x[!is.na(test_x)]))
        if (n_groups_paired != 2) {
          simtab_abort_input(c(
            "{.code paired = TRUE} requires exactly 2 groups.",
            "i" = "Found {n_groups_paired} group(s) in the comparison variable.",
            "v" = "Restrict the data to two groups, or use {.code paired = FALSE}."
          ))
        }
        pair <- .paired_group_values(as.numeric(test_y), test_x)
      }
      tryCatch(
        {
          n_groups <- length(unique(x[!is.na(x)]))
          if (n_groups >= 2) {
            if (isTRUE(args$paired)) {
              keep_pair <- stats::complete.cases(pair[[1]], pair[[2]])
              if (sum(keep_pair) >= 2) {
                stats_res <- if (stat_v == "mean") {
                  stats::t.test(pair[[1]][keep_pair], pair[[2]][keep_pair], paired = TRUE)
                } else {
                  stats::wilcox.test(pair[[1]][keep_pair], pair[[2]][keep_pair], paired = TRUE, exact = FALSE)
                }
              }
            } else if (stat_v == "mean") {
              stats_res <- if (n_groups == 2) {
                stats::t.test(y ~ x)
              } else {
                fit <- stats::lm(y ~ x)
                list(
                  p.value = stats::anova(fit)$`Pr(>F)`[1],
                  method = "One-way ANOVA"
                )
              }
            } else {
              stats_res <- if (n_groups == 2) {
                stats::wilcox.test(y ~ x, exact = FALSE)
              } else {
                stats::kruskal.test(y ~ x)
              }
            }
          }
        },
        error = function(e) NULL
      )
    }
    adj <- .tb_adjust_test_p(stats_res, args$p.adjust)
    stats_res <- adj$stats
    if (!is.null(adj$tests)) {
      res_data$tests <- adj$tests
    }
    res_meta$stats <- stats_res
  } else {
    if (!is.null(args$ref)) {
      row_var <- if (is.factor(vars[[1]])) vars[[1]] else factor(vars[[1]])
      ref_str <- as.character(args$ref)
      if (ref_str %in% levels(row_var)) {
        vars[[1]] <- stats::relevel(row_var, ref = ref_str)
      } else if (is.numeric(args$ref) && args$ref %in% seq_along(levels(row_var))) {
        vars[[1]] <- stats::relevel(row_var, ref = levels(row_var)[args$ref])
      } else {
        simtab_abort_input(c(
          "Reference level {.val {ref_str}} not found in row variable levels.",
          "i" = "Levels available: {.val {levels(row_var)}}.",
          "v" = "Name one of the listed levels, or pass its position as a number."
        ))
      }
    } else if (!is.null(measure)) {
      levs <- if (is.factor(vars[[1]])) {
        levels(vars[[1]])
      } else {
        levels(factor(vars[[1]]))
      }
      message(sprintf(
        paste0(
          "Note: No reference level specified for PR/OR calculation. ",
          "Defaulting to the first level: '%s'."
        ),
        levs[1]
      ))
    }

    useNA <- if (args$flags$missing) "always" else "no"
    tab <- if (!isTRUE(is_stratified) && identical(useNA, "no")) {
      .tabulate_factor_table(vars)
    } else {
      NULL
    }
    if (is.null(tab)) {
      vars <- lapply(vars, function(v) if (is.factor(v)) droplevels(v) else v)
      tab <- table(vars, useNA = useNA)
    }

    if (sum(tab) == 0) {
      simtab_abort_engine(c(
        "The contingency table is empty.",
        "i" = "Every observation was missing on at least one of the tabulated variables.",
        "v" = "Check the variables for all-missing columns, or widen the subset."
      ))
    }

    # Effect ratios and association tests are computed on the complete-case
    # table. Under the `miss`/`m` flag, `tab` carries <NA> margin rows/columns
    # for DISPLAY only; including them would treat missingness as an
    # exposure/outcome level -- picking the <NA> column as the "event" column,
    # inflating the risk denominators, and turning a 2x2 chi-squared into a 3x3
    # test. `tab` (with the NA margins) is still used below for the frequency
    # and percentage display.
    tab_cc <- if (all(vapply(vars, function(v) !anyNA(v), logical(1)))) {
      .drop_zero_table_margins(tab)
    } else {
      .drop_na_table_margins(vars)
    }

    pct_full <- NULL
    if (args$flags$percent) {
      if (length(dim(tab)) == 1) {
        pct_full <- as.vector(tab / sum(tab) * 100)
      } else {
        nr <- nrow(tab)
        nc <- ncol(tab)
        pct_full <- matrix(NA_real_, nr, nc)
        pct_calc <- switch(
          args$flags$by,
          total = tab / sum(tab),
          row = tab / rowSums(tab),
          col = sweep(tab, 2, colSums(tab), "/")
        )
        pct_full[seq_len(nr), seq_len(nc)] <- pct_calc * 100
      }
    }

    mh_df <- NULL
    if (isTRUE(is_stratified) && !is.null(measure) && !is.null(mh_col_val)) {
      mh_df <- .tb_mh_from_vectors(
        row = vars[[1]],
        outcome = mh_col_val,
        strat = strat_val,
        measure = measure,
        conf.level = args$conf.level,
        variable = row_var_name
      )
    }

    ratios_df <- NULL
    if (!is.null(measure) && !isTRUE(is_stratified) && length(dim(tab_cc)) == 2 && ncol(tab_cc) >= 2) {
      event_col_idx <- ncol(tab_cc)
      total_events <- sum(tab_cc[, event_col_idx])
      total_n <- sum(tab_cc)
      prev <- total_events / total_n

      # A ratio measure is a two-outcome quantity. With three or more outcome
      # columns the last level is the "event" and every earlier level becomes the
      # non-event denominator; that collapse is a modelling decision, so announce
      # which level was scored rather than letting it pass as a plain 2x2 result.
      if (ncol(tab_cc) > 2L) {
        warning(
          sprintf(
            paste0(
              "%s treats '%s' as a binary outcome: level '%s' is scored as the event and ",
              "level(s) %s are pooled as the non-event. Collapse the outcome yourself, ",
              "or subset to two levels, if that is not the contrast you want."
            ),
            measure,
            col_var_name %||% "the column variable",
            colnames(tab_cc)[event_col_idx],
            paste0("'", colnames(tab_cc)[-event_col_idx], "'", collapse = ", ")
          ),
          call. = FALSE,
          immediate. = TRUE
        )
      }

      row_levs <- levels(factor(vars[[1]]))
      events <- stats::setNames(numeric(length(row_levs)), row_levs)
      totals <- stats::setNames(numeric(length(row_levs)), row_levs)
      matched_rows <- intersect(row_levs, rownames(tab_cc))
      events[matched_rows] <- tab_cc[matched_rows, event_col_idx]
      totals[matched_rows] <- rowSums(tab_cc[matched_rows, , drop = FALSE])

      ratios_df <- data.frame(
        variable = rep(row_var_name, length(row_levs)),
        level = row_levs,
        estimate = rep(NA_real_, length(row_levs)),
        lower_ci = rep(NA_real_, length(row_levs)),
        upper_ci = rep(NA_real_, length(row_levs)),
        p_value = rep(NA_real_, length(row_levs)),
        ref = rep(FALSE, length(row_levs)),
        type = rep(measure, length(row_levs))
      )
      ratios_df$ref[1] <- TRUE
      if (totals[1] > 0) {
        ratios_df$estimate[1] <- 1
      }

      calc <- if (measure %in% c("PR", "RR")) .calc_pr_katz else .calc_or_woolf
      corrected_levels <- character(0)
      for (i in seq_along(row_levs)[-1]) {
        res_i <- calc(events[i], totals[i], events[1], totals[1], args$conf.level)
        if (!is.na(res_i$estimate)) {
          ratios_df$estimate[i] <- res_i$estimate
          ratios_df$lower_ci[i] <- res_i$lower
          ratios_df$upper_ci[i] <- res_i$upper
          ratios_df$p_value[i] <- res_i$p
          if (isTRUE(res_i$corrected)) {
            corrected_levels <- c(corrected_levels, row_levs[i])
          }
        }
      }
      if (length(corrected_levels) > 0) {
        res_meta$test_notes <- c(res_meta$test_notes, list(list(
          id = "zero_cell_correction",
          variable = row_var_name,
          levels = corrected_levels,
          measure = measure
        )))
      }

      na_levels <- ratios_df$level[is.na(ratios_df$estimate)]
      if (length(na_levels) > 0) {
        warning(
          sprintf(
            "PR/OR not estimable for level(s): %s (zero cell). Reported as NA.",
            paste(na_levels, collapse = ", ")
          ),
          call. = FALSE,
          immediate. = TRUE
        )
      }
    }

    # An effect measure was asked for but nothing could be estimated: without
    # this the requested ratio column simply renders empty, which reads as "no
    # association" rather than "not computed".
    if (!is.null(measure) && is.null(ratios_df) && is.null(mh_df)) {
      warning(
        sprintf(
          "%s was requested but could not be estimated for '%s': %s.",
          measure,
          row_var_name,
          .tb_no_effect_reason(tab_cc, is_stratified, strat_val, args$flags$missing)
        ),
        call. = FALSE,
        immediate. = TRUE
      )
    }

    res_data$frequencies <- tab
    res_data$percentages <- pct_full
    res_data$ratios <- ratios_df
    res_data$mh <- mh_df

    stats_res <- NULL
    if ((isTRUE(args$test) || is.character(args$test)) && length(dim(tab_cc)) == 2) {
      run_fisher <- function(tb_in) {
        tryCatch(
          stats::fisher.test(tb_in, workspace = 2e7),
          error = function(e) {
            sim <- stats::fisher.test(tb_in, simulate.p.value = TRUE, B = 1e5)
            sim$method <- paste0(sim$method, " (Monte Carlo simulation)")
            sim
          }
        )
      }

      if (!is.null(mh_df) && any(mh_df$row_type == "pooled")) {
        pooled <- mh_df[mh_df$row_type == "pooled", , drop = FALSE][1, ]
        stats_res <- list(
          statistic = stats::setNames(pooled$cmh_statistic, "Mantel-Haenszel X-squared"),
          parameter = stats::setNames(pooled$cmh_df, "df"),
          p.value = pooled$cmh_p,
          method = "Cochran-Mantel-Haenszel test",
          data.name = paste(row_var_name, "by", col_var_name, "stratified by", strat_var_name)
        )
        class(stats_res) <- "htest"
      } else if (identical(args$test, "trend") ||
          (isTRUE(args$test) && is.ordered(vars[[1]]) && nrow(tab_cc) >= 3 && ncol(tab_cc) == 2)) {
        stats_res <- .tb_trend_htest(vars[[1]], vars[[2]], row_var_name, col_var_name)
      } else if (isTRUE(args$test)) {
        decision <- .campbell_expected_decision(tab_cc)
        stats_res <- tryCatch(
          if (identical(decision$method, "fisher")) {
            run_fisher(tab_cc)
          } else {
            suppressWarnings(.chisq_campbell_or_pearson(tab_cc))
          },
          error = function(e) NULL
        )
        if (!is.null(stats_res)) {
          stats_res$simtab_note <- .with_campbell_variable(
            decision$note,
            paste(row_var_name, "by", col_var_name)
          )
        }
      } else if (identical(args$test, "chisq")) {
        stats_res <- tryCatch(
          suppressWarnings(.chisq_campbell_or_pearson(tab_cc)),
          error = function(e) NULL
        )
        exp_counts <- tryCatch(
          suppressWarnings(stats::chisq.test(tab_cc, correct = FALSE)$expected),
          error = function(e) NULL
        )
        if (!is.null(exp_counts)) {
          n_small <- sum(exp_counts < 5)
          if (n_small > 0) {
            warning(
              sprintf(
                paste0(
                  "Chi-squared test: %d cell(s) (%d%%) have expected counts < 5 (min: %.1f).\n",
                  "Fisher's exact test is recommended for sparse tables. ",
                  "Set test = \"fisher\" to apply it."
                ),
                n_small,
                round(100 * n_small / length(exp_counts)),
                round(min(exp_counts), 1)
              ),
              call. = FALSE,
              immediate. = TRUE
            )
          }
        }
      } else if (identical(args$test, "fisher")) {
        stats_res <- tryCatch(run_fisher(tab_cc), error = function(e) NULL)
      } else if (identical(args$test, "mcnemar")) {
        stats_res <- tryCatch(stats::mcnemar.test(tab_cc), error = function(e) NULL)
      }
    }
    adj <- .tb_adjust_test_p(stats_res, args$p.adjust)
    stats_res <- adj$stats
    if (!is.null(adj$tests)) {
      res_data$tests <- adj$tests
    }
    res_meta$stats <- stats_res
    if (!is.null(stats_res$simtab_note)) {
      res_meta$test_notes <- c(res_meta$test_notes, list(stats_res$simtab_note))
    }
  }

  list(data = res_data, meta = res_meta)
}

#########
# EFFECT ESTIMATION AND HYPOTHESIS TESTING DIAGNOSTICS
# Explanatory diagnostics for non-estimable measures and test p-value recording.

#' Explain why a requested effect ratio could not be computed
#' @keywords internal
#' @noRd
.tb_no_effect_reason <- function(tab_cc, is_stratified, strat_val, show_missing) {
  if (isTRUE(is_stratified)) {
    n_strata <- length(unique(strat_val[!is.na(strat_val)]))
    if (n_strata < 2L) {
      return(sprintf(
        "the stratification variable has %d usable level(s), so there is nothing to pool across (drop `strat` for the crude estimate)",
        n_strata
      ))
    }
    return("no stratum contained both exposure groups and both outcome levels")
  }
  if (length(dim(tab_cc)) != 2L) {
    return("the comparison needs a second variable to tabulate against")
  }
  if (ncol(tab_cc) < 2L) {
    return(sprintf(
      "the outcome has only %d observed level(s) among complete cases, so no ratio of two proportions exists",
      ncol(tab_cc)
    ))
  }
  if (nrow(tab_cc) < 2L) {
    return("the exposure has only one observed level among complete cases, so there is no contrast to estimate")
  }
  "the complete-case table is degenerate"
}

#' Record multiplicity-adjusted p-values on a test result
#' @keywords internal
#' @noRd
.tb_adjust_test_p <- function(stats_res, method) {
  if (is.null(stats_res) || identical(method, "none")) {
    return(list(stats = stats_res, tests = NULL))
  }
  raw_p <- stats_res$p.value
  # Every multiplicity method is the identity on a family of one, so recording an
  # "adjusted" p-value here would relabel the column while leaving the number
  # untouched -- a claim of multiplicity control that was never applied. A tb()
  # table carries exactly one p-value; the honest response is to say so and
  # leave the display unadjusted.
  if (length(raw_p) < 2L) {
    warning(
      sprintf(
        paste0(
          "p.adjust = \"%s\" was ignored: a tb() table contains a single p-value, ",
          "so there is no family to adjust. Use table1(...) |> test(p.adjust = \"%s\") to control ",
          "multiplicity across a family of variables."
        ),
        method, method
      ),
      call. = FALSE
    )
    return(list(stats = stats_res, tests = NULL))
  }
  adj_p <- stats::p.adjust(raw_p, method = method)
  stats_res$p.value.raw <- raw_p
  stats_res$p.value.adjusted <- adj_p
  stats_res$p.adjust.method <- method
  list(
    stats = stats_res,
    tests = data.frame(
      method = stats_res$method,
      p_value = raw_p,
      p_value_adjusted = adj_p,
      p_adjust_method = method
    )
  )
}

#########
# SPECIFICATION RESOLUTION AND MARGIN ADJUSTMENT
# Extracts bivariate engine parameters and rebuilds complete-case marginal tables.

#' Extract bivariate engine execution arguments from specification object
#' @keywords internal
#' @noRd
.tb_args_from_spec <- function(spec) {
  tb <- spec$engine_opts$bivariate
  if (is.null(tb)) {
    simtab_abort_spec(c(
      "Malformed bivariate specification: missing bivariate engine options.",
      "i" = "The {.val bivariate} engine was selected without its options block.",
      "v" = "Build the specification through {.fn tb} rather than by hand."
    ))
  }

  list(
    var_names = tb$var_names,
    flags = tb$flags,
    style = spec$style,
    style.rp = tb$style.rp,
    style.or = tb$style.or,
    subset = if (identical(tb[["subset"]], quote(NULL))) NULL else tb[["subset"]],
    subset_env = tb[["subset_env"]] %||% baseenv(),
    strat = if (identical(tb[["strat"]], quote(NULL))) NULL else tb[["strat"]],
    strat_env = tb[["strat_env"]] %||% baseenv(),
    measure = spec$effect$measure,
    ref = spec$effect$ref,
    conf.level = spec$effect$conf.level %||% 0.95,
    var.type = tb$var.type,
    stat.cont = tb$stat.cont %||% "auto",
    labels = spec$fmt$labels,
    d = as.integer(spec$fmt$d %||% 1),
    big_mark = spec$fmt$big_mark %||% "",
    decimal_mark = spec$fmt$decimal_mark %||% ".",
    test = tb$test,
    p.adjust = tb$p.adjust %||% "none",
    paired = isTRUE(tb$paired)
  )
}

#' Rebuild contingency table from complete cases dropping NA margins
#' @keywords internal
#' @noRd
.drop_na_table_margins <- function(vectors) {
  keep <- Reduce(`&`, lapply(vectors, function(v) !is.na(v)))
  table(lapply(vectors, function(v) {
    out <- v[keep]
    if (is.factor(out)) droplevels(out) else out
  }), useNA = "no")
}

#' Remove zero-count margins from a contingency table without rebuilding
#' @keywords internal
#' @noRd
.drop_zero_table_margins <- function(tab) {
  dims <- dim(tab)
  if (length(dims) == 1L) {
    return(tab[tab > 0])
  }
  if (length(dims) == 2L) {
    return(tab[
      rowSums(tab, na.rm = TRUE) > 0,
      colSums(tab, na.rm = TRUE) > 0,
      drop = FALSE
    ])
  }
  tab
}

#' Build fast integer-coded contingency table for clean factors
#' @keywords internal
#' @noRd
.tabulate_factor_table <- function(vars) {
  if (!length(vars) %in% 1:2 ||
      !all(vapply(vars, is.factor, logical(1))) ||
      any(vapply(vars, is.ordered, logical(1))) ||
      any(vapply(vars, anyNA, logical(1)))) {
    return(NULL)
  }

  dims <- unname(vapply(vars, nlevels, integer(1)))
  cells <- prod(as.double(dims))
  if (any(dims < 1L) || cells > .Machine$integer.max) {
    return(NULL)
  }

  codes <- lapply(vars, unclass)
  index <- if (length(vars) == 1L) {
    codes[[1L]]
  } else {
    codes[[1L]] + (codes[[2L]] - 1L) * dims[[1L]]
  }
  dimnames <- lapply(vars, levels)
  names(dimnames) <- names(vars)

  out <- structure(
    tabulate(index, nbins = as.integer(cells)),
    dim = dims,
    dimnames = dimnames,
    class = "table"
  )
  .drop_zero_table_margins(out)
}

#########
# EXPRESSION EVALUATION AND STRATIFIED MANTEL-HAENSZEL POOLING
# Safe subsetting evaluation and multi-stratum Mantel-Haenszel effect synthesis.

#' Safely evaluate stratification or subsetting expression in data environment
#' @keywords internal
#' @noRd
.tb_eval_expr <- function(expr, data, env, error_message) {
  if (is.character(expr) && length(expr) == 1) {
    expr <- as.symbol(expr)
  }

  tryCatch(
    eval(expr, data, env),
    # `error_message` is caller-supplied text: interpolating it as a value keeps
    # any braces it contains from being read as cli markup.
    error = function(e) {
      simtab_abort_binding(c(
        "{error_message}",
        "i" = "The expression failed with: {conditionMessage(e)}",
        "v" = "Refer to a column of the data frame being analysed."
      ))
    }
  )
}

#' Deparse expression into a concise variable name string
#' @keywords internal
#' @noRd
.tb_expr_name <- function(expr) {
  if (is.character(expr) && length(expr) == 1) {
    return(expr)
  }
  deparse(expr, width.cutoff = 500L)[1]
}

#' Wrap Cochran-Armitage test into an htest class structure
#' @keywords internal
#' @noRd
.tb_trend_htest <- function(row, col, row_name, col_name) {
  tr <- .cochran_armitage_test(row, col)
  if (is.null(tr)) {
    return(NULL)
  }
  out <- list(
    statistic = stats::setNames(tr$statistic, "X-squared"),
    parameter = stats::setNames(tr$parameter, "df"),
    p.value = tr$p.value,
    method = tr$method,
    data.name = paste(row_name, "by", col_name)
  )
  class(out) <- "htest"
  out
}

#' Compute stratum-specific and pooled Mantel-Haenszel association measures
#' @keywords internal
#' @noRd
.tb_mh_from_vectors <- function(row, outcome, strat, measure, conf.level, variable) {
  ok <- !is.na(row) & !is.na(outcome) & !is.na(strat)
  row <- droplevels(factor(row[ok]))
  outcome <- droplevels(factor(outcome[ok]))
  strat <- droplevels(factor(strat[ok]))
  if (nlevels(row) < 2 || nlevels(outcome) != 2 || nlevels(strat) < 2) {
    return(NULL)
  }

  ref_level <- levels(row)[1]
  event_level <- levels(outcome)[nlevels(outcome)]
  nonevent_level <- levels(outcome)[1]
  calc_crude <- if (measure %in% c("PR", "RR")) .calc_pr_katz else .calc_or_woolf
  calc_pooled <- if (measure %in% c("PR", "RR")) .calc_mh_rr else .calc_mh_or

  rows <- list()
  for (level in levels(row)[-1]) {
    strata_tabs <- lapply(levels(strat), function(stratum_level) {
      in_stratum <- strat == stratum_level
      a_index <- sum(row == level & outcome == event_level & in_stratum)
      b_index <- sum(row == level & outcome == nonevent_level & in_stratum)
      a_ref <- sum(row == ref_level & outcome == event_level & in_stratum)
      b_ref <- sum(row == ref_level & outcome == nonevent_level & in_stratum)
      matrix(
        c(a_index, b_index, a_ref, b_ref),
        nrow = 2,
        byrow = TRUE,
        dimnames = list(c(level, ref_level), c(event_level, nonevent_level))
      )
    })
    names(strata_tabs) <- levels(strat)

    for (stratum_level in names(strata_tabs)) {
      tab <- strata_tabs[[stratum_level]]
      crude <- calc_crude(
        tab[1, 1],
        sum(tab[1, ]),
        tab[2, 1],
        sum(tab[2, ]),
        conf.level
      )
      rows[[length(rows) + 1]] <- data.frame(
        variable = variable,
        level = level,
        stratum = stratum_level,
        row_type = "stratum",
        estimate = crude$estimate,
        lower_ci = crude$lower,
        upper_ci = crude$upper,
        p_value = crude$p,
        cmh_statistic = NA_real_,
        cmh_df = NA_real_,
        cmh_p = NA_real_,
        homogeneity_statistic = NA_real_,
        homogeneity_df = NA_real_,
        homogeneity_p = NA_real_,
        ref = FALSE,
        type = measure,
        event_level = event_level
      )
    }

    pooled <- calc_pooled(strata_tabs, conf.level = conf.level)
    cmh <- .calc_mh_or(strata_tabs, conf.level = conf.level)
    bd <- .breslow_day(strata_tabs)
    rows[[length(rows) + 1]] <- data.frame(
      variable = variable,
      level = level,
      stratum = "Mantel-Haenszel pooled",
      row_type = "pooled",
      estimate = pooled$estimate,
      lower_ci = pooled$lower,
      upper_ci = pooled$upper,
      p_value = cmh$p,
      cmh_statistic = cmh$statistic,
      cmh_df = cmh$parameter,
      cmh_p = cmh$p,
      homogeneity_statistic = bd$statistic,
      homogeneity_df = bd$parameter,
      homogeneity_p = bd$p,
      ref = FALSE,
      type = measure,
      event_level = event_level
    )
  }

  if (length(rows) == 0) {
    return(NULL)
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
