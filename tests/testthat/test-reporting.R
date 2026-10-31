library(testthat)
library(SimtablR)
data(epitabl, package = "SimtablR")

test_that("sensitivity computes unmeasured confounding bounds and variations", {
  tab <- suppressMessages(table1(epitabl, "hypertension", by = "adjudicated_acs", measure = "PR", ref = "No"))
  sens <- suppressMessages(sensitivity(tab, denominator = "complete"))
  expect_s3_class(sens, "simtab_sensitivity")
  expect_s3_class(sens, "simtab_report")
})

test_that("e_value calculates E-values from simtab_result", {
  tab <- suppressMessages(tb(epitabl, hypertension, adjudicated_acs, flags = "rr"))
  ev <- e_value(tab)
  expect_s3_class(ev, "simtab_e_value")
  expect_s3_class(ev, "simtab_result")
  expect_true(is.numeric(ev$data$e_value))
})

test_that("e_value on a tb result skips reference rows and names levels", {
  tab <- suppressMessages(tb(epitabl, smoking, diabetes, or = TRUE, ref = "Never"))
  ev <- e_value(tab)
  expect_identical(nrow(ev$data), 2L)
  expect_setequal(ev$data$term, c("smoking: Former", "smoking: Current"))
  expect_false(any(ev$data$estimate == 1 & is.na(ev$data$conf.low)))
})

test_that("flow tracks participant flow and exclusions", {
  tab <- suppressMessages(table1(epitabl, "age", by = "adjudicated_acs"))
  fl <- flow(tab)
  expect_s3_class(fl, "simtab_flow")
  expect_s3_class(fl, "simtab")
})

test_that("codebook produces data dictionary summary of dataset", {
  cb <- codebook(epitabl[, c("age", "sex", "adjudicated_acs")])
  expect_s3_class(cb, "simtab_codebook")
  expect_s3_class(cb, "data.frame")
  expect_true(all(c("variable", "type") %in% names(cb)))
})

test_that("strobe returns STROBE reporting checklist for descriptive result", {
  tab <- suppressMessages(table1(epitabl, "age", by = "adjudicated_acs"))
  st <- strobe(tab)
  expect_s3_class(st, "simtab_checklist")
})

test_that("stard returns STARD reporting checklist for diagnostic result", {
  dat <- make_diag_data(tp = 36, fp = 8, fn = 4, tn = 52)
  dres <- suppressMessages(diag_test(dat, test = rapid, ref = gold, positive = "Yes"))
  sd <- stard(dres)
  expect_s3_class(sd, "simtab_checklist")
})

test_that("why explains design and analysis choices", {
  tab <- suppressMessages(tb(epitabl, hypertension, adjudicated_acs, flags = "rr"))
  wh <- why(tab)
  expect_s3_class(wh, "simtab_explanation")
  expect_output(print(wh))
})

test_that("as_methods generates reproducible methods paragraph", {
  tab <- suppressMessages(table1(epitabl, vars = c("age", "sex"), by = "adjudicated_acs"))
  meth <- as_methods(tab)
  expect_type(meth, "character")
  expect_true(nzchar(meth))
})


test_that("sensitivity(adjust = NULL) warns when there is no adjustment to drop", {
  tab <- suppressMessages(table1(epitabl, renal_impairment, by = mace_event, measure = "RR"))
  expect_warning(sensitivity(tab, adjust = NULL), "left the analysis unchanged")

  # Covariates added with adjust() are dropped, and silently so.
  adjusted <- suppressMessages(adjust(tab, sex))
  expect_no_warning(sens <- sensitivity(adjusted, adjust = NULL))
  rows <- attr(sens, "sensitivity_rows")
  expect_false(isTRUE(all.equal(rows$estimate[[1]], rows$estimate[[2]])))
})

test_that("regtab and survtab reviewer tools use the recorded exposure", {
  fit <- suppressMessages(regtab(
    epitabl, "mace_event", ~ age + sex + renal_impairment,
    family = binomial("log"), design = "cohort", exposure = renal_impairment
  ))
  expect_identical(SimtablR:::.model_exposure_coefficients(fit), "renal_impairmentYes")

  ev <- e_value(fit)
  expect_identical(ev$data$term, "renal_impairmentYes")
  expect_identical(ev$data$measure, "RR")

  # adjust = NULL drops the formula covariates and those added with adjust().
  sens <- suppressMessages(sensitivity(adjust(fit, diabetes), adjust = NULL))
  rows <- attr(sens, "sensitivity_rows")
  expect_identical(rows$measure, c("RR", "RR"))
  crude <- suppressMessages(tb(epitabl, renal_impairment, mace_event, rr, ref = "No"))
  expect_equal(rows$estimate[[2]], crude$data$ratios$estimate[[2]], tolerance = 1e-6)

  # The forest plot, why(), as_methods() and the manifest name the exposure.
  expect_identical(nrow(SimtablR:::.effect_forest_frame(fit)), 1L)
  invisible(capture.output(expl <- why(fit)))
  expect_true("Exposure: renal_impairment; adjustment set: age, sex" %in% expl)
  expect_match(as_methods(fit), "adjusted for Age at index presentation (years) and Sex", fixed = TRUE)
  expect_identical(repro_manifest(fit)$engine_decisions$adjustment, c("age", "sex"))
  fallacy <- Filter(function(a) identical(a$id, "table2_fallacy"), fit$advice)
  expect_match(fallacy[[1]]$message, "Rows for age, sex are adjustment covariates", fixed = TRUE)

  # A string works, and the exposure must be a model term.
  by_name <- suppressMessages(regtab(epitabl, "mace_event", ~ age + renal_impairment,
                                     family = binomial("log"), exposure = "renal_impairment"))
  expect_identical(nrow(e_value(by_name)$data), 1L)
  expect_error(
    regtab(epitabl, "mace_event", ~ age, family = binomial("log"), exposure = sex),
    class = "simtab_error"
  )

  skip_if_not_installed("survival")
  cox <- survtab(epitabl, mace_time_days, mace_event, ~ renal_impairment + age, exposure = renal_impairment)
  expect_identical(e_value(cox)$data$term, "renal_impairmentYes")
  cox_rows <- attr(suppressMessages(sensitivity(cox, adjust = NULL)), "sensitivity_rows")
  expect_identical(cox_rows$measure, c("HR", "HR"))
  expect_false(isTRUE(all.equal(cox_rows$estimate[[1]], cox_rows$estimate[[2]])))
})

test_that("model reviewer tools without an exposure warn or stop instead of guessing", {
  fit <- suppressMessages(regtab(epitabl, "mace_event", ~ age + renal_impairment, family = binomial("log")))
  expect_warning(ev <- e_value(fit), "including covariates")
  expect_identical(nrow(ev$data), 2L)
  expect_error(sensitivity(fit, adjust = NULL), class = "simtab_error")
  invisible(capture.output(expl <- why(fit)))
  expect_true(any(grepl("^Exposure: not recorded", expl)))
})

test_that("e_value and sensitivity use the pooled estimate of a stratified tb result", {
  strat <- suppressMessages(tb(epitabl, renal_impairment, mace_event, rr, strat = sex))
  pooled <- strat$data$mh[strat$data$mh$row_type == "pooled", ]

  ev <- e_value(strat)
  expect_identical(nrow(ev$data), 1L)
  expect_equal(ev$data$estimate, pooled$estimate)
  expect_match(ev$data$term, "Mantel-Haenszel")

  sens <- suppressMessages(sensitivity(strat, measure = "OR"))
  expect_equal(attr(sens, "sensitivity_rows")$estimate[[1]], pooled$estimate)
})

test_that("tb forest plots draw each ratio once, not once per outcome level", {
  skip_if_not_installed("ggplot2")
  res <- suppressMessages(tb(epitabl, smoking, mace_event, rr, ref = "Never"))
  frame <- SimtablR:::.effect_forest_frame(res)
  expect_identical(nrow(frame), nrow(res$data$ratios))
  expect_length(unique(frame$panel), 1L)
  expect_equal(frame$estimate, res$data$ratios$estimate)

  strat <- suppressMessages(tb(epitabl, renal_impairment, mace_event, rr, strat = sex))
  p <- ggplot2::autoplot(strat)
  expect_s3_class(p, "ggplot")
  expect_identical(nrow(p$data), nrow(strat$data$mh))
})

test_that("as_methods names the adjusted model and its covariates", {
  or_adj <- suppressMessages(table1(epitabl, sex, by = mace_event, measure = "OR", adjust = c(age, diabetes)))
  meth <- as_methods(or_adj)
  expect_match(meth, "Crude OR was estimated using the Woolf/logit", fixed = TRUE)
  expect_match(meth, "Adjusted OR was estimated by logistic regression", fixed = TRUE)
  expect_match(meth, "Age at index presentation (years) and History of diabetes", fixed = TRUE)

  rr_adj <- suppressMessages(table1(epitabl, sex, by = mace_event, measure = "RR", adjust = age))
  expect_match(as_methods(rr_adj), "Adjusted RR was estimated by log-binomial regression adjusting for Age", fixed = TRUE)

  # Adding covariates through the verb updates the prose too.
  crude <- suppressMessages(table1(epitabl, sex, by = mace_event, measure = "OR"))
  expect_false(grepl("Adjusted", as_methods(crude)))
  expect_match(as_methods(suppressMessages(adjust(crude, age))), "adjusting for Age", fixed = TRUE)
})

expect_manifest_matches_prose <- function(result) {
  prose <- as_methods(result)
  decisions <- SimtablR:::.result_decisions(result)
  manifest <- repro_manifest(result)
  named <- c(decisions$estimator, decisions$interval_method, decisions$tests, decisions$corrections)
  expect_gt(length(named), 0)
  for (decision in named) {
    expect_true(grepl(decision, prose, fixed = TRUE), info = decision)
  }
  expect_identical(manifest$estimator, if (length(decisions$estimator)) decisions$estimator)
  expect_identical(manifest$interval_method, if (length(decisions$interval_method)) decisions$interval_method)
  expect_identical(manifest$tests, decisions$tests)
  if (length(decisions$corrections)) {
    expect_identical(manifest$engine_decisions$corrections, decisions$corrections)
  }
  expect_true(all(c("SimtablR", "cli", "stats") %in% names(manifest$packages)))
  expect_false("testthat" %in% names(manifest$packages))
  manifest
}

test_that("manifest records every decision the methods prose names, for each engine", {
  zero <- data.frame(
    e = factor(c(rep("No", 20), rep("Yes", 10))),
    o = factor(c(rep("No", 15), rep("Yes", 5), rep("No", 10)))
  )
  suppressMessages({
    pr <- expect_manifest_matches_prose(tb(epitabl, hypertension, adjudicated_acs, design = "cross_sectional"))
    expect_identical(unname(pr$estimator), "Katz log prevalence-ratio estimator")
    strat <- expect_manifest_matches_prose(
      tb(epitabl, renal_impairment, mace_event, strat = sex, flags = c("rr", "p"), design = "cohort")
    )
    expect_identical(unname(strat$interval_method[["pooled"]]), "Greenland-Robins confidence intervals")
    zero_m <- expect_manifest_matches_prose(tb(zero, e, o, flags = c("or", "p")))
    expect_match(zero_m$engine_decisions$corrections, "Haldane-Anscombe", fixed = TRUE)
    expect_manifest_matches_prose(table1(epitabl, c(age, sex), by = adjudicated_acs, test = TRUE, measure = "PR"))
    adjusted <- expect_manifest_matches_prose(
      adjust(table1(epitabl, renal_impairment, by = mace_event, design = "cohort"), age, sex)
    )
    expect_true("adjusted_log_binomial" %in% names(adjusted$estimator))
    expect_manifest_matches_prose(adjust(table1(epitabl, renal_impairment, by = mace_event, measure = "OR"), age, sex))
    expect_manifest_matches_prose(diag_test(epitabl, poc_hstn_positive, adjudicated_acs, positive = "Yes"))
    glm <- expect_manifest_matches_prose(
      regtab(epitabl, "adjudicated_acs", ~ age + sex, family = binomial("log"), design = "cross_sectional")
    )
    expect_identical(glm$engine_decisions$convergence, "all models converged")
  })
})

test_that("manifest packages list the Suggests each engine used", {
  skip_if_not_installed("pROC")
  skip_if_not_installed("survival")
  suppressMessages({
    roc_m <- expect_manifest_matches_prose(roc(epitabl, poc_hstn_value, adjudicated_acs))
    cox_m <- expect_manifest_matches_prose(survtab(epitabl, mace_time_days, mace_event, ~ renal_impairment + age))
  })
  expect_true("pROC" %in% names(roc_m$packages))
  expect_false("survival" %in% names(roc_m$packages))
  expect_true("survival" %in% names(cox_m$packages))
  expect_identical(cox_m$engine_decisions$ties, "Efron")

  skip_if_not_installed("logistf")
  firth <- suppressMessages(expect_manifest_matches_prose(
    regtab(epitabl, "adjudicated_acs", ~ age + sex, family = binomial("logit"), method = "firth")
  ))
  expect_true("logistf" %in% names(firth$packages))
})

test_that("e_value accepts prevalence ratios with the risk-ratio formula", {
  pr <- suppressMessages(tb(epitabl, hypertension, adjudicated_acs, design = "cross_sectional"))
  ev <- e_value(pr)
  expect_identical(ev$data$measure, "PR")
  expect_false(ev$data$approximation)
  est <- pr$data$ratios[!pr$data$ratios$ref, ]
  expect_equal(ev$data$e_value, est$estimate + sqrt(est$estimate * (est$estimate - 1)))
  expect_match(as_methods(ev), "prevalence ratios used the risk-ratio formula", fixed = TRUE)
  expect_identical(names(repro_manifest(ev)$estimator), c("e_value", "PR"))

  # The cross-sectional simtablr() report works end to end.
  rep <- suppressMessages(simtablr(
    epitabl, outcome = adjudicated_acs, exposure = hypertension,
    vars = c(age, sex), adjust = c(age, sex), design = "cross_sectional"
  ))
  expect_true(all(e_value(rep$table2)$data$measure == "PR"))

  # regtab reads the table's measure: modified Poisson and log-binomial by design.
  suppressMessages({
    poisson_pr <- regtab(epitabl, "adjudicated_acs", ~ hypertension + age,
                         design = "cross_sectional", exposure = hypertension)
    poisson_rr <- regtab(epitabl, "mace_event", ~ renal_impairment + age,
                         design = "cohort", exposure = renal_impairment)
    logbin_pr <- regtab(epitabl, "adjudicated_acs", ~ hypertension + age, family = binomial("log"),
                        design = "cross_sectional", exposure = hypertension)
    counts <- regtab(epitabl, "ed_visits", ~ hypertension + age, exposure = hypertension)
  })
  expect_identical(e_value(poisson_pr)$data$measure, "PR")
  expect_identical(e_value(poisson_rr)$data$measure, "RR")
  expect_identical(e_value(logbin_pr)$data$measure, "PR")
  expect_error(e_value(counts), "incidence-rate ratios", class = "simtab_error")

  skip_if_not_installed("EValue")
  oracle <- EValue::evalues.RR(est$estimate, est$lower_ci, est$upper_ci)
  expect_equal(ev$data$e_value, unname(oracle["E-values", "point"]))
})


test_that("methods prose names variables, writes test sentences and reports missing data", {
  t1 <- suppressMessages(table1(epitabl, c(age, bmi, presentation_hours, sex), by = adjudicated_acs, test = TRUE))
  prose <- as_methods(t1)
  expect_match(prose, "Hours from symptom onset to presentation as median [IQR] owing to skewness", fixed = TRUE)
  expect_match(prose, "Sex recorded for clinical assessment as counts and percentages", fixed = TRUE)
  expect_match(prose, "using the Welch t-test for Age at index presentation (years) and Body mass index (kg/m2)", fixed = TRUE)
  expect_match(prose, "Body mass index (kg/m2) (n = 63)", fixed = TRUE)
  expect_no_match(prose, "Group comparisons used", fixed = TRUE)
  manifest <- repro_manifest(t1)
  expect_true(grepl(manifest$engine_decisions$missing_data, prose, fixed = TRUE))

  dat <- epitabl
  dat$mace_event[1:30] <- NA
  fit <- suppressMessages(regtab(dat, "mace_event", ~ renal_impairment + age, family = binomial("log")))
  expect_match(as_methods(fit), "n = 1470 of 1500", fixed = TRUE)

  acc <- suppressMessages(diag_test(epitabl, poc_hstn_positive, adjudicated_acs, positive = "Yes"))
  expect_match(as_methods(acc), "against Adjudicated acute coronary syndrome as the reference standard", fixed = TRUE)
  expect_match(as_methods(acc), "899 of 1500 participants", fixed = TRUE)

  tab <- suppressMessages(tb(epitabl, hypertension, adjudicated_acs, p, ref = "No"))
  expect_match(as_methods(tab), "compared across Adjudicated acute coronary syndrome using the N-1 chi-squared test", fixed = TRUE)
})

test_that("as_methods(software = TRUE) names the R and SimtablR versions", {
  tab <- suppressMessages(tb(epitabl, hypertension, adjudicated_acs, pr, ref = "No"))
  expect_no_match(as_methods(tab), "Analyses used R", fixed = TRUE)
  versioned <- as_methods(tab, software = TRUE)
  expect_match(versioned, paste0("Analyses used R ", R.version$major, ".", R.version$minor), fixed = TRUE)
  expect_match(versioned, paste("SimtablR", as.character(utils::packageVersion("SimtablR"))), fixed = TRUE)

  rep <- suppressMessages(simtablr(epitabl, outcome = adjudicated_acs, exposure = hypertension,
                                   vars = c(age, sex), design = "cross_sectional"))
  expect_match(as_methods(rep, software = TRUE), "Analyses used R", fixed = TRUE)

  skip_if_not_installed("pROC")
  roc_res <- suppressMessages(roc(epitabl, poc_hstn_value, adjudicated_acs))
  expect_match(as_methods(roc_res, software = TRUE), "with pROC", fixed = TRUE)
})


test_that("small UX fixes: diag print, NA rows, reference note, checklists, report advice", {
  acc <- suppressMessages(diag_test(epitabl, poc_hstn_positive, adjudicated_acs, positive = "Yes"))
  printed <- capture.output(print(acc))
  expect_false(any(grepl("gold standard", printed, fixed = TRUE)))
  expect_true(any(grepl("Reference standard: Adjudicated acute coronary syndrome", printed, fixed = TRUE)))

  with_na <- epitabl
  with_na$mace_event[1:20] <- NA
  freq <- suppressMessages(tb(with_na, renal_impairment, mace_event, flags = c("row", "miss")))$data$frequencies
  expect_false(anyNA(rownames(freq)))
  expect_true(anyNA(colnames(freq)))

  expect_silent(tb(epitabl, hypertension, adjudicated_acs, rr))
  expect_message(tb(epitabl, sex, adjudicated_acs, rr), "Defaulting to the first level: 'Female'", fixed = TRUE)

  rep <- suppressMessages(simtablr(epitabl, outcome = adjudicated_acs, exposure = hypertension,
                                   vars = c(age, sex, diabetes), design = "cross_sectional"))
  checklist <- capture.output(print(strobe(rep)))
  expect_identical(length(checklist), 2L + 1L + 22L)

  withr::local_options(simtablr.guidance = "teaching")
  # Advice is emitted as messages; route them to stdout to keep their order.
  out <- capture.output(withCallingHandlers(print(rep), message = function(m) {
    cat(conditionMessage(m))
    invokeRestart("muffleMessage")
  }))
  advice_line <- grep("compare groups in a descriptive table", out)
  expect_length(advice_line, 1L)
  expect_lt(advice_line, grep("^table2$", out))
})
