library(testthat)
library(SimtablR)
data(epitabl, package = "SimtablR")

# Validation against reference implementations. Fixed values come from
# epiR::epi.2by2() (epiR 2.0.96, 2026-10-31) and were checked against the
# closed-form formulas; epiR is not a dependency. Suggests packages are used
# directly when installed. Refitted models are compared with a tolerance.

ratio_row <- function(x) x$data$ratios[!x$data$ratios$ref, ]

test_that("crude 2x2 PR, RR and OR match epiR with Katz and Woolf intervals", {
  # hypertension x adjudicated ACS: exposed 303/768, unexposed 269/732
  pr <- ratio_row(suppressMessages(tb(epitabl, hypertension, adjudicated_acs, pr, ref = "No")))
  expect_equal(c(pr$estimate, pr$lower_ci, pr$upper_ci), c(1.073594331, 0.9434137513, 1.221738379), tolerance = 1e-8)
  or <- ratio_row(suppressMessages(tb(epitabl, hypertension, adjudicated_acs, or, ref = "No")))
  expect_equal(c(or$estimate, or$lower_ci, or$upper_ci), c(1.121549346, 0.9103779263, 1.381704125), tolerance = 1e-8)

  # renal impairment x MACE: exposed 75/316, unexposed 145/1184
  rr <- ratio_row(suppressMessages(tb(epitabl, renal_impairment, mace_event, rr, ref = "No")))
  expect_equal(c(rr$estimate, rr$lower_ci, rr$upper_ci), c(1.938018333, 1.509900416, 2.487525017), tolerance = 1e-8)
})

test_that("Haldane-Anscombe correction matches the +0.5 closed form", {
  # exposed 0/10 events, unexposed 5/20: every cell gets +0.5
  z <- data.frame(
    e = factor(c(rep("No", 20), rep("Yes", 10))),
    o = factor(c(rep("Yes", 5), rep("No", 15), rep("No", 10)), levels = c("No", "Yes"))
  )
  rr <- ratio_row(suppressMessages(tb(z, e, o, rr, ref = "No")))
  expect_equal(c(rr$estimate, rr$lower_ci, rr$upper_ci), c(0.17355371901, 0.01053646605, 2.85872827277), tolerance = 1e-8)
  or <- ratio_row(suppressMessages(tb(z, e, o, or, ref = "No")))
  expect_equal(c(or$estimate, or$lower_ci, or$upper_ci), c(0.134199134199, 0.006686892212, 2.693240304753), tolerance = 1e-8)
})

test_that("N-1 chi-squared matches (N - 1) / N times Pearson", {
  stats <- suppressMessages(tb(epitabl, hypertension, adjudicated_acs, p, ref = "No"))$meta$stats
  expect_identical(stats$method, "N-1 chi-squared")
  expect_equal(unname(stats$statistic), 1.161184015, tolerance = 1e-8)
  expect_equal(stats$p.value, 0.2812201215, tolerance = 1e-8)
})

test_that("Mantel-Haenszel RR matches epiR with the Greenland-Robins interval", {
  st <- suppressMessages(tb(epitabl, renal_impairment, mace_event, rr, ref = "No", strat = sex))
  mh <- st$data$mh
  pooled <- mh[mh$row_type == "pooled", ]
  expect_equal(c(pooled$estimate, pooled$lower_ci, pooled$upper_ci), c(1.943826007, 1.513439812, 2.496603773), tolerance = 1e-8)
  strata <- mh[mh$row_type == "stratum", ]
  expect_equal(strata$estimate, c(1.741217320, 2.122983257), tolerance = 1e-8)
  expect_equal(strata$lower_ci, c(1.194468163, 1.517964414), tolerance = 1e-8)
})

test_that("adjusted log-binomial RR matches stats::glm with model-based Wald intervals", {
  t1 <- suppressMessages(table1(epitabl, renal_impairment, by = mace_event, measure = "RR", adjust = c(age, sex)))
  adjusted <- t1$data$renal_impairment$adjusted
  adjusted <- adjusted[!adjusted$ref, ]
  dat <- epitabl
  dat$mace01 <- as.integer(dat$mace_event == "Yes")
  fit <- stats::glm(mace01 ~ renal_impairment + age + sex, family = binomial("log"), data = dat)
  ci <- exp(stats::confint.default(fit)["renal_impairmentYes", ])
  expect_equal(adjusted$estimate, unname(exp(stats::coef(fit)["renal_impairmentYes"])), tolerance = 1e-6)
  expect_equal(c(adjusted$lower, adjusted$upper), unname(ci), tolerance = 1e-6)
})

test_that("regtab robust intervals match sandwich::vcovHC", {
  skip_if_not_installed("sandwich")
  dat <- epitabl
  dat$mace01 <- as.integer(dat$mace_event == "Yes")
  fit <- stats::glm(mace01 ~ renal_impairment + age, family = binomial(), data = dat)
  for (type in c("HC0", "HC3")) {
    res <- suppressMessages(regtab(dat, "mace01", ~ renal_impairment + age, family = binomial(), robust = type))
    se <- sqrt(diag(sandwich::vcovHC(fit, type = type)))[res$data$term]
    beta <- stats::coef(fit)[res$data$term]
    z <- stats::qnorm(0.975)
    expect_equal(res$data$estimate, unname(exp(beta)), tolerance = 1e-6, info = type)
    expect_equal(res$data$lower, unname(exp(beta - z * se)), tolerance = 1e-6, info = type)
    expect_equal(res$data$upper, unname(exp(beta + z * se)), tolerance = 1e-6, info = type)
  }
})

test_that("Firth regression matches logistf", {
  skip_if_not_installed("logistf")
  res <- suppressMessages(regtab(epitabl, "adjudicated_acs", ~ age + sex, family = binomial("logit"), method = "firth"))
  ref <- logistf::logistf(adjudicated_acs ~ age + sex, data = epitabl, pl = TRUE)
  expect_equal(res$data$estimate, unname(exp(stats::coef(ref)[res$data$term])), tolerance = 1e-6)
  expect_equal(res$data$lower, unname(exp(ref$ci.lower[res$data$term])), tolerance = 1e-6)
  expect_equal(res$data$upper, unname(exp(ref$ci.upper[res$data$term])), tolerance = 1e-6)
})

test_that("Cox hazard ratios match survival::coxph, including stratify()", {
  skip_if_not_installed("survival")
  res <- suppressMessages(survtab(epitabl, mace_time_days, mace_event, ~ renal_impairment + age))
  dat <- epitabl
  dat$event01 <- as.integer(dat$mace_event == "Yes")
  ref <- survival::coxph(survival::Surv(mace_time_days, event01) ~ renal_impairment + age, data = dat, ties = "efron")
  terms <- res$data$terms
  expect_equal(terms$estimate, unname(exp(stats::coef(ref))[terms$term]), tolerance = 1e-6)
  expect_equal(terms$lower, unname(exp(stats::confint(ref))[terms$term, 1]), tolerance = 1e-6)

  strat <- suppressMessages(stratify(res, sex))
  ref_strat <- survival::coxph(
    survival::Surv(mace_time_days, event01) ~ renal_impairment + age + survival::strata(sex),
    data = dat, ties = "efron"
  )
  expect_equal(strat$data$terms$estimate, unname(exp(stats::coef(ref_strat))[strat$data$terms$term]), tolerance = 1e-6)
})

test_that("ROC AUC and DeLong interval match pROC", {
  skip_if_not_installed("pROC")
  res <- suppressMessages(roc(epitabl, poc_hstn_value, adjudicated_acs))
  auc <- res$data$auc
  cc <- stats::complete.cases(epitabl[c("poc_hstn_value", "adjudicated_acs")])
  ref <- pROC::roc(
    epitabl$adjudicated_acs[cc], epitabl$poc_hstn_value[cc],
    levels = c("No", "Yes"), direction = auc$direction_used, quiet = TRUE
  )
  ci <- pROC::ci.auc(ref, method = "delong")
  expect_equal(auc$auc, as.numeric(pROC::auc(ref)), tolerance = 1e-8)
  expect_equal(c(auc$conf.low, auc$conf.high), as.numeric(ci)[c(1, 3)], tolerance = 1e-8)
  expect_identical(c(auc$n_pos, auc$n_neg), c(length(ref$cases), length(ref$controls)))
})

test_that("E-values match EValue for RR, OR and HR", {
  skip_if_not_installed("EValue")
  ci_evalue <- function(ref) {
    bounds <- ref["E-values", c("lower", "upper")]
    unname(bounds[!is.na(bounds)][1])
  }
  rr_res <- e_value(suppressMessages(tb(epitabl, renal_impairment, mace_event, rr, ref = "No")))$data
  rr_ref <- EValue::evalues.RR(rr_res$estimate, rr_res$conf.low, rr_res$conf.high)
  expect_equal(rr_res$e_value, unname(rr_ref["E-values", "point"]), tolerance = 1e-8)
  expect_equal(rr_res$e_value_ci, ci_evalue(rr_ref), tolerance = 1e-8)

  or_res <- e_value(suppressMessages(tb(epitabl, renal_impairment, mace_event, or, ref = "No")))$data
  or_ref <- EValue::evalues.OR(or_res$estimate, or_res$conf.low, or_res$conf.high, rare = FALSE)
  expect_equal(or_res$e_value, unname(or_ref["E-values", "point"]), tolerance = 1e-8)
  expect_equal(or_res$e_value_ci, ci_evalue(or_ref), tolerance = 1e-8)

  skip_if_not_installed("survival")
  cox <- suppressMessages(survtab(epitabl, mace_time_days, mace_event, ~ renal_impairment + age,
                                  exposure = renal_impairment))
  hr_res <- e_value(cox)$data
  hr_ref <- EValue::evalues.HR(hr_res$estimate, hr_res$conf.low, hr_res$conf.high, rare = FALSE)
  expect_equal(hr_res$e_value, unname(hr_ref["E-values", "point"]), tolerance = 1e-8)
  expect_equal(hr_res$e_value_ci, ci_evalue(hr_ref), tolerance = 1e-8)
})

test_that("evidence verbs on a result equal the direct call, for every engine that uses them", {
  q <- suppressMessages
  # bivariate engine
  expect_equal(q(stratify(tb(epitabl, renal_impairment, mace_event, rr, ref = "No"), sex))$data,
               q(tb(epitabl, renal_impairment, mace_event, rr, ref = "No", strat = sex))$data)
  expect_equal(q(measure(tb(epitabl, renal_impairment, mace_event, ref = "No"), "RR", ref = "No"))$data,
               q(tb(epitabl, renal_impairment, mace_event, rr, ref = "No"))$data)
  expect_equal(q(test(tb(epitabl, renal_impairment, mace_event), "fisher"))$meta$stats$p.value,
               q(tb(epitabl, renal_impairment, mace_event, test = "fisher"))$meta$stats$p.value)
  expect_equal(q(set_summary(tb(epitabl, age, mace_event), "median"))$data,
               q(tb(epitabl, age, mace_event, summary = "median"))$data)
  with_na <- epitabl
  with_na$mace_event[1:20] <- NA
  expect_equal(q(missingness(tb(with_na, renal_impairment, mace_event), display = TRUE))$data,
               q(tb(with_na, renal_impairment, mace_event, miss))$data)

  # descriptive engine
  base <- q(table1(epitabl, c(age, sex, renal_impairment), by = mace_event))
  expect_equal(q(stratify(q(table1(epitabl, c(age, sex, renal_impairment))), mace_event))$data, base$data)
  expect_equal(q(measure(base, "RR"))$data, q(table1(epitabl, c(age, sex, renal_impairment), by = mace_event, measure = "RR"))$data)
  expect_equal(q(adjust(q(measure(base, "RR")), age, sex))$data,
               q(table1(epitabl, c(age, sex, renal_impairment), by = mace_event, measure = "RR", adjust = c(age, sex)))$data)
  expect_equal(q(test(base))$data, q(table1(epitabl, c(age, sex, renal_impairment), by = mace_event, test = TRUE))$data)
  expect_equal(q(set_summary(base, "median"))$data,
               q(table1(epitabl, c(age, sex, renal_impairment), by = mace_event, summary = "median"))$data)
  expect_equal(q(missingness(base, denominator = "complete"))$data,
               q(table1(epitabl, c(age, sex, renal_impairment), by = mace_event, denominator = "complete"))$data)

  # GLM engine
  expect_equal(q(adjust(q(regtab(epitabl, "mace_event", ~ renal_impairment, family = binomial("log"))), age))$data,
               q(regtab(epitabl, "mace_event", ~ renal_impairment + age, family = binomial("log")))$data)

  # Cox engine
  skip_if_not_installed("survival")
  expect_equal(q(adjust(survtab(epitabl, mace_time_days, mace_event, ~ renal_impairment), age))$data,
               q(survtab(epitabl, mace_time_days, mace_event, ~ renal_impairment + age))$data)
})

test_that("style() and fmt() never change evidence, for every engine", {
  q <- suppressMessages
  results <- list(
    table1 = q(table1(epitabl, c(age, sex), by = mace_event, measure = "RR", test = TRUE)),
    tb = q(tb(epitabl, renal_impairment, mace_event, rr, p, ref = "No")),
    tb_strat = q(tb(epitabl, renal_impairment, mace_event, rr, ref = "No", strat = sex)),
    regtab = q(regtab(epitabl, "mace_event", ~ renal_impairment + age, family = binomial("log"))),
    diag = q(diag_test(epitabl, poc_hstn_positive, adjudicated_acs, positive = "Yes"))
  )
  if (requireNamespace("survival", quietly = TRUE)) {
    results$survtab <- q(survtab(epitabl, mace_time_days, mace_event, ~ renal_impairment + age))
  }
  if (requireNamespace("pROC", quietly = TRUE)) {
    results$roc <- q(roc(epitabl, poc_hstn_value, adjudicated_acs))
  }
  for (nm in names(results)) {
    x <- results[[nm]]
    expect_identical(style(x, "lancet")$data, x$data, info = nm)
    expect_identical(fmt(x, d = 3)$data, x$data, info = nm)
  }
})
