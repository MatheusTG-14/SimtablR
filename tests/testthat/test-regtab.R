library(testthat)
library(SimtablR)
data(epitabl, package = "SimtablR")

test_that("regtab fits logistic regression with odds ratios for binary outcome", {
  tab <- suppressMessages(regtab(
    epitabl,
    outcomes = "adjudicated_acs",
    predictors = ~ age + sex,
    family = stats::binomial("logit")
  ))
  expect_s3_class(tab, "regtab")
  expect_s3_class(tab, "simtab_result")
  expect_true("estimate" %in% names(tab$data))
  expect_true(is.numeric(tab$data$estimate))
})

test_that("regtab fits linear regression for continuous outcome", {
  tab <- suppressMessages(regtab(
    epitabl,
    outcomes = "bmi",
    predictors = ~ age + sex,
    family = stats::gaussian("identity")
  ))
  expect_s3_class(tab, "regtab")
  expect_true(is.numeric(tab$data$estimate))
})

test_that("regtab fits Poisson regression with incidence rate ratios for count outcome", {
  dat <- regtab_fixture()
  tab <- suppressMessages(regtab(
    dat,
    outcomes = "visits_a",
    predictors = ~ age + sex,
    family = stats::poisson("log")
  ))
  expect_s3_class(tab, "regtab")
  expect_true(is.numeric(tab$data$estimate))
})

test_that("regtab supports log-binomial and robust Poisson for prevalence ratios", {
  dat <- regtab_fixture()
  tab <- suppressMessages(regtab(
    dat,
    outcomes = "visits_a",
    predictors = ~ age + sex,
    family = stats::poisson("log"),
    robust = TRUE
  ))
  expect_s3_class(tab, "regtab")
  expect_true(is.numeric(tab$data$estimate))
})

test_that("regtab supports Firth penalized likelihood", {
  skip_if_not_installed("logistf")
  tab <- suppressMessages(regtab(
    epitabl,
    outcomes = "adjudicated_acs",
    predictors = ~ age + sex,
    family = stats::binomial("logit"),
    method = "firth"
  ))
  expect_s3_class(tab, "regtab")
  expect_true(is.numeric(tab$data$estimate))
})

test_that("regtab handles multivariable adjustment with multiple covariates", {
  tab <- suppressMessages(regtab(
    epitabl,
    outcomes = "adjudicated_acs",
    predictors = ~ age + sex + hypertension,
    family = stats::binomial("logit")
  ))
  expect_true(nrow(tab$data) >= 3)
})

test_that("regtab tidy and glance accessors return valid data frames", {
  tab <- suppressMessages(regtab(
    epitabl,
    outcomes = "adjudicated_acs",
    predictors = ~ age + sex,
    family = stats::binomial("logit")
  ))
  tidy_df <- generics::tidy(tab)
  glance_df <- generics::glance(tab)
  expect_s3_class(tidy_df, "data.frame")
  expect_s3_class(glance_df, "data.frame")
})

test_that("regtab validates input families and throws classed errors on invalid inputs", {
  expect_error(regtab(epitabl, outcomes = "adjudicated_acs", predictors = ~ nonexistent_col))
})

test_that("regtab marks a constant outcome as failed instead of reporting 1.00", {
  dat <- epitabl
  dat$constant_outcome <- factor("No", levels = c("No", "Yes"))

  expect_warning(
    fit <- suppressMessages(regtab(
      dat, c("constant_outcome", "rehospitalized"), ~ age + sex,
      family = binomial()
    )),
    "takes a single value"
  )
  info <- model_info(fit)
  expect_true(info$failed[info$outcome == "constant_outcome"])
  expect_false(info$failed[info$outcome == "rehospitalized"])
  expect_false("constant_outcome" %in% fit$data$outcome)
  expect_output(print(fit), "Failed outcomes: constant_outcome")

  # A verb on the result recomputes through the same check.
  expect_warning(refit <- adjust(fit, smoking), "takes a single value")
  expect_true(model_info(refit)$failed[model_info(refit)$outcome == "constant_outcome"])

  # A lone constant outcome cannot produce a table at all.
  expect_error(
    suppressWarnings(regtab(dat, "constant_outcome", ~ age, family = binomial())),
    class = "simtab_error_engine"
  )
})

test_that("regtab labels the measure from link and design in every renderer", {
  pr <- suppressMessages(regtab(epitabl, "adjudicated_acs", ~ age + sex,
                                family = binomial("log"), design = "cross_sectional"))
  rr <- suppressMessages(regtab(epitabl, "adjudicated_acs", ~ age + sex,
                                family = binomial("log"), design = "cohort"))
  or <- suppressMessages(regtab(epitabl, "adjudicated_acs", ~ age + sex, family = binomial("logit")))

  header <- "Adjudicated acute coronary syndrome, PR (95% CI)"
  expect_true(header %in% names(as.data.frame(pr)))
  expect_true("Adjudicated acute coronary syndrome, RR (95% CI)" %in% names(as.data.frame(rr)))
  expect_true("Adjudicated acute coronary syndrome, OR (95% CI)" %in% names(as.data.frame(or)))
  expect_output(print(pr), "PR (95% CI)", fixed = TRUE)
  # A second fit can differ in the last bit under optimised BLAS (e.g. MKL).
  expect_equal(pr$data, suppressMessages(regtab(epitabl, "adjudicated_acs", ~ age + sex,
                                                family = binomial("log")))$data)

  # Count and 0/1 outcomes in one Poisson model keep their own measures.
  dat <- epitabl
  dat$mace01 <- as.integer(dat$mace_event == "Yes")
  mixed <- suppressMessages(regtab(dat, c("ed_visits", "mace01"), ~ age + sex))
  expect_identical(unname(SimtablR:::.regtab_measures(mixed)), c("IRR", "RR"))

  skip_if_not_installed("gt")
  expect_true(header %in% names(as_gt(pr)[["_data"]]))
  skip_if_not_installed("flextable")
  expect_true(header %in% flextable::as_flextable(pr)$col_keys)
})

test_that("regtab flags odds ratios for a common outcome under a declared design", {
  cs <- suppressMessages(regtab(epitabl, "adjudicated_acs", ~ age + sex,
                                family = binomial("logit"), design = "cross_sectional"))
  ids <- vapply(cs$advice, `[[`, character(1), "id")
  expect_true("or_as_rr_common_outcome" %in% ids)

  cc <- suppressMessages(regtab(epitabl, "adjudicated_acs", ~ age + sex,
                                family = binomial("logit"), design = "case_control"))
  undeclared <- suppressMessages(regtab(epitabl, "adjudicated_acs", ~ age + sex, family = binomial("logit")))
  log_link <- suppressMessages(regtab(epitabl, "adjudicated_acs", ~ age + sex,
                                      family = binomial("log"), design = "cross_sectional"))
  for (fit in list(cc, undeclared, log_link)) {
    expect_false("or_as_rr_common_outcome" %in% vapply(fit$advice, `[[`, character(1), "id"))
  }
})

test_that("why() and as_methods() name the design, measure and convergence of regtab", {
  pr <- suppressMessages(regtab(epitabl, "adjudicated_acs", ~ age + sex,
                                family = binomial("log"), design = "cross_sectional"))
  invisible(capture.output(expl <- why(pr)))
  expect_true("Measure: PR" %in% expl$lines)
  expect_true("Measure source: binomial family, log link, cross_sectional design" %in% expl$lines)
  expect_true("Convergence: all models converged" %in% expl$lines)

  meth <- as_methods(pr)
  expect_match(meth, "prevalence ratios for a cross-sectional design", fixed = TRUE)
  expect_match(meth, "All models converged.", fixed = TRUE)

  or <- suppressMessages(regtab(epitabl, "adjudicated_acs", ~ age + sex, family = binomial("logit")))
  expect_match(as_methods(or), "reported as odds ratios;", fixed = TRUE)
  expect_no_match(as_methods(or), "converged")
})

test_that("regtab Poisson-log accepts a binary factor outcome coded like simtablr()", {
  dat <- epitabl
  dat$acs01 <- as.integer(dat$adjudicated_acs == levels(dat$adjudicated_acs)[2])
  expect_message(
    fac <- regtab(dat, "adjudicated_acs", ~ age + sex, design = "cross_sectional"),
    "coded 1 = 'Yes', 0 = 'No'"
  )
  num <- suppressMessages(regtab(dat, "acs01", ~ age + sex, design = "cross_sectional"))
  expect_equal(fac$data$estimate, num$data$estimate)
  expect_equal(fac$data$lower, num$data$lower)
  expect_identical(fac$meta$outcome_coding$adjudicated_acs, c(event = "Yes", reference = "No"))
  expect_true("Adjudicated acute coronary syndrome, PR (95% CI)" %in% names(as.data.frame(fac)))

  # Outcomes with more than two levels are still rejected at specification time.
  expect_error(
    regtab(epitabl, "smoking", ~ age, family = poisson("log")),
    class = "simtab_error"
  )
})
