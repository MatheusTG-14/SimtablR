# Extracted from test-reporting.R:347

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "SimtablR", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
library(testthat)
library(SimtablR)
data(epitabl, package = "SimtablR")
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

# test -------------------------------------------------------------------------
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
out <- capture.output(print(rep))
advice_line <- grep("unadjusted p-values", out)
expect_length(advice_line, 1L)
expect_lt(advice_line, grep("^table2$", out))
