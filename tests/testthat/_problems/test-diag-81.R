# Extracted from test-diag.R:81

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "SimtablR", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
library(testthat)
library(SimtablR)
data(epitabl, package = "SimtablR")

# test -------------------------------------------------------------------------
dat <- data.frame(
    rapid = factor(rep("Positive", 50), levels = c("Negative", "Positive")),
    gold = factor(c(rep("Yes", 30), rep("No", 20)), levels = c("No", "Yes"))
  )
res <- suppressWarnings(suppressMessages(diag_test(dat, test = rapid, ref = gold)))
expect_lt(res$data$metrics["kappa", "conf.low"], 0)
display <- as.data.frame(res)
kappa_ci <- display$CI[display$Metric == "Cohen's Kappa"]
expect_false(grepl("-0.00", kappa_ci, fixed = TRUE))
expect_output(print(res), "Cohen's Kappa\\s+:\\s+0\\.00\\s+\\(0\\.00 - 0\\.00\\)")
