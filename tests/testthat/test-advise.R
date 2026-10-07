library(testthat)
library(SimtablR)
data(epitabl, package = "SimtablR")

test_that("advise returns methodological guidance on simtab_result", {
  tab <- suppressMessages(table1(epitabl, vars = "age", by = "adjudicated_acs"))
  adv <- advise(tab)
  expect_type(adv, "list")
})

test_that("advise rules list registered epidemiological guidance rules", {
  rules <- list_rules()
  expect_s3_class(rules, "data.frame")
  expect_true(nrow(rules) >= 20)
  expect_true(all(c("id", "severity", "citation") %in% names(rules)))
})

test_that("advise triggers model complexity or small sample warnings on regressions", {
  small_df <- data.frame(
    y = factor(c(rep("No", 5), rep("Yes", 5))),
    x1 = rnorm(10),
    x2 = rnorm(10),
    x3 = rnorm(10)
  )
  fit <- suppressMessages(suppressWarnings(regtab(small_df, outcomes = "y", predictors = ~ x1 + x2 + x3, family = stats::binomial("logit"))))
  aud <- advise(fit, audit = TRUE)
  df <- as.data.frame(aud)
  expect_true(any(df$fired))
})

test_that("advise identifies multiple unadjusted comparisons", {
  tab <- suppressMessages(table1(epitabl, vars = c("age", "bmi", "cholesterol"), by = "adjudicated_acs", test = TRUE))
  aud <- advise(tab, audit = TRUE)
  df <- as.data.frame(aud)
  expect_true("multiplicity_unadjusted" %in% df$id)
})

test_that("advise with audit = TRUE returns complete rule audit data frame", {
  tab <- suppressMessages(table1(epitabl, "age", by = "adjudicated_acs"))
  aud <- advise(tab, audit = TRUE)
  expect_s3_class(aud, "simtab_audit")
  expect_s3_class(as.data.frame(aud), "data.frame")
  expect_true(all(c("id", "severity", "fired") %in% names(as.data.frame(aud))))
})

test_that("advise rules remain non-blocking and do not mutate result evidence", {
  tab <- suppressMessages(table1(epitabl, "age", by = "adjudicated_acs"))
  raw_before <- tab$data
  adv <- advise(tab)
  expect_identical(tab$data, raw_before)
})
