library(testthat)
library(SimtablR)
data(epitabl, package = "SimtablR")

test_that("table1 produces basic descriptive table across variable types", {
  tab <- suppressMessages(table1(epitabl, vars = c("age", "sex", "hypertension")))
  expect_s3_class(tab, "simtab_table1")
  expect_s3_class(tab, "simtab_result")
  expect_true(all(c("age", "sex", "hypertension") %in% names(tab$data)))
  expect_type(tab$data$age$summary$Overall, "double")
})

test_that("table1 handles stratification by group", {
  tab <- suppressMessages(table1(epitabl, vars = c("age", "sex"), by = "adjudicated_acs"))
  expect_equal(rlang::eval_tidy(tab$spec$roles$by), "adjudicated_acs")
  expect_true(all(c("Overall", "No", "Yes") %in% colnames(tab$data$sex$freq)))
})

test_that("table1 supports continuous summary presets (mean/sd vs median/iqr)", {
  t_med <- suppressMessages(table1(epitabl, "age", summary = "median"))
  expect_length(t_med$data$age$summary$Overall, 3) # median, Q1, Q3
  t_mean <- suppressMessages(table1(epitabl, "age", summary = "mean"))
  expect_length(t_mean$data$age$summary$Overall, 2) # mean, SD
})

test_that("table1 handles missingness options", {
  t_no <- suppressMessages(table1(epitabl, "age", missing = FALSE))
  t_yes <- suppressMessages(table1(epitabl, "age", missing = TRUE))
  expect_s3_class(t_no, "simtab_table1")
  expect_s3_class(t_yes, "simtab_table1")
})

test_that("table1 reports p-values and test methods", {
  tab <- suppressMessages(table1(epitabl, vars = "sex", by = "adjudicated_acs", test = TRUE))
  expect_true(!is.null(tab$data$sex$test$p.value))
  expect_true(is.numeric(tab$data$sex$test$p.value))
})

test_that("tb computes unadjusted 2x2 contingency table and odds ratio", {
  tab <- suppressMessages(tb(epitabl, hypertension, adjudicated_acs, flags = c("row", "or")))
  expect_s3_class(tab, "tb")
  expect_s3_class(tab, "simtab_result")
  expect_true(!is.null(tab$data$ratios$estimate))
  expect_true(is.numeric(tab$data$ratios$estimate))
})

test_that("tb computes relative risk and prevalence ratio", {
  tab_rr <- suppressMessages(tb(epitabl, hypertension, adjudicated_acs, flags = "rr"))
  tab_pr <- suppressMessages(tb(epitabl, hypertension, adjudicated_acs, flags = "pr"))
  expect_identical(tab_rr$spec$effect$measure, "RR")
  expect_identical(tab_pr$spec$effect$measure, "PR")
})

test_that("tb supports Mantel-Haenszel stratification by a third variable", {
  tab <- suppressMessages(tb(epitabl, hypertension, adjudicated_acs, strat = sex, flags = "or"))
  expect_s3_class(tab, "tb")
  expect_true(!is.null(tab$data$mh))
})

test_that("tb supports hypothesis testing options (chi-square and Fisher)", {
  tab_chi <- suppressMessages(tb(epitabl, hypertension, adjudicated_acs, test = "chisq"))
  tab_fish <- suppressMessages(tb(epitabl, hypertension, adjudicated_acs, test = "fisher"))
  expect_true(is.numeric(tab_chi$meta$stats$p.value))
  expect_true(is.numeric(tab_fish$meta$stats$p.value))
})

test_that("tb calculates row, col, and cell percentages", {
  tab_row <- suppressMessages(tb(epitabl, hypertension, adjudicated_acs, flags = "row"))
  tab_col <- suppressMessages(tb(epitabl, hypertension, adjudicated_acs, flags = "col"))
  tab_cell <- suppressMessages(tb(epitabl, hypertension, adjudicated_acs, flags = "cell"))
  expect_s3_class(tab_row, "tb")
  expect_s3_class(tab_col, "tb")
  expect_s3_class(tab_cell, "tb")
})

test_that("rbind stacks multiple tb objects into simtab_rbind_tb", {
  t1 <- suppressMessages(tb(epitabl, hypertension, adjudicated_acs))
  t2 <- suppressMessages(tb(epitabl, sex, adjudicated_acs))
  stacked <- rbind(t1, t2)
  expect_s3_class(stacked, "simtab_rbind_tb")
  expect_length(stacked$data, 2)
})

test_that("table1 and tb raise simtab errors on invalid variables or empty input", {
  expect_error(table1(epitabl, vars = "nonexistent_variable"))
  expect_error(tb(epitabl, nonexistent, adjudicated_acs))
})
