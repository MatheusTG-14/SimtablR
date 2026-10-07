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

