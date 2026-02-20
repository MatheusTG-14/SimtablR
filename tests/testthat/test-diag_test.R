library(testthat)
library(SimtablR)

epitabl <- epitabl


test_that("diag_test() retorna classe 'diag_test'para S3", {
  result <- diag_test(epitabl, test = rapid_test, ref = lab_confirmed)
  expect_s3_class(result, "diag_test")
})

test_that("diag_test() tem estrutura correta", {
  result <- diag_test(epitabl, test = rapid_test, ref = lab_confirmed)
  expect_named(result, c("table", "stats", "labels", "sample_size", "conf.level"),
               ignore.order = TRUE)
})

test_that("diag_test() matriz 2x2 é correta", {
  result <- diag_test(epitabl, test = rapid_test, ref = lab_confirmed)
  expect_equal(dim(result$table), c(2L, 2L))
})

test_that("diag_test() tem métricas adequadas", {
  result <- diag_test(epitabl, test = rapid_test, ref = lab_confirmed)
  required_metrics <- c("Sensitivity", "Specificity", "Pos Pred Value (PPV)", "Neg Pred Value (NPV)",
                        "Accuracy", "Prevalence", "Likelihood Ratio +", "Likelihood Ratio -",
                        "Youden Index", "F1 Score")
  expect_true(all(required_metrics %in% result$stats$Metric))
})

test_that("diag_test() sensibilidade e especificidade estão em [0, 1]", {
  result <- diag_test(epitabl, test = rapid_test, ref = lab_confirmed)
  sens <- result$stats$Estimate[result$stats$Metric == "Sensitivity"]
  spec <- result$stats$Estimate[result$stats$Metric == "Specificity"]
  expect_true(sens >= 0 && sens <= 1)
  expect_true(spec >= 0 && spec <= 1)
})

test_that("diag_test() CI lower < Estimate < CI upper p/ sensibilidade", {
  result <- diag_test(epitabl, test = rapid_test, ref = lab_confirmed)
  row <- result$stats[result$stats$Metric == "Sensitivity", ]
  expect_true(row$LowerCI <= row$Estimate)
  expect_true(row$Estimate <= row$UpperCI)
})

test_that("diag_test() Youden Index = Sens + Spec - 1", {
  result <- diag_test(epitabl, test = rapid_test, ref = lab_confirmed)
  sens <- result$stats$Estimate[result$stats$Metric == "Sensitivity"]
  spec <- result$stats$Estimate[result$stats$Metric == "Specificity"]
  youden_expected <- sens + spec - 1
  youden_actual   <- result$stats$Estimate[result$stats$Metric == "Youden Index"]
  expect_equal(youden_actual, youden_expected, tolerance = 1e-8)
})

test_that("diag_test() F1 = 2*PPV*Sens / (PPV + Sens)", {
  result <- diag_test(epitabl, test = rapid_test, ref = lab_confirmed)
  ppv  <- result$stats$Estimate[result$stats$Metric == "Pos Pred Value (PPV)"]
  sens <- result$stats$Estimate[result$stats$Metric == "Sensitivity"]
  f1_expected <- 2 * ppv * sens / (ppv + sens)
  f1_actual   <- result$stats$Estimate[result$stats$Metric == "F1 Score"]
  expect_equal(f1_actual, f1_expected, tolerance = 1e-8)
})

test_that("diag_test() retira e notifica valores NA", {
  ep2 <- epitabl
  ep2$rapid_test[1:5] <- NA
  expect_message(
    diag_test(ep2, test = rapid_test, ref = lab_confirmed),
    "Removed"
  )
})

test_that("diag_test() aceita conf.level = 0.99", {
  r95 <- diag_test(epitabl, test = rapid_test, ref = lab_confirmed, conf.level = 0.95)
  r99 <- diag_test(epitabl, test = rapid_test, ref = lab_confirmed, conf.level = 0.99)
  # 99% CI should be wider than 95% CI
  width95 <- r95$stats$UpperCI[1] - r95$stats$LowerCI[1]
  width99 <- r99$stats$UpperCI[1] - r99$stats$LowerCI[1]
  expect_true(width99 > width95)
})

# --- Error handling ---

test_that("diag_test() sem dados", {
  expect_error(diag_test(), "No data provided")
})

test_that("diag_test() Var não encontrada", {
  expect_error(
    diag_test(epitabl, test = nonexistent_var, ref = lab_confirmed),
    "not found"
  )
})

test_that("diag_test() conf.level inválido", {
  expect_error(
    diag_test(epitabl, test = rapid_test, ref = lab_confirmed, conf.level = 2),
    "'conf.level' must be a single numeric value between 0 and 1."
  )
})

# --- S3 methods ---

test_that("print.diag_test() output sem erro", {
  result <- diag_test(epitabl, test = rapid_test, ref = lab_confirmed)
  expect_output(print(result))
})

test_that("plot.diag_test() sem erro", {
  result <- diag_test(epitabl, test = rapid_test, ref = lab_confirmed)
  expect_no_error(plot(result))
})
