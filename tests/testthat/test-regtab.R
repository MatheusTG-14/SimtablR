library(testthat)
# library(SimtablR)

test_that("regtab() ajusta múltiplos desfechos e retorna um data.frame 'wide'", {
  res <- regtab(epitabl,
                outcomes = c("outcome1", "outcome2"),
                predictors = ~ age + sex + smoking,
                family = poisson(),
                robust = FALSE)

  expect_s3_class(res, "data.frame")

  expect_true(all(c("Variable", "outcome1", "outcome2") %in% names(res)))

  expect_false("(Intercept)" %in% res$Variable)
})

test_that("regtab() aplica erros-padrão robustos (HC0) corretamente", {
  r_rob <- regtab(epitabl, outcomes = "outcome1",
                  predictors = ~ age + sex,
                  family = poisson(), robust = TRUE)
  r_mod <- regtab(epitabl, outcomes = "outcome1",
                  predictors = ~ age + sex,
                  family = poisson(), robust = FALSE)

  expect_false(identical(r_rob, r_mod))
})


test_that("regtab() extrai e formata P-valores corretamente", {
  res_p <- regtab(epitabl, c("outcome1", "outcome2"), ~ age,
                  family = poisson(), p_values = TRUE, robust = FALSE)

  expect_true("P_outcome1" %in% names(res_p))
  expect_true("P_outcome2" %in% names(res_p))

  expect_type(res_p$P_outcome1, "character")
})

test_that("regtab() renomeia desfechos com 'labels' e aceita intercepto", {
  res_lab <- regtab(epitabl, c("outcome1", "outcome2"), ~ age, family = poisson(),
                    robust = FALSE,
                    labels = c(outcome1 = "Primary_Visits"))

  #nome da coluna outcome1 deve ter mudado, mas a outcome2 não
  expect_true("Primary_Visits" %in% names(res_lab))
  expect_true("outcome2" %in% names(res_lab))
})

test_that("regtab() intercept removido", {
  result <- regtab(
    epitabl,
    outcomes          = "outcome1",
    predictors        = ~ age,
    family            = poisson(),
    include_intercept = FALSE
  )
  expect_false("(Intercept)" %in% result$Variable)
})

test_that("regtab() intercept incluído se include_intercept = TRUE", {
  result <- regtab(
    epitabl,
    outcomes          = "outcome1",
    predictors        = ~ age,
    family            = poisson(),
    include_intercept = TRUE
  )
  expect_true("(Intercept)" %in% result$Variable)
})

# --- Error handling ---

test_that("regtab() lida com falha de convergência/erro no modelo", {
  #  desfecho falso ( string pura)
  df_test <- epitabl
  df_test$bogus_var <- "error"

  expect_warning(
    res_fail <- regtab(df_test, c("outcome1", "bogus_var"), ~ age, family = poisson(), robust = FALSE),
    "Model fitting failed for outcome 'bogus_var'"
  )
  expect_true("outcome1" %in% names(res_fail))
  expect_false("bogus_var" %in% names(res_fail))
})

# --- S3 methods ---

test_that("print.regtab() produces output without error", {
  result <- regtab(epitabl, outcomes = "outcome1",
                   predictors = ~ age, family = poisson())
  expect_output(print(result))
})
