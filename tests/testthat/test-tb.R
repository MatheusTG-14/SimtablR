library(testthat)
library(SimtablR)

epitabl <- epitabl

test_that("tb() gera tabelas 1D e 2D básicas corretamente", {
  # Tabela 1D (soma deve ser 1000)
  res1 <- tb(epitabl, disease)
  expect_s3_class(res1, "tb")
  expect_equal(sum(unclass(res1)), 1000) # 1000 por conta do total, deve dobrar a contagem

  # Tabela 2D (smoking: 3 níveis, disease: 2 níveis)
  # Com addmargins, a matriz final deve ser 4x3 (linhas: 3+Total, colunas: 2+Total)
  res2 <- tb(epitabl, smoking, disease)
  expect_equal(dim(res2), c(4, 3))
})

test_that("tb() retorna classe tb para S3", {
  result <- tb(epitabl, smoking)
  expect_s3_class(result, "tb")
  expect_true(is.table(result) || is.matrix(result))
})

test_that("tb() contagens estão corretas mesmo para 1 var", {
  result <- tb(epitabl, smoking)
  # Total should equal nrow(df_bin)
  expect_equal(sum(as.integer(result)), nrow(epitabl)*2)
})

test_that("tb() inclui NA quando m é verdadeiro", {
  epitabl_na <- epitabl
  epitabl_na$sex[1:10] <- NA
  result_no_na <- tb(epitabl_na, sex, m = FALSE)
  result_with_na <- tb(epitabl_na, sex, m = TRUE)
  # With m = TRUE the sum of the table should include the NAs
  expect_gt(sum(as.integer(result_with_na)), sum(as.integer(result_no_na)))
})

test_that("tb() usa argumento d corretamente", {
  # No direct value to test, but it should not error
  expect_no_error(tb(epitabl, sex, disease, p, d = 2))
})

test_that("tb() NSE flags de porcentagem funcionam (col, row, p)", {
  res_col <- tb(epitabl, smoking, disease, col)
  pct_col <- attr(res_col, "percent")
  expect_false(is.null(pct_col))

  # A soma das porcentagens da primeira coluna (excluindo a margem 'Total') deve ser 100
  expect_equal(sum(pct_col[1:3, 1]), 100, tolerance = 1e-4)

  res_row <- tb(epitabl, smoking, disease, row)
  pct_row <- attr(res_row, "percent")

  # A soma das porcentagens da primeira linha (excluindo a margem 'Total') deve ser 100
  expect_equal(sum(pct_row[1, 1:2]), 100, tolerance = 1e-4)
})

test_that("tb() calcula Prevalence Ratio (RP) corretamente", {
  # Exposições (linhas): smoking | Desfecho (colunas): disease
  res_rp <- tb(epitabl, smoking, disease, rp = TRUE, ref = "Never")

  rp_col <- attr(res_rp, "rp")
  expect_false(is.null(rp_col))

  # A primeira linha de RP deve ser a referência
  expect_equal(rp_col[1], "1.00 (Ref)")
  # As outras linhas não devem estar vazias/nulas
  expect_true(nchar(rp_col[2]) > 3)
})

test_that("tb() calcula Odds Ratio (OR) corretamente", {
  res_or <- tb(epitabl, smoking, disease, or = TRUE, ref = "Never")

  or_col <- attr(res_or, "or")
  expect_false(is.null(or_col))

  # A primeira linha de OR deve ser a referência
  expect_equal(or_col[1], "1.00 (Ref)")
})

test_that("tb() lida com valores NA via flag 'm'", {
  # Variável 'income' possui 40 NA's segundo o summary
  res_na <- tb(epitabl, income, m)

  # Verifica se a tabela considerou os valores NA
  nomes_linhas <- rownames(unclass(res_na))
  expect_true(any(is.na(nomes_linhas)) || any(nomes_linhas == "<NA>"))
})

test_that("tb() lida com variáveis contínuas", {
  # Média e desvio padrão de 'age' cruzado com 'disease'
  res_cont <- tb(epitabl, age, disease, var.type = c(age = "continuous"), stat.cont = "mean")

  expect_true(attr(res_cont, "is_continuous"))
  expect_equal(attr(res_cont, "stat_label"), "Mean (SD)")

  # Dimensão deve ser 1 linha (age) e 3 colunas (No, Yes, Total)
  expect_equal(dim(res_cont), c(1, 3))
})

test_that("tb() aplica testes estatísticos (Chi-squared)", {
  res_test <- tb(epitabl, smoking, disease, test = TRUE)
  stats <- attr(res_test, "stats")

  expect_false(is.null(stats))
  # Como a tabela é 3x2, ele deve aplicar o teste Qui-quadrado de Pearson
  expect_true(grepl("Chi-squared", stats$method))
})

test_that("tb() estratifica corretamente", {
  res_strat <- tb(epitabl, smoking, disease, strat = region)
  cols <- colnames(res_strat)

  # Verifica se a coluna tem o nome combinado da estratificação, ex: "North : No"
  expect_true(any(grepl("North :", cols)) || any(grepl("South :", cols)))
})

test_that("tb() aplica filtros (subset) corretamente", {
  # O dataset epitabl original tem 500 linhas.
  # Vamos filtrar apenas os pacientes com idade > 60.
  res_sub <- tb(epitabl, disease, subset = age > 60)
  total_filtrado <- sum(unclass(res_sub))

  # O total deve ser estritamente menor que 500
  expect_true(total_filtrado < 500)
  expect_true(total_filtrado > 0)
})

test_that("tb() suporta testes estatísticos específicos (Fisher e McNemar)", {
  # Teste Exato de Fisher
  res_fisher <- tb(epitabl, rapid_test, lab_confirmed, test = "fisher")
  stats_fisher <- attr(res_fisher, "stats")
  expect_equal(stats_fisher$method, "Fisher's Exact Test for Count Data")

  # Teste de McNemar (ideal para matrizes 2x2 pareadas)
  res_mcnemar <- tb(epitabl, rapid_test, lab_confirmed, test = "mcnemar")
  stats_mcnemar <- attr(res_mcnemar, "stats")
  expect_true(grepl("McNemar", stats_mcnemar$method))
})

test_that("tb() exibe Mediana (IQR) como padrão para variáveis contínuas", {
  # Não passamos 'stat.cont', então deve usar o default "median"
  res_med <- tb(epitabl, bmi, disease, var.type = c(bmi = "cont"))

  expect_equal(attr(res_med, "stat_label"), "Median (IQR)")

  # O formato de saída deve conter um traço separando os quartis, ex: "26.9 (23.7 - 30.2)"
  mat <- as.matrix(unclass(res_med))
  expect_true(grepl("\\(", mat[1, 1]))
  expect_true(grepl("-", mat[1, 1]))
})

test_that("as.data.frame.tb converte corretamente a tabela e aplica estilos customizados", {
  # Usando um estilo de string customizado para as porcentagens
  res_custom <- tb(epitabl, smoking, disease, col, style = "{n} [{p}%]")
  df_custom <- as.data.frame(res_custom)

  expect_s3_class(df_custom, "data.frame")

  # A formatação customizada deve refletir no data.frame convertido
  # Exemplo: deve haver algo como "100 [20.0%]" nas células numéricas
  expect_true(any(grepl("\\[.*\\]", df_custom[1, 2])))
})

test_that("as.data.frame.tb() anexa as colunas extras de PR e OR", {
  res_rp <- tb(epitabl, smoking, disease, rp = TRUE)
  df_rp <- as.data.frame(res_rp)

  # O dataframe resultante deve ter "PR (95% CI)" como uma de suas colunas
  expect_true("PR (95% CI)" %in% colnames(df_rp))

  # O nome da primeira coluna (que representa as linhas) deve ser herdado corretamente
  expect_equal(colnames(df_rp)[1], "Smoking status")
})

# --- Error handling ---

test_that("tb() sem dados", {
  expect_error(tb(), "No data provided")
})

test_that("tb() +2 vars", {
  expect_error(tb(epitabl, disease, sex, region),
               "Maximum of 2 variables")
})

test_that("tb() d inválido", {
  expect_error(tb(epitabl, disease, d = -1), "'d' must be")
})

test_that("tb() conf.level inválido", {
  expect_error(tb(epitabl, disease, sex, conf.level = 1.5), "'conf.level' must be")
})

test_that("tb() RP com strat", {
  expect_warning(
    tb(epitabl, disease, sex, strat = region, rp = TRUE),
    "PR/OR disabled when stratification is used"
  )
})

