library(testthat)
library(SimtablR)
data(epitabl, package = "SimtablR")

test_that("simtab initializes an inert simtab_spec", {
  spec <- simtab(epitabl)
  expect_s3_class(spec, "simtab_spec")
  expect_true(all(c("data_src", "engine", "roles", "summary", "effect") %in% names(spec)))
})

test_that("evaluate transforms simtab_spec into computed simtab_result", {
  spec <- simtab(epitabl) |> describe(age, sex)
  res <- suppressMessages(evaluate(spec))
  expect_s3_class(res, "simtab_result")
  expect_true(all(c("spec", "data", "meta", "used", "advice", "call") %in% names(res)))
})

test_that("grammar verbs adjust, stratify, and measure modify spec correctly", {
  spec <- simtab(epitabl) |>
    describe(hypertension) |>
    stratify(adjudicated_acs) |>
    measure("OR") |>
    adjust(age, sex)
  expect_identical(rlang::as_label(spec$roles$by), "adjudicated_acs")
  expect_identical(spec$effect$measure, "OR")
  expect_identical(unname(vapply(spec$roles$adjust, rlang::as_label, character(1))), c("age", "sex"))
})

test_that("formatting verbs set_summary, missingness, and label update layout", {
  spec <- simtab(epitabl) |>
    describe(age) |>
    set_summary(stat = "mean") |>
    missingness(display = FALSE) |>
    label(age = "Patient Age (years)")
  expect_identical(spec$summary$default, "mean")
  expect_identical(spec$missing$display, FALSE)
  expect_identical(spec$fmt$labels[["age"]], "Patient Age (years)")
})

test_that("verb closure recomputes evidence when applied to simtab_result", {
  res1 <- suppressMessages(table1(epitabl, "age", summary = "median"))
  res2 <- suppressMessages(set_summary(res1, stat = "mean"))
  expect_s3_class(res2, "simtab_result")
  expect_length(res2$data$age$summary$Overall, 2)
})

test_that("terse flags resolve properly through central machinery", {
  t_flags <- suppressMessages(tb(epitabl, hypertension, adjudicated_acs, flags = c("row", "or", "p")))
  expect_identical(t_flags$spec$effect$measure, "OR")
  expect_identical(t_flags$meta$flags$by, "row")
})

test_that("simtablr composes Table 1 and Table 2 into simtab_report", {
  rep <- suppressMessages(simtablr(epitabl, outcome = adjudicated_acs, exposure = hypertension, vars = c(age, sex)))
  expect_s3_class(rep, "simtab_report")
  expect_true(all(c("table1", "table2") %in% names(rep$items)))
})

test_that("tidy and as.data.frame methods extract evidence from simtab_result", {
  res <- suppressMessages(table1(epitabl, "age"))
  expect_s3_class(as.data.frame(res), "data.frame")
  expect_s3_class(generics::tidy(res), "data.frame")
})

test_that("spec preserves provenance and analysis decisions", {
  spec <- simtab(epitabl) |> describe(age)
  expect_type(spec$data_src$hash, "character")
  expect_true(nzchar(spec$data_src$hash))
})

test_that("invalid measure or design combinations throw classed errors", {
  expect_error(simtab(epitabl) |> measure("INVALID_MEASURE") |> evaluate())
})
