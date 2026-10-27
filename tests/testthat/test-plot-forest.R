# Forest-plot render-edge labels: measure follows the recorded link, terms use
# the same variable labels as the table renderer.

forest_axis <- function(x) ggplot2::autoplot(x)$labels$x
forest_y <- function(x) levels(ggplot2::autoplot(x)$data$label)

test_that("forest axis label follows the recorded link, not the family alone", {
  skip_if_not_installed("ggplot2")
  d <- regtab_fixture()
  data(epitabl, package = "SimtablR")

  logit <- regtab(d, "disease_a", ~ age + sex, family = binomial("logit"))
  expect_equal(forest_axis(logit), "OR (95% CI)")

  logbin <- regtab(epitabl, "mace_event", ~ renal_impairment + age, family = binomial("log"))
  expect_equal(forest_axis(logbin), "RR (95% CI)")

  # Modified Poisson on a 0/1 outcome is a risk ratio; a design picks PR.
  modpois <- regtab(d, "disease_a", ~ age + sex, family = poisson("log"))
  expect_equal(forest_axis(modpois), "RR (95% CI)")
  cross <- regtab(d, "disease_a", ~ age + sex, family = poisson("log"),
                  design = "cross_sectional")
  expect_equal(forest_axis(cross), "PR (95% CI)")

  # Counts stay incidence rate ratios.
  counts <- regtab(d, "visits_a", ~ age + sex, family = poisson("log"))
  expect_equal(forest_axis(counts), "IRR (95% CI)")
})

test_that("forest axis label covers Firth and non-exponentiated fits", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("logistf")
  d <- regtab_fixture()
  data(epitabl, package = "SimtablR")
  firth <- regtab(d, "disease_a", ~ age + sex, family = binomial("logit"), method = "firth")
  expect_equal(forest_axis(firth), "OR (95% CI)")

  raw <- regtab(epitabl, "mace_event", ~ renal_impairment + age,
                family = binomial("log"), exponentiate = FALSE)
  expect_equal(SimtablR:::.forest_measure(raw), "Estimate")
})

test_that("forest plotting does not change stored evidence", {
  skip_if_not_installed("ggplot2")
  data(epitabl, package = "SimtablR")
  fit <- regtab(epitabl, "mace_event", ~ renal_impairment + age, family = binomial("log"))
  before <- fit$data
  invisible(ggplot2::autoplot(fit))
  expect_identical(fit$data, before)
})

test_that("forest y-axis uses the table's variable labels for regtab", {
  skip_if_not_installed("ggplot2")
  d <- regtab_fixture()
  attr(d$age, "label") <- "Age at index (years)"
  fit <- regtab(d, "disease_a", ~ age + sex, family = binomial("logit"))
  table_terms <- unname(SimtablR:::.regtab_term_labels(fit))
  expect_setequal(forest_y(fit), table_terms)
  expect_true("Age at index (years)" %in% forest_y(fit))
  expect_false("age" %in% forest_y(fit))

  override <- label(fit, age = "Years of age")
  expect_true("Years of age" %in% forest_y(override))
})

test_that("forest y-axis uses variable labels for Cox results", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("survival")
  data(epitabl, package = "SimtablR")
  fit <- survtab(epitabl, time = "mace_time_days", event = "mace_event",
                 predictors = ~ renal_impairment + age)
  y <- forest_y(fit)
  expect_equal(forest_axis(fit), "HR (95% CI)")
  expect_false(any(grepl("^renal_impairment", y)))
  expect_setequal(y, as.character(as.data.frame(fit)$Term))
})
