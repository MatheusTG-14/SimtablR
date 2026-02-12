#' @importFrom stats addmargins anova as.formula binom.test chisq.test coef
#' @importFrom stats confint fisher.test glm kruskal.test lm mcnemar.test
#' @importFrom stats pnorm poisson qnorm quantile relevel sd setNames t.test
#' @importFrom stats update wilcox.test
#' @importFrom utils write.csv
#' @importFrom dplyr %>%
#' @keywords internal
"_PACKAGE"

# The following block is strictly for namespace management
NULL
utils::globalVariables(c("Variable", "Result", "Outcome", "P_Value"))
