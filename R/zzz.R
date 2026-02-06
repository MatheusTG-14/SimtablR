.onAttach <- function(libname, pkgname) {
  ver <- utils::packageVersion(pkgname)
  msg <- cli::rule(left = paste0("SimtabR ", ver), right = "Simple R functions for usefull tables")
  packageStartupMessage(cli::col_cyan(msg))
  packageStartupMessage(
    cli::col_green(cli::symbol$tick), " ", cli::col_blue("tb()"),    cli::col_white(": Descriptive tables (One-Table)\n"),
    cli::col_green(cli::symbol$tick), " ", cli::col_blue("diag_test()"),cli::col_white(": Confusion matrix and Diagnostic Values\n"),
    cli::col_green(cli::symbol$tick), " ", cli::col_blue("regtab()"),   cli::col_white(": Regression Tables by Outcome (GLM)")
  )
  packageStartupMessage(
    cli::col_silver("Use suppressPackageStartupMessages() to silence.")
  )
}
