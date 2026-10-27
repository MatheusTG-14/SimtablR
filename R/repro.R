# COMPUTATIONAL REPRODUCIBILITY AND MANIFEST GENERATION
# Records runtime environments, session dependencies, data hash fingerprints,
# methodological choices, and design resolutions for transparent scientific auditing.

#########
# REPRODUCIBILITY MANIFEST GENERATOR
# S3 method dispatch and manifest compilation across specifications, results, and reports.

#' Build a reproducibility manifest
#'
#' Captures the runtime and recorded analysis decisions needed to reproduce a
#' SimtablR result. The data hash and ruleset version are read from the result;
#' the data are not re-hashed.
#'
#' @param result A `simtab_result` or `simtab_spec`.
#' @return A `simtab_manifest` list. `estimator`, `interval_method`, `tests`,
#'   and the corrections in `engine_decisions` are the same recorded decisions
#'   that [as_methods()] writes as prose. `packages` lists SimtablR, its
#'   Imports, and the suggested packages the result's engine used (for
#'   example pROC for [roc()]), alongside the R version and platform in `r`.
#'   `design` and `design_source` describe
#'   any study design recorded on the specification or the source data frame,
#'   whether or not it took part in resolving `measure`; `design_used`
#'   distinguishes the two, and is `TRUE` only when `resolved_from` is
#'   `"design"`. A design recorded only on the data frame (`set_design()` on
#'   a `data.frame`) is advisory and never sets `design_used`; see
#'   [set_design()].
#' @examples
#' res <- tb(epitabl, sex, diabetes)
#' repro_manifest(res)
#' @export
repro_manifest <- function(result) {
  UseMethod("repro_manifest")
}

#' @export
repro_manifest.simtab_spec <- function(result) {
  repro_manifest(evaluate(result))
}

#' @export
repro_manifest.simtab_result <- function(result) {
  result <- .compute_if_spec(result)
  result <- validate_simtab_result(result)
  decisions <- .result_decisions(result)
  structure(
    c(
      .manifest_header(result$used, decisions$packages),
      list(
        engine = result$meta$engine %||% result$spec$engine,
        estimator = .manifest_value(decisions$estimator),
        interval_method = .manifest_value(decisions$interval_method),
        engine_decisions = .manifest_engine_decisions(result, decisions),
        design = .result_design(result),
        design_source = .result_design_source(result),
        design_used = identical(result$spec$effect$resolved_from, "design"),
        measure = result$spec$effect$measure %||% result$meta$effect %||% decisions$measure,
        resolved_from = result$spec$effect$resolved_from %||% NULL,
        measure_computation = result$meta$effect_measure_meta %||% NULL,
        tests = decisions$tests,
        seed = .manifest_seed(),
        ruleset_version = result$meta$ruleset_version %||% .simtab_ruleset_version()
      )
    ),
    class = "simtab_manifest"
  )
}

#' @export
repro_manifest.simtab_report <- function(result) {
  result <- validate_simtab_report(result)
  item_manifests <- lapply(result$items, .manifest_report_item)
  packages <- unlist(lapply(result$items, function(item) .result_decisions(item)$packages))
  structure(
    c(
      .manifest_header(result$used, packages),
      list(
        items = item_manifests,
        methods = as_methods(result),
        advice_ids = vapply(result$advice, `[[`, character(1), "id"),
        seed = .manifest_seed(),
        ruleset_version = .simtab_ruleset_version()
      )
    ),
    class = "simtab_manifest"
  )
}

#########
# MANIFEST METADATA AND PROVENANCE EXTRACTORS
# Compiles session environments, data hashes, engine choices, and hypothesis tests.

#' Compile runtime environment and captured data hash header
#' @keywords internal
#' @noRd
.manifest_header <- function(used, packages = character()) {
  list(
    r = list(
      version.string = R.version.string,
      platform = R.version$platform
    ),
    packages = .manifest_packages(packages),
    data_hash = used$hash,
    data_hash_algorithm = "xxhash64",
    data = list(
      nrow = used$nrow,
      ncol = used$ncol,
      names = used$names
    )
  )
}

#' Extract reproducibility manifest sub-record for an individual report item
#' @keywords internal
#' @noRd
.manifest_report_item <- function(result) {
  manifest <- repro_manifest(result)
  list(
    class = class(result),
    engine = manifest$engine,
    estimator = manifest$estimator,
    interval_method = manifest$interval_method,
    engine_decisions = manifest$engine_decisions,
    design = manifest$design,
    design_source = manifest$design_source,
    design_used = manifest$design_used,
    measure = manifest$measure,
    resolved_from = manifest$resolved_from,
    measure_computation = manifest$measure_computation,
    tests = manifest$tests,
    ruleset_version = manifest$ruleset_version
  )
}

#' Keep an empty recorded decision as NULL in the manifest
#' @keywords internal
#' @noRd
.manifest_value <- function(x) {
  if (length(x) == 0) NULL else x
}

#' Extract recorded analytical choices and model metadata from result
#' @keywords internal
#' @noRd
.manifest_engine_decisions <- function(result, decisions = .result_decisions(result)) {
  fields <- c(
    "measures", "outcome", "method", "vcov", "exposure", "exposure_levels",
    "family", "link", "model_n"
  )
  out <- result$meta[intersect(fields, names(result$meta))]
  extra <- decisions[setdiff(names(decisions), c(
    "measure", "estimator", "interval_method", "tests", "homogeneity", "packages"
  ))]
  out <- c(out, extra)
  out[!vapply(out, function(value) length(value) == 0, logical(1))]
}

#' Report versions of SimtablR, its Imports, and the Suggests a result used
#' @keywords internal
#' @noRd
.manifest_packages <- function(packages = character()) {
  description <- system.file("DESCRIPTION", package = "SimtablR")
  imports <- if (nzchar(description)) {
    fields <- read.dcf(description, fields = "Imports")[1, "Imports"]
    trimws(sub("\\(.*$", "", strsplit(fields, ",")[[1]]))
  } else {
    character()
  }
  pkgs <- sort(unique(c(imports[nzchar(imports)], packages)))
  versions <- vapply(pkgs, function(pkg) {
    tryCatch(as.character(utils::packageVersion(pkg)), error = function(e) NA_character_)
  }, character(1))
  simtab_ver <- tryCatch(as.character(utils::packageVersion("SimtablR")), error = function(e) NA_character_)
  c(SimtablR = simtab_ver, versions)
}

#' Capture active random number generator seed if initialized
#' @keywords internal
#' @noRd
.manifest_seed <- function() {
  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    return(get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)[1])
  }
  NULL
}

#########
# S3 PRESENTATION METHODS
# Formats reproducibility manifest for human-readable console inspection.

#' @export
print.simtab_manifest <- function(x, ...) {
  cat("SimtablR reproducibility manifest\n")
  cat("  R: ", x$r$version.string, "\n", sep = "")
  cat("  data hash (", x$data_hash_algorithm, "): ", x$data_hash, "\n", sep = "")
  if (!is.null(x$items)) {
    cat("  items: ", paste(names(x$items), collapse = ", "), "\n", sep = "")
  } else {
    cat("  design: ", x$design %||% "<unset>", "\n", sep = "")
    if (!is.null(x$design) && !isTRUE(x$design_used)) {
      cat("  design used: FALSE (advisory only; not part of measure resolution)\n")
    }
    cat("  measure: ", x$measure %||% "<unset>", "\n", sep = "")
  }
  cat("  ruleset: ", x$ruleset_version %||% "<unset>", "\n", sep = "")
  invisible(x)
}
